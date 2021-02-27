(*
    Copyright (c) 2021 David C. J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    Licence version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public Licence for more details.
    
    You should have received a copy of the GNU Lesser General Public
    Licence along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

functor Arm64GenCode (
    structure FallBackCG: GENCODESIG
    and       BackendTree: BackendIntermediateCodeSig
    and       CodeArray: CODEARRAYSIG
    and       Arm64Assembly: Arm64Assembly
    and       Debug: DEBUG
    and       Arm64Foreign: FOREIGNCALLSIG
    
    sharing FallBackCG.Sharing = BackendTree.Sharing = CodeArray.Sharing =
        Arm64Assembly.Sharing
) : GENCODESIG =
struct

    open BackendTree CodeArray Arm64Assembly Address
    
    exception InternalError = Misc.InternalError
    
    exception Fallback of string
    
    (* tag a short constant *)
    fun tag c = 2 * c + 1
    and semitag c = 2*c
    
    fun taggedWord w: word = w * 0w2 + 0w1
    and taggedWord64 w: Word64.word = w * 0w2 + 0w1
    
    val tagBitMask = Word64.<<(Word64.fromInt ~1, 0w1)
    
    fun gen(instr, code) = code := instr :: !code

    fun genPushReg(reg, code) = gen(storeRegPreIndex{regT=reg, regN=X_MLStackPtr, byteOffset= ~8}, code)
    and genPopReg(reg, code) = gen(loadRegPostIndex{regT=reg, regN=X_MLStackPtr, byteOffset= 8}, code)
    
    (* Move register.  The ARM64 alias uses XZR as Rn. *)
    fun genMoveRegToReg{sReg, dReg} = orrShiftedReg{regN=XZero, regM=sReg, regD=dReg, shift=ShiftNone}

    (* Add a constant word to the source register and put the result in the
       destination.  regW is used as a work register if necessary.  This is used
       both for addition and subtraction. *)
    fun addConstantWord({regS, regD, value=0w0, ...}, code) =
        if regS = regD then () else gen(genMoveRegToReg{sReg=regS, dReg=regD}, code)
    
    |   addConstantWord({regS, regD, regW, value}, code) =
        let
            (* If we have to load the constant it's better if the top 32-bits are
               zero if possible. *)
            val (isSub, unsigned) =
                if value > Word64.<<(0w1, 0w63)
                then (true, ~ value)
                else (false, value)
        in
            if unsigned < Word64.<<(0w1, 0w24)
            then (* We can put up to 24 in a shifted and an unshifted constant. *)
            let
                val w = Word.fromLarge(Word64.toLarge unsigned)
                val high = Word.andb(Word.>>(w, 0w12), 0wxfff)
                val low = Word.andb(w, 0wxfff)
                val addSub = if isSub then subImmediate else addImmediate
            in
                if high <> 0w0
                then
                (
                    gen(addSub{regN=regS, regD=regD, immed=high, shifted=true}, code);
                    if low <> 0w0
                    then gen(addSub{regN=regD, regD=regD, immed=low, shifted=false}, code)
                    else ()
                )
                else gen(addSub{regN=regS, regD=regD, immed=low, shifted=false}, code)
            end
            else
            let
                (* To minimise the constant and increase the chances that it
                   will fit in a single word look to see if we can shift it. *)
                fun getShift(value, shift) =
                    if Word64.andb(value, 0w1) = 0w0
                    then getShift(Word64.>>(value, 0w1), shift+0w1)
                    else (value, shift)
                val (shifted, shift) = getShift(unsigned, 0w0)
            in
                gen(loadNonAddressConstant(regW, shifted), code);
                gen((if isSub then subShiftedReg else addShiftedReg)
                    {regM=regW, regN=regS, regD=regD, shift=ShiftLSL shift}, code)
            end
        end
    
    (* Remove items from the stack. If the second argument is true the value
       on the top of the stack has to be moved. *)
    fun resetStack(0, _, _) = ()
    |   resetStack(nItems, true, code) =
        (
            genPopReg(X0, code);
            resetStack(nItems, false, code);
            genPushReg(X0, code)
        )
    |   resetStack(nItems, false, code) =
            addConstantWord({regS=X_MLStackPtr, regD=X_MLStackPtr, regW=X3,
                value=Word64.fromLarge(Word.toLarge wordSize) * Word64.fromInt nItems}, code)

    fun compareRegs(reg1, reg2, code) =
        gen(subSShiftedReg{regM=reg2, regN=reg1, regD=XZero, shift=ShiftNone}, code)

    (* Sequence to allocate on the heap.  Returns the result in X0.  The words are not initialised
       apart from the length word. *)
    fun genAllocateFixedSize(words, flags, code) =
    let
        val label = createLabel()
    in
        (* Subtract the number of bytes required from the heap pointer and put in X0. *)
        addConstantWord({regS=X_MLHeapAllocPtr, regD=X0, regW=X3,
            value= ~ (Word64.fromLarge(Word.toLarge wordSize)) * Word64.fromInt(words+1)}, code);
        compareRegs(X0, X_MLHeapLimit, code);
        gen(conditionalBranch(condCarrySet, label), code);
        gen(loadRegScaled{regT=X16, regN=X_MLAssemblyInt, unitOffset=heapOverflowCallOffset}, code);
        gen(branchAndLinkReg X16, code);
        gen(registerMask [], code); (* Not used at the moment. *)
        gen(setLabel label, code);
        gen(genMoveRegToReg{sReg=X0, dReg=X_MLHeapAllocPtr}, code);
        gen(loadNonAddressConstant(X1,
            Word64.orb(Word64.fromInt words, Word64.<<(Word64.fromLarge(Word8.toLarge flags), 0w56))), code);
        (* Store the length word.  Have to use the unaligned version because offset is -ve. *)
        gen(storeRegUnscaled{regT=X1, regN=X0, byteOffset= ~8}, code)
    end

    (* Allocate space on the heap for a vector, string etc.  sizeReg and flagsReg
       contain the size and flags as untagged values. sizeReg is unchanged, flagsReg
       is modified.  The result address is in resultReg.  All the registers must
       be different. *)
    fun allocateVariableSize({sizeReg, flagsReg, resultReg}, code) =
    let
        val trapLabel = createLabel() and noTrapLabel = createLabel()
    in
        (* Subtract the size as a number of bytes from the allocation ptr. *)
        gen(subShiftedReg{regM=sizeReg, regN=X_MLHeapAllocPtr, regD=resultReg, shift=ShiftLSL 0w3}, code);
        (* Subtract another 8 to allow for the length word. *)
        gen(subImmediate{regN=resultReg, regD=resultReg, immed=0w8, shifted=false}, code);
        (* If the size is large enough it is possible that this could wrap round.  To check for that
           we trap if either the result is less than the limit or if it is now greater than
           the allocation pointer. *)
        compareRegs(resultReg, X_MLHeapLimit, code);
        gen(conditionalBranch(condCarryClear, trapLabel), code);
        compareRegs(resultReg, X_MLHeapAllocPtr, code);
        gen(conditionalBranch(condCarryClear, noTrapLabel), code);
        gen(setLabel trapLabel, code);
        gen(loadRegScaled{regT=X16, regN=X_MLAssemblyInt, unitOffset=heapOverflowCallOffset}, code);
        gen(branchAndLinkReg X16, code);
        gen(registerMask [], code); (* Not used at the moment. *)
        gen(setLabel noTrapLabel, code);
        gen(genMoveRegToReg{sReg=resultReg, dReg=X_MLHeapAllocPtr}, code);
        (* Combine the size with the flags in the top byte. *)
        gen(orrShiftedReg{regM=flagsReg, regN=sizeReg, regD=flagsReg, shift=ShiftLSL 0w56}, code);
        (* Store the length word.  Have to use the unaligned version because offset is -ve. *)
        gen(storeRegUnscaled{regT=flagsReg, regN=resultReg, byteOffset= ~8}, code)
    end

    (* Set a register to either tagged(1) i.e. true or tagged(0) i.e. false. *)
    fun setBooleanCondition(reg, condition, code) =
    (
        gen(loadNonAddressConstant(reg, Word64.fromInt(tag 1)), code);
        (* If the condition is false the value used is the XZero incremented by 1 i.e. 1 *)
        gen(conditionalSetIncrement{regD=reg, regTrue=reg, regFalse=XZero, cond=condition}, code)
    )

    (* Raise the overflow exception if the overflow bit has been set. *)
    fun checkOverflow code =
    let
        val noOverflow = createLabel()
    in
        gen(conditionalBranch(condNoOverflow, noOverflow), code);
        gen(loadAddressConstant(X0, toMachineWord Overflow), code);
        gen(loadRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset}, code);
        gen(loadRegScaled{regT=X1, regN=X_MLStackPtr, unitOffset=0}, code);
        gen(branchRegister X1, code);
        gen(setLabel noOverflow, code)
    end

    (* Stack check code: this is inserted at the start of a function to check that there
       is sufficient ML stack available.  It is also inserted, with a zero space value,
       in a loop to ensure that the RTS can interrupt a function.
       debugTrapAlways can be used to set a sort of breakpoint during debugging. *)
    fun checkStackCode(regW, space, debugTrapAlways, code) =
    let
        val skipCheck = createLabel()
        val defaultWords = 10 (* This is wired into the RTS. *)
        val (testReg, entryPt) =
            if space <= defaultWords
            then (X_MLStackPtr, stackOverflowCallOffset)
            else
            (
                (* This is only used at the start of the code.  X9 is wired into the RTS. *)
                addConstantWord({regS=X_MLStackPtr, regD=X9, regW=regW,
                    value= ~ (Word64.fromLarge(Word.toLarge wordSize)) * Word64.fromInt space}, code);
                (X9, stackOverflowXCallOffset)
            )
    in
        gen(loadRegScaled{regT=regW, regN=X_MLAssemblyInt, unitOffset=stackLimitOffset}, code);
        if debugTrapAlways
        then ()
        else
        (
            compareRegs(testReg, regW, code);
            gen(conditionalBranch(condCarrySet, skipCheck), code)
        );
        gen(loadRegScaled{regT=X16, regN=X_MLAssemblyInt, unitOffset=entryPt}, code);
        gen(branchAndLinkReg X16, code);
        gen(registerMask [], code); (* Not used at the moment. *)
        gen(setLabel skipCheck, code)
    end

    fun toDo s = raise Fallback s

    fun genOpcode (n, _) =  toDo n

    fun genDoubleToFloat _ =  toDo "genDoubleToFloat"
    fun genRealToInt _ =  toDo "genRealToInt"
    fun genFloatToInt _ =  toDo "genFloatToInt"

    val opcode_atomicExchAdd = "opcode_atomicExchAdd"
and opcode_atomicReset = "opcode_atomicReset"
and opcode_longWToTagged = "opcode_longWToTagged"
and opcode_signedToLongW = "opcode_signedToLongW"
and opcode_unsignedToLongW = "opcode_unsignedToLongW"
and opcode_realAbs = "opcode_realAbs"
and opcode_realNeg = "opcode_realNeg"
and opcode_fixedIntToReal = "opcode_fixedIntToReal"
and opcode_fixedIntToFloat = "opcode_fixedIntToFloat"
and opcode_floatToReal = "opcode_floatToReal"
and opcode_floatAbs = "opcode_floatAbs"
and opcode_floatNeg = "opcode_floatNeg"
and opcode_fixedQuot = "opcode_fixedQuot"
and opcode_fixedRem = "opcode_fixedRem"
and opcode_lgWordEqual = "opcode_lgWordEqual"
and opcode_lgWordLess = "opcode_lgWordLess"
and opcode_lgWordLessEq = "opcode_lgWordLessEq"
and opcode_lgWordGreater = "opcode_lgWordGreater"
and opcode_lgWordGreaterEq = "opcode_lgWordGreaterEq"
and opcode_lgWordAdd = "opcode_lgWordAdd"
and opcode_lgWordSub = "opcode_lgWordSub"
and opcode_lgWordMult = "opcode_lgWordMult"
and opcode_lgWordDiv = "opcode_lgWordDiv"
and opcode_lgWordMod = "opcode_lgWordMod"
and opcode_lgWordAnd = "opcode_lgWordAnd"
and opcode_lgWordOr = "opcode_lgWordOr"
and opcode_lgWordXor = "opcode_lgWordXor"
and opcode_lgWordShiftLeft = "opcode_lgWordShiftLeft"
and opcode_lgWordShiftRLog = "opcode_lgWordShiftRLog"
and opcode_lgWordShiftRArith = "opcode_lgWordShiftRArith"
and opcode_realEqual = "opcode_realEqual"
and opcode_realLess = "opcode_realLess"
and opcode_realLessEq = "opcode_realLessEq"
and opcode_realGreater = "opcode_realGreater"
and opcode_realGreaterEq = "opcode_realGreaterEq"
and opcode_realUnordered = "opcode_realUnordered"
and opcode_realAdd = "opcode_realAdd"
and opcode_realSub = "opcode_realSub"
and opcode_realMult = "opcode_realMult"
and opcode_realDiv = "opcode_realDiv"
and opcode_floatEqual = "opcode_floatEqual"
and opcode_floatLess = "opcode_floatLess"
and opcode_floatLessEq = "opcode_floatLessEq"
and opcode_floatGreater = "opcode_floatGreater"
and opcode_floatGreaterEq = "opcode_floatGreaterEq"
and opcode_floatUnordered = "opcode_floatUnordered"
and opcode_floatAdd = "opcode_floatAdd"
and opcode_floatSub = "opcode_floatSub"
and opcode_floatMult = "opcode_floatMult"
and opcode_floatDiv = "opcode_floatDiv"

and opcode_loadC8 = "opcode_loadC8"
and opcode_loadC16 = "opcode_loadC16"
and opcode_loadC32 = "opcode_loadC32"
and opcode_loadC64 = "opcode_loadC64"
and opcode_loadCFloat = "opcode_loadCFloat"
and opcode_loadCDouble = "opcode_loadCDouble"
and opcode_storeC8 = "opcode_storeC8"
and opcode_storeC16 = "opcode_storeC16"
and opcode_storeC32 = "opcode_storeC32"
and opcode_storeC64 = "opcode_storeC64"
and opcode_storeCFloat = "opcode_storeCFloat"
and opcode_storeCDouble = "opcode_storeCDouble"
and opcode_allocCSpace = "opcode_allocCSpace"
and opcode_freeCSpace = "opcode_freeCSpace"
and opcode_arbAdd = "opcode_arbAdd"
and opcode_arbSubtract = "opcode_arbSubtract"
and opcode_arbMultiply = "opcode_arbMultiply"
and cpuPause = "cpuPause"

    type caseForm =
        {
            cases   : (backendIC * word) list,
            test    : backendIC,
            caseType: caseType,
            default : backendIC
        }
   
    (* Where the result, if any, should go *)
    datatype whereto =
        NoResult     (* discard result *)
    |   ToStack     (* Need a result but it can stay on the pseudo-stack *)
    |   ToX0        (* Need a result in X0. *)
  
    (* Are we at the end of the function. *)
    datatype tail =
        EndOfProc
    |   NotEnd

    (* Code generate a function or global declaration *)
    fun codegen (pt, name, resultClosure, numOfArgs, localCount, parameters) =
    let
        val cvec = ref []
        
        datatype decEntry =
            StackAddr of int
        |   Empty
    
        val decVec = Array.array (localCount, Empty)
    
        (* Count of number of items on the stack.  This excludes the arguments and
           the return address. *)
        val realstackptr = ref 1 (* The closure ptr is already there *)
        
        (* Maximum size of the stack. *)
        val maxStack = ref 1

        (* Whether the top of the stack is actually in X0. *)
        val topInX0 = ref false

        (* Push a value onto the stack. *)
        fun incsp () =
        (
            realstackptr := !realstackptr + 1;
            if !realstackptr > !maxStack
            then maxStack := !realstackptr
            else ()
        )

        (* An entry has been removed from the stack. *)
        fun decsp () = realstackptr := !realstackptr - 1
        
        fun ensureX0 () = if ! topInX0 then (genPushReg(X0, cvec); incsp(); topInX0 := false) else ()

        (* generates code from the tree *)
        fun gencde (pt : backendIC, whereto : whereto, tailKind : tail, loopAddr) : unit =
        let
            (* Save the stack pointer value here. We may want to reset the stack. *)
            val oldsp = !realstackptr;

            (* Operations on ML memory always have the base as an ML address.
               Word operations are always word aligned.  The higher level will
               have extracted any constant offset and scaled it if necessary.
               That's helpful for the X86 but not for the ARM.  We
               have to turn them back into indexes. *)
            (* This pushes two values to the stack: the base address and the index. *)
            fun genMLAddress({base, index, offset}, scale) =
            (
                gencde (base, ToStack, NotEnd, loopAddr);
                offset mod scale = 0 orelse raise InternalError "genMLAddress";
                case (index, offset div scale) of
                    (NONE, soffset) =>
                        (gen(loadNonAddressConstant(X0, Word64.fromInt(tag soffset)), cvec); genPushReg(X0, cvec); incsp())
                |   (SOME indexVal, 0) => gencde (indexVal, ToStack, NotEnd, loopAddr)
                |   (SOME indexVal, soffset) =>
                    (
                        gencde (indexVal, ToX0, NotEnd, loopAddr);
                        (* Add the offset as a shifted but not tagged value. *)
                        addConstantWord({regS=X0, regD=X0, regW=X1, value=Word64.fromInt(semitag soffset)}, cvec);
                        genPushReg(X0, cvec);
                        incsp();
                        topInX0 := false
                    )
            )

            datatype mlLoadKind = MLLoadOffset of int | MLLoadReg of xReg
            
            fun genMLLoadAddress({base, index=NONE, offset}, scale) =
                (* The index, if any, is a constant. *)
                (
                    gencde (base, ToX0, NotEnd, loopAddr);
                    (X0, MLLoadOffset(offset div scale))
                )
            
            |   genMLLoadAddress({base, index=SOME indexVal, offset}, scale) =
                (
                    gencde (base, ToStack, NotEnd, loopAddr); (* Push base addr to stack. *)
                    gencde (indexVal, ToX0, NotEnd, loopAddr);
                    (* Shift right to remove the tag.  N.B.  Indexes into ML memory are
                       unsigned.  Unlike on the X86 we can't remove the tag by providing
                       a displacement and the only options are to scale by either 1 or 8. *)
                    gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize64, shift=0w1}, cvec);
                    (* Add any constant offset.  Does nothing if it's zero. *)
                    addConstantWord({regS=X0, regD=X0, regW=X3,
                        value=Word64.fromInt (* unsigned *)(offset div scale)}, cvec);
                    genPopReg(X1, cvec); (* Pop base reg into X1. *)
                    decsp();
                    (X1, MLLoadReg X0)
                )

           (* Load the address, index value and offset for non-byte operations.
              Because the offset has already been scaled by the size of the operand
              we have to load the index and offset separately. *)
           fun genCAddress{base, index, offset} =
            (
                gencde (base, ToStack, NotEnd, loopAddr);
                case index of
                    NONE =>
                        (gen(loadNonAddressConstant(X0, Word64.fromInt(tag 0)), cvec); genPushReg(X0, cvec); incsp())
                |   SOME indexVal => gencde (indexVal, ToStack, NotEnd, loopAddr);
                gen(loadNonAddressConstant(X0, Word64.fromInt(tag offset)), cvec);
                genPushReg(X0, cvec); incsp()
            )

            (* Compare a block of bytes.  Jumps to labelEqual if all the bytes are
               equal up to the length.  Otherwise it drops through with the condition
               code set to the last byte comparison that tested unequal. *)
            fun blockCompareBytes(leftArg, rightArg, length, labelEqual, setZeroCC) =
            let
                val loopLabel = createLabel()
            in
                genMLAddress(leftArg, 1);
                genMLAddress(rightArg, 1);
                gencde (length, ToX0, NotEnd, loopAddr);
                genPopReg(X2, cvec); (* right arg index - tagged value. *)
                genPopReg(X1, cvec); (* right arg base address. *)
                (* Add in the index N.B. ML index values are unsigned. *)
                gen(addShiftedReg{regM=X2, regN=X1, regD=X1, shift=ShiftLSR 0w1}, cvec);
                genPopReg(X3, cvec); (* left index *)
                genPopReg(X2, cvec);
                decsp(); decsp(); decsp(); decsp();
                gen(addShiftedReg{regM=X3, regN=X2, regD=X2, shift=ShiftLSR 0w1}, cvec);
                (* Untag the length *)
                gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize64, shift=0w1}, cvec);
                (* If necessary set the cc for the case where the length is zero. *)
                if setZeroCC then compareRegs(X0, X0, cvec) else ();
                gen(setLabel loopLabel, cvec);
                gen(compareBranchZero(X0, WordSize64, labelEqual), cvec);
                (* X2 is left arg addr, X1 is right arg addr. *)
                gen(loadRegPostIndexByte{regT=X4, regN=X2, byteOffset=1}, cvec);
                gen(loadRegPostIndexByte{regT=X3, regN=X1, byteOffset=1}, cvec);
                compareRegs(X4, X3, cvec);
                gen(subImmediate{regN=X0, regD=X0, immed=0w1, shifted=false}, cvec);
                (* Loop if they're equal. *)
                gen(conditionalBranch(condEqual, loopLabel), cvec)
            end
    
         val () =
           case pt of
                BICEval evl => genEval (evl, tailKind)

            |   BICExtract ext =>
                    (* This may just be being used to discard a value which isn't
                       used on this branch.  N.B. genProc for mutual closures
                       assumes that this does not affect X1. *)
                if whereto = NoResult then ()
                else
                let     
                    fun loadLocalStackValue addr =
                    (
                        gen(loadRegScaled{regT=X0, regN=X_MLStackPtr, unitOffset= !realstackptr + addr}, cvec);
                        topInX0 := true
                    )
                in
                    case ext of
                        BICLoadArgument locn =>
                            (* The register arguments appear in order on the
                               stack, followed by the stack argumens in reverse
                               order. *)
                            if locn < 8
                            then loadLocalStackValue (locn+1)
                            else loadLocalStackValue (numOfArgs-locn+8)
                    |   BICLoadLocal locn =>
                        (
                            case Array.sub (decVec, locn) of
                                StackAddr n => loadLocalStackValue (~ n)
                            |   _ => (* Should be on the stack, not a function. *)
                                raise InternalError "locaddr: bad stack address"
                        )
                    |   BICLoadClosure locn =>
                        (
                            loadLocalStackValue ~1; (* The closure itself. *)
                            gen(loadRegScaled{regT=X0, regN=X0, unitOffset=locn+1 (* The first word is the code *)}, cvec)
                        )
                    |   BICLoadRecursive =>
                            loadLocalStackValue ~1 (* The closure itself - first value on the stack. *)
                end

            |   BICField {base, offset} =>
                (
                    gencde (base, ToX0, NotEnd, loopAddr);
                    gen(loadRegScaled{regT=X0, regN=X0, unitOffset=offset}, cvec)
                )

            |   BICLoadContainer {base, offset} =>
                (
                    gencde (base, ToX0, NotEnd, loopAddr);
                    gen(loadRegScaled{regT=X0, regN=X0, unitOffset=offset}, cvec)
                )
       
            |   BICLambda lam => genProc (lam, false, fn () => ())
           
            |   BICConstnt(w, _) =>
                (
                    (*if isShort w
                    then gen(loadNonAddressConstantX0, Word64.fromInt(tag(Word.toIntX(toShort w))), cvec)
                    else *)gen(loadAddressConstant(X0, w), cvec);
                    topInX0 := true
                )

            |   BICCond (testPart, thenPart, elsePart) =>
                    genCond (testPart, thenPart, elsePart, whereto, tailKind, loopAddr)
  
            |   BICNewenv(decls, exp) =>
                let         
                    (* Processes a list of entries. *)
            
                    (* Mutually recursive declarations. May be either lambdas or constants. Recurse down
                       the list pushing the addresses of the closure vectors, then unwind the 
                       recursion and fill them in. *)
                    fun genMutualDecs [] = ()

                    |   genMutualDecs ({lambda, addr, ...} :: otherDecs) =
                            genProc (lambda, true,
                                fn() =>
                                (
                                    Array.update (decVec, addr, StackAddr (! realstackptr));
                                    genMutualDecs (otherDecs)
                                ))

                    fun codeDecls(BICRecDecs dl) = genMutualDecs dl

                    |   codeDecls(BICDecContainer{size, addr}) =
                        (
                            (* If this is a container we have to process it here otherwise it
                               will be removed in the stack adjustment code. *)
                            (* The stack entries have to be initialised.  Set them to tagged(0). *)
                            gen(loadNonAddressConstant(X0, Word64.fromInt(tag 0)), cvec);
                            let fun pushN 0 = () | pushN n = (genPushReg(X0, cvec); pushN (n-1)) in pushN size end;
                            gen(genMoveRegToReg{sReg=X_MLStackPtr, dReg=X0}, cvec);
                            genPushReg(X0, cvec); (* Push the address of this container. *)
                            realstackptr := !realstackptr + size + 1; (* Pushes N words plus the address. *)
                            Array.update (decVec, addr, StackAddr(!realstackptr))
                        )

                    |   codeDecls(BICDeclar{value, addr, ...}) =
                        (
                            gencde (value, ToStack, NotEnd, loopAddr);
                            Array.update (decVec, addr, StackAddr(!realstackptr))
                        )
                    |   codeDecls(BICNullBinding exp) = gencde (exp, NoResult, NotEnd, loopAddr)
                in
                    List.app codeDecls decls;
                    gencde (exp, whereto, tailKind, loopAddr)
                end
          
            |   BICBeginLoop {loop=body, arguments} =>
                (* Execute the body which will contain at least one Loop instruction.
                   There will also be path(s) which don't contain Loops and these
                   will drop through. *)
                let
                    val args = List.map #1 arguments
                    (* Evaluate each of the arguments, pushing the result onto the stack. *)
                    fun genLoopArg ({addr, value, ...}) =
                        (
                         gencde (value, ToStack, NotEnd, loopAddr);
                         Array.update (decVec, addr, StackAddr (!realstackptr));
                         !realstackptr (* Return the posn on the stack. *)
                        )
                    val argIndexList = map genLoopArg args;

                    val startSp = ! realstackptr; (* Remember the current top of stack. *)
                    val startLoop = createLabel ()
                    val () = gen(setLabel startLoop, cvec) (* Start of loop *)
                in
                    (* Process the body, passing the jump-back address down for the Loop instruction(s). *)
                    gencde (body, whereto, tailKind, SOME(startLoop, startSp, argIndexList))
                    (* Leave the arguments on the stack.  They can be cleared later if needed. *)
                end

            |   BICLoop argList => (* Jump back to the enclosing BeginLoop. *)
                let
                    val (startLoop, startSp, argIndexList) =
                        case loopAddr of
                            SOME l => l
                        |   NONE => raise InternalError "No BeginLoop for Loop instr"
                    (* Evaluate the arguments.  First push them to the stack because evaluating
                       an argument may depend on the current value of others.  Only when we've
                       evaluated all of them can we overwrite the original argument positions. *)
                    fun loadArgs ([], []) = !realstackptr - startSp (* The offset of all the args. *)
                      | loadArgs (arg:: argList, _ :: argIndexList) =
                        let
                            (* Evaluate all the arguments. *)
                            val () = gencde (arg, ToStack, NotEnd, NONE);
                            val argOffset = loadArgs(argList, argIndexList);
                        in
                            genPopReg(X0, cvec);
                            gen(storeRegScaled{regT=X0, regN=X_MLStackPtr, unitOffset=argOffset-1}, cvec);
                            decsp(); (* The argument has now been popped. *)
                            argOffset
                        end
                      | loadArgs _ = raise InternalError "loadArgs: Mismatched arguments";

                    val _: int = loadArgs(List.map #1 argList, argIndexList)
                in
                    if !realstackptr <> startSp
                    then resetStack (!realstackptr - startSp, false, cvec) (* Remove any local variables. *)
                    else ();
            
                    (* Jump back to the start of the loop. *)
                    checkStackCode(X10, 0, false, cvec);
                    
                    gen(conditionalBranch(condAlways, startLoop), cvec)
                end
  
            |   BICRaise exp =>
                (
                    gencde (exp, ToStack, NotEnd, loopAddr);
                    genPopReg(X0, cvec);
                    (* Copy the handler "register" into the stack pointer.  Then
                       jump to the address in the first word.  The second word is
                       the next handler.  This is set up in the handler.  We have a lot
                       more raises than handlers since most raises are exceptional conditions
                       such as overflow so it makes sense to minimise the code in each raise. *)
                    gen(loadRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset}, cvec);
                    gen(loadRegScaled{regT=X1, regN=X_MLStackPtr, unitOffset=0}, cvec);
                    gen(branchRegister X1, cvec)
                )
  
            |   BICHandle {exp, handler, exPacketAddr} =>
                let
                    (* Save old handler *)
                    val () = gen(loadRegScaled{regT=X0, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset}, cvec)
                    val () = genPushReg(X0, cvec)
                    val () = incsp ()
                    val handlerLabel = createLabel()
                    (* Push address of handler. *)
                    val () = gen(loadLabelAddress(X0, handlerLabel), cvec)
                    val () = genPushReg(X0, cvec)
                    val () = incsp()
                    (* Store the address of the stack pointer into the handler register. *)
                    val () = gen(storeRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset}, cvec)

                    (* Code generate the body; "NotEnd" because we have to come back
                       to remove the handler; "ToStack" because delHandler needs
                       a result to carry down. *)
                    val () = gencde (exp, ToStack, NotEnd, loopAddr)
      
                    (* Now get out of the handler and restore the old one. *)
                    val () = genPopReg(X0, cvec) (* Pop the result. *)
                    val () = genPopReg(X1, cvec) (* Pop and discard the handler address. *)
                    val () = genPopReg(X1, cvec) (* Pop the old handler. *)
                    val () = gen(storeRegScaled{regT=X1, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset}, cvec)
                    val () = genPushReg(X0, cvec) (* Push the result. *)

                    val skipHandler = createLabel()
                    val () = gen(conditionalBranch (condAlways, skipHandler), cvec)
                    val () = realstackptr := oldsp
                    val () = gen(setLabel handlerLabel, cvec)
                    (* The exception raise code resets the stack pointer to the value in the exception handler
                       so this is probably redundant.  Leave it for the moment, *)
                    val () = gen(loadRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset}, cvec)
                    (* We must, though, restore the old handler. *)
                    val () = genPopReg(X1, cvec) (* Pop and discard the handler address. *)
                    val () = genPopReg(X1, cvec) (* Pop the old handler. *)
                    val () = gen(storeRegScaled{regT=X1, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset}, cvec)
                    (* Push the exception packet which is in X0 and set the address. *)
                    val () = genPushReg(X0, cvec)
                    val () = incsp ()
                    val () = Array.update (decVec, exPacketAddr, StackAddr(!realstackptr))
                    val () = gencde (handler, ToStack, NotEnd, loopAddr)
                    (* Have to remove the exception packet. *)
                    val () = resetStack(1, true, cvec)
                    val () = decsp()
          
                    (* Finally fix-up the jump around the handler *)
                    val () = gen(setLabel skipHandler, cvec)
                in
                    ()
                end
  
            |   BICCase ({cases, test, default, firstIndex, ...}) =>
                let
                    val () = gencde (test, ToStack, NotEnd, loopAddr)
                    (* Label to jump to at the end of each case. *)
                    val exitJump = createLabel()
                    val () = genPopReg(X0, cvec)
                    val () = decsp ()

                    (* Subtract the minimum even if it is zero to remove the tag.
                       This leaves us with a shifted but untagged value. Don't check for overflow.
                       Instead allow large values to wrap around and check later.*)
                    val () = addConstantWord({regS=X0, regD=X0, regW=X1,
                                    value= ~(taggedWord64(Word64.fromLarge(Word.toLargeX firstIndex)))}, cvec)

                    (* Create the case labels. *)
                    val nCases = List.length cases
                    val caseLabels = List.tabulate(nCases, fn _ => createLabel())
                    val defaultLabel = createLabel()
                    
                    (* Compare with the number of cases and go to the default if it is
                       not less. We use an unsigned comparison and compare with
                       the semitagged value because we've removed the tag bit. *)
                    (* TODO: Not necessary if it exhaustive. *)
                    (* For the moment load the value into a register and compare. *)
                    val () = gen(loadNonAddressConstant(X1, Word64.<<(Word64.fromInt nCases, 0w2)), cvec)
                    val () = compareRegs(X0, X1, cvec)
                    val () = gen(conditionalBranch(condCarrySet, defaultLabel), cvec)
                    (* Load the address of the jump table. *)
                    val tableLabel = createLabel()
                    val () = gen(loadLabelAddress(X1, tableLabel), cvec)
                    (* Add the value shifted by one since it's already shifted. *)
                    val () = gen(addShiftedReg{regM=X0, regN=X1, regD=X0, shift=ShiftLSL 0w1}, cvec)
                    val () = gen(branchRegister X0, cvec)
                    (* Put in the branch table. *)
                    val () = gen(setLabel tableLabel, cvec)
                    val () = List.app(fn label => gen(conditionalBranch(condAlways, label), cvec)) caseLabels

                    (* The default case, if any, follows the case statement. *)
                    (* If we have a jump to the default set it to jump here. *)
                    local
                        fun fixDefault(NONE, defCase) = gen(setLabel defCase, cvec)
                        |   fixDefault(SOME _, _) = ()
                    in
                        val () = ListPair.appEq fixDefault (cases, caseLabels)
                    end
                    val () = gen(setLabel defaultLabel, cvec)
                    val () = gencde (default, whereto, tailKind, loopAddr)

                    fun genCases(SOME body, label) =
                        (
                            (* First exit from the previous case or the default if
                               this is the first. *)
                            gen(conditionalBranch(condAlways, exitJump), cvec);
                            (* Remove the result - the last case will leave it. *)
                            case whereto of ToStack => decsp () | NoResult => () | ToX0 => ();
                            topInX0 := false;
                            (* Fix up the jump to come here. *)
                            gen(setLabel label, cvec);
                            gencde (body, whereto, tailKind, loopAddr)
                        )
                    |   genCases(NONE, _) = ()
                
                    val () = ListPair.appEq genCases (cases, caseLabels)
     
                    (* Finally set the exit jump to come here. *)
                    val () = gen(setLabel exitJump, cvec)
                in
                    ()
                end
  
            |   BICTuple recList =>
                let
                    val size = List.length recList
                in
                    (* Get the fields and push them to the stack. *)
                    List.app(fn v => gencde (v, ToStack, NotEnd, loopAddr)) recList;
                    genAllocateFixedSize(size, 0w0, cvec);
                    List.foldl(fn (_, w) =>
                        (genPopReg(X1, cvec); gen(storeRegScaled{regT=X1, regN=X0, unitOffset=w-1}, cvec); w-1))
                            size recList;
                    topInX0 := true;
                    realstackptr := !realstackptr - size
                end

            |   BICSetContainer{container, tuple, filter} =>
                (* Copy the contents of a tuple into a container.  If the tuple is a
                   Tuple instruction we can avoid generating the tuple and then
                   unpacking it and simply copy the fields that make up the tuple
                   directly into the container. *)
                (
                    case tuple of
                        BICTuple cl =>
                            (* Simply set the container from the values. *)
                        let
                            (* Push the address of the container to the stack. *)
                            val _ = gencde (container, ToStack, NotEnd, loopAddr)

                            fun setValues([], _, _) = ()

                            |   setValues(v::tl, sourceOffset, destOffset) =
                                if sourceOffset < BoolVector.length filter andalso BoolVector.sub(filter, sourceOffset)
                                then
                                (
                                    (* Get the value to store into X0. *)
                                    gencde (v, ToX0, NotEnd, loopAddr);
                                    (* Load the address of the container from the stack and store
                                       the value into the container. *)
                                    gen(loadRegScaled{regT=X1, regN=X_MLStackPtr, unitOffset=0}, cvec);
                                    gen(storeRegScaled{regT=X0, regN=X1, unitOffset=destOffset}, cvec);
                                    topInX0 := false; (* We've used it. *)
                                    setValues(tl, sourceOffset+1, destOffset+1)
                                )
                                else setValues(tl, sourceOffset+1, destOffset)
                        in
                            setValues(cl, 0, 0)
                            (* The container address is still on the stack. *)
                        end

                    |   _ =>
                        let (* General case: copy values from the source tuple. *)
                            (* First the target tuple, then the container. *)
                            val () = gencde (tuple, ToStack, NotEnd, loopAddr)
                            val () = gencde (container, ToX0, NotEnd, loopAddr)
                            val () = genPopReg(X1, cvec)
                            val () = decsp()
                            (* Container address is in X0, tuple in X1. *)
                            
                            val last = BoolVector.foldli(fn (i, true, _) => i | (_, false, n) => n) ~1 filter

                            fun copy (sourceOffset, destOffset) =
                                if BoolVector.sub(filter, sourceOffset)
                                then
                                (
                                    (* Load the value in the tuple. *)
                                    gen(loadRegScaled{regT=X2, regN=X1, unitOffset=sourceOffset}, cvec);
                                    (* Store into the container. *)
                                    gen(storeRegScaled{regT=X2, regN=X0, unitOffset=destOffset}, cvec);
                                    if sourceOffset = last
                                    then ()
                                    else copy (sourceOffset+1, destOffset+1)
                                )
                                else copy(sourceOffset+1, destOffset)
                        in
                            copy (0, 0);
                            topInX0 := true (* Container address is in X0 *)
                        end
                )

            |   BICTagTest { test, tag=tagValue, ... } =>
                (
                    gencde (test, ToStack, NotEnd, loopAddr);
                    genPopReg(X0, cvec);
                    gen(subSImmediate{regN=X0, regD=XZero, immed=taggedWord tagValue, shifted=false}, cvec);
                    setBooleanCondition(X0, condEqual, cvec);
                    genPushReg(X0, cvec)
                )

            |   BICNullary {oper=BuiltIns.GetCurrentThreadId} =>
                (
                    gen(loadRegScaled{regT=X0, regN=X_MLAssemblyInt, unitOffset=threadIdOffset}, cvec);
                    genPushReg(X0, cvec);
                    incsp()
                )

            |   BICNullary {oper=BuiltIns.CheckRTSException} =>
                (* Raise an exception in ML if the last RTS call set the exception packet. *)
                let (* It may be better to do this in all RTS calls. *)
                    val noException = createLabel()
                in
                    (* Load the packet and see if it is nil (tagged 0) *)
                    gen(loadRegScaled{regT=X0, regN=X_MLAssemblyInt, unitOffset=exceptionPacketOffset}, cvec);
                    gen(subSImmediate{regN=X0, regD=XZero, immed=taggedWord 0w0, shifted=false}, cvec);
                    gen(conditionalBranch(condEqual, noException), cvec);
                    (* If it isn't then raise the exception. *)
                    gen(loadRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset}, cvec);
                    gen(loadRegScaled{regT=X1, regN=X_MLStackPtr, unitOffset=0}, cvec);
                    gen(branchRegister X1, cvec);
                    gen(setLabel noException, cvec)
                end

            |   BICNullary {oper=BuiltIns.CPUPause} =>
                (
                    genOpcode(cpuPause, cvec)
                )

            |   BICUnary { oper, arg1 } =>
                let
                    open BuiltIns
                    val () = gencde (arg1, ToX0, NotEnd, loopAddr)
                in
                    case oper of
                        NotBoolean =>
                            (* Flip true to false and the reverse. *)
                            gen(bitwiseXorImmediate{wordSize=WordSize32, bits=0w2, regN=X0, regD=X0}, cvec)

                    |   IsTaggedValue =>
                        (
                            gen(testBitPattern(X0, 0w1), cvec);
                            setBooleanCondition(X0, condNotEqual (*Non-zero*), cvec)
                        )

                    |   MemoryCellLength =>
                        (
                            (* Load the length word. *)
                            gen(loadRegUnscaled{regT=X0, regN=X0, byteOffset= ~8}, cvec);
                            (* Extract the length, excluding the flag bytes and shift by one bit. *)
                            gen(unsignedBitfieldInsertinZeros
                                {wordSize=WordSize64, lsb=0w1, width=0w56, regN=X0, regD=X0}, cvec);
                            (* Set the tag bit. *)
                            gen(bitwiseOrImmediate{wordSize=WordSize64, bits=0w1, regN=X0, regD=X0}, cvec)
                        )

                    |   MemoryCellFlags =>
                        (
                            (* Load the flags byte. *)
                            gen(loadRegUnscaledByte{regT=X0, regN=X0, byteOffset= ~1}, cvec);
                            (* Tag the result. *)
                            gen(logicalShiftLeft{wordSize=WordSize64, shift=0w1, regN=X0, regD=X0}, cvec);
                            gen(bitwiseOrImmediate{wordSize=WordSize64, bits=0w1, regN=X0, regD=X0}, cvec)
                        )

                    |   ClearMutableFlag =>
                        (
                            gen(loadRegUnscaledByte{regT=X1, regN=X0, byteOffset= ~1}, cvec);
                            gen(bitwiseAndImmediate{wordSize=WordSize32, bits=Word64.xorb(0wxffffffff, 0wx40), regN=X1, regD=X1}, cvec);
                            gen(storeRegUnscaledByte{regT=X1, regN=X0, byteOffset= ~1}, cvec)
                        )

                    |   AtomicReset => genOpcode(opcode_atomicReset, cvec)
                    |   LongWordToTagged => genOpcode(opcode_longWToTagged, cvec)
                    |   SignedToLongWord => genOpcode(opcode_signedToLongW, cvec)
                    |   UnsignedToLongWord => genOpcode(opcode_unsignedToLongW, cvec)
                    |   RealAbs PrecDouble => genOpcode(opcode_realAbs, cvec)
                    |   RealNeg PrecDouble => genOpcode(opcode_realNeg, cvec)
                    |   RealFixedInt PrecDouble => genOpcode(opcode_fixedIntToReal, cvec)
                    |   RealAbs PrecSingle => genOpcode(opcode_floatAbs, cvec)
                    |   RealNeg PrecSingle => genOpcode(opcode_floatNeg, cvec)
                    |   RealFixedInt PrecSingle => genOpcode(opcode_fixedIntToFloat, cvec)
                    |   FloatToDouble => genOpcode(opcode_floatToReal, cvec)
                    |   DoubleToFloat rnding => genDoubleToFloat(rnding, cvec)
                    |   RealToInt (PrecDouble, rnding) => genRealToInt(rnding, cvec)
                    |   RealToInt (PrecSingle, rnding) => genFloatToInt(rnding, cvec)
                    |   TouchAddress => topInX0 := false (* Discard this *)
                    |   AllocCStack => genOpcode(opcode_allocCSpace, cvec)
                end

            |   BICBinary { oper, arg1, arg2 } =>
                let
                    open BuiltIns
                    (* Generate the first argument to the stack and the second to X0. *)
                    val () = gencde (arg1, ToStack, NotEnd, loopAddr)
                    val () = gencde (arg2, ToX0, NotEnd, loopAddr)
                    
                    fun compareWords cond =
                    (
                        genPopReg(X1, cvec); (* First argument. *)
                        compareRegs(X1, X0, cvec);
                        setBooleanCondition(X0, cond, cvec)
                    )
                in
                    case oper of
                        WordComparison{test=TestEqual, ...} => compareWords condEqual
                    |   WordComparison{test=TestLess, isSigned=true} => compareWords condSignedLess
                    |   WordComparison{test=TestLessEqual, isSigned=true} => compareWords condSignedLessEq
                    |   WordComparison{test=TestGreater, isSigned=true} => compareWords condSignedGreater
                    |   WordComparison{test=TestGreaterEqual, isSigned=true} => compareWords condSignedGreaterEq
                    |   WordComparison{test=TestLess, isSigned=false} => compareWords condCarryClear
                    |   WordComparison{test=TestLessEqual, isSigned=false} => compareWords condUnsignedLowOrEq
                    |   WordComparison{test=TestGreater, isSigned=false} => compareWords condUnsignedHigher
                    |   WordComparison{test=TestGreaterEqual, isSigned=false} => compareWords condCarrySet
                    |   WordComparison{test=TestUnordered, ...} => raise InternalError "WordComparison: TestUnordered"

                    |   PointerEq => compareWords condEqual

                    |   FixedPrecisionArith ArithAdd =>
                        (
                            (* Subtract the tag bit. *)
                            gen(subImmediate{regN=X0, regD=X0, immed=0w1, shifted=false}, cvec);
                            genPopReg(X1, cvec);
                            (* Add and set the flag bits *)
                            gen(addSShiftedReg{regN=X1, regM=X0, regD=X0, shift=ShiftNone}, cvec);
                            checkOverflow cvec
                        )
                    |   FixedPrecisionArith ArithSub =>
                        (
                            (* Subtract the tag bit. *)
                            gen(subImmediate{regN=X0, regD=X0, immed=0w1, shifted=false}, cvec);
                            genPopReg(X1, cvec);
                            (* Subtract and set the flag bits *)
                            gen(subSShiftedReg{regN=X1, regM=X0, regD=X0, shift=ShiftNone}, cvec);
                            checkOverflow cvec
                        )
                    |   FixedPrecisionArith ArithMult =>
                        let
                            (* There's no simple way of detecting overflow.  We have to compute the
                               high-order word and then check that it is either all zeros with
                               the sign bit zero or all ones with the sign bit one. *)
                            val noOverflow = createLabel()
                        in
                            (* Compute the result in the same way as for Word.* apart from the
                               arithmetic shift. *)
                            genPopReg(X1, cvec);
                            (* Shift to remove the tags on one argument suing . *)
                            gen(arithmeticShiftRight{regN=X0, regD=X2, wordSize=WordSize64, shift=0w1}, cvec);
                            (* Remove the tag on the other. *)
                            gen(bitwiseAndImmediate{regN=X1, regD=X1, wordSize=WordSize64, bits=tagBitMask}, cvec);
                            gen(multiplyAndAdd{regM=X1, regN=X2, regA=XZero, regD=X0}, cvec);
                            (* Put back the tag. *)
                            gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec);
                            (* Compute the high order part into X2 *)
                            gen(signedMultiplyHigh{regM=X1, regN=X2, regD=X2}, cvec);
                            (* Compare with the sign bit of the result. *)
                            gen(subSShiftedReg{regD=XZero, regN=X2, regM=X0, shift=ShiftASR 0w63}, cvec);
                            gen(conditionalBranch(condEqual, noOverflow), cvec);
                            gen(loadAddressConstant(X0, toMachineWord Overflow), cvec);
                            gen(loadRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset}, cvec);
                            gen(loadRegScaled{regT=X1, regN=X_MLStackPtr, unitOffset=0}, cvec);
                            gen(branchRegister X1, cvec);
                            gen(setLabel noOverflow, cvec)
                        end
                    |   FixedPrecisionArith ArithQuot => genOpcode(opcode_fixedQuot, cvec)
                    |   FixedPrecisionArith ArithRem => genOpcode(opcode_fixedRem, cvec)
                    |   FixedPrecisionArith ArithDiv => raise InternalError "TODO: FixedPrecisionArith ArithDiv"
                    |   FixedPrecisionArith ArithMod => raise InternalError "TODO: FixedPrecisionArith ArithMod"

                    |   WordArith ArithAdd =>
                        (
                            (* Subtract the tag bit. *)
                            gen(subImmediate{regN=X0, regD=X0, immed=0w1, shifted=false}, cvec);
                            genPopReg(X1, cvec);
                            gen(addShiftedReg{regN=X1, regM=X0, regD=X0, shift=ShiftNone}, cvec)
                        )
                    |   WordArith ArithSub =>
                        (
                            (* Subtract the tag bit. *)
                            gen(subImmediate{regN=X0, regD=X0, immed=0w1, shifted=false}, cvec);
                            genPopReg(X1, cvec);
                            gen(subShiftedReg{regN=X1, regM=X0, regD=X0, shift=ShiftNone}, cvec)
                        )
                    |   WordArith ArithMult =>
                        (
                            genPopReg(X1, cvec);
                            (* Shift to remove the tags on one argument. *)
                            gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize64, shift=0w1}, cvec);
                            (* Remove the tag on the other. *)
                            gen(bitwiseAndImmediate{regN=X1, regD=X1, wordSize=WordSize64, bits=tagBitMask}, cvec);
                            gen(multiplyAndAdd{regM=X1, regN=X0, regA=XZero, regD=X0}, cvec);
                            (* Put back the tag. *)
                            gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
                        )
                    |   WordArith ArithDiv =>
                        (
                            genPopReg(X1, cvec);
                            (* Shift to remove the tag on the divisor *)
                            gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize64, shift=0w1}, cvec);
                            (* Untag but don't shift the dividend. *)
                            gen(bitwiseAndImmediate{regN=X1, regD=X1, wordSize=WordSize64, bits=tagBitMask}, cvec);
                            gen(unsignedDivide{regM=X0, regN=X1, regD=X0}, cvec);
                            (* Restore the tag: Note: it may already be set depending on the result of
                               the division. *)
                            gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
                        )
                    |   WordArith ArithMod =>
                        (
                            (* There's no direct way to get the remainder - have to use divide and multiply. *)
                            genPopReg(X1, cvec);
                            (* Shift to remove the tag on the divisor *)
                            gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize64, shift=0w1}, cvec);
                            (* Untag but don't shift the dividend. *)
                            gen(bitwiseAndImmediate{regN=X1, regD=X2, wordSize=WordSize64, bits=tagBitMask}, cvec);
                            gen(unsignedDivide{regM=X0, regN=X2, regD=X2}, cvec);
                            (* Clear the bottom bit before the multiplication. *)
                            gen(bitwiseAndImmediate{regN=X2, regD=X2, wordSize=WordSize64, bits=tagBitMask}, cvec);
                            (* X0 = X1 - (X2/X0)*X0 *)
                            gen(multiplyAndSub{regM=X2, regN=X0, regA=X1, regD=X0}, cvec)
                            (* Because we're subtracting from the original, tagged, dividend
                               the result is tagged. *)
                        )
                    |   WordArith _ => raise InternalError "WordArith - unimplemented instruction"
                
                    |   WordLogical LogicalAnd =>
                        (
                            genPopReg(X1, cvec);
                            (* Since they're both tagged the tag bit is preserved. *)
                            gen(andShiftedReg{regN=X1, regM=X0, regD=X0, shift=ShiftNone}, cvec)
                        )
                    |   WordLogical LogicalOr =>
                        (
                            genPopReg(X1, cvec);
                            (* Since they're both tagged the tag bit is preserved. *)
                            gen(orrShiftedReg{regN=X1, regM=X0, regD=X0, shift=ShiftNone}, cvec)
                        )
                    |   WordLogical LogicalXor =>
                        (
                            genPopReg(X1, cvec);
                            (* Have to restore the tag bit because that will be cleared. *)
                            gen(eorShiftedReg{regN=X1, regM=X0, regD=X0, shift=ShiftNone}, cvec);
                            gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
                        )

                        (* Shifts: ARM64 shifts are taken modulo the word length but that's
                           dealt with at a higher level. *)
                    |   WordShift ShiftLeft =>
                        (
                            genPopReg(X1, cvec);
                            (* Remove the tag from value we're shifting. *)
                            gen(bitwiseAndImmediate{regN=X1, regD=X1, wordSize=WordSize64, bits=tagBitMask}, cvec);
                            (* Untag the shift amount.  Can use 32-bit op here. *)
                            gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize32, shift=0w1}, cvec);
                            gen(logicalShiftLeftVariable{regM=X0, regN=X1, regD=X0}, cvec);
                            (* Put back the tag. *)
                            gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
                        )
                    |   WordShift ShiftRightLogical =>
                        (
                            genPopReg(X1, cvec);
                            (* Don't need to remove the tag. *)
                            (* Untag the shift amount.  Can use 32-bit op here. *)
                            gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize32, shift=0w1}, cvec);
                            gen(logicalShiftRightVariable{regM=X0, regN=X1, regD=X0}, cvec);
                            (* Put back the tag. *)
                            gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
                        )
                    |   WordShift ShiftRightArithmetic =>
                        (
                            genPopReg(X1, cvec);
                            (* Don't need to remove the tag. *)
                            (* Untag the shift amount.  Can use 32-bit op here. *)
                            gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize32, shift=0w1}, cvec);
                            gen(arithmeticShiftRightVariable{regM=X0, regN=X1, regD=X0}, cvec);
                            (* Put back the tag. *)
                            gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
                        )
                 
                    |   AllocateByteMemory =>
                        (* Allocate memory for byte data.  Unlike for word data it is not necessary to
                           initialise it before any further allocation provided it has the mutable bit
                           set. *)
                        (
                            (* Load and untag the size and flags.  The size is the number of words even
                               though this is byte data. *)
                            gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize32 (*byte*), shift=0w1}, cvec);
                            genPopReg(X1, cvec);
                            gen(logicalShiftRight{regN=X1, regD=X1, wordSize=WordSize64, shift=0w1}, cvec);
                            allocateVariableSize({sizeReg=X1, flagsReg=X0, resultReg=X2}, cvec);
                            gen(genMoveRegToReg{sReg=X2, dReg=X0}, cvec)
                        )
                
                    |   LargeWordComparison TestEqual => genOpcode(opcode_lgWordEqual, cvec)
                    |   LargeWordComparison TestLess => genOpcode(opcode_lgWordLess, cvec)
                    |   LargeWordComparison TestLessEqual => genOpcode(opcode_lgWordLessEq, cvec)
                    |   LargeWordComparison TestGreater => genOpcode(opcode_lgWordGreater, cvec)
                    |   LargeWordComparison TestGreaterEqual => genOpcode(opcode_lgWordGreaterEq, cvec)
                    |   LargeWordComparison TestUnordered => raise InternalError "LargeWordComparison: TestUnordered"
                
                    |   LargeWordArith ArithAdd => genOpcode(opcode_lgWordAdd, cvec)
                    |   LargeWordArith ArithSub => genOpcode(opcode_lgWordSub, cvec)
                    |   LargeWordArith ArithMult => genOpcode(opcode_lgWordMult, cvec)
                    |   LargeWordArith ArithDiv => genOpcode(opcode_lgWordDiv, cvec)
                    |   LargeWordArith ArithMod => genOpcode(opcode_lgWordMod, cvec)
                    |   LargeWordArith _ => raise InternalError "LargeWordArith - unimplemented instruction"

                    |   LargeWordLogical LogicalAnd => genOpcode(opcode_lgWordAnd, cvec)
                    |   LargeWordLogical LogicalOr => genOpcode(opcode_lgWordOr, cvec)
                    |   LargeWordLogical LogicalXor => genOpcode(opcode_lgWordXor, cvec)
                    |   LargeWordShift ShiftLeft => genOpcode(opcode_lgWordShiftLeft, cvec)
                    |   LargeWordShift ShiftRightLogical => genOpcode(opcode_lgWordShiftRLog, cvec)
                    |   LargeWordShift ShiftRightArithmetic => genOpcode(opcode_lgWordShiftRArith, cvec)

                    |   RealComparison (TestEqual, PrecDouble) => genOpcode(opcode_realEqual, cvec)
                    |   RealComparison (TestLess, PrecDouble) => genOpcode(opcode_realLess, cvec)
                    |   RealComparison (TestLessEqual, PrecDouble) => genOpcode(opcode_realLessEq, cvec)
                    |   RealComparison (TestGreater, PrecDouble) => genOpcode(opcode_realGreater, cvec)
                    |   RealComparison (TestGreaterEqual, PrecDouble) => genOpcode(opcode_realGreaterEq, cvec)
                    |   RealComparison (TestUnordered, PrecDouble) => genOpcode(opcode_realUnordered, cvec)

                    |   RealComparison (TestEqual, PrecSingle) => genOpcode(opcode_floatEqual, cvec)
                    |   RealComparison (TestLess, PrecSingle) => genOpcode(opcode_floatLess, cvec)
                    |   RealComparison (TestLessEqual, PrecSingle) => genOpcode(opcode_floatLessEq, cvec)
                    |   RealComparison (TestGreater, PrecSingle) => genOpcode(opcode_floatGreater, cvec)
                    |   RealComparison (TestGreaterEqual, PrecSingle) => genOpcode(opcode_floatGreaterEq, cvec)
                    |   RealComparison (TestUnordered, PrecSingle) => genOpcode(opcode_floatUnordered, cvec)

                    |   RealArith (ArithAdd, PrecDouble) => genOpcode(opcode_realAdd, cvec)
                    |   RealArith (ArithSub, PrecDouble) => genOpcode(opcode_realSub, cvec)
                    |   RealArith (ArithMult, PrecDouble) => genOpcode(opcode_realMult, cvec)
                    |   RealArith (ArithDiv, PrecDouble) => genOpcode(opcode_realDiv, cvec)

                    |   RealArith (ArithAdd, PrecSingle) => genOpcode(opcode_floatAdd, cvec)
                    |   RealArith (ArithSub, PrecSingle) => genOpcode(opcode_floatSub, cvec)
                    |   RealArith (ArithMult, PrecSingle) => genOpcode(opcode_floatMult, cvec)
                    |   RealArith (ArithDiv, PrecSingle) => genOpcode(opcode_floatDiv, cvec)

                    |   RealArith _ => raise InternalError "RealArith - unimplemented instruction"
                
                    |   FreeCStack => genOpcode(opcode_freeCSpace, cvec)
                
                    |   AtomicExchangeAdd => genOpcode(opcode_atomicExchAdd, cvec)
                     ;
                    decsp() (* Removes one item from the stack. *)
                end
            
            |   BICAllocateWordMemory {numWords, flags, initial } =>
                let
                    fun doAllocateAndInit() =
                    let
                        val () = gencde (numWords, ToStack, NotEnd, loopAddr)
                        val () = gencde (flags, ToStack, NotEnd, loopAddr)
                        val () = gencde (initial, ToX0, NotEnd, loopAddr)
                        val exitLabel = createLabel() and loopLabel = createLabel()
                    in
                        genPopReg(X2, cvec); (* Flags as tagged value. *)
                        gen(logicalShiftRight{regN=X2, regD=X2, wordSize=WordSize32 (*byte*), shift=0w1}, cvec);
                        genPopReg(X1, cvec); (* Length as tagged value. *)
                        gen(logicalShiftRight{regN=X1, regD=X1, wordSize=WordSize64, shift=0w1}, cvec);
                        genPushReg(X0, cvec); (* Save initialiser - TODO: Add it to save set. *)
                        allocateVariableSize({sizeReg=X1, flagsReg=X2, resultReg=X0}, cvec);
                        genPopReg(X3, cvec); (* Pop initialiser. *)
                        (* Add the length in bytes so we point at the end. *)
                        gen(addShiftedReg{regM=X1, regN=X0, regD=X1, shift=ShiftLSL 0w3}, cvec);
                        (* Loop to initialise. *)
                        gen(setLabel loopLabel, cvec);
                        compareRegs(X1, X0, cvec); (* Are we at the start? *)
                        gen(conditionalBranch(condEqual, exitLabel), cvec);
                        gen(storeRegPreIndex{regT=X3, regN=X1, byteOffset= ~8}, cvec);
                        gen(conditionalBranch(condAlways, loopLabel), cvec);
                        gen(setLabel exitLabel, cvec);
                        decsp(); decsp()
                    end
                in
                    case (numWords, flags) of
                        (BICConstnt(length, _), BICConstnt(flagValue, _)) =>
                            if isShort length andalso toShort length = 0w1 andalso isShort flagValue
                            then (* This is a very common case for refs. *)
                            let
                                val flagByte = Word8.fromLargeWord(Word.toLargeWord(toShort flagValue))
                            in
                                gencde (initial, ToStack, NotEnd, loopAddr); (* Initialiser. *)
                                genAllocateFixedSize(1, flagByte, cvec);
                                genPopReg(X1, cvec);
                                gen(storeRegScaled{regT=X1, regN=X0, unitOffset=0}, cvec);
                                decsp(); topInX0 := true
                            end
                            else (* Constant but not a single. *) doAllocateAndInit()
                    |   _ => (* Not constant. *) doAllocateAndInit()
                end

            |   BICLoadOperation { kind=LoadStoreMLWord _, address} =>
                (
                    case genMLLoadAddress(address, Word.toInt wordSize) of
                        (base, MLLoadOffset offset) =>
                            gen(loadRegScaled{regT=X0, regN=base, unitOffset=offset}, cvec)
                    |   (base, MLLoadReg indexR) =>
                            gen(loadRegIndexed{regN=base, regM=indexR, regT=X0, option=ExtUXTX ScaleOrShift}, cvec)
                )

            |   BICLoadOperation { kind=LoadStoreMLByte _, address} =>
                (
                    case genMLLoadAddress(address, 1) of
                        (base, MLLoadOffset offset) =>
                            gen(loadRegScaledByte{regT=X0, regN=base, unitOffset=offset}, cvec)
                    |   (base, MLLoadReg indexR) =>
                            gen(loadRegIndexedByte{regN=base, regM=indexR, regT=X0, option=ExtUXTX NoScale}, cvec);

                    (* Have to tag the result. *)
                    gen(logicalShiftLeft{regN=X0, regD=X0, wordSize=WordSize32, shift=0w1}, cvec);
                    gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize32, bits=0w1}, cvec)
                )

            |   BICLoadOperation { kind=LoadStoreC8, address} =>
                (
                    genCAddress address;
                    genOpcode(opcode_loadC8, cvec);
                    decsp(); decsp()
                )

            |   BICLoadOperation { kind=LoadStoreC16, address} =>
                (
                    genCAddress address;
                    genOpcode(opcode_loadC16, cvec);
                    decsp(); decsp()
                )

            |   BICLoadOperation { kind=LoadStoreC32, address} =>
                (
                    genCAddress address;
                    genOpcode(opcode_loadC32, cvec);
                    decsp(); decsp()
                )

            |   BICLoadOperation { kind=LoadStoreC64, address} =>
                (
                    genCAddress address;
                    genOpcode(opcode_loadC64, cvec);
                    decsp(); decsp()
                )

            |   BICLoadOperation { kind=LoadStoreCFloat, address} =>
                (
                    genCAddress address;
                    genOpcode(opcode_loadCFloat, cvec);
                    decsp(); decsp()
                )

            |   BICLoadOperation { kind=LoadStoreCDouble, address} =>
                (
                    genCAddress address;
                    genOpcode(opcode_loadCDouble, cvec);
                    decsp(); decsp()
                )

            |   BICLoadOperation { kind=LoadStoreUntaggedUnsigned, address} =>
                (
                    case genMLLoadAddress(address, Word.toInt wordSize) of
                        (base, MLLoadOffset offset) =>
                            gen(loadRegScaled{regT=X0, regN=base, unitOffset=offset}, cvec)
                    |   (base, MLLoadReg indexR) =>
                            gen(loadRegIndexed{regN=base, regM=indexR, regT=X0, option=ExtUXTX ScaleOrShift}, cvec);

                    (* Have to tag the result. *)
                    gen(logicalShiftLeft{regN=X0, regD=X0, wordSize=WordSize64, shift=0w1}, cvec);
                    gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
                )

            |   BICStoreOperation { kind=LoadStoreMLWord _, address, value } =>
                (
                    genMLAddress(address, Word.toInt wordSize);
                    gencde (value, ToStack, NotEnd, loopAddr);
                    genPopReg(X0, cvec); (* Value to store *)
                    genPopReg(X1, cvec); (* Index: a tagged value. *)
                    (* Shift right to remove the tag.  N.B.  Indexes into ML memory are
                       unsigned.  Unlike on the X86 we can't remove the tag by providing
                       a displacement and the only options are to scale by either 1 or 8. *)
                    gen(logicalShiftRight{regN=X1, regD=X1, wordSize=WordSize64, shift=0w1}, cvec);
                    genPopReg(X2, cvec); (* Base address. *)
                    gen(storeRegIndexed{regN=X2, regM=X1, regT=X0, option=ExtUXTX ScaleOrShift}, cvec);
                    (* Don't put the unit result in; it probably isn't needed, *)
                    decsp(); decsp(); decsp()
                )

            |   BICStoreOperation { kind=LoadStoreMLByte _, address, value } =>
                (
                    (* Untag the value and store the byte. *)
                    genMLAddress(address, 1);
                    gencde (value, ToStack, NotEnd, loopAddr);
                    genPopReg(X0, cvec); (* Value to store *)
                    gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize64, shift=0w1}, cvec); (* Untag it *)
                    genPopReg(X1, cvec); (* Index: a tagged value. *)
                    (* Shift right to remove the tag.  N.B.  Indexes into ML memory are
                       unsigned.  Unlike on the X86 we can't remove the tag by providing
                       a displacement and the only options are to scale by either 1 or 8. *)
                    gen(logicalShiftRight{regN=X1, regD=X1, wordSize=WordSize64, shift=0w1}, cvec);
                    genPopReg(X2, cvec); (* Base address. *)
                    gen(storeRegIndexedByte{regN=X2, regM=X1, regT=X0, option=ExtUXTX NoScale}, cvec);
                    (* Don't put the unit result in; it probably isn't needed, *)
                    decsp(); decsp(); decsp()
                )

            |   BICStoreOperation { kind=LoadStoreC8, address, value} =>
                (
                    genCAddress address;
                    gencde (value, ToStack, NotEnd, loopAddr);
                    genOpcode(opcode_storeC8, cvec);
                    decsp(); decsp(); decsp()
                )

            |   BICStoreOperation { kind=LoadStoreC16, address, value} =>
                (
                    genCAddress address;
                    gencde (value, ToStack, NotEnd, loopAddr);
                    genOpcode(opcode_storeC16, cvec);
                    decsp(); decsp(); decsp()
                )

            |   BICStoreOperation { kind=LoadStoreC32, address, value} =>
                (
                    genCAddress address;
                    gencde (value, ToStack, NotEnd, loopAddr);
                    genOpcode(opcode_storeC32, cvec);
                    decsp(); decsp(); decsp()
                )

            |   BICStoreOperation { kind=LoadStoreC64, address, value} =>
                (
                    genCAddress address;
                    gencde (value, ToStack, NotEnd, loopAddr);
                    genOpcode(opcode_storeC64, cvec);
                    decsp(); decsp(); decsp()
                )

            |   BICStoreOperation { kind=LoadStoreCFloat, address, value} =>
                (
                    genCAddress address;
                    gencde (value, ToStack, NotEnd, loopAddr);
                    genOpcode(opcode_storeCFloat, cvec);
                    decsp(); decsp(); decsp()
                )

            |   BICStoreOperation { kind=LoadStoreCDouble, address, value} =>
                (
                    genCAddress address;
                    gencde (value, ToStack, NotEnd, loopAddr);
                    genOpcode(opcode_storeCDouble, cvec);
                    decsp(); decsp(); decsp()
                )

            |   BICStoreOperation { kind=LoadStoreUntaggedUnsigned, address, value} =>
                (
                    (* Almost the same as LoadStoreMLWord except that the value to be stored
                       must be untagged before it is stored.  This is used primarily to set
                       the length word on a string. *)
                    genMLAddress(address, Word.toInt wordSize);
                    gencde (value, ToStack, NotEnd, loopAddr);
                    genPopReg(X0, cvec); (* Value to store *)
                    gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize64, shift=0w1}, cvec);
                    genPopReg(X1, cvec); (* Index: a tagged value. *)
                    (* Shift right to remove the tag.  N.B.  Indexes into ML memory are
                       unsigned.  Unlike on the X86 we can't remove the tag by providing
                       a displacement and the only options are to scale by either 1 or 8. *)
                    gen(logicalShiftRight{regN=X1, regD=X1, wordSize=WordSize64, shift=0w1}, cvec);
                    genPopReg(X2, cvec); (* Base address. *)
                    gen(storeRegIndexed{regN=X2, regM=X1, regT=X0, option=ExtUXTX ScaleOrShift}, cvec);
                    (* Don't put the unit result in; it probably isn't needed, *)
                    decsp(); decsp(); decsp()
                )

            |   BICBlockOperation { kind=BlockOpMove{isByteMove}, sourceLeft, destRight, length } =>
                let
                    val exitLabel = createLabel() and loopLabel = createLabel()
                in
                    genMLAddress(sourceLeft, 1);
                    genMLAddress(destRight, 1);
                    gencde (length, ToX0, NotEnd, loopAddr); (* Length *)
                    genPopReg(X2, cvec); (* Dest index - tagged value. *)
                    genPopReg(X1, cvec); (* Dest base address. *)
                    (* Add in the index N.B. ML index values are unsigned. *)
                    gen(addShiftedReg{regM=X2, regN=X1, regD=X1, shift=ShiftLSR 0w1}, cvec);
                    genPopReg(X3, cvec); (* Source index *)
                    genPopReg(X2, cvec);
                    gen(addShiftedReg{regM=X3, regN=X2, regD=X2, shift=ShiftLSR 0w1}, cvec);
                    (* Untag the length *)
                    gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize64, shift=0w1}, cvec);
                    (* Test the loop value at the top in case it's already zero. *)
                    compareRegs(X0, X0, cvec); (* Set condition code just in case. *)
                    gen(setLabel loopLabel, cvec);
                    gen(compareBranchZero(X0, WordSize64, exitLabel), cvec);
                    if isByteMove
                    then
                    (
                        gen(loadRegPostIndexByte{regT=X3, regN=X2, byteOffset=1}, cvec);
                        gen(storeRegPostIndexByte{regT=X3, regN=X1, byteOffset=1}, cvec)
                    )
                    else
                    (
                        gen(loadRegPostIndex{regT=X3, regN=X2, byteOffset=8}, cvec);
                        gen(storeRegPostIndex{regT=X3, regN=X1, byteOffset=8}, cvec)
                    );
                    gen(subImmediate{regN=X0, regD=X0, immed=0w1, shifted=false}, cvec);
                    (* Back to the start. *)
                    gen(conditionalBranch(condAlways, loopLabel), cvec);
                    gen(setLabel exitLabel, cvec);
                    topInX0 := false; (* X0 does not contain "unit" *)
                    decsp(); decsp(); decsp(); decsp()
                end

            |   BICBlockOperation { kind=BlockOpEqualByte, sourceLeft, destRight, length } =>
                (* Compare byte vectors for equality - returns a boolean result. *)
                let
                    val equalLabel = createLabel()
                in
                    blockCompareBytes(sourceLeft, destRight, length, equalLabel, true);
                    gen(setLabel equalLabel, cvec);
                    (* Set the result condition. *)
                    setBooleanCondition(X0, condEqual, cvec)
                end

            |   BICBlockOperation { kind=BlockOpCompareByte, sourceLeft, destRight, length } =>
                (* Compare byte vectors for ordering - return tagged -1, 0, +1. *)
                let
                    val equalLabel = createLabel() and resultLabel = createLabel()
                in
                    blockCompareBytes(sourceLeft, destRight, length, equalLabel, false);
                    (* We drop through if we have found unequal bytes. *)
                    gen(loadNonAddressConstant(X0, Word64.fromInt(tag 1)), cvec);
                    (* Set X0 to either 1 or -1 depending on whether it's greater or less. *)
                    gen(conditionalSetInverted{regD=X0, regTrue=X0, regFalse=XZero, cond=condUnsignedHigher}, cvec);
                    gen(conditionalBranch(condAlways, resultLabel), cvec);
                    gen(setLabel equalLabel, cvec);
                    (* Equal case - set it to zero. *)
                    gen(loadNonAddressConstant(X0, Word64.fromInt(tag 0)), cvec);
                    gen(setLabel resultLabel, cvec)
                end
       
           |    BICArbitrary { oper, arg1, arg2, ... } =>
                let
                    open BuiltIns
                    val () = gencde (arg1, ToStack, NotEnd, loopAddr)
                    val () = gencde (arg2, ToStack, NotEnd, loopAddr)
                in
                    case oper of
                        ArithAdd  => genOpcode(opcode_arbAdd, cvec)
                    |   ArithSub  => genOpcode(opcode_arbSubtract, cvec)
                    |   ArithMult => genOpcode(opcode_arbMultiply, cvec)
                    |   _ => raise InternalError "Unknown arbitrary precision operation";
                    decsp() (* Removes one item from the stack. *)
                end

        in (* body of gencde *) 

          (* This ensures that there is precisely one item on the stack if
             whereto = ToStack and no items if whereto = NoResult. *)
            case whereto of
                ToStack =>
                let
                    val () = ensureX0()
                    val newsp = oldsp + 1;
                    val adjustment = !realstackptr - newsp

                    val () =
                        if adjustment = 0
                        then ()
                        else if adjustment < ~1
                        then raise InternalError ("gencde: bad adjustment " ^ Int.toString adjustment)
                        (* Hack for declarations that should push values, but don't *)
                        else if adjustment = ~1
                        then
                        (
                            gen(loadNonAddressConstant(X0, Word64.fromInt(tag 0)), cvec);
                            genPushReg(X0, cvec)
                        )
                        else resetStack (adjustment, true, cvec)
                in
                    realstackptr := newsp
                end
          
            |   NoResult =>
                let
                    val adjustment = !realstackptr - oldsp

                    val () =
                        if adjustment = 0
                        then ()
                        else if adjustment < 0
                        then raise InternalError ("gencde: bad adjustment " ^ Int.toString adjustment)
                        else resetStack (adjustment, false, cvec)
                in
                    realstackptr := oldsp
                end

            |   ToX0 =>
                let
                    (* If we have not pushed anything we have to push a unit result. *)
                    val () =
                        if !topInX0 then ()
                        else if !realstackptr = oldsp
                        then gen(loadNonAddressConstant(X0, Word64.fromInt(tag 0)), cvec)
                        else
                        (
                            genPopReg(X0, cvec);
                            decsp()
                        )
                    val () = topInX0 := true

                    val adjustment = !realstackptr - oldsp

                    val () =
                        if adjustment = 0
                        then ()
                        else if adjustment < 0
                        then raise InternalError ("gencde: bad adjustment " ^ Int.toString adjustment)
                        else resetStack (adjustment, false, cvec)
                in
                    realstackptr := oldsp
                end
        end (* gencde *)

       (* doNext is only used for mutually recursive functions where a
         function may not be able to fill in its closure if it does not have
         all the remaining declarations. *)
        (* TODO: This always creates the closure on the heap even when makeClosure is false. *) 
       and genProc ({ closure=[], localCount, body, argTypes, name, ...}: bicLambdaForm, mutualDecs, doNext: unit -> unit) : unit =
            let
                (* Create a one word item for the closure.  This is returned for recursive references
                   and filled in with the address of the code when we've finished. *)
                val closure = makeConstantClosure()
                (* Code-gen function. No non-local references. *)
                val () = codegen (body, name, closure, List.length argTypes, localCount, parameters);
                val () = gen(loadAddressConstant(X0, closureAsAddress closure), cvec)
                val () = genPushReg(X0, cvec)
                val () = incsp();
            in
                if mutualDecs then doNext () else ()
            end

        |   genProc ({ localCount, body, name, argTypes, closure, ...}, mutualDecs, doNext) =
            let (* Full closure required. *)
                val resClosure = makeConstantClosure()
                (* Code-gen function. *)
                val () = codegen (body, name, resClosure, List.length argTypes, localCount, parameters)
                (* Since we're using native words rather than 32-in-64 we can load this now. *)
                val codeAddr = codeAddressFromClosure resClosure
                val closureVars = List.length closure (* Size excluding the code address *)
            in
                if mutualDecs
                then
                let (* Have to make the closure now and fill it in later. *)
                    val () = genAllocateFixedSize(closureVars+1, F_mutable, cvec)
                    val () = gen(loadAddressConstant(X1, codeAddr), cvec);
                    val () = gen(storeRegScaled{regT=X1, regN=X0, unitOffset=0}, cvec)
                    val () = genPushReg(X0, cvec)
                    val () = incsp ()
           
                    val entryAddr : int = !realstackptr
                    (* Set the address of this entry in the declaration table and
                       then process any other mutual-recursive functions. *)
                    val () = doNext ()

                    (* Reload the address of the vector - If we have processed other
                       closures the closure will no longer be on the top of the stack. *)
                    val () = gen(loadRegScaled{regT=X1, regN=X_MLStackPtr, unitOffset= !realstackptr - entryAddr}, cvec);

                    (* Load items for the closure. *)
                    fun loadItems ([], _) = ()
                    |   loadItems (v :: vs, addr : int) =
                    (
                        (* Generate an item and move it into the closure *)
                        gencde (BICExtract v, ToX0, NotEnd, NONE);
                        (* The closure "address" excludes the code address. *)
                        gen(storeRegScaled{regT=X0, regN=X1, unitOffset=addr+1}, cvec);
                        topInX0 := false;
                        loadItems (vs, addr + 1)
                    )
             
                    val () = loadItems (closure, 0)

                    (* Lock it by setting the top byte to zero. *)
                    val () = gen(storeRegUnscaledByte{regT=XZero, regN=X1, byteOffset= ~1}, cvec)
                in
                    () (* Don't need to do anything now. *)
                end
         
                else
                let
                    val () = List.app (fn pt => gencde (BICExtract pt, ToStack, NotEnd, NONE)) closure
                in
                    genAllocateFixedSize(closureVars+1, 0w0, cvec);
                    List.foldl(fn (_, w) =>
                        (genPopReg(X1, cvec); gen(storeRegScaled{regT=X1, regN=X0, unitOffset=w-1}, cvec); w-1))
                            (closureVars+1) closure;
                    gen(loadAddressConstant(X1, codeAddr), cvec);
                    gen(storeRegScaled{regT=X1, regN=X0, unitOffset=0}, cvec);
                    genPushReg(X0, cvec);
                    realstackptr := !realstackptr - closureVars + 1 (* Popped the closure vars and pushed the address. *)
                end
            end

        and genCond (testCode, thenCode, elseCode, whereto, tailKind, loopAddr) =
        let
            (* andalso and orelse are turned into conditionals with constants.
               Convert this into a series of tests. *)
            fun genTest(BICConstnt(w, _), jumpOn, targetLabel) =
                let
                    val cVal = case toShort w of 0w0 => false | 0w1 => true | _ => raise InternalError "genTest"
                in
                    if cVal = jumpOn
                    then gen(conditionalBranch (condAlways, targetLabel), cvec)
                    else ()
                end

            |   genTest(BICUnary { oper=BuiltIns.NotBoolean, arg1 }, jumpOn, targetLabel) =
                    genTest(arg1, not jumpOn, targetLabel)

            |   genTest(BICCond (testPart, thenPart, elsePart), jumpOn, targetLabel) =
                let
                    val toElse = createLabel() and exitJump = createLabel()
                in
                    genTest(testPart, false, toElse);
                    genTest(thenPart, jumpOn, targetLabel);
                    gen(conditionalBranch (condAlways, exitJump), cvec);
                    gen(setLabel toElse, cvec);
                    genTest(elsePart, jumpOn, targetLabel);
                    gen(setLabel exitJump, cvec)
                end

            |   genTest(testCode, jumpOn, targetLabel) =
                (
                    gencde (testCode, ToStack, NotEnd, loopAddr);
                    genPopReg(X0, cvec);
                    gen(subSImmediate{regN=X0, regD=XZero, immed=taggedWord 0w1, shifted=false}, cvec);
                    gen(conditionalBranch(if jumpOn then condEqual else condNotEqual, targetLabel), cvec);
                    decsp() (* conditional branch pops a value. *)
                )

            val toElse = createLabel() and exitJump = createLabel()
            val () = genTest(testCode, false, toElse)
            val () = gencde (thenCode, whereto, tailKind, loopAddr)
            (* Get rid of the result from the stack. If there is a result then the
            ``else-part'' will push it. *)
            val () = case whereto of ToStack => decsp () | NoResult => () | ToX0 => ()
            val () = topInX0 := false

            val () = gen(conditionalBranch (condAlways, exitJump), cvec)

            (* start of "else part" *)
            val () = gen(setLabel toElse, cvec)
            val () = gencde (elseCode, whereto, tailKind, loopAddr)
            val () = gen(setLabel exitJump, cvec)
        in
            ()
        end (* genCond *)

        and genEval (eval, tailKind : tail) : unit =
        let
            val argList : backendIC list = List.map #1 (#argList eval)
            val argsToPass : int = List.length argList;

            (* Load arguments *)
            fun loadArgs [] = ()
            |   loadArgs (v :: vs) =
            let (* Push each expression onto the stack. *)
                val () = gencde(v, ToStack, NotEnd, NONE)
            in
                loadArgs vs
            end;

            (* Have to guarantee that the expression to return the function
              is evaluated before the arguments. *)

            (* Returns true if evaluating it later is safe. *)
            fun safeToLeave (BICConstnt _) = true
            |   safeToLeave (BICLambda _) = true
            |   safeToLeave (BICExtract _) = true
            |   safeToLeave (BICField {base, ...}) = safeToLeave base
            |   safeToLeave (BICLoadContainer {base, ...}) = safeToLeave base
            |   safeToLeave _ = false

            val () =
                if (case argList of [] => true | _ => safeToLeave (#function eval))
                then
                let
                    (* Can load the args first. *)
                    val () = loadArgs argList
                in 
                    gencde (#function eval, ToStack, NotEnd, NONE)
                end

                else
                let
                    (* The expression for the function is too complicated to
                       risk leaving. It might have a side-effect and we must
                       ensure that any side-effects it has are done before the
                       arguments are loaded. *)
                    val () = gencde(#function eval, ToStack, NotEnd, NONE);
                    val () = loadArgs(argList);
                    (* Load the function again. *)
                    val () = gen(loadRegScaled{regT=X0, regN=X_MLStackPtr, unitOffset=argsToPass}, cvec)
                    val () = genPushReg(X0, cvec)
                in
                    incsp ()
                end

        in (* body of genEval *)
            case tailKind of
                NotEnd => (* Normal call. *)
                let
                    val () = genPopReg(X8, cvec) (* Pop the closure pointer. *)
                    (* We need to put the first 8 arguments into registers and
                       leave the rest on the stack. *)
                    fun loadArg(n, reg) =
                        if argsToPass > n
                        then gen(loadRegScaled{regT=reg, regN=X_MLStackPtr, unitOffset=argsToPass-n-1}, cvec)
                        else ()
                    val () = loadArg(0, X0)
                    val () = loadArg(1, X1)
                    val () = loadArg(2, X2)
                    val () = loadArg(3, X3)
                    val () = loadArg(4, X4)
                    val () = loadArg(5, X5)
                    val () = loadArg(6, X6)
                    val () = loadArg(7, X7)
                in
                    gen(loadRegScaled{regT=X9, regN=X8, unitOffset=0}, cvec); (* Entry point *)
                    gen(branchAndLinkReg X9, cvec);
                    (* We have popped the closure pointer.  The caller has popped the stack
                       arguments and we have pushed the result value. The register arguments
                       are still on the stack. *)
                    topInX0 := true;
                    realstackptr := !realstackptr - Int.max(argsToPass-8, 0) - 1 (* Args popped by caller. *)
                end
     
            |   EndOfProc => (* Tail recursive call. *)
                let
                    val () = genPopReg(X8, cvec) (* Pop the closure pointer. *)
                    val () = decsp()
                    (* Get the return address into X30. *)
                    val () = gen(loadRegScaled{regT=X30, regN=X_MLStackPtr, unitOffset= !realstackptr}, cvec)

                    (* Load the register arguments *)
                    fun loadArg(n, reg) =
                        if argsToPass > n
                        then gen(loadRegScaled{regT=reg, regN=X_MLStackPtr, unitOffset=argsToPass-n-1}, cvec)
                        else ()
                    val () = loadArg(0, X0)
                    val () = loadArg(1, X1)
                    val () = loadArg(2, X2)
                    val () = loadArg(3, X3)
                    val () = loadArg(4, X4)
                    val () = loadArg(5, X5)
                    val () = loadArg(6, X6)
                    val () = loadArg(7, X7)
                    (* We need to move the stack arguments into the original argument area. *)

                    (* This is the total number of words that this function is responsible for.
                       It includes the stack arguments that the caller expects to be removed. *)
                    val itemsOnStack = !realstackptr + 1 + numOfArgs

                    (* Stack arguments are moved using X9. *)
                    fun moveStackArg n =
                    if n >= argsToPass
                    then ()
                    else
                    let
                        val () = loadArg(n, X9)
                        val destOffset = itemsOnStack - (n-8) - 1
                        val () = gen(storeRegScaled{regT=X9, regN=X_MLStackPtr, unitOffset=destOffset}, cvec)
                    in
                        moveStackArg(n+1)
                    end

                    val () = moveStackArg 8
                in
                    resetStack(itemsOnStack - Int.max(argsToPass-8, 0), false, cvec);
                    gen(loadRegScaled{regT=X9, regN=X8, unitOffset=0}, cvec); (* Entry point *)
                    gen(branchRegister X9, cvec)
                    (* Since we're not returning we can ignore the stack pointer value. *)
                end
        end (* genEval *)

        (* Begin generating the code for the function. *)
        val prefix = ref []
        (* Push the arguments passed in registers. *)
        val () = if numOfArgs >= 8 then genPushReg (X7, prefix) else ()
        val () = if numOfArgs >= 7 then genPushReg (X6, prefix) else ()
        val () = if numOfArgs >= 6 then genPushReg (X5, prefix) else ()
        val () = if numOfArgs >= 5 then genPushReg (X4, prefix) else ()
        val () = if numOfArgs >= 4 then genPushReg (X3, prefix) else ()
        val () = if numOfArgs >= 3 then genPushReg (X2, prefix) else ()
        val () = if numOfArgs >= 2 then genPushReg (X1, prefix) else ()
        val () = if numOfArgs >= 1 then genPushReg (X0, prefix) else ()
        val () = genPushReg (X30, prefix)
        val () = genPushReg (X8, prefix) (* Push closure pointer *)

        (* Generate the function. *)
        (* Assume we always want a result. There is otherwise a problem if the
          called routine returns a result of type void (i.e. no result) but the
          caller wants a result (e.g. the identity function). *)
        val () = gencde (pt, ToX0, EndOfProc, NONE)
        val () = resetStack(1, false, cvec) (* Skip over the pushed closure *)
        val () = genPopReg(X30, cvec) (* Return address => pop into X30 *)
        val () = resetStack(numOfArgs, false, cvec) (* Remove the arguments *)
        val () = gen(returnRegister X30, cvec) (* Jump to X30 *)
        
        (* Now we know the maximum stack size we can code-gen the stack check.
           This needs to go in after we have saved X30. *)
        val () = checkStackCode(X10, !maxStack, false, prefix)
        val instructions = List.rev(!prefix) @ List.rev(!cvec)

    in (* body of codegen *)
       (* Having code-generated the body of the function, it is copied
          into a new data segment. *)
        generateCode{instrs=instructions, name=name, parameters=parameters, resultClosure=resultClosure}
    end (* codegen *)

    fun gencodeLambda(lambda as { name, body, argTypes, localCount, ...}:bicLambdaForm, parameters, closure) =
    if (*false andalso *)Debug.getParameter Debug.compilerDebugTag parameters = 0
    then FallBackCG.gencodeLambda(lambda, parameters, closure)
    else
        codegen (body, name, closure, List.length argTypes, localCount, parameters)
        handle Fallback s =>
        (
            Pretty.getSimplePrinter(parameters, []) ("TODO: " ^ s ^ "\n");
            FallBackCG.gencodeLambda(lambda, parameters, closure)
        )

    structure Foreign = Arm64Foreign

    structure Sharing =
    struct
        open BackendTree.Sharing
        type closureRef = closureRef
    end

end;
