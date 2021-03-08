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
    structure BackendTree: BackendIntermediateCodeSig
    and       CodeArray: CODEARRAYSIG
    and       Arm64Assembly: Arm64Assembly
    and       Debug: DEBUG
    and       Arm64Foreign: FOREIGNCALLSIG
    and       Arm64Sequences: Arm64Sequences
    
    sharing BackendTree.Sharing = CodeArray.Sharing = Arm64Assembly.Sharing = Arm64Sequences.Sharing

) : GENCODESIG =
struct

    open BackendTree CodeArray Arm64Assembly Address Arm64Sequences
    
    open BuiltIns
    
    exception InternalError = Misc.InternalError

    (* tag a short constant *)
    fun tag c = 2 * c + 1
    and semitag c = 2*c
    
    fun taggedWord w: word = w * 0w2 + 0w1
    and taggedWord64 w: Word64.word = w * 0w2 + 0w1
    
    val tagBitMask = Word64.<<(Word64.fromInt ~1, 0w1)
    
    fun gen(instr, code) = code := instr :: !code

    fun genList([], _) = ()
    |   genList(instr :: instrs, code) = (gen(instr, code); genList(instrs, code))

    fun genPushReg(reg, code) = gen(storeRegPreIndex{regT=reg, regN=X_MLStackPtr, byteOffset= ~8}, code)
    and genPopReg(reg, code) = gen(loadRegPostIndex{regT=reg, regN=X_MLStackPtr, byteOffset= 8}, code)

    (* Load a value using a scaled offset.  This uses a normal scaled load if
       the offset is in the range and an indexed offset if it is not. *)
    fun loadScaledOffset(scale, loadScaled, loadIndexed) {base, dest, work, offset} =
        if offset < 0 then raise InternalError "loadScaledOffset: negative offset"
        else if offset < 0x1000
        then [loadScaled{regT=dest, regN=base, unitOffset=offset}]
        else
            loadNonAddress(work, Word64.fromInt offset) @
            [loadIndexed{regN=base, regM=work, regT=dest,
                option=ExtUXTX(if scale = 1 then NoScale else ScaleOrShift)}]

    (* Similar store. *)
    and storeScaledOffset(scale, storeScaled, storeIndexed) {base, store, work, offset} =
        if offset < 0 then raise InternalError "storeScaledOffset: negative offset"
        else if offset < 0x1000
        then [storeScaled{regT=store, regN=base, unitOffset=offset}]
        else
            loadNonAddress(work, Word64.fromInt offset) @
            [storeIndexed{regN=base, regM=work, regT=store,
                option=ExtUXTX(if scale = 1 then NoScale else ScaleOrShift)}]

    val loadScaledWord = loadScaledOffset(8, loadRegScaled, loadRegIndexed)
    and storeScaledWord = storeScaledOffset(8, storeRegScaled, storeRegIndexed)

    (* Add a constant word to the source register and put the result in the
       destination.  regW is used as a work register if necessary.  This is used
       both for addition and subtraction. *)
    fun addConstantWord({regS, regD, value=0w0, ...}, code) =
        if regS = regD then () else gen(moveRegToReg{sReg=regS, dReg=regD}, code)
    
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
                genList(loadNonAddress(regW, shifted), code);
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

    (* Sequence to allocate on the heap.  The words are not initialised
       apart from the length word. *)
    fun genAllocateFixedSize(words, flags, resultReg, workReg, code) =
    let
        val label = createLabel()
    in
        (* Subtract the number of bytes required from the heap pointer and put in X0. *)
        addConstantWord({regS=X_MLHeapAllocPtr, regD=X0, regW=X3,
            value= ~ (Word64.fromLarge(Word.toLarge wordSize)) * Word64.fromInt(words+1)}, code);
        compareRegs(resultReg, X_MLHeapLimit, code);
        gen(conditionalBranch(condCarrySet, label), code);
        gen(loadRegScaled{regT=X16, regN=X_MLAssemblyInt, unitOffset=heapOverflowCallOffset}, code);
        gen(branchAndLinkReg X16, code);
        gen(registerMask [], code); (* Not used at the moment. *)
        gen(setLabel label, code);
        gen(moveRegToReg{sReg=resultReg, dReg=X_MLHeapAllocPtr}, code);
        genList(loadNonAddress(workReg,
            Word64.orb(Word64.fromInt words, Word64.<<(Word64.fromLarge(Word8.toLarge flags), 0w56))), code);
        (* Store the length word.  Have to use the unaligned version because offset is -ve. *)
        gen(storeRegUnscaled{regT=workReg, regN=resultReg, byteOffset= ~8}, code)
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
        gen(moveRegToReg{sReg=resultReg, dReg=X_MLHeapAllocPtr}, code);
        (* Combine the size with the flags in the top byte. *)
        gen(orrShiftedReg{regM=flagsReg, regN=sizeReg, regD=flagsReg, shift=ShiftLSL 0w56}, code);
        (* Store the length word.  Have to use the unaligned version because offset is -ve. *)
        gen(storeRegUnscaled{regT=flagsReg, regN=resultReg, byteOffset= ~8}, code)
    end

    (* Set a register to either tagged(1) i.e. true or tagged(0) i.e. false. *)
    fun setBooleanCondition(reg, condition, code) =
    (
        genList(loadNonAddress(reg, Word64.fromInt(tag 1)), code);
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

    (* Allocate a single byte cell and store the register into it. The result is
       in X0 so reg must not be X0. *)
    fun boxLargeWord(reg, code) =
    (
        reg <> X0 orelse raise InternalError "boxLargeWord: X0";
        genAllocateFixedSize(1, F_bytes, X0, X5, code);
        gen(storeRegScaled{regT=reg, regN=X0, unitOffset=0}, code)
    )
    
    (* Allocate a single byte cell for a "real" i.e. double-precision floating
       point number. *)
    fun boxDouble(reg, code) =
    (
        genAllocateFixedSize(1, F_bytes, X0, X5, code);
        gen(storeRegScaledDouble{regT=reg, regN=X0, unitOffset=0}, code)
    )

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
            val _ = !topInX0 andalso raise InternalError "topInX0 true at start"

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
                        (genList(loadNonAddress(X0, Word64.fromInt(tag soffset)), cvec); genPushReg(X0, cvec); incsp())
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
            
            val genCAddress = genMLAddress

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

            (* Similar to genMLLoadAddress but for C addresses.  There are two
               differences.  The index is signed so we use an arithmetic shift and
               the base address is a LargeWord value i.e. the actual
               address is held in the word pointed at by "base", unlike with
               ML addresses. *)
            fun genCLoadAddress({base, index=NONE, offset}, scale) =
                (
                    gencde (base, ToX0, NotEnd, loopAddr);
                    gen(loadRegScaled{regT=X0, regN=X0, unitOffset=0}, cvec);
                    (X0, MLLoadOffset(offset div scale))
                )
            |   genCLoadAddress({base, index=SOME indexVal, offset}, scale) =
                (
                    gencde (base, ToStack, NotEnd, loopAddr); (* Push base addr to stack. *)
                    gencde (indexVal, ToX0, NotEnd, loopAddr);
                    (* Shift right to remove the tag.  C indexes are SIGNED. *)
                    gen(arithmeticShiftRight{regN=X0, regD=X0, wordSize=WordSize64, shift=0w1}, cvec);
                    (* Add any constant offset.  Does nothing if it's zero. *)
                    addConstantWord({regS=X0, regD=X0, regW=X3,
                        value=Word64.fromInt (* unsigned *)(offset div scale)}, cvec);
                    genPopReg(X1, cvec); (* Pop base reg into X1. *)
                    gen(loadRegScaled{regT=X1, regN=X1, unitOffset=0}, cvec);
                    decsp();
                    (X1, MLLoadReg X0)
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
                        genList (loadScaledWord{dest=X0, base=X_MLStackPtr, work=X16, offset= !realstackptr + addr}, cvec);
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
                            genList (loadScaledWord{dest=X0, base=X0, work=X16,
                                offset=locn+1 (* The first word is the code *)}, cvec)
                        )
                    |   BICLoadRecursive =>
                            loadLocalStackValue ~1 (* The closure itself - first value on the stack. *)
                end

            |   BICField {base, offset} =>
                (
                    gencde (base, ToX0, NotEnd, loopAddr);
                    genList (loadScaledWord{dest=X0, base=X0, work=X16, offset=offset}, cvec)
                )

            |   BICLoadContainer {base, offset} =>
                (
                    gencde (base, ToX0, NotEnd, loopAddr);
                    genList (loadScaledWord{dest=X0, base=X0, work=X16, offset=offset}, cvec)
                )
       
            |   BICLambda lam => genProc (lam, false, fn () => ())
           
            |   BICConstnt(w, _) =>
                (
                    if isShort w
                    then genList(loadNonAddress(X0,
                                    taggedWord64(Word64.fromLarge(Word.toLargeX(toShort w)))), cvec)
                    else gen(loadAddressConstant(X0, w), cvec);
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
                            genList(loadNonAddress(X0, Word64.fromInt(tag 0)), cvec);
                            let fun pushN 0 = () | pushN n = (genPushReg(X0, cvec); pushN (n-1)) in pushN size end;
                            gen(moveRegToReg{sReg=X_MLStackPtr, dReg=X0}, cvec);
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
                            genList(storeScaledWord{store=X0, base=X_MLStackPtr, work=X16, offset=argOffset-1}, cvec);
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
                    val () = genList(loadNonAddress(X1, Word64.fromInt nCases * 0w2), cvec)
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
                    genAllocateFixedSize(size, 0w0, X0, X1, cvec);
                    List.foldl(fn (_, w) =>
                        (
                            genPopReg(X1, cvec);
                            genList(storeScaledWord{store=X1, base=X0, work=X16, offset=w-1}, cvec);
                            w-1)
                        )
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
                                    genList(storeScaledWord{store=X0, base=X1, work=X16, offset=destOffset}, cvec);
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
                                    genList(loadScaledWord{dest=X2, base=X1, work=X16, offset=sourceOffset}, cvec);
                                    (* Store into the container. *)
                                    genList(storeScaledWord{store=X2, base=X0, work=X16, offset=destOffset}, cvec);
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

            |   BICNullary {oper=GetCurrentThreadId} =>
                (
                    gen(loadRegScaled{regT=X0, regN=X_MLAssemblyInt, unitOffset=threadIdOffset}, cvec);
                    genPushReg(X0, cvec);
                    incsp()
                )

            |   BICNullary {oper=CheckRTSException} =>
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

            |   BICNullary {oper=CPUPause} => gen(yield, cvec)

            |   BICUnary { oper, arg1 } => genUnary(oper, arg1, loopAddr)

            |   BICBinary { oper, arg1, arg2 } => genBinary(oper, arg1, arg2, loopAddr)
            
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
                                genAllocateFixedSize(1, flagByte, X0, X1, cvec);
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
                            genList(loadScaledWord{dest=X0, base=base, work=X16, offset=offset}, cvec)
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
                    case genCLoadAddress(address, 1) of
                        (base, MLLoadOffset offset) =>
                            if offset < 0 (* C offsets can be negative. *)
                            then gen(loadRegUnscaledByte{regT=X0, regN=base, byteOffset=offset}, cvec)
                            else gen(loadRegScaledByte{regT=X0, regN=base, unitOffset=offset}, cvec)
                    |   (base, MLLoadReg indexR) =>
                            gen(loadRegIndexedByte{regN=base, regM=indexR, regT=X0, option=ExtUXTX NoScale}, cvec);

                    (* Have to tag the result. *)
                    gen(logicalShiftLeft{regN=X0, regD=X0, wordSize=WordSize32, shift=0w1}, cvec);
                    gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize32, bits=0w1}, cvec)
                )

            |   BICLoadOperation { kind=LoadStoreC16, address} =>
                (
                    case genCLoadAddress(address, 2) of
                        (base, MLLoadOffset offset) =>
                            if offset < 0 (* C offsets can be negative. *)
                            then gen(loadRegUnscaled16{regT=X0, regN=base, byteOffset=offset*2}, cvec)
                            else gen(loadRegScaled16{regT=X0, regN=base, unitOffset=offset}, cvec)
                    |   (base, MLLoadReg indexR) =>
                            gen(loadRegIndexed16{regN=base, regM=indexR, regT=X0, option=ExtUXTX ScaleOrShift}, cvec);

                    (* Have to tag the result. *)
                    gen(logicalShiftLeft{regN=X0, regD=X0, wordSize=WordSize32, shift=0w1}, cvec);
                    gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize32, bits=0w1}, cvec)
                )

            |   BICLoadOperation { kind=LoadStoreC32, address} =>
                (
                    case genCLoadAddress(address, 4) of
                        (base, MLLoadOffset offset) =>
                            if offset < 0 (* C offsets can be negative. *)
                            then gen(loadRegUnscaled32{regT=X0, regN=base, byteOffset=offset*4}, cvec)
                            else gen(loadRegScaled32{regT=X0, regN=base, unitOffset=offset}, cvec)
                    |   (base, MLLoadReg indexR) =>
                            gen(loadRegIndexed32{regN=base, regM=indexR, regT=X0, option=ExtUXTX ScaleOrShift}, cvec);

                    (* Have to tag the result. *)
                    gen(logicalShiftLeft{regN=X0, regD=X0, wordSize=WordSize64 (* Must use 64-bits *), shift=0w1}, cvec);
                    gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
                )

            |   BICLoadOperation { kind=LoadStoreC64, address} =>
                (
                    case genCLoadAddress(address, 8) of
                        (base, MLLoadOffset offset) =>
                            if offset < 0 (* C offsets can be negative. *)
                            then gen(loadRegUnscaled{regT=X1, regN=base, byteOffset=offset*8}, cvec)
                            else gen(loadRegScaled{regT=X1, regN=base, unitOffset=offset}, cvec)
                    |   (base, MLLoadReg indexR) =>
                            gen(loadRegIndexed{regN=base, regM=indexR, regT=X1, option=ExtUXTX ScaleOrShift}, cvec);

                    (* Load the value at the address and box it. *)
                    boxLargeWord(X1, cvec)
                )

            |   BICLoadOperation { kind=LoadStoreCFloat, address} =>
                (
                    case genCLoadAddress(address, 4) of
                        (base, MLLoadOffset offset) =>
                            if offset < 0 (* C offsets can be negative. *)
                            then gen(loadRegUnscaledFloat{regT=V0, regN=base, byteOffset=offset*4}, cvec)
                            else gen(loadRegScaledFloat{regT=V0, regN=base, unitOffset=offset}, cvec)
                    |   (base, MLLoadReg indexR) =>
                            gen(loadRegIndexedFloat{regN=base, regM=indexR, regT=V0, option=ExtUXTX ScaleOrShift}, cvec);
                    (* This is defined to return a "real" i.e. a double so we need to convert it
                       to a double and then box the result. *)
                    gen(convertFloatToDouble{regN=V0, regD=V0}, cvec);
                    boxDouble(V0, cvec)
                )

            |   BICLoadOperation { kind=LoadStoreCDouble, address} =>
                (
                    case genCLoadAddress(address, 8) of
                        (base, MLLoadOffset offset) =>
                            if offset < 0 (* C offsets can be negative. *)
                            then gen(loadRegUnscaledDouble{regT=V0, regN=base, byteOffset=offset*8}, cvec)
                            else gen(loadRegScaledDouble{regT=V0, regN=base, unitOffset=offset}, cvec)
                    |   (base, MLLoadReg indexR) =>
                            gen(loadRegIndexedDouble{regN=base, regM=indexR, regT=V0, option=ExtUXTX ScaleOrShift}, cvec);
                    (* Box the result. *)
                    boxDouble(V0, cvec)
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
                    genCAddress(address, 1);
                    gencde (value, ToX0, NotEnd, loopAddr); (* Value to store *)
                    gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize32, shift=0w1}, cvec); (* Untag it *)
                    genPopReg(X1, cvec); (* Index: a tagged value. *)
                    (* Untag.  C indexes are signed. *)
                    gen(arithmeticShiftRight{regN=X1, regD=X1, wordSize=WordSize64, shift=0w1}, cvec);
                    genPopReg(X2, cvec); (* Base address as a SysWord.word value. *)
                    gen(loadRegScaled{regT=X2, regN=X2, unitOffset=0}, cvec); (* Actual address *)
                    gen(storeRegIndexedByte{regN=X2, regM=X1, regT=X0, option=ExtUXTX NoScale}, cvec);
                    topInX0 := false;
                    decsp(); decsp()
                )

            |   BICStoreOperation { kind=LoadStoreC16, address, value} =>
                (
                    genCAddress(address, 2);
                    gencde (value, ToX0, NotEnd, loopAddr); (* Value to store *)
                    gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize32, shift=0w1}, cvec); (* Untag it *)
                    genPopReg(X1, cvec); (* Index: a tagged value. *)
                    (* Untag.  C indexes are signed. *)
                    gen(arithmeticShiftRight{regN=X1, regD=X1, wordSize=WordSize64, shift=0w1}, cvec);
                    genPopReg(X2, cvec); (* Base address as a SysWord.word value. *)
                    gen(loadRegScaled{regT=X2, regN=X2, unitOffset=0}, cvec); (* Actual address *)
                    gen(storeRegIndexed16{regN=X2, regM=X1, regT=X0, option=ExtUXTX ScaleOrShift}, cvec);
                    topInX0 := false;
                    decsp(); decsp()
                )

            |   BICStoreOperation { kind=LoadStoreC32, address, value} =>
                (
                    genCAddress(address, 4);
                    gencde (value, ToX0, NotEnd, loopAddr); (* Value to store *)
                    gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize64, shift=0w1}, cvec); (* Untag it *)
                    genPopReg(X1, cvec); (* Index: a tagged value. *)
                    (* Untag.  C indexes are signed. *)
                    gen(arithmeticShiftRight{regN=X1, regD=X1, wordSize=WordSize64, shift=0w1}, cvec);
                    genPopReg(X2, cvec); (* Base address as a SysWord.word value. *)
                    gen(loadRegScaled{regT=X2, regN=X2, unitOffset=0}, cvec); (* Actual address *)
                    gen(storeRegIndexed32{regN=X2, regM=X1, regT=X0, option=ExtUXTX ScaleOrShift}, cvec);
                    topInX0 := false;
                    decsp(); decsp()
                )

            |   BICStoreOperation { kind=LoadStoreC64, address, value} =>
                (
                    genCAddress(address, 8);
                    gencde (value, ToX0, NotEnd, loopAddr); (* Value to store. This is boxed. *)
                    gen(loadRegScaled{regT=X0, regN=X0, unitOffset=0}, cvec);
                    genPopReg(X1, cvec); (* Index: a tagged value. *)
                    (* Untag.  C indexes are signed. *)
                    gen(arithmeticShiftRight{regN=X1, regD=X1, wordSize=WordSize64, shift=0w1}, cvec);
                    genPopReg(X2, cvec); (* Base address as a SysWord.word value. *)
                    gen(loadRegScaled{regT=X2, regN=X2, unitOffset=0}, cvec); (* Actual address *)
                    gen(storeRegIndexed{regN=X2, regM=X1, regT=X0, option=ExtUXTX ScaleOrShift}, cvec);
                    topInX0 := false;
                    decsp(); decsp()
                )

            |   BICStoreOperation { kind=LoadStoreCFloat, address, value} =>
                (
                    genCAddress(address, 4);
                    gencde (value, ToX0, NotEnd, loopAddr); (* Value to store *)
                    (* This is a boxed double.  It needs to be converted to a float. *)
                    gen(loadRegScaledDouble{regT=V0, regN=X0, unitOffset=0}, cvec); (* Untag it *)
                    gen(convertDoubleToFloat{regN=V0, regD=V0}, cvec);
                    genPopReg(X1, cvec); (* Index: a tagged value. *)
                    (* Untag.  C indexes are signed. *)
                    gen(arithmeticShiftRight{regN=X1, regD=X1, wordSize=WordSize64, shift=0w1}, cvec);
                    genPopReg(X2, cvec); (* Base address as a SysWord.word value. *)
                    gen(loadRegScaled{regT=X2, regN=X2, unitOffset=0}, cvec); (* Actual address *)
                    gen(storeRegIndexedFloat{regN=X2, regM=X1, regT=V0, option=ExtUXTX ScaleOrShift}, cvec);
                    topInX0 := false;
                    decsp(); decsp()
                )

            |   BICStoreOperation { kind=LoadStoreCDouble, address, value} =>
                (
                    genCAddress(address, 8);
                    gencde (value, ToX0, NotEnd, loopAddr); (* Value to store *)
                    (* This is a boxed double. *)
                    gen(loadRegScaledDouble{regT=V0, regN=X0, unitOffset=0}, cvec); (* Untag it *)
                    genPopReg(X1, cvec); (* Index: a tagged value. *)
                    (* Untag.  C indexes are signed. *)
                    gen(arithmeticShiftRight{regN=X1, regD=X1, wordSize=WordSize64, shift=0w1}, cvec);
                    genPopReg(X2, cvec); (* Base address as a SysWord.word value. *)
                    gen(loadRegScaled{regT=X2, regN=X2, unitOffset=0}, cvec); (* Actual address *)
                    gen(storeRegIndexedDouble{regN=X2, regM=X1, regT=V0, option=ExtUXTX ScaleOrShift}, cvec);
                    topInX0 := false;
                    decsp(); decsp()
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
                    genList(loadNonAddress(X0, Word64.fromInt(tag 1)), cvec);
                    (* Set X0 to either 1 or -1 depending on whether it's greater or less. *)
                    gen(conditionalSetInverted{regD=X0, regTrue=X0, regFalse=XZero, cond=condUnsignedHigher}, cvec);
                    gen(conditionalBranch(condAlways, resultLabel), cvec);
                    gen(setLabel equalLabel, cvec);
                    (* Equal case - set it to zero. *)
                    genList(loadNonAddress(X0, Word64.fromInt(tag 0)), cvec);
                    gen(setLabel resultLabel, cvec)
                end

           |    BICArbitrary { oper=ArithAdd, shortCond, arg1, arg2, longCall } =>
                let
                    val startLong = createLabel() and resultLabel = createLabel()
                in
                    (* Check tag bits *)
                    gencde (shortCond, ToX0, NotEnd, loopAddr);
                    gen(subSImmediate{regN=X0, regD=XZero, immed=taggedWord 0w1, shifted=false}, cvec);
                    (* Go to the long case if it's not short. *)
                    gen(conditionalBranch(condNotEqual, startLong), cvec);
                    topInX0 := false;
                    gencde (arg1, ToStack, NotEnd, loopAddr);
                    gencde (arg2, ToX0, NotEnd, loopAddr);
                    gen(subImmediate{regN=X0, regD=X0, immed=0w1, shifted=false}, cvec);
                    genPopReg(X1, cvec);
                    decsp();
                    (* Add and set the flag bits *)
                    gen(addSShiftedReg{regN=X1, regM=X0, regD=X0, shift=ShiftNone}, cvec);
                    (* If there's no overflow skip to the end otherwise drop into
                       the call to the RTS. *)
                    gen(conditionalBranch(condNoOverflow, resultLabel), cvec);
                    gen(setLabel startLong, cvec);
                    topInX0 := false;
                    gencde (longCall, ToX0, tailKind, loopAddr);
                    gen(setLabel resultLabel, cvec)
                end

           |    BICArbitrary { oper=ArithSub, shortCond, arg1, arg2, longCall } =>
                let
                    val startLong = createLabel() and resultLabel = createLabel()
                in
                    (* Check tag bits *)
                    gencde (shortCond, ToX0, NotEnd, loopAddr);
                    gen(subSImmediate{regN=X0, regD=XZero, immed=taggedWord 0w1, shifted=false}, cvec);
                    (* Go to the long case if it's not short. *)
                    gen(conditionalBranch(condNotEqual, startLong), cvec);
                    topInX0 := false;
                    gencde (arg1, ToStack, NotEnd, loopAddr);
                    gencde (arg2, ToX0, NotEnd, loopAddr);
                    gen(subImmediate{regN=X0, regD=X0, immed=0w1, shifted=false}, cvec);
                    genPopReg(X1, cvec);
                    decsp();
                    (* Subtract and set the flag bits *)
                    gen(subSShiftedReg{regN=X1, regM=X0, regD=X0, shift=ShiftNone}, cvec);
                    gen(conditionalBranch(condNoOverflow, resultLabel), cvec);
                    gen(setLabel startLong, cvec);
                    topInX0 := false;
                    gencde (longCall, ToX0, tailKind, loopAddr);
                    gen(setLabel resultLabel, cvec)
                end

           |    BICArbitrary { oper=ArithMult, longCall, ... } =>
                (* Multiply - Just implement as a call to the long-precision case. *)
                (
                    gencde (longCall, whereto, tailKind, loopAddr)
                )

           |    BICArbitrary _ =>
                    raise InternalError "BICArbitrary: unimplemented operation"

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
                            genList(loadNonAddress(X0, Word64.fromInt(tag 0)), cvec);
                            genPushReg(X0, cvec)
                        )
                        else resetStack (adjustment, true, cvec)
                in
                    realstackptr := newsp
                end
          
            |   NoResult =>
                let
                    val () = topInX0 := false
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
                        then genList(loadNonAddress(X0, Word64.fromInt(tag 0)), cvec)
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


        and genUnary(NotBoolean, arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                (* Flip true to false and the reverse. *)
                gen(bitwiseXorImmediate{wordSize=WordSize32, bits=0w2, regN=X0, regD=X0}, cvec)
            )

        |   genUnary(IsTaggedValue, arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                gen(testBitPattern(X0, 0w1), cvec);
                setBooleanCondition(X0, condNotEqual (*Non-zero*), cvec)
            )

        |   genUnary(MemoryCellLength, arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                (* Load the length word. *)
                gen(loadRegUnscaled{regT=X0, regN=X0, byteOffset= ~8}, cvec);
                (* Extract the length, excluding the flag bytes and shift by one bit. *)
                gen(unsignedBitfieldInsertinZeros
                    {wordSize=WordSize64, lsb=0w1, width=0w56, regN=X0, regD=X0}, cvec);
                (* Set the tag bit. *)
                gen(bitwiseOrImmediate{wordSize=WordSize64, bits=0w1, regN=X0, regD=X0}, cvec)
            )

        |   genUnary(MemoryCellFlags, arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                (* Load the flags byte. *)
                gen(loadRegUnscaledByte{regT=X0, regN=X0, byteOffset= ~1}, cvec);
                (* Tag the result. *)
                gen(logicalShiftLeft{wordSize=WordSize64, shift=0w1, regN=X0, regD=X0}, cvec);
                gen(bitwiseOrImmediate{wordSize=WordSize64, bits=0w1, regN=X0, regD=X0}, cvec)
            )

        |   genUnary(ClearMutableFlag, arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                gen(loadRegUnscaledByte{regT=X1, regN=X0, byteOffset= ~1}, cvec);
                gen(bitwiseAndImmediate{wordSize=WordSize32, bits=Word64.xorb(0wxffffffff, 0wx40), regN=X1, regD=X1}, cvec);
                gen(storeRegUnscaledByte{regT=X1, regN=X0, byteOffset= ~1}, cvec)
            )

        |   genUnary(AtomicReset, arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                (* Clear the mutex. Simply setting it to tagged 0 will work.
                   If another thread is in the ldaxr/stlxr loop it will see
                   the value has changed and retry. *)
                genList(loadNonAddress(X1, taggedWord64 0w0), cvec);
                gen(storeRegScaled{regT=X1, regN=X0, unitOffset=0}, cvec)
            )

        |   genUnary(LongWordToTagged, arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                (* Load the value and tag it. *)
                gen(loadRegScaled{regT=X0, regN=X0, unitOffset=0}, cvec);
                (* Tag the result. *)
                gen(logicalShiftLeft{wordSize=WordSize64, shift=0w1, regN=X0, regD=X0}, cvec);
                gen(bitwiseOrImmediate{wordSize=WordSize64, bits=0w1, regN=X0, regD=X0}, cvec)
            )

        |   genUnary(SignedToLongWord, arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                gen(arithmeticShiftRight{wordSize=WordSize64, shift=0w1, regN=X0, regD=X1}, cvec);
                boxLargeWord(X1, cvec)
            )

        |   genUnary(UnsignedToLongWord, arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                gen(logicalShiftRight{wordSize=WordSize64, shift=0w1, regN=X0, regD=X1}, cvec);
                boxLargeWord(X1, cvec)
            )

        |   genUnary(RealAbs PrecDouble, arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                gen(loadRegScaledDouble{regT=V0, regN=X0, unitOffset=0}, cvec);
                gen(absDouble{regN=V0, regD=V0}, cvec);
                boxDouble(V0, cvec)
            )

        |   genUnary(RealNeg PrecDouble, arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                gen(loadRegScaledDouble{regT=V0, regN=X0, unitOffset=0}, cvec);
                gen(negDouble{regN=V0, regD=V0}, cvec);
                boxDouble(V0, cvec)
            )

        |   genUnary(RealFixedInt PrecDouble, arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                (* Shift to remove the tag. *)
                gen(arithmeticShiftRight{wordSize=WordSize64, shift=0w1, regN=X0, regD=X0}, cvec);
                gen(convertIntToDouble{regN=X0, regD=V0}, cvec);
                boxDouble(V0, cvec)
            )

        |   genUnary(RealAbs PrecSingle, arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                gen(logicalShiftRight{wordSize=WordSize64, shift=0w32, regN=X0, regD=X0}, cvec);
                gen(moveGeneralToFloat{regN=X0, regD=V0}, cvec);
                gen(absFloat{regN=V0, regD=V0}, cvec);
                gen(moveFloatToGeneral{regN=V0, regD=X0}, cvec);
                gen(logicalShiftLeft{wordSize=WordSize64, shift=0w32, regN=X0, regD=X0}, cvec);
                gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
            )

        |   genUnary(RealNeg PrecSingle, arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                gen(logicalShiftRight{wordSize=WordSize64, shift=0w32, regN=X0, regD=X0}, cvec);
                gen(moveGeneralToFloat{regN=X0, regD=V0}, cvec);
                gen(negFloat{regN=V0, regD=V0}, cvec);
                gen(moveFloatToGeneral{regN=V0, regD=X0}, cvec);
                gen(logicalShiftLeft{wordSize=WordSize64, shift=0w32, regN=X0, regD=X0}, cvec);
                gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
            )

        |   genUnary(RealFixedInt PrecSingle, arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                 (* Shift to remove the tag. *)
                gen(arithmeticShiftRight{wordSize=WordSize64, shift=0w1, regN=X0, regD=X0}, cvec);
                gen(convertIntToFloat{regN=X0, regD=V0}, cvec);
                gen(moveFloatToGeneral{regN=V0, regD=X0}, cvec);
                gen(logicalShiftLeft{wordSize=WordSize64, shift=0w32, regN=X0, regD=X0}, cvec);
                gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
           )

        |   genUnary(FloatToDouble, arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                gen(logicalShiftRight{wordSize=WordSize64, shift=0w32, regN=X0, regD=X0}, cvec);
                gen(moveGeneralToFloat{regN=X0, regD=V0}, cvec);
                gen(convertFloatToDouble{regN=V0, regD=V0}, cvec);
                boxDouble(V0, cvec)
            )

        |   genUnary(DoubleToFloat, arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                (* Convert double to float using current rounding mode. *)
                gen(loadRegScaledDouble{regT=V0, regN=X0, unitOffset=0}, cvec);
                gen(convertDoubleToFloat{regN=V0, regD=V0}, cvec);
                gen(moveFloatToGeneral{regN=V0, regD=X0}, cvec);
                gen(logicalShiftLeft{wordSize=WordSize64, shift=0w32, regN=X0, regD=X0}, cvec);
                gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
            )

        |   genUnary(RealToInt (PrecDouble, rnding), arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                (* We could get an overflow in either the conversion to integer
                   or in the conversion to a tagged value.  Fortunately if the
                   conversion detects an overflow it sets the result to a
                   value that will cause an overflow in the addition. *)
                gen(loadRegScaledDouble{regT=V0, regN=X0, unitOffset=0}, cvec);
                gen(convertDoubleToInt rnding {regN=V0, regD=X0}, cvec);
                gen(addSShiftedReg{regM=X0, regN=X0, regD=X0, shift=ShiftNone}, cvec);
                gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec);
                checkOverflow cvec
            )

        |   genUnary(RealToInt (PrecSingle, rnding), arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                gen(logicalShiftRight{wordSize=WordSize64, shift=0w32, regN=X0, regD=X0}, cvec);
                gen(moveGeneralToFloat{regN=X0, regD=V0}, cvec);
                gen(convertFloatToInt rnding {regN=V0, regD=X0}, cvec);
                gen(addSShiftedReg{regM=X0, regN=X0, regD=X0, shift=ShiftNone}, cvec);
                gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec);
                checkOverflow cvec
            )

        |   genUnary(TouchAddress, arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                topInX0 := false (* Discard this *)
            )

        |   genUnary(AllocCStack, arg1, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                (* Allocate space on the stack.  The higher levels have already aligned
                   the size to a multiple of 16. *)
                (* Remove the tag and then use add-extended.  This can use SP unlike
                   the shifted case. *)
                gen(logicalShiftRight{wordSize=WordSize64, shift=0w1, regN=X0, regD=X0}, cvec);
                gen(subExtendedReg{regM=X0, regN=XSP, regD=XSP, extend=ExtUXTX 0w0}, cvec);
                (* The result is a large-word.  We can't box SP directly.
                   We have to use add here to get the SP into X1 instead of the usual move. *)
                gen(addImmediate{regN=XSP, regD=X1, immed=0w0, shifted=false}, cvec);
                boxLargeWord(X1, cvec)
            )

        and genBinary(WordComparison{test, isSigned}, arg1, arg2, loopAddr) =
            let
                val cond =
                    case (test, isSigned) of
                        (TestEqual,         _) => condEqual
                    |   (TestLess,          true) => condSignedLess
                    |   (TestLessEqual,     true) => condSignedLessEq
                    |   (TestGreater,       true) => condSignedGreater
                    |   (TestGreaterEqual,  true) => condSignedGreaterEq
                    |   (TestLess,          false) => condCarryClear
                    |   (TestLessEqual,     false) => condUnsignedLowOrEq
                    |   (TestGreater,       false) => condUnsignedHigher
                    |   (TestGreaterEqual,  false) => condCarrySet
                    |   (TestUnordered,     _) => raise InternalError "WordComparison: TestUnordered"
            in
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec); (* First argument. *)
                compareRegs(X1, X0, cvec);
                setBooleanCondition(X0, cond, cvec)
            end

        |   genBinary(PointerEq, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec); (* First argument. *)
                compareRegs(X1, X0, cvec);
                setBooleanCondition(X0, condEqual, cvec)
            )

        |   genBinary(FixedPrecisionArith ArithAdd, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                (* Subtract the tag bit. *)
                gen(subImmediate{regN=X0, regD=X0, immed=0w1, shifted=false}, cvec);
                genPopReg(X1, cvec);
                (* Add and set the flag bits *)
                gen(addSShiftedReg{regN=X1, regM=X0, regD=X0, shift=ShiftNone}, cvec);
                checkOverflow cvec
            )

        |   genBinary(FixedPrecisionArith ArithSub, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                (* Subtract the tag bit. *)
                gen(subImmediate{regN=X0, regD=X0, immed=0w1, shifted=false}, cvec);
                genPopReg(X1, cvec);
                (* Subtract and set the flag bits *)
                gen(subSShiftedReg{regN=X1, regM=X0, regD=X0, shift=ShiftNone}, cvec);
                checkOverflow cvec
            )

        |   genBinary(FixedPrecisionArith ArithMult, arg1, arg2, loopAddr) =
            let
                (* There's no simple way of detecting overflow.  We have to compute the
                   high-order word and then check that it is either all zeros with
                   the sign bit zero or all ones with the sign bit one. *)
                val noOverflow = createLabel()
            in
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
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

        |   genBinary(FixedPrecisionArith ArithQuot, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                (* The word version avoids an extra shift.  Don't do that here at least
                   for the moment.  Division by zero and overflow are checked for at
                   the higher level. *)
                genPopReg(X1, cvec);
                (* Shift to remove the tags on the arguments *)
                gen(arithmeticShiftRight{regN=X0, regD=X0, wordSize=WordSize64, shift=0w1}, cvec);
                gen(arithmeticShiftRight{regN=X1, regD=X1, wordSize=WordSize64, shift=0w1}, cvec);
                gen(signedDivide{regM=X0, regN=X1, regD=X0}, cvec);
                (* Restore the tag. *)
                gen(logicalShiftLeft{regN=X0, regD=X0, wordSize=WordSize64, shift=0w1}, cvec);
                gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
            )

        |   genBinary(FixedPrecisionArith ArithRem, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                (* For the moment we remove the tags and then retag afterwards.  The word
                   version avoids this but at least for the moment we do it the longer way. *)
                (* There's no direct way to get the remainder - have to use divide and multiply. *)
                genPopReg(X1, cvec);
                (* Shift to remove the tags on the arguments *)
                gen(arithmeticShiftRight{regN=X0, regD=X0, wordSize=WordSize64, shift=0w1}, cvec);
                gen(arithmeticShiftRight{regN=X1, regD=X1, wordSize=WordSize64, shift=0w1}, cvec);
                gen(signedDivide{regM=X0, regN=X1, regD=X2}, cvec);
                (* X0 = X1 - (X2/X0)*X0 *)
                gen(multiplyAndSub{regM=X2, regN=X0, regA=X1, regD=X0}, cvec);
                (* Restore the tag. *)
                gen(logicalShiftLeft{regN=X0, regD=X0, wordSize=WordSize64, shift=0w1}, cvec);
                gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
            )

        |   genBinary(FixedPrecisionArith ArithDiv, _, _, _) =
                raise InternalError "unimplemented operation: FixedPrecisionArith ArithDiv"

        |   genBinary(FixedPrecisionArith ArithMod, _, _, _) =
                raise InternalError "unimplemented operation: FixedPrecisionArith ArithMod"

        |   genBinary(WordArith ArithAdd, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                (* Subtract the tag bit. *)
                gen(subImmediate{regN=X0, regD=X0, immed=0w1, shifted=false}, cvec);
                genPopReg(X1, cvec);
                gen(addShiftedReg{regN=X1, regM=X0, regD=X0, shift=ShiftNone}, cvec)
            )

        |   genBinary(WordArith ArithSub, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                (* Subtract the tag bit. *)
                gen(subImmediate{regN=X0, regD=X0, immed=0w1, shifted=false}, cvec);
                genPopReg(X1, cvec);
                gen(subShiftedReg{regN=X1, regM=X0, regD=X0, shift=ShiftNone}, cvec)
            )

        |   genBinary(WordArith ArithMult, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                (* Shift to remove the tags on one argument. *)
                gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize64, shift=0w1}, cvec);
                (* Remove the tag on the other. *)
                gen(bitwiseAndImmediate{regN=X1, regD=X1, wordSize=WordSize64, bits=tagBitMask}, cvec);
                gen(multiplyAndAdd{regM=X1, regN=X0, regA=XZero, regD=X0}, cvec);
                (* Put back the tag. *)
                gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
            )

        |   genBinary(WordArith ArithDiv, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
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

        |   genBinary(WordArith ArithMod, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
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

        |   genBinary(WordArith _, _, _, _) = raise InternalError "WordArith - unimplemented instruction"

        |   genBinary(WordLogical LogicalAnd, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                (* Since they're both tagged the tag bit is preserved. *)
                gen(andShiftedReg{regN=X1, regM=X0, regD=X0, shift=ShiftNone}, cvec)
            )

        |   genBinary(WordLogical LogicalOr, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                (* Since they're both tagged the tag bit is preserved. *)
                gen(orrShiftedReg{regN=X1, regM=X0, regD=X0, shift=ShiftNone}, cvec)
            )

        |   genBinary(WordLogical LogicalXor, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                (* Have to restore the tag bit because that will be cleared. *)
                gen(eorShiftedReg{regN=X1, regM=X0, regD=X0, shift=ShiftNone}, cvec);
                gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
            )

            (* Shifts: ARM64 shifts are taken modulo the word length but that's
               dealt with at a higher level. *)
        |   genBinary(WordShift ShiftLeft, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                (* Remove the tag from value we're shifting. *)
                gen(bitwiseAndImmediate{regN=X1, regD=X1, wordSize=WordSize64, bits=tagBitMask}, cvec);
                (* Untag the shift amount.  Can use 32-bit op here. *)
                gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize32, shift=0w1}, cvec);
                gen(logicalShiftLeftVariable{regM=X0, regN=X1, regD=X0}, cvec);
                (* Put back the tag. *)
                gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
            )

        |   genBinary(WordShift ShiftRightLogical, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                (* Don't need to remove the tag. *)
                (* Untag the shift amount.  Can use 32-bit op here. *)
                gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize32, shift=0w1}, cvec);
                gen(logicalShiftRightVariable{regM=X0, regN=X1, regD=X0}, cvec);
                (* Put back the tag. *)
                gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
            )

        |   genBinary(WordShift ShiftRightArithmetic, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                (* Don't need to remove the tag. *)
                (* Untag the shift amount.  Can use 32-bit op here. *)
                gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize32, shift=0w1}, cvec);
                gen(arithmeticShiftRightVariable{regM=X0, regN=X1, regD=X0}, cvec);
                (* Put back the tag. *)
                gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
            )

        |   genBinary(AllocateByteMemory, arg1, arg2, loopAddr) =
            (* Allocate memory for byte data.  Unlike for word data it is not necessary to
               initialise it before any further allocation provided it has the mutable bit
               set. *)
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                (* Load and untag the size and flags.  The size is the number of words even
                   though this is byte data. *)
                gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize32 (*byte*), shift=0w1}, cvec);
                genPopReg(X1, cvec);
                gen(logicalShiftRight{regN=X1, regD=X1, wordSize=WordSize64, shift=0w1}, cvec);
                allocateVariableSize({sizeReg=X1, flagsReg=X0, resultReg=X2}, cvec);
                gen(moveRegToReg{sReg=X2, dReg=X0}, cvec)
            )

        |   genBinary(LargeWordComparison test, arg1, arg2, loopAddr) =
            let
                val cond =
                    case test of
                        TestEqual => condEqual
                    |   TestLess => condCarryClear
                    |   TestLessEqual => condUnsignedLowOrEq
                    |   TestGreater => condUnsignedHigher
                    |   TestGreaterEqual => condCarrySet
                    |   TestUnordered => raise InternalError "LargeWordComparison: TestUnordered"
            in
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                (* The values are boxed so have to be loaded first. *)
                genPopReg(X1, cvec);
                gen(loadRegScaled{regT=X0, regN=X0, unitOffset=0}, cvec);
                gen(loadRegScaled{regT=X1, regN=X1, unitOffset=0}, cvec);
                compareRegs(X1, X0, cvec);
                setBooleanCondition(X0, cond, cvec)
            end

        |   genBinary(LargeWordArith ArithAdd, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                gen(loadRegScaled{regT=X0, regN=X0, unitOffset=0}, cvec);
                gen(loadRegScaled{regT=X1, regN=X1, unitOffset=0}, cvec);
                gen(addShiftedReg{regN=X1, regM=X0, regD=X1, shift=ShiftNone}, cvec);
                boxLargeWord(X1, cvec)
            )

        |   genBinary(LargeWordArith ArithSub, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                gen(loadRegScaled{regT=X0, regN=X0, unitOffset=0}, cvec);
                gen(loadRegScaled{regT=X1, regN=X1, unitOffset=0}, cvec);
                gen(subShiftedReg{regN=X1, regM=X0, regD=X1, shift=ShiftNone}, cvec);
                boxLargeWord(X1, cvec)
            )

        |   genBinary(LargeWordArith ArithMult, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                gen(loadRegScaled{regT=X0, regN=X0, unitOffset=0}, cvec);
                gen(loadRegScaled{regT=X1, regN=X1, unitOffset=0}, cvec);
                gen(multiplyAndAdd{regM=X1, regN=X0, regA=XZero, regD=X1}, cvec);
                boxLargeWord(X1, cvec)
            )

        |   genBinary(LargeWordArith ArithDiv, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                gen(loadRegScaled{regT=X0, regN=X0, unitOffset=0}, cvec);
                gen(loadRegScaled{regT=X1, regN=X1, unitOffset=0}, cvec);
                gen(unsignedDivide{regM=X0, regN=X1, regD=X1}, cvec);
                boxLargeWord(X1, cvec)
            )

        |   genBinary(LargeWordArith ArithMod, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                gen(loadRegScaled{regT=X0, regN=X0, unitOffset=0}, cvec);
                gen(loadRegScaled{regT=X1, regN=X1, unitOffset=0}, cvec);
                gen(unsignedDivide{regM=X0, regN=X1, regD=X2}, cvec);
                gen(multiplyAndSub{regM=X2, regN=X0, regA=X1, regD=X1}, cvec);
                boxLargeWord(X1, cvec)
            )

        |   genBinary(LargeWordArith _, _, _, _) =
                raise InternalError "LargeWordArith - unimplemented instruction"

        |   genBinary(LargeWordLogical LogicalAnd, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                gen(loadRegScaled{regT=X0, regN=X0, unitOffset=0}, cvec);
                gen(loadRegScaled{regT=X1, regN=X1, unitOffset=0}, cvec);
                gen(andShiftedReg{regN=X1, regM=X0, regD=X1, shift=ShiftNone}, cvec);
                boxLargeWord(X1, cvec)
            )

        |   genBinary(LargeWordLogical LogicalOr, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                gen(loadRegScaled{regT=X0, regN=X0, unitOffset=0}, cvec);
                gen(loadRegScaled{regT=X1, regN=X1, unitOffset=0}, cvec);
                gen(orrShiftedReg{regN=X1, regM=X0, regD=X1, shift=ShiftNone}, cvec);
                boxLargeWord(X1, cvec)
            )

        |   genBinary(LargeWordLogical LogicalXor, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                gen(loadRegScaled{regT=X0, regN=X0, unitOffset=0}, cvec);
                gen(loadRegScaled{regT=X1, regN=X1, unitOffset=0}, cvec);
                gen(eorShiftedReg{regN=X1, regM=X0, regD=X1, shift=ShiftNone}, cvec);
                boxLargeWord(X1, cvec)
            )

            (* The shift is always a Word.word value i.e. tagged.  There is a check at the higher level
               that the shift does not exceed 32/64 bits. *)
        |   genBinary(LargeWordShift ShiftLeft, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                gen(loadRegScaled{regT=X1, regN=X1, unitOffset=0}, cvec);
                (* Untag the shift amount.  Can use 32-bit op here. *)
                gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize32, shift=0w1}, cvec);
                gen(logicalShiftLeftVariable{regM=X0, regN=X1, regD=X1}, cvec);
                boxLargeWord(X1, cvec)
            )

        |   genBinary(LargeWordShift ShiftRightLogical, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                gen(loadRegScaled{regT=X1, regN=X1, unitOffset=0}, cvec);
                (* Untag the shift amount.  Can use 32-bit op here. *)
                gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize32, shift=0w1}, cvec);
                gen(logicalShiftRightVariable{regM=X0, regN=X1, regD=X1}, cvec);
                boxLargeWord(X1, cvec)
            )

        |   genBinary(LargeWordShift ShiftRightArithmetic, arg1, arg2, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                gen(loadRegScaled{regT=X1, regN=X1, unitOffset=0}, cvec);
                (* Untag the shift amount.  Can use 32-bit op here. *)
                gen(logicalShiftRight{regN=X0, regD=X0, wordSize=WordSize32, shift=0w1}, cvec);
                gen(arithmeticShiftRightVariable{regM=X0, regN=X1, regD=X1}, cvec);
                boxLargeWord(X1, cvec)
            )

            (* Floating point comparisons.
               The fcmp instruction differs from integer comparison.  If either
               argument is a NaN the overflow bit is set and the other bits are
               cleared.  That means that in order to get a true result only
               if the values are not NaNs we have to test that at least one of
               C, N, or Z are set.  We use unsigned tests for < and <=
               and signed tests for > and >=. *)
        |   genBinary(RealComparison (test, PrecDouble), arg1, arg2, loopAddr) =
            let
                val cond =
                    case test of
                        TestEqual => condEqual
                    |   TestLess => condCarryClear
                    |   TestLessEqual => condUnsignedLowOrEq
                    |   TestGreater => condSignedGreater
                    |   TestGreaterEqual => condSignedGreaterEq
                    |   TestUnordered => condOverflow
            in
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                gen(loadRegScaledDouble{regT=V0, regN=X0, unitOffset=0}, cvec);
                gen(loadRegScaledDouble{regT=V1, regN=X1, unitOffset=0}, cvec);
                gen(compareDouble{regM=V0, regN=V1}, cvec);
                setBooleanCondition(X0, cond, cvec)
            end

        |   genBinary(RealComparison (test, PrecSingle), arg1, arg2, loopAddr) =
            let
                val cond =
                    case test of
                        TestEqual => condEqual
                    |   TestLess => condCarryClear
                    |   TestLessEqual => condUnsignedLowOrEq
                    |   TestGreater => condSignedGreater
                    |   TestGreaterEqual => condSignedGreaterEq
                    |   TestUnordered => condOverflow
            in
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                gen(logicalShiftRight{wordSize=WordSize64, shift=0w32, regN=X0, regD=X0}, cvec);
                gen(logicalShiftRight{wordSize=WordSize64, shift=0w32, regN=X1, regD=X1}, cvec);
                gen(moveGeneralToFloat{regN=X0, regD=V0}, cvec);
                gen(moveGeneralToFloat{regN=X1, regD=V1}, cvec);
                gen(compareFloat{regM=V0, regN=V1}, cvec);
                setBooleanCondition(X0, cond, cvec)
            end

        |   genBinary(RealArith (oper, PrecDouble), arg1, arg2, loopAddr) =
            let
                val operation =
                    case oper of
                        ArithAdd => addDouble
                    |   ArithSub => subtractDouble
                    |   ArithMult => multiplyDouble
                    |   ArithDiv => divideDouble
                    |   _ => raise InternalError "RealArith - unimplemented instruction"
            in
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                gen(loadRegScaledDouble{regT=V0, regN=X0, unitOffset=0}, cvec);
                gen(loadRegScaledDouble{regT=V1, regN=X1, unitOffset=0}, cvec);
                gen(operation{regM=V0, regN=V1, regD=V0}, cvec);
                boxDouble(V0, cvec)
            end

        |   genBinary(RealArith (oper, PrecSingle), arg1, arg2, loopAddr) =
            let
                val operation =
                    case oper of
                        ArithAdd => addFloat
                    |   ArithSub => subtractFloat
                    |   ArithMult => multiplyFloat
                    |   ArithDiv => divideFloat
                    |   _ => raise InternalError "RealArith - unimplemented instruction"
            in
                (* 32-bit floats are represented as the value in the top 32-bits of
                   a general register with the low-order word containing all zeros
                   except the bottom bit which is one. *)
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec);
                gen(logicalShiftRight{wordSize=WordSize64, shift=0w32, regN=X0, regD=X0}, cvec);
                gen(logicalShiftRight{wordSize=WordSize64, shift=0w32, regN=X1, regD=X1}, cvec);
                gen(moveGeneralToFloat{regN=X0, regD=V0}, cvec);
                gen(moveGeneralToFloat{regN=X1, regD=V1}, cvec);
                gen(operation{regM=V0, regN=V1, regD=V0}, cvec);
                gen(moveFloatToGeneral{regN=V0, regD=X0}, cvec);
                gen(logicalShiftLeft{wordSize=WordSize64, shift=0w32, regN=X0, regD=X0}, cvec);
                gen(bitwiseOrImmediate{regN=X0, regD=X0, wordSize=WordSize64, bits=0w1}, cvec)
            end

        |   genBinary(FreeCStack, arg1, arg2, loopAddr) =
            (* Free space on the C stack. This is a binary operation that takes the base address
               and the size.  The base address isn't used in this version. *)
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec); (* Pop and discard the address *)
                (* Can't use the shifted addition which would remove the tag as part of the add. *)
                gen(logicalShiftRight{wordSize=WordSize64, shift=0w1, regN=X0, regD=X0}, cvec);
                gen(addExtendedReg{regM=X0, regN=XSP, regD=XSP, extend=ExtUXTX 0w0}, cvec)
            )

        |   genBinary(AtomicExchangeAdd, arg1, arg2, loopAddr) =
            (* The earliest versions of the Arm8 do not have the LDADD instruction which
               will do this directly.  To preserve compatibility we use LDAXR/STLXR
               which require a loop. *)
            let
                val loopLabel = createLabel()
            in
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                genPopReg(X1, cvec); (* Address of mutex *)
                (* Untag the value to add. *)
                gen(subImmediate{regN=X0, regD=X3, immed=0w1, shifted=false}, cvec);
                gen(setLabel loopLabel, cvec);
                (* Get the original value into X0. *)
                gen(loadAcquireExclusiveRegister{regN=X1, regT=X0}, cvec);
                (* Add and put the result into X3 *)
                gen(addShiftedReg{regM=X0, regN=X3, regD=X2, shift=ShiftNone}, cvec);
                (* Store the result of the addition. W4 will be zero if this succeeded. *)
                gen(storeReleaseExclusiveRegister{regS=X4, regT=X2, regN=X1}, cvec);
                gen(compareBranchNonZero(X4, WordSize32, loopLabel), cvec);
                (* Put in the memory barrier. *)
                gen(dmbIsh, cvec)
            end

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
                    val () = genAllocateFixedSize(closureVars+1, F_mutable, X0, X1, cvec)
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
                    val () = genList(loadScaledWord{dest=X1, base=X_MLStackPtr, work=X16, offset= !realstackptr - entryAddr}, cvec)

                    (* Load items for the closure. *)
                    fun loadItems ([], _) = ()
                    |   loadItems (v :: vs, addr : int) =
                    (
                        (* Generate an item and move it into the closure *)
                        gencde (BICExtract v, ToX0, NotEnd, NONE);
                        (* The closure "address" excludes the code address. *)
                        genList(storeScaledWord{store=X0, base=X1, work=X16, offset=addr+1}, cvec);
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
                    genAllocateFixedSize(closureVars+1, 0w0, X0, X1, cvec);
                    List.foldl(fn (_, w) =>
                        (
                            genPopReg(X1, cvec);
                            genList(storeScaledWord{store=X1, base=X0, work=X16, offset=w-1}, cvec);
                            w-1
                        )
                    ) (closureVars+1) closure;
                    gen(loadAddressConstant(X1, codeAddr), cvec);
                    gen(storeRegScaled{regT=X1, regN=X0, unitOffset=0}, cvec);
                    genPushReg(X0, cvec);
                    realstackptr := !realstackptr - closureVars + 1 (* Popped the closure vars and pushed the address. *)
                end
            end

        and genCond (testCode, thenCode, elseCode, whereto, tailKind, loopAddr) =
        let
            val toElse = createLabel() and exitJump = createLabel()
            val () = genTest(testCode, false, toElse, loopAddr)
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

        (* andalso and orelse are turned into conditionals with constants.
           Convert this into a series of tests. *)
        and genTest(BICConstnt(w, _), jumpOn, targetLabel, _) =
            let
                val cVal = case toShort w of 0w0 => false | 0w1 => true | _ => raise InternalError "genTest"
            in
                if cVal = jumpOn
                then gen(conditionalBranch (condAlways, targetLabel), cvec)
                else ()
            end

        |   genTest(BICUnary { oper=NotBoolean, arg1 }, jumpOn, targetLabel, loopAddr) =
                genTest(arg1, not jumpOn, targetLabel, loopAddr)

        |   genTest(BICUnary { oper=IsTaggedValue, arg1 }, jumpOn, targetLabel, loopAddr) =
            (
                gencde (arg1, ToX0, NotEnd, loopAddr);
                topInX0 := false;
                gen((if jumpOn then testBitBranchNonZero else testBitBranchZero)
                    (X0, 0w0, targetLabel), cvec)
            )

        |   genTest(BICBinary{oper=WordComparison{test, isSigned}, arg1, arg2}, jumpOn, targetLabel, loopAddr) =
            let
                val (cond, condNot) =
                    case (test, isSigned) of
                        (TestEqual,         _) => (condEqual, condNotEqual)
                    |   (TestLess,          true) => (condSignedLess, condSignedGreaterEq)
                    |   (TestLessEqual,     true) => (condSignedLessEq, condSignedGreater)
                    |   (TestGreater,       true) => (condSignedGreater, condSignedLessEq)
                    |   (TestGreaterEqual,  true) => (condSignedGreaterEq, condSignedLess)
                    |   (TestLess,          false) => (condCarryClear, condCarrySet)
                    |   (TestLessEqual,     false) => (condUnsignedLowOrEq, condUnsignedHigher)
                    |   (TestGreater,       false) => (condUnsignedHigher, condUnsignedLowOrEq)
                    |   (TestGreaterEqual,  false) => (condCarrySet, condCarryClear)
                    |   (TestUnordered,     _) => raise InternalError "WordComparison: TestUnordered"
            in
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                topInX0 := false;
                genPopReg(X1, cvec); (* First argument. *)
                compareRegs(X1, X0, cvec);
                gen(conditionalBranch(if jumpOn then cond else condNot, targetLabel), cvec)
            end

        |   genTest(BICBinary{oper=PointerEq, arg1, arg2}, jumpOn, targetLabel, loopAddr) =
            (
                gencde (arg1, ToStack, NotEnd, loopAddr);
                gencde (arg2, ToX0, NotEnd, loopAddr);
                decsp();
                topInX0 := false;
                genPopReg(X1, cvec); (* First argument. *)
                compareRegs(X1, X0, cvec);
                gen(conditionalBranch(if jumpOn then condEqual else condNotEqual, targetLabel), cvec)
            )

        |   genTest(BICTagTest { test, tag=tagValue, ... }, jumpOn, targetLabel, loopAddr) =
            (
                gencde (test, ToX0, NotEnd, loopAddr);
                topInX0 := false;
                gen(subSImmediate{regN=X0, regD=XZero, immed=taggedWord tagValue, shifted=false}, cvec);
                gen(conditionalBranch(if jumpOn then condEqual else condNotEqual, targetLabel), cvec)
            )

        |   genTest(BICCond (testPart, thenPart, elsePart), jumpOn, targetLabel, loopAddr) =
            let
                val toElse = createLabel() and exitJump = createLabel()
            in
                genTest(testPart, false, toElse, loopAddr);
                genTest(thenPart, jumpOn, targetLabel, loopAddr);
                gen(conditionalBranch (condAlways, exitJump), cvec);
                gen(setLabel toElse, cvec);
                genTest(elsePart, jumpOn, targetLabel, loopAddr);
                gen(setLabel exitJump, cvec)
            end

        |   genTest(testCode, jumpOn, targetLabel, loopAddr) =
            (
                gencde (testCode, ToStack, NotEnd, loopAddr);
                genPopReg(X0, cvec);
                gen(subSImmediate{regN=X0, regD=XZero, immed=taggedWord 0w1, shifted=false}, cvec);
                gen(conditionalBranch(if jumpOn then condEqual else condNotEqual, targetLabel), cvec);
                decsp() (* conditional branch pops a value. *)
            )

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
                    val () = genList(loadScaledWord{dest=X0, base=X_MLStackPtr, work=X16, offset=argsToPass}, cvec)
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
                        then genList(loadScaledWord{dest=reg, base=X_MLStackPtr, work=X16, offset=argsToPass-n-1}, cvec)
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
                    val () = genList(loadScaledWord{dest=X30, base=X_MLStackPtr, work=X16, offset= !realstackptr}, cvec)

                    (* Load the register arguments *)
                    fun loadArg(n, reg) =
                        if argsToPass > n
                        then genList(loadScaledWord{dest=reg, base=X_MLStackPtr, work=X16, offset=argsToPass-n-1}, cvec)
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
                        val () = genList(storeScaledWord{store=X9, base=X_MLStackPtr, work=X16, offset=destOffset}, cvec)
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
        codegen (body, name, closure, List.length argTypes, localCount, parameters)


    structure Foreign = Arm64Foreign

    structure Sharing =
    struct
        open BackendTree.Sharing
        type closureRef = closureRef
    end

end;
