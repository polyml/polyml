(*
    Copyright David C. J. Matthews 2021

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

functor Arm64CodetreeToICode(
    structure BackendTree: BACKENDINTERMEDIATECODE
    structure Arm64ICode: ARM64ICODE
    structure Debug: DEBUG
    structure Arm64Foreign: FOREIGNCALL
    structure ICodeTransform: ARM64ICODETRANSFORM
    structure CodeArray: CODEARRAY
    
    structure Arm64Fallback: GENCODE
    structure Pretty:PRETTY

    sharing Arm64ICode.Sharing = ICodeTransform.Sharing = CodeArray.Sharing = Arm64Fallback.Sharing = BackendTree.Sharing
): GENCODE =
struct
    open BackendTree
    open Address
    open Arm64ICode
    open CodeArray
    open BuiltIns
    
    exception InternalError = Misc.InternalError
    
    fun taggedWord64 w: Word64.word = w * 0w2 + 0w1
    and taggedWord w: word = w * 0w2 + 0w1
    
    datatype blockStruct =
        BlockSimple of arm64ICode
    |   BlockExit of arm64ICode
    |   BlockLabel of int
    |   BlockFlow of controlFlow
    |   BlockBegin of { regArgs: (preg * xReg) list, stackArgs: stackLocn list }
    |   BlockRaiseAndHandle of arm64ICode * int
    |   BlockOptionalHandle of {call: arm64ICode, handler: int, label: int }


    val moveRegister = BlockSimple o MoveRegister
    and loadNonAddressConstant = BlockSimple o LoadNonAddressConstant
    and loadAddressConstant = BlockSimple o LoadAddressConstant
    and loadWithConstantOffset = BlockSimple o LoadWithConstantOffset
    and loadWithIndexedOffset = BlockSimple o LoadWithIndexedOffset
    and loadMemReg = BlockSimple o LoadMemReg
    and objectIndexAddressToAbsolute = BlockSimple o ObjectIndexAddressToAbsolute
    and absoluteToObjectIndex = BlockSimple o AbsoluteToObjectIndex
    and allocateMemoryFixed = BlockSimple o AllocateMemoryFixed
    and allocateMemoryVariable = BlockSimple o AllocateMemoryVariable
    and initialiseMem = BlockSimple o InitialiseMem
    and storeWithConstantOffset = BlockSimple o StoreWithConstantOffset
    and storeWithIndexedOffset = BlockSimple o StoreWithIndexedOffset
    and addSubImmediate = BlockSimple o AddSubImmediate
    and addSubRegister = BlockSimple o AddSubRegister
    and logicalImmediate = BlockSimple o LogicalImmediate
    and logicalRegister = BlockSimple o LogicalRegister
    and shiftRegister = BlockSimple o ShiftRegister
    and multiplication = BlockSimple o Multiplication
    and division = BlockSimple o Division
    and pushToStack = BlockSimple o PushToStack
    and loadStack = BlockSimple o LoadStack
    and storeToStack = BlockSimple o StoreToStack
    and containerAddress = BlockSimple o ContainerAddress
    and resetStackPtr = BlockSimple o ResetStackPtr
    and tagValue = BlockSimple o TagValue
    and untagValue = BlockSimple o UntagValue
    and boxLarge = BlockSimple o BoxLarge
    and unboxLarge = BlockSimple o UnboxLarge
    and boxTagFloat = BlockSimple o BoxTagFloat
    and unboxTagFloat = BlockSimple o UnboxTagFloat
    and loadAcquire = BlockSimple o LoadAcquire
    and storeRelease = BlockSimple o StoreRelease
    and bitFieldShift = BlockSimple o BitFieldShift
    and bitFieldInsert = BlockSimple o BitFieldInsert

    val shiftConstant = BlockSimple o shiftConstant

    (* Many operations use 32-bit arguments in 32-in-64 and 64-bit in native 64. *)
    val polyWordLoadSize = if is32in64 then Load32 else Load64
    val polyWordOpSize = if is32in64 then OpSize32 else OpSize64

    val tagBitMask64 = Word64.<<(Word64.fromInt ~1, 0w1)
    val tagBitMask32 = Word64.andb(tagBitMask64, 0wxffffffff)

    val polyWordTagBitMask = if is32in64 then tagBitMask32 else tagBitMask64

    (* The flags byte is the high-order byte of length word. *)
    val flagsByteOffset = if isBigEndian then ~ (Word.toInt wordSize) else ~1

    fun opWordSize Load64 = 8
    |   opWordSize Load32 = 4
    |   opWordSize Load16 = 2
    |   opWordSize Load8 = 1

    fun codeFunctionToArm64({body, localCount, name, argTypes, closure, ...}:bicLambdaForm, debugSwitches, resultClosure) =
    let
        (* Pseudo-registers are allocated sequentially and the properties added to the list. *)
        val pregCounter = ref 0
        val pregPropList = ref []
        fun newPReg() =
        let
            val regNo = !pregCounter before pregCounter := !pregCounter + 1
            val () = pregPropList := RegPropGeneral :: !pregPropList
        in
            PReg regNo
        end
        
        and newUReg() =
        let
            val regNo = !pregCounter before pregCounter := !pregCounter + 1
            val () = pregPropList := RegPropUntagged :: !pregPropList
        in
            PReg regNo
        end
        
        and newStackLoc size =
        let
            val regNo = !pregCounter before pregCounter := !pregCounter + 1
            val () = pregPropList := RegPropStack size :: !pregPropList
        in
            StackLoc{size=size, rno=regNo}
        end
        
        and newMergeReg() =
        let
            val regNo = !pregCounter before pregCounter := !pregCounter + 1
            val () = pregPropList := RegPropMultiple :: !pregPropList
        in
            PReg regNo
        end
        
        datatype locationValue =
            NoLocation
        |   PregLocation of preg
        |   ContainerLocation of { container: stackLocn, stackOffset: int }

        val locToPregArray = Array.array(localCount, NoLocation)
        val labelCounter = ref 1 (* Start at 1.  Zero is used for the root. *)
        fun newLabel() = !labelCounter before labelCounter := !labelCounter + 1
        val ccRefCounter = ref 0
        fun newCCRef() = CcRef(!ccRefCounter) before ccRefCounter := !ccRefCounter + 1

        (* The profile object is a single mutable with the F_bytes bit set. *)        
        val profileObject = CodeArray.createProfileObject()

        (* Switch to indicate if we want to trace where live data has been allocated. *)
        (* TODO: This should be used in AllocateMemoryOperation and BoxValue and possibly AllocateMemoryVariable. *)
        val addAllocatingFunction =
            Debug.getParameter Debug.profileAllocationTag debugSwitches = 1

        datatype destination =
            SpecificPReg of preg
        |   NoResult
        |   AnyReg

        (* Context type. *)
        type context =
            { loopArgs: (preg list * int * int) option, stackPtr: int, currHandler: int option,
              overflowBlock: int option ref }

        datatype argLoc =
            ArgumentIsInReg of { realReg: xReg, argReg: preg }
        |   ArgumentIsOnStack of { stackOffset: int, stackReg: stackLocn }

        (* An address as either suitable for Load/StoreWithConstantOffset or else Load/StoreWithIndexedOffset. *)
        datatype addressKind =
            AddrOffset of {base: preg, offset: int}
        |   AddrIndex of {base: preg, index: preg}

        (* Pseudo-regs for the result, the closure and the args that were passed in real regs. *)
        val resultTarget = newPReg()
        val closureRegAddr = newPReg()
        val returnAddrReg = newPReg()
        
        val generalArgRegs = [X0, X1, X2, X3, X4, X5, X6, X7]

        (* Create a map for the arguments indicating their register or stack location. *)
        local
            (* Select the appropriate argument register depending on the argument type. *)
            fun argTypesToArgEntries([], _, _) = ([], [], [], [])

            |   argTypesToArgEntries(_ :: tl, gReg :: gRegs, n) =
                (* This deals with general arguments but also with extra floating point arguments.
                   They are boxed as usual. *)
                let
                    val (argTypes, argCode, argRegs, stackArgs) =
                        argTypesToArgEntries(tl, gRegs, n-1)
                    val argReg=newPReg()
                in
                    (ArgumentIsInReg{realReg=gReg, argReg=argReg} :: argTypes, argCode, (argReg, gReg) :: argRegs, stackArgs)
                end

            |   argTypesToArgEntries(_ :: tl, [], n) =
                let
                    val (argTypes, argCode, argRegs, stackArgs) = argTypesToArgEntries(tl, [], n-1)
                    val stackLoc = newStackLoc 1
                in
                    (ArgumentIsOnStack {stackOffset=n, stackReg = stackLoc } :: argTypes, argCode, argRegs, stackLoc :: stackArgs)
                end

            val (argEntries, argCode, argRegs, stackArguments) =
                argTypesToArgEntries(argTypes, generalArgRegs, List.length argTypes)
            val clReg = case closure of [] => [] | _ => [(closureRegAddr, X8)]
            val retReg = [(returnAddrReg, X30)]
        in
            val argumentVector = Vector.fromList argEntries

            (* Start code for the function. *)
            val beginInstructions = argCode @
                [BlockBegin{regArgs=retReg @ clReg @ argRegs, stackArgs=stackArguments }]

            (* The number of arguments on the stack.  Needed in return instrs and tail calls. *)
            val currentStackArgs = List.length stackArguments
        end

        fun returnInstruction({stackPtr, ...}, resReg, tailCode) =
        let
        in
            BlockExit(ReturnResultFromFunction{resultReg=resReg, returnReg = returnAddrReg, numStackArgs=currentStackArgs}) ::
                (if stackPtr <> 0 then resetStackPtr{numWords=stackPtr} :: tailCode else tailCode)
        end

        fun asTarget(SpecificPReg preg) = preg
        |   asTarget _ = newPReg()

        fun moveToResult(SpecificPReg tReg, code, sReg) =
                (moveRegister{source=sReg, dest=tReg} :: code, tReg, false)
        |   moveToResult(AnyReg, code, sReg) = (code, sReg, false)
        |   moveToResult(NoResult, code, sReg) =
            let
                val tReg = newPReg()
            in
                (moveRegister{source=sReg, dest=tReg} :: code, tReg, false)
            end

        (* Store a register at a given offset.  This may have to use an index register
           if the offset is too large. *)
        fun storeAtWordOffset(toStore, offset, base, loadSize, tailCode) =
        let
            val wSize = opWordSize loadSize
            val byteOffset = offset*wSize
        in
            if offset < 4096 andalso byteOffset > ~256 
            then storeWithConstantOffset{base=base, source=toStore,
                            byteOffset=byteOffset, loadType=loadSize} :: tailCode
            else
            let
                val indexReg = newUReg()
            in
                storeWithIndexedOffset{ base=base, source=toStore, index=indexReg, loadType=loadSize } ::
                    loadNonAddressConstant{ source=LargeWord.fromInt offset, dest=indexReg } :: tailCode
            end
        end

        (* Allocate a fixed size cell with a reference to the profile object if we want
           to trace the location of live data.  Currently only used for tuples and closures. *)
        fun allocateWithProfileRev(n, flags, memAddr, tlCode) =
        let
            fun doAllocation(words, flags, tlCode) =
            let
                val wordsRequired =
                    if is32in64
                    then (* Have to round this up to 8 bytes *)
                        Word64.andb(Word64.fromInt(words+2), ~ 0w2)
                    else Word64.fromInt(words+1)
                val bytesRequired = Word64.fromLarge(Word.toLarge wordSize) * wordsRequired
                val lengthWord =
                    Word64.orb(Word64.fromInt words, Word64.<<(Word64.fromLarge(Word8.toLarge flags),
                        if is32in64 then 0w24 else 0w56))
                val lengthReg = newUReg()
            in
                storeWithConstantOffset{ source=lengthReg, base=memAddr, byteOffset= ~(Word.toInt wordSize), loadType=polyWordLoadSize } ::
                loadNonAddressConstant{ source=lengthWord, dest=lengthReg } ::
                allocateMemoryFixed{bytesRequired=bytesRequired, dest=memAddr, saveRegs=[]} :: tlCode
            end
        in
            if addAllocatingFunction
            then
            let
                val profReg = newPReg()
            in
                storeAtWordOffset(profReg, n, memAddr, polyWordLoadSize,
                    loadAddressConstant{ source=profileObject, dest=profReg} ::
                        doAllocation(n+1, Word8.orb(flags, Address.F_profile), tlCode))
            end
            else doAllocation(n, flags, tlCode)
        end

        (* Return a unit result. *)
        fun returnUnit(target, code, exit) =
        let
            val tReg = asTarget target
        in
            (loadNonAddressConstant{source=taggedWord64 0w0, dest=tReg} :: code, tReg, exit)
        end

        (* Create a bool result from a test by returning true or false. *)
        fun makeBoolResultRev(condition, ccRef, target, testCode) =
        let
            val trueLab = newLabel() and falseLab = newLabel() and mergeLab = newLabel()
            val mergeReg = newMergeReg()
        in
            moveRegister{dest=target, source=mergeReg} ::
            BlockLabel mergeLab ::
            BlockFlow(Unconditional mergeLab) ::
            loadNonAddressConstant{dest=mergeReg, source=taggedWord64 0w0} ::
            BlockLabel falseLab ::
            BlockFlow(Unconditional mergeLab) ::
            loadNonAddressConstant{dest=mergeReg, source=taggedWord64 0w1} ::
            BlockLabel trueLab ::
            BlockFlow(Conditional{ ccRef=ccRef, condition=condition, trueJump=trueLab, falseJump=falseLab }) ::
            testCode
        end

        (* Return an absolute address in both native addressing and 32-in-64. *)
        fun getAbsoluteAddress(code, baseReg) =
            if is32in64
            then let val absReg = newUReg() in (objectIndexAddressToAbsolute{ source=baseReg, dest=absReg } :: code, absReg) end
            else (code, baseReg)

        (* Load a value aligned on a 64 or 32-bit boundary.  offset is the number
           of units.  Typically this will be a polyword. *)
        fun wordAddressOffset(destination, baseReg1, offset, loadOp, code) =
        let
            val dReg = asTarget destination
            val opWordSize = opWordSize loadOp
            val byteOffset = offset * opWordSize
            val (codeBase, baseReg) = getAbsoluteAddress(code, baseReg1)
            val code =
                if offset < 4096 andalso byteOffset > ~256
                then loadWithConstantOffset{base=baseReg, dest=dReg, byteOffset=byteOffset, loadType=loadOp} :: codeBase
                else
                let
                    val indexReg = newUReg()
                in
                    loadWithIndexedOffset{ base=baseReg, dest=dReg, index=indexReg, loadType=loadOp } ::
                        loadNonAddressConstant{ source=LargeWord.fromInt offset, dest=indexReg } :: codeBase
                end
        in
            (code, dReg, false)
        end

        (* See if we have a container and return the entry if present. *)
        fun getContainerIfPresent(BICExtract(BICLoadLocal l)) =
            ( case Array.sub(locToPregArray, l) of ContainerLocation container => SOME container | _ => NONE )
        |   getContainerIfPresent _ = NONE

        (* General function for loads and stores.  We want this to be mutually recursive so need to pass codeToICodeRev
           explicitly *)
        fun loadAndStoreWithAddress codeValue
                ({base, index, offset}, getStoreValue, loadType, isCAddress, loadStoreOffset, loadStoreIndex, context, code) =
        let
            val opWordSize = opWordSize loadType
            val byteOffset = offset * opWordSize
            (* Get the base register value *)
            val (bCode, bReg1, _) = codeValue(base, context, false, AnyReg, code)
            (* If this is a store get the value to store here. *)
            val (sCode, sValue) = getStoreValue bCode

            (* Get any index register value. *)
            val (iCode, iReg1Opt) =
                case index of
                    NONE =>
                        if offset < 4096 andalso byteOffset > ~256
                        then (sCode, NONE) (* We can use this offset. *)
                        else
                        let
                            val iReg = newUReg()
                        in
                            (loadNonAddressConstant{ source=LargeWord.fromInt offset, dest=iReg } :: sCode, SOME iReg)
                        end
                |   SOME index =>
                        let
                            val (iCode1, iReg1, _) = codeValue(index, context, false, AnyReg, sCode)
                            (* The index is a tagged integer containing the number of units (words, bytes etc).
                               It has to be untagged.  If this is a C address it may be negative. *)
                            val iReg2 = newUReg()
                            (* Logical shift if this is a Poly address, arithmetic shift if this is
                               a C address. *)
                            val iCode2 =
                                untagValue{source=iReg1, dest=iReg2, opSize=polyWordOpSize, isSigned=isCAddress } :: iCode1
                        in
                            if offset = 0
                            then (iCode2, SOME iReg2)
                            else
                            let
                                (* If there's some constant offset add it to the index.  Because it's a byte offset
                                   we need to divide it by the scale but it should always be a multiple. *)
                                val cReg = newUReg() and iReg2 = newUReg()
                                val shift = case loadType of Load64 => 0w3 | Load32 => 0w2 | Load16 => 0w1 | Load8 => 0w0
                                val offsetAsWord = LargeWord.fromInt offset
                                (* It could be negative if it's a C address. *)
                                val shiftedOffset = (if isCAddress then LargeWord.~>> else LargeWord.>>) (offsetAsWord, shift)
                            in
                                (addSubRegister{ base=iReg2, shifted=cReg, dest=SOME iReg2, ccRef=NONE, isAdd=true,
                                    length=OpSize64, shift=ShiftNone} ::
                                 loadNonAddressConstant{ source=shiftedOffset, dest=cReg } :: iCode2,
                                 SOME iReg2)
                            end
                        end

            (* If this is 32in64 get the absolute address. *)
            val (absBCode, absBReg) = getAbsoluteAddress(iCode, bReg1)

            (* If this is a C address the "base address" is actually a LargeWord value that needs to be unboxed. *)
            val (effBCode, effBReg) =
                if isCAddress
                then let val bReg = newUReg() in (unboxLarge{ source=absBReg, dest=bReg } :: absBCode, bReg) end
                else (absBCode, absBReg)

        in
            case iReg1Opt of
                SOME iReg => loadStoreIndex(effBReg, iReg, sValue, effBCode)
            |   NONE => loadStoreOffset(effBReg, offset, sValue, effBCode)
        end

        (* Some operations require a single absolute address. *)
        fun loadAndStoreWithAbsolute codeValue (address, getStoreValue, loadType, loadStore, context, code) =
        let
            (* Have to add the offset/index register. *)
            fun loadStoreOffset(bReg, 0, sReg, code) = loadStore(bReg, sReg, code)
            |   loadStoreOffset(bReg, offset, sReg, code) =
                let
                    val cReg = newUReg() and aReg = newUReg()
                in
                    loadStore(aReg, sReg,
                        addSubRegister{ base=bReg, shifted=cReg, dest=SOME aReg, ccRef=NONE, isAdd=true,
                                        length=OpSize64, shift=ShiftNone} ::
                        loadNonAddressConstant{ source=LargeWord.fromInt offset, dest=cReg } :: code)
                end

            and loadStoreIndex(bReg, iReg, sReg, code) =
            let
                val aReg = newUReg()
                (* The index register is a number of words/bytes etc so has to be multiplied when it's
                   added in. *)
                val indexShift =
                    case loadType of
                        Load64 => ShiftLSL 0w3 | Load32 => ShiftLSL 0w2 | Load16 => ShiftLSL 0w1 | Load8 => ShiftNone
            in
                loadStore(aReg, sReg,
                    addSubRegister{ base=bReg, shifted=iReg, dest=SOME aReg, ccRef=NONE, isAdd=true,
                                    length=OpSize64, shift=indexShift} :: code)
            end
        in
            loadAndStoreWithAddress codeValue (address, getStoreValue, loadType, false, loadStoreOffset, loadStoreIndex, context, code)
        end

        (* Overflow check.  This raises Overflow if the condition is satisfied.  Normally this will be
           that the overflow bit is set but for multiplication it's more complicated.  This generates
           a single block for the function unless there is a handler.
           As well as reducing the size of the code this also means that overflow checks are generally
           BO instructions to the end of the code.  Since the default branch prediction is not to take
           forward jumps this should improve prefetching on the normal, non-overflow, path. *)
        fun checkOverflow (condition, {currHandler=NONE, overflowBlock=ref(SOME overFlowLab), ...}, ccRef) =
            (* It's already been set and there's no surrounding handler - use this. *)
            let
                val noOverflowLab = newLabel()
            in
                [
                    BlockLabel noOverflowLab,
                    BlockFlow(Conditional{ ccRef=ccRef, condition=condition, trueJump=overFlowLab, falseJump=noOverflowLab })
                ]
            end

        |   checkOverflow (condition, {currHandler=NONE, overflowBlock, ...}, ccRef) =
            let
                (* *)
                val overFlowLab = newLabel() and noOverflowLab = newLabel()
                val packetReg = newPReg()
                val () = overflowBlock := SOME overFlowLab
            in
                [
                    BlockLabel noOverflowLab,
                    BlockExit(RaiseExceptionPacket{packetReg=packetReg}),
                    loadAddressConstant{source=toMachineWord(Overflow), dest=packetReg},
                    BlockLabel overFlowLab,
                    BlockFlow(Conditional{ ccRef=ccRef, condition=condition, trueJump=overFlowLab, falseJump=noOverflowLab })
                ]
            end

        |   checkOverflow (condition, {currHandler=SOME h, ...}, ccRef) =
            let
                val overFlowLab = newLabel() and noOverflowLab = newLabel()
                val packetReg = newPReg()
            in
                [
                    BlockLabel noOverflowLab,
                    BlockRaiseAndHandle(RaiseExceptionPacket{packetReg=packetReg}, h),
                    loadAddressConstant{source=toMachineWord(Overflow), dest=packetReg},
                    BlockLabel overFlowLab,
                    BlockFlow(Conditional{ ccRef=ccRef, condition=condition, trueJump=overFlowLab, falseJump=noOverflowLab })
                ]
            end

        fun codeToICodeRev(BICNewenv (bindings, exp), context: context as {stackPtr=initialSp, ...}, isTail, destination, tailCode) =
            let
                (* Process a list of bindings.  We need to accumulate the space used by
                   any containers and reset the stack pointer at the end if necessary. *)
                fun doBindings([], context, tailCode) = (tailCode, context)
 
                |   doBindings(BICDeclar{value=BICExtract(BICLoadLocal l), addr, ...} :: decs, context, tailCode) =
                    let
                        (* Giving a new name to an existing entry.  This should have been removed
                           at a higher level but it doesn't always seem to be.  In particular we
                           must treat this specially if it's a container. *)
                        val original = Array.sub(locToPregArray, l)
                        val () = Array.update(locToPregArray, addr, original)
                    in
                        doBindings(decs, context, tailCode)
                    end

                |   doBindings(BICDeclar{value, addr, ...} :: decs, context, tailCode) =
                    let
                        val (code, dest, _) = codeToICodeRev(value, context, false, AnyReg, tailCode)
                        val () = Array.update(locToPregArray, addr, PregLocation dest)
                    in
                        doBindings(decs, context, code)
                    end

                |   doBindings(BICRecDecs [{lambda, addr, ...}] :: decs, context, tailCode) =
                    (* We shouldn't have single entries in RecDecs but it seems to occur at the moment. *)
                    let
                        val dest = newPReg()
                        val (code, _, _) = codeToICodeRev(BICLambda lambda, context, false, SpecificPReg dest, tailCode)
                        val () = Array.update(locToPregArray, addr, PregLocation dest)
                    in
                        doBindings(decs, context, code)
                    end

                |   doBindings(BICRecDecs recDecs :: decs, context, tailCode) =
                    let
                        val destRegs = map (fn _ => newPReg()) recDecs
                        val flagsValue = if is32in64 then F_closure else 0w0
                        (* First build the closures as mutable cells containing zeros.  Set the
                           entry in the address table to the register containing the address. *)
                        fun makeClosure({lambda={closure, ...}, addr, ...}, dest, tailCode) =
                        let
                            val () = Array.update(locToPregArray, addr, PregLocation dest)
                            val wordsRequired = List.length closure + (if is32in64 then 2 else 1)
                            val absAddr = if is32in64 then newUReg() else dest
                            val zeroReg = newPReg()
                            val allocAndSetZero =
                                loadNonAddressConstant{ source=taggedWord64 0w0, dest=zeroReg} ::
                                    allocateWithProfileRev(wordsRequired, Word8.orb(F_mutable, flagsValue), absAddr, tailCode)
                            val (_, clearCode) =
                                List.foldl(fn (_, (n, l)) =>
                                    (n+1, storeAtWordOffset(zeroReg, n, absAddr, polyWordLoadSize, l))) (0, allocAndSetZero) closure
                        in
                            if is32in64
                            then absoluteToObjectIndex{ source=absAddr, dest=dest } :: clearCode
                            else clearCode
                        end
                        val allocClosures = ListPair.foldlEq makeClosure tailCode (recDecs, destRegs)

                        fun setClosure({lambda, ...}, dest, l) =
                        let
                            val absAddr = if is32in64 then newUReg() else dest
                            val flagsReg = newUReg()
                            (* Lock the closure by storing the flags byte without the mutable flag.
                               TODO: We could simply use XZ here. *)
                        in
                            storeWithConstantOffset{ base=absAddr, source=flagsReg, byteOffset=flagsByteOffset,
                                    loadType=Load8 } ::
                            loadNonAddressConstant{ source=Word8.toLarge flagsValue, dest=flagsReg } ::
                            storeIntoClosure(lambda, absAddr, context,
                                    if is32in64
                                    then objectIndexAddressToAbsolute{ source=dest, dest=absAddr } :: l else l)
                        end
                            
                        val setAndLockClosures = ListPair.foldlEq setClosure allocClosures (recDecs, destRegs)
                    in
                        doBindings(decs, context, setAndLockClosures)
                    end

                |   doBindings(BICNullBinding exp :: decs, context, tailCode) =
                    let
                        val (code, _, _) = codeToICodeRev(exp, context, false, NoResult, tailCode) (* And discard result. *)
                    in
                        doBindings(decs, context, code)
                    end
       
                |   doBindings(BICDecContainer{ addr, size } :: decs, {loopArgs, stackPtr, currHandler, overflowBlock}, tailCode) =
                    let
                        val containerLoc = newStackLoc size
                        val () = Array.update(locToPregArray, addr,
                                    ContainerLocation{container=containerLoc, stackOffset=stackPtr+size})
                        val zeroReg = newPReg()
                    in
                        doBindings(decs,
                            {loopArgs=loopArgs, stackPtr=stackPtr+size, currHandler=currHandler, overflowBlock=overflowBlock},
                            pushToStack{copies=size, container=containerLoc, source=zeroReg} ::
                            loadNonAddressConstant{ source=taggedWord64 0w0, dest=zeroReg } :: tailCode)
                    end

                val (codeBindings, resContext as {stackPtr=finalSp, ...}) = doBindings(bindings, context, tailCode)
                (* If we have had a container we'll need to reset the stack *)
            in
                if initialSp <> finalSp
                then
                let
                    val _ = finalSp >= initialSp orelse raise InternalError "codeToICode - stack ptr"
                    val bodyReg = newPReg() and resultReg = asTarget destination
                    val (codeExp, result, haveExited) =
                        codeToICodeRev(exp, resContext, isTail, SpecificPReg bodyReg, codeBindings)
                    val afterAdjustSp =
                        if haveExited
                        then codeExp
                        else
                            moveRegister{source=result, dest=resultReg} ::
                            resetStackPtr{numWords=finalSp-initialSp} :: codeExp
                in
                    (afterAdjustSp, resultReg, haveExited)
                end
                else codeToICodeRev(exp, resContext, isTail, destination, codeBindings)
            end

        |   codeToICodeRev(BICExtract(BICLoadLocal l), {stackPtr, ...}, _, destination, tailCode) =
            (
                case Array.sub(locToPregArray, l) of
                    NoLocation => raise InternalError "codeToICodeRev - local unset"
                |   PregLocation preg => moveToResult(destination, tailCode, preg)
                |   ContainerLocation{container, stackOffset} =>
                    let
                        val target = asTarget destination
                    in
                        (containerAddress{dest=target, container=container, stackOffset=stackPtr-stackOffset}
                            :: tailCode, target, false)
                    end
            )

        |   codeToICodeRev(BICExtract(BICLoadArgument a), {stackPtr, ...}, _, destination, tailCode) =
            (
                case Vector.sub(argumentVector, a) of
                    ArgumentIsInReg{argReg, ...} => (* It was originally in a register.  It's now in a preg. *)
                        moveToResult(destination, tailCode, argReg)
                |   ArgumentIsOnStack{stackOffset, stackReg} => (* Pushed before call. *)
                    let
                        val target = asTarget destination
                    in
                        (loadStack{wordOffset=stackOffset+stackPtr, container=stackReg, field=0,
                                               dest=target} :: tailCode, target, false)
                    end
            )

        |   codeToICodeRev(BICExtract(BICLoadClosure c), _, _, destination, tailCode) =
            let
                (* Add the number of words for the code address.  This is 1 in native but 2 in 32-in-64. *)
                val offset = if is32in64 then c+2 else c+1
            in
                if c >= List.length closure then raise InternalError "BICExtract: closure" else ();
                wordAddressOffset(destination, closureRegAddr, offset, polyWordLoadSize, tailCode)
            end

        |   codeToICodeRev(BICExtract BICLoadRecursive, _, _, destination, tailCode) =
                (* If the closure is empty we must use the constant.  We can't guarantee that
                   the caller will actually load the closure register if it knows the closure
                   is empty. *)
            (
                 case closure of
                    [] =>
                        let
                            val dReg = asTarget destination
                        in
                            (loadAddressConstant{source=closureAsAddress resultClosure, dest=dReg} :: tailCode, dReg, false)
                        end
                 |  _ => moveToResult(destination, tailCode, closureRegAddr)
            )

        |   codeToICodeRev(BICConstnt(w, _), _, _, destination, tailCode) =
            let
                val dReg = asTarget destination
                val instr =
                    if isShort w
                    then (* When converting to Word64 we do NOT want to use sign-extension.
                            In 32-in-64 signed fixed-precision ints need to have zeros
                            in the top 32 bits. *)
                        LoadNonAddressConstant{source=taggedWord64(Word64.fromLarge(Word.toLarge(toShort w))),
                            dest=dReg}
                    else LoadAddressConstant{source=w, dest=dReg}
            in
                (BlockSimple instr :: tailCode, dReg, false)
            end

        |   codeToICodeRev(BICField{base, offset}, context, _, destination, tailCode) =
            let
                val (codeBase, baseReg, _) = codeToICodeRev(base, context, false, AnyReg, tailCode)
            in
                wordAddressOffset(destination, baseReg, offset, polyWordLoadSize, codeBase)
            end

        |   codeToICodeRev(BICCond(test, thenPt, elsePt), context, isTail, NoResult, tailCode) =
            let
                (* If we don't want the result but are only evaluating for side-effects we
                   may be able to optimise special cases.  This was easier in the forward
                   case but for now we don't bother and leave it to the lower levels. *)
                val startElse = newLabel() and skipElse = newLabel()
                val codeTest = codeConditionRev(test, context, false, startElse, tailCode)
                val (codeThen, _, _) =
                    codeToICodeRev(thenPt, context, isTail, NoResult, codeTest)
                val (codeElse, _, _) =
                     codeToICodeRev(elsePt, context, isTail, NoResult,
                        BlockLabel startElse ::
                        BlockFlow(Unconditional skipElse) :: codeThen)
            in
                returnUnit(NoResult, BlockLabel skipElse :: codeElse, false(*??*))
            end

        |   codeToICodeRev(BICCond(test, thenPt, elsePt), context, isTail, destination, tailCode) =
            let
                (* Because we may push the result onto the stack we have to create a new preg to
                   hold the result and then copy that to the final result. *)
                (* If this is a tail each arm will exit separately and neither will return a result. *)
                val target = asTarget destination
                val condResult = newMergeReg()
                val thenTarget = if isTail then newPReg() else condResult
                val startElse = newLabel()
                val testCode = codeConditionRev(test, context, false, startElse, tailCode)
                
                (* Put the result in the target register. *)
                val (thenCode, _, thenExited) = codeToICodeRev(thenPt, context, isTail, SpecificPReg thenTarget, testCode)
                (* Add a jump round the else-part except that if this is a tail we
                   return.  The then-part could have exited e.g. with a raise or a loop. *)
                val (exitThen, thenLabel, elseTarget) =
                    if thenExited then (thenCode, [], target (* Can use original target. *))
                    else if isTail then (returnInstruction(context, thenTarget, thenCode), [], newPReg())
                    else
                    let
                        val skipElse = newLabel()
                    in
                        (BlockFlow(Unconditional skipElse) :: thenCode,
                         [moveRegister{source=condResult, dest=target},
                          BlockLabel skipElse],
                         condResult)
                    end
                val (elseCode, _, elseExited) =
                    codeToICodeRev(elsePt, context, isTail, SpecificPReg elseTarget,
                        BlockLabel startElse :: exitThen)
                (* Add a return to the else-part if necessary so we will always exit on a tail. *)
                val exitElse =
                    if isTail andalso not elseExited
                    then returnInstruction(context, elseTarget, elseCode) else elseCode
            in
                (thenLabel @ exitElse, target, isTail orelse thenExited andalso elseExited)
            end

        |   codeToICodeRev(BICUnary instr, context, isTail, destination, tailCode) =
                codeToICodeUnaryRev(instr, context, isTail, destination, tailCode)

        |   codeToICodeRev(BICBinary instr, context, isTail, destination, tailCode) =
                codeToICodeBinaryRev(instr, context, isTail, destination, tailCode)

        |   codeToICodeRev(BICTagTest{test, tag=tagValue, ...}, context, isTail, destination, tailCode) =
            (* Check the "tag" word of a union (datatype).  N.B.  Not the same as testing the
               tag bit of a word.  Just generate it as a general word comparison.  The optimiser
               will sort out whether the tag value can be an immediate. *)
                codeToICodeRev(BICBinary{oper=WordComparison{test=TestEqual, isSigned=false},
                                         arg1=test, arg2=BICConstnt(toMachineWord tagValue, [])},
                        context, isTail, destination, tailCode)

        |   codeToICodeRev(BICTuple fields, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                (* The allocator sets the register to the absolute address.  It
                   has to be converted to an object pointer in 32-in-64. *)
                val absAddr = if is32in64 then newUReg() else target
                
                fun loadFields([], n, tlCode) = allocateWithProfileRev(n, 0w0, absAddr, tlCode)
                
                |   loadFields((f as BICConstnt _) :: rest, n, tlCode) =
                    let
                        (* Unlike the X86 we still need to load a constant into a register in order to
                           store it in the new tuple.  However, it's better to leave that until after
                           the allocation and move it then.  That way we can use the same register
                           for different constants if we have a very large tuple. *)
                        val restAndAlloc = loadFields(rest, n+1, tlCode)
                        val (code1, source, _) = codeToICodeRev(f, context, false, AnyReg, restAndAlloc)
                    in
                        storeAtWordOffset(source, n, absAddr, polyWordLoadSize, code1)
                    end
                    
                |   loadFields(f :: rest, n, tlCode) =
                    let
                        val (code1, source, _) = codeToICodeRev(f, context, false, AnyReg, tlCode)
                        val restAndAlloc = loadFields(rest, n+1, code1)
                    in
                        storeAtWordOffset(source, n, absAddr, polyWordLoadSize, restAndAlloc)
                    end
                val allocAndStore = loadFields(fields, 0, tailCode)
                val code =
                    if is32in64
                    then absoluteToObjectIndex{source=absAddr, dest=target} :: allocAndStore
                    else allocAndStore
            in
                (code, target, false)
            end

        |   codeToICodeRev(BICRaise exc, context as { currHandler, ...}, _, destination, tailCode) =
            let
                val (code, packetReg, _) = codeToICodeRev(exc, context, false, AnyReg, tailCode)
                val raiseCode = RaiseExceptionPacket{packetReg=packetReg}
                val block =
                    case currHandler of
                        NONE => BlockExit raiseCode | SOME h => BlockRaiseAndHandle(raiseCode, h)
            in
                returnUnit(destination,  block :: code, true (* Always exits *))
            end

        |   codeToICodeRev(BICEval{function, argList, ...}, context as { currHandler, ...}, isTail, destination, tailCode) =
            let
                val target = asTarget destination
                (* Create pregs for the closure and each argument. *)
                val clPReg = newPReg()
                (* If we have a constant closure we can go directly to the entry point.
                   If the closure is a single word we don't need to load the closure register. *)
                val (functionCode, closureEntry, callKind) =
                    case function of
                        BICConstnt(addr, _) =>
                        let
                            val addrAsAddr = toAddress addr
                            (* If this is a closure we're still compiling we can't get the code address.
                               However if this is directly recursive we can use the recursive
                               convention. *)
                        in
                            if wordEq(closureAsAddress resultClosure, addr)
                            then (tailCode, [], Recursive)
                            else if flags addrAsAddr <> Address.F_words andalso flags addrAsAddr <> Address.F_closure
                            then (loadAddressConstant{source=addr, dest=clPReg} :: tailCode,
                                      [(ArgInReg clPReg, X8)], FullCall)
                            else if is32in64
                            then (* The code address is a 64-bit value so we have to load it
                                    at run-time.  The X86 version passes the closure address here
                                    and generates a relative CALL/JMP.  The actual offset is
                                    computed by the RTS.
                                    For the moment just use a full call. *)
                                (loadAddressConstant{source=addr, dest=clPReg} :: tailCode,
                                      [(ArgInReg clPReg, X8)], FullCall)
                            else (* Native 64-bits. *)
                            let
                                val addrLength = length addrAsAddr
                                val _ = addrLength >= 0w1 orelse raise InternalError "BICEval address"
                                val codeAddr = loadWord(addrAsAddr, 0w0)
                                val _ = isCode (toAddress codeAddr) orelse raise InternalError "BICEval address not code"
                            in
                                if addrLength = 0w1
                                then (tailCode, [], ConstantCode codeAddr)
                                else (loadAddressConstant{source=addr, dest=clPReg} :: tailCode,
                                      [(ArgInReg clPReg, X8)], ConstantCode codeAddr)
                            end
                        end

                    |   BICExtract BICLoadRecursive =>
                        (
                            (* If the closure is empty we don't need to load X8 *)
                            case closure of
                                [] => (tailCode, [], Recursive)
                            |   _ =>
                                    (moveRegister {source=closureRegAddr, dest=clPReg} :: tailCode,
                                     [(ArgInReg clPReg, X8)], Recursive)
                        )

                    |   function => (* General case. *)
                            (#1 (codeToICodeRev(function, context, false, SpecificPReg clPReg, tailCode)), [(ArgInReg clPReg, X8)], FullCall)

                (* Load the first arguments into registers and the rest to the stack. *)
                fun loadArgs ([], _, tailCode) = (tailCode, [], [])

                |   loadArgs ((arg, _) :: args, gReg::gRegs, tailCode) =
                    let (* General register argument. *)
                        val (c, r, _) = codeToICodeRev(arg, context, false, AnyReg, tailCode)
                        val (code, regArgs, stackArgs) = loadArgs(args, gRegs, c)
                    in
                        (code, (ArgInReg r, gReg) :: regArgs, stackArgs)
                    end

                |   loadArgs ((arg, _) :: args, [], tailCode) =
                    let (* Stack argument. *)
                        val (c, r, _) = codeToICodeRev(arg, context, false, AnyReg, tailCode)
                        val (code, regArgs, stackArgs) = loadArgs(args, [], c)
                    in
                        (code, regArgs, ArgInReg r :: stackArgs)
                    end

                val (codeArgs, regArgs, stackArgs) = loadArgs(argList, generalArgRegs, functionCode)
                
                (* If this is at the end of the function and the result types are the
                   same we can use a tail-recursive call. *)
                val tailCall = isTail (*andalso resultType = fnResultType*)
                
                val callCode =
                    if tailCall
                    then
                    let
                        val {stackPtr, ...} = context
                        (* The number of arguments currently on the stack. *)
                        val currentStackArgCount = currentStackArgs
                        val newStackArgCount = List.length stackArgs
                        (* The offset of the first argument.  Offsets can be negative. *)
                        val stackOffset = stackPtr
                        fun makeStackArgs([], _) = []
                        |   makeStackArgs(arg::args, offset) = {src=arg, stack=offset} :: makeStackArgs(args, offset-1)
                        val stackArgs = makeStackArgs(stackArgs, currentStackArgCount-1)
                        (* The stack adjustment needed to compensate for any items that have been pushed
                           and the differences in the number of arguments.  May be positive or negative. *)
                        val stackAdjust = currentStackArgCount - newStackArgCount
                        (* Add an entry for the return address to the register arguments. *)
                    in
                        BlockExit(TailRecursiveCall{regArgs=(ArgInReg returnAddrReg, X30) :: closureEntry @ regArgs, stackArgs=stackArgs,
                                  stackAdjust = stackAdjust, currStackSize=stackOffset, callKind=callKind}) ::
                                  codeArgs
                    end
                    else
                    let
                        val call =
                            FunctionCall{regArgs=closureEntry @ regArgs, stackArgs=stackArgs, dest=target,
                                         callKind=callKind, saveRegs=[]}
                        val callBlock =
                            case currHandler of
                                NONE => BlockSimple call :: codeArgs
                            |   SOME h => BlockOptionalHandle{call=call, handler=h, label=newLabel()}  :: codeArgs
                    in
                        callBlock
                    end
            in
                (callCode, target, tailCall (* We've exited if this was a tail jump *))
            end

        |   codeToICodeRev(BICNullary{oper=BuiltIns.GetCurrentThreadId}, _, _, destination, tailCode) =
            (* Get the ID of the current thread. *)
            let
                val target = asTarget destination
            in
                (loadMemReg{wordOffset=threadIdOffset, dest=target} :: tailCode, target, false)
            end

        |   codeToICodeRev(BICNullary{oper=BuiltIns.CheckRTSException}, { currHandler, ...}, _, destination, tailCode) =
            (* TODO: This really does need to be part of the RTS call.  It's included
               in the long case of every arbitrary precision operation as well as
               every other RTS call. *)
            let
                (* Raise an exception in ML if the last RTS call set the exception packet. *)
                val haveException = newLabel() and noException = newLabel()
                val ccRef = newCCRef()
                val testReg = newPReg()
                val raiseCode = RaiseExceptionPacket{packetReg=testReg}
                val code =
                    BlockLabel noException ::
                    (case currHandler of
                        NONE => BlockExit raiseCode | SOME h => BlockRaiseAndHandle(raiseCode, h)) ::
                    BlockLabel haveException ::
                    BlockFlow(Conditional{ ccRef=ccRef, condition=CondNotEqual, trueJump=haveException, falseJump=noException }) ::
                    addSubImmediate{source=testReg, dest=NONE, immed=taggedWord 0w0, isAdd=false, length=OpSize64, ccRef=SOME ccRef} ::
                    loadMemReg{wordOffset=exceptionPacketOffset, dest=testReg} ::
                    tailCode
            in
                returnUnit(destination, code, false)
            end

        |   codeToICodeRev(BICNullary {oper=CreateMutex}, _, _, _, _) =
                raise Fallback "codeToICodeRev: BICNullary: CreateMutex"

        |   codeToICodeRev(BICArbitrary { oper=ArithMult, longCall, ... }, context, isTail, destination, tailCode) =
                (* Just call the long function to do this.  Overflow detection makes this too complicated. *)
                codeToICodeRev(longCall, context, isTail, destination, tailCode)

        |   codeToICodeRev(BICArbitrary { oper, shortCond, arg1, arg2, longCall }, context, _, destination, tailCode) =
            let
                val startLong = newLabel() and resultLabel = newLabel()
                val condResult = newMergeReg()
                (* Test to see if the arguments are short and go straight to the long case if not. *)
                val testCode = codeConditionRev(shortCond, context, false, startLong, tailCode) 
                (* Do the short case *)
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, testCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                (* We need to subtract the tag from one of the arguments and then do the
                   addition.  The optimiser will do the subtraction at compile time if we
                   subtract from a constant so if this is and Add we try to put the constant
                   in the second arg. *)
                val (firstReg, secondReg) =
                    case (arg1, oper) of
                        (BICConstnt _, ArithAdd) => (aReg2, aReg1) | _ => (aReg1, aReg2)
                (* Generate code for the short case.  Put the result in the merge register.  Jump to
                   the result if there's no overflow and to the long case if there is. *)
                val codeShort =
                    case oper of
                        ArithAdd =>
                        let
                            val uReg = newUReg() and chkOverflow = newCCRef()
                        in
                            BlockFlow(Conditional{ ccRef=chkOverflow, condition=CondOverflow, trueJump=startLong, falseJump=resultLabel }) ::
                            addSubRegister{base=firstReg, shifted=uReg, dest=SOME condResult, length=polyWordOpSize,
                                         ccRef=SOME chkOverflow, isAdd=true, shift=ShiftNone} ::
                            addSubImmediate{dest=SOME uReg, source=secondReg, immed=0w1, length=polyWordOpSize,
                                            ccRef=NONE, isAdd=false}:: arg2Code
                        end

                    |   ArithSub =>
                        let
                            val uReg = newUReg() and chkOverflow = newCCRef()
                        in
                            BlockFlow(Conditional{ ccRef=chkOverflow, condition=CondOverflow, trueJump=startLong, falseJump=resultLabel }) ::
                            addSubRegister{base=firstReg, shifted=uReg, dest=SOME condResult, length=polyWordOpSize,
                                         ccRef=SOME chkOverflow, isAdd=false, shift=ShiftNone} ::
                            addSubImmediate{dest=SOME uReg, source=secondReg, immed=0w1, length=polyWordOpSize,
                                            ccRef=NONE, isAdd=false}:: arg2Code
                        end

                    |   _ => raise InternalError "BICArbitrary: unimplemented operation"

                (* Code for the long case. Put the result into the merge register. *)
                (* TODO: This could use a tail call if this is at the end of the function. *)
                val (codeLong, _, _) =
                    codeToICodeRev(longCall, context, false, SpecificPReg condResult,
                                   BlockLabel startLong :: codeShort)
                val target = asTarget destination
                (* Copy the merge register into the result. *)
                val finalCode =
                    moveRegister{source=condResult, dest=target} ::
                    BlockLabel resultLabel :: codeLong
            in
                (finalCode, target, false)
            end

        |   codeToICodeRev(BICLambda(lambda as { closure = [], ...}), _, _, destination, tailCode) =
            (* Empty closure - create a constant closure for any recursive calls. *)
            let
                val closure = makeConstantClosure()
                val () = codeFunctionToArm64(lambda, debugSwitches, closure)
                val dReg = asTarget destination
                (* Return the closure itself as the value. *)
            in
                (BlockSimple(LoadAddressConstant{source=closureAsAddress closure, dest=dReg}) :: tailCode, dReg, false)
            end

        |   codeToICodeRev(BICLambda(lambda as { closure, ...}), context, _, destination, tailCode) =
            (* Non-empty closure.  Ignore stack closure option at the moment. *)
            let
                val wordsRequired = List.length closure  + (if is32in64 then 2 else 1)
                val target = asTarget destination
                val absAddr = if is32in64 then newUReg() else target
                (* The values we're storing are all either constants or local/closure variables so
                   we can allocate the memory and then store into it. *)
                val allocCode =
                    allocateWithProfileRev(wordsRequired, if is32in64 then F_closure else 0w0, absAddr, tailCode)
                val storeCode =
                    storeIntoClosure(lambda, absAddr, context, allocCode)
                val finalCode =
                    if is32in64 then BlockSimple(AbsoluteToObjectIndex{source=absAddr, dest=target}) :: storeCode else storeCode
            in
                (finalCode, target, false)
            end

        |   codeToICodeRev(BICCase { cases, test, default, isExhaustive, firstIndex}, context, isTail, destination, tailCode) =
            let
                (* We have to create a new preg for the result in case we need to push
                   it to the stack. *)
                val targetReg = newMergeReg()
                
                local
                    val (testCode, initialTestReg, _) = codeToICodeRev(test, context, false, AnyReg, tailCode)
                    (* Subtract the minimum even if it is zero to remove the tag.
                       This leaves us with a shifted but untagged value. Don't check for overflow.
                       Instead allow large values to wrap around and check later. *)
                    val cReg1 = newUReg()
                    val subValue = taggedWord64(Word64.fromLarge(Word.toLargeX firstIndex))
                in
                    val testReg = newUReg()
                    val testCode =
                        addSubRegister{ base=initialTestReg, shifted=cReg1, dest=SOME testReg, ccRef=NONE, isAdd=false,
                                        length=polyWordOpSize, shift=ShiftNone} ::
                        loadNonAddressConstant{ source=subValue, dest=cReg1 } :: testCode
                end

                val (rangeCheck, extraDefaults) =
                    if isExhaustive then (testCode, [])
                    else
                    let
                        (* Check the value is within the number of cases, *2 because this is shifted. *)
                        val cReg2 = newUReg() and ccRef1 = newCCRef()
                        val nCases = List.length cases
                        val continueLab = newLabel() and defaultLab1 = newLabel()
                        val rangeCheck =
                            BlockLabel continueLab ::
                            BlockFlow(Conditional{ccRef=ccRef1, condition=CondCarrySet, trueJump=defaultLab1, falseJump=continueLab}) ::
                            addSubRegister{base=testReg, shifted=cReg2, dest=NONE, ccRef=SOME ccRef1, isAdd=false,
                                           length=OpSize64, shift=ShiftNone} ::
                            loadNonAddressConstant{ source=Word64.fromInt nCases * 0w2, dest=cReg2 } :: testCode
                    in
                        (rangeCheck, [defaultLab1])
                    end

                (* Make a label for each item in the list. *)
                val codeLabels = map (fn _ => newLabel()) cases

                (* Create an exit label in case it's needed. *)
                val labelForExit = newLabel()

                (* Generate the code for each of the cases and the default.  We need to put an
                   unconditional branch after each to skip the other cases. *)
                fun codeCases (SOME c :: otherCases, startLabel :: otherLabels, tailCode) =
                    let
                        val caseTarget = if isTail then newPReg() else targetReg
                        (* Put in the case with a jump to the end of the sequence. *)
                        val (codeThisCase, _, caseExited) =
                            codeToICodeRev(c, context, isTail, SpecificPReg caseTarget,
                                BlockLabel startLabel :: tailCode) 
                        val exitThisCase =
                            if caseExited then codeThisCase
                            else if isTail then returnInstruction(context, caseTarget, codeThisCase)
                            else BlockFlow(Unconditional labelForExit) :: codeThisCase
                    in
                        codeCases(otherCases, otherLabels, exitThisCase)
                    end

                |   codeCases(NONE :: otherCases, _ :: otherLabels, tailCode) = codeCases(otherCases, otherLabels, tailCode)
                        
                |   codeCases ([], [], tailCode) =
                    let
                        (* We need to add labels for all the gaps we filled and also for a "default" label for
                           the indexed-case instruction itself as well as any range checks. *)
                        fun addDefault (startLabel, NONE, l) = BlockLabel startLabel :: l
                        |   addDefault (_, SOME _, l) = l
                        fun asForward l = BlockLabel l
                        val dLabs = map asForward extraDefaults @ tailCode
                        val defLabels = ListPair.foldlEq addDefault dLabs (codeLabels, cases)
                        val defaultTarget = if isTail then newPReg() else targetReg
                        val (defaultCode, _, defaultExited) =
                            codeToICodeRev(default, context, isTail, SpecificPReg defaultTarget, defLabels)
                    in
                        (* Put in the default.  Because this is the last we don't need to
                           jump round it.  However if this is a tail and we haven't exited
                           we put in a return.  That way the case will always have
                           exited if this is a tail. *)
                         if isTail andalso not defaultExited
                         then returnInstruction(context, defaultTarget, defaultCode)
                         else defaultCode
                    end

                |   codeCases _ = raise InternalError "codeCases: mismatch"
                    
                val codedCases =
                    codeCases(cases, codeLabels,
                        BlockFlow(IndexedBr codeLabels) ::
                        BlockSimple(IndexedCaseOperation{testReg=testReg}) ::
                        rangeCheck)
   
                (* We can now copy to the target.  If we need to push the result this load
                   will be converted into a push. *)
                val target = asTarget destination
                val copyToTarget =
                    if isTail then codedCases
                    else moveRegister{source=targetReg, dest=target} :: BlockLabel labelForExit :: codedCases
            in
                (copyToTarget, target, isTail (* We have always exited on a tail. *))
            end

        |   codeToICodeRev(BICBeginLoop {loop, arguments}, context as { stackPtr, currHandler, overflowBlock, ...},
                           isTail, destination, tailCode) =
            let
                val target = asTarget destination
                
                fun codeArgs ([], tailCode) = ([], tailCode)
                |   codeArgs (({value, addr}, _) :: rest, tailCode) =
                    let
                        val pr = newPReg()
                        val () = Array.update(locToPregArray, addr, PregLocation pr)
                        val (code, _, _) = codeToICodeRev(value, context, false, SpecificPReg pr, tailCode)
                        val (pregs, othercode) = codeArgs(rest, code)
                    in
                        (pr::pregs, othercode)
                    end
                val (loopRegs, argCode) = codeArgs(arguments, tailCode)

                val loopLabel = newLabel()
                val (loopBody, _, loopExited) =
                    codeToICodeRev(loop,
                        {loopArgs=SOME (loopRegs, loopLabel, stackPtr), stackPtr=stackPtr,
                         currHandler=currHandler, overflowBlock=overflowBlock },
                            isTail, SpecificPReg target, BlockLabel loopLabel :: BlockSimple BeginLoop :: argCode)
            in
                (loopBody, target, loopExited)
            end

        |   codeToICodeRev(BICLoop args, context as {loopArgs=SOME (loopRegs, loopLabel, loopSp), stackPtr, currHandler, ...}, _, destination, tailCode) =
            let
                val target = asTarget destination
                (* Registers to receive the evaluated arguments.  We can't put the
                   values into the loop variables yet because the values could depend
                   on the current values of the loop variables. *)
                val argPRegs = map(fn _ => newPReg()) args
                val codeArgs =
                    ListPair.foldlEq(fn ((arg, _), pr, l) =>
                        #1 (codeToICodeRev(arg, context, false, SpecificPReg pr, l))) tailCode
                        (args, argPRegs)
                val jumpArgs = ListPair.mapEq(fn (s, l) => {src=ArgInReg s, dst=l}) (argPRegs, loopRegs)
                (* If we've allocated a container in the loop we have to remove it before jumping back. *)
                val stackReset =
                    if loopSp = stackPtr then codeArgs
                    else resetStackPtr{numWords=stackPtr-loopSp} :: codeArgs
                val jumpLoop = JumpLoop{regArgs=jumpArgs, stackArgs=[], checkInterrupt=SOME[]}
                (* "checkInterrupt" could result in a Interrupt exception so we treat this like
                   a function call. *)
                val code =
                    case currHandler of
                        NONE => BlockFlow(Unconditional loopLabel) :: BlockSimple jumpLoop :: stackReset
                    |   SOME h => BlockOptionalHandle{call=jumpLoop, handler=h, label=loopLabel} :: stackReset
            in
                (code, target, true)
            end

        |   codeToICodeRev(BICLoop _, {loopArgs=NONE, ...}, _, _, _) = raise InternalError "BICLoop without BICBeginLoop"

            (* Copy the source tuple into the container.  There are important special cases for
               both the source tuple and the container.  If the source tuple is a BICTuple we have
               the fields and can store them without creating a tuple on the heap.  If the
               destination is a local container we can store directly into the stack. *)
        |   codeToICodeRev(BICSetContainer{container, tuple, filter}, context as {stackPtr, ...}, _, destination, tailCode) =
            let
                local
                    fun createStore containerReg (source, destWord, tail) =
                        storeAtWordOffset(source, destWord, containerReg, Load64, tail)
                in
                    val findContainer =
                        case container of
                            BICExtract(BICLoadLocal l) =>
                            (
                                case Array.sub(locToPregArray, l) of
                                    ContainerLocation{container, stackOffset} =>
                                    let
                                        fun store(source, destWord, tail) =
                                            storeToStack{source=source, container=container, field=destWord,
                                                stackOffset=stackPtr-stackOffset+destWord} :: tail
                                    in
                                        SOME store
                                    end
                               |    _ => NONE
                           )
                       |    _ => NONE

                    val (codeContainer, storeInstr) =
                        case findContainer of
                            SOME storeToStack => (tailCode, storeToStack)
                        |   NONE => 
                            let
                                val containerTarget = newPReg()
                                val (codeContainer, _, _) =
                                    codeToICodeRev(container, context, false, SpecificPReg containerTarget, tailCode)
                            in
                                (codeContainer, createStore containerTarget)
                            end
                end
                
                val filterLength = BoolVector.length filter

                val code =
                    case tuple of
                        BICTuple cl =>
                        let
                            (* In theory it's possible that the tuple could contain fields that are not
                               used but nevertheless need to be evaluated for their side-effects.
                               Create all the fields and push to the stack. *)
                            fun codeField(arg, (regs, tailCode)) =
                            let
                                val (c, r, _) =
                                    codeToICodeRev(arg, context, false, AnyReg, tailCode)
                            in
                                (r :: regs, c)
                            end

                            val (pregsRev, codeFields) = List.foldl codeField ([], codeContainer) cl
                            val pregs = List.rev pregsRev

                            fun copyField(srcReg, (sourceWord, destWord, tailCode)) =
                                if sourceWord < filterLength andalso BoolVector.sub(filter, sourceWord)
                                then (sourceWord+1, destWord+1, storeInstr(srcReg, destWord, tailCode))
                                else (sourceWord+1, destWord, tailCode)
                            
                            val (_, _, resultCode) = List.foldl copyField (0, 0, codeFields) pregs
                        in
                            resultCode
                        end

                    |   tuple =>
                        let (* Copy a heap tuple.  It is possible that this is another container in which case
                               we must load the fields directly.  We mustn't load its address and then copy
                               because loading the address would be the last reference and might cause
                               the container to be reused prematurely. ??? Is that an old comment ?? *)
                            val (codeTuple, loadField) =
                                case getContainerIfPresent tuple of
                                    SOME {container, stackOffset} =>
                                    let
                                        fun getAddr(destReg, sourceWord, tail) =
                                            loadStack{dest=destReg, wordOffset=stackPtr-stackOffset+sourceWord, container=container,
                                                          field=sourceWord} :: tail
                                    in
                                        (codeContainer, getAddr)
                                    end
                                |   NONE =>
                                    let
                                        val (codeTuple, tupleTarget, _) = codeToICodeRev(tuple, context, false, AnyReg, codeContainer)
                                        fun loadField(destReg: preg, sourceWord: int, tail): blockStruct list =
                                        let
                                            val (code, _, _) =
                                                wordAddressOffset(SpecificPReg destReg, tupleTarget, sourceWord, polyWordLoadSize, tail)
                                        in
                                            code
                                        end
                                    in
                                        (codeTuple, loadField)
                                    end

                            fun copyContainer(sourceWord, destWord, tailCode) =
                            if sourceWord = filterLength
                            then tailCode
                            else if BoolVector.sub(filter, sourceWord)
                            then
                            let
                                val loadReg = newPReg()
                                val code =
                                    storeInstr(loadReg, destWord, loadField(loadReg, sourceWord, tailCode))
                            in
                                copyContainer(sourceWord+1, destWord+1, code)
                            end
                            else copyContainer(sourceWord+1, destWord, tailCode)
                        in
                            copyContainer(0, 0, codeTuple)
                        end
            in
                returnUnit(destination, code, false)
            end

        |   codeToICodeRev(BICLoadContainer{base, offset}, context as {stackPtr, ...}, _, destination, tailCode) =
            (
                case getContainerIfPresent base of
                    SOME {container, stackOffset} =>
                    let (* If this is a local container we extract the field. *)
                        val target = asTarget destination
                        val finalOffset = stackPtr-stackOffset+offset
                        val _ = finalOffset >= 0 orelse raise InternalError "offset"
                    in
                        (BlockSimple(LoadStack{wordOffset=finalOffset, container=container, field=offset,
                            dest=target}) :: tailCode, target, false)
                    end

                |   NONE =>
                    let
                        val (codeBase, baseEntry, _) = codeToICodeRev(base, context, false, AnyReg, tailCode)
                    in
                        wordAddressOffset(destination, baseEntry, offset, Load64, codeBase)
                    end
            )

        |   codeToICodeRev(BICLoadOperation{ kind, address}, context, _, destination, tailCode) =
                codeLoadOperation(kind, address, context, asTarget destination, tailCode)

        |   codeToICodeRev(BICStoreOperation{ kind, address, value}, context, _, destination, tailCode) =
                codeStoreOperation(kind, address, value, context, destination, tailCode)

        |   codeToICodeRev(BICBlockOperation _, _, _, _, _) = raise Fallback "codeToICodeRev: BICBlockOperation"

        |   codeToICodeRev(BICAllocateWordMemory {numWords, flags, initial }, context, _, destination, tailCode) =
            let
                (* Allocate a block of memory and initialise it. *)
                val target = asTarget destination
                val (codeSize, sizeReg, _) = codeToICodeRev(numWords, context, false, AnyReg, tailCode)
                val (codeFlags, flagsReg, _) = codeToICodeRev(flags, context, false, AnyReg, codeSize)
                val (codeInit, initReg, _) = codeToICodeRev(initial, context, false, AnyReg, codeFlags)
                val uSizeReg = newUReg() and shiftFReg = newUReg() and lengthWord = newUReg()
                val absAddr = if is32in64 then newUReg() else target
                val untagSize = untagValue{source=sizeReg, dest=uSizeReg, opSize=polyWordOpSize, isSigned=false} :: codeInit
                val allocateMem = allocateMemoryVariable{ size=uSizeReg, dest=absAddr, saveRegs=[]} :: untagSize
                (* Make the length word by first shifting the flags into the length word reg by
                   55 or 23 bits.  This puts the tag bit in the top bit of the size.  Then insert the size
                   into this which will overwrite the flag's tag bit. *)
                val makeLengthWord =
                    bitFieldInsert{ source=uSizeReg, destAsSource=shiftFReg, dest=lengthWord, length=polyWordOpSize,
                              immr=0w0 (*bit 0*), imms=if is32in64 then 0w23 else 0w55 (*width-1*) } ::
                    shiftConstant{direction=Arm64ICode.ShiftLeft, dest=shiftFReg, source=flagsReg,
                        shift=if is32in64 then 0w23 else 0w55, opSize=polyWordOpSize } :: allocateMem
                val setLengthWordAndInit =
                    initialiseMem{ size=uSizeReg, addr=absAddr, init=initReg} ::
                    storeWithConstantOffset{ source=lengthWord, base=absAddr,
                        byteOffset= ~(Word.toInt wordSize), loadType=polyWordLoadSize } :: makeLengthWord
                val finalCode =
                    if is32in64 then absoluteToObjectIndex{ source=absAddr, dest=target } :: setLengthWordAndInit
                    else setLengthWordAndInit
            in
                (finalCode, target, false)
            end

        |   codeToICodeRev(BICHandle{exp, handler, exPacketAddr}, context as { stackPtr, loopArgs, overflowBlock, ... }, isTail, destination, tailCode) =
            let
                (* As with BICCond and BICCase we need to create a new register for the
                   result in case we need to push it to the stack. *)
                val handleResult = newMergeReg()
                val handlerLab = newLabel() and startHandling = newLabel()
                val (bodyTarget, handlerTarget) =
                    if isTail then (newPReg(), newPReg()) else (handleResult, handleResult)
                (* TODO: Even if we don't actually want a result we force one in here by
                   using "asTarget".  *)
                (* The expression cannot be treated as a tail because the handler has
                   to be removed after.  It may "exit" if it has raised an unconditional
                   exception.  If it has we mustn't generate a PopExceptionHandler because
                   there won't be any result for resultReg.
                   We need to add two words to the stack to account for the items pushed by
                   PushExceptionHandler.
                   We create an instruction to push the handler followed by a block fork to
                   the start of the code and, potentially the handler, then a label to start
                   the code that the handler is in effect for. *)
                val initialCode =
                    BlockLabel startHandling ::
                    BlockFlow(SetHandler{handler=handlerLab, continue=startHandling}) ::
                    BlockSimple(PushExceptionHandler) :: tailCode
                val (expCode, _, expExit) =
                    codeToICodeRev(exp, {stackPtr=stackPtr+2, loopArgs=loopArgs, currHandler=SOME handlerLab, overflowBlock=overflowBlock},
                        false (* Not tail *), SpecificPReg bodyTarget, initialCode)
                (* If this is the tail we can replace the jump at the end of the
                   handled code with returns.  If the handler has exited we don't need
                   a return there.  Otherwise we need to add an unconditional jump to
                   skip the handler. *)
                val (atExpEnd, skipExpLabel) =
                    case (isTail, expExit) of
                        (true, true) => (* Tail and exited. *) (expCode, NONE)
                    |   (true, false) => (* Tail and not exited. *)
                            (returnInstruction(context, bodyTarget, BlockSimple(PopExceptionHandler) :: expCode), NONE)
                    |   (false, true) => (* Not tail but exited. *) (expCode, NONE)
                    |   (false, false) =>
                        let
                            val skipHandler = newLabel()
                        in
                            (BlockFlow(Unconditional skipHandler) ::
                             BlockSimple(PopExceptionHandler) :: expCode, SOME skipHandler)
                        end
                (* Make a register to hold the exception packet and put eax into it. *)
                val packetAddr = newPReg()
                val () = Array.update(locToPregArray, exPacketAddr, PregLocation packetAddr)
                val (handleCode, _, handleExit) =
                    codeToICodeRev(handler, context, isTail, SpecificPReg handlerTarget,
                        BlockSimple(BeginHandler{packetReg=packetAddr}) :: BlockLabel handlerLab :: atExpEnd)
                val target = asTarget destination
                val afterHandler =
                    case (isTail, handleExit) of
                        (true, true) => (* Tail and exited. *) handleCode
                    |   (true, false) => (* Tail and not exited. *)
                            returnInstruction(context, handlerTarget, handleCode)
                    |   (false, _) => (* Not tail. *) handleCode
                
                val addLabel =
                    case skipExpLabel of
                        SOME lab => BlockLabel lab:: afterHandler
                    |   NONE => afterHandler
            in
                (moveRegister{source=handleResult, dest=target} :: addLabel, target, isTail)
            end

        and codeConditionRev(condition, context, jumpOn, jumpLabel, tailCode) =
       (* Jump optimisation is done later.  Just generate the general case.
          Load the value into a register and compare it with 1 (true) *)
        let
            val ccRef = newCCRef()
            val (testCode, testReg, _) = codeToICodeRev(condition, context, false, AnyReg, tailCode)
            val noJumpLabel = newLabel()
        in
            BlockLabel noJumpLabel ::
            BlockFlow(Conditional{ccRef=ccRef,
                       condition=if jumpOn then CondEqual else CondNotEqual, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
            (* Compare: SUBS XZ,reg,3.  Can use 32-bit comparison because it's either tagged 0 or tagged 1. *)
            addSubImmediate{source=testReg, immed=taggedWord 0w1, isAdd=false, dest=NONE, length=OpSize32, ccRef=SOME ccRef} ::
            testCode
        end

        and codeToICodeUnaryRev({oper=NotBoolean, arg1}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val ccRef = newCCRef()
                val (argCode, testDest, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
            in
                (* Test the argument and return a boolean result.  If either the argument is a condition
                   or the result is used in a test this will be better than using XOR. *)
                (makeBoolResultRev(CondNotEqual, ccRef, target,
                        addSubImmediate{source=testDest, immed=taggedWord 0w1, isAdd=false,
                                dest=NONE, length=OpSize32 (* Always either tagged 0 or tagged 1 *), ccRef=SOME ccRef} ::
                            argCode), target, false)
            end

        |   codeToICodeUnaryRev({oper=IsTaggedValue, arg1}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val ccRef = newCCRef()
                val (argCode, testDest, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
            in
                (* Test the argument and return a boolean result.  This ought to be optimised at
                   a lower level to use a test-and-branch. *)
                (makeBoolResultRev(CondNotEqual, ccRef, target,
                            logicalImmediate{source=testDest, immed=0w1 (* The tag bit*), logOp=LogAnd,
                                dest=NONE, length=OpSize32 (* Always either tagged 0 or tagged 1 *), ccRef=SOME ccRef} ::
                            argCode), target, false)
            end

        |   codeToICodeUnaryRev({oper=MemoryCellLength, arg1}, context, _, destination, tailCode) =
            let
                val ureg1 = newUReg() and ureg2 = newUReg()
                val (codeBase, baseReg, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                (* Load the word at -1 (words) into a ureg *)
                val (codeLoad, _, _) = wordAddressOffset(SpecificPReg ureg1, baseReg, ~1, polyWordLoadSize, codeBase)
                (* Select 56 or 24 bits and shift it left.  This disassembles as UBFIZ..*)
                val lsb = 0w1 and width = if is32in64 then 0w24 else 0w56
                (* Encoding for unsignedBitfieldInsertinZeros64/32 *)
                val immr = if is32in64 then Word.~ lsb mod 0w32 else Word.~ lsb mod 0w64
                val imms = width-0w1
                val maskAndShift =
                    bitFieldShift{source=ureg1, dest=ureg2, isSigned=false, length=polyWordOpSize, immr=immr, imms=imms} :: codeLoad
                val target = asTarget destination
                val addTag =
                    addSubImmediate{dest=SOME target, source=ureg2, immed=0w1, length=polyWordOpSize,
                                            ccRef=NONE, isAdd=true} :: maskAndShift
            in
                (addTag, target, false)
            end

        |   codeToICodeUnaryRev({oper=MemoryCellFlags, arg1}, context, _, destination, tailCode) =
            let
                (* Load the flags byte and tag it. *)
                val (codeBase, baseReg, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (codeRealBase, realBaseReg) = getAbsoluteAddress(codeBase, baseReg)
                val ureg = newUReg()
                val codeLoad =
                    loadWithConstantOffset{ base=realBaseReg, dest=ureg, byteOffset=flagsByteOffset, loadType=Load8 } :: codeRealBase
                val target = asTarget destination
                val withTag = tagValue{ source=ureg, dest=target, isSigned=false, opSize=OpSize32 } :: codeLoad
            in
                (withTag, target, false)
            end

        |   codeToICodeUnaryRev({oper=ClearMutableFlag, arg1}, context, _, destination, tailCode) =
            let
                val (codeBase, baseReg, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (codeRealBase, realBaseReg) = getAbsoluteAddress(codeBase, baseReg)
                val ureg1 = newUReg() and ureg2 = newUReg()
                (* Load the flags, mask off the mutable bit and store it back. *)
                val code =
                    storeWithConstantOffset{ base=realBaseReg, source=ureg2, byteOffset=flagsByteOffset, loadType=Load8 } ::
                    logicalImmediate{ source=ureg1, dest=SOME ureg2, ccRef=NONE,
                                      immed=Word64.xorb(0wxffffffff, 0wx40), logOp=LogAnd, length=OpSize32 } ::
                    loadWithConstantOffset{ base=realBaseReg, dest=ureg1, byteOffset=flagsByteOffset, loadType=Load8 } :: codeRealBase
            in
                returnUnit(destination, code, false)
            end

        |   codeToICodeUnaryRev({oper=LongWordToTagged, arg1}, context, _, destination, tailCode) =
            let
                val (codeBase, baseReg, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val uReg = newUReg()
                val target = asTarget destination
                val code =
                    tagValue{ source=uReg, dest=target, isSigned=false, opSize=polyWordOpSize } ::
                    unboxLarge{ source=baseReg, dest=uReg } :: codeBase
            in
                (code, target, false)
            end

        |   codeToICodeUnaryRev({oper=SignedToLongWord, arg1}, context, _, destination, tailCode) =
            let
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val uReg = newUReg()
                val target = asTarget destination
                (* We can use a single instruction here on both 32-in-64 and native 64-bits.
                   On 64-bits this is equivalent to an arithmetic shift; on 32-bits
                   it propagates the sign bit into the high-order part. *)
                val code =
                    boxLarge{ source=uReg, dest=target, saveRegs=[] } ::
                    bitFieldShift{ source=aReg1, dest=uReg, isSigned=true, length=OpSize64,
                                   immr=0w1, imms=if is32in64 then 0wx1f else 0wx3f } ::
                    arg1Code
            in
                (code, target, false)
            end

        |   codeToICodeUnaryRev({oper=UnsignedToLongWord, arg1}, context, _, destination, tailCode) =
            let
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val uReg = newUReg()
                val target = asTarget destination
                (* This amounts to a logical shift.  Since the top half of the
                   register is zero in 32-in-64 we don't have to select just the
                   low word but there's no advantage in not. *)
                val code =
                    boxLarge{ source=uReg, dest=target, saveRegs=[] } ::
                    bitFieldShift{ source=aReg1, dest=uReg, isSigned=false, length=OpSize64,
                                   immr=0w1, imms=if is32in64 then 0wx1f else 0wx3f } ::
                    arg1Code
            in
                (code, target, false)
            end

        |   codeToICodeUnaryRev({oper=RealAbs _, arg1}, context, _, destination, tailCode) =
                raise Fallback "codeToICodeUnaryRev: RealAbs"

        |   codeToICodeUnaryRev({oper=RealNeg _, arg1}, context, _, destination, tailCode) =
                raise Fallback "codeToICodeUnaryRev: RealNeg"

        |   codeToICodeUnaryRev({oper=RealFixedInt _, arg1}, context, _, destination, tailCode) =
                raise Fallback "codeToICodeUnaryRev: RealFixedInt"

        |   codeToICodeUnaryRev({oper=FloatToDouble, arg1}, context, _, destination, tailCode) =
                raise Fallback "codeToICodeUnaryRev: FloatToDouble"

        |   codeToICodeUnaryRev({oper=DoubleToFloat, arg1}, context, _, destination, tailCode) =
                raise Fallback "codeToICodeUnaryRev: DoubleToFloat"

        |   codeToICodeUnaryRev({oper=RealToInt _, arg1}, context, _, destination, tailCode) =
                raise Fallback "codeToICodeUnaryRev: RealToInt"

        |   codeToICodeUnaryRev({oper=TouchAddress, arg1}, context, _, destination, tailCode) =
                raise Fallback "codeToICodeUnaryRev: TouchAddress"

        |   codeToICodeUnaryRev({oper=AllocCStack, arg1}, context, _, destination, tailCode) =
                raise Fallback "codeToICodeUnaryRev: AllocCStack"

        |   codeToICodeUnaryRev({oper=LockMutex, arg1}, context, _, destination, tailCode) =
                raise Fallback "codeToICodeUnaryRev: LockMutex"

        |   codeToICodeUnaryRev({oper=TryLockMutex, arg1}, context, _, destination, tailCode) =
                raise Fallback "codeToICodeUnaryRev: TryLockMutex"

        |   codeToICodeUnaryRev({oper=UnlockMutex, arg1}, context, _, destination, tailCode) =
                raise Fallback "codeToICodeUnaryRev: UnlockMutex"

        and codeToICodeBinaryRev({oper=WordComparison{test, isSigned}, arg1, arg2},
                context, _, destination, tailCode) =
            let
                (* Comparisons. This is now only used for tagged values, not for pointer equality. *)
                val ccRef = newCCRef()
                val (testCode1, testDest1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (testCode2, testDest2, _) = codeToICodeRev(arg2, context, false, AnyReg, testCode1)
                val comparison =
                    addSubRegister{base=testDest1, shifted=testDest2, dest=NONE, length=polyWordOpSize,
                                   ccRef=SOME ccRef, isAdd=false, shift=ShiftNone} :: testCode2
                val target = asTarget destination
                open BuiltIns
                val cond =
                    case (test, isSigned) of
                        (TestEqual,         _) => CondEqual
                    |   (TestLess,          true) => CondSignedLess
                    |   (TestLessEqual,     true) => CondSignedLessEq
                    |   (TestGreater,       true) => CondSignedGreater
                    |   (TestGreaterEqual,  true) => CondSignedGreaterEq
                    |   (TestLess,          false) => CondCarryClear
                    |   (TestLessEqual,     false) => CondUnsignedLowOrEq
                    |   (TestGreater,       false) => CondUnsignedHigher
                    |   (TestGreaterEqual,  false) => CondCarrySet
                    |   (TestUnordered,     _) => raise InternalError "WordComparison: TestUnordered"
            in
                (makeBoolResultRev(cond, ccRef, target, comparison), target, false)
            end

        |   codeToICodeBinaryRev({oper=FixedPrecisionArith ArithAdd, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                (* We need to subtract the tag from one of the arguments and then do the
                   addition.  The optimiser will do the subtraction at compile time if we
                   subtract from a constant so try to put the constant in the second arg. *)
                val (firstReg, secondReg) =
                    case arg1 of BICConstnt _ => (aReg2, aReg1) | _ => (aReg1, aReg2)
                val uReg = newUReg()
                val chkOverflow = newCCRef()
                val code =
                    checkOverflow(CondOverflow, context, chkOverflow) @
                    addSubRegister{base=firstReg, shifted=uReg, dest=SOME target, length=polyWordOpSize,
                                 ccRef=SOME chkOverflow, isAdd=true, shift=ShiftNone} ::
                    addSubImmediate{dest=SOME uReg, source=secondReg, immed=0w1, length=polyWordOpSize, ccRef=NONE, isAdd=false}:: arg2Code
            in
                (code , target, false)
            end

        |   codeToICodeBinaryRev({oper=FixedPrecisionArith ArithSub, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                val uReg = newUReg()
                val chkOverflow = newCCRef()
                val code =
                    checkOverflow(CondOverflow, context, chkOverflow) @
                    addSubRegister{base=aReg1, shifted=uReg, dest=SOME target, length=polyWordOpSize,
                                 ccRef=SOME chkOverflow, isAdd=false, shift=ShiftNone} ::
                    addSubImmediate{dest=SOME uReg, source=aReg2, immed=0w1, length=polyWordOpSize, ccRef=NONE, isAdd=false}:: arg2Code
            in
                (code , target, false)
            end

        |   codeToICodeBinaryRev({oper=FixedPrecisionArith ArithMult, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                val uReg1 = newUReg() and uReg2 = newUReg() and uReg3 = newUReg() and uReg4 = newUReg()
                val chkOverflow = newCCRef()

                (* Untag one argument. subtract the tag from the second, multiply and add back the tag. *)
                val multiplyCode =
                    addSubImmediate{dest=SOME target, source=uReg3, immed=0w1, length=polyWordOpSize, ccRef=NONE, isAdd=true} ::
                    multiplication{kind=if is32in64 then SignedMultAddLong else MultAdd64, dest=uReg3, sourceA=NONE,
                                   sourceM=uReg1, sourceN=uReg2} ::
                    addSubImmediate{dest=SOME uReg2, source=aReg2, immed=0w1, length=polyWordOpSize, ccRef=NONE, isAdd=false} ::
                    untagValue{ source=aReg1, dest=uReg1, isSigned=true, opSize=polyWordOpSize } ::
                    arg2Code

                (* Overflow check:  The condition for overflow is that the high order part (64-bits in native
                   64-bits, 32-bits in 32-in-64) must be zero if the result is positive and all ones if the
                   result is negative.  The high-order part is in uReg3 in 32-in-64 since we've already used
                   SignedMultAddLong but in native 64-bits we need to use SignedMultHigh to get the
                   high order part.  In both cases we can use a comparison with ShiftASR to give a
                   value containing just the sign of the result. *)
                val checkOverflowCode =
                    if is32in64
                    then addSubRegister{ base=uReg4, shifted=target, dest=NONE, ccRef=SOME chkOverflow,
                            isAdd=false, length=OpSize32, shift=ShiftASR 0w31 } ::
                         shiftConstant{direction=Arm64ICode.ShiftRightArithmetic, source=uReg3, dest=uReg4, shift=0w32,
                            opSize=OpSize64 (* Have to start with 64-bits *)} :: multiplyCode
                    else addSubRegister{ base=uReg4, shifted=target, dest=NONE, ccRef=SOME chkOverflow,
                            isAdd=false, length=OpSize64, shift=ShiftASR 0w63 } ::
                         multiplication{kind=SignedMultHigh, dest=uReg4, sourceA=NONE,
                                       sourceM=uReg1, sourceN=uReg2} :: multiplyCode
                val code =
                    checkOverflow(CondNotEqual, context, chkOverflow) @ checkOverflowCode
            in
                (code, target, false)
            end

        |   codeToICodeBinaryRev({oper=FixedPrecisionArith ArithQuot, arg1, arg2}, context, _, destination, tailCode) =
            let
                (* The word version avoids an extra shift.  Don't do that here at least
                   for the moment.  Division by zero and overflow are checked for at
                   the higher level. *)
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                val uReg1 = newUReg() and uReg2 = newUReg() and uReg3 = newUReg()
                val code =
                    tagValue { source=uReg3, dest=target, opSize=polyWordOpSize, isSigned=true } ::
                    division{isSigned=true, opSize=polyWordOpSize, dest=uReg3, dividend=uReg1, divisor=uReg2} ::
                    untagValue{ source=aReg2, dest=uReg2, isSigned=true, opSize=polyWordOpSize } ::
                    untagValue{ source=aReg1, dest=uReg1, isSigned=true, opSize=polyWordOpSize } :: arg2Code
            in
                (code, target, false)
            end

        |   codeToICodeBinaryRev({oper=FixedPrecisionArith ArithRem, arg1, arg2}, context, _, destination, tailCode) =
            let
                (* For the moment we remove the tags and then retag afterwards.  The word
                   version avoids this but at least for the moment we do it the longer way. *)
                (* There's no direct way to get the remainder - have to use divide and multiply. *)
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                val uReg1 = newUReg() and uReg2 = newUReg() and uReg3 = newUReg() and uReg4 = newUReg()
                val code =
                    tagValue { source=uReg4, dest=target, opSize=polyWordOpSize, isSigned=true } ::
                    multiplication{kind=if is32in64 then MultSub32 else MultSub64, dest=uReg4,
                                   sourceM=uReg3, sourceN=uReg2, sourceA=SOME aReg1} ::
                    division{isSigned=true, opSize=polyWordOpSize, dest=uReg3, dividend=uReg1, divisor=uReg2} ::
                    untagValue{ source=aReg2, dest=uReg2, isSigned=true, opSize=polyWordOpSize } ::
                    untagValue{ source=aReg1, dest=uReg1, isSigned=true, opSize=polyWordOpSize } :: arg2Code
            in
                (code, target, false)
            end

        |   codeToICodeBinaryRev({oper=FixedPrecisionArith ArithDiv, ...}, _, _, _, _) =
               raise InternalError "unimplemented operation: FixedPrecisionArith ArithDiv"

        |   codeToICodeBinaryRev({oper=FixedPrecisionArith ArithMod, ...}, _, _, _, _) =
                raise InternalError "unimplemented operation: FixedPrecisionArith ArithMod"

        |   codeToICodeBinaryRev({oper=WordArith ArithAdd, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                (* We need to subtract the tag from one of the arguments and then do the
                   addition.  The optimiser will do the subtraction at compile time if we
                   subtract from a constant so try to put the constant in the second arg. *)
                val (firstReg, secondReg) =
                    case arg1 of BICConstnt _ => (aReg2, aReg1) | _ => (aReg1, aReg2)
                val uReg = newUReg()
                val code =
                    addSubRegister{base=firstReg, shifted=uReg, dest=SOME target, length=polyWordOpSize,
                                 ccRef=NONE, isAdd=true, shift=ShiftNone} ::
                    addSubImmediate{dest=SOME uReg, source=secondReg, immed=0w1, length=polyWordOpSize, ccRef=NONE, isAdd=false}:: arg2Code
            in
                (code , target, false)
            end

        |   codeToICodeBinaryRev({oper=WordArith ArithSub, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                val uReg = newUReg()
                (* TODO: If the first argument is a constant we could add one to that rather than
                   subtracting one from the second argument.  We're not concerned with overflow. *)
                val code =
                    addSubRegister{base=aReg1, shifted=uReg, dest=SOME target, length=polyWordOpSize,
                                 ccRef=NONE, isAdd=false, shift=ShiftNone} ::
                    addSubImmediate{dest=SOME uReg, source=aReg2, immed=0w1, length=polyWordOpSize, ccRef=NONE, isAdd=false}:: arg2Code
            in
                (code , target, false)
            end

        |   codeToICodeBinaryRev({oper=WordArith ArithMult, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                val uReg1 = newUReg() and uReg2 = newUReg() and uReg3 = newUReg()
                (* Untag one argument. subtract the tag from the second, multiply and add back the tag. *)
                val code =
                    addSubImmediate{dest=SOME target, source=uReg3, immed=0w1, length=polyWordOpSize, ccRef=NONE, isAdd=true} ::
                    multiplication{kind=if is32in64 then MultAdd32 else MultAdd64, dest=uReg3, sourceA=NONE,
                                   sourceM=uReg1, sourceN=uReg2} ::
                    addSubImmediate{dest=SOME uReg2, source=aReg2, immed=0w1, length=polyWordOpSize, ccRef=NONE, isAdd=false} ::
                    untagValue{ source=aReg1, dest=uReg1, isSigned=false, opSize=polyWordOpSize } :: arg2Code
            in
                (code, target, false)
            end

        |   codeToICodeBinaryRev({oper=WordArith ArithDiv, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                val uReg1 = newUReg() and uReg2 = newUReg() and uReg3 = newUReg()
                (* Untag the divisor (into uReg2). subtract the tag from the dividend (into uReg1),
                   divide and or in the tag.  The tag may have been set already depending on
                   the result of the division. *)
                val code =
                    logicalImmediate{dest=SOME target, source=uReg3, immed=0w1, length=polyWordOpSize, ccRef=NONE, logOp=LogOr} ::
                    division{isSigned=false, opSize=polyWordOpSize, dest=uReg3, dividend=uReg1, divisor=uReg2} ::
                    addSubImmediate{dest=SOME uReg1, source=aReg1, immed=0w1, length=polyWordOpSize, ccRef=NONE, isAdd=false} ::
                    untagValue{ source=aReg2, dest=uReg2, isSigned=false, opSize=polyWordOpSize } :: arg2Code
            in
                (code, target, false)
            end

        |   codeToICodeBinaryRev({oper=WordArith ArithMod, arg1, arg2}, context, _, destination, tailCode) =
            let
                (* There's no direct way to get the remainder - have to use divide and multiply. *)
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                val uReg1 = newUReg() and uReg2 = newUReg() and uReg3 = newUReg() and uReg4 = newUReg()
                (* Untag the divisor (into uReg2). subtract the tag from the dividend (into uReg1)
                Untag one argument. subtract the tag from the second, divide and or in the tag.  The
                   tag may have been set already depending on the result of the division. *)
                val tagBitMask = Word64.<<(Word64.fromInt ~1, 0w1) (* Requires a 64-bit AND. *)
                val code =
                    (* Multiply the result of the division by the divisor and subtract this from the original, tagged
                       dividend.  This leaves us a tagged value so it can go straight into the result. *)
                    multiplication{kind=if is32in64 then MultSub32 else MultSub64, dest=target,
                                   sourceM=uReg4, sourceN=uReg2, sourceA=SOME aReg1} ::
                    (* Clear the bottom bit before the multiplication. *)
                    logicalImmediate{dest=SOME uReg4, source=uReg3, immed=tagBitMask, length=OpSize64, ccRef=NONE, logOp=LogAnd} ::
                    division{isSigned=false, opSize=polyWordOpSize, dest=uReg3, dividend=uReg1, divisor=uReg2} ::
                    addSubImmediate{dest=SOME uReg1, source=aReg1, immed=0w1, length=polyWordOpSize, ccRef=NONE, isAdd=false} ::
                    untagValue{ source=aReg2, dest=uReg2, isSigned=false, opSize=polyWordOpSize } :: arg2Code
            in
                (code, target, false)
            end

        |   codeToICodeBinaryRev({oper=WordArith _, ...}, _, _, _, _) =
                raise InternalError "WordArith - unimplemented instruction"

        |   codeToICodeBinaryRev({oper=WordLogical LogicalAnd, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                (* Since both values are tagged the tag will be preserved. *)
                val code =
                    logicalRegister{base=aReg1, shifted=aReg2, dest=SOME target, length=polyWordOpSize,
                                    ccRef=NONE, logOp=LogAnd, shift=ShiftNone} :: arg2Code
            in
                (code , target, false)
            end

        |   codeToICodeBinaryRev({oper=WordLogical LogicalOr, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                (* Since both values are tagged the tag will be preserved. *)
                val code =
                    logicalRegister{base=aReg1, shifted=aReg2, dest=SOME target, length=polyWordOpSize,
                                    ccRef=NONE, logOp=LogOr, shift=ShiftNone} :: arg2Code
            in
                (code , target, false)
            end

        |   codeToICodeBinaryRev({oper=WordLogical LogicalXor, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                (* If we just XOR the values together the tag bit in the result will be zero.
                   It's better to remove one of the tag bits beforehand.  As with Add, we
                   try to choose a constant. *)
                val (firstReg, secondReg) =
                    case arg1 of BICConstnt _ => (aReg2, aReg1) | _ => (aReg1, aReg2)
                val uReg = newUReg()
                val code =
                    logicalRegister{base=firstReg, shifted=uReg, dest=SOME target, length=polyWordOpSize,
                                 ccRef=NONE, logOp=LogXor, shift=ShiftNone} ::
                    addSubImmediate{dest=SOME uReg, source=secondReg, immed=0w1, length=polyWordOpSize, ccRef=NONE, isAdd=false}:: arg2Code
            in
                (code , target, false)
            end

        |   codeToICodeBinaryRev({oper=WordShift ShiftLeft, arg1, arg2}, context, _, destination, tailCode) =
            let
                val ureg1 = newUReg() and ureg2 = newUReg() and ureg3 = newUReg()
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                val code =
                    (* Put back the tag. *)
                    logicalImmediate{ source=ureg3, dest=SOME target, ccRef=NONE, immed=0w1, logOp=LogOr, length=polyWordOpSize } ::
                    (* Do the shift *)
                    shiftRegister{direction=Arm64ICode.ShiftLeft, dest=ureg3, source=ureg1, shift=ureg2, opSize=polyWordOpSize} ::
                    (* Untag the shift amount.  Since it's at most 64 we can use a 32-bit operation. *)
                    untagValue{source=aReg2, dest=ureg2, opSize=OpSize32, isSigned=false} ::
                    (* Remove tag bit from the value we're shifting. *)
                    logicalImmediate{ source=aReg1, dest=SOME ureg1, ccRef=NONE, immed=polyWordTagBitMask,
                              logOp=LogAnd, length=polyWordOpSize } :: arg2Code
            in
                (code, target, false)
            end

        |   codeToICodeBinaryRev({oper=WordShift ShiftRightLogical, arg1, arg2}, context, _, destination, tailCode) =
            let
                val ureg1 = newUReg() and ureg2 = newUReg()
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                val code =
                    (* Put back the tag. *)
                    logicalImmediate{ source=ureg2, dest=SOME target, ccRef=NONE, immed=0w1, logOp=LogOr, length=polyWordOpSize } ::
                    (* Do the shift *)
                    shiftRegister{direction=Arm64ICode.ShiftRightLogical, dest=ureg2, source=aReg1, shift=ureg1, opSize=polyWordOpSize} ::
                    (* Untag the shift amount.  Since it's at most 64 we can use a 32-bit operation. *)
                    untagValue{source=aReg2, dest=ureg1, opSize=OpSize32, isSigned=false} :: arg2Code
                    
            in
                (code, target, false)
            end

        |   codeToICodeBinaryRev({oper=WordShift ShiftRightArithmetic, arg1, arg2}, context, _, destination, tailCode) =
            let
                val ureg1 = newUReg() and ureg2 = newUReg()
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                val code =
                    (* Put back the tag. *)
                    logicalImmediate{ source=ureg2, dest=SOME target, ccRef=NONE, immed=0w1, logOp=LogOr, length=polyWordOpSize } ::
                    (* Do the shift *)
                    shiftRegister{direction=Arm64ICode.ShiftRightArithmetic, dest=ureg2, source=aReg1, shift=ureg1, opSize=polyWordOpSize} ::
                    (* Untag the shift amount.  Since it's at most 64 we can use a 32-bit operation. *)
                    untagValue{source=aReg2, dest=ureg1, opSize=OpSize32, isSigned=false} :: arg2Code
                    
            in
                (code, target, false)
            end

        |   codeToICodeBinaryRev({oper=AllocateByteMemory, arg1, arg2}, context, _, destination, tailCode) =
            let
                (* Allocate a block of memory and without initialisation.  If the flags include
                   the "bytes" bit the GC won't look at it so it doesn't matter that it's not
                   initialised.
                   This is identical to AllocateWordMemory apart from the lack of initialisation. *)
                val target = asTarget destination
                val (codeSize, sizeReg, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (codeFlags, flagsReg, _) = codeToICodeRev(arg2, context, false, AnyReg, codeSize)
                val uSizeReg = newUReg() and shiftFReg = newUReg() and lengthWord = newUReg()
                val absAddr = if is32in64 then newUReg() else target
                val untagSize = untagValue{source=sizeReg, dest=uSizeReg, opSize=polyWordOpSize, isSigned=false} :: codeFlags
                val allocateMem = allocateMemoryVariable{ size=uSizeReg, dest=absAddr, saveRegs=[]} :: untagSize
                (* Make the length word by first shifting the flags into the length word reg by
                   55 or 23 bits.  This puts the tag bit in the top bit of the size.  Then insert the size
                   into this which will overwrite the flag's tag bit. *)
                val makeLengthWord =
                    bitFieldInsert{ source=uSizeReg, destAsSource=shiftFReg, dest=lengthWord, length=polyWordOpSize,
                              immr=0w0 (*bit 0*), imms=if is32in64 then 0w23 else 0w55 (*width-1*) } ::
                    shiftConstant{direction=Arm64ICode.ShiftLeft, dest=shiftFReg, source=flagsReg,
                        shift=if is32in64 then 0w23 else 0w55, opSize=polyWordOpSize } :: allocateMem
                val setLengthWordAndInit =
                    storeWithConstantOffset{ source=lengthWord, base=absAddr,
                        byteOffset= ~(Word.toInt wordSize), loadType=polyWordLoadSize } :: makeLengthWord
                val finalCode =
                    if is32in64 then absoluteToObjectIndex{ source=absAddr, dest=target } :: setLengthWordAndInit
                    else setLengthWordAndInit
            in
                (finalCode, target, false)
            end

        |   codeToICodeBinaryRev({oper=LargeWordComparison test, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val ccRef = newCCRef()
                val (testCode1, testDest1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (testCode2, testDest2, _) = codeToICodeRev(arg2, context, false, AnyReg, testCode1)
                val uReg1 = newUReg() and uReg2 = newUReg()
                val comparison =
                    addSubRegister{base=uReg1, shifted=uReg2, dest=NONE, length=OpSize64,
                                   ccRef=SOME ccRef, isAdd=false, shift=ShiftNone} ::
                    unboxLarge{ source=testDest2, dest=uReg2 } ::
                    unboxLarge{ source=testDest1, dest=uReg1 } :: testCode2
                open BuiltIns
                val cond =
                    case test of
                        TestEqual => CondEqual
                    |   TestLess => CondCarryClear
                    |   TestLessEqual => CondUnsignedLowOrEq
                    |   TestGreater => CondUnsignedHigher
                    |   TestGreaterEqual => CondCarrySet
                    |   TestUnordered => raise InternalError "LargeWordComparison: TestUnordered"
            in
                (makeBoolResultRev(cond, ccRef, target, comparison), target, false)
            end

        |   codeToICodeBinaryRev({oper=LargeWordArith ArithAdd, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                val uReg1 = newUReg() and uReg2 = newUReg() and uReg3 = newUReg()
                val code =
                    boxLarge{ source=uReg3, dest=target, saveRegs=[] } ::
                    addSubRegister{base=uReg1, shifted=uReg2, dest=SOME uReg3, length=OpSize64,
                                   ccRef=NONE, isAdd=true, shift=ShiftNone} ::
                    unboxLarge{ source=aReg2, dest=uReg2 } ::
                    unboxLarge{ source=aReg1, dest=uReg1 } :: arg2Code
            in
                (code, target, false)
            end

        |   codeToICodeBinaryRev({oper=LargeWordArith ArithSub, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                val uReg1 = newUReg() and uReg2 = newUReg() and uReg3 = newUReg()
                val code =
                    boxLarge{ source=uReg3, dest=target, saveRegs=[] } ::
                    addSubRegister{base=uReg1, shifted=uReg2, dest=SOME uReg3, length=OpSize64,
                                   ccRef=NONE, isAdd=false, shift=ShiftNone} ::
                    unboxLarge{ source=aReg2, dest=uReg2 } ::
                    unboxLarge{ source=aReg1, dest=uReg1 } :: arg2Code
            in
                (code, target, false)
            end

        |   codeToICodeBinaryRev({oper=LargeWordArith ArithMult, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                val uReg1 = newUReg() and uReg2 = newUReg() and uReg3 = newUReg()
                val code =
                    boxLarge{ source=uReg3, dest=target, saveRegs=[] } ::
                    multiplication{kind=MultAdd64, sourceA=NONE, sourceM=uReg1, sourceN=uReg2, dest=uReg3} ::
                    unboxLarge{ source=aReg2, dest=uReg2 } ::
                    unboxLarge{ source=aReg1, dest=uReg1 } :: arg2Code
            in
                (code, target, false)
            end

        |   codeToICodeBinaryRev({oper=LargeWordArith ArithDiv, arg1, arg2}, context, isTail, destination, tailCode) =
            raise Fallback "codeToICodeBinaryRev: LargeWordArith: ArithDiv"

        |   codeToICodeBinaryRev({oper=LargeWordArith ArithMod, arg1, arg2}, context, isTail, destination, tailCode) =
            raise Fallback "codeToICodeBinaryRev: LargeWordArith: ArithMod"

        |   codeToICodeBinaryRev({oper=LargeWordArith _, ...}, _, _, _, _) =
                raise InternalError "LargeWordArith - unimplemented instruction"

        |   codeToICodeBinaryRev({oper=LargeWordLogical logop, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                val uReg1 = newUReg() and uReg2 = newUReg() and uReg3 = newUReg()
                val logicalOp = case logop of LogicalAnd => LogAnd | LogicalOr => LogOr | LogicalXor => LogXor
                val code =
                    boxLarge{ source=uReg3, dest=target, saveRegs=[] } ::
                    logicalRegister{base=uReg1, shifted=uReg2, dest=SOME uReg3, length=OpSize64,
                                   ccRef=NONE, logOp=logicalOp, shift=ShiftNone} ::
                    unboxLarge{ source=aReg2, dest=uReg2 } ::
                    unboxLarge{ source=aReg1, dest=uReg1 } :: arg2Code
            in
                (code, target, false)
            end

        |   codeToICodeBinaryRev({oper=LargeWordShift shiftKind, arg1, arg2}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val (arg1Code, aReg1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (arg2Code, aReg2, _) = codeToICodeRev(arg2, context, false, AnyReg, arg1Code)
                val uReg1 = newUReg() and uReg2 = newUReg() and uReg3 = newUReg()
                val shiftType =
                    case shiftKind of
                        ShiftLeft => Arm64ICode.ShiftLeft
                    |   ShiftRightLogical => Arm64ICode.ShiftRightLogical
                    |   ShiftRightArithmetic => Arm64ICode.ShiftRightArithmetic
                val code =
                    boxLarge{ source=uReg3, dest=target, saveRegs=[] } ::
                    shiftRegister{direction=shiftType, source=uReg1, shift=uReg2, dest=uReg3, opSize=OpSize64 } ::
                    (* The shift amount is a word, not a large word. *)
                    untagValue{ source=aReg2, dest=uReg2, opSize=OpSize32, isSigned=false } ::
                    unboxLarge{ source=aReg1, dest=uReg1 } :: arg2Code
            in
                (code, target, false)
            end

        |   codeToICodeBinaryRev({oper=RealComparison _, arg1, arg2}, context, isTail, destination, tailCode) =
            raise Fallback "codeToICodeBinaryRev: RealComparison"

        |   codeToICodeBinaryRev({oper=RealArith _, arg1, arg2}, context, isTail, destination, tailCode) =
            raise Fallback "codeToICodeBinaryRev: RealArith"

        |   codeToICodeBinaryRev({oper=PointerEq, arg1, arg2}, context, _, destination, tailCode) =
            let
                (* Equality of general values which can include pointers. This can be treated exactly as a word equality.
                   It has to be analysed differently for indexed cases. *)
                val ccRef = newCCRef()
                val (testCode1, testDest1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (testCode2, testDest2, _) = codeToICodeRev(arg2, context, false, AnyReg, testCode1)
                val comparison =
                    addSubRegister{base=testDest1, shifted=testDest2, dest=NONE, length=polyWordOpSize,
                                   ccRef=SOME ccRef, isAdd=false, shift=ShiftNone} :: testCode2
                val target = asTarget destination
            in
                (makeBoolResultRev(CondEqual, ccRef, target, comparison), target, false)
            end

        |   codeToICodeBinaryRev({oper=FreeCStack, arg1, arg2}, context, isTail, destination, tailCode) =
            raise Fallback "codeToICodeBinaryRev: FreeCStack"

        (* Store the code address and the closure items into a previously allocated closure on the
           heap.  This is used both in the simple case and also with mutually recursive declarations. *)
        and storeIntoClosure(lambda as { closure, ...}, absClosureAddr, context, tailCode) =
        let
            val closureRef = makeConstantClosure()
            val () = codeFunctionToArm64(lambda, debugSwitches, closureRef)
            val codeAddrWords = if is32in64 then 2 else 1

            fun storeAValue(f, (n, tlCode)) =
            let
                val (code, source, _) = codeToICodeRev(BICExtract f, context, false, AnyReg, tlCode)
            in
                (n+1, storeAtWordOffset(source, n, absClosureAddr, polyWordLoadSize, code))
            end
            (* Store the code address in the first 64-bits. *)
            val storeCodeAddress =
                if is32in64
                then
                let
                    (* We can't use codeAddressFromClosure on 32-in-64 because it always returns
                       a 64-bit value.  Instead we have to get the code address at run-time. *)
                    val clReg = newPReg() and absClReg = newUReg() and absCodeReg = newUReg()
                in
                    storeAtWordOffset(absCodeReg, 0, absClosureAddr, Load64,
                        loadWithConstantOffset{base=absClReg, dest=absCodeReg, byteOffset=0, loadType=Load64} ::
                        objectIndexAddressToAbsolute{ source=clReg, dest=absClReg } ::
                        loadAddressConstant{source=closureAsAddress closureRef, dest=clReg} :: tailCode)
                end
                else
                let
                    val cReg = newPReg()
                in
                    storeAtWordOffset(cReg, 0, absClosureAddr, Load64,
                        loadAddressConstant{source=codeAddressFromClosure closureRef, dest=cReg} :: tailCode)
                end
            val (_, storeCode) = List.foldl storeAValue (codeAddrWords, storeCodeAddress) closure
        in
            storeCode
        end

        (* Load operations. *)
        and codeLoadOperation(kind, address, context, target, tailCode) =
        let
            val code =
            case kind of
                LoadStoreMLWord {isImmutable=false} =>
                let
                    fun loadOp(addrReg, _, code) =
                        loadAcquire{base=addrReg, dest=target, loadType=polyWordLoadSize} :: code
                in
                    loadAndStoreWithAbsolute codeToICodeRev (address, fn code => (code, ()), polyWordLoadSize, loadOp, context, tailCode)
                end

            |   LoadStoreMLWord {isImmutable=true} =>
                let
                    fun loadConstOffset(base, offset, _, code) =
                        loadWithConstantOffset{base=base, dest=target, byteOffset=offset, loadType=polyWordLoadSize} :: code
                    fun loadIndexed(base, index, _, code) =
                        loadWithIndexedOffset{base=base, dest=target, index=index, loadType=polyWordLoadSize} :: code
                in
                    loadAndStoreWithAddress codeToICodeRev
                        (address, fn code => (code, ()), polyWordLoadSize, false, loadConstOffset, loadIndexed, context, tailCode)
                end

            |   LoadStoreMLByte {isImmutable=false} =>
                let
                    (* Have to load into a ureg and then tag it. *)
                    val destReg = newUReg()
                    fun loadOp(addrReg, _, code) =
                        loadAcquire{base=addrReg, dest=destReg, loadType=Load8} :: code
                in
                    tagValue{source=destReg, dest=target, isSigned=false, opSize=OpSize32} ::
                        loadAndStoreWithAbsolute codeToICodeRev (address, fn code => (code, ()), Load8, loadOp, context, tailCode)
                end

            |   LoadStoreMLByte {isImmutable=true} =>
                let
                    (* Have to load into a ureg and then tag it. *)
                    val destReg = newUReg()
                    fun loadConstOffset(base, offset, _, code) =
                        loadWithConstantOffset{base=base, dest=destReg, byteOffset=offset, loadType=Load8} :: code
                    fun loadIndexed(base, index, _, code) =
                        loadWithIndexedOffset{base=base, dest=destReg, index=index, loadType=Load8} :: code
                in
                    tagValue{source=destReg, dest=target, isSigned=false, opSize=OpSize32} ::
                        loadAndStoreWithAddress codeToICodeRev
                            (address, fn code => (code, ()), Load8, false, loadConstOffset, loadIndexed, context, tailCode)
                end

            |   LoadStoreC8 => raise Fallback "codeLoadOperation: LoadStoreC8 immutable"
            |   LoadStoreC16 => raise Fallback "codeLoadOperation: LoadStoreC8 immutable"
            |   LoadStoreC32 => raise Fallback "codeLoadOperation: LoadStoreC8 immutable"
            |   LoadStoreC64 => raise Fallback "codeLoadOperation: LoadStoreC8 immutable"
            |   LoadStoreCFloat => raise Fallback "codeLoadOperation: LoadStoreC8 immutable"
            |   LoadStoreCDouble => raise Fallback "codeLoadOperation: LoadStoreC8 immutable"

            |   LoadStoreUntaggedUnsigned =>
                let
                    (* LoadStoreMLWord {isImmutable=true} except it has to be tagged. *)
                    val ureg = newUReg()
                    fun loadConstOffset(base, offset, _, code) =
                        loadWithConstantOffset{base=base, dest=ureg, byteOffset=offset, loadType=polyWordLoadSize} :: code
                    fun loadIndexed(base, index, _, code) =
                        loadWithIndexedOffset{base=base, dest=ureg, index=index, loadType=polyWordLoadSize} :: code
                in
                    tagValue{source=ureg, dest=target, isSigned=false, opSize=polyWordOpSize} ::
                        loadAndStoreWithAddress codeToICodeRev
                            (address, fn code => (code, ()), polyWordLoadSize, false, loadConstOffset, loadIndexed, context, tailCode)
                end

        in
            (code, target, false)
        end

        (* Store operations. *)
        and codeStoreOperation(kind, address, value, context, destination, tailCode) =
        let
            val storeCode =
            case kind of
                LoadStoreMLWord {isImmutable=false} =>
                let
                    fun getStoreValue code =
                        let val (sCode, sReg, _) = codeToICodeRev(value, context, false, AnyReg, code) in (sCode, sReg) end
                    fun storeOp(addrReg, valReg, code) =
                        storeRelease{base=addrReg, source=valReg, loadType=polyWordLoadSize} :: code
                in
                    loadAndStoreWithAbsolute codeToICodeRev (address, getStoreValue, polyWordLoadSize, storeOp, context, tailCode)
                end

            |   LoadStoreMLWord {isImmutable=true} => raise InternalError "codeStoreOperation: MLWord immutable" (* Not allowed *)

            |   LoadStoreMLByte {isImmutable=false} =>
                let
                    fun getStoreValue code =
                    let
                        (* The value to store has to be untagged. *)
                        val (sCode, sReg, _) = codeToICodeRev(value, context, false, AnyReg, code)
                        val tReg = newUReg()
                    in
                        (untagValue{source=sReg, dest=tReg, isSigned=false, opSize=OpSize32} :: sCode, tReg)
                    end
                    fun storeOp(addrReg, valReg, code) =
                        storeRelease{base=addrReg, source=valReg, loadType=Load8} :: code
                in
                    loadAndStoreWithAbsolute codeToICodeRev (address, getStoreValue, Load8, storeOp, context, tailCode)
                end

            |   LoadStoreMLByte {isImmutable=true} => raise InternalError "codeStoreOperation: MLByte immutable" (* Not allowed *)
            |   LoadStoreC8 => raise Fallback "codeStoreOperation: LoadStoreC8 "
            |   LoadStoreC16 => raise Fallback "codeStoreOperation: LoadStoreC16 "
            |   LoadStoreC32 => raise Fallback "codeStoreOperation: LoadStoreC32 "
            |   LoadStoreC64 => raise Fallback "codeStoreOperation: LoadStoreC64 "
            |   LoadStoreCFloat => raise Fallback "codeStoreOperation: LoadStoreCFloat "
            |   LoadStoreCDouble => raise Fallback "codeStoreOperation: LoadStoreCDouble "
            |   LoadStoreUntaggedUnsigned => raise Fallback "codeStoreOperation: LoadStoreUntaggedUnsigned"
        in
            returnUnit(destination, storeCode, false)
        end


        (*Turn the codetree structure into icode. *)
        val bodyContext = {loopArgs=NONE, stackPtr=0, currHandler=NONE, overflowBlock=ref NONE}
        val (bodyCode, _, bodyExited) =
            codeToICodeRev(body, bodyContext, true, SpecificPReg resultTarget, beginInstructions)
        val icode = if bodyExited then bodyCode else returnInstruction(bodyContext, resultTarget, bodyCode)
        
        (* Turn the icode list into basic blocks.  The input list is in reverse so as part of
           this we reverse the list. *)
        local
            val resArray = Array.array(!labelCounter, BasicBlock{ block=[], flow=ExitCode })
            
            fun createEntry (blockNo, block, flow) =
                Array.update(resArray, blockNo, BasicBlock{ block=block, flow=flow})
            
            fun splitCode([], _, _) = 
                (* End of code.  We should have had a BeginFunction. *)
                raise InternalError "splitCode - no begin"
            
            |   splitCode(BlockBegin args :: _, sinceLabel, flow) =
                    (* Final instruction.  Create the initial block and exit. *)
                    createEntry(0, BeginFunction args ::sinceLabel, flow)
            
            |   splitCode(BlockSimple instr :: rest, sinceLabel, flow) =
                    splitCode(rest, instr :: sinceLabel, flow)

            |   splitCode(BlockLabel label :: rest, sinceLabel, flow) =
                    (* Label - finish this block and start another. *)
                (
                    createEntry(label, sinceLabel, flow);
                    (* Default to a jump to this label.  That is used if we have
                       assumed a drop-through. *)
                    splitCode(rest, [], Unconditional label)
                )
            
            |   splitCode(BlockExit instr :: rest, _, _) =
                    splitCode(rest, [instr], ExitCode)

            |   splitCode(BlockFlow flow :: rest, _, _) =
                    splitCode(rest, [], flow)
            
            |   splitCode(BlockRaiseAndHandle(instr, handler) :: rest, _, _) =
                    splitCode(rest, [instr], UnconditionalHandle handler)

            |   splitCode(BlockOptionalHandle{call, handler, label} :: rest, sinceLabel, flow) =
                let
                    (* A function call within a handler.  This could go to the handler but
                       if there is no exception will go to the next instruction.
                       Also includes JumpLoop since the stack check could result in an
                       Interrupt exception. *)
                in
                    createEntry(label, sinceLabel, flow);
                    splitCode(rest, [call], ConditionalHandle{handler=handler, continue=label})
                end

        in
            val () = splitCode(icode, [], ExitCode)
            val resultVector = Array.vector resArray
        end
      
        open ICodeTransform
        
        val pregProperties = Vector.fromList(List.rev(! pregPropList))
    in
        codeICodeFunctionToArm64{blocks = resultVector, functionName = name, pregProps = pregProperties,
            ccCount= ! ccRefCounter, debugSwitches = debugSwitches, resultClosure = resultClosure,
            profileObject = profileObject}
    end

    fun gencodeLambda(lambda as { name, ...}, debugSwitches, closure) =
    let
        open Debug Universal
        val tryNewCG = Debug.getParameter Debug.compilerDebugTag debugSwitches
    in
        if false andalso tryNewCG = 0
        then Arm64Fallback.gencodeLambda(lambda, debugSwitches, closure)
        else
        let
            val newDebugSwitches =
                [tagInject icodeTag true,
                tagInject assemblyCodeTag true] @ debugSwitches
            val printStream = Pretty.getSimplePrinter(debugSwitches, [])
        in
            codeFunctionToArm64(lambda, if tryNewCG > 1 then newDebugSwitches else debugSwitches, closure)
                handle Fallback reason =>
                    (
                        printStream "Fallback-"; printStream reason; printStream " at "; printStream name; printStream "\n";
                        Arm64Fallback.gencodeLambda(lambda, debugSwitches, closure)
                    )
        end
    end

    structure Foreign = Arm64Foreign
    
    structure Sharing =
    struct
        type backendIC = backendIC
        and  bicLoadForm = bicLoadForm
        and argumentType = argumentType
        and closureRef = closureRef
    end
    
end;
