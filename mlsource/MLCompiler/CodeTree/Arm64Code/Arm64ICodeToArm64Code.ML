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

functor Arm64ICodeToArm64Code(

    structure Arm64Assembly: ARM64ASSEMBLY
    structure Arm64Sequences: ARM64SEQUENCES
    structure Debug: DEBUG
    structure Arm64ICode: ARM64ICODE
    structure Identify: ARM64IDENTIFYREFERENCES
    structure IntSet: INTSET
    structure Pretty: PRETTY

    structure Strongly:
        sig
            val stronglyConnectedComponents: {nodeAddress: 'a -> int, arcs: 'a -> int list } -> 'a list -> 'a list list
        end
    
    sharing Arm64Assembly.Sharing = Arm64Sequences.Sharing = Arm64ICode.Sharing = Identify.Sharing = IntSet
): ARM64ICODEGENERATE =
struct
    open Identify
    open Arm64ICode
    open Arm64Assembly
    open Arm64Sequences
    open Address
    exception InternalError = Misc.InternalError

    (* These aren't currently used for anything. *)
    val workReg1 = X16 and workReg2 = X17

    fun icodeToArm64Code {blocks, functionName, stackRequired, debugSwitches, allocatedRegisters: reg vector, resultClosure, profileObject, ...} =
    let
        val numBlocks = Vector.length blocks
        fun getAllocatedReg(PReg r) = Vector.sub(allocatedRegisters, r)
        
        fun getAllocatedGenReg r =
            case getAllocatedReg r of GenReg r => r | FPReg _ => raise InternalError "getAllocateGenReg: returned FP Reg"
        and getAllocatedFPReg r =
            case getAllocatedReg r of FPReg r => r | GenReg _ => raise InternalError "getAllocatedFPReg: returned Gen Reg"

        (* Load from and store to stack. *)
        fun loadFromStack(destReg, wordOffset, code) =
            if wordOffset >= 4096
            then loadRegIndexed{regT=destReg, regN=X_MLStackPtr, regM=destReg, option=ExtUXTX ScaleOrShift} ::
                    List.rev(loadNonAddress(destReg, Word64.fromInt wordOffset)) @ code
            else loadRegScaled{regT=destReg, regN=X_MLStackPtr, unitOffset=wordOffset} :: code

        and storeToStack(sourceReg, wordOffset, workReg, code) =
            if wordOffset >= 4096
            then storeRegIndexed{regT=sourceReg, regN=X_MLStackPtr, regM=workReg, option=ExtUXTX ScaleOrShift} ::
                    List.rev(loadNonAddress(workReg, Word64.fromInt wordOffset)) @ code
            else storeRegScaled{regT=sourceReg, regN=X_MLStackPtr, unitOffset=wordOffset} :: code

        datatype srcAndDest = IsInReg of xReg | IsOnStack of int

        local
            (* The registers are numbered from 0.  Choose values that don't conflict with
               the stack addresses. *)
            fun regNo(XReg r) = ~1 - Word8.toInt r | regNo _ = ~1 - 31
            type node = {src: srcAndDest, dst: srcAndDest }
            
            fun nodeAddress({dst=IsInReg r, ...}: node) = regNo r
            |   nodeAddress({dst=IsOnStack a, ...}) = a
            
            fun arcs({src=IsOnStack wordOffset, ...}: node) = [wordOffset]
            |   arcs{src=IsInReg r, ...} = [regNo r]
        in
            val stronglyConnected = Strongly.stronglyConnectedComponents { nodeAddress=nodeAddress, arcs=arcs }
        end
        
        (* This is a general function for moving values into registers or to the stack
           where it is possible that the source values might also be in use as destinations.
           The stack is used for destinations only for tail recursive calls. *)
        fun moveMultipleValues(moves, code) =
        let
            fun moveValues ([], code) = code (* We're done. *)

            |   moveValues (arguments, code) =
                let
                    (* stronglyConnectedComponents does two things.  It detects loops where
                       it's not possible to move items without breaking the loop but more
                       importantly it orders the dependencies so that if there are no loops we
                       can load the source and store it in the destination knowing that
                       we won't overwrite anything we might later need. *)
                    
                    val ordered = stronglyConnected arguments
                    
                    fun loadIntoReg(IsInReg sReg, dReg, code) =
                            if sReg = dReg then code else moveRegToReg{sReg=sReg, dReg=dReg} :: code
                    |   loadIntoReg(IsOnStack wordOffset, dReg, code) =
                            loadFromStack(dReg, wordOffset, code)

                    fun moveEachValue ([], code) = code

                    |   moveEachValue ([{dst=IsInReg dReg, src}] :: rest, code) =
                            moveEachValue(rest, loadIntoReg(src, dReg, code))

                    |   moveEachValue ([{dst=IsOnStack wordOffset, src=IsInReg sReg}] :: rest, code) =
                            (* Storing into the stack. *)
                            moveEachValue(rest, storeToStack(sReg, wordOffset, workReg1, code))

                    |   moveEachValue ([{dst=IsOnStack dstOffset, src=IsOnStack srcOffset}] :: rest, code) =
                            (* Copy a stack location - needs a load and store unless the address is the same. *)
                            if dstOffset = srcOffset
                            then moveEachValue(rest, code)
                            else moveEachValue(rest,
                                    storeToStack(workReg2, dstOffset, workReg1,
                                        loadFromStack(workReg2, srcOffset, code)))

                    |   moveEachValue((cycle as first :: _ :: _) :: rest, code) =
                        (* We have a cycle. *)
                        let
                            (* We need to exchange some of the arguments.  Doing an exchange here will
                               set the destination with the correct source.  However we have to process
                               every subsequent entry with the swapped registers.  That may well mean that
                               one of those entries becomes trivial.
                               We also need to rerun stronglyConnectedComponents on at least the rest of
                               this cycle.  It's easiest to flatten the rest and do everything. *)
                            (* Exchange the source and destination.  We don't have an exchange instruction
                               and there's a further complication.  We could be copying between stack locations
                               and their offsets could be > 4096.  Since we've only got two work registers
                               we need to use the hardware stack as an extra location. Stack-stack
                               exchange is very rare so the extra overhead to handle the general case
                               is worth it. *)
                            local
                                fun storeToDest(sReg, IsInReg dReg, _, code) =
                                        moveRegToReg{sReg=sReg, dReg=dReg} :: code
                                |   storeToDest(sReg, IsOnStack wordOffset, work, code) =
                                        storeToStack(sReg, wordOffset, work, code)
                            in
                                fun exchange(IsInReg arg1Reg, arg2, code) =
                                    moveRegToReg{sReg=workReg2, dReg=arg1Reg} ::
                                    storeToDest(arg1Reg, arg2, workReg1,
                                        loadIntoReg(arg2, workReg2, code))

                                |   exchange(arg1, IsInReg arg2Reg, code) =
                                    moveRegToReg{sReg=workReg2, dReg=arg2Reg} ::
                                    storeToDest(arg2Reg, arg1, workReg1,
                                        loadIntoReg(arg1, workReg2, code))

                                |   exchange(arg1, arg2, code) =
                                    (* The hardware stack must be 16-byte aligned. *)
                                    storeToDest(workReg2, arg2, workReg1,
                                        loadRegPostIndex{regT=workReg2, regN=XSP, byteOffset=16} ::
                                        storeToDest(workReg2, arg1, workReg1,
                                            loadIntoReg(arg2, workReg2,
                                                storeRegPreIndex{regT=workReg2, regN=XSP, byteOffset= ~16} ::
                                                    loadIntoReg(arg1, workReg2, code))))
                            end
                            
                            (* Try to find either a register-register move or a register-stack move.
                               If not use the first.  If there's a stack-register move there will
                               also be a register-stack so we don't need to look for both. *)
                               
                            val {dst=selectDst, src=selectSrc} = first

                            (* This includes this entry but after the swap we'll eliminate it. *)
                            val flattened = List.foldl(fn (a, b) => a @ b) [] (cycle :: rest)
                            val destAsSource = selectDst

                            fun match(s1: srcAndDest, s2) = s1 = s2
                            
                            fun swapSources{src, dst} =
                                if match(src, selectSrc) then {src=destAsSource, dst=dst}
                                else if match(src, destAsSource) then {src=selectSrc, dst=dst}
                                else {src=src, dst=dst}

                            val exchangeCode = exchange(selectDst, selectSrc, code)                                    
                        in
                            moveValues(List.map swapSources flattened, exchangeCode)
                        end

                    |   moveEachValue(([]) :: _, _) = (* This should not happen - avoid warning. *)
                            raise InternalError "moveEachValue - empty set"
                in
                    moveEachValue(ordered, code)
                end
        in
            moveValues(moves, code)
        end

        (* Where we have multiple specific registers as either source or
           destination there is the potential that a destination register
           if currently in use as a source. *) 
        fun moveMultipleRegisters(regPairList, code) =
        let
            val regPairsAsDests =
                List.map(fn {src, dst} => {src=IsInReg src, dst=IsInReg dst}) regPairList
        in
            moveMultipleValues(regPairsAsDests, code)
        end

        fun moveIfNecessary({src, dst}, code) =
            if src = dst then code
            else moveRegToReg{sReg=src, dReg=dst} :: code

        (* Add a constant word to the source register and put the result in the
           destination.  regW is used as a work register if necessary.  This is used
           both for addition and subtraction. *)
        fun addConstantWord({regS, regD, value=0w0, ...}, code) =
            if regS = regD then code else moveRegToReg{sReg=regS, dReg=regD} :: code
    
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
                        (if low <> 0w0 then [addSub{regN=regD, regD=regD, immed=low, shifted=false}] else []) @
                        addSub{regN=regS, regD=regD, immed=high, shifted=true} :: code
                    )
                    else addSub{regN=regS, regD=regD, immed=low, shifted=false} :: code
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
                    (if isSub then subShiftedReg else addShiftedReg)
                        {regM=regW, regN=regS, regD=regD, shift=ShiftLSL shift} ::
                    List.rev (loadNonAddress(regW, shifted)) @ code
                end
            end

        val getSaveRegs = List.map getAllocatedGenReg
        
        fun getSaveRegsAndSeparate saveRegs =
        let
            val realSaveRegs = getSaveRegs saveRegs
            val saveX30 = List.exists (fn r => r = X30) realSaveRegs
            val preserve = List.filter (fn r => r <> X30) realSaveRegs
        in
            { saveX30=saveX30, saveRegs=preserve }
        end

        val startOfFunctionLabel = createLabel() (* Used for recursive calls/jumps *)

        val blockToLabelMap = Vector.tabulate(numBlocks, fn _ => createLabel())

        fun getBlockLabel blockNo = Vector.sub(blockToLabelMap, blockNo)

        fun codeExtended _ (MoveRegister{source, dest, ...}, code) =
            moveIfNecessary({src=getAllocatedGenReg source, dst=getAllocatedGenReg dest}, code)

        |   codeExtended _ (LoadNonAddressConstant{source, dest, ...}, code) =
                List.rev(loadNonAddress(getAllocatedGenReg dest, source)) @ code

        |   codeExtended _ (LoadAddressConstant{source, dest, ...}, code) =
                loadAddressConstant(getAllocatedGenReg dest, source) :: code

        |   codeExtended _ (LoadWithConstantOffset{dest, base, byteOffset, loadType, ...}, code) =
            if byteOffset < 0
            then
            let
                val loadInstr =
                    case loadType of
                        Load64 => loadRegUnscaled
                    |   Load32 => loadRegUnscaled32
                    |   Load16 => loadRegUnscaled16
                    |   Load8 => loadRegUnscaledByte
            in
                loadInstr{regT=getAllocatedGenReg dest, regN=getAllocatedGenReg base, byteOffset=byteOffset} :: code
            end
            else
            let
                val (loadInstr, scale) =
                    case loadType of
                        Load64 => (loadRegScaled, 8)
                    |   Load32 => (loadRegScaled32, 4)
                    |   Load16 => (loadRegScaled16, 2)
                    |   Load8 => (loadRegScaledByte, 1)
            in
                loadInstr{regT=getAllocatedGenReg dest, regN=getAllocatedGenReg base, unitOffset=byteOffset div scale} :: code
            end

        |   codeExtended _ (LoadWithIndexedOffset{dest, base, index, loadType, ...}, code) =
            let
                val (loadInstr, opt)  =
                    case loadType of
                        Load64 => (loadRegIndexed, ExtUXTX ScaleOrShift)
                    |   Load32 => (loadRegIndexed32, ExtUXTX ScaleOrShift)
                    |   Load16 => (loadRegIndexed16, ExtUXTX ScaleOrShift)
                    |   Load8 => (loadRegIndexedByte, ExtUXTX NoScale)
            in
                loadInstr{regT=getAllocatedGenReg dest, regN=getAllocatedGenReg base, regM=getAllocatedGenReg index, option=opt} :: code
            end

        |   codeExtended _ (LoadMemReg { wordOffset, dest}, code) =
                (* Load the thread id or RTS exception.  This is always a 64-bit value. *)
                loadRegScaled{regT=getAllocatedGenReg dest, regN=X_MLAssemblyInt, unitOffset=wordOffset} :: code

        |   codeExtended _ (ObjectIndexAddressToAbsolute{source, dest, ...}, code) =
                addShiftedReg{regM=getAllocatedGenReg source, regN=X_Base32in64, regD=getAllocatedGenReg dest, shift=ShiftLSL 0w2} :: code

        |   codeExtended _ (AbsoluteToObjectIndex{source, dest, ...}, code) =
            let
                val destReg = getAllocatedGenReg dest
            in
                logicalShiftRight{shift=0w2, regN=destReg, regD=destReg} ::
                subShiftedReg{regM=X_Base32in64, regN=getAllocatedGenReg source, regD=destReg, shift=ShiftNone} :: code
            end

        |   codeExtended _ (AllocateMemoryFixed{ bytesRequired, dest, saveRegs, ... }, code) =
            let
                val {saveX30, saveRegs=preserve} = getSaveRegsAndSeparate saveRegs
                val label = createLabel()
                val destReg = getAllocatedGenReg dest
                (* N.B.  This is in reverse order so read from the bottom up. *)
            in
                moveRegToReg{sReg=destReg, dReg=X_MLHeapAllocPtr} ::
                setLabel label ::
                (if saveX30 then [loadRegPostIndex{regT=X30, regN=X_MLStackPtr, byteOffset= 8}] else []) @
                registerMask preserve :: branchAndLinkReg workReg1 ::
                loadRegScaled{regT=workReg1, regN=X_MLAssemblyInt, unitOffset=heapOverflowCallOffset} ::
                (if saveX30 then [storeRegPreIndex{regT=X30, regN=X_MLStackPtr, byteOffset= ~8}] else []) @
                conditionalBranch(CondCarrySet, label) :: (* Skip the trap if it's ok. *)
                (* Compare with heap limit. *)
                subSShiftedReg{regM=X_MLHeapLimit, regN=destReg, regD=XZero, shift=ShiftNone} ::
                (* Subtract the number of bytes required from the heap pointer and put in result reg. *)
                (if bytesRequired >= 0w4096
                then subShiftedReg{regM=workReg1, regN=X_MLHeapAllocPtr, regD=destReg, shift=ShiftNone} ::
                     loadNonAddressConstant(workReg1, bytesRequired) :: code
                else subImmediate{regN=X_MLHeapAllocPtr, regD=destReg, immed=Word.fromLarge bytesRequired, shifted=false} :: code)
            end

        |   codeExtended _ (AllocateMemoryVariable{ size, dest, saveRegs, ... }, code) =
            let
                val {saveX30, saveRegs=preserve} = getSaveRegsAndSeparate saveRegs
                val trapLabel = createLabel() and noTrapLabel = createLabel()
                val destReg = getAllocatedGenReg dest and sizeReg = getAllocatedGenReg size
                (* Subtract the size into the result register.  Subtract a further word for
                   the length word and round down in 32-in-64. *)
                val subtractSize =
                    if is32in64
                    then bitwiseAndImmediate{bits= ~ 0w8, regN=destReg, regD=destReg} ::
                            subImmediate{regN=destReg, regD=destReg, immed=0w4, shifted=false} ::
                            subShiftedReg{regM=sizeReg, regN=X_MLHeapAllocPtr, regD=destReg, shift=ShiftLSL 0w2} :: code
                    else subImmediate{regN=destReg, regD=destReg, immed=0w8, shifted=false} ::
                            subShiftedReg{regM=sizeReg, regN=X_MLHeapAllocPtr, regD=destReg, shift=ShiftLSL 0w3} :: code
                (* Check against the limit. If the size is large enough it is possible that this could wrap round. 
                   To check for that we trap if either the result is less than the limit or if it is
                   now greater than the allocation pointer. *)
            in
                (* N.B.  This is in reverse order so read from the bottom up. *)
                moveRegToReg{sReg=destReg, dReg=X_MLHeapAllocPtr} ::
                setLabel noTrapLabel ::
                (if saveX30 then [loadRegPostIndex{regT=X30, regN=X_MLStackPtr, byteOffset= 8}] else []) @
                registerMask preserve :: branchAndLinkReg workReg1 ::
                loadRegScaled{regT=X16, regN=X_MLAssemblyInt, unitOffset=heapOverflowCallOffset} ::
                (if saveX30 then [storeRegPreIndex{regT=X30, regN=X_MLStackPtr, byteOffset= ~8}] else []) @
                setLabel trapLabel ::
                conditionalBranch(CondCarryClear, noTrapLabel) ::
                subSShiftedReg{regM=X_MLHeapAllocPtr, regN=destReg, regD=XZero, shift=ShiftNone} ::
                conditionalBranch(CondCarryClear, trapLabel) ::
                subSShiftedReg{regM=X_MLHeapLimit, regN=destReg, regD=XZero, shift=ShiftNone} :: subtractSize
            end

        |   codeExtended _ (InitialiseMem{ size, addr, init}, code) =
            let
                val sizeReg = getAllocatedGenReg size
                and addrReg = getAllocatedGenReg addr
                and initReg = getAllocatedGenReg init
                val exitLabel = createLabel() and loopLabel = createLabel()
                (* Yhis uses a loop to initialise.  It's possible the size is
                   zero so we have to check at the top of the loop. *)
            in
                setLabel exitLabel ::
                unconditionalBranch loopLabel ::
                (if is32in64
                 then storeRegPreIndex32{regT=initReg, regN=workReg1, byteOffset= ~4}
                 else storeRegPreIndex{regT=initReg, regN=workReg1, byteOffset= ~8}) ::
                conditionalBranch(CondEqual, exitLabel) ::
                (* Are we at the start? *)
                subSShiftedReg{regM=workReg1, regN=addrReg, regD=XZero, shift=ShiftNone} ::
                setLabel loopLabel ::
                (* Add the length in bytes so we point at the end. *)
                addShiftedReg{regM=sizeReg, regN=addrReg, regD=workReg1,
                            shift=ShiftLSL(if is32in64 then 0w2 else 0w3)} :: code
            end

        |   codeExtended _ (BeginLoop, code) = code

        |   codeExtended _ (JumpLoop{regArgs, stackArgs, checkInterrupt}, code) =
            let
                (* TODO: We could have a single list and use ArgOnStack and ArgInReg to distinguish. *)
                fun convertArg(ArgOnStack{wordOffset, ...}) = IsOnStack wordOffset
                |   convertArg(ArgInReg reg) = IsInReg(getAllocatedGenReg reg)
                val extStackArgs =
                    map (fn {wordOffset, src, ...} => {src=convertArg src, dst=IsOnStack wordOffset}) stackArgs
                val extRegArgs =
                    map (fn {dst, src} => {src=convertArg src, dst=convertArg(ArgInReg dst)}) regArgs
                val code2 = moveMultipleValues(extStackArgs @ extRegArgs, code)
            in
                case checkInterrupt of
                    NONE => code2
                |   SOME saveRegs =>
                    let
                        val {saveX30, saveRegs} = getSaveRegsAndSeparate saveRegs
                        val skipCheck = createLabel()
                    in
                        (* Put in stack-check code to allow this to be interrupted. *)
                        setLabel skipCheck ::
                        (if saveX30 then [loadRegPostIndex{regT=X30, regN=X_MLStackPtr, byteOffset= 8}] else []) @
                        registerMask saveRegs ::
                        branchAndLinkReg workReg1 ::
                        loadRegScaled{regT=workReg1, regN=X_MLAssemblyInt, unitOffset=stackOverflowCallOffset} ::
                        (if saveX30 then [storeRegPreIndex{regT=X30, regN=X_MLStackPtr, byteOffset= ~8}] else []) @
                        conditionalBranch(CondCarrySet, skipCheck) ::
                        subSShiftedReg{regM=workReg1, regN=X_MLStackPtr, regD=XZero, shift=ShiftNone} ::
                        loadRegScaled{regT=workReg1, regN=X_MLAssemblyInt, unitOffset=stackLimitOffset}  ::
                        code2
                    end
            end

        |   codeExtended _ (StoreWithConstantOffset{source, base, byteOffset, loadType, ...}, code) =
            if byteOffset < 0
            then
            let
                val storeInstr =
                    case loadType of
                        Load64 => storeRegUnscaled
                    |   Load32 => storeRegUnscaled32
                    |   Load16 => storeRegUnscaled16
                    |   Load8 => storeRegUnscaledByte
            in
                storeInstr{regT=getAllocatedGenReg source, regN=getAllocatedGenReg base, byteOffset=byteOffset} :: code
            end
            else
            let
                val (storeInstr, scale) =
                    case loadType of
                        Load64 => (storeRegScaled, 8)
                    |   Load32 => (storeRegScaled32, 4)
                    |   Load16 => (storeRegScaled16, 2)
                    |   Load8 => (storeRegScaledByte, 1)
            in
                storeInstr{regT=getAllocatedGenReg source, regN=getAllocatedGenReg base, unitOffset=byteOffset div scale} :: code
            end

        |   codeExtended _ (StoreWithIndexedOffset{source, base, index, loadType, ...}, code) =
            let
                val (storeInstr, opt)  =
                    case loadType of
                        Load64 => (storeRegIndexed, ExtUXTX ScaleOrShift)
                    |   Load32 => (storeRegIndexed32, ExtUXTX ScaleOrShift)
                    |   Load16 => (storeRegIndexed16, ExtUXTX ScaleOrShift)
                    |   Load8 => (storeRegIndexedByte, ExtUXTX NoScale)
            in
                storeInstr{regT=getAllocatedGenReg source, regN=getAllocatedGenReg base, regM=getAllocatedGenReg index, option=opt} :: code
            end

        |   codeExtended _ (AddSubImmediate{ source, dest, immed, isAdd, length, ccRef}, code) =
            let
                val instr =
                    case (isAdd, ccRef, length) of
                        (true, NONE, OpSize64) => addImmediate
                    |   (true, SOME _, OpSize64) => addSImmediate
                    |   (false, NONE, OpSize64) => subImmediate
                    |   (false, SOME _, OpSize64) => subSImmediate
                    |   (true, NONE, OpSize32) => addImmediate32
                    |   (true, SOME _, OpSize32) => addSImmediate32
                    |   (false, NONE, OpSize32) => subImmediate32
                    |   (false, SOME _, OpSize32) => subSImmediate32
                val destReg =
                    case dest of
                        NONE => XZero
                    |   SOME dreg => getAllocatedGenReg dreg
            in
                instr{regN=getAllocatedGenReg source, regD=destReg, immed=immed, shifted=false} :: code
            end

        |   codeExtended _ (AddSubRegister{ base, shifted, dest, isAdd, length, ccRef, shift}, code) =
            let
                val instr =
                    case (isAdd, ccRef, length) of
                        (true, NONE, OpSize64) => addShiftedReg
                    |   (true, SOME _, OpSize64) => addSShiftedReg
                    |   (false, NONE, OpSize64) => subShiftedReg
                    |   (false, SOME _, OpSize64) => subSShiftedReg
                    |   (true, NONE, OpSize32) => addShiftedReg32
                    |   (true, SOME _, OpSize32) => addSShiftedReg32
                    |   (false, NONE, OpSize32) => subShiftedReg32
                    |   (false, SOME _, OpSize32) => subSShiftedReg32
                val destReg =
                    case dest of
                        NONE => XZero
                    |   SOME dreg => getAllocatedGenReg dreg
            in
                instr{regN=getAllocatedGenReg base, regM=getAllocatedGenReg shifted, regD=destReg, shift=shift} :: code
            end           

        |   codeExtended _ (LogicalImmediate{ source, dest, immed, logOp, length, ccRef}, code) =
            let
                val instr =
                    case (logOp, ccRef, length) of
                        (LogAnd, NONE, OpSize64) => bitwiseAndImmediate
                    |   (LogAnd, SOME _, OpSize64) => bitwiseAndSImmediate
                    |   (LogOr, NONE, OpSize64) => bitwiseOrImmediate
                    |   (LogXor, NONE, OpSize64) => bitwiseXorImmediate

                    |   (LogAnd, NONE, OpSize32) => bitwiseAndImmediate32
                    |   (LogAnd, SOME _, OpSize32) => bitwiseAndSImmediate32
                    |   (LogOr, NONE, OpSize32) => bitwiseOrImmediate32
                    |   (LogXor, NONE, OpSize32) => bitwiseXorImmediate32

                    |   _ => raise InternalError "ccRef not valid with OR or XOR"

                val destReg =
                    case dest of
                        NONE => XZero
                    |   SOME dreg => getAllocatedGenReg dreg
            in
                instr{regN=getAllocatedGenReg source, regD=destReg, bits=immed} :: code
            end

        |   codeExtended _ (LogicalRegister{ base, shifted, dest, logOp, length, ccRef, shift}, code) =
            let
                val instr =
                    case (logOp, ccRef, length) of
                        (LogAnd, NONE, OpSize64) => andShiftedReg
                    |   (LogAnd, SOME _, OpSize64) => andsShiftedReg
                    |   (LogOr, NONE, OpSize64) => orrShiftedReg
                    |   (LogXor, NONE, OpSize64) => eorShiftedReg

                    |   (LogAnd, NONE, OpSize32) => andShiftedReg32
                    |   (LogAnd, SOME _, OpSize32) => andsShiftedReg32
                    |   (LogOr, NONE, OpSize32) => orrShiftedReg32
                    |   (LogXor, NONE, OpSize32) => eorShiftedReg32

                    |   _ => raise InternalError "ccRef not valid with OR or XOR"
                
                (* There are also versions of AND/OR/XOR which operate on a complement (NOT)
                   of the shifted register.  It's probably not worth looking for a use for them. *)
                val destReg =
                    case dest of
                        NONE => XZero
                    |   SOME dreg => getAllocatedGenReg dreg
            in
                instr{regN=getAllocatedGenReg base, regM=getAllocatedGenReg shifted, regD=destReg, shift=shift} :: code
            end

        |   codeExtended _ (ShiftRegister{ direction, dest, source, shift, opSize }, code) =
            let
                val instr =
                    case (direction, opSize) of
                        (ShiftLeft, OpSize64) => logicalShiftLeftVariable
                    |   (ShiftLeft, OpSize32) => logicalShiftLeftVariable32
                    |   (ShiftRightLogical, OpSize64) => logicalShiftRightVariable
                    |   (ShiftRightLogical, OpSize32) => logicalShiftRightVariable32
                    |   (ShiftRightArithmetic, OpSize64) => arithmeticShiftRightVariable
                    |   (ShiftRightArithmetic, OpSize32) => arithmeticShiftRightVariable32
            in
                instr{regN=getAllocatedGenReg source, regM=getAllocatedGenReg shift, regD=getAllocatedGenReg dest} :: code
            end

        |   codeExtended _ (BeginFunction{regArgs, ...}, code) =
            let
                (* The real registers used for arguments. X30 is there but saved separately. *)
                val saveRegs = List.filter (fn r => r <> X30) (List.map #2 regArgs)
                val skipCheck = createLabel()
                val defaultWords = 10 (* This is wired into the RTS. *)
                val workRegister = workReg1
                val debugTrapAlways = true (*false*) (* Can be set to true for debugging *)

                (* Test with either the stack-pointer or a high-water value.
                   The RTS assumes that X9 has been used as the high-water if it is
                   called through stackOverflowXCallOffset rather than stackOverflowCallOffset *)
                val (testReg, entryPt, code1) =
                    if stackRequired <= defaultWords
                    then (X_MLStackPtr, stackOverflowCallOffset, code)
                    else (X9, stackOverflowXCallOffset,
                            addConstantWord({regS=X_MLStackPtr, regD=X9, regW=workRegister,
                            value= ~ (Word64.fromLarge(Word.toLarge nativeWordSize)) * Word64.fromInt stackRequired}, code))

                (* Skip the RTS call if there is enough stack.  N.B. The RTS can modify the end-of-stack value
                   to force a trap here even if there is really enough stack. *)
                val code2 =
                    (if debugTrapAlways then []
                     else
                     [conditionalBranch(CondCarrySet, skipCheck),
                      subSShiftedReg{regM=workRegister, regN=testReg, regD=XZero, shift=ShiftNone}]) @
                    (* Load the end-of-stack value. *)
                    loadRegScaled{regT=workRegister, regN=X_MLAssemblyInt, unitOffset=stackLimitOffset} :: code1

                val code3 =
                    (* Call the RTS but save X30 across the call *)
                    setLabel skipCheck ::
                    loadRegPostIndex{regT=X30, regN=X_MLStackPtr, byteOffset= 8} ::
                    registerMask saveRegs ::
                    branchAndLinkReg X16 ::
                    loadRegScaled{regT=X16, regN=X_MLAssemblyInt, unitOffset=entryPt} ::
                    storeRegPreIndex{regT=X30, regN=X_MLStackPtr, byteOffset= ~8} :: code2

                val usedRegs = regArgs
                fun mkPair(pr, rr) = {src=rr,dst=getAllocatedGenReg pr}
                val regPairs = List.map mkPair usedRegs
            in
                moveMultipleRegisters(regPairs, code3)
            end

        |   codeExtended _ (TailRecursiveCall{callKind, regArgs, stackArgs, stackAdjust, currStackSize}, code: instr list) =
            let
                fun convertArg(ArgOnStack{wordOffset, ...}) = IsOnStack wordOffset
                |   convertArg(ArgInReg reg) = IsInReg(getAllocatedGenReg reg)

                val extStackArgs = map (fn {stack, src} => {dst=IsOnStack(stack+currStackSize), src=convertArg src}) stackArgs
                val extRegArgs = map (fn (a, r) => {src=convertArg a, dst=IsInReg r}) regArgs

                (* Tail recursive calls are complicated because we generally have to overwrite the existing stack.
                   That means storing the arguments in the right order to avoid overwriting a
                   value that we are using for a different argument. *)
                fun codeTailCall(arguments, stackAdjust, code) =
                    if stackAdjust < 0
                    then
                    let
                        (* If the function we're calling takes more arguments on the stack than the
                           current function we will have to extend the stack.  Do that by pushing the
                           argument whose offset is at -1.  Then adjust all the offsets and repeat. *)
                        val {src=argM1, ...} = valOf(List.find(fn {dst=IsOnStack ~1, ...} => true | _ => false) arguments)
                        fun renumberArgs [] = []
                        |   renumberArgs ({dst=IsOnStack ~1, ...} :: args) = renumberArgs args (* Remove the one we've done. *)
                        |   renumberArgs ({dst, src} :: args) =
                            let
                                val newDest = case dst of IsOnStack d => IsOnStack(d+1) | regDest => regDest
                                val newSrc =
                                    case src of
                                        IsOnStack wordOffset => IsOnStack(wordOffset+1)
                                    |   other => other
                            in
                                {dst=newDest, src=newSrc} :: renumberArgs args
                            end
                        val pushCode =
                            case argM1 of
                                IsOnStack wordOffset =>
                                    storeRegPreIndex{regT=workReg2, regN=X_MLStackPtr, byteOffset= ~8} ::
                                        loadFromStack(workReg2, wordOffset, code)
                            |   IsInReg reg => storeRegPreIndex{regT=reg, regN=X_MLStackPtr, byteOffset= ~8} :: code
                                
                    in
                        codeTailCall(renumberArgs arguments, stackAdjust+1, pushCode)
                    end
                    else
                    let
                        val loadArgs = moveMultipleValues(arguments, code)
                    in
                        if stackAdjust = 0
                        then loadArgs
                        else addConstantWord({regS=X_MLStackPtr, regD=X_MLStackPtr, regW=workReg1,
                                value=Word64.fromInt stackAdjust * Word.toLarge nativeWordSize}, loadArgs)
                    end
                val setArgumentsCode = codeTailCall(extStackArgs @ extRegArgs, stackAdjust+currStackSize, code)
                
                val jumpToFunctionCode =
                    case callKind of
                        Recursive => [unconditionalBranch startOfFunctionLabel]
                    |   ConstantCode m =>
                            if is32in64
                            then raise InternalError "ConstantCode" (* Not currently *)
                            else [branchRegister workReg1, loadAddressConstant(workReg1, m)]
                    |   FullCall =>
                            if is32in64
                            then [branchRegister workReg1,
                                 loadRegScaled{regT=workReg1, regN=workReg1, unitOffset=0},
                                 addShiftedReg{regM=X8, regN=X_Base32in64, regD=workReg1, shift=ShiftLSL 0w2}]
                            else [branchRegister workReg1,loadRegScaled{regT=workReg1, regN=X8, unitOffset=0}]
            in
                jumpToFunctionCode @ setArgumentsCode
            end

        |   codeExtended _ (FunctionCall{callKind, regArgs=regArgs, stackArgs=stackArgs, dest, saveRegs, ...}, code: instr list) =
            let
                val destReg = getAllocatedGenReg dest

                local
                    fun pushStackArgs ([], _, code) = code

                    |   pushStackArgs (ArgOnStack {wordOffset, ...} ::args, argNum, code) =
                        let
                            (* Have to adjust the offsets of stack arguments. *)
                            val adjustedOffset = wordOffset+argNum
                        in
                            pushStackArgs(args, argNum+1,
                                storeRegPreIndex{regT=workReg1, regN=X_MLStackPtr, byteOffset= ~8} ::
                                    loadFromStack(workReg1, adjustedOffset, code))
                        end

                    |   pushStackArgs (ArgInReg reg ::args, argNum, code) =
                            pushStackArgs(args, argNum+1,
                                storeRegPreIndex{regT=getAllocatedGenReg reg, regN=X_MLStackPtr, byteOffset= ~8} :: code)

                    val pushedArgs = pushStackArgs(stackArgs, 0, code (* Initial code *))

                    (* We have to adjust any stack offset to account for the arguments we've pushed. *)
                    val numStackArgs = List.length stackArgs

                    fun convertArg(ArgOnStack{wordOffset, ...}) = IsOnStack(wordOffset+numStackArgs)
                    |   convertArg(ArgInReg reg) = IsInReg(getAllocatedGenReg reg)

                in
                    val extRegArgs = map (fn (a, r) => {src=convertArg a, dst=IsInReg r}) regArgs
                    val loadArgs = moveMultipleValues(extRegArgs, pushedArgs)
                end

                (* Push the registers before the call and pop them afterwards. *)
                fun makeSavesAndCall([], code) =
                    let
                        val callFunctionCode =
                            case callKind of
                                Recursive => [branchAndLink startOfFunctionLabel]
                            |   ConstantCode m =>
                                    if is32in64
                                    then raise InternalError "ConstantCode" (* Not currently *)
                                    else [branchAndLinkReg workReg1, loadAddressConstant(workReg1, m)]
                            |   FullCall =>
                                    if is32in64
                                    then [branchAndLinkReg workReg1,
                                         loadRegScaled{regT=workReg1, regN=workReg1, unitOffset=0},
                                         addShiftedReg{regM=X8, regN=X_Base32in64, regD=workReg1, shift=ShiftLSL 0w2}]
                                    else [branchAndLinkReg workReg1,loadRegScaled{regT=workReg1, regN=X8, unitOffset=0}]
                    in
                        callFunctionCode @ code
                    end

                |   makeSavesAndCall(reg::regs, code) =
                    let
                        val areg = getAllocatedGenReg reg
                    in
                        loadRegPostIndex{regT=areg, regN=X_MLStackPtr, byteOffset= 8} ::
                            makeSavesAndCall(regs, storeRegPreIndex{regT=areg, regN=X_MLStackPtr, byteOffset= ~8} :: code)
                    end

            in
                moveIfNecessary({dst=destReg, src=X0}, makeSavesAndCall(saveRegs, loadArgs))
            end

        |   codeExtended _ (ReturnResultFromFunction { resultReg, returnReg, numStackArgs }, code) =
            let
                val resultReg = getAllocatedGenReg resultReg
                and returnReg = getAllocatedGenReg returnReg

                fun resetStack(0, code) = code
                |   resetStack(nItems, code) =
                        addConstantWord({regS=X_MLStackPtr, regD=X_MLStackPtr, regW=X3,
                                        value=Word64.fromLarge(Word.toLarge nativeWordSize) * Word64.fromInt nItems}, code)
            in
                returnRegister returnReg :: resetStack(numStackArgs, moveIfNecessary({src=resultReg, dst=X0}, code))
            end

        |   codeExtended _ (RaiseExceptionPacket{ packetReg }, code) =
                (* We need a work register here.  It can be any register other than X0 since
                   we don't preserve registers across calls. *)
                (* Copy the handler "register" into the stack pointer.  Then
                   jump to the address in the first word.  The second word is
                   the next handler.  This is set up in the handler.  We have a lot
                   more raises than handlers since most raises are exceptional conditions
                   such as overflow so it makes sense to minimise the code in each raise. *)
                branchRegister workReg1 :: 
                loadRegScaled{regT=workReg1, regN=X_MLStackPtr, unitOffset=0} ::
                loadRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset} ::
                moveIfNecessary({src=getAllocatedGenReg packetReg, dst=X0}, code)

        |   codeExtended _ (PushToStack{ source, copies, ... }, code) =
            let
                val reg = getAllocatedGenReg source
                val _ = copies > 0 orelse raise InternalError "PushToStack: copies<1"
                fun pushn(0, c) = c
                |   pushn(n, c) = pushn(n-1, storeRegPreIndex{regT=reg, regN=X_MLStackPtr, byteOffset= ~8} :: c)
            in
                pushn(copies, code)
            end

        |   codeExtended _ (LoadStack{ dest, wordOffset, ... }, code) =
                loadFromStack(getAllocatedGenReg dest, wordOffset, code)

        |   codeExtended _ (StoreToStack{ source, stackOffset, ... }, code) =
                (* Store into the stack to set a field of a container.  Always 64-bits. *)
                storeToStack(getAllocatedGenReg source, stackOffset, workReg1, code)

        |   codeExtended _ (ContainerAddress{ dest, stackOffset, ... }, code) =
                (* Set the register to an offset in the stack. *)
            let
                val destReg = getAllocatedGenReg dest
                val _ = stackOffset >= 0 orelse raise InternalError "codeGenICode: ContainerAddress - negative offset"
                val byteOffset = stackOffset * Word.toInt nativeWordSize
            in
                if byteOffset >= 4096
                then addShiftedReg{regN=X_MLStackPtr, regM=destReg, regD=destReg, shift=ShiftNone} ::
                        List.rev(loadNonAddress(destReg, Word64.fromInt byteOffset)) @ code
                else addImmediate{regN=X_MLStackPtr, regD=destReg, immed=Word.fromInt byteOffset, shifted=false} :: code
            end

        |   codeExtended _ (ResetStackPtr{ numWords, ... }, code) =
                addConstantWord({regS=X_MLStackPtr, regD=X_MLStackPtr, regW=workReg1,
                    value=Word64.fromInt numWords * Word.toLarge nativeWordSize}, code)

        |   codeExtended _ (TagValue{ source, dest, isSigned=_, opSize }, code) =
            let
                val sourceReg = getAllocatedGenReg source
                and destReg = getAllocatedGenReg dest
                (* Shift left by one bit and add one. *)
            in
                case opSize of
                    OpSize64 =>
                        bitwiseOrImmediate{regN=destReg, regD=destReg, bits=0w1} ::
                        logicalShiftLeft{regN=sourceReg, regD=destReg, shift=0w1} :: code
                |   OpSize32 =>
                        bitwiseOrImmediate32{regN=destReg, regD=destReg, bits=0w1} ::
                        logicalShiftLeft32{regN=sourceReg, regD=destReg, shift=0w1} :: code
            end

        |   codeExtended _ (UntagValue{ source, dest, isSigned, opSize }, code) =
            let
                (* Shift right by one bit.  The type of shift depends on the length and whether it's signed. *)
                val shiftType =
                    case (isSigned, opSize) of
                        (false, OpSize64) => logicalShiftRight
                    |   (false, OpSize32) => logicalShiftRight32
                    |   (true, OpSize64) => arithmeticShiftRight
                    |   (true, OpSize32) => arithmeticShiftRight32
            in
                shiftType{regN=getAllocatedGenReg source, regD=getAllocatedGenReg dest, shift=0w1} :: code
            end

        |   codeExtended _ (BoxLarge{ source, dest, saveRegs }, code) =
                List.rev(boxSysWord{source=getAllocatedGenReg source, destination=getAllocatedGenReg dest,
                                    workReg=workReg1, saveRegs=getSaveRegs saveRegs}) @ code

        |   codeExtended _ (UnboxLarge{ source, dest }, code) =
            let
                (* Unbox a large word.  The argument is a poly word. *)
                val destReg = getAllocatedGenReg dest
                and srcReg = getAllocatedGenReg source
            in
                if is32in64
                then loadRegScaled{regT=destReg, regN=destReg, unitOffset=0} ::
                        addShiftedReg{regM=srcReg, regN=X_Base32in64, regD=destReg, shift=ShiftLSL 0w2} :: code
                else loadRegScaled{regT=destReg, regN=srcReg, unitOffset=0} :: code
            end

        |   codeExtended _ (BoxTagFloat{ floatSize=Double64, source, dest, saveRegs }, code) =
                List.rev(boxDouble{source=getAllocatedFPReg source, destination=getAllocatedGenReg dest,
                                    workReg=workReg1, saveRegs=getSaveRegs saveRegs}) @ code

        |   codeExtended _ (BoxTagFloat{ floatSize=Float32, source, dest, saveRegs }, code) =
                if is32in64
                then List.rev(boxFloat{source=getAllocatedFPReg source, destination=getAllocatedGenReg dest,
                                    workReg=workReg1, saveRegs=getSaveRegs saveRegs}) @ code
                else raise InternalError "BoxTagFloat: tagged float"
 
        |   codeExtended _ (UnboxTagFloat _, _) = raise InternalError "codeExtended; Unbox"

        |   codeExtended _ (LoadAcquire{dest, base, loadType, ...}, code) =
            let
                val loadInstr  =
                    case loadType of
                        Load64 => loadAcquire
                    |   Load32 => loadAcquire32
                    |   Load16 => raise InternalError "LoadAcquire: 16bits" (* Not used *)
                    |   Load8 => loadAcquireByte
            in
                loadInstr{regT=getAllocatedGenReg dest, regN=getAllocatedGenReg base} :: code
            end

        |   codeExtended _ (StoreRelease{source, base, loadType, ...}, code) =
            let
                val storeInstr  =
                    case loadType of
                        Load64 => storeRelease
                    |   Load32 => storeRelease32
                    |   Load16 => raise InternalError "StoreRelease: 16bits" (* Not used *)
                    |   Load8 => storeReleaseByte
            in
                storeInstr{regT=getAllocatedGenReg source, regN=getAllocatedGenReg base} :: code
            end

        |   codeExtended _ (BitFieldShift{ source, dest, isSigned, length, immr, imms }, code) =
            let
                val bfInstr =
                    case (isSigned, length) of
                        (true, OpSize64) => signedBitfieldMove64
                    |   (false, OpSize64) => unsignedBitfieldMove64
                    |   (true, OpSize32) => signedBitfieldMove32
                    |   (false, OpSize32) => unsignedBitfieldMove32
                val srcReg = getAllocatedGenReg source
                val destReg = getAllocatedGenReg dest
            in
                bfInstr{immr=immr, imms=imms, regN=srcReg, regD=destReg} :: code
            end

        |   codeExtended _ (BitFieldInsert{ source, destAsSource, dest, length, immr, imms }, code) =
            let
                (* If we're using BitFieldMove we retain some of the bits of the
                   destination.  The higher levels require us to treat that as a source.  *)
                val sourceReg = getAllocatedGenReg source
                and destReg = getAllocatedGenReg dest
                val _ = sourceReg = destReg andalso raise InternalError "codeExtended: bitfield: dest=source"
                val bfInstr =
                    case length of OpSize64 => bitfieldMove64 | OpSize32 => bitfieldMove32
            in
                bfInstr{immr=immr, imms=imms, regN=getAllocatedGenReg source, regD=destReg} ::
                    moveIfNecessary({src=getAllocatedGenReg destAsSource, dst=destReg}, code)
            end

        |   codeExtended {flow} (IndexedCaseOperation{testReg}, code) =
            let
                (* testReg contains the original value after the lowest value has been subtracted.
                   Since both the original value and the lowest value were tagged it contains a
                   shifted but untagged value. *)
                (* This should only be within a block with an IndexedBr flow type. *)
                val cases =
                    case flow of IndexedBr cases => cases | _ => raise InternalError "codeGenICode: IndexedCaseOperation"
                val caseLabels = map getBlockLabel cases
                val tableLabel = createLabel()
                val startOfCase =
                    setLabel tableLabel ::
                    branchRegister workReg1 ::
                    (* Add the value shifted by one since it's already shifted. *)
                    addShiftedReg{regN=workReg1, regD=workReg1, regM=getAllocatedGenReg testReg, shift=ShiftLSL 0w1} ::
                    loadLabelAddress(workReg1, tableLabel) :: code
                val addCases =
                    List.foldl (fn (label, code) => unconditionalBranch label :: code) startOfCase caseLabels
            in
                addCases
            end

        |   codeExtended {flow} (PushExceptionHandler, code) =
            let
                (* This should only be within a block with a SetHandler flow type. *)
                val handleLabel =
                    case flow of
                        SetHandler{ handler, ...} => handler
                    |   _ => raise InternalError "codeGenICode: PushExceptionHandler"
                val labelRef = getBlockLabel handleLabel
            in
                (* Push the old handler and the handler entry point and set the "current handler"
                   to point to the stack after we've pushed these. *)
                storeRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset} ::
                storeRegPreIndex{regT=workReg2, regN=X_MLStackPtr, byteOffset= ~8} ::
                storeRegPreIndex{regT=workReg1, regN=X_MLStackPtr, byteOffset= ~8} ::
                loadLabelAddress(workReg2, labelRef) ::
                loadRegScaled{regT=workReg1, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset} :: code
            end

        |   codeExtended _ (PopExceptionHandler, code) =
                (* Remove and discard the handler we've set up.  Pop the previous handler
                   and put into "current handler". *)
                storeRegScaled{regT=workReg2, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset} ::
                loadRegPostIndex{regT=workReg2, regN=X_MLStackPtr, byteOffset=8} ::
                loadRegPostIndex{regT=workReg1, regN=X_MLStackPtr, byteOffset=8} :: code

        |   codeExtended _ (BeginHandler{packetReg}, code) =
            let
                val beginHandleCode =
                    (* Remove the handler entry for this handler. *)
                    storeRegScaled{regT=workReg2, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset} ::
                    loadRegPostIndex{regT=workReg2, regN=X_MLStackPtr, byteOffset=8} ::
                    loadRegPostIndex{regT=workReg1, regN=X_MLStackPtr, byteOffset=8} ::
                    (* The exception raise code resets the stack pointer to the value in the exception handler
                           so this is probably redundant.  Leave it for the moment, *)
                    loadRegScaled{regT=X_MLStackPtr, regN=X_MLAssemblyInt, unitOffset=exceptionHandlerOffset} :: code
            in
                moveIfNecessary({src=X0, dst=getAllocatedGenReg packetReg }, beginHandleCode)
            end

        local
            (* processed - set to true when a block has been processed. *)
            val processed = Array.array(numBlocks, false)
            fun haveProcessed n = Array.sub(processed, n)
            
            (* Find the blocks that reference this one.  This isn't essential but
               allows us to try to generate blocks in the order of the control
               flow.  This in turn may allow us to use short branches rather
               than long ones. *)
            val labelRefs = Array.array(numBlocks, [])

            datatype flowCode =
                FlowCodeSimple of int
            |   FlowCodeCMove of {code: instr list, trueJump: int, falseJump: int}
            
            (* Process this recursively to set the references.  If we have
               unreachable blocks, perhaps because they've been merged, we
               don't want to include them in the reference counting.
               This shouldn't happen now that IdentifyReferences removes
               unreferenced blocks. *)
            fun setReferences fromLabel toLabel =
                case Array.sub(labelRefs, toLabel) of
                    [] => (* Not yet visited at all. *)
                    let
                        val ExtendedBasicBlock{ flow, ...} = Vector.sub(blocks, toLabel)
                        val refs =
                            case flow of
                                ExitCode => []
                            |   Unconditional lab => [lab]
                            |   Conditional{trueJump, falseJump, ... } => [trueJump, falseJump]
                            |   IndexedBr labs => labs
                            |   SetHandler { handler, continue } => [handler, continue]
                            |   UnconditionalHandle _ => []
                            |   ConditionalHandle { continue, ...} => [continue]

                        val () =
                            if fromLabel >= 0 then Array.update(labelRefs, toLabel, [fromLabel]) else ()
                    in
                        List.app (setReferences toLabel) refs
                    end
                    
                |   refs =>
                    (* We've visiting this at least once.  Just add us to the list. *)
                        Array.update(labelRefs, toLabel, fromLabel :: refs)
            
            val _ = setReferences 0 0
            
            (* Process the blocks.  We keep the "stack" explicit rather than using recursion
               because this allows us to select both arms of a conditional branch sooner. *)
            fun genCode(toDo, lastFlow, code) =
            case List.filter (not o haveProcessed) toDo of
                [] =>
                let
                    (* There's nothing left to do. We may need to add a final branch to the end. *)
                    val finalBranch =
                        case lastFlow of
                            ExitCode => []
                        |   IndexedBr _ => []
                        |   Unconditional dest => [unconditionalBranch(getBlockLabel dest)]
                        |   Conditional { condition, trueJump, falseJump, ...} =>
                                [
                                    unconditionalBranch(getBlockLabel falseJump),
                                    conditionalBranch(condition, getBlockLabel trueJump)
                                ]
                        |   SetHandler { continue, ...} => [unconditionalBranch(getBlockLabel continue)]
                        |   UnconditionalHandle _ => []
                        |   ConditionalHandle { continue, ...} => [unconditionalBranch(getBlockLabel continue)]
                in
                    finalBranch @ code (* Done. *)
                end

            |   stillToDo as head :: _ =>
                let
                    local
                        (* Check the references.  If all the sources that lead up to this have
                           already been we won't have any backward jumps. *)
                        fun available dest = List.all haveProcessed (Array.sub(labelRefs, dest))

                        val continuation =
                            case lastFlow of
                                ExitCode => NONE
                            |   IndexedBr _ => NONE (* We could put the last branch in here. *)
                            |   Unconditional dest =>
                                    if not (haveProcessed dest) andalso available dest
                                    then SOME(FlowCodeSimple dest)
                                    else NONE
                            |   Conditional {trueJump, falseJump, condition, ...} =>
                                (* We can usually choose either destination and in nearly all cases
                                   it won't matter.  The default branch is not to take forward jumps
                                   so if there is reason to believe that one branch is more likely
                                   we should follow that branch now and leave the other.  If we
                                   have Cond(No)Overflow we assume that overflow is unusual.
                                   If one branch raises an exception we assume that that is unusual. *)
                                let
                                    val (first, second) =
                                        case (condition, Vector.sub(blocks, falseJump)) of
                                            (CondNoOverflow, _) => (trueJump, falseJump)
                                        |   (_, ExtendedBasicBlock{ flow=ExitCode, block, ...}) =>
                                                if List.exists(fn{instr=RaiseExceptionPacket _, ...} => true | _ => false) block
                                                then (trueJump, falseJump)
                                                else (falseJump, trueJump)
                                        |   _ => (falseJump, trueJump)
                                in
                                    if not (haveProcessed first) andalso available first
                                    then SOME(FlowCodeSimple first)
                                    else if not (haveProcessed second) andalso available second
                                    then SOME(FlowCodeSimple second)
                                    else NONE
                                end
                           |    SetHandler { continue, ... } =>
                                    (* We want the continuation if possible.  We'll need a
                                       branch round the handler so that won't help. *)
                                    if not (haveProcessed continue) andalso available continue
                                    then SOME(FlowCodeSimple continue)
                                    else NONE
                           |    UnconditionalHandle _ => NONE
                           |    ConditionalHandle _ => NONE
                    in
                        (* First choice - continue the existing block.
                           Second choice - the first item whose sources have all been
                           processed.
                           Third choice - something from the list. *)
                        val picked =
                            case continuation of
                                SOME c => c
                            |   NONE =>
                                    case List.find available stillToDo of
                                        SOME c => FlowCodeSimple c
                                    |   NONE => FlowCodeSimple head
                    end
                    
                in
                    case picked of
                        FlowCodeSimple picked =>
                        let
                            val () = Array.update(processed, picked, true)

                            (* Code to terminate the previous block. *)
                            val startCode =
                                case lastFlow of
                                    ExitCode => []
                                |   IndexedBr _ => []
                                |   UnconditionalHandle _ => []
                                |   Unconditional dest =>
                                        if dest = picked then [] else [unconditionalBranch(getBlockLabel dest)]
                                |   ConditionalHandle { continue, ...} =>
                                        if continue = picked then [] else [unconditionalBranch(getBlockLabel continue)]
                                |   SetHandler { continue, ... } =>
                                        if continue = picked then [] else [unconditionalBranch(getBlockLabel continue)]
                                |   Conditional { condition, trueJump, falseJump, ...} =>
                                    if picked = falseJump (* Usual case. *)
                                    then [conditionalBranch(condition, getBlockLabel trueJump)]
                                    else if picked = trueJump
                                    then (* We have a jump to the true condition. Invert the jump.
                                            This is more than an optimisation.  Because this immediately precedes the
                                            true block we're not going to generate a label. *)
                                        [conditionalBranch(invertTest condition, getBlockLabel falseJump)]
                                    else
                                    [
                                        unconditionalBranch(getBlockLabel falseJump),
                                        conditionalBranch(condition, getBlockLabel trueJump)
                                    ]

                            (* Code-generate the body with the code we've done so far
                               at the end.  Add a label at the start if necessary. *)
                            local
                                (* If the previous block dropped through to this and this was
                                   the only reference then we don't need a label. *)
                                fun onlyJumpingHere (lab: int) =
                                    if lab <> picked then false
                                    else case Array.sub(labelRefs, picked) of
                                        [singleton] => singleton = lab
                                    |   _ => false
                    
                                val noLabel =
                                    case lastFlow of
                                        ExitCode => picked = 0 (* Unless this was the first block. *)
                                    |   Unconditional dest => onlyJumpingHere dest
                                    |   Conditional { trueJump, falseJump, ...} =>
                                            onlyJumpingHere trueJump orelse onlyJumpingHere falseJump
                                    |   IndexedBr _ => false
                                    |   SetHandler _ => false
                                    |   UnconditionalHandle _ => false
                                    |   ConditionalHandle { continue, ...} => onlyJumpingHere continue
                            in
                                val startLabel = if noLabel then [] else [setLabel(getBlockLabel picked)]
                            end

                            val ExtendedBasicBlock { flow, block, ...} = Vector.sub(blocks, picked)

                            local
                                fun genCodeBlock({instr, ...}, code) = codeExtended {flow=flow} (instr, code)
                            in
                                val bodyCode = List.foldl genCodeBlock (startLabel @ startCode @ code) block
                            end

                            val addSet =
                                case flow of
                                    ExitCode => []
                                |   IndexedBr cases => cases
                                |   Unconditional dest => [dest]
                                |   Conditional {trueJump, falseJump, ...} => [falseJump, trueJump]
                                |   SetHandler { handler, continue } => [handler, continue]
                                |   UnconditionalHandle _ => []
                                |   ConditionalHandle { continue, ...} => [continue]

                        in
                            genCode(addSet @ stillToDo, flow, bodyCode)
                        end
                
                    |   FlowCodeCMove{code, trueJump, falseJump} =>
                        let
                            (* We've generated a conditional move and possibly a return.  If the
                               trueJump and falseJump are only ever referenced from this block
                               they're done, otherwise we still need to do them. *)
                            val _ =
                                case Array.sub(labelRefs, trueJump) of
                                    [_] => Array.update(processed, trueJump, true)
                                |   _ => ()
                            val _ =
                                case Array.sub(labelRefs, falseJump) of
                                    [_] => Array.update(processed, falseJump, true)
                                |   _ => ()
                            val ExtendedBasicBlock { flow, ...} = Vector.sub(blocks, trueJump)
                            val addSet =
                                case flow of
                                    ExitCode => []
                                |   Unconditional dest => [dest]
                                |   _ => raise InternalError "FlowCodeCMove"
                        in
                            genCode(addSet @ stillToDo, flow, code)
                        end
                end
        in
            val ops = genCode([0], ExitCode, [setLabel startOfFunctionLabel])
        end
    in
        Arm64Assembly.generateCode{instrs=List.rev ops, name=functionName, resultClosure=resultClosure,
                                   parameters=debugSwitches, profileObject=profileObject}
    end

    structure Sharing =
    struct
        type intSet             = intSet
        and extendedBasicBlock  = extendedBasicBlock
        and regProperty         = regProperty
        and reg                 = reg
        and closureRef          = closureRef
    end

end;
