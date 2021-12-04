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

    fun icodeToArm64Code {blocks, functionName, stackRequired, debugSwitches, allocatedRegisters, resultClosure, profileObject, ...} =
    let
        val numBlocks = Vector.length blocks
        fun getAllocatedReg r = Vector.sub(allocatedRegisters, r)

        local
            val numRegisters = Vector.length allocatedRegisters
            val uses = Array.array(numRegisters, false)
            fun used(PReg r) = Array.update(uses, r, true)
            fun isUsed(PReg r) = Array.sub(uses, r)

            (* LoadArgument, TagValue, CopyToCache, UntagValue and BoxValue are eliminated if their destination
               is not used.  In that case their source are not used either. *)
            fun instructionUses(MoveRegister { source, dest, ...}) = if isUsed dest then used source else ()
            |   instructionUses(LoadNonAddressConstant _) = ()
            |   instructionUses(LoadAddressConstant _) = ()
            |   instructionUses(LoadWithConstantOffset{dest, base, ...}) = if isUsed dest then used base else ()
            |   instructionUses(LoadWithIndexedOffset{dest, base, index, ...}) = if isUsed dest then (used base; used index) else ()
            |   instructionUses(ObjectIndexAddressToAbsolute{dest, source, ...}) = if isUsed dest then used source else ()
            |   instructionUses(AbsoluteToObjectIndex{dest, source, ...}) = if isUsed dest then used source else ()
            |   instructionUses(AllocateMemoryOperation _) = ()
            |   instructionUses(StoreWithConstantOffset{source, base, ...}) = (used source; used base)
            |   instructionUses(StoreWithIndexedOffset{source, base, index, ...}) = (used source; used base; used index)
            |   instructionUses(AddSubImmediate{ source, ... }) = used source
            |   instructionUses(AddSubRegister{ operand1, operand2, ... }) = (used operand1; used operand2)
            |   instructionUses(BeginFunction _) = ()
            |   instructionUses(ReturnResultFromFunction{resultReg, returnReg, ...}) = (used resultReg; used returnReg)
            |   instructionUses(RaiseExceptionPacket{packetReg}) = used packetReg
            |   instructionUses(PushToStack{source, ...}) = used source
            |   instructionUses(LoadStack _) = ()
            |   instructionUses(StoreToStack{source, ...}) = used source
            |   instructionUses(ContainerAddress _) = ()
            |   instructionUses(ResetStackPtr _) = ()

            (* Depth-first scan. *)
            val visited = Array.array(numBlocks, false)

            fun processBlocks blockNo =
            if Array.sub(visited, blockNo)
            then ()  (* Done or currently being done. *)
            else
            let
                val () = Array.update(visited, blockNo, true)
                val ExtendedBasicBlock { flow, block,...} = Vector.sub(blocks, blockNo)
                val () =
                    (* Process the dependencies first. *)
                    case flow of
                        ExitCode => ()
                    |   Unconditional m => processBlocks m
                    |   Conditional {trueJump, falseJump, ...} =>
                            (processBlocks trueJump; processBlocks falseJump)
                    |   IndexedBr cases => List.app processBlocks cases
                    |   SetHandler{ handler, continue } =>
                            (processBlocks handler; processBlocks continue)
                    |   UnconditionalHandle _ => ()
                    |   ConditionalHandle { continue, ...} => processBlocks continue
                (* Now this block. *)
            in
                List.foldr(fn ({instr, ...}, ()) => instructionUses instr) () block
            end

        in
            val () = processBlocks 0
            val isUsed = isUsed
        end

        local
            type node = {src: xReg, dst: xReg }
            
            fun nodeAddress({dst=XReg r, ...}: node) = Word8.toInt r
            |   nodeAddress _ = raise InternalError "nodeAddress"

            fun arcs({src=XReg r, ...}: node) = [Word8.toInt r]
            |   arcs _ = raise InternalError "nodeAddress"
        in
            val stronglyConnected = Strongly.stronglyConnectedComponents { nodeAddress=nodeAddress, arcs=arcs }
        end
        
        (* When moving arguments, typically in a function call, we may find the same register
           in use as a source and a destination. *)
        fun moveMultipleRegisters(moves, code) =
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
                    
                    fun moveEachValue ([], code) = code

                    |   moveEachValue ([{dst, src}] :: rest, code) =
                            (* Source and dest are both regs - only move if they're different. *)
                            if dst = src
                            then moveEachValue(rest, code)
                            else moveEachValue(rest, moveRegToReg{sReg=src, dReg=dst} :: code)

                    |   moveEachValue((cycle as first :: _ :: _) :: rest, code) =
                            raise Fallback "moveEachValue: cycle"

                    |   moveEachValue(([]) :: _, _) = (* This should not happen - avoid warning. *)
                            raise InternalError "moveEachValue - empty set"
                in
                    moveEachValue(ordered, code)
                end
        in
            moveValues(moves, code)
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

        (* Only get the registers that are actually used. *)
        val getSaveRegs = List.mapPartial(fn (reg as PReg r) => if isUsed reg then SOME(getAllocatedReg r) else NONE)

        fun codeExtended _ ({instr=MoveRegister{source=PReg sreg, dest as PReg dreg, ...}, ...}, code) =
            if not (isUsed dest)
            then code
            else moveIfNecessary({src=getAllocatedReg sreg, dst=getAllocatedReg dreg}, code)

        |   codeExtended _ ({instr=LoadNonAddressConstant{source, dest=PReg dreg, ...}, ...}, code) =
                List.rev(loadNonAddress(getAllocatedReg dreg, source)) @ code

        |   codeExtended _ ({instr=LoadAddressConstant{source, dest=PReg dreg, ...}, ...}, code) =
                loadAddressConstant(getAllocatedReg dreg, source) :: code

        |   codeExtended _ ({instr=LoadWithConstantOffset{dest=PReg dreg, base=PReg breg, byteOffset, loadType, ...}, ...}, code) =
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
                loadInstr{regT=getAllocatedReg dreg, regN=getAllocatedReg breg, byteOffset=byteOffset} :: code
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
                loadInstr{regT=getAllocatedReg dreg, regN=getAllocatedReg breg, unitOffset=byteOffset div scale} :: code
            end

        |   codeExtended _ ({instr=LoadWithIndexedOffset{dest=PReg dreg, base=PReg breg, index=PReg ireg, loadType, ...}, ...}, code) =
            let
                val (loadInstr, opt)  =
                    case loadType of
                        Load64 => (loadRegIndexed, ExtUXTX ScaleOrShift)
                    |   Load32 => (loadRegIndexed32, ExtUXTX ScaleOrShift)
                    |   Load16 => (loadRegIndexed16, ExtUXTX ScaleOrShift)
                    |   Load8 => (loadRegIndexedByte, ExtUXTX NoScale)
            in
                loadInstr{regT=getAllocatedReg dreg, regN=getAllocatedReg breg, regM=getAllocatedReg ireg, option=opt} :: code
            end

        |   codeExtended _ ({instr=ObjectIndexAddressToAbsolute{source=PReg sreg, dest=PReg dreg, ...}, ...}, code) =
                addShiftedReg{regM=getAllocatedReg sreg, regN=X_Base32in64, regD=getAllocatedReg dreg, shift=ShiftLSL 0w2} :: code

        |   codeExtended _ ({instr=AbsoluteToObjectIndex{source=PReg sreg, dest=PReg dreg, ...}, ...}, code) =
            let
                val destReg = getAllocatedReg dreg
            in
                logicalShiftRight{shift=0w2, regN=destReg, regD=destReg} ::
                subShiftedReg{regM=X_Base32in64, regN=getAllocatedReg sreg, regD=destReg, shift=ShiftNone} :: code
            end

        |   codeExtended _ ({instr=AllocateMemoryOperation{ bytesRequired, dest=PReg dreg, saveRegs, ... }, ...}, code) =
            let
                val realSaveRegs = getSaveRegs saveRegs
                (* We may have X30 in the set but we're going to use it if we trap.
                   We could move it out but it's probably better to save it across the call
                   even though that's two instructions since traps are infrequent. *)
                val saveX30 = List.exists (fn r => r = X30) realSaveRegs
                val preserve = List.filter (fn r => r <> X30) realSaveRegs
                val label = createLabel()
                val destReg = getAllocatedReg dreg
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

        |   codeExtended _ ({instr=StoreWithConstantOffset{source=PReg sreg, base=PReg breg, byteOffset, loadType, ...}, ...}, code) =
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
                storeInstr{regT=getAllocatedReg sreg, regN=getAllocatedReg breg, byteOffset=byteOffset} :: code
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
                storeInstr{regT=getAllocatedReg sreg, regN=getAllocatedReg breg, unitOffset=byteOffset div scale} :: code
            end

        |   codeExtended _ ({instr=StoreWithIndexedOffset{source=PReg sreg, base=PReg breg, index=PReg ireg, loadType, ...}, ...}, code) =
            let
                val (storeInstr, opt)  =
                    case loadType of
                        Load64 => (storeRegIndexed, ExtUXTX ScaleOrShift)
                    |   Load32 => (storeRegIndexed32, ExtUXTX ScaleOrShift)
                    |   Load16 => (storeRegIndexed16, ExtUXTX ScaleOrShift)
                    |   Load8 => (storeRegIndexedByte, ExtUXTX NoScale)
            in
                storeInstr{regT=getAllocatedReg sreg, regN=getAllocatedReg breg, regM=getAllocatedReg ireg, option=opt} :: code
            end

        |   codeExtended _ ({instr=AddSubImmediate{ source=PReg sreg, dest, immed, isAdd, length, ccRef}, ...}, code) =
            let
                val instr =
                    case (isAdd, ccRef, length) of
                        (true, NONE, Arith64) => addImmediate
                    |   (true, SOME _, Arith64) => addSImmediate
                    |   (false, NONE, Arith64) => subImmediate
                    |   (false, SOME _, Arith64) => subSImmediate
                    |   (true, NONE, Arith32) => addImmediate32
                    |   (true, SOME _, Arith32) => addSImmediate32
                    |   (false, NONE, Arith32) => subImmediate32
                    |   (false, SOME _, Arith32) => subSImmediate32
                val destReg =
                    case dest of
                        NONE => XZero
                    |   SOME(PReg dreg) => getAllocatedReg dreg
            in
                instr{regN=getAllocatedReg sreg, regD=destReg, immed=immed, shifted=false} :: code
            end
 
        |   codeExtended _ ({instr=AddSubRegister{ operand1=PReg oreg1, operand2=PReg oreg2, dest, isAdd, length, ccRef}, ...}, code) =
            let
                val instr =
                    case (isAdd, ccRef, length) of
                        (true, NONE, Arith64) => addShiftedReg
                    |   (true, SOME _, Arith64) => addSShiftedReg
                    |   (false, NONE, Arith64) => subShiftedReg
                    |   (false, SOME _, Arith64) => subSShiftedReg
                    |   (true, NONE, Arith32) => addShiftedReg32
                    |   (true, SOME _, Arith32) => addSShiftedReg32
                    |   (false, NONE, Arith32) => subShiftedReg32
                    |   (false, SOME _, Arith32) => subSShiftedReg32
                val destReg =
                    case dest of
                        NONE => XZero
                    |   SOME(PReg dreg) => getAllocatedReg dreg
            in
                instr{regN=getAllocatedReg oreg1, regM=getAllocatedReg oreg2, regD=destReg, shift=ShiftNone} :: code
            end           

        |   codeExtended _ ({instr=BeginFunction{regArgs, ...}, ...}, code) =
            let
                (* The real registers used for arguments. X30 is there but saved separately. *)
                val saveRegs = List.filter (fn r => r <> X30) (List.map #2 regArgs)
                val skipCheck = createLabel()
                val defaultWords = 10 (* This is wired into the RTS. *)
                val workRegister = X10  (* Spare: not used yet. *)
                val debugTrapAlways = false (* Can be set to true for debugging *)

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

                val usedRegs = List.filter (isUsed o #1) regArgs
                fun mkPair(PReg pr, rr) = {src=rr,dst=getAllocatedReg pr}
                val regPairs = List.map mkPair usedRegs
            in
                moveMultipleRegisters(regPairs, code3)
            end

        |   codeExtended _ ({instr=ReturnResultFromFunction { resultReg=PReg resReg, returnReg=PReg retReg, numStackArgs }, ...}, code) =
            let
                val resultReg = getAllocatedReg resReg
                and returnReg = getAllocatedReg retReg

                fun resetStack(0, code) = code
                |   resetStack(nItems, code) =
                        addConstantWord({regS=X_MLStackPtr, regD=X_MLStackPtr, regW=X3,
                                        value=Word64.fromLarge(Word.toLarge nativeWordSize) * Word64.fromInt nItems}, code)
            in
                returnRegister returnReg :: resetStack(numStackArgs, moveIfNecessary({src=resultReg, dst=X0}, code))
            end

        |   codeExtended _ ({instr=RaiseExceptionPacket{ packetReg=PReg preg }, ...}, code) =
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
                moveIfNecessary({src=getAllocatedReg preg, dst=X0}, code)

        |   codeExtended _ ({instr=PushToStack{ source=PReg sreg, copies, ... }, ...}, code) =
            let
                val reg = getAllocatedReg sreg
                val _ = copies > 0 orelse raise InternalError "PushToStack: copies<1"
                fun pushn(0, c) = c
                |   pushn(n, c) = pushn(n-1, storeRegPreIndex{regT=reg, regN=X_MLStackPtr, byteOffset= ~8} :: c)
            in
                pushn(copies, code)
            end

        |   codeExtended _ ({instr=LoadStack{ dest=PReg dreg, cache=SOME(PReg creg), ... }, ...}, code) =
                (* Load a value from the stack when the value is cached - use the register *)
                moveIfNecessary({src=getAllocatedReg creg, dst=getAllocatedReg dreg}, code)

        |   codeExtended _ ({instr=LoadStack{ dest=PReg dreg, wordOffset, cache=NONE, ... }, ...}, code) =
                (* Load from the stack.  Always 64-bits. *)
            let
                val destReg = getAllocatedReg dreg
            in
                if wordOffset >= 4096
                then loadRegIndexed{regT=destReg, regN=X_MLStackPtr, regM=destReg, option=ExtUXTX ScaleOrShift} ::
                        List.rev(loadNonAddress(destReg, Word64.fromInt wordOffset)) @ code
                else loadRegScaled{regT=destReg, regN=X_MLStackPtr, unitOffset=wordOffset} :: code
            end

        |   codeExtended _ ({instr=StoreToStack{ source=PReg sreg, stackOffset, ... }, ...}, code) =
                (* Store into the stack to set a field of a container.  Always 64-bits. *)
            let
                val sourceReg = getAllocatedReg sreg
            in
                if stackOffset >= 4096
                then storeRegIndexed{regT=sourceReg, regN=X_MLStackPtr, regM=workReg1, option=ExtUXTX ScaleOrShift} ::
                        List.rev(loadNonAddress(workReg1, Word64.fromInt stackOffset)) @ code
                else storeRegScaled{regT=sourceReg, regN=X_MLStackPtr, unitOffset=stackOffset} :: code
            end

        |   codeExtended _ ({instr=ContainerAddress{ dest=PReg dreg, stackOffset, ... }, ...}, code) =
                (* Set the register to an offset in the stack. *)
            let
                val destReg = getAllocatedReg dreg
                val _ = stackOffset >= 0 orelse raise InternalError "codeGenICode: ContainerAddress - negative offset"
                val byteOffset = stackOffset * Word.toInt nativeWordSize
            in
                if byteOffset >= 4096
                then addShiftedReg{regN=X_MLStackPtr, regM=destReg, regD=destReg, shift=ShiftNone} ::
                        List.rev(loadNonAddress(destReg, Word64.fromInt byteOffset)) @ code
                else addImmediate{regN=X_MLStackPtr, regD=destReg, immed=Word.fromInt byteOffset, shifted=false} :: code
            end

        |   codeExtended _ ({instr=ResetStackPtr{ numWords, ... }, ...}, code) =
            let
                val _ = numWords >= 0 orelse raise InternalError "codeGenICode: ResetStackPtr - negative offset"
                (* Assume that this will fit in 24 bits since that would be over 2M entries pushed. *)
                val numBytes = numWords * Word.toInt nativeWordSize
                val _ = numBytes < Word.toInt(Word.<<(0w1, 0w24)) orelse
                            raise InternalError "codeGenICode: ResetStackPtr - offset exceeds 24 bits";
                val loword = Word.andb(Word.fromInt numBytes, 0wxfff)
                val hiword = Word.>>(Word.fromInt numBytes, 0w12)
                fun addToSP(value, shift) =
                    if value = 0w0 then []
                    else [addImmediate{regN=X_MLStackPtr, regD=X_MLStackPtr, immed=value, shifted=shift}]
            in
                addToSP(loword, false) @ addToSP(hiword, true) @ code
            end

        val blockToLabelMap = Vector.tabulate(numBlocks, fn _ => createLabel())

        fun getBlockLabel blockNo = Vector.sub(blockToLabelMap, blockNo)

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
                                fun genCodeBlock(instr, code) = codeExtended {flow=flow} (instr, code)
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
            val ops = genCode([0], ExitCode, [])
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
        and xReg                = xReg
        and closureRef          = closureRef
    end

end;
