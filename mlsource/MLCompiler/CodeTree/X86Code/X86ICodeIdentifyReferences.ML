(*
    Copyright (c) 2016-17 David C.J. Matthews

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

functor X86ICodeIdentifyReferences(
    structure ICODE: ICodeSig
    structure DEBUG: DEBUGSIG
    structure INTSET: INTSETSIG
): X86IDENTIFYREFSSIG =
struct
    open ICODE
    open INTSET
    
    datatype pushState = MustPush | MustNotPush | MayPush

    type regState =
    { 
        active: int, refs: int, pushState: pushState
    }

    and conflictState =
    {
        conflicts: int list, realConflicts: reg list
    }
    
    datatype extendedBasicBlock =
        ExtendedBasicBlock of
        {
            block: {instr: abstract x86ICode, current: intSet, active: intSet } list,
            flow: controlFlow,
            locals: intSet, (* Defined and used entirely within the block. *)
            imports: intSet, (* Defined outside the block, used inside it, but not needed afterwards. *)
            exports: intSet, (* Defined within the block, possibly used inside, but used outside. *)
            passThrough: intSet (* Active throughout the block. May be referred to by it but needed afterwards. *)
        }
    
    exception InternalError = Misc.InternalError
    
    (* Return the list of blocks that are the immediate successor of this. *)
    fun blockSuccessors(BasicBlock{flow=Unconditional l, ...}) = [l]
    |   blockSuccessors(BasicBlock{flow=Conditional{trueJump, falseJump, ...}, ...}) = [trueJump, falseJump]
    |   blockSuccessors(BasicBlock{flow=ExitCode, ...}) = []
    |   blockSuccessors(BasicBlock{flow=IndexedBr cases, ...}) = cases
    |   blockSuccessors(BasicBlock{flow=SetHandler{handler, continue, ...}, ...}) = [handler, continue]

    (* Find the registers from an argument. *)
    fun argRegs(RegisterArgument rarg) = [rarg]
    |   argRegs(MemoryLocation { base, index, ...}) = base :: argIndex index
    |   argRegs _ = []
    
    and argIndex NoMemIndex = []
    |   argIndex(MemIndex1 arg) = [arg]
    |   argIndex(MemIndex2 arg) = [arg]
    |   argIndex(MemIndex4 arg) = [arg]
    |   argIndex(MemIndex8 arg) = [arg]

    (* Return the set of registers used by the instruction.
       sources are registers that must have values after the instruction.
       dests are registers that are given values or modified by the instruction. *)
    fun getInstructionRegisters(LoadArgument { source, dest, ...}) =
        { sources=argRegs source, dests=[dest] }

    |   getInstructionRegisters(StoreArgument{ source, base, index, ...}) =
            { sources=argRegs source @ [base] @ argIndex index, dests=[] }

    |   getInstructionRegisters(LoadMemReg { dest, ...}) =
            { sources=[], dests=[dest] }

    |   getInstructionRegisters(ExchangeRegisters _) =
            (* Should not occur at this stage. *)
            raise InternalError "getInstructionRegisters - ExchangeRegisters"

    |   getInstructionRegisters(BeginFunction {regArgs, stackArgs}) =
            { sources=[], dests=map #1 regArgs (*@ map (fn StackLoc{rno, ...} => rno) stackArgs*)}

    |   getInstructionRegisters(FunctionCall{regArgs, stackArgs, dest, ...}) =
        let
            (* Non-tail-recursive.  Behaves as a normal reference to sources. *)
            val stackSources = List.foldl(fn (arg, srcs) => argRegs arg @ srcs) [] stackArgs
        
            fun regSource((arg, _), srcs) = argRegs arg @ srcs
            val regSources = List.foldl regSource [] regArgs
        in
            { sources=stackSources @ regSources, dests=[dest] }
        end

    |   getInstructionRegisters(TailRecursiveCall{regArgs, stackArgs, ...}) =
        let
            (* Tail recursive call.  References the argument sources but exits. *)
            val stackSources = List.foldl(fn ({src, ...}, srcs) => argRegs src @ srcs) [] stackArgs
            fun regSource((arg, _), srcs) = argRegs arg @ srcs
            val regSources = List.foldl regSource [] regArgs
        in
            { sources=stackSources @ regSources, dests=[] }
        end

    |   getInstructionRegisters(AllocateMemoryOperation{dest, ...}) =
            { sources=[], dests=[dest] }

    |   getInstructionRegisters(AllocateMemoryVariable{size, dest, ...}) =
            { sources=[size], dests=[dest] }

    |   getInstructionRegisters(InitialiseMem{size, addr, init}) =
            { sources=[size, addr, init], dests=[] }

    |   getInstructionRegisters(InitialisationComplete) =
            (* This is just a marker.  It doesn't actually generate any code. *)
            { sources=[], dests=[] }

    |   getInstructionRegisters(JumpLoop{regArgs, stackArgs, ...}) =
        let
            val regSourceAsRegs =
                List.foldl(fn ((source, _), srcs) => argRegs source @ srcs) [] regArgs
            val sourceAsRegs =
                List.foldl(fn ((source, _), srcs) => argRegs source @ srcs) regSourceAsRegs stackArgs
        in
            { sources=sourceAsRegs, dests=[] }
        end

    |   getInstructionRegisters(RaiseExceptionPacket{packet}) =
            { sources=argRegs packet, dests=[] }

    |   getInstructionRegisters(ReserveContainer{address, ...}) =
            { sources=[], dests=[address] }

    |   getInstructionRegisters(IndexedCaseOperation{testReg, workReg, ...}) =
            { sources=[testReg], dests=[workReg]}

    |   getInstructionRegisters(LockMutable{addr}) =
            { sources=[addr], dests=[] }

    |   getInstructionRegisters(WordComparison{arg1, arg2, ...}) =
            { sources=argRegs arg1 @ argRegs arg2, dests=[] }

    |   getInstructionRegisters(PushExceptionHandler{workReg, ...}) =
            { sources=[], dests=[workReg]}

    |   getInstructionRegisters(PopExceptionHandler{ workReg }) =
            { sources=[], dests=[workReg]}

    |   getInstructionRegisters(BeginHandler{ workReg, packetReg, ...}) =
            { sources=[], dests=[packetReg, workReg]}

    |   getInstructionRegisters(ReturnResultFromFunction{resultReg, ...}) =
            { sources=[resultReg], dests=[] }

    |   getInstructionRegisters(ArithmeticFunction{resultReg, operand1, operand2, ...}) =
            { sources=argRegs operand1 @ argRegs operand2, dests=[resultReg] }

    |   getInstructionRegisters(TestTagBit{arg, ...}) =
            { sources=argRegs arg, dests=[] }

    |   getInstructionRegisters(PushValue {arg}) =
            { sources=argRegs arg, dests=[] }

    |   getInstructionRegisters(ResetStackPtr _) =
            { sources=[], dests=[] }

    |   getInstructionRegisters(TagValue{source, dest}) =
            { sources=[source], dests=[dest]}

    |   getInstructionRegisters(UntagValue{source, dest, ...}) =
            { sources=argRegs source, dests=[dest]}

    |   getInstructionRegisters(LoadEffectiveAddress{base, index, dest, ...}) =
        let
            val bRegs =
                case base of NONE => [] | SOME bReg => [bReg]
            val iRegs = argIndex index
        in
            { sources=bRegs @ iRegs, dests=[dest] }
        end

    |   getInstructionRegisters(ShiftOperation{resultReg, operand, shiftAmount, ...}) =
            { sources=argRegs operand @ argRegs shiftAmount, dests=[resultReg] }

    |   getInstructionRegisters(Multiplication{resultReg, operand1, operand2, ...}) =
            { sources=argRegs operand1 @ argRegs operand2, dests=[resultReg] }

    |   getInstructionRegisters(Division{dividend, divisor, quotient, remainder, ...}) =
            { sources=dividend :: argRegs divisor, dests=[quotient, remainder] }

    |   getInstructionRegisters(AtomicExchangeAndAdd{base, source}) =
            { sources=[base, source], dests=[] }

    |   getInstructionRegisters(BoxValue{source, dest, ...}) =
            { sources=[source], dests=[dest] }

    |   getInstructionRegisters(CompareByteVectors{vec1Addr, vec2Addr, length, ...}) =
            { sources=[vec1Addr, vec2Addr, length], dests=[] }

    |   getInstructionRegisters(BlockMove{srcAddr, destAddr, length, ...}) =
            { sources=[srcAddr, destAddr, length], dests=[] }

    |   getInstructionRegisters(CompareFloatingPt{arg1, arg2, ...}) =
            { sources=argRegs arg1 @ argRegs arg2, dests=[] }

    |   getInstructionRegisters(X87FPGetCondition{dest, ...}) =
            { sources=[], dests=[dest] }

    |   getInstructionRegisters(X87FPArith{resultReg, arg1, arg2, ...}) =
            { sources=argRegs arg1 @ argRegs arg2, dests=[resultReg] }

    |   getInstructionRegisters(X87FPUnaryOps{dest, source, ...}) =
            { sources=argRegs source, dests=[dest] }

    |   getInstructionRegisters(FloatFixedInt{dest, source}) =
            { sources=argRegs source, dests=[dest] }

    |   getInstructionRegisters(SSE2FPArith{resultReg, arg1, arg2, ...}) =
            { sources=argRegs arg1 @ argRegs arg2, dests=[resultReg]}

    fun identifyRegisters(blockVector, pregProps): extendedBasicBlock vector * regState vector =
    let
        val maxPRegs = Vector.length pregProps
        val vectorLength = Vector.length blockVector
        (* Initial arrays - declarationArray is the set of registers given
           values by the block, importArray is the set of registers referenced by
           the block and not declared locally. *)
        val declarationArray = Array.array(vectorLength, emptySet)
        and importArray = Array.array(vectorLength, emptySet)
        
        (* References - this is used locally to see if a register is ever
           actually used and also included in the result which uses it as
           part of the choice of which register to spill. *)
        val regRefs = Array.array(maxPRegs, 0)
        (* Whether the register must be saved on the stack, must not or could be. *)
        and regPushState = Array.array(maxPRegs, MayPush)
        
        fun incrRef r = Array.update(regRefs, r, Array.sub(regRefs, r)+1)
        
        (* Contains the, possibly filtered, code for each block. *)
        val resultCode = Array.array(vectorLength, NONE)
        
        (* First pass - for each block build up the sets of registers defined and
           used in the block.  We do this depth-first so that we can use "refs" to
           see if a register is used.  If this is an instruction that can be eliminated
           we don't need to generate it and can ignore any references it makes. *)
        local
            fun blockScan blockNo =
            if isSome(Array.sub(resultCode, blockNo)) then ()
            else
            let
                val () = Array.update(resultCode, blockNo, SOME []) (* Prevent looping. *)
                val thisBlock as BasicBlock { block, ...} = Vector.sub(blockVector, blockNo)
                val successors = blockSuccessors thisBlock
                (* Visit everything reachable first. *)
                val () = List.app blockScan successors
                
                fun scanCode(instr, original as { code, decs, refs }) =
                let
                    val { sources, dests } = getInstructionRegisters instr
                    val eliminateable =
                        case instr of
                            LoadArgument _ => true
                        |   TagValue _ => true
                        |   UntagValue _ => true
                        |   LoadEffectiveAddress _ => true
                        |   BoxValue _ => true
                        |   _ => false
                    fun regNo(PReg(i, _)) = i
                    val destRegNos = map regNo dests
                    and sourceRegNos = map regNo sources
                in
                    if eliminateable andalso
                        List.all(fn dReg => Array.sub(regRefs, dReg) = 0) destRegNos
                    then original (* Don't include this instruction. *)
                    else
                    let
                        (* Only mark the sources as referred after we know we're going to need this.
                           In that way we may eliminate the instruction that created this source. *)
                        val () = List.app incrRef sourceRegNos
                        fun setPushState(PReg(i, PRegUntagged)) = Array.update(regPushState, i, MustNotPush)
                        |   setPushState _ = ()
                        val () = List.app setPushState dests
                    in
                        { code = instr :: code, decs = union(listToSet destRegNos, decs), refs = union(listToSet sourceRegNos, refs) }
                    end
                end
                
                val { code, decs, refs } = List.foldr scanCode {code=[], decs=emptySet, refs=emptySet} block
            in
                Array.update(declarationArray, blockNo, decs);
                (* refs includes local declarations. Remove before adding to the result. *)
                Array.update(importArray, blockNo, minus(refs, decs));
                Array.update(resultCode, blockNo, SOME code)
            end
        in
            val () = blockScan 0 (* Start with the root block. *)
        end
        
        (* Second phase - Propagate reference information between the blocks.
           We need to consider loops here.  Do a depth-first scan marking each
           block.  If we find a loop we save the import information we've used.
           If when we come to process that block we find the import information
           is different we need to reprocess. *)
        (* Pass through array - values used in other blocks after this that
           are not declared in this block. *)
        val passThroughArray = Array.array(vectorLength, emptySet)
        (* Exports - those of our declarations that are used in other blocks. *)
        val exportArray = Array.array(vectorLength, emptySet)
        local
            datatype loopData = Unprocessed | Processing | Processed | Looped of intSet
            
            fun reprocessLoop () =
            let
                val reprocess = ref false
                val loopArray = Array.array(vectorLength, Unprocessed)
            
                fun processBlocks blockNo =
                    case Array.sub(loopArray, blockNo) of
                        Processed => (* Already seen this by a different route. *)
                            union(Array.sub(passThroughArray, blockNo), Array.sub(importArray, blockNo))
                    |   Looped s => s (* We've already seen this in a loop. *)
                    |   Processing => (* We have a loop. *)
                        let
                            (* Use the existing input array. *)
                            val inputs =
                                union(Array.sub(passThroughArray, blockNo), Array.sub(importArray, blockNo))
                            val () = Array.update(loopArray, blockNo, Looped inputs)
                        in
                            inputs
                        end
                    |   Unprocessed => (* Normal case - not visited yet. *)
                        let
                            val () = Array.update(loopArray, blockNo, Processing)
                            val thisBlock = Vector.sub(blockVector, blockNo)
                            val ourDeclarations = Array.sub(declarationArray, blockNo)
                            val successors = blockSuccessors thisBlock
                            fun addSuccessor b =
                            let
                                val theirImports = processBlocks b
                                (* Split the imports.  If a register is a local declaration then
                                   it becomes an export.  If it is not it becomes part of our
                                   passThrough. *)
                                val (addToExp, addToImp) =
                                    INTSET.partition (fn i => member(i, ourDeclarations)) theirImports
                            in
                                Array.update(exportArray, blockNo,
                                    union(Array.sub(exportArray, blockNo), addToExp));
                                Array.update(passThroughArray, blockNo,
                                    union(Array.sub(passThroughArray, blockNo), addToImp))
                            end
                            val () = List.app addSuccessor successors
                            val ourInputs =
                                union(Array.sub(passThroughArray, blockNo), Array.sub(importArray, blockNo))
                        in
                            (* Was this block used in a loop? *)
                            case Array.sub(loopArray, blockNo) of
                                Looped s =>
                                    if setToList s = setToList ourInputs then ()
                                    else reprocess := true
                            |   _ => ();
                            Array.update(loopArray, blockNo, Processed);
                            ourInputs
                        end

            in
                reprocess := false;
                processBlocks 0;
                if !reprocess then reprocessLoop () else ()
            end
        in
            val () = reprocessLoop ()
        end
        
        (* Third pass - Build the result list with the active registers for each
           instruction.  We don't include registers in the passThrough set since
           they are active throughout the block. *)
        local
            (* Number of instrs for which this is active.  We use this to try to select a
               register to push to the stack if we have too many.  Registers that have
               only a short lifetime are less likely to be pushed than those that are
               active longer. *)
            val regActive = Array.array(maxPRegs, 0)
            fun addActivity n r = Array.update(regActive, r, Array.sub(regActive, r)+n)
            
            fun createResultInstrs passThrough (instr, (tail, activeAfterThis)) =
            let
                val { sources, dests } = getInstructionRegisters instr
                fun regNo(PReg(i, _)) = i
                val destRegNos = map regNo dests
                and sourceRegNos = map regNo sources
                val destSet = listToSet destRegNos
                (* Remove any sources that are present in passThrough since
                   they are going to be active throughout the block. *)
                and sourceSet = minus(listToSet sourceRegNos, passThrough)

                val afterRemoveDests = minus(activeAfterThis, destSet)
                val activeForPrevious = union(sourceSet, afterRemoveDests)
            
                (* The "active" set is the set of registers that need to be active DURING the
                   instruction.  It includes destinations, which will usually be in
                   "activeAfterThis", because there may be destinations that are not actually used
                   subsequently but still need a register.  That will also include work registers.
                   Usually sources aren't included if this is the last use but the
                   AllocateMemoryVariable "instruction" can't set the size after the memory is
                   allocated so the active set includes the source(s). *)
                val activeForInstr =
                    case instr of
                        FunctionCall _ => sourceSet (* Is this still needed? *)
                    |   TailRecursiveCall _ => sourceSet (* Is this still needed? *)
                    |   AllocateMemoryVariable _ => (* We can only set the size after the memory is allocated. *)
                            union(activeAfterThis, union(sourceSet, destSet))
                    |   BoxValue _ => (* We can only store the value in the box after the box is allocated. *)
                            union(activeAfterThis, union(sourceSet, destSet))
                    |   _ => union(activeAfterThis, destSet)
                
                val () = List.app(addActivity 1) (setToList activeForInstr)

                local
                    (* If we are allocating memory we have to save the current registers if
                       they could contain an address.  We mustn't push untagged registers
                       and we mustn't push the destination. *)
                    fun getSaveSet dReg =
                    let
                        val activeAfter = union(activeAfterThis, passThrough)
                        (* Remove any registers marked - must-not-push.  These are
                           registers holding non-address values.  They will actually
                           be saved by the RTS across any GC but not checked or
                           modified by the GC.
                           Exclude the result register. *)
                        fun getSave i =
                            if i = dReg
                            then NONE
                            else case Array.sub(regPushState, i) of
                                MustNotPush => NONE
                            |   MustPush => NONE (* Already on the stack. *)
                            |   _ => SOME(PReg(i, PRegGeneral))
                    in
                        List.mapPartial getSave (setToList activeAfter)
                    end
                in
                    (* Sometimes we need to modify the instruction e.g. to include the set
                       of registers to save. *)
                    val convertedInstr =
                        case instr of
                            AllocateMemoryOperation{size, flags, dest, saveRegs=_} =>
                                AllocateMemoryOperation{size=size, flags=flags, dest=dest,
                                    saveRegs=getSaveSet(regNo dest)}

                        |   AllocateMemoryVariable{size, dest, saveRegs=_} =>
                                AllocateMemoryVariable{size=size, dest=dest, saveRegs=getSaveSet(regNo dest)}

                        |   BoxValue{source, dest, boxKind, saveRegs=_} =>
                                BoxValue{source=source, dest=dest, boxKind=boxKind,
                                    saveRegs=getSaveSet(regNo dest)}
                        
                        |   JumpLoop{regArgs, stackArgs, checkInterrupt, ...} =>
                            let
                                (* If we have to check for interrupts we must preserve registers across
                                   the RTS call. *)
                                fun getSave i =
                                    case Array.sub(regPushState, i) of
                                        MustNotPush => NONE
                                    |   MustPush => NONE (* Already on the stack. *)
                                    |   _ => SOME(PReg(i, PRegGeneral))
                                val currentRegs = union(activeAfterThis, passThrough)
                                val check =
                                    case checkInterrupt of
                                        NONE => NONE
                                    |   SOME _ => SOME(List.mapPartial getSave (setToList currentRegs))
                            in
                                JumpLoop{regArgs=regArgs, stackArgs=stackArgs, checkInterrupt=check}
                            end

                        |   _ => instr
                end
                
                (* FunctionCall and Handler need to mark all registers as "push". *)
                local
                    fun pushRegisters () =
                    let
                        val activeAfter = union(activeAfterThis, passThrough)
                        fun pushAllButDests i =
                            if List.exists(fn j => i=j) destRegNos
                            then () else Array.update(regPushState, i, MustPush)
                    in
                        (* We need to push everything active after this
                           except the result register. *)
                        List.app pushAllButDests (setToList activeAfter)
                    end
                in
                    val () =
                        case instr of
                            FunctionCall _ => pushRegisters ()
                        |   BeginHandler _ => pushRegisters ()
                        |   _ => ()
                end
            in
                ({instr=convertedInstr, active=activeForInstr, current=activeAfterThis} :: tail, activeForPrevious)
            end

            fun createResult blockNo =
            let
                val BasicBlock{ flow, ...} = Vector.sub(blockVector, blockNo)
                val declSet   = Array.sub(declarationArray, blockNo)
                and importSet = Array.sub(importArray, blockNo)
                and passSet   = Array.sub(passThroughArray, blockNo)
                and exportSet = Array.sub(exportArray, blockNo)
                val filteredCode = getOpt(Array.sub(resultCode, blockNo), [])
                (* At the end of the block we should have the exports active. *)
                val (resultInstrs, _) = List.foldr (createResultInstrs passSet) ([], exportSet) filteredCode
                (* Set the active count for the pass through. *)
                val instrCount = List.length filteredCode
                val () = List.app(addActivity instrCount) (setToList passSet)
            in
                ExtendedBasicBlock {
                    block = resultInstrs,
                    flow=flow,
                    locals = minus(declSet, exportSet),
                    imports = importSet,
                    exports = exportSet,
                    passThrough = passSet
                }
            end
        in
            val resultBlocks = Vector.tabulate(vectorLength, createResult)
            val regActive = regActive
        end
        
        val registerState: regState vector =
            Vector.tabulate(maxPRegs,
                fn i => {
                    active = Array.sub(regActive, i),
                    refs = Array.sub(regRefs, i),
                    pushState = Array.sub(regPushState, i)
                }
            )
    in
        (resultBlocks, registerState)
    end

    structure Sharing =
    struct
        type 'reg x86ICode = 'reg x86ICode
        and reg = reg
        and pushState = pushState
        and abstract = abstract
        and intSet = intSet
        and 'reg basicBlock = 'reg basicBlock
        and extendedBasicBlock = extendedBasicBlock
        and controlFlow = controlFlow
        and 'reg argument = 'reg argument
        and 'reg memoryIndex = 'reg memoryIndex
        and regProperty = regProperty
    end
end;
