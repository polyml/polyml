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

functor X86ICodeGetConflictSets(
    structure ICODE: ICodeSig
    structure INTSET: INTSETSIG
): X86GETCONFLICTSETSIG =
struct
    open ICODE
    open INTSET

    type conflictState =
    {
        conflicts: intSet, realConflicts: reg list
    }
    
    exception InternalError = Misc.InternalError
    
    (* Get the conflict sets.  This code was originally part of identifyRegisterState and
       was split off. *)
    fun getConflictStates (icode, maxPRegs) =
    let
        (* Other registers that conflict with this i.e. cannot share the same
           real register. *)
        val regConflicts = Array.array(maxPRegs, emptySet)
        (* Real registers that cannot be used for this because are needed for
           an instruction e.g. shift or block move, that requires these. *)
        and regRealConflicts = Array.array(maxPRegs, []: reg list)
 
        (* Find the sources when the argument is used as a source. *)
        fun sourceRegs(RegisterArgument rarg) = [sourceReg rarg]
        |   sourceRegs(MemoryLocation { base, index, ...}) =
            let
                val bRegs = sourceReg base
                val iRegs = sourceIndex index
            in
                bRegs :: iRegs
            end
        |   sourceRegs _ = []
        
        and sourceReg(PReg(i, _)) = i
        
        and sourceIndex NoMemIndex = []
        |   sourceIndex (MemIndex1 arg) = [sourceReg arg]
        |   sourceIndex (MemIndex2 arg) = [sourceReg arg]
        |   sourceIndex (MemIndex4 arg) = [sourceReg arg]
        |   sourceIndex (MemIndex8 arg) = [sourceReg arg]

        (* Any particular argument is only ever a single destination. *)
        fun destReg(PReg(i, _)) = i
        
        local        
            fun addConflictsTo(aReg, conflicts) =
                Array.update(regConflicts, aReg, union(Array.sub(regConflicts, aReg), conflicts))
        in
            (* Registers are allocated from the end back so we only need to consider conflicts
               when a register is first encountered (i.e. the last use).  We don't need to
               add new registers to the conflict sets for registers in the currentSet. *)           
            fun addNewConflicts(currentSet, newItems) =
            let
                (* We have to establish conflicts in both directions for new items. *)
                val united = union(currentSet, listToSet newItems)
                val () = List.app(fn i => addConflictsTo(i, united))  newItems
            in
                ()
            end
        end
        
        (* If we need to use a specific register for an argument e.g. ecx for a shift,
           we mustn't use that register for anything else.
           We need to pass an exclude set because it's possible that the
           register could be in the current active set because it's needed
           later on. *)
        fun addRealRegConflicts(currentSet, exclude, reg) =
        let
            fun isInset reg set = List.exists (fn r => r = reg) set
            fun checkAndAdd i =
            if isInset i exclude
            then ()
            else
            let
                val currentConflicts = Array.sub(regRealConflicts, i)
            in
                if isInset reg currentConflicts
                then ()
                else Array.update(regRealConflicts, i, reg :: currentConflicts)
            end
        in
            List.app checkAndAdd currentSet
        end
        
        (* Filter the destination.  We don't need the full power of List.filter since there's
           at most one.  It may not be there if we've never used it. *)
        val removeDest = removeFromSet
        
        fun removeDests(dests, fromSet) = List.foldl (fn (d, l) => removeDest(d, l)) fromSet dests
        
        fun addSourcesAndDestsExt(sourcesPre, sourcesPost, dests, oldCurrent) =
        let
            (* The current set is the set of registers that are in use AFTER this
               instruction.  So if this the last use of a source register (i.e. working
               backwards we've not seen it before) it won't be in currentRegs.
               The conflict set is the set of registers that need to be different.
               That's going to include this destination.
               The conflict set is the union of the current set and the destinations
               The destinations will be in the current set if they are used but we
               can have destinations, e.g. work registers or quotient/remainder
               that need to have registers allocated that aren't then used. *)
            val currentRegs = oldCurrent
            val afterRemoveDests = removeDests(dests, currentRegs)
            (* Conflict sets.  We consider two cases.  We need to mark the conflict that
               applies after this instruction and also the conflict before.
               Normally the destination will be among the currentRegs but if
               it is not actually used it won't be so we need to add it explicitly.
               We also need to add any special sources that still need to be valid
               after the instruction.
               The conflict set before the instruction adds any sources that
               first occur in this instruction. *)
            val () = addNewConflicts(currentRegs, dests @ sourcesPost)
            (* Adding to the set also marks them as conflicts. *)
            val () = addNewConflicts(afterRemoveDests, sourcesPre @ sourcesPost)
        in
            ()
        end

        (* Simpler version of addSourcesAndDestsExt which can be used if we don't need sourcesPost. *)
        and addSourcesAndDests(sources, dests, oldCurrent) =
            addSourcesAndDestsExt(sources, [], dests, oldCurrent)

        and identify({instr=LoadArgument { source, dest, ...}, current=oldActive, ...}) =
                (* Moving to a preg, the destination. *)
                addSourcesAndDests(sourceRegs source, [destReg dest], oldActive)

        |   identify({instr=StoreArgument { source as RegisterArgument sReg, base, index, kind=MoveByte, ...}, current=oldActive, ...}) =
                (* Storing a byte value.  This is messy on X86/32 because we can't use edi or esi as the register
                   to store.  To get round this we reserve ecx as a possible register as with shifts.
                   We don't actually need to use this but it is available if necessary. *)
                if isX64
                then addSourcesAndDests(sourceRegs source @ [sourceReg base] @sourceIndex index, [], oldActive)
                else
                let
                    val storeReg = sourceReg sReg
                    val addrRegs = sourceReg base :: sourceIndex index
                    val () = addSourcesAndDests(storeReg :: addrRegs, [], oldActive)
                    val () = addRealRegConflicts(setToList oldActive @ addrRegs, [storeReg], GenReg ecx)
                in
                    ()
                end

        |   identify({instr=StoreArgument { source, base, index, ...}, current=oldActive, ...}) =
                (* Moving to memory.  The base and index registers are sources not destinations. *)
                addSourcesAndDests(sourceRegs source @ [sourceReg base] @ sourceIndex index, [], oldActive)

        |   identify({instr=LoadMemReg { dest, ...}, current=oldActive, ...}) =
                addSourcesAndDests([], [destReg dest], oldActive)

        |   identify({instr=ExchangeRegisters _, ...}) =
                (* Should not occur at this stage. *)
                raise InternalError "identify - ExchangeRegisters"

        |   identify({instr=BeginFunction {regArgs}, current=oldActive, ...}) =
            let
                val destRegs = map (destReg o #1) regArgs
            in
                addSourcesAndDests([], destRegs, oldActive)
            end

        |   identify({instr=FunctionCall{regArgs, stackArgs, ...}, ...}) =
            let 
                (* Non-tail-recursive.  Behaves as a normal reference to sources. *)
                val stackSources = List.foldl(fn (arg, srcs) => sourceRegs arg @ srcs) [] stackArgs
                
                fun regSource((arg, _), srcs) = sourceRegs arg @ srcs
                val regSources = List.foldl regSource [] regArgs
                (* Since the registers have been pushed we don't need to add them to the active set.
                   The active set is just the arguments. *)
                val sources = stackSources @ regSources
                val () = addNewConflicts(emptySet, sources)
            in
                ()
            end

        |   identify({instr=TailRecursiveCall{regArgs, stackArgs, ...}, ...}) =
            let 
                (* Tail recursive call.  References the argument sources but exits. *)
                val stackSources = List.foldl(fn ({src, ...}, srcs) => sourceRegs src @ srcs) [] stackArgs
                
                fun regSource((arg, _), srcs) = sourceRegs arg @ srcs
                val regSources = List.foldl regSource [] regArgs
                val active = stackSources @ regSources
            in
                addNewConflicts(emptySet, active)
            end

        |   identify({instr=AllocateMemoryOperation{dest, ...}, current=oldActive, ...}) =
                addSourcesAndDests([], [destReg dest], oldActive)

        |   identify({instr=AllocateMemoryVariable{size, dest, ...}, current=oldActive, ...}) =
            let
                val dReg = destReg dest
                val sources = [sourceReg size]
                (* We have to include the sources in the conflict set.  Unlike, say LoadArgument where
                   a register in the source can be reused in the destination, we can't reuse the same
                   register for the result as any of the arguments. *)
            in
                addSourcesAndDestsExt([], sources, [dReg], oldActive)
            end

        |   identify({instr=InitialiseMem{size, addr, init}, current=oldActive, ...}) =
            let
                (* We are going to use rep stosl/q to set the memory.
                   That requires the length to be in ecx, the initialiser to be in eax and
                   the address to be edi. *)
                val aReg = sourceReg addr
                val iReg = sourceReg init
                val sReg = sourceReg size
                val sources = [aReg, iReg, sReg]
                val () =
                    addSourcesAndDests(sources, [], oldActive)
                (* Since we don't have any destinations we won't have removed anything from the
                   set.  newActive will therefore be currentRegs plus the sources. *)
                val potentialConflicts = setToList oldActive @ sources
                val () = addRealRegConflicts(potentialConflicts, [aReg], GenReg edi)
                val () = addRealRegConflicts(potentialConflicts, [iReg], GenReg eax)
                val () = addRealRegConflicts(potentialConflicts, [sReg], GenReg ecx)
            in
                ()
            end

        |   identify({instr=InitialisationComplete, ...}) = ()

        |   identify({instr=StartLoop _, ...}) = ()

        |   identify({instr=EndLoop _, current=oldLoopRegs, ...}) =
                (* Establish the conflict between the registers in the current set if necessary.
                   This avoids the need to do it on each JumpLoop.
                   The loop labels have to include ALL the registers in the nested
                   loops. *)
                addNewConflicts(emptySet, setToList oldLoopRegs)

        |   identify({instr=JumpLoop{regArgs, stackArgs, ...}, current=oldCurrent, ...}) =
            let
                val regSourceAsRegs =
                    List.foldl(fn ((source, _), srcs) => sourceRegs source @ srcs) [] regArgs
                val sourceAsRegs =
                    List.foldl(fn ((source, _), srcs) => sourceRegs source @ srcs) regSourceAsRegs stackArgs
                (* Because this is an unconditional branch the state afterwards is empty. *)
                val () = addNewConflicts(emptySet, sourceAsRegs)
                (* Add the loop registers as conflicts just in case they are never used.
                   We should remove unused arguments at a higher level. *)
                val destRegs = List.foldl(fn ((_, loopReg), dests) => destReg loopReg :: dests) [] regArgs
            in
                addNewConflicts(oldCurrent, destRegs)
            end

        |   identify({instr=RaiseExceptionPacket _, ...}) = ()

        |   identify({instr=ReserveContainer{address, ...}, current=oldActive, ...}) =
                addSourcesAndDests([], [destReg address], oldActive)

        |   identify({instr=IndexedCaseOperation{testReg, workReg, ...}, current=oldCurrent, ...}) =
               (* We have to add both the work register and the test register to the
                   conflict set so that we will get different registers. *)
                addNewConflicts(oldCurrent, [destReg workReg, sourceReg testReg])

        |   identify({instr=LockMutable{addr}, current=oldActive, ...}) =
                addSourcesAndDests([sourceReg addr], [], oldActive)

        |   identify({instr=ForwardJumpLabel _, ...}) = ()

        |   identify({instr=UnconditionalForwardJump _, ...}) = ()

        |   identify({instr=ConditionalForwardJump _, current=oldCurrent, ...}) = addNewConflicts(emptySet, setToList oldCurrent)

        |   identify({instr=WordComparison{arg1, arg2, ...}, current=oldActive, ...}) =
                addSourcesAndDests(sourceRegs arg1 @ sourceRegs arg2, [], oldActive)

        |   identify({instr=PushExceptionHandler{workReg, ...}, current=oldCurrent, ...}) =
                addNewConflicts(oldCurrent, [destReg workReg])

        |   identify({instr=PopExceptionHandler{workReg, ...}, current=oldCurrent, ...}) =
            (* Pop an exception handler.  If the result register is not used we must discard it.
               handlerAddr is not actually used.  It's just there as a marker for codeToIcode. *)
                addNewConflicts(oldCurrent, [destReg workReg])

        |   identify({instr=BeginHandler _, ...}) = ()

        |   identify({instr=ReturnResultFromFunction _, ...}) = ()

        |   identify({instr=ArithmeticFunction{oper=SUB, resultReg, operand1, operand2, ...}, current=oldActive, ...}) =
                (* Special case for SUB - we can't use the same register for the result as we do for the second argument
                   so we have to make a conflict between them. *)
                addSourcesAndDestsExt(
                    sourceRegs operand1, sourceRegs operand2, [destReg resultReg], oldActive)

            (* Other arithmetic operations - we can do them either way round but we may have to move an argument into the result
               reg before we do the operation.  That could overwrite a value if the other argument is using the register as
               a memory base.  If it's actually holding the argument we'd use it.  Leave this as a known problem. *)
        |   identify({instr=ArithmeticFunction{resultReg, operand1, operand2, ...}, current=oldActive, ...}) =
                addSourcesAndDests(sourceRegs operand1 @ sourceRegs operand2, [destReg resultReg], oldActive)

        |   identify({instr=TestTagBit{arg, ...}, current=oldActive, ...}) =
                addSourcesAndDests(sourceRegs arg, [], oldActive)

        |   identify({instr=PushValue {arg}, current=oldActive, ...}) =
                addSourcesAndDests(sourceRegs arg, [], oldActive)

        |   identify({instr=ResetStackPtr _, ...}) = ()

        |   identify({instr=TagValue{source, dest}, current=oldActive, ...}) =
                addSourcesAndDests([sourceReg source], [destReg dest], oldActive)

        |   identify({instr=UntagValue{source, dest, ...}, current=oldActive, ...}) =
                addSourcesAndDests(sourceRegs source, [destReg dest], oldActive)

        |   identify({instr=LoadEffectiveAddress{base, index, dest, ...}, current=oldActive, ...}) =
            let
                val bRegs =
                    case base of NONE => [] | SOME bReg => [sourceReg bReg]
                val iRegs = sourceIndex index
            in
                addSourcesAndDests(bRegs @ iRegs, [destReg dest], oldActive)
            end

        |   identify({instr=ShiftOperation{resultReg, operand, shiftAmount=IntegerConstant _ , ...}, current=oldActive, ...}) =
                (* Constant shifts don't require ecx. *)
                addSourcesAndDests(sourceRegs operand, [destReg resultReg], oldActive)

        |   identify({instr=ShiftOperation{resultReg, operand, shiftAmount=RegisterArgument shiftAmount, ...}, current=oldActive, ...}) =
            let
                val shiftReg = sourceReg shiftAmount
                val valueToShift = sourceRegs operand
                val dReg = destReg resultReg
                val () =
                    addSourcesAndDestsExt(shiftReg :: valueToShift, [], [dReg], oldActive)
                (* We need to mark all the currentRegs except shiftReg as conflicting with ecx.
                   We also need to mark the value to shift and the result as excluding that. *)
                val () = addRealRegConflicts(dReg :: (setToList oldActive @ valueToShift), [shiftReg], GenReg ecx)
            in
                ()
            end
        
        |   identify({instr=ShiftOperation _, ...}) = raise InternalError "identify - ShiftOperation"

        |   identify({instr=Multiplication{resultReg, operand1, operand2, ...}, current=oldActive, ...}) =
                addSourcesAndDests(sourceRegs operand1 @ sourceRegs operand2, [destReg resultReg], oldActive)

        |   identify({instr=Division{dividend, divisor, quotient, remainder, ...}, current=oldCurrent, ...}) =
            let
                (* Division is specific as to the registers.  The dividend must be eax, quotient is
                   eax and the remainder is edx.  The divisor must not be in either edx or eax because
                   we need to sign extend the dividend before the division. *)
                val quotReg = destReg quotient and remReg = destReg remainder and diviReg = sourceReg dividend
                val divisorRegs = sourceRegs divisor
                val () =
                    addSourcesAndDestsExt(diviReg :: divisorRegs, [], [quotReg, remReg], oldCurrent)
                (* We need to add conflicts for each of the registers. *)
                val () = addRealRegConflicts(remReg :: divisorRegs @ setToList oldCurrent, [quotReg, diviReg], GenReg eax)
                val () = addRealRegConflicts(quotReg :: divisorRegs @ diviReg :: setToList oldCurrent, [remReg], GenReg edx)
            in
                ()
            end

        |   identify({instr=AtomicExchangeAndAdd{base, source}, current=oldActive, ...}) =
                addSourcesAndDests([sourceReg base, sourceReg source], [], oldActive)

        |   identify({instr=BoxValue{source, dest, ...}, current=oldCurrent, ...}) =
            let
                val dReg = destReg dest
                val sources = [sourceReg source]
                val dests = [dReg]
                val afterRemoveDests = removeDests(dests, oldCurrent)
                val () = addNewConflicts(afterRemoveDests, sources)
                    (* The conflict set includes the value we're going to store.  We need to
                       allocate the memory into the destination register before we can free
                       the source. *)
                val () = addNewConflicts(oldCurrent, dests @ sources)
            in
                ()
            end

        |   identify({instr=CompareByteVectors{vec1Addr, vec2Addr, length, ...}, current=oldActive, ...}) =
            let
                (* We have to use specific registers. *)
                val v1Addr = sourceReg vec1Addr and v2Addr = sourceReg vec2Addr and len = sourceReg length
                val () =
                    addSourcesAndDests([v1Addr, v2Addr, len], [], oldActive)
                val potentialConflicts = v1Addr :: v2Addr :: len :: setToList oldActive
                val () = addRealRegConflicts(potentialConflicts, [v1Addr], GenReg esi)
                val () = addRealRegConflicts(potentialConflicts, [v2Addr], GenReg edi)
                val () = addRealRegConflicts(potentialConflicts, [len], GenReg ecx)
            in
                ()
            end

        |   identify({instr=BlockMove{srcAddr, destAddr, length, ...}, current=oldActive, ...}) =
            let
                val sAddr = sourceReg srcAddr and dAddr = sourceReg destAddr and len = sourceReg length
                val () =
                    addSourcesAndDests([sAddr, dAddr, len], [], oldActive)
                (* We need to add conflicts for each of the registers. *)
                val potentialConflicts = sAddr :: dAddr :: len :: setToList oldActive
                val () = addRealRegConflicts(potentialConflicts, [sAddr], GenReg esi)
                val () = addRealRegConflicts(potentialConflicts, [dAddr], GenReg edi)
                val () = addRealRegConflicts(potentialConflicts, [len], GenReg ecx)
            in
                ()
            end

        |   identify({instr=CompareFloatingPt{arg1, arg2, ...}, current=oldActive, ...}) =
                addSourcesAndDests(sourceRegs arg1 @ sourceRegs arg2, [], oldActive)

        |   identify({instr=X87FPGetCondition{dest, ...}, current=oldCurrent, ...}) =
            let
                (* This can only put the result in rax. *)
                val dReg = destReg dest
                (* Mark all the current registers as conflicting with eax except the destination. *)
                val () = addRealRegConflicts(setToList oldCurrent, [dReg], GenReg eax)
            in
                ()
            end

        |   identify({instr=X87FPArith{resultReg, arg1, arg2, ...}, current=oldActive, ...}) =
                addSourcesAndDests(sourceRegs arg1 @ sourceRegs arg2, [destReg resultReg], oldActive)

        |   identify({instr=X87FPUnaryOps{dest, source, ...}, current=oldActive, ...}) =
                addSourcesAndDests(sourceRegs source, [destReg dest], oldActive)

        |   identify({instr=FloatFixedInt{dest, source}, current=oldActive, ...}) =
                addSourcesAndDests(sourceRegs source, [destReg dest], oldActive)

        |   identify({instr=SSE2FPArith{resultReg, arg1, arg2, ...}, current=oldActive, ...}) =
                addSourcesAndDests(sourceRegs arg1 @ sourceRegs arg2, [destReg resultReg], oldActive)

        val () = List.app identify icode
        
        val conflictState: conflictState vector =
            Vector.tabulate(maxPRegs,
                fn i => {
                    conflicts = Array.sub(regConflicts, i),
                    realConflicts = Array.sub(regRealConflicts, i)
                }
            )
    in
        conflictState
    end

    structure Sharing =
    struct
        type 'reg x86ICode = 'reg x86ICode
        and reg = reg
        and abstract = abstract
        and intSet = intSet
    end
end;
