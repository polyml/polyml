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
        active: int, defs: int, refs: int, pushState: pushState
    }

    type conflictState =
    {
        conflicts: int list, realConflicts: reg list
    }
    
    exception InternalError = Misc.InternalError
     
    (* This function scans the Icode to identify the usage patterns of the pseudo-registers.
       It scans forward building up a list of the registers that have been defined then
       scans backwards building up a list of references.  A register is then "live" if
       it has been both defined and referenced otherwise it is dead.  *)
    fun identifyRegisterState (icode, loopLabels, maxPRegs) =
    let
        (* Number of instrs for which this is active. *)
        val regActive = Array.array(maxPRegs, 0)
        (* Number of defining occurrences.  Usually one but may be more if
           the register is returning the result of a conditional/case/handler. *)
        and regDefs = Array.array(maxPRegs, 0)
        (* Number of uses of the register. *)
        and regRefs = Array.array(maxPRegs, 0)
        (* Whether the register must be saved on the stack, must not or could be. *)
        and regPushState = Array.array(maxPRegs, MayPush)
        
        fun incrArray(v, i) = Array.update(v, i, Array.sub(v, i)+1)

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
        
        and sourceReg(PReg(i, _)) = (incrArray(regRefs, i); i)
        
        and sourceIndex index =
                    case index of
                        NoMemIndex => []
                    |   MemIndex1 arg => [sourceReg arg]
                    |   MemIndex2 arg => [sourceReg arg]
                    |   MemIndex4 arg => [sourceReg arg]
                    |   MemIndex8 arg => [sourceReg arg]

        (* Any particular argument is only ever a single destination. *)
        fun destReg(PReg(i, regKind)) =
            (
                case regKind of PRegUntagged => Array.update(regPushState, i, MustNotPush) | _ => ();
                incrArray(regDefs, i);
                i
            )

        (* Find the label and extract the corresponding  Remove this label from
           the list and return the updated list. *)
        fun findLabel([], _) = raise InternalError "findLabel: Missing label"
        |   findLabel((this as (thisLabel, thisActive)) :: tail, label) =
                if thisLabel = label then (thisActive, tail)
                else
                let
                    val (foundActive, foundTail) = findLabel(tail, label)
                in
                    (foundActive, this :: foundTail)
                end

        (* If a label is optional first check that it is there and don't rebuild
           the list if we don't need to. *)
        fun findOptionalLabel(list, label) =
            if List.exists(fn (thisLabel, _) => thisLabel = label) list
            then
            let
                val (found, newList) = findLabel(list, label)
            in
                (SOME found, newList)
            end
            else (NONE, list)

        (* We only process the condition code to see if it is used.  An arithmetic
           operation cannot be removed if its condition code is used even if its
           result is not. *)
        datatype ccState = CCIndeterminate | CCState of ccRef

        (* If we are allocating memory we have to save the current registers if
           they could contain an address.  We mustn't push untagged registers
           and we mustn't push the destination. *)
        fun getSaveSet(currentRegs, dReg) =
        let
            fun getSave i =
                if i = dReg
                then NONE
                else case Array.sub(regPushState, i) of
                    MustNotPush => NONE
                |   MustPush => NONE (* Already on the stack. *)
                |   _ => SOME(PReg(i, PRegGeneral))
        in
            List.mapPartial getSave currentRegs
        end
        
        (* Filter the destination.  We don't need the full power of List.filter since there's
           at most one.  It may not be there if we've never used it. *)
        val removeDest = removeFromSet
        
        fun removeDests(dests, fromSet) = List.foldl (fn (d, l) => removeDest(d, l)) fromSet dests
        
        fun removePushed fromSet = filterSet(fn i => Array.sub(regPushState, i) <> MustPush) fromSet

        (* This needs to be at least as many registers as we have. *)
        val maxRegisters = if isX64 then 20 else 10
        
        (* Add sources to the active set.  We also need to mark them as conflicts. *)
        fun addSourcesToActive(sources, existing) =
        let
            (* If we must push this we don't need to include them since they can't conflict. *)
            val filtered = List.filter(fn i => Array.sub(regPushState, i) <> MustPush) sources
            val newSet = union(listToSet filtered, existing)

            (* If we have too many registers in the active set we select one or
               more to push to the stack.  We can then reduce the size of the
               set.  We'll have to do that eventually although it may be that
               we choose the "wrong" register.  We choose the registers
               that have the longest to go divided by the number of
               references but that doesn't include any uses
               earlier (we're working from the end back).  We need to do this
               to keep the sets bounded otherwise we get quadratic performance. *)
            fun pushExcess regSet =
            if cardinality regSet <= maxRegisters
            then regSet
            else
            let
                fun choose(r :: rlist, best, bestActive) =
                let
                    val pushState = Array.sub(regPushState, r)
                    val rActive =
                        Int.quot(Array.sub(regActive, r), Int.max(Array.sub(regRefs, r), 1))
                in
                    if pushState = MayPush andalso rActive > bestActive
                    then choose(rlist, r, rActive)
                    else choose(rlist, best, bestActive)
                end
                |   choose([], best, _) = best
                val bestChoice = choose (setToList regSet, ~1, ~1)
                val () = Array.update(regPushState, bestChoice, MustPush)
            in
                pushExcess(removeFromSet(bestChoice, regSet))
            end
        in
            pushExcess newSet
        end
        
        fun addSourcesAndDestsExt(instr, rest, labels, loopLabels, loopNest,ccIn, sourcesPre, sourcesPost, dests) =
        let
            (* Process the rest, and get the set of registers that are in use after
               this instruction. *)
            val (tail, refLabels, resLoopLabels, ccUsed, currentRegs) =
                identify(rest, labels, loopLabels, loopNest, ccIn)
            (* The current set is the set of registers that are in use AFTER this
               instruction.  So if this the last use of a source register (i.e. working
               backwards we've not seen it before) it won't be in currentRegs.
               The destinations will be in the current set if they are used but we
               can have destinations, e.g. work registers or quotient/remainder
               that need to have registers allocated that aren't then used. *)
            
            (* The currentRegs set is those registers that have
               actually been used after this but have not yet been given values.
               The resulting newActive set is the original currentRegs set less
               our destinations plus our sources. *)
            val afterRemoveDests = removeDests(dests, currentRegs)
            (* Adding to the set also marks them as conflicts. *)
            val newActive = addSourcesToActive(sourcesPre @ sourcesPost, afterRemoveDests)
            (* Add the active registers to this instruction.  That is the registers that
               need to be active until the instruction is complete.  It includes
               destinations whether they are used or not and also sources but only
               if they need to be active until the instruction is complete.
               Sources whose last use is in this instruction are not
               included.
               TailRecursiveCall and FunctionCall are different.  The active sets
               there are the arguments because we need to know the sets of
               registers that have to be loaded in order to evaluate all the
               arguments. *)
            val active = union(currentRegs, listToSet(dests @ sourcesPost))
            val () = List.app(fn i => incrArray(regActive, i)) (setToList active)
        in
            ({instr=instr, active=active, current=currentRegs} :: tail, refLabels, resLoopLabels, ccUsed, newActive, currentRegs)
        end

        and addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, ccIn, sources, dests) =
        let
            val (instrs, refLabels, resLoopLabels, ccUsed, newActive, _) =
                addSourcesAndDestsExt(instr, rest, labels, loopLabels, loopNest, ccIn, sources, [], dests)
        in
            (instrs, refLabels, resLoopLabels, ccUsed, newActive)
        end

        (* Move, tag and untag instructions can be eliminated if the result is never used. *)
        and eliminateableInstruction(instr, rest, labels, loopLabels, loopNest, ccIn, sourceRegisters, dReg) =
        let
            val (tail, refLabels, resLoopLabels, ccUsed, currentRegs) =
                identify(rest, labels, loopLabels, loopNest, ccIn)
        in
            (* If the destination register has not been used we can remove
               this instruction. Otherwise we have to mark this register as
               conflicting with every other register that needs to have a value. *)
            if Array.sub(regRefs, dReg) <> 0
            then
            let
                val sources = sourceRegs sourceRegisters
                val dests = [dReg]
                val afterRemoveDests = removeDests(dests, currentRegs)
                val newActive = addSourcesToActive(sources, afterRemoveDests)
                val () = List.app(fn i => incrArray(regActive, i)) (setToList currentRegs)
            in
                ({instr=instr, active=currentRegs, current=currentRegs} :: tail, refLabels, resLoopLabels, ccUsed, newActive)
            end
            else (tail, refLabels, resLoopLabels, ccUsed, currentRegs)
        end

        and mergedLoopRegs(loopLabels, loopNest) =
        (* To get the register set for a loop we have to merge the registers for
           all the outer loops as well. *)
        let
            fun getCurrentRegs(loopLab, currRegs) =
                case List.find(fn (lab, _) => lab = loopLab) loopLabels of
                    NONE => currRegs
                |   SOME (_, extraActive) => union(currRegs, extraActive)
        in
            List.foldl getCurrentRegs emptySet loopNest
        end

        and identify ([], _, _, _, _) = ([], [], [], CCIndeterminate, emptySet)
        
        |   identify((instr as LoadArgument { source, dest, ...}) :: rest, labels, loopLabels, loopNest, ccIn) =
                (* Moving to a preg, the destination. *)
                eliminateableInstruction(instr, rest, labels, loopLabels, loopNest, ccIn, source, destReg dest)

        |   identify((instr as StoreArgument { source as RegisterArgument sReg, base, index, kind=MoveByte, ...}) :: rest, labels, loopLabels, loopNest, ccIn) =
                (* Storing a byte value.  This is messy on X86/32 because we can't use edi or esi as the register
                   to store.  To get round this we reserve ecx as a possible register as with shifts.
                   We don't actually need to use this but it is available if necessary. *)
                if isX64
                then addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, ccIn, sourceRegs source @ [sourceReg base] @sourceIndex index, [])
                else
                let
                    val storeReg = sourceReg sReg
                    val addrRegs = sourceReg base :: sourceIndex index
                    val (instrs, refLabels, resLoopLabels, ccUsed, newActive) =
                        addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, ccIn, storeReg :: addrRegs, [])
                in
                    (instrs, refLabels, resLoopLabels, ccUsed, newActive)
                end

        |   identify((instr as StoreArgument { source, base, index, ...}) :: rest, labels, loopLabels, loopNest, ccIn) =
                (* Moving to memory.  The base and index registers are sources not destinations. *)
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, ccIn, sourceRegs source @ [sourceReg base] @ sourceIndex index, [])

        |   identify((instr as LoadMemReg { dest, ...}) :: rest, labels, loopLabels, loopNest, ccIn) =
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, ccIn, [], [destReg dest])

        |   identify(ExchangeRegisters _ :: _, _, _, _, _) =
                (* Should not occur at this stage. *)
                raise InternalError "identify - ExchangeRegisters"

        |   identify((instr as BeginFunction {regArgs}) :: rest, labels, loopLabels, loopNest, ccIn) =
            let
                val destRegs = map (destReg o #1) regArgs
            in
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, ccIn, [], destRegs)
            end

        |   identify((instr as FunctionCall{regArgs, stackArgs, dest, ...}) :: rest, labels, loopLabels, loopNest, ccIn) =
            let 
                (* Non-tail-recursive.  Behaves as a normal reference to sources. *)
                val stackSources = List.foldl(fn (arg, srcs) => sourceRegs arg @ srcs) [] stackArgs
                
                fun regSource((arg, _), srcs) = sourceRegs arg @ srcs
                val regSources = List.foldl regSource [] regArgs
                val dReg = destReg dest
                val (tail, refLabels, resLoopLabels, _, currentRegs) =
                    identify(rest, labels, loopLabels, loopNest, ccIn)
                (* The push set, though, excludes the result register. *)
                val () =
                    List.app(
                        fn i => (if i = dReg then () else Array.update(regPushState, i, MustPush))) (setToList currentRegs)
                (* Since the registers have been pushed we don't need to add them to the active set.
                   The active set is just the arguments. *)
                val sources = stackSources @ regSources
            in
                ({instr=instr, active=listToSet sources, current=currentRegs} :: tail, refLabels, resLoopLabels, CCIndeterminate, removePushed(listToSet sources))
            end

        |   identify((instr as TailRecursiveCall{regArgs, stackArgs, ...}) :: rest, labels, loopLabels, loopNest, ccIn) =
            let 
                (* Tail recursive call.  References the argument sources but exits. *)
                val stackSources = List.foldl(fn ({src, ...}, srcs) => sourceRegs src @ srcs) [] stackArgs
                
                fun regSource((arg, _), srcs) = sourceRegs arg @ srcs
                val regSources = List.foldl regSource [] regArgs
                val (tail, refLabels, resLoopLabels, _, _) = identify(rest, labels, loopLabels, loopNest, ccIn)
                val active = stackSources @ regSources
            in
                ({instr=instr, active=listToSet active, current=emptySet} :: tail, refLabels, resLoopLabels, CCIndeterminate, addSourcesToActive(active, emptySet))
            end

        |   identify((instr as AllocateMemoryOperation{size, flags, dest, saveRegs=_}) :: rest, labels, loopLabels, loopNest, _) =
            let
                val dReg = destReg dest
                val (instrs, refLabels, resLoopLabels, ccUsed, newActive, currentRegs) =
                    addSourcesAndDestsExt(instr, rest, labels, loopLabels, loopNest, CCIndeterminate, [], [], [dReg])
                val saveRegs = getSaveSet(setToList currentRegs, dReg)
                val allocInstr = AllocateMemoryOperation{size=size, flags=flags, dest=dest, saveRegs=saveRegs}
                val newInstrs =
                    case instrs of
                        {active, current, ...} :: tail => {instr=allocInstr, active=active, current=current} :: tail
                    |   _ => raise Empty
            in
                (newInstrs, refLabels, resLoopLabels, ccUsed, newActive)
            end

        |   identify((instr as AllocateMemoryVariable{size, dest, ...}) :: rest, labels, loopLabels, loopNest, _) =
            let
                val dReg = destReg dest
                val sources = [sourceReg size]
                (* We have to include the sources in the conflict set.  Unlike, say LoadArgument where
                   a register in the source can be reused in the destination, we can't reuse the same
                   register for the result as any of the arguments. *)
                val (instrs, refLabels, resLoopLabels, ccUsed, newActive, currentRegs) =
                    addSourcesAndDestsExt(instr, rest, labels, loopLabels, loopNest, CCIndeterminate, [], sources, [dReg])
                (* We need to save the registers if we can't do an allocation. *)
                val saveRegs = getSaveSet(setToList currentRegs, dReg)
                val allocInstr =
                    AllocateMemoryVariable{size=size, dest=dest, saveRegs=saveRegs}
                val newInstrs =
                    case instrs of
                        {active, current, ...} :: tail => {instr=allocInstr, active=active, current=current} :: tail
                    |   _ => raise Empty
            in
                (newInstrs, refLabels, resLoopLabels, ccUsed, newActive)
            end

        |   identify((instr as InitialiseMem{size, addr, init}) :: rest, labels, loopLabels, loopNest, _) =
            let
                (* We are going to use rep stosl/q to set the memory.
                   That requires the length to be in ecx, the initialiser to be in eax and
                   the address to be edi. *)
                val aReg = sourceReg addr
                val iReg = sourceReg init
                val sReg = sourceReg size
                val sources = [aReg, iReg, sReg]
                val (instrs, refLabels, resLoopLabels, ccUsed, newActive) =
                    addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, CCIndeterminate, sources, [])
                (* Since we don't have any destinations we won't have removed anything from the
                   set.  newActive will therefore be currentRegs plus the sources. *)
            in
                (instrs, refLabels, resLoopLabels, ccUsed, newActive)
            end

        |   identify((instr as InitialisationComplete) :: rest, labels, loopLabels, loopNest, ccIn) =
                (* This is just a marker.  It doesn't actually generate any code. *)
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, ccIn, [], [])

        |   identify((instr as StartLoop{loopLabel}) :: rest, labels, loopLabels, loopNest, ccIn) =
            let
                (* The active set is the environment plus the loop variables. *)
                val (tail, refLabels, resLoopLabels, _, currentRegs) =
                    identify(rest, labels, loopLabels, loopLabel :: loopNest, ccIn)
                (* Finally we need to add the sources.  They are used to initialise the
                   loop variables so aren't required within the loop itself. *)
                val () = List.app(fn i => incrArray(regActive, i)) (setToList currentRegs)
            in
                (* Add the loop entries to the result this.  They can then be added during the next pass. *)
                ({instr=instr, active=currentRegs, current=currentRegs} :: tail, refLabels,
                        (loopLabel, currentRegs) :: resLoopLabels, CCIndeterminate, currentRegs)
            end

        |   identify(EndLoop{loopLabel, ...} :: rest, labels, loopLabels, loopNest, ccIn) =
            let
                (* At the end of the loop we should have the label as the head of the list. *)
                val _ = hd loopNest = loopLabel orelse raise InternalError "identify - loop nesting"
                val removedLabel = tl loopNest
                val (tail, refLabels, resLoopLabels, _, newActive) =
                    identify(rest, labels, loopLabels, removedLabel, ccIn)
                val loopRegs = mergedLoopRegs(loopLabels, loopNest)
                (* We have to pass these to the next stage.  They are treated as part of the active
                   set in each JumpLoop.  We assume that when we come to allocate real registers
                   anything in the active set has already been allocated (working back from the end).
                   We have to ensure that is the case by allocating them at the EndLoop.  Then any
                   new registers we allocate will be correctly found as conflicts. *)
                val staticRegs = List.map (fn r => PReg(r, PRegGeneral)) (setToList loopRegs)
            in
                (* We set the current to the loopRegs since we need that for the conflict sets. *)
                ({instr=EndLoop{loopLabel=loopLabel, staticRegs=staticRegs}, active=newActive, current=loopRegs} :: tail,
                        refLabels, resLoopLabels, CCIndeterminate, newActive)
            end

        |   identify(JumpLoop{regArgs, stackArgs, loopLabel, checkInterrupt, stackAdjust} :: rest, labels, loopLabels, loopNest, ccIn) =
            let
                val _ = hd loopNest = loopLabel orelse raise InternalError "identify - loop nesting"
                val regSourceAsRegs =
                    List.foldl(fn ((source, _), srcs) => sourceRegs source @ srcs) [] regArgs
                val sourceAsRegs =
                    List.foldl(fn ((source, _), srcs) => sourceRegs source @ srcs) regSourceAsRegs stackArgs
                (* Because this is an unconditional branch the state afterwards is empty. *)
                val (tail, refLabels, resLoopLabels, _, _) = identify(rest, labels, loopLabels, loopNest, ccIn)
                (* If this is the second pass we need to add the extra active registers
                   to the references. *)
                val loopRegs = removePushed(mergedLoopRegs(loopLabels, loopNest))
                val currentRegs = addSourcesToActive(sourceAsRegs, loopRegs)
                val () = List.app(fn i => incrArray(regActive, i)) (setToList currentRegs)
                (* If we have to check for interrupts we must preserve registers across
                   the RTS call. *)
                fun getSave i =
                    case Array.sub(regPushState, i) of
                        MustNotPush => NONE
                    |   MustPush => NONE (* Already on the stack. *)
                    |   _ => SOME(PReg(i, PRegGeneral))
                val check =
                    case checkInterrupt of
                        NONE => NONE
                    |   SOME _ => SOME(List.mapPartial getSave (setToList currentRegs))
                val jumpInstr =
                    JumpLoop{regArgs=regArgs, stackArgs=stackArgs, loopLabel=loopLabel, stackAdjust=stackAdjust, checkInterrupt=check}
            in
                ({instr=jumpInstr, active=emptySet, current=currentRegs} :: tail, refLabels, resLoopLabels, CCIndeterminate, currentRegs)
            end

        |   identify((instr as RaiseExceptionPacket{packet}) :: rest, labels, loopLabels, loopNest, ccIn) =
            let
                val sReg = sourceRegs packet
                (* This is an unconditional exit.  The only active register is
                   the exception packet. *)
                val (tail, refLabels, resLoopLabels, _, currentRegs) = identify(rest, labels, loopLabels, loopNest, ccIn)
            in
                ({instr=instr, active=emptySet, current=currentRegs} :: tail, refLabels, resLoopLabels, CCIndeterminate, removePushed(listToSet sReg))
            end

        |   identify((instr as ReserveContainer{address, ...}) :: rest, labels, loopLabels, loopNest, ccIn) =
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, ccIn, [], [destReg address])

        |   identify((instr as IndexedCaseOperation{testReg, workReg, cases, ...}) :: rest, labels, loopLabels, loopNest, ccIn) =
            let
                (* This is rather like an unconditional branch except that we have a
                   list of destination branches rather than a single destination. *)
                val tReg = sourceReg testReg
                (* Add all the labels to the list.  For each branch the starting
                   state is the same as the current state. *)
                val labelsFromCase =
                    List.foldl (fn (lab, labs) => (lab, []) :: labs) labels cases
                val (tail, refLabels, resLoopLabels, _, _) = identify(rest, labelsFromCase, loopLabels, loopNest, ccIn)
                (* Now extract the case labels from the result list.  This is all the
                   references from all various cases.  We need the union of these.
                   i.e. if we need a register to be active on any of the branches it
                   must be active before this indexed case. *)
                val (newRefLabels, currentRegs) =
                    List.foldl (
                        fn (caseLab, (refLabs, acts)) =>
                        let
                            val (newActive, newRefLabs) = findLabel(refLabs, caseLab)
                        in
                            (newRefLabs, union(acts, removePushed newActive))
                        end ) (refLabels, emptySet) cases
                val newNewActive = addSourcesToActive([tReg], currentRegs)
                val activeForInstr = union(currentRegs, listToSet [destReg workReg, tReg])
                val () = List.app(fn i => incrArray(regActive, i)) (setToList activeForInstr)
            in
                ({instr=instr, active=activeForInstr, current=currentRegs} :: tail, newRefLabels, resLoopLabels, CCIndeterminate, newNewActive)
            end

        |   identify((instr as LockMutable{addr}) :: rest, labels, loopLabels, loopNest, ccIn) =
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, ccIn, [sourceReg addr], [])

        |   identify((ForwardJumpLabel{label, result}) :: rest, labels, loopLabels, loopNest, ccIn) =
            let
                (* Find the set of active registers when we jumped here. 
                   The label may not have been used. That could happen if we have a
                   "drop-through" case in an andalso/orelse or if we didn't generate a
                   jump because we raised an exception or made a tail-jump.  *)
                val (optSrcs, newSrcLabels) = findOptionalLabel(labels, label)
            in
                case optSrcs of
                    NONE =>
                        (* If this was never used delete this instruction. *)
                        identify(rest, newSrcLabels, loopLabels, loopNest, ccIn)
                |   SOME _ =>
                    let
                        (* Add the sources that applied at the jump to the current set.
                           In particular if we're following an unconditional jump this will
                           set the state. *)
                        val (tail, refLabels, resLoopLabels, _, newActive) =
                            identify(rest, newSrcLabels, loopLabels, loopNest, ccIn)
                        (* We include "references" in both the immediate result and as a label
                           entry.  The registers are live in the immediately preceding code, if
                           there is any, and also at the jump to this label. *)
                        (* If the result is not actually used we don't need to pass it. *)
                        val newResult =
                            case result of
                                NONE => NONE
                            |   SOME(r as PReg(n, _)) => if Array.sub(regRefs, n) > 0 then SOME r else NONE 
                        val newInstr = ForwardJumpLabel{label=label, result=newResult}
                    in
                        ({instr=newInstr, active=newActive, current=newActive} :: tail,
                                (label, newActive)::refLabels, resLoopLabels, CCIndeterminate, newActive)
                    end
            end

        |   identify((instr as UnconditionalForwardJump{label}) :: rest, labels, loopLabels, loopNest, ccIn) =
            let
                (* Add the current state to the label list.  It will be picked up when we
                   encounter the label itself. *)
                val (tail, refLabels, resLoopLabels, _, _) = identify(rest, (label, []) :: labels, loopLabels, loopNest, ccIn)
                (* Now find the label in the result list.  This will contain the set of registers
                   that are going to be used in the code after the label. *)
                val (currentRegs, newRefLabels) = findLabel(refLabels, label)
            in
                ({instr=instr, active=emptySet, current=currentRegs} :: tail, newRefLabels, resLoopLabels, CCIndeterminate, removePushed currentRegs)
            end

        |   identify((instr as ConditionalForwardJump{label, ccRef, ...}) :: rest, labels, loopLabels, loopNest, ccIn) =
            let
                (* The current state applies both immediately after this and also at the
                   label. *)
                (* We may have more than one jump to the same label.  When working forward
                   we use the orginal state but when working back we need to add the
                   references together. *)
                (* Check that we've defined the correct cc. *)
                val _ =
                    (case ccIn of CCState s => s = ccRef | CCIndeterminate => false)
                        orelse raise InternalError "identify - cc mismatch"
                val oldLabel = List.find (fn (thisLabel, _) => thisLabel = label) labels
                val (tail, refLabels, resLoopLabels, _, newActive) =
                    identify(rest, (label, []) :: labels, loopLabels, loopNest, ccIn)
                (* Find the registers that were active at the label. *)
                val (activeFromLabel, newRefLabels) = findLabel(refLabels, label)
                val currentRegs = union(newActive, removePushed activeFromLabel)
                (* N.B. The current set is after the merge *)
                val () = List.app(fn i => incrArray(regActive, i)) (setToList newActive)
            in
                ({instr=instr, active=newActive, current=currentRegs} :: tail,
                    (* If the label was already in use when we came here leave the
                       result label in the list. *)
                    if isSome oldLabel then refLabels else newRefLabels, resLoopLabels, CCState ccRef, currentRegs)
            end

        |   identify((instr as WordComparison{arg1, arg2, ccRef, ...}) :: rest, labels, loopLabels, loopNest, _) =
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, CCState ccRef, sourceRegs arg1 @ sourceRegs arg2, [])

        |   identify((instr as PushExceptionHandler{handleStart, ...}) :: rest, labels, loopLabels, loopNest, ccIn) =
            let
                (* Begin an exception handler.  Similar to a conditional jump.  The current state is the
                   state at the start of the handler.  The handler itself will have been removed.
                   handlerAddr is actually a work register. *)
                (* Find the references that are active from the body. *)
                val (tail, refLabels, resLoopLabels, _, newActive) =
                    identify(rest, (handleStart, []) :: labels, loopLabels, loopNest, ccIn)
                (* Find the registers that were active in the handler. *)
                val (activeFromHandler, newRefLabels) = findLabel(refLabels, handleStart)
                val currentRegs = union(newActive, removePushed activeFromHandler)
                val () = List.app(fn i => incrArray(regActive, i)) (setToList newActive)
            in
                ({instr=instr, active=newActive, current=currentRegs} :: tail, newRefLabels, resLoopLabels, CCIndeterminate, currentRegs)
            end

        |   identify((PopExceptionHandler{ resultReg, workReg}) :: rest, labels, loopLabels, loopNest, ccIn) =
            (* Pop an exception handler.  If the result register is not used we must discard it.
               handlerAddr is not actually used.  It's just there as a marker for codeToIcode. *)
            let
                val (tail, refLabels, resLoopLabels, ccUsed, currentRegs) =
                    identify(rest,labels, loopLabels, loopNest, ccIn)
                val newResultReg =
                    case resultReg of
                        NONE => NONE
                    |   SOME(r as PReg(n, _)) => if Array.sub(regRefs, n) > 0 then SOME r else NONE
                val newInstr = PopExceptionHandler{resultReg=newResultReg, workReg=workReg}
                val active = union(currentRegs, listToSet[destReg workReg])
                val () = List.app(fn i => incrArray(regActive, i)) (setToList active)
            in
                ({instr=newInstr, active=active, current=currentRegs} :: tail, refLabels, resLoopLabels, ccUsed, currentRegs)
            end

        |   identify((instr as BeginHandler{handleStart, workReg=_, packetReg}) :: rest, labels, loopLabels, loopNest, ccIn) =
            let
                (* Begin a handler.  This is similar to ForwardJumpLabel. *)
                val (_, newSrcLabels) = findLabel(labels, handleStart)
                (* Add the sources that applied at the jump to the current set.
                   In particular if we're following an unconditional jump this will
                   set the state. *)
                val dReg = destReg packetReg
                val (tail, refLabels, resLoopLabels, _, currentRegs) =
                    identify(rest, newSrcLabels, loopLabels, loopNest, ccIn)
                (* All the active registers, apart from the exception packet, will have to be pushed
                   to the stack so they are available if the handler is entered. *)
                val () =
                    List.app(
                        fn i => (if i = dReg then () else Array.update(regPushState, i, MustPush))) (setToList currentRegs)
                val newNewActive = removeDest(dReg, currentRegs)
                val active = union(currentRegs, listToSet [dReg])
                val () = List.app(fn i => incrArray(regActive, i)) (setToList active)
            in
                ({instr=instr, active=active, current=currentRegs} :: tail,
                    (handleStart, newNewActive)::refLabels, resLoopLabels, CCIndeterminate, newNewActive)
            end

        |   identify((instr as ReturnResultFromFunction{resultReg, ...}) :: rest, labels, loopLabels, loopNest, ccIn) =
            let
                val sRegs = [sourceReg resultReg] (* Should only ever be one. *)
                (* This is an unconditional exit.  The only active register is
                   the result. *)
                val (tail, refLabels, resLoopLabels, _, currentRegs) = identify(rest, labels, loopLabels, loopNest, ccIn)
            in
                ({instr=instr, active=listToSet [destReg resultReg], current=currentRegs} :: tail, refLabels, resLoopLabels, CCIndeterminate, listToSet sRegs)
            end

        |   identify((instr as ArithmeticFunction{oper=SUB, resultReg, operand1, operand2, ccRef, ...}) :: rest, labels, loopLabels, loopNest, _) =
            let
                (* Special case for SUB - we can't use the same register for the result as we do for the second argument
                   so we have to make a conflict between them. *)
                val (instrs, refLabels, resLoopLabels, ccUsed, newActive, _) =
                    addSourcesAndDestsExt(instr, rest, labels, loopLabels, loopNest, CCState ccRef,
                        sourceRegs operand1, sourceRegs operand2, [destReg resultReg])
            in
                (instrs, refLabels, resLoopLabels, ccUsed, newActive)
            end

            (* Other arithmetic operations - we can do them either way round but we may have to move an argument into the result
               reg before we do the operation.  That could overwrite a value if the other argument is using the register as
               a memory base.  If it's actually holding the argument we'd use it.  Leave this as a known problem. *)
        |   identify((instr as ArithmeticFunction{resultReg, operand1, operand2, ccRef, ...}) :: rest, labels, loopLabels, loopNest, _) =
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, CCState ccRef, sourceRegs operand1 @ sourceRegs operand2, [destReg resultReg])

        |   identify((instr as TestTagBit{arg, ccRef, ...}) :: rest, labels, loopLabels, loopNest, _) =
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, CCState ccRef, sourceRegs arg, [])

        |   identify((instr as PushValue {arg}) :: rest, labels, loopLabels, loopNest, ccIn) =
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, ccIn, sourceRegs arg, [])

        |   identify((instr as ResetStackPtr _) :: rest, labels, loopLabels, loopNest, ccIn) =
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, ccIn, [], [])

        |   identify((instr as TagValue{source, dest}) :: rest, labels, loopLabels, loopNest, ccIn) =
                eliminateableInstruction(instr, rest, labels, loopLabels, loopNest, ccIn, RegisterArgument source, destReg dest)

        |   identify((instr as UntagValue{source, dest, ...}) :: rest, labels, loopLabels, loopNest, ccIn) =
                eliminateableInstruction(instr, rest, labels, loopLabels, loopNest, ccIn, source, destReg dest)

        |   identify((instr as LoadEffectiveAddress{base, index, dest, ...}) :: rest, labels, loopLabels, loopNest, ccIn) =
            let
                val bRegs =
                    case base of NONE => [] | SOME bReg => [sourceReg bReg]
                val iRegs = sourceIndex index
            in
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, ccIn, bRegs @ iRegs, [destReg dest])
            end

        |   identify((instr as ShiftOperation{resultReg, operand, shiftAmount=IntegerConstant _ , ccRef, ...}) :: rest, labels, loopLabels, loopNest, _) =
                (* Constant shifts don't require ecx. *)
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, CCState ccRef, sourceRegs operand, [destReg resultReg])

        |   identify((instr as ShiftOperation{resultReg, operand, shiftAmount=RegisterArgument shiftAmount, ccRef, ...}) :: rest, labels, loopLabels, loopNest, _) =
            let
                val shiftReg = sourceReg shiftAmount
                val valueToShift = sourceRegs operand
                val dReg = destReg resultReg
            in
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, CCState ccRef, shiftReg :: valueToShift, [dReg])
            end
        
        |   identify(ShiftOperation _ :: _, _, _, _, _) = raise InternalError "identify - ShiftOperation"

        |   identify((instr as Multiplication{resultReg, operand1, operand2, ccRef, ...}) :: rest, labels, loopLabels, loopNest, _) =
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, CCState ccRef, sourceRegs operand1 @ sourceRegs operand2, [destReg resultReg])

        |   identify((instr as Division{dividend, divisor, quotient, remainder, ...}) :: rest, labels, loopLabels, loopNest, _) =
            let
                (* Division is specific as to the registers.  The dividend must be eax, quotient is
                   eax and the remainder is edx.  The divisor must not be in either edx or eax because
                   we need to sign extend the dividend before the division. *)
                val quotReg = destReg quotient and remReg = destReg remainder and diviReg = sourceReg dividend
                val divisorRegs = sourceRegs divisor
            in
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, CCIndeterminate, diviReg :: divisorRegs, [quotReg, remReg])
            end

        |   identify((instr as AtomicExchangeAndAdd{base, source}) :: rest, labels, loopLabels, loopNest, ccIn) =
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, ccIn, [sourceReg base, sourceReg source], [])

        |   identify(BoxValue{source, dest, boxKind, saveRegs=_} :: rest, labels, loopLabels, loopNest, ccIn) =
            let
                val dReg = destReg dest
                val (tail, refLabels, resLoopLabels, ccUsed, currentRegs) =
                    identify(rest, labels, loopLabels, loopNest, ccIn)
                (* If the result is not used we can eliminate this instruction but because it
                   allocates we have to compute the set of registers that need to be preserved. *)
            in
                if Array.sub(regRefs, dReg) <> 0
                then
                let
                    val sources = [sourceReg source]
                    val dests = [dReg]
                    val afterRemoveDests = removeDests(dests, currentRegs)
                    val newActive = addSourcesToActive(sources, afterRemoveDests)
                    (* The save set is the other registers that are active.  We don't save
                       the destination nor the value we're boxing. *)
                    val saveRegs = getSaveSet(setToList currentRegs, dReg)
                    val boxInstr = BoxValue{source=source, dest=dest, boxKind=boxKind, saveRegs=saveRegs}
                    val active = union(currentRegs, listToSet(dests @ sources))
                    val () = List.app(fn i => incrArray(regActive, i)) (setToList active)
                in
                    ({instr=boxInstr, active=active, current=currentRegs} :: tail, refLabels, resLoopLabels, ccUsed, newActive)
                end
                else (tail, refLabels, resLoopLabels, ccUsed, currentRegs)
            end

        |   identify((instr as CompareByteVectors{vec1Addr, vec2Addr, length, ccRef}) :: rest, labels, loopLabels, loopNest, _) =
            let
                (* We have to use specific registers. *)
                val v1Addr = sourceReg vec1Addr and v2Addr = sourceReg vec2Addr and len = sourceReg length
            in
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, CCState ccRef, [v1Addr, v2Addr, len], [])
            end

        |   identify((instr as BlockMove{srcAddr, destAddr, length, ...}) :: rest, labels, loopLabels, loopNest, _) =
            let
                val sAddr = sourceReg srcAddr and dAddr = sourceReg destAddr and len = sourceReg length
            in
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, CCIndeterminate, [sAddr, dAddr, len], [])
            end

        |   identify((instr as CompareFloatingPt{arg1, arg2, ccRef, ...}) :: rest, labels, loopLabels, loopNest, _) =
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, CCState ccRef, sourceRegs arg1 @ sourceRegs arg2, [])

        |   identify((instr as X87FPGetCondition{dest, ccRef}) :: rest, labels, loopLabels, loopNest, ccIn) =
            let
                (* This can only put the result in rax. *)
                val dReg = destReg dest
                (* Check that we've defined the correct cc. *)
                val _ =
                    (case ccIn of CCState s => s = ccRef | CCIndeterminate => false)
                        orelse raise InternalError "identify - cc mismatch"
                val (tail, refLabels, resLoopLabels, _, currentRegs) =
                    identify(rest, labels, loopLabels, loopNest, ccIn)
                val active = union(currentRegs, listToSet[dReg])
                val () = List.app(fn i => incrArray(regActive, i)) (setToList active)
            in
                ({instr=instr, active=active, current=currentRegs} :: tail, refLabels, resLoopLabels, CCState ccRef, removeDest(dReg, currentRegs))
            end

        |   identify((instr as X87FPArith{resultReg, arg1, arg2, ...}) :: rest, labels, loopLabels, loopNest, ccIn) =
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, ccIn, sourceRegs arg1 @ sourceRegs arg2, [destReg resultReg])

        |   identify((instr as X87FPUnaryOps{dest, source, ...}) :: rest, labels, loopLabels, loopNest, ccIn) =
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, ccIn, sourceRegs source, [destReg dest])

        |   identify((instr as FloatFixedInt{dest, source}) :: rest, labels, loopLabels, loopNest, ccIn) =
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, ccIn, sourceRegs source, [destReg dest])

        |   identify((instr as SSE2FPArith{resultReg, arg1, arg2, ...}) :: rest, labels, loopLabels, loopNest, ccIn) =
                addSourcesAndDests(instr, rest, labels, loopLabels, loopNest, ccIn, sourceRegs arg1 @ sourceRegs arg2, [destReg resultReg])

        val (decorated, _, resultLoop, _, _) = identify(icode, [], loopLabels, [], CCIndeterminate)
        
        val registerState: regState vector =
            Vector.tabulate(maxPRegs,
                fn i => {
                    active = Array.sub(regActive, i),
                    defs = Array.sub(regDefs, i),
                    refs = Array.sub(regRefs, i),
                    pushState = Array.sub(regPushState, i)
                }
            )
     in
        (decorated, resultLoop, registerState)
    end

    fun identifyRegisters { icode, maxPRegs} =
        (* Process the list once.  If we have one or more loops in it we have to
           reprocess it with the loop information we found in the first pass. *)
        case identifyRegisterState(icode, [], maxPRegs) of
            (codePass1, [], regState1) => (codePass1, regState1)
        |   (_, loopLabels, _) =>
            let
                val (codePass2, _, regState2) = identifyRegisterState(icode, loopLabels, maxPRegs)
            in
                (codePass2, regState2)
            end

    structure Sharing =
    struct
        type 'reg x86ICode = 'reg x86ICode
        and reg = reg
        and pushState = pushState
        and abstract = abstract
        and intSet = intSet
    end
end;
