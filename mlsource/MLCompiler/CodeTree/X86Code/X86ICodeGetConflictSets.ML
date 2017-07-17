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
    structure DEBUG: DEBUGSIG
): X86GETCONFLICTSETSIG =
struct
    open ICODE

    type conflictState =
    {
        conflicts: int list, realConflicts: reg list
    }
    
    exception InternalError = Misc.InternalError
    
    (* Register sets implemented as ordered lists.  These are in
       ascending order because they are used for conflict sets.
       Registers are added to the conflict set as we work back.
       A register that has a long lifetime may have many conflicts and the
       list may get long but typically items will be added to the list
       with numbers that decrease.  N.B.  The size of the conflict set
       does not affect whether a register needs to be spilled. *)
    abstype intSet = IntSet of int list
    with
        val emptySet = IntSet []
        
        fun setToList(IntSet s) = s
        
        local
            fun addItem(i, []) = [i]
            |   addItem(i, hd::tl) =
                    if i = hd then hd :: tl
                    else if i < hd then i :: hd :: tl
                    else hd :: addItem(i, tl)
        in
            fun addToList(items, IntSet toSet) = IntSet(List.foldl(fn (d, l) => addItem(d, l)) toSet items)
            
            fun listToSet items = addToList(items, IntSet [])
        end
        
        local
            fun removeItem(_, []) = []
            |   removeItem(item, hd :: tl) = if item = hd then tl else hd :: removeItem(item, tl)
        in
            fun removeFromSet(item, IntSet fromSet) = IntSet(removeItem(item, fromSet))
        end
        
        local
            (* If the lists are already sorted we can merge them.
               This is an allocation hot-spot.  Avoid recreating the list if possible. *)
            fun mergeLists(listA as a::tlA, listB as b::tlB) =
                if a = b
                then
                let
                    val (tail, tailEq) = mergeLists(tlA, tlB)
                in
                    if PolyML.pointerEq(tlA, tail)
                    then (listA, tailEq)
                    else if PolyML.pointerEq(tlB, tail)
                    then (listB, tailEq)
                    else (a :: tail, false)
                end
                else if a < b
                then
                let
                    val (tail, tailEq) = mergeLists(tlA, listB)
                in
                    if PolyML.pointerEq(tail, tlA) orelse tailEq
                    then (listA, false)
                    else (a :: tail, false)
                end
                else
                let
                    val (tail, tailEq) = mergeLists(listA, tlB)
                in
                    if PolyML.pointerEq(tail, tlB) orelse tailEq
                    then (listB, false)
                    else (b :: tail, false)
                end
            |   mergeLists([], []) = ([], true)
            |   mergeLists([], b) = (b, false)
            |   mergeLists(a, []) = (a, false)
            
        in
            fun union(IntSet setA, IntSet setB) =
            let
                val (result, _) = mergeLists(setA, setB)
            in
                IntSet result
            end
        end
    end
    
    datatype passes = Pass1 | Pass2
    
    (* This function scans the Icode to identify the usage patterns of the pseudo-registers.
       It scans forward building up a list of the registers that have been defined then
       scans backwards building up a list of references.  A register is then "live" if
       it has been both defined and referenced otherwise it is dead.  *)
    fun identifyRegisterState (icode, loopLabels, maxPRegs, passes) =
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
        
        and sourceIndex index =
                    case index of
                        NoMemIndex => []
                    |   MemIndex1 arg => [sourceReg arg]
                    |   MemIndex2 arg => [sourceReg arg]
                    |   MemIndex4 arg => [sourceReg arg]
                    |   MemIndex8 arg => [sourceReg arg]

        (* Any particular argument is only ever a single destination. *)
        fun destReg(PReg(i, _)) = i

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

            fun mergeRegSets(al, bl) =
            let
                val united = union(al, bl)
                (* Each element of the set has to be set to the union. *)
                val () = List.app(fn i => addConflictsTo(i, united)) (setToList united)
            in
                united
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
        
        (* Add sources to the active set.  We also need to mark them as conflicts. *)
        fun addSourcesToActive(sources, existing) =
        let
            (* If we must push this we don't need to include them since they can't conflict. *)
            val filtered = sources
            val () = addNewConflicts(existing, filtered)
            val newSet = addToList(filtered, existing)
        in
            newSet
        end
        
        fun addSourcesAndDestsExt(rest, labels, loopLabels, loopNest, sourcesPre, sourcesPost, dests, oldCurrent) =
        let
            (* Process the rest, and get the set of registers that are in use after
               this instruction. *)
            val (refLabels, resLoopLabels, currentRegs) =
                identify(rest, labels, loopLabels, loopNest)
            (* currentRegs and oldActive are not exactly the same.
               currentRegs is the set of registers that are active AFTER this instruction.
               oldActive is the set of registers that are active DURING this instruction.
               It includes those active after but also destinations and sourcesPost
               whether they are used afterwards or not. *)
            val _ = passes = Pass1 orelse setToList currentRegs = oldCurrent orelse raise InternalError "Whoops"
            (*val currentRegs = listToSet oldActive*)
            (* The current set is the set of registers that are in use AFTER this
               instruction.  So if this the last use of a source register (i.e. working
               backwards we've not seen it before) it won't be in currentRegs.
               The conflict set is the set of registers that need to be different.
               That's going to include this destination.
               The conflict set is the union of the current set and the destinations
               The destinations will be in the current set if they are used but we
               can have destinations, e.g. work registers or quotient/remainder
               that need to have registers allocated that aren't then used. *)
            
            (* The currentRegs set is those registers that have
               actually been used after this but have not yet been given values.
               The resulting newActive set is the original currentRegs set less
               our destinations plus our sources. *)
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
        in
            (refLabels, resLoopLabels, newActive, currentRegs)
        end

        (* Simpler version of addSourcesAndDestsExt which can be used if we don't want the
           currentRegs or need sourcesPost. *)
        and addSourcesAndDests(rest, labels, loopLabels, loopNest, sources, dests, oldCurrent) =
        let
            val (refLabels, resLoopLabels, newActive, _) =
                addSourcesAndDestsExt(rest, labels, loopLabels, loopNest, sources, [], dests, oldCurrent)
        in
            (refLabels, resLoopLabels, newActive)
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

        and identify ([], _, _, _) = ([], [], emptySet)
        
        |   identify({instr=LoadArgument { source, dest, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                (* Moving to a preg, the destination. *)
                addSourcesAndDests(rest, labels, loopLabels, loopNest, sourceRegs source, [destReg dest], oldActive)

        |   identify({instr=StoreArgument { source as RegisterArgument sReg, base, index, kind=MoveByte, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                (* Storing a byte value.  This is messy on X86/32 because we can't use edi or esi as the register
                   to store.  To get round this we reserve ecx as a possible register as with shifts.
                   We don't actually need to use this but it is available if necessary. *)
                if isX64
                then addSourcesAndDests(rest, labels, loopLabels, loopNest, sourceRegs source @ [sourceReg base] @sourceIndex index, [], oldActive)
                else
                let
                    val storeReg = sourceReg sReg
                    val addrRegs = sourceReg base :: sourceIndex index
                    val (refLabels, resLoopLabels, newActive, currentRegs) =
                        addSourcesAndDestsExt(rest, labels, loopLabels, loopNest, storeReg :: addrRegs, [], [], oldActive)
                    val () = addRealRegConflicts(setToList currentRegs @ addrRegs, [storeReg], GenReg ecx)
                in
                    (refLabels, resLoopLabels, newActive)
                end

        |   identify({instr=StoreArgument { source, base, index, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                (* Moving to memory.  The base and index registers are sources not destinations. *)
                addSourcesAndDests(rest, labels, loopLabels, loopNest, sourceRegs source @ [sourceReg base] @ sourceIndex index, [], oldActive)

        |   identify({instr=LoadMemReg { dest, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                addSourcesAndDests(rest, labels, loopLabels, loopNest, [], [destReg dest], oldActive)

        |   identify({instr=ExchangeRegisters _, ...} :: _, _, _, _) =
                (* Should not occur at this stage. *)
                raise InternalError "identify - ExchangeRegisters"

        |   identify({instr=BeginFunction {regArgs}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
            let
                val destRegs = map (destReg o #1) regArgs
            in
                addSourcesAndDests(rest, labels, loopLabels, loopNest, [], destRegs, oldActive)
            end

        |   identify({instr=FunctionCall{regArgs, stackArgs, ...}, ...} :: rest, labels, loopLabels, loopNest) =
            let 
                (* Non-tail-recursive.  Behaves as a normal reference to sources. *)
                val stackSources = List.foldl(fn (arg, srcs) => sourceRegs arg @ srcs) [] stackArgs
                
                fun regSource((arg, _), srcs) = sourceRegs arg @ srcs
                val regSources = List.foldl regSource [] regArgs
                val (refLabels, resLoopLabels, _) = identify(rest, labels, loopLabels, loopNest)
                (* Since the registers have been pushed we don't need to add them to the active set.
                   The active set is just the arguments. *)
                val sources = stackSources @ regSources
                val () = addNewConflicts(emptySet, sources)
            in
                (refLabels, resLoopLabels, listToSet sources)
            end

        |   identify({instr=TailRecursiveCall{regArgs, stackArgs, ...}, ...} :: rest, labels, loopLabels, loopNest) =
            let 
                (* Tail recursive call.  References the argument sources but exits. *)
                val stackSources = List.foldl(fn ({src, ...}, srcs) => sourceRegs src @ srcs) [] stackArgs
                
                fun regSource((arg, _), srcs) = sourceRegs arg @ srcs
                val regSources = List.foldl regSource [] regArgs
                val (refLabels, resLoopLabels, _) = identify(rest, labels, loopLabels, loopNest)
                val active = stackSources @ regSources
            in
                (refLabels, resLoopLabels, addSourcesToActive(active, emptySet))
            end

        |   identify({instr=AllocateMemoryOperation{dest, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                addSourcesAndDests(rest, labels, loopLabels, loopNest, [], [destReg dest], oldActive)

        |   identify({instr=AllocateMemoryVariable{size, dest, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
            let
                val dReg = destReg dest
                val sources = [sourceReg size]
                (* We have to include the sources in the conflict set.  Unlike, say LoadArgument where
                   a register in the source can be reused in the destination, we can't reuse the same
                   register for the result as any of the arguments. *)
                val (refLabels, resLoopLabels, newActive, _) =
                    addSourcesAndDestsExt(rest, labels, loopLabels, loopNest, [], sources, [dReg], oldActive)
            in
                (refLabels, resLoopLabels, newActive)
            end

        |   identify({instr=InitialiseMem{size, addr, init}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
            let
                (* We are going to use rep stosl/q to set the memory.
                   That requires the length to be in ecx, the initialiser to be in eax and
                   the address to be edi. *)
                val aReg = sourceReg addr
                val iReg = sourceReg init
                val sReg = sourceReg size
                val sources = [aReg, iReg, sReg]
                val (refLabels, resLoopLabels, newActive) =
                    addSourcesAndDests(rest, labels, loopLabels, loopNest, sources, [], oldActive)
                (* Since we don't have any destinations we won't have removed anything from the
                   set.  newActive will therefore be currentRegs plus the sources. *)
                val potentialConflicts = setToList newActive
                val () = addRealRegConflicts(potentialConflicts, [aReg], GenReg edi)
                val () = addRealRegConflicts(potentialConflicts, [iReg], GenReg eax)
                val () = addRealRegConflicts(potentialConflicts, [sReg], GenReg ecx)
            in
                (refLabels, resLoopLabels, newActive)
            end

        |   identify({instr=InitialisationComplete, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                (* This is just a marker.  It doesn't actually generate any code. *)
                addSourcesAndDests(rest, labels, loopLabels, loopNest, [], [], oldActive)

        |   identify({instr=StartLoop{loopLabel}, ...} :: rest, labels, loopLabels, loopNest) =
            let
                (* The active set is the environment plus the loop variables. *)
                val (refLabels, resLoopLabels, currentRegs) =
                    identify(rest, labels, loopLabels, loopLabel :: loopNest)
                (* Finally we need to add the sources.  They are used to initialise the
                   loop variables so aren't required within the loop itself. *)
            in
                (* Add the loop entries to the result this.  They can then be added during the next pass. *)
                (refLabels, (loopLabel, currentRegs) :: resLoopLabels, currentRegs)
            end

        |   identify({instr=EndLoop{loopLabel, ...}, current=oldLoopRegs, ...} :: rest, labels, loopLabels, loopNest) =
            let
                (* At the end of the loop we should have the label as the head of the list. *)
                val _ = hd loopNest = loopLabel orelse raise InternalError "identify - loop nesting"
                val removedLabel = tl loopNest
                val (refLabels, resLoopLabels, newActive) =
                    identify(rest, labels, loopLabels, removedLabel)
                (* Establish the conflict between the registers in the current set if necessary.
                   This avoids the need to do it on each JumpLoop.
                   The loop labels have to include ALL the registers in the nested
                   loops. *)
                val loopRegs = mergedLoopRegs(loopLabels, loopNest)
                val _ = passes = Pass1 orelse setToList loopRegs = oldLoopRegs orelse raise InternalError "Whoops"
                val () = addNewConflicts(emptySet, setToList loopRegs)
            in
                (refLabels, resLoopLabels, newActive)
            end

        |   identify({instr=JumpLoop{regArgs, stackArgs, loopLabel, ...}, current=oldCurrent, ...} :: rest, labels, loopLabels, loopNest) =
            let
                val _ = hd loopNest = loopLabel orelse raise InternalError "identify - loop nesting"
                val regSourceAsRegs =
                    List.foldl(fn ((source, _), srcs) => sourceRegs source @ srcs) [] regArgs
                val sourceAsRegs =
                    List.foldl(fn ((source, _), srcs) => sourceRegs source @ srcs) regSourceAsRegs stackArgs
                (* Because this is an unconditional branch the state afterwards is empty. *)
                val (refLabels, resLoopLabels, _) = identify(rest, labels, loopLabels, loopNest)
                (* If this is the second pass we need to add the extra active registers
                   to the references. *)
                val loopRegs = mergedLoopRegs(loopLabels, loopNest)
                val currentRegs = addSourcesToActive(sourceAsRegs, loopRegs)
                val _ = passes = Pass1 orelse setToList currentRegs = oldCurrent orelse raise InternalError "Whoops"
                val destRegs = List.foldl(fn ((_, loopReg), dests) => destReg loopReg :: dests) [] regArgs
                (* Add the loop registers as conflicts just in case they are never used.
                   We should remove unused arguments at a higher level. *)
                val () = addNewConflicts(currentRegs, destRegs)
            in
                (refLabels, resLoopLabels, currentRegs)
            end

        |   identify({instr=RaiseExceptionPacket{packet}, ...} :: rest, labels, loopLabels, loopNest) =
            let
                val sReg = sourceRegs packet
                (* This is an unconditional exit.  The only active register is
                   the exception packet. *)
                val (refLabels, resLoopLabels, _) = identify(rest, labels, loopLabels, loopNest)
            in
                (refLabels, resLoopLabels, listToSet sReg)
            end

        |   identify({instr=ReserveContainer{address, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                addSourcesAndDests(rest, labels, loopLabels, loopNest, [], [destReg address], oldActive)

        |   identify({instr=IndexedCaseOperation{testReg, workReg, cases, ...}, current=oldCurrent, ...} :: rest, labels, loopLabels, loopNest) =
            let
                (* This is rather like an unconditional branch except that we have a
                   list of destination branches rather than a single destination. *)
                val tReg = sourceReg testReg
                (* Add all the labels to the list.  For each branch the starting
                   state is the same as the current state. *)
                val labelsFromCase =
                    List.foldl (fn (lab, labs) => (lab, []) :: labs) labels cases
                val (refLabels, resLoopLabels, _) = identify(rest, labelsFromCase, loopLabels, loopNest)
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
                            (newRefLabs, mergeRegSets(acts, newActive))
                        end ) (refLabels, emptySet) cases
                (* We have to add both the work register and the test register to the
                   conflict set so that we will get different registers. *)
                val () = addNewConflicts(currentRegs, [destReg workReg, tReg])
                val _ = passes = Pass1 orelse setToList currentRegs = oldCurrent orelse raise InternalError "Whoops"
                val newNewActive = addSourcesToActive([tReg], currentRegs)
            in
                (newRefLabels, resLoopLabels, newNewActive)
            end

        |   identify({instr=LockMutable{addr}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                addSourcesAndDests(rest, labels, loopLabels, loopNest, [sourceReg addr], [], oldActive)

        |   identify({instr=ForwardJumpLabel{label, ...}, ...} :: rest, labels, loopLabels, loopNest) =
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
                        identify(rest, newSrcLabels, loopLabels, loopNest)
                |   SOME _ =>
                    let
                        (* Add the sources that applied at the jump to the current set.
                           In particular if we're following an unconditional jump this will
                           set the state. *)
                        val (refLabels, resLoopLabels, newActive) =
                            identify(rest, newSrcLabels, loopLabels, loopNest)
                    in
                        ((label, newActive)::refLabels, resLoopLabels, newActive)
                    end
            end

        |   identify({instr=UnconditionalForwardJump{label}, ...} :: rest, labels, loopLabels, loopNest) =
            let
                (* Add the current state to the label list.  It will be picked up when we
                   encounter the label itself. *)
                val (refLabels, resLoopLabels, _) = identify(rest, (label, []) :: labels, loopLabels, loopNest)
                (* Now find the label in the result list.  This will contain the set of registers
                   that are going to be used in the code after the label. *)
                val (currentRegs, newRefLabels) = findLabel(refLabels, label)
            in
                (newRefLabels, resLoopLabels, currentRegs)
            end

        |   identify({instr=ConditionalForwardJump{label, ...}, ...} :: rest, labels, loopLabels, loopNest) =
            let
                (* The current state applies both immediately after this and also at the
                   label. *)
                (* We may have more than one jump to the same label.  When working forward
                   we use the orginal state but when working back we need to add the
                   references together. *)
                val oldLabel = List.find (fn (thisLabel, _) => thisLabel = label) labels
                val (refLabels, resLoopLabels, newActive) =
                    identify(rest, (label, []) :: labels, loopLabels, loopNest)
                (* Find the registers that were active at the label. *)
                val (activeFromLabel, newRefLabels) = findLabel(refLabels, label)
                val currentRegs = mergeRegSets(newActive, activeFromLabel)
                (* N.B. The current set is after the merge *)
            in
                ((* If the label was already in use when we came here leave the
                       result label in the list. *)
                    if isSome oldLabel then refLabels else newRefLabels, resLoopLabels, currentRegs)
            end

        |   identify({instr=WordComparison{arg1, arg2, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                addSourcesAndDests(rest, labels, loopLabels, loopNest, sourceRegs arg1 @ sourceRegs arg2, [], oldActive)

        |   identify({instr=PushExceptionHandler{workReg, handleStart}, current=oldCurrent, ...} :: rest, labels, loopLabels, loopNest) =
            let
                (* Begin an exception handler.  Similar to a conditional jump.  The current state is the
                   state at the start of the handler.  The handler itself will have been removed.
                   handlerAddr is actually a work register. *)
                (* Find the references that are active from the body. *)
                val (refLabels, resLoopLabels, newActive) =
                    identify(rest, (handleStart, []) :: labels, loopLabels, loopNest)
                (* Find the registers that were active in the handler. *)
                val (activeFromHandler, newRefLabels) = findLabel(refLabels, handleStart)
                val currentRegs = mergeRegSets(newActive, activeFromHandler)
                val _ = passes = Pass1 orelse setToList currentRegs = oldCurrent orelse raise InternalError "Whoops"
                (* The current set is after doing the merge. *)
                val () = addNewConflicts(currentRegs, [destReg workReg])
            in
                (newRefLabels, resLoopLabels, currentRegs)
            end

        |   identify({instr=PopExceptionHandler{workReg, ...}, current=oldCurrent, ...} :: rest, labels, loopLabels, loopNest) =
            (* Pop an exception handler.  If the result register is not used we must discard it.
               handlerAddr is not actually used.  It's just there as a marker for codeToIcode. *)
            let
                val (refLabels, resLoopLabels, currentRegs) =
                    identify(rest,labels, loopLabels, loopNest)
                val _ = passes = Pass1 orelse setToList currentRegs = oldCurrent orelse raise InternalError "Whoops"
                val () = addNewConflicts(currentRegs, [destReg workReg])
            in
                (refLabels, resLoopLabels, currentRegs)
            end

        |   identify({instr=BeginHandler{handleStart, packetReg, ...}, current=oldCurrent, ...} :: rest, labels, loopLabels, loopNest) =
            let
                (* Begin a handler.  This is similar to ForwardJumpLabel. *)
                val (_, newSrcLabels) = findLabel(labels, handleStart)
                (* Add the sources that applied at the jump to the current set.
                   In particular if we're following an unconditional jump this will
                   set the state. *)
                val dReg = destReg packetReg
                val (refLabels, resLoopLabels, currentRegs) =
                    identify(rest, newSrcLabels, loopLabels, loopNest)
                val _ = passes = Pass1 orelse setToList currentRegs = oldCurrent orelse raise InternalError "Whoops"
                (* All the active registers, apart from the exception packet, will have to be pushed
                   to the stack so they are available if the handler is entered. *)
                val newNewActive = removeDest(dReg, currentRegs)
            in
                ((handleStart, newNewActive)::refLabels, resLoopLabels, newNewActive)
            end

        |   identify({instr=ReturnResultFromFunction{resultReg, ...}, ...} :: rest, labels, loopLabels, loopNest) =
            let
                val sRegs = [sourceReg resultReg] (* Should only ever be one. *)
                (* This is an unconditional exit.  The only active register is
                   the result. *)
                val (refLabels, resLoopLabels, _) = identify(rest, labels, loopLabels, loopNest)
            in
                (refLabels, resLoopLabels, listToSet sRegs)
            end

        |   identify({instr=ArithmeticFunction{oper=SUB, resultReg, operand1, operand2, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
            let
                (* Special case for SUB - we can't use the same register for the result as we do for the second argument
                   so we have to make a conflict between them. *)
                val (refLabels, resLoopLabels, newActive, _) =
                    addSourcesAndDestsExt(rest, labels, loopLabels, loopNest,
                        sourceRegs operand1, sourceRegs operand2, [destReg resultReg], oldActive)
            in
                (refLabels, resLoopLabels, newActive)
            end

            (* Other arithmetic operations - we can do them either way round but we may have to move an argument into the result
               reg before we do the operation.  That could overwrite a value if the other argument is using the register as
               a memory base.  If it's actually holding the argument we'd use it.  Leave this as a known problem. *)
        |   identify({instr=ArithmeticFunction{resultReg, operand1, operand2, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                addSourcesAndDests(rest, labels, loopLabels, loopNest, sourceRegs operand1 @ sourceRegs operand2, [destReg resultReg], oldActive)

        |   identify({instr=TestTagBit{arg, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                addSourcesAndDests(rest, labels, loopLabels, loopNest, sourceRegs arg, [], oldActive)

        |   identify({instr=PushValue {arg}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                addSourcesAndDests(rest, labels, loopLabels, loopNest, sourceRegs arg, [], oldActive)

        |   identify({instr=ResetStackPtr _, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                addSourcesAndDests(rest, labels, loopLabels, loopNest, [], [], oldActive)

        |   identify({instr=TagValue{source, dest}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                addSourcesAndDests(rest, labels, loopLabels, loopNest, [sourceReg source], [destReg dest], oldActive)

        |   identify({instr=UntagValue{source, dest, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                addSourcesAndDests(rest, labels, loopLabels, loopNest, sourceRegs source, [destReg dest], oldActive)

        |   identify({instr=LoadEffectiveAddress{base, index, dest, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
            let
                val bRegs =
                    case base of NONE => [] | SOME bReg => [sourceReg bReg]
                val iRegs = sourceIndex index
            in
                addSourcesAndDests(rest, labels, loopLabels, loopNest, bRegs @ iRegs, [destReg dest], oldActive)
            end

        |   identify({instr=ShiftOperation{resultReg, operand, shiftAmount=IntegerConstant _ , ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                (* Constant shifts don't require ecx. *)
                addSourcesAndDests(rest, labels, loopLabels, loopNest, sourceRegs operand, [destReg resultReg], oldActive)

        |   identify({instr=ShiftOperation{resultReg, operand, shiftAmount=RegisterArgument shiftAmount, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
            let
                val shiftReg = sourceReg shiftAmount
                val valueToShift = sourceRegs operand
                val dReg = destReg resultReg
                val (refLabels, resLoopLabels, newActive, currentRegs) =
                    addSourcesAndDestsExt(rest, labels, loopLabels, loopNest, shiftReg :: valueToShift, [], [dReg], oldActive)
                (* We need to mark all the currentRegs except shiftReg as conflicting with ecx.
                   We also need to mark the value to shift and the result as excluding that. *)
                val () = addRealRegConflicts(dReg :: (setToList currentRegs @ valueToShift), [shiftReg], GenReg ecx)
            in
                (refLabels, resLoopLabels, newActive)
            end
        
        |   identify({instr=ShiftOperation _, ...} :: _, _, _, _) = raise InternalError "identify - ShiftOperation"

        |   identify({instr=Multiplication{resultReg, operand1, operand2, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                addSourcesAndDests(rest, labels, loopLabels, loopNest, sourceRegs operand1 @ sourceRegs operand2, [destReg resultReg], oldActive)

        |   identify({instr=Division{dividend, divisor, quotient, remainder, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
            let
                (* Division is specific as to the registers.  The dividend must be eax, quotient is
                   eax and the remainder is edx.  The divisor must not be in either edx or eax because
                   we need to sign extend the dividend before the division. *)
                val quotReg = destReg quotient and remReg = destReg remainder and diviReg = sourceReg dividend
                val divisorRegs = sourceRegs divisor
                val (refLabels, resLoopLabels, newActive, currentRegs) =
                    addSourcesAndDestsExt(rest, labels, loopLabels, loopNest, diviReg :: divisorRegs, [], [quotReg, remReg], oldActive)
                (* We need to add conflicts for each of the registers. *)
                val () = addRealRegConflicts(remReg :: divisorRegs @ setToList currentRegs, [quotReg, diviReg], GenReg eax)
                val () = addRealRegConflicts(quotReg :: divisorRegs @ diviReg :: setToList currentRegs, [remReg], GenReg edx)
            in
                (refLabels, resLoopLabels, newActive)
            end

        |   identify({instr=AtomicExchangeAndAdd{base, source}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                addSourcesAndDests(rest, labels, loopLabels, loopNest, [sourceReg base, sourceReg source], [], oldActive)

        |   identify({instr=BoxValue{source, dest, ...}, current=oldCurrent, ...} :: rest, labels, loopLabels, loopNest) =
            let
                val dReg = destReg dest
                val (refLabels, resLoopLabels, currentRegs) =
                    identify(rest, labels, loopLabels, loopNest)
                val _ = passes = Pass1 orelse setToList currentRegs = oldCurrent orelse raise InternalError "Whoops"
                val sources = [sourceReg source]
                val dests = [dReg]
                val afterRemoveDests = removeDests(dests, currentRegs)
                val newActive = addSourcesToActive(sources, afterRemoveDests)
                    (* The conflict set includes the value we're going to store.  We need to
                       allocate the memory into the destination register before we can free
                       the source. *)
                val () = addNewConflicts(currentRegs, dests @ sources)
            in
                (refLabels, resLoopLabels, newActive)
            end

        |   identify({instr=CompareByteVectors{vec1Addr, vec2Addr, length, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
            let
                (* We have to use specific registers. *)
                val v1Addr = sourceReg vec1Addr and v2Addr = sourceReg vec2Addr and len = sourceReg length
                val (refLabels, resLoopLabels, newActive) =
                    addSourcesAndDests(rest, labels, loopLabels, loopNest, [v1Addr, v2Addr, len], [], oldActive)
                (* We need to add conflicts for each of the registers. *)
                val potentialConflicts = setToList newActive
                val () = addRealRegConflicts(potentialConflicts, [v1Addr], GenReg esi)
                val () = addRealRegConflicts(potentialConflicts, [v2Addr], GenReg edi)
                val () = addRealRegConflicts(potentialConflicts, [len], GenReg ecx)
            in
                (refLabels, resLoopLabels, newActive)
            end

        |   identify({instr=BlockMove{srcAddr, destAddr, length, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
            let
                val sAddr = sourceReg srcAddr and dAddr = sourceReg destAddr and len = sourceReg length
                val (refLabels, resLoopLabels, newActive) =
                    addSourcesAndDests(rest, labels, loopLabels, loopNest, [sAddr, dAddr, len], [], oldActive)
                (* We need to add conflicts for each of the registers. *)
                val potentialConflicts = setToList newActive
                val () = addRealRegConflicts(potentialConflicts, [sAddr], GenReg esi)
                val () = addRealRegConflicts(potentialConflicts, [dAddr], GenReg edi)
                val () = addRealRegConflicts(potentialConflicts, [len], GenReg ecx)
            in
                (refLabels, resLoopLabels, newActive)
            end

        |   identify({instr=CompareFloatingPt{arg1, arg2, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                addSourcesAndDests(rest, labels, loopLabels, loopNest, sourceRegs arg1 @ sourceRegs arg2, [], oldActive)

        |   identify({instr=X87FPGetCondition{dest, ...}, ...} :: rest, labels, loopLabels, loopNest) =
            let
                (* This can only put the result in rax. *)
                val dReg = destReg dest
                val (refLabels, resLoopLabels, currentRegs) =
                    identify(rest, labels, loopLabels, loopNest)
                (* Mark all the current registers as conflicting with eax except the destination. *)
                val () = addRealRegConflicts(setToList currentRegs, [dReg], GenReg eax)
            in
                (refLabels, resLoopLabels, removeDest(dReg, currentRegs))
            end

        |   identify({instr=X87FPArith{resultReg, arg1, arg2, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                addSourcesAndDests(rest, labels, loopLabels, loopNest, sourceRegs arg1 @ sourceRegs arg2, [destReg resultReg], oldActive)

        |   identify({instr=X87FPUnaryOps{dest, source, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                addSourcesAndDests(rest, labels, loopLabels, loopNest, sourceRegs source, [destReg dest], oldActive)

        |   identify({instr=FloatFixedInt{dest, source}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                addSourcesAndDests(rest, labels, loopLabels, loopNest, sourceRegs source, [destReg dest], oldActive)

        |   identify({instr=SSE2FPArith{resultReg, arg1, arg2, ...}, current=oldActive, ...} :: rest, labels, loopLabels, loopNest) =
                addSourcesAndDests(rest, labels, loopLabels, loopNest, sourceRegs arg1 @ sourceRegs arg2, [destReg resultReg], oldActive)

        val ( _, resultLoop, _) = identify(icode, [], loopLabels, [])
        
        val conflictState: conflictState vector =
            Vector.tabulate(maxPRegs,
                fn i => {
                    conflicts = setToList(Array.sub(regConflicts, i)),
                    realConflicts = Array.sub(regRealConflicts, i)
                }
            )
    in
        (resultLoop, conflictState)
    end

    fun getConflictStates(codeAndActivity, maxPRegs) =
       case identifyRegisterState(codeAndActivity, [], maxPRegs, Pass1) of
            (loopLabels, _) =>
            let
                val (_, conflicts2) = identifyRegisterState(codeAndActivity, loopLabels, maxPRegs, Pass2)
            in
                conflicts2
            end

    structure Sharing =
    struct
        type 'reg x86ICode = 'reg x86ICode
        and reg = reg
        and abstract = abstract
    end
end;
