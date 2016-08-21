(*
    Copyright (c) 2016 David C.J. Matthews

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
): X86IDENTIFYREFSSIG =
struct
    open ICODE
    
    datatype pushState = MustPush | MustNotPush | MayPush

    type regState =
    { 
        active: int, defs: int, refs: int, pushState: pushState,
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
        (* Other registers that conflict with this i.e. cannot share the same
           real register. *)
        and regConflicts = Array.array(maxPRegs, []: word list)
        (* Real registers that cannot be used for this because are needed for
           an instruction e.g. shift or block move, that requires these. *)
        and regRealConflicts = Array.array(maxPRegs, []: reg list)
        
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

        (* The sets of registers are represented by lists of bits.  This is more
           efficient than BoolVector since it allows union and intersection to be
           implemented as word operations. *)
        fun listToSet s =
        let
            fun toSet([], _) = []
            |   toSet(l, start) =
                let
                    val max = start+Word.wordSize
                    val (less, grtr) = List.partition(fn i => i < max) l
                    val this =
                        List.foldl (fn (i, w) => Word.orb(w, Word.<<(0w1, Word.fromInt(i-start)))) 0w0 less
                in
                    this :: toSet(grtr, max)
                end
        in
            toSet(s, 0)
        end
        
        fun intersect(r1::t1, r2::t2) = Word.andb(r1, r2) :: intersect(t1, t2)
        |   intersect _ = []
        
        fun union(r1::t1, r2::t2) = Word.orb(r1, r2) :: union(t1, t2)
        |   union(a, []) = a
        |   union([], b) = b
      
        val emptySet = []
        
        fun isInSet(i, r1::t1) =
            if i < Word.wordSize
            then Word.andb(r1, Word.<<(0w1, Word.fromInt i)) <> 0w0
            else isInSet(i-Word.wordSize, t1)
        |   isInSet(_, []) = false

        fun setToList s =
        let
            fun toList([], _) = []
            |   toList(n :: tl, start) =
                let
                    fun findBits 0w0 = toList(tl, start+Word.wordSize)
                    |   findBits n =
                        let
                            val abit = Word.andb(n, 0w0-n)
                            fun log2(i, b) =
                                if b = abit then i
                                else log2(i+1, Word.<<(b, 0w1))
                        in
                            log2(start, 0w1) :: findBits(Word.xorb(n, abit))
                        end
                in
                    findBits n 
                end
        in
            toList(s, 0)
        end

        (* This is used for debugging only and could be removed. *)
        fun checkSources(regSet :: regTail, stateSet :: stateTail) =
            (
                Word.andb(regSet, stateSet) = regSet orelse raise InternalError "checkSources";
                checkSources(regTail, stateTail)
            )
        |   checkSources([], _) = ()
        |   checkSources _ = raise InternalError "checkSources"

        (* Find the label and extract the corresponding state,  Remove this label from
           the list and return the updated list. *)
        fun findOptionalLabel([], _) = (NONE, [])
        |   findOptionalLabel((this as (thisLabel, thisState)) :: tail, label) =
                if thisLabel = label then (SOME thisState, tail)
                else
                let
                    val (foundState, foundTail) = findOptionalLabel(tail, label)
                in
                    (foundState, this :: foundTail)
                end
        
        fun findLabel(list, label) =
            case findOptionalLabel(list, label) of
                (NONE, _) => raise InternalError "findLabel: Missing label"
            |   (SOME res, resList) => (res, resList)

        (* We only process the condition code to see if it is used.  An arithmetic
           operation cannot be removed if its condition code is used even if its
           result is not. *)
        datatype ccState = CCIndeterminate | CCState of ccRef
        
        fun addConflicts conflicts =
        let
            val conflictRegs = setToList conflicts
        in
            List.app(fn r => Array.update(regConflicts, r, union(Array.sub(regConflicts, r), conflicts))) conflictRegs
        end
        
        (* If we need to use a specific register for an argument e.g. ecx for a shift,
           we mustn't use that register for anything else. *)
        fun addRealRegConflicts(currentSet, exclude, reg) =
        let
            fun checkAndAdd i =
            if i = exclude
            then ()
            else
            let
                val currentConflicts = Array.sub(regRealConflicts, i)
            in
                if List.exists (fn r => r = reg) currentConflicts
                then ()
                else Array.update(regRealConflicts, i, reg :: currentConflicts)
            end
        in
            List.app checkAndAdd currentSet
        end

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
                |   _ => SOME(PReg(i, PRegGeneral))
        in
            List.mapPartial getSave currentRegs
        end
        
        fun identifyAndComputeSets(rest, state, labels, loopLabels, ccIn, sources, dests) =
        let
            val sourceSet = listToSet sources
            and destSet = listToSet dests
            val () = checkSources(sourceSet, state)
            (* Add the destinations to the active set.  These are active after this instruction. *)
            val active = union(state, destSet)
            (* Process the rest, adding each register as it is initialised. *)
            val (tail, references, refLabels, resLoopLabels, ccUsed) =
                identify(rest, active, labels, loopLabels, ccIn)
            (* The result for this instruction is all the registers that were given values
               either before this instruction or by it and also need to have a value AFTER it.
               So we don't include registers whose last use is in this instruction or
               result registers from this instruction that are never used. (e.g. the
               remainder in a division or a work register). *)
            val current = intersect(active, references)
            val currentRegs = setToList current
            val () = List.app(fn i => incrArray(regActive, i)) currentRegs
            (* Have to add our sources to the result state. These are all the
               registers that have to have a value BEFORE this instruction. *)
            val nowActive = union(references, sourceSet)
            (* The conflict set is the union of the current set and the destinations
               The destinations will be in the current set if they are used but we
               can have destinations, e.g. work registers or quotient/remainder
               that need to have registers allocated that aren't then used. *)
            val conflictSet = union(current, destSet)
        in
            (currentRegs, conflictSet, tail, nowActive, refLabels, resLoopLabels, ccUsed)
        end 

        and addSourcesAndDests(instr, rest, state, labels, loopLabels, ccIn, sources, dests) =
        let
            val (_, conflictSet, tail, nowActive, refLabels, resLoopLabels, ccUsed) =
                identifyAndComputeSets(rest, state, labels, loopLabels, ccIn, sources, dests)
            (* Compute the conflict set.  We don't need to do this there are no destinations. *)
            val () = addConflicts conflictSet
        in
            (instr :: tail, nowActive, refLabels, resLoopLabels, ccUsed)
        end
        
        (* Move, tag and untag instructions can be eliminated if the result is never used. *)
        and eliminateableInstruction(instr, rest, state, labels, loopLabels, ccIn, sources, dReg) =
        let
            val (currentRegs, conflictSet, tail, nowActive, refLabels, resLoopLabels, ccUsed) =
                identifyAndComputeSets(rest, state, labels, loopLabels, ccIn, sources, [dReg])
        in
            (* If the destination register is not in the set of registers that need to
               have values after this instruction we can remove this move completely along
               with the destination register.  Otherwise we have to mark this register as
               conflicting with every other register that needs to have a value. *)
            if List.exists(fn i => i = dReg) currentRegs
            then
            (
                addConflicts conflictSet;
                (instr :: tail, nowActive, refLabels, resLoopLabels, ccUsed)
            )
            else (tail, nowActive, refLabels, resLoopLabels, ccUsed)
        end

        and identify ([], _, _, _, _) = ([], emptySet, [], [], CCIndeterminate)
        
        |   identify((instr as LoadArgument { source, dest, ...}) :: rest, state, labels, loopLabels, ccIn) =
                (* Moving to a preg, the destination. *)
                eliminateableInstruction(instr, rest, state, labels, loopLabels, ccIn, sourceRegs source, destReg dest)

        |   identify((instr as StoreArgument { source, base, index, ...}) :: rest, state, labels, loopLabels, ccIn) =
                (* Moving to memory.  The base and index registers are sources not destinations. *)
                addSourcesAndDests(instr, rest, state, labels, loopLabels, ccIn, sourceRegs source @ [sourceReg base] @sourceIndex index, [])

        |   identify((instr as LoadStack { dest, ...}) :: rest, state, labels, loopLabels, ccIn) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, ccIn, [], [destReg dest])

        |   identify((instr as LoadMemReg { dest, ...}) :: rest, state, labels, loopLabels, ccIn) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, ccIn, [], [destReg dest])

        |   identify(ExchangeRegisters _ :: _, _, _, _, _) =
                (* Should not occur at this stage. *)
                raise InternalError "identify - ExchangeRegisters"

        |   identify((instr as BeginFunction {regArgs}) :: rest, state, labels, loopLabels, ccIn) =
            let
                val destRegs = map (fn (dest, src) => destReg dest) regArgs
            in
                addSourcesAndDests(instr, rest, state, labels, loopLabels, ccIn, [], destRegs)
            end

        |   identify((instr as FunctionCall{regArgs, stackArgs, dest, isTail, ...}) :: rest, state, labels, loopLabels, ccIn) =
            let 
                val stackSources = List.foldl(fn (arg, srcs) => sourceRegs arg @ srcs) [] stackArgs
                
                fun regSource((arg, argReg), srcs) = sourceRegs arg @ srcs
                val regSources = List.foldl regSource [] regArgs
            in
                if isTail
                then (* Tail recursive call.  References the argument sources but exits. *)
                let
                    val sources = listToSet(stackSources @ regSources)
                    val () = checkSources(sources, state)
                    val (tail, _, refLabels, resLoopLabels, _) = identify(rest, emptySet, labels, loopLabels, ccIn)
                in
                    (instr :: tail, sources, refLabels, resLoopLabels, CCIndeterminate)
                end
                else (* Non-tail-recursive.  Behaves as a normal reference to sources. *)
                let
                    val dReg = destReg dest
                    (* We don't use addSourcesAndDests because we also have to mark the
                       active registers as MustPush. *)
                    val sourceSet = listToSet(stackSources @ regSources)
                    and destSet = listToSet [dReg]
                    val () = checkSources(sourceSet, state)
                    val active = union(state, destSet)
                    val (tail, references, refLabels, resLoopLabels, _) =
                        identify(rest, active, labels, loopLabels, ccIn)
                    val current = setToList(intersect(active, references))
                    (* The push set, though, excludes the result register. *)
                    val () =
                        List.app(
                            fn i => (incrArray(regActive, i); if i = dReg then () else Array.update(regPushState, i, MustPush))) current
                    val nowActive = union(references, sourceSet)
                    (* No need for conflict sets.  Everything that could conflict must have been pushed. *)
                in
                    (instr :: tail, nowActive, refLabels, resLoopLabels, CCIndeterminate)
                end
            end

        |   identify(AllocateMemoryOperation{size, flags, dest, saveRegs=_} :: rest, state, labels, loopLabels, _) =
            let
                val dReg = destReg dest
                val (currentRegs, conflictSet, tail, nowActive, refLabels, resLoopLabels, ccUsed) =
                    identifyAndComputeSets(rest, state, labels, loopLabels, CCIndeterminate, [], [dReg])
                val () = addConflicts conflictSet
                val saveRegs = getSaveSet(currentRegs, dReg)
                val allocInstr = AllocateMemoryOperation{size=size, flags=flags, dest=dest, saveRegs=saveRegs}
            in
                (allocInstr :: tail, nowActive, refLabels, resLoopLabels, ccUsed)
            end

        |   identify(AllocateMemoryVariable{size, flags, dest, initialiser, saveRegs=_} :: rest, state, labels, loopLabels, _) =
            let
                val dReg = destReg dest
                val iSrc = case initialiser of NONE => [] | SOME init => [sourceReg init]
                val sources = sourceRegs size @ sourceRegs flags @ iSrc
                val (currentRegs, conflictSet, tail, nowActive, refLabels, resLoopLabels, ccUsed) =
                    identifyAndComputeSets(rest, state, labels, loopLabels, CCIndeterminate, sources, [dReg])
                (* We have to include the sources in the conflict set.  Unlike, say LoadArgument where
                   a register in the source can be reused in the destination, we can't reuse the same
                   register for the result as any of the arguments. *)
                val () = addConflicts(union(conflictSet, listToSet sources))
                (* We need to save the registers if we can't do an allocation.
                   The initialising value could be an address so if that's in a tagged
                   register if has to be included. *)
                val saveRegs = getSaveSet(currentRegs @ iSrc, dReg)
                val allocInstr =
                    AllocateMemoryVariable{size=size, flags=flags, dest=dest, initialiser=initialiser, saveRegs=saveRegs}
            in
                (allocInstr :: tail, nowActive, refLabels, resLoopLabels, ccUsed)
            end

        |   identify((instr as InitialisationComplete{...}) :: rest, state, labels, loopLabels, ccIn) =
                (* This is just a marker.  It doesn't actually generate any code. *)
                addSourcesAndDests(instr, rest, state, labels, loopLabels, ccIn, [], [])

        |   identify((instr as StartLoop{arguments, loopLabel}) :: rest, state, labels, loopLabels, ccIn) =
            let
                val sources =
                    listToSet(List.foldl(fn ({source, ...}, srcs) => sourceRegs source @ srcs) [] arguments)
                val () = checkSources(sources, state)
                val dests =
                    listToSet(List.foldl(fn ({loopReg, ...}, dests) => destReg loopReg :: dests) [] arguments)
                (* The active set is the environment plus the loop variables. *)
                val active = union(state, dests)
                (* First pass - process the loop with an empty reference set. *)
                val (tail, references, refLabels, resLoopLabels, _) =
                    identify(rest, active, labels, loopLabels, ccIn)
                (* These variables are live at the start of the loop.  They have to be
                   live everywhere in the loop.  Do the loop variables have to be live?
                   If we use the current value and then compute a new value we don't
                   actually need them to be live between these two. *)
                val activeForLoop = intersect(active, references)
                (* Finally we need to add the sources.  They are used to initialise the
                   loop variables so aren't required within the loop itself. *)
                val nowActive = union(references, sources)
                val current = setToList (intersect(state, references))
                val () = List.app(fn i => incrArray(regActive, i)) current
            in
                (* Add the loop entries to the result this.  They can then be added during the next pass. *)
                (instr :: tail, nowActive, refLabels, (loopLabel, activeForLoop) :: resLoopLabels, CCIndeterminate)
            end

        |   identify((instr as EndLoop{...}) :: rest, state, labels, loopLabels, ccIn) =
            let
                val (tail, references, refLabels, resLoopLabels, _) =
                    identify(rest, state, labels, loopLabels, ccIn)
            in
                (instr :: tail, references, refLabels, resLoopLabels, CCIndeterminate)
            end

        |   identify((instr as JumpLoop{arguments, loopLabel}) :: rest, state, labels, loopLabels, ccIn) =
            let
                val sources =
                    listToSet(List.foldl(fn ({source, ...}, srcs) => sourceRegs source @ srcs) [] arguments)
                val () = checkSources(sources, state)
                (* Because this is an unconditional branch the state afterwards is empty. *)
                val (tail, references, refLabels, resLoopLabels, _) = identify(rest, emptySet, labels, loopLabels, ccIn)
                (* If this is the second pass we need to add the extra active registers
                   to the references. *)
                val compositeRefs =
                    case List.find(fn (lab, _) => lab = loopLabel) loopLabels of
                        NONE => references
                    |   SOME (_, extras) => union(references, extras)
                val current = setToList (intersect(state, compositeRefs))
                val nowActive = union(compositeRefs, sources)
                val () = List.app(fn i => incrArray(regActive, i)) current
           in
                (instr :: tail, nowActive, refLabels, resLoopLabels, CCIndeterminate)
            end

        |   identify((instr as RaiseExceptionPacket{packet}) :: rest, state, labels, loopLabels, ccIn) =
            let
                val sReg = sourceReg packet
                val sources = listToSet [sReg]
                val () = checkSources(sources, state)
                (* This is an unconditional exit.  The only active register is
                   the exception packet. *)
                val (tail, _, refLabels, resLoopLabels, _) = identify(rest, emptySet, labels, loopLabels, ccIn)
            in
                (instr :: tail, sources, refLabels, resLoopLabels, CCIndeterminate)
            end

        |   identify((instr as ReserveContainer{address, ...}) :: rest, state, labels, loopLabels, ccIn) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, ccIn, [], [destReg address])

        |   identify((instr as IndexedCaseOperation{testReg, workReg, cases, ...}) :: rest, state, labels, loopLabels, ccIn) =
            let
                (* This is rather like an unconditional branch except that we have a
                   list of destination branches rather than a single destination. *)
                val sources = listToSet[sourceReg testReg]
                val () = checkSources(sources, state)
                (* Add all the labels to the list.  For each branch the starting
                   state is the same as the current state. *)
                val labelsFromCase =
                    List.foldl (fn (lab, labs) => (lab, state) :: labs) labels cases
                val (tail, _, refLabels, resLoopLabels, _) = identify(rest, emptySet, labelsFromCase, loopLabels, ccIn)
                (* Now extract the case labels from the result list.  This is all the
                   references from all various cases.  We need the union of these.
                   i.e. if we need a register to be active on any of the branches it
                   must be active before this indexed case. *)
                val (newRefLabels, resultRefs) =
                    List.foldl (
                        fn (caseLab, (refLabs, refs)) =>
                        let
                            val (newRefs, newRefLabs) = findLabel(refLabs, caseLab)
                        in
                            (newRefLabs, union(newRefs, refs))
                        end ) (refLabels, emptySet) cases
                val current = intersect(state, resultRefs)
                val () = addConflicts(union(current, listToSet[destReg workReg]))
                val currentRegs = setToList current
                val nowActive = union(resultRefs, sources)
                val () = List.app(fn i => incrArray(regActive, i)) currentRegs
            in
                (instr :: tail, nowActive, newRefLabels, resLoopLabels, CCIndeterminate)
            end

        |   identify((instr as LockMutable{addr}) :: rest, state, labels, loopLabels, ccIn) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, ccIn, sourceRegs addr, [])

        |   identify((ForwardJumpLabel{label, result}) :: rest, state, labels, loopLabels, ccIn) =
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
                        identify(rest, state, newSrcLabels, loopLabels, ccIn)
                |   SOME srcs =>
                    let
                        (* Add the sources that applied at the jump to the current set.
                           In particular if we're following an unconditional jump this will
                           set the state. *)
                        val active = union(srcs, state)
                        val (tail, references, refLabels, resLoopLabels, _) =
                            identify(rest, active, newSrcLabels, loopLabels, ccIn)
                        val current = intersect(active, references)
                        (* We include "references" in both the immediate result and as a label
                           entry.  The registers are live in the immediately preceding code, if
                           there is any, and also at the jump to this label. *)
                        (* If the result is not actually used we don't need to pass it. *)
                        val newResult =
                            case result of
                                NONE => NONE
                            |   SOME(r as PReg(n, _)) => if isInSet(n, current) then SOME r else NONE 
                        val newInstr = ForwardJumpLabel{label=label, result=newResult}
                    in
                        (newInstr :: tail, references, (label, references)::refLabels, resLoopLabels, CCIndeterminate)
                    end
            end

        |   identify((instr as UnconditionalForwardJump{label}) :: rest, state, labels, loopLabels, ccIn) =
            let
                (* Add the current state to the label list.  It will be picked up when we
                   encounter the label itself. *)
                val (tail, _, refLabels, resLoopLabels, _) = identify(rest, emptySet, (label, state) :: labels, loopLabels, ccIn)
                (* Now find the label in the result list.  This will contain the set of registers
                   that are going to be used in the code after the label. *)
                val (refs, newRefLabels) = findLabel(refLabels, label)
                val current = setToList(intersect(state, refs))
                val () = List.app(fn i => incrArray(regActive, i)) current
            in
                (instr :: tail, refs, newRefLabels, resLoopLabels, CCIndeterminate)
            end

        |   identify((instr as ConditionalForwardJump{label, ccRef, ...}) :: rest, state, labels, loopLabels, ccIn) =
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
                val (oldLabel, _) = findOptionalLabel(labels, label)
                val (tail, references, refLabels, resLoopLabels, _) =
                    identify(rest, state, (label, state) :: labels, loopLabels, ccIn)
                (* Find the registers that were active at the label. *)
                val (refsFromLabel, newRefLabels) = findLabel(refLabels, label)
                (* A register is active if it is used either in the code immediately after or
                   in the code after the label.  We need the union of the sets. *)
                val resultState = union(references, refsFromLabel)
                val current = setToList(intersect(state, resultState))
                val () = List.app(fn i => incrArray(regActive, i)) current
            in
                (instr :: tail, resultState,
                    (* If the label was already in use when we came here leave the
                       result label in the list. *)
                    if isSome oldLabel then refLabels else newRefLabels, resLoopLabels, CCState ccRef)
            end

        |   identify((instr as WordComparison{arg1, arg2, ccRef, ...}) :: rest, state, labels, loopLabels, _) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, CCState ccRef, sourceRegs arg1 @ sourceRegs arg2, [])

        |   identify((instr as PushExceptionHandler{handlerAddr, handleStart}) :: rest, state, labels, loopLabels, ccIn) =
            let
                (* Begin an exception handler.  Similar to a conditional jump.  The current state is the
                   state at the start of the handler.  The handler itself will have been removed.
                   We have one "destination" which is the handler entry on the stack. *)
                val dests = listToSet([destReg handlerAddr])
                val active = union(state, dests)
                (* Find the references that are active from the body. *)
                val (tail, references, refLabels, resLoopLabels, _) =
                    identify(rest, active, (handleStart, state) :: labels, loopLabels, ccIn)
                (* Find the registers that were active in the handler. *)
                val (refsFromHandler, newRefLabels) = findLabel(refLabels, handleStart)
                (* The active set is the union of the two. *)
                val nowActive = union(references, refsFromHandler)
                val current = setToList(intersect(state, nowActive))
                val () = List.app(fn i => incrArray(regActive, i)) current
            in
                (instr :: tail, nowActive, newRefLabels, resLoopLabels, CCIndeterminate)
            end

        |   identify((PopExceptionHandler{handlerAddr, resultReg, workReg}) :: rest, state, labels, loopLabels, ccIn) =
            (* Pop an exception handler.  If the result register is not used we must discard it. *)
            let
                val sourceSet = listToSet[sourceReg handlerAddr]
                and destSet = listToSet [destReg workReg]
                val () = checkSources(sourceSet, state)
                val active = union(state, destSet)
                val (tail, references, refLabels, resLoopLabels, ccUsed) =
                    identify(rest, active, labels, loopLabels, ccIn)
                val current = intersect(active, references)
                val () = List.app(fn i => incrArray(regActive, i)) (setToList current)
                val nowActive = union(references, sourceSet)
                val newResultReg =
                    case resultReg of
                        NONE => NONE
                    |   SOME(r as PReg(n, _)) => if isInSet(n, current) then SOME r else NONE
                val newInstr = PopExceptionHandler{handlerAddr=handlerAddr, resultReg=newResultReg, workReg=workReg}
            in
                (newInstr :: tail, nowActive, refLabels, resLoopLabels, ccUsed)
            end

        |   identify((instr as BeginHandler{handleStart, workReg=_, packetReg}) :: rest, state, labels, loopLabels, ccIn) =
            let
                (* Begin a handler.  This is similar to ForwardJumpLabel. *)
                val (srcs, newSrcLabels) = findLabel(labels, handleStart)
                (* Add the sources that applied at the jump to the current set.
                   In particular if we're following an unconditional jump this will
                   set the state. *)
                val dReg = destReg packetReg
                val destSet = listToSet [dReg]
                val active = union(union(srcs, state), destSet)
                val (tail, references, refLabels, resLoopLabels, _) =
                    identify(rest, active, newSrcLabels, loopLabels, ccIn)
                val current = setToList(intersect(active, references))
                (* All the active registers, apart from the exception packet, will have to be pushed
                   to the stack so they are available if the handler is entered. *)
                val () =
                    List.app(
                        fn i => (incrArray(regActive, i); if i = dReg then () else Array.update(regPushState, i, MustPush))) current
            in
                (instr :: tail, references, (handleStart, references)::refLabels, resLoopLabels, CCIndeterminate)
            end

        |   identify((instr as ReturnResultFromFunction{resultReg, ...}) :: rest, state, labels, loopLabels, ccIn) =
            let
                val sRegs = [sourceReg resultReg] (* Should only ever be one. *)
                val sources = listToSet sRegs
                val () = checkSources(sources, state)
                (* This is an unconditional exit.  The only active register is
                   the result. *)
                val (tail, _, refLabels, resLoopLabels, _) = identify(rest, emptySet, labels, loopLabels, ccIn)
            in
                (instr :: tail, sources, refLabels, resLoopLabels, CCIndeterminate)
            end

        |   identify((instr as ArithmeticFunction{resultReg, operand1, operand2, ccRef, ...}) :: rest, state, labels, loopLabels, _) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, CCState ccRef, sourceRegs operand1 @ sourceRegs operand2, [destReg resultReg])

        |   identify((instr as TestTagBit{arg, ccRef, ...}) :: rest, state, labels, loopLabels, _) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, CCState ccRef, sourceRegs arg, [])

        |   identify(PushValue _ :: _, _, _, _, _) =
                (* Should not occur at this stage. *)
                raise InternalError "identify - PushValue"

        |   identify(ResetStackPtr _ :: _, _, _, _, _) =
                (* Should not occur at this stage. *)
                raise InternalError "identify - ResetStackPtr"

        |   identify((instr as TagValue{source, dest}) :: rest, state, labels, loopLabels, ccIn) =
                eliminateableInstruction(instr, rest, state, labels, loopLabels, ccIn, [sourceReg source], destReg dest)

        |   identify((instr as UntagValue{source, dest, ...}) :: rest, state, labels, loopLabels, ccIn) =
                eliminateableInstruction(instr, rest, state, labels, loopLabels, ccIn, sourceRegs source, destReg dest)

        |   identify((instr as LoadEffectiveAddress{base, index, dest, ...}) :: rest, state, labels, loopLabels, ccIn) =
            let
                val bRegs =
                    case base of NONE => [] | SOME bReg => [sourceReg bReg]
                val iRegs = sourceIndex index
            in
                addSourcesAndDests(instr, rest, state, labels, loopLabels, ccIn, bRegs @ iRegs, [destReg dest])
            end

        |   identify((instr as ShiftOperation{resultReg, operand, shiftAmount=IntegerConstant _ , ccRef, ...}) :: rest, state, labels, loopLabels, _) =
                (* Constant shifts don't require ecx. *)
                addSourcesAndDests(instr, rest, state, labels, loopLabels, CCState ccRef, sourceRegs operand, [destReg resultReg])

        |   identify((instr as ShiftOperation{resultReg, operand, shiftAmount=RegisterArgument shiftAmount, ccRef, ...}) :: rest, state, labels, loopLabels, _) =
            let
                val shiftReg = sourceReg shiftAmount
                val valueToShift = sourceRegs operand
                val dReg = destReg resultReg
                val (currentRegs, conflictSet, tail, nowActive, refLabels, resLoopLabels, ccUsed) =
                    identifyAndComputeSets(rest, state, labels, loopLabels, CCState ccRef, shiftReg :: valueToShift, [dReg])
                val () = addConflicts conflictSet
                (* We need to mark all the currentRegs except shiftReg as conflicting with ecx.
                   We also need to mark the value to shift and the result as excluding that. *)
                val () = addRealRegConflicts(dReg :: (currentRegs @ valueToShift), shiftReg, GenReg ecx)
            in
                (instr :: tail, nowActive, refLabels, resLoopLabels, ccUsed)
            end
        
        |   identify(ShiftOperation _ :: _, _, _, _, _) = raise InternalError "identify - ShiftOperation"

        |   identify((instr as Multiplication{resultReg, operand1, operand2, ccRef, ...}) :: rest, state, labels, loopLabels, _) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, CCState ccRef, sourceRegs operand1 @ sourceRegs operand2, [destReg resultReg])

        |   identify((instr as Division{dividend, divisor, quotient, remainder, ...}) :: rest, state, labels, loopLabels, ccIn) =
            let
                (* Division is specific as to the registers.  The dividend must be eax, quotient is
                   eax and the remainder is edx.  The trouble is we also have large-word division
                   and that uses memory arguments. *)
                val quotReg = destReg quotient and remReg = destReg remainder and diviReg = sourceRegs dividend
            in
                addSourcesAndDests(instr, rest, state, labels, loopLabels, ccIn, diviReg @ sourceRegs divisor, [quotReg, remReg])
            end

        |   identify((instr as AtomicExchangeAndAdd{base, source}) :: rest, state, labels, loopLabels, ccIn) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, ccIn, [sourceReg base, sourceReg source], [])

        |   identify(BoxValue{source, dest, boxKind, saveRegs=_} :: rest, state, labels, loopLabels, ccIn) =
            let
                val dReg = destReg dest
                val sources = [sourceReg source]
                val (currentRegs, conflictSet, tail, nowActive, refLabels, resLoopLabels, ccUsed) =
                    identifyAndComputeSets(rest, state, labels, loopLabels, ccIn, sources, [dReg])
                (* If the result is not used we can eliminate this instruction but because it
                   allocates we have to compute the set of registers that need to be preserved. *)
            in
                if List.exists(fn i => i = dReg) currentRegs
                then
                let
                    (* The conflict set includes the value we're going to store.  We need to
                       allocate the memory into the destination register before we can free
                       the source. *)
                    val () = addConflicts(union(conflictSet, listToSet sources))
                    (* The save set is the other registers that are active.  We don't save
                       the destination nor the value we're boxing. *)
                    val saveRegs = getSaveSet(currentRegs, dReg)
                    val boxInstr = BoxValue{source=source, dest=dest, boxKind=boxKind, saveRegs=saveRegs}
                in
                    (boxInstr :: tail, nowActive, refLabels, resLoopLabels, ccUsed)
                end
                else (tail, nowActive, refLabels, resLoopLabels, ccUsed)
            end

        |   identify((instr as CompareByteVectors{vec1Addr, vec2Addr, length, ccRef, ...}) :: rest, state, labels, loopLabels, _) =
            let
                (* We have to use specific registers. *)
                val v1Addr = sourceReg vec1Addr and v2Addr = sourceReg vec2Addr and len = sourceReg length
                val (currentRegs, conflictSet, tail, nowActive, refLabels, resLoopLabels, ccUsed) =
                    identifyAndComputeSets(rest, state, labels, loopLabels, CCState ccRef, [v1Addr, v2Addr, len], [])
                val () = addConflicts conflictSet
                (* We need to add conflicts for each of the registers. *)
                val potentialConflicts = v1Addr :: v2Addr :: len :: currentRegs
                val () = addRealRegConflicts(potentialConflicts, v1Addr, GenReg esi)
                val () = addRealRegConflicts(potentialConflicts, v2Addr, GenReg edi)
                val () = addRealRegConflicts(potentialConflicts, len, GenReg ecx)
            in
                (instr :: tail, nowActive, refLabels, resLoopLabels, ccUsed)
            end

        |   identify((instr as BlockMove{srcAddr, destAddr, length, ...}) :: rest, state, labels, loopLabels, ccIn) =
            let
                val sAddr = sourceReg srcAddr and dAddr = sourceReg destAddr and len = sourceReg length
                val (currentRegs, conflictSet, tail, nowActive, refLabels, resLoopLabels, ccUsed) =
                    identifyAndComputeSets(rest, state, labels, loopLabels, ccIn, [sAddr, dAddr, len], [])
                val () = addConflicts conflictSet
                (* We need to add conflicts for each of the registers. *)
                val potentialConflicts = sAddr :: dAddr :: len :: currentRegs
                val () = addRealRegConflicts(potentialConflicts, sAddr, GenReg esi)
                val () = addRealRegConflicts(potentialConflicts, dAddr, GenReg edi)
                val () = addRealRegConflicts(potentialConflicts, len, GenReg ecx)
            in
                (instr :: tail, nowActive, refLabels, resLoopLabels, ccUsed)
            end

        |   identify((instr as CompareFloatingPt{arg1, arg2, ccRef, ...}) :: rest, state, labels, loopLabels, _) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, CCState ccRef, sourceRegs arg1 @ sourceRegs arg2, [])

        |   identify((instr as X87FPGetCondition{dest, ccRef}) :: rest, state, labels, loopLabels, ccIn) =
            let
                val dReg = destReg dest
                (* Check that we've defined the correct cc. *)
                val _ =
                    (case ccIn of CCState s => s = ccRef | CCIndeterminate => false)
                        orelse raise InternalError "identify - cc mismatch"
                val active = union(state, listToSet [dReg])
                val (tail, references, refLabels, resLoopLabels, _) =
                    identify(rest, active, labels, loopLabels, ccIn)
                val current = setToList(intersect(active, references))
                val () = List.app(fn i => incrArray(regActive, i)) current
            in
                (instr :: tail, references, refLabels, resLoopLabels, CCState ccRef)
            end

        |   identify((instr as X87FPArith{resultReg, arg1, arg2, ...}) :: rest, state, labels, loopLabels, ccIn) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, ccIn, sourceRegs arg1 @ sourceRegs arg2, [destReg resultReg])

        |   identify((instr as X87FPUnaryOps{dest, source, ...}) :: rest, state, labels, loopLabels, ccIn) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, ccIn, sourceRegs source, [destReg dest])

        |   identify((instr as FloatFixedInt{dest, source}) :: rest, state, labels, loopLabels, ccIn) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, ccIn, sourceRegs source, [destReg dest])

        |   identify((instr as SSE2FPArith{resultReg, arg1, arg2, ...}) :: rest, state, labels, loopLabels, ccIn) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, ccIn, sourceRegs arg1 @ sourceRegs arg2, [destReg resultReg])

        val (decorated, _, _, resultLoop, _) = identify(icode, emptySet, [], loopLabels, CCIndeterminate)
        
        val registerState: regState vector =
            Vector.tabulate(maxPRegs,
                fn i => {
                    active = Array.sub(regActive, i),
                    defs = Array.sub(regDefs, i),
                    refs = Array.sub(regRefs, i),
                    pushState = Array.sub(regPushState, i),
                    conflicts = setToList(Array.sub(regConflicts, i)),
                    realConflicts = Array.sub(regRealConflicts, i)
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
    end
end;
