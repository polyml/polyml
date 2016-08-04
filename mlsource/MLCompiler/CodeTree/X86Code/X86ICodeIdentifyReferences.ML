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
    
    exception InternalError = Misc.InternalError
    
    (* This function scans the Icode to identify the usage patterns of the pseudo-registers.
       It scans forward building up a list of the registers that have been defined then
       scans backwards building up a list of references.  A register is then "live" if
       it has been both defined and referenced otherwise it is dead.  *)
    fun identifyRegisters { icode, maxPRegs=_ } =
    let
        (* Find the sources when the argument is used as a source. *)
        fun sourceRegs(PReg(i, _)) = [i]
        |   sourceRegs(MemoryLocation { base, index, ...}) =
            let
                val bRegs = sourceRegs base
                val iRegs =
                    case index of
                        NoMemIndex => []
                    |   MemIndex1 arg => sourceRegs arg
                    |   MemIndex2 arg => sourceRegs arg
                    |   MemIndex4 arg => sourceRegs arg
                    |   MemIndex8 arg => sourceRegs arg
            in
                bRegs @ iRegs
            end
        |   sourceRegs _ = []

        fun destRegs(PReg(i, _)) = [i]
        |   destRegs _ = raise InternalError "destRegs"

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
        
        fun addSourcesAndDests(instr, rest, state, labels, loopLabels, sources, dests) =
        let
            val sourceSet = listToSet sources
            and destSet = listToSet dests
            val () = checkSources(sourceSet, state)
            (* Add the destinations to the active set.  These are active after this instruction. *)
            val active = union(state, destSet)
            (* Process the rest, adding each register as it is initialised. *)
            val (tail, references, refLabels, resLoopLabels) =
                identify(rest, active, labels, loopLabels)
            (* The result for this instruction is all the registers that were given values
               either before this instruction or by it and also need to have a value AFTER it.
               So we don't include registers whose last use is in this instruction or
               result registers from this instruction that are never used. (e.g. the
               remainder in a division or a work register). *)
            val current = intersect(active, references)
            (* Have to add our sources to the result state. These are all the
               registers that have to have a value BEFORE this instruction. *)
            val nowActive = union(references, sourceSet)
        in
            ((instr, setToList current) :: tail, nowActive, refLabels, resLoopLabels)
        end

        and identify ([], _, _, _) = ([], emptySet, [], [])
        
        |   identify((instr as MoveArgument { source, dest=PReg(dReg, _), ...}) :: rest, state, labels, loopLabels) =
                (* Moving to a preg, the destination. *)
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs source, [dReg])
        
        |   identify((instr as MoveArgument { source, dest, ...}) :: rest, state, labels, loopLabels) =
                (* Moving to memory.  The base and index registers are sources not destinations. *)
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs source @ sourceRegs dest, [])

        |   identify((instr as FunctionCall{regArgs, stackArgs, isTail=true, ...}) :: rest,
                                    state, labels, loopLabels) =
            let (* Tail recursive call.  References the argument sources but exits. *)
                val stackSources = List.foldl(fn (arg, srcs) => sourceRegs arg @ srcs) [] stackArgs
                val regSources = List.foldl(fn ((arg, _), srcs) => sourceRegs arg @ srcs) [] regArgs
                val sources = listToSet(stackSources @ regSources)
                val () = checkSources(sources, state)
                val (tail, _, refLabels, resLoopLabels) = identify(rest, emptySet, labels, loopLabels)
            in
                ((instr, []) :: tail, sources, refLabels, resLoopLabels)
            end

        |   identify((instr as FunctionCall{regArgs, stackArgs, dest, isTail=false, ...}) :: rest, state, labels, loopLabels) =
            let (* Non-tail-recursive.  Behaves as a normal reference to sources. *)
                val stackSources = List.foldl(fn (arg, srcs) => sourceRegs arg @ srcs) [] stackArgs
                val regSources = List.foldl(fn ((arg, _), srcs) => sourceRegs arg @ srcs) [] regArgs
            in
                addSourcesAndDests(instr, rest, state, labels, loopLabels, stackSources @ regSources, destRegs dest)
            end

        |   identify((instr as AllocateMemoryOperation{dest, ...}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, [], destRegs dest)

        |   identify((instr as AllocateMemoryVariable{size, flags, dest, initialiser}) :: rest, state, labels, loopLabels) =
            let
                val iSrc = case initialiser of NONE => [] | SOME init => sourceRegs init
                val sources = sourceRegs size @ sourceRegs flags @ iSrc
            in
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sources, destRegs dest)
            end

        |   identify((instr as InitialisationComplete{dest}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, [], destRegs dest)

        |   identify((instr as StartLoop{arguments, loopLabel}) :: rest, state, labels, loopLabels) =
            let
                val sources =
                    listToSet(List.foldl(fn ({source, ...}, srcs) => sourceRegs source @ srcs) [] arguments)
                val () = checkSources(sources, state)
                val dests =
                    listToSet(List.foldl(fn ({loopReg, ...}, dests) => destRegs loopReg @ dests) [] arguments)
                (* The active set is the environment plus the loop variables. *)
                val active = union(state, dests)
                (* First pass - process the loop with an empty reference set. *)
                val (tail, references, refLabels, resLoopLabels) =
                    identify(rest, active, labels, loopLabels)
                (* These variables are live at the start of the loop.  They have to be
                   live everywhere in the loop.  Do the loop variables have to be live?
                   If we use the current value and then compute a new value we don't
                   actually need them to be live between these two. *)
                val activeForLoop = intersect(active, references)
                (* Finally we need to add the sources.  They are used to initialise the
                   loop variables so aren't required within the loop itself. *)
                val nowActive = union(references, sources)
                val current = intersect(state, references)
            in
                (* Add the loop entries to the result this.  They can then be added during the next pass. *)
                ((instr, setToList current) :: tail, nowActive, refLabels, (loopLabel, activeForLoop) :: resLoopLabels)
            end

        |   identify((instr as EndLoop{...}) :: rest, state, labels, loopLabels) =
            let
                val (tail, references, refLabels, resLoopLabels) =
                    identify(rest, state, labels, loopLabels)
            in
                ((instr, setToList(intersect(state, references))) :: tail, references, refLabels, resLoopLabels)
            end

        |   identify((instr as JumpLoop{arguments, loopLabel}) :: rest, state, labels, loopLabels) =
            let
                val sources =
                    listToSet(List.foldl(fn ({source, ...}, srcs) => sourceRegs source @ srcs) [] arguments)
                val () = checkSources(sources, state)
                (* Because this is an unconditional branch the state afterwards is empty. *)
                val (tail, references, refLabels, resLoopLabels) = identify(rest, emptySet, labels, loopLabels)
                (* If this is the second pass we need to add the extra active registers
                   to the references. *)
                val compositeRefs =
                    case List.find(fn (lab, _) => lab = loopLabel) loopLabels of
                        NONE => references
                    |   SOME (_, extras) => union(references, extras)
                val current = intersect(state, compositeRefs)
                val nowActive = union(compositeRefs, sources)
            in
                ((instr, setToList current) :: tail, nowActive, refLabels, resLoopLabels)
            end

        |   identify((instr as RaiseExceptionPacket{packet}) :: rest, state, labels, loopLabels) =
            let
                val sources = listToSet(sourceRegs packet)
                val () = checkSources(sources, state)
                (* This is an unconditional exit.  The only active register is
                   the exception packet. *)
                val (tail, _, refLabels, resLoopLabels) = identify(rest, emptySet, labels, loopLabels)
            in
                ((instr, []) :: tail, sources, refLabels, resLoopLabels)
            end

        |   identify((instr as ReserveContainer{address, ...}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, [], destRegs address)

        |   identify((instr as IndexedCaseOperation{testReg, workReg=_, cases, ...}) :: rest, state, labels, loopLabels) =
            let
                (* This is rather like an unconditional branch except that we have a
                   list of destination branches rather than a single destination. *)
                val sources = listToSet(sourceRegs testReg)
                val () = checkSources(sources, state)
                (* Add all the labels to the list.  For each branch the starting
                   state is the same as the current state. *)
                val labelsFromCase =
                    List.foldl (fn (lab, labs) => (lab, state) :: labs) labels cases
                val (tail, _, refLabels, resLoopLabels) = identify(rest, emptySet, labelsFromCase, loopLabels)
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
                val nowActive = union(resultRefs, sources)
            in
                ((instr, setToList current) :: tail, nowActive, newRefLabels, resLoopLabels)
            end

        |   identify((instr as LockMutable{addr}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs addr, [])

        |   identify((instr as ForwardJumpLabel{label, result=_}) :: rest, state, labels, loopLabels) =
            let
                (* Ignore the "result" at the moment.  This is the optional register that
                   contains the result if we are merging.  It must have been defined before here
                   and referenced afterwards. *)
                (* Find the set of active registers when we jumped here. 
                   The label may not have been used. That could happen if we have a
                   "drop-through" case in an andalso/orelse or if we didn't generate a
                   jump because we raised an exception or made a tail-jump.  *)
                val (optSrcs, newSrcLabels) = findOptionalLabel(labels, label)
            in
                case optSrcs of
                    NONE =>
                        (* If this was never used delete this instruction. *)
                        identify(rest, state, newSrcLabels, loopLabels)
                |   SOME srcs =>
                    let
                        (* Add the sources that applied at the jump to the current set.
                           In particular if we're following an unconditional jump this will
                           set the state. *)
                        val active = union(srcs, state)
                        val (tail, references, refLabels, resLoopLabels) =
                            identify(rest, active, newSrcLabels, loopLabels)
                        val current = intersect(active, references)
                        (* We include "references" in both the immediate result and as a label
                           entry.  The registers are live in the immediately preceding code, if
                           there is any, and also at the jump to this label. *)
                    in
                        ((instr, setToList current) :: tail, references, (label, references)::refLabels, resLoopLabels)
                    end
            end

        |   identify((instr as UnconditionalForwardJump{label}) :: rest, state, labels, loopLabels) =
            let
                (* Add the current state to the label list.  It will be picked up when we
                   encounter the label itself. *)
                val (tail, _, refLabels, resLoopLabels) = identify(rest, emptySet, (label, state) :: labels, loopLabels)
                (* Now find the label in the result list.  This will contain the set of registers
                   that are going to be used in the code after the label. *)
                val (refs, newRefLabels) = findLabel(refLabels, label)
                val current = intersect(state, refs)
            in
                ((instr, setToList current) :: tail, refs, newRefLabels, resLoopLabels)
            end

        |   identify((instr as ConditionalForwardJump{label, ...}) :: rest, state, labels, loopLabels) =
            let
                (* The current state applies both immediately after this and also at the
                   label. *)
                (* We may have more than one jump to the same label.  When working forward
                   we use the orginal state but when working back we need to add the
                   references together. *)
                val (oldLabel, _) = findOptionalLabel(labels, label)
                val (tail, references, refLabels, resLoopLabels) =
                    identify(rest, state, (label, state) :: labels, loopLabels)
                (* Find the registers that were active at the label. *)
                val (refsFromLabel, newRefLabels) = findLabel(refLabels, label)
                (* A register is active if it is used either in the code immediately after or
                   in the code after the label.  We need the union of the sets. *)
                val resultState = union(references, refsFromLabel)
                val current = intersect(state, resultState)
            in
                ((instr, setToList current) :: tail, resultState,
                    (* If the label was already in use when we came here leave the
                       result label in the list. *)
                    if isSome oldLabel then refLabels else newRefLabels, resLoopLabels)
            end

        |   identify((instr as WordComparison{arg1, arg2, ...}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs arg1 @ sourceRegs arg2, [])

        |   identify((instr as PushExceptionHandler{handlerAddr, handleStart}) :: rest, state, labels, loopLabels) =
            let
                (* Begin an exception handler.  Similar to a conditional jump.  The current state is the
                   state at the start of the handler.  The handler itself will have been removed.
                   We have one "destination" which is the handler entry on the stack. *)
                val dests = listToSet(destRegs handlerAddr)
                val active = union(state, dests)
                (* Find the references that are active from the body. *)
                val (tail, references, refLabels, resLoopLabels) =
                    identify(rest, active, (handleStart, state) :: labels, loopLabels)
                (* Find the registers that were active in the handler. *)
                val (refsFromHandler, newRefLabels) = findLabel(refLabels, handleStart)
                (* The active set is the union of the two. *)
                val nowActive = union(references, refsFromHandler)
                val current = intersect(state, nowActive)
            in
                ((instr, setToList current) :: tail, nowActive, newRefLabels, resLoopLabels)
            end

        |   identify((instr as PopExceptionHandler{handlerAddr, resultReg=_, workReg}) :: rest, state, labels, loopLabels) =
                (* Pop an exception handler. *)
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs handlerAddr, destRegs workReg)

        |   identify((instr as BeginHandler{handleStart, workReg=_}) :: rest, state, labels, loopLabels) =
            let
                (* Begin a handler.  This is similar to ForwardJumpLabel. *)
                val (srcs, newSrcLabels) = findLabel(labels, handleStart)
                (* Add the sources that applied at the jump to the current set.
                   In particular if we're following an unconditional jump this will
                   set the state. *)
                val active = union(srcs, state)
                val (tail, references, refLabels, resLoopLabels) =
                    identify(rest, active, newSrcLabels, loopLabels)
                val current = intersect(active, references)
            in
                ((instr, setToList current) :: tail, references, (handleStart, references)::refLabels, resLoopLabels)
            end

        |   identify((instr as ReturnResultFromFunction{resultReg, ...}) :: rest, state, labels, loopLabels) =
            let
                val sources = listToSet(sourceRegs resultReg)
                val () = checkSources(sources, state)
                (* This is an unconditional exit.  The only active register is
                   the result. *)
                val (tail, _, refLabels, resLoopLabels) = identify(rest, emptySet, labels, loopLabels)
            in
                ((instr, []) :: tail, sources, refLabels, resLoopLabels)
            end

        |   identify((instr as ArithmeticFunction{resultReg, operand1, operand2, ...}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs operand1 @ sourceRegs operand2, destRegs resultReg)

        |   identify((instr as TestTagBit{arg, ...}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs arg, [])

        |   identify(PushValue _ :: _, _, _, _) =
                (* Should not occur at this stage. *)
                raise InternalError "identify - PushValue"

        |   identify(ResetStackPtr _ :: _, _, _, _) =
                (* Should not occur at this stage. *)
                raise InternalError "identify - PushValue"

        |   identify((instr as TagValue{source, dest}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs source, destRegs dest)

        |   identify((instr as UntagValue{source, dest, ...}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs source, destRegs dest)

        |   identify((instr as LoadEffectiveAddress{base, index, dest, ...}) :: rest, state, labels, loopLabels) =
            let
                val bRegs =
                    case base of NONE => [] | SOME bReg => sourceRegs bReg
                val iRegs =
                    case index of
                        NoMemIndex => []
                    |   MemIndex1 arg => sourceRegs arg
                    |   MemIndex2 arg => sourceRegs arg
                    |   MemIndex4 arg => sourceRegs arg
                    |   MemIndex8 arg => sourceRegs arg
            in
                addSourcesAndDests(instr, rest, state, labels, loopLabels, bRegs @ iRegs, destRegs dest)
            end

        |   identify((instr as ShiftOperation{resultReg, operand, shiftAmount, ...}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs operand @ sourceRegs shiftAmount, destRegs resultReg)

        |   identify((instr as Multiplication{resultReg, operand1, operand2, ...}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs operand1 @ sourceRegs operand2, destRegs resultReg)

        |   identify((instr as Division{dividend, divisor, quotient, remainder, ...}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs dividend @ sourceRegs divisor,
                            destRegs quotient @ destRegs remainder)

        |   identify((instr as AtomicExchangeAndAdd{destAddr, source}) :: rest, state, labels, loopLabels) =
                (* The destination is an address so functions as a source *)
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs destAddr @ sourceRegs source, [])

        |   identify((instr as BoxValue{source, dest, ...}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs source, destRegs dest)

        |   identify((instr as CompareByteVectors{vec1Addr, vec2Addr, length, ...}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels,
                        sourceRegs vec1Addr @ sourceRegs vec2Addr @ sourceRegs length, [])

        |   identify((instr as BlockMove{srcAddr, destAddr, length, ...}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels,
                        sourceRegs srcAddr @ sourceRegs destAddr @ sourceRegs length, [])

        |   identify((instr as CompareFloatingPt{arg1, arg2, ...}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs arg1 @ sourceRegs arg2, [])

        |   identify((instr as X87FPGetCondition{dest, ...}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, [], destRegs dest)

        |   identify((instr as X87FPArith{resultReg, arg1, arg2, ...}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs arg1 @ sourceRegs arg2, destRegs resultReg)

        |   identify((instr as X87FPUnaryOps{dest, source, ...}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs source, destRegs dest)

        |   identify((instr as FloatFixedInt{dest, source}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs source, destRegs dest)

        |   identify((instr as SSE2FPArith{resultReg, arg1, arg2, ...}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs arg1 @ sourceRegs arg2, destRegs resultReg)

    in
        (* Process the list once.  If we have one or more loops in it we have to
           reprocess it with the loop information we found in the first pass. *)
        case identify(icode, emptySet, [], []) of
            (codePass1, _, _, []) => codePass1
        |   (_, _, _, loopLabels) => #1 (identify(icode, emptySet, [], loopLabels))
    end

    structure Sharing =
    struct
        type x86ICode = x86ICode
    end
end;
