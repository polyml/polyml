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
        
        (* The list is registers is ordered with decreasing values.  That
           corresponds roughly to the order in which registers are added to
           the instruction list. *)
        fun isSet([], _) = false
        |   isSet(hd::tl, r) = if hd = r then true else if r > hd then false else isSet(tl, r)

        fun addItem([], reg) = [reg]
        |   addItem(l as i::r, reg) =
                if i = reg then l
                else if reg < i then i :: addItem(r, reg)
                else reg :: l
        
        fun intersect(ref1, ref2) =
            List.foldl(fn (r, s) => if isSet(ref1, r) then addItem(s, r) else s) [] ref2
        
        fun union(ref1, ref2) = List.foldl (fn(r, s) => addItem(s, r)) ref1 ref2
        
        fun checkSources(regs, state) =
            List.app(fn r => (isSet(state, r) orelse raise InternalError "checkSources"; ())) regs
        
        (* Find the label and extract the corresponding state,  Remove this label from
           the list and return the updated list. *)
        fun findLabel([], _) = raise InternalError "findLabel: label not found."
        |   findLabel((this as (thisLabel, thisState)) :: tail, label) =
                if thisLabel = label then (thisState, tail)
                else
                let
                    val (foundState, foundTail) = findLabel(tail, label)
                in
                    (foundState, this :: foundTail)
                end
        
        fun addSourcesAndDests(instr, rest, state, labels, loopLabels, sources, dests) =
        let
            val () = checkSources(sources, state)
            (* Add the destinations to the active set. *)
            val active = union(state, dests)
            (* Process the rest, adding each register as it is initialised. *)
            val (tail, references, refLabels) =
                identify(rest, active, labels, loopLabels)
            (* Have to add our sources to the result state.  These are now active. *)
            val nowActive = union(references, sources)
            (* Interset the references (the registers that are used after this) with the
               sources (the registers that are given values before this). *)
            val current = intersect(state, nowActive)
            (* Add our destinations to the list even if they are not used anywhere else. *)
            val addDests = union(current, dests)
        in
            ((instr, addDests) :: tail, nowActive, refLabels)
        end

        and identify ([], _, _, _) = ([], [], [])
        
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
                val sources = stackSources @ regSources
                val () = checkSources(sources, state)
                val (tail, _, refLabels) = identify(rest, [], labels, loopLabels)
            in
                ((instr, sources) :: tail, sources, refLabels)
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
                    List.foldl(fn ({source, ...}, srcs) => sourceRegs source @ srcs) [] arguments
                val () = checkSources(sources, state)
                val dests =
                    List.foldl(fn ({loopReg, ...}, dests) => destRegs loopReg @ dests) [] arguments
                (* The active set is the envionment plus the loop variables. *)
                val active = union(state, dests)
                (* First pass - process the loop with an empty reference set. *)
                val (_, references, _) =
                    identify(rest, active, labels, (loopLabel, NONE) :: loopLabels)
                (* These variables are live at the start of the loop.  They have to be
                   live everywhere in the loop.  Do the loop variables have to be live?
                   If we use the current value and then compute a new value we don't
                   actually need them to be live between these two. *)
                val activeForLoop = intersect(active, references)  
                (* Second pass - reprocess the loop.   Currently this reprocesses the
                   whole of the input which is unfortunate. *)
                val (tail, newReferences, refLabels) =
                    identify(rest, active, labels, (loopLabel, SOME activeForLoop) :: loopLabels)
                (* The reference ought to be the same - just check. *)
                val _ = references = newReferences orelse raise InternalError "identify - mismatch"
                (* Finally we need to add the sources.  They are used to initialise the
                   loop variables so aren't required within the loop itself. *)
                val nowActive = union(references, sources)
                val current = intersect(state, nowActive)
                val addDests = union(current, dests)
            in
                ((instr, addDests) :: tail, nowActive, refLabels)
            end

        |   identify((instr as EndLoop{loopLabel}) :: rest, state, labels, (thisLoopLab, _) :: otherLoopLabels) =
            let
                (* First pass - we need to process the rest of the input.  *)
                val _ = thisLoopLab = loopLabel orelse raise InternalError "identify: EndLoop mismatch"
                val (tail, references, refLabels) =
                    identify(rest, state, labels, otherLoopLabels)
            in
                ((instr, intersect(state, references)) :: tail, references, refLabels)
            end

        |   identify(EndLoop _ :: _, _, _, _) =
                raise InternalError "identify: EndLoop outside loop"

        |   identify((instr as JumpLoop{arguments, loopLabel}) :: rest, state, labels,
                     loopLabels as (thisLoopLab, thisLoopState) :: _) =
            let
                val _ = thisLoopLab = loopLabel orelse raise InternalError "identify: JumpLoop mismatch"
                val sources =
                    List.foldl(fn ({source, ...}, srcs) => sourceRegs source @ srcs) [] arguments
                val () = checkSources(sources, state)
                (* Because this is an unconditional branch the state afterwards is empty. *)
                val (tail, references, refLabels) = identify(rest, [], labels, loopLabels)
                (* If this is the second pass we need to add the extra active registers
                   to the references. *)
                val compositeRefs =
                    case thisLoopState of
                        NONE => references
                    |   SOME extras => union(references, extras)
                val nowActive = union(compositeRefs, sources)
                val current = intersect(state, nowActive)
            in
                ((instr, current) :: tail, nowActive, refLabels)
            end

        |   identify(JumpLoop _ :: _, _, _, _) =
                raise InternalError "identify: JumpLoop outside loop"

        |   identify((instr as RaiseExceptionPacket{packet}) :: rest, state, labels, loopLabels) =
            let
                val sources = sourceRegs packet
                val () = checkSources(sources, state)
                (* This is an unconditional exit.  The only active register is
                   the exception packet. *)
                val (tail, _, refLabels) = identify(rest, [], labels, loopLabels)
            in
                ((instr, sources) :: tail, sources, refLabels)
            end

        |   identify((instr as ReserveContainer{address, ...}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, [], destRegs address)

        |   identify((instr as IndexedCaseOperation{testReg, workReg, cases, ...}) :: rest, state, labels, loopLabels) =
                raise InternalError "TODO: IndexedCaseOperation"

        |   identify((instr as LockMutable{addr}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs addr, [])

        |   identify((instr as ForwardJumpLabel{label, result=_}) :: rest, state, labels, loopLabels) =
            let
                (* Ignore the "result" at the moment.  This is the optional register that
                   contains the result if we are merging.  It must have been defined before here
                   and referenced afterwards. *)
                (* Find the set of active registers when we jumped here. *)
                val (srcs, newSrcLabels) = findLabel(labels, label)
                (* Add the sources that applied at the jump to the current set.
                   In particular if we're following an unconditional jump this will
                   set the state. *)
                val active = union(srcs, state)
                val (tail, references, refLabels) =
                    identify(rest, active, newSrcLabels, loopLabels)
                val current = intersect(active, references)
            in
                (* We include "references" in both the immediate result and as a label
                   entry.  The registers are live in the immediately preceding code, if
                   there is any, and also at the jump to this label. *)
                ((instr, current) :: tail, references, (label, references)::refLabels)
            end

        |   identify((instr as UnconditionalForwardJump{label}) :: rest, state, labels, loopLabels) =
            let
                (* Add the current state to the label list.  It will be picked up when we
                   encounter the label itself. *)
                val (tail, _, refLabels) = identify(rest, [], (label, state) :: labels, loopLabels)
                (* Now find the label in the result list.  This will contain the set of registers
                   that are going to be used in the code after the label. *)
                val (refs, newRefLabels) = findLabel(refLabels, label)
                val current = intersect(state, refs)
            in
                ((instr, current) :: tail, refs, newRefLabels)
            end

        |   identify((instr as ConditionalForwardJump{label, ...}) :: rest, state, labels, loopLabels) =
            let
                (* The current state applies both immediately after this and also at the
                   label. *)
                val (tail, references, refLabels) =
                    identify(rest, state, (label, state) :: labels, loopLabels)
                (* Find the registers that were active at the label. *)
                val (refsFromLabel, newRefLabels) = findLabel(refLabels, label)
                (* A register is active if it is used either in the code immediately after or
                   in the code after the label.  We need the union of the sets. *)
                val resultState = union(references, refsFromLabel)
                val current = intersect(state, resultState)
            in
                ((instr, current) :: tail, resultState, newRefLabels)
            end

        |   identify((instr as WordComparison{arg1, arg2, ...}) :: rest, state, labels, loopLabels) =
                addSourcesAndDests(instr, rest, state, labels, loopLabels, sourceRegs arg1 @ sourceRegs arg2, [])

        |   identify((instr as PushExceptionHandler{handlerAddr, handleStart}) :: rest, state, labels, loopLabels) =
                raise InternalError "TODO: PushExceptionHandler"

        |   identify((instr as PopExceptionHandler{handlerAddr, resultReg, workReg}) :: rest, state, labels, loopLabels) =
                raise InternalError "TODO: PopExceptionHandler"

        |   identify((instr as BeginHandler{handleStart, workReg}) :: rest, state, labels, loopLabels) =
                raise InternalError "TODO: BeginHandler"

        |   identify((instr as ReturnResultFromFunction{resultReg, ...}) :: rest, state, labels, loopLabels) =
            let
                val sources = sourceRegs resultReg
                val () = checkSources(sources, state)
                (* This is an unconditional exit.  The only active register is
                   the result. *)
                val (tail, _, refLabels) = identify(rest, [], labels, loopLabels)
            in
                ((instr, sources) :: tail, sources, refLabels)
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
        identify(icode, [], [], [])
    end

    structure Sharing =
    struct
        type x86ICode = x86ICode
    end
end;
