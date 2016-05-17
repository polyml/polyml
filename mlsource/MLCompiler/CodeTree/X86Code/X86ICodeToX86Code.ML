(*
    Copyright David C. J. Matthews 2016

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

functor X86ICodeToX86Code(

    structure X86CODE: X86CODESIG

    structure X86OPTIMISE:
    sig
        type operation
        type code
        type operations = operation list

        val optimise: code * operations -> operations

        structure Sharing:
        sig
            type operation = operation
            type code = code
        end
    end

    structure DEBUG: DEBUGSIG
    
    sharing X86CODE.Sharing = X86OPTIMISE.Sharing
): ICodeSig =
struct
    open X86CODE

    open Address

    datatype argument =
        PReg of int (* A pseudo-register - an abstract register. *)
    |   RealRegister of reg (* A real register. *)
    |   AddressConstant of machineWord (* A constant that is an address. *)
    |   IntegerConstant of LargeInt.int (* A non-address constant.  Will usually be shifted and tagged. *)
    |   MemoryLocation of { base: argument, offset: int } (* A memory location.  Could be the stack. *)

    datatype iLabel = ILabel of int

    datatype x86ICode =
        (* Load, store and move, depending on the kinds of arguments.  Only certain combinations
           are supported. *)
        MoveArgument of { source: argument, dest: argument}

    |   MoveStackArgToPreg of { offset: int, dest: argument} (* Load an argument or the return address *)
    
        (* Call or jump to a function.  Currently the closure is loaded into edx/rdx as one of the
           regArgs. The result is stored in the destination register.  If isTail is true this is tail-recursive
           and should be a jump. *)
    |   FunctionCall of { regArgs: (argument * genReg) list, stackArgs: argument list, dest: argument, isTail: bool}

        (* Allocate a tuple or a function closure. *)
    |   AllocateMemoryToPreg of { size: int, isMutable: bool, dest: argument}

        (* Signal that a tuple has been fully initialised.  Really a check in the
           low-level code-generator. *)
    |   InitialisationComplete of { dest: argument}

        (* Begin a loop.  A set of loop registers are initialised and the loop is entered.
           The loopLabel in all of these is for checking only, at least at the moment. *)
    |   StartLoop of { arguments: {source: argument, loopReg: argument} list, loopLabel: iLabel }

        (* End a loop.  Causes the loop stack to be popped. *)
    |   EndLoop of { loopLabel: iLabel }

        (* Within a loop the loop registers are updated from the source registers and
           a jump is made back to the containing StartLoop *)
    |   JumpLoop of { arguments: {source: argument, loopReg: argument} list, loopLabel: iLabel }
    
    |   RaiseExceptionPacket of { packet: argument }

        (* Reserve a contiguous area on the stack to receive a result tuple. *)
    |   ReserveContainer of { size: int, address: argument }

        (* Indexed case.  This could possibly be simplified. *)
    |   IndexedCaseOperation of { testReg: argument, workReg: argument, default: iLabel,
                         cases: (iLabel * word) list, exhaustive: bool, min: word, max: word,
                         arbitrary: bool }

        (* Lock a mutable cell by turning off the mutable bit. *)
    |   LockMutable of { addr: argument }

        (* Forward branches. *)
    |   ForwardJumpLabel of { label: iLabel, result: argument option }
    
    |   UnconditionalForwardJump of { label: iLabel }

        (* Compare and branch between two word values. *)
    |   JumpOnWordComparison of { arg1: argument, arg2: argument, condition: branchOps, label: iLabel }
    
        (* Exception handling.  - Set up an exception handler. *)
    |   PushExceptionHandler of { handlerAddr: argument, handleStart: iLabel }

        (* End of a handled section.  Restore the previous handler. *)
    |   PopExceptionHandler of { handlerAddr: argument, resultReg: argument }

        (* Marks the start of a handler.  This sets the stack pointer and
           restores the old handler.  As a special case it must ensure that rax is
           preserved because that contains the exception packet. *) 
    |   BeginHandler of { handleStart: iLabel }

        (* Return from the function. *)
    |   ReturnResultFromFunction of { resultReg: argument, numStackArgs: int }
    
        (* Arithmetic or logical operation. *)
    |   ArithmeticFunction of { oper: arithOp, resultReg: argument, operand1: argument, operand2: argument }

        (* Test the tag bit and jump if set (onShort=true) or clear (onShort=false). *)
    |   JumpOnTag of { arg: argument, onShort: bool, label: iLabel }

    
    exception InternalError = Misc.InternalError

    (* tag a short constant *)
    fun tag c = 2 * c + 1

    fun condBranch(test, predict) =
    let
        val label as Labels{uses, ...} = mkLabel()
    in
        uses := 1;
        ([ConditionalBranch{test=test, predict=predict, label=label}], label)
    end
    
    and uncondBranch() =
    let
        val label as Labels{uses, ...} = mkLabel()
    in
        uses := 1;
        ([UncondBranch label], label)
    end

    and backJumpLabel() =
    let
        val loopLabel = mkLabel()
    in
        ([JumpLabel loopLabel], loopLabel)
    end

    and forwardJumpLabel v = [JumpLabel v]

    and jumpBackwards (v as Labels{uses, ...}) = (uses := !uses+1; [UncondBranch v])

    fun loadMemory(reg, base, offset) =
        [LoadMemR{source=BaseOffset{base=base, offset=offset, index=NoIndex}, output=reg}]
    and loadStack(reg, offset) = loadMemory(reg, esp, offset)
    and storeMemory(reg, base, offset) =
        [StoreRegToMemory{toStore=reg, address=BaseOffset{base=base, offset=offset, index=NoIndex}}]

    (* Allocate memory.  N.B. Instructions are in reverse order. *)
    fun allocStore{size, flags, output, preserve} =
    if isX64 andalso flags <> 0w0
    then
        [StoreByteConstToMemory{toStore=flags, address=BaseOffset{offset= ~1, base=output, index=NoIndex}},
         StoreConstToMemory{toStore=LargeInt.fromInt size, address=BaseOffset{offset= ~wordSize, base=output, index=NoIndex}},
         AllocStore{size=size, output=output, saveRegs=preserve}]
    else
    let
        val lengthWord = IntInf.orb(IntInf.fromInt size, IntInf.<<(Word8.toLargeInt flags, 0w24))
    in
        [StoreConstToMemory{toStore=lengthWord, address=BaseOffset{offset= ~wordSize, base=output, index=NoIndex}},
         AllocStore{size=size, output=output, saveRegs=preserve}]
    end


    fun argToPReg(PReg n) = n | argToPReg _ = raise InternalError "argToPReg"


    fun codeICodeFunctionToX86{icode, functionName, maxLabels, maxPRegs= _, argRegsUsed, hasFullClosure, currentStackArgs, debugSwitches} =
    let
        (* The profile object is a single mutable with the F_bytes bit set. *)
        val profileObject = toMachineWord (alloc(0w1, Word8.orb(F_mutable, F_bytes), toMachineWord 0w0))

        (* We may need to create additional labels. *)
        val labelCounter = ref maxLabels
        fun newLabel() = ILabel(!labelCounter) before labelCounter := !labelCounter + 1

        local
            val addAllocatingFunction =
                DEBUG.getParameter DEBUG.profileAllocationTag debugSwitches = 1
        in
            fun allocateHeap(size, flag, toReg) =
            (* If we need to add the profile object *)
            if addAllocatingFunction
            then
                allocStore {size=size+1, flags=Word8.orb(flag, Address.F_profile), output=toReg, preserve=[]} @
                    [StoreLongConstToMemory{ toStore=profileObject, address=BaseOffset{base=toReg, offset=size*wordSize, index=NoIndex}}]
            else allocStore {size=size, flags=flag, output=toReg, preserve=[]}
        end

        val maxStack = ref 0 (* High-water mark *)

        datatype pregLoc = InRax | OnStack of int

        datatype state = NormalState of { valueStack : {preg: int, loc: pregLoc} list } | Exited
        
        val initialState = {valueStack=[]} (* Initial and final state. *)

        (* Return the location on the real stack immediately above the top item. *)
        local
            fun stackP [] = 0
            |   stackP ({loc=OnStack sp, ...} :: _) = sp+1
            |   stackP (_ :: stack) = stackP stack
        in
            fun stackPtr{valueStack} = stackP valueStack
        end

        local
            fun findE (_, []) = raise InternalError "findEntry: not found"
            |   findE (reg, {preg, loc} :: rest) = if reg = preg then loc else findE(reg, rest)
        in
            fun findEntry(reg, {valueStack}) = findE(reg, valueStack)
        end
    
        fun loadSource(source, dest, state) =
            case findEntry (source, state) of
                InRax => if dest = eax then [] else [MoveRR { source=eax, output=dest }]
            |   OnStack s => loadStack(dest, (stackPtr state - s - 1) * wordSize)

        (* If the top of the stack is in RAX push it otherwise do nothing. *)
        fun pushRax (state as {valueStack={preg, loc=InRax} :: stack }) =
            let
                val sp = stackPtr state
                val () = if sp >= !maxStack then maxStack := sp+1 else ()
            in
                (* Return the updated state with the value pushed. *)
                ({valueStack={preg=preg, loc=OnStack sp} :: stack}, [PushR eax])
            end
        |   pushRax stack = (stack, [])

        (* Add a pseudo-register containing rax to the top of the stack. *)
        fun addRaxToState({valueStack}, destReg) = {valueStack={preg=destReg, loc=InRax} :: valueStack}

        (* Check that the top of the stack is the required pseudo-register and the value is in Rax.
           Returns the state with this item popped. *)
        fun checkTopInRax({valueStack={preg, loc=InRax } :: resultState}, checkReg) =
            if preg = checkReg then {valueStack=resultState} else raise InternalError "checkTopInRax"
        |   checkTopInRax _ = raise InternalError "checkTopInRax"
        
        (* Reset the stack to value in the original state except that
           the target register should be moved into the result state. *)
        fun resetStack(state, oldState, target) =
        let
            (* Check that the target is on the top of the stack and in rax. *)
            val _ = checkTopInRax(state, target)
            (* Check that the top of the old state is not in a register. *)
            val () =
                case oldState of
                    {valueStack={loc=InRax, ...} :: _} => raise InternalError "resetStack"
                |   _ => ()
            val currentSp = stackPtr state
            and oldSp = stackPtr oldState
            val newState = addRaxToState(oldState, target)
            val code =
                if currentSp = oldSp then [] else [ResetStack(currentSp - oldSp)]
        in
            (newState, code)
        end

        (* Process the code until the end.  The code list is built up in reverse order. *)
        (* If we reach the end we haven't exited or reached a JumpLoop so we just return the
           final state. *)
        fun codeICode{icode=[], state: state, code, ...} = (state, code)

            (* Destination of a forward jump.  If the state was Exited this can
               continue.
               We should have the label in the list.
               We need to merge the states associated with dropping through and the
               state associated with the branch.  We can only adjust the state on the
               "drop through" side so if we need to change the state on the branch
               side we have to create a new branch. *)
        |   codeICode{icode=ForwardJumpLabel{label, result} :: rest, state, code, context as {forwardLabels, loopArgs, handlers}} =
            let
                (* Find the label in the list.  Remove it if it's there.  It may not be if
                   we never used it.  That could happen if we have a "drop-through" case in
                   an andalso/orelse or if we didn't generate a jump because we raised an
                   exception or made a tail-jump. *)
                fun findAndRemoveLabel(label: iLabel, (hd as (l, _, _)) :: tl) =
                    if label = l
                    then (SOME hd, tl)
                    else
                    let
                        val (found, newTail) = findAndRemoveLabel(label, tl)
                    in
                        (found, hd::newTail)
                    end
                |   findAndRemoveLabel _ = (NONE, [])
                val (labelCondition, newLabels) = findAndRemoveLabel(label, forwardLabels)
            in
                case (state, labelCondition) of
                    (state, NONE) => (* We didn't actually take a branch.  Discard the label and continue. *)
                        codeICode{icode=rest, state=state, code=code, context={forwardLabels=newLabels, loopArgs=loopArgs, handlers=handlers}}

                |   (Exited, SOME(_, labelState, labelCode)) =>
                        (* There is no drop-through to consider because the preceding instruction
                           was an unconditional jump or something similar.  We can just set the state
                           and continue. *)
                        codeICode{icode=rest, state=NormalState labelState, code=forwardJumpLabel labelCode @ code,
                                  context={forwardLabels=newLabels, loopArgs=loopArgs, handlers=handlers}}

                |   (NormalState cstate, SOME(_, labelState, labelCode)) =>
                    (* There was a drop-through.  Merge the states.  We can only actually change the
                       drop-through state i.e. before the label.  If the state on the incoming branch
                       needs to be changed we have to add an extra branch. *)
                    let
                        (* We should have the result register on both arms. *)
                        fun removeResult s =
                            case result of
                                NONE => s
                            |   SOME resArg => checkTopInRax(s, argToPReg resArg)
                            
                        val dropThrough = removeResult cstate and labState = removeResult labelState
                        (* After removing the top item the tails of the states will match up to the
                           point they diverged.  The common part is the resulting state. *)
                        fun matchStates([], _) = []
                        |   matchStates(_, []) = []
                        |   matchStates((hd as {loc=loc1, preg=preg1})::l1, {loc=loc2, preg=preg2}::l2) =
                            if preg1 <> preg2
                            then []
                            else if loc1 <> loc2
                            then raise InternalError "matchStates"
                                else hd :: matchStates(l1, l2)

                        val composite =
                            {valueStack=List.rev(matchStates(List.rev (#valueStack dropThrough), List.rev (#valueStack labState)))}
                        (* Currently the only thing we need to adjust is the stack pointer. *)
                        val finalSp = stackPtr composite
                        and labelSp = stackPtr labelState

                        val (finalState, adjustSpOnDropThrough) =
                            case result of
                                NONE =>
                                let
                                    val currentSp = stackPtr cstate and finalSp = stackPtr composite
                                in
                                    (composite, if currentSp = finalSp then [] else [ResetStack(currentSp-finalSp)])
                                end
                            |   SOME resReg => resetStack(cstate, composite, argToPReg resReg)
                    in
                        if labelSp = finalSp
                        then (* We don't need to do anything to the incoming branch. *)
                            codeICode{icode=rest, state=NormalState finalState, code=forwardJumpLabel labelCode @ adjustSpOnDropThrough @ code,
                                      context={forwardLabels=newLabels, loopArgs=loopArgs, handlers=handlers}}
                        else (* We're going to have to adjust the stack pointer on the incoming branch.
                                We can't actually do that so instead we have to branch and merge again. *)
                        let
                            val extraLabel = newLabel()
                        in
                            codeICode{
                                (* Put in an unconditional jump followed by the label we're currently trying to merge in.
                                   Because it's following an unconditional jump the state will simply be set to the
                                   label state.  When we merge in again we will be able to adjust the stack pointer and
                                   it should all work. *)
                                icode=UnconditionalForwardJump{label=extraLabel} :: ForwardJumpLabel{label=label, result=result} ::
                                      ForwardJumpLabel{label=extraLabel, result=result} :: rest,
                                state=NormalState finalState, code=adjustSpOnDropThrough @ code,
                                context=context (* Original context - we haven't fixed up the label yet. *)}
                        end
                    end
            end

            (* Start of a handler.  This is similar to a ForwardLabel and should only occur in an Exited state.
               The handlers form a stack so the one we want should be on the top. *)
        |   codeICode{
                icode=BeginHandler{handleStart} :: rest, state=Exited, code,
                context={forwardLabels, loopArgs, handlers=(handleLab, handlerStartState, handlerLabRef) :: otherHandlers}} =
            let
                val _ = handleLab = handleStart orelse raise InternalError "codeICode: BeginHandler handler stack mismatch"
                val handlerCode =
                    [
                        StoreRegToMemory{
                            toStore=ebx, address=BaseOffset{offset=memRegHandlerRegister, base=ebp, index=NoIndex}},
                        PopR ebx, ResetStack 1,
                        LoadMemR{ source=BaseOffset{base=ebp, offset=memRegHandlerRegister, index=NoIndex}, output=esp },
                        StartHandler{handlerLab=handlerLabRef}
                    ]
            in
                codeICode{icode=rest, state=NormalState handlerStartState, code=handlerCode @ code,
                        context={forwardLabels=forwardLabels, loopArgs=loopArgs, handlers=otherHandlers}}
            end

        |   codeICode{icode=BeginHandler _ :: _, ...} =
                raise InternalError "codeICode: BeginHandler not preceded by jump or not in handler"
                
        |   codeICode{icode=EndLoop{loopLabel} :: rest, state, code, context={forwardLabels, loopArgs=(_, _, lab)::otherLoops, handlers}} =
            let
                val _ = lab = loopLabel orelse raise InternalError "codeICode: EndLoop - mismatched labels"
            in
                codeICode{icode=rest, state=state, code=code,
                        context={forwardLabels=forwardLabels, loopArgs=otherLoops, handlers=handlers}}
            end

        |   codeICode{icode=EndLoop _ :: _, ...} = raise InternalError "codeICode: EndLoop outside loop"

            (* Any other Exited conditions. Skip the instructions.  There are various situations where this
               can arise.  e.g. OS.Process.exit contains an infinite loop processing atExit functions
               until it eventually exits by calling a "stop" function. *)
        |   codeICode{icode=_ :: rest, state=Exited, code, context} =
               codeICode{icode=rest, state=Exited, code=code, context=context}

        |   codeICode{icode=UnconditionalForwardJump{label} :: rest, state=NormalState state, code, context={forwardLabels, loopArgs, handlers}} =
            let
                val (branchCode, lab) = uncondBranch()
            in
                codeICode{icode=rest, state=Exited, code=branchCode @ code,
                                      context={forwardLabels=(label, state, lab) :: forwardLabels, loopArgs=loopArgs, handlers=handlers}}
            end

            (* Word comparison. *)
        |   codeICode{icode=JumpOnWordComparison{arg1=PReg testR, arg2=IntegerConstant testVal, condition, label} :: rest,
                      state=NormalState state, code, context={forwardLabels, loopArgs, handlers}} =
            let
                (* The value on the top of the stack should be in rax and the value we want to test. *)
                val startState = checkTopInRax(state, testR)
                val testIt = ArithRConst {opc=CMP, output=eax, source=testVal}
                val (condJump, branchLabel) = condBranch(condition, PredictNeutral)
            in
                codeICode{icode=rest, state=NormalState startState, code=condJump @ [testIt] @ code,
                                      context={forwardLabels=(label, startState, branchLabel) :: forwardLabels, loopArgs=loopArgs, handlers=handlers}}
            end

        |   codeICode{icode=JumpOnWordComparison{arg1=PReg testR, arg2=AddressConstant testVal, condition, label} :: rest,
                      state=NormalState state, code, context={forwardLabels, loopArgs, handlers}} =
            let
                (* The value on the top of the stack should be in rax and the value we want to test. *)
                val startState = checkTopInRax(state, testR)
                val testIt = ArithRLongConst {opc=CMP, output=eax, source=testVal}
                val (condJump, branchLabel) = condBranch(condition, PredictNeutral)
            in
                codeICode{icode=rest, state=NormalState startState, code=condJump @ [testIt] @ code,
                                      context={forwardLabels=(label, startState, branchLabel) :: forwardLabels, loopArgs=loopArgs, handlers=handlers}}
            end

        |   codeICode{icode=JumpOnWordComparison{arg1=PReg testR1, arg2=PReg testR2, condition, label} :: rest,
                      state=NormalState state, code, context={forwardLabels, loopArgs, handlers}} =
            let
                (* The second argument should be on the top of the stack and in rax. *)
                val startState = checkTopInRax(state, testR2)
                val secondArg =
                    case findEntry (testR1, state) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: JumpOnWordComparison"
                val testIt = ArithRMem{ opc=CMP, output=eax, offset=(stackPtr state - secondArg - 1) * wordSize, base=esp }
                val (condJump, branchLabel) = condBranch(condition, PredictNeutral)
            in
                codeICode{icode=rest, state=NormalState startState, code=condJump @ [testIt] @ code,
                                      context={forwardLabels=(label, startState, branchLabel) :: forwardLabels, loopArgs=loopArgs, handlers=handlers}}
            end
            
        |   codeICode{icode=JumpOnWordComparison _ :: _, ...} =
                raise InternalError "codeICode: JumpOnWordComparison TODO"

        |   codeICode{icode=JumpOnTag{arg, onShort, label} :: rest,
                      state=NormalState state, code, context={forwardLabels, loopArgs, handlers}} =
            let
                val argReg = argToPReg arg
                (* The value on the top of the stack should be in rax and the value we want to test. *)
                val startState = checkTopInRax(state, argReg)
                val testIt = TestTagR eax
                val (condJump, branchLabel) = condBranch(if onShort then JNE else JE, PredictNeutral)
            in
                codeICode{icode=rest, state=NormalState startState, code=condJump @ [testIt] @ code,
                                      context={forwardLabels=(label, startState, branchLabel) :: forwardLabels, loopArgs=loopArgs, handlers=handlers}}
            end

            (* Set up an exception handler. *)
        |   codeICode{icode=PushExceptionHandler{ handlerAddr, handleStart } :: rest, state=NormalState state, code, context={forwardLabels, loopArgs, handlers}} =
            let
                val sHandle = argToPReg handlerAddr
                val (pushedState, pushCode) = pushRax state
                val sp = stackPtr pushedState
                val labelRef = ref addrZero (* Will be set to the address of the start of the handler. *)
                val thisHandler = (handleStart, pushedState, labelRef)
                (* Set up the handler by pushing the old handler to the stack, pushing the
                   entry point and setting the handler address to the current stack pointer. *)
                val setHandler =
                    [
                        StoreRegToMemory{
                            toStore=esp, address=BaseOffset{offset=memRegHandlerRegister, base=ebp, index=NoIndex}},
                        PushR eax,
                        LoadHandlerAddress{ handlerLab=labelRef, output=eax},
                        PushMem{base=ebp, offset=memRegHandlerRegister}
                    ]
                val startState = {valueStack={preg=sHandle, loc=OnStack(sp+1) (* Two words *)} :: #valueStack pushedState}
            in
                codeICode{icode=rest, state=NormalState startState, code=setHandler @ pushCode @ code,
                                      context={forwardLabels=forwardLabels, loopArgs=loopArgs, handlers=thisHandler :: handlers}}
            end

            (* Pop an exception handler at the end of a handled section.  Executed if no exception has been raised. *)
        |   codeICode{icode=PopExceptionHandler{ handlerAddr, resultReg } :: rest, state=NormalState state, code, context} =
            let
                val resReg = argToPReg resultReg and handleAddr = argToPReg handlerAddr
                (* handlerStartState is the state BEFORE we pushed the handler. *)
                (* Search down the state until we find the handler. *)
                fun popState [] = raise InternalError "codeICode: PopExceptionHandler - not found"
                |   popState (hState as ({preg, ...} :: tail)) =
                        if preg = handleAddr then hState else popState tail
                val handlerStartState = {valueStack=popState(#valueStack state)}
                (* Reset to just above the handler. *)
                val (_, resetCode) = resetStack(state, handlerStartState, resReg)
                val restoreHandle =
                    [
                        StoreRegToMemory{
                            toStore=ebx, address=BaseOffset{offset=memRegHandlerRegister, base=ebp, index=NoIndex}},
                        PopR ebx, ResetStack 1
                    ]
                (* The result state is after the handler has been popped. *)
                val resultState = addRaxToState({valueStack=tl(#valueStack handlerStartState)}, resReg)
            in
                codeICode{icode=rest, state=NormalState resultState, code=restoreHandle @ resetCode @ code, context=context}
            end

            (* MoveArgument - this is used for loads, stores and moves.  *)
            (* Create a new Preg containing the contents of another Preg.
               We only ever want the top item to be in RAX so we push the top of the stack. *)
        |   codeICode{icode=MoveArgument{source=PReg srcReg, dest=PReg destReg} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val load = loadSource(srcReg, eax, pushedState)
            in
                codeICode{
                    icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=load @ pushCode @ code, context=context}
            end

        |   codeICode{icode=MoveArgument{source=RealRegister(GenReg srcReg), dest=PReg destReg} :: rest, state=NormalState state, code, context} =
            (* Move a specific register to the stack.  If it is anything but RAX we push it immediately. *)
            let
                val (pushedState, pushCode) = pushRax state
            in
                if srcReg = eax
                then codeICode{
                        icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                        code=pushCode @ code, context=context}
                else
                let
                    val sp = stackPtr pushedState
                in
                    codeICode{
                        icode=rest, state=NormalState({valueStack={preg=destReg, loc=OnStack sp} :: #valueStack pushedState}),
                        code=PushR srcReg :: (pushCode @ code), context=context}
                end
            end
            
            (* Move a constant to a preg.  First push anything in RAX then load the constant into RAX. *)
        |   codeICode{icode=MoveArgument{source=AddressConstant srcValue, dest=PReg destReg} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                (* The constant should be an address. *)
                val _ = isShort srcValue andalso raise InternalError "AddressConstant: short"
            in
                codeICode{
                    icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=MoveLongConstR{source=srcValue, output=eax} :: (pushCode @ code),
                    context=context}
            end

            (* Move a short constant to a preg.  First push anything in RAX then load the constant into RAX. *)
        |   codeICode{icode=MoveArgument{source=IntegerConstant srcValue, dest=PReg destReg} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
            in
                codeICode{
                    icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=MoveConstR{source=srcValue, output=eax} :: (pushCode @ code),
                    context=context}
            end

            (* Load a value from a piece of memory, typically the offset in a tuple. *)
        |   codeICode{icode=MoveArgument{ source=MemoryLocation{offset, base}, dest=PReg destReg} :: rest, state=NormalState state, code, context} =
            let
                val bReg = argToPReg base
                (* Currently we always make a new preg for the base and that is
                   on the top of the state.  We can just remove it and replace it
                   with the result. *)
                val poppedState = checkTopInRax(state, bReg)
            in
                codeICode{icode=rest, state=NormalState(addRaxToState(poppedState, destReg)),
                    code=loadMemory(eax, eax, offset) @ code, context=context}
            end

            (* Store a pseudo-register in a memory location. *)
        |   codeICode{icode=MoveArgument{ source=PReg srcReg, dest=MemoryLocation{offset, base}} :: rest, state=NormalState state, code, context} =
            let
                val bReg = argToPReg base
                (* We currently generate these so that the top of the state is the allocated
                   memory and the next item to store is immediately below it. *)
                val newState =
                    case #valueStack state of
                        {preg=tReg, loc=InRax} :: {preg=nReg, loc=OnStack _} :: state =>
                            if tReg = bReg andalso nReg = srcReg
                            then {valueStack=state}
                            else raise InternalError "MoveArgument to MemoryLocation"
                    |   _ => raise InternalError "MoveArgument to MemoryLocation"
                (* Pop the value to store into ebx and store it in the tuple. *)
                val store = storeMemory(ebx, eax, offset) @ [PopR ebx]
            in
                codeICode{
                    (* We currently have the base in eax.  Leave it there because it may be used again. *)
                    icode=rest, state=NormalState(addRaxToState(newState, bReg)), code=store @ code, context=context}
            end

            (* Other combinations of move aren't used at the moment. *)
        |   codeICode{icode=MoveArgument _ :: _, ...} =
                raise InternalError "codeICode: MoveArgument"

            (* Load a value from the real stack.  Offset zero is the return address,
               subsequent values are arguments that were pushed by the caller. *)
        |   codeICode{icode=MoveStackArgToPreg{offset, dest} :: rest, state=NormalState state, code, context} =
            let
                val destReg = argToPReg dest
                val (pushedState, pushCode) = pushRax state
                val load = loadStack(eax, (stackPtr pushedState + offset) * wordSize)
            in
                codeICode{
                    icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=load @ pushCode @ code, context=context}
            end
            
        |   codeICode{icode=AllocateMemoryToPreg{ size, isMutable, dest} :: rest, state=NormalState state, code, context} =
            let
                val destReg = argToPReg dest
                val (pushedState, pushCode) = pushRax state
                val alloc = allocateHeap(size, if isMutable then Address.F_mutable else 0w0, eax)
            in
                codeICode{
                    icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=alloc @ pushCode @ code, context=context}
            end

            (* Marks the end of setting the fields of a tuple.  This is used in the next
               level to avoid reordering a new allocation until a previously allocated
               section of memory has been fully initialised. *)
        |   codeICode{icode=InitialisationComplete _ :: rest, state, code, context} =
                codeICode{icode=rest, state=state, code=StoreInitialised :: code, context=context}

            (* Function call. *)
        |   codeICode{icode=FunctionCall{ regArgs, stackArgs, dest, isTail=false} :: rest, state=NormalState state, code, context} =
            let
                val destReg = argToPReg dest
                val (pushedState, pushCode) = pushRax state
                (* We should find the stack arguments occupy the immediately preceding
                   entries on the stack.  If not we've got a problem. *)
                val numStackArgs = List.length stackArgs
                val currentSp = stackPtr pushedState

                fun checkStackArg ([], _) = ()
                |   checkStackArg (sa::args, num) =
                    let
                        val e = findEntry(argToPReg sa, pushedState)
                    in
                        case e of
                            InRax => raise InternalError "checkStackArg"
                        |   OnStack s => if s = num then () else raise InternalError "checkStackArg";
                        checkStackArg(args, num+1)
                    end
                val () = checkStackArg(stackArgs, currentSp-numStackArgs)
                
                (* Move the register arguments into the correct real registers. *)
                val doLoad = List.foldl (fn ((pr, gr), l) => l @ loadSource(argToPReg pr, gr, pushedState)) [] regArgs
                
                (* After the call the stack arguments will have been removed by the caller. *)
                (* We can just drop the stack args from the state.  We've checked them already.
                   Actually, we haven't checked that they were immediately preceding on this state list. *)
                val returnState = List.drop(#valueStack pushedState, numStackArgs)
                (* We haven't actually checked that the entries we're dropping really
                   are the register args. *)
                val poppedState = {valueStack=List.drop(returnState, List.length regArgs)}
                (* Add the result value in Rax to the state *)
                val (finalState, stackReset) = resetStack(addRaxToState({valueStack=returnState}, destReg), poppedState, destReg)
                val functionCode =
                    stackReset @ [CallFunction FullCall] @ doLoad @ pushCode @ code
            in
                codeICode{icode=rest, state=NormalState finalState, code=functionCode, context=context}
            end

            (* Jump to a function.  The arguments have to be moved into the space currently used
               by the arguments to this function.  That could result in values being overwritten
               so we have to save them, especially the return address. *)
        |   codeICode{icode=FunctionCall{ regArgs, stackArgs, dest= _, isTail=true} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val currentSp = stackPtr pushedState

                (* Load the earlier arguments into registers.  Do that first because
                   they're lower down the stack and we could overwrite them if we wait. *)
                (* Load until we've done all the arguments or all the registers whichever is earlier.
                   This includes the closure register. *)
                val doLoad = List.foldl (fn ((pr, gr), l) => l @ loadSource(argToPReg pr, gr, pushedState)) [] regArgs
                
                (* Load the return address.  This could be overwritten by an argument. *)
                val loadReturn = loadStack(edi, currentSp * wordSize)

                (* Move the stack arguments into the space occupied by this function's args. *)
                val numArgsToMove = List.length stackArgs
                val argOffset = currentSp + currentStackArgs - numArgsToMove
                fun copyArgs (_, nil) = []
                |   copyArgs (n, r :: rl) =
                        (* N.B. This is reversed *)
                        copyArgs(n-1, rl) @ storeMemory(esi, esp, (argOffset+n)*wordSize) @ loadSource(argToPReg r, esi, pushedState)
                val moveArgs = copyArgs(numArgsToMove, stackArgs)
                val storeReturn = storeMemory(edi, esp, argOffset*wordSize)

                val functionCode =
                    JumpToFunction FullCall :: ResetStack argOffset ::
                        (storeReturn @ moveArgs @ loadReturn @ doLoad @ pushCode @ code)
            in
                codeICode{icode=rest, state=Exited, code=functionCode, context=context}
            end

            (* Start of a loop.  The loop body contains one or more tail jumps to the start. *)
        |   codeICode{icode=StartLoop{arguments, loopLabel} :: rest, state=NormalState state, code,
                      context={forwardLabels, handlers, loopArgs}} =
            let
                (* The arguments have been evaluated to the stack.  Replace
                   the preg entries in the state with those of the loop variables. *)
                val (pushedState, pushCode) = pushRax state
                val numArgs = List.length arguments
                val headList = List.take(#valueStack pushedState, numArgs)
                fun repSourceWithDest({source, loopReg}, {preg, loc}) =
                     if argToPReg source = preg then {preg=argToPReg loopReg, loc=loc} else raise InternalError "codeICode: StartLoop"
                val newHead = ListPair.mapEq repSourceWithDest (List.rev arguments, headList)
                val tailList = List.drop(#valueStack pushedState, numArgs)
                val bodyState = {valueStack=newHead @ tailList}
                val (startLoopCode, startLoop) = backJumpLabel()
            in
                codeICode{icode=rest, state=NormalState bodyState, code=startLoopCode @ pushCode @ code,
                          context={loopArgs=(bodyState, startLoop, loopLabel) :: loopArgs, forwardLabels=forwardLabels, handlers=handlers}}
            end

            (* A loop tail jump.  The arguments have been evaluated to the stack. *)
        |   codeICode{icode=JumpLoop{arguments, loopLabel} :: rest, state=NormalState state, code,
                      context=context as {loopArgs=(startState, startLoop, lab) :: _, ...}} =
            let
                val _ = loopLabel = lab orelse raise InternalError "codeICode: JumpLoop - mismatched labels"
                (* Move each argument from the source to the loop variable.  This isn't the same as
                   MoveArgument because the destination already exists. *)
                fun moveArg{source, loopReg} =
                let
                    val load = loadSource(argToPReg source, eax, state)
                    val store =
                        case findEntry (argToPReg loopReg, state) of
                            OnStack s => storeMemory(eax, esp, (stackPtr state - s - 1) * wordSize)
                        |   InRax => raise InternalError "codeICode: JumpLoop"
                in
                    store @ load
                end
                val storeAll = List.foldl(fn (arg, l) => l @ moveArg arg) [] arguments
                val resetCode = [ResetStack(stackPtr state - stackPtr startState)]
            in
                codeICode{icode=rest, state=Exited,
                          code=jumpBackwards startLoop @ resetCode @ storeAll @ code, context=context}
            end

        |   codeICode{icode=JumpLoop _ :: _, ...} =
                raise InternalError "codeICode: JumpLoop out of loop"

            (* Raise an exception using the value in the "packet" register as the exception packet.
               Since this exits we can ignore any further code and return the code and state. *)
        |   codeICode{icode=RaiseExceptionPacket{packet} :: rest, state=NormalState state, code, context} =
            let
                val pktReg = argToPReg packet
                val load = loadSource(pktReg, eax, state)
            in
                codeICode{icode=rest, state=Exited, code=RaiseException :: (load @ code), context=context}
            end

            (* Reserve a contiguous area of memory on the stack.  This is used to receive the
               results of a tuple.  It may be set within the same function or the address
               may be passed into another function to allow it to return multiple results. *)
        |   codeICode{icode=ReserveContainer{size, address} :: rest, state=NormalState state, code, context} =
            let
                val addrReg = argToPReg address
                val (pushedState, pushCode) = pushRax state
                (* The memory must be cleared in case we have a GC. *)
                val pushAll = List.tabulate(size, fn _ => PushConst(tag 0))
                val sp = stackPtr pushedState
            in
                codeICode{icode=rest,
                    state=NormalState({valueStack={loc=InRax, preg=addrReg} :: {loc=OnStack(sp+size-1), preg= ~1} :: #valueStack pushedState}),
                    code=MoveRR{source=esp, output=eax} :: (pushAll @ pushCode @ code), context=context}
            end

        |   codeICode{icode=IndexedCaseOperation{ testReg, workReg=_, default, cases, exhaustive, min, max, arbitrary} :: rest,
                      state=NormalState state, code, context={loopArgs, handlers, forwardLabels}} =
            let
                val tReg = argToPReg testReg
                (* We should have the value to test at the top of the stack. *)
                val startState = checkTopInRax(state, tReg)

                val testReg=eax and workReg=ebx
                val defaultLabel as Labels{uses=defUses, ...} = mkLabel()
                fun makeLab _ =
                let
                    val lab as Labels{uses, ...} = mkLabel()
                in
                    uses := 1;
                    lab
                end
                val caseLabels = List.map makeLab cases
                val testCode =
                    if exhaustive
                    then []
                    else
                    let
                        (* If this is an arbitrary precision int we need to check it's short
                           and go to the default if it isn't. *)
                        val testTag =
                            if arbitrary
                            then
                            (
                                defUses := 3;
                                [ConditionalBranch{test=JE, predict=PredictNotTaken, label=defaultLabel},
                                 TestTagR testReg]
                            )
                            else (defUses := 2; [])
                        (* Range checks. *)
                        val rangeChecks =
                            [ConditionalBranch{test=JA, predict=PredictNotTaken, label=defaultLabel},
                             ArithRConst{ opc=CMP, output=testReg, source=tag(Word.toLargeInt max) },
                             ConditionalBranch{test=JB, predict=PredictNotTaken, label=defaultLabel},
                             ArithRConst{ opc=CMP, output=testReg, source=tag(Word.toLargeInt min) }]
                    in
                        rangeChecks @ testTag 
                    end
                val indexCaseInstr = IndexedCase{testReg=testReg, workReg=workReg, min=min, cases=caseLabels} :: testCode

                (* This behaves rather like an unconditional branch except that it can go to several places.
                   The case labels and the default label have to be added to the label list. *)
                val labelsFromCase =
                    (default, startState, defaultLabel) ::
                        ListPair.mapEq(fn ((ilab, _), clab) => (ilab, startState, clab)) (cases, caseLabels)
                        @ forwardLabels
            in
                codeICode{icode=rest, state=Exited, code=indexCaseInstr @ code,
                        context={loopArgs=loopArgs, handlers=handlers, forwardLabels=labelsFromCase}}
            end

            (* Lock a mutable segment. *)
        |   codeICode{icode=LockMutable{ addr } :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val load = loadSource(argToPReg addr, eax, pushedState)
                val lockCode = LockMutableSegment eax :: (load @ pushCode) @ code
            in
                codeICode{icode=rest, state=NormalState pushedState, code=lockCode, context=context}
            end

       |    codeICode{icode=ReturnResultFromFunction{ resultReg, numStackArgs } :: rest, state=NormalState state, code, context} =
            let
                val resReg = argToPReg resultReg
                val (_, resetCode) = resetStack(state, initialState, resReg)
                val returnCode = ReturnFromFunction numStackArgs :: (resetCode @ code)
            in
                (* We may have more than one return point so we can't just stop here. *)
                codeICode{icode=rest, state=Exited, code=returnCode, context=context}
            end

        |   codeICode{icode=ArithmeticFunction{ oper, resultReg, operand1, operand2 } :: rest, state=NormalState state, code, context} =
            let
                val resReg = argToPReg resultReg
                and oper1Reg = argToPReg operand1
                val op2Value = case operand2 of IntegerConstant v => v | _ => raise InternalError "TODO: ArithmeticFunction"
                val topState = checkTopInRax(state, oper1Reg)
                val resultState = addRaxToState(topState, resReg)
                val resCode = ArithRConst { opc=oper, output=eax, source=op2Value } :: code
            in
                codeICode{icode=rest, state=NormalState resultState, code=resCode, context=context}
            end

        val (fnState, fnCode) = codeICode{icode=icode, state=NormalState initialState, code=[], context={loopArgs=[], forwardLabels=[], handlers=[]}}
        val _ = case fnState of Exited => () | _ => raise InternalError "fnstate not exited"

        (* Finally reverse the code. *)
        val ops = List.rev fnCode 

        val stackRequired = !maxStack

        (* The stack limit register is set at least twice this far from the
           end of the stack so we can simply compare the stack pointer with
           the stack limit register if we need less than this much. Setting
           it at twice this value means that functions that use up to this
           much stack and do not call any other functions do not need to
           check the stack at all. *)
        (* TODO: The only functions that don't check the stack are RTS functions
           and any hand-coded functions. *)
        val minStackCheck = 20
        
        (* Adds the constants onto the code, and copies the code into a new segment *)
        (* Prelude consists of stack checking code. *)
        fun testRegAndTrap(reg, entryPt) =
        let
            (* If we need to take the trap we save the argument and closure registers
               across the trap. *)
            val saveRegs = argRegsUsed @ (if hasFullClosure then [edx] else [])
            fun pushThenPop [] = [CallRTS entryPt]
            |   pushThenPop (aReg::regs) = PushR aReg :: (pushThenPop regs @ [PopR aReg])
            (* Normally we won't have a stack overflow so we will skip the check. *)
            val (skipCheck, skipCheckLab) = condBranch(JNB, PredictTaken)
        in
            [ArithRMem{ opc=CMP, output=reg, offset=memRegStackLimit, base=ebp }] @
            skipCheck @ pushThenPop saveRegs @ forwardJumpLabel skipCheckLab
        end
        val preludeCode =
            if stackRequired >= minStackCheck
            then
            let
                (* Compute the necessary amount in edi and compare that. *)
                val stackByteAdjust = ~wordSize * stackRequired
                val testEdiCode =
                    testRegAndTrap (edi, memRegStackOverflowCallEx)
            in
                 [LoadAddress{output=edi, base=SOME esp, index=NoIndex, offset=stackByteAdjust}] @ testEdiCode
            end
     
            else testRegAndTrap (esp, memRegStackOverflowCall)
        val newCode = codeCreate (functionName, profileObject, debugSwitches) 
    in
        createCodeSegment(X86OPTIMISE.optimise(newCode, preludeCode @ ops), newCode)
    end

    structure Sharing =
    struct
        type genReg         = genReg
        and  argument       = argument
        and  iLabel         = iLabel
        and  x86ICode       = x86ICode
        and  branchOps      = branchOps
    end

end;
