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

functor X86ICodeTransform(
    structure ICODE: ICodeSig
    structure DEBUG: DEBUGSIG
    structure IDENTIFY: X86IDENTIFYREFSSIG
    structure PRETTY: PRETTYSIG
    sharing ICODE.Sharing = IDENTIFY.Sharing
) : X86ICODETRANSFORMSIG
=
struct
    open ICODE
    open Address
    open IDENTIFY

    val totalFns = ref 0 and successFns = ref 0
    val notDoneInstr = ref 0 and tooManyRegs = ref 0 and mustPushReg = ref 0 and anOther = ref 0
    
    exception InternalError = Misc.InternalError

    (* tag a short constant *)
    fun tag c = 2 * c + 1

    fun argAsPReg(RegisterArgument(PReg(n, PRegGeneral))) = n | argAsPReg _ = raise InternalError "argAsPReg"
    and argAsUReg(RegisterArgument(PReg(n, PRegUntagged))) = n | argAsUReg _ = raise InternalError "argAsUReg"
    
    fun pRegNum(PReg(n, PRegGeneral)) = n | pRegNum _ = raise InternalError "pRegNum"
    and uRegNum(PReg(n, PRegUntagged)) = n | uRegNum _ = raise InternalError "uRegNum"

    fun loadMemory(reg, base, offset) =
        [LoadArgument{
            source=MemoryLocation{base=(GenReg base), offset=offset, index=NoMemIndex},
            dest=GenReg reg, kind=MoveWord}]
    and loadStack(reg, offset) = loadMemory(reg, esp, offset)
    and storeMemory(reg, base, offset) =
        [StoreArgument{
            source=RegisterArgument(GenReg reg),
            base=(GenReg base), offset=offset, index=NoMemIndex, kind=MoveWord}]

    val raxAsArg = (GenReg eax) and rbxAsArg = (GenReg ebx)
    and rcxAsArg = (GenReg ecx) and rsiAsArg = (GenReg esi)
    and rdxAsArg = (GenReg edx) and rdiAsArg = (GenReg edi)
    
    val generalRegisters =
        List.map GenReg
        (if isX64
        then [r14, r13, r12, r11, r10, r9, r8, edi, esi, edx, ecx, ebx, eax]
        else [edi, esi, edx, ecx, ebx, eax])

    (* Generally we have an offset in words and no index register. *)
    fun wordOffsetAddress(offset, baseReg) =
        MemoryLocation{offset=offset*wordSize, base=(GenReg baseReg), index=NoMemIndex}

    (* Transform the version of the ICode generated from the codetree by replacing the pseudo-registers by
       real locations.  When this is complete it passes it on to the next phase for code-generation.
       This is currently very simple-minded and just uses rax as an accumulator and the
       stack for the rest. *)

    datatype pregLoc = InRax | OnStack of int
    
    (* The state of the condition codes. *)
    datatype ccState = CCIndeterminate | CCState of ccRef * (branchOps->branchOps)

    datatype state = NormalState of normalState | Exited
    withtype normalState =
        { valueStack : {preg: int, loc: pregLoc} list, ccState: ccState, untaggedRegs: {ureg: int, reg: reg} list }
    
    val initialState:normalState =
        {valueStack=[], ccState=CCIndeterminate, untaggedRegs=[]} (* Initial and final state. *)

    (* Return the location on the real stack immediately above the top item. *)
    local
        fun stackP [] = 0
        |   stackP ({loc=OnStack sp, ...} :: _) = sp+1
        |   stackP (_ :: stack) = stackP stack
    in
        fun stackPtr({valueStack, ...}: normalState) = stackP valueStack
    end

    local
        fun findE (_, []) = raise InternalError "findEntry: not found"
        |   findE (reg, {preg, loc} :: rest) = if reg = preg then loc else findE(reg, rest)
    in
        fun findEntry(reg, {valueStack, ...}) = findE(reg, valueStack)
    end

    fun loadSource(RegisterArgument reg, dest, state) = loadRegister(reg: abstract, dest, state)

    |   loadSource(AddressConstant a, dest, _) =
            [LoadArgument{ source=AddressConstant a, dest=GenReg dest, kind=MoveWord}]
    |   loadSource(IntegerConstant i, dest, _) =
            [LoadArgument{ source=IntegerConstant i, dest=GenReg dest, kind=MoveWord}]

    |   loadSource(MemoryLocation{ base, offset, index=NoMemIndex }, dest, state) =
            (* Use the destination register as a base address. *)
            LoadArgument {
                source=MemoryLocation {base=(GenReg dest), offset=offset, index=NoMemIndex },
                dest=GenReg dest, kind=MoveWord} ::
                    loadRegister(base, dest, state)

    |   loadSource _ = raise InternalError "loadSource"
    
    and loadRegister(PReg(src, PRegGeneral), dest, state) =
    (
        case findEntry (src, state) of
            InRax =>
                if dest = eax
                then []
                else [LoadArgument{ source=RegisterArgument(GenReg eax), dest=GenReg dest, kind=MoveWord}]
        |   OnStack s => loadStack(dest, (stackPtr state - s - 1) * wordSize)
    )
    |   loadRegister _ = raise InternalError "loadRegister"

    val loadSource =
        fn(arg, dest, state as {valueStack={preg=_, loc=InRax } :: _, ...}) =>
            if dest = eax then raise InternalError "loadSource: eax" else loadSource(arg, dest, state)
        |  (arg, dest, state) => loadSource(arg, dest, state)
   
    fun clearCC({valueStack, untaggedRegs, ...}:normalState): normalState =
        {valueStack=valueStack, ccState=CCIndeterminate, untaggedRegs=untaggedRegs}

    (* After we use an untagged value remove it from the state.  We only have
       a few registers and only use at most two untagged values before tagging them.
       For now, if we tag a value we clear the untagged set.  Division returns two
       tagged values but only one is actually used. *)
    fun removeAfterTag({valueStack, ccState, ...}, _) =
        {valueStack=valueStack, ccState=ccState, untaggedRegs=[]}

    fun codeICodeFunctionToX86{icode, functionName, maxLabels, maxPRegs, argRegsUsed, hasFullClosure, currentStackArgs, debugSwitches} =
    let
        (* We may need to create additional labels. *)
        val labelCounter = ref maxLabels
        fun newLabel() = ILabel(!labelCounter) before labelCounter := !labelCounter + 1

        val maxStack = ref 0 (* High-water mark *)

        (* If the top of the stack is in RAX push it otherwise do nothing. *)
        fun pushRax (state as {valueStack={preg, loc=InRax} :: stack, ccState, untaggedRegs }) =
            let
                val sp = stackPtr state
                val () = if sp >= !maxStack then maxStack := sp+1 else ()
            in
                (* Return the updated state with the value pushed.  Pushing does not affect condition codes. *)
                ({valueStack={preg=preg, loc=OnStack sp} :: stack, ccState=ccState, untaggedRegs=untaggedRegs},
                    [PushValue{arg=RegisterArgument raxAsArg}])
            end
        |   pushRax stack = (stack, [])

        (* Add a pseudo-register containing rax to the top of the stack. *)
        fun addRaxToState({valueStack, ccState, untaggedRegs}, destReg) =
            {valueStack={preg=destReg, loc=InRax} :: valueStack, ccState=ccState, untaggedRegs=untaggedRegs}

        (* Check that the top of the stack is the required pseudo-register and the value is in Rax.
           Returns the state with this item popped. *)
        fun checkTopInRax({valueStack={preg, loc=InRax } :: resultState, ccState, untaggedRegs}, checkReg) =
            if preg = checkReg
            then {valueStack=resultState, ccState=ccState, untaggedRegs=untaggedRegs}
            else raise InternalError "checkTopInRax"
        |   checkTopInRax _ = raise InternalError "checkTopInRax"

        fun loadToRax(state, arg) =
        let
            val (pushedState, pushCode) = pushRax state
            val load = loadSource(arg, eax, pushedState)
        in
            (pushedState, load @ pushCode)
        end
 
        (* Reset the stack to value in the original state except that
           the target register should be moved into the result state. *)
        fun resetStack(state, oldState, target) =
        let
            (* Check that the target is on the top of the stack and in rax. *)
            val _ = checkTopInRax(state, target)
            (* Check that the top of the old state is not in a register. *)
            val () =
                case oldState of
                    {valueStack={loc=InRax, ...} :: _, ...} => raise InternalError "resetStack"
                |   _ => ()
            val currentSp = stackPtr state
            and oldSp = stackPtr oldState
            val newState = addRaxToState(oldState, target)
            val code =
                if currentSp = oldSp then [] else [resetStackPtr(currentSp - oldSp)]
        in
            (* Currently ResetStackPtr uses arithmetic operations that affect the condition codes. *)
            (clearCC newState, code)
        end
        
        and resetStackPtr numWords =
            if numWords < 0 then raise InternalError "resetStackPtr"
            else ResetStackPtr{numWords=numWords}

        (* Find a real register for an untagged pseudo-register value. *)
        fun getUreg(iReg, {untaggedRegs, ...}: normalState) =
            case List.find (fn {ureg, ...} => ureg = iReg) untaggedRegs of
                SOME {reg as GenReg _, ...} => reg
            |   _ => raise InternalError "getUreg"

        fun getFPreg(iReg, {untaggedRegs, ...}: normalState) =
            case List.find (fn {ureg, ...} => ureg = iReg) untaggedRegs of
                SOME {reg as XMMReg _, ...} => reg
            |   SOME {reg as FPReg _, ...} => reg
            |   _ => raise InternalError "getFPreg"

        (* Get a new untagged register.  We only handle three at the moment. *)
        fun newUreg (iReg, {valueStack, untaggedRegs, ccState}: normalState) =
        let
            val newReg =
                case untaggedRegs of
                    [] => edx
                |   [_] => ecx
                |   [_, _] => esi
                |   _ => raise InternalError "newUreg"
        in
            ({valueStack=valueStack, ccState=ccState, untaggedRegs={ureg=iReg, reg=GenReg newReg}::untaggedRegs}, (GenReg newReg))
        end

        (* Process the code until the end.  The code list is built up in reverse order. *)
        (* If we reach the end we haven't exited or reached a JumpLoop so we just return the
           final state. *)
        fun codeICode{icode=[]: abstract x86ICode list, state: state, code: reg x86ICode list, ...} = (state, code: reg x86ICode list)

            (* Destination of a forward jump.  If the state was Exited this can
               continue.
               We should have the label in the list.
               We need to merge the states associated with dropping through and the
               state associated with the branch.  We can only adjust the state on the
               "drop through" side so if we need to change the state on the branch
               side we have to create a new branch. *)
        |   codeICode{icode=(ForwardJumpLabel{label, result}: abstract x86ICode) :: rest, state, code: reg x86ICode list, context as {forwardLabels, loopArgs, handlers}} =
            let
                (* Find the label in the list.  Remove it if it's there.  It may not be if
                   we never used it.  That could happen if we have a "drop-through" case in
                   an andalso/orelse or if we didn't generate a jump because we raised an
                   exception or made a tail-jump. *)
                fun findAndRemoveLabel(label: iLabel, (hd as (l, _)) :: tl) =
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

                |   (Exited, SOME(_, labelState)) =>
                        (* There is no drop-through to consider because the preceding instruction
                           was an unconditional jump or something similar.  We can just set the state
                           and continue. *)
                        codeICode{icode=rest, state=NormalState labelState, code=ForwardJumpLabel{label=label, result=NONE} :: code,
                                  context={forwardLabels=newLabels, loopArgs=loopArgs, handlers=handlers}}

                |   (NormalState cstate, SOME(_, labelState)) =>
                    (* There was a drop-through.  Merge the states.  We can only actually change the
                       drop-through state i.e. before the label.  If the state on the incoming branch
                       needs to be changed we have to add an extra branch. *)
                    let
                        (* We should have the result register on both arms. *)
                        fun removeResult s =
                            case result of
                                NONE => s
                            |   SOME resArg => checkTopInRax(s, pRegNum resArg)
                            
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
                            {valueStack=List.rev(matchStates(List.rev (#valueStack dropThrough), List.rev (#valueStack labState))),
                             ccState=CCIndeterminate, untaggedRegs= []}
                        (* Currently the only thing we need to adjust is the stack pointer. *)
                        val finalSp = stackPtr composite
                        and labelSp = stackPtr labelState

                        val (finalState, adjustSpOnDropThrough) =
                            case result of
                                NONE =>
                                let
                                    val currentSp = stackPtr cstate and finalSp = stackPtr composite
                                in
                                    (composite, if currentSp = finalSp then [] else [resetStackPtr(currentSp-finalSp)])
                                end
                            |   SOME resReg => resetStack(cstate, composite, pRegNum resReg)
                    in
                        if labelSp = finalSp
                        then (* We don't need to do anything to the incoming branch. *)
                        let
                            val newContext = {forwardLabels=newLabels, loopArgs=loopArgs, handlers=handlers}
                        in
                            codeICode{icode=rest, state=NormalState finalState,
                                      code=ForwardJumpLabel{label=label, result=NONE} :: adjustSpOnDropThrough @ code,
                                      context=newContext}
                        end
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
                icode=(BeginHandler{handleStart, packetReg, ...}) :: rest, state=Exited, code,
                context={forwardLabels, loopArgs, handlers=(handleLab, handlerStartState) :: otherHandlers}} =
            let
                val _ = handleLab = handleStart orelse raise InternalError "codeICode: BeginHandler handler stack mismatch"
                val resultState = addRaxToState(handlerStartState, pRegNum packetReg)
            in
                codeICode{icode=rest, state=NormalState resultState,
                        code=BeginHandler{handleStart=handleStart, workReg=rbxAsArg, packetReg=raxAsArg} :: code,
                        context={forwardLabels=forwardLabels, loopArgs=loopArgs, handlers=otherHandlers}}
            end

        |   codeICode{icode=BeginHandler _ :: _, ...} =
                raise InternalError "codeICode: BeginHandler not preceded by jump or not in handler"
                
        |   codeICode{icode=EndLoop{loopLabel} :: rest, state, code, context={forwardLabels, loopArgs=(_, lab)::otherLoops, handlers}} =
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

        |   codeICode{icode=(UnconditionalForwardJump{label}) :: rest, state=NormalState state, code, context={forwardLabels, loopArgs, handlers}} =
                codeICode{icode=rest, state=Exited, code=UnconditionalForwardJump{label=label} :: code,
                                      context={forwardLabels=(label, state) :: forwardLabels, loopArgs=loopArgs, handlers=handlers}}

        |   codeICode{icode=ConditionalForwardJump{ccRef, condition, label} :: rest,
                      state as NormalState labelState, code, context={forwardLabels, loopArgs, handlers}} =
            let
                (* Get the condition code.  It should be the state we set but we may have to change the
                   test if that wasn't the original. *)
                val testCondition =
                    case labelState of
                        {ccState=CCState(currentRef, condOp), ...} =>
                            if currentRef = ccRef then condOp condition
                            else raise InternalError "codeICode: condition ref incorrect"
                    |   _ => raise InternalError "codeICode: condition ref unset"
            in
                codeICode{icode=rest, state=state, code=ConditionalForwardJump{ ccRef=ccRef, condition=testCondition, label=label } :: code,
                                      context={forwardLabels=(label, labelState) :: forwardLabels, loopArgs=loopArgs, handlers=handlers}}
            end

            (* Word comparison. *)
        |   codeICode{icode=WordComparison{arg1 as RegisterArgument(PReg(_, PRegGeneral)), arg2=IntegerConstant i, ccRef} :: rest,
                      state=NormalState state, code, context} =
            let
                (* The value on the top of the stack should be in rax and the value we want to test. *)
                val (startState, loadCode) = loadToRax(state, arg1)
                (* The resulting state contains the condition we want to test. *)
                val checkState =
                    {valueStack= #valueStack startState, ccState=CCState(ccRef, fn i=>i), untaggedRegs= []}
            in
                codeICode{icode=rest, state=NormalState checkState,
                          code=WordComparison{ arg1=RegisterArgument raxAsArg, arg2=IntegerConstant i, ccRef=ccRef } :: loadCode @ code, context=context}
            end

        |   codeICode{icode=WordComparison{arg1 as RegisterArgument(PReg(_, PRegGeneral)), arg2=AddressConstant a, ccRef} :: rest,
                      state=NormalState state, code, context} =
            let
                (* The value on the top of the stack should be in rax and the value we want to test. *)
                val (startState, loadCode) = loadToRax(state, arg1)
                val checkState = {valueStack= #valueStack startState, ccState=CCState(ccRef, fn i=>i), untaggedRegs= []}
            in
                codeICode{icode=rest, state=NormalState checkState,
                          code=WordComparison{ arg1=RegisterArgument raxAsArg, arg2=AddressConstant a, ccRef=ccRef } :: loadCode @ code,
                          context=context}
            end

        |   codeICode{icode=WordComparison{arg1=RegisterArgument(PReg(testR1, PRegGeneral)), arg2 as RegisterArgument(PReg(_, PRegGeneral)), ccRef} :: rest,
                      state=NormalState state, code, context=context} =
            let
                (* The second argument should be on the top of the stack and in rax. *)
                val (startState, loadCode) = loadToRax(state, arg2)
                val secondArg =
                    case findEntry (testR1, startState) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: JumpOnWordComparison"
                (* We want the first argument in the register so that means the condition codes
                   we will test are reversed.. *)
                fun revCond JA  = JB
                |   revCond JNA = JNB
                |   revCond JB  = JA
                |   revCond JNB = JNA
                |   revCond JL  = JG
                |   revCond JLE = JGE
                |   revCond JG  = JL
                |   revCond JGE = JLE
                |   revCond t   = t
                val checkState = {valueStack= #valueStack startState, ccState=CCState(ccRef, revCond), untaggedRegs= []}
            in
                codeICode{icode=rest, state=NormalState checkState,
                          code=WordComparison{arg1=RegisterArgument raxAsArg, arg2=wordOffsetAddress(stackPtr startState - secondArg - 1, esp), ccRef=ccRef} ::
                               loadCode @ code,
                          context=context}
            end

        |   codeICode{
                icode=WordComparison{
                    arg1=RegisterArgument(PReg(arg1Reg, PRegUntagged)),
                    arg2=MemoryLocation{offset=offset2, base=bReg2, index=NoMemIndex}, ccRef} :: rest,
                state=NormalState state, code, context=context} =
                (* This is used for LargeWord comparisons. *)
            let
                val ({ valueStack, untaggedRegs, ...}, loadArg2Base) =
                    loadToRax(state, RegisterArgument bReg2)
                val realArgReg =
                    case List.find (fn {ureg, ...} => ureg = arg1Reg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: WordComparison"
                val resultState = {valueStack= valueStack, ccState=CCState(ccRef, fn i=>i), untaggedRegs= []}
            in
                codeICode{icode=rest, state=NormalState resultState,
                          code=
                            WordComparison{arg1=RegisterArgument realArgReg,
                                           arg2=MemoryLocation{offset=offset2, base=raxAsArg, index=NoMemIndex}, ccRef=ccRef} ::
                                loadArg2Base @ code,
                          context=context}
            end
            
        |   codeICode{icode=WordComparison _ :: _, ...} =
                raise InternalError "codeICode: JumpOnWordComparison TODO"

        |   codeICode{icode=TestTagBit{arg, ccRef} :: rest, state=NormalState state, code, context} =
            let
                (* The value on the top of the stack should be in rax and the value we want to test. *)
                val (startState, loadCode) = loadToRax(state, arg)
                val checkState = {valueStack= #valueStack startState, ccState=CCState(ccRef, fn i=>i), untaggedRegs= []}
            in
                codeICode{icode=rest, state=NormalState checkState,
                    (* TestTagBit only sets the condition codes.  It doesn't affect the register. *)
                          code=TestTagBit{arg=RegisterArgument raxAsArg, ccRef=ccRef} :: loadCode @ code, context=context}
            end

            (* Set up an exception handler. *)
        |   codeICode{icode=PushExceptionHandler{ handlerAddr, handleStart } :: rest, state=NormalState state, code, context={forwardLabels, loopArgs, handlers}} =
            let
                val sHandle = pRegNum handlerAddr
                val (pushedState, pushCode) = pushRax state
                val sp = stackPtr pushedState
                val thisHandler = (handleStart, pushedState)
                val startState =
                    {valueStack={preg=sHandle, loc=OnStack(sp+1) (* Two words *)} :: #valueStack pushedState,
                     ccState=CCIndeterminate, untaggedRegs= []}
                val newContext = {forwardLabels=forwardLabels, loopArgs=loopArgs, handlers=thisHandler :: handlers}
            in
                codeICode{icode=rest, state=NormalState startState,
                          code=PushExceptionHandler{handlerAddr=raxAsArg, handleStart=handleStart} :: pushCode @ code, context=newContext}
            end

            (* Pop an exception handler at the end of a handled section.  Executed if no exception has been raised. *)
        |   codeICode{icode=PopExceptionHandler{ handlerAddr, resultReg, workReg=_ } :: rest, state=NormalState state, code, context} =
            let
                val handleAddr = pRegNum handlerAddr
                (* handlerStartState is the state BEFORE we pushed the handler. *)
                (* Search down the state until we find the handler. *)
                fun popState [] = raise InternalError "codeICode: PopExceptionHandler - not found"
                |   popState (hState as ({preg, ...} :: tail)) =
                        if preg = handleAddr then hState else popState tail
                val handlerStartState = {valueStack=popState(#valueStack state), ccState=CCIndeterminate, untaggedRegs= []}
                (* Reset to just above the handler.
                   The result state is after the handler has been popped. *)
                val postPopState = {valueStack=tl(#valueStack handlerStartState), ccState=CCIndeterminate, untaggedRegs= []}
                val (resultState, resetCode) =
                    case resultReg of
                        SOME(PReg(resReg, _)) =>
                        let
                            val (_, resetCode) = resetStack(state, handlerStartState, resReg)
                            val resultState = addRaxToState(postPopState, resReg)
                        in
                            (resultState, resetCode)
                        end
                    |   NONE => (postPopState, [resetStackPtr(stackPtr state - stackPtr handlerStartState)])
                val workReg = (GenReg ebx) (* Must be different from the return register. *)
            in
                codeICode{icode=rest, state=NormalState resultState,
                          code=PopExceptionHandler{ handlerAddr=raxAsArg (* unused*), resultReg=NONE (* unused*), workReg=workReg } :: resetCode @ code,
                          context=context}
            end

            (* Create a new Preg containing the contents of another Preg.
               We only ever want the top item to be in RAX so we push the top of the stack. *)
        |   codeICode{
                icode=LoadArgument{source as RegisterArgument(PReg(_, PRegGeneral)), dest=PReg(destReg, PRegGeneral), kind=MoveWord} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val load = loadSource(source, eax, pushedState)
            in
                codeICode{
                    icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=load @ pushCode @ code, context=context}
            end
            
            (* Move a constant to a preg.  First push anything in RAX then load the constant into RAX. *)
        |   codeICode{icode=LoadArgument{source=AddressConstant srcValue, dest=PReg(destReg, PRegGeneral), kind} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                (* The constant should be an address. *)
                val _ = isShort srcValue andalso raise InternalError "AddressConstant: short"
            in
                codeICode{
                    icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=LoadArgument{source=AddressConstant srcValue, dest=raxAsArg, kind=kind} :: pushCode @ code,
                    context=context}
            end

            (* Move a short constant to a preg.  First push anything in RAX then load the constant into RAX. *)
        |   codeICode{icode=LoadArgument{source=IntegerConstant i, dest=PReg(destReg, PRegGeneral), kind} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
            in
                codeICode{
                    icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=LoadArgument{source=IntegerConstant i, dest=raxAsArg, kind=kind} :: pushCode @ code,
                    context=context}
            end

            (* Move a short constant to an untagged reg. *)
        |   codeICode{icode=LoadArgument{source=IntegerConstant i, dest=PReg(destReg, PRegUntagged), kind} :: rest, state=NormalState state, code, context} =
            let
                val (resultState, resReg) = newUreg(destReg, state)
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code=LoadArgument{source=IntegerConstant i, dest=resReg, kind=kind} :: code,
                    context=context}
            end

            (* Load a value from a piece of memory, typically the offset in a tuple. *)
        |   codeICode{
                icode=LoadArgument{ source=MemoryLocation{offset, base as PReg(_, PRegGeneral), index=NoMemIndex}, dest=PReg(destReg, dType), kind} :: rest,
                state=NormalState state, code, context} =
            let
                (* This previously replaced the top.  Pushing the previous value is safer but means
                   values accumulate on the stack. *)
                val (pushedState, pushCode) = pushRax state
                val load = loadSource(RegisterArgument base, eax, pushedState)
                val (resultState, resultReg) =
                    case dType of
                        PRegGeneral => (addRaxToState(pushedState, destReg), raxAsArg)
                    |   PRegUntagged => newUreg(destReg, pushedState)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=LoadArgument{ source=MemoryLocation{offset=offset, base=raxAsArg, index=NoMemIndex}, dest=resultReg, kind=kind} :: 
                        load @ pushCode @ code, context=context}
            end

        |   codeICode{
                icode=LoadArgument{ source=MemoryLocation{offset, base=PReg(bReg, PRegUntagged), index=NoMemIndex}, dest=PReg(destReg, dType), kind} :: rest,
                state=NormalState(state as {untaggedRegs, ...}), code, context} =
            let
                (* CMem loads with a fixed offset *)
                val realBReg =
                    case List.find (fn {ureg, ...} => ureg = bReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                val (resultState, resultReg) =
                    case (dType, kind, fpMode) of
                        (PRegUntagged, MoveFloat, FPModeX87) =>
                            ({valueStack= #valueStack state, ccState= #ccState state,
                                untaggedRegs =[{ureg=destReg, reg=FPReg fp0}]}, (FPReg fp0))
                    |   (PRegUntagged, MoveFloat, FPModeSSE2) =>
                            ({valueStack= #valueStack state, ccState= #ccState state,
                                untaggedRegs =[{ureg=destReg, reg=XMMReg xmm0}]}, (XMMReg xmm0))
                    |   (PRegUntagged, MoveDouble, FPModeX87) =>
                            ({valueStack= #valueStack state, ccState= #ccState state,
                                untaggedRegs =[{ureg=destReg, reg=FPReg fp0}]}, (FPReg fp0))
                    |   (PRegUntagged, MoveDouble, FPModeSSE2) =>
                            ({valueStack= #valueStack state, ccState= #ccState state,
                                untaggedRegs =[{ureg=destReg, reg=XMMReg xmm0}]}, (XMMReg xmm0))
                    |   (PRegGeneral, MoveFloat, _) => raise InternalError "MoveFloat to normal register"
                    |   (PRegGeneral, MoveDouble, _) => raise InternalError "MoveDouble to normal register"
                    |   (PRegGeneral, _, _) => (addRaxToState(state, destReg), raxAsArg)
                    |   (PRegUntagged, _, _) => newUreg(destReg, state)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=LoadArgument{ source=MemoryLocation{offset=offset, base=realBReg, index=NoMemIndex}, dest=resultReg, kind=kind} :: 
                        code, context=context}
            end

        |   codeICode{
                icode=LoadArgument{ source=MemoryLocation {offset, base, index=MemIndex1(PReg(iReg, PRegUntagged))}, dest=PReg(destReg, dType), kind} :: rest,
                state=NormalState (state as {untaggedRegs, ...}), code, context} =
            let
                (* The index is an untagged value. *)
                val realIReg =
                    case List.find (fn {ureg, ...} => ureg = iReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                (* The base may be a tagged or untagged register.  CMem loads extract the base address from
                   a LargeWord.word boxed value into an untagged register. *)
                val (realBReg, codeBreg) =
                    case base of
                        PReg(bReg, PRegGeneral) =>
                        let
                            val baseOnStack =
                                case findEntry (bReg, state) of
                                    OnStack s => s
                                |   _ => raise InternalError "codeICode: MoveArgument"
                            val code =
                                [LoadArgument{source=wordOffsetAddress(stackPtr state - baseOnStack - 1, esp), dest=rbxAsArg, kind=MoveWord}]
                        in
                            (rbxAsArg, code)
                        end
                    |   PReg(bReg, PRegUntagged) =>
                        let
                            val realBReg =
                                case List.find (fn {ureg, ...} => ureg = bReg) untaggedRegs of
                                    SOME {reg as GenReg _, ...} => reg
                                |   _ => raise InternalError "codeICode: find tagged"
                        in
                            (realBReg, [])
                        end
                val (resultState, resultReg) =
                    case dType of
                        PRegGeneral => (addRaxToState(state, destReg), raxAsArg)
                    |   PRegUntagged =>
                            ({valueStack= #valueStack state, ccState= #ccState state,
                                untaggedRegs =[{ureg=destReg, reg=GenReg ecx}]}, rcxAsArg)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code =
                        LoadArgument{source=MemoryLocation {offset=offset, base=realBReg, index=MemIndex1 realIReg}, dest=resultReg, kind=kind} ::
                        codeBreg @ code,
                    context=context}
            end

        |   codeICode{
                icode=LoadArgument{ source=MemoryLocation {offset, base, index}, dest=PReg(destReg, dType), kind} :: rest,
                state=NormalState state, code, context} =
            let
                (* CMem operations will have pushed the index.  The easiest way to proceed is always
                   to do that. *)
                val (pushedState as { untaggedRegs, ...}, pushCode) = pushRax state
                val (iReg, indexed) =
                    case index of
                        MemIndex1 indexArg => (indexArg, MemIndex1 raxAsArg)
                    |   MemIndex2 indexArg => (indexArg, MemIndex2 raxAsArg)
                    |   MemIndex4 indexArg => (indexArg, MemIndex4 raxAsArg)
                    |   MemIndex8 indexArg => (indexArg, MemIndex8 raxAsArg)
                    |   _ => raise InternalError "codeICode: MemIndex"
                val (realBReg, codeBreg) =
                    case base of
                        PReg(_, PRegGeneral) => (rbxAsArg, loadSource(RegisterArgument base, ebx, pushedState))
                    |   PReg(bReg, PRegUntagged) =>
                        let
                            val realBReg =
                                case List.find (fn {ureg, ...} => ureg = bReg) untaggedRegs of
                                    SOME {reg as GenReg _, ...} => reg
                                |   _ => raise InternalError "codeICode: find tagged"
                        in
                            (realBReg, [])
                        end
                        
                val loadCode = codeBreg @ loadSource(RegisterArgument iReg, eax, pushedState)
                val (resultState, resultReg) =
                    case (dType, kind, fpMode) of
                        (PRegUntagged, MoveFloat, FPModeX87) =>
                            ({valueStack= #valueStack pushedState, ccState= #ccState pushedState,
                                untaggedRegs =[{ureg=destReg, reg=FPReg fp0}]}, (FPReg fp0))
                    |   (PRegUntagged, MoveFloat, FPModeSSE2) =>
                            ({valueStack= #valueStack pushedState, ccState= #ccState pushedState,
                                untaggedRegs =[{ureg=destReg, reg=XMMReg xmm0}]}, (XMMReg xmm0))
                    |   (PRegUntagged, MoveDouble, FPModeX87) =>
                            ({valueStack= #valueStack pushedState, ccState= #ccState pushedState,
                                untaggedRegs =[{ureg=destReg, reg=FPReg fp0}]}, (FPReg fp0))
                    |   (PRegUntagged, MoveDouble, FPModeSSE2) =>
                            ({valueStack= #valueStack pushedState, ccState= #ccState pushedState,
                                untaggedRegs =[{ureg=destReg, reg=XMMReg xmm0}]}, (XMMReg xmm0))
                    |   (PRegGeneral, MoveFloat, _) => raise InternalError "MoveFloat to normal register"
                    |   (PRegGeneral, MoveDouble, _) => raise InternalError "MoveDouble to normal register"
                    |   (PRegGeneral, _, _) => (addRaxToState(pushedState, destReg), raxAsArg)
                    |   (PRegUntagged, _, _) =>
                            ({valueStack= #valueStack pushedState, ccState= #ccState pushedState,
                                untaggedRegs =[{ureg=destReg, reg=GenReg ecx}]}, rcxAsArg)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code =
                        LoadArgument{source=MemoryLocation {offset=offset, base=realBReg, index=indexed}, dest=resultReg, kind=kind} ::
                        loadCode @ pushCode @ code,
                    context=context}
            end

        |   codeICode{icode=LoadArgument{ source=StackLocation{offset, ...}, dest, kind=MoveWord} :: rest, state=NormalState state, code, context} =
            (* Load from the stack *)
            let
                val destReg = pRegNum dest
                val (pushedState, pushCode) = pushRax state
                (* We have to adjust the offset. *)
                val load = loadStack(eax, stackPtr pushedState * wordSize  + offset)
            in
                codeICode{icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=load @ pushCode @ code, context=context}
            end

            (* Store a pseudo-register in a memory location. *)
        |   codeICode{
                icode=StoreArgument{
                        source=RegisterArgument(PReg(valueReg, PRegUntagged)),
                        offset, base as PReg(_, PRegGeneral), index=NoMemIndex, kind} :: rest,
                      state=NormalState state, code, context} =
            let
                val (pushedState as { valueStack, ccState, untaggedRegs}, pushCode) = pushRax state
                val baseOnStack = 
                    case findEntry (pRegNum base, pushedState) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val realVReg =
                    case List.find (fn {ureg, ...} => ureg = valueReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                val loadBaseToEax = loadStack(eax, (stackPtr pushedState - baseOnStack -1) * wordSize)
                val store =
                    [StoreArgument{
                        source=RegisterArgument realVReg,
                        base=(GenReg eax), offset=offset, index=NoMemIndex, kind=kind}]
                val resultState = { valueStack= valueStack, ccState=ccState, untaggedRegs=[]}
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code=store @ loadBaseToEax @ pushCode @ code, context=context}
            end

            (* As above except the base register is in an untagged register.  This is used for CMem stores. *)
        |   codeICode{
                icode=StoreArgument{
                        source=RegisterArgument(PReg(valueReg, PRegUntagged)),
                        offset, base=PReg(bReg, PRegUntagged), index=NoMemIndex, kind} :: rest,
                      state=NormalState{untaggedRegs, valueStack, ccState, ...}, code, context} =
            let
                (* CMem loads with a fixed offset *)
                val realBReg =
                    case List.find (fn {ureg, ...} => ureg = bReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                val realVReg =
                    case List.find (fn {ureg, ...} => ureg = valueReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                val store =
                    [StoreArgument{
                        source=RegisterArgument realVReg,
                        base=realBReg, offset=offset, index=NoMemIndex, kind=kind}]
                val resultState = { valueStack= valueStack, ccState=ccState, untaggedRegs=[]}
            in
                codeICode{
                    icode=rest, state=NormalState resultState, code=store @ code, context=context}
            end

            (* Store an untagged register in a memory location.  This is currently used to store the string
               length word. *)
        |   codeICode{icode=StoreArgument{ source=RegisterArgument(PReg(srcReg, PRegGeneral)), offset, base, index=NoMemIndex, kind} :: rest,
                      state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val baseOnStack = 
                    case findEntry (pRegNum base, pushedState) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val valueOnStack =
                    case findEntry (srcReg, pushedState) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val loadValueToEbx = loadStack(ebx, (stackPtr pushedState - valueOnStack -1) * wordSize)
                val loadBaseToEax = loadStack(eax, (stackPtr pushedState - baseOnStack -1) * wordSize)
                (* Pop the value to store into ebx and store it in the tuple. *)
                val store =
                    [StoreArgument{
                        source=RegisterArgument(GenReg ebx),
                        base=(GenReg eax), offset=offset, index=NoMemIndex, kind=kind}]
            in
                codeICode{
                    icode=rest, state=NormalState pushedState,
                    code=store @ loadBaseToEax @ loadValueToEbx @ pushCode @ code, context=context}
            end

            (* Store a constant in memory. *)
        |   codeICode{icode=StoreArgument{ source=AddressConstant value, offset, base, index=NoMemIndex, kind} :: rest,
                      state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val baseOnStack = 
                    case findEntry (pRegNum base, pushedState) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val loadBaseToEax = loadStack(eax, (stackPtr pushedState - baseOnStack -1) * wordSize)
                val store =
                    [StoreArgument{
                        source=AddressConstant value,
                        base=(GenReg eax), offset=offset, index=NoMemIndex, kind=kind}]
            in
                codeICode{
                    icode=rest, state=NormalState pushedState,
                    code=store @ loadBaseToEax @ pushCode @ code, context=context}
            end

            (* Store a constant in memory. *)
        |   codeICode{icode=StoreArgument{ source=IntegerConstant value, offset, base, index=NoMemIndex, kind} :: rest,
                      state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val baseOnStack = 
                    case findEntry (pRegNum base, pushedState) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val loadBaseToEax = loadStack(eax, (stackPtr pushedState - baseOnStack -1) * wordSize)
                val store =
                    [StoreArgument{
                        source=IntegerConstant value,
                        base=(GenReg eax), offset=offset, index=NoMemIndex, kind=kind}]
            in
                codeICode{
                    icode=rest, state=NormalState pushedState,
                    code=store @ loadBaseToEax @ pushCode @ code, context=context}
            end

        |   codeICode{icode=StoreArgument{ source=RegisterArgument(PReg(valueReg, PRegGeneral)), offset, base, index, kind} :: rest,
                state=NormalState state, code, context} =
            let
                (* The top of the stack should be the value to store.  That will be in rax.
                   We load the base address into rcx and the index into rbx. *)
                val startState = checkTopInRax(state, valueReg)
                val (iReg, indexed) =
                    case index of
                        MemIndex1(PReg(r, PRegGeneral)) => (r, MemIndex1 rbxAsArg)
                    |   MemIndex2(PReg(r, PRegGeneral)) => (r, MemIndex2 rbxAsArg)
                    |   MemIndex4(PReg(r, PRegGeneral)) => (r, MemIndex4 rbxAsArg)
                    |   MemIndex8(PReg(r, PRegGeneral)) => (r, MemIndex8 rbxAsArg)
                    |   _ => raise InternalError "codeICode: MemIndex"
                val indexOnStack =
                    case findEntry (iReg, state) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val baseOnStack =
                    case findEntry (pRegNum base, state) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: MoveArgument"
                val rcxAsArg = (GenReg ecx)
            in
                codeICode{icode=rest, state=NormalState startState,
                    code =
                        StoreArgument{source=RegisterArgument raxAsArg, offset=offset, base=rcxAsArg, index=indexed, kind=kind} ::
                        LoadArgument{source=wordOffsetAddress(stackPtr state - baseOnStack - 1, esp), dest=rcxAsArg, kind=MoveWord} ::
                        LoadArgument{source=wordOffsetAddress(stackPtr state - indexOnStack - 1, esp), dest=rbxAsArg, kind=MoveWord} ::
                        code,
                    context=context}
            end

        |   codeICode{icode=
                       StoreArgument{ source=RegisterArgument(PReg(valueReg, PRegUntagged)),
                                     offset, base, index=MemIndex1(PReg(iReg, PRegUntagged)), kind} :: rest,
                    state=NormalState state, code, context} =
            let
                (* This is used for index stores of bytes.  N.B.  If we're storing a byte from a register
                   we must not use ESI or EDI for the value register in 32-bit mode.  That's because the
                   encoding actually refers to DH and BH.  In 64-bit mode the REX prefix overrides this. *)
                val ({ valueStack, ccState, untaggedRegs}, pushCode) = pushRax state
                (* The index is an untagged value. *)
                val realIReg =
                    case List.find (fn {ureg, ...} => ureg = iReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                val realVReg =
                    case List.find (fn {ureg, ...} => ureg = valueReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                (* The base may be a tagged or untagged register.  CMem stores extract the base address from
                   a LargeWord.word boxed value into an untagged register. *)
                val (realBReg, codeBreg) =
                    case base of
                        PReg(bReg, PRegGeneral) =>
                        let
                            val baseOnStack =
                                case findEntry (bReg, state) of
                                    OnStack s => s
                                |   _ => raise InternalError "codeICode: MoveArgument"
                            val code =
                                [LoadArgument{source=wordOffsetAddress(stackPtr state - baseOnStack - 1, esp), dest=rbxAsArg, kind=MoveWord}]
                        in
                            (rbxAsArg, code)
                        end
                    |   PReg(bReg, PRegUntagged) =>
                        let
                            val realBReg =
                                case List.find (fn {ureg, ...} => ureg = bReg) untaggedRegs of
                                    SOME {reg as GenReg _, ...} => reg
                                |   _ => raise InternalError "codeICode: find tagged"
                        in
                            (realBReg, [])
                        end
                val resultState = { valueStack= valueStack, ccState=ccState, untaggedRegs=[]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code =
                        StoreArgument{source=RegisterArgument realVReg, offset=offset, base=realBReg, index=MemIndex1 realIReg, kind=kind} ::
                        codeBreg @ pushCode @ code,
                    context=context}
            end

        |   codeICode{icode=
                       StoreArgument{ source=RegisterArgument(PReg(valueReg, PRegUntagged)), offset, base, index, kind} :: rest,
                    state=NormalState state, code, context} =
            (* Store an untagged value with an index value in a tagged register. This is used for CMem stores. *)
            let
                val (pushedState as { untaggedRegs, valueStack, ccState, ...}, pushCode) = pushRax state
                val (iReg, indexed) =
                    case index of
                        MemIndex1 indexArg => (indexArg, MemIndex1 raxAsArg)
                    |   MemIndex2 indexArg => (indexArg, MemIndex2 raxAsArg)
                    |   MemIndex4 indexArg => (indexArg, MemIndex4 raxAsArg)
                    |   MemIndex8 indexArg => (indexArg, MemIndex8 raxAsArg)
                    |   _ => raise InternalError "codeICode: MemIndex"
                (* The base address will be in an untagged register. *)
                val (realBReg, codeBreg) =
                    case base of
                        PReg(_, PRegGeneral) => (rbxAsArg, loadSource(RegisterArgument base, ebx, pushedState))
                    |   PReg(bReg, PRegUntagged) =>
                        let
                            val realBReg =
                                case List.find (fn {ureg, ...} => ureg = bReg) untaggedRegs of
                                    SOME {reg as GenReg _, ...} => reg
                                |   _ => raise InternalError "codeICode: find tagged"
                        in
                            (realBReg, [])
                        end
                val realVReg =
                    case List.find (fn {ureg, ...} => ureg = valueReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                        
                val loadCode = codeBreg @ loadSource(RegisterArgument iReg, eax, pushedState)
                val resultState = { valueStack= valueStack, ccState=ccState, untaggedRegs=[]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code =
                        StoreArgument{source=RegisterArgument realVReg, offset=offset, base=realBReg, index=indexed, kind=kind} ::
                        loadCode @ pushCode @ code,
                    context=context}
            end

         |   codeICode{icode=
                        StoreArgument{
                            source=MemoryLocation {offset=srcOffset, base=srcBase, index=NoMemIndex},
                            offset, base=PReg(bReg, PRegUntagged), index=NoMemIndex, kind} :: rest,
                    state=NormalState state, code, context} =
            let
                (* This is used to store floating point values into C memory.  The source will be a PReg and the base will
                   be an untagged register. *)
                val _ = kind = MoveFloat orelse kind = MoveDouble orelse raise InternalError "Move memory to memory - not float"
                val (pushedState as { untaggedRegs, valueStack, ccState, ...}, pushCode) = pushRax state 
                val loadArgAddr = loadSource(RegisterArgument srcBase, ebx, pushedState)
                val resultReg =
                    case fpMode of
                        FPModeX87 => (FPReg fp0)
                    |   FPModeSSE2 => (XMMReg xmm0)
                val loadToFpReg =
                    LoadArgument{ source=MemoryLocation{offset=srcOffset, base=rbxAsArg, index=NoMemIndex}, dest=resultReg, kind=MoveDouble}
                (* The base address will be in an untagged register. *)
                val realBReg =
                    case List.find (fn {ureg, ...} => ureg = bReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                val storeToMem =
                    StoreArgument { source=RegisterArgument resultReg, offset=offset, base=realBReg, index=NoMemIndex, kind=kind}
                val resultState = { valueStack= valueStack, ccState=ccState, untaggedRegs=[]}
                val cvtToFloat =
                    case (fpMode, kind) of
                        (FPModeSSE2, MoveFloat) =>
                            [SSE2FPArith { opc=SSE2DoubleToFloat, resultReg=resultReg, arg1=RegisterArgument resultReg, arg2=RegisterArgument resultReg }]
                    |   _ => []
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code =
                        storeToMem :: cvtToFloat @ (loadToFpReg :: loadArgAddr @ pushCode @ code),
                    context=context}
            end
           
        |   codeICode{icode=
                        StoreArgument{
                            source=MemoryLocation {offset=srcOffset, base=srcBase, index=NoMemIndex},
                            offset, base=PReg(bReg, PRegUntagged), index, kind} :: rest,
                    state=NormalState state, code, context} =
            let
                (* This is used to store floating point values into C memory.  The source will be a PReg and the base will
                   be an untagged register.  The index will be a tagged register. *)
                val _ = kind = MoveFloat orelse kind = MoveDouble orelse raise InternalError "Move memory to memory - not float"
                val (pushedState as { untaggedRegs, valueStack, ccState, ...}, pushCode) = pushRax state 
                val loadArgAddr = loadRegister(srcBase, ebx, pushedState)
                val resultReg =
                    case fpMode of
                        FPModeX87 => (FPReg fp0)
                    |   FPModeSSE2 => (XMMReg xmm0)
                val loadToFpReg =
                    LoadArgument{ source=MemoryLocation{offset=srcOffset, base=rbxAsArg, index=NoMemIndex}, dest=resultReg, kind=MoveDouble}
                val (iReg, indexed) =
                    case index of
                        MemIndex1 indexArg => (indexArg, MemIndex1 raxAsArg)
                    |   MemIndex2 indexArg => (indexArg, MemIndex2 raxAsArg)
                    |   MemIndex4 indexArg => (indexArg, MemIndex4 raxAsArg)
                    |   MemIndex8 indexArg => (indexArg, MemIndex8 raxAsArg)
                    |   _ => raise InternalError "codeICode: MemIndex - move float"
                (* The base address will be in an untagged register. *)
                val realBReg =
                    case List.find (fn {ureg, ...} => ureg = bReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find tagged"
                val cvtToFloat =
                    case (fpMode, kind) of
                        (FPModeSSE2, MoveFloat) =>
                            [SSE2FPArith { opc=SSE2DoubleToFloat, resultReg=resultReg, arg1=RegisterArgument resultReg, arg2=RegisterArgument resultReg }]
                    |   _ => []
                val storeToMem =
                    StoreArgument { source=RegisterArgument resultReg, offset=offset, base=realBReg, index=indexed, kind=kind}
                val resultState = { valueStack= valueStack, ccState=ccState, untaggedRegs=[]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code =
                        storeToMem :: cvtToFloat @ loadRegister(iReg, eax, pushedState) @ (loadToFpReg :: loadArgAddr @ pushCode @ code),
                    context=context}
            end

            (* Other combinations of move aren't used at the moment. *)
        |   codeICode{icode=LoadArgument _ :: _, ...} =
                raise InternalError "codeICode: LoadArgument"

        |   codeICode{icode=StoreArgument _ :: _, ...} =
                raise InternalError "codeICode: StoreArgument"

        |   codeICode{ icode=LoadMemReg{offset, dest} :: rest, state=NormalState state, code, context} =
            (* Load from ebp/rbp *)
            let
                val destReg = pRegNum dest
                val (pushedState, pushCode) = pushRax state
                val load = loadMemory(eax, ebp, offset)
            in
                (* We also generate loads from offsets in ebp. *)
                codeICode{icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=load @ pushCode @ code, context=context}
            end
        
        |   codeICode {
                icode=BeginFunction{regArgs} :: rest, state=NormalState state, code, context} =
            let
                fun pushRegister([], state , code) = (state, code)
                |   pushRegister((dest, srcReg)::others, state, code) =
                let
                    val destReg = pRegNum dest
                    val sp = stackPtr state
                    val {valueStack, ccState, untaggedRegs, ...} = state
                    val newState =
                        {valueStack={preg=destReg, loc=OnStack sp} :: valueStack, ccState=ccState, untaggedRegs=untaggedRegs}
                in
                    pushRegister(others, newState, PushValue{arg=RegisterArgument srcReg} :: code)
                end

                val (pushedState, pushCode) = pushRax state
                val (resultState, resultCode) = pushRegister(regArgs, pushedState, pushCode @ code)
            in
                codeICode{icode=rest, state=NormalState resultState, code=resultCode, context=context}
            end
            
        |   codeICode{icode=AllocateMemoryOperation{ size, flags, dest, saveRegs=_} :: rest, state=NormalState state, code, context} =
            let
                val destReg = pRegNum dest
                val (pushedState, pushCode) = pushRax state
                val resultState = clearCC(addRaxToState(pushedState, destReg)) (* Affects CC *)
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code=AllocateMemoryOperation{ size=size, flags=flags, dest=raxAsArg, saveRegs=[]} :: pushCode @ code, context=context}
            end

        |   codeICode{icode=AllocateMemoryVariable{ size, dest, saveRegs=_} :: rest, state=NormalState state, code, context} =
            let
                (* Allocate memory without initialisation.  Used for byte segments that don't need to be initialised. *)
                val destReg = pRegNum dest
                val (pushedState, pushCode) = pushRax state
                val loadSize = loadSource(RegisterArgument size, edx, pushedState)
                (* We put the size in rdx, the flags in rax and the result in rdi. *)
                val allocMem =
                    [AllocateMemoryVariable{size=rdxAsArg, dest=rdiAsArg, saveRegs=[]}]
                val moveToRAX = [LoadArgument{source=RegisterArgument rdiAsArg, dest=raxAsArg, kind=MoveWord}]
                val resultState = clearCC(addRaxToState(pushedState, destReg))
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code= moveToRAX @ allocMem @ loadSize @ pushCode @ code, context=context}
            end

        |   codeICode{icode=InitialiseMem{ size, addr, init} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState as { untaggedRegs, valueStack, ...}, pushCode) = pushRax state
                
                val sizeReg = uRegNum size
                val realSReg =
                    case List.find (fn {ureg, ...} => ureg = sizeReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: find untagged"

                val loadSize = [LoadArgument{source=RegisterArgument realSReg, dest=rcxAsArg, kind=MoveWord}]
                val loadInit = loadSource(RegisterArgument init, eax, pushedState)
                val loadAddr = loadSource(RegisterArgument addr, edi, pushedState)

                val initMem =
                    [InitialiseMem{size=rcxAsArg, addr=rdiAsArg, init=raxAsArg}]
                val resultState = {valueStack=valueStack, untaggedRegs=[], ccState=CCIndeterminate}
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code= initMem @ loadAddr @ loadInit @ loadSize @ pushCode @ code, context=context}
            end

            (* Marks the end of setting the fields of a tuple.  This is used in the next
               level to avoid reordering a new allocation until a previously allocated
               section of memory has been fully initialised. *)
        |   codeICode{icode=(InitialisationComplete _) :: rest, state, code, context} =
                codeICode{icode=rest, state=state, code=InitialisationComplete{dest=raxAsArg (* unused *)} :: code, context=context}

            (* Function call. *)
        |   codeICode{icode=FunctionCall{ regArgs, stackArgs, dest, callKind} :: rest,
                      state=NormalState state, code, context} =
            let
                val destReg = pRegNum dest
                val (pushedState, pushCode) = pushRax state
                (* In an earlier version we assumed that the values were actually pushed to
                   the stack in sequence.  That's no longer true. Explicitly push the
                   stack arguments. *)
                local
                    fun pushAllArgs ([], state, code) = (state, code)
                    |   pushAllArgs (arg::args, state as {valueStack, ccState, untaggedRegs}, code) =
                        let
                            (* TODO: We can push constants and memory addresses directly without loading
                               them into a register. *)
                            val load = loadSource(arg, eax, state)
                            val sp = stackPtr state
                            val newState =
                                {valueStack={preg= ~1, loc=OnStack sp} :: valueStack, ccState=ccState, untaggedRegs=untaggedRegs}
                        in
                            pushAllArgs(args, newState, PushValue{arg=RegisterArgument raxAsArg} :: load @ code)
                        end
                in
                    val (afterArgs, argCode) = pushAllArgs (stackArgs, pushedState, [])
                end

                (* Move the register arguments into the correct real registers. *)
                fun asGenReg(GenReg r) = r | asGenReg _ = raise InternalError "asGenReg"
                val doLoad = List.foldl (fn ((pr, gr), l) => l @ loadSource(pr, asGenReg gr, afterArgs)) [] regArgs
                (* After the call the stack arguments will have been removed by the caller. *)
                val returnState = addRaxToState({valueStack= #valueStack pushedState, ccState= CCIndeterminate, untaggedRegs=[]}, destReg)
                val functionCode =
                    FunctionCall{regArgs=[], stackArgs=[], dest=raxAsArg, callKind=callKind} ::
                        doLoad @ argCode @ pushCode @ code
            in
                codeICode{icode=rest, state=NormalState returnState, code=functionCode, context=context}
            end

            (* Jump to a function.  The arguments have to be moved into the space currently used
               by the arguments to this function.  That could result in values being overwritten
               so we have to save them, especially the return address. *)
        |   codeICode{icode=TailRecursiveCall{ regArgs, stackArgs, callKind, ...} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val currentSp = stackPtr pushedState

                (* Load the earlier arguments into registers.  Do that first because
                   they're lower down the stack and we could overwrite them if we wait. *)
                (* Load until we've done all the arguments or all the registers whichever is earlier.
                   This includes the closure register. *)
                fun asGenReg(GenReg r) = r | asGenReg _ = raise InternalError "asGenReg"
                val doLoad = List.foldl (fn ((pr, gr), l) => l @ loadSource(pr, asGenReg gr, pushedState)) [] regArgs

                (* Load the return address.  This could be overwritten by an argument. *)
                val loadReturn = loadStack(edi, currentSp * wordSize)

                (* Move the stack arguments into the space occupied by this function's args. *)
                val numArgsToMove = List.length stackArgs
                (* There's a bit of a nasty here.  If we have several constant arguments that we
                   haven't pushed to the stack we could find that argOffset is negative.  To avoid
                   that we push some zeros to line everything up. *)
                val (argOffset, extraPushes, currentState) =
                    if currentSp + currentStackArgs >= numArgsToMove
                    then (currentSp + currentStackArgs - numArgsToMove, [], pushedState)
                    else
                    let
                        val needExtras = numArgsToMove - (currentSp + currentStackArgs)
                        val extras = List.tabulate(needExtras, fn _ => PushValue { arg=IntegerConstant 0 })
                        and afterPush =
                            {valueStack= {loc=OnStack(currentSp+needExtras-1), preg= ~1} :: # valueStack pushedState, ccState=CCIndeterminate, untaggedRegs=[]}
                    in
                        (0, extras, afterPush)
                    end

                fun copyArgs (_, nil) = []
                |   copyArgs (n, {src=r, ...} :: rl) =
                    let
                        (* N.B. This is reversed *)
                        fun storeToDest src =
                            StoreArgument{source=src, base=(GenReg esp), offset=(argOffset+n)*wordSize, index=NoMemIndex, kind=MoveWord}
                        (* If this is a 32-bit constant we can do a direct memory move. Othewise load
                           it into a register.  It might be easier to do some of this in the codetree
                           translation.  Treating all constants this way saves loading them into
                           "registers" in order to save the values. *)
                        val copy =
                            case r of
                                IntegerConstant value =>
                                    if is32bit value
                                    then [storeToDest(IntegerConstant value)]
                                    else [storeToDest(RegisterArgument rsiAsArg),
                                          LoadArgument{source=IntegerConstant value, dest=rsiAsArg, kind=MoveWord}]
                            |   AddressConstant a =>
                                    if isX64
                                    then [storeToDest(RegisterArgument rsiAsArg),
                                          LoadArgument{source=AddressConstant a, dest=rsiAsArg, kind=MoveWord}]
                                    else [storeToDest(AddressConstant a)]
                           |    r => storeToDest(RegisterArgument rsiAsArg) :: loadSource(r, esi, currentState)
                    in
                        copyArgs(n-1, rl) @ copy
                    end

                val moveArgs = copyArgs(numArgsToMove, stackArgs)
                val storeReturn = storeMemory(edi, esp, argOffset*wordSize)
                val functionCode =
                        TailRecursiveCall{regArgs=[], stackArgs=[], callKind=callKind, returnAddr={srcStack=0, stack=0}, stackAdjust=0} ::
                                      resetStackPtr argOffset :: (storeReturn @ moveArgs @ extraPushes @ loadReturn @ doLoad @ pushCode @ code)
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
                     if argAsPReg source = preg then {preg=pRegNum loopReg, loc=loc} else raise InternalError "codeICode: StartLoop"
                val newHead = ListPair.mapEq repSourceWithDest (List.rev arguments, headList)
                val tailList = List.drop(#valueStack pushedState, numArgs)
                (* The CCstate is indeterminate because we may have looped. *)
                val bodyState = {valueStack=newHead @ tailList, ccState= CCIndeterminate, untaggedRegs= []}
                val newContext = {loopArgs=(bodyState, loopLabel) :: loopArgs, forwardLabels=forwardLabels, handlers=handlers}
            in
                codeICode{icode=rest, state=NormalState bodyState, code=StartLoop{arguments=[], loopLabel=loopLabel} :: pushCode @ code,
                          context=newContext}
            end

            (* A loop tail jump.  The arguments have been evaluated to the stack. *)
        |   codeICode{icode=(JumpLoop{arguments, loopLabel}) :: rest, state=NormalState state, code,
                      context=context as {loopArgs=(startState, lab) :: _, ...}} =
            let
                val _ = loopLabel = lab orelse raise InternalError "codeICode: JumpLoop - mismatched labels"
                val (pushedState, pushCode) = pushRax state
                (* Move each argument from the source to the loop variable.  This isn't the same as
                   MoveArgument because the destination already exists. *)
                fun moveArg{source, loopReg} =
                let
                    val load = loadSource(source, eax, pushedState)
                    val store =
                        case findEntry (pRegNum loopReg, pushedState) of
                            OnStack s => storeMemory(eax, esp, (stackPtr pushedState - s - 1) * wordSize)
                        |   InRax => raise InternalError "codeICode: JumpLoop"
                in
                    store @ load
                end
                val storeAll = List.foldl(fn (arg, l) => l @ moveArg arg) [] arguments
                val instrs =
                    JumpLoop{arguments=[], loopLabel=loopLabel} :: resetStackPtr(stackPtr pushedState - stackPtr startState) :: storeAll
            in
                codeICode{icode=rest, state=Exited, code=instrs @ pushCode @ code, context=context}
            end

        |   codeICode{icode=JumpLoop _ :: _, ...} =
                raise InternalError "codeICode: JumpLoop out of loop"

            (* Raise an exception using the value in the "packet" register as the exception packet.
               Since this exits we can ignore any further code and return the code and state. *)
        |   codeICode{icode=RaiseExceptionPacket{packet} :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val load = loadSource(RegisterArgument packet, eax, pushedState)
            in
                codeICode{icode=rest, state=Exited, code=RaiseExceptionPacket {packet=raxAsArg } :: load @ pushCode @ code, context=context}
            end

            (* Reserve a contiguous area of memory on the stack.  This is used to receive the
               results of a tuple.  It may be set within the same function or the address
               may be passed into another function to allow it to return multiple results. *)
        |   codeICode{icode=ReserveContainer{size, address} :: rest, state=NormalState state, code, context} =
            let
                val addrReg = pRegNum address
                val (pushedState, pushCode) = pushRax state
                (* The memory must be cleared in case we have a GC. *)
                val pushAll = List.tabulate(size, fn _ => PushValue{arg=IntegerConstant(tag 0)})
                val sp = stackPtr pushedState
            in
                codeICode{icode=rest,
                    state=NormalState(
                            {valueStack={loc=InRax, preg=addrReg} :: {loc=OnStack(sp+size-1), preg= ~1} :: #valueStack pushedState,
                             ccState=CCIndeterminate, untaggedRegs= []}),
                    code=LoadArgument{source=RegisterArgument(GenReg esp), dest=raxAsArg, kind=MoveWord} :: (pushAll @ pushCode) @ code, context=context}
            end

        |   codeICode{icode=IndexedCaseOperation{ testReg, workReg=_, cases, startValue} :: rest,
                      state=NormalState state, code, context={loopArgs, handlers, forwardLabels}} =
            let
                val tReg = pRegNum testReg
                (* We should have the value to test at the top of the stack. *)
                val startState = checkTopInRax(state, tReg)

                (* This behaves rather like an unconditional branch except that it can go to several places.
                   The case labels and the default label have to be added to the label list. *)
                val labelsFromCase =
                    map(fn ilab => (ilab, startState)) cases @ forwardLabels
            in
                codeICode{icode=rest, state=Exited,
                          code=IndexedCaseOperation{ testReg=raxAsArg, workReg=(GenReg ebx), cases=cases, startValue=startValue} :: code,
                          context={loopArgs=loopArgs, handlers=handlers, forwardLabels=labelsFromCase}}
            end

            (* Lock a mutable segment. *)
        |   codeICode{icode=LockMutable{ addr } :: rest, state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                val load = loadSource(addr, eax, pushedState)
                val lockCode = LockMutable {addr=RegisterArgument raxAsArg} :: (load @ pushCode)
                val resultState = clearCC pushedState (* May affect the CC *)
            in
                codeICode{icode=rest, state=NormalState resultState, code=lockCode @ code, context=context}
            end

       |    codeICode{icode=ReturnResultFromFunction{ resultReg, numStackArgs } :: rest, state=NormalState state, code, context} =
            let
                val resReg = pRegNum resultReg
                val (_, resetCode) = resetStack(state, initialState, resReg)
                val returnCode = ReturnResultFromFunction{resultReg=raxAsArg, numStackArgs=numStackArgs } :: resetCode
            in
                (* We may have more than one return point so we can't just stop here. *)
                codeICode{icode=rest, state=Exited, code=returnCode @ code, context=context}
            end

        |   codeICode{icode=ArithmeticFunction{ oper, resultReg=PReg(resReg, PRegGeneral), operand1 as RegisterArgument(PReg(_, PRegGeneral)), operand2=IntegerConstant i, ccRef } :: rest,
                      state=NormalState state, code, context} =
            let
                (* The result is a tagged register.  This is used when the constant operand can be untagged. *)
                val (pushedState, pushCode) = pushRax state
                val loadCode = loadSource(operand1, eax, pushedState)
                val resultState = addRaxToState(pushedState, resReg)
                val checkState =
                    {valueStack= #valueStack resultState, ccState=CCState(ccRef, fn i=>i), untaggedRegs= #untaggedRegs resultState}
            in
                codeICode{icode=rest, state=NormalState checkState,
                    code=ArithmeticFunction { oper=oper, resultReg=raxAsArg, operand1=RegisterArgument raxAsArg, operand2=IntegerConstant i, ccRef=ccRef } :: loadCode @ pushCode @ code,
                    context=context}
            end

        |   codeICode{icode=ArithmeticFunction{ oper, resultReg=PReg(dReg, PRegUntagged), operand1 as RegisterArgument(PReg(_, PRegGeneral)), operand2=IntegerConstant i, ccRef } :: rest,
                      state=NormalState state, code, context} =
            let
                (* The result is an untagged register.  Typically used to untag a value before an operation when we
                   don't want to shift the argument. *)
                val (pushedState, pushCode) = pushRax state (* Simplifies things *)
                (* Find a spare untagged register. *)
                val {valueStack, untaggedRegs, ...} = pushedState
                val spareReg =
                    case untaggedRegs of
                        [] => edx
                    |   [_] => ecx
                    |   _ => raise InternalError "codeICode: ArithmeticFunction"
                val resReg = (GenReg spareReg)
                val loadCode = loadSource(operand1, spareReg, pushedState)
                val resultState =
                    {valueStack= valueStack, ccState= CCState(ccRef, fn i=>i), untaggedRegs ={ureg=dReg, reg=GenReg spareReg} :: untaggedRegs}
             in
                codeICode{icode=rest, state=NormalState resultState,
                    code=ArithmeticFunction { oper=oper, resultReg=resReg, operand1=RegisterArgument resReg, operand2=IntegerConstant i, ccRef=ccRef } :: loadCode @ pushCode @ code,
                    context=context}
            end

        |   codeICode{icode=ArithmeticFunction{ oper, resultReg=PReg(resReg, PRegGeneral), operand1=RegisterArgument(PReg(oper1Reg, PRegUntagged)), operand2=IntegerConstant i, ccRef } :: rest,
                      state=NormalState (state as {untaggedRegs, ...}), code, context} =
            let
                (* The result is a tagged register but the argument is untagged.  This is typically when we want to set the tag on a  value. *)
                val realSReg =
                    case List.find (fn {ureg, ...} => ureg = oper1Reg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: ArithmeticFunction"
                val (pushedState, pushCode) = pushRax state
                val addState = addRaxToState(pushedState, resReg)
                val resultState = removeAfterTag(addState, oper1Reg)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code= (* TODO: We could use LEA since this is normally (always?) ADD *)
                        LoadArgument{source=RegisterArgument realSReg, dest=raxAsArg, kind=MoveWord} ::
                        ArithmeticFunction { oper=oper, resultReg=realSReg, operand1=RegisterArgument realSReg, operand2=IntegerConstant i, ccRef=ccRef } :: pushCode @ code,
                    context=context}
            end

        |   codeICode{icode=ArithmeticFunction{ oper, resultReg=PReg(resReg, PRegUntagged), operand1=RegisterArgument(PReg(oper1Reg, PRegUntagged)), operand2=IntegerConstant i, ccRef } :: rest,
                      state=NormalState ({valueStack, untaggedRegs, ...}), code, context} =
            let
                (* The result and argument are both untagged.  This is used in computing floating point comparisons. *)
                val realSReg =
                    case List.find (fn {ureg, ...} => ureg = oper1Reg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: ArithmeticFunction"
                (* Leave the untagged register where it was but add the condition code. *)
                val resultState = {valueStack=valueStack, ccState=CCState(ccRef, fn i => i), untaggedRegs=[{ureg=resReg, reg=realSReg}]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        ArithmeticFunction { oper=oper, resultReg=realSReg, operand1=RegisterArgument realSReg, operand2=IntegerConstant i, ccRef=ccRef } :: code,
                    context=context}
            end

        |   codeICode{icode=ArithmeticFunction{ oper, resultReg, operand1=RegisterArgument(PReg(oper1Reg, PRegGeneral)), operand2 as RegisterArgument(PReg(_, PRegGeneral)), ccRef } :: rest,
                      state=NormalState state, code, context} =
            let
                (* The top item should be in RAX but that is the second argument.  That's all right for
                   addition but wrong for subtraction.  Move the first argument into RBX, load the second into
                   RAX and then do the operation. *)
                val resReg = pRegNum resultReg
                val (topState, loadArg2Base) = loadToRax(state, operand2)
                val secondArg =
                    case findEntry (oper1Reg, topState) of
                        OnStack s => s
                    |   _ => raise InternalError "codeICode: ArithmeticFunction"
                val resultState = addRaxToState(topState, resReg)
                val checkState =
                    {valueStack= #valueStack resultState, ccState=CCState(ccRef, fn i=>i), untaggedRegs= #untaggedRegs resultState}
            in
                codeICode{icode=rest, state=NormalState checkState,
                    code= (* N.B. In reverse order *)
                        ArithmeticFunction {
                            oper=oper, resultReg=raxAsArg, operand1=RegisterArgument raxAsArg, operand2=RegisterArgument(GenReg ebx), ccRef=ccRef } ::
                        LoadArgument
                            { source=MemoryLocation{offset=(stackPtr topState - secondArg - 1) * wordSize, base=(GenReg esp), index=NoMemIndex},
                              dest=raxAsArg, kind=MoveWord } ::
                        LoadArgument { source=RegisterArgument raxAsArg, dest=GenReg ebx, kind=MoveWord} ::
                        loadArg2Base @ code,
                    context=context}
            end

        |   codeICode{
                icode=ArithmeticFunction{ oper, resultReg=PReg(resReg, PRegUntagged),
                    operand1=RegisterArgument(PReg(oper1Reg, PRegUntagged)),
                    operand2=MemoryLocation{offset=offset2, base=bReg2, index=NoMemIndex}, ccRef } :: rest,
                state=NormalState state, code, context} =
                (* This is used for LargeWord operations. *)
            let
                val ({ valueStack, untaggedRegs, ...}, loadArg2Base) =
                    loadToRax(state, RegisterArgument bReg2)
                (* The result and argument are both untagged. *)
                val realSReg =
                    case List.find (fn {ureg, ...} => ureg = oper1Reg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: ArithmeticFunction"
                val resultState = {valueStack=valueStack, ccState=CCState(ccRef, fn i => i), untaggedRegs=[{ureg=resReg, reg=realSReg}]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        ArithmeticFunction {
                            oper=oper, resultReg=realSReg, operand1=RegisterArgument realSReg,
                            operand2=MemoryLocation{offset=offset2, base=raxAsArg, index=NoMemIndex}, ccRef=ccRef } ::
                        loadArg2Base @ code,
                    context=context}
            end

        |   codeICode{icode=ArithmeticFunction _ :: _, ...} =
                raise InternalError "TODO: CodeTransform ArithmeticFunction"

        |   codeICode{icode=TagValue { source, dest } :: rest,
                      state=NormalState (state as {untaggedRegs, ...}), code, context} =
            let
                val sReg = uRegNum source and destReg = pRegNum dest
                val realSReg =
                    case List.find (fn {ureg, ...} => ureg = sReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: TagValue"
                val (pushedState, pushCode) = pushRax state
                val addState = addRaxToState(pushedState, destReg)
                val resultState = removeAfterTag(addState, sReg)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=LoadEffectiveAddress { base=NONE, offset=1, index=MemIndex2 realSReg, dest=raxAsArg } :: pushCode @ code,
                    context=context}
            end

        |   codeICode{icode=UntagValue { source, dest, isSigned } :: rest, state=NormalState state, code, context} =
            let
                val dReg = uRegNum dest
                val (pushedState, pushCode) = pushRax state (* Simplifies things *)
                val (resultState, regAsArg) = newUreg(dReg, pushedState)
                val resReg = case regAsArg of (GenReg r) => r | _ => raise Match
                val loadCode = loadSource(source, resReg, pushedState)
                val resultState = clearCC resultState
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        ShiftOperation{shift=if isSigned then SAR else SHR, resultReg=regAsArg, operand=RegisterArgument regAsArg,
                            shiftAmount=IntegerConstant 1, ccRef=CcRef 0 } ::
                        loadCode @ pushCode @ code,
                    context=context}
            end

        |   codeICode{
                icode=LoadEffectiveAddress {
                    base=SOME baseSource, offset, index=MemIndex1(PReg(iReg, PRegUntagged)), dest=PReg(dReg, PRegUntagged)
                    } :: rest,
                state=NormalState state, code, context} =
            let
                (* This is used to compute the address for a CompareByteVectors instruction.
                   We have to reuse the untagged register for the result.  Use eax for the base. *)
                val (pushedState as { untaggedRegs, valueStack, ccState}, pushCode) = pushRax state (* Simplifies things *)
                val realIReg =
                    case List.find (fn {ureg, ...} => ureg = iReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: LoadEffectiveAddress"
                val loadCode = loadRegister(baseSource, eax, pushedState)
                val filteredUntagged =
                    List.filter (fn {ureg, ...} => ureg <> iReg) untaggedRegs
                val resultState =
                    {valueStack=valueStack, ccState=ccState, untaggedRegs={ureg=dReg, reg=realIReg}::filteredUntagged}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        LoadEffectiveAddress {base=SOME raxAsArg, offset=offset, index=MemIndex1(realIReg), dest=realIReg} ::
                        loadCode @ pushCode@code,
                    context=context}
            end

        |   codeICode{
                icode=LoadEffectiveAddress { base=SOME baseSource, offset, index=NoMemIndex, dest=PReg(dReg, PRegUntagged)} :: rest,
                state=NormalState state, code, context} =
            let
                (* Used to compute the base address for block operations.  The result is always an untagged register
                   because it points into a block of ML memory. *)
                val (pushedState, pushCode) = pushRax state (* Simplifies things *)
                val loadCode = loadRegister(baseSource, eax, pushedState)
                val (resultState, iReg) = newUreg(dReg, pushedState)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        LoadEffectiveAddress {base=SOME raxAsArg, offset=offset, index=NoMemIndex, dest=iReg} ::
                        loadCode @ pushCode@code,
                    context=context}
            end

        |   codeICode{
                icode=LoadEffectiveAddress { base=SOME baseSource, offset, index, dest=PReg(dReg, PRegUntagged)} :: rest,
                state=NormalState state, code, context} =
            let
                (* Used to compute the base address for block word moves.  Unlike the MemIndex1 case the index
                   register is not an untagged register. *)
                val (pushedState, pushCode) = pushRax state (* Simplifies things *)
                val (iReg, indexed) =
                    case index of
                        MemIndex2 src => (src, MemIndex2 raxAsArg)
                    |   MemIndex4 src => (src, MemIndex4 raxAsArg)
                    |   MemIndex8 src => (src, MemIndex8 raxAsArg)
                    |   _ => raise InternalError "codeICode: MemIndex"
                val loadCode = loadRegister(baseSource, ebx, pushedState) @ loadRegister(iReg, eax, pushedState)
                val (resultState, realDReg) = newUreg(dReg, pushedState)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        LoadEffectiveAddress {base=SOME rbxAsArg, offset=offset, index=indexed, dest=realDReg} ::
                        loadCode @ pushCode @ code,
                    context=context}
            end

(*        |   codeICode{icode=LoadEffectiveAddress { base=NONE, offset, index=MemIndex2(PReg iReg), dest } :: rest,
                      state=NormalState state, code, context} =
            (* Currently just used for tagging. *)
            let
                val resReg = argToPReg dest
                val topState = checkTopInRax(state, iReg)
                val resultState = addRaxToState(topState, resReg)
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=LoadEffectiveAddress{ base=NONE, offset=offset, index=MemIndex2 raxAsArg, dest=raxAsArg } :: code,
                    context=context}
            end*)

        |   codeICode{icode=LoadEffectiveAddress _ :: _, ...} =
                raise InternalError "TODO: CodeTransform LoadEffectiveAddress"

        |   codeICode{icode=ShiftOperation {shift, resultReg, operand=RegisterArgument(PReg(operReg, PRegGeneral)), shiftAmount=IntegerConstant i, ccRef } :: rest,
                      state=NormalState state, code, context} =
            let
                val resReg = pRegNum resultReg
                val topState = checkTopInRax(state, operReg)
                val resultState = addRaxToState(topState, resReg)
                val checkState =
                    {valueStack= #valueStack resultState, ccState=CCState(ccRef, fn i=>i), untaggedRegs= #untaggedRegs resultState}
            in
                codeICode{icode=rest, state=NormalState checkState,
                    code=ShiftOperation { shift=shift, resultReg=raxAsArg, operand=RegisterArgument raxAsArg, shiftAmount=IntegerConstant i, ccRef=ccRef } :: code,
                    context=context}
            end

        |   codeICode{icode=ShiftOperation {shift, resultReg, operand=RegisterArgument(PReg(operReg, PRegUntagged)), shiftAmount=RegisterArgument(PReg(shiftReg, PRegUntagged)), ccRef } :: rest,
                      state=NormalState ({untaggedRegs, valueStack, ...}), code, context} =
            let
                (* Shift by a variable.  This should be in rcx which is where we want it. *)
                val resReg = uRegNum resultReg
                val realOperReg =
                    case List.find (fn {ureg, ...} => ureg = operReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: ShiftOperation"
                val realShiftReg =
                    case List.find (fn {ureg, ...} => ureg = shiftReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: ShiftOperation"
                val _ = realShiftReg = GenReg ecx orelse raise InternalError "ShiftOperation: not ecx"
                val resultState =
                    {valueStack=valueStack, ccState=CCState(ccRef, fn i=>i), untaggedRegs =[{ureg=resReg, reg=realOperReg}]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=ShiftOperation{ shift=shift, resultReg=realOperReg, operand=RegisterArgument realOperReg,
                                         shiftAmount=RegisterArgument realShiftReg, ccRef=ccRef} :: code, context=context}
            end

        |   codeICode{
                icode=ShiftOperation {shift, resultReg,
                    operand as MemoryLocation{index=NoMemIndex, ...},
                    shiftAmount=RegisterArgument(PReg(shiftReg, PRegUntagged)), ccRef } :: rest,
                state=NormalState (state as {untaggedRegs, valueStack, ...}), code, context} =
            let
                (* Shift by a variable.  This is used for LargeWord shifts.  The argument is boxed rather than untagged
                   so we will have loaded the shift amount, which is tagged, into edx.  *)
                val resReg = uRegNum resultReg
                val realShiftReg =
                    case List.find (fn {ureg, ...} => ureg = shiftReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: ShiftOperation"
                val _ = realShiftReg = GenReg edx orelse raise InternalError "ShiftOperation: it is ecx"
                (* Move the shift amount into ecx then load the value to be shifted into edx. *)
                val loadCode = loadSource(operand, edx, state)
                val resultState =
                    {valueStack=valueStack, ccState=CCState(ccRef, fn i=>i), untaggedRegs =[{ureg=resReg, reg=GenReg edx}]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=ShiftOperation{ shift=shift, resultReg=rdxAsArg, operand=RegisterArgument rdxAsArg,
                                         shiftAmount=RegisterArgument rcxAsArg, ccRef=ccRef} ::
                         loadCode @
                         (LoadArgument{source=RegisterArgument rdxAsArg, dest=rcxAsArg, kind=MoveWord} :: code), context=context}
            end

        |   codeICode{icode=ShiftOperation _ :: _, ...} =
                raise InternalError "TODO: CodeTransform ShiftOperation"

        |   codeICode{icode=Multiplication { resultReg, operand1, operand2, ccRef } :: rest,
                      state=NormalState ({untaggedRegs, valueStack, ...}), code, context} =
            let
                (* For the moment we only implement the case of multiplying two values in registers.
                   The X86 supports various other multiplication forms if one of the arguments is
                   a constant including one which takes three arguments. *)
                val resReg = uRegNum resultReg and op1Reg = argAsUReg operand1 and op2Reg = argAsUReg operand2
                val realOp1Reg =
                    case List.find (fn {ureg, ...} => ureg = op1Reg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: Multiplication"
                val realOp2Reg =
                    case List.find (fn {ureg, ...} => ureg = op2Reg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: Multiplication"
                (* We can replace both the untagged registers. *)
                val resultState =
                    {valueStack=valueStack, ccState=CCState(ccRef, fn i=>i), untaggedRegs =[{ureg=resReg, reg=realOp1Reg}]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=Multiplication{ resultReg=realOp1Reg, operand1=RegisterArgument realOp1Reg,
                                         operand2=RegisterArgument realOp2Reg, ccRef=ccRef} :: code, context=context}
            end

(*        |   codeICode{
                icode=Division { isSigned,
                        dividend as MemoryLocation{index=NoMemIndex, ...},
                        divisor as MemoryLocation{index=NoMemIndex, ...}, quotient, remainder } :: rest,
                state=NormalState state, code, context} =
            let
                (* We need the dividend in Rax so we can't use it as the base reg for the divisor. *)
                val (pushState, pushCode) = pushRax state (* We need RAX. *)
                val loadDivisor = loadSource(divisor, ecx, pushState)
                val loadDividend = loadSource(dividend, eax, pushState)
                val resultState =
                    {valueStack= #valueStack pushState, ccState=CCIndeterminate,
                        untaggedRegs =[{ureg=uRegNum quotient, reg=GenReg eax}, {ureg=uRegNum remainder, reg=GenReg edx}]}
             in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        Division{ isSigned=isSigned, dividend=RegisterArgument raxAsArg, divisor=RegisterArgument rcxAsArg, quotient=raxAsArg, remainder=rdxAsArg} ::
                        loadDividend @ loadDivisor @ pushCode @ code, context=context}
            end*)

        |   codeICode{icode=Division { isSigned, dividend, divisor, quotient, remainder } :: rest,
                      state=NormalState state, code, context} =
            let
                val dividendReg = uRegNum dividend and divisorReg = argAsUReg divisor
                val ({ untaggedRegs, valueStack, ...}, pushCode) = pushRax state (* We need RAX. *)
                val realDividend =
                    case List.find (fn {ureg, ...} => ureg = dividendReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: Division"
                val realDivisor =
                    case List.find (fn {ureg, ...} => ureg = divisorReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: Division"
                val resultState =
                    {valueStack=valueStack, ccState=CCIndeterminate,
                        untaggedRegs =[{ureg=uRegNum quotient, reg=GenReg eax}, {ureg=uRegNum remainder, reg=GenReg edx}]}
                (* The untagging code puts the first value into edx and the second into ecx.  We
                   have to move the dividend into eax. *)
                val _ = realDividend = GenReg edx orelse raise InternalError "codeICode: dividend"
                val _ = realDivisor = GenReg ecx orelse raise InternalError "codeICode: divisor"
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code=
                        Division{ isSigned=isSigned, dividend=raxAsArg, divisor=RegisterArgument rcxAsArg, quotient=raxAsArg, remainder=rdxAsArg} ::
                        LoadArgument { source=RegisterArgument realDividend, dest=raxAsArg, kind=MoveWord } :: pushCode @ code, context=context}
            end

        |   codeICode{icode=AtomicExchangeAndAdd{ base, source } :: rest,
                      state=NormalState state, code, context} =
            let
                val (pushedState, pushCode) = pushRax state
                (* Load the "destination" address into rax. *)
                val load = loadSource(RegisterArgument base, eax, pushedState)
                (* The "source" is an untagged register. *)
                val srcReg = getUreg(uRegNum source, pushedState)
            in
                codeICode{icode=rest, state=NormalState pushedState,
                    code=AtomicExchangeAndAdd{base=raxAsArg, source=srcReg} :: load @ pushCode @ code, context=context}
            end

        |   codeICode{icode=BoxValue{ boxKind=BoxLargeWord, dest, source, saveRegs=_ } :: rest, state=NormalState state, code, context} =
            let
                (* Allocate a one word cell and store the untagged value into it. *)
                val destReg = pRegNum dest
                val srcReg = getUreg(uRegNum source, state)
                val realSource =
                    case srcReg of (GenReg r) => r | _ => raise InternalError "BoxLargeWord"
                val (newSrcReg, pushedState, pushCode) =
                    if realSource = eax
                    then
                    let
                        (* If the value we want to store is in eax i.e. the quotient, we
                           must move it to another register before the allocation. *)
                        val pushCode =
                            [LoadArgument{ source=RegisterArgument srcReg, dest=rcxAsArg, kind=MoveWord}]
                    in
                        (rcxAsArg, state, pushCode)
                    end
                    else
                    let
                        val (pushedState, pushCode) = pushRax state
                    in
                        (srcReg, pushedState, pushCode)
                    end
                val resultState =
                    removeAfterTag(clearCC(addRaxToState(pushedState, destReg)) (* Affects CC *), srcReg)
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code=
                        InitialisationComplete{dest=raxAsArg} ::
                        StoreArgument{ source=RegisterArgument newSrcReg, offset=0, base=GenReg eax, index=NoMemIndex, kind=MoveWord} ::
                        AllocateMemoryOperation{ size=1, flags=0wx1, dest=raxAsArg, saveRegs=[]} :: pushCode @ code, context=context}
            end

        |   codeICode{icode=BoxValue{ boxKind=BoxFloat, dest, source, saveRegs=_ } :: rest, state=NormalState state, code, context} =
            let
                (* Allocate a one or two word cell and store the untagged value into it. *)
                val destReg = pRegNum dest
                val srcReg = getFPreg(uRegNum source, state)
                val (pushedState, pushCode) = pushRax state
                val resultState = removeAfterTag(clearCC(addRaxToState(pushedState, destReg)) (* Affects CC *), srcReg)
                val boxSize = Int.quot(8, wordSize)
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code=
                        InitialisationComplete{dest=raxAsArg} ::
                        StoreArgument{ source=RegisterArgument srcReg, offset=0, base=GenReg eax, index=NoMemIndex, kind=MoveDouble} ::
                        AllocateMemoryOperation{ size=boxSize, flags=0wx1, dest=raxAsArg, saveRegs=[]} :: pushCode @ code, context=context}
            end

        |   codeICode{icode=CompareByteVectors { vec1Addr, vec2Addr, length, ccRef } :: rest, state=NormalState state, code, context} =
            let
                val {untaggedRegs, ...} = state
                val vec1Reg = uRegNum vec1Addr and vec2Reg = uRegNum vec2Addr
                val realVec1Reg =
                    case List.find (fn {ureg, ...} => ureg = vec1Reg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: CompareByteVectors"
                val realVec2Reg =
                    case List.find (fn {ureg, ...} => ureg = vec2Reg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: CompareByteVectors"
                (* Put the length into RCX. *)
                (* There's a complication here.  CompareByteVectors generates REPE CMPSB to compare
                   the vectors but the condition code is only set if CMPSB is executed at least
                   once.  If the value in RCX/ECX is zero it will never be executed and the
                   condition code will be unchanged.  We want the result to be "equal" in that
                   case so we need to ensure that is the case.  It's quite possible that the
                   condition code has just been set by shifting RCX/ECX to remove the tag in which
                   case it will have set "equal" if the value was zero.  We use CMP R/ECX,R/ECX which
                   is two bytes in 32-bit but three in 64-bit. *)
                val (lengthCode, setZeroFlag) =
                    let
                        val lenReg = uRegNum length
                        val realLenReg =
                            case List.find (fn {ureg, ...} => ureg = lenReg) untaggedRegs of
                                SOME {reg as GenReg _, ...} => reg
                            |   _ => raise InternalError "codeICode: CompareByteVectors"
                        val _ = realLenReg = GenReg esi orelse raise InternalError "codeICode: lenReg"
                    in
                        (LoadArgument{source=RegisterArgument realLenReg, dest=rcxAsArg, kind=MoveWord},
                         [WordComparison {arg1=RegisterArgument rcxAsArg, arg2=RegisterArgument rcxAsArg, ccRef=ccRef}]) (* Set CC - any reg will do. *)
                    end
                (* These aren't in the registers we want so we have to shuffle them. *)
                val _ = realVec1Reg = GenReg edx orelse raise InternalError "codeICode: vec1Reg"
                val _ = realVec2Reg = GenReg ecx orelse raise InternalError "codeICode: vec2Reg"
                
                val resultState =
                    {valueStack= #valueStack state, ccState=CCState(ccRef, fn i=>i), untaggedRegs= []}
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code =
                        CompareByteVectors{vec1Addr=rsiAsArg, vec2Addr=rdiAsArg, length=rcxAsArg, ccRef=ccRef} ::
                        setZeroFlag @ (
                        LoadArgument{source=RegisterArgument realVec1Reg, dest=rsiAsArg, kind=MoveWord} ::
                        lengthCode ::
                        LoadArgument{source=RegisterArgument realVec2Reg, dest=rdiAsArg, kind=MoveWord} :: code), context=context}
            end

        |   codeICode{icode=BlockMove { srcAddr, destAddr, length, isByteMove } :: rest, state=NormalState state, code, context} =
            let
                val {untaggedRegs, ...} = state
                val srcReg = uRegNum srcAddr and destReg = uRegNum destAddr and lenReg = uRegNum length
                val realSrcReg =
                    case List.find (fn {ureg, ...} => ureg = srcReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: BlockMove"
                val realDestReg =
                    case List.find (fn {ureg, ...} => ureg = destReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: BlockMove"
                val realLenReg =
                    case List.find (fn {ureg, ...} => ureg = lenReg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: BlockMove"
                (* These aren't in the registers we want so we have to shuffle them. *)
                val _ = realSrcReg = GenReg edx orelse raise InternalError "codeICode: vec1Reg"
                val _ = realDestReg = GenReg ecx orelse raise InternalError "codeICode: vec2Reg"
                val _ = realLenReg = GenReg esi orelse raise InternalError "codeICode: lenReg"
                val resultState =
                    {valueStack= #valueStack state, ccState=CCIndeterminate, untaggedRegs= []}
            in
                codeICode{
                    icode=rest, state=NormalState resultState,
                    code =
                        BlockMove{srcAddr=rsiAsArg, destAddr=rdiAsArg, length=rcxAsArg, isByteMove=isByteMove} ::
                        LoadArgument{source=RegisterArgument realSrcReg, dest=rsiAsArg, kind=MoveWord} ::
                        LoadArgument{source=RegisterArgument realLenReg, dest=rcxAsArg, kind=MoveWord} ::
                        LoadArgument{source=RegisterArgument realDestReg, dest=rdiAsArg, kind=MoveWord} :: code, context=context}
            end

        |   codeICode{
                icode=CompareFloatingPt{
                        arg1=MemoryLocation{offset=offset1, base=bReg1, index=NoMemIndex},
                        arg2=MemoryLocation{offset=offset2, base=bReg2, index=NoMemIndex}, ccRef} :: rest,
                      state=NormalState state, code, context=context} =
            let
                (* Load the first argument into an SSE2 register or the X87 stack.
                   Compare with the second. *)
                val (startState as { valueStack, ...}, arg2Code) = loadToRax(state, RegisterArgument bReg2)
                val arg1Code = loadRegister(bReg1, ebx, startState)
                val floatingPointReg =
                    case fpMode of
                        FPModeX87 => FPReg fp0
                    |   FPModeSSE2 => XMMReg xmm0
                val checkState = {valueStack= valueStack, ccState=CCState(ccRef, fn c => c), untaggedRegs= []}
            in
                codeICode{icode=rest, state=NormalState checkState,
                          code=
                            CompareFloatingPt{arg1=RegisterArgument floatingPointReg, arg2=MemoryLocation{offset=offset2, base=raxAsArg, index=NoMemIndex}, ccRef=ccRef} ::
                            LoadArgument{source=MemoryLocation{offset=offset1, base=rbxAsArg, index=NoMemIndex},
                                dest=floatingPointReg, kind=MoveDouble} ::
                            arg1Code @ arg2Code @ code,
                          context=context}
            end

        |   codeICode{icode=CompareFloatingPt _ :: _, ...} =
                raise InternalError "codeICode: CompareFloatingPt"

        |   codeICode{
                icode=X87FPGetCondition{ ccRef, dest } :: rest, state=NormalState state, code, context=context} =
            let
                val destReg = uRegNum dest
                (* Check we have the condition. *)
                (* We can only use RAX here. *)
                val ({valueStack, ccState, ...}, pushCode) = pushRax state
                val () =
                    case ccState of
                        CCState(currentRef, _) =>
                            if currentRef = ccRef then ()
                            else raise InternalError "codeICode: condition ref incorrect"
                    |   _ => raise InternalError "codeICode: condition ref unset"
                val resultState =
                    {valueStack=valueStack, ccState=ccState, untaggedRegs=[{ureg=destReg, reg=GenReg eax}]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                    code= X87FPGetCondition{ ccRef=ccRef, dest=raxAsArg } :: pushCode @ code,
                    context=context}
            end

        |   codeICode{
                icode=X87FPArith{
                        opc, resultReg,
                        arg1=MemoryLocation{offset=offset1, base=bReg1, index=NoMemIndex},
                        arg2=MemoryLocation{offset=offset2, base=bReg2, index=NoMemIndex}} :: rest,
                      state=NormalState state, code, context=context} =
            let
                (* The address of the first argument will be on the stack.  The second
                   will be in Rax. *)
                val startState as {valueStack, ...} = checkTopInRax(state, pRegNum bReg2)
                val loadArgAddr = loadRegister(bReg1, ebx, startState)

                val floatingPointReg = FPReg fp0
                val destReg = uRegNum resultReg
                val resultState =
                    {valueStack=valueStack, ccState=CCIndeterminate, untaggedRegs=[{ureg=destReg, reg=floatingPointReg}]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                          code=
                            X87FPArith{
                                opc=opc, resultReg=floatingPointReg, arg1=RegisterArgument floatingPointReg,
                                arg2=MemoryLocation{offset=offset2, base=raxAsArg, index=NoMemIndex}} ::
                            LoadArgument{source=MemoryLocation{offset=offset1, base=rbxAsArg, index=NoMemIndex},
                                dest=floatingPointReg, kind=MoveDouble} ::
                            loadArgAddr @ code,
                          context=context}
            end

        |   codeICode{icode=X87FPArith _ :: _, ...} =
                raise InternalError "codeICode: X87FPArith"

        |   codeICode{
                icode=SSE2FPArith{
                        opc, resultReg,
                        arg1=MemoryLocation{offset=offset1, base=bReg1, index=NoMemIndex},
                        arg2=MemoryLocation{offset=offset2, base=bReg2, index=NoMemIndex}} :: rest,
                      state=NormalState state, code, context=context} =
            let
                (* The address of the first argument will be on the stack.  The second
                   will be in Rax. *)
                val startState as {valueStack, ...} = checkTopInRax(state, pRegNum bReg2)
                val loadArgAddr = loadRegister(bReg1, ebx, startState)

                val floatingPointReg = XMMReg xmm0
                val destReg = uRegNum resultReg
                val resultState =
                    {valueStack=valueStack, ccState=CCIndeterminate, untaggedRegs=[{ureg=destReg, reg=floatingPointReg}]}
                (* We can't use this for the logical operations since they require 128-bit args. *)
                val () =
                    case opc of
                        SSE2Xor => raise InternalError "codeICode: SSE2FPArith - SSE2Xor"
                    |   SSE2And => raise InternalError "codeICode: SSE2FPArith - SSE2And"
                    |   _ => ()
            in
                codeICode{icode=rest, state=NormalState resultState,
                          code=
                            SSE2FPArith{
                                opc=opc, resultReg=floatingPointReg, arg1=RegisterArgument floatingPointReg,
                                arg2=MemoryLocation{offset=offset2, base=raxAsArg, index=NoMemIndex}} ::
                            LoadArgument{source=MemoryLocation{offset=offset1, base=rbxAsArg, index=NoMemIndex},
                                dest=floatingPointReg, kind=MoveDouble} ::
                            loadArgAddr @ code,
                          context=context}
            end

        |   codeICode{
                icode=SSE2FPArith{
                        opc, resultReg,
                        arg1=MemoryLocation{offset=offset1, base=bReg1, index=NoMemIndex},
                        arg2=AddressConstant a} :: rest,
                      state=NormalState state, code, context=context} =
            let
                (* This is used for the logical operations to invert or mask the sign bit.
                   Both arguments need to be loaded into registers because any memory
                   argument must be a 128-bit value. *)
                val {valueStack, ...} = checkTopInRax(state, pRegNum bReg1)
                val floatingPointReg0 = XMMReg xmm0
                val floatingPointReg1 = XMMReg xmm1
                val destReg = uRegNum resultReg
                val resultState =
                    {valueStack=valueStack, ccState=CCIndeterminate, untaggedRegs=[{ureg=destReg, reg=floatingPointReg0}]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                          code=
                            SSE2FPArith{
                                opc=opc, resultReg=floatingPointReg0, arg1=RegisterArgument floatingPointReg0,
                                arg2=RegisterArgument floatingPointReg1} ::
                            LoadArgument{source=AddressConstant a, dest=floatingPointReg1, kind=MoveDouble} ::
                            LoadArgument{source=MemoryLocation{offset=offset1, base=raxAsArg, index=NoMemIndex},
                                dest=floatingPointReg0, kind=MoveDouble} ::
                            code,
                          context=context}
            end

        |   codeICode{icode=SSE2FPArith _ :: _, ...} =
                raise InternalError "codeICode: SSE2FPArith"

        |   codeICode{
                icode=X87FPUnaryOps{ fpOp, dest, source=MemoryLocation{offset=offset, base=bReg, index=NoMemIndex}} :: rest,
                      state=NormalState state, code, context=context} =
            let
                (* The address of the argument will be on the stack. *)
                val {valueStack, ...} = checkTopInRax(state, pRegNum bReg)

                val floatingPointReg = FPReg fp0
                val destReg = uRegNum dest
                val resultState =
                    {valueStack=valueStack, ccState=CCIndeterminate, untaggedRegs=[{ureg=destReg, reg=floatingPointReg}]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                          code=
                            X87FPUnaryOps{fpOp=fpOp, dest=floatingPointReg, source=RegisterArgument floatingPointReg} ::
                            LoadArgument{source=MemoryLocation{offset=offset, base=raxAsArg, index=NoMemIndex},
                                dest=floatingPointReg, kind=MoveDouble} ::
                            code,
                          context=context}
            end

        |   codeICode{icode=X87FPUnaryOps _ :: _, ...} =
                raise InternalError "codeICode: X87FPUnaryOps"

        |   codeICode{icode=FloatFixedInt{ dest, source} :: rest, state=NormalState ({untaggedRegs, valueStack, ...}), code, context=context} =
            let
                (* The fixed point value should be in an untagged register. *)
                val op1Reg = argAsUReg source
                val realOpReg =
                    case List.find (fn {ureg, ...} => ureg = op1Reg) untaggedRegs of
                        SOME {reg as GenReg _, ...} => reg
                    |   _ => raise InternalError "codeICode: Multiplication"
                val (floatingPointReg, floatCode) =
                    case fpMode of
                        FPModeX87 => 
                        let
                            (* This is complicated.  The integer value has to be in memory not in a
                               register so we have to push it to the stack and then make sure it is
                               popped afterwards.  Because it is untagged it is unsafe to leave it. *)
                            val fpReg = FPReg fp0
                            val fpCode =
                                [
                                    ResetStackPtr {numWords=1},
                                    FloatFixedInt{dest=fpReg, source=wordOffsetAddress(0, esp)},
                                    PushValue{arg=RegisterArgument realOpReg}
                                ]
                        in
                            (fpReg, fpCode)
                        end
                    |   FPModeSSE2 =>
                        let
                            val fpReg = XMMReg xmm0
                        in
                            (fpReg, [FloatFixedInt{dest=fpReg, source=RegisterArgument realOpReg}])
                        end
                val destReg = uRegNum dest
                val resultState =
                    {valueStack=valueStack, ccState=CCIndeterminate, untaggedRegs=[{ureg=destReg, reg=floatingPointReg}]}
            in
                codeICode{icode=rest, state=NormalState resultState, code=floatCode @ code,context=context}
            end

        |   codeICode{icode=ResetStackPtr _ :: rest, state, code, context} =
            (* This can be skipped. *)
                codeICode{icode=rest, state=state, code=code, context=context}

            (* These should not occur here.  They are only added during this phase. *)
        |   codeICode{icode=PushValue _ :: _, ...} = raise InternalError "codeICode: PushValue"
        |   codeICode{icode=ExchangeRegisters _ :: _, ...} = raise InternalError "codeICode: ExchangeRegisters"
        
        datatype tooHardReason = NotEnoughRegs | MustPushReg | NotDoneInstr | OtherReason
        
        exception TooHard of tooHardReason

        val (identifiedCode, regStates) = identifyRegisters {icode=icode, maxPRegs=maxPRegs}
        
        val reportSuccess = ref false

        (* Process arithmetic operations in the code.  Arithmetic operations require at least one
           operand in a register and deliver the result in the same register.  We need to
           check whether we can reuse an argument and copy it into a new register if
           necessary. *)
(*        local
            val pregCounter = ref 0
            val pregMap = Array.array(maxPRegs, NONE: int option)
            
            fun mapSrcReg(PReg(n, kind)) = PReg(valOf(Array.sub(pregMap, n)), kind)
            
            fun mapIndex NoMemIndex = NoMemIndex
            |   mapIndex(MemIndex1 r) = MemIndex1(mapSrcReg r)
            |   mapIndex(MemIndex2 r) = MemIndex2(mapSrcReg r)
            |   mapIndex(MemIndex4 r) = MemIndex4(mapSrcReg r)
            |   mapIndex(MemIndex8 r) = MemIndex8(mapSrcReg r)

            fun mapSource(RegisterArgument reg) = RegisterArgument(mapSrcReg reg)
            |   mapSource(MemoryLocation{base, offset, index}) =
                    MemoryLocation{base=mapSrcReg base, offset=offset, index=mapIndex index}
            |   mapSource(c as AddressConstant _) = c
            |   mapSource(c as IntegerConstant _) = c

            (* Map the destination.  If we're returning the result from a conditional
               we can have several points that use the same destination register. *)
            fun mapDest(PReg(n, kind)) =
                case Array.sub(pregMap, n) of
                    SOME newReg => PReg(newReg, kind)
                |   NONE =>
                    let
                        val newReg = !pregCounter before pregCounter := !pregCounter+1
                    in
                        Array.update(pregMap, n, SOME newReg);
                        PReg(newReg, kind)
                    end

            fun codeArithmetic((LoadArgument{source, dest=dest, kind}, _)) =
                    LoadArgument{source=mapSource source, dest=mapDest dest, kind=kind}

            |   codeArithmetic((StoreArgument{source, offset, base, index, kind}, _)) =
                    StoreArgument{source=mapSource source, base=mapSrcReg base, offset=offset, index=mapIndex index, kind=kind}

            |   codeArithmetic((LoadStack { offset, dest}, _)) =
                    LoadStack { offset=offset, dest=mapDest dest}

            |   codeArithmetic((LoadMemReg { offset, dest}, _)) =
                    LoadMemReg { offset=offset, dest=mapDest dest}

            |   codeArithmetic((BeginFunction {regArgs}, _)) =
                    BeginFunction {regArgs=map(fn (p, r) => (mapDest p, r)) regArgs}

            |   codeArithmetic((FunctionCall{callKind, regArgs, stackArgs, dest, isTail}, _)) =
                    FunctionCall{
                        callKind=callKind,
                        regArgs=map(fn (p, r) => (mapSource p, r)) regArgs,
                        stackArgs=map mapSource stackArgs, dest=mapDest dest, isTail=isTail}

            |   codeArithmetic((AllocateMemoryOperation{size, flags, dest}, _)) =
                    AllocateMemoryOperation{size=size, flags=flags, dest=mapDest dest}

            |   codeArithmetic((AllocateMemoryVariable{size, flags, dest, initialiser}, _)) =
                    AllocateMemoryVariable{size=mapSource size, flags=mapSource flags, dest=mapDest dest, initialiser=Option.map mapSrcReg initialiser}

            |   codeArithmetic((InitialisationComplete{dest}, _)) =
                    InitialisationComplete{dest=mapSrcReg dest}

            |   codeArithmetic((StartLoop{arguments, loopLabel}, _)) =
                    StartLoop{
                        arguments=map (fn {loopReg, source} => {loopReg=mapDest loopReg, source=mapSource source}) arguments,
                        loopLabel=loopLabel}

            |   codeArithmetic((EndLoop{loopLabel}, _)) = EndLoop{loopLabel=loopLabel}

            |   codeArithmetic((JumpLoop{arguments, loopLabel}, _)) =
                    JumpLoop{
                        arguments=map (fn {loopReg, source} => {loopReg=mapSrcReg loopReg, source=mapSource source}) arguments,
                        loopLabel=loopLabel}

            |   codeArithmetic((RaiseExceptionPacket{packet}, _)) =
                    RaiseExceptionPacket{packet=mapSource packet}

            |   codeArithmetic((ReserveContainer{size, address}, _)) =
                    ReserveContainer{size=size, address=mapDest address}

            |   codeArithmetic((IndexedCaseOperation{testReg, workReg, cases, startValue}, _)) =
                    IndexedCaseOperation{testReg=mapSrcReg testReg, workReg=mapDest workReg, cases=cases, startValue=startValue}

            |   codeArithmetic((LockMutable{addr}, _)) =
                    LockMutable{addr=mapSource addr}
        
            |   codeArithmetic((ForwardJumpLabel{label, result}, _)) =
                    ForwardJumpLabel{label=label, result=Option.map mapSrcReg result}

            |   codeArithmetic((UnconditionalForwardJump{label}, _)) = UnconditionalForwardJump{label=label}

            |   codeArithmetic((ConditionalForwardJump{condition, label, ccRef}, _)) =
                    ConditionalForwardJump{condition=condition, label=label, ccRef=ccRef}

            |   codeArithmetic((WordComparison{arg1, arg2, ccRef}, _)) =
                    WordComparison{arg1=mapSource arg1, arg2=mapSource arg2, ccRef=ccRef}

            |   codeArithmetic((PushExceptionHandler{handlerAddr, handleStart}, _)) =
                    PushExceptionHandler{handlerAddr=mapDest handlerAddr, handleStart=handleStart}

            |   codeArithmetic((PopExceptionHandler{handlerAddr, resultReg, workReg}, _)) =
                    PopExceptionHandler{handlerAddr=mapSrcReg handlerAddr, resultReg=Option.map mapSrcReg resultReg, workReg=mapDest workReg}

            |   codeArithmetic((BeginHandler{handleStart, packetReg, workReg}, _)) =
                    BeginHandler{handleStart=handleStart, packetReg=mapDest packetReg, workReg=mapDest workReg}

            |   codeArithmetic((ReturnResultFromFunction{resultReg, numStackArgs}, _)) =
                    ReturnResultFromFunction{resultReg=mapSrcReg resultReg, numStackArgs=numStackArgs}

            |   codeArithmetic((ArithmeticFunction{oper, resultReg, operand1, operand2, ccRef}, _)) =
                    ArithmeticFunction{oper=oper, resultReg=mapDest resultReg, operand1=mapSource operand1, operand2=mapSource operand2, ccRef=ccRef}

            |   codeArithmetic((TestTagBit{arg, ccRef}, _)) =
                    TestTagBit{arg=mapSource arg, ccRef=ccRef}

            |   codeArithmetic((PushValue{arg}, _)) = PushValue{arg=mapSource arg}

            |   codeArithmetic((ResetStackPtr{numWords}, _)) = ResetStackPtr{numWords=numWords}

            |   codeArithmetic((TagValue{source, dest}, _)) =
                    TagValue{source=mapSrcReg source, dest=mapDest dest}

            |   codeArithmetic((UntagValue{source, dest, isSigned}, _)) =
                    UntagValue{source=mapSource source, dest=mapDest dest, isSigned=isSigned}

            |   codeArithmetic((LoadEffectiveAddress{base, offset, index, dest}, _)) =
                    LoadEffectiveAddress{base=Option.map mapSrcReg base, offset=offset, index=mapIndex index, dest=mapDest dest}

            |   codeArithmetic((ShiftOperation{shift, resultReg, operand, shiftAmount, ccRef}, _)) =
                    ShiftOperation{shift=shift, resultReg=mapDest resultReg, operand=mapSource operand, shiftAmount=mapSource shiftAmount, ccRef=ccRef}

            |   codeArithmetic((Multiplication{resultReg, operand1, operand2, ccRef}, _)) =
                    Multiplication{resultReg=mapDest resultReg, operand1=mapSource operand1, operand2=mapSource operand2, ccRef=ccRef}

            |   codeArithmetic((Division{isSigned, dividend, divisor, quotient, remainder}, _)) =
                    Division{isSigned=isSigned, dividend=mapSource dividend, divisor=mapSource divisor, quotient=mapDest quotient, remainder=mapDest remainder}

            |   codeArithmetic((AtomicExchangeAndAdd{destAddr, source}, _)) =
                    AtomicExchangeAndAdd{destAddr=mapSource destAddr, source=mapSrcReg source}

            |   codeArithmetic((BoxValue{boxKind, source, dest}, _)) =
                    BoxValue{boxKind=boxKind, source=mapSrcReg source, dest=mapDest dest}

            |   codeArithmetic((CompareByteVectors{vec1Addr, vec2Addr, length, ccRef}, _)) =
                    CompareByteVectors{vec1Addr=mapSrcReg vec1Addr, vec2Addr=mapSrcReg vec2Addr, length=mapSource length, ccRef=ccRef}

            |   codeArithmetic((BlockMove{srcAddr, destAddr, length, isByteMove}, _)) =
                    BlockMove{srcAddr=mapSrcReg srcAddr, destAddr=mapSrcReg destAddr, length=mapSource length, isByteMove=isByteMove}

            |   codeArithmetic((CompareFloatingPt{arg1, arg2, ccRef}, _)) =
                    CompareFloatingPt{arg1=mapSource arg1, arg2=mapSource arg2, ccRef=ccRef}

            |   codeArithmetic((X87FPGetCondition{dest, ccRef}, _)) =
                    X87FPGetCondition{dest=mapDest dest, ccRef=ccRef}

            |   codeArithmetic((X87FPArith{opc, resultReg, arg1, arg2}, _)) =
                    X87FPArith{opc=opc, resultReg=mapDest resultReg, arg1=mapSource arg1, arg2=mapSource arg2}
        
            |   codeArithmetic((X87FPUnaryOps{fpOp, dest, source}, _)) =
                    X87FPUnaryOps{fpOp=fpOp, dest=mapDest dest, source=mapSource source}

            |   codeArithmetic((FloatFixedInt{dest, source}, _)) =
                    FloatFixedInt{dest=mapDest dest, source=mapSource source}
        
            |   codeArithmetic((SSE2FPArith{opc, resultReg, arg1, arg2}, _)) =
                    SSE2FPArith{opc=opc, resultReg=mapDest resultReg, arg1=mapSource arg1, arg2=mapSource arg2}
        in
            val postArithCode = map codeArithmetic decoratedCode
            val postArithPRegCount = !pregCounter
        end *)

        (* Allocated registers.  This is set to the real register that is used
           for a specific pseudo-register.  Once a register is allocated that
           is fixed. *)
        val allocatedRegs = Array.array(maxPRegs, NONE: reg option)
        
        (* Hint values.  The idea of hints is that by using a hinted register
           we may avoid an unnecessary move instruction.  realHints is set when
           a pseudo-register is going to be loaded from a specific register
           e.g. a register argument, or moved into one e.g. ecx for a shift.
           friends is set to the other pseudo-registers that may be associated
           with the pseudo-register.  E.g. the argument and destination of
           an arithmetic operation where choosing the same register for
           each may avoid a move. *)
        val realHints = Array.array(maxPRegs, NONE: reg option)
        val friends = Array.array(maxPRegs, []: int list)
        
        fun addRealHint(r, reg) =
            case Array.sub(realHints, r) of
                NONE => Array.update(realHints, r, SOME reg)
            |   SOME _ => ()
        
        fun addFriends(f1, f2) =
        let
            val {conflicts, ...} = Vector.sub(regStates, f1)
            
            fun addF (f, toF) =
                let
                    val currentFriends = Array.sub(friends, toF)
                in
                    if List.exists(fn i => i=f) currentFriends
                    then ()
                    else Array.update(friends, toF, f :: currentFriends)
                end
        in
            (* Add a friend if there's no conflict. *)
            if List.exists(fn i => i=f2) conflicts
            then ()
            else (addF(f1, f2); addF(f2, f1))
        end

        (* Find a real register for a preg.
           1.  If a register is already allocated use that.
           2.  Try the "preferred" register if one has been given.
           3.  Try the realHints value if there is one.
           4.  See if there is a "friend" that has an appropriate register
           5.  Look at all the registers and find one. *)
        fun findRegister(r, pref, regSet) =
            case Array.sub(allocatedRegs, r) of
                SOME reg => reg
            |   NONE =>
                let
                    val {conflicts, realConflicts, pushState, ...} = Vector.sub(regStates, r)
                    (* If we must push this we've got a problem. *)
                    val _ = pushState <> MustPush orelse raise TooHard MustPushReg
                    (* Find the registers we've already allocated that may conflict. *)
                    val conflictingRegs =
                        List.mapPartial(fn i => Array.sub(allocatedRegs, i)) conflicts @
                            realConflicts

                    fun isFree aReg = not (List.exists(fn i => i=aReg) conflictingRegs)

                    fun tryAReg NONE = NONE
                    |   tryAReg (somePref as SOME prefReg) =
                            if isFree prefReg
                            then (Array.update(allocatedRegs, r, somePref); somePref)
                            else NONE
                    
                    fun findAReg [] = raise TooHard NotEnoughRegs
                    |   findAReg (reg::regs) =
                            if isFree reg then (Array.update(allocatedRegs, r, SOME reg); reg)
                            else findAReg regs

                    (* See if there is a friend that has a register already or a
                       hint.  Friends are registers that don't conflict and can
                       possibly avoid an extra move. *) 
                    fun findAFriend([], _) = NONE
                    |   findAFriend(friend :: tail, old) =
                        let
                            val possReg =
                                case Array.sub(allocatedRegs, friend) of
                                    v as SOME _ => tryAReg v
                                |   NONE => tryAReg(Array.sub(realHints, friend))
                        in
                            case possReg of
                                reg as SOME _ => reg
                            |   NONE =>
                                let
                                    (* Add a friend of a friend to the list if we haven't already
                                       seen it and it doesn't conflict. *)
                                    fun newFriend f =
                                        not(List.exists (fn n => n=f) old) andalso
                                            not(List.exists (fn n => n=f) conflicts)
                                    val fOfF = List.filter newFriend (Array.sub(friends, friend))
                                in
                                    findAFriend(tail @ fOfF, friend :: old)
                                end
                        end
                in
                    case tryAReg pref of
                        SOME r => r
                    |   NONE =>
                        (
                            case tryAReg (Array.sub(realHints, r)) of
                                SOME r => r
                            |   NONE =>
                                (
                                    case findAFriend(Array.sub(friends, r), []) of
                                        SOME r => r
                                        (* Look through the registers to find one that's free. *)
                                    |   NONE => findAReg regSet
                                )
                        )
                end

        fun findGeneralReg r = findRegister(r, NONE, generalRegisters)

        fun codeExtArgument(RegisterArgument(PReg(r, _))) = RegisterArgument(findGeneralReg r)
        |   codeExtArgument(AddressConstant m) = AddressConstant m
        |   codeExtArgument(IntegerConstant i) = IntegerConstant i
        |   codeExtArgument(MemoryLocation{base=PReg(bReg, _), offset, index}) =
                MemoryLocation{base=findGeneralReg bReg, offset=offset, index=codeExtIndex index}
        |   codeExtArgument(StackLocation{offset, adjustment}) =
                MemoryLocation{base=GenReg esp, offset=offset+adjustment, index=NoMemIndex}

        and codeExtIndex NoMemIndex = NoMemIndex
        |   codeExtIndex(MemIndex1(PReg(r, _))) = MemIndex1(findGeneralReg r)
        |   codeExtIndex(MemIndex2(PReg(r, _))) = MemIndex2(findGeneralReg r)
        |   codeExtIndex(MemIndex4(PReg(r, _))) = MemIndex4(findGeneralReg r)
        |   codeExtIndex(MemIndex8(PReg(r, _))) = MemIndex8(findGeneralReg r)

        fun moveRR{src, dst} = LoadArgument{source=RegisterArgument src, dest=dst, kind=MoveWord}

        fun moveIfNecessary{src, dst} =
            if src = dst then [] else [moveRR{src=src, dst=dst}]

        (* Where we have multiple specific registers as either source or
           destination there is the potential that a destination register
           if currently in use as a source.  We have to do the moves in the
           right order and use exchanges if necessary.
           Since we're only dealing with a very few registers there's
           no need to avoid quadratic performance. *) 
        fun moveMultipleRegisters regPairList =
            (* Filter out cases where they're (now) in the right registers. *)
            case List.filter(fn {src, dst} => src <> dst) regPairList of
                [] => []
            |   filtered =>
                let
                    (* Look for destinations that don't appear as sources.  For those
                       we can use simple moves. *)
                    val (simpleMove, needXchng) =
                        List.partition(fn {dst, ...} => List.all(fn {src, ...} => src <> dst) filtered) filtered
                    val moveInstrs = List.map moveRR simpleMove
                    (* What's left are items that appear as both sources and destinations.  If we exchange the
                       source and destination of an item we will set that destination as the correct value.
                       Afterwards the old destination value is in the source register and vice versa. *)
                    val (exchangeCode, remainder) =
                        case needXchng of
                            [] => ([], [])
                        |   {src=oldSrc, dst=oldDest} :: rest =>
                            let
                                (* Swap the values. *)
                                fun swapSrcs {src, dst} =
                                    {src=if src=oldSrc then oldDest else if src=oldDest then oldSrc else src, dst=dst}
                                val swapped = List.map swapSrcs rest
                            in
                                ([ExchangeRegisters{regX=oldSrc, regY=oldDest}], swapped)
                            end
                in
                    moveInstrs @ exchangeCode @ moveMultipleRegisters remainder
                end

        fun codeExtended([], _) = []

        |   codeExtended(LoadArgument{ kind=MoveFloat, ... } :: _, _) = raise TooHard NotDoneInstr
        |   codeExtended(LoadArgument{ kind=MoveDouble, ... } :: _, _) = raise TooHard NotDoneInstr

        |   codeExtended(LoadArgument{source=RegisterArgument(PReg(sreg, _)), dest=PReg(dreg, _), kind=MoveWord} :: rest, context) =
            (* Register to register move.  Try to use the same register for the source as the destination
               to eliminate the instruction. *)
            let
                val () = addFriends (sreg, dreg)
                val code = codeExtended(rest, context)
            in
                case Array.sub(allocatedRegs, dreg) of
                    NONE => (* The result was never used *) code
                |   SOME realDestReg =>
                    let
                        (* Get the source register using the current destination as a preference. *)
                        val realSrcReg = findRegister(sreg, SOME realDestReg, generalRegisters)
                    in
                        (* If the source is the same as the destination we don't need to do anything. *)
                        moveIfNecessary{src=realSrcReg, dst=realDestReg} @ code
                    end
            end

        |   codeExtended(LoadArgument{source, dest=PReg(pr, _), kind} :: rest, context) =
            (* Loads of constants or from an address. *)
            let
                val code = codeExtended(rest, context)
            in
                (* If we don't have a register that means the result is never used. *)
                case Array.sub(allocatedRegs, pr) of
                    SOME regResult => LoadArgument{source=codeExtArgument source, dest=regResult, kind=kind} :: code
                |   NONE => code
            end

        |   codeExtended(StoreArgument{ kind=MoveFloat, ... } :: _, _) = raise TooHard NotDoneInstr
        |   codeExtended(StoreArgument{ kind=MoveDouble, ... } :: _, _) = raise TooHard NotDoneInstr

        |   codeExtended(
                StoreArgument{
                    source as RegisterArgument(PReg(sReg, _)), base=PReg(bReg, _), offset, index, kind=MoveByte, ... } :: rest, context) =
            if isX64
            then
            let
                val code = codeExtended(rest, context)
            in
                StoreArgument{
                    source=codeExtArgument source, base=findGeneralReg bReg, offset=offset, index=codeExtIndex index, kind=MoveByte}
                        :: code
            end
            else
            (* This is complicated on X86/32.  We can't use edi or esi for the store registers.  Instead
               we reserve ecx (see special case in "identify") and use that if we have to. *)
            let
                val () = addRealHint(sReg, GenReg ecx)
                val code = codeExtended(rest, context)
                val realStoreReg = findRegister(sReg, SOME(GenReg ecx), generalRegisters)
                val (moveCode, storeReg) =
                    if realStoreReg = GenReg edi orelse realStoreReg = GenReg esi
                    then (moveIfNecessary{src=realStoreReg, dst=GenReg ecx}, GenReg ecx)
                    else ([], realStoreReg)
            in
                moveCode @
                (StoreArgument{
                    source=RegisterArgument storeReg, base=findGeneralReg bReg, offset=offset, index=codeExtIndex index, kind=MoveByte}
                        :: code)
            end

        |   codeExtended(StoreArgument{ source, base=PReg(bReg, _), offset, index, kind as MoveWord } :: rest, context) =
            let
                val code = codeExtended(rest, context)
            in
                StoreArgument{
                    source=codeExtArgument source, base=findGeneralReg bReg, offset=offset, index=codeExtIndex index, kind=kind}
                        :: code
            end

        |   codeExtended(StoreArgument{ source, base=PReg(bReg, _), offset, index, kind } :: rest, context) =
            let
                val code = codeExtended(rest, context)
            in
                StoreArgument{
                    source=codeExtArgument source, base=findGeneralReg bReg, offset=offset, index=codeExtIndex index, kind=kind}
                        :: code
            end

        |   codeExtended(LoadMemReg { offset, dest=PReg(pr, _)} :: rest, context) =
            (* Load from the "memory registers" pointed at by ebp. *)
            let
                val code = codeExtended(rest, context)
            in
                (* If we don't have a register that means the result is never used. *)
                case Array.sub(allocatedRegs, pr) of
                    SOME regResult =>
                        LoadArgument{source=MemoryLocation{base=GenReg ebp, offset=offset, index=NoMemIndex}, dest=regResult, kind=MoveWord} :: code
                |   NONE => code
            end

        |   codeExtended(ExchangeRegisters _ :: _, _) =
                (* This is only generated in this pass. *)
                raise InternalError "codeExtended - ExchangeRegisters"

        |   codeExtended(BeginFunction{regArgs} :: rest, context) =
            let
                val () = List.app (fn (PReg(pr, _), reg) => addRealHint(pr, reg)) regArgs
                val code = codeExtended(rest, context)

                (* Look up the register we need to move the argument into.
                   If there's no register then we don't use this argument and
                   can drop it. *)
                fun mkPair(PReg(pr, _), rr) =
                    case Array.sub(allocatedRegs, pr) of
                        NONE => NONE
                    |   SOME regResult => SOME{src=rr,dst=regResult}
                val regPairs = List.mapPartial mkPair regArgs
            in
                moveMultipleRegisters regPairs @ code
            end

        |   codeExtended(TailRecursiveCall{callKind, regArgs, stackArgs=[], returnAddr as {srcStack, stack}, stackAdjust} :: rest, context) =
            let
                (* Currently just handle the case where neither the function being called
                   nor this function have stack args.  That means that the return address
                   stays where it is. *)
                val _ = currentStackArgs = 0 orelse raise TooHard NotDoneInstr
                val _ = stackAdjust >= 0 orelse raise InternalError "codeExtended - stack adjust"
                val _ = if stackAdjust > 0 then reportSuccess := true else ()
                val () = List.app (fn (RegisterArgument(PReg(pr, _)), reg) => addRealHint(pr, reg) | _ => ()) regArgs
                val code = codeExtended(rest, context)
                (* The register argument list contains "argument" values.  The entries are
                   either RegisterArgument or constants.  Do the register arguments first.
                   We may be using some of the registers that need to contain constants. *)
                fun getRegArgs(RegisterArgument(PReg(pr, _)), reg) =
                        SOME{dst=reg, src=findRegister(pr, SOME reg, generalRegisters)}
                |   getRegArgs _ = NONE
                val loadRegArgs = moveMultipleRegisters(List.mapPartial getRegArgs regArgs)

                fun getConstArgs(AddressConstant m, reg) =
                    SOME(LoadArgument{source=AddressConstant m, dest=reg, kind=MoveWord})
                |   getConstArgs(IntegerConstant i, reg) =
                    SOME(LoadArgument{source=IntegerConstant i, dest=reg, kind=MoveWord})
                |   getConstArgs(RegisterArgument _, _) = NONE
                |   getConstArgs(MemoryLocation _, _) = raise InternalError "FunctionCall - tail - MemoryLocation"
                |   getConstArgs(StackLocation _, _) = raise InternalError "FunctionCall - tail - StackLocation"
                val loadConstArgs = List.mapPartial getConstArgs regArgs
                val adjustStack =
                    if stackAdjust = 0
                    then []
                    else [ResetStackPtr{numWords=stackAdjust}]
            in
                loadRegArgs @ loadConstArgs @ adjustStack @
                (TailRecursiveCall{regArgs=[], stackArgs=[], callKind=callKind, returnAddr={srcStack=0, stack=0}, stackAdjust=0} :: code)
            end

        |   codeExtended(TailRecursiveCall _ :: _, _) = raise TooHard NotDoneInstr

        |   codeExtended(FunctionCall{callKind, regArgs, stackArgs=[], dest=PReg(dReg, _)} :: rest, context) =
            let
                val () = List.app (fn (RegisterArgument(PReg(pr, _)), reg) => addRealHint(pr, reg) | _ => ()) regArgs
                val () = addRealHint(dReg, GenReg eax)
                val code = codeExtended(rest, context)
                val destReg = findRegister(dReg, SOME(GenReg eax), generalRegisters)
                
                (* We don't currently allow the arguments to be memory locations and instead
                   force them into registers.  That may be simpler especially if we can get the
                   values directly into the required register. *)
                fun getRegArgs(RegisterArgument(PReg(pr, _)), reg) =
                        SOME{dst=reg, src=findRegister(pr, SOME reg, generalRegisters)}
                |   getRegArgs _ = NONE
                val loadRegArgs = moveMultipleRegisters(List.mapPartial getRegArgs regArgs)

                fun getConstArgs(AddressConstant m, reg) =
                    SOME(LoadArgument{source=AddressConstant m, dest=reg, kind=MoveWord})
                |   getConstArgs(IntegerConstant i, reg) =
                    SOME(LoadArgument{source=IntegerConstant i, dest=reg, kind=MoveWord})
                |   getConstArgs(RegisterArgument _, _) = NONE
                |   getConstArgs(MemoryLocation _, _) = raise InternalError "FunctionCall - MemoryLocation"
                |   getConstArgs(StackLocation _, _) = raise InternalError "FunctionCall - StackLocation"
                val loadConstArgs = List.mapPartial getConstArgs regArgs
            in
                loadRegArgs @ loadConstArgs @
                    (FunctionCall{regArgs=[], stackArgs=[], dest=raxAsArg, callKind=callKind} ::
                        moveIfNecessary{dst=destReg, src=GenReg eax}) @ code
            end

        |   codeExtended(FunctionCall _ :: _, _) = raise TooHard OtherReason

        |   codeExtended(AllocateMemoryOperation{ size, flags, dest=PReg(dReg, _), saveRegs} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val preserve = List.map(fn (PReg(r, _)) => findGeneralReg r) saveRegs
            in
                AllocateMemoryOperation{ size=size, flags=flags, dest=findGeneralReg dReg, saveRegs=preserve} :: code
            end

        |   codeExtended(AllocateMemoryVariable{size=PReg(sReg, _), dest=PReg(dReg, _), saveRegs} :: rest, context) =
            let
                (* Simple case - no initialiser. *)
                val code = codeExtended(rest, context)
                val preserve = List.map(fn (PReg(r, _)) => findGeneralReg r) saveRegs
                val destReg = findGeneralReg dReg
            in
                AllocateMemoryVariable{size=findGeneralReg sReg, dest=destReg, saveRegs=preserve} :: code
            end

        |   codeExtended(InitialiseMem{size=PReg(sReg, _), addr=PReg(aReg, _), init=PReg(iReg, _)} :: rest, context) =
            let
                (* We are going to use rep stosl/q to set the memory.
                   That requires the length to be in ecx, the initialiser to be in eax and
                   the destination to be edi. *)
                val () = addRealHint(aReg, GenReg edi)
                val () = addRealHint(iReg, GenReg eax)
                val () = addRealHint(sReg, GenReg ecx)
                val code = codeExtended(rest, context)
                val realAddrReg = findRegister(aReg, SOME(GenReg edi), generalRegisters)
                val realInitReg = findRegister(iReg, SOME(GenReg eax), generalRegisters)
                val realSizeReg = findRegister(sReg, SOME(GenReg ecx), generalRegisters)
            in
                moveMultipleRegisters[
                    {src=realInitReg, dst=GenReg eax}, {src=realSizeReg, dst=GenReg ecx}, {src=realAddrReg, dst=GenReg edi}] @
                    InitialiseMem{size=GenReg ecx, addr=GenReg edi, init=GenReg eax} :: code
            end

        |   codeExtended(InitialisationComplete _ :: rest, context) =
                InitialisationComplete{dest=raxAsArg (* unused *)} :: codeExtended(rest, context)

        |   codeExtended(StartLoop{arguments, loopLabel} :: rest, context) =
            let
                (* Make the loop arguments and the sources "friends". *)
                fun makeFriends {loopReg=PReg(dReg, _), source=RegisterArgument(PReg(sReg, _))} = addFriends(dReg, sReg)
                |   makeFriends _ = ()
                val () = List.app makeFriends arguments
                val code = codeExtended(rest, context)
                (* Evaluate the arguments.  We currently just generate register arguments
                   and that may be simpler anyway.  *)
                fun moveArg {loopReg=PReg(dReg, _), source=RegisterArgument(PReg(sReg, _))} =
                let
                    val destReg = findGeneralReg dReg
                    val srcReg = findRegister(sReg, SOME destReg, generalRegisters)
                in
                    {src=srcReg, dst=destReg}
                end
                |   moveArg _ = raise InternalError "moveArg"

                val moveList = List.map moveArg arguments
            in
                moveMultipleRegisters moveList @ (StartLoop{arguments=[], loopLabel=loopLabel} :: code)
            end

        |   codeExtended(EndLoop _ :: rest, context) = (* Don't need to do anything. *)
                codeExtended(rest, context)

        |   codeExtended(JumpLoop{arguments, loopLabel} :: rest, context) =
            let
                (* TODO: Make the sources and destinations "friends". *)
                val code = codeExtended(rest, context)
                (* Evaluate the arguments.  We currently just generate register arguments
                   and that may be simpler anyway.  *)
                fun moveArg {loopReg=PReg(dReg, _), source=RegisterArgument(PReg(sReg, _))} =
                let
                    val destReg = findGeneralReg dReg
                    val srcReg = findRegister(sReg, SOME destReg, generalRegisters)
                in
                    {src=srcReg, dst=destReg}
                end
                |   moveArg _ = raise InternalError "moveArg"

                val moveList = List.map moveArg arguments
            in
                moveMultipleRegisters moveList @ (JumpLoop{arguments=[], loopLabel=loopLabel} :: code)
            end

        |   codeExtended(RaiseExceptionPacket{ packet=PReg(preg, _) } :: rest, context) =
            let
                val () = addRealHint(preg, GenReg eax)
                val code = codeExtended(rest, context)
                (* The argument must be put into rax. *)
                val argReg = findRegister(preg, SOME(GenReg eax), generalRegisters)
            in
                moveIfNecessary{src=argReg, dst=raxAsArg} @
                    RaiseExceptionPacket {packet= raxAsArg } :: code
            end

        |   codeExtended(ReserveContainer{size, address=PReg(aReg, _)} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val addrReg = findRegister(aReg, NONE, generalRegisters)
                (* The memory must be cleared in case we have a GC. *)
                val pushAll = List.tabulate(size, fn _ => PushValue{arg=IntegerConstant(tag 0)})
            in
                pushAll @ LoadArgument{source=RegisterArgument(GenReg esp), dest=addrReg, kind=MoveWord} :: code
            end

        |   codeExtended(IndexedCaseOperation{testReg, workReg, cases, startValue} :: rest, context) =
                raise TooHard NotDoneInstr

        |   codeExtended(LockMutable{addr=RegisterArgument(PReg(pr, _))} :: rest, context) =
            (* Currently it's always a register. *)
            let
                val code = codeExtended(rest, context)
                val argReg = findRegister(pr, NONE, generalRegisters)
            in
                LockMutable{addr=RegisterArgument argReg} :: code
            end

        |   codeExtended(LockMutable _ :: _, _) = raise InternalError "codeExtended - LockMutable"

        |   codeExtended(ForwardJumpLabel{ label, result=_ } :: rest, context) =
                ForwardJumpLabel{ label=label, result=NONE } :: codeExtended(rest, context)

        |   codeExtended(UnconditionalForwardJump { label } :: rest, context) =
                UnconditionalForwardJump { label=label } :: codeExtended(rest, context)

        |   codeExtended(ConditionalForwardJump{ ccRef, condition, label } :: rest, context) =
                ConditionalForwardJump{ ccRef=ccRef, condition=condition, label=label } :: codeExtended(rest, context)

        |   codeExtended(WordComparison{ arg1 as RegisterArgument _, arg2, ccRef } :: rest, context) =
            let
                val code = codeExtended(rest, context)
            in
                WordComparison{ arg1=codeExtArgument arg1, arg2=codeExtArgument arg2, ccRef=ccRef } :: code
            end

        |   codeExtended(WordComparison _ :: _, _) = raise InternalError "codeExtended - WordComparison"

            (* Set up an exception handler. *)
        |   codeExtended(PushExceptionHandler{handlerAddr=PReg(hReg, _), handleStart} :: rest, context) =
            let
                (* handlerAddr is really a work register. *)
                val code = codeExtended(rest, context)
                val handleReg = findGeneralReg hReg
            in
                PushExceptionHandler{handlerAddr=handleReg, handleStart=handleStart} :: code
            end

            (* Pop an exception handler at the end of a handled section.  Executed if no exception has been raised.
               This removes items from the stack. *)
        |   codeExtended(PopExceptionHandler{workReg=PReg(wReg, _), ...} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val realWork = findGeneralReg wReg
            in
                PopExceptionHandler{handlerAddr=realWork (* Not used. *), resultReg=NONE, workReg=realWork} :: code
            end

            (* Start of a handler.  Sets the address associated with PushExceptionHandler and
               provides a register for the packet.  There is a work register but we could
               use any register other than rax since we will have pushed anything we need. *) 
        |   codeExtended(BeginHandler{handleStart, packetReg=PReg(pReg, _), workReg=_} :: rest, context) =
            let
                (* The exception packet is in rax. *)
                val () = addRealHint(pReg, GenReg eax)
                val code = codeExtended(rest, context)
                val realPktReg = findRegister(pReg, SOME(GenReg eax), generalRegisters)
            in
                BeginHandler{handleStart=handleStart, workReg=GenReg ebx, packetReg=GenReg eax} ::
                    (moveIfNecessary{src=GenReg eax, dst=realPktReg } @ code)
            end

        |   codeExtended(ReturnResultFromFunction { resultReg=PReg(resReg, _), numStackArgs } :: rest, context) =
            let
                val () = addRealHint(resReg, GenReg eax)
                val code = codeExtended(rest, context)
                val resultReg = findRegister(resReg, SOME(GenReg eax), generalRegisters)
                (* If for some reason it's not in the right register we have to move it there. *)
                val moveCode = moveIfNecessary{src=resultReg, dst=raxAsArg}
            in
                moveCode @
                    (ReturnResultFromFunction{resultReg=raxAsArg, numStackArgs=numStackArgs} :: code)
            end

        |   codeExtended(ArithmeticFunction{oper=SUB, resultReg=PReg(resReg, _), operand1=RegisterArgument(PReg(op1Reg, _)),
                                            operand2, ccRef} :: rest, context) =
            (* Subtraction - this is special because it can only be done one way round.  The first argument must
               be in a register. *)
            let
                val () = addFriends (resReg, op1Reg)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                (* Try to put the argument into the same register as the result. *)
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, generalRegisters)
                val op2Arg = codeExtArgument operand2
                (* If we couldn't put it in the result register we have to copy it there. *)
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg} @
                    (ArithmeticFunction{oper=SUB, resultReg=realDestReg, operand1=RegisterArgument realDestReg, operand2=op2Arg, ccRef=ccRef} :: code)
            end

        |   codeExtended(ArithmeticFunction{oper=SUB, ...} :: _, _) = raise InternalError "codeExtended - ArithmeticFunction"

        |   codeExtended(ArithmeticFunction{oper, resultReg=PReg(resReg, _), operand1=RegisterArgument(PReg(op1Reg, _)),
                                            operand2=RegisterArgument(PReg(op2Reg, _)), ccRef} :: rest, context) =
            (* Arithmetic operation with both arguments as registers.  These operations are all symmetric so
               we can try to put either argument into the result reg and then do the operation on the other arg. *)
            let
                val () = addFriends (resReg, op1Reg)
                val () = addFriends (resReg, op2Reg)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, generalRegisters)
                and realOp2Reg = findRegister(op2Reg, SOME realDestReg, generalRegisters)
                val (operandReg, moveInstr) =
                    if realOp1Reg = realDestReg
                    then (realOp2Reg, [])
                    else if realOp2Reg = realDestReg
                    then (realOp1Reg, [])
                    else (realOp2Reg, [moveRR{src=realOp1Reg, dst=realDestReg}])
            in
                moveInstr @
                    (ArithmeticFunction{oper=oper, resultReg=realDestReg, operand1=RegisterArgument realDestReg,
                                        operand2=RegisterArgument operandReg, ccRef=ccRef} :: code)
            end

        |   codeExtended(ArithmeticFunction{oper, resultReg=PReg(resReg, _), operand1=RegisterArgument(PReg(op1Reg, _)),
                                            operand2, ccRef} :: rest, context) =
            (* Arithemtic operation with the first argument in a register and the second a constant or memory location. *)
            let
                val () = addFriends (resReg, op1Reg)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, generalRegisters)
                val op2Arg = codeExtArgument operand2
                (* If we couldn't put it in the result register we have to copy it there. *)
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg} @
                    (ArithmeticFunction{oper=oper, resultReg=realDestReg, operand1=RegisterArgument realDestReg, operand2=op2Arg, ccRef=ccRef} :: code)
            end

        |   codeExtended(ArithmeticFunction{oper, resultReg=PReg(resReg, _), operand1,
                                            operand2=RegisterArgument(PReg(op2Reg, _)), ccRef} :: rest, context) =
            (* Arithemtic operation with the second argument in a register and the first a constant or memory location. *)
            let
                val () = addFriends (resReg, op2Reg)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                val realOp2Reg = findRegister(op2Reg, SOME realDestReg, generalRegisters)
                val op1Arg = codeExtArgument operand1
            in
                moveIfNecessary{src=realOp2Reg, dst=realDestReg} @
                    (ArithmeticFunction{oper=oper, resultReg=realDestReg, operand1=RegisterArgument realDestReg, operand2=op1Arg, ccRef=ccRef} :: code)
            end

        |   codeExtended(ArithmeticFunction _ :: _, _) =
                raise InternalError "codeExtended - ArithmeticFunction"

        |   codeExtended(TestTagBit{arg as RegisterArgument _, ccRef} :: rest, context) =
            let
                (* We currently only handle register arguments but testing a memory
                   operand is perfectly possible. *)
                val code = codeExtended(rest, context)
            in
                TestTagBit{ arg=codeExtArgument arg, ccRef=ccRef } :: code
            end

        |   codeExtended(TestTagBit _ :: _, _) = raise InternalError "codeExtended - TestTagBit"

            (* Not currently generated. *)
        |   codeExtended(PushValue _ :: _, _) = raise InternalError "codeExtended - PushValue"

        |   codeExtended(ResetStackPtr {numWords} :: rest, context) =
            (* This is needed to remove containers on the stack. *)
                ResetStackPtr{numWords=numWords} :: codeExtended(rest, context)

        |   codeExtended(TagValue{source=PReg(srcReg, _), dest=PReg(dReg, _)} :: rest, context) =
            let
                val code = codeExtended(rest, context)
            in
                case Array.sub(allocatedRegs, dReg) of
                    SOME regResult =>
                        let
                            (* If we're using LEA to tag there's we can use any source register. *)
                            val realSReg = findRegister(srcReg, NONE, generalRegisters)
                        in
                            LoadEffectiveAddress { base=NONE, offset=1, index=MemIndex2 realSReg, dest=regResult }  :: code
                        end
                |   NONE => code
            end

        |   codeExtended(UntagValue{source=RegisterArgument(PReg(sReg, _)), dest=PReg(dReg, _), isSigned} :: rest, context) =
            (* Always generates register argument at the moment. *)
            let
                val () = addFriends (sReg, dReg)
                val code = codeExtended(rest, context)
            in
                case Array.sub(allocatedRegs, dReg) of
                    SOME regResult =>
                        let
                            val realSReg = findRegister(sReg, SOME regResult, generalRegisters)
                        in
                            moveIfNecessary{src=realSReg, dst=regResult} @
                                (ShiftOperation{shift=if isSigned then SAR else SHR, resultReg=regResult, operand=RegisterArgument regResult,
                                    shiftAmount=IntegerConstant 1, ccRef=CcRef 0 } :: code)
                        end
                |   NONE => code
            end

        |   codeExtended(UntagValue _ :: _, _) =
                raise InternalError "UntagValue"

        |   codeExtended(LoadEffectiveAddress{base=SOME base, offset=0, index=NoMemIndex, dest} :: rest, context) =
                (* This should be handled at the higher level. *)
                codeExtended(LoadArgument{source=RegisterArgument base, dest=dest, kind=MoveWord} :: rest, context)

        |   codeExtended(LoadEffectiveAddress{base, offset, index, dest=PReg(dReg, _)} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val destReg = findGeneralReg dReg
                val bReg = case base of SOME(PReg(br, _)) => SOME(findGeneralReg br) | NONE => NONE
                val iReg = codeExtIndex index
            in
                LoadEffectiveAddress{base=bReg, offset=offset, index=iReg, dest=destReg} :: code
            end

        |   codeExtended(ShiftOperation{shift, resultReg=PReg(resReg, _), operand=RegisterArgument(PReg(operReg, _)), shiftAmount=IntegerConstant i, ccRef} :: rest, context) =
            let
                val () = addFriends (resReg, operReg)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                val realOpReg = findRegister(operReg, SOME realDestReg, generalRegisters)
            in
                 moveIfNecessary{src=realOpReg, dst=realDestReg} @
                    (ShiftOperation{shift=shift, resultReg=realDestReg, operand=RegisterArgument realDestReg,
                                    shiftAmount=IntegerConstant i, ccRef=ccRef} :: code)
            end

        |   codeExtended(ShiftOperation{shift, resultReg=PReg(resReg, _), operand=RegisterArgument(PReg(operReg, _)),
                                        shiftAmount=RegisterArgument(PReg(shiftReg, _)), ccRef} :: rest, context) =
            let
                val () = addFriends (resReg, operReg)
                val () = addRealHint(shiftReg, GenReg ecx)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                val realShiftReg = findRegister(shiftReg, SOME(GenReg ecx), generalRegisters)
                val realOpReg = findRegister(operReg, SOME realDestReg, generalRegisters)
                (* We want the shift in ecx.  We may not have got it there but the register
                   should be free. *)
            in
                 moveIfNecessary{src=realOpReg, dst=realDestReg} @ moveIfNecessary{src=realShiftReg, dst=GenReg ecx} @
                    (ShiftOperation{shift=shift, resultReg=realDestReg, operand=RegisterArgument realDestReg,
                                    shiftAmount=RegisterArgument(GenReg ecx), ccRef=ccRef} :: code)
            end

        |   codeExtended(ShiftOperation _ :: _, _) = raise InternalError "codeExtended - ShiftOperation"

        |   codeExtended(
                Multiplication{resultReg=PReg(resReg, _), operand1=RegisterArgument(PReg(op1Reg, _)),
                               operand2=RegisterArgument(PReg(op2Reg, _)), ccRef} :: rest, context) =
            let
                (* Treat exactly the same as ArithmeticFunction. *)
                val () = addFriends (resReg, op1Reg)
                val () = addFriends (resReg, op2Reg)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, generalRegisters)
                and realOp2Reg = findRegister(op2Reg, SOME realDestReg, generalRegisters)
                val (operandReg, moveInstr) =
                    if realOp1Reg = realDestReg
                    then (realOp2Reg, [])
                    else if realOp2Reg = realDestReg
                    then (realOp1Reg, [])
                    else (realOp2Reg, [moveRR{src=realOp1Reg, dst=realDestReg}])
            in
                moveInstr @
                    (Multiplication{resultReg=realDestReg, operand1=RegisterArgument realDestReg,
                                        operand2=RegisterArgument operandReg, ccRef=ccRef} :: code)
            end

            (* We currently only generate the register/register case. *)
        |   codeExtended(Multiplication _ :: _, _) = raise InternalError "codeExtended - multiplication TODO"

        |   codeExtended(Division{isSigned, dividend=PReg(regDivid, _), divisor, quotient=PReg(regQuot, _),
                                  remainder=PReg(regRem, _)} :: rest, context) =
            let
                (* Division is specific as to the registers.  The dividend must be eax, quotient is
                   eax and the remainder is edx. *)
                val () = addRealHint(regDivid, GenReg eax)
                val () = addRealHint(regQuot, GenReg eax)
                val () = addRealHint(regRem, GenReg edx)
                val code = codeExtended(rest, context)
                val realDiviReg = findRegister(regDivid, SOME(GenReg eax), generalRegisters)
                val realQuotReg = findRegister(regQuot, SOME(GenReg eax), generalRegisters)
                val realRemReg = findRegister(regRem, SOME(GenReg edx), generalRegisters)
                val divisorArg = codeExtArgument divisor
            in
                (* We may need to move one or more of the registers although normally that
                   won't be necessary.  Almost certainly only either the remainder or the
                   quotient will actually be used. *)
                moveIfNecessary{src=realDiviReg, dst=GenReg eax} @
                    Division{isSigned=isSigned, dividend=GenReg eax, divisor=divisorArg,
                              quotient=GenReg eax, remainder=GenReg edx} ::
                    moveMultipleRegisters[{src=GenReg eax, dst=realQuotReg}, {src=GenReg edx, dst=realRemReg}] @ code
            end

        |   codeExtended(AtomicExchangeAndAdd{base=PReg(bReg, _), source=PReg(sReg, _)} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val srcReg = findGeneralReg sReg
                val baseReg = findGeneralReg bReg
            in
                AtomicExchangeAndAdd{base=baseReg, source=srcReg} :: code
            end

        |   codeExtended(BoxValue{boxKind=BoxLargeWord, source=PReg(sReg, _), dest=PReg(dReg, _), saveRegs} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val preserve = List.map(fn (PReg(r, _)) => findGeneralReg r) saveRegs
                val srcReg = findGeneralReg sReg
                and dstReg = findGeneralReg dReg
            in
                AllocateMemoryOperation{ size=1, flags=0wx1, dest=dstReg, saveRegs=preserve} ::
                StoreArgument{ source=RegisterArgument srcReg, offset=0, base=dstReg, index=NoMemIndex, kind=MoveWord} ::
                InitialisationComplete{dest=dstReg (* unused *)} :: code
            end

        |   codeExtended(BoxValue{boxKind, source, dest, saveRegs} :: rest, context) =
            let
            in
                raise TooHard NotDoneInstr
            end

        |   codeExtended(CompareByteVectors{vec1Addr=PReg(v1Reg, _), vec2Addr=PReg(v2Reg, _), length=PReg(lReg, _), ccRef} :: rest, context) =
            let
                val () = addRealHint(v1Reg, GenReg esi)
                val () = addRealHint(v2Reg, GenReg edi)
                val () = addRealHint(lReg, GenReg ecx)
                val code = codeExtended(rest, context)
                val realV1Reg = findRegister(v1Reg, SOME(GenReg esi), generalRegisters)
                val realV2Reg = findRegister(v2Reg, SOME(GenReg edi), generalRegisters)
                val realLengthReg = findRegister(lReg, SOME(GenReg ecx), generalRegisters)
                (* There's a complication here.  CompareByteVectors generates REPE CMPSB to compare
                   the vectors but the condition code is only set if CMPSB is executed at least
                   once.  If the value in RCX/ECX is zero it will never be executed and the
                   condition code will be unchanged.  We want the result to be "equal" in that
                   case so we need to ensure that is the case.  It's quite possible that the
                   condition code has just been set by shifting RCX/ECX to remove the tag in which
                   case it will have set "equal" if the value was zero.  We use CMP R/ECX,R/ECX which
                   is two bytes in 32-bit but three in 64-bit.
                   If we knew the length was non-zero (e.g. a constant) we could avoid this. *)
            in
                moveIfNecessary{src=realV1Reg, dst=GenReg esi} @ moveIfNecessary{src=realV2Reg, dst=GenReg edi} @
                    moveIfNecessary{src=realLengthReg, dst=GenReg ecx} @
                    (WordComparison {arg1=RegisterArgument rcxAsArg, arg2=RegisterArgument rcxAsArg, ccRef=ccRef} ::
                     CompareByteVectors{vec1Addr=GenReg esi, vec2Addr=GenReg edi, length=GenReg ecx, ccRef=ccRef} :: code)
            end

        |   codeExtended(BlockMove{srcAddr=PReg(sReg, _), destAddr=PReg(dReg, _), length=PReg(lReg, _), isByteMove} :: rest, context) =
            let
                val () = addRealHint(sReg, GenReg esi)
                val () = addRealHint(dReg, GenReg edi)
                val () = addRealHint(lReg, GenReg ecx)
                val code = codeExtended(rest, context)
                val realSrcReg = findRegister(sReg, SOME(GenReg esi), generalRegisters)
                val realDestReg = findRegister(dReg, SOME(GenReg edi), generalRegisters)
                val realLengthReg = findRegister(lReg, SOME(GenReg ecx), generalRegisters)
            in
                moveIfNecessary{src=realSrcReg, dst=GenReg esi} @ moveIfNecessary{src=realDestReg, dst=GenReg edi} @
                    moveIfNecessary{src=realLengthReg, dst=GenReg ecx} @
                    (BlockMove{srcAddr=GenReg esi, destAddr=GenReg edi, length=GenReg ecx, isByteMove=isByteMove} :: code)
            end

        |   codeExtended(CompareFloatingPt{arg1, arg2, ccRef} :: rest, context) =
                raise TooHard NotDoneInstr

        |   codeExtended(X87FPGetCondition{dest, ccRef} :: rest, context) =
                raise TooHard NotDoneInstr

        |   codeExtended(X87FPArith{opc, resultReg, arg1, arg2} :: rest, context) =
                raise TooHard NotDoneInstr
    
        |   codeExtended(X87FPUnaryOps{fpOp, dest, source} :: rest, context) =
                raise TooHard NotDoneInstr

        |   codeExtended(FloatFixedInt{dest, source} :: rest, context) =
                raise TooHard NotDoneInstr
    
        |   codeExtended(SSE2FPArith{opc, resultReg, arg1, arg2} :: rest, context) =
                raise TooHard NotDoneInstr
        
        val icodeTabs = [8, 20, 60]
        
        val () =
            (* Print the code before the transformation. *)
            if DEBUG.getParameter DEBUG.icodeTag debugSwitches
            then
            let
                val printStream = PRETTY.getSimplePrinter(debugSwitches, icodeTabs)

                fun printRegs([], _) = ()
                |   printRegs(_, 0) = printStream "..."
                |   printRegs([i], _) = printStream(Int.toString i)
                |   printRegs(i::l, n) = (printStream(Int.toString i ^ ","); printRegs(l, n-1))

                fun printCode c =
                (
                    printICodeAbstract(c, printStream);
                    printStream "\n"
                )
                
                fun printRegData(i, { conflicts, realConflicts=_, active, defs, refs, pushState }) =
                (
                    printStream (Int.toString i ^ "\t");
                    printStream ("Conflicts="); printRegs(conflicts, 20);
                    printStream (" Active=" ^ Int.toString active);
                    printStream (" Rfs=" ^ Int.toString refs);
                    printStream (" Defs=" ^ Int.toString defs);
                    case pushState of
                        MustPush => printStream " Must push" | MustNotPush => printStream " No push" | MayPush => ();
                    printStream "\n"
                )
            in
                printStream(functionName ^ "\n");
                List.app printCode identifiedCode;
                printStream "\n";
                Vector.appi printRegData regStates
            end
            else ()

        val () = totalFns := !totalFns + 1

        val postTransformCode =
            let
                val code = codeExtended(identifiedCode, [])
                val _ = if !reportSuccess then print("-------Done " ^ functionName ^ "\n") else ()
                val () = successFns := !successFns + 1
            in
                code
            end
            handle TooHard reason =>
            let
                val () = reportSuccess := false
                val incrReg =
                    case reason of NotEnoughRegs => tooManyRegs | MustPushReg => mustPushReg | NotDoneInstr => notDoneInstr | OtherReason => anOther
                val () = incrReg := !incrReg + 1
                val (fnState, fnCode) =
                    codeICode{icode=icode, state=NormalState initialState, code=[], context={loopArgs=[], forwardLabels=[], handlers=[]}}
                val _ = case fnState of Exited => () | _ => raise InternalError "fnstate not exited"
            in
                List.rev fnCode (* Reverse the list *)
            end
        val () =
            if !totalFns mod 200 = 0
            then
                print(concat["---Succeeded ", Int.toString(!successFns), " of ", Int.toString(!totalFns),
                     " Not done=", Int.toString(!notDoneInstr), " Too many regs=", Int.toString(!tooManyRegs), " Must push=", Int.toString(!mustPushReg),
                     " Another=", Int.toString(!anOther), "\n"])
            else ()
        val () =
            (* Print the code after the transformation. *)
            if DEBUG.getParameter DEBUG.icodeTag debugSwitches
            then
            let
                val printStream = PRETTY.getSimplePrinter(debugSwitches, icodeTabs)

                fun printCode c =
                ( printICodeConcrete(c, printStream); printStream "\n")
            in
                printStream(functionName ^ "\n");
                List.app printCode postTransformCode;
                printStream "\n"
            end
            else ()
    in
        (* Ccode-generate it. *)
        codeAsX86Code{icode=postTransformCode, maxLabels = !labelCounter, stackRequired = !maxStack,
                      inputRegisters= argRegsUsed @ (if hasFullClosure then [GenReg edx] else []), debugSwitches=debugSwitches,
                      functionName=functionName}
    end

    structure Sharing =
    struct
        type 'reg x86ICode = 'reg x86ICode
        and abstract = abstract
        and reg   = reg
    end
end;
