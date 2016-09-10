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
    structure STRONGLY:
        sig
            type node
            val stronglyConnectedComponents: node list -> node list list
        end
        where type node = {src: ICODE.reg ICODE.argument, dst: ICODE.destinations}
    sharing ICODE.Sharing = IDENTIFY.Sharing
) : X86ICODETRANSFORMSIG
=
struct
    open ICODE
    open Address
    open IDENTIFY

    val totalFns = ref 0 and successFns = ref 0
    val notDoneInstr = ref 0 and tooManyRegs = ref 0 and mustPushReg = ref 0 and anOther = ref 0 and afterPush = ref 0
    
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
    
    val floatingPtRegisters =
        case fpMode of
            FPModeSSE2 => List.map XMMReg [xmm6, xmm5, xmm4, xmm3, xmm2, xmm1, xmm0]
        |   FPModeX87 => List.map FPReg [fp0, fp1, fp2, fp3, fp4, fp5, fp6, fp7]

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

    fun fallBackTransform{icode, maxLabels, currentStackArgs} =
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

        local
            open RunCall
            val F_mutable_bytes =  Word.fromLargeWord(Word8.toLargeWord(Word8.orb (F_mutable, F_bytes)))
            fun makeRealConst l =
            let
                val r = allocateByteMemory(0wx8 div bytesPerWord, F_mutable_bytes)
                fun setBytes([], _) = ()
                |   setBytes(hd::tl, n) = (storeByte(r, n, hd); setBytes(tl, n+0wx1))
                val () = setBytes(l, 0w0)
                val () = clearMutableBit r
            in
                r
            end
        in
            (* These are floating point constants used to change and mask the sign bit. *)
            val realSignBit: machineWord = makeRealConst [0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx80]
            and realAbsMask: machineWord = makeRealConst [0wxff, 0wxff, 0wxff, 0wxff, 0wxff, 0wxff, 0wxff, 0wx7f]
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
        |   codeICode{icode=PushExceptionHandler{ handleStart, ... } :: rest, state=NormalState state, code, context={forwardLabels, loopArgs, handlers}} =
            let
                val sHandle = ~9999 (* Use this as a temporary marker. *)
                val (pushedState, pushCode) = pushRax state
                val sp = stackPtr pushedState
                val thisHandler = (handleStart, pushedState)
                val startState =
                    {valueStack={preg=sHandle, loc=OnStack(sp+1) (* Two words *)} :: #valueStack pushedState,
                     ccState=CCIndeterminate, untaggedRegs= []}
                val newContext = {forwardLabels=forwardLabels, loopArgs=loopArgs, handlers=thisHandler :: handlers}
            in
                codeICode{icode=rest, state=NormalState startState,
                          code=PushExceptionHandler{workReg=raxAsArg, handleStart=handleStart} :: pushCode @ code, context=newContext}
            end

            (* Pop an exception handler at the end of a handled section.  Executed if no exception has been raised. *)
        |   codeICode{icode=PopExceptionHandler{ resultReg, workReg=_, ... } :: rest, state=NormalState state, code, context} =
            let
                (* handlerStartState is the state BEFORE we pushed the handler. *)
                (* Search down the state until we find the handler. *)
                fun popState [] = raise InternalError "codeICode: PopExceptionHandler - not found"
                |   popState (hState as ({preg, ...} :: tail)) =
                        if preg = ~9999 then hState else popState tail
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
                          code=PopExceptionHandler{ resultReg=NONE (* unused*), workReg=workReg } :: resetCode @ code,
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
                    |   (PRegUntagged, _, _) => newUreg(destReg, pushedState)
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

        |   codeICode{icode=LoadArgument{ source=StackLocation{wordOffset, ...}, dest, kind=MoveWord} :: rest, state=NormalState state, code, context} =
            (* Load from the stack *)
            let
                val destReg = pRegNum dest
                val (pushedState, pushCode) = pushRax state
                (* We have to adjust the offset. *)
                val load = loadStack(eax, (stackPtr pushedState + wordOffset) * wordSize)
            in
                codeICode{icode=rest, state=NormalState(addRaxToState(pushedState, destReg)),
                    code=load @ pushCode @ code, context=context}
            end

        |   codeICode{
                icode=LoadArgument{ source=AddressConstant _, dest=PReg _, kind=MoveDouble} :: rest,
                state=NormalState state, code, context} =
            let
                (* This is just used for abs and ~ for reals. Handle that later. *)
            in
                codeICode{icode=rest, state=NormalState state,
                    code=code, context=context}
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
        |   codeICode{icode=StartLoop{regArgs, loopLabel} :: rest, state=NormalState state, code,
                      context={forwardLabels, handlers, loopArgs}} =
            let
                (* The arguments have been evaluated to the stack.  Replace
                   the preg entries in the state with those of the loop variables. *)
                val (pushedState, pushCode) = pushRax state
                val numArgs = List.length regArgs
                val headList = List.take(#valueStack pushedState, numArgs)
                fun repSourceWithDest((source, loopReg), {preg, loc}) =
                     if argAsPReg source = preg then {preg=pRegNum loopReg, loc=loc} else raise InternalError "codeICode: StartLoop"
                val newHead = ListPair.mapEq repSourceWithDest (List.rev regArgs, headList)
                val tailList = List.drop(#valueStack pushedState, numArgs)
                (* The CCstate is indeterminate because we may have looped. *)
                val bodyState = {valueStack=newHead @ tailList, ccState= CCIndeterminate, untaggedRegs= []}
                val newContext = {loopArgs=(bodyState, loopLabel) :: loopArgs, forwardLabels=forwardLabels, handlers=handlers}
            in
                codeICode{icode=rest, state=NormalState bodyState, code=StartLoop{regArgs=[], loopLabel=loopLabel} :: pushCode @ code,
                          context=newContext}
            end

            (* A loop tail jump.  The arguments have been evaluated to the stack. *)
        |   codeICode{icode=(JumpLoop{regArgs, stackArgs, loopLabel, ...}) :: rest, state=NormalState state, code,
                      context=context as {loopArgs=(startState, lab) :: _, ...}} =
            let
                val _ = loopLabel = lab orelse raise InternalError "codeICode: JumpLoop - mismatched labels"
                val _ = null stackArgs orelse raise InternalError "codeICode: JumpLoop - stack args"
                val (pushedState, pushCode) = pushRax state
                (* Move each argument from the source to the loop variable.  This isn't the same as
                   MoveArgument because the destination already exists. *)
                fun moveArg(source, loopReg) =
                let
                    val load = loadSource(source, eax, pushedState)
                    val store =
                        case findEntry (pRegNum loopReg, pushedState) of
                            OnStack s => storeMemory(eax, esp, (stackPtr pushedState - s - 1) * wordSize)
                        |   InRax => raise InternalError "codeICode: JumpLoop"
                in
                    store @ load
                end
                val storeAll = List.foldl(fn (arg, l) => l @ moveArg arg) [] regArgs
                val instrs =
                    JumpLoop{regArgs=[], stackArgs=[], loopLabel=loopLabel, stackAdjust=0} :: resetStackPtr(stackPtr pushedState - stackPtr startState) :: storeAll
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
                val load = loadSource(packet, eax, pushedState)
            in
                codeICode{icode=rest, state=Exited, code=RaiseExceptionPacket {packet=RegisterArgument raxAsArg } :: load @ pushCode @ code, context=context}
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
                val load = loadSource(RegisterArgument addr, eax, pushedState)
                val lockCode = LockMutable {addr=raxAsArg} :: (load @ pushCode)
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
                        arg1=RegisterArgument arg1Reg,
                        arg2=MemoryLocation{offset=offset2, base=bReg2, index=NoMemIndex}, ccRef} :: rest,
                      state=NormalState state, code, context=context} =
            let
                (* Load the first argument into an SSE2 register or the X87 stack.
                   Compare with the second. *)
                val srcReg = getFPreg(uRegNum arg1Reg, state)
                val ({ valueStack, ...}, arg2Code) = loadToRax(state, RegisterArgument bReg2)
                val checkState = {valueStack= valueStack, ccState=CCState(ccRef, fn c => c), untaggedRegs= []}
            in
                codeICode{icode=rest, state=NormalState checkState,
                          code=
                            CompareFloatingPt{arg1=RegisterArgument srcReg, arg2=MemoryLocation{offset=offset2, base=raxAsArg, index=NoMemIndex}, ccRef=ccRef} ::
                            arg2Code @ code,
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
                        arg1=RegisterArgument arg1Reg,
                        arg2=MemoryLocation{offset=offset2, base=bReg2, index=NoMemIndex}} :: rest,
                      state=NormalState state, code, context=context} =
            let
                (* The address of the first argument will be on the stack.  The second
                   will be in Rax. *)
                val (pushedState as {valueStack, ...}, pushCode) = pushRax state
                (* Load the argument address into rax. *)
                val load = loadSource(RegisterArgument bReg2, eax, pushedState)
                val srcReg = getFPreg(uRegNum arg1Reg, pushedState)

                val destFPReg = FPReg fp0
                val destReg = uRegNum resultReg
                val resultState =
                    {valueStack=valueStack, ccState=CCIndeterminate, untaggedRegs=[{ureg=destReg, reg=destFPReg}]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                          code=
                            X87FPArith{
                                opc=opc, resultReg=destFPReg, arg1=RegisterArgument srcReg,
                                arg2=MemoryLocation{offset=offset2, base=raxAsArg, index=NoMemIndex}} ::
                            load @ pushCode @ code,
                          context=context}
            end

        |   codeICode{icode=X87FPArith _ :: _, ...} =
                raise InternalError "codeICode: X87FPArith"

        |   codeICode{
                icode=SSE2FPArith{
                        opc, resultReg,
                        arg1=RegisterArgument arg1Reg,
                        arg2=MemoryLocation{offset=offset2, base=bReg2, index=NoMemIndex}} :: rest,
                      state=NormalState state, code, context=context} =
            let
                val (pushedState as {valueStack, ...}, pushCode) = pushRax state
                (* Load the argument address into rax. *)
                val load = loadSource(RegisterArgument bReg2, eax, pushedState)
                val srcReg = getFPreg(uRegNum arg1Reg, pushedState)
                val destReg = uRegNum resultReg
                val resultState =
                    {valueStack=valueStack, ccState=CCIndeterminate, untaggedRegs=[{ureg=destReg, reg=srcReg}]}
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
                                opc=opc, resultReg=srcReg, arg1=RegisterArgument srcReg,
                                arg2=MemoryLocation{offset=offset2, base=raxAsArg, index=NoMemIndex}} :: 
                            load @ pushCode @ code,
                          context=context}
            end

        |   codeICode{
                icode=SSE2FPArith{
                        opc, resultReg,
                        arg1=RegisterArgument arg1Reg,
                        arg2=RegisterArgument _} :: rest,
                      state=NormalState state, code, context=context} =
            let
                (* This is used for the logical operations to invert or mask the sign bit.
                   Both arguments need to be loaded into registers because any memory
                   argument must be a 128-bit value. *)
                val srcReg = getFPreg(uRegNum arg1Reg, state)
                val floatingPointReg1 = XMMReg xmm1
                val destReg = uRegNum resultReg
                val resultState =
                    {valueStack= #valueStack state, ccState=CCIndeterminate, untaggedRegs=[{ureg=destReg, reg=srcReg}]}
                val addrConstant =
                    case opc of
                        SSE2Xor => realSignBit
                    |   SSE2And => realAbsMask
                    |   _ => raise InternalError "codeICode - SSE2FPArith"
            in
                codeICode{icode=rest, state=NormalState resultState,
                          code=
                            SSE2FPArith{
                                opc=opc, resultReg=srcReg, arg1=RegisterArgument srcReg,
                                arg2=RegisterArgument floatingPointReg1} ::
                            LoadArgument{source=AddressConstant addrConstant, dest=floatingPointReg1, kind=MoveDouble} ::
                            code,
                          context=context}
            end

        |   codeICode{icode=SSE2FPArith _ :: _, ...} =
                raise InternalError "codeICode: SSE2FPArith"

        |   codeICode{
                icode=X87FPUnaryOps{ fpOp, dest, source=RegisterArgument arg1Reg} :: rest,
                      state=NormalState state, code, context=context} =
            let
                val srcReg = getFPreg(uRegNum arg1Reg, state)
                val destFPReg = FPReg fp0
                val destReg = uRegNum dest
                val resultState =
                    {valueStack= #valueStack state, ccState=CCIndeterminate, untaggedRegs=[{ureg=destReg, reg=destFPReg}]}
            in
                codeICode{icode=rest, state=NormalState resultState,
                          code=
                            X87FPUnaryOps{fpOp=fpOp, dest=destFPReg, source=RegisterArgument srcReg} ::
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

                val (fnState, fnCode) =
                    codeICode{icode=icode, state=NormalState initialState, code=[], context={loopArgs=[], forwardLabels=[], handlers=[]}}
                val _ = case fnState of Exited => () | _ => raise InternalError "fnstate not exited"
    in
        {icode = List.rev fnCode (* Reverse the list *), maxLabels = !labelCounter, stackRequired = !maxStack }
    end
        
    datatype tooHardReason = NotEnoughRegs | MustPushReg | NotDoneInstr | OtherReason
       
    exception TooHard of tooHardReason

    fun addRegisterPushes(code, regStates, maxPRegs, maxLabels) =
    let
        val reportSuccess = ref false
        val labelCounter = ref maxLabels
        val pregCounter = ref 0
        
        (* Each preg in the input is mapped to either a new preg or the stack. *)
        datatype pregMapType = Unset | ToPReg of abstract | ToStack
        val pregMap = Array.array(maxPRegs, Unset)
        
        
        (* The stack contains both entries in the input code and entries added here. *)
        datatype stackEntry =
            NewEntry of {pregNo: int}
        |   ContainerEntry of { size: int }
        |   HandlerEntry
        
        type context =
        {
            oldCount: int,      (* The stack entries from the input code. *)
            newCount: int,      (* The total number of items on the stack. *)
            stack: stackEntry list
        }

        val labelState = Array.array(maxLabels, NONE: context option)

        val emptyContext: context = { oldCount=0, newCount=0, stack=[]}

        fun newPReg kind = PReg(!pregCounter before pregCounter := !pregCounter + 1, kind)
        
        fun getStackOffset(regNo: int, {stack, newCount, ...}: context) =
        let
            fun findEntry(offset, NewEntry {pregNo} :: tl) =
                if regNo = pregNo
                then offset
                else findEntry(offset+1, tl)
            |   findEntry(offset, ContainerEntry {size} :: tl) = findEntry(offset+size, tl)
            |   findEntry(offset, HandlerEntry :: tl) = findEntry(offset+2, tl) (* Occupies two words *)
            |   findEntry(_, []) = raise InternalError "findEntry - not found"
            val stackOffset = findEntry(0, stack)
            val _ = stackOffset < newCount orelse raise InternalError "getStackOffset - bad offset"
        in
            stackOffset
        end
        
        fun mapOldStackOffset(offset, {oldCount, newCount, ...}: context) =
        let
            (* Currently this applies only to arguments or the return address
               that should be beyond the old stack. *)
            val _ = offset >= oldCount orelse raise InternalError "mapOldStackOffset"
        in
            offset - oldCount + newCount
        end
        
        (* Map a source register.  This always loads the argument. *)
        fun mapSrcReg(PReg(n, _), context: context) =
            case Array.sub(pregMap, n) of
                Unset => raise InternalError "mapSrcReg - unset"
            |   ToPReg preg => (preg, [])
            |   ToStack =>
                let
                    (* Make a new untagged register. That will prevent us pushing it if
                       we have to spill registers. *)
                    val newReg = newPReg PRegUntagged
                in
                    (newReg,
                        [LoadArgument{source=StackLocation{wordOffset=getStackOffset(n, context), adjustment=0}, dest=newReg, kind=MoveWord}])
                end
       
        fun mapDestReg(PReg(n, kind), context: context as {stack, oldCount, newCount}) =
        let
            val currentLocation = Array.sub(pregMap, n)
            val {pushState, ...} = Vector.sub(regStates, n)
        in
            case pushState of
                MustPush =>
                let
                    (* This should not have been seen before. *)
                    val _ = case currentLocation of Unset => () | _ => raise InternalError "mapDestReg - already set"
                    val newReg = newPReg kind
                    val () = Array.update(pregMap, n, ToStack)
                    val newContext = { stack = NewEntry{pregNo=n}::stack, oldCount=oldCount, newCount=newCount+1}
                in
                    (newReg, [PushValue{arg=RegisterArgument newReg}], newContext)
                end
            |   _ =>
                let
                (* See if we already have a number for it.  We may encounter the same preg
                   as a destination when returning the result from a conditional in which
                   case we have to use the same number.  We shouldn't have pushed it. *)
                    val newReg =
                        case currentLocation of
                            Unset =>
                            let
                                val newReg = newPReg kind
                                val () = Array.update(pregMap, n, ToPReg newReg)
                            in
                                newReg
                            end
                        |   ToPReg preg => preg
                        |   ToStack => raise InternalError "mapDestReg - already on stack"
                in
                    (newReg, [], context)
                end
        end
        
        (* A work register must be a normal register. *)
        fun mapWorkReg(PReg(n, kind)) =
        let
            val currentLocation = Array.sub(pregMap, n)
            val {pushState, ...} = Vector.sub(regStates, n)
            val _ = case pushState of MustPush => raise InternalError "mapWorkReg - MustPush" | _ => ()
        in
            case currentLocation of
                Unset =>
                let
                    val newReg = newPReg kind
                    val () = Array.update(pregMap, n, ToPReg newReg)
                in
                    newReg
                end
            |   ToPReg preg => preg
            |   ToStack => raise InternalError "mapWorkReg - on stack"
                
        end

        fun mapIndex(NoMemIndex, _) = (NoMemIndex, [])
        |   mapIndex(MemIndex1 r, context) =
                let val (sreg, c) = mapSrcReg(r, context) in (MemIndex1 sreg, c) end
        |   mapIndex(MemIndex2 r, context) =
                let val (sreg, c) = mapSrcReg(r, context) in (MemIndex2 sreg, c) end
        |   mapIndex(MemIndex4 r, context) =
                let val (sreg, c) = mapSrcReg(r, context) in (MemIndex4 sreg, c) end
        |   mapIndex(MemIndex8 r, context) =
                let val (sreg, c) = mapSrcReg(r, context) in (MemIndex8 sreg, c) end

        fun mapSource(RegisterArgument(PReg(r, _)), context: context) =
            (
                case Array.sub(pregMap, r) of
                    Unset => raise InternalError "mapSrcReg - unset"
                |   ToPReg preg => (RegisterArgument preg, [])
                |   ToStack => (StackLocation{wordOffset=getStackOffset(r, context), adjustment=0}, [])
            )

        |   mapSource(a as AddressConstant _, _) = (a, [])
        |   mapSource(i as IntegerConstant _, _) = (i, [])

        |   mapSource(MemoryLocation{base, offset, index}, context) =
            let
                val (baseReg, baseCode) = mapSrcReg(base, context)
                val (indexValue, indexCode) = mapIndex(index, context)
            in
                (MemoryLocation{base=baseReg, offset=offset, index=indexValue}, baseCode @ indexCode)
            end

        |   mapSource(StackLocation{wordOffset, adjustment}, context) =
                (StackLocation{wordOffset=mapOldStackOffset(wordOffset+adjustment, context), adjustment=0}, [])

        (* If the argument AFTER processing is a stack entry, then load it. *)
        fun loadStack(stack as StackLocation _) =
            let
                val newReg = newPReg PRegUntagged
            in
                (RegisterArgument newReg, [LoadArgument{source=stack, dest=newReg, kind=MoveWord}])
            end
        |   loadStack other = (other, [])


        local
            (* We don't need to save registers if we've pushed them.
               N.B.  If we ever cache registers we'll need to clear the cache. *)
            fun mapSaveReg (PReg(n, _), l) =
                case Array.sub(pregMap, n) of
                    Unset => raise InternalError "mapSaveReg - unset"
                |   ToPReg preg => preg :: l
                |   ToStack => l
        in
            val mapSaveRegs = List.foldl mapSaveReg [] 
        end
        
        (* Record the current state against the label.  In almost all cases the label
           will not previously have been set.  The one exception is that floating
           point comparisons sometimes have different jumps to check the parity bit
           and the zero bit. The context will be the same for both so it's safe. *)
        fun addNewLabel(ILabel lab, context) = Array.update(labelState, lab, SOME context)
        
        (* Rewrite the code, replacing any registers that need to be pushed with references to
           the stack.  The result is built up in reverse order and then reversed. *)
        fun pushRegisters([]: abstract x86ICode list, code: abstract x86ICode list, _: context) = code
        
        |   pushRegisters(LoadArgument{source, dest=PReg(dReg, dKind), kind} :: rest, code, context as {stack, oldCount, newCount}) =
            let
                val (sourceVal, sourceCode) = mapSource(source, context)
                (* If we have to push the value we don't have to first load it into a register. *)
                val {pushState, ...} = Vector.sub(regStates, dReg)
                val currentLocation = Array.sub(pregMap, dReg)

                val (destCode, newContext) =
                    case pushState of
                        MustPush =>
                        let
                            val _ = case currentLocation of Unset => () | _ => raise InternalError "LoadArgument - already set"
                            val () = Array.update(pregMap, dReg, ToStack)
                            val newContext = { stack = NewEntry{pregNo=dReg}::stack, oldCount=oldCount, newCount=newCount+1}
                        in
                            (PushValue{arg=sourceVal}, newContext)
                        end
                    |   _ =>
                        let
                            val destReg =
                                case currentLocation of
                                    Unset =>
                                    let
                                        val newReg = newPReg dKind
                                        val () = Array.update(pregMap, dReg, ToPReg newReg)
                                    in
                                        newReg
                                    end
                                |   ToPReg preg => preg
                                |   ToStack => raise InternalError "LoadArgument - already on stack"
                        in
                            (LoadArgument{source=sourceVal, dest=destReg, kind=kind}, context)
                        end
            in
                pushRegisters(rest, destCode  :: sourceCode @ code, newContext)
            end

        |   pushRegisters(StoreArgument{source, offset, base, index, kind} :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSource(source, context)
                (* We can't have a memory-memory store so we have to load the source if it's now the stack. *)
                val (loadedSource, loadCode) = loadStack sourceVal
                val (baseReg, baseCode) = mapSrcReg(base, context)
                val (indexValue, indexCode) = mapIndex(index, context)
            in
                pushRegisters(rest,
                    StoreArgument{source=loadedSource, base=baseReg, offset=offset, index=indexValue, kind=kind} ::
                        indexCode @ baseCode @ loadCode @ sourceCode @ code, context)
            end

        |   pushRegisters(LoadMemReg { offset, dest} :: rest, code, context) =
            let
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ LoadMemReg { offset=offset, dest=destVal} :: code, newContext)
            end

        |   pushRegisters(ExchangeRegisters _ :: _, _, _) = (* This is added at the next level. *)
                raise InternalError "pushRegisters - ExchangeRegisters"

        |   pushRegisters(BeginFunction {regArgs} :: rest, code, context) =
            let
                (* Push any registers that need to be pushed. *)
                fun pushReg((preg, rreg), (others, code, context)) =
                let
                    val (newReg, newCode, newContext) = mapDestReg(preg, context)
                in
                    ((newReg, rreg) :: others, newCode @ code, newContext)
                end
                val (newRegArgs, pushCode, newContext) = List.foldl pushReg ([], [], context) regArgs
            in
                pushRegisters(rest,
                    pushCode @ (BeginFunction {regArgs=newRegArgs} :: code), newContext)
            end

        |   pushRegisters(FunctionCall{callKind, regArgs, stackArgs, dest} :: rest, code, context) =
            let
                (* It's possible that this could lead to having to spill registers in order
                   to load others.  Leave that problem for the moment. *)
                fun loadStackArg (arg, (otherLoads, otherArgs)) =
                let
                    val (argVal, loadCode) = mapSource(arg, context)
                in
                    (loadCode @ otherLoads, argVal :: otherArgs)
                end
                val (stackArgLoads, newStackArgs) = List.foldr loadStackArg ([], []) stackArgs

                fun loadRegArg ((arg, reg), (otherLoads, otherArgs)) =
                let
                    val (argVal, loadCode) = mapSource(arg, context)
                in
                    (loadCode @ otherLoads, (argVal, reg) :: otherArgs)
                end
                val (regArgLoads, newRegArgs) = List.foldr loadRegArg ([], []) regArgs
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @
                        FunctionCall{ callKind=callKind, regArgs=newRegArgs, stackArgs=newStackArgs, dest=destVal} ::
                            regArgLoads @ stackArgLoads @ code, newContext)
            end

        |   pushRegisters(TailRecursiveCall{callKind, regArgs, stackArgs, returnAddr={srcStack, stack}, stackAdjust} :: rest, code,
                          context as {oldCount, newCount, ...}) =
            let
                (* We have to adjust all the stack offsets to account for anything we've pushed.
                   Note: we mustn't use mapOldStackOffset for destination stack offsets.
                   These are not actual source stack items but overwrite the stack. *)
                fun loadStackArg ({src, stack}, (otherLoads, otherArgs)) =
                let
                    val (argVal, loadCode) = mapSource(src, context)
                in
                    (loadCode @ otherLoads, {src=argVal, stack=stack+newCount-oldCount} :: otherArgs)
                end
                val (stackArgLoads, newStackArgs) = List.foldr loadStackArg ([], []) stackArgs
                
                fun loadRegArg ((arg, reg), (otherLoads, otherArgs)) =
                let
                    val (argVal, loadCode) = mapSource(arg, context)
                in
                    (loadCode @ otherLoads, (argVal, reg) :: otherArgs)
                end
                val (regArgLoads, newRegArgs) = List.foldr loadRegArg ([], []) regArgs

                val returnAddr = {srcStack=mapOldStackOffset(srcStack, context), stack=stack+newCount-oldCount}
            in
                skipToLabel(rest,
                    TailRecursiveCall{ callKind=callKind, regArgs=newRegArgs,
                        stackArgs=newStackArgs, returnAddr=returnAddr, stackAdjust=stackAdjust+newCount-oldCount} ::
                            regArgLoads @ stackArgLoads @ code)
            end

        |   pushRegisters(AllocateMemoryOperation{size, flags, dest, saveRegs} :: rest, code, context) =
            let
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ AllocateMemoryOperation{size=size, flags=flags, dest=destVal, saveRegs=mapSaveRegs saveRegs} :: code, newContext)
            end

        |   pushRegisters(AllocateMemoryVariable{size, dest, saveRegs} :: rest, code, context) =
            let
                val (sizeVal, sizeCode) = mapSrcReg(size, context)
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ AllocateMemoryVariable{size=sizeVal, dest=destVal, saveRegs=mapSaveRegs saveRegs} :: sizeCode @ code, newContext)
            end

        |   pushRegisters(InitialiseMem{size, addr, init} :: rest, code, context) =
            let
                val (sizeVal, sizeCode) = mapSrcReg(size, context)
                val (addrVal, addrCode) = mapSrcReg(addr, context)
                val (initVal, initCode) = mapSrcReg(init, context)
            in
                pushRegisters(rest,
                    InitialiseMem{size=sizeVal, addr=addrVal, init=initVal} :: initCode @ addrCode @ sizeCode @ code, context)
            end

        |   pushRegisters(InitialisationComplete{dest} :: rest, code, context) =
            let
                (* TODO: This is only a marker so we really don't want to generate any code at all. *)
                val (destVal, destCode) = mapSrcReg(dest, context)
            in
                pushRegisters(rest,
                    InitialisationComplete{dest=destVal} :: destCode @ code, context)
            end

        |   pushRegisters(StartLoop{regArgs, loopLabel} :: rest, code, context) =
            let
                fun loadArguments ([], ctxt) = ([], [], ctxt)
                |   loadArguments ((source, dest as PReg(n, _)) :: rest, ctxt) =
                    let
                        val (sourceVal, sourceCode) = mapSource(source, ctxt)
                        val (destVal, destCode, newContext) = mapDestReg(dest, ctxt)
                        val (codeRest, otherRegArgs, resContext) =
                            loadArguments(rest, newContext)
                    in
                        (* Did we push this? *)
                        case Array.sub(pregMap, n) of
                            ToStack => (* Yes.  This will become a stack argument. *)
                            let
                                val move = [LoadArgument{source=sourceVal, dest=destVal, kind=MoveWord}]
                            in
                                (codeRest @ destCode @ move @ sourceCode, otherRegArgs, resContext)
                            end
                        |   ToPReg newReg =>
                                (codeRest @ destCode @ sourceCode, (sourceVal, newReg) :: otherRegArgs, resContext)
                       |    Unset => raise InternalError "pushRegisters - StartLoop - unset"
                    end
                val (loadCode, newRegArgs, newContext) = loadArguments(regArgs, context)

                (* Set up the state.  Every jump has to reset to this.  Only the
                   stack pointer is needed. *)
                val () = addNewLabel(loopLabel, newContext)                
            in
                pushRegisters(rest,
                    StartLoop{regArgs=newRegArgs, loopLabel=loopLabel} :: loadCode @ code, newContext)
            end

        |   pushRegisters(EndLoop{loopLabel} :: rest, code, context) =
                pushRegisters(rest, EndLoop{loopLabel=loopLabel} :: code, context)

        |   pushRegisters(JumpLoop{regArgs, stackArgs, loopLabel as ILabel lab, stackAdjust} :: rest, code, context as {newCount, ...}) =
            let
                val _ = null stackArgs orelse raise InternalError "pushRegisters - JumpLoop - stackArgs"
                val {newCount=labNewCount, ...} = valOf(Array.sub(labelState, lab))
                fun getValues [] = ([], [], [])
                |   getValues ((source, PReg(n, _)) :: rest) =
                    let
                        val (otherRegArgs, otherStackArgs, otherCode) = getValues rest
                        val (sourceVal, sourceCode) = mapSource(source, context)
                    in
                        case Array.sub(pregMap, n) of
                            ToPReg lReg => ((sourceVal, lReg) :: otherRegArgs, otherStackArgs, sourceCode @ otherCode)
                        |   ToStack =>
                            let
                                val stackOff = getStackOffset(n, context)
                            in
                                (otherRegArgs, (sourceVal, stackOff) :: otherStackArgs, sourceCode @ otherCode)
                            end
                        |   Unset => raise InternalError "pushRegisters - JumpLoop - unset"
                    end
                val (newRegArguments, newStackArgs, sourceCode) = getValues regArgs
            in
                skipToLabel(rest,
                    JumpLoop{ regArgs=newRegArguments, stackArgs=newStackArgs, loopLabel=loopLabel,
                              stackAdjust=stackAdjust + newCount-labNewCount} :: sourceCode @ code)
            end

        |   pushRegisters(RaiseExceptionPacket{packet} :: rest, code, context) =
            let
                val (packetVal, packetCode) = mapSource(packet, context)
            in
                skipToLabel(rest, RaiseExceptionPacket{packet=packetVal} :: packetCode @ code)
            end

        |   pushRegisters(ReserveContainer{size, address} :: rest, code, {oldCount, newCount, stack}) =
            let
                val containerContext =
                    {oldCount=oldCount+size, newCount=newCount+size, stack=ContainerEntry{size=size}::stack}
                val (destVal, destCode, newContext) = mapDestReg(address, containerContext)
            in
                pushRegisters(rest, destCode @ ReserveContainer{size=size, address=destVal} :: code, newContext)
            end

        |   pushRegisters(IndexedCaseOperation{testReg, workReg, cases, startValue} :: rest, code, context) =
            let
                val (srcVal, srcCode) = mapSrcReg(testReg, context)
                val newWorkReg = mapWorkReg workReg
                (* Set the incoming state for each of the cases. *)
                val () = List.app (fn caseLab => addNewLabel(caseLab, context)) cases
            in
                (* This is an unconditional branch. *)
                skipToLabel(rest, IndexedCaseOperation{testReg=srcVal, workReg=newWorkReg, cases=cases, startValue=startValue} :: srcCode @ code)
            end

        |   pushRegisters(LockMutable{addr} :: rest, code, context) =
            let
                val (addrVal, addrCode) = mapSrcReg(addr, context)
            in
                pushRegisters(rest, LockMutable{addr=addrVal} :: addrCode @ code, context)
            end
    
        |   pushRegisters(ForwardJumpLabel{label as ILabel lab, result} :: rest, code,
                          context as {newCount=currentNewCount, stack=currentStack, ...}) =
            (* This is a forward jump label after other code.  We have to merge the states. *)
            (
                (* Get the state.  The label may never have been jumped to. *)
                case Array.sub(labelState, lab) of
                    NONE => pushRegisters(rest, code, context)
                |   SOME {newCount=jumpNewCount, stack=jumpStack, ...} =>
                    let
                        (* Merge the common entries. Stop as soon as we find a difference. *)
                        fun matchStacks((entry as NewEntry {pregNo=reg1}) :: tl1, NewEntry {pregNo=reg2} :: tl2, combined, old, new) =
                            if reg1 = reg2
                            then matchStacks(tl1, tl2, entry :: combined, old, new+1)
                            else {newCount=new, oldCount=old, stack=combined}

                        |   matchStacks((entry as ContainerEntry {size=size1}) :: tl1, ContainerEntry {size=size2} :: tl2, combined, old, new) =
                            if size1 = size2
                            then matchStacks(tl1, tl2, entry :: combined, old+size1, new+size1)
                            else {newCount=new, oldCount=old, stack=combined}
                            
                        |   matchStacks(HandlerEntry :: tl1, HandlerEntry :: tl2, combined, old, new) =
                                matchStacks(tl1, tl2, HandlerEntry :: combined, old+2, new+2)

                        |   matchStacks(_, _, combined, old, new) = {newCount=new, oldCount=old, stack=combined}

                        val newContext as {newCount=newSp, ...} = matchStacks(List.rev currentStack, List.rev jumpStack, [], 0, 0)

                        val adjustStack =
                            if newSp = currentNewCount
                            then []
                            else [ResetStackPtr{numWords=currentNewCount-newSp}]
                        (* If there is a result register it should not have been pushed to the stack. *)
                        val resultReg =
                            case result of
                                NONE => NONE
                            |   SOME(PReg(dReg, _)) =>
                                    case Array.sub(pregMap, dReg) of
                                        ToPReg preg => SOME preg
                                    |   _ => raise InternalError "pushRegisters merge register"
                        val resultCode =
                            if newSp = jumpNewCount
                            then (* We don't need to do anything to the incoming branch. *)
                                ForwardJumpLabel{label=label, result=resultReg} :: adjustStack  @ code
                            else (* We're going to have to adjust the stack pointer on the incoming branch.
                                    This requires an extra label and branch. *)
                            let
                                val extraLabel = ILabel(! labelCounter) before labelCounter := !labelCounter+1
                           in
                                ForwardJumpLabel{label=extraLabel, result=resultReg} ::
                                ResetStackPtr{numWords=jumpNewCount-newSp} ::
                                ForwardJumpLabel{label=label, result=resultReg} :: (* Come in here. *)
                                UnconditionalForwardJump{label=extraLabel} :: adjustStack  @ code
                            end
                    in
                        pushRegisters(rest, resultCode, newContext)
                    end
            )

        |   pushRegisters(UnconditionalForwardJump{label} :: rest, code, context) =
            (
                (* Save the state.  Then enter the "unconditional transfer" state. *)
                addNewLabel(label, context);
                skipToLabel(rest, UnconditionalForwardJump{label=label} :: code)
            )

        |   pushRegisters(ConditionalForwardJump{condition, label, ccRef} :: rest, code, context) =
            (
                (* Save the state but continue. *)
                addNewLabel(label, context);
                pushRegisters(rest, ConditionalForwardJump{condition=condition, label=label, ccRef=ccRef} :: code, context)
            )

        |   pushRegisters(WordComparison{arg1, arg2, ccRef} :: rest, code, context) =
            let
                (* codeExtended expects the first argument to be in a register.
                   We may be able to do something clever if the second is actually in the
                   register but for the moment just force a load. *)
                val (op1Val, op1Code) = mapSource(arg1, context)
                val (loadedOp1, loadCode) = loadStack op1Val
                val (op2Val, op2Code) = mapSource(arg2, context)
            in
                pushRegisters(rest, WordComparison{arg1=loadedOp1, arg2=op2Val, ccRef=ccRef} :: op2Code @ loadCode @ op1Code @ code, context)
            end

        |   pushRegisters(PushExceptionHandler{workReg, handleStart} :: rest, code, context as {oldCount, newCount, stack}) =
            let
                (* Save the state. This is a bit like a conditional jump *)
                val () = addNewLabel(handleStart, context)
                val newWorkReg = mapWorkReg workReg
                (* Add a handler entry to the stack. *)
                val newContext = {oldCount=oldCount+2, newCount=newCount+2, stack=HandlerEntry :: stack}
            in
                pushRegisters(rest, PushExceptionHandler{workReg=newWorkReg, handleStart=handleStart} :: code, newContext)
            end

        |   pushRegisters(PopExceptionHandler{workReg, ...} :: rest, code, {oldCount, newCount, stack}) =
            let
                val newWorkReg = mapWorkReg workReg
                (* Get the state after removing the handler. *)
                fun popContext ([], _, _) = raise InternalError "pushRegisters - pop handler"
                |   popContext (HandlerEntry :: tl, old, new) = {oldCount=old-2, newCount=new-2, stack=tl}
                |   popContext (ContainerEntry _ :: _, _, _) = raise InternalError "pushRegisters - pop handler - container"
                |   popContext (NewEntry _ :: tl, old, new) = popContext(tl, old, new-1)
                val newContext as { newCount=newNewCount, ...} = popContext(stack, oldCount, newCount)
                (* Reset the stack to just above the two words of the handler. *)
                val resetCode =
                    if newCount <> newNewCount+2
                    then [ResetStackPtr{numWords=newCount-newNewCount-2}]
                    else []
            in
                pushRegisters(rest, PopExceptionHandler{resultReg=NONE, workReg=newWorkReg} :: resetCode @ code, newContext)
            end

        |   pushRegisters(BeginHandler _ :: _, _, _) =
                (* This should only ever happen after an unconditional jump. *)
                raise InternalError "pushRegisters - BeginHandler"

        |   pushRegisters(ReturnResultFromFunction{resultReg, numStackArgs} :: rest, code, context as {newCount, ...}) =
            let
                val (resultValue, loadResult) = mapSrcReg(resultReg, context)
                val resetCode =
                    if newCount = 0 then [] else [ResetStackPtr{numWords=newCount}]
            in
                skipToLabel(rest,
                    ReturnResultFromFunction{resultReg=resultValue, numStackArgs=numStackArgs} :: resetCode @ loadResult @ code)
            end

        |   pushRegisters(ArithmeticFunction{oper, resultReg, operand1, operand2, ccRef} :: rest, code, context) =
            let
                (* codeExtended expects the first argument to be in a register.
                   We may be able to do something clever if the second is actually in the
                   register but for the moment just force a load. *)
                val (op1Val, op1Code) = mapSource(operand1, context)
                val (loadedOp1, loadCode) = loadStack op1Val
                val (op2Val, op2Code) = mapSource(operand2, context)
                val (destVal, destCode, newContext) = mapDestReg(resultReg, context)
            in
                pushRegisters(rest,
                    destCode @ ArithmeticFunction{oper=oper, resultReg=destVal, operand1=loadedOp1, operand2=op2Val, ccRef=ccRef} ::
                        op2Code @ loadCode @ op1Code @ code, newContext)
            end

        |   pushRegisters(TestTagBit{arg, ccRef} :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSource(arg, context)
            in
                pushRegisters(rest, TestTagBit{arg=sourceVal, ccRef=ccRef} :: sourceCode @ code, context)
            end

        |   pushRegisters(PushValue{arg} :: rest, code, context) =
            let
                (* We don't currently have this and if we do we'll have to add them to the stack. *)
                val _ = raise InternalError "pushRegisters - PushValue"
                val (sourceVal, sourceCode) = mapSource(arg, context)
            in
                pushRegisters(rest, PushValue{arg=sourceVal} :: sourceCode @ code, context)
            end

        |   pushRegisters(ResetStackPtr{numWords} :: rest, code, context as {newCount=oldSp, ...}) =
            (* This indicates that an old container entry is no longer required.  Remove it and anything
               else we've pushed since.  Because of the nesting anything pushed more recently should
               now be free. *)
            let
                fun removeItems(0, state) = state
                |   removeItems(_, {stack=[], ...}) = raise InternalError "removeItems - stack empty"
                |   removeItems(toRemove, {stack=ContainerEntry {size} :: tl, oldCount, newCount}) =
                        if toRemove < size then raise InternalError "removeItems - container size"
                        else removeItems(toRemove-size, {stack=tl, oldCount=oldCount-size, newCount=newCount-size})
                |   removeItems(_, {stack=HandlerEntry :: _, ...}) = raise InternalError "removeItems - handler"
                |   removeItems(toRemove, {stack=NewEntry _ :: tl, oldCount, newCount}) =
                        removeItems(toRemove, {stack=tl, oldCount=oldCount, newCount=newCount-1})
                val newContext as {newCount=newSp, ...} = removeItems(numWords, context)
            in
                pushRegisters(rest, ResetStackPtr{numWords=oldSp-newSp} :: code, newContext)
            end

        |   pushRegisters(TagValue{source, dest} :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSrcReg(source, context)
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest, destCode @ TagValue{source=sourceVal, dest=destVal} :: sourceCode @ code, newContext)
            end

        |   pushRegisters(UntagValue{source, dest, isSigned} :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSource(source, context)
                val (loadedSource, loadCode) = loadStack sourceVal
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ UntagValue{source=loadedSource, dest=destVal, isSigned=isSigned} :: loadCode @ sourceCode @ code, newContext)
            end

        |   pushRegisters(LoadEffectiveAddress{base, offset, index, dest} :: rest, code, context) =
            let
                val (baseVal, baseCode) =
                    case base of
                        NONE => (NONE, [])
                    |   SOME bReg =>
                            let val (newBReg, regCode) = mapSrcReg(bReg, context) in (SOME newBReg, regCode) end
                val (indexVal, indexCode) = mapIndex(index, context)
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ LoadEffectiveAddress{base=baseVal, offset=offset, index=indexVal, dest=destVal} :: indexCode @ baseCode @ code, newContext)
            end

        |   pushRegisters(ShiftOperation{shift, resultReg, operand, shiftAmount, ccRef} :: rest, code, context) =
            let
                val (opVal, opCode) = mapSource(operand, context)
                val (shiftVal, shiftCode) = mapSource(shiftAmount, context)
                val (destVal, destCode, newContext) = mapDestReg(resultReg, context)
            in
                pushRegisters(rest,
                    destCode @ ShiftOperation{shift=shift, resultReg=destVal, operand=opVal, shiftAmount=shiftVal, ccRef=ccRef} ::
                        shiftCode @ opCode @ code, newContext)
            end

        |   pushRegisters(Multiplication{resultReg, operand1, operand2, ccRef} :: rest, code, context) =
            let
                val (op1Val, op1Code) = mapSource(operand1, context)
                val (op2Val, op2Code) = mapSource(operand2, context)
                val (destVal, destCode, newContext) = mapDestReg(resultReg, context)
            in
                pushRegisters(rest,
                    destCode @ Multiplication{resultReg=destVal, operand1=op1Val, operand2=op2Val, ccRef=ccRef} :: op2Code @ op1Code @ code, newContext)
            end

        |   pushRegisters(Division{isSigned, dividend, divisor, quotient, remainder} :: rest, code, context) =
            let
                val (dividendVal, dividendCode) = mapSrcReg(dividend, context)
                val (divisorVal, divisorCode) = mapSource(divisor, context)
                val (quotVal, quotCode, quotContext) = mapDestReg(quotient, context)
                val (remVal, remCode, newContext) = mapDestReg(remainder, quotContext)
            in
                pushRegisters(rest,
                    remCode @ quotCode @
                        Division{isSigned=isSigned, dividend=dividendVal, divisor=divisorVal, quotient=quotVal, remainder=remVal} ::
                            divisorCode @ dividendCode @ code, newContext)
            end

        |   pushRegisters(AtomicExchangeAndAdd{base, source} :: rest, code, context) =
            let
                val (baseVal, baseCode) = mapSrcReg(base, context)
                val (sourceVal, sourceCode) = mapSrcReg(source, context)
                (* The "source" is also a result and must be in a register.  It's an untagged reg
                   so it shouldn't have been marked as to be pushed. *)
                val _ = case sourceCode of [] => () | _ => raise InternalError "pushRegisters - AtomicExchangeAndAdd"
            in
                pushRegisters(rest, AtomicExchangeAndAdd{base=baseVal, source=sourceVal} :: baseCode @ code, context)
            end

        |   pushRegisters(BoxValue{boxKind, source, dest, saveRegs} :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSrcReg(source, context)
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ BoxValue{boxKind=boxKind, source=sourceVal, dest=destVal, saveRegs=mapSaveRegs saveRegs} :: sourceCode @ code, newContext)
            end

        |   pushRegisters(CompareByteVectors{vec1Addr, vec2Addr, length, ccRef} :: rest, code, context) =
            let
                val (vec1Val, vec1Code) = mapSrcReg(vec1Addr, context)
                val (vec2Val, vec2Code) = mapSrcReg(vec2Addr, context)
                val (lengthVal, lengthCode) = mapSrcReg(length, context)
            in
                pushRegisters(rest,
                    CompareByteVectors{vec1Addr=vec1Val, vec2Addr=vec2Val, length=lengthVal, ccRef=ccRef} ::
                        lengthCode @ vec2Code @ vec1Code @ code, context)
            end

        |   pushRegisters(BlockMove{srcAddr, destAddr, length, isByteMove} :: rest, code, context) =
            let
                val (srcVal, srcCode) = mapSrcReg(srcAddr, context)
                val (destVal, destCode) = mapSrcReg(destAddr, context)
                val (lengthVal, lengthCode) = mapSrcReg(length, context)
            in
                pushRegisters(rest,
                    BlockMove{srcAddr=srcVal, destAddr=destVal, length=lengthVal, isByteMove=isByteMove} ::
                        lengthCode @ destCode @ srcCode @ code, context)
            end

        |   pushRegisters(CompareFloatingPt{arg1, arg2, ccRef} :: rest, code, context) =
            let
                val (arg1Val, arg1Code) = mapSource(arg1, context)
                val (arg2Val, arg2Code) = mapSource(arg2, context)
            in
                pushRegisters(rest, CompareFloatingPt{arg1=arg1Val, arg2=arg2Val, ccRef=ccRef} :: arg2Code @ arg1Code @ code, context)
            end

        |   pushRegisters(X87FPGetCondition{dest, ccRef} :: rest, code, context) =
            let
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest, destCode @ X87FPGetCondition{dest=destVal, ccRef=ccRef} :: code, newContext)
            end

        |   pushRegisters(X87FPArith{opc, resultReg, arg1, arg2} :: rest, code, context) =
            let
                val (arg1Val, arg1Code) = mapSource(arg1, context)
                val (arg2Val, arg2Code) = mapSource(arg2, context)
                val (destVal, destCode, newContext) = mapDestReg(resultReg, context)
            in
                pushRegisters(rest,
                    destCode @ X87FPArith{opc=opc, resultReg=destVal, arg1=arg1Val, arg2=arg2Val} ::
                        arg2Code @ arg1Code @ code, newContext)
            end
    
        |   pushRegisters(X87FPUnaryOps{fpOp, dest, source} :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSource(source, context)
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ X87FPUnaryOps{fpOp=fpOp, dest=destVal, source=sourceVal} :: sourceCode @ code, newContext)
            end

        |   pushRegisters(FloatFixedInt{dest, source} :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSource(source, context)
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest, destCode @ FloatFixedInt{dest=destVal, source=sourceVal} :: sourceCode @ code, newContext)
            end
    
        |   pushRegisters(SSE2FPArith{opc, resultReg, arg1, arg2} :: rest, code, context) =
            let
                val (arg1Val, arg1Code) = mapSource(arg1, context)
                val (arg2Val, arg2Code) = mapSource(arg2, context)
                val (destVal, destCode, newContext) = mapDestReg(resultReg, context)
            in
                pushRegisters(rest,
                    destCode @ SSE2FPArith{opc=opc, resultReg=destVal, arg1=arg1Val, arg2=arg2Val} ::
                        arg2Code @ arg1Code @ code, newContext)
            end
        
        and skipToLabel([], code) = code

        |   skipToLabel(ForwardJumpLabel{label as ILabel lab, ...} :: rest, code) =
            (* Label after unconditional transfer.  No need to merge the states. *)
            (
                (* Look at the state.  We may never have branched to this label.  This sometimes
                   happens with andalso/orelse.  In that case just continue. *)
                case Array.sub(labelState, lab) of
                    NONE => skipToLabel(rest, code)
                |   SOME state => pushRegisters(rest, ForwardJumpLabel{label=label, result=NONE} :: code, state)
            )
        
        |   skipToLabel(BeginHandler{handleStart as ILabel lab, packetReg, workReg, ...} :: rest, code) =
                (* A handler should only ever occur after an unconditional transfer. *)
            let
                val context = valOf(Array.sub(labelState, lab))
                val newWorkReg = mapWorkReg workReg
                val (pktReg, pktCode, newContext) = mapDestReg(packetReg, context)
            in
                pushRegisters(rest, pktCode @ BeginHandler{handleStart=handleStart, packetReg=pktReg, workReg=newWorkReg} :: code, newContext)
            end
        
        |   skipToLabel(EndLoop{loopLabel} :: rest, code) =
                (* Normally we will have at least one branch that exits the loop
                   but it could be that the loop is exited by raising an exception.
                   This may be needed if we have to pop loop state. *)
                skipToLabel(rest, EndLoop{loopLabel=loopLabel} :: code)

            (* Any other Exited conditions. Skip the instructions.  There are various situations where this
               can arise.  e.g. OS.Process.exit contains an infinite loop processing atExit functions
               until it eventually exits by calling a "stop" function. *)
        |   skipToLabel(_ :: rest, code) = skipToLabel(rest, code)

        val pushedCode = pushRegisters(code, [], emptyContext)
        val resultCode = List.rev pushedCode
    in
        (resultCode, !pregCounter, !labelCounter, !reportSuccess)
    end


    fun codeAbstractToConcrete(identifiedCode, regStates, maxPRegs) =
    let
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
        and findFloatReg r = findRegister(r, NONE, floatingPtRegisters)

        fun codeExtArgument(RegisterArgument(PReg(r, _)), regSet) = RegisterArgument(findRegister(r, NONE, regSet))
        |   codeExtArgument(AddressConstant m, _) = AddressConstant m
        |   codeExtArgument(IntegerConstant i, _) = IntegerConstant i
        |   codeExtArgument(MemoryLocation{base=PReg(bReg, _), offset, index}, _) =
                MemoryLocation{base=findGeneralReg bReg, offset=offset, index=codeExtIndex index}
        |   codeExtArgument(StackLocation{wordOffset, adjustment}, _) =
                MemoryLocation{base=GenReg esp, offset=(wordOffset+adjustment)*wordSize, index=NoMemIndex}
        
        and codeExtArgGeneral arg = codeExtArgument(arg, generalRegisters)
        and codeExtArgFloat arg = codeExtArgument(arg, floatingPtRegisters)

        and codeExtIndex NoMemIndex = NoMemIndex
        |   codeExtIndex(MemIndex1(PReg(r, _))) = MemIndex1(findGeneralReg r)
        |   codeExtIndex(MemIndex2(PReg(r, _))) = MemIndex2(findGeneralReg r)
        |   codeExtIndex(MemIndex4(PReg(r, _))) = MemIndex4(findGeneralReg r)
        |   codeExtIndex(MemIndex8(PReg(r, _))) = MemIndex8(findGeneralReg r)

        fun moveRR{src, dst} = LoadArgument{source=RegisterArgument src, dest=dst, kind=MoveWord}

        fun moveIfNecessary{src, dst} =
            if src = dst then [] else [moveRR{src=src, dst=dst}]
        
        (* This is a general function for moving values into registers or to the stack
           where it is possible that the source values might also be in use as destinations.
           The stack is used for destinations only for tail recursive calls. *)
        fun moveMultipleValues(moves, reservedRegs) =
        let
            fun moveValues([], _) = [] (* We're done. *)

            |   moveValues(arguments, reservedRegs) =
                let
                    (* stronglyConnectedComponents does two things.  It detects loops where
                       it's not possible to move items without breaking the loop but more
                       importantly it orders the dependencies so that if there are no loops we
                       can load the source and store it in the destination knowing that
                       we won't overwrite anything we might later need. *)
                    val ordered = STRONGLY.stronglyConnectedComponents arguments
                    
                    fun moveEachValue ([], _) = []

                    |   moveEachValue ([{dst=RegDest reg, src as RegisterArgument r}] :: rest, reserved) =
                            (* Source and dest are both regs - only move if they're different. *)
                            if r = reg
                            then moveEachValue(rest, reg :: reserved)
                            else LoadArgument{source=src, dest=reg, kind=MoveWord} ::
                                    moveEachValue(rest, reg :: reserved)

                    |   moveEachValue ([{dst=RegDest reg, src}] :: rest, reserved) =
                            (* Load from store or a constant. *)
                            LoadArgument{source=src, dest=reg, kind=MoveWord} ::
                                moveEachValue(rest, reg :: reserved)

                    |   moveEachValue ([{dst=StackDest _, src=MemoryLocation _ }] :: _, _) =
                            raise InternalError "moveEachValue - MemoryArgument"

                    |   moveEachValue ([{dst=StackDest addr, src as StackLocation{ wordOffset, adjustment}}] :: rest, reserved) =
                            (* Copy a stack location - needs a load and store unless the address is the same. *)
                            if addr = wordOffset + adjustment
                            then moveEachValue(rest, reserved)
                            else
                            let
                                (* Find a free register.  We cannot use any reserved register, which
                                   includes those we've loaded, or any source register that we
                                   haven't yet used.  We start with all the registers, after filtering
                                   those we can't use, and remove a source register from the list.  *)
                                val filteredInUse =
                                    List.filter (fn r => not(List.exists (fn i => i = r) reserved)) generalRegisters
                                
                                fun filterASet(_, []) = [] (* Stop if there's nothing there. *)
                                |   filterASet([], regs) = regs
                                |   filterASet({src=RegisterArgument srcReg, ...} :: rest, regs) =
                                        filterASet(rest, List.filter(fn r => r <> srcReg) regs)
                                |   filterASet({src=MemoryLocation _, ...} :: _, _) = raise InternalError "filterASet - Memory"
                                |   filterASet(_ :: rest, regs) = filterASet(rest, regs)
                                
                                fun filterList(_, []) = []
                                |   filterList([], regs) = regs
                                |   filterList(set::rest, regs) = filterList(rest, filterASet(set, regs))
                                
                                (* So far it seems we always find a register this way if we need one.
                                   Just in case, fall back to pushing a register, using it and popping it
                                   again. *)
                                val loadAndStoreCode =
                                    case filterList(rest, filteredInUse) of
                                        workReg :: _ =>
                                            [LoadArgument{source=src, dest=workReg, kind=MoveWord},
                                             StoreArgument{
                                                    source=RegisterArgument workReg, base=GenReg esp, index=NoMemIndex,
                                                    offset = addr*wordSize, kind=MoveWord}]
                                    |   [] =>
                                        let
                                            val workReg = GenReg eax
                                            val workRegAsArg = RegisterArgument workReg
                                            val destAddr = addr+1
                                            val newSrc =
                                                case src of
                                                    StackLocation{wordOffset, adjustment} =>
                                                        StackLocation{wordOffset=wordOffset+1, adjustment=adjustment}
                                                |   src => src
                                        in
                                            [
                                            PushValue{arg=workRegAsArg},
                                            LoadArgument{source=newSrc, dest=workReg, kind=MoveWord},
                                            StoreArgument{
                                                source=RegisterArgument workReg, base=GenReg esp, index=NoMemIndex,
                                                offset = destAddr*wordSize, kind=MoveWord},
                                            LoadArgument{source=StackLocation{wordOffset=0, adjustment=0}, dest=workReg, kind=MoveWord},
                                            ResetStackPtr{numWords=1}
                                            ]
                                        end
                            in
                                loadAndStoreCode @ moveEachValue(rest, reserved)
                            end

                    |   moveEachValue ([{dst=StackDest addr, src}] :: rest, reserved) =
                            (* Store from a register or a constant. *)
                            StoreArgument{
                                source=src, base=GenReg esp, index=NoMemIndex, offset = addr*wordSize, kind=MoveWord} ::
                                    moveEachValue(rest, reserved)

                    |   moveEachValue(({dst=RegDest regA, src=RegisterArgument regB} :: (cycle as _ :: _)) :: rest, reserved) =
                        (* Only do registers at the moment.  It may be possible in the future to get loops as a result
                           of spilling registers onto the stack and then finding that a tail call to a
                           function with more stack arguments needs to overwrite those addresses. *)
                        let
                            (* We need to exchange some of the arguments.  Doing an exchange here will
                               set the destination with the correct source.  However we have to process
                               every subsequent entry with the swapped registers.  That may well mean that
                               one of those entries becomes trivial.
                               We also need to rerun stronglyConnectedComponents on at least the rest of
                               this cycle.  It's easiest to flatten the rest and do everything. *)
                            val flattened = List.foldl(fn (a, b) => a @ b) [] (cycle :: rest)
                            fun swapRegs r = if r = regA then regB else if r = regB then regA else r
                            fun swapSources{src=RegisterArgument r, dst} =
                                    {src=RegisterArgument(swapRegs r), dst=dst}
                            |   swapSources{src=MemoryLocation _, ...} =
                                    raise InternalError "swapSources: MemoryLocation"
                            |   swapSources sd = sd
                        in
                            ExchangeRegisters{regX=regA, regY=regB} ::
                                moveValues(List.map swapSources flattened, regA :: regB :: reserved)
                        end

                    |   moveEachValue((_ :: _ :: _) :: _, _) =
                            raise InternalError "moveEachValue - TODO cycle"

                    |   moveEachValue(([]) :: _, _) = (* This should not happen - avoid warning. *)
                            raise InternalError "moveEachValue - empty set"
                in
                    moveEachValue(ordered, reservedRegs)
                end
        in
            moveValues(moves, reservedRegs)
        end

        (* Where we have multiple specific registers as either source or
           destination there is the potential that a destination register
           if currently in use as a source. *) 
        fun moveMultipleRegisters regPairList =
        let
            val regPairsAsDests =
                List.map(fn {src, dst} => {src=RegisterArgument src, dst=RegDest dst}) regPairList
            (* We don't need a work register so this doesn't matter. *)
            val reserveRegs = generalRegisters (* Consider all registers to be in use. *)
        in
            moveMultipleValues(regPairsAsDests, reserveRegs)
        end

        (* Tail recursive calls are complicated because we generally have to overwrite the existing stack.
           That means storing the arguments in the right order to avoid overwriting a
           value that we are using for a different argument. *)
        type tailCopy = STRONGLY.node

        fun codeTailCall(callKind, arguments: tailCopy list, stackAdjust) =
        if stackAdjust < 0
        then
        let
            (* If the function we're calling takes more arguments on the stack than the
               current function we will have to extend the stack.  Do that by pushing the
               argument whose offset is at -1.  Then adjust all the offsets and repeat. *)
            val {src=argM1, ...} = valOf(List.find(fn {dst=StackDest ~1, ...} => true | _ => false) arguments)
            fun renumberArgs [] = []
            |   renumberArgs ({dst=StackDest ~1, ...} :: args) = renumberArgs args (* Remove the one we've done. *)
            |   renumberArgs ({dst, src} :: args) =
                let
                    val newDest = case dst of StackDest d => StackDest(d+1) | regDest => regDest
                    val newSrc =
                        case src of
                            StackLocation{wordOffset, adjustment} =>
                                StackLocation{wordOffset=wordOffset+1, adjustment=adjustment}
                        |   other => other
                in
                    {dst=newDest, src=newSrc} :: renumberArgs args
                end
        in
            PushValue{arg=argM1} :: codeTailCall(callKind, renumberArgs arguments, stackAdjust+1)
        end
        else
        let
            val loadArgs = moveMultipleValues(arguments, [] (* We only need argument registers. *))

            val adjustStack =
                if stackAdjust = 0
                then []
                else [ResetStackPtr{numWords=stackAdjust}]
        in
            loadArgs @ adjustStack @
            [TailRecursiveCall{regArgs=[], stackArgs=[], callKind=callKind, returnAddr={srcStack=0, stack=0}, stackAdjust=0}]
        end

        fun codeExtended([], _) = []

        |   codeExtended(LoadArgument{source=RegisterArgument(PReg(sreg, _)), dest=PReg(dreg, _), kind} :: rest, context) =
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
                        val regset =
                            case kind of
                                MoveFloat => floatingPtRegisters
                            |   MoveDouble => floatingPtRegisters
                            |   _ => generalRegisters
                        (* Get the source register using the current destination as a preference. *)
                        val realSrcReg = findRegister(sreg, SOME realDestReg, regset)
                    in
                        (* If the source is the same as the destination we don't need to do anything. *)
                        moveIfNecessary{src=realSrcReg, dst=realDestReg} @ code
                    end
            end

        |   codeExtended(LoadArgument{source, dest=PReg(pr, _), kind} :: rest, context) =
            (* Loads of constants or from an address. *)
            let
                val code = codeExtended(rest, context)
                val regSet =
                    case kind of MoveFloat => floatingPtRegisters | MoveDouble => floatingPtRegisters | _ => generalRegisters
            in
                (* If we don't have a register that means the result is never used. *)
                case Array.sub(allocatedRegs, pr) of
                    SOME regResult => LoadArgument{source=codeExtArgument(source, regSet), dest=regResult, kind=kind} :: code
                |   NONE => code
            end

        |   codeExtended(
                StoreArgument{
                    source as RegisterArgument(PReg(sReg, _)), base=PReg(bReg, _), offset, index, kind=MoveByte, ... } :: rest, context) =
            if isX64
            then
            let
                val code = codeExtended(rest, context)
            in
                StoreArgument{
                    source=codeExtArgGeneral source, base=findGeneralReg bReg, offset=offset, index=codeExtIndex index, kind=MoveByte}
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

        |   codeExtended(StoreArgument{ source, base=PReg(bReg, _), offset, index, kind } :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val regSet =
                    case kind of MoveFloat => floatingPtRegisters | MoveDouble => floatingPtRegisters | _ => generalRegisters
            in
                StoreArgument{
                    source=codeExtArgument(source, regSet), base=findGeneralReg bReg, offset=offset, index=codeExtIndex index, kind=kind}
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

        |   codeExtended(TailRecursiveCall{callKind, regArgs, stackArgs, returnAddr={srcStack, stack}, stackAdjust} :: rest, context) =
            let
                val () = List.app (fn (RegisterArgument(PReg(pr, _)), reg) => addRealHint(pr, reg) | _ => ()) regArgs
                val code = codeExtended(rest, context)
                (* Add the return address as an extra argument.  This is a temporary hack because codeICode doesn't
                   do it that way. *)
                val returnEntry = {src=StackLocation{wordOffset=srcStack, adjustment=0}, dst=StackDest stack}
                val extStackArgs = map (fn {stack, src} => {dst=StackDest stack, src=codeExtArgGeneral src}) stackArgs
                val extRegArgs = map (fn (a, r) => {src=codeExtArgGeneral a, dst=RegDest r}) regArgs
            in
                codeTailCall(callKind, returnEntry :: extStackArgs @ extRegArgs, stackAdjust) @ code
            end

        |   codeExtended(FunctionCall{callKind, regArgs, stackArgs, dest=PReg(dReg, _)} :: rest, context) =
            let
                val () = List.app (fn (RegisterArgument(PReg(pr, _)), reg) => addRealHint(pr, reg) | _ => ()) regArgs
                val () = addRealHint(dReg, GenReg eax)
                val code = codeExtended(rest, context)
                val destReg = findRegister(dReg, SOME(GenReg eax), generalRegisters)
                
                fun pushStackArgs ([], _) = []
                |   pushStackArgs (arg ::args, argNum) =
                    let
                        (* Have to adjust the offsets of stack arguments. *)
                        val adjusted =
                            case arg of
                                StackLocation {wordOffset, adjustment} =>
                                    StackLocation{wordOffset=wordOffset+argNum, adjustment=adjustment}
                            |   arg => arg
                    in
                        PushValue {arg=codeExtArgGeneral adjusted} :: pushStackArgs(args, argNum+1)
                    end
                val pushedArgs = pushStackArgs(stackArgs, 0)
                (* We have to adjust any stack offset to account for the arguments we've pushed. *)
                val numStackArgs = List.length stackArgs
                
                (* We don't currently allow the arguments to be memory locations and instead
                   force them into registers.  That may be simpler especially if we can get the
                   values directly into the required register. *)
                fun getRegArgs(RegisterArgument(PReg(pr, _)), reg) =
                        SOME{dst=reg, src=findRegister(pr, SOME reg, generalRegisters)}
                |   getRegArgs(MemoryLocation _, _) = raise InternalError "FunctionCall - MemoryLocation"
                |   getRegArgs _ = NONE
                val loadRegArgs = moveMultipleRegisters(List.mapPartial getRegArgs regArgs)

                (* These are all items we can load without requiring a source register.
                   That includes loading from the stack. *)
                fun getConstArgs(AddressConstant m, reg) =
                        SOME(LoadArgument{source=AddressConstant m, dest=reg, kind=MoveWord})
                |   getConstArgs(IntegerConstant i, reg) =
                        SOME(LoadArgument{source=IntegerConstant i, dest=reg, kind=MoveWord})
                |   getConstArgs(StackLocation { wordOffset, adjustment }, reg) =
                        SOME(LoadArgument{source=StackLocation{wordOffset=wordOffset+numStackArgs, adjustment=adjustment},
                                          dest=reg, kind=MoveWord})
                |   getConstArgs(RegisterArgument _, _) = NONE
                |   getConstArgs(MemoryLocation _, _) = NONE
                val loadConstArgs = List.mapPartial getConstArgs regArgs
            in
                pushedArgs @ loadRegArgs @ loadConstArgs @
                    (FunctionCall{regArgs=[], stackArgs=[], dest=raxAsArg, callKind=callKind} ::
                        moveIfNecessary{dst=destReg, src=GenReg eax}) @ code
            end

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

        |   codeExtended(StartLoop{regArgs, loopLabel} :: rest, context) =
            let
                (* Make the loop arguments and the sources "friends". *)
                fun makeFriends (RegisterArgument(PReg(sReg, _)), PReg(dReg, _)) = addFriends(dReg, sReg)
                |   makeFriends _ = ()
                val () = List.app makeFriends regArgs
                val code = codeExtended(rest, context)
                val extRegArgs = map (fn (a, PReg(r, _)) => {src=codeExtArgGeneral a, dst=RegDest(findGeneralReg r)}) regArgs
            in
                moveMultipleValues(extRegArgs, generalRegisters) @ (StartLoop{regArgs=[], loopLabel=loopLabel} :: code)
            end

        |   codeExtended(EndLoop _ :: rest, context) = (* Don't need to do anything. *)
                codeExtended(rest, context)

        |   codeExtended(JumpLoop{regArgs, stackArgs, loopLabel, stackAdjust} :: rest, context) =
            let
                (* TODO: Make the sources and destinations "friends". *)
                val code = codeExtended(rest, context)
                val extStackArgs = map (fn (src, stack) => {dst=StackDest stack, src=codeExtArgGeneral src}) stackArgs
                val extRegArgs = map (fn (a, PReg(r, _)) => {src=codeExtArgGeneral a, dst=RegDest(findGeneralReg r)}) regArgs
                val resetCode =
                    if stackAdjust = 0 then [] else [ResetStackPtr{numWords=stackAdjust}]
            in
                moveMultipleValues(extStackArgs @ extRegArgs, generalRegisters) @ resetCode @
                    (JumpLoop{regArgs=[], stackArgs=[], loopLabel=loopLabel, stackAdjust = 0} :: code)
            end

        |   codeExtended(RaiseExceptionPacket{ packet=RegisterArgument(PReg(preg, _)) } :: rest, context) =
            let
                val () = addRealHint(preg, GenReg eax)
                val code = codeExtended(rest, context)
                (* The argument must be put into rax. *)
                val argReg = findRegister(preg, SOME(GenReg eax), generalRegisters)
            in
                moveIfNecessary{src=argReg, dst=raxAsArg} @
                    RaiseExceptionPacket {packet=RegisterArgument raxAsArg } :: code
            end

        |   codeExtended(RaiseExceptionPacket{ packet } :: rest, context) =
            let
                (* Because we're exiting we don't have to worry about conflicts - just load it. *)
                val code = codeExtended(rest, context)
            in
                LoadArgument{source=codeExtArgument(packet, generalRegisters), dest=raxAsArg, kind=MoveWord} ::
                    RaiseExceptionPacket {packet=RegisterArgument raxAsArg } :: code
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

        |   codeExtended(IndexedCaseOperation{testReg=PReg(tReg, _), workReg=PReg(wReg, _), cases, startValue} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val testReg = findRegister(tReg, NONE, generalRegisters)
                val workReg = findRegister(wReg, NONE, generalRegisters)
                val _ = testReg <> workReg orelse raise InternalError "IndexedCaseOperation - same registers"
            in
                IndexedCaseOperation{ testReg=testReg, workReg=workReg, cases=cases, startValue=startValue} :: code
            end

        |   codeExtended(LockMutable{addr=PReg(pr, _)} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val argReg = findRegister(pr, NONE, generalRegisters)
            in
                LockMutable{addr=argReg} :: code
            end

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
                WordComparison{ arg1=codeExtArgGeneral arg1, arg2=codeExtArgGeneral arg2, ccRef=ccRef } :: code
            end

        |   codeExtended(WordComparison _ :: _, _) = raise InternalError "codeExtended - WordComparison"

            (* Set up an exception handler. *)
        |   codeExtended(PushExceptionHandler{workReg=PReg(hReg, _), handleStart} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val handleReg = findGeneralReg hReg
            in
                PushExceptionHandler{workReg=handleReg, handleStart=handleStart} :: code
            end

            (* Pop an exception handler at the end of a handled section.  Executed if no exception has been raised.
               This removes items from the stack. *)
        |   codeExtended(PopExceptionHandler{workReg=PReg(wReg, _), ...} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val realWork = findGeneralReg wReg
            in
                PopExceptionHandler{resultReg=NONE, workReg=realWork} :: code
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
                val op2Arg = codeExtArgGeneral operand2
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
            (* Arithmetic operation with the first argument in a register and the second a constant or memory location. *)
            let
                val () = addFriends (resReg, op1Reg)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, generalRegisters)
                val op2Arg = codeExtArgGeneral operand2
                (* If we couldn't put it in the result register we have to copy it there. *)
                (* TODO: Is there the potential for a problem?  We don't worry about a conflict
                   between the result register and the arguments.  What if the second argument is a memory
                   location with the result reg as a base or index? *)
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
                val op1Arg = codeExtArgGeneral operand1
            in
                moveIfNecessary{src=realOp2Reg, dst=realDestReg} @
                    (ArithmeticFunction{oper=oper, resultReg=realDestReg, operand1=RegisterArgument realDestReg, operand2=op1Arg, ccRef=ccRef} :: code)
            end

        |   codeExtended(ArithmeticFunction _ :: _, _) =
                raise InternalError "codeExtended - ArithmeticFunction"

        |   codeExtended(TestTagBit{arg, ccRef} :: rest, context) =
            let
                val code = codeExtended(rest, context)
            in
                TestTagBit{ arg=codeExtArgGeneral arg, ccRef=ccRef } :: code
            end

        |   codeExtended(PushValue {arg} :: rest, context) =
            let
                val code = codeExtended(rest, context)
            in
                PushValue {arg=codeExtArgGeneral arg} :: code
            end

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
            (* Always generates register argument at the moment.  TODO: This should really just take a single register arg. *)
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
                val divisorArg = codeExtArgGeneral divisor
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

        |   codeExtended(BoxValue{boxKind, source=PReg(sReg, _), dest=PReg(dReg, _), saveRegs} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val preserve = List.map(fn (PReg(r, _)) => findGeneralReg r) saveRegs
                val (srcReg, boxSize, moveKind) =
                    case boxKind of
                        BoxLargeWord => (findGeneralReg sReg, 1, MoveWord)
                    |   BoxFloat => (findFloatReg sReg, Int.quot(8, wordSize), MoveDouble)
                val dstReg = findGeneralReg dReg
            in
                AllocateMemoryOperation{ size=boxSize, flags=0wx1, dest=dstReg, saveRegs=preserve} ::
                StoreArgument{ source=RegisterArgument srcReg, offset=0, base=dstReg, index=NoMemIndex, kind=moveKind} ::
                InitialisationComplete{dest=dstReg (* unused *)} :: code
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

        |   codeExtended(CompareFloatingPt{arg1 as RegisterArgument _, arg2, ccRef} :: rest, context) =
            let
                val code = codeExtended(rest, context)
            in
                CompareFloatingPt{ arg1=codeExtArgFloat arg1, arg2=codeExtArgFloat arg2, ccRef=ccRef } :: code
            end

        |   codeExtended(CompareFloatingPt _ :: _, _) = raise InternalError "codeExtended - CompareFloatingPt"

        |   codeExtended(X87FPGetCondition{dest=PReg(dReg, _), ccRef} :: rest, context) =
            let
                (* We can only use RAX here. *)
                val () = addRealHint(dReg, GenReg eax)
                val code = codeExtended(rest, context)
                val destReg = findRegister(dReg, SOME(GenReg eax), generalRegisters)
            in
                X87FPGetCondition{dest=raxAsArg, ccRef=ccRef} ::
                    moveIfNecessary{dst=destReg, src=GenReg eax} @ code
            end

        |   codeExtended(X87FPArith{opc, resultReg=PReg(resReg, _), arg1=RegisterArgument(PReg(op1Reg, _)), arg2} :: rest, context) =
            let
                val () = addFriends (resReg, op1Reg)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, floatingPtRegisters)
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, floatingPtRegisters)
                val op2Arg = codeExtArgFloat arg2
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg} @
                    (X87FPArith{opc=opc, resultReg=realDestReg, arg1=RegisterArgument realDestReg, arg2=op2Arg} :: code)
            end

        |   codeExtended(X87FPArith _ :: _, _) = raise InternalError "codeExtended - X87FPArith"
    
        |   codeExtended(X87FPUnaryOps{fpOp, dest=PReg(resReg, _), source=RegisterArgument(PReg(op1Reg, _))} :: rest, context) =
            let
                val () = addFriends (resReg, op1Reg)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, floatingPtRegisters)
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, floatingPtRegisters)
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg} @
                    (X87FPUnaryOps{fpOp=fpOp, dest=realDestReg, source=RegisterArgument realDestReg} :: code)
            end

        |   codeExtended(X87FPUnaryOps _ :: _, _) = raise InternalError "codeExtended - X87FPArith"

        |   codeExtended(FloatFixedInt{dest=PReg(resReg, _), source} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val intSource = codeExtArgGeneral source
                val fpReg = findRegister(resReg, NONE, floatingPtRegisters)
                val floatCode =
                    case fpMode of
                        FPModeX87 => 
                            (* This is complicated.  The integer value has to be in memory not in a
                               register so we have to push it to the stack and then make sure it is
                               popped afterwards.  Because it is untagged it is unsafe to leave it. *)
                            [
                                PushValue{arg=intSource},
                                FloatFixedInt{dest=fpReg, source=wordOffsetAddress(0, esp)},
                                ResetStackPtr {numWords=1}
                            ]
                    |   FPModeSSE2 => [FloatFixedInt{dest=fpReg, source=intSource}]
            in
                floatCode @ code
            end
    
        |   codeExtended(SSE2FPArith{opc, resultReg=PReg(resReg, _), arg1=RegisterArgument(PReg(op1Reg, _)), arg2} :: rest, context) =
            let
                val () = addFriends (resReg, op1Reg)
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, floatingPtRegisters)
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, floatingPtRegisters)
                val op2Arg = codeExtArgFloat arg2
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg} @
                    (SSE2FPArith{opc=opc, resultReg=realDestReg, arg1=RegisterArgument realDestReg, arg2=op2Arg} :: code)
            end
    
        |   codeExtended(SSE2FPArith _ :: _, _) = raise InternalError "codeExtended - SSE2FPArith"

    in
        codeExtended(identifiedCode, [])
    end


    fun codeICodeFunctionToX86{icode, functionName, maxLabels, maxPRegs, argRegsUsed, hasFullClosure, currentStackArgs, debugSwitches} =
    let
        val icodeTabs = [8, 20, 60]
        
        fun printCodeAndStates(identifiedCode, regStates) =
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
                
                fun printRegData(i, { conflicts, realConflicts=_, defs, refs, pushState }) =
                (
                    printStream (Int.toString i ^ "\t");
                    printStream ("Conflicts="); printRegs(conflicts, 20);
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
        
        val reportSuccess = ref false

        val (postTransformCode, maxLabels, stackRequired) =
            let
                (* First pass - identify register use patterns *)
                val (identifiedPhase1, regStatesPhase1) = identifyRegisters {icode=icode, maxPRegs=maxPRegs}
                val () = printCodeAndStates(identifiedPhase1, regStatesPhase1)
                
                (* If we have a "must-push" entry we'll have to reprocess. *)
                val needPhase2 =
                    Vector.exists(fn {pushState=MustPush, ...} => true | _ => false) regStatesPhase1
                val (identifiedCode, regStates, maxLabels, maxPRegs) =
                    if needPhase2
                    then
                    let
                        (* Push those registers we need to.  This also adds and renumbers pregs
                           and may add labels. *)
                        val (postPushCode, maxPRegsPhase2, maxLabelsPhase2, repSuccess) =
                            addRegisterPushes(identifiedPhase1, regStatesPhase1, maxPRegs, maxLabels)
                        val (identifiedPhase2, regStatesPhase2) =
                            identifyRegisters {icode=postPushCode, maxPRegs=maxPRegsPhase2}
                        val () = printCodeAndStates(identifiedPhase2, regStatesPhase2)
                        
                        val () = if repSuccess then reportSuccess := true else ()

                        val () = afterPush := !afterPush + 1
                    in
                        (identifiedPhase2, regStatesPhase2, maxLabelsPhase2, maxPRegsPhase2)
                    end
                    else (identifiedPhase1, regStatesPhase1, maxLabels, maxPRegs)
                
                val code = codeAbstractToConcrete(identifiedCode, regStates, maxPRegs)
                val _ = if ! reportSuccess then print("-------Done " ^ functionName ^ "\n") else ()
                val () = successFns := !successFns + 1
            in
                (code, maxLabels, 0)
            end
            handle TooHard reason =>
            let
                val incrReg =
                    case reason of NotEnoughRegs => tooManyRegs | MustPushReg => mustPushReg | NotDoneInstr => notDoneInstr | OtherReason => anOther
                val () = incrReg := !incrReg + 1
                val {icode, maxLabels, stackRequired} =
                    fallBackTransform{icode=icode, maxLabels=maxLabels, currentStackArgs=currentStackArgs}
            in
                (icode, maxLabels, stackRequired)
            end
        val () =
            if !totalFns mod 200 = 0
            then
                print(concat["---Succeeded ", Int.toString(!successFns), " of ", Int.toString(!totalFns),
                     " After push=", Int.toString(!afterPush),
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
        codeAsX86Code{icode=postTransformCode, maxLabels = maxLabels, stackRequired = stackRequired,
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
