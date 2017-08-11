(*
    Copyright David C. J. Matthews 2016-17

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

functor X86PushRegisters(
    structure ICODE: ICodeSig
    structure INTSET: INTSETSIG
    structure IDENTIFY: X86IDENTIFYREFSSIG
    sharing ICODE.Sharing = IDENTIFY.Sharing = INTSET
) : X86PUSHREGISTERSIG
=
struct
    open ICODE
    open INTSET
    open IDENTIFY
    
    exception InternalError = Misc.InternalError

    fun addRegisterPushes(code: {instr: abstract x86ICode, active: intSet, current: intSet} list, pushVec: bool vector, maxPRegs, maxLabels) =
    let
        val labelCounter = ref maxLabels
        val pregCounter = ref 0
        val maxStack = ref 0
        
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
        
        (* Adjust a stack offset from the old state to the new state. *)
        fun mapOldStackOffset(offset, {oldCount, newCount, stack, ...}: context) =
        let
            fun adjustStack(old, new, []) =
                let
                    val _ = old = offset-oldCount orelse raise InternalError "mapOldStackOffset - old"
                    val _ = new = newCount orelse raise InternalError "mapOldStackOffset - new"
                in
                    old+new
                end
            |   adjustStack(old, new, entry :: tl) =
                let
                    val _ = old < 0 andalso raise InternalError "adjustStack"
                in
                    case entry of
                        NewEntry _ => adjustStack(old, new+1, tl)
                    |   ContainerEntry {size} =>
                            if old = 0
                            then new
                            else adjustStack(old-size, new+size, tl)
                    |   HandlerEntry =>
                            if old = 0
                            then new
                            else adjustStack(old-2, new+2, tl)
                end
        in
            adjustStack(offset, 0, stack)
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
        in
            if Vector.sub(pushVec, n)
            then
                let
                    (* This should not have been seen before. *)
                    val _ = case currentLocation of Unset => () | _ => raise InternalError "mapDestReg - already set"
                    val newReg = newPReg kind
                    val () = Array.update(pregMap, n, ToStack)
                    val newContext = { stack = NewEntry{pregNo=n}::stack, oldCount=oldCount, newCount=newCount+1}
                    val () = maxStack := Int.max(!maxStack, newCount+1)
                in
                    (newReg, [PushValue{arg=RegisterArgument newReg}], newContext)
                end
            else
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
            val _ = Vector.sub(pushVec, n) andalso raise InternalError "mapWorkReg - MustPush"
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
        fun pushRegisters([]: {instr: abstract x86ICode, active: intSet, current: intSet} list, code: abstract x86ICode list, _: context) = code
        
        |   pushRegisters({instr=LoadArgument{source, dest=PReg(dReg, dKind), kind}, ...} :: rest, code, context as {stack, oldCount, newCount}) =
            let
                val (sourceVal, sourceCode) = mapSource(source, context)
                (* If we have to push the value we don't have to first load it into a register. *)
                val currentLocation = Array.sub(pregMap, dReg)

                val (destCode, newContext) =
                    if Vector.sub(pushVec, dReg)
                    then
                        let
                            val _ = case currentLocation of Unset => () | _ => raise InternalError "LoadArgument - already set"
                            val () = Array.update(pregMap, dReg, ToStack)
                            val newContext = { stack = NewEntry{pregNo=dReg}::stack, oldCount=oldCount, newCount=newCount+1}
                            val () = maxStack := Int.max(!maxStack, newCount+1)
                        in
                            (PushValue{arg=sourceVal}, newContext)
                        end
                    else
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

        |   pushRegisters({instr=StoreArgument{source, offset, base, index, kind}, ...} :: rest, code, context) =
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

        |   pushRegisters({instr=LoadMemReg { offset, dest}, ...} :: rest, code, context) =
            let
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ LoadMemReg { offset=offset, dest=destVal} :: code, newContext)
            end

        |   pushRegisters({instr=ExchangeRegisters _, ...} :: _, _, _) = (* This is added at the next level. *)
                raise InternalError "pushRegisters - ExchangeRegisters"

        |   pushRegisters({instr=BeginFunction {regArgs}, ...} :: rest, code, context) =
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

        |   pushRegisters({instr=FunctionCall{callKind, regArgs, stackArgs, dest}, ...} :: rest, code, context) =
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

        |   pushRegisters({instr=TailRecursiveCall{callKind, regArgs, stackArgs, returnAddr={srcStack, stack}, stackAdjust}, ...} :: rest, code,
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
                (* Because we're adjusting to the base of the stack we don't need to look at what we've
                   actually pushed. *) 
                val newStackAdjust = stackAdjust+newCount-oldCount
            in
                skipToLabel(rest,
                    TailRecursiveCall{ callKind=callKind, regArgs=newRegArgs,
                        stackArgs=newStackArgs, returnAddr=returnAddr, stackAdjust=newStackAdjust} ::
                            regArgLoads @ stackArgLoads @ code)
            end

        |   pushRegisters({instr=AllocateMemoryOperation{size, flags, dest, saveRegs}, ...} :: rest, code, context) =
            let
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ AllocateMemoryOperation{size=size, flags=flags, dest=destVal, saveRegs=mapSaveRegs saveRegs} :: code, newContext)
            end

        |   pushRegisters({instr=AllocateMemoryVariable{size, dest, saveRegs}, ...} :: rest, code, context) =
            let
                val (sizeVal, sizeCode) = mapSrcReg(size, context)
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ AllocateMemoryVariable{size=sizeVal, dest=destVal, saveRegs=mapSaveRegs saveRegs} :: sizeCode @ code, newContext)
            end

        |   pushRegisters({instr=InitialiseMem{size, addr, init}, ...} :: rest, code, context) =
            let
                val (sizeVal, sizeCode) = mapSrcReg(size, context)
                val (addrVal, addrCode) = mapSrcReg(addr, context)
                val (initVal, initCode) = mapSrcReg(init, context)
            in
                pushRegisters(rest,
                    InitialiseMem{size=sizeVal, addr=addrVal, init=initVal} :: initCode @ addrCode @ sizeCode @ code, context)
            end

        |   pushRegisters({instr=InitialisationComplete, ...} :: rest, code, context) =
                pushRegisters(rest, InitialisationComplete :: code, context)

        |   pushRegisters({instr=StartLoop{loopLabel}, ...} :: rest, code, context) =
            let
                (* Set up the state.  Every jump has to reset to this.  Only the
                   stack pointer is needed. *)
                val () = addNewLabel(loopLabel, context)
            in
                pushRegisters(rest, StartLoop{loopLabel=loopLabel} :: code, context)
            end

        |   pushRegisters({instr=EndLoop{loopLabel, staticRegs}, ...} :: rest, code, context) =
                (* We don't really need to map the static regs since we'll call "Identify" again after this. *)
                pushRegisters(rest, EndLoop{loopLabel=loopLabel, staticRegs=mapSaveRegs staticRegs} :: code, context)

        |   pushRegisters({instr=JumpLoop{regArgs, stackArgs, loopLabel as ILabel lab, checkInterrupt, staticRegs, ...}, ...} :: rest,
                           code, context as {newCount, ...}) =
            let
                val {newCount=labNewCount, ...} = valOf(Array.sub(labelState, lab))
                fun getValues [] = ([], [], [])
                |   getValues ((source, PReg(n, _)) :: rest) =
                    let
                        val (otherRegArgs, otherStackArgs, otherCode) = getValues rest
                    in
                        case Array.sub(pregMap, n) of
                            ToPReg lReg =>
                            let
                                val (sourceVal, sourceCode) = mapSource(source, context)
                            in
                                ((sourceVal, lReg) :: otherRegArgs, otherStackArgs, sourceCode @ otherCode)
                            end
                        |   ToStack =>
                            let
                                val (sourceVal, sourceCode) = mapSource(source, context)
                                val stackOff = getStackOffset(n, context)
                            in
                                (otherRegArgs, (sourceVal, stackOff) :: otherStackArgs, sourceCode @ otherCode)
                            end
                        |   Unset => (* Drop it.  It's never used. Probably a unit argument. *)
                                (otherRegArgs, otherStackArgs, otherCode)
                    end
                val (newRegArguments, newStackArgs, sourceCode) = getValues regArgs
                fun loadStackArg((source, destStack), (otherLoads, otherArgs)) =
                let
                    val (sourceVal, sourceCode) = mapSource(source, context)
                in
                    (sourceCode @ otherLoads, (sourceVal, mapOldStackOffset(destStack, context)) :: otherArgs)
                end
                
                val (stackArgLoads, oldStackArgs) = List.foldr loadStackArg ([], []) stackArgs
                val newStackAdjust = newCount-labNewCount
                val check = Option.map mapSaveRegs checkInterrupt
            in
                skipToLabel(rest,
                    JumpLoop{ regArgs=newRegArguments, stackArgs=oldStackArgs @ newStackArgs, loopLabel=loopLabel,
                              stackAdjust=newStackAdjust, checkInterrupt=check, staticRegs=mapSaveRegs staticRegs} :: sourceCode @ stackArgLoads @ code)
            end

        |   pushRegisters({instr=RaiseExceptionPacket{packet}, ...} :: rest, code, context) =
            let
                val (packetVal, packetCode) = mapSource(packet, context)
            in
                skipToLabel(rest, RaiseExceptionPacket{packet=packetVal} :: packetCode @ code)
            end

        |   pushRegisters({instr=ReserveContainer{size, address}, ...} :: rest, code, {oldCount, newCount, stack}) =
            let
                val containerContext =
                    {oldCount=oldCount+size, newCount=newCount+size, stack=ContainerEntry{size=size}::stack}
                val (destVal, destCode, newContext) = mapDestReg(address, containerContext)
                val () = maxStack := Int.max(!maxStack, newCount+size)
            in
                pushRegisters(rest, destCode @ ReserveContainer{size=size, address=destVal} :: code, newContext)
            end

        |   pushRegisters({instr=IndexedCaseOperation{testReg, workReg, cases}, ...} :: rest, code, context) =
            let
                val (srcVal, srcCode) = mapSrcReg(testReg, context)
                val newWorkReg = mapWorkReg workReg
                (* Set the incoming state for each of the cases. *)
                val () = List.app (fn caseLab => addNewLabel(caseLab, context)) cases
            in
                (* This is an unconditional branch. *)
                skipToLabel(rest, IndexedCaseOperation{testReg=srcVal, workReg=newWorkReg, cases=cases} :: srcCode @ code)
            end

        |   pushRegisters({instr=LockMutable{addr}, ...} :: rest, code, context) =
            let
                val (addrVal, addrCode) = mapSrcReg(addr, context)
            in
                pushRegisters(rest, LockMutable{addr=addrVal} :: addrCode @ code, context)
            end
    
        |   pushRegisters({instr=ForwardJumpLabel{label as ILabel lab, result}, ...} :: rest, code,
                    context as {newCount=currentNewCount, stack=currentStack, ...}) =
            (* This is a forward jump label after other code.  We have to merge the states.
               This was an allocation hot-spot. *)
            (
                (* Get the state.  The label may never have been jumped to. *)
                case Array.sub(labelState, lab) of
                    NONE => pushRegisters(rest, code, context)
                |   SOME {newCount=jumpNewCount, stack=jumpStack, ...} =>
                    let
                        (* Merge the stacks.  Because the stacks can be very deep we need to do this in a
                           way that minimises the rebuilding necessary. *)
                        val lenCurrent = List.length currentStack and lenJump = List.length jumpStack
                        val (currStackTrim, jumpStackTrim) =
                            if lenCurrent > lenJump
                            then (List.drop(currentStack, lenCurrent-lenJump), jumpStack)
                            else (currentStack, List.drop(jumpStack, lenJump-lenCurrent))
                        val trimLength = Int.min(lenCurrent, lenJump)
                        (* We now have the stacks the same length. *)
                        fun findLastDifference(entry1 :: tl1, entry2 :: tl2, tailLen) =
                            let
                                val matchRest = findLastDifference(tl1, tl2, tailLen-1)
                            in
                                if matchRest <> tailLen-1
                                then matchRest
                                else case (entry1, entry2) of
                                    (NewEntry {pregNo=reg1}, NewEntry {pregNo=reg2}) =>
                                        if reg1 = reg2 then tailLen else tailLen-1
                                |   (ContainerEntry {size=size1}, ContainerEntry {size=size2}) =>
                                        if size1 = size2 then tailLen else tailLen-1
                                |   (HandlerEntry, HandlerEntry) => tailLen
                                |   _ => tailLen-1
                            end
                        |   findLastDifference([], [], 0) = 0
                        |   findLastDifference _ = raise InternalError "findLastDifference"
                        
                        val commonLength = findLastDifference(currStackTrim, jumpStackTrim, trimLength)
                        val commonStack = List.drop(currStackTrim, trimLength - commonLength)
                        
                        fun countSizes([], old, new) = (old, new)
                        |   countSizes(NewEntry _ :: tl, old, new) = countSizes(tl, old, new+1)
                        |   countSizes(ContainerEntry{size} :: tl, old, new) = countSizes(tl, old+size, new+size)
                        |   countSizes(HandlerEntry :: tl, old, new) = countSizes(tl, old+2, new+2)
                        
                        val (oldCount, newCount) = countSizes(commonStack, 0, 0)
                        val newContext as {newCount=newSp, ...} = {newCount=newCount, oldCount=oldCount, stack=commonStack}

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
        |   pushRegisters({instr=UnconditionalForwardJump{label}, ...} :: rest, code, context) =
            (
                (* Save the state.  Then enter the "unconditional transfer" state. *)
                addNewLabel(label, context);
                skipToLabel(rest, UnconditionalForwardJump{label=label} :: code)
            )

        |   pushRegisters({instr=ConditionalForwardJump{condition, label, ccRef}, ...} :: rest, code, context) =
            (
                (* Save the state but continue. *)
                addNewLabel(label, context);
                pushRegisters(rest, ConditionalForwardJump{condition=condition, label=label, ccRef=ccRef} :: code, context)
            )

        |   pushRegisters({instr=WordComparison{arg1, arg2, ccRef}, ...} :: rest, code, context) =
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

        |   pushRegisters({instr=PushExceptionHandler{workReg, handleStart}, ...} :: rest, code, context as {oldCount, newCount, stack}) =
            let
                (* Save the state. This is a bit like a conditional jump *)
                val () = addNewLabel(handleStart, context)
                val newWorkReg = mapWorkReg workReg
                (* Add a handler entry to the stack. *)
                val newContext = {oldCount=oldCount+2, newCount=newCount+2, stack=HandlerEntry :: stack}
                val () = maxStack := Int.max(!maxStack, newCount+2)
            in
                pushRegisters(rest, PushExceptionHandler{workReg=newWorkReg, handleStart=handleStart} :: code, newContext)
            end

        |   pushRegisters({instr=PopExceptionHandler{workReg, ...}, ...} :: rest, code, {oldCount, newCount, stack}) =
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

        |   pushRegisters({instr=BeginHandler _, ...} :: _, _, _) =
                (* This should only ever happen after an unconditional jump. *)
                raise InternalError "pushRegisters - BeginHandler"

        |   pushRegisters({instr=ReturnResultFromFunction{resultReg, numStackArgs}, ...} :: rest, code, context as {newCount, ...}) =
            let
                val (resultValue, loadResult) = mapSrcReg(resultReg, context)
                val resetCode =
                    if newCount = 0 then [] else [ResetStackPtr{numWords=newCount}]
            in
                skipToLabel(rest,
                    ReturnResultFromFunction{resultReg=resultValue, numStackArgs=numStackArgs} :: resetCode @ loadResult @ code)
            end

        |   pushRegisters({instr=ArithmeticFunction{oper, resultReg, operand1, operand2, ccRef}, ...} :: rest, code, context) =
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

        |   pushRegisters({instr=TestTagBit{arg, ccRef}, ...} :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSource(arg, context)
            in
                pushRegisters(rest, TestTagBit{arg=sourceVal, ccRef=ccRef} :: sourceCode @ code, context)
            end

        |   pushRegisters({instr=PushValue{arg}, ...} :: rest, code, context as {oldCount, newCount, stack}) =
            let
                val (sourceVal, sourceCode) = mapSource(arg, context)
                (* This was a push from a previous pass.  Treat as a container of size 1. *)
                val pushContext =
                    {oldCount=oldCount+1, newCount=newCount+1, stack=ContainerEntry{size=1}::stack}
            in
                pushRegisters(rest, PushValue{arg=sourceVal} :: sourceCode @ code, pushContext)
            end

        |   pushRegisters({instr=ResetStackPtr{numWords}, ...} :: rest, code, context as {newCount=oldSp, ...}) =
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

        |   pushRegisters({instr=TagValue{source, dest}, ...} :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSrcReg(source, context)
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest, destCode @ TagValue{source=sourceVal, dest=destVal} :: sourceCode @ code, newContext)
            end

        |   pushRegisters({instr=UntagValue{source, dest, isSigned}, ...} :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSource(source, context)
                val (loadedSource, loadCode) = loadStack sourceVal
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ UntagValue{source=loadedSource, dest=destVal, isSigned=isSigned} :: loadCode @ sourceCode @ code, newContext)
            end

        |   pushRegisters({instr=LoadEffectiveAddress{base, offset, index, dest}, ...} :: rest, code, context) =
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

        |   pushRegisters({instr=ShiftOperation{shift, resultReg, operand, shiftAmount, ccRef}, ...} :: rest, code, context) =
            let
                val (opVal, opCode) = mapSource(operand, context)
                val (shiftVal, shiftCode) = mapSource(shiftAmount, context)
                val (destVal, destCode, newContext) = mapDestReg(resultReg, context)
            in
                pushRegisters(rest,
                    destCode @ ShiftOperation{shift=shift, resultReg=destVal, operand=opVal, shiftAmount=shiftVal, ccRef=ccRef} ::
                        shiftCode @ opCode @ code, newContext)
            end

        |   pushRegisters({instr=Multiplication{resultReg, operand1, operand2, ccRef}, ...} :: rest, code, context) =
            let
                val (op1Val, op1Code) = mapSource(operand1, context)
                val (op2Val, op2Code) = mapSource(operand2, context)
                val (destVal, destCode, newContext) = mapDestReg(resultReg, context)
            in
                pushRegisters(rest,
                    destCode @ Multiplication{resultReg=destVal, operand1=op1Val, operand2=op2Val, ccRef=ccRef} :: op2Code @ op1Code @ code, newContext)
            end

        |   pushRegisters({instr=Division{isSigned, dividend, divisor, quotient, remainder}, ...} :: rest, code, context) =
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

        |   pushRegisters({instr=AtomicExchangeAndAdd{base, source}, ...} :: rest, code, context) =
            let
                val (baseVal, baseCode) = mapSrcReg(base, context)
                val (sourceVal, sourceCode) = mapSrcReg(source, context)
                (* The "source" is also a result and must be in a register.  It's an untagged reg
                   so it shouldn't have been marked as to be pushed. *)
                val _ = case sourceCode of [] => () | _ => raise InternalError "pushRegisters - AtomicExchangeAndAdd"
            in
                pushRegisters(rest, AtomicExchangeAndAdd{base=baseVal, source=sourceVal} :: baseCode @ code, context)
            end

        |   pushRegisters({instr=BoxValue{boxKind, source, dest, saveRegs}, ...} :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSrcReg(source, context)
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ BoxValue{boxKind=boxKind, source=sourceVal, dest=destVal, saveRegs=mapSaveRegs saveRegs} :: sourceCode @ code, newContext)
            end

        |   pushRegisters({instr=CompareByteVectors{vec1Addr, vec2Addr, length, ccRef}, ...} :: rest, code, context) =
            let
                val (vec1Val, vec1Code) = mapSrcReg(vec1Addr, context)
                val (vec2Val, vec2Code) = mapSrcReg(vec2Addr, context)
                val (lengthVal, lengthCode) = mapSrcReg(length, context)
            in
                pushRegisters(rest,
                    CompareByteVectors{vec1Addr=vec1Val, vec2Addr=vec2Val, length=lengthVal, ccRef=ccRef} ::
                        lengthCode @ vec2Code @ vec1Code @ code, context)
            end

        |   pushRegisters({instr=BlockMove{srcAddr, destAddr, length, isByteMove}, ...} :: rest, code, context) =
            let
                val (srcVal, srcCode) = mapSrcReg(srcAddr, context)
                val (destVal, destCode) = mapSrcReg(destAddr, context)
                val (lengthVal, lengthCode) = mapSrcReg(length, context)
            in
                pushRegisters(rest,
                    BlockMove{srcAddr=srcVal, destAddr=destVal, length=lengthVal, isByteMove=isByteMove} ::
                        lengthCode @ destCode @ srcCode @ code, context)
            end

        |   pushRegisters({instr=CompareFloatingPt{arg1, arg2, ccRef}, ...} :: rest, code, context) =
            let
                val (arg1Val, arg1Code) = mapSource(arg1, context)
                val (arg2Val, arg2Code) = mapSource(arg2, context)
            in
                pushRegisters(rest, CompareFloatingPt{arg1=arg1Val, arg2=arg2Val, ccRef=ccRef} :: arg2Code @ arg1Code @ code, context)
            end

        |   pushRegisters({instr=X87FPGetCondition{dest, ccRef}, ...} :: rest, code, context) =
            let
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest, destCode @ X87FPGetCondition{dest=destVal, ccRef=ccRef} :: code, newContext)
            end

        |   pushRegisters({instr=X87FPArith{opc, resultReg, arg1, arg2}, ...} :: rest, code, context) =
            let
                val (arg1Val, arg1Code) = mapSource(arg1, context)
                val (arg2Val, arg2Code) = mapSource(arg2, context)
                val (destVal, destCode, newContext) = mapDestReg(resultReg, context)
            in
                pushRegisters(rest,
                    destCode @ X87FPArith{opc=opc, resultReg=destVal, arg1=arg1Val, arg2=arg2Val} ::
                        arg2Code @ arg1Code @ code, newContext)
            end
    
        |   pushRegisters({instr=X87FPUnaryOps{fpOp, dest, source}, ...} :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSource(source, context)
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest,
                    destCode @ X87FPUnaryOps{fpOp=fpOp, dest=destVal, source=sourceVal} :: sourceCode @ code, newContext)
            end

        |   pushRegisters({instr=FloatFixedInt{dest, source}, ...} :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSource(source, context)
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
            in
                pushRegisters(rest, destCode @ FloatFixedInt{dest=destVal, source=sourceVal} :: sourceCode @ code, newContext)
            end
    
        |   pushRegisters({instr=SSE2FPArith{opc, resultReg, arg1, arg2}, ...} :: rest, code, context) =
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

        |   skipToLabel({instr=ForwardJumpLabel{label as ILabel lab, ...}, ...} :: rest, code) =
            (* Label after unconditional transfer.  No need to merge the states. *)
            (
                (* Look at the state.  We may never have branched to this label.  This sometimes
                   happens with andalso/orelse.  In that case just continue. *)
                case Array.sub(labelState, lab) of
                    NONE => skipToLabel(rest, code)
                |   SOME state => pushRegisters(rest, ForwardJumpLabel{label=label, result=NONE} :: code, state)
            )
        
        |   skipToLabel({instr=BeginHandler{handleStart as ILabel lab, packetReg, workReg, ...}, ...} :: rest, code) =
            (* A handler should only ever occur after an unconditional transfer. *)
            (
                (* It is possible that we have an exception handler inside dead code. *)
                case Array.sub(labelState, lab) of
                    NONE => skipToLabel(rest, code)
                |   SOME context =>
                    let
                        val newWorkReg = mapWorkReg workReg
                        val (pktReg, pktCode, newContext) = mapDestReg(packetReg, context)
                    in
                        pushRegisters(rest, pktCode @ BeginHandler{handleStart=handleStart, packetReg=pktReg, workReg=newWorkReg} :: code, newContext)
                    end
            )
        
        |   skipToLabel({instr=EndLoop{loopLabel as ILabel lab, staticRegs}, ...} :: rest, code) =
            (* Normally we will have at least one branch that exits the loop
               but it could be that the loop is exited by raising an exception.
               It is also possible that the loop has never been started because
               the whole loop is dead code.  We have to check the label otherwise
               mapSaveRegs could raise an exception. *)
            (
                case Array.sub(labelState, lab) of
                    NONE => skipToLabel(rest, code)
                |   _ => skipToLabel(rest, EndLoop{loopLabel=loopLabel, staticRegs=mapSaveRegs staticRegs} :: code)
            )

            (* Any other Exited conditions. Skip the instructions.  There are various situations where this
               can arise.  e.g. OS.Process.exit contains an infinite loop processing atExit functions
               until it eventually exits by calling a "stop" function. *)
        |   skipToLabel(_ :: rest, code) = skipToLabel(rest, code)

        val pushedCode = pushRegisters(code, [], emptyContext)
        val resultCode = List.rev pushedCode
    in
        (resultCode, !pregCounter, !labelCounter, !maxStack)
    end

    fun addRegisterPushes(code: extendedBasicBlock vector, pushVec: bool vector, maxPRegs, maxLabels): abstract basicBlock vector * int * int * int =
        raise Fail "TODO - addRegisterPushes"

    structure Sharing =
    struct
        type 'reg x86ICode = 'reg x86ICode
        and abstract = abstract
        and intSet = intSet
        and extendedBasicBlock = extendedBasicBlock
        and 'reg basicBlock = 'reg basicBlock
    end
end;
