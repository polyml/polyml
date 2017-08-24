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
    
    (* Curried subscript functions *)
    fun asub a i = Array.sub(a, i)
    and vsub v i = Vector.sub(v, i)
    
    exception InternalError = Misc.InternalError

    fun addRegisterPushes{code: extendedBasicBlock vector, pushVec: bool vector, maxPRegs} =
    let
        val numberOfBlocks = Vector.length code
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
        (*fun addNewLabel(ILabel lab, context) = Array.update(labelState, lab, SOME context)*)
        
        (* Rewrite the code, replacing any registers that need to be pushed with references to
           the stack.  The result is built up in reverse order and then reversed. *)
        fun pushRegisters([], code, context) = (code, context)
        
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
                val _ = null rest orelse raise InternalError "pushRegisters - TailRecursiveCall"
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
                (TailRecursiveCall{ callKind=callKind, regArgs=newRegArgs,
                        stackArgs=newStackArgs, returnAddr=returnAddr, stackAdjust=newStackAdjust} ::
                            regArgLoads @ stackArgLoads @ code, context)
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

        |   pushRegisters({instr=JumpLoop{regArgs, stackArgs, checkInterrupt}, ...} :: rest, code, context) =
            let
                (* Normally JumpLoop will be the last item in a block but it is possible that we've
                   added a reset-stack after it. *)
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
                val check = Option.map mapSaveRegs checkInterrupt
            in
                pushRegisters(rest,
                    JumpLoop{ regArgs=newRegArguments, stackArgs=oldStackArgs @ newStackArgs, checkInterrupt=check} ::
                         sourceCode @ stackArgLoads @ code, context)
            end

        |   pushRegisters({instr=RaiseExceptionPacket{packet}, ...} :: rest, code, context) =
            let
                val (packetVal, packetCode) = mapSource(packet, context)
                (* Should be last. *)
                val _ = null rest orelse raise InternalError "pushRegisters:RaiseExceptionPacket"
            in
                (RaiseExceptionPacket{packet=packetVal} :: packetCode @ code, context)
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

        |   pushRegisters({instr=IndexedCaseOperation{testReg, workReg}, ...} :: rest, code, context) =
            let
                (* Should be last. *)
                val _ = null rest orelse raise InternalError "pushRegisters:IndexedCaseOperation"
                val (srcVal, srcCode) = mapSrcReg(testReg, context)
                val newWorkReg = mapWorkReg workReg
            in
                (* This is an unconditional branch. *)
                (IndexedCaseOperation{testReg=srcVal, workReg=newWorkReg} :: srcCode @ code, context)
            end

        |   pushRegisters({instr=LockMutable{addr}, ...} :: rest, code, context) =
            let
                val (addrVal, addrCode) = mapSrcReg(addr, context)
            in
                pushRegisters(rest, LockMutable{addr=addrVal} :: addrCode @ code, context)
            end
 
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

        |   pushRegisters({instr=PushExceptionHandler{workReg}, ...} :: rest, code, {oldCount, newCount, stack}) =
            let
                val _ = null rest orelse raise InternalError "pushRegisters - PushExceptionHandler"
                val newWorkReg = mapWorkReg workReg
                (* Add a handler entry to the stack. *)
                val newContext = {oldCount=oldCount+2, newCount=newCount+2, stack=HandlerEntry :: stack}
                val () = maxStack := Int.max(!maxStack, newCount+2)
            in
                (PushExceptionHandler{workReg=newWorkReg} :: code, newContext)
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
                pushRegisters(rest, PopExceptionHandler{workReg=newWorkReg} :: resetCode @ code, newContext)
            end

        |   pushRegisters({instr=BeginHandler{packetReg, workReg, ...}, ...} :: rest, code, {oldCount, newCount, stack}) =
            let
                (* Pop the context as with PopExceptionHandler.  However this will adjust the stack. *)
                fun popContext ([], _, _) = raise InternalError "pushRegisters - pop handler"
                |   popContext (HandlerEntry :: tl, old, new) = {oldCount=old-2, newCount=new-2, stack=tl}
                |   popContext (ContainerEntry _ :: _, _, _) = raise InternalError "pushRegisters - pop handler - container"
                |   popContext (NewEntry _ :: tl, old, new) = popContext(tl, old, new-1)
                val postPopContext = popContext(stack, oldCount, newCount)
                val newWorkReg = mapWorkReg workReg
                val (pktReg, pktCode, postPktRegContext) = mapDestReg(packetReg, postPopContext)
            in
                pushRegisters(rest, pktCode @ BeginHandler{packetReg=pktReg, workReg=newWorkReg} :: code, postPktRegContext)
            end

        |   pushRegisters({instr=ReturnResultFromFunction{resultReg, numStackArgs}, ...} :: rest, code, context as {newCount, ...}) =
            let
                val _ = null rest orelse raise InternalError "pushRegisters - ReturnResultFromFunction"
                val (resultValue, loadResult) = mapSrcReg(resultReg, context)
                val resetCode =
                    if newCount = 0 then [] else [ResetStackPtr{numWords=newCount}]
            in
                (ReturnResultFromFunction{resultReg=resultValue, numStackArgs=numStackArgs} :: resetCode @ loadResult @ code,
                    context)
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

        (* The difficult bit here is matching together the stack if we have more than one
           branch to the same block.  We can work out the stack space by looking at the
           various register sets.  Each export that needs to be pushed increases the
           stack space by one.  Each import that has been pushed can be popped, decreasing
           the stack space.  Locals only need stack space within the block so aren't a
           problem.  PassThroughs remain on the stack.
           
           In addition we may already have items on the stack.  ReserveContainer and PushExceptionHandler
           both use the stack and a previous pass may have added items with PushValue.
           
           The code above assumes that we will always process both arms of a conditional, or all
           the arms of an indexed branch, before we reach the merge point.  When the first branch
           is made to a label we record the state. Subsequently when we reach that label by dropping
           through we merge the states.  Loops are done by recording the state at the start of
           the loop and resetting to it when we jump back.
           
           Merging may involve adding extra branches if at the merge point we find we want to reset
           the stack further than the previous branch did.
        *)
        
        (* Work out the dependencies of each block i.e. the immediate predecessors. *)
        val blockReferences = Array.array(numberOfBlocks, [])
        
        (* This needs to be recursive.  There may be blocks that aren't reachable
           but refer to blocks that are. *)
        local
            fun setReferences fromLabel =
            let
                val ExtendedBasicBlock{ flow, block, ...} = vsub code fromLabel
            in
                (* If this has a JumpLoop don't add it on.  That way we will process
                   the start of a loop and then the jump backwards later. *)
                if List.exists(fn {instr=JumpLoop _, ...} => true | _ => false) block
                then ()
                else
                let
                
                    val refs =
                        case flow of
                            ExitCode => []
                        |   Unconditional lab => [lab]
                        |   Conditional{trueJump, falseJump, ... } => [trueJump, falseJump]
                        |   IndexedBr labs => labs
                        |   SetHandler { handler, continue } => [handler, continue]
                
                    (* We're going to add this label as a reference to each of the
                       blocks we refer to.  If there have not yet been any references
                       we need to process them*)
                    val processNext = List.filter(null o asub blockReferences) refs

                    fun setRefs toLabel =
                        Array.update(blockReferences, toLabel,
                            fromLabel :: Array.sub(blockReferences, toLabel))
                    val () = List.app setRefs refs
                in
                    List.app setReferences processNext
                end
           end
        in
            val () = setReferences 0
        end
        
        (* The state for each of the blocks. *)
        val outputStates = Array.array(numberOfBlocks, NONE: context option)
        (* Input states.  These are only every used for loops. *)
        val inputStates = Array.array(numberOfBlocks, emptyContext)
        (* The result code. *)
        val resultCode = Array.array(numberOfBlocks, NONE)
        (* The flow for each block i.e. where it goes to next.  This may be
           updated if we need to add a block to adjust the stack. *)
        val flowArray =
            Array.tabulate(numberOfBlocks, (fn ExtendedBasicBlock{flow, ...} => flow) o vsub code)
        
        (* Extra blocks to adjust the stack are added here. *)
        val extraBlocks: abstract basicBlock list ref = ref []
        val blockCounter = ref numberOfBlocks
        
        (* Possibly generate code to match the input state to the output
           by removing items from the stack.  If they don't match we need
           to add an adjustment block. *)
        fun createMergedState(predecessors, destBlock) =
        let
            (* If we have multiple states they should all be set.  There shouldn't be
               a loop. *)
            val inputStates = List.map(valOf o (asub outputStates)) predecessors
            (* Choose any of them and remove everything up to the first active entry. *)
            val ExtendedBasicBlock{ imports, passThrough, ...} = vsub code destBlock
            local
                fun removeItems [] = []

                |   removeItems (stack as NewEntry{pregNo} :: rest) =
                    if member(pregNo, imports) orelse member(pregNo, passThrough)
                    then stack
                    else removeItems rest

                    (* Containers and handlers are removed explicitly so if they
                       are on the stack they stay there. *)
                |   removeItems (stack as ContainerEntry _ :: _) = stack

                |   removeItems (stack as HandlerEntry :: _) = stack
            in
                val commonStack = removeItems(#stack(hd inputStates))
            end
            
            fun countSizes([], old, new) = (old, new)
            |   countSizes(NewEntry _ :: tl, old, new) = countSizes(tl, old, new+1)
            |   countSizes(ContainerEntry{size} :: tl, old, new) = countSizes(tl, old+size, new+size)
            |   countSizes(HandlerEntry :: tl, old, new) = countSizes(tl, old+2, new+2)
            
            val (oldCount, newCount) = countSizes(commonStack, 0, 0)
            (* This is the common context that will be used as the input. *)
            val newContext as {newCount=newSp, ...} = {newCount=newCount, oldCount=oldCount, stack=commonStack}
            
            (* We now have to adjust any of the inputs that don't match this. *)
            fun adjustStack(inputBlockNo, {newCount, ...}) =
            if newCount = newSp
            then () (* Nothing to do*)
            else
            let
                val _ = newCount > newSp orelse raise InternalError "adjustStack"
                val adjustCode = [ResetStackPtr{numWords=newCount-newSp}]
                (* If the input is an unconditional branch we could just add it
                   on to the end of that block.  For the moment we always create a
                   new block. *)
                val newBlock = BasicBlock{block=adjustCode, flow=Unconditional destBlock}
                val newBlockNo = !blockCounter before blockCounter := !blockCounter+1
                val () = extraBlocks := newBlock :: !extraBlocks
                (* We need to change the "flow" for the input block so that it points to
                   the new block rather than the old. *)
                fun changeDest d = if d = destBlock then newBlockNo else d
                val newFlow =
                    case asub flowArray inputBlockNo of
                        IndexedBr cases => IndexedBr(List.map changeDest cases)
                    |   Unconditional dest => Unconditional(changeDest dest)
                    |   Conditional{condition, ccRef, trueJump, falseJump} =>
                            Conditional{ccRef=ccRef, condition=condition,
                                    trueJump=changeDest trueJump, falseJump=changeDest falseJump}
                    |   _ => raise InternalError "adjustStack: flow"
            in
                Array.update(flowArray, inputBlockNo, newFlow)
            end
            
            val () = ListPair.appEq adjustStack (predecessors, inputStates)
        in
            newContext
        end
        
        val processTrace = ref []
        
        local
            fun haveProcessed n = isSome(Array.sub(resultCode, n))

            fun processBlocks toDo =
            case List.filter (not o haveProcessed) toDo of
                [] => () (* Finished *)

            |   stillToDo =>
                let
                    (* Select a block all of whose predecessors have been processed.
                       If we can't find one we have a loop and have to select a
                       block that starts a loop. *)
                    fun available dest = List.all haveProcessed (asub blockReferences dest)
                    val picked =
                        case List.find available stillToDo of
                            SOME c => c
                        |   NONE => raise InternalError "processBlocks - nothing available"
                
                    val () = processTrace :=
                        (picked, if isSome(List.find available stillToDo) then NONE else SOME stillToDo) :: ! processTrace

                    val ExtendedBasicBlock{ block, flow, ...} = vsub code picked
                    
                    (* Construct the input state for this block depending on its
                       predecessors. *)
                    local
                        val predecessors = asub blockReferences picked
                        (* Look at the states we actually have.  If this is the start of a
                           loop we will have unset states and we should have just one that is set.*)
                        val predecessorStates =
                            List.mapPartial(asub outputStates) predecessors
                    in
                        val inputState =
                            case predecessorStates of
                                [] => emptyContext (* Initial block. *)
                                        (* Create a merged state even if we just have one.
                                           It removes unnecessary entries and avoids a
                                           problem if this is a loop. *)
                            |   _ => createMergedState(predecessors, picked)
                    end

                    (* Set this as the input state just in case we loop here. *)
                    val () = Array.update(inputStates, picked, inputState)
                    
                    val (codeResult, stateResult) = pushRegisters(block, [], inputState)

                    val successors =
                        case asub flowArray picked of
                            ExitCode => []
                        |   IndexedBr cases => cases
                        |   Unconditional dest => [dest]
                        |   Conditional {trueJump, falseJump, ...} => [falseJump, trueJump]
                        |   SetHandler { handler, continue } => [handler, continue]
                   
                    (* Do we need to adjust the stack because we're looping back?
                       Look to see if this is a JumpLoop and set the adjustment in that.
                       If there's no JumpLoop this should not be branching back. *)
                    val postAdjust =
                        if List.exists(fn JumpLoop _ => true | _ => false) codeResult
                        then
                        let
                            val destination =
                                case flow of
                                    Unconditional dest => dest
                                |   _ => raise InternalError "processBlocks - JumpLoop"
                            val {newCount=destStackCount, ...} = asub inputStates destination
                            val _ = isSome(asub resultCode destination) orelse raise InternalError "processBlocks - not loop"
                            val adjustStack = #newCount stateResult - destStackCount
                            val _ = adjustStack >= 0 orelse raise InternalError "processBlocks - jump loop stack"
                        in
                            if adjustStack = 0
                            then codeResult
                            else ResetStackPtr{numWords=adjustStack} :: codeResult
                        end
                        else
                            (
                                (* Otherwise none of our successors should have been processed yet. *)
                                List.exists(isSome o asub resultCode) successors
                                    andalso raise InternalError "processBlocks - loop?";
                                codeResult
                            )

                    val () = Array.update(resultCode, picked, SOME (List.rev postAdjust))
                    val () = Array.update(outputStates, picked, SOME stateResult)

                    (* Now we've done this block add the blocks it jumps to.
                       They will use the output state as their input state after
                       possibly removing something from the stack. *)
                in
                    processBlocks(successors @ stillToDo)
                end
        in
            val () = processBlocks [0]
        end

        (* Put together the result code and blocks.  N.B. We may have unreferenced and therefore
           unprocessed blocks. *)
        val resultBlocks =
            Vector.tabulate(numberOfBlocks,
                fn i => BasicBlock{block = getOpt(asub resultCode i, []), flow = asub flowArray i})
        
        (* Add any extra blocks to the result. *)
        val finalResult =
            case !extraBlocks of
                [] => resultBlocks
            |   blocks => Vector.concat[resultBlocks, Vector.fromList(List.rev blocks)]
    in
        {code=finalResult, maxPRegs= !pregCounter, maxStack= !maxStack}
    end

    structure Sharing =
    struct
        type 'reg x86ICode = 'reg x86ICode
        and abstract = abstract
        and intSet = intSet
        and extendedBasicBlock = extendedBasicBlock
        and 'reg basicBlock = 'reg basicBlock
    end
end;
