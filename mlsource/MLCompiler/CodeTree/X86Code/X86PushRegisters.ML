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

    fun addRegisterPushes{code: extendedBasicBlock vector, pushVec: bool vector, pregProps} =
    let
        val maxPRegs = Vector.length pregProps
        val numberOfBlocks = Vector.length code
        (* Output registers and properties. *)
        val pregCounter = ref 0
        val pregPropList = ref []

        val maxStack = ref 0
        
        (* Each preg in the input is mapped to either a new preg or the stack. *)
        datatype pregMapType = Unset | ToPReg of abstract | ToStack of int
        val pregMap = Array.array(maxPRegs, Unset)

        (* The stack contains both entries in the input code and entries added here. *)
        datatype stackEntry =
            NewEntry of {pregNo: int}
        |   ContainerEntry of { size: int, active: bool }
        |   HandlerEntry
        
        type context =
        {
            oldCount: int,      (* The stack entries from the input code. *)
            newCount: int,      (* The total number of items on the stack. *)
            stack: stackEntry list
        }

        val emptyContext: context = { oldCount=0, newCount=0, stack=[]}

        fun newPReg propKind =
        let
            val regNo = !pregCounter before pregCounter := !pregCounter + 1
            val () = pregPropList := propKind :: !pregPropList
        in
            PReg regNo
        end
        
        and newStackLoc size =
        let
            val regNo = !pregCounter before pregCounter := !pregCounter + 1
            val () = pregPropList := RegPropStack size :: !pregPropList
        in
            StackLoc{size=size, rno=regNo}
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
                    |   ContainerEntry {size, active=false} => adjustStack(old, new+size, tl)
                    |   ContainerEntry {size, ...} =>
                            if old < size
                            then new + old
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
        fun mapSrcReg(PReg n, { newCount, ...}: context) =
            case Array.sub(pregMap, n) of
                Unset => raise InternalError "mapSrcReg - unset"
            |   ToPReg preg => (preg, [])
            |   ToStack stackLoc =>
                let
                    (* Make a new untagged register. That will prevent us pushing it if
                       we have to spill registers. *)
                    val newReg = newPReg RegPropUntagged
                in
                    (newReg,
                        [LoadArgument{source=StackLocation{wordOffset=newCount-stackLoc}, dest=newReg, kind=MoveWord}])
                end
       
        fun mapDestReg(PReg n, context: context as {stack, oldCount, newCount}) =
        let
            val currentLocation = Array.sub(pregMap, n)
            val kind = Vector.sub(pregProps, n)
        in
            if Vector.sub(pushVec, n)
            then
                let
                    (* This should not have been seen before. *)
                    val _ = case currentLocation of Unset => () | _ => raise InternalError "mapDestReg - already set"
                    val newReg = newPReg kind
                    val () = Array.update(pregMap, n, ToStack (newCount+1))
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
                        |   ToStack _ => raise InternalError "mapDestReg - already on stack"
                in
                    (newReg, [], context)
                end
        end
        
        (* A work register must be a normal register. *)
        fun mapWorkReg(PReg n) =
        let
            val currentLocation = Array.sub(pregMap, n)
            val _ = Vector.sub(pushVec, n) andalso raise InternalError "mapWorkReg - MustPush"
        in
            case currentLocation of
                Unset =>
                let
                    val kind = Vector.sub(pregProps, n)
                    val newReg = newPReg kind
                    val () = Array.update(pregMap, n, ToPReg newReg)
                in
                    newReg
                end
            |   ToPReg preg => preg
            |   ToStack _ => raise InternalError "mapWorkReg - on stack"
                
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

        (* For the moment return the old container since we don't do anything with it. *)
        fun mapContainer(container, _) = container

        fun mapSource(RegisterArgument(PReg r), { newCount, ...}: context) =
            (
                case Array.sub(pregMap, r) of
                    Unset => raise InternalError "mapSrcReg - unset"
                |   ToPReg preg => (RegisterArgument preg, [])
                |   ToStack stackLoc => (StackLocation{wordOffset=newCount-stackLoc}, [])
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

        |   mapSource(StackLocation{wordOffset}, context) =
                (StackLocation{wordOffset=mapOldStackOffset(wordOffset, context)}, [])

        (* If the argument AFTER processing is a stack entry, then load it. *)
        fun loadStack(stack as StackLocation _) =
            let
                val newReg = newPReg RegPropUntagged
            in
                (RegisterArgument newReg, [LoadArgument{source=stack, dest=newReg, kind=MoveWord}])
            end
        |   loadStack other = (other, [])


        local
            (* We don't need to save registers if we've pushed them.
               N.B.  If we ever cache registers we'll need to clear the cache. *)
            fun mapSaveReg (PReg n, l) =
                case Array.sub(pregMap, n) of
                    Unset => raise InternalError "mapSaveReg - unset"
                |   ToPReg preg => preg :: l
                |   ToStack _ => l
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
        
        |   pushRegisters({instr=LoadArgument{source, dest=PReg dReg, kind}, ...} :: rest, code, context as {stack, oldCount, newCount}) =
            let
                val (sourceVal, sourceCode) = mapSource(source, context)
                (* If we have to push the value we don't have to first load it into a register. *)
                val currentLocation = Array.sub(pregMap, dReg)

                val (destCode, newContext) =
                    if Vector.sub(pushVec, dReg)
                    then
                        let
                            val _ = case currentLocation of Unset => () | _ => raise InternalError "LoadArgument - already set"
                            val () = Array.update(pregMap, dReg, ToStack(newCount+1))
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
                                        val dKind = Vector.sub(pregProps, dReg)
                                        val newReg = newPReg dKind
                                        val () = Array.update(pregMap, dReg, ToPReg newReg)
                                    in
                                        newReg
                                    end
                                |   ToPReg preg => preg
                                |   ToStack _ => raise InternalError "LoadArgument - already on stack"
                        in
                            (LoadArgument{source=sourceVal, dest=destReg, kind=kind}, context)
                        end
            in
                pushRegisters(rest, destCode  :: sourceCode @ code, newContext)
            end

        |   pushRegisters({instr=StoreArgument{source, offset, base, index, kind}, ...} :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSource(source, context)
                (* We can't have a memory-memory store so we have to load the source if it's now on the stack. *)
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

        |   pushRegisters({instr=BeginFunction {regArgs, stackArgs}, ...} :: rest, code, context) =
            let
                val newStackArgs = List.map(fn _ => newStackLoc 1) stackArgs
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
                    pushCode @ (BeginFunction {regArgs=newRegArgs, stackArgs=newStackArgs} :: code), newContext)
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

        |   pushRegisters({instr=TailRecursiveCall{callKind, regArgs, stackArgs, stackAdjust, stackOffset}, ...} :: rest, code,
                          context as {oldCount, newCount, ...}) =
            let
                val _ = null rest orelse raise InternalError "pushRegisters - TailRecursiveCall"
                fun loadStackArg ({src, stack}, (otherLoads, otherArgs)) =
                let
                    val (argVal, loadCode) = mapSource(src, context)
                in
                    (loadCode @ otherLoads, {src=argVal, stack=stack} :: otherArgs)
                end
                val (stackArgLoads, newStackArgs) = List.foldr loadStackArg ([], []) stackArgs
                
                fun loadRegArg ((arg, reg), (otherLoads, otherArgs)) =
                let
                    val (argVal, loadCode) = mapSource(arg, context)
                in
                    (loadCode @ otherLoads, (argVal, reg) :: otherArgs)
                end
                val (regArgLoads, newRegArgs) = List.foldr loadRegArg ([], []) regArgs

                (* stackOffset is the current stack size.  We need it at the moment because codeExtended
                   doesn't have a current stack pointer. *) 
                val _ = stackOffset = oldCount orelse raise InternalError "pushRegisters - tail"
                val newStackOffset = newCount
            in
                (TailRecursiveCall{ callKind=callKind, regArgs=newRegArgs,
                        stackArgs=newStackArgs, stackAdjust=stackAdjust, stackOffset=newStackOffset} ::
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

        |   pushRegisters({instr=JumpLoop{regArgs, stackArgs, checkInterrupt}, ...} :: rest, code, context as {newCount, ...}) =
            let
                (* Normally JumpLoop will be the last item in a block but it is possible that we've
                   added a reset-stack after it. *)
                fun getValues [] = ([], [], [])
                |   getValues ((source, PReg n) :: rest) =
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
                        |   ToStack stackLoc =>
                            let
                                val (sourceVal, sourceCode) = mapSource(source, context)
                                val stackOff = newCount - stackLoc
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

        |   pushRegisters({instr=ReserveContainer{size, container}, ...} :: rest, code, {oldCount, newCount, stack}) =
            let
                val containerContext =
                    {oldCount=oldCount+size, newCount=newCount+size, stack=ContainerEntry{size=size, active=true}::stack}
                val () = maxStack := Int.max(!maxStack, newCount+size)
            in
                pushRegisters(rest, ReserveContainer{size=size, container=container} :: code, containerContext)
            end

        |   pushRegisters({instr=LoadContainerAddress{container, dest, stackOffset}, ...} :: rest, code, context) =
            let
                val newStackOffset = mapOldStackOffset(stackOffset, context)
                val (destVal, destCode, newContext) = mapDestReg(dest, context)
                val newContainer = mapContainer(container, context)
            in
                pushRegisters(rest, destCode @ LoadContainerAddress{container=newContainer, dest=destVal, stackOffset=newStackOffset} :: code, newContext)
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
                |   popContext (ContainerEntry{active=true, ...} :: _, _, _) = raise InternalError "pushRegisters - pop handler - container"
                |   popContext (ContainerEntry{active=false, size} :: tl, old, new) = popContext(tl, old, new-size)
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
                    {oldCount=oldCount+1, newCount=newCount+1, stack=ContainerEntry{size=1, active=true}::stack}
            in
                pushRegisters(rest, PushValue{arg=sourceVal} :: sourceCode @ code, pushContext)
            end

        |   pushRegisters({instr=ResetStackPtr{numWords}, ...} :: rest, code, {newCount, oldCount, stack}) =
            (* This indicates that an old entry is no longer required.  We can't actually remove
               it because we may have pushed something since.  Instead we mark it as inactive and remove
               it when everything above is free. *)
            let
                fun removeItems(0, stack) = stack
                |   removeItems(_, []) = raise InternalError "removeItems - stack empty"
                |   removeItems(toRemove, (entry as ContainerEntry {active=false, ...}) :: tl) =
                        entry :: removeItems(toRemove, tl)
                |   removeItems(toRemove, ContainerEntry {size, active=true} :: tl) =
                        if toRemove < size then raise InternalError "removeItems - container size"
                        else ContainerEntry {size=size, active=false} :: removeItems(toRemove-size, tl)
                |   removeItems(_, HandlerEntry :: _) = raise InternalError "removeItems - handler"
                |   removeItems(toRemove, entry :: tl) = entry :: removeItems(toRemove, tl)
                
                val newStack = removeItems(numWords, stack)
                (* Even though the entries have been marked for removal we only deduct
                   this from the old count. *)
                val newContext = {newCount=newCount, oldCount=oldCount-numWords, stack=newStack}
            in
                pushRegisters(rest, code, newContext)
            end

        |   pushRegisters({instr=StoreToStack{source, container, field, stackOffset}, ...} :: rest, code, context) =
            let
                val (sourceVal, sourceCode) = mapSource(source, context)
                (* We can't have a memory-memory store so we have to load the source if it's now on the stack. *)
                val (loadedSource, loadCode) = loadStack sourceVal
                val newOffset = mapOldStackOffset(stackOffset, context)
                val newContainer = mapContainer(container, context)
            in
                pushRegisters(rest,
                    StoreToStack{source=loadedSource, container=newContainer, field=field, stackOffset=newOffset} ::
                        loadCode @ sourceCode @ code,
                    context)
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
        
        (* The result code. *)
        val resultBlocks = Array.array(numberOfBlocks, BasicBlock{flow=ExitCode, block=[]})
        
        (* Extra blocks to adjust the stack are added here. *)
        val extraBlocks: abstract basicBlock list ref = ref []
        val blockCounter = ref numberOfBlocks
         
        (* Recursive scan of the blocks.  For each block we produce an input and output state.
           The input state is the output state of the predecessor i.e. some block that jumps to
           this, but with any entries removed that are not used in this block.  It is then
           necessary to match the input state, if necessary by adding extra blocks that just
           do the matching. *)
        local
            (* The stack size we've assumed for the block.  Also indicates if
               a block has already been processed. *)
            val inputStackSizes = Array.array(numberOfBlocks, NONE: int option)

            fun processBlocks(blockNo, lastOutputState) =
            case asub inputStackSizes blockNo of
                SOME stackSize => stackSize
                (* Already done.  Return the stack size it is expecting. *)

            |   NONE =>
                let
                    (* This is the first time we've come to this block.  *)
                    val ExtendedBasicBlock{ block, flow, imports, passThrough, ...} = vsub code blockNo

                    (* Remove any items from the input state that are no longer needed for
                       this block.  They could be local to the previous block or needed by
                       a different successor. *)
                    fun removeItems([], 0, 0) = {newCount=0, oldCount=0, stack=[]}
                
                    |   removeItems([], _, _) = raise InternalError "removeItems - stack size"

                    |   removeItems (stack as NewEntry{pregNo} :: rest, oldSize, newSize) =
                        if member(pregNo, imports) orelse member(pregNo, passThrough)
                        then {newCount=newSize, oldCount=oldSize, stack=stack}
                        else removeItems(rest, oldSize, newSize-1)

                        (* This container has been deleted.  Its size has already been removed from
                           the old count. *)
                    |   removeItems (ContainerEntry{active=false, size} :: rest, oldSize, newSize) =
                            removeItems(rest, oldSize, newSize-size)

                    |   removeItems (stack, oldSize, newSize) = {newCount=newSize, oldCount=oldSize, stack=stack}

                    val {stack, newCount, oldCount} = lastOutputState

                    val newContext as {newCount=newSp, ...} = removeItems(stack, oldCount, newCount)
                    
                    (* Save the stack size in case we come by a different route. *)
                    val () = Array.update(inputStackSizes, blockNo, SOME newSp)

                    (* We can't adjust the stack at this point. *)
                    val (codeResult, stateResult as {newCount=outputCount, ...}) =
                        pushRegisters(block, [], newContext)
                    
                    (* Process the successor.  If we need a stack adjustment this will require
                       an adjustment block.  TODO: We could put a pre-adjustment if we only have one
                       branch to this block. *)
                    fun matchStacks targetBlock =
                    let
                        (* Process the destination.  If it hasn't been processed. *)
                        val expectedInput = processBlocks(targetBlock, stateResult)
                    in
                        if expectedInput = outputCount
                        then targetBlock
                        else
                        let
                            val _ = outputCount > expectedInput orelse raise InternalError "adjustStack"
                            val adjustCode = [ResetStackPtr{numWords=outputCount-expectedInput}]
                            val newBlock = BasicBlock{block=adjustCode, flow=Unconditional targetBlock}
                            val newBlockNo = !blockCounter before blockCounter := !blockCounter+1
                            val () = extraBlocks := newBlock :: !extraBlocks
                        in
                            newBlockNo
                        end
                    end
                    
                    val (finalCode, newFlow) =
                        case flow of
                            ExitCode => (codeResult, ExitCode)
                        
                        |   Unconditional m =>
                            let
                                (* Process the block.  Since we're making an unconditional jump
                                   we can include any stack adjustment needed to match the
                                   destination in here.  In particular this includes loops. *)
                                val expectedInput = processBlocks(m, stateResult)
                                val resultCode =
                                    if expectedInput = outputCount
                                    then codeResult
                                    else ResetStackPtr{numWords=outputCount-expectedInput} :: codeResult
                            in
                                (resultCode, Unconditional m)
                            end

                            (* For any of these, if we need to adjust the stack we have to add an
                               adjustment block. *)
                        |   Conditional {trueJump, falseJump, ccRef, condition} =>
                            (codeResult,
                                Conditional{trueJump=matchStacks trueJump, falseJump=matchStacks falseJump,
                                    ccRef=ccRef, condition=condition})
                        
                        |   SetHandler{ handler, continue } =>
                            (codeResult, SetHandler{ handler=matchStacks handler, continue=matchStacks continue})
                        
                        |   IndexedBr cases => (codeResult, IndexedBr(map matchStacks cases))
                    val () =
                        Array.update(resultBlocks, blockNo, BasicBlock{block=List.rev finalCode, flow=newFlow})
                in
                    newSp (* Return our input stack size. *)
                end
        in
            val _ = processBlocks(0, emptyContext)
        end

        (* Put together the result code and blocks. *)
        val resultBlocks = Array.vector resultBlocks
        
        (* Add any extra blocks to the result. *)
        val finalResult =
            case !extraBlocks of
                [] => resultBlocks
            |   blocks => Vector.concat[resultBlocks, Vector.fromList(List.rev blocks)]
        
        val pregProperties = Vector.fromList(List.rev(! pregPropList))
    in
        {code=finalResult, pregProps=pregProperties, maxStack= !maxStack}
    end

    structure Sharing =
    struct
        type 'reg x86ICode = 'reg x86ICode
        and abstract = abstract
        and intSet = intSet
        and extendedBasicBlock = extendedBasicBlock
        and 'reg basicBlock = 'reg basicBlock
        and regProperty = regProperty
    end
end;
