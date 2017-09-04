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

    (* Each preg in the input is mapped to either a new preg or the stack. *)
    datatype pregMapType = Unset | ToPReg of preg | ToStack of int * stackLocn

    (* The stack contains both entries in the input code and entries added here.
       It is really used to ensure that the stack at run time is the same size
       at the start of a block whichever block has jumped to it. *)
    datatype stackEntry =
        NewEntry of {pregNo: int} (* pregNo is the original preg that has been pushed here. *)
    |   ContainerEntry of { size: int, active: bool, stackLoc: stackLocn }
    |   HandlerEntry
   
    fun addRegisterPushes{code: extendedBasicBlock vector, pushVec: bool vector, pregProps} =
    let
        val maxPRegs = Vector.length pregProps
        val numberOfBlocks = Vector.length code
        (* Output registers and properties. *)
        val pregCounter = ref 0
        val pregPropList = ref []
        val pregMap = Array.array(maxPRegs, Unset)
        
        (* Cache registers.  The index is the original preg or stackloc. *)
        local
            (* The number of items cached is likely to be a small proportion of
               the total number of pregs which can be very large.  To avoid scanning
               the whole array each time we keep a list of the currently cached items
               and just clear that. *)
            val cacheMap: preg option ref vector = Vector.tabulate(maxPRegs, fn _ => ref NONE)
            val currentCache: preg option ref list ref = ref []
        in
            fun clearCache() =
                List.app(fn r => r := NONE) (! currentCache) before currentCache := []
            
            fun getCache n = ! (Vector.sub(cacheMap, n))
            
            fun setCache(n, newItem) =
            let
                val entry = Vector.sub(cacheMap, n)
            in
                (* If this was not previously cached add it to the list.
                   Note: we may clear an entry and later it set it again.
                   This will add the item to the list a second time. *)
                case !entry of
                    NONE => currentCache := entry :: ! currentCache
                |   SOME _ => ();
                entry := newItem
            end
        end

        val maxStack = ref 0
         (* The result code. *)
        val resultBlocks = Array.array(numberOfBlocks, BasicBlock{flow=ExitCode, block=[]})
        
        (* Extra blocks to adjust the stack are added here. *)
        val extraBlocks: basicBlock list ref = ref []
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
                    fun removeItems(result as {stack=[], stackCount=0}) = result
                
                    |   removeItems{stack=[], ...} = raise InternalError "removeItems - stack size"

                    |   removeItems {stack as NewEntry{pregNo} :: rest, stackCount=newSize} =
                        if member(pregNo, imports) orelse member(pregNo, passThrough)
                        then {stackCount=newSize, stack=stack}
                        else removeItems{stack=rest, stackCount=newSize-1}

                        (* This container has been deleted.  Its size has already been removed from
                           the old count. *)
                    |   removeItems {stack=ContainerEntry{active=false, size, ...} :: rest, stackCount=newSize} =
                            removeItems{stack=rest, stackCount=newSize-size}

                    |   removeItems result = result

                    val {stackCount=newSp, stack=newStack} = removeItems lastOutputState
                    
                    (* References to hold the current stack count (number of words on the stack)
                       and the list of items on the stack.  The list is not used directly to map
                       stack addresses.  Instead it is used to match the stack at the beginning
                       and end of a block. *)
                    val stackCount = ref newSp
                    val stack = ref newStack
                    
                    (* Save the stack size in case we come by a different route. *)
                    val () = Array.update(inputStackSizes, blockNo, SOME newSp)
                    
                    fun pushItemToStack item =
                    let
                        val size =
                            case item of
                                NewEntry _ => 1
                            |   ContainerEntry{size, ...} => size
                            |   HandlerEntry => 2
                    in
                        stackCount := ! stackCount+size;
                        stack := item :: ! stack;
                        maxStack := Int.max(!maxStack, !stackCount)
                    end
 
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
        
                    (* Map a source register.  This always loads the argument. *)
                    fun mapSrcReg(PReg n, _) =
                        case Array.sub(pregMap, n) of
                            Unset => raise InternalError "mapSrcReg - unset"
                        |   ToPReg preg => (preg, [])
                        |   ToStack(stackLoc, container as StackLoc{size, ...}) =>
                            let
                                (* Make a new untagged register. That will prevent us pushing it if
                                   we have to spill registers. *)
                                val newReg = newPReg RegPropUntagged
                                val sourceCache = getCache n
                                val stackSource =
                                    StackLocation{wordOffset= !stackCount-stackLoc-size, container=container, field=0, cache=sourceCache}
                                (* Because this is in a register we can copy it to a cache register. *)
                                val newCacheReg = newPReg RegPropCache
                                val () = setCache(n, SOME newCacheReg)
                            in
                                (newReg, [CopyToCache{source=newReg, dest=newCacheReg}, LoadArgument{source=stackSource, dest=newReg, kind=MoveWord}])
                            end


                    fun mapDestReg(PReg n, _) =
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
                            val newContainer = newStackLoc 1
                            val () = Array.update(pregMap, n, ToStack (!stackCount, newContainer))
                            val () = pushItemToStack(NewEntry{pregNo=n})
                        in
                            (newReg, [PushValue{arg=RegisterArgument newReg, container=newContainer}])
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
                            (newReg, [])
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
                    |   mapIndex(MemIndex1 r, current) =
                            let val (sreg, c) = mapSrcReg(r, current) in (MemIndex1 sreg, c) end
                    |   mapIndex(MemIndex2 r, current) =
                            let val (sreg, c) = mapSrcReg(r, current) in (MemIndex2 sreg, c) end
                    |   mapIndex(MemIndex4 r, current) =
                            let val (sreg, c) = mapSrcReg(r, current) in (MemIndex4 sreg, c) end
                    |   mapIndex(MemIndex8 r, current) =
                            let val (sreg, c) = mapSrcReg(r, current) in (MemIndex8 sreg, c) end

                    (* Adjust a stack offset from the old state to the new state. *)
                    fun mapContainerAndStack(StackLoc{rno, size}, field, _) =
                    let
                        val (newStackAddr, newContainer) =
                        case Array.sub(pregMap, rno) of
                            Unset => raise InternalError "mapContainer - unset"
                        |   ToPReg _ => raise InternalError "mapContainer - ToPReg"
                        |   ToStack stackContainer => stackContainer

                        val newOffset = !stackCount-(newStackAddr+size) + field
                    in
                        (newOffset, newContainer)
                    end
        
                    (* Add an entry for an existing stack entry. *)
                    fun mapDestContainer(StackLoc{rno, size}, locn) =
                    (
                        case Array.sub(pregMap, rno) of
                            Unset =>
                            let
                                val newContainer = newStackLoc size
                                val () = Array.update(pregMap, rno, ToStack(locn, newContainer))
                            in
                                newContainer
                            end
                        |   _ => raise InternalError "mapDestContainer: already set"
                    )

                    fun mapSource(RegisterArgument(PReg r), _) =
                        (
                            case Array.sub(pregMap, r) of
                                Unset => raise InternalError "mapSrcReg - unset"
                            |   ToPReg preg => (RegisterArgument preg, [])
                            |   ToStack(stackLoc, container as StackLoc{size, ...}) =>
                                let
                                    val sourceCache = getCache r
                                    val stackLoc =
                                        StackLocation{wordOffset= !stackCount-stackLoc-size, container=container, field=0, cache=sourceCache}
                                    (* If this is cached we need to make a new cache register and copy it there. *)
                                    val cacheCode =
                                        case sourceCache of
                                            NONE => []
                                        |   SOME cacheR =>
                                            let
                                                val newCacheReg = newPReg RegPropCache
                                                val () = setCache(r, SOME newCacheReg)
                                            in
                                                [CopyToCache{source=cacheR, dest=newCacheReg}]
                                            end
                                in
                                    (stackLoc, cacheCode)
                                end
                        )

                    |   mapSource(a as AddressConstant _, _) = (a, [])
                    |   mapSource(i as IntegerConstant _, _) = (i, [])

                    |   mapSource(MemoryLocation{base, offset, index}, current) =
                        let
                            val (baseReg, baseCode) = mapSrcReg(base, current)
                            val (indexValue, indexCode) = mapIndex(index, current)
                        in
                            (MemoryLocation{base=baseReg, offset=offset, index=indexValue}, baseCode @ indexCode)
                        end

                    |   mapSource(StackLocation{container as StackLoc{rno, ...}, field, cache, ...}, current) =
                        let
                            val (newOffset, newContainer) = mapContainerAndStack(container, field, current)
                            (* Was the item previously cached?  If it wasn't or the cache reg has been marked
                               as "must push" we can't use a cache. *)
                            val newCache =
                                case cache of
                                    NONE => NONE
                                |   SOME (PReg c) =>
                                    if Vector.sub(pushVec, c)
                                    then NONE (* We had marked this as to be pushed - we can't use a cache here. *)
                                    else getCache rno
                            val stackLoc =
                                StackLocation{wordOffset=newOffset, container=newContainer, field=field, cache=newCache}
                            val cacheCode =
                                case newCache of
                                    NONE => (setCache(rno, NONE); [])
                                |   SOME oldCacheReg =>
                                    let
                                        val newCacheReg = newPReg RegPropCache
                                        val () = setCache(rno, SOME newCacheReg)
                                    in
                                        [CopyToCache{source=oldCacheReg, dest=newCacheReg}]
                                    end
                        in
                            (stackLoc, cacheCode)
                        end

                    (* Force a load of the source into a register if it is on the stack.
                       This is used in cases where a register or literal is allowed but not
                       a memory location. If we do load it we can cache the register. *)
                    fun mapAndLoad(source as RegisterArgument(PReg r), current) =
                        let
                            val (sourceVal, sourceCode) = mapSource(source, current)
                        in
                            case sourceVal of
                                stack as StackLocation _ =>
                                let
                                    val newReg = newPReg RegPropUntagged
                                    val newCacheReg = newPReg RegPropCache
                                    val _ = setCache(r, SOME newCacheReg)
                                in
                                    (RegisterArgument newReg,
                                        CopyToCache{source=newReg, dest=newCacheReg} :: LoadArgument{source=stack, dest=newReg, kind=MoveWord} :: sourceCode)
                                end
                            |   _ => (sourceVal, sourceCode)
                        end

                    |   mapAndLoad(StackLocation _, _) = raise InternalError "mapAndLoad - already a stack loc"
                    |   mapAndLoad(MemoryLocation _, _) = raise InternalError "mapAndLoad - already a mem loc"
                    |   mapAndLoad(source, current) = mapSource(source, current)
        
                    (* Rewrite the code, replacing any registers that need to be pushed with references to
                       the stack.  The result is built up in reverse order and then reversed. *)
                    fun pushRegisters([], code) = code
        
                    |   pushRegisters({instr=LoadArgument{source, dest=PReg dReg, kind}, current, ...} :: rest, code) =
                        let
                            val (sourceVal, sourceCode) = mapSource(source, current)
                            (* If we have to push the value we don't have to first load it into a register. *)
                            val currentLocation = Array.sub(pregMap, dReg)

                            val destCode =
                                if Vector.sub(pushVec, dReg)
                                then
                                let
                                    val _ = case currentLocation of Unset => () | _ => raise InternalError "LoadArgument - already set"
                                    val container = newStackLoc 1
                                    val () = Array.update(pregMap, dReg, ToStack(! stackCount, container))
                                    val () = pushItemToStack(NewEntry{pregNo=dReg})
                                in
                                    (PushValue{arg=sourceVal, container=container})
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
                                    (LoadArgument{source=sourceVal, dest=destReg, kind=kind})
                                end
                        in
                            pushRegisters(rest, destCode  :: sourceCode @ code)
                        end

                    |   pushRegisters({instr=StoreArgument{source, offset, base, index, kind}, current, ...} :: rest, code) =
                        let
                            val (loadedSource, sourceCode) = mapAndLoad(source, current)
                            (* We can't have a memory-memory store so we have to load the source if it's now on the stack. *)
                            val (baseReg, baseCode) = mapSrcReg(base, current)
                            val (indexValue, indexCode) = mapIndex(index, current)
                        in
                            pushRegisters(rest,
                                StoreArgument{source=loadedSource, base=baseReg, offset=offset, index=indexValue, kind=kind} ::
                                    indexCode @ baseCode @ sourceCode @ code)
                        end

                    |   pushRegisters({instr=LoadMemReg { offset, dest}, current, ...} :: rest, code) =
                        let
                            val (destVal, destCode) = mapDestReg(dest, current)
                        in
                            pushRegisters(rest,
                                destCode @ LoadMemReg { offset=offset, dest=destVal} :: code)
                        end

                    |   pushRegisters({instr=BeginFunction {regArgs, stackArgs}, current, ...} :: rest, code) =
                        let
                            (* Create a new container list.  The offsets begin at -numArgs. *)
                            fun newContainers(src :: srcs, offset) =
                                let
                                    val newContainer = mapDestContainer(src, offset)
                                in
                                    newContainer :: newContainers(srcs, offset+1)
                                end
                            |   newContainers _ = []

                            val newStackArgs = newContainers(stackArgs, ~ (List.length stackArgs))
                            (* Push any registers that need to be pushed. *)
                            fun pushReg((preg, rreg), (others, code)) =
                            let
                                val (newReg, newCode) = mapDestReg(preg, current)
                            in
                                ((newReg, rreg) :: others, newCode @ code)
                            end
                            val (newRegArgs, pushCode) = List.foldl pushReg ([], []) regArgs
                        in
                            pushRegisters(rest,
                                pushCode @ (BeginFunction {regArgs=newRegArgs, stackArgs=newStackArgs} :: code))
                        end

                    |   pushRegisters({instr=FunctionCall{callKind, regArgs, stackArgs, dest}, current, ...} :: rest, code) =
                        let
                            (* It's possible that this could lead to having to spill registers in order
                               to load others.  Leave that problem for the moment. *)
                            fun loadStackArg (arg, (otherLoads, otherArgs)) =
                            let
                                val (argVal, loadCode) = mapSource(arg, current)
                            in
                                (loadCode @ otherLoads, argVal :: otherArgs)
                            end
                            val (stackArgLoads, newStackArgs) = List.foldr loadStackArg ([], []) stackArgs

                            fun loadRegArg ((arg, reg), (otherLoads, otherArgs)) =
                            let
                                val (argVal, loadCode) = mapSource(arg, current)
                            in
                                (loadCode @ otherLoads, (argVal, reg) :: otherArgs)
                            end
                            val (regArgLoads, newRegArgs) = List.foldr loadRegArg ([], []) regArgs
                            val (destVal, destCode) = mapDestReg(dest, current)
                            (* Now clear the cache table. *)
                            val () = clearCache()
                        in
                            pushRegisters(rest,
                                destCode @
                                    FunctionCall{ callKind=callKind, regArgs=newRegArgs, stackArgs=newStackArgs, dest=destVal} ::
                                        regArgLoads @ stackArgLoads @ code)
                        end

                    |   pushRegisters({instr=TailRecursiveCall{callKind, regArgs, stackArgs, stackAdjust, ...}, current, ...} :: rest, code) =
                        let
                            val _ = null rest orelse raise InternalError "pushRegisters - TailRecursiveCall"
                            fun loadStackArg ({src, stack}, (otherLoads, otherArgs)) =
                            let
                                val (argVal, loadCode) = mapSource(src, current)
                            in
                                (loadCode @ otherLoads, {src=argVal, stack=stack} :: otherArgs)
                            end
                            val (stackArgLoads, newStackArgs) = List.foldr loadStackArg ([], []) stackArgs
                
                            fun loadRegArg ((arg, reg), (otherLoads, otherArgs)) =
                            let
                                val (argVal, loadCode) = mapSource(arg, current)
                            in
                                (loadCode @ otherLoads, (argVal, reg) :: otherArgs)
                            end
                            val (regArgLoads, newRegArgs) = List.foldr loadRegArg ([], []) regArgs

                            (* stackOffset is the current stack size.  We need it at the moment because codeExtended
                               doesn't have a current stack pointer. *) 
                            val newStackOffset = !stackCount
                        in
                            (TailRecursiveCall{ callKind=callKind, regArgs=newRegArgs,
                                    stackArgs=newStackArgs, stackAdjust=stackAdjust, stackOffset=newStackOffset} ::
                                        regArgLoads @ stackArgLoads @ code)
                        end

                    |   pushRegisters({instr=AllocateMemoryOperation{size, flags, dest, ...}, current, ...} :: rest, code) =
                        let
                            val (destVal, destCode) = mapDestReg(dest, current)
                        in
                            pushRegisters(rest,
                                destCode @ AllocateMemoryOperation{size=size, flags=flags, dest=destVal, saveRegs=[]} :: code)
                        end

                    |   pushRegisters({instr=AllocateMemoryVariable{size, dest, ...}, current, ...} :: rest, code) =
                        let
                            val (sizeVal, sizeCode) = mapSrcReg(size, current)
                            val (destVal, destCode) = mapDestReg(dest, current)
                        in
                            pushRegisters(rest,
                                destCode @ AllocateMemoryVariable{size=sizeVal, dest=destVal, saveRegs=[]} :: sizeCode @ code)
                        end

                    |   pushRegisters({instr=InitialiseMem{size, addr, init}, current, ...} :: rest, code) =
                        let
                            val (sizeVal, sizeCode) = mapSrcReg(size, current)
                            val (addrVal, addrCode) = mapSrcReg(addr, current)
                            val (initVal, initCode) = mapSrcReg(init, current)
                        in
                            pushRegisters(rest,
                                InitialiseMem{size=sizeVal, addr=addrVal, init=initVal} :: initCode @ addrCode @ sizeCode @ code)
                        end

                    |   pushRegisters({instr=InitialisationComplete, ...} :: rest, code) =
                            pushRegisters(rest, InitialisationComplete :: code)

                    |   pushRegisters({instr=JumpLoop{regArgs, stackArgs, checkInterrupt}, current, ...} :: rest, code) =
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
                                            val (sourceVal, sourceCode) = mapSource(source, current)
                                        in
                                            ((sourceVal, lReg) :: otherRegArgs, otherStackArgs, sourceCode @ otherCode)
                                        end
                                    |   ToStack(stackLoc, stackC as StackLoc{size, ...}) =>
                                        let
                                            val (sourceVal, sourceCode) = mapSource(source, current)
                                            val stackOff = !stackCount - stackLoc - size
                                        in
                                            (otherRegArgs, (sourceVal, stackOff, stackC) :: otherStackArgs, sourceCode @ otherCode)
                                        end
                                    |   Unset => (* Drop it.  It's never used. Probably a unit argument. *)
                                            (otherRegArgs, otherStackArgs, otherCode)
                                end
                            val (newRegArguments, newStackArgs, sourceCode) = getValues regArgs
                            fun loadStackArg((source, _, destC), (otherLoads, otherArgs)) =
                            let
                                val (sourceVal, sourceCode) = mapSource(source, current)
                                val (newOffset, newContainer) = mapContainerAndStack(destC, 0, current)
                            in
                                (sourceCode @ otherLoads, (sourceVal, newOffset, newContainer) :: otherArgs)
                            end
                
                            val (stackArgLoads, oldStackArgs) = List.foldr loadStackArg ([], []) stackArgs
                            val check = case checkInterrupt of NONE => NONE | SOME _ => SOME []
                        in
                            pushRegisters(rest,
                                JumpLoop{ regArgs=newRegArguments, stackArgs=oldStackArgs @ newStackArgs, checkInterrupt=check} ::
                                     sourceCode @ stackArgLoads @ code)
                        end

                    |   pushRegisters({instr=RaiseExceptionPacket{packet}, current, ...} :: rest, code) =
                        let
                            val (packetVal, packetCode) = mapSource(packet, current)
                            (* Should be last. *)
                            val _ = null rest orelse raise InternalError "pushRegisters:RaiseExceptionPacket"
                        in
                            (RaiseExceptionPacket{packet=packetVal} :: packetCode @ code)
                        end

                    |   pushRegisters({instr=ReserveContainer{size, container}, ...} :: rest, code) =
                        let
                            val newContainer = mapDestContainer(container, !stackCount)
                            val () = pushItemToStack(ContainerEntry{size=size, active=true, stackLoc=newContainer})
                        in
                            pushRegisters(rest, ReserveContainer{size=size, container=newContainer} :: code)
                        end

                    |   pushRegisters({instr=LoadContainerAddress{container, dest, ...}, current, ...} :: rest, code) =
                        let
                            val (newStackOffset, newContainer) = mapContainerAndStack(container, 0, current)
                            val (destVal, destCode) = mapDestReg(dest, current)
                        in
                            pushRegisters(rest, destCode @ LoadContainerAddress{container=newContainer, dest=destVal, stackOffset=newStackOffset} :: code)
                        end

                    |   pushRegisters({instr=IndexedCaseOperation{testReg, workReg}, current, ...} :: rest, code) =
                        let
                            (* Should be last. *)
                            val _ = null rest orelse raise InternalError "pushRegisters:IndexedCaseOperation"
                            val (srcVal, srcCode) = mapSrcReg(testReg, current)
                            val newWorkReg = mapWorkReg workReg
                        in
                            (* This is an unconditional branch. *)
                            (IndexedCaseOperation{testReg=srcVal, workReg=newWorkReg} :: srcCode @ code)
                        end

                    |   pushRegisters({instr=LockMutable{addr}, current, ...} :: rest, code) =
                        let
                            val (addrVal, addrCode) = mapSrcReg(addr, current)
                        in
                            pushRegisters(rest, LockMutable{addr=addrVal} :: addrCode @ code)
                        end
 
                    |   pushRegisters({instr=WordComparison{arg1, arg2, ccRef}, current, ...} :: rest, code) =
                        let
                            (* codeExtended expects the first argument to be in a register.
                               We may be able to do something clever if the second is actually in the
                               register but for the moment just force a load. *)
                            val (loadedOp1, op1Code) = mapAndLoad(arg1, current)
                            val (op2Val, op2Code) = mapSource(arg2, current)
                        in
                            pushRegisters(rest, WordComparison{arg1=loadedOp1, arg2=op2Val, ccRef=ccRef} :: op2Code @ op1Code @ code)
                        end

                    |   pushRegisters({instr=PushExceptionHandler{workReg}, ...} :: rest, code) =
                        let
                            val _ = null rest orelse raise InternalError "pushRegisters - PushExceptionHandler"
                            val newWorkReg = mapWorkReg workReg
                            (* Add a handler entry to the stack. *)
                            val () = pushItemToStack HandlerEntry
                        in
                            (PushExceptionHandler{workReg=newWorkReg} :: code)
                        end

                    |   pushRegisters({instr=PopExceptionHandler{workReg, ...}, ...} :: rest, code) =
                        let
                            val newWorkReg = mapWorkReg workReg
                            (* Appears at the end of the block whose exceptions are being handled.  Delete the
                               handler and anything above it. *)
                            (* Get the state after removing the handler. *)
                            fun popContext ([], _) = raise InternalError "pushRegisters - pop handler"
                            |   popContext (HandlerEntry :: tl, new) = (tl, new-2)
                            |   popContext (ContainerEntry{active=true, ...} :: _, _) = raise InternalError "pushRegisters - pop handler - container"
                            |   popContext (ContainerEntry{active=false, size, ...} :: tl, new) = popContext(tl, new-size)
                            |   popContext (NewEntry _ :: tl, new) = popContext(tl, new-1)
                            val (newStack, nnCount) = popContext(!stack, !stackCount)
                            val () = stack := newStack
                            val oldStackPtr = ! stackCount
                            val () = stackCount := nnCount
                            (* Reset the stack to just above the two words of the handler. *)
                            val resetCode =
                                if oldStackPtr <> nnCount+2
                                then [ResetStackPtr{numWords=oldStackPtr-nnCount-2}]
                                else []
                        in
                            pushRegisters(rest, PopExceptionHandler{workReg=newWorkReg} :: resetCode @ code)
                        end

                    |   pushRegisters({instr=BeginHandler{packetReg, workReg, ...}, current, ...} :: rest, code) =
                        let
                            (* Start of a handler.  The top active entry should be the handler. *)
                            val () =
                                case !stack of
                                    HandlerEntry :: tl => stack := tl
                                |   _ => raise InternalError "pushRegisters: BeginHandler"
                            val () = stackCount := !stackCount - 2
                            val newWorkReg = mapWorkReg workReg
                            val (pktReg, pktCode) = mapDestReg(packetReg, current)
                        in
                            pushRegisters(rest, pktCode @ BeginHandler{packetReg=pktReg, workReg=newWorkReg} :: code)
                        end

                    |   pushRegisters({instr=ReturnResultFromFunction{resultReg, numStackArgs}, current, ...} :: rest, code) =
                        let
                            val _ = null rest orelse raise InternalError "pushRegisters - ReturnResultFromFunction"
                            val (resultValue, loadResult) = mapSrcReg(resultReg, current)
                            val resetCode =
                                if !stackCount = 0 then [] else [ResetStackPtr{numWords= !stackCount}]
                        in
                            ReturnResultFromFunction{resultReg=resultValue, numStackArgs=numStackArgs} :: resetCode @ loadResult @ code
                        end

                    |   pushRegisters({instr=ArithmeticFunction{oper, resultReg, operand1, operand2, ccRef}, current, ...} :: rest, code) =
                        let
                            (* codeExtended expects the first argument to be in a register.
                               We may be able to do something clever if the second is actually in the
                               register but for the moment just force a load. *)
                            val (loadedOp1, op1Code) = mapAndLoad(operand1, current)
                            val (op2Val, op2Code) = mapSource(operand2, current)
                            val (destVal, destCode) = mapDestReg(resultReg, current)
                        in
                            pushRegisters(rest,
                                destCode @ ArithmeticFunction{oper=oper, resultReg=destVal, operand1=loadedOp1, operand2=op2Val, ccRef=ccRef} ::
                                    op2Code @ op1Code @ code)
                        end

                    |   pushRegisters({instr=TestTagBit{arg, ccRef}, current, ...} :: rest, code) =
                        let
                            val (sourceVal, sourceCode) = mapSource(arg, current)
                        in
                            pushRegisters(rest, TestTagBit{arg=sourceVal, ccRef=ccRef} :: sourceCode @ code)
                        end

                    |   pushRegisters({instr=PushValue{arg, container, ...}, current, ...} :: rest, code) =
                        let
                            val (sourceVal, sourceCode) = mapSource(arg, current)
                            (* This was a push from a previous pass.  Treat as a container of size 1. *)
                            val newContainer = mapDestContainer(container, !stackCount)
                            val () = pushItemToStack(ContainerEntry{size=1, active=true, stackLoc=newContainer})
                        in
                            pushRegisters(rest, PushValue{arg=sourceVal, container=newContainer} :: sourceCode @ code)
                        end
                    
                    |   pushRegisters({instr=CopyToCache _, ...} :: rest, code) =
                            (* This was added on a previous pass.  Discard it.  If we are going to cache this again we'll
                               add new CopyToCache instructions. *)
                            pushRegisters(rest, code)

                    |   pushRegisters({instr=ResetStackPtr{numWords}, ...} :: rest, code) =
                        (* This indicates that an old entry is no longer required.  We can't actually remove
                           it because we may have pushed something since.  Instead we mark it as inactive and remove
                           it when everything above is free. *)
                        let
                            fun removeItems(0, stack) = stack
                            |   removeItems(_, []) = raise InternalError "removeItems - stack empty"
                            |   removeItems(toRemove, (entry as ContainerEntry {active=false, ...}) :: tl) =
                                    entry :: removeItems(toRemove, tl)
                            |   removeItems(toRemove, ContainerEntry {size, active=true, stackLoc} :: tl) =
                                    if toRemove < size then raise InternalError "removeItems - container size"
                                    else ContainerEntry {size=size, active=false, stackLoc=stackLoc} :: removeItems(toRemove-size, tl)
                            |   removeItems(_, HandlerEntry :: _) = raise InternalError "removeItems - handler"
                            |   removeItems(toRemove, entry :: tl) = entry :: removeItems(toRemove, tl)
                
                            val () = stack := removeItems(numWords, !stack)
                            (* This has just marked the items as free.  They haven't actually been removed
                               from the stack yet. *)
                        in
                            pushRegisters(rest, code)
                        end

                    |   pushRegisters({instr=StoreToStack{source, container, field, ...}, current, ...} :: rest, code) =
                        let
                            val (loadedSource, sourceCode) = mapAndLoad(source, current)
                            (* We can't have a memory-memory store so we have to load the source if it's now on the stack. *)
                            val (newOffset, newContainer) = mapContainerAndStack(container, field, current)
                        in
                            pushRegisters(rest,
                                StoreToStack{source=loadedSource, container=newContainer, field=field, stackOffset=newOffset} ::
                                    sourceCode @ code)
                        end

                    |   pushRegisters({instr=TagValue{source, dest}, current, ...} :: rest, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg(source, current)
                            val (destVal, destCode) = mapDestReg(dest, current)
                        in
                            pushRegisters(rest, destCode @ TagValue{source=sourceVal, dest=destVal} :: sourceCode @ code)
                        end

                    |   pushRegisters({instr=UntagValue{source, dest, isSigned}, current, ...} :: rest, code) =
                        let
                            val (loadedSource, sourceCode) = mapAndLoad(source, current)
                            val (destVal, destCode) = mapDestReg(dest, current)
                        in
                            pushRegisters(rest,
                                destCode @ UntagValue{source=loadedSource, dest=destVal, isSigned=isSigned} :: sourceCode @ code)
                        end

                    |   pushRegisters({instr=LoadEffectiveAddress{base, offset, index, dest}, current, ...} :: rest, code) =
                        let
                            val (baseVal, baseCode) =
                                case base of
                                    NONE => (NONE, [])
                                |   SOME bReg =>
                                        let val (newBReg, regCode) = mapSrcReg(bReg, current) in (SOME newBReg, regCode) end
                            val (indexVal, indexCode) = mapIndex(index, current)
                            val (destVal, destCode) = mapDestReg(dest, current)
                        in
                            pushRegisters(rest,
                                destCode @ LoadEffectiveAddress{base=baseVal, offset=offset, index=indexVal, dest=destVal} :: indexCode @ baseCode @ code)
                        end

                    |   pushRegisters({instr=ShiftOperation{shift, resultReg, operand, shiftAmount, ccRef}, current, ...} :: rest, code) =
                        let
                            val (opVal, opCode) = mapSource(operand, current)
                            val (shiftVal, shiftCode) = mapSource(shiftAmount, current)
                            val (destVal, destCode) = mapDestReg(resultReg, current)
                        in
                            pushRegisters(rest,
                                destCode @ ShiftOperation{shift=shift, resultReg=destVal, operand=opVal, shiftAmount=shiftVal, ccRef=ccRef} ::
                                    shiftCode @ opCode @ code)
                        end

                    |   pushRegisters({instr=Multiplication{resultReg, operand1, operand2, ccRef}, current, ...} :: rest, code) =
                        let
                            val (op1Val, op1Code) = mapSource(operand1, current)
                            val (op2Val, op2Code) = mapSource(operand2, current)
                            val (destVal, destCode) = mapDestReg(resultReg, current)
                        in
                            pushRegisters(rest,
                                destCode @ Multiplication{resultReg=destVal, operand1=op1Val, operand2=op2Val, ccRef=ccRef} :: op2Code @ op1Code @ code)
                        end

                    |   pushRegisters({instr=Division{isSigned, dividend, divisor, quotient, remainder}, current, ...} :: rest, code) =
                        let
                            val (dividendVal, dividendCode) = mapSrcReg(dividend, current)
                            val (divisorVal, divisorCode) = mapSource(divisor, current)
                            val (quotVal, quotCode) = mapDestReg(quotient, current)
                            val (remVal, remCode) = mapDestReg(remainder, current)
                        in
                            pushRegisters(rest,
                                remCode @ quotCode @
                                    Division{isSigned=isSigned, dividend=dividendVal, divisor=divisorVal, quotient=quotVal, remainder=remVal} ::
                                        divisorCode @ dividendCode @ code)
                        end

                    |   pushRegisters({instr=AtomicExchangeAndAdd{base, source}, current, ...} :: rest, code) =
                        let
                            val (baseVal, baseCode) = mapSrcReg(base, current)
                            val (sourceVal, sourceCode) = mapSrcReg(source, current)
                            (* The "source" is also a result and must be in a register.  It's an untagged reg
                               so it shouldn't have been marked as to be pushed. *)
                            val _ = case sourceCode of [] => () | _ => raise InternalError "pushRegisters - AtomicExchangeAndAdd"
                        in
                            pushRegisters(rest, AtomicExchangeAndAdd{base=baseVal, source=sourceVal} :: baseCode @ code)
                        end

                    |   pushRegisters({instr=BoxValue{boxKind, source, dest, ...}, current, ...} :: rest, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg(source, current)
                            val (destVal, destCode) = mapDestReg(dest, current)
                        in
                            pushRegisters(rest,
                                destCode @ BoxValue{boxKind=boxKind, source=sourceVal, dest=destVal, saveRegs=[]} :: sourceCode @ code)
                        end

                    |   pushRegisters({instr=CompareByteVectors{vec1Addr, vec2Addr, length, ccRef}, current, ...} :: rest, code) =
                        let
                            val (vec1Val, vec1Code) = mapSrcReg(vec1Addr, current)
                            val (vec2Val, vec2Code) = mapSrcReg(vec2Addr, current)
                            val (lengthVal, lengthCode) = mapSrcReg(length, current)
                        in
                            pushRegisters(rest,
                                CompareByteVectors{vec1Addr=vec1Val, vec2Addr=vec2Val, length=lengthVal, ccRef=ccRef} ::
                                    lengthCode @ vec2Code @ vec1Code @ code)
                        end

                    |   pushRegisters({instr=BlockMove{srcAddr, destAddr, length, isByteMove}, current, ...} :: rest, code) =
                        let
                            val (srcVal, srcCode) = mapSrcReg(srcAddr, current)
                            val (destVal, destCode) = mapSrcReg(destAddr, current)
                            val (lengthVal, lengthCode) = mapSrcReg(length, current)
                        in
                            pushRegisters(rest,
                                BlockMove{srcAddr=srcVal, destAddr=destVal, length=lengthVal, isByteMove=isByteMove} ::
                                    lengthCode @ destCode @ srcCode @ code)
                        end

                    |   pushRegisters({instr=CompareFloatingPt{arg1, arg2, ccRef}, current, ...} :: rest, code) =
                        let
                            val (arg1Val, arg1Code) = mapSource(arg1, current)
                            val (arg2Val, arg2Code) = mapSource(arg2, current)
                        in
                            pushRegisters(rest, CompareFloatingPt{arg1=arg1Val, arg2=arg2Val, ccRef=ccRef} :: arg2Code @ arg1Code @ code)
                        end

                    |   pushRegisters({instr=X87FPGetCondition{dest, ccRef}, current, ...} :: rest, code) =
                        let
                            val (destVal, destCode) = mapDestReg(dest, current)
                        in
                            pushRegisters(rest, destCode @ X87FPGetCondition{dest=destVal, ccRef=ccRef} :: code)
                        end

                    |   pushRegisters({instr=X87FPArith{opc, resultReg, arg1, arg2}, current, ...} :: rest, code) =
                        let
                            val (arg1Val, arg1Code) = mapSource(arg1, current)
                            val (arg2Val, arg2Code) = mapSource(arg2, current)
                            val (destVal, destCode) = mapDestReg(resultReg, current)
                        in
                            pushRegisters(rest,
                                destCode @ X87FPArith{opc=opc, resultReg=destVal, arg1=arg1Val, arg2=arg2Val} ::
                                    arg2Code @ arg1Code @ code)
                        end
    
                    |   pushRegisters({instr=X87FPUnaryOps{fpOp, dest, source}, current, ...} :: rest, code) =
                        let
                            val (sourceVal, sourceCode) = mapSource(source, current)
                            val (destVal, destCode) = mapDestReg(dest, current)
                        in
                            pushRegisters(rest,
                                destCode @ X87FPUnaryOps{fpOp=fpOp, dest=destVal, source=sourceVal} :: sourceCode @ code)
                        end

                    |   pushRegisters({instr=FloatFixedInt{dest, source}, current, ...} :: rest, code) =
                        let
                            val (sourceVal, sourceCode) = mapSource(source, current)
                            val (destVal, destCode) = mapDestReg(dest, current)
                        in
                            pushRegisters(rest, destCode @ FloatFixedInt{dest=destVal, source=sourceVal} :: sourceCode @ code)
                        end
    
                    |   pushRegisters({instr=SSE2FPArith{opc, resultReg, arg1, arg2}, current, ...} :: rest, code) =
                        let
                            val (arg1Val, arg1Code) = mapSource(arg1, current)
                            val (arg2Val, arg2Code) = mapSource(arg2, current)
                            val (destVal, destCode) = mapDestReg(resultReg, current)
                        in
                            pushRegisters(rest,
                                destCode @ SSE2FPArith{opc=opc, resultReg=destVal, arg1=arg1Val, arg2=arg2Val} ::
                                    arg2Code @ arg1Code @ code)
                        end
        

                    (* For the moment always clear the cache. *)
                    val () = clearCache()
                    (* We can't adjust the stack at this point. *)
                    val codeResult = pushRegisters(block, [])
                    val outputCount = ! stackCount
                    (* Remember these.  They will be the inputs for all the successors. *)
                    val stateResult = { stackCount= outputCount, stack= !stack }
                    
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
            val _ = processBlocks(0, {stack=[], stackCount=0})
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
        type x86ICode = x86ICode
        and preg = preg
        and intSet = intSet
        and extendedBasicBlock = extendedBasicBlock
        and basicBlock = basicBlock
        and regProperty = regProperty
    end
end;
