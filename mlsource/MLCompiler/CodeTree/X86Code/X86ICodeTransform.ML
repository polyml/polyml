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

functor X86ICodeTransform(
    structure ICODE: ICodeSig
    structure DEBUG: DEBUGSIG
    structure IDENTIFY: X86IDENTIFYREFSSIG
    structure CONFLICTSETS: X86GETCONFLICTSETSIG
    structure PUSHREGISTERS: X86PUSHREGISTERSIG
    structure PRETTY: PRETTYSIG
    structure STRONGLY:
        sig
            type node
            val stronglyConnectedComponents: node list -> node list list
        end
        where type node = {src: ICODE.reg ICODE.argument, dst: ICODE.destinations}
    structure INTSET: INTSETSIG
    sharing ICODE.Sharing = IDENTIFY.Sharing = CONFLICTSETS.Sharing = PUSHREGISTERS.Sharing = INTSET
) : X86ICODETRANSFORMSIG
=
struct
    open ICODE
    open Address
    open IDENTIFY
    open CONFLICTSETS
    open PUSHREGISTERS
    open INTSET
    
    exception InternalError = Misc.InternalError

    (* tag a short constant *)
    fun tag c = 2 * c + 1

    val raxAsArg = (GenReg eax)
    and rcxAsArg = (GenReg ecx)
    
    val generalRegisters =
        List.map GenReg
        (if isX64
        then [r14, r13, r12, r11, r10, r9, r8, edi, esi, edx, ecx, ebx, eax]
        else [edi, esi, edx, ecx, ebx, eax])
    
    val floatingPtRegisters =
        case fpMode of
            FPModeSSE2 => List.map XMMReg [xmm6, xmm5, xmm4, xmm3, xmm2, xmm1, xmm0]
        |   FPModeX87 => List.map FPReg [fp0, fp1, fp2, fp3, fp4, fp5, fp6, fp7]
    
    val dummyC = StackLoc{size=1, rno=0} (* Needed in various places. *)

    (* Generally we have an offset in words and no index register. *)
    fun wordOffsetAddress(offset, baseReg) =
        MemoryLocation{offset=offset*wordSize, base=(GenReg baseReg), index=NoMemIndex}
    exception RegisterOverflow of intSet

    fun codeAbstractToConcrete(identifiedBlocks: extendedBasicBlock vector, regStates: conflictState vector): reg basicBlock vector =
    let
        val maxPRegs = Vector.length regStates

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

        (* Sources and destinations.  These indicate the registers that are
           the sources and destinations of the indexing register and are used
           as hints.  If a register has been allocated for a source or destination
           we may be able to reuse it.  *)
        val sourceRegs = Array.array(maxPRegs, []: int list)
        and destinationRegs = Array.array(maxPRegs, []: int list)

        fun addRealHint(r, reg) =
            case Array.sub(realHints, r) of
                NONE => Array.update(realHints, r, SOME reg)
            |   SOME _ => ()
        
        fun addSourceAndDestinationHint{src, dst} =
        let
            val {conflicts, ...} = Vector.sub(regStates, src)
        in
            (* If they conflict we can't add them. *)
            if member(dst, conflicts)
            then ()
            else
            let
                val currentDests = Array.sub(destinationRegs, src)
                val currentSources = Array.sub(sourceRegs, dst)
            in
                if List.exists(fn i => i=dst) currentDests
                then ()
                else Array.update(destinationRegs, src, dst :: currentDests);
                if List.exists(fn i => i=src) currentSources
                then ()
                else Array.update(sourceRegs, dst, src :: currentSources)
            end
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
                    val {conflicts, realConflicts, ...} = Vector.sub(regStates, r)
                    (* Find the registers we've already allocated that may conflict. *)
                    val conflictingRegs =
                        List.mapPartial(fn i => Array.sub(allocatedRegs, i)) (setToList conflicts) @
                            realConflicts

                    fun isFree aReg = not (List.exists(fn i => i=aReg) conflictingRegs)

                    fun tryAReg NONE = NONE
                    |   tryAReg (somePref as SOME prefReg) =
                            if isFree prefReg
                            then (Array.update(allocatedRegs, r, somePref); somePref)
                            else NONE
                    
                    fun findAReg [] = raise RegisterOverflow conflicts
                    |   findAReg (reg::regs) =
                            if isFree reg then (Array.update(allocatedRegs, r, SOME reg); reg)
                            else findAReg regs


                    (* Search the sources and destinations to see if a register has
                       already been allocated or there is a hint. *)
                    fun findAFriend([], [], _) = NONE

                    |   findAFriend(aDest :: otherDests, sources, alreadySeen) =
                        let
                            val possReg =
                                case Array.sub(allocatedRegs, aDest) of
                                    v as SOME _ => tryAReg v
                                |   NONE => tryAReg(Array.sub(realHints, aDest))
                        in
                            case possReg of
                                reg as SOME _ => reg
                            |   NONE =>
                                let
                                    (* Add the destinations of the destinations to the list
                                       if they don't conflict and haven't been seen. *)
                                    fun newFriend f =
                                        not(List.exists (fn n => n=f) alreadySeen) andalso not(member(f, conflicts))
                                    val fOfF = List.filter newFriend (Array.sub(destinationRegs, aDest))
                                in
                                    findAFriend(otherDests @ fOfF, sources, aDest :: alreadySeen)
                                end
                        end

                    |   findAFriend([], aSrc :: otherSrcs, alreadySeen) =
                        let
                            val possReg =
                                case Array.sub(allocatedRegs, aSrc) of
                                    v as SOME _ => tryAReg v
                                |   NONE => tryAReg(Array.sub(realHints, aSrc))
                        in
                            case possReg of
                                reg as SOME _ => reg
                            |   NONE =>
                                let
                                    (* Add the sources of the sources to the list
                                       if they don't conflict and haven't been seen. *)
                                    fun newFriend f =
                                        not(List.exists (fn n => n=f) alreadySeen) andalso not(member(f, conflicts))
                                    val fOfF = List.filter newFriend (Array.sub(sourceRegs, aSrc))
                                in
                                    findAFriend([], otherSrcs @ fOfF, aSrc :: alreadySeen)
                                end
                        end

                    (* See if there is a friend that has a register already or a
                       hint.  Friends are registers that don't conflict and can
                       possibly avoid an extra move. *) 
(*                    fun findAFriend([], _) = NONE
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
                        end*)
                in
                    case tryAReg pref of
                        SOME r => r
                    |   NONE =>
                        (
                            case tryAReg (Array.sub(realHints, r)) of
                                SOME r => r
                            |   NONE =>
                                (
                                    case findAFriend(Array.sub(destinationRegs, r), Array.sub(sourceRegs, r), []) of
                                        SOME r => r
                                        (* Look through the registers to find one that's free. *)
                                    |   NONE => findAReg regSet
                                )
                        )
                end

        fun findGeneralReg r = findRegister(r, NONE, generalRegisters)
        and findFloatReg r = findRegister(r, NONE, floatingPtRegisters)

        fun codeExtArgument(RegisterArgument(PReg r), regSet) = RegisterArgument(findRegister(r, NONE, regSet))
        |   codeExtArgument(AddressConstant m, _) = AddressConstant m
        |   codeExtArgument(IntegerConstant i, _) = IntegerConstant i
        |   codeExtArgument(MemoryLocation{base=PReg bReg, offset, index}, _) =
                MemoryLocation{base=findGeneralReg bReg, offset=offset, index=codeExtIndex index}
        |   codeExtArgument(StackLocation{wordOffset, ...}, _) =
                MemoryLocation{base=GenReg esp, offset=wordOffset*wordSize, index=NoMemIndex}
        
        and codeExtArgGeneral arg = codeExtArgument(arg, generalRegisters)
        and codeExtArgFloat arg = codeExtArgument(arg, floatingPtRegisters)

        and codeExtIndex NoMemIndex = NoMemIndex
        |   codeExtIndex(MemIndex1(PReg r)) = MemIndex1(findGeneralReg r)
        |   codeExtIndex(MemIndex2(PReg r)) = MemIndex2(findGeneralReg r)
        |   codeExtIndex(MemIndex4(PReg r)) = MemIndex4(findGeneralReg r)
        |   codeExtIndex(MemIndex8(PReg r)) = MemIndex8(findGeneralReg r)

        fun moveRR{src, dst} = LoadArgument{source=RegisterArgument src, dest=dst, kind=MoveWord}

        fun moveIfNecessary{src, dst} =
            if src = dst then [] else [moveRR{src=src, dst=dst}]
        
        (* This is a general function for moving values into registers or to the stack
           where it is possible that the source values might also be in use as destinations.
           The stack is used for destinations only for tail recursive calls. *)
        fun moveMultipleValues(moves, reservedRegs) =
        let
            fun getWorkRegister(reserved, rest) =
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
            in
                case filterList(rest, filteredInUse) of
                    workReg :: _ => SOME workReg
                |   [] => NONE
            end

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

                    |   moveEachValue ([{dst=StackDest addr, src as StackLocation{wordOffset, ...}}] :: rest, reserved) =
                            (* Copy a stack location - needs a load and store unless the address is the same. *)
                            if addr = wordOffset
                            then moveEachValue(rest, reserved)
                            else
                            let
                                val loadAndStoreCode =
                                    case getWorkRegister(reserved, rest) of
                                        SOME workReg =>
                                            [LoadArgument{source=src, dest=workReg, kind=MoveWord},
                                             StoreArgument{
                                                    source=RegisterArgument workReg, base=GenReg esp, index=NoMemIndex,
                                                    offset = addr*wordSize, kind=MoveWord}]
                                    |   NONE =>
                                        (* So far it seems we always find a register this way if we need one.
                                           Just in case, fall back to pushing a register, using it and popping it
                                           again. *)
                                        let
                                            val workReg = GenReg eax
                                            val workRegAsArg = RegisterArgument workReg
                                            val destAddr = addr+1
                                            val newSrc =
                                                case src of
                                                    StackLocation{wordOffset, container, field} =>
                                                        StackLocation{wordOffset=wordOffset+1, container=container, field=field+1}
                                                |   src => src
                                        in
                                            [
                                            PushValue{arg=workRegAsArg, container=dummyC},
                                            LoadArgument{source=newSrc, dest=workReg, kind=MoveWord},
                                            StoreArgument{
                                                source=RegisterArgument workReg, base=GenReg esp, index=NoMemIndex,
                                                offset = destAddr*wordSize, kind=MoveWord},
                                            LoadArgument{source=StackLocation{wordOffset=0, container=dummyC, field=0}, dest=workReg, kind=MoveWord},
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
                        (* Cycle with register arguments - swap the registers. *)
                        let
                            (* We need to exchange some of the arguments.  Doing an exchange here will
                               set the destination with the correct source.  However we have to process
                               every subsequent entry with the swapped registers.  That may well mean that
                               one of those entries becomes trivial.  Using XCHG means that we can move
                               N registers in N-1 exchanges.
                               We also need to rerun stronglyConnectedComponents on at least the rest of
                               this cycle.  It's easiest to flatten the rest and do everything. *)
                            val flattened = List.foldl(fn (a, b) => a @ b) [] (cycle :: rest)
                            fun swapRegs r = if r = regA then regB else if r = regB then regA else r
                            fun swapSources{src=RegisterArgument r, dst} =
                                    {src=RegisterArgument(swapRegs r), dst=dst}
                            |   swapSources{src=MemoryLocation{base, index, offset}, dst} =
                                let
                                    val newIndex =
                                        case index of
                                            NoMemIndex => NoMemIndex
                                        |   MemIndex1 r => MemIndex1(swapRegs r)
                                        |   MemIndex2 r => MemIndex2(swapRegs r)
                                        |   MemIndex4 r => MemIndex4(swapRegs r)
                                        |   MemIndex8 r => MemIndex8(swapRegs r)
                                in
                                    {src=MemoryLocation{base=swapRegs base, index=newIndex, offset=offset}, dst=dst}
                                end
                            |   swapSources sd = sd
                        in
                            ExchangeRegisters{regX=regA, regY=regB} ::
                                moveValues(List.map swapSources flattened, regA :: reserved)
                        end

                    |   moveEachValue(cycle as ({src=originalSrc, ...} :: _ :: _) :: _, reserved) =
                        let
                            (* Anything else.  Break the cycle by getting a work register and
                               loading the source into that.  This is requires more instructions
                               than using an exchange but we don't want to use XCHG between
                               a register and memory.  That generates a memory lock and
                               we don't want the overhead. *)
                            val flattened = List.foldl(fn (a, b) => a @ b) [] cycle
                            val workReg =
                                case getWorkRegister(reserved, cycle) of
                                    SOME workReg => workReg
                                |   _ => raise RegisterOverflow emptySet  (* Force an extra register spill here. *)
                            (* Replace any reference to the source by the new register. *)
                            val replace = RegisterArgument workReg
                            fun swapSources{src, dst} =
                            let
                                val newSource =
                                    case (src, originalSrc) of
                                        (RegisterArgument r1, RegisterArgument r2) =>
                                            if r1=r2 then replace else src
                                    |   (StackLocation{wordOffset=offset1, ...},
                                         StackLocation{wordOffset=offset2, ...}) =>
                                            if offset1 = offset2 then replace else src
                                    |   (MemoryLocation _, _) => raise InternalError "swapSources: memory loc"
                                    |   (_, MemoryLocation _) => raise InternalError "swapSources: memory loc"
                                    |   _ => src
                            in
                                {src=newSource, dst=dst}
                            end
                        in
                            LoadArgument{source=originalSrc, dest=workReg, kind=MoveWord} ::
                                (* Process the whole input including this instruction. Don't include
                                   the work reg as reserved.  It's present as a source now so won't
                                   be reused until we've copied it to the real destination. *)
                                moveValues(List.map swapSources flattened, reserved)
                        end

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
                            StackLocation{wordOffset, container, field} =>
                                StackLocation{wordOffset=wordOffset+1, container=container, field=field+1}
                        |   other => other
                in
                    {dst=newDest, src=newSrc} :: renumberArgs args
                end
        in
            PushValue{arg=argM1, container=dummyC} :: codeTailCall(callKind, renumberArgs arguments, stackAdjust+1)
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
            [TailRecursiveCall{regArgs=[], stackArgs=[], callKind=callKind, stackAdjust=0, stackOffset=0}]
        end

        fun codeExtended([], _) = []

        |   codeExtended({instr=LoadArgument{source=RegisterArgument(PReg sreg), dest=PReg dreg, kind}, ...} :: rest, context) =
            (* Register to register move.  Try to use the same register for the source as the destination
               to eliminate the instruction. *)
            let
                val () = addSourceAndDestinationHint {src=sreg, dst=dreg}
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

        |   codeExtended({instr=LoadArgument{source, dest=PReg pr, kind}, ...} :: rest, context) =
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

        |   codeExtended({instr=
                StoreArgument{
                    source as RegisterArgument(PReg sReg), base=PReg bReg, offset, index, kind=MoveByte, ... }, ...} :: rest, context) =
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

        |   codeExtended({instr=StoreArgument{ source, base=PReg bReg, offset, index, kind }, ...} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val regSet =
                    case kind of MoveFloat => floatingPtRegisters | MoveDouble => floatingPtRegisters | _ => generalRegisters
            in
                StoreArgument{
                    source=codeExtArgument(source, regSet), base=findGeneralReg bReg, offset=offset, index=codeExtIndex index, kind=kind}
                        :: code
            end

        |   codeExtended({instr=LoadMemReg { offset, dest=PReg pr}, ...} :: rest, context) =
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

        |   codeExtended({instr=ExchangeRegisters _, ...} :: _, _) =
                (* This is only generated in this pass. *)
                raise InternalError "codeExtended - ExchangeRegisters"

        |   codeExtended({instr=BeginFunction{regArgs, ...}, ...} :: rest, context) =
            let
                val () = List.app (fn (PReg pr, reg) => addRealHint(pr, reg)) regArgs
                val code = codeExtended(rest, context)

                (* Look up the register we need to move the argument into.
                   If there's no register then we don't use this argument and
                   can drop it. *)
                fun mkPair(PReg pr, rr) =
                    case Array.sub(allocatedRegs, pr) of
                        NONE => NONE
                    |   SOME regResult => SOME{src=rr,dst=regResult}
                val regPairs = List.mapPartial mkPair regArgs
            in
                moveMultipleRegisters regPairs @ code
            end

        |   codeExtended({instr=TailRecursiveCall{callKind, regArgs, stackArgs, stackAdjust, stackOffset}, ...} :: rest, context) =
            let
                val () = List.app (fn (RegisterArgument(PReg pr), reg) => addRealHint(pr, reg) | _ => ()) regArgs
                val code = codeExtended(rest, context)
                (* We must leave stack entries as stack entries for the moment. *)
                fun codeArg(StackLocation stack) = StackLocation stack
                |   codeArg arg = codeExtArgGeneral arg

                val extStackArgs = map (fn {stack, src} => {dst=StackDest(stack+stackOffset), src=codeArg src}) stackArgs
                val extRegArgs = map (fn (a, r) => {src=codeArg a, dst=RegDest r}) regArgs
                val codeTail =
                    codeTailCall(callKind, extStackArgs @ extRegArgs, stackAdjust+stackOffset)
                        handle RegisterOverflow _ =>
                        let
                            (* We've not been able to find enough free registers and have to push something.
                               Find the registers that are needed for the arguments.  This duplicates code in
                               identifyReferences but means we don't need to pass the active set everywhere. *)
                            fun getSourceRegs(RegisterArgument(PReg rarg)) = [rarg]
                            |   getSourceRegs(MemoryLocation { base=PReg base, index=NoMemIndex, ...}) = [base]
                            |   getSourceRegs(MemoryLocation { base=PReg base, index=MemIndex1(PReg index), ...}) = [base, index]
                            |   getSourceRegs(MemoryLocation { base=PReg base, index=MemIndex2(PReg index), ...}) = [base, index]
                            |   getSourceRegs(MemoryLocation { base=PReg base, index=MemIndex4(PReg index), ...}) = [base, index]
                            |   getSourceRegs(MemoryLocation { base=PReg base, index=MemIndex8(PReg index), ...}) = [base, index]
                            |   getSourceRegs _ = []

                            val activeFromRegs =
                                List.foldl(fn((arg, _), srcs) => getSourceRegs arg @ srcs) [] regArgs
                            val active =
                                List.foldl(fn ({src, ...}, srcs) => getSourceRegs src @ srcs) activeFromRegs stackArgs
                        in
                            raise RegisterOverflow(listToSet active)
                        end
            in
                codeTail @ code
            end

        |   codeExtended({instr=FunctionCall{callKind, regArgs, stackArgs, dest=PReg dReg}, ...} :: rest, context) =
            let
                val () = List.app (fn (RegisterArgument(PReg pr), reg) => addRealHint(pr, reg) | _ => ()) regArgs
                val () = addRealHint(dReg, GenReg eax)
                val code = codeExtended(rest, context)
                val destReg = findRegister(dReg, SOME(GenReg eax), generalRegisters)
                
                fun pushStackArgs ([], _) = []
                |   pushStackArgs (arg ::args, argNum) =
                    let
                        (* Have to adjust the offsets of stack arguments. *)
                        val adjusted =
                            case arg of
                                StackLocation {wordOffset, container, field} =>
                                    StackLocation{wordOffset=wordOffset+argNum, container=container, field=field+argNum}
                            |   arg => arg
                    in
                        PushValue {arg=codeExtArgGeneral adjusted, container=dummyC} :: pushStackArgs(args, argNum+1)
                    end
                val pushedArgs = pushStackArgs(stackArgs, 0)
                (* We have to adjust any stack offset to account for the arguments we've pushed. *)
                val numStackArgs = List.length stackArgs
                
                (* We don't currently allow the arguments to be memory locations and instead
                   force them into registers.  That may be simpler especially if we can get the
                   values directly into the required register. *)
                fun getRegArgs(RegisterArgument(PReg pr), reg) =
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
                |   getConstArgs(StackLocation { wordOffset, container, field}, reg) =
                        SOME(LoadArgument{source=StackLocation{wordOffset=wordOffset+numStackArgs, container=container, field=field+numStackArgs},
                                          dest=reg, kind=MoveWord})
                |   getConstArgs(RegisterArgument _, _) = NONE
                |   getConstArgs(MemoryLocation _, _) = NONE
                val loadConstArgs = List.mapPartial getConstArgs regArgs
            in
                pushedArgs @ loadRegArgs @ loadConstArgs @
                    (FunctionCall{regArgs=[], stackArgs=[], dest=raxAsArg, callKind=callKind} ::
                        moveIfNecessary{dst=destReg, src=GenReg eax}) @ code
            end

        |   codeExtended({instr=AllocateMemoryOperation{ size, flags, dest=PReg dReg, saveRegs}, ...} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val preserve = List.map(fn (PReg r) => findGeneralReg r) saveRegs
            in
                AllocateMemoryOperation{ size=size, flags=flags, dest=findGeneralReg dReg, saveRegs=preserve} :: code
            end

        |   codeExtended({instr=AllocateMemoryVariable{size=PReg sReg, dest=PReg dReg, saveRegs}, ...} :: rest, context) =
            let
                (* Simple case - no initialiser. *)
                val code = codeExtended(rest, context)
                val preserve = List.map(fn (PReg r) => findGeneralReg r) saveRegs
                val destReg = findGeneralReg dReg
            in
                AllocateMemoryVariable{size=findGeneralReg sReg, dest=destReg, saveRegs=preserve} :: code
            end

        |   codeExtended({instr=InitialiseMem{size=PReg sReg, addr=PReg aReg, init=PReg iReg}, ...} :: rest, context) =
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

        |   codeExtended({instr=InitialisationComplete, ...} :: rest, context) =
                InitialisationComplete :: codeExtended(rest, context)

        |   codeExtended({instr=JumpLoop{regArgs, stackArgs, checkInterrupt}, ...} :: rest, context) =
            let
                (* TODO: Make the sources and destinations "friends". *)
                val code = codeExtended(rest, context)
                (* We must leave stack entries as stack entries for the moment as with TailCall. *)
                fun codeArg(StackLocation stack) = StackLocation stack
                |   codeArg arg = codeExtArgGeneral arg
                val extStackArgs = map (fn (src, stack, _) => {dst=StackDest stack, src=codeArg src}) stackArgs
                val extRegArgs = map (fn (a, PReg r) => {src=codeArg a, dst=RegDest(findGeneralReg r)}) regArgs
                val check = Option.map(List.map(fn (PReg r) => findGeneralReg r)) checkInterrupt
            in
                moveMultipleValues(extStackArgs @ extRegArgs, generalRegisters) @
                    (JumpLoop{regArgs=[], stackArgs=[], checkInterrupt=check} :: code)
            end

        |   codeExtended({instr=RaiseExceptionPacket{ packet=RegisterArgument(PReg preg) }, ...} :: rest, context) =
            let
                val () = addRealHint(preg, GenReg eax)
                val code = codeExtended(rest, context)
                (* The argument must be put into rax. *)
                val argReg = findRegister(preg, SOME(GenReg eax), generalRegisters)
            in
                moveIfNecessary{src=argReg, dst=raxAsArg} @
                    RaiseExceptionPacket {packet=RegisterArgument raxAsArg } :: code
            end

        |   codeExtended({instr=RaiseExceptionPacket{ packet }, ...} :: rest, context) =
            let
                (* Because we're exiting we don't have to worry about conflicts - just load it. *)
                val code = codeExtended(rest, context)
            in
                LoadArgument{source=codeExtArgument(packet, generalRegisters), dest=raxAsArg, kind=MoveWord} ::
                    RaiseExceptionPacket {packet=RegisterArgument raxAsArg } :: code
            end

        |   codeExtended({instr=ReserveContainer{size, ...}, ...} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                (* The memory must be cleared in case we have a GC. *)
                val pushAll = List.tabulate(size, fn _ => PushValue{arg=IntegerConstant(tag 0), container=dummyC})
            in
                pushAll @ code
            end

        |   codeExtended({instr=LoadContainerAddress{stackOffset, dest=PReg aReg, ...}, ...} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val addrReg = findRegister(aReg, NONE, generalRegisters)
                val loadAddr =
                    if stackOffset = 0
                    then LoadArgument{source=RegisterArgument(GenReg esp), dest=addrReg, kind=MoveWord}
                    else LoadEffectiveAddress { base=SOME(GenReg esp), offset=stackOffset*wordSize, index=NoMemIndex, dest=addrReg }
            in
                loadAddr :: code
            end

        |   codeExtended({instr=IndexedCaseOperation{testReg=PReg tReg, workReg=PReg wReg}, ...} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val testReg = findRegister(tReg, NONE, generalRegisters)
                val workReg = findRegister(wReg, NONE, generalRegisters)
                val _ = testReg <> workReg orelse raise InternalError "IndexedCaseOperation - same registers"
            in
                IndexedCaseOperation{ testReg=testReg, workReg=workReg} :: code
            end

        |   codeExtended({instr=LockMutable{addr=PReg pr}, ...} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val argReg = findRegister(pr, NONE, generalRegisters)
            in
                LockMutable{addr=argReg} :: code
            end

        |   codeExtended({instr=WordComparison{ arg1 as RegisterArgument _, arg2, ccRef }, ...} :: rest, context) =
            let
                val code = codeExtended(rest, context)
            in
                WordComparison{ arg1=codeExtArgGeneral arg1, arg2=codeExtArgGeneral arg2, ccRef=ccRef } :: code
            end

        |   codeExtended({instr=WordComparison _, ...} :: _, _) = raise InternalError "codeExtended - WordComparison"

            (* Set up an exception handler. *)
        |   codeExtended({instr=PushExceptionHandler{workReg=PReg hReg}, ...} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val handleReg = findGeneralReg hReg
            in
                PushExceptionHandler{workReg=handleReg} :: code
            end

            (* Pop an exception handler at the end of a handled section.  Executed if no exception has been raised.
               This removes items from the stack. *)
        |   codeExtended({instr=PopExceptionHandler{workReg=PReg wReg, ...}, ...} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val realWork = findGeneralReg wReg
            in
                PopExceptionHandler{workReg=realWork} :: code
            end

            (* Start of a handler.  Sets the address associated with PushExceptionHandler and
               provides a register for the packet.  There is a work register but we could
               use any register other than rax since we will have pushed anything we need. *) 
        |   codeExtended({instr=BeginHandler{packetReg=PReg pReg, workReg=_}, ...} :: rest, context) =
            let
                (* The exception packet is in rax. *)
                val () = addRealHint(pReg, GenReg eax)
                val code = codeExtended(rest, context)
                val realPktReg = findRegister(pReg, SOME(GenReg eax), generalRegisters)
            in
                BeginHandler{workReg=GenReg ebx, packetReg=GenReg eax} ::
                    (moveIfNecessary{src=GenReg eax, dst=realPktReg } @ code)
            end

        |   codeExtended({instr=ReturnResultFromFunction { resultReg=PReg resReg, numStackArgs }, ...} :: rest, context) =
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

        |   codeExtended({instr=ArithmeticFunction{oper=SUB, resultReg=PReg resReg, operand1=RegisterArgument(PReg op1Reg),
                                            operand2, ccRef}, ...} :: rest, context) =
            (* Subtraction - this is special because it can only be done one way round.  The first argument must
               be in a register. *)
            let
                val () = addSourceAndDestinationHint {dst=resReg, src=op1Reg}
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

        |   codeExtended({instr=ArithmeticFunction{oper=SUB, ...}, ...} :: _, _) = raise InternalError "codeExtended - ArithmeticFunction"

        |   codeExtended({instr=ArithmeticFunction{oper, resultReg=PReg resReg, operand1=RegisterArgument(PReg op1Reg),
                                            operand2=RegisterArgument(PReg op2Reg), ccRef}, ...} :: rest, context) =
            (* Arithmetic operation with both arguments as registers.  These operations are all symmetric so
               we can try to put either argument into the result reg and then do the operation on the other arg. *)
            let
                val () = addSourceAndDestinationHint {dst=resReg, src=op1Reg}
                val () = addSourceAndDestinationHint {dst=resReg, src=op2Reg}
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

        |   codeExtended({instr=ArithmeticFunction{oper, resultReg=PReg resReg, operand1=RegisterArgument(PReg op1Reg),
                                            operand2, ccRef}, ...} :: rest, context) =
            (* Arithmetic operation with the first argument in a register and the second a constant or memory location. *)
            let
                val () = addSourceAndDestinationHint{dst=resReg, src=op1Reg}
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

        |   codeExtended({instr=ArithmeticFunction{oper, resultReg=PReg resReg, operand1,
                                            operand2=RegisterArgument(PReg op2Reg), ccRef}, ...} :: rest, context) =
            (* Arithemtic operation with the second argument in a register and the first a constant or memory location. *)
            let
                val () = addSourceAndDestinationHint{dst=resReg, src=op2Reg}
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                val realOp2Reg = findRegister(op2Reg, SOME realDestReg, generalRegisters)
                val op1Arg = codeExtArgGeneral operand1
            in
                moveIfNecessary{src=realOp2Reg, dst=realDestReg} @
                    (ArithmeticFunction{oper=oper, resultReg=realDestReg, operand1=RegisterArgument realDestReg, operand2=op1Arg, ccRef=ccRef} :: code)
            end

        |   codeExtended({instr=ArithmeticFunction _, ...} :: _, _) =
                raise InternalError "codeExtended - ArithmeticFunction"

        |   codeExtended({instr=TestTagBit{arg, ccRef}, ...} :: rest, context) =
            let
                val code = codeExtended(rest, context)
            in
                TestTagBit{ arg=codeExtArgGeneral arg, ccRef=ccRef } :: code
            end

        |   codeExtended({instr=PushValue {arg, container}, ...} :: rest, context) =
            let
                val code = codeExtended(rest, context)
            in
                PushValue {arg=codeExtArgGeneral arg, container=container} :: code
            end

        |   codeExtended({instr=ResetStackPtr {numWords}, ...} :: rest, context) =
            (* This is needed to remove containers on the stack. *)
                ResetStackPtr{numWords=numWords} :: codeExtended(rest, context)

        |   codeExtended({instr=StoreToStack{ source, stackOffset, ... }, ...} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val codeSource = codeExtArgument(source, generalRegisters)
            in
                StoreArgument{
                    source=codeSource, base=GenReg esp, offset=stackOffset*wordSize, index=NoMemIndex, kind=MoveWord}
                        :: code
            end

        |   codeExtended({instr=TagValue{source=PReg srcReg, dest=PReg dReg}, ...} :: rest, context) =
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

        |   codeExtended({instr=UntagValue{source=RegisterArgument(PReg sReg), dest=PReg dReg, isSigned}, ...} :: rest, context) =
            (* Always generates register argument at the moment.  TODO: This should really just take a single register arg. *)
            let
                val () = addSourceAndDestinationHint{src=sReg, dst=dReg}
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

        |   codeExtended({instr=UntagValue _, ...} :: _, _) =
                raise InternalError "UntagValue"

        |   codeExtended({instr=LoadEffectiveAddress{base=SOME base, offset=0, index=NoMemIndex, dest}, active, current} :: rest, context) =
                (* This should be handled at the higher level. *)
                codeExtended({instr=LoadArgument{source=RegisterArgument base, dest=dest, kind=MoveWord}, active=active, current=current} :: rest, context)

        |   codeExtended({instr=LoadEffectiveAddress{base, offset, index, dest=PReg dReg}, ...} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val destReg = findGeneralReg dReg
                val bReg = case base of SOME(PReg br) => SOME(findGeneralReg br) | NONE => NONE
                val iReg = codeExtIndex index
            in
                LoadEffectiveAddress{base=bReg, offset=offset, index=iReg, dest=destReg} :: code
            end

        |   codeExtended({instr=ShiftOperation{shift, resultReg=PReg resReg, operand=RegisterArgument(PReg operReg), shiftAmount=IntegerConstant i, ccRef}, ...} :: rest, context) =
            let
                val () = addSourceAndDestinationHint{dst=resReg, src=operReg}
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                val realOpReg = findRegister(operReg, SOME realDestReg, generalRegisters)
            in
                 moveIfNecessary{src=realOpReg, dst=realDestReg} @
                    (ShiftOperation{shift=shift, resultReg=realDestReg, operand=RegisterArgument realDestReg,
                                    shiftAmount=IntegerConstant i, ccRef=ccRef} :: code)
            end

        |   codeExtended({instr=ShiftOperation{shift, resultReg=PReg resReg, operand=RegisterArgument(PReg operReg),
                                        shiftAmount=RegisterArgument(PReg shiftReg), ccRef}, ...} :: rest, context) =
            let
                val () = addSourceAndDestinationHint{dst=resReg, src=operReg}
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

        |   codeExtended({instr=ShiftOperation _, ...} :: _, _) = raise InternalError "codeExtended - ShiftOperation"

        |   codeExtended({instr=
                Multiplication{resultReg=PReg resReg, operand1=RegisterArgument(PReg op1Reg),
                               operand2=RegisterArgument(PReg op2Reg), ccRef}, ...} :: rest, context) =
            let
                (* Treat exactly the same as ArithmeticFunction. *)
                val () = addSourceAndDestinationHint{dst=resReg, src=op1Reg}
                val () = addSourceAndDestinationHint{dst=resReg, src=op2Reg}
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
        |   codeExtended({instr=Multiplication _, ...} :: _, _) = raise InternalError "codeExtended - multiplication TODO"

        |   codeExtended({instr=Division{isSigned, dividend=PReg regDivid, divisor, quotient=PReg regQuot,
                                  remainder=PReg regRem}, ...} :: rest, context) =
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

        |   codeExtended({instr=AtomicExchangeAndAdd{base=PReg bReg, source=PReg sReg}, ...} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val srcReg = findGeneralReg sReg
                val baseReg = findGeneralReg bReg
            in
                AtomicExchangeAndAdd{base=baseReg, source=srcReg} :: code
            end

        |   codeExtended({instr=BoxValue{boxKind, source=PReg sReg, dest=PReg dReg, saveRegs}, ...} :: rest, context) =
            let
                val code = codeExtended(rest, context)
                val preserve = List.map(fn (PReg r) => findGeneralReg r) saveRegs
                val (srcReg, boxSize, moveKind) =
                    case boxKind of
                        BoxLargeWord => (findGeneralReg sReg, 1, MoveWord)
                    |   BoxFloat => (findFloatReg sReg, Int.quot(8, wordSize), MoveDouble)
                val dstReg = findGeneralReg dReg
            in
                AllocateMemoryOperation{ size=boxSize, flags=0wx1, dest=dstReg, saveRegs=preserve} ::
                StoreArgument{ source=RegisterArgument srcReg, offset=0, base=dstReg, index=NoMemIndex, kind=moveKind} ::
                InitialisationComplete :: code
            end

        |   codeExtended({instr=CompareByteVectors{vec1Addr=PReg v1Reg, vec2Addr=PReg v2Reg, length=PReg lReg, ccRef}, ...} :: rest, context) =
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

        |   codeExtended({instr=BlockMove{srcAddr=PReg sReg, destAddr=PReg dReg, length=PReg lReg, isByteMove}, ...} :: rest, context) =
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

        |   codeExtended({instr=CompareFloatingPt{arg1 as RegisterArgument _, arg2, ccRef}, ...} :: rest, context) =
            let
                val code = codeExtended(rest, context)
            in
                CompareFloatingPt{ arg1=codeExtArgFloat arg1, arg2=codeExtArgFloat arg2, ccRef=ccRef } :: code
            end

        |   codeExtended({instr=CompareFloatingPt _, ...} :: _, _) = raise InternalError "codeExtended - CompareFloatingPt"

        |   codeExtended({instr=X87FPGetCondition{dest=PReg dReg, ccRef}, ...} :: rest, context) =
            let
                (* We can only use RAX here. *)
                val () = addRealHint(dReg, GenReg eax)
                val code = codeExtended(rest, context)
                val destReg = findRegister(dReg, SOME(GenReg eax), generalRegisters)
            in
                X87FPGetCondition{dest=raxAsArg, ccRef=ccRef} ::
                    moveIfNecessary{dst=destReg, src=GenReg eax} @ code
            end

        |   codeExtended({instr=X87FPArith{opc, resultReg=PReg resReg, arg1=RegisterArgument(PReg op1Reg), arg2}, ...} :: rest, context) =
            let
                val () = addSourceAndDestinationHint{dst=resReg, src=op1Reg}
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, floatingPtRegisters)
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, floatingPtRegisters)
                val op2Arg = codeExtArgFloat arg2
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg} @
                    (X87FPArith{opc=opc, resultReg=realDestReg, arg1=RegisterArgument realDestReg, arg2=op2Arg} :: code)
            end

        |   codeExtended({instr=X87FPArith _, ...} :: _, _) = raise InternalError "codeExtended - X87FPArith"
    
        |   codeExtended({instr=X87FPUnaryOps{fpOp, dest=PReg resReg, source=RegisterArgument(PReg op1Reg)}, ...} :: rest, context) =
            let
                val () = addSourceAndDestinationHint{dst=resReg, src=op1Reg}
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, floatingPtRegisters)
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, floatingPtRegisters)
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg} @
                    (X87FPUnaryOps{fpOp=fpOp, dest=realDestReg, source=RegisterArgument realDestReg} :: code)
            end

        |   codeExtended({instr=X87FPUnaryOps _, ...} :: _, _) = raise InternalError "codeExtended - X87FPArith"

        |   codeExtended({instr=FloatFixedInt{dest=PReg resReg, source}, ...} :: rest, context) =
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
                                PushValue{arg=intSource, container=dummyC},
                                FloatFixedInt{dest=fpReg, source=wordOffsetAddress(0, esp)},
                                ResetStackPtr {numWords=1}
                            ]
                    |   FPModeSSE2 => [FloatFixedInt{dest=fpReg, source=intSource}]
            in
                floatCode @ code
            end
    
        |   codeExtended({instr=SSE2FPArith{opc, resultReg=PReg resReg, arg1=RegisterArgument(PReg op1Reg), arg2}, ...} :: rest, context) =
            let
                val () = addSourceAndDestinationHint{dst=resReg, src=op1Reg}
                val code = codeExtended(rest, context)
                val realDestReg = findRegister(resReg, NONE, floatingPtRegisters)
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, floatingPtRegisters)
                val op2Arg = codeExtArgFloat arg2
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg} @
                    (SSE2FPArith{opc=opc, resultReg=realDestReg, arg1=RegisterArgument realDestReg, arg2=op2Arg} :: code)
            end
    
        |   codeExtended({instr=SSE2FPArith _, ...} :: _, _) = raise InternalError "codeExtended - SSE2FPArith"

        (* Depth-first scan. *)
        local
            val numBlocks = Vector.length identifiedBlocks
            val emptyBlock = BasicBlock { block = [], flow = ExitCode }
        in
            val visited = Array.array(numBlocks, false)
            val results = Array.array(numBlocks, emptyBlock)
        end

        fun processBlocks blockNo =
        if Array.sub(visited, blockNo)
        then ()  (* Done or currently being done. *)
        else
        let
            val () = Array.update(visited, blockNo, true)
            val ExtendedBasicBlock { flow, block, passThrough, exports, ...} =
                Vector.sub(identifiedBlocks, blockNo)
            val () =
                (* Process the dependencies first. *)
                case flow of
                    ExitCode => ()
                |   Unconditional m => processBlocks m
                |   Conditional {trueJump, falseJump, ...} =>
                        (processBlocks trueJump; processBlocks falseJump)
                |   IndexedBr cases => List.app processBlocks cases
                |   SetHandler{ handler, continue } =>
                        (processBlocks handler; processBlocks continue)
            (* Now this block. *)
            (* We assume that anything used later will have been allocated a register.
               This is generally true except for a loop where the use may occur earlier. *)
            val exported = setToList passThrough @ setToList exports
            val () = List.app(fn r => (findRegister(r, NONE, generalRegisters); ())) exported
            val codeBlock = codeExtended(block, [])
            val resultBlock = BasicBlock { block = codeBlock, flow = flow }
        in
            Array.update(results, blockNo, resultBlock)
        end

        val () = processBlocks 0
    in
        Array.vector results
    end
    
    (* Find out the registers that need to be pushed to the stack, if any.
       We include those marked as "must push" because we need to save them across a
       function call or handler and also any we need to push because the set of active
       registers is more than the number of general registers we have.  This second case
       involves choosing suitable registers and is a first attempt to check we have enough
       registers.  We can also get a failure in codeExtended when we actually allocate
       the registers. *)
    fun spillRegisters(identified: extendedBasicBlock vector, regStates: regState vector) =
    let
        val maxPRegs = Vector.length regStates
        val pushArray = Array.array(maxPRegs, false)
        (* Mark anything already marked as "must push" *)
        val () = Vector.appi(fn (i, {pushState=MustPush, ...}) => Array.update(pushArray, i, true) | _ => ()) regStates
        val nGenRegs = List.length generalRegisters
        (* Make a list of all the active sets ignoring those marked to be pushed.
           Do that first because we need to know how many sets each register is in. *)
        fun nowActive regs =
            List.foldl (fn (r, l) => if Array.sub(pushArray, r) then l else r :: l) [] regs
        fun getBlockSets(ExtendedBasicBlock{block, passThrough, ...}, sets) =
        let
            fun getSets({active, ...}, l) =
            let
                val set = nowActive(setToList(union(active, passThrough)))
            in
                if List.length set > nGenRegs
                then set :: l
                else l
            end
        in
            List.foldl getSets sets block
        end
        val activeSets = Vector.foldl getBlockSets [] identified
    in
        if null activeSets then ()
        else
        let
            (* See how many times each register appears in a set. *)
            val activeIn = Array.array(maxPRegs, 0)
            val () =
                List.app (fn regs => List.app(fn r => Array.update(activeIn, r, Array.sub(activeIn, r)+1)) regs) activeSets
            (* We want to choose the best registers to spill. *)
            fun spillSomeRegs activeSet =
            let
                (* We may have already marked some of these to push. *)
                val currentActive = nowActive activeSet
                val regCount = List.length currentActive
                fun addCosts r =
                let
                    val {active, pushState, refs, ...} = Vector.sub(regStates, r)
                in
                    case pushState of
                        MustNotPush => (r, ~1, ~1)
                    |   _ => (r, Array.sub(activeIn, r), if refs = 0 then 0 else Int.quot(active, refs))
                end
                val withCosts = List.map addCosts currentActive
                (* Order so that the earlier items are those that appear in more sets and
                   if items appear in the same number of sets those that are active
                   longer come earlier. *)
                fun compare (_, in1, a1)  (_, in2, a2) = if in1 > in2 then true else if in1 < in2 then false else a1 > a2
                val sorted = Misc.quickSort compare withCosts

                fun markAsPush([], _) = ()
                |   markAsPush((reg, _, _) :: regs, n) =
                    if n <= 0
                    then ()
                    else
                    let
                        val {pushState, ...} = Vector.sub(regStates, reg)
                        val _ = case pushState of MustNotPush => raise InternalError "markAsPush" | _ => ()
                    in
                        Array.update(pushArray, reg, true);
                        markAsPush(regs, n-1)
                    end
            in
                markAsPush(sorted, regCount-nGenRegs)
            end
        in
            List.app spillSomeRegs activeSets
        end;
        (* Return the vector showing those that must be pushed. *)
        Array.vector pushArray
    end
     
    type triple = {instr: abstract x86ICode, current: intSet, active: intSet}

    fun codeICodeFunctionToX86{blocks, functionName, pregProps, argRegsUsed, hasFullClosure, debugSwitches, ...} =
    let
       (*val maxPRegs = Vector.length pregProps*)
        val icodeTabs = [8, 20, 60]
        val wantPrintCode = DEBUG.getParameter DEBUG.icodeTag debugSwitches
        
        fun printCode identifiedCode =
            (* Print the code before the transformation. *)
            if wantPrintCode
            then
            let
                val printStream = PRETTY.getSimplePrinter(debugSwitches, icodeTabs)
            in
                printStream(functionName ^ "\n");
                printICodeAbstract(identifiedCode, printStream);
                printStream "\n"
            end
            else ()
        
        fun printConflicts(regStates: conflictState vector) =
            if wantPrintCode
            then
            let
                val printStream = PRETTY.getSimplePrinter(debugSwitches, icodeTabs)

                fun printRegs([], _) = ()
                |   printRegs(_, 0) = printStream "..."
                |   printRegs([i], _) = printStream(Int.toString i)
                |   printRegs(i::l, n) = (printStream(Int.toString i ^ ","); printRegs(l, n-1))
                
                fun printRegData(i, { conflicts, ... }) =
                (
                    printStream (Int.toString i ^ "\t");
                    printStream ("Conflicts="); printRegs(setToList conflicts, 20);
                    printStream "\n"
                )
            in
                Vector.appi printRegData regStates
            end
            else ()            

        fun processCode(basicBlocks, pregProps: regProperty vector, maxStack, passes) =
        let
            val () =
                if wantPrintCode
                then printCode basicBlocks
                else ()
            (* First pass - identify register use patterns *)
            val (identified, regStates) = identifyRegisters(basicBlocks, pregProps)
            val regsToSpill = spillRegisters(identified, regStates)
            val needPhase2 = Vector.exists(fn t => t) regsToSpill
        in
            if needPhase2
            then
            let
                (* Push those registers we need to.  This also adds and renumbers pregs
                   and may add labels. *)
                val {code=postPushCode, pregProps=regPropsPhase2, maxStack=maxStackPhase2} =
                    addRegisterPushes{code=identified, pushVec=regsToSpill, pregProps=pregProps}
            in
                (* And reprocess. *)
                processCode(postPushCode, regPropsPhase2, maxStackPhase2, passes+1)
            end
            else
            let
                val maxPRegs = Vector.length regStates
                fun selectARegisterToSpill active =
                let
                    fun chooseReg([], bestReg, _) = bestReg
                    |   chooseReg(reg::regs, bestReg, bestCost) =
                        let
                            val {active, pushState, refs, ...} = Vector.sub(regStates, reg)
                            val cost = if refs = 0 then 0 else Int.quot(active, refs)
                        in
                            case pushState of
                                MustNotPush => chooseReg(regs, bestReg, bestCost)
                            |   _ =>
                                if cost >= bestCost
                                then chooseReg(regs, reg, active)
                                else chooseReg(regs, bestReg, bestCost)
                        end
                    val choice = chooseReg(setToList active, ~1, 0)
                    val _ = choice >= 0 orelse raise InternalError "chooseReg"
                in
                    Vector.tabulate(maxPRegs, fn i => i = choice)
                end
                (* Now get the conflict sets. *)
                val conflictSets = getConflictStates(identified, maxPRegs)
                local
                    fun mapFromExtended(ExtendedBasicBlock{block, flow, ...}) =
                        BasicBlock{block=List.map #instr block, flow=flow}
                in
                    val () =
                        if wantPrintCode then (printCode(Vector.map mapFromExtended identified); printConflicts conflictSets) else ()
                end

            in
                (codeAbstractToConcrete(identified, conflictSets), maxStack)
                    handle RegisterOverflow activeSet =>
                    let
                        val regsToSpill = selectARegisterToSpill activeSet
                        val {code=postPushCode, pregProps=pregPropsPhase2, maxStack=maxStackPhase2} =
                            addRegisterPushes{code=identified, pushVec=regsToSpill, pregProps=pregProps}
                    in
                        processCode(postPushCode, pregPropsPhase2, maxStackPhase2, passes+1)
                    end
            end
        end

        val (postTransformCode: reg basicBlock vector, stackRequired) =
            processCode(blocks, pregProps, 0 (* Should include handlers and containers. *), 0)

        val () =
            (* Print the code after the transformation. *)
            if DEBUG.getParameter DEBUG.icodeTag debugSwitches
            then
            let
                val printStream = PRETTY.getSimplePrinter(debugSwitches, icodeTabs)
            in
                printStream(functionName ^ "\n");
                printICodeConcrete(postTransformCode, printStream)
            end
            else ()
    in
        (* Code-generate it. *)
        codeAsX86Code{blockCode=postTransformCode, stackRequired = stackRequired,
                      inputRegisters= argRegsUsed @ (if hasFullClosure then [GenReg edx] else []), debugSwitches=debugSwitches,
                      functionName=functionName}
    end
    structure Sharing =
    struct
        type abstract        = abstract
        and reg             = reg
        and 'reg basicBlock = 'reg basicBlock
        and regProperty = regProperty
    end
end;
