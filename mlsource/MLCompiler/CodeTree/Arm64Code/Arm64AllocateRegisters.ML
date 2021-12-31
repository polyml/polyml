(*
    Copyright David C. J. Matthews 2016-21

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

functor Arm64AllocateRegisters(
    structure Arm64ICode: ARM64ICODE
    structure Identify: ARM64IDENTIFYREFERENCES
    structure ConflictSets: ARM64ICODECONFLICTS
    structure IntSet: INTSET
    
    sharing Arm64ICode.Sharing = Identify.Sharing = ConflictSets.Sharing = IntSet
): ARM64ALLOCATEREGISTERS =
struct
    open Arm64ICode
    open Identify
    open ConflictSets
    open IntSet
    open Address
    
    exception InternalError = Misc.InternalError

    datatype allocateResult =
        AllocateSuccess of basicBlockConcrete vector
    |   AllocateFailure of intSet list

    (* General registers.  X24 is used as the global heap base in 32-in-64. X30 is the
       return address set by blr but is otherwise a general register.
       Put the argument registers at the end of the list so they'll only be
       used when hinted. *)
    val generalRegisters =
        map GenReg ([X9, X10, X11, X12, X13, X14, X15, X19, X20, X21, X22, X23,
                      X0, X1, X2, X3, X4, X5, X6, X7, X8, X30] @ (if is32in64 then [] else [X24]))

    val floatingPtRegisters =
        map FPReg [V7, V6, V5, V4, V3, V2, V1]

    fun allocateRegisters{blocks, regStates, regProps, ...} =
    let
        val maxPRegs = Vector.length regStates
        and numBlocks = Vector.length blocks

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
        local
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
        
        in
            (* Add the hints to steer the register allocation.  The idea is to avoid moves between
               registers by getting values into the appropriate register in advance. *)
            fun addHints{instr=MoveRegister{source=PReg sreg, dest=PReg dreg, ...}, ...} =
                    addSourceAndDestinationHint {src=sreg, dst=dreg}

            |   addHints{instr=BitFieldInsert{destAsSource=PReg dsReg, dest=PReg dReg, ...}, ...} =
                    (* The "destAsSource" is the destination if some bits are retained. *)
                    addSourceAndDestinationHint {src=dsReg, dst=dReg}

            |   addHints{instr=ReturnResultFromFunction { resultReg=PReg resReg, returnReg=PReg retReg, ... }, ...} =
                (
                    addRealHint(resReg, GenReg X0);
                    addRealHint(retReg, GenReg X30) (* It may be there from earlier. *)
                )

            |   addHints{instr=JumpLoop{regArgs, ...}, ...} =
                let
                    fun addRegArg {src=ArgInReg(PReg argReg), dst=PReg resReg} =
                            addSourceAndDestinationHint {dst=resReg, src=argReg}
                    |   addRegArg {src=ArgOnStack _, ...} = ()
                in
                    List.app addRegArg regArgs
                end

            |   addHints{instr=BeginFunction{regArgs, ...}, ...} =
                    List.app (fn (PReg pr, reg) => addRealHint(pr, GenReg reg)) regArgs

            |   addHints{instr=TailRecursiveCall{regArgs, ...}, ...} =
                let
                    fun setHint(ArgInReg(PReg pr), reg) = addRealHint(pr, GenReg reg) | setHint _ = ()
                in
                    List.app setHint regArgs
                end
        
            |   addHints{instr=FunctionCall{regArgs, dest=PReg dreg, ...}, ...} =
                let
                    fun setHint(ArgInReg(PReg pr), reg) = addRealHint(pr, GenReg reg) | setHint _ = ()
                in
                    addRealHint(dreg, GenReg X0);
                    List.app setHint regArgs
                end

                (* Exception packets are in X0 *)
            |   addHints{instr=RaiseExceptionPacket{ packetReg=PReg preg }, ...} = addRealHint(preg, GenReg X0)
            |   addHints{instr=BeginHandler{ packetReg=PReg preg }, ...} = addRealHint(preg, GenReg X0)

            |   addHints _ = ()
        
        end

        val allocatedRegs = Array.array(maxPRegs, NONE: reg option)
        val failures = ref []: intSet list ref

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

                fun findAReg [] =
                    (
                        (* This failed.  We're going to have to spill something. *)
                        failures := conflicts :: ! failures;
                        hd regSet (* Return a register to satisfy everything. *)
                    )
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
            
        fun allocateRegister args = ignore(findRegister args)
        val allocateFindRegister = findRegister

        fun allocateGenReg(PReg r) = allocateRegister(r, NONE, generalRegisters)
        and allocateFloatReg(PReg r) = allocateRegister(r, NONE, floatingPtRegisters)
        and allocateOptGenReg(SomeReg reg) = allocateGenReg reg | allocateOptGenReg ZeroReg = ()

        val allocateGenRegs = List.app allocateGenReg
        and allocateFloatRegs = List.app allocateFloatReg

        fun registerAllocate{instr=MoveRegister{source=PReg sreg, dest=PReg dreg}, ...} =
            let
                val realDestReg = findRegister(dreg, NONE, generalRegisters)
            in
                allocateRegister(sreg, SOME realDestReg, generalRegisters)
            end

        |   registerAllocate{instr=LoadNonAddressConstant{dest, ...}, ...} = allocateGenReg dest

        |   registerAllocate{instr=LoadAddressConstant{dest, ...}, ...} = allocateGenReg dest

        |   registerAllocate{instr=LoadWithConstantOffset{dest, base, ...}, ...} =
            (
                allocateGenReg dest;
                allocateGenReg base
            )

        |   registerAllocate{instr=LoadFPWithConstantOffset{dest, base, ...}, ...} =
            (
                allocateFloatReg dest;
                allocateGenReg  base
            )

        |   registerAllocate{instr=LoadWithIndexedOffset{dest, base, index, ...}, ...} =
            (
                allocateGenReg dest;
                allocateGenRegs[base, index]
            )

        |   registerAllocate{instr=LoadFPWithIndexedOffset{dest, base, index, ...}, ...} =
            (
                allocateFloatReg dest;
                allocateGenRegs[base, index]
            )

        |   registerAllocate{instr=GetThreadId { dest, ...}, ...} = allocateGenReg dest

        |   registerAllocate{instr=ObjectIndexAddressToAbsolute{dest, source, ...}, ...} = allocateGenRegs[dest, source]

        |   registerAllocate{instr=AbsoluteToObjectIndex{dest, source, ...}, ...} = allocateGenRegs[dest, source]

        |   registerAllocate({instr=AllocateMemoryFixed{ dest, saveRegs, ...}, ...}) = allocateGenRegs (dest :: saveRegs)

        |   registerAllocate({instr=AllocateMemoryVariable{ size, dest, saveRegs, ...}, ...}) =
                allocateGenRegs (size :: dest :: saveRegs)

        |   registerAllocate({instr=InitialiseMem{ size, addr, init}, ...}) = allocateGenRegs [size, addr, init]

        |   registerAllocate{instr=BeginLoop, ...} = ()

        |   registerAllocate({instr=JumpLoop{regArgs, stackArgs, checkInterrupt}, ...}) =
            (
                List.app (fn {src=ArgInReg sreg, dst} => (allocateGenReg sreg; allocateGenReg dst) | _ => ()) regArgs;
                List.app (fn {src=ArgInReg sreg, ...} => allocateGenReg sreg | _ => ()) stackArgs;
                case checkInterrupt of SOME regs => List.app(fn reg => allocateGenReg reg) regs | NONE => ()
            )

        |   registerAllocate{instr=StoreWithConstantOffset{source, base, ...}, ...} = allocateGenRegs[source, base]

        |   registerAllocate{instr=StoreFPWithConstantOffset{source, base, ...}, ...} =
            (
                allocateFloatReg source;
                allocateGenReg base
            )

        |   registerAllocate{instr=StoreWithIndexedOffset{source, base, index, ...}, ...} = allocateGenRegs[source, base, index]

        |   registerAllocate{instr=StoreFPWithIndexedOffset{source, base, index, ...}, ...} =
            (
                allocateFloatReg source;
                allocateGenRegs[base, index]
            )

        |   registerAllocate{instr=AddSubImmediate{ source, dest, ... }, ...} =
            (
                allocateOptGenReg dest;
                allocateGenReg source
            )

        |   registerAllocate{instr=AddSubRegister{ base, shifted, dest, ... }, ...} =
            (
                allocateOptGenReg dest;
                allocateGenRegs[base, shifted]
            )

        |   registerAllocate{instr=LogicalImmediate{ source, dest, ... }, ...} =
            (
                allocateOptGenReg dest;
                allocateGenReg source
            )

        |   registerAllocate{instr=LogicalRegister{ base, shifted, dest, ... }, ...} =
            (
                allocateOptGenReg dest;
                allocateGenRegs[base, shifted]
            )

        |   registerAllocate{instr=ShiftRegister{ dest, source, shift, ... }, ...} = allocateGenRegs[dest, source, shift]

        |   registerAllocate{instr=Multiplication{ dest, sourceA, sourceM, sourceN, ... }, ...} =
                (allocateGenRegs[dest, sourceM, sourceN]; allocateOptGenReg sourceA)

        |   registerAllocate{instr=Division{ dest, dividend, divisor, ... }, ...} = allocateGenRegs[dest, dividend, divisor]

        |   registerAllocate{instr=BeginFunction{regArgs, ...}, ...} =
                (* Check that every argument has a register allocated including any that are unused.
                   Unused arguments should be discarded at a higher level because we could allocate
                   a different register and copy the argument register only to discard it. *)
                allocateGenRegs(List.map #1 regArgs)

        |   registerAllocate({instr=TailRecursiveCall{regArgs=regArgs, stackArgs=stackArgs, ...}, ...}) =
            let
                fun allocateRegArg(ArgInReg argReg, _) = allocateGenReg argReg
                |   allocateRegArg _ = ()
            in
                (* We've already hinted the arguments but we want to allocate these first
                   to reduce the chance that they'll be used for stack arguments. *)
                List.app allocateRegArg regArgs;
                List.app (fn {src=ArgInReg argReg, ...} => allocateGenReg argReg | _ => ()) stackArgs
            end

        |   registerAllocate({instr=FunctionCall{regArgs=regArgs, stackArgs=stackArgs, dest=PReg dReg, saveRegs, ...}, ...}) =
            let
                fun allocateRegArg(ArgInReg argReg, _) = allocateGenReg argReg
                |   allocateRegArg _ = ()
            in
                (* We've already hinted the arguments but we want to allocate these first
                   to reduce the chance that they'll be used for stack arguments. *)
                List.app allocateRegArg regArgs;
                List.app (fn ArgInReg argReg => allocateGenReg argReg | _ => ()) stackArgs;
                allocateGenRegs saveRegs;
                (* Result will be in X0. *)
                allocateRegister(dReg, SOME(GenReg X0), [GenReg X0])
            end

        |   registerAllocate{instr=ReturnResultFromFunction { resultReg=PReg resReg, returnReg, ... }, ...} =
            (
                allocateRegister(resReg, SOME(GenReg X0), [GenReg X0] (* It MUST be in this register *));
                allocateGenReg returnReg
            )

        |   registerAllocate{instr=RaiseExceptionPacket{packetReg}, ...} = allocateGenReg packetReg

        |   registerAllocate{instr=PushToStack{ source, ... }, ...} = allocateGenReg source

        |   registerAllocate{instr=LoadStack{ dest,  ... }, ...} = allocateGenReg dest

        |   registerAllocate{instr=StoreToStack{ source, ... }, ...} = allocateGenReg source

        |   registerAllocate{instr=ContainerAddress{ dest, ... }, ...} = allocateGenReg dest

        |   registerAllocate{instr=ResetStackPtr _, ...} = ()

        |   registerAllocate({instr=TagValue{source, dest, ...}, ...}) = allocateGenRegs[source, dest]

        |   registerAllocate({instr=UntagValue{source, dest, ...}, ...}) = allocateGenRegs[source, dest]

        |   registerAllocate({instr=BoxLarge{source, dest, saveRegs}, ...}) =
                (allocateGenRegs saveRegs; allocateGenRegs[source, dest])

        |   registerAllocate({instr=UnboxLarge{source, dest}, ...}) = allocateGenRegs[source, dest]

        |   registerAllocate({instr=BoxTagFloat{source, dest, saveRegs, ...}, ...}) =
            (
                allocateGenRegs saveRegs;
                allocateFloatReg source;
                allocateGenReg dest
            )

        |   registerAllocate({instr=UnboxTagFloat{source, dest, ...}, ...}) =
            (
                allocateFloatReg dest;
                allocateGenReg source
            )

        |   registerAllocate{instr=LoadAcquire{dest, base, ...}, ...} = allocateGenRegs[dest, base]

        |   registerAllocate{instr=StoreRelease{source, base, ...}, ...} = allocateGenRegs[source, base]

        |   registerAllocate{instr=BitFieldShift{ source, dest, ... }, ...} = allocateGenRegs[source, dest]

        |   registerAllocate{instr=BitFieldInsert{ source, dest, destAsSource, ... }, ...} =
                allocateGenRegs[source, destAsSource, dest]

        |   registerAllocate({instr=IndexedCaseOperation{testReg}, ...}) = allocateGenReg testReg
        
        |   registerAllocate({instr=PushExceptionHandler, ...}) = ()
        |   registerAllocate({instr=PopExceptionHandler, ...}) = ()

        |   registerAllocate({instr=BeginHandler{packetReg}, ...}) = allocateGenReg packetReg

        |   registerAllocate({instr=CompareByteVectors{vec1Addr, vec2Addr, length, ...}, ...}) =
                allocateGenRegs[vec1Addr, vec2Addr, length]

        |   registerAllocate({instr=BlockMove{srcAddr, destAddr, length, ...}, ...}) =
                allocateGenRegs[srcAddr, destAddr, length]

        |   registerAllocate({instr=AddSubXSP{source, dest, ...}, ...}) =
                ( allocateOptGenReg dest; allocateGenReg source )

        |   registerAllocate({instr=TouchValue{source, ...}, ...}) = allocateGenReg source

        |   registerAllocate({instr=LoadAcquireExclusive{ base, dest }, ...}) = allocateGenRegs[base, dest]

        |   registerAllocate({instr=StoreReleaseExclusive{ base, source, result }, ...}) =
                ( allocateGenRegs[base, result];  allocateOptGenReg source )

        |   registerAllocate({instr=MemoryBarrier, ...}) = ()

        |   registerAllocate({instr=ConvertIntToFloat{ source, dest, ...}, ...}) =
                (allocateFloatReg dest; allocateGenReg source)

        |   registerAllocate({instr=ConvertFloatToInt{ source, dest, ...}, ...}) =
                (allocateGenReg dest; allocateFloatReg source)

        |   registerAllocate({instr=UnaryFloatingPt{ source, dest, ...}, ...}) =
                allocateFloatRegs[source, dest]

        |   registerAllocate({instr=BinaryFloatingPoint{ arg1, arg2, dest, ...}, ...}) =
                allocateFloatRegs[arg1, arg2, dest]

        |   registerAllocate({instr=CompareFloatingPoint{ arg1, arg2, ...}, ...})  =
                allocateFloatRegs[arg1, arg2]

        (* Depth-first scan. *)
        val visited = Array.array(numBlocks, false)

        fun processBlocks blockNo =
        if Array.sub(visited, blockNo)
        then ()  (* Done or currently being done. *)
        else
        let
            val () = Array.update(visited, blockNo, true)
            val ExtendedBasicBlock { flow, block, passThrough, exports, ...} =
                Vector.sub(blocks, blockNo)
            (* Add the hints for this block before the actual allocation of registers. *)
            val _ = List.app addHints block
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
                |   UnconditionalHandle _ => ()
                |   ConditionalHandle { continue, ...} => processBlocks continue
            (* Now this block. *)
            local
                (* We assume that anything used later will have been allocated a register.
                   This is generally true except for a loop where the use may occur earlier. *)
                val exported = setToList passThrough @ setToList exports
                fun findAReg r =
                    case Vector.sub(regProps, r) of
                        RegPropStack _ => ()
                    |   _ => ignore(allocateFindRegister(r, NONE, generalRegisters))
            in
                val () = List.app findAReg exported
            end
        in
            List.foldr(fn (c, ()) => registerAllocate c) () block
        end


        (* Turn the abstract icode into a concrete version by allocating the registers. *)
        local
            fun getAllocatedReg(PReg r) = getOpt(Array.sub(allocatedRegs, r), GenReg XZero)
        
            fun getAllocatedGenReg r =
                case getAllocatedReg r of GenReg r => r | FPReg _ => raise InternalError "getAllocateGenReg: returned FP Reg"
            and getAllocatedFPReg r =
                case getAllocatedReg r of FPReg r => r | GenReg _ => raise InternalError "getAllocatedFPReg: returned Gen Reg"

            fun getAllocatedGenRegOrZero ZeroReg = XZero
            |   getAllocatedGenRegOrZero (SomeReg reg) = getAllocatedGenReg reg

            fun getAllocatedArg(ArgInReg reg) = ArgInReg(getAllocatedGenReg reg)
            |   getAllocatedArg(ArgOnStack stackLoc) = ArgOnStack stackLoc

            val getSaveRegs = List.map getAllocatedGenReg

            fun absToConcrete({instr=MoveRegister{ source, dest}, ...}): iCodeConcrete =
                    MoveRegister { source=getAllocatedGenReg source, dest=getAllocatedGenReg dest}

            |   absToConcrete({instr=LoadNonAddressConstant { dest, source}, ...}) =
                    LoadNonAddressConstant { dest=getAllocatedGenReg dest, source=source}

            |   absToConcrete({instr=LoadAddressConstant { dest, source}, ...}) =
                    LoadAddressConstant { dest=getAllocatedGenReg dest, source=source}

            |   absToConcrete({instr=LoadWithConstantOffset { base, dest, byteOffset, loadType}, ...}) =
                    LoadWithConstantOffset { base=getAllocatedGenReg base, dest=getAllocatedGenReg dest, byteOffset=byteOffset, loadType=loadType}

            |   absToConcrete({instr=LoadFPWithConstantOffset { base, dest, byteOffset, floatSize}, ...}) =
                    LoadFPWithConstantOffset { base=getAllocatedGenReg base, dest=getAllocatedFPReg dest, byteOffset=byteOffset, floatSize=floatSize}

            |   absToConcrete({instr=LoadWithIndexedOffset { base, dest, index, loadType}, ...}) =
                    LoadWithIndexedOffset { base=getAllocatedGenReg base, dest=getAllocatedGenReg dest, index=getAllocatedGenReg index, loadType=loadType}

            |   absToConcrete({instr=LoadFPWithIndexedOffset { base, dest, index, floatSize}, ...}) =
                    LoadFPWithIndexedOffset { base=getAllocatedGenReg base, dest=getAllocatedFPReg dest, index=getAllocatedGenReg index, floatSize=floatSize}

            |   absToConcrete({instr=GetThreadId { dest}, ...}) = GetThreadId { dest=getAllocatedGenReg dest}

            |   absToConcrete({instr=ObjectIndexAddressToAbsolute { source, dest}, ...}) =
                    ObjectIndexAddressToAbsolute { source=getAllocatedGenReg source, dest=getAllocatedGenReg dest}

            |   absToConcrete({instr=AbsoluteToObjectIndex { source, dest}, ...}) =
                    AbsoluteToObjectIndex { source=getAllocatedGenReg source, dest=getAllocatedGenReg dest}

            |   absToConcrete({instr=AllocateMemoryFixed { bytesRequired, dest, saveRegs }, ...}) =
                    AllocateMemoryFixed { dest=getAllocatedGenReg dest, bytesRequired=bytesRequired, saveRegs=getSaveRegs saveRegs}

            |   absToConcrete({instr=AllocateMemoryVariable{size, dest, saveRegs}, ...}) =
                    AllocateMemoryVariable{size=getAllocatedGenReg size, dest=getAllocatedGenReg dest, saveRegs=getSaveRegs saveRegs}

            |   absToConcrete({instr=InitialiseMem{size, addr, init}, ...}) =
                    InitialiseMem{size=getAllocatedGenReg size, addr=getAllocatedGenReg addr, init=getAllocatedGenReg init}

            |   absToConcrete({instr=BeginLoop, ...}) = BeginLoop

            |   absToConcrete({instr=JumpLoop{regArgs, stackArgs, checkInterrupt}, ...}) =
                let
                    fun getStackArg{src, wordOffset, stackloc} =
                            {src=getAllocatedArg src, wordOffset=wordOffset, stackloc=stackloc}
                    and getRegArg{src, dst} = {src=getAllocatedArg src, dst=getAllocatedGenReg dst}
                in
                    JumpLoop{ regArgs=map getRegArg regArgs, stackArgs=map getStackArg stackArgs,
                              checkInterrupt=Option.map getSaveRegs checkInterrupt}
                end

            |   absToConcrete({instr=StoreWithConstantOffset { base, source, byteOffset, loadType}, ...}) =
                    StoreWithConstantOffset{ base=getAllocatedGenReg base, source=getAllocatedGenReg source, byteOffset=byteOffset, loadType=loadType}

            |   absToConcrete({instr=StoreFPWithConstantOffset { base, source, byteOffset, floatSize}, ...}) =
                    StoreFPWithConstantOffset{ base=getAllocatedGenReg base, source=getAllocatedFPReg source, byteOffset=byteOffset, floatSize=floatSize}

            |   absToConcrete({instr=StoreWithIndexedOffset { base, source, index, loadType}, ...}) =
                    StoreWithIndexedOffset{ base=getAllocatedGenReg base, source=getAllocatedGenReg source, index=getAllocatedGenReg index, loadType=loadType}

            |   absToConcrete({instr=StoreFPWithIndexedOffset { base, source, index, floatSize}, ...}) =
                    StoreFPWithIndexedOffset{ base=getAllocatedGenReg base, source=getAllocatedFPReg source, index=getAllocatedGenReg index, floatSize=floatSize}

            |   absToConcrete({instr=AddSubImmediate{ source, dest, ccRef, immed, isAdd, length }, ...}) =
                    AddSubImmediate { source=getAllocatedGenReg source, dest=getAllocatedGenRegOrZero dest, ccRef=ccRef,
                                                 immed=immed, isAdd=isAdd, length=length}

            |   absToConcrete({instr=AddSubRegister{ base, shifted, dest, ccRef, isAdd, length, shift}, ...}) =
                    AddSubRegister { base=getAllocatedGenReg base, shifted=getAllocatedGenReg shifted,
                                     dest=getAllocatedGenRegOrZero dest, ccRef=ccRef,
                                     isAdd=isAdd, length=length, shift=shift}

            |   absToConcrete({instr=LogicalImmediate{ source, dest, ccRef, immed, logOp, length }, ...}) =
                    LogicalImmediate { source=getAllocatedGenReg source, dest=getAllocatedGenRegOrZero dest, ccRef=ccRef,
                                                  immed=immed, logOp=logOp, length=length}

            |   absToConcrete({instr=LogicalRegister{ base, shifted, dest, ccRef, logOp, length, shift}, ...}) =
                    LogicalRegister { base=getAllocatedGenReg base, shifted=getAllocatedGenReg shifted,
                                      dest=getAllocatedGenRegOrZero dest, ccRef=ccRef,
                                      logOp=logOp, length=length, shift=shift}

            |   absToConcrete({instr=ShiftRegister{ direction, dest, source, shift, opSize}, ...}) =
                    ShiftRegister { source=getAllocatedGenReg source, shift=getAllocatedGenReg shift, dest=getAllocatedGenReg dest,
                                               direction=direction, opSize=opSize}

            |   absToConcrete({instr=Multiplication{ kind, dest, sourceA, sourceM, sourceN }, ...}) =
                    Multiplication { kind=kind, sourceA=getAllocatedGenRegOrZero sourceA, sourceM=getAllocatedGenReg sourceM,
                                     sourceN=getAllocatedGenReg sourceN, dest=getAllocatedGenReg dest}

            |   absToConcrete({instr=Division{ isSigned, dest, dividend, divisor, opSize }, ...}) =
                    Division { isSigned=isSigned, dividend=getAllocatedGenReg dividend, divisor=getAllocatedGenReg divisor,
                               dest=getAllocatedGenReg dest, opSize=opSize}

            |   absToConcrete({instr=BeginFunction {regArgs, stackArgs}, ...}) =
                    BeginFunction {regArgs=map (fn (src, dst) => (getAllocatedGenReg src, dst)) regArgs, stackArgs=stackArgs}

            |   absToConcrete({instr=FunctionCall{callKind, regArgs, stackArgs, dest, containers, saveRegs, ...}, ...}) =
                let
                    fun getRegArg(src, dst) = (getAllocatedArg src, dst)
                in
                    FunctionCall{ callKind=callKind, regArgs=map getRegArg regArgs, stackArgs=map getAllocatedArg stackArgs,
                                  dest=getAllocatedGenReg dest, saveRegs=getSaveRegs saveRegs, containers=containers}
                end

            |   absToConcrete({instr=TailRecursiveCall{callKind, regArgs, stackArgs, stackAdjust, currStackSize}, ...}) =
                let
                    fun getRegArg(src, dst) = (getAllocatedArg src, dst)
                    and getStackArg{src, stack} = {src=getAllocatedArg src, stack=stack}
                in
                    TailRecursiveCall{ callKind=callKind, regArgs=map getRegArg regArgs,
                            stackArgs=map getStackArg stackArgs, stackAdjust=stackAdjust, currStackSize=currStackSize}
                end

            |   absToConcrete({instr=ReturnResultFromFunction{resultReg, returnReg, numStackArgs}, ...}) =
                    ReturnResultFromFunction{resultReg=getAllocatedGenReg resultReg, returnReg=getAllocatedGenReg returnReg, numStackArgs=numStackArgs}

            |   absToConcrete({instr=RaiseExceptionPacket{packetReg}, ...}) =
                    RaiseExceptionPacket{packetReg=getAllocatedGenReg packetReg}

            |   absToConcrete({instr=PushToStack{ source, container, copies }, ...}) =
                    PushToStack{source=getAllocatedGenReg source, container=container, copies=copies}

            |   absToConcrete({instr=LoadStack{ dest, container, field, wordOffset}, ...}) =
                    LoadStack{ dest=getAllocatedGenReg dest, container=container, field=field, wordOffset=wordOffset }

            |   absToConcrete({instr=StoreToStack{source, container, field, stackOffset}, ...}) =
                    StoreToStack{source=getAllocatedGenReg source, container=container, field=field, stackOffset=stackOffset}

            |   absToConcrete({instr=ContainerAddress{ dest, container, stackOffset}, ...}) =
                    ContainerAddress{ dest=getAllocatedGenReg dest, container=container, stackOffset=stackOffset }

            |   absToConcrete({instr=ResetStackPtr {numWords}, ...}) = ResetStackPtr {numWords=numWords}

            |   absToConcrete({instr=TagValue{source, dest, isSigned, opSize}, ...}) =
                    TagValue{source=getAllocatedGenReg source, dest=getAllocatedGenReg dest, isSigned=isSigned, opSize=opSize}

            |   absToConcrete({instr=UntagValue{source, dest, isSigned, opSize, ...}, ...}) =
                    UntagValue{source=getAllocatedGenReg source, dest=getAllocatedGenReg dest, isSigned=isSigned, opSize=opSize}

            |   absToConcrete({instr=BoxLarge{source, dest, saveRegs, ...}, ...}) =
                    BoxLarge{source=getAllocatedGenReg source, dest=getAllocatedGenReg dest, saveRegs=getSaveRegs saveRegs}

            |   absToConcrete({instr=UnboxLarge{source, dest}, ...}) =
                    UnboxLarge{source=getAllocatedGenReg source, dest=getAllocatedGenReg dest}

            |   absToConcrete({instr=BoxTagFloat{floatSize, source, dest, saveRegs}, ...}) =
                    BoxTagFloat{floatSize=floatSize, source=getAllocatedFPReg source,
                                dest=getAllocatedGenReg dest, saveRegs=getSaveRegs saveRegs}

            |   absToConcrete({instr=UnboxTagFloat{floatSize, source, dest}, ...}) =
                    UnboxTagFloat{floatSize=floatSize, source=getAllocatedGenReg source, dest=getAllocatedFPReg dest}

            |   absToConcrete({instr=LoadAcquire { base, dest, loadType}, ...}) =
                    LoadAcquire { base=getAllocatedGenReg base, dest=getAllocatedGenReg dest, loadType=loadType}

            |   absToConcrete({instr=StoreRelease { base, source, loadType}, ...}) =
                    StoreRelease{ base=getAllocatedGenReg base, source=getAllocatedGenReg source, loadType=loadType}

            |   absToConcrete({instr=BitFieldShift{source, dest, isSigned, length, immr, imms}, ...}) =
                    BitFieldShift { source=getAllocatedGenReg source, dest=getAllocatedGenReg dest, isSigned=isSigned,
                                    immr=immr, imms=imms, length=length}

            |   absToConcrete({instr=BitFieldInsert{source, destAsSource, dest, length, immr, imms}, ...}) =
                    BitFieldInsert { source=getAllocatedGenReg source, destAsSource=getAllocatedGenReg destAsSource, dest=getAllocatedGenReg dest,
                                     immr=immr, imms=imms, length=length}

            |   absToConcrete({instr=IndexedCaseOperation{testReg}, ...}) =
                    IndexedCaseOperation{testReg=getAllocatedGenReg testReg}

            |   absToConcrete({instr=PushExceptionHandler, ...}) = PushExceptionHandler

            |   absToConcrete({instr=PopExceptionHandler, ...}) = PopExceptionHandler

            |   absToConcrete({instr=BeginHandler{packetReg}, ...}) = BeginHandler{packetReg=getAllocatedGenReg packetReg}

            |   absToConcrete({instr=CompareByteVectors{vec1Addr, vec2Addr, length, ccRef}, ...}) =
                    CompareByteVectors{vec1Addr=getAllocatedGenReg vec1Addr, vec2Addr=getAllocatedGenReg vec2Addr,
                                       length=getAllocatedGenReg length, ccRef=ccRef}

            |   absToConcrete({instr=BlockMove{srcAddr, destAddr, length, isByteMove}, ...}) =
                    BlockMove{srcAddr=getAllocatedGenReg srcAddr, destAddr=getAllocatedGenReg destAddr,
                              length=getAllocatedGenReg length, isByteMove=isByteMove}

            |   absToConcrete({instr=AddSubXSP{source, dest, isAdd}, ...}) =
                    AddSubXSP { source=getAllocatedGenReg source, dest=getAllocatedGenRegOrZero dest, isAdd=isAdd}

            |   absToConcrete({instr=TouchValue{source}, ...}) = TouchValue { source=getAllocatedGenReg source}

            |   absToConcrete({instr=LoadAcquireExclusive{ base, dest }, ...}) =
                    LoadAcquireExclusive { base=getAllocatedGenReg base, dest=getAllocatedGenReg dest}

            |   absToConcrete({instr=StoreReleaseExclusive{ base, source, result }, ...}) =
                    StoreReleaseExclusive{ base=getAllocatedGenReg base, source=getAllocatedGenRegOrZero source,
                                           result=getAllocatedGenReg result}

            |   absToConcrete({instr=MemoryBarrier, ...}) = MemoryBarrier

            |   absToConcrete({instr=ConvertIntToFloat{ source, dest, srcSize, destSize}, ...}) =
                    ConvertIntToFloat{ source=getAllocatedGenReg source, dest=getAllocatedFPReg dest, srcSize=srcSize, destSize=destSize}

            |   absToConcrete({instr=ConvertFloatToInt{ source, dest, srcSize, destSize, rounding}, ...}) =
                    ConvertFloatToInt{ source=getAllocatedFPReg source, dest=getAllocatedGenReg dest, srcSize=srcSize,
                                       destSize=destSize, rounding=rounding}

            |   absToConcrete({instr=UnaryFloatingPt{ source, dest, fpOp}, ...}) =
                    UnaryFloatingPt{ source=getAllocatedFPReg source, dest=getAllocatedFPReg dest, fpOp=fpOp}

            |   absToConcrete({instr=BinaryFloatingPoint{ arg1, arg2, dest, fpOp, opSize}, ...}) =
                    BinaryFloatingPoint{ arg1=getAllocatedFPReg arg1, arg2=getAllocatedFPReg arg2,
                                         dest=getAllocatedFPReg dest, fpOp=fpOp, opSize=opSize}

            |   absToConcrete({instr=CompareFloatingPoint{ arg1, arg2, opSize, ccRef}, ...}) =
                    CompareFloatingPoint{ arg1=getAllocatedFPReg arg1, arg2=getAllocatedFPReg arg2, opSize=opSize, ccRef=ccRef}

        in
            fun concreteBlock(ExtendedBasicBlock{ block, flow, ...}) =
                    BasicBlock{block=map absToConcrete block, flow=flow}
        end

    in
        processBlocks 0;
        (* If the failures list is empty we succeeded. *)
        case !failures of
            [] =>  (* Return the allocation vector.  We may have unused registers, *)
                AllocateSuccess(Vector.map concreteBlock blocks)
            (* Else we'll have to spill something. *)
        |   l => AllocateFailure l
    end

    val nGenRegs = List.length generalRegisters

    structure Sharing =
    struct
        type intSet             = intSet
        and extendedBasicBlock  = extendedBasicBlock
        and ('genReg, 'optGenReg, 'fpReg) basicBlock = ('genReg, 'optGenReg, 'fpReg) basicBlock
        and regProperty         = regProperty
        and reg                 = reg
        and xReg                = xReg
        and vReg                = vReg
        and allocateResult      = allocateResult
    end

end;
