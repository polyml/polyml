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
    structure IntSet: INTSET
    
    sharing Arm64ICode.Sharing = Identify.Sharing = IntSet
): ARM64ALLOCATEREGISTERS =
struct
    open Arm64ICode
    open Identify
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
    type conflictState =
    {
        conflicts: intSet, realConflicts: reg list
    }
    
    type triple = {instr: iCodeAbstract, current: intSet, active: intSet}

    exception InternalError = Misc.InternalError

    (* Get the conflict sets.  This code was originally part of identifyRegisterState and
       was split off. *)
    fun getConflictStates (blocks: extendedBasicBlock vector, maxPRegs) =
    let
        (* Other registers that conflict with this i.e. cannot share the same
           real register. *)
        val regConflicts = Array.array(maxPRegs, emptySet)
        (* Real registers that cannot be used for this because they are needed for
           an instruction.  Only X30 in calls and RTS traps. *)
        and regRealConflicts = Array.array(maxPRegs, []: reg list)
 
        fun addConflictsTo(addTo, conflicts) =
            List.app(fn aReg => Array.update(regConflicts, aReg, union(Array.sub(regConflicts, aReg), conflicts))) addTo

        (* To reserve a register we need to add the real register to the
           real conflict sets of all the abstract conflicts. *)
        local
            fun isInset reg set = List.exists (fn r => r = reg) set
        in
            fun reserveRegister(reserveFor, reg) =
            let
                fun reserveAReg r =
                let
                    val absConflicts = Array.sub(regConflicts, r)
                    fun addConflict i =
                        if isInset i reserveFor then () else addRealConflict (i, reg)
                in
                    List.app addConflict (setToList absConflicts)
                end
            in
                List.app reserveAReg reserveFor
            end
        
            and addRealConflict (i, reg) =
            let
                val currentConflicts = Array.sub(regRealConflicts, i)
            in
                if isInset reg currentConflicts
                then ()
                else Array.update(regRealConflicts, i, reg :: currentConflicts)
            end
        end
      
        fun conflictsForInstr passThrough {instr, current, ...} =
        let
            val {sources, dests} = getInstructionRegisters instr
            fun regNo(PReg i) = i
            val destRegNos = map regNo dests
            and sourceRegNos = map regNo sources
            val destSet = listToSet destRegNos
            val afterRemoveDests = minus(current, destSet)
            
            local
                (* In almost all circumstances the destination and sources don't
                   conflict and the same register can be used as a destination and
                   a source.  BoxLarge can only store the value after the memory
                   has been allocated.  BitFieldInsert has to copy the "destAsSource"
                   value into the destination so cannot use the same register for
                   the "source". *)
                val postInstruction =
                    case instr of
                        BoxLarge _ => destRegNos @ sourceRegNos
                    |   BoxTagFloat _ => destRegNos @ sourceRegNos (* Not sure about this. *)
                    |   BitFieldInsert{source, ...} => regNo source :: destRegNos
                    |   _ => destRegNos
            in
                (* If there is more than one destination they conflict with each other. *)
                val () = addConflictsTo(postInstruction, listToSet postInstruction);
                (* Mark conflicts for the destinations, i.e. after the instruction.
                   The destinations conflict with the registers that are used
                   subsequently. *)
                val () = addConflictsTo(postInstruction, current);
                val () = addConflictsTo(postInstruction, passThrough);

                (* Mark conflicts for the sources i.e. before the instruction. *)
                (* Sources must be set up as conflicts with each other i.e. when we
                   come to allocate registers we must choose different real registers
                   for different abstract registers. *)
                val () = addConflictsTo(sourceRegNos, listToSet sourceRegNos)
                val () = addConflictsTo(sourceRegNos, afterRemoveDests);
                val () = addConflictsTo(sourceRegNos, passThrough)
            end
            
            (* I'm not sure if this is needed.  There was a check in the old code to ensure that
               different registers were used for loop variables even if they were actually unused.
               This may happen anyway.
               Comment and code copied from X86 version.  Retain it for the moment. *)
            val () =
                case instr of
                    JumpLoop{regArgs, ...} =>
                    let
                        val destRegs = List.foldl(fn ({dst=PReg loopReg, ...}, dests) => loopReg :: dests) [] regArgs
                    in
                        addConflictsTo(destRegs, listToSet destRegs);
                        addConflictsTo(destRegs, current);
                        addConflictsTo(destRegs, passThrough)
                    end
                |   _ => ()

            (* Certain instructions are specific as to the real registers. *)
            val () =
                case instr of
                    ReturnResultFromFunction{ returnReg=PReg retReg, ... } =>
                        (* We're going to put the return value in X0 so we can't use that for
                           the return address. *)
                        addRealConflict(retReg, GenReg X0)

                |   RaiseExceptionPacket{ packetReg } =>
                        (* This wasn't needed previously because we always pushed the registers
                           across an exception. *)
                        reserveRegister([regNo packetReg], GenReg X0)
                
                |   BeginHandler { packetReg, ...} =>
                        reserveRegister([regNo packetReg], GenReg X0)
                
                |   FunctionCall { dest, regArgs, ...} =>
                        (* This is only needed if we are saving the registers rather
                           than marking them as "must push". *)
                    let
                        val destReg = regNo dest
                    in
                        reserveRegister([destReg], GenReg X0);
                        (* The argument registers also conflict.  In order to execute this call we need to load
                           the arguments into specific registers so we can't use them for values that we want after
                           the call.   We use regNo dest here because that will conflict with everything
                           immediately afterwards. *)
                        List.app(fn (_, r) => reserveRegister([destReg], GenReg r)) regArgs;
                        (* Likewise X30 since that's the return address. *)
                        addRealConflict(destReg, GenReg X30)
                    end

                    (* We can't use X30 as the result because it's needed for the return addr if we have to GC. *)
                |   AllocateMemoryFixed{dest=PReg destReg, ...} => addRealConflict(destReg, GenReg X30)
                |   AllocateMemoryVariable{dest=PReg destReg, ...} => addRealConflict(destReg, GenReg X30)

                |   _ => ()
        in
            ()
        end
        
        (* Process the block. *)
        fun conflictsForBlock(ExtendedBasicBlock{block, passThrough, exports, ...}) =
        let
            (* We need to establish conflicts between all the registers active at
               the end of the block since they may not be established elsewhere.
               This isn't necessary for an unconditional branch since the
               same registers will be included in the block that is the target
               of the branch, possibly along with others.  However if this is
               a conditional or indexed branch we may have different sets at
               each of the targets and we have to ensure that all the registers
               differ. *)
            val united = union(exports, passThrough)
            val () = addConflictsTo(setToList united, united)

            val () = List.app (conflictsForInstr passThrough) block
        in
            ()
        end

        val () = Vector.app conflictsForBlock blocks
        
        (* Test - ensure that conflicts are symmetrical. *)
        val () = Array.appi(fn (rno, rconflicts) => List.app (fn a => addConflictsTo([a], listToSet[rno])) (setToList rconflicts)) regConflicts
        
        val conflictState: conflictState vector =
            Vector.tabulate(maxPRegs,
                fn i => {
                    conflicts = Array.sub(regConflicts, i),
                    realConflicts = Array.sub(regRealConflicts, i)
                }
            )
    in
        conflictState
    end

    (* Get the conflict states, allocate registers and return the code with the allocated
       registers if it is successful. *)
    fun allocateRegisters{blocks, regProps=_, maxPRegs, ...} =
    let
        val regStates = getConflictStates(blocks, maxPRegs)
    
        val numBlocks = Vector.length blocks

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
            SOME reg =>
                let
                    val {conflicts, realConflicts, ...} = Vector.sub(regStates, r)
                    (* Find the registers we've already allocated that may conflict. *)
                    val conflictingRegs =
                        List.mapPartial(fn i => if i = r then NONE else Array.sub(allocatedRegs, i)) (setToList conflicts) @
                            realConflicts
                    
                    val _ = if null(!failures) andalso List.exists(fn q => q=reg) conflictingRegs then raise InternalError "bad" else ()
                in
                    reg
                end
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
                    let
                        (* This failed.  We're going to have to spill something. *)
                        val () = failures := conflicts :: ! failures
                        val reg = hd regSet (* Pick a register to satisfy everything. *)
                        val () = Array.update(allocatedRegs, r, SOME reg)
                    in
                        reg
                    end
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

        (* Turn the abstract icode into a concrete version by allocating the registers. *)
        local
            fun getAllocatedGenReg(PReg r) =
                case findRegister(r, NONE, generalRegisters) of GenReg reg => reg | _ => raise InternalError "getAllocatedGenReg"
            and getAllocatedFPReg(PReg r) =
                case findRegister(r, NONE, floatingPtRegisters) of FPReg reg => reg | _ => raise InternalError "getAllocatedFPReg"

            fun getAllocatedGenRegOrZero ZeroReg = XZero
            |   getAllocatedGenRegOrZero (SomeReg reg) = getAllocatedGenReg reg

            fun getAllocatedArg(ArgInReg reg) = ArgInReg(getAllocatedGenReg reg)
            |   getAllocatedArg(ArgOnStack stackLoc) = ArgOnStack stackLoc

            val getSaveRegs = List.map getAllocatedGenReg

            (* Reversed cons and append to make the code easier to read. *)
            infix 5 <::> <@>
            fun tl <::> hd = hd :: tl
            (*and snd <@> fst = fst @ snd*)

            fun absToConcrete([], context, code) = (context, code)

            |   absToConcrete({instr=MoveRegister{ source, dest as PReg dReg}, ...} :: rest, context, code) =
                let
                    (* Try to use the register we've allocated for the source as the destination so
                       that we can eliminate this instruction altogether. *)
                    val sourceReg = getAllocatedGenReg source
                    val () = allocateRegister(dReg, SOME(GenReg sourceReg), generalRegisters)
                in
                    absToConcrete(rest, context, code <::> MoveRegister { source=sourceReg, dest=getAllocatedGenReg dest})
                end

            |   absToConcrete({instr=LoadNonAddressConstant { dest, source}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> LoadNonAddressConstant { dest=getAllocatedGenReg dest, source=source})

            |   absToConcrete({instr=LoadAddressConstant { dest, source}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> LoadAddressConstant { dest=getAllocatedGenReg dest, source=source})

            |   absToConcrete({instr=LoadWithConstantOffset { base, dest, byteOffset, loadType}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        LoadWithConstantOffset { base=getAllocatedGenReg base, dest=getAllocatedGenReg dest, byteOffset=byteOffset, loadType=loadType})

            |   absToConcrete({instr=LoadFPWithConstantOffset { base, dest, byteOffset, floatSize}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        LoadFPWithConstantOffset { base=getAllocatedGenReg base, dest=getAllocatedFPReg dest, byteOffset=byteOffset, floatSize=floatSize})

            |   absToConcrete({instr=LoadWithIndexedOffset { base, dest, index, loadType}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        LoadWithIndexedOffset { base=getAllocatedGenReg base, dest=getAllocatedGenReg dest, index=getAllocatedGenReg index, loadType=loadType})

            |   absToConcrete({instr=LoadFPWithIndexedOffset { base, dest, index, floatSize}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        LoadFPWithIndexedOffset { base=getAllocatedGenReg base, dest=getAllocatedFPReg dest, index=getAllocatedGenReg index, floatSize=floatSize})

            |   absToConcrete({instr=GetThreadId { dest}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> GetThreadId { dest=getAllocatedGenReg dest})

            |   absToConcrete({instr=ObjectIndexAddressToAbsolute { source, dest}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> ObjectIndexAddressToAbsolute { source=getAllocatedGenReg source, dest=getAllocatedGenReg dest})

            |   absToConcrete({instr=AbsoluteToObjectIndex { source, dest}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> AbsoluteToObjectIndex { source=getAllocatedGenReg source, dest=getAllocatedGenReg dest})

            |   absToConcrete({instr=AllocateMemoryFixed { bytesRequired, dest, saveRegs }, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        AllocateMemoryFixed { dest=getAllocatedGenReg dest, bytesRequired=bytesRequired, saveRegs=getSaveRegs saveRegs})

            |   absToConcrete({instr=AllocateMemoryVariable{size, dest, saveRegs}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        AllocateMemoryVariable{size=getAllocatedGenReg size, dest=getAllocatedGenReg dest, saveRegs=getSaveRegs saveRegs})

            |   absToConcrete({instr=InitialiseMem{size, addr, init}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> InitialiseMem{size=getAllocatedGenReg size, addr=getAllocatedGenReg addr, init=getAllocatedGenReg init})

            |   absToConcrete({instr=BeginLoop, ...} :: rest, context, code) = absToConcrete(rest, context, code <::> BeginLoop)

            |   absToConcrete({instr=JumpLoop{regArgs, stackArgs, checkInterrupt}, ...} :: rest, context, code) =
                let
                    fun getStackArg{src, wordOffset, stackloc} =
                            {src=getAllocatedArg src, wordOffset=wordOffset, stackloc=stackloc}
                    and getRegArg{src, dst} = {src=getAllocatedArg src, dst=getAllocatedGenReg dst}
                in
                    absToConcrete(rest, context, code <::> JumpLoop{ regArgs=map getRegArg regArgs, stackArgs=map getStackArg stackArgs,
                              checkInterrupt=Option.map getSaveRegs checkInterrupt})
                end

            |   absToConcrete({instr=StoreWithConstantOffset { base, source, byteOffset, loadType}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        StoreWithConstantOffset{ base=getAllocatedGenReg base, source=getAllocatedGenReg source, byteOffset=byteOffset, loadType=loadType})

            |   absToConcrete({instr=StoreFPWithConstantOffset { base, source, byteOffset, floatSize}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        StoreFPWithConstantOffset{ base=getAllocatedGenReg base, source=getAllocatedFPReg source, byteOffset=byteOffset, floatSize=floatSize})

            |   absToConcrete({instr=StoreWithIndexedOffset { base, source, index, loadType}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        StoreWithIndexedOffset{ base=getAllocatedGenReg base, source=getAllocatedGenReg source, index=getAllocatedGenReg index, loadType=loadType})

            |   absToConcrete({instr=StoreFPWithIndexedOffset { base, source, index, floatSize}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        StoreFPWithIndexedOffset{ base=getAllocatedGenReg base, source=getAllocatedFPReg source, index=getAllocatedGenReg index, floatSize=floatSize})

            |   absToConcrete({instr=AddSubImmediate{ source, dest, ccRef, immed, isAdd, length }, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        AddSubImmediate { source=getAllocatedGenReg source, dest=getAllocatedGenRegOrZero dest, ccRef=ccRef,
                                                 immed=immed, isAdd=isAdd, length=length})

            |   absToConcrete({instr=AddSubRegister{ base, shifted, dest, ccRef, isAdd, length, shift}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        AddSubRegister { base=getAllocatedGenReg base, shifted=getAllocatedGenReg shifted,
                                     dest=getAllocatedGenRegOrZero dest, ccRef=ccRef,
                                     isAdd=isAdd, length=length, shift=shift})

            |   absToConcrete({instr=LogicalImmediate{ source, dest, ccRef, immed, logOp, length }, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        LogicalImmediate { source=getAllocatedGenReg source, dest=getAllocatedGenRegOrZero dest, ccRef=ccRef,
                                                  immed=immed, logOp=logOp, length=length})

            |   absToConcrete({instr=LogicalRegister{ base, shifted, dest, ccRef, logOp, length, shift}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        LogicalRegister { base=getAllocatedGenReg base, shifted=getAllocatedGenReg shifted,
                                      dest=getAllocatedGenRegOrZero dest, ccRef=ccRef,
                                      logOp=logOp, length=length, shift=shift})

            |   absToConcrete({instr=ShiftRegister{ direction, dest, source, shift, opSize}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        ShiftRegister { source=getAllocatedGenReg source, shift=getAllocatedGenReg shift, dest=getAllocatedGenReg dest,
                                               direction=direction, opSize=opSize})

            |   absToConcrete({instr=Multiplication{ kind, dest, sourceA, sourceM, sourceN }, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        Multiplication { kind=kind, sourceA=getAllocatedGenRegOrZero sourceA, sourceM=getAllocatedGenReg sourceM,
                                     sourceN=getAllocatedGenReg sourceN, dest=getAllocatedGenReg dest})

            |   absToConcrete({instr=Division{ isSigned, dest, dividend, divisor, opSize }, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        Division { isSigned=isSigned, dividend=getAllocatedGenReg dividend, divisor=getAllocatedGenReg divisor,
                               dest=getAllocatedGenReg dest, opSize=opSize})

            |   absToConcrete({instr=BeginFunction {regArgs, stackArgs}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        BeginFunction {regArgs=map (fn (src, dst) => (getAllocatedGenReg src, dst)) regArgs, stackArgs=stackArgs})

            |   absToConcrete({instr=FunctionCall{callKind, regArgs, stackArgs, dest, containers, saveRegs, ...}, ...} :: rest, context, code) =
                let
                    fun getRegArg(src, dst) = (getAllocatedArg src, dst)
                in
                    absToConcrete(rest, context, code <::>
                        FunctionCall{ callKind=callKind, regArgs=map getRegArg regArgs, stackArgs=map getAllocatedArg stackArgs,
                                  dest=getAllocatedGenReg dest, saveRegs=getSaveRegs saveRegs, containers=containers})
                end

            |   absToConcrete({instr=TailRecursiveCall{callKind, regArgs, stackArgs, stackAdjust, currStackSize}, ...} :: rest, context, code) =
                let
                    fun getRegArg(src, dst) = (getAllocatedArg src, dst)
                    and getStackArg{src, stack} = {src=getAllocatedArg src, stack=stack}
                in
                    absToConcrete(rest, context, code <::>
                        TailRecursiveCall{ callKind=callKind, regArgs=map getRegArg regArgs,
                            stackArgs=map getStackArg stackArgs, stackAdjust=stackAdjust, currStackSize=currStackSize})
                end

            |   absToConcrete({instr=ReturnResultFromFunction{resultReg, returnReg, numStackArgs}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        ReturnResultFromFunction{resultReg=getAllocatedGenReg resultReg, returnReg=getAllocatedGenReg returnReg, numStackArgs=numStackArgs})

            |   absToConcrete({instr=RaiseExceptionPacket{packetReg}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> RaiseExceptionPacket{packetReg=getAllocatedGenReg packetReg})

            |   absToConcrete({instr=PushToStack{ source, container, copies }, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> PushToStack{source=getAllocatedGenReg source, container=container, copies=copies})

            |   absToConcrete({instr=LoadStack{ dest, container, field, wordOffset}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        LoadStack{ dest=getAllocatedGenReg dest, container=container, field=field, wordOffset=wordOffset })

            |   absToConcrete({instr=StoreToStack{source, container, field, stackOffset}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        StoreToStack{source=getAllocatedGenReg source, container=container, field=field, stackOffset=stackOffset})

            |   absToConcrete({instr=ContainerAddress{ dest, container, stackOffset}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        ContainerAddress{ dest=getAllocatedGenReg dest, container=container, stackOffset=stackOffset })

            |   absToConcrete({instr=ResetStackPtr {numWords}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> ResetStackPtr {numWords=numWords})

            |   absToConcrete({instr=TagValue{source, dest, isSigned, opSize}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        TagValue{source=getAllocatedGenReg source, dest=getAllocatedGenReg dest, isSigned=isSigned, opSize=opSize})

            |   absToConcrete({instr=UntagValue{source, dest, isSigned, opSize, ...}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        UntagValue{source=getAllocatedGenReg source, dest=getAllocatedGenReg dest, isSigned=isSigned, opSize=opSize})

            |   absToConcrete({instr=BoxLarge{source, dest, saveRegs, ...}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        BoxLarge{source=getAllocatedGenReg source, dest=getAllocatedGenReg dest, saveRegs=getSaveRegs saveRegs})

            |   absToConcrete({instr=UnboxLarge{source, dest}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> UnboxLarge{source=getAllocatedGenReg source, dest=getAllocatedGenReg dest})

            |   absToConcrete({instr=BoxTagFloat{floatSize, source, dest, saveRegs}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> BoxTagFloat{floatSize=floatSize, source=getAllocatedFPReg source,
                                dest=getAllocatedGenReg dest, saveRegs=getSaveRegs saveRegs})

            |   absToConcrete({instr=UnboxTagFloat{floatSize, source, dest}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> UnboxTagFloat{floatSize=floatSize, source=getAllocatedGenReg source, dest=getAllocatedFPReg dest})

            |   absToConcrete({instr=LoadAcquire { base, dest, loadType}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> LoadAcquire { base=getAllocatedGenReg base, dest=getAllocatedGenReg dest, loadType=loadType})

            |   absToConcrete({instr=StoreRelease { base, source, loadType}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> StoreRelease{ base=getAllocatedGenReg base, source=getAllocatedGenReg source, loadType=loadType})

            |   absToConcrete({instr=BitFieldShift{source, dest, isSigned, length, immr, imms}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> BitFieldShift { source=getAllocatedGenReg source, dest=getAllocatedGenReg dest, isSigned=isSigned,
                                    immr=immr, imms=imms, length=length})

            |   absToConcrete({instr=BitFieldInsert{source, destAsSource, dest, length, immr, imms}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        BitFieldInsert { source=getAllocatedGenReg source, destAsSource=getAllocatedGenReg destAsSource, dest=getAllocatedGenReg dest,
                                     immr=immr, imms=imms, length=length})

            |   absToConcrete({instr=IndexedCaseOperation{testReg}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> IndexedCaseOperation{testReg=getAllocatedGenReg testReg})

            |   absToConcrete({instr=PushExceptionHandler, ...} :: rest, context, code) = absToConcrete(rest, context, code <::> PushExceptionHandler)

            |   absToConcrete({instr=PopExceptionHandler, ...} :: rest, context, code) = absToConcrete(rest, context, code <::> PopExceptionHandler)

            |   absToConcrete({instr=BeginHandler{packetReg}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> BeginHandler{packetReg=getAllocatedGenReg packetReg})

            |   absToConcrete({instr=CompareByteVectors{vec1Addr, vec2Addr, length, ccRef}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> CompareByteVectors{vec1Addr=getAllocatedGenReg vec1Addr, vec2Addr=getAllocatedGenReg vec2Addr,
                                       length=getAllocatedGenReg length, ccRef=ccRef})

            |   absToConcrete({instr=BlockMove{srcAddr, destAddr, length, isByteMove}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> BlockMove{srcAddr=getAllocatedGenReg srcAddr, destAddr=getAllocatedGenReg destAddr,
                              length=getAllocatedGenReg length, isByteMove=isByteMove})

            |   absToConcrete({instr=AddSubXSP{source, dest, isAdd}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> AddSubXSP { source=getAllocatedGenReg source, dest=getAllocatedGenRegOrZero dest, isAdd=isAdd})

            |   absToConcrete({instr=TouchValue{source}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> TouchValue { source=getAllocatedGenReg source})

            |   absToConcrete({instr=LoadAcquireExclusive{ base, dest }, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> LoadAcquireExclusive { base=getAllocatedGenReg base, dest=getAllocatedGenReg dest})

            |   absToConcrete({instr=StoreReleaseExclusive{ base, source, result }, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> StoreReleaseExclusive{ base=getAllocatedGenReg base, source=getAllocatedGenRegOrZero source,
                                           result=getAllocatedGenReg result})

            |   absToConcrete({instr=MemoryBarrier, ...} :: rest, context, code) = absToConcrete(rest, context, code <::> MemoryBarrier)

            |   absToConcrete({instr=ConvertIntToFloat{ source, dest, srcSize, destSize}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        ConvertIntToFloat{ source=getAllocatedGenReg source, dest=getAllocatedFPReg dest, srcSize=srcSize, destSize=destSize})

            |   absToConcrete({instr=ConvertFloatToInt{ source, dest, srcSize, destSize, rounding}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        ConvertFloatToInt{ source=getAllocatedFPReg source, dest=getAllocatedGenReg dest, srcSize=srcSize,
                                       destSize=destSize, rounding=rounding})

            |   absToConcrete({instr=UnaryFloatingPt{ source, dest, fpOp}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> UnaryFloatingPt{ source=getAllocatedFPReg source, dest=getAllocatedFPReg dest, fpOp=fpOp})

            |   absToConcrete({instr=BinaryFloatingPoint{ arg1, arg2, dest, fpOp, opSize}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> BinaryFloatingPoint{ arg1=getAllocatedFPReg arg1, arg2=getAllocatedFPReg arg2,
                                         dest=getAllocatedFPReg dest, fpOp=fpOp, opSize=opSize})

            |   absToConcrete({instr=CompareFloatingPoint{ arg1, arg2, opSize, ccRef}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        CompareFloatingPoint{ arg1=getAllocatedFPReg arg1, arg2=getAllocatedFPReg arg2, opSize=opSize, ccRef=ccRef})

        in
            fun concreteBlock(ExtendedBasicBlock{ block, flow, ...}) =
            let
                val ((), code) = absToConcrete(block, (), [])
            in
                BasicBlock{block=List.rev code, flow=flow}
            end
        end

        val resultArray = Array.array(numBlocks, NONE)

        fun processBlocks blockNo =
        case Array.sub(resultArray, blockNo) of
            SOME _ => () (* Done . *)
        |   NONE =>
            let
                val thisBlock as ExtendedBasicBlock { flow, block, ...} = Vector.sub(blocks, blockNo)
            in
                (* Add the hints for this block before the actual allocation of registers. *)
                List.app addHints block;
                (* Process this block and add it to the results. *)
                Array.update(resultArray, blockNo, SOME(concreteBlock thisBlock));
            
                (* Now the blocks that depend on this. *)
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
            end
    in
        processBlocks 0;
        (* If the failures list is empty we succeeded. *)
        case !failures of
            [] =>  (* Return the allocation vector.  We may have unused registers, *)
                AllocateSuccess(Vector.tabulate(numBlocks, fn i => valOf(Array.sub(resultArray, i))) )
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
