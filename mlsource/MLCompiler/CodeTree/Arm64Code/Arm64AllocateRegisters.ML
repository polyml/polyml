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
        AllocateSuccess of xReg vector
    |   AllocateFailure of intSet list

    (* General registers.  X24 is used as the global heap base in 32-in-64. X30 is the
       return address set by blr but is otherwise a general register.
       Put the argument registers at the end of the list so they'll only be
       used when hinted. *)
    val generalRegisters =
        [X9, X10, X11, X12, X13, X14, X15, X19, X20, X21, X22, X23,
         X0, X1, X2, X3, X4, X5, X6, X7, X8, X30] @ (if is32in64 then [] else [X24])

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
        val realHints = Array.array(maxPRegs, NONE: xReg option)

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

            |   addHints{instr=ReturnResultFromFunction { resultReg=PReg resReg, returnReg=PReg retReg, ... }, ...} =
                (
                    addRealHint(resReg, X0);
                    addRealHint(retReg, X30) (* It may be there from earlier. *)
                )
        
            |   addHints{instr=BeginFunction{regArgs, ...}, ...} =
                    List.app (fn (PReg pr, reg) => addRealHint(pr, reg)) regArgs

            |   addHints{instr=TailRecursiveCall{regArgs, ...}, ...} =
                let
                    fun setHint(ArgInReg(PReg pr), reg) = addRealHint(pr, reg) | setHint _ = ()
                in
                    List.app setHint regArgs
                end
        
            |   addHints{instr=FunctionCall{regArgs, dest=PReg dreg, ...}, ...} =
                let
                    fun setHint(ArgInReg(PReg pr), reg) = addRealHint(pr, reg) | setHint _ = ()
                in
                    addRealHint(dreg, X0);
                    List.app setHint regArgs
                end

            |   addHints{instr=RaiseExceptionPacket{ packetReg=PReg preg }, ...} = addRealHint(preg, X0)

            |   addHints _ = ()
        
        end

        val allocatedRegs = Array.array(maxPRegs, NONE: xReg option)
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
                        raise Fallback "findAReg"; (* Raise an exception for the moment. *)
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

        fun registerAllocate{instr=MoveRegister{source=PReg sreg, dest=PReg dreg}, ...} =
            let
                val realDestReg = findRegister(dreg, NONE, generalRegisters)
            in
                allocateRegister(sreg, SOME realDestReg, generalRegisters)
            end

        |   registerAllocate{instr=LoadNonAddressConstant{dest=PReg dreg, ...}, ...} =
                allocateRegister(dreg, NONE, generalRegisters)

        |   registerAllocate{instr=LoadAddressConstant{dest=PReg dreg, ...}, ...} =
                allocateRegister(dreg, NONE, generalRegisters)

        |   registerAllocate{instr=LoadWithConstantOffset{dest=PReg dreg, base=PReg breg, ...}, ...} =
            (
                allocateRegister(dreg, NONE, generalRegisters);
                allocateRegister(breg, NONE, generalRegisters)
            )

        |   registerAllocate{instr=LoadWithIndexedOffset{dest=PReg dreg, base=PReg breg, index=PReg ireg, ...}, ...} =
            (
                allocateRegister(dreg, NONE, generalRegisters);
                allocateRegister(breg, NONE, generalRegisters);
                allocateRegister(ireg, NONE, generalRegisters)
            )

        |   registerAllocate{instr=ObjectIndexAddressToAbsolute{dest=PReg dreg, source=PReg sreg, ...}, ...} =
            (
                allocateRegister(dreg, NONE, generalRegisters);
                allocateRegister(sreg, NONE, generalRegisters)
            )

        |   registerAllocate{instr=AbsoluteToObjectIndex{dest=PReg dreg, source=PReg sreg, ...}, ...} =
            (
                allocateRegister(dreg, NONE, generalRegisters);
                allocateRegister(sreg, NONE, generalRegisters)
            )

        |   registerAllocate({instr=AllocateMemoryOperation{ dest=PReg dReg, saveRegs, ...}, ...}) =
            (
                List.app(fn (PReg r) => allocateRegister(r, NONE, generalRegisters)) saveRegs;
                allocateRegister(dReg, NONE, generalRegisters)
            )

        |   registerAllocate{instr=StoreWithConstantOffset{source=PReg sreg, base=PReg breg, ...}, ...} =
            (
                allocateRegister(sreg, NONE, generalRegisters);
                allocateRegister(breg, NONE, generalRegisters)
            )

        |   registerAllocate{instr=StoreWithIndexedOffset{source=PReg sreg, base=PReg breg, index=PReg ireg, ...}, ...} =
            (
                allocateRegister(sreg, NONE, generalRegisters);
                allocateRegister(breg, NONE, generalRegisters);
                allocateRegister(ireg, NONE, generalRegisters)
            )

        |   registerAllocate{instr=AddSubImmediate{ source=PReg sreg, dest, ... }, ...} =
            (
                case dest of
                    SOME(PReg dreg) => allocateRegister(dreg, NONE, generalRegisters)
                |   NONE => ();
                allocateRegister(sreg, NONE, generalRegisters)
            )

        |   registerAllocate{instr=AddSubRegister{ operand1=PReg oreg1, operand2=PReg oreg2, dest, ... }, ...} =
            (
                case dest of
                    SOME(PReg dreg) => allocateRegister(dreg, NONE, generalRegisters)
                |   NONE => ();
                allocateRegister(oreg1, NONE, generalRegisters);
                allocateRegister(oreg2, NONE, generalRegisters)
            )

        |   registerAllocate{instr=BeginFunction _, ...} = ()
            (* Any registers that are referenced will have been allocated real registers. *)

        |   registerAllocate({instr=TailRecursiveCall{regArgs=regArgs, stackArgs=stackArgs, ...}, ...}) =
            let
                fun allocateRegArg(ArgInReg(PReg argReg), _) = allocateRegister(argReg, NONE, generalRegisters)
                |   allocateRegArg _ = ()
            in
                (* We've already hinted the arguments but we want to allocate these first
                   to reduce the chance that they'll be used for stack arguments. *)
                List.app allocateRegArg regArgs;
                List.app (fn {src=ArgInReg(PReg argReg), ...} => allocateRegister(argReg, NONE, generalRegisters) | _ => ()) stackArgs
            end

        |   registerAllocate({instr=FunctionCall{regArgs=regArgs, stackArgs=stackArgs, dest=PReg dReg, saveRegs, ...}, ...}) =
            let
                fun allocateRegArg(ArgInReg(PReg argReg), _) = allocateRegister(argReg, NONE, generalRegisters)
                |   allocateRegArg _ = ()
            in
                (* We've already hinted the arguments but we want to allocate these first
                   to reduce the chance that they'll be used for stack arguments. *)
                List.app allocateRegArg regArgs;
                List.app (fn ArgInReg(PReg argReg) => allocateRegister(argReg, NONE, generalRegisters) | _ => ()) stackArgs;
                List.app(fn (PReg r) => allocateRegister(r, NONE, generalRegisters)) saveRegs;
                (* Result will be in X0. *)
                allocateRegister(dReg, SOME X0, [X0])
            end

        |   registerAllocate{instr=ReturnResultFromFunction { resultReg=PReg resReg, returnReg=PReg returnReg, ... }, ...} =
            (
                allocateRegister(resReg, SOME X0, [X0] (* It MUST be in this register *));
                allocateRegister(returnReg, NONE, generalRegisters)
            )

        |   registerAllocate{instr=RaiseExceptionPacket{packetReg=PReg preg}, ...} =
                allocateRegister(preg, NONE, generalRegisters)

        |   registerAllocate{instr=PushToStack{ source=PReg sreg, ... }, ...} =
                allocateRegister(sreg, NONE, generalRegisters)

        |   registerAllocate{instr=LoadStack{ dest=PReg dreg,  ... }, ...} =
                allocateRegister(dreg, NONE, generalRegisters)

        |   registerAllocate{instr=StoreToStack{ source=PReg sreg, ... }, ...} =
                allocateRegister(sreg, NONE, generalRegisters)

        |   registerAllocate{instr=ContainerAddress{ dest=PReg dreg, ... }, ...} =
                allocateRegister(dreg, NONE, generalRegisters)

        |   registerAllocate{instr=ResetStackPtr _, ...} = ()

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

    in
        processBlocks 0;
        (* If the failures list is empty we succeeded. *)
        case !failures of
            [] =>  (* Return the allocation vector.  If a register isn't used replace it with X0. *)
                AllocateSuccess(Vector.tabulate(maxPRegs, fn i => getOpt(Array.sub(allocatedRegs, i), X0)))
            (* Else we'll have to spill something. *)
        |   l => AllocateFailure l
    end

    val nGenRegs = List.length generalRegisters

    structure Sharing =
    struct
        type intSet             = intSet
        and extendedBasicBlock  = extendedBasicBlock
        and regProperty         = regProperty
        and xReg                = xReg
        and allocateResult      = allocateResult
    end

end;
