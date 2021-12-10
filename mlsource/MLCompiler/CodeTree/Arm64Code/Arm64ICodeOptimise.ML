(*
    Copyright David C. J. Matthews 2021

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

functor Arm64ICodeOptimise(
    structure Arm64ICode: ARM64ICODE
    structure IntSet: INTSET
    structure Identify: ARM64IDENTIFYREFERENCES
    structure Debug: DEBUG
    structure Pretty: PRETTY
    sharing Arm64ICode.Sharing = Identify.Sharing = IntSet = Arm64ICode
): ARM64ICODEOPTIMISE =
struct
    open Arm64ICode
    open IntSet
    open Identify

    datatype optimise = Changed of basicBlock vector * regProperty vector | Unchanged
    
    exception InternalError = Misc.InternalError
    
    (* Optimiser.
       This could incorporate optimisations done elsewhere.
       IdentifyReferences currently removes instructions that
       produce results in registers that are never used.

       PushRegisters deals with caching. Caching involves
       speculative changes that can be reversed if there is a need
       to spill registers.
       
       The optimiser currently deals with booleans and conditions.
    *)
    
    (* This is a rewrite of the last instruction to set a boolean.
       This is almost always rewriting the next instruction.  The only
       possibility is that we have a ResetStackPtr in between. *)
    datatype boolRegRewrite =
        BRNone
        (* BRSetConditionToConstant - we have a comparison of two constant value.
           This will usually happen because we've duplicated a branch and
           set a register to a constant which we then compare. *)
    |   BRSetConditionToConstant of
            { srcCC: ccRef, signedCompare: order, unsignedCompare: order }

    fun optimiseICode{ code, pregProps, ccCount=_, debugSwitches=_ } =
    let
        val hasChanged = ref false
        val regCounter = ref(Vector.length pregProps)
        val regList = ref []
        fun newReg kind =
        (
            regList := kind :: ! regList;
            PReg (!regCounter)
        ) before regCounter := !regCounter + 1

        datatype regValue = ARegister of preg | AConstant of Word64.word

        (* If this argument is a register and the register is mapped to a constant
           or another register replace the value.  Unlike the X86 version we
           don't map memory locations but we do map registers. *)
        (* TODO: This is potentially quadratic if we have a long piece
           of code with very many registers. *)
        fun getRegisterValue(preg as PReg pregNo, kill, regMap) =
        (
            case List.find(fn {dest, ... } => dest = preg) regMap of
                SOME { source, ...} =>
                (
                    source,
                    (* Filter it if it is the last reference. *)
                    if member(pregNo, kill)
                    then List.filter(fn {dest, ...} => dest <> preg) regMap
                    else regMap
                )
            |   NONE => (ARegister preg, regMap)
        )

        fun optimiseBlock processed (block, flow, outCCState) =
        let
            fun optCode([], brCond, regMap, code) = (code, brCond, regMap)

            |   optCode({instr=AddSubImmediate{source, dest=NONE, ccRef=SOME ccRefOut, immed, isAdd=false, length}, kill, ...} :: rest,
                        _, regMap, code) =
                let
                    val (repArg1, memRefsOut) = getRegisterValue(source, kill, regMap)
                in
                    case repArg1 of
                        AConstant test =>
                        (* AddSubImmediate is put in by CodetreeToIcode to test a boolean value.  It can also
                           arise as the result of pattern matching on booleans or even by tests such as = true.
                           If the source register is now a constant we want to propagate the constant
                           condition. *)
                        let
                            (* This comparison reduces to a constant.  *)
                            val _ = hasChanged := true
                            (* Signed comparison.  If this is a 32-bit operation the top word
                               could be zero so we need to convert this as Word32.
                               immediate values are always unsigned. *)
                            val testValue =
                                case length of
                                    OpSize64 => Word64.toLargeIntX test
                                |   OpSize32 => Word32.toLargeIntX(Word32.fromLarge test)
                            (* Put in a replacement so that if we were previously testing ccRefOut
                               we should instead test ccRef. *)
                            val repl =
                                BRSetConditionToConstant{srcCC=ccRefOut,
                                    signedCompare=LargeInt.compare(testValue, Word.toLargeInt immed),
                                    unsignedCompare=Word64.compare(test, Word64.fromLarge(Word.toLargeWord immed))}
                            val _ = isSome outCCState andalso raise InternalError "optCode: CC exported"
                        in
                            optCode(rest, repl, memRefsOut, code)
                        end
                
                    |   ARegister repArg1 =>
                            optCode(rest, BRNone, memRefsOut,
                                AddSubImmediate{source=repArg1, dest=NONE, ccRef=SOME ccRefOut, immed=immed, isAdd=false, length=length} ::code)
                end

            |   optCode({instr as AddSubRegister{base, shifted, dest, ccRef, isAdd, length, shift}, kill, ...} :: rest,
                        _, regMap, code) =
                let
                    (* If we have a constant as the second argument we can change this to the
                       immediate form. *)
                    val (repOp1, mapOp1) = getRegisterValue(base, kill, regMap)
                    val (repOp2, mapOp2) = getRegisterValue(shifted, kill, mapOp1)
                in
                    case repOp2 of
                        AConstant op2Const =>
                        let
                            val shiftedConst =
                                case shift of
                                    ShiftNone => op2Const
                                |   _ => 0w4096 (* This doesn't currently happen. *)
                        in
                            if shiftedConst < 0w4096
                            then optCode(rest, BRNone, mapOp2,
                                AddSubImmediate{source=base, dest=dest, ccRef=ccRef,
                                    immed=Word.fromLargeWord(Word64.toLargeWord shiftedConst), isAdd=isAdd, length=length} :: code)
                            else optCode(rest, BRNone, mapOp2, instr :: code)
                        end
                    |   ARegister repReg =>
                            optCode(rest, BRNone, mapOp2,
                                AddSubRegister{base=base, shifted=repReg, dest=dest, ccRef=ccRef, isAdd=isAdd, length=length, shift=shift} :: code)
                end

            |   optCode({instr=MoveRegister{dest, source}, kill, ...} :: rest, inCond, regMap, code) =
                let
                    val (repSource, mapAfterReplace) = getRegisterValue(source, kill, regMap)
                    val newSource =
                        case repSource of ARegister repReg => repReg | AConstant _ => source
                in
                    (* If the source is mapped to a different register use that otherwise
                       leave it unchanged.  Add this to the map.
                       The X86 version replaces the source here but it's probably better to
                       leave this as a register move. *)
                    optCode(rest, inCond, {dest=dest, source=repSource} :: mapAfterReplace,
                        MoveRegister{dest=dest, source=newSource}::code)
                end

            |   optCode({instr as LoadNonAddressConstant{dest, source}, ...} :: rest, inCond, regMap, code) =
                let
                    (* If we already have a register with this constant we would probably
                       be better off reusing it.  The map, though, needs to indicate that the
                       destination register contains the constant.
                       The X86 version always uses a load-constant here. *)
                    val newInstr =
                        case List.find(fn {source=AConstant c, ... } => c = source | _ => false) regMap of
                            SOME{dest=cDest, ...} => MoveRegister{dest=dest, source=cDest}
                        |   NONE => instr
                in
                    optCode(rest, inCond, {dest=dest, source=AConstant source} :: regMap, newInstr :: code)
                end

                (* TODO: It could be worth rewriting every instruction with any replaced source register. *)
            |   optCode({instr, ...} :: rest, inCond, regMap, code) =
                let
                    (* If this instruction affects the CC the cached SetToCondition will no longer be valid. *)
                    val afterCond =
                        case getInstructionCC instr of
                            CCUnchanged => inCond
                        |   _ => BRNone
                in
                    optCode(rest, afterCond, regMap, instr::code)
                end

            val (blkCode, finalRepl, finalMap) = optCode(block, BRNone, [], processed)
        in
            case (flow, finalRepl) of
                (* We have a Condition and a change to the condition. *)
                (flow as Conditional{ccRef, condition, trueJump, falseJump},
                 BRSetConditionToConstant({srcCC, signedCompare, unsignedCompare, ...})) =>
                    if srcCC = ccRef
                    then
                    let
                        val testResult =
                            case (condition, signedCompare, unsignedCompare) of
                                (CondEqual,             EQUAL,  _)   => true
                            |   (CondEqual,             _,      _)   => false
                            |   (CondNotEqual,          EQUAL,  _)   => false
                            |   (CondNotEqual,          _,      _)   => true
                            |   (CondSignedLess,        LESS,   _)   => true
                            |   (CondSignedLess,        _,      _)   => false
                            |   (CondSignedGreater,     GREATER,_)   => true
                            |   (CondSignedGreater,     _,      _)   => false
                            |   (CondSignedLessEq,      GREATER,_)   => false
                            |   (CondSignedLessEq,      _,      _)   => true
                            |   (CondSignedGreaterEq,   LESS,   _)   => false
                            |   (CondSignedGreaterEq,   _,      _)   => true
                            |   (CondCarryClear,        _, LESS  )   => true
                            |   (CondCarryClear,        _,      _)   => false
                            |   (CondUnsignedHigher,    _,GREATER)   => true
                            |   (CondUnsignedHigher,    _,      _)   => false
                            |   (CondUnsignedLowOrEq,    _,GREATER)   => false
                            |   (CondUnsignedLowOrEq,    _,      _)   => true
                            |   (CondCarrySet,          _, LESS  )   => false
                            |   (CondCarrySet,          _,      _)   => true
                                (* The overflow and parity checks should never occur. *)
                            |   _   => raise InternalError "getCondResult: comparison"

                        val newFlow =
                            if testResult
                            then Unconditional trueJump
                            else Unconditional falseJump

                        val() = hasChanged := true
                    in
                        BasicBlock{flow=newFlow, block=List.rev blkCode}
                    end
                    else BasicBlock{flow=flow, block=List.rev blkCode}
              
            |   (flow as Unconditional jmp, _) =>
                let
                    val ExtendedBasicBlock{block=targetBlck, locals, exports, flow=targetFlow, outCCState=targetCC, ...} =
                        Vector.sub(code, jmp)
                    (* If the target is empty or is simply one or more Resets or a Return we're
                       better off merging this in rather than doing the jump.  We allow a single
                       Load  e.g. when loading a constant or moving a register.
                       If we have a CompareLiteral and we're comparing with a register in the map
                       that has been set to a constant we include that because the comparison will
                       then be reduced to a constant. *)
                    fun isSimple([], _, _) = true
                    |   isSimple ({instr=ResetStackPtr _, ...} :: instrs, moves, regMap) = isSimple(instrs, moves, regMap)
                    |   isSimple ({instr=ReturnResultFromFunction _, ...} :: instrs, moves, regMap) = isSimple(instrs, moves, regMap)
                    |   isSimple ({instr=RaiseExceptionPacket _, ...} :: instrs, moves, regMap) = isSimple(instrs, moves, regMap)
                    |   isSimple ({instr=MoveRegister{source, dest}, ...} :: instrs, moves, regMap) =
                        let
                            (* We frequently have a move of the original register into a new register before the test. *)
                            val newMap =
                                case List.find(fn {dest, ... } => dest = source) regMap of
                                    SOME {source, ...} => {dest=dest, source=source} :: regMap
                                |   NONE => regMap
                        in
                            moves = 0 andalso isSimple(instrs, moves+1, newMap)
                        end
                    |   isSimple ({instr=LoadNonAddressConstant _, ...} :: instrs, moves, regMap) =
                            moves = 0 andalso isSimple(instrs, moves+1, regMap)
                    |   isSimple ({instr=LoadAddressConstant _, ...} :: instrs, moves, regMap) =
                            moves = 0 andalso isSimple(instrs, moves+1, regMap)
                    |   isSimple ({instr=AddSubImmediate{source, dest=NONE, ...}, ...} :: instrs, moves, regMap) =
                        let
                            val isReplace = List.find(fn {dest, ... } => dest = source) regMap
                        in
                            case isReplace of
                                SOME {source=AConstant _, ...} => isSimple(instrs, moves, regMap)
                            |   _ => false
                        end
                    |   isSimple _ = false
 
                in
                    (* Merge trivial blocks.  This previously also tried to merge non-trivial blocks if
                       they only had one reference but this ends up duplicating non-trivial code.  If we
                       have a trivial block that has multiple references but is the only reference to
                       a non-trivial block we can merge the non-trivial block into it.  That would
                       be fine except that at the same time we may merge this trivial block elsewhere. *)
                    (* The restriction that a block must only export "merge" registers is unfortunate
                       but necessary to avoid the situation where a non-merge register is defined at
                       multiple points and cannot be pushed to the stack.  This really isn't an issue
                       with blocks with unconditional branches but there are cases where we have
                       successive tests of the same condition and that results in local registers
                       being defined and then exported.  This occurs in, for example,
                       fun f x = if x > "abcde" then "yes" else "no"; *)
                    if isSimple(targetBlck, 0, finalMap) andalso
                            List.all (fn i => Vector.sub(pregProps, i) = RegPropMultiple) (setToList exports)
                    then
                    let
                        (* Copy the block, creating new registers for the locals. *)
                        val localMap = List.map (fn r => (PReg r, newReg(Vector.sub(pregProps, r)))) (setToList locals)
                        fun mapReg r = case List.find (fn (s, _) => r = s) localMap of SOME(_, s) => s | NONE => r
                        fun mapInstr(instr as ResetStackPtr _) = instr
                        |   mapInstr(ReturnResultFromFunction{resultReg, returnReg, numStackArgs}) =
                                ReturnResultFromFunction{resultReg=mapReg resultReg, returnReg=mapReg returnReg, numStackArgs=numStackArgs}
                        |   mapInstr(RaiseExceptionPacket{packetReg}) =
                                RaiseExceptionPacket{packetReg=mapReg packetReg}
                        |   mapInstr(MoveRegister{source, dest}) =
                                MoveRegister{source=mapReg source, dest=mapReg dest}
                        |   mapInstr(LoadNonAddressConstant{source, dest}) =
                                LoadNonAddressConstant{source=source, dest=mapReg dest}
                        |   mapInstr(LoadAddressConstant{source, dest}) =
                                LoadAddressConstant{source=source, dest=mapReg dest}
                        |   mapInstr(AddSubImmediate{source, dest=NONE, immed, ccRef, isAdd, length}) =
                                AddSubImmediate{source=mapReg source, dest=NONE, immed=immed, ccRef=ccRef, isAdd=isAdd, length=length}
                        |   mapInstr _ = raise InternalError "mapInstr: other instruction"
                        fun mapRegNo i = case(mapReg(PReg i)) of PReg r => r
                        (* Map the instructions and the sets although we only use the kill set. *)
                        fun mapCode{instr, current, active, kill} =
                            {instr=mapInstr instr, current=listToSet(map mapRegNo (setToList current)),
                             active=listToSet(map mapRegNo (setToList active)), kill=listToSet(map mapRegNo (setToList kill))}
                    in
                        hasChanged := true;
                        optimiseBlock blkCode(map mapCode targetBlck, targetFlow, targetCC)
                    end
                    else BasicBlock{flow=flow, block=List.rev blkCode}
                end

            |   (flow, _) => BasicBlock{flow=flow, block=List.rev blkCode}
        end
        
        fun optBlck(ExtendedBasicBlock{block, flow, outCCState, ...}) = optimiseBlock [] (block, flow, outCCState)
        val resVector = Vector.map optBlck code
    in
        if !hasChanged
        then
        let
            val extraRegs = List.rev(! regList)
            val props =
                if null extraRegs
                then pregProps
                else Vector.concat[pregProps, Vector.fromList extraRegs]
        in
            Changed(resVector, props)
        end
        else Unchanged
    end

    structure Sharing =
    struct
        type extendedBasicBlock = extendedBasicBlock
        and basicBlock = basicBlock
        and regProperty = regProperty
        and optimise = optimise
    end
end;
