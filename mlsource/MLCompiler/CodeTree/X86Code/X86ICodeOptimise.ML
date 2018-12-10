(*
    Copyright David C. J. Matthews 2018

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

functor X86ICodeOptimise(
    structure ICODE: ICodeSig
    structure INTSET: INTSETSIG
    structure IDENTIFY: X86IDENTIFYREFSSIG
    structure X86CODE: X86CODESIG (* For invertTest. *)
    sharing ICODE.Sharing = IDENTIFY.Sharing = INTSET = X86CODE
): X86ICODEOPTSIG =
struct
    open ICODE
    open INTSET
    open IDENTIFY
    val InternalError = Misc.InternalError

    datatype optimise = Changed of basicBlock vector | Unchanged
    
    (* Optimiser.
       This could incorporate optimisations done elsewhere.
       IdentifyReferences currently removes instructions that
       produce results in registers that are never used.

       PushRegisters deals with caching. Caching involves
       speculative changes that can be reversed if there is a need
       to spill registers.
       
       The optimiser currently deals with booleans and conditions
       and with moving memory loads into an instruction operand.
    *)
    
    (* This is a rewrite of the last instruction to set a boolean.
       This is almost always rewriting the next instruction.  The only
       possibility is that we have a ResetStackPtr in between. *)
    datatype boolRegRewrite =
        BRNone
    |   BRSetCond of { dest: preg, test: changeArg }
    |   BRNot of { dest: preg, source: preg }
        (* This follows a comparison with a constant where the value we're comparing came from a BRSetCond or
           a load of a short constant.  It's complicated because we include comparisons with constants and
           also have the possibility of comparing with a value other than tagged 0 or tagged 1 and having
           ordering rather than just equality. *)
    |   BRChangeCond of
            { srcCC: ccRef, compare: LargeInt.int, test: changeArg }

    and changeArg =
        CASetCond of { condition: branchOps, outCC: ccRef, highOrder: preg } (*i.e. from SetCond *)
    |   CAConst of LargeInt.int
    
    val constTrue = CAConst 3 and constFalse = CAConst 1
    
    fun notCond{condition, outCC, highOrder} =
        {condition=X86CODE.invertTest condition, outCC=outCC, highOrder=highOrder}

    (* Sort out comparisons.  Start with the boolean values (0, 1) *)
    fun getCondResult({compare=3, test=CASetCond condcc, ...}, JE) = CASetCond condcc
    |   getCondResult({compare=1, test=CASetCond condcc, ...}, JE) = CASetCond(notCond condcc)
    |   getCondResult({test=CASetCond _, ...}, JE) = raise InternalError "getCondResult"
    
    |   getCondResult({compare=3, test=CASetCond condcc, ...}, JNE) = CASetCond(notCond condcc)
    |   getCondResult({compare=1, test=CASetCond condcc, ...}, JNE) = CASetCond condcc
    |   getCondResult({test=CASetCond _, ...}, JNE) = raise InternalError "getCondResult"
 
    |   getCondResult({compare=3, test=CASetCond condcc, ...}, JL) = CASetCond(notCond condcc)
    |   getCondResult({compare=1, test=CASetCond _, ...}, JL) = constFalse (* Never < 0 *)
    |   getCondResult({test=CASetCond _, ...}, JL) = raise InternalError "getCondResult"

    |   getCondResult({compare=3, test=CASetCond _, ...}, JG) = constFalse (* Never > 1 *)
    |   getCondResult({compare=1, test=CASetCond condcc, ...}, JG) = CASetCond condcc
    |   getCondResult({test=CASetCond _, ...}, JG) = raise InternalError "getCondResult"

    |   getCondResult({compare=3, test=CASetCond _, ...}, JLE) = constTrue (* Always <= 1 *)
    |   getCondResult({compare=1, test=CASetCond condcc, ...}, JLE) = CASetCond(notCond condcc)
    |   getCondResult({test=CASetCond _, ...}, JLE) = raise InternalError "getCondResult"

    |   getCondResult({compare=3, test=CASetCond condcc, ...}, JGE) = CASetCond condcc
    |   getCondResult({compare=1, test=CASetCond _, ...}, JGE) = constTrue (* Always >= 0 *)
    |   getCondResult({test=CASetCond _, ...}, JGE) = raise InternalError "getCondResult"

        (* Constant values. *)
    |   getCondResult({compare, test=CAConst testVal, ...},   JE) =
            if testVal = compare then constTrue else constFalse

    |   getCondResult({compare, test=CAConst testVal, ...},   JNE) =
            if testVal <> compare then constTrue else constFalse

    |   getCondResult({compare, test=CAConst testVal, ...},  JL) =
            if testVal < compare then constTrue else constFalse

    |   getCondResult({compare, test=CAConst testVal, ...},  JG) =
            if testVal > compare then constTrue else constFalse

    |   getCondResult({compare, test=CAConst testVal, ...},  JLE) =
            if testVal <= compare then constTrue else constFalse

    |   getCondResult({compare, test=CAConst testVal, ...},  JGE) =
            if testVal >= compare then constTrue else constFalse

        (* Unsigned tests.  We converted the values from Word to LargeInt.  We can therefore
           turn the tests back to Word for the unsigned comparisons. *)
    |   getCondResult({compare, test=CAConst testVal, ...}, JB) =
            if Word.fromLargeInt testVal < Word.fromLargeInt compare then constTrue else constFalse

    |   getCondResult({compare, test=CAConst testVal, ...}, JA) =
            if Word.fromLargeInt testVal >Word.fromLargeInt compare then constTrue else constFalse

    |   getCondResult({compare, test=CAConst testVal, ...}, JNA) =
            if Word.fromLargeInt testVal <= Word.fromLargeInt compare then constTrue else constFalse

    |   getCondResult({compare, test=CAConst testVal, ...}, JNB) =
            if Word.fromLargeInt testVal >=Word.fromLargeInt compare then constTrue else constFalse

        (* The overflow and parity checks should never occur. *)
    |   getCondResult _ = raise InternalError "getCondResult"

    fun optimiseICode{ code, pregProps=_, ccCount=_ } =
    let
        val hasChanged = ref false

        val refCounts = IntArray.array(Vector.length code, 0)

        local
            open IntArray
            fun visitRefs i =
                if sub(refCounts, i) = 0
                then
                let
                    val ExtendedBasicBlock{flow, ...} = Vector.sub(code, i)
                    val () = update(refCounts, i, 1)
                in
                    List.app visitRefs (successorBlocks flow)
                end
                else update(refCounts, i, 1+sub(refCounts, i))
        in
            val () = visitRefs 0 (* Start from the root. *)
        end
        
        (* If this argument is a register that refers to a recently loaded memory address we can
           replace the argument with the memory reference. *)
        (* TODO: We include the opSize in the size because of a problem with ConditionalMove.
           codetreeToIcode makes Register-register nativeWord just in case we're moving a
           return address or a container address.  If they are then combined into a
           ConditionalMove it will be 64-bit.  We can't then use a 64-bit Cmov with a
           32-bit memory location. *)
        fun replaceWithMemory(arg as RegisterArgument (preg as PReg pregNo), kill, memRefs, instrOpSize) =
        (
            case List.find(fn {dest, opSize, ... } => dest = preg andalso opSize = instrOpSize) memRefs of
                SOME { source, ...} =>
                    (
                    if member(pregNo, kill)
                    then ( hasChanged := true; source )
                    else arg,
                    (* Filter this from the list.  If this is not the last
                       reference we want to use the register and if it is then
                       we don't need it any longer. *)
                    List.filter(fn {dest, ...} => dest <> preg) memRefs)
           |    NONE => (arg, memRefs)
        )
        
        |   replaceWithMemory(arg, _, memRefs, _) = (arg, memRefs)

        fun optimiseBlock processed (blockNo, ExtendedBasicBlock{block, flow, outCCState, ...}) =
        let
            fun optCode([], brCond, _, code) = (code, brCond)

            |   optCode({instr=SetToCondition({dest, highOrder, ccRef, condition=setCCond, trueValue, falseValue, opSize}), ...} :: rest,
                         BRChangeCond(ccCond as {srcCC, ...}), memRefs, code) =
                let
                    (* If we have a BRChangeCond then we need to adjust the condition. *)
                    val _ = ccRef = srcCC orelse raise InternalError "optCode: SetToCondition/BRChangeCond"
                    val () = hasChanged := true
                    val ccCond = getCondResult(ccCond, setCCond)
                    val outInstr =
                        case ccCond of
                            CASetCond({condition=outCondition, outCC, ...}) =>
                                SetToCondition{ dest=dest, highOrder=highOrder, ccRef=outCC, condition=outCondition,
                                                trueValue=trueValue, falseValue=falseValue, opSize=opSize }
                        |   CAConst cv =>
                            let
                                val constVal = if cv = 3 then trueValue else falseValue
                                val moveKind = case opSize of OpSize32 => Move32Bit | OpSize64 => Move64Bit
                            in
                                LoadArgument{dest=dest, source=IntegerConstant constVal, kind=moveKind}
                            end
                    val outCond =
                        if trueValue = 3 andalso falseValue = 1
                        then BRSetCond {dest=dest, test=ccCond}
                        else BRNone
                in
                    optCode(rest, outCond, memRefs, outInstr :: code)
                end

            |   optCode({instr as SetToCondition{dest, highOrder, ccRef, condition, trueValue=3, falseValue=1, opSize=_}, ...} :: rest, _, memRefs, code) =
                    (* Record this SetCondition in case it can be used later. *)
                    optCode(rest,
                        BRSetCond{dest=dest, test=CASetCond{condition=condition, highOrder=highOrder, outCC=ccRef}},
                        memRefs, instr :: code)

            |   optCode({instr=ConditionalMove{dest, falseSrc, trueSrc, ccRef, condition=moveCond, opSize}, ...} :: rest,
                         BRChangeCond(ccCond as {srcCC, ...}), memRefs, code) =
                let
                    val _ = ccRef = srcCC orelse raise InternalError "optCode: ConditionalMove/BRChangeCond"
                    val () = hasChanged := true
                    val ccCond = getCondResult(ccCond, moveCond)
                    val outInstr =
                        case ccCond of
                            CASetCond({condition=outCondition, outCC, ...}) =>
                                ConditionalMove{dest=dest, falseSrc=falseSrc, trueSrc=trueSrc, ccRef=outCC, condition=outCondition, opSize=opSize}
                        |   CAConst cv =>
                            let
                                val use = if cv = 1 then falseSrc else trueSrc
                                val kind = if opSize = OpSize32 then Move32Bit else Move64Bit
                            in
                                LoadArgument{dest=dest, source=use, kind=kind}
                            end
                in
                    optCode(rest, BRNone, memRefs, outInstr :: code)
                end

            |   optCode({instr=ConditionalMove{dest, falseSrc, trueSrc, ccRef, condition, opSize}, kill, ...} :: rest,
                         _, memRefs, code) =
                let
                    (* Replace register references with memory if possible.   It's safe to use memory
                       references at this point because the reference must have been loaded on the common
                       path so must be valid whether or not the condition is true. *)
                    val (fSource, memRefsOut1) = replaceWithMemory(falseSrc, kill, memRefs, opSize)
                    val (tSource, memRefsOut) = replaceWithMemory(trueSrc, kill, memRefsOut1, opSize)
                in
                    optCode(rest, BRNone, memRefsOut,
                        ConditionalMove{dest=dest, falseSrc=fSource, trueSrc=tSource, ccRef=ccRef, condition=condition, opSize=opSize} ::code)
                end

            |   optCode({instr as LoadArgument{dest=dest, source=IntegerConstant 1, ...}, ...} :: rest, _, memRefs, code) =
                    (* This MAY be a value of false. *)
                    optCode(rest, BRSetCond{dest=dest, test=CAConst 1}, memRefs, instr :: code)

            |   optCode({instr as LoadArgument{dest=dest, source=IntegerConstant 3, ...}, ...} :: rest, _, memRefs, code) =
                    (* This MAY be a value of true. *)
                    optCode(rest, BRSetCond{dest=dest, test=CAConst 3}, memRefs, instr :: code)

            |   optCode({instr as BooleanNot(boolOp as {source=notSrc, dest=notDest}), ...} :: rest,
                        BRSetCond{dest=setDest, test}, memRefs, code) =
                    if setDest = notSrc
                    then
                    let
                        val _ = hasChanged := true
                        val (outSet, outInstr) =
                            case test of
                                CASetCond{outCC, condition, highOrder} =>
                                let
                                    (* Turn the NOT into a set of the reverse of the condition.  Use the
                                       original zero register since we're probably going to be eliminating
                                       the previous SetCondition.  If we weren't we'd be better to use
                                       a new one. *)
                                    val outCond = X86CODE.invertTest condition
                                    val condNot =
                                        {dest=notDest, ccRef=outCC, condition=outCond, highOrder = highOrder,
                                         trueValue=3, falseValue=1, opSize=polyWordOpSize}
                                    val caNot = {outCC=outCC, condition=outCond, highOrder = highOrder}
                                in
                                    (CASetCond caNot, SetToCondition condNot)
                                end
                            |   CAConst cv =>
                                let
                                    (* The source was set by a constant.  This must have come from merging
                                       after an unconditional jump otherwise it would have been processed
                                       in the simplifier. *)
                                    val newCv = if cv = 3 then 1 else 3
                                    val newInstr =
                                        LoadArgument{dest=notDest, source=IntegerConstant newCv, kind=Move32Bit}
                                in
                                    (CAConst newCv, newInstr)
                                end
                                    
                    in
                        (* Replace the BooleanNot with a SetToCondition.  Pass this
                           SetToCondition on in case we have a TestBoolean. *)
                        optCode(rest, BRSetCond{dest=notDest, test=outSet}, memRefs, outInstr :: code)
                    end
                    else 
                    (* The source wasn't set by a SetToCondition so we can't eliminate
                       the not by changing the SetCondition.  Perhaps it will be tested
                       in which case we can eliminate it in the test.  Don't bother
                       to see if the source is a "NOT".  That should have been
                       eliminated in the higher levels. *)
                    optCode(rest, BRNot boolOp, memRefs, instr :: code)

            |   optCode({instr as BooleanNot boolOp, ...} :: rest, _, memRefs, code) =
                    optCode(rest, BRNot boolOp, memRefs, instr :: code)

            |   optCode({instr as CompareLiteral{arg1, arg2, ccRef=ccRefOut, opSize}, kill, ...} :: rest,
                        brCond, memRefs, code) =
                let
                    val (repArg1, memRefsOut) = replaceWithMemory(arg1, kill, memRefs, opSize)
                in
                    case repArg1 of
                        RegisterArgument source =>
                        (* CompareLiteral is put in by CodetreeToIcode to test a boolean value.  It can also
                           arise as the result of pattern matching on booleans or even by tests such as = true. *)
                        if arg2 = 1 orelse arg2 = 3
                        then
                        (
                            case brCond of
                                BRSetCond{dest=setDest, test} =>
                                if setDest = source
                                then
                                (*  We recently set this register to a condition and the condition is still
                                    active. Rather than test it again we can just change references to the
                                    condition we're setting to refer to the old one.  We have to check that
                                    we're not exporting the condition to another block since we only
                                    change references in this block.
                                    N.B.  If this is a boolean we will only ever be testing it against
                                    true (tagged 1 i.e. 3) or false (tagged 0 i.e. 1).
                                    However the CODETREE_SIMPLIFIER turns any if..then..else that returns just
                                    these two values into a conditional and we could then compare this with
                                    something else.  Since the comparison could be an ordering it gets very
                                    complicated.  Just consider 1 and 3 for the moment. *)
                                let
                                    val _ = hasChanged := true
                                    (* Put in a replacement so that if we were previously testing ccRefOut
                                       we should instead test ccRef. *)
                                    val _ = arg2 = 1 orelse arg2 = 3 orelse raise InternalError "cv"
                                    val repl = BRChangeCond{srcCC=ccRefOut, compare=arg2, test=test}
                                    val _ = isSome outCCState andalso raise InternalError "optCode: CC exported"
                                in
                                    optCode(rest, repl, memRefsOut, code)
                                end
                                else optCode(rest, BRNone, memRefsOut, instr::code)

                            |   BRNot{dest, source=notSource} =>
                                if dest = source
                                then
                                let
                                    (* We have a NOT of a boolean other than a test.  There's no test
                                       to pass on but we can change this test. *)
                                    val _ = hasChanged := true
                                    val _ = isSome outCCState andalso raise InternalError "optCode: CC exported"
                                    val outArg =
                                        case arg2 of
                                            1 (* i.e. tagged false *) => 3
                                        |   3 (* i.e. tagged true *)  => 1
                                        |   _ => raise InternalError "optCode: test is not true or false"
                                    val outCompare =
                                        CompareLiteral{arg1=RegisterArgument notSource, arg2=outArg,
                                            opSize=OpSize32, ccRef=ccRefOut}
                                in
                                    optCode(rest, BRNone, memRefsOut, outCompare :: code)
                                end
                                else optCode(rest, BRNone, memRefsOut, instr::code)
                     

                            |   _ => optCode(rest, BRNone, memRefsOut, instr::code)
                        )
                        else optCode(rest, BRNone, memRefsOut, instr::code)
                
                    |   repArg1 =>
                            optCode(rest, BRNone, memRefsOut,
                                CompareLiteral{arg1=repArg1, arg2=arg2, ccRef=ccRefOut, opSize=opSize}::code)
                end

            |   optCode({instr=ZeroRegister{dest}, ...} :: rest, brCond as BRSetCond{test=CASetCond{highOrder, ...}, ...}, memRefs, code) =
                (* We've had a SetCondition before this and we're about to test some condition in order
                   to do another SetCondition.  It may be the original condition or some other.  If it's
                   the original we need to preserve the CC if we're going to pass on the BRSetCond
                   but ZeroRegister will clobber it.  Change this to use a Move from the original high-order
                   which will preserve the CC. *)
                    optCode(rest, brCond, memRefs, LoadArgument{source=RegisterArgument highOrder, dest=dest, kind=Move32Bit} :: code)

            |   optCode({instr as ZeroRegister _, ...} :: rest, brCond as BRSetCond{test=CAConst _, ...}, memRefs, code) =
                (* Because this is a constant it won't be clobbered and can be passed on to the test. *)
                    optCode(rest, brCond, memRefs, instr :: code)

            |   optCode({instr as ZeroRegister _, ...} :: rest, _, memRefs, code) = optCode(rest, BRNone, memRefs, instr :: code)

            |   optCode({instr as LoadArgument{dest, source=RegisterArgument srReg, ...} , ...} :: rest,
                        brCond as BRSetCond{dest=setDest, test}, memRefs, code) =
                (* We frequently have a move if we have followed an unconditional branch. *)
                if srReg = setDest
                then optCode(rest, BRSetCond{dest=dest, test=test}, memRefs, instr :: code)
                else optCode(rest, brCond, memRefs, instr :: code)

            |   optCode({instr as LoadArgument{dest, source as MemoryLocation _, kind} , ...} :: rest, inCond, memRefs, code) =
                let
                    (* If we load a memory location add it to the list in case we can use it later. *)
                    val memRefsOut =
                        case kind of
                            Move64Bit => {dest=dest, source=source, opSize=OpSize64} :: memRefs
                        |   Move32Bit => {dest=dest, source=source, opSize=OpSize32} :: memRefs
                        |   _ => memRefs
                in
                    optCode(rest, inCond, memRefsOut, instr::code)
                end

            |   optCode({instr as StoreArgument _, ...} :: rest, inCond, _, code) =
                    (* For safety assume that this clears the "cache". *)
                    optCode(rest, inCond, [], instr::code)

            |   optCode({instr as FunctionCall _, ...} :: rest, _, _, code) =
                    optCode(rest, BRNone, [], instr::code)                

                (* These instructions could take memory operands.  This isn't the full set but the others are
                   rare or only take memory operands that refer to boxed memory. *)
            |   optCode({instr=WordComparison{arg1, arg2, ccRef, opSize}, kill, ...} :: rest, _, memRefs, code) =
                let
                    (* Replace register reference with memory if possible. *)
                    val (source, memRefsOut) = replaceWithMemory(arg2, kill, memRefs, opSize)
                in
                    (* This affects the CC. *)
                    optCode(rest, BRNone, memRefsOut, WordComparison{arg1=arg1, arg2=source, ccRef=ccRef, opSize=opSize}::code)
                end

            |   optCode({instr=ArithmeticFunction{oper, resultReg, operand1, operand2, ccRef, opSize}, kill, ...} :: rest, _, memRefs, code) =
                let
                    (* Replace register reference with memory if possible. *)
                    val (source, memRefsOut) = replaceWithMemory(operand2, kill, memRefs, opSize)
                in
                    (* This affects the CC. *)
                    optCode(rest, BRNone, memRefsOut,
                        ArithmeticFunction{oper=oper, resultReg=resultReg, operand1=operand1,
                                           operand2=source, ccRef=ccRef, opSize=opSize}::code)
                end

            |   optCode({instr=TestTagBit{arg, ccRef}, kill, ...} :: rest, _, memRefs, code) =
                let
                    (* Replace register reference with memory if possible. *)
                    val (source, memRefsOut) = replaceWithMemory(arg, kill, memRefs, polyWordOpSize)
                in
                    (* This affects the CC. *)
                    optCode(rest, BRNone, memRefsOut, TestTagBit{arg=source, ccRef=ccRef}::code)
                end

            |   optCode({instr=UntagFloat{source, dest, cache=_}, kill, ...} :: rest, _, memRefs, code) =
                let
                    (* Replace register reference with memory if possible. *)
                    val (source, memRefsOut) = replaceWithMemory(source, kill, memRefs, polyWordOpSize)
                in
                    (* Not sure if this affects the CC but assume it might. *)
                    optCode(rest, BRNone, memRefsOut, UntagFloat{source=source, dest=dest, cache=NONE}::code)
                end

            |   optCode({instr, ...} :: rest, inCond, memRefs, code) =
                let
                    (* If this instruction affects the CC the cached SetToCondition will no longer be valid. *)
                    val afterCond =
                        case getInstructionCC instr of
                            CCUnchanged => inCond
                        |   _ => BRNone
                in
                    optCode(rest, afterCond, memRefs, instr::code)
                end

            val (blkCode, finalRepl) =
                if IntArray.sub(refCounts, blockNo) = 0
                then ([], BRNone)
                else optCode(block, BRNone, [], processed)
        in
            case (flow, finalRepl) of
                (* We have a Condition and a change to the condition. *)
                (flow as Conditional{ccRef, condition=brCond, trueJump, falseJump},
                 BRChangeCond(cCond as {srcCC, ...})) =>
                    if srcCC = ccRef
                    then
                    let
                        val newFlow =
                            case getCondResult(cCond, brCond) of
                                CASetCond{condition, outCC, ...} =>
                                    Conditional{ccRef=outCC, condition=condition, trueJump=trueJump, falseJump=falseJump}
                            |   CAConst 3 => Unconditional trueJump
                            |   CAConst 1 => Unconditional falseJump
                            |   CAConst _ => raise InternalError "BRChangeCond: CAConst"
                    in
                        BasicBlock{flow=newFlow, block=List.rev blkCode}
                    end
                    else BasicBlock{flow=flow, block=List.rev blkCode}

            |   (flow as Conditional{trueJump, falseJump, condition, ccRef}, _) =>
                let
                    (* Can we turn this into a ConditionalMove?  Yes, if both arms move
                       a constant or a register into the same register or both return a constant
                       or a register.  If they move a memory location into a register we can't
                       because that memory may only be accessible if the condition holds. *)
                    val trueCode = Vector.sub(code, trueJump)
                    and falseCode = Vector.sub(code, falseJump)

                    fun memMove(MemoryLocation _) = true
                    |   memMove _ = false
                in
                    case (trueCode, falseCode) of
                        (ExtendedBasicBlock{flow=Unconditional toTrue,
                            block=[{instr as LoadArgument{source=sourceT, dest=destT, kind=kindT, ...}, ...}], ...},
                         ExtendedBasicBlock{flow=Unconditional toFalse,
                            block=[{instr=LoadArgument{source=sourceF, dest=destF, kind=kindF, ...}, ...}], ...}) =>
                        if toTrue = toFalse andalso destT = destF andalso not (memMove sourceT) andalso not (memMove sourceF)
                           andalso kindT = kindF andalso (kindT = Move32Bit orelse kindT = Move64Bit)
                        then
                        let
                            val () =
                                case (sourceT, sourceF) of
                                    (IntegerConstant _, IntegerConstant _) =>
                                        print("CMov ?" ^ PolyML.makestring(sourceT, sourceF) ^ "\n")
                                |   _ => ()
                            val () = hasChanged := true
                            val opSize = if kindT = Move32Bit then OpSize32 else OpSize64
                            (* Convert to CMov.  It's possible that the registers are the same if the test is
                               actually redundant in which case just put in one of the moves. *)
                            val cmov =
                                if (case (sourceT, sourceF) of (RegisterArgument t, RegisterArgument f) => t = f | _ => false)
                                then instr
                                else ConditionalMove{trueSrc=sourceT, falseSrc=sourceF, dest=destT, ccRef=ccRef,
                                                condition=condition, opSize=opSize}
                            val target = Vector.sub(code, toTrue)
                        in
                            optimiseBlock (cmov :: blkCode) (toTrue, target)
                        end
                        else BasicBlock{flow=flow, block=List.rev blkCode}

                    |   (ExtendedBasicBlock{flow=ExitCode,
                            block=[{instr as LoadArgument{source=sourceT, dest=destT, kind=kindT, ...}, ...},
                                   {instr=ReturnResultFromFunction{resultReg=resT, numStackArgs=numStack}, ...}], ...},
                         ExtendedBasicBlock{flow=ExitCode,
                            block=[{instr=LoadArgument{source=sourceF, dest=destF, kind=kindF, ...}, ...},
                                   {instr=ReturnResultFromFunction{resultReg=resF, ...}, ...}], ...}) =>
                        if destT = resT andalso destF = resF andalso not (memMove sourceT) andalso not (memMove sourceF)
                           andalso kindT = kindF andalso (kindT = Move32Bit orelse kindT = Move64Bit)
                        then
                        let
                            val () =
                                case (sourceT, sourceF) of
                                    (IntegerConstant _, IntegerConstant _) =>
                                        print("CMov ?" ^ PolyML.makestring(sourceT, sourceF) ^ "\n")
                                |   _ => ()
                            val () = hasChanged := true
                            val opSize = if kindT = Move32Bit then OpSize32 else OpSize64
                            val cmov =
                                if (case (sourceT, sourceF) of (RegisterArgument t, RegisterArgument f) => t = f | _ => false)
                                then instr
                                else ConditionalMove{trueSrc=sourceT, falseSrc=sourceF, dest=destT, ccRef=ccRef,
                                                condition=condition, opSize=opSize}
                            val resCode =
                                ReturnResultFromFunction{resultReg=destT, numStackArgs=numStack} :: cmov :: blkCode
                        in
                            BasicBlock{flow=ExitCode, block=List.rev resCode}
                        end
                        else BasicBlock{flow=flow, block=List.rev blkCode}

                    |   _ => BasicBlock{flow=flow, block=List.rev blkCode}
                end
            
            |   (flow as Unconditional jmp, BRSetCond{dest=destCond, ...}) =>
                (* We have an unconditional branch with a SetCondition.  It could be that we
                   want the boolean result but it's also possible that we're going to test it. *)
                let
                    val target as ExtendedBasicBlock{block=targetBlck, ...} = Vector.sub(code, jmp)
                    
                    datatype possOpt = POReg of preg | POCond of ccRef

                    (* We want to see if following this can be optimised. *)
                    fun isMerge([], _) = true
                    |   isMerge({instr=ResetStackPtr _, ...} :: rest, possopt) = isMerge(rest, possopt)
                    |   isMerge({instr=CompareLiteral{arg1=RegisterArgument source, ccRef, ...}, ...} :: rest, POReg testR) =
                            source = testR andalso isMerge(rest, POCond ccRef)
                    |   isMerge({instr=LoadArgument{source=RegisterArgument source, dest, ...}, ...} :: rest, POReg testR) =
                            source = testR andalso isMerge(rest, POReg dest)
                    |   isMerge({instr=ZeroRegister _, ...} :: rest, possopt) = isMerge(rest, possopt)
                    |   isMerge({instr=SetToCondition{dest, ccRef, ...}, ...} :: rest, POCond ccCond) =
                            ccRef = ccCond andalso isMerge(rest, POReg dest)
                    |   isMerge({instr=BooleanNot{source, dest}, ...} :: rest, POReg testR) =
                            testR = source andalso isMerge(rest, POReg dest)
                    |   isMerge({instr=ReturnResultFromFunction _, ...} :: rest, possopt) =
                            (* This should always be the last but check it anyway. *)
                            isMerge(rest, possopt)
                    |   isMerge _ = false
                in
                    (* Merge it if the target block is only referenced from here or if it
                       is small enough. *)
                    if IntArray.sub(refCounts, jmp) = 1 orelse isMerge(targetBlck, POReg destCond)
                    then (hasChanged := true; optimiseBlock blkCode (jmp, target))
                    else BasicBlock{flow=flow, block=List.rev blkCode}
                end
             
            |   (flow as Unconditional jmp, _) =>
                let
                    val target as ExtendedBasicBlock{block=targetBlck, ...} = Vector.sub(code, jmp)
                    (* If the target is empty or is simply one or more Resets or a Return we're
                       better off merging this in rather than doing the jump.  We allow a single
                       Load  e.g. when loading a constant or moving a register. *)
                    fun isSimple([], _) = true
                    |   isSimple ({instr=ResetStackPtr _, ...} :: instrs, moves) = isSimple(instrs, moves)
                    |   isSimple ({instr=ReturnResultFromFunction _, ...} :: instrs, moves) = isSimple(instrs, moves)
                    |   isSimple ({instr=RaiseExceptionPacket _, ...} :: instrs, moves) = isSimple(instrs, moves)
                    |   isSimple ({instr=LoadArgument _, ...} :: instrs, moves) = moves = 0 andalso isSimple(instrs, moves+1)
                    |   isSimple _ = false
                in
                    (* Merge it if the target block is only referenced from here or if it is small enough. *)
                    if IntArray.sub(refCounts, jmp) = 1 orelse isSimple(targetBlck, 0)
                    then
                    (
                        hasChanged := true;
                        optimiseBlock blkCode (jmp, target)
                    )
                    else BasicBlock{flow=flow, block=List.rev blkCode}
                end

            |   (flow, _) => BasicBlock{flow=flow, block=List.rev blkCode}
        end
        
        val resVector = Vector.mapi (optimiseBlock []) code
    in
        if !hasChanged then Changed resVector else Unchanged
    end

    structure Sharing =
    struct
        type extendedBasicBlock = extendedBasicBlock
        and basicBlock = basicBlock
        and regProperty = regProperty
        and optimise = optimise
    end
end;