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
       
       The optimiser currently deals with booleans and conditions.
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
            { srcCC: ccRef, compare: compareVal, test: changeArg }

    and changeArg =
        CASetCond of { condition: branchOps, outCC: ccRef, highOrder: preg } (*i.e. from SetCond *)
    |   CAConst of compareVal
    
    and compareVal = CVTrue | CVFalse
    
    fun compareValAsInt CVTrue = 3 | compareValAsInt CVFalse = 1
    
    and intAsCompareVal 3 = CVTrue
    |   intAsCompareVal 1 = CVFalse
    |   intAsCompareVal n = raise InternalError ("intAsCompareVal:" ^ Int.toString n)
    
    fun notCond{condition, outCC, highOrder} =
        {condition=X86CODE.invertTest condition, outCC=outCC, highOrder=highOrder}

    (* Sort out comparisons.  Start with the boolean values (0, 1) *)
    fun getCondResult({compare=CVTrue, test=CASetCond condcc, ...}, JE) = CASetCond condcc
    |   getCondResult({compare=CVFalse, test=CASetCond condcc, ...}, JE) = CASetCond(notCond condcc)
    
    |   getCondResult({compare=CVTrue, test=CASetCond condcc, ...}, JNE) = CASetCond(notCond condcc)
    |   getCondResult({compare=CVFalse, test=CASetCond condcc, ...}, JNE) = CASetCond condcc

    |   getCondResult({compare=CVTrue, test=CASetCond condcc, ...}, JL) = CASetCond(notCond condcc)
    |   getCondResult({compare=CVFalse, test=CASetCond _, ...}, JL) = CAConst CVFalse (* Never < 0 *)

    |   getCondResult({compare=CVTrue, test=CASetCond _, ...}, JG) = CAConst CVFalse (* Never > 1 *)
    |   getCondResult({compare=CVFalse, test=CASetCond condcc, ...}, JG) = CASetCond condcc

    |   getCondResult({compare=CVTrue, test=CASetCond _, ...}, JLE) = CAConst CVTrue (* Always <= 1 *)
    |   getCondResult({compare=CVFalse, test=CASetCond condcc, ...}, JLE) = CASetCond(notCond condcc)

    |   getCondResult({compare=CVTrue, test=CASetCond condcc, ...}, JGE) = CASetCond condcc
    |   getCondResult({compare=CVFalse, test=CASetCond _, ...}, JGE) = CAConst CVTrue (* Always >= 0 *)

        (* Constant values. *)
    |   getCondResult({compare=CVTrue, test=CAConst CVTrue, ...},   JE) = CAConst CVTrue
    |   getCondResult({compare=CVFalse, test=CAConst CVFalse, ...}, JE) = CAConst CVTrue
    |   getCondResult({test=CAConst _, ...},                        JE) = CAConst CVFalse

    |   getCondResult({compare=CVTrue, test=CAConst CVTrue, ...},   JNE) = CAConst CVFalse
    |   getCondResult({compare=CVFalse, test=CAConst CVFalse, ...}, JNE) = CAConst CVFalse
    |   getCondResult({test=CAConst _, ...},                        JNE) = CAConst CVTrue

    |   getCondResult({compare=CVTrue, test=CAConst CVFalse, ...},  JL) = CAConst CVTrue
    |   getCondResult({test=CAConst _, ...},                        JL) = CAConst CVFalse

    |   getCondResult({compare=CVFalse, test=CAConst CVTrue, ...},  JG) = CAConst CVTrue
    |   getCondResult({test=CAConst _, ...},                        JG) = CAConst CVFalse

    |   getCondResult({compare=CVFalse, test=CAConst CVTrue, ...},  JLE) = CAConst CVFalse
    |   getCondResult({test=CAConst _, ...},                        JLE) = CAConst CVTrue

    |   getCondResult({compare=CVTrue, test=CAConst CVFalse, ...},  JGE) = CAConst CVFalse
    |   getCondResult({test=CAConst _, ...},                        JGE) = CAConst CVTrue

        (* Since we only have 0 and 1 the unsigned tests are the same. *)
    |   getCondResult(test, JB) = getCondResult(test, JL)
    |   getCondResult(test, JA) = getCondResult(test, JG)
    |   getCondResult(test, JNA) = getCondResult(test, JLE)
    |   getCondResult(test, JNB) = getCondResult(test, JGE)

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

        fun optimiseBlock processed (blockNo, ExtendedBasicBlock{block, flow, outCCState, ...}) =
        let
            fun optCode([], brCond, code) = (code, brCond)

            |   optCode({instr as SetToCondition({dest, highOrder, ccRef, condition=setCCond}), ...} :: rest,
                         BRChangeCond(ccCond as {srcCC, ...}), code) =
                if ccRef = srcCC
                then
                let
                    val ccCond = getCondResult(ccCond, setCCond)
                    val outInstr =
                        case ccCond of
                            CASetCond({condition=outCondition, outCC, ...}) =>
                                SetToCondition{ dest=dest, highOrder=highOrder, ccRef=outCC, condition=outCondition }
                        |   CAConst cv =>
                                LoadArgument{dest=dest, source=IntegerConstant(compareValAsInt cv), kind=Move32Bit}
                in
                    optCode(rest, BRSetCond {dest=dest, test=ccCond}, outInstr :: code)
                end
                else optCode(rest,
                    BRSetCond{dest=dest, test=CASetCond{condition=setCCond, highOrder=highOrder, outCC=ccRef}},
                    instr :: code)

            |   optCode({instr as SetToCondition{dest, highOrder, ccRef, condition}, ...} :: rest, _, code) =
                    (* Record this SetCondition in case it can be used later. *)
                    optCode(rest,
                        BRSetCond{dest=dest, test=CASetCond{condition=condition, highOrder=highOrder, outCC=ccRef}},
                        instr :: code)

            |   optCode({instr as LoadArgument{dest=dest, source=IntegerConstant 1, ...}, ...} :: rest, _, code) =
                    (* This MAY be a value of false. *)
                    optCode(rest, BRSetCond{dest=dest, test=CAConst(intAsCompareVal 1)}, instr :: code)

            |   optCode({instr as LoadArgument{dest=dest, source=IntegerConstant 3, ...}, ...} :: rest, _, code) =
                    (* This MAY be a value of true. *)
                    optCode(rest, BRSetCond{dest=dest, test=CAConst(intAsCompareVal 3)}, instr :: code)

            |   optCode({instr as BooleanNot(boolOp as {source=notSrc, dest=notDest}), ...} :: rest,
                        BRSetCond{dest=setDest, test}, code) =
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
                                        {dest=notDest, ccRef=outCC, condition=outCond, highOrder = highOrder}
                                    val caNot = {outCC=outCC, condition=outCond, highOrder = highOrder}
                                in
                                    (CASetCond caNot, SetToCondition condNot)
                                end
                            |   CAConst cv =>
                                let
                                    (* The source was set by a constant.  This must have come from merging
                                       after an unconditional jump otherwise it would have been processed
                                       in the simplifier. *)
                                    val newCv = case cv of CVTrue => CVFalse | CVFalse => CVTrue
                                    val newInstr =
                                        LoadArgument{dest=notDest, source=IntegerConstant(compareValAsInt newCv), kind=Move32Bit}
                                in
                                    (CAConst newCv, newInstr)
                                end
                                    
                    in
                        (* Replace the BooleanNot with a SetToCondition.  Pass this
                           SetToCondition on in case we have a TestBoolean. *)
                        optCode(rest, BRSetCond{dest=notDest, test=outSet}, outInstr :: code)
                    end
                    else 
                    (* The source wasn't set by a SetToCondition so we can't eliminate
                       the not by changing the SetCondition.  Perhaps it will be tested
                       in which case we can eliminate it in the test.  Don't bother
                       to see if the source is a "NOT".  That should have been
                       eliminated in the higher levels. *)
                    optCode(rest, BRNot boolOp, instr :: code)

            |   optCode({instr as BooleanNot boolOp, ...} :: rest, _, code) =
                    optCode(rest, BRNot boolOp, instr :: code)

            |   optCode({instr as CompareLiteral{arg1=RegisterArgument source, arg2, ccRef=ccRefOut, ...}, ...} :: rest,
                        brCond, code) =
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
                            val cv = case arg2 of 1 => CVFalse | 3 => CVTrue | _ => raise InternalError "Compare val"
                            val repl = BRChangeCond{srcCC=ccRefOut, compare=cv, test=test}
                            val _ = isSome outCCState andalso raise InternalError "optCode: CC exported"
                        in
                            optCode(rest, repl, code)
                        end
                        else optCode(rest, BRNone, instr::code)

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
                            optCode(rest, BRNone, outCompare :: code)
                        end
                        else optCode(rest, BRNone, instr::code)
                     

                    |   _ => optCode(rest, BRNone, instr::code)
                )
                else optCode(rest, BRNone, instr::code)

            |   optCode({instr=ZeroRegister{dest}, ...} :: rest, brCond as BRSetCond{test=CASetCond{highOrder, ...}, ...}, code) =
                (* We've had a SetCondition before this and we're about to test some condition in order
                   to do another SetCondition.  It may be the original condition or some other.  If it's
                   the original we need to preserve the CC if we're going to pass on the BRSetCond
                   but ZeroRegister will clobber it.  Change this to use a Move from the original high-order
                   which will preserve the CC. *)
                    optCode(rest, brCond, LoadArgument{source=RegisterArgument highOrder, dest=dest, kind=Move32Bit} :: code)

            |   optCode({instr as ZeroRegister _, ...} :: rest, _, code) = optCode(rest, BRNone, instr :: code)

            |   optCode({instr as LoadArgument{dest, source=RegisterArgument srReg, ...} , ...} :: rest,
                        brCond as BRSetCond{dest=setDest, test}, code) =
                (* We frequently have a move if we have followed an unconditional branch. *)
                if srReg = setDest
                then optCode(rest, BRSetCond{dest=dest, test=test}, instr :: code)
                else optCode(rest, brCond, instr :: code)

            |   optCode({instr, ...} :: rest, inCond, code) =
                let
                    (* If this instruction affects the CC the cached SetToCondition will no longer be valid. *)
                    val afterCond =
                        case getInstructionCC instr of
                            CCUnchanged => inCond
                        |   _ => BRNone
                in
                    optCode(rest, afterCond, instr::code)
                end

            val (blkCode, finalRepl) =
                if IntArray.sub(refCounts, blockNo) = 0
                then ([], BRNone)
                else optCode(block, BRNone, processed)
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
                            |   CAConst CVTrue => Unconditional trueJump
                            |   CAConst CVFalse => Unconditional falseJump
                    in
                        BasicBlock{flow=newFlow, block=List.rev blkCode}
                    end
                    else BasicBlock{flow=flow, block=List.rev blkCode}
            
            |   (flow as Conditional _, BRNone) => BasicBlock{flow=flow, block=List.rev blkCode}

            |   (flow as Conditional _, BRSetCond _) => BasicBlock{flow=flow, block=List.rev blkCode}

            |   (Conditional _, _) => raise InternalError "Conditional??"
            
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
