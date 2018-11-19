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
    |   BRSetCond of { dest: preg, highOrder: preg, ccRef: ccRef, condition: branchOps }
    |   BRNot of { dest: preg, source: preg }
    |   BRChangeCond of { srcCC: ccRef, outCC: ccRef, condition: branchOps }

    fun optimiseICode{ code, pregProps=_, ccCount=_ } =
    let
        val hasChanged = ref false

        fun optimiseBlock(ExtendedBasicBlock{block, flow, outCCState, ...}) =
        let
            fun optCode([], brCond, code) = (code, brCond)

            |   optCode({instr = SetToCondition _, ...} :: _, BRChangeCond _, _) =
                    (* Can this ever happen? *)
                    raise InternalError "SetCond/BRChangeCond"

            |   optCode({instr as SetToCondition cond, ...} :: rest, _, code) =
                    (* Record this SetCondition in case it can be used later. *)
                    optCode(rest, BRSetCond cond, instr :: code)

            |   optCode({instr as BooleanNot(boolOp as {source=notSrc, dest=notDest}), ...} :: rest,
                        BRSetCond{dest=setDest, ccRef, condition, highOrder}, code) =
                    if setDest = notSrc
                    then 
                    let
                        val _ = hasChanged := true
                        (* Turn the NOT into a set of the reverse of the condition.  Use the
                           original zero register since we're probably going to be eliminating
                           the previous SetCondition.  If we weren't we'd be better to use
                           a new one. *)
                        val condNot =
                            {dest=notDest, ccRef=ccRef, condition=X86CODE.invertTest condition,
                             highOrder = highOrder}
                    in
                        (* Replace the BooleanNot with a SetToCondition.  Pass this
                           SetToCondition on in case we have a TestBoolean. *)
                        optCode(rest, BRSetCond condNot, SetToCondition condNot :: code)
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
                (
                    case brCond of
                        BRSetCond{dest=setDest, ccRef, condition, ...} =>
                        if setDest = source
                        then
                        (*  We recently set this register to a condition and the condition is still
                            active. Rather than test it again we can just change references to the
                            condition we're setting to refer to the old one.  We have to check that
                            we're not exporting the condition to another block since we only
                            change references in this block. *)
                        let
                            val _ = hasChanged := true
                            val outCond =
                                case arg2 of
                                    1 (* i.e. tagged false *) => X86CODE.invertTest condition
                                |   3 (* i.e. tagged true *)  => condition
                                |   _ => raise InternalError "optCode: test is not true or false"
                            val repl = BRChangeCond{srcCC=ccRefOut, outCC=ccRef, condition=outCond}
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

            val (resCode, finalRepl) = optCode(block, BRNone, [])
            val resCode = List.rev resCode

            val newFlow =
                case (finalRepl, flow) of
                    (BRChangeCond{srcCC, outCC, condition},
                      flow as Conditional{ccRef, condition=brCond, trueJump, falseJump}) =>
                        if srcCC = ccRef
                        then
                        let
                            val newCond =
                                case brCond of
                                    JE => condition
                                |   JNE => X86CODE.invertTest condition
                                |   _ => raise InternalError "Not JE or JNE"
                        in
                            Conditional{ccRef=outCC, condition=newCond, trueJump=trueJump, falseJump=falseJump}
                        end
                        else flow
                |   (_, flow) => flow
        in
            BasicBlock{block=resCode, flow=newFlow}
        end
        
        val resVector = Vector.map optimiseBlock code
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
