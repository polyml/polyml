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
       1.  NotBool following a SetCondition of the source register,
           If the condition we set is still valid we can replace the
           NotBool with a SetCondition of the inverse of the condition and
           hopefully remove the first SetCondition.
       2.  SetCondition followed by TestBoolean which must shortly be followed
           by a Conditional branch.  We can remove the TestBoolean and change
           the condition in the Conditional branch to test the original
           condition in the SetCondition.  The SetCondition should then be
           unused and removed.
       3.  NotBool followed by a TestBoolean of the result followed by a
           Conditional branch.  
    *)
    
    (* This is a rewrite of the last instruction to set a boolean.
       This is almost always rewriting the next instruction.  The only
       possibility is that we have a ResetStackPtr in between. *)
    datatype boolRegRewrite =
        BRNone
    |   BRSetCond of { dest: preg, highOrder: preg, ccRef: ccRef, condition: branchOps }
    |   BRNot of { dest: preg, source: preg }

    fun optimiseICode{ code, pregProps=_, ccCount=_ } =
    let
        val hasChanged = ref false

        fun optimiseBlock(ExtendedBasicBlock{block, flow, outCCState, ...}) =
        let
            fun optCode([], _, code) = (code, NONE)

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

            |   optCode({instr as TestBoolean{source, ccRef=ccRefOut}, ...} :: rest, brCond, code) =
                let
                    (* This should be the last instruction except for a reset stack pointer. *)
                    val tail =
                        case rest of
                            [] => []
                        |   [{instr as ResetStackPtr{preserveCC=true, ...}, ...}] => [instr]
                        |   _ => raise InternalError "TestBoolean - not last"
                in
                    case brCond of
                        BRSetCond{dest=setDest, ccRef, condition, ...} =>
                        if setDest = source
                        then
                        (* We have a TestBoolean and the register we're testing was set
                           recently.  The condition code is still active.  We can eliminate
                           this instruction but only if we change every reference to the CC
                           that this instruction is producing into a reference to the original
                           CC with appropriate testing.
                           The higher levels generate TestBoolean as the last instruction in
                           a block which then has a Conditional to test the CC.  This CC should
                           never be exported from this block.  If it were we'd have to change
                           references in other blocks. *)
                        let
                            val _ = hasChanged := true
                            val repl = {srcCC=ccRefOut, outCC=ccRef, condition=condition}
                            val _ = isSome outCCState andalso raise InternalError "optCode: CC exported"
                        in
                            (tail @ code, SOME repl)
                        end
                        else (tail @ instr:: code, NONE)

                    |   BRNot{dest, source=notSource} =>
                        if dest = source
                        then
                        let
                            (* We have a NOT of a boolean other than a test.  Change this test to
                               test the original boolean and change the Conditional at the end of
                               the block to invert the test. *)
                            val _ = hasChanged := true
                            val _ = isSome outCCState andalso raise InternalError "optCode: CC exported"
                            val repl = {srcCC=ccRefOut, outCC=ccRefOut, condition=JNE}
                        in
                            (TestBoolean{source=notSource, ccRef=ccRefOut}::code, SOME repl)
                        end
                        else (tail @ instr::code, NONE)
                    
                    |   BRNone => (tail @ instr::code, NONE)
                end

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
                    (SOME{srcCC, outCC, condition}, flow as Conditional{ccRef, condition=brCond, trueJump, falseJump}) =>
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
