(*
    Copyright David C. J. Matthews 2016

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

functor X86CodetreeToX86Code(

    structure BACKENDTREE: BackendIntermediateCodeSig
    
	structure X86CODE: X86CODESIG

	structure X86OPTIMISE:
	sig
	    type operation
	    type code
	    type operations = operation list

	    val optimise: code * operations -> operations

	    structure Sharing:
	    sig
	        type operation = operation
	        type code = code
	    end
	end

    structure DEBUG: DEBUGSIG
    
    sharing X86CODE.Sharing = X86OPTIMISE.Sharing
):
sig
    type backendIC
    type machineWord = Address.machineWord
    val gencode: backendIC * Universal.universal list * int -> (unit -> machineWord) * Universal.universal list
    structure Sharing: sig type backendIC = backendIC end
end
=
struct
    open X86CODE
    open BACKENDTREE
    open Address

    val ioOp : int -> machineWord = RunCall.run_call1 RuntimeCalls.POLY_SYS_io_operation;

    (* tag a short constant *)
    fun tag c = 2 * c + 1

    fun condBranch(test, predict) =
    let
        val label as Labels{uses, ...} = mkLabel()
    in
        uses := 1;
        ([ConditionalBranch{test=test, predict=predict, label=label}], label)
    end

    and forwardJumpLabel v = [JumpLabel v]

    local
        (* The stack limit register is set at least twice this far from the
           end of the stack so we can simply compare the stack pointer with
           the stack limit register if we need less than this much. Setting
           it at twice this value means that procedures which use up to this
           much stack and do not call any other procedures do not need to
           check the stack at all. *)
        val minStackCheck = 20
    in
        (* Adds the constants onto the code, and copies the code into a new segment *)
        fun buildCode (cvec, operations, stackRequired, registerSet, callsAProc) : address =
        let
            (* Prelude consists of stack checking code. *)
            val preludeCode =
            let
                fun testRegAndTrap(reg, entryPt) =
                let
                    (* Normally we won't have a stack overflow so we will skip the check. *)
                    val (skipCheck, skipCheckLab) = condBranch(JNB, PredictTaken)
                in
                    [ArithRMem{ opc=CMP, output=reg, offset=memRegStackLimit, base=ebp }] @
                    skipCheck @ [CallRTS entryPt] @ forwardJumpLabel skipCheckLab
                end
            in
                if stackRequired >= minStackCheck
                then
                let
                    (* Compute the necessary amount in edi and compare that. *)
                    val stackByteAdjust = ~wordSize * stackRequired
                    val testEdiCode =
                        testRegAndTrap (edi, memRegStackOverflowCallEx)
                in
                     [LoadAddress{output=edi, base=SOME esp, index=NoIndex, offset=stackByteAdjust}] @ testEdiCode
                end
         
                else if callsAProc (* check for interrupt *)
                then testRegAndTrap (esp, memRegStackOverflowCall)
               
                else [] (* no stack check required *)
            end

            (* The code is in reverse order.  Reverse it and put the prelude first. *)
        in
            createCodeSegment(X86OPTIMISE.optimise(cvec, preludeCode @ operations), registerSet, cvec)
        end
    end

    fun codeFunctionToX86(code, localCount, context) =
    let
        (* This gives the offset in the stack for each binding. *) 
        val stackOffsetArray = Array.array(localCount, ~1)
        val stackCount = ref 0

        (* This is a naive code-generator that simply evaluates everything to eax. *)
        fun codeToX86(BICNewenv (bindings, exp), context) = raise Fail "TODO: BICNewenv"

        |   codeToX86(BICConstnt(value, _), _) =
                if isShort value
                then [MoveConstR{source=tag(Word.toIntX(toShort value)), output=eax}]
                else [MoveLongConstR{source=value, output=eax}]
    
        |   codeToX86(BICExtract(load, _(*lastRef*)), context) = raise Fail "TODO: BICExtract"
    
        |   codeToX86(BICField{base, offset}, context) = raise Fail "TODO: BICField"
    
        |   codeToX86(BICEval{function, argList, resultType}, context) = raise Fail "TODO: BICEval"

        |   codeToX86(BICBuiltIn(rtsNo, args), context) =
                (* For the moment just call the RTS function. *)
                codeToX86(
                    BICEval{function=BICConstnt(ioOp rtsNo, []),
                            argList = map (fn c => (c, GeneralType)) args, resultType=GeneralType}, context)

        |   codeToX86(BICLambda lambda, context) = raise Fail "TODO: BICLambda"

        |   codeToX86(BICCond(test, thenPt, elsePt), context) = raise Fail "TODO: BICCond"

        |   codeToX86(BICCase{ cases, test, caseType, default}, context) = raise Fail "TODO: BICCase"

        |   codeToX86(BICBeginLoop{loop, arguments}, context) = raise Fail "TODO: BICBeginLoop"

        |   codeToX86(BICLoop args, context) = raise Fail "TODO: BICLoop"

        |   codeToX86(BICKillItems{expression, ...}, context) =
                codeToX86(expression, context)

        |   codeToX86(BICRaise exc, context) =
                codeToX86(exc, context) @ [RaiseException]

        |   codeToX86(BICLdexc, _) = [] (* Do nothing - the packet is in eax. *)

        |   codeToX86(BICHandle{exp, handler}, context) = raise Fail "TODO: BICHandle"

        |   codeToX86(BICTuple fields, context) = raise Fail "TODO: BICTuple"

        |   codeToX86(BICContainer size, context) = raise Fail "TODO: BICContainer"

        |   codeToX86(BICSetContainer{container, tuple, filter}, context) = raise Fail "TODO: BICSetContainer"

        |   codeToX86(BICTagTest{test, tag, maxTag}, context) = raise Fail "TODO: BICBuiltIn"

    in
        codeToX86(code, context)
    end

    fun gencode(code, debugSwitches, localCount) =
    let
        val ops = codeFunctionToX86(code, localCount, {debugSwitches=debugSwitches})
    in
        raise Fail "TODO"
    end
end;
