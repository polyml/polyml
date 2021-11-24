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

functor Arm64ICodeToArm64Code(

    structure Arm64Assembly: ARM64ASSEMBLY
    structure Arm64Sequences: ARM64SEQUENCES
    structure Debug: DEBUG
    structure Arm64ICode: ARM64ICODE
    structure Identify: ARM64IDENTIFYREFERENCES
    structure IntSet: INTSET
    structure Pretty: PRETTY

    structure Strongly:
        sig
            val stronglyConnectedComponents: {nodeAddress: 'a -> int, arcs: 'a -> int list } -> 'a list -> 'a list list
        end
    
    sharing Arm64Assembly.Sharing = Arm64Sequences.Sharing = Arm64ICode.Sharing = Identify.Sharing = IntSet
): ARM64ICODEGENERATE =
struct
    open Identify
    open Arm64ICode
    open Arm64Assembly
    open Arm64Sequences
    open Address
    exception InternalError = Misc.InternalError

    fun icodeToArm64Code {blocks, functionName, stackRequired, debugSwitches, allocatedRegisters, resultClosure, profileObject, ...} =
    let
        val numBlocks = Vector.length blocks
        fun getAllocatedReg r = Vector.sub(allocatedRegisters, r)

        local
            val numRegisters = Vector.length allocatedRegisters
            val uses = Array.array(numRegisters, false)
            fun used(PReg r) = Array.update(uses, r, true)
            fun isUsed(PReg r) = Array.sub(uses, r)

            (* LoadArgument, TagValue, CopyToCache, UntagValue and BoxValue are eliminated if their destination
               is not used.  In that case their source are not used either. *)
            fun instructionUses(MoveRegister { source, dest, ...}) = if isUsed dest then used source else ()
            |   instructionUses(LoadNonAddressConstant _) = ()
            |   instructionUses(LoadAddressConstant _) = ()
            |   instructionUses(BeginFunction _) = ()
            |   instructionUses(ReturnResultFromFunction{resultReg, returnReg, ...}) = (used resultReg; used returnReg)

            (* Depth-first scan. *)
            val visited = Array.array(numBlocks, false)

            fun processBlocks blockNo =
            if Array.sub(visited, blockNo)
            then ()  (* Done or currently being done. *)
            else
            let
                val () = Array.update(visited, blockNo, true)
                val ExtendedBasicBlock { flow, block,...} = Vector.sub(blocks, blockNo)
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
            in
                List.foldr(fn ({instr, ...}, ()) => instructionUses instr) () block
            end

        in
            val () = processBlocks 0
            val isUsed = isUsed
        end

        fun moveIfNecessary({src, dst}, code) =
            if src = dst then code
            else moveRegToReg{sReg=src, dReg=dst} :: code

        (* Add a constant word to the source register and put the result in the
           destination.  regW is used as a work register if necessary.  This is used
           both for addition and subtraction. *)
        fun addConstantWord({regS, regD, value=0w0, ...}, code) =
            if regS = regD then code else moveRegToReg{sReg=regS, dReg=regD} :: code
    
        |   addConstantWord({regS, regD, regW, value}, code) =
            let
                (* If we have to load the constant it's better if the top 32-bits are
                   zero if possible. *)
                val (isSub, unsigned) =
                    if value > Word64.<<(0w1, 0w63)
                    then (true, ~ value)
                    else (false, value)
            in
                if unsigned < Word64.<<(0w1, 0w24)
                then (* We can put up to 24 in a shifted and an unshifted constant. *)
                let
                    val w = Word.fromLarge(Word64.toLarge unsigned)
                    val high = Word.andb(Word.>>(w, 0w12), 0wxfff)
                    val low = Word.andb(w, 0wxfff)
                    val addSub = if isSub then subImmediate else addImmediate
                in
                    if high <> 0w0
                    then
                    (
                        (if low <> 0w0 then [addSub{regN=regD, regD=regD, immed=low, shifted=false}] else []) @
                        addSub{regN=regS, regD=regD, immed=high, shifted=true} :: code
                    )
                    else addSub{regN=regS, regD=regD, immed=low, shifted=false} :: code
                end
                else
                let
                    (* To minimise the constant and increase the chances that it
                       will fit in a single word look to see if we can shift it. *)
                    fun getShift(value, shift) =
                        if Word64.andb(value, 0w1) = 0w0
                        then getShift(Word64.>>(value, 0w1), shift+0w1)
                        else (value, shift)
                    val (shifted, shift) = getShift(unsigned, 0w0)
                in
                    (if isSub then subShiftedReg else addShiftedReg)
                        {regM=regW, regN=regS, regD=regD, shift=ShiftLSL shift} ::
                    List.rev (loadNonAddress(regW, shifted)) @ code
                end
            end

        fun codeExtended _ ({instr=MoveRegister{source=PReg sreg, dest as PReg dreg, ...}, ...}, code) =
            if not (isUsed dest)
            then code
            else moveIfNecessary({src=getAllocatedReg sreg, dst=getAllocatedReg dreg}, code)

        |   codeExtended _ ({instr=LoadNonAddressConstant{source, dest=PReg dreg, ...}, ...}, code) =
                List.rev(loadNonAddress(getAllocatedReg dreg, source)) @ code

        |   codeExtended _ ({instr=LoadAddressConstant{source, dest=PReg dreg, ...}, ...}, code) =
                loadAddressConstant(getAllocatedReg dreg, source) :: code

        |   codeExtended _ ({instr=BeginFunction{regArgs, ...}, ...}, code) =
            let
                val saveRegs = List.map #2 regArgs (* The real registers used for arguments. *)
                val skipCheck = createLabel()
                val defaultWords = 10 (* This is wired into the RTS. *)
                val workRegister = X10  (* Spare: not used yet. *)
                val debugTrapAlways = false (* Can be set to true for debugging *)

                (* Test with either the stack-pointer or a high-water value.
                   The RTS assumes that X9 has been used as the high-water if it is
                   called through stackOverflowXCallOffset rather than stackOverflowCallOffset *)
                val (testReg, entryPt, code1) =
                    if stackRequired <= defaultWords
                    then (X_MLStackPtr, stackOverflowCallOffset, code)
                    else (X9, stackOverflowXCallOffset,
                            addConstantWord({regS=X_MLStackPtr, regD=X9, regW=workRegister,
                            value= ~ (Word64.fromLarge(Word.toLarge nativeWordSize)) * Word64.fromInt stackRequired}, code))

                (* Skip the RTS call if there is enough stack.  N.B. The RTS can modify the end-of-stack value
                   to force a trap here even if there is really enough stack. *)
                val code2 =
                    (if debugTrapAlways then []
                     else
                     [conditionalBranch(condCarrySet, skipCheck),
                      subSShiftedReg{regM=workRegister, regN=testReg, regD=XZero, shift=ShiftNone}]) @
                    (* Load the end-of-stack value. *)
                    loadRegScaled{regT=workRegister, regN=X_MLAssemblyInt, unitOffset=stackLimitOffset} :: code1

                val code3 =
                    (* Call the RTS but save X30 across the call *)
                    setLabel skipCheck ::
                    loadRegPostIndex{regT=X30, regN=X_MLStackPtr, byteOffset= 8} ::
                    registerMask saveRegs ::
                    branchAndLinkReg X16 ::
                    loadRegScaled{regT=X16, regN=X_MLAssemblyInt, unitOffset=entryPt} ::
                    storeRegPreIndex{regT=X30, regN=X_MLStackPtr, byteOffset= ~8} :: code2

                val usedRegs = List.filter (isUsed o #1) regArgs
                fun mkPair(PReg pr, rr) = {src=rr,dst=getAllocatedReg pr}
                val regPairs = List.map mkPair usedRegs
            in
                (*moveMultipleRegisters(regPairs, code3)*) code3
            end

        |   codeExtended _ ({instr=ReturnResultFromFunction { resultReg=PReg resReg, returnReg=PReg retReg, numStackArgs }, ...}, code) =
            let
                val resultReg = getAllocatedReg resReg
                and returnReg = getAllocatedReg retReg

                fun resetStack(0, code) = code
                |   resetStack(nItems, code) =
                        addConstantWord({regS=X_MLStackPtr, regD=X_MLStackPtr, regW=X3,
                                        value=Word64.fromLarge(Word.toLarge nativeWordSize) * Word64.fromInt nItems}, code)
            in
                returnRegister returnReg :: resetStack(numStackArgs, moveIfNecessary({src=resultReg, dst=X0}, code))
            end
        

        val blockToLabelMap = Vector.tabulate(numBlocks, fn _ => createLabel())

        fun getBlockLabel blockNo = Vector.sub(blockToLabelMap, blockNo)

        local
            (* processed - set to true when a block has been processed. *)
            val processed = Array.array(numBlocks, false)
            fun haveProcessed n = Array.sub(processed, n)
            
            (* Find the blocks that reference this one.  This isn't essential but
               allows us to try to generate blocks in the order of the control
               flow.  This in turn may allow us to use short branches rather
               than long ones. *)
            val labelRefs = Array.array(numBlocks, [])

            datatype flowCode =
                FlowCodeSimple of int
            |   FlowCodeCMove of {code: instr list, trueJump: int, falseJump: int}
            
            (* Process this recursively to set the references.  If we have
               unreachable blocks, perhaps because they've been merged, we
               don't want to include them in the reference counting.
               This shouldn't happen now that IdentifyReferences removes
               unreferenced blocks. *)
            fun setReferences fromLabel toLabel =
                case Array.sub(labelRefs, toLabel) of
                    [] => (* Not yet visited at all. *)
                    let
                        val ExtendedBasicBlock{ flow, ...} = Vector.sub(blocks, toLabel)
                        val refs =
                            case flow of
                                ExitCode => []
                            |   Unconditional lab => [lab]
                            |   Conditional{trueJump, falseJump, ... } => [trueJump, falseJump]
                            |   IndexedBr labs => labs
                            |   SetHandler { handler, continue } => [handler, continue]
                            |   UnconditionalHandle _ => []
                            |   ConditionalHandle { continue, ...} => [continue]

                        val () =
                            if fromLabel >= 0 then Array.update(labelRefs, toLabel, [fromLabel]) else ()
                    in
                        List.app (setReferences toLabel) refs
                    end
                    
                |   refs =>
                    (* We've visiting this at least once.  Just add us to the list. *)
                        Array.update(labelRefs, toLabel, fromLabel :: refs)
            
            val _ = setReferences 0 0
            
            (* Process the blocks.  We keep the "stack" explicit rather than using recursion
               because this allows us to select both arms of a conditional branch sooner. *)
            fun genCode(toDo, lastFlow, code) =
            case List.filter (not o haveProcessed) toDo of
                [] =>
                let
                    (* There's nothing left to do. We may need to add a final branch to the end. *)
                    val finalBranch =
                        case lastFlow of
                            ExitCode => []
                        |   IndexedBr _ => []
                        |   Unconditional dest => [unconditionalBranch(getBlockLabel dest)]
                        |   Conditional { condition, trueJump, falseJump, ...} =>
                                [
                                    unconditionalBranch(getBlockLabel falseJump),
                                    conditionalBranch(condition, getBlockLabel trueJump)
                                ]
                        |   SetHandler { continue, ...} => [unconditionalBranch(getBlockLabel continue)]
                        |   UnconditionalHandle _ => []
                        |   ConditionalHandle { continue, ...} => [unconditionalBranch(getBlockLabel continue)]
                in
                    finalBranch @ code (* Done. *)
                end

            |   stillToDo as head :: _ =>
                let
                    local
                        (* Check the references.  If all the sources that lead up to this have
                           already been we won't have any backward jumps. *)
                        fun available dest = List.all haveProcessed (Array.sub(labelRefs, dest))

                        val continuation =
                            case lastFlow of
                                ExitCode => NONE
                            |   IndexedBr _ => NONE (* We could put the last branch in here. *)
                            |   Unconditional dest =>
                                    if not (haveProcessed dest) andalso available dest
                                    then SOME(FlowCodeSimple dest)
                                    else NONE
                            |   Conditional {trueJump, falseJump, condition, ...} =>
                                let
                                    (* Can we replace this with a SETCC or CMOV?  If both arms simply set
                                       a register to a value and either return or jump to the same location
                                       we can use a SETCC or a CMOV.  *)
                                    val ExtendedBasicBlock { flow=tFlow, block=tBlock, ...} = Vector.sub(blocks, trueJump)
                                    and ExtendedBasicBlock { flow=fFlow, block=fBlock, ...} = Vector.sub(blocks, falseJump)
                                in
                                        (* We can usually choose either destination and in nearly all cases
                                           it won't matter.  The default branch is not to take forward jumps
                                           so if there is reason to believe that one branch is more likely
                                           we should follow that branch now and leave the other.  If we
                                           have JO/JNO we assume that overflow is unusual.  If one branch
                                           raises an exception we assume that that is unusual. *)
                                        let
                                            val (first, second) =
                                                (*case (condition, Vector.sub(blocks, falseJump)) of
                                                |   _ =>*) (falseJump, trueJump)
                                        in
                                            if not (haveProcessed first) andalso available first
                                            then SOME(FlowCodeSimple first)
                                            else if not (haveProcessed second) andalso available second
                                            then SOME(FlowCodeSimple second)
                                            else NONE
                                        end
                                end
                           |    SetHandler { continue, ... } =>
                                    (* We want the continuation if possible.  We'll need a
                                       branch round the handler so that won't help. *)
                                    if not (haveProcessed continue) andalso available continue
                                    then SOME(FlowCodeSimple continue)
                                    else NONE
                           |    UnconditionalHandle _ => NONE
                           |    ConditionalHandle _ => NONE
                    in
                        (* First choice - continue the existing block.
                           Second choice - the first item whose sources have all been
                           processed.
                           Third choice - something from the list. *)
                        val picked =
                            case continuation of
                                SOME c => c
                            |   NONE =>
                                    case List.find available stillToDo of
                                        SOME c => FlowCodeSimple c
                                    |   NONE => FlowCodeSimple head
                    end
                    
                in
                    case picked of
                        FlowCodeSimple picked =>
                        let
                            val () = Array.update(processed, picked, true)

                            (* Code to terminate the previous block. *)
                            val startCode =
                                case lastFlow of
                                    ExitCode => []
                                |   IndexedBr _ => []
                                |   UnconditionalHandle _ => []
                                |   Unconditional dest =>
                                        if dest = picked then [] else [unconditionalBranch(getBlockLabel dest)]
                                |   ConditionalHandle { continue, ...} =>
                                        if continue = picked then [] else [unconditionalBranch(getBlockLabel continue)]
                                |   SetHandler { continue, ... } =>
                                        if continue = picked then [] else [unconditionalBranch(getBlockLabel continue)]
                                |   Conditional { condition, trueJump, falseJump, ...} =>
                                    if picked = falseJump (* Usual case. *)
                                    then [conditionalBranch(condition, getBlockLabel trueJump)]
                                    else if picked = trueJump
                                    then (* We have a jump to the true condition. Invert the jump.
                                            This is more than an optimisation.  Because this immediately precedes the
                                            true block we're not going to generate a label. *)
                                        [conditionalBranch(invertTest condition, getBlockLabel falseJump)]
                                    else
                                    [
                                        unconditionalBranch(getBlockLabel falseJump),
                                        conditionalBranch(condition, getBlockLabel trueJump)
                                    ]

                            (* Code-generate the body with the code we've done so far
                               at the end.  Add a label at the start if necessary. *)
                            local
                                (* If the previous block dropped through to this and this was
                                   the only reference then we don't need a label. *)
                                fun onlyJumpingHere (lab: int) =
                                    if lab <> picked then false
                                    else case Array.sub(labelRefs, picked) of
                                        [singleton] => singleton = lab
                                    |   _ => false
                    
                                val noLabel =
                                    case lastFlow of
                                        ExitCode => picked = 0 (* Unless this was the first block. *)
                                    |   Unconditional dest => onlyJumpingHere dest
                                    |   Conditional { trueJump, falseJump, ...} =>
                                            onlyJumpingHere trueJump orelse onlyJumpingHere falseJump
                                    |   IndexedBr _ => false
                                    |   SetHandler _ => false
                                    |   UnconditionalHandle _ => false
                                    |   ConditionalHandle { continue, ...} => onlyJumpingHere continue
                            in
                                val startLabel = if noLabel then [] else [setLabel(getBlockLabel picked)]
                            end

                            val ExtendedBasicBlock { flow, block, ...} = Vector.sub(blocks, picked)

                            local
                                fun genCodeBlock(instr, code) = codeExtended {flow=flow} (instr, code)
                            in
                                val bodyCode = List.foldl genCodeBlock (startLabel @ startCode @ code) block
                            end

                            val addSet =
                                case flow of
                                    ExitCode => []
                                |   IndexedBr cases => cases
                                |   Unconditional dest => [dest]
                                |   Conditional {trueJump, falseJump, ...} => [falseJump, trueJump]
                                |   SetHandler { handler, continue } => [handler, continue]
                                |   UnconditionalHandle _ => []
                                |   ConditionalHandle { continue, ...} => [continue]

                        in
                            genCode(addSet @ stillToDo, flow, bodyCode)
                        end
                
                    |   FlowCodeCMove{code, trueJump, falseJump} =>
                        let
                            (* We've generated a conditional move and possibly a return.  If the
                               trueJump and falseJump are only ever referenced from this block
                               they're done, otherwise we still need to do them. *)
                            val _ =
                                case Array.sub(labelRefs, trueJump) of
                                    [_] => Array.update(processed, trueJump, true)
                                |   _ => ()
                            val _ =
                                case Array.sub(labelRefs, falseJump) of
                                    [_] => Array.update(processed, falseJump, true)
                                |   _ => ()
                            val ExtendedBasicBlock { flow, ...} = Vector.sub(blocks, trueJump)
                            val addSet =
                                case flow of
                                    ExitCode => []
                                |   Unconditional dest => [dest]
                                |   _ => raise InternalError "FlowCodeCMove"
                        in
                            genCode(addSet @ stillToDo, flow, code)
                        end
                end
        in
            val ops = genCode([0], ExitCode, [])
        end
    in
        Arm64Assembly.generateCode{instrs=List.rev ops, name=functionName, resultClosure=resultClosure, parameters=debugSwitches}
    end

    structure Sharing =
    struct
        type intSet             = intSet
        and extendedBasicBlock  = extendedBasicBlock
        and regProperty         = regProperty
        and xReg                = xReg
        and closureRef          = closureRef
    end

end;
