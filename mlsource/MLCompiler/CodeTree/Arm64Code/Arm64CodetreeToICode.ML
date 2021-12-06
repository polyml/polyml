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

functor Arm64CodetreeToICode(
    structure BackendTree: BACKENDINTERMEDIATECODE
    structure Arm64ICode: ARM64ICODE
    structure Debug: DEBUG
    structure Arm64Foreign: FOREIGNCALL
    structure ICodeTransform: ARM64ICODETRANSFORM
    structure CodeArray: CODEARRAY
    
    structure Arm64Fallback: GENCODE

    sharing Arm64ICode.Sharing = ICodeTransform.Sharing = CodeArray.Sharing = Arm64Fallback.Sharing = BackendTree.Sharing
): GENCODE =
struct
    open BackendTree
    open Address
    open Arm64ICode
    open CodeArray
    
    exception InternalError = Misc.InternalError
    
    fun taggedWord64 w: Word64.word = w * 0w2 + 0w1
    and taggedWord w: word = w * 0w2 + 0w1
    
    datatype blockStruct =
        BlockSimple of arm64ICode
    |   BlockExit of arm64ICode
    |   BlockLabel of int
    |   BlockFlow of controlFlow
    |   BlockBegin of { regArgs: (preg * xReg) list, stackArgs: stackLocn list }
    |   BlockRaiseAndHandle of arm64ICode * int
    |   BlockOptionalHandle of {call: arm64ICode, handler: int, label: int }

    val polyWordLoadSize = if is32in64 then Load32 else Load64
    (* The flags byte is the high-order byte of length word. *)
    val flagsByteOffset = if isBigEndian then ~ (Word.toInt wordSize) else ~1

    fun opWordSize Load64 = 8
    |   opWordSize Load32 = 4
    |   opWordSize Load16 = 2
    |   opWordSize Load8 = 1

    fun codeFunctionToArm64({body, localCount, name, argTypes, closure, ...}:bicLambdaForm, debugSwitches, resultClosure) =
    let
        (* Pseudo-registers are allocated sequentially and the properties added to the list. *)
        val pregCounter = ref 0
        val pregPropList = ref []
        fun newPReg() =
        let
            val regNo = !pregCounter before pregCounter := !pregCounter + 1
            val () = pregPropList := RegPropGeneral :: !pregPropList
        in
            PReg regNo
        end
        
        and newUReg() =
        let
            val regNo = !pregCounter before pregCounter := !pregCounter + 1
            val () = pregPropList := RegPropUntagged :: !pregPropList
        in
            PReg regNo
        end
        
        and newStackLoc size =
        let
            val regNo = !pregCounter before pregCounter := !pregCounter + 1
            val () = pregPropList := RegPropStack size :: !pregPropList
        in
            StackLoc{size=size, rno=regNo}
        end
        
        and newMergeReg() =
        let
            val regNo = !pregCounter before pregCounter := !pregCounter + 1
            val () = pregPropList := RegPropMultiple :: !pregPropList
        in
            PReg regNo
        end
        
        datatype locationValue =
            NoLocation
        |   PregLocation of preg
        |   ContainerLocation of { container: stackLocn, stackOffset: int }

        val locToPregArray = Array.array(localCount, NoLocation)
        val labelCounter = ref 1 (* Start at 1.  Zero is used for the root. *)
        fun newLabel() = !labelCounter before labelCounter := !labelCounter + 1
        val ccRefCounter = ref 0
        fun newCCRef() = CcRef(!ccRefCounter) before ccRefCounter := !ccRefCounter + 1

        (* The profile object is a single mutable with the F_bytes bit set. *)        
        val profileObject = CodeArray.createProfileObject()

        (* Switch to indicate if we want to trace where live data has been allocated. *)
        (* TODO: This should be used in AllocateMemoryOperation and BoxValue and possibly AllocateMemoryVariable. *)
        val addAllocatingFunction =
            Debug.getParameter Debug.profileAllocationTag debugSwitches = 1

        datatype destination =
            SpecificPReg of preg
        |   NoResult
        |   AnyReg

        (* Context type. *)
        type context =
            { loopArgs: (preg list * int * int) option, stackPtr: int, currHandler: int option,
              overflowBlock: int option ref }

        datatype argLoc =
            ArgumentIsInReg of { realReg: xReg, argReg: preg }
        |   ArgumentIsOnStack of { stackOffset: int, stackReg: stackLocn }

        (* Pseudo-regs for the result, the closure and the args that were passed in real regs. *)
        val resultTarget = newPReg()
        val closureRegAddr = newPReg()
        val returnAddrReg = newPReg()
        
        val generalArgRegs = [X0, X1, X2, X3, X4, X5, X6, X7]

        (* Create a map for the arguments indicating their register or stack location. *)
        local
            (* Select the appropriate argument register depending on the argument type. *)
            fun argTypesToArgEntries([], _, _) = ([], [], [], [])

            |   argTypesToArgEntries(_ :: tl, gReg :: gRegs, n) =
                (* This deals with general arguments but also with extra floating point arguments.
                   They are boxed as usual. *)
                let
                    val (argTypes, argCode, argRegs, stackArgs) =
                        argTypesToArgEntries(tl, gRegs, n-1)
                    val argReg=newPReg()
                in
                    (ArgumentIsInReg{realReg=gReg, argReg=argReg} :: argTypes, argCode, (argReg, gReg) :: argRegs, stackArgs)
                end

            |   argTypesToArgEntries(_ :: tl, [], n) =
                let
                    val (argTypes, argCode, argRegs, stackArgs) = argTypesToArgEntries(tl, [], n-1)
                    val stackLoc = newStackLoc 1
                in
                    (ArgumentIsOnStack {stackOffset=n, stackReg = stackLoc } :: argTypes, argCode, argRegs, stackLoc :: stackArgs)
                end

            val (argEntries, argCode, argRegs, stackArguments) =
                argTypesToArgEntries(argTypes, generalArgRegs, List.length argTypes)
            val clReg = case closure of [] => [] | _ => [(closureRegAddr, X8)]
            val retReg = [(returnAddrReg, X30)]
        in
            val argumentVector = Vector.fromList argEntries

            (* Start code for the function. *)
            val beginInstructions = argCode @
                [BlockBegin{regArgs=retReg @ clReg @ argRegs, stackArgs=stackArguments }]

            (* The number of arguments on the stack.  Needed in return instrs and tail calls. *)
            val currentStackArgs = List.length stackArguments
        end

        fun returnInstruction({stackPtr, ...}, resReg, tailCode) =
        let
        in
            BlockExit(ReturnResultFromFunction{resultReg=resReg, returnReg = returnAddrReg, numStackArgs=currentStackArgs}) ::
                (if stackPtr <> 0 then BlockSimple(ResetStackPtr{numWords=stackPtr}) :: tailCode else tailCode)
        end

        fun asTarget(SpecificPReg preg) = preg
        |   asTarget _ = newPReg()

        fun moveToResult(SpecificPReg tReg, code, sReg) =
                (BlockSimple(MoveRegister{source=sReg, dest=tReg}) :: code, tReg, false)
        |   moveToResult(AnyReg, code, sReg) = (code, sReg, false)
        |   moveToResult(NoResult, code, sReg) =
            let
                val tReg = newPReg()
            in
                (BlockSimple(MoveRegister{source=sReg, dest=tReg}) :: code, tReg, false)
            end

        (* Store a register at a given offset.  This may have to use an index register
           if the offset is too large. *)
        fun storeAtWordOffset(toStore, offset, base, loadSize, tailCode) =
        let
            val wSize = opWordSize loadSize
            val byteOffset = offset*wSize
        in
            if offset < 4096 andalso byteOffset > ~256 
            then BlockSimple(
                    StoreWithConstantOffset{base=base, source=toStore,
                            byteOffset=byteOffset, loadType=loadSize}) :: tailCode
            else
            let
                val indexReg = newUReg()
            in
                BlockSimple(StoreWithIndexedOffset{ base=base, source=toStore, index=indexReg, loadType=loadSize }) ::
                    BlockSimple(LoadNonAddressConstant{ source=LargeWord.fromInt offset, dest=indexReg }) :: tailCode
            end
        end

        (* Allocate a fixed size cell with a reference to the profile object if we want
           to trace the location of live data.  Currently only used for tuples and closures. *)
        fun allocateWithProfileRev(n, flags, memAddr, tlCode) =
        let
            fun doAllocation(words, flags, tlCode) =
            let
                val wordsRequired =
                    if is32in64
                    then (* Have to round this up to 8 bytes *)
                        Word64.andb(Word64.fromInt(words+2), ~ 0w2)
                    else Word64.fromInt(words+1)
                val bytesRequired = Word64.fromLarge(Word.toLarge wordSize) * wordsRequired
                val lengthWord =
                    Word64.orb(Word64.fromInt words, Word64.<<(Word64.fromLarge(Word8.toLarge flags),
                        if is32in64 then 0w24 else 0w56))
                val lengthReg = newUReg()
            in
                BlockSimple(StoreWithConstantOffset{ source=lengthReg, base=memAddr, byteOffset= ~(Word.toInt wordSize), loadType=polyWordLoadSize }) ::
                BlockSimple(LoadNonAddressConstant{ source=lengthWord, dest=lengthReg }) ::
                BlockSimple(AllocateMemoryOperation{bytesRequired=bytesRequired, dest=memAddr, saveRegs=[]}) :: tlCode
            end
        in
            if addAllocatingFunction
            then
            let
                val profReg = newPReg()
            in
                storeAtWordOffset(profReg, n, memAddr, polyWordLoadSize,
                    BlockSimple(LoadAddressConstant{ source=profileObject, dest=profReg}) ::
                        doAllocation(n+1, Word8.orb(flags, Address.F_profile), tlCode))
            end
            else doAllocation(n, flags, tlCode)
        end

        (* Return a unit result. *)
        fun returnUnit(target, code, exit) =
        let
            val tReg = asTarget target
        in
            (BlockSimple(LoadNonAddressConstant{source=taggedWord64 0w0, dest=tReg}) :: code, tReg, exit)
        end

        (* Create a bool result from a test by returning true or false. *)
        fun makeBoolResultRev(condition, ccRef, target, testCode) =
        let
            val trueLab = newLabel() and falseLab = newLabel() and mergeLab = newLabel()
            val mergeReg = newMergeReg()
        in
            BlockSimple(MoveRegister{dest=target, source=mergeReg}) ::
            BlockLabel mergeLab ::
            BlockFlow(Unconditional mergeLab) ::
            BlockSimple(LoadNonAddressConstant{dest=mergeReg, source=taggedWord64 0w0}) ::
            BlockLabel falseLab ::
            BlockFlow(Unconditional mergeLab) ::
            BlockSimple(LoadNonAddressConstant{dest=mergeReg, source=taggedWord64 0w1}) ::
            BlockLabel trueLab ::
            BlockFlow(Conditional{ ccRef=ccRef, condition=condition, trueJump=trueLab, falseJump=falseLab }) ::
            testCode
        end

        (* Load a value aligned on a 64 or 32-bit boundary.  offset is the number
           of units.  Typically this will be a polyword. *)
        fun wordAddressOffset(destination, baseReg1, offset, loadOp, code) =
        let
            val dReg = asTarget destination
            val opWordSize = opWordSize loadOp
            val byteOffset = offset * opWordSize
            val (codeBase, baseReg) =
                if is32in64
                then
                let
                    val absReg = newUReg()
                in
                    (BlockSimple(ObjectIndexAddressToAbsolute{ source=baseReg1, dest=absReg }) :: code, absReg)
                end
                else (code, baseReg1)
            val code =
                if offset < 4096 andalso byteOffset > ~256
                then BlockSimple(LoadWithConstantOffset{base=baseReg, dest=dReg, byteOffset=byteOffset, loadType=loadOp}) :: codeBase
                else
                let
                    val indexReg = newUReg()
                in
                    BlockSimple(LoadWithIndexedOffset{ base=baseReg, dest=dReg, index=indexReg, loadType=loadOp }) ::
                        BlockSimple(LoadNonAddressConstant{ source=LargeWord.fromInt offset, dest=indexReg }) :: codeBase
                end
        in
            (code, dReg, false)
        end

        (* See if we have a container and return the entry if present. *)
        fun getContainerIfPresent(BICExtract(BICLoadLocal l)) =
            ( case Array.sub(locToPregArray, l) of ContainerLocation container => SOME container | _ => NONE )
        |   getContainerIfPresent _ = NONE


        fun codeToICodeRev(BICNewenv (bindings, exp), context: context as {stackPtr=initialSp, ...}, isTail, destination, tailCode) =
            let
                (* Process a list of bindings.  We need to accumulate the space used by
                   any containers and reset the stack pointer at the end if necessary. *)
                fun doBindings([], context, tailCode) = (tailCode, context)
 
                |   doBindings(BICDeclar{value=BICExtract(BICLoadLocal l), addr, ...} :: decs, context, tailCode) =
                    let
                        (* Giving a new name to an existing entry.  This should have been removed
                           at a higher level but it doesn't always seem to be.  In particular we
                           must treat this specially if it's a container. *)
                        val original = Array.sub(locToPregArray, l)
                        val () = Array.update(locToPregArray, addr, original)
                    in
                        doBindings(decs, context, tailCode)
                    end

                |   doBindings(BICDeclar{value, addr, ...} :: decs, context, tailCode) =
                    let
                        val (code, dest, _) = codeToICodeRev(value, context, false, AnyReg, tailCode)
                        val () = Array.update(locToPregArray, addr, PregLocation dest)
                    in
                        doBindings(decs, context, code)
                    end

                |   doBindings(BICRecDecs [{lambda, addr, ...}] :: decs, context, tailCode) =
                    (* We shouldn't have single entries in RecDecs but it seems to occur at the moment. *)
                    let
                        val dest = newPReg()
                        val (code, _, _) = codeToICodeRev(BICLambda lambda, context, false, SpecificPReg dest, tailCode)
                        val () = Array.update(locToPregArray, addr, PregLocation dest)
                    in
                        doBindings(decs, context, code)
                    end

                |   doBindings(BICRecDecs recDecs :: decs, context, tailCode) =
                    let
                        val destRegs = map (fn _ => newPReg()) recDecs
                        val flagsValue = if is32in64 then F_closure else 0w0
                        (* First build the closures as mutable cells containing zeros.  Set the
                           entry in the address table to the register containing the address. *)
                        fun makeClosure({lambda={closure, ...}, addr, ...}, dest, tailCode) =
                        let
                            val () = Array.update(locToPregArray, addr, PregLocation dest)
                            val wordsRequired = List.length closure + (if is32in64 then 2 else 1)
                            val absAddr = if is32in64 then newUReg() else dest
                            val zeroReg = newPReg()
                            val allocAndSetZero =
                                BlockSimple(LoadNonAddressConstant{ source=taggedWord64 0w0, dest=zeroReg}) ::
                                    allocateWithProfileRev(wordsRequired, Word8.orb(F_mutable, flagsValue), absAddr, tailCode)
                            val (_, clearCode) =
                                List.foldl(fn (_, (n, l)) =>
                                    (n+1, storeAtWordOffset(zeroReg, n, absAddr, polyWordLoadSize, l))) (0, allocAndSetZero) closure
                        in
                            if is32in64
                            then BlockSimple(AbsoluteToObjectIndex{ source=absAddr, dest=dest }) :: clearCode
                            else clearCode
                        end
                        val allocClosures = ListPair.foldlEq makeClosure tailCode (recDecs, destRegs)

                        fun setClosure({lambda, ...}, dest, l) =
                        let
                            val absAddr = if is32in64 then newUReg() else dest
                            val flagsReg = newUReg()
                            (* Lock the closure by storing the flags byte without the mutable flag.
                               TODO: We could simply use XZ here. *)
                        in
                            BlockSimple(StoreWithConstantOffset{ base=absAddr, source=flagsReg, byteOffset=flagsByteOffset,
                                    loadType=Load8 }) ::
                            BlockSimple(LoadNonAddressConstant{ source=Word8.toLarge flagsValue, dest=flagsReg }) ::
                            storeIntoClosure(lambda, absAddr, context,
                                    if is32in64
                                    then BlockSimple(ObjectIndexAddressToAbsolute{ source=dest, dest=absAddr }) :: l else l)
                        end
                            
                        val setAndLockClosures = ListPair.foldlEq setClosure allocClosures (recDecs, destRegs)
                    in
                        doBindings(decs, context, setAndLockClosures)
                    end

                |   doBindings(BICNullBinding exp :: decs, context, tailCode) =
                    let
                        val (code, _, _) = codeToICodeRev(exp, context, false, NoResult, tailCode) (* And discard result. *)
                    in
                        doBindings(decs, context, code)
                    end
       
                |   doBindings(BICDecContainer{ addr, size } :: decs, {loopArgs, stackPtr, currHandler, overflowBlock}, tailCode) =
                    let
                        val containerLoc = newStackLoc size
                        val () = Array.update(locToPregArray, addr,
                                    ContainerLocation{container=containerLoc, stackOffset=stackPtr+size})
                        val zeroReg = newPReg()
                    in
                        doBindings(decs,
                            {loopArgs=loopArgs, stackPtr=stackPtr+size, currHandler=currHandler, overflowBlock=overflowBlock},
                            BlockSimple(PushToStack{copies=size, container=containerLoc, source=zeroReg}) ::
                            BlockSimple(LoadNonAddressConstant{ source=taggedWord64 0w0, dest=zeroReg }) :: tailCode)
                    end

                val (codeBindings, resContext as {stackPtr=finalSp, ...}) = doBindings(bindings, context, tailCode)
                (* If we have had a container we'll need to reset the stack *)
            in
                if initialSp <> finalSp
                then
                let
                    val _ = finalSp >= initialSp orelse raise InternalError "codeToICode - stack ptr"
                    val bodyReg = newPReg() and resultReg = asTarget destination
                    val (codeExp, result, haveExited) =
                        codeToICodeRev(exp, resContext, isTail, SpecificPReg bodyReg, codeBindings)
                    val afterAdjustSp =
                        if haveExited
                        then codeExp
                        else
                            BlockSimple(MoveRegister{source=result, dest=resultReg}) ::
                            BlockSimple(ResetStackPtr{numWords=finalSp-initialSp}) :: codeExp
                in
                    (afterAdjustSp, resultReg, haveExited)
                end
                else codeToICodeRev(exp, resContext, isTail, destination, codeBindings)
            end

        |   codeToICodeRev(BICExtract(BICLoadLocal l), {stackPtr, ...}, _, destination, tailCode) =
            (
                case Array.sub(locToPregArray, l) of
                    NoLocation => raise InternalError "codeToICodeRev - local unset"
                |   PregLocation preg => moveToResult(destination, tailCode, preg)
                |   ContainerLocation{container, stackOffset} =>
                    let
                        val target = asTarget destination
                    in
                        (BlockSimple(ContainerAddress{dest=target, container=container, stackOffset=stackPtr-stackOffset})
                            :: tailCode, target, false)
                    end
            )

        |   codeToICodeRev(BICExtract(BICLoadArgument a), {stackPtr, ...}, _, destination, tailCode) =
            (
                case Vector.sub(argumentVector, a) of
                    ArgumentIsInReg{argReg, ...} => (* It was originally in a register.  It's now in a preg. *)
                        moveToResult(destination, tailCode, argReg)
                |   ArgumentIsOnStack{stackOffset, stackReg} => (* Pushed before call. *)
                    let
                        val target = asTarget destination
                    in
                        (BlockSimple(LoadStack{wordOffset=stackOffset+stackPtr, container=stackReg, field=0,
                                               dest=target}) :: tailCode, target, false)
                    end
            )

        |   codeToICodeRev(BICExtract(BICLoadClosure c), _, _, destination, tailCode) =
            let
                (* Add the number of words for the code address.  This is 1 in native but 2 in 32-in-64. *)
                val offset = if is32in64 then c+2 else c+1
            in
                if c >= List.length closure then raise InternalError "BICExtract: closure" else ();
                wordAddressOffset(destination, closureRegAddr, offset, polyWordLoadSize, tailCode)
            end

        |   codeToICodeRev(BICExtract BICLoadRecursive, _, _, destination, tailCode) =
                (* If the closure is empty we must use the constant.  We can't guarantee that
                   the caller will actually load the closure register if it knows the closure
                   is empty. *)
            (
                 case closure of
                    [] =>
                        let
                            val dReg = asTarget destination
                        in
                            (BlockSimple(LoadAddressConstant{source=closureAsAddress resultClosure, dest=dReg}) :: tailCode, dReg, false)
                        end
                 |  _ => moveToResult(destination, tailCode, closureRegAddr)
            )

        |   codeToICodeRev(BICConstnt(w, _), _, _, destination, tailCode) =
            let
                val dReg = asTarget destination
                val instr =
                    if isShort w
                    then (* When converting to Word64 we do NOT want to use sign-extension.
                            In 32-in-64 signed fixed-precision ints need to have zeros
                            in the top 32 bits. *)
                        LoadNonAddressConstant{source=taggedWord64(Word64.fromLarge(Word.toLarge(toShort w))),
                            dest=dReg}
                    else LoadAddressConstant{source=w, dest=dReg}
            in
                (BlockSimple instr :: tailCode, dReg, false)
            end

        |   codeToICodeRev(BICField{base, offset}, context, _, destination, tailCode) =
            let
                val (codeBase, baseReg, _) = codeToICodeRev(base, context, false, AnyReg, tailCode)
            in
                wordAddressOffset(destination, baseReg, offset, polyWordLoadSize, codeBase)
            end

        |   codeToICodeRev(BICCond(test, thenPt, elsePt), context, isTail, NoResult, tailCode) =
            let
                (* If we don't want the result but are only evaluating for side-effects we
                   may be able to optimise special cases.  This was easier in the forward
                   case but for now we don't bother and leave it to the lower levels. *)
                val startElse = newLabel() and skipElse = newLabel()
                val codeTest = codeConditionRev(test, context, false, startElse, tailCode)
                val (codeThen, _, _) =
                    codeToICodeRev(thenPt, context, isTail, NoResult, codeTest)
                val (codeElse, _, _) =
                     codeToICodeRev(elsePt, context, isTail, NoResult,
                        BlockLabel startElse ::
                        BlockFlow(Unconditional skipElse) :: codeThen)
            in
                returnUnit(NoResult, BlockLabel skipElse :: codeElse, false(*??*))
            end

        |   codeToICodeRev(BICCond(test, thenPt, elsePt), context, isTail, destination, tailCode) =
            let
                (* Because we may push the result onto the stack we have to create a new preg to
                   hold the result and then copy that to the final result. *)
                (* If this is a tail each arm will exit separately and neither will return a result. *)
                val target = asTarget destination
                val condResult = newMergeReg()
                val thenTarget = if isTail then newPReg() else condResult
                val startElse = newLabel()
                val testCode = codeConditionRev(test, context, false, startElse, tailCode)
                
                (* Put the result in the target register. *)
                val (thenCode, _, thenExited) = codeToICodeRev(thenPt, context, isTail, SpecificPReg thenTarget, testCode)
                (* Add a jump round the else-part except that if this is a tail we
                   return.  The then-part could have exited e.g. with a raise or a loop. *)
                val (exitThen, thenLabel, elseTarget) =
                    if thenExited then (thenCode, [], target (* Can use original target. *))
                    else if isTail then (returnInstruction(context, thenTarget, thenCode), [], newPReg())
                    else
                    let
                        val skipElse = newLabel()
                    in
                        (BlockFlow(Unconditional skipElse) :: thenCode,
                         [BlockSimple(MoveRegister{source=condResult, dest=target}),
                          BlockLabel skipElse],
                         condResult)
                    end
                val (elseCode, _, elseExited) =
                    codeToICodeRev(elsePt, context, isTail, SpecificPReg elseTarget,
                        BlockLabel startElse :: exitThen)
                (* Add a return to the else-part if necessary so we will always exit on a tail. *)
                val exitElse =
                    if isTail andalso not elseExited
                    then returnInstruction(context, elseTarget, elseCode) else elseCode
            in
                (thenLabel @ exitElse, target, isTail orelse thenExited andalso elseExited)
            end

        |   codeToICodeRev(BICUnary instr, context, isTail, destination, tailCode) =
                codeToICodeUnaryRev(instr, context, isTail, destination, tailCode)

        |   codeToICodeRev(BICBinary instr, context, isTail, destination, tailCode) =
                codeToICodeBinaryRev(instr, context, isTail, destination, tailCode)

        |   codeToICodeRev(BICTagTest{test, tag=tagValue, ...}, context, isTail, destination, tailCode) =
            (* Check the "tag" word of a union (datatype).  N.B.  Not the same as testing the
               tag bit of a word.  Just generate it as a general word comparison.  The optimiser
               will sort out whether the tag value can be an immediate. *)
                codeToICodeRev(BICBinary{oper=BuiltIns.WordComparison{test=BuiltIns.TestEqual, isSigned=false},
                                         arg1=test, arg2=BICConstnt(toMachineWord tagValue, [])},
                        context, isTail, destination, tailCode)

        |   codeToICodeRev(BICTuple fields, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                (* The allocator sets the register to the absolute address.  It
                   has to be converted to an object pointer in 32-in-64. *)
                val absAddr = if is32in64 then newUReg() else target
                
                fun loadFields([], n, tlCode) = allocateWithProfileRev(n, 0w0, absAddr, tlCode)
                
                |   loadFields((f as BICConstnt _) :: rest, n, tlCode) =
                    let
                        (* Unlike the X86 we still need to load a constant into a register in order to
                           store it in the new tuple.  However, it's better to leave that until after
                           the allocation and move it then.  That way we can use the same register
                           for different constants if we have a very large tuple. *)
                        val restAndAlloc = loadFields(rest, n+1, tlCode)
                        val (code1, source, _) = codeToICodeRev(f, context, false, AnyReg, restAndAlloc)
                    in
                        storeAtWordOffset(source, n, absAddr, polyWordLoadSize, code1)
                    end
                    
                |   loadFields(f :: rest, n, tlCode) =
                    let
                        val (code1, source, _) = codeToICodeRev(f, context, false, AnyReg, tlCode)
                        val restAndAlloc = loadFields(rest, n+1, code1)
                    in
                        storeAtWordOffset(source, n, absAddr, polyWordLoadSize, restAndAlloc)
                    end
                val allocAndStore = loadFields(fields, 0, tailCode)
                val code =
                    if is32in64
                    then BlockSimple(AbsoluteToObjectIndex{source=absAddr, dest=target}) :: allocAndStore
                    else allocAndStore
            in
                (code, target, false)
            end

        |   codeToICodeRev(BICRaise exc, context as { currHandler, ...}, _, destination, tailCode) =
            let
                val (code, packetReg, _) = codeToICodeRev(exc, context, false, AnyReg, tailCode)
                val raiseCode = RaiseExceptionPacket{packetReg=packetReg}
                val block =
                    case currHandler of
                        NONE => BlockExit raiseCode | SOME h => BlockRaiseAndHandle(raiseCode, h)
            in
                returnUnit(destination,  block :: code, true (* Always exits *))
            end

        |   codeToICodeRev(BICEval{function, argList, ...}, context as { currHandler, ...}, isTail, destination, tailCode) =
            let
                val target = asTarget destination
                (* Create pregs for the closure and each argument. *)
                val clPReg = newPReg()
                (* If we have a constant closure we can go directly to the entry point.
                   If the closure is a single word we don't need to load the closure register. *)
                val (functionCode, closureEntry, callKind) =
                    case function of
                        BICConstnt(addr, _) =>
                        let
                            val addrAsAddr = toAddress addr
                            (* If this is a closure we're still compiling we can't get the code address.
                               However if this is directly recursive we can use the recursive
                               convention. *)
                        in
                            if wordEq(closureAsAddress resultClosure, addr)
                            then (tailCode, [], Recursive)
                            else if flags addrAsAddr <> Address.F_words andalso flags addrAsAddr <> Address.F_closure
                            then (BlockSimple(LoadAddressConstant{source=addr, dest=clPReg}) :: tailCode,
                                      [(ArgInReg clPReg, X8)], FullCall)
                            else if is32in64
                            then (* We can't actually load the code address here. *)
                            let
                                val addrLength = length addrAsAddr
                                val _ = addrLength >= 0w1 orelse raise InternalError "BICEval address"
                                val _ = flags addrAsAddr = Address.F_closure orelse raise InternalError "BICEval address not a closure"
                            in
                                if addrLength = 0w2
                                then (tailCode, [], ConstantCode addr)
                                else (BlockSimple(LoadAddressConstant{source=addr, dest=clPReg}) :: tailCode,
                                      [(ArgInReg clPReg, X8)], ConstantCode addr)
                            end
                            else (* Native 64-bits. *)
                            let
                                val addrLength = length addrAsAddr
                                val _ = addrLength >= 0w1 orelse raise InternalError "BICEval address"
                                val codeAddr = loadWord(addrAsAddr, 0w0)
                                val _ = isCode (toAddress codeAddr) orelse raise InternalError "BICEval address not code"
                            in
                                if addrLength = 0w1
                                then (tailCode, [], ConstantCode codeAddr)
                                else (BlockSimple(LoadAddressConstant{source=addr, dest=clPReg}) :: tailCode,
                                      [(ArgInReg clPReg, X8)], ConstantCode codeAddr)
                            end
                        end

                    |   BICExtract BICLoadRecursive =>
                        (
                            (* If the closure is empty we don't need to load rdx *)
                            case closure of
                                [] => (tailCode, [], Recursive)
                            |   _ =>
                                    (BlockSimple(MoveRegister {source=closureRegAddr, dest=clPReg}) :: tailCode,
                                     [(ArgInReg clPReg, X8)], Recursive)
                        )

                    |   function => (* General case. *)
                            (#1 (codeToICodeRev(function, context, false, SpecificPReg clPReg, tailCode)), [(ArgInReg clPReg, X8)], FullCall)

                (* Load the first arguments into registers and the rest to the stack. *)
                fun loadArgs ([], _, tailCode) = (tailCode, [], [])

                |   loadArgs ((arg, _) :: args, gReg::gRegs, tailCode) =
                    let (* General register argument. *)
                        val (c, r, _) = codeToICodeRev(arg, context, false, AnyReg, tailCode)
                        val (code, regArgs, stackArgs) = loadArgs(args, gRegs, c)
                    in
                        (code, (ArgInReg r, gReg) :: regArgs, stackArgs)
                    end

                |   loadArgs ((arg, _) :: args, [], tailCode) =
                    let (* Stack argument. *)
                        val (c, r, _) = codeToICodeRev(arg, context, false, AnyReg, tailCode)
                        val (code, regArgs, stackArgs) = loadArgs(args, [], c)
                    in
                        (code, regArgs, ArgInReg r :: stackArgs)
                    end

                val (codeArgs, regArgs, stackArgs) = loadArgs(argList, generalArgRegs, functionCode)
                
                (* If this is at the end of the function and the result types are the
                   same we can use a tail-recursive call. *)
                val tailCall = isTail (*andalso resultType = fnResultType*)
                
                val callCode =
                    if tailCall
                    then
                    let
                        val {stackPtr, ...} = context
                        (* The number of arguments currently on the stack. *)
                        val currentStackArgCount = currentStackArgs
                        val newStackArgCount = List.length stackArgs
                        (* The offset of the first argument or the return address if there are
                           no stack arguments.  Offsets can be negative. *)
                        val stackOffset = stackPtr
                        val firstArgumentAddr = currentStackArgCount
                        fun makeStackArgs([], _) = []
                        |   makeStackArgs(arg::args, offset) = {src=arg, stack=offset} :: makeStackArgs(args, offset-1)
                        val stackArgs = makeStackArgs(stackArgs, firstArgumentAddr)
                        (* The stack adjustment needed to compensate for any items that have been pushed
                           and the differences in the number of arguments.  May be positive or negative.
                           This is also the destination address of the return address so when we enter
                           the new function the return address will be the first item on the stack. *)
                        val stackAdjust = firstArgumentAddr - newStackArgCount
                        (* Add an entry for the return address to the stack arguments. *)
                    in
                        BlockExit(TailRecursiveCall{regArgs=(ArgInReg returnAddrReg, X30) :: closureEntry @ regArgs, stackArgs=stackArgs,
                                  stackAdjust = stackAdjust, currStackSize=stackOffset, callKind=callKind}) ::
                                  codeArgs
                    end
                    else
                    let
                        val call =
                            FunctionCall{regArgs=closureEntry @ regArgs, stackArgs=stackArgs, dest=target,
                                         callKind=callKind, saveRegs=[]}
                        val callBlock =
                            case currHandler of
                                NONE => BlockSimple call :: codeArgs
                            |   SOME h => BlockOptionalHandle{call=call, handler=h, label=newLabel()}  :: codeArgs
                    in
                        callBlock
                    end
            in
                (callCode, target, tailCall (* We've exited if this was a tail jump *))
            end

        |   codeToICodeRev(BICNullary _, _, _, _, _) = raise Fallback "codeToICodeRev: BICNullary"

        |   codeToICodeRev(BICArbitrary _, _, _, _, _) = raise Fallback "codeToICodeRev: BICArbitrary"

        |   codeToICodeRev(BICLambda(lambda as { closure = [], ...}), _, _, destination, tailCode) =
            (* Empty closure - create a constant closure for any recursive calls. *)
            let
                val closure = makeConstantClosure()
                val () = codeFunctionToArm64(lambda, debugSwitches, closure)
                val dReg = asTarget destination
                (* Return the closure itself as the value. *)
            in
                (BlockSimple(LoadAddressConstant{source=closureAsAddress closure, dest=dReg}) :: tailCode, dReg, false)
            end

        |   codeToICodeRev(BICLambda(lambda as { closure, ...}), context, _, destination, tailCode) =
            (* Non-empty closure.  Ignore stack closure option at the moment. *)
            let
                val wordsRequired = List.length closure  + (if is32in64 then 2 else 1)
                val target = asTarget destination
                val absAddr = if is32in64 then newUReg() else target
                (* The values we're storing are all either constants or local/closure variables so
                   we can allocate the memory and then store into it. *)
                val allocCode =
                    allocateWithProfileRev(wordsRequired, if is32in64 then F_closure else 0w0, absAddr, tailCode)
                val storeCode =
                    storeIntoClosure(lambda, absAddr, context, allocCode)
                val finalCode =
                    if is32in64 then BlockSimple(AbsoluteToObjectIndex{source=absAddr, dest=target}) :: storeCode else storeCode
            in
                (finalCode, target, false)
            end

        |   codeToICodeRev(BICCase _, _, _, _, _) = raise Fallback "codeToICodeRev: BICCase"

        |   codeToICodeRev(BICBeginLoop _, _, _, _, _) = raise Fallback "codeToICodeRev: BICBeginLoop"

        |   codeToICodeRev(BICLoop _, _, _, _, _) = raise Fallback "codeToICodeRev: BICLoop"

            (* Copy the source tuple into the container.  There are important special cases for
               both the source tuple and the container.  If the source tuple is a BICTuple we have
               the fields and can store them without creating a tuple on the heap.  If the
               destination is a local container we can store directly into the stack. *)
        |   codeToICodeRev(BICSetContainer{container, tuple, filter}, context as {stackPtr, ...}, _, destination, tailCode) =
            let
                local
                    fun createStore containerReg (source, destWord, tail) =
                        storeAtWordOffset(source, destWord, containerReg, Load64, tail)
                in
                    val findContainer =
                        case container of
                            BICExtract(BICLoadLocal l) =>
                            (
                                case Array.sub(locToPregArray, l) of
                                    ContainerLocation{container, stackOffset} =>
                                    let
                                        fun storeToStack(source, destWord, tail) =
                                            BlockSimple(StoreToStack{source=source, container=container, field=destWord,
                                                stackOffset=stackPtr-stackOffset+destWord}) :: tail
                                    in
                                        SOME storeToStack
                                    end
                               |    _ => NONE
                           )
                       |    _ => NONE

                    val (codeContainer, storeInstr) =
                        case findContainer of
                            SOME storeToStack => (tailCode, storeToStack)
                        |   NONE => 
                            let
                                val containerTarget = newPReg()
                                val (codeContainer, _, _) =
                                    codeToICodeRev(container, context, false, SpecificPReg containerTarget, tailCode)
                            in
                                (codeContainer, createStore containerTarget)
                            end
                end
                
                val filterLength = BoolVector.length filter

                val code =
                    case tuple of
                        BICTuple cl =>
                        let
                            (* In theory it's possible that the tuple could contain fields that are not
                               used but nevertheless need to be evaluated for their side-effects.
                               Create all the fields and push to the stack. *)
                            fun codeField(arg, (regs, tailCode)) =
                            let
                                val (c, r, _) =
                                    codeToICodeRev(arg, context, false, AnyReg, tailCode)
                            in
                                (r :: regs, c)
                            end

                            val (pregsRev, codeFields) = List.foldl codeField ([], codeContainer) cl
                            val pregs = List.rev pregsRev

                            fun copyField(srcReg, (sourceWord, destWord, tailCode)) =
                                if sourceWord < filterLength andalso BoolVector.sub(filter, sourceWord)
                                then (sourceWord+1, destWord+1, storeInstr(srcReg, destWord, tailCode))
                                else (sourceWord+1, destWord, tailCode)
                            
                            val (_, _, resultCode) = List.foldl copyField (0, 0, codeFields) pregs
                        in
                            resultCode
                        end

                    |   tuple =>
                        let (* Copy a heap tuple.  It is possible that this is another container in which case
                               we must load the fields directly.  We mustn't load its address and then copy
                               because loading the address would be the last reference and might cause
                               the container to be reused prematurely. ??? Is that an old comment ?? *)
                            val (codeTuple, loadField) =
                                case getContainerIfPresent tuple of
                                    SOME {container, stackOffset} =>
                                    let
                                        fun getAddr(destReg, sourceWord, tail) =
                                            BlockSimple(LoadStack{dest=destReg, wordOffset=stackPtr-stackOffset+sourceWord, container=container,
                                                          field=sourceWord}) :: tail
                                    in
                                        (codeContainer, getAddr)
                                    end
                                |   NONE =>
                                    let
                                        val (codeTuple, tupleTarget, _) = codeToICodeRev(tuple, context, false, AnyReg, codeContainer)
                                        fun loadField(destReg: preg, sourceWord: int, tail): blockStruct list =
                                        let
                                            val (code, _, _) =
                                                wordAddressOffset(SpecificPReg destReg, tupleTarget, sourceWord, polyWordLoadSize, tail)
                                        in
                                            code
                                        end
                                    in
                                        (codeTuple, loadField)
                                    end

                            fun copyContainer(sourceWord, destWord, tailCode) =
                            if sourceWord = filterLength
                            then tailCode
                            else if BoolVector.sub(filter, sourceWord)
                            then
                            let
                                val loadReg = newPReg()
                                val code =
                                    storeInstr(loadReg, destWord, loadField(loadReg, sourceWord, tailCode))
                            in
                                copyContainer(sourceWord+1, destWord+1, code)
                            end
                            else copyContainer(sourceWord+1, destWord, tailCode)
                        in
                            copyContainer(0, 0, codeTuple)
                        end
            in
                returnUnit(destination, code, false)
            end

        |   codeToICodeRev(BICLoadContainer{base, offset}, context as {stackPtr, ...}, _, destination, tailCode) =
            (
                case getContainerIfPresent base of
                    SOME {container, stackOffset} =>
                    let (* If this is a local container we extract the field. *)
                        val target = asTarget destination
                        val finalOffset = stackPtr-stackOffset+offset
                        val _ = finalOffset >= 0 orelse raise InternalError "offset"
                    in
                        (BlockSimple(LoadStack{wordOffset=finalOffset, container=container, field=offset,
                            dest=target}) :: tailCode, target, false)
                    end

                |   NONE =>
                    let
                        val (codeBase, baseEntry, _) = codeToICodeRev(base, context, false, AnyReg, tailCode)
                    in
                        wordAddressOffset(destination, baseEntry, offset, Load64, codeBase)
                    end
            )

        |   codeToICodeRev(BICLoadOperation _, _, _, _, _) = raise Fallback "codeToICodeRev: BICLoadOperation"

        |   codeToICodeRev(BICStoreOperation _, _, _, _, _) = raise Fallback "codeToICodeRev: BICStoreOperation"

        |   codeToICodeRev(BICBlockOperation _, _, _, _, _) = raise Fallback "codeToICodeRev: BICBlockOperation"

        |   codeToICodeRev(BICAllocateWordMemory _, _, _, _, _) = raise Fallback "codeToICodeRev: BICAllocateWordMemory"

        |   codeToICodeRev(BICHandle _, _, _, _, _) = raise Fallback "codeToICodeRev: BICAllocateWordMemory"

        and codeConditionRev(condition, context, jumpOn, jumpLabel, tailCode) =
       (* Jump optimisation is done later.  Just generate the general case.
          Load the value into a register and compare it with 1 (true) *)
        let
            val ccRef = newCCRef()
            val (testCode, testReg, _) = codeToICodeRev(condition, context, false, AnyReg, tailCode)
            val noJumpLabel = newLabel()
        in
            BlockLabel noJumpLabel ::
            BlockFlow(Conditional{ccRef=ccRef,
                       condition=if jumpOn then CondEqual else CondNotEqual, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
            (* Compare: SUBS XZ,reg,3.  Can use 32-bit comparison because it's either tagged 0 or tagged 1. *)
            BlockSimple(AddSubImmediate{source=testReg, immed=taggedWord 0w1, isAdd=false, dest=NONE, length=Arith32, ccRef=SOME ccRef}) ::
            testCode
        end

        and codeToICodeUnaryRev({oper=BuiltIns.NotBoolean, arg1}, context, _, destination, tailCode) =
            let
                val target = asTarget destination
                val ccRef = newCCRef()
                val (argCode, testDest, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
            in
                (* Test the argument and return a boolean result.  If either the argument is a condition
                   or the result is used in a test this will be better than using XOR. *)
                (makeBoolResultRev(CondNotEqual, ccRef, target,
                        BlockSimple(
                            AddSubImmediate{source=testDest, immed=taggedWord 0w1, isAdd=false,
                                dest=NONE, length=Arith32 (* Always either tagged 0 or tagged 1 *), ccRef=SOME ccRef}) ::
                            argCode), target, false)
            end

        |   codeToICodeUnaryRev(instr, context, isTail, destination, tailCode) =
            raise Fallback "codeToICodeUnaryRev"

        and codeToICodeBinaryRev({oper=BuiltIns.WordComparison{test, isSigned}, arg1, arg2},
                context, _, destination, tailCode) =
            let
                (* Comparisons. This is now only used for tagged values, not for pointer equality. *)
                val ccRef = newCCRef()
                val (testCode1, testDest1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (testCode2, testDest2, _) = codeToICodeRev(arg2, context, false, AnyReg, testCode1)
                val opSize = if is32in64 then Arith32 else Arith64
                val comparison =
                    BlockSimple(
                        AddSubRegister{operand1=testDest1, operand2=testDest2, dest=NONE, length=opSize, ccRef=SOME ccRef, isAdd=false}) ::
                            testCode2
                val target = asTarget destination
                open BuiltIns
                val cond =
                    case (test, isSigned) of
                        (TestEqual,         _) => CondEqual
                    |   (TestLess,          true) => CondSignedLess
                    |   (TestLessEqual,     true) => CondSignedLessEq
                    |   (TestGreater,       true) => CondSignedGreater
                    |   (TestGreaterEqual,  true) => CondSignedGreaterEq
                    |   (TestLess,          false) => CondCarryClear
                    |   (TestLessEqual,     false) => CondUnsignedLowOrEq
                    |   (TestGreater,       false) => CondUnsignedHigher
                    |   (TestGreaterEqual,  false) => CondCarrySet
                    |   (TestUnordered,     _) => raise InternalError "WordComparison: TestUnordered"
            in
                (makeBoolResultRev(cond, ccRef, target, comparison), target, false)
            end

        |   codeToICodeBinaryRev({oper=BuiltIns.PointerEq, arg1, arg2}, context, _, destination, tailCode) =
            let
                (* Equality of general values which can include pointers. This can be treated exactly as a word equality.
                   It has to be analysed differently for indexed cases. *)
                val ccRef = newCCRef()
                val (testCode1, testDest1, _) = codeToICodeRev(arg1, context, false, AnyReg, tailCode)
                val (testCode2, testDest2, _) = codeToICodeRev(arg2, context, false, AnyReg, testCode1)
                val opSize = if is32in64 then Arith32 else Arith64
                val comparison =
                    BlockSimple(
                        AddSubRegister{operand1=testDest1, operand2=testDest2, dest=NONE, length=opSize, ccRef=SOME ccRef, isAdd=false}) ::
                            testCode2
                val target = asTarget destination
            in
                (makeBoolResultRev(CondEqual, ccRef, target, comparison), target, false)
            end

        |   codeToICodeBinaryRev(instr, context, isTail, destination, tailCode) =
            raise Fallback "codeToICodeBinaryRev"

        (* Store the code address and the closure items into a previously allocated closure on the
           heap.  This is used both in the simple case and also with mutually recursive declarations. *)
        and storeIntoClosure(lambda as { closure, ...}, absClosureAddr, context, tailCode) =
        let
            val closureRef = makeConstantClosure()
            val () = codeFunctionToArm64(lambda, debugSwitches, closureRef)
            val codeAddrWords = if is32in64 then 2 else 1

            fun storeAValue(f, (n, tlCode)) =
            let
                val (code, source, _) = codeToICodeRev(BICExtract f, context, false, AnyReg, tlCode)
            in
                (n+1, storeAtWordOffset(source, n, absClosureAddr, polyWordLoadSize, code))
            end
            (* Store the code address in the first 64-bits. *)
            val storeCodeAddress =
                if is32in64
                then
                let
                    (* We can't use codeAddressFromClosure on 32-in-64 because it always returns
                       a 64-bit value.  Instead we have to get the code address at run-time. *)
                    val clReg = newPReg() and absClReg = newUReg() and absCodeReg = newUReg()
                in
                    storeAtWordOffset(absCodeReg, 0, absClosureAddr, Load64,
                        BlockSimple(LoadWithConstantOffset{base=absClReg, dest=absCodeReg, byteOffset=0, loadType=Load64}) ::
                        BlockSimple(ObjectIndexAddressToAbsolute{ source=clReg, dest=absClReg }) ::
                        BlockSimple(LoadAddressConstant{source=closureAsAddress closureRef, dest=clReg}) :: tailCode)
                end
                else
                let
                    val cReg = newPReg()
                in
                    storeAtWordOffset(cReg, 0, absClosureAddr, Load64,
                        BlockSimple(LoadAddressConstant{source=codeAddressFromClosure closureRef, dest=cReg}) :: tailCode)
                end
            val (_, storeCode) = List.foldl storeAValue (codeAddrWords, storeCodeAddress) closure
        in
            storeCode
        end

        (*Turn the codetree structure into icode. *)
        val bodyContext = {loopArgs=NONE, stackPtr=0, currHandler=NONE, overflowBlock=ref NONE}
        val (bodyCode, _, bodyExited) =
            codeToICodeRev(body, bodyContext, true, SpecificPReg resultTarget, beginInstructions)
        val icode = if bodyExited then bodyCode else returnInstruction(bodyContext, resultTarget, bodyCode)
        
        (* Turn the icode list into basic blocks.  The input list is in reverse so as part of
           this we reverse the list. *)
        local
            val resArray = Array.array(!labelCounter, BasicBlock{ block=[], flow=ExitCode })
            
            fun createEntry (blockNo, block, flow) =
                Array.update(resArray, blockNo, BasicBlock{ block=block, flow=flow})
            
            fun splitCode([], _, _) = 
                (* End of code.  We should have had a BeginFunction. *)
                raise InternalError "splitCode - no begin"
            
            |   splitCode(BlockBegin args :: _, sinceLabel, flow) =
                    (* Final instruction.  Create the initial block and exit. *)
                    createEntry(0, BeginFunction args ::sinceLabel, flow)
            
            |   splitCode(BlockSimple instr :: rest, sinceLabel, flow) =
                    splitCode(rest, instr :: sinceLabel, flow)

            |   splitCode(BlockLabel label :: rest, sinceLabel, flow) =
                    (* Label - finish this block and start another. *)
                (
                    createEntry(label, sinceLabel, flow);
                    (* Default to a jump to this label.  That is used if we have
                       assumed a drop-through. *)
                    splitCode(rest, [], Unconditional label)
                )
            
            |   splitCode(BlockExit instr :: rest, _, _) =
                    splitCode(rest, [instr], ExitCode)

            |   splitCode(BlockFlow flow :: rest, _, _) =
                    splitCode(rest, [], flow)
            
            |   splitCode(BlockRaiseAndHandle(instr, handler) :: rest, _, _) =
                    splitCode(rest, [instr], UnconditionalHandle handler)

            |   splitCode(BlockOptionalHandle{call, handler, label} :: rest, sinceLabel, flow) =
                let
                    (* A function call within a handler.  This could go to the handler but
                       if there is no exception will go to the next instruction.
                       Also includes JumpLoop since the stack check could result in an
                       Interrupt exception. *)
                in
                    createEntry(label, sinceLabel, flow);
                    splitCode(rest, [call], ConditionalHandle{handler=handler, continue=label})
                end

        in
            val () = splitCode(icode, [], ExitCode)
            val resultVector = Array.vector resArray
        end
      
        open ICodeTransform
        
        val pregProperties = Vector.fromList(List.rev(! pregPropList))
    in
        codeICodeFunctionToArm64{blocks = resultVector, functionName = name, pregProps = pregProperties,
            ccCount= ! ccRefCounter, debugSwitches = debugSwitches, resultClosure = resultClosure,
            profileObject = profileObject}
    end

    fun gencodeLambda(lambda, debugSwitches, closure) =
    let
        open Debug Universal
        val tryNewCG = Debug.getParameter Debug.compilerDebugTag debugSwitches
    in
        if tryNewCG = 0
        then Arm64Fallback.gencodeLambda(lambda, debugSwitches, closure)
        else
        let
            val newDebugSwitches =
                [tagInject Pretty.compilerOutputTag (Pretty.prettyPrint(print, 70)),
                tagInject assemblyCodeTag true] @ debugSwitches
        in
            codeFunctionToArm64(lambda, if tryNewCG > 1 then newDebugSwitches else debugSwitches, closure)
                handle Fallback reason =>
                    (
                        print "Fallback-"; print reason; print "\n";
                        Arm64Fallback.gencodeLambda(lambda, debugSwitches, closure)
                    )
        end
    end

    structure Foreign = Arm64Foreign
    
    structure Sharing =
    struct
        type backendIC = backendIC
        and  bicLoadForm = bicLoadForm
        and argumentType = argumentType
        and closureRef = closureRef
    end
    
end;
