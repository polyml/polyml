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
    type backendIC and argumentType and machineWord
    type bicLambdaForm =
    {
        body          : backendIC,
        name          : string,
        closure       : backendIC list,
        argTypes      : argumentType list,
        resultType    : argumentType,
        closureRefs   : int,
        localCount    : int,
        heapClosure   : bool,
        argLifetimes  : int list
    }
    val gencodeLambda: bicLambdaForm * Universal.universal list * Address.address -> unit

    structure Sharing: sig type backendIC = backendIC and argumentType = argumentType end
end =
struct
    open X86CODE
    open BACKENDTREE
    open Address
    exception InternalError = Misc.InternalError

    val ioOp : int -> machineWord = RunCall.run_call1 RuntimeCalls.POLY_SYS_io_operation

    val argRegs = if isX64 then [eax, ebx, r8, r9, r10] else [eax, ebx]
    val numArgRegs = List.length argRegs

    (* tag a short constant *)
    fun tag c = 2 * c + 1

    fun condBranch(test, predict) =
    let
        val label as Labels{uses, ...} = mkLabel()
    in
        uses := 1;
        ([ConditionalBranch{test=test, predict=predict, label=label}], label)
    end
    
    and uncondBranch() =
    let
        val label as Labels{uses, ...} = mkLabel()
    in
        uses := 1;
        ([UncondBranch label], label)
    end

    and backJumpLabel() =
    let
        val loopLabel = mkLabel()
    in
        ([JumpLabel loopLabel], loopLabel)
    end

    and forwardJumpLabel v = [JumpLabel v]

    and jumpBackwards (v as Labels{uses, ...}) = (uses := !uses+1; [UncondBranch v])

    and indexedCase { testReg, workReg, minCase, maxCase, isArbitrary, isExhaustive } =
    let
        val defaultLab as Labels{uses=defUses, ...} = mkLabel()
        fun makeLab _ =
        let
            val lab as Labels{uses, ...} = mkLabel()
        in
            uses := 1;
            lab
        end
        val indexLabels =
            List.tabulate(Word.toInt(maxCase-minCase+0w1), makeLab)
        val testCode =
            if isExhaustive
            then []
            else
            let
                (* If this is an arbitrary precision int we need to check it's short
                   and go to the default if it isn't. *)
                val testTag =
                    if isArbitrary
                    then
                    (
                        defUses := 3;
                        [TestTagR testReg,
                         ConditionalBranch{test=JE, predict=PredictNotTaken, label=defaultLab}]
                    )
                    else (defUses := 2; [])
                (* Range checks. *)
                val range1 =
                    [ArithRConst{ opc=CMP, output=testReg, source=tag(Word.toLargeInt minCase) },
                     ConditionalBranch{test=JB, predict=PredictNotTaken, label=defaultLab}]
                val range2 =
                    [ArithRConst{ opc=CMP, output=testReg, source=tag(Word.toLargeInt maxCase) },
                     ConditionalBranch{test=JA, predict=PredictNotTaken, label=defaultLab}]
            in
                testTag @ range1 @ range2
            end
        val code = testCode @ [IndexedCase{testReg=testReg, workReg=workReg, min=minCase, cases=indexLabels}]
    in
        (code, indexLabels, defaultLab)
    end
    fun loadMemory(reg, base, offset) =
        [LoadMemR{source=BaseOffset{base=base, offset=offset, index=NoIndex}, output=reg}]
    and loadStack(reg, offset) = loadMemory(reg, esp, offset)
    and storeMemory(reg, base, offset) =
        [StoreRegToMemory{toStore=reg, address=BaseOffset{base=base, offset=offset, index=NoIndex}}]

    fun allocStore{size, flags, output, preserve} =
    if isX64 andalso flags <> 0w0
    then
        [AllocStore{size=size, output=output, saveRegs=preserve},
         StoreConstToMemory{toStore=LargeInt.fromInt size, address=BaseOffset{offset= ~wordSize, base=output, index=NoIndex}},
         StoreByteConstToMemory{toStore=flags, address=BaseOffset{offset= ~1, base=output, index=NoIndex}} ]
    else
    let
        val lengthWord = IntInf.orb(IntInf.fromInt size, IntInf.<<(Word8.toLargeInt flags, 0w24))
    in
        [AllocStore{size=size, output=output, saveRegs=preserve},
         StoreConstToMemory{toStore=lengthWord, address=BaseOffset{offset= ~wordSize, base=output, index=NoIndex}}]
    end

    val allocationComplete = [StoreInitialised]

    fun createProfileObject _ (*functionName*) =
    let
        (* The profile object is a single mutable with the F_bytes bit set. *)
        open Address
        val profileObject = alloc(0w1, Word8.orb(F_mutable, F_bytes), toMachineWord 0w0);
    in
        toMachineWord profileObject
    end

    fun codeFunctionToX86({body, localCount, name, argTypes, closure, ...}:bicLambdaForm, debugSwitches, closureOpt) =
    let
        val profileObject = createProfileObject name
        val newCode = codeCreate (name, profileObject, debugSwitches)
        (* This gives the offset in the stack for each binding. *) 
        val stackOffsetArray = Array.array(localCount, NONE: int option)
        val stackCount = ref 0 (* Count of number of items *)
        val maxStack = ref 0

        fun push reg =
            (stackCount := ! stackCount + 1; maxStack := Int.max(!maxStack, !stackCount); [PushR reg])
        and pop reg =
            (stackCount := ! stackCount - 1; [PopR reg])
        and resetStack oldStack =
            if oldStack = ! stackCount then []
            else if oldStack > ! stackCount then raise InternalError "resetStack"
            else let val c = [ResetStack(! stackCount - oldStack)] in stackCount := oldStack; c end

        (* Push the closure register and any arguments on entry. *)
        val pushClosureReg = case closure of [] => [] | _ => push edx
        val closureRegAddr = !stackCount

        val numArgs = List.length argTypes
        val argRegsUsed =
            if numArgs >= List.length argRegs
            then argRegs
            else List.take(argRegs, numArgs)
        val pushArgs =
            List.foldr (fn (r, l) => push r @ l) [] argRegsUsed

        fun allocateHeap(size, flag, toReg) =
        let
            val addAllocatingFunction =
                DEBUG.getParameter DEBUG.profileAllocationTag debugSwitches = 1
        in
            (* If we need to add the profile object *)
            if addAllocatingFunction
            then
                allocStore {size=size+1, flags=Word8.orb(flag, Address.F_profile), output=toReg, preserve=[]} @
                    [StoreLongConstToMemory{ toStore=profileObject, address=BaseOffset{base=toReg, offset=size*wordSize, index=NoIndex}}]
            else allocStore {size=size, flags=flag, output=toReg, preserve=[]}
        end

        (* This is a naive code-generator that simply evaluates everything to eax. *)
        fun codeToX86(BICNewenv (bindings, exp), context) =
            let
                val startStack = !stackCount
                fun doBinding(BICDeclar{value, addr, ...}, l) =
                    let
                        val code = codeToX86(value, context)
                        val pushR = push eax
                        val () = Array.update(stackOffsetArray, addr, SOME(!stackCount))
                    in
                        l @ code @ pushR
                    end

                |   doBinding(BICRecDecs [{lambda, addr, ...}], l) =
                    (* We shouldn't have single entries in RecDecs but it seems to occur at the moment. *)
                    let
                        val code = codeToX86(BICLambda lambda, context)
                        val pushR = push eax
                        val () = Array.update(stackOffsetArray, addr, SOME(!stackCount))
                    in
                        l @ code @ pushR
                    end

                |   doBinding(BICRecDecs decs, l) =
                    let
                        (* First build the closures as mutable cells containing zeros.  Set the
                           entry in the address table to the address of the cell. *)
                        fun buildClosure({lambda={closure, ...}, addr, ...}, l) =
                        let
                            val sizeClosure = List.length closure + 1
                            val mem = allocStore{size=sizeClosure, flags=Address.F_mutable, output=eax, preserve=[]}
                            val pushR = push eax
                            val () = Array.update(stackOffsetArray, addr, SOME(!stackCount))
                            fun clearAll n =
                                if n = sizeClosure
                                then []
                                else StoreConstToMemory{toStore=tag 0, address=BaseOffset{offset=n*wordSize, base=eax, index=NoIndex}} ::
                                        clearAll (n+1)
                            val clear = clearAll 0
                        in
                            l @ mem @ pushR @ clear @ allocationComplete
                        end
                        val allocClosures = List.foldl buildClosure [] decs
                        (* Evaluate the closure entries, store them and lock the closure. *)
                        fun setClosure({lambda as {closure, ...}, addr, ...}, l) =
                        let
                            val codeAddr = codeFunctionToX86(lambda, debugSwitches, NONE)
                            (* Basically the same as tuple except we load the address of
                               the closure we've made.  *)
                            fun loadFields([], _) =
                                    loadStack(eax, (!stackCount - valOf(Array.sub(stackOffsetArray, addr)))*wordSize)
                            |   loadFields(f :: rest, n) =
                                let
                                    val code = codeToX86(f, context) @ push eax
                                    val restAndAlloc = loadFields(rest, n+1)
                                    val storeValue =
                                        pop ebx @ storeMemory(ebx, eax, n * wordSize)
                                in
                                    code @ restAndAlloc @ storeValue
                                end
                            val setFields = loadFields(BICConstnt(toMachineWord codeAddr, []) :: closure, 0)
                        in
                            l @ setFields @ [LockMutableSegment eax]
                        end
                        val setClosures = List.foldl setClosure [] decs
                    in
                        l @ allocClosures @ setClosures
                    end

                |   doBinding(BICNullBinding exp, l) =
                        l @ codeToX86(exp, context) (* And discard result. *)
                val code =
                    List.foldl doBinding [] bindings @ codeToX86(exp, context)
            in
                code @ resetStack startStack
            end

        |   codeToX86(BICConstnt(value, _), _) =
                if isShort value
                then [MoveConstR{source=tag(Word.toLargeIntX(toShort value)), output=eax}]
                else [MoveLongConstR{source=value, output=eax}]

        |   codeToX86(BICExtract(BICLoadLocal l, _(*lastRef*)), _) =
                loadStack(eax, (!stackCount - valOf(Array.sub(stackOffsetArray, l)))*wordSize)

        |   codeToX86(BICExtract(BICLoadArgument a, _(*lastRef*)), _) =
            if a < numArgRegs
            then (* It was originally in a register. *) loadStack(eax, (!stackCount - closureRegAddr - 1 - a)*wordSize)
            else (* Pushed before call. *) loadStack(eax, (!stackCount + List.length argTypes - a)*wordSize)

        |   codeToX86(BICExtract(BICLoadClosure c, _(*lastRef*)), _) =
            (* N.B.  We need to add one to the closure entry because zero is the code address. *)
            if c >= List.length closure then raise InternalError "BICExtract: closure"
            else loadStack(eax, (!stackCount - closureRegAddr)*wordSize) @
                    loadMemory(eax, eax, (c+1) * wordSize)

        |   codeToX86(BICExtract(BICLoadRecursive, _(*lastRef*)), _) =
            (
                (* If the closure is empty we must use the constant.  We can't guarantee that
                   the caller will actually load the closure register if it knows the closure
                   is empty. *)
                case closure of
                    [] => [MoveLongConstR{source=toMachineWord(valOf closureOpt), output=eax}]
                |   _ => loadStack(eax, (!stackCount - closureRegAddr)*wordSize)
            )

        |   codeToX86(BICField{base, offset}, context) =
                codeToX86(base, context) @ loadMemory(eax, eax, offset * wordSize)
    
        |   codeToX86(BICEval{function, argList, ...}, context) =
            let
                val startStack = !stackCount
                (* Codegenerate the closure and the arguments and push everything to
                   the stack. *)
                val codeArgs =
                    List.foldl(fn (arg, l) => l @ codeToX86(arg, context) @ push eax) []
                        (function :: List.map #1 argList)
                (* Load the earlier arguments into registers. *)
                (* Load until we've done all the arguments or all the registers whichever is earlier. *)
                fun loadArgRegs(_ :: args, argReg :: argRegs, argOffset) =
                    loadStack(argReg, argOffset) @ loadArgRegs(args, argRegs, argOffset-wordSize)
                |   loadArgRegs _ = []
                val numArgs = List.length argList
                val loadRegs = loadArgRegs(argList, argRegs, (numArgs-1)*wordSize)
                (* The later arguments are already on the stack. *)
                val loadClosure = loadStack(edx, numArgs*wordSize)
                val callFunction = [CallFunction FullCall]
                (* The caller will remove the stack arguments. *)
                val numStackArgs =
                    if numArgs > numArgRegs then numArgs - numArgRegs else 0
                val () = stackCount := ! stackCount - numStackArgs
                (* Remove the saved arguments. *)
            in
                codeArgs @ loadRegs @ loadClosure @ callFunction @ resetStack startStack
            end

        |   codeToX86(BICBuiltIn(rtsNo, args), context) =
                (* For the moment just call the RTS function. *)
                codeToX86(
                    BICEval{function=BICConstnt(ioOp rtsNo, []),
                            argList = map (fn c => (c, GeneralType)) args, resultType=GeneralType}, context)

        |   codeToX86(BICLambda(lambda as { closure = [], ...}), _) =
            (* Empty closure - create a constant closure for any recursive calls. *)
            let
                val closure = Address.alloc(0w1, Word8.orb (F_mutable, F_words), Address.toMachineWord 0w0)
                val codeAddr = codeFunctionToX86(lambda, debugSwitches, SOME closure)
                open Address
            in
                assignWord(closure, 0w0, toMachineWord codeAddr);
                lock closure;
                [MoveLongConstR{source=toMachineWord closure, output=eax}]
            end

        |   codeToX86(BICLambda(lambda as { closure, ...}), context) =
            (* Non-empty closure.  Ignore stack closure option at the moment. *)
            let
                val codeAddr = codeFunctionToX86(lambda, debugSwitches, NONE)
            in
                (* Treat it as a tuple with the code as the first field. *)
                codeToX86(BICTuple(BICConstnt(toMachineWord codeAddr, []) :: closure), context)
            end

        |   codeToX86(BICCond(test, thenPt, elsePt), context) =
            let
                val startStack = ! stackCount
                val testCode = codeToX86(test, context)
                val testIt = ArithRConst {opc=CMP, output=eax, source= tag 0 }
                val (condJump, startElse) = condBranch(JE, PredictNeutral)
                val (skipToEnd, endIf) = uncondBranch()
                val thenCode = codeToX86(thenPt, context)
                val _ = !stackCount = startStack orelse raise InternalError "Check stack- then"
                val elseCode = codeToX86(elsePt, context)
                val _ = !stackCount = startStack orelse raise InternalError "Check stack- else"
            in
                (* Evaluate the test.  See if it is zero and go to ELSE if it is.
                   If non-zero execute the then-part and branch to the end.  If it
                   is zero execute the else-part. *)
                testCode @ [testIt] @ condJump @ thenCode @ skipToEnd @
                    forwardJumpLabel startElse @ elseCode @ forwardJumpLabel endIf
            end

        |   codeToX86(BICCase{ cases, test, caseType, default}, context) =
            let (* Indexed case.  This is produced from if-then-else. *)
                
                (* Sort the cases into ascending order.  It's possible that we may have
                   duplicates if this came from an if-then-else construction so we
                   need to retain the ordering for items with the same case label. *)
                (* TODO: This should be done in the higher level. *)
                local
                    val labelCount = List.length cases
                    (* Add an extra field before sorting which retains the ordering for
                       equal labels. *)
                    val ordered = ListPair.zipEq (cases, List.tabulate(labelCount, fn n=>n))
                    fun leq ((_, w1: word), n1: int) ((_, w2), n2) =
                        if w1 = w2 then n1 <= n2 else w1 < w2
                    val sorted = List.map #1 (Misc.quickSort leq ordered)
                    (* Filter out any duplicates. *)
                    fun filter [] = []
                    |   filter [p] = [p]
                    |   filter ((p as (_, lab1)) :: (q as (_, lab2)) :: tl) =
                            if lab1 = lab2
                            then p :: filter tl
                            else p :: filter (q :: tl)
                in
                    val cases = filter sorted
                end

                val (isExhaustive, min, max) =
                    case caseType of
                        CaseTag max => (true, 0w0, max)
                    |   _ =>
                        let
                            val (_, aLabel) = hd cases
                            fun foldCases((_, w), (min, max)) = (Word.min(w, min), Word.max(w, max))
                            val (min, max) = List.foldl foldCases (aLabel, aLabel) cases
                        in
                            (false, min, max)
                        end
                (* The value to test. *)
                val testValue = codeToX86(test, context)

                (* Check the range and branch into the table. *)
                val (indexCaseInstr, caseLabels, defaultLabel) =
                    indexedCase{testReg=eax, workReg=ebx, minCase=min, maxCase=max, 
                            isArbitrary = case caseType of CaseInt => true | _ => false,
                            isExhaustive=isExhaustive}

                (* Put in the default case.  Even when the case is exhaustive one entry is
                   always treated as a default and not included in the list of cases. *)
                local
                    val startDefault = forwardJumpLabel defaultLabel
                    (* Go down the list of cases and fix up any default labels to come here.
                       Default entries are represented by "holes" in the case list. *)
                    fun genDefaults(indexVal, label :: labelList, cl as ((_, caseLabel) :: cps)) =
                        if indexVal = caseLabel
                        then genDefaults(indexVal+0w1, labelList, cps)
                        else forwardJumpLabel label @
                            genDefaults(indexVal+0w1, labelList, cl)

                    |   genDefaults(indexVal, label :: labelList, []) =
                            forwardJumpLabel label @
                            genDefaults(indexVal+0w1, labelList, [])

                    |   genDefaults(_, [], _) = []
                    
                    val otherDefaults = genDefaults(min, caseLabels, cases)
                    
                    val defaultCode = codeToX86(default, context)
                    
                    val (branchCode, lab) = uncondBranch()
                in
                    val defaultCode = startDefault @ otherDefaults @ defaultCode @ branchCode
                    val exitDefault = lab
                end

                (* Generate the cases. *)
                fun genCases(indexVal, label :: labelList, (caseExp, caseLabel) :: cps) =
                    if indexVal <> caseLabel
                    then (* We have a hole.  Skip this item. *)
                        genCases(indexVal+0w1, labelList, (caseExp, caseLabel) :: cps)
                    else (* The index value corresponds to a label. *)
                    let
                        val startCode = forwardJumpLabel label

                        (* Generate this case. *)
                        val expCode = codeToX86(caseExp, context)
                    in
                        if null cps
                        then (*  Finished. *) startCode @ expCode
                        else
                        let
                            val (branchCode, lab) = uncondBranch()
                            val otherCases = genCases(indexVal+0w1, labelList, cps)
                            val fixUp = forwardJumpLabel lab
                        in
                            startCode @ expCode @ branchCode @ otherCases @ fixUp
                        end
                    end
                | genCases _ = raise InternalError "genCase - null case list"

                val caseResult = genCases(min, caseLabels, cases)
            in
                testValue @ indexCaseInstr @ defaultCode @ caseResult @ forwardJumpLabel exitDefault
            end

        |   codeToX86(BICBeginLoop{loop, arguments}, context) =
            let (* Begin a loop.  This is produced from tail-recursive inline functions.  The loop variables
                   are initialised and the loop is entered.  There will be at least one Loop instruction
                   which computes new values for the loop variables and jumps back to the start. *)
                val startStack = !stackCount
                (* Evaluate the arguments and push them to the stack. *)
                fun pushArg(({addr, value, ...}, _), l) =
                let
                    val code = codeToX86(value, context)
                    val pushR = push eax
                    val () = Array.update(stackOffsetArray, addr, SOME(!stackCount))
                in
                    l @ code @ pushR
                end

                val pushLoop = List.foldl pushArg [] arguments
                val (startLoopCode, startLoop) = backJumpLabel()
                val loopBody = codeToX86(loop, {loopAddr=SOME {startStack=startStack, startLoop=startLoop}})
            in
                pushLoop @ startLoopCode @ loopBody @ resetStack startStack
            end

        |   codeToX86(BICLoop args, context as {loopAddr=SOME {startStack=startStack, startLoop=startLoop}, ...}) =
            let
                (* Evaluate the arguments and push them to the stack.  Then pop them off
                   and store them to the loop argument locations. *)
                val nArgs = List.length args
                val loopStart = ! stackCount
                fun evalArgs [] = []
                |   evalArgs((arg, _) :: argRest) =
                    let
                        val codeArg = codeToX86(arg, context) @ push eax
                        val codeRest = evalArgs argRest
                        val popArg = pop eax
                        (* The offset here does not depend on the number of args or the count
                           because as we pop each argument we move sp so that the offset for each
                           argument is the same *)
                        val storeArg = storeMemory(eax, esp, (loopStart - startStack - 1)*wordSize)
                    in
                        codeArg @ codeRest @ popArg @ storeArg
                    end
                val codeArgs = evalArgs args
                (* We may be inside a newEnv block with items pushed.  We have to remove
                   them before we jump back.  We don't use resetStack because that changes
                   stackCount and messes up our checks. *)
                val reset = [ResetStack(!stackCount - (startStack+nArgs))]
            in
                (* Finally jump back to the start of the loop. *)
                codeArgs @ reset @ jumpBackwards startLoop
            end

        |   codeToX86(BICLoop _, {loopAddr=NONE, ...}) =
                raise InternalError "BICLoop without BICBeginLoop"

        |   codeToX86(BICKillItems{expression, ...}, context) =
                codeToX86(expression, context)

        |   codeToX86(BICRaise exc, context) =
                codeToX86(exc, context) @ [RaiseException]

        |   codeToX86(BICLdexc, _) = [] (* Do nothing - the packet is in eax. *)

        |   codeToX86(BICHandle{exp, handler}, context) =
            let
                fun storeToHandler reg =
                    [StoreRegToMemory{
                        toStore=reg, address=BaseOffset{offset=memRegHandlerRegister, base=ebp, index=NoIndex}}]
                val startStack = ! stackCount
                val pushCurrentHandler = [PushMem{base=ebp, offset=memRegHandlerRegister}]
                val () = stackCount := ! stackCount + 1
                val labelRef = ref addrZero
                (* Load the address of the handler into a register. *)
                val loadCode = [LoadHandlerAddress{ handlerLab=labelRef, output=eax}]
                (* Push the address of the start of the handler. *)
                val pushR = push eax
                (* Set the handler to the current stack pointer. *)
                val setCurrentHandler = storeToHandler esp
                (* Process the expression to be "handled". *)
                val codeExp = codeToX86(exp, context)
                (* We have our result in eax so we mustn't use it here. *)
                (* Pop the handler and restore the old handler. *)
                val restoreHandle =
                    resetStack(startStack+1) @ pop ebx @ storeToHandler ebx
                (* Jump round the handler code. *)
                val (skipHandler, toExit) = uncondBranch()

                (* Start of handler.  Set the label, reload esp from the handler register, remove the handler
                   address and restore old handler. *)
                val startHandler =
                    [StartHandler{handlerLab=labelRef},
                     LoadMemR{ source=BaseOffset{base=ebp, offset=memRegHandlerRegister, index=NoIndex}, output=esp },
                     LoadMemR{ source=BaseOffset{base=esp, offset=wordSize, index=NoIndex}, output=ebx }, ResetStack 2] @
                    storeToHandler ebx
                val codeHandler = codeToX86(handler, context)
            in
                pushCurrentHandler @ loadCode @ pushR @ setCurrentHandler @ codeExp @ restoreHandle @
                skipHandler @ startHandler @ codeHandler @ forwardJumpLabel toExit
            end

        |   codeToX86(BICTuple fields, context) =
            let
                (* Generate the fields and push to the stack. *)
                fun loadFields([], n) =
                        allocateHeap(n, 0w0, eax)
                |   loadFields(f :: rest, n) =
                    let
                        val code = codeToX86(f, context) @ push eax
                        val restAndAlloc = loadFields(rest, n+1)
                        val storeValue =
                            pop ebx @ storeMemory(ebx, eax, n * wordSize)
                    in
                        code @ restAndAlloc @ storeValue
                    end
            in
                loadFields(fields, 0) @ allocationComplete
            end

        |   codeToX86(BICContainer size, _) =
            (* Reserve a section of memory on the stack.  It must be zeroed. *)
            let
                fun pushZero 0 = []
                |   pushZero n = PushConst(tag 0) :: pushZero(n-1)
                val pushAll = pushZero size
                val () = stackCount := !stackCount + size
            in
                pushAll @ [MoveRR{source=esp, output=eax}]
            end

        |   codeToX86(BICSetContainer{container, tuple, filter}, context) =
            let
                val containerAddr = codeToX86(container, context)
                val pushC = push eax
                val tupleAddr = codeToX86(tuple, context)
                val loadContainer = pop ebx
                (* The source (tuple) address is in eax and the destination (container) address in ebx.
                   Copy the required fields using ecx as a work register. *)
                fun copyContainer(sourceWord, destWord) =
                    if sourceWord = BoolVector.length filter
                    then []
                    else if BoolVector.sub(filter, sourceWord)
                    then
                    let
                        val loadSource = loadMemory(ecx, eax, sourceWord*wordSize)
                        val storeDest = storeMemory(ecx, ebx, destWord*wordSize)
                    in
                        loadSource @ storeDest @ copyContainer(sourceWord+1, destWord+1)
                    end
                    else copyContainer(sourceWord+1, destWord)
            in
                containerAddr @ pushC @ tupleAddr @ loadContainer @ copyContainer(0, 0)
            end

        |   codeToX86(BICTagTest{test, tag, ...}, context) =
            let
                (* Convert this into a simple equality function. *)
                val code =
                    BICEval {
                        function = BICConstnt(ioOp RuntimeCalls.POLY_SYS_word_eq, []),
                        argList=[(test, GeneralType), (BICConstnt(Address.toMachineWord tag, []), GeneralType)],
                        resultType=GeneralType }
            in
                codeToX86 (code, context)
            end

        val ops =
            pushClosureReg @ pushArgs @ codeToX86(body, {loopAddr=NONE}) @
                [ResetStack(!stackCount), ReturnFromFunction(Int.max(0, numArgs-numArgRegs))]

        (* The stack limit register is set at least twice this far from the
           end of the stack so we can simply compare the stack pointer with
           the stack limit register if we need less than this much. Setting
           it at twice this value means that procedures which use up to this
           much stack and do not call any other procedures do not need to
           check the stack at all. *)
        val minStackCheck = 20
        val stackRequired = ! maxStack
        (* Adds the constants onto the code, and copies the code into a new segment *)

            (* Prelude consists of stack checking code. *)
        fun testRegAndTrap(reg, entryPt) =
        let
            (* If we need to take the trap we save the argument and closure registers
               across the trap. *)
            val saveRegs = argRegsUsed @ (case closure of [] => [] | _ => [edx])
            fun pushThenPop [] = [CallRTS entryPt]
            |   pushThenPop (aReg::regs) = PushR aReg :: (pushThenPop regs @ [PopR aReg])
            (* Normally we won't have a stack overflow so we will skip the check. *)
            val (skipCheck, skipCheckLab) = condBranch(JNB, PredictTaken)
        in
            [ArithRMem{ opc=CMP, output=reg, offset=memRegStackLimit, base=ebp }] @
            skipCheck @ pushThenPop saveRegs @ forwardJumpLabel skipCheckLab
        end
        val preludeCode =
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
     
            else testRegAndTrap (esp, memRegStackOverflowCall)
    in
        createCodeSegment(X86OPTIMISE.optimise(newCode, preludeCode @ ops), newCode)
    end

    fun gencodeLambda(lambda, debugSwitches, closure) =
    let
        val codeAddr = codeFunctionToX86(lambda, debugSwitches, SOME closure)
        open Address
    in
        assignWord(closure, 0w0, toMachineWord codeAddr);
        lock closure
    end
    
end;
