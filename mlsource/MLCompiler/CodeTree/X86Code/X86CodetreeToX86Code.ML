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
    
    structure X86FOREIGN: FOREIGNCALLSIG

    sharing X86CODE.Sharing = X86OPTIMISE.Sharing
): GENCODESIG =
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
        (* Reverse the list.  TODO: Build it in reverse order to save having to do this. *)
        (List.rev code, indexLabels, defaultLab)
    end

    fun loadMemory(reg, base, offset) =
        [LoadMemR{source=BaseOffset{base=base, offset=offset, index=NoIndex}, output=reg}]
    and loadStack(reg, offset) = loadMemory(reg, esp, offset)
    and storeMemory(reg, base, offset) =
        [StoreRegToMemory{toStore=reg, address=BaseOffset{base=base, offset=offset, index=NoIndex}}]

    (* Allocate memory.  N.B. Instructions are in reverse order. *)
    fun allocStore{size, flags, output, preserve} =
    if isX64 andalso flags <> 0w0
    then
        [StoreByteConstToMemory{toStore=flags, address=BaseOffset{offset= ~1, base=output, index=NoIndex}},
         StoreConstToMemory{toStore=LargeInt.fromInt size, address=BaseOffset{offset= ~wordSize, base=output, index=NoIndex}},
         AllocStore{size=size, output=output, saveRegs=preserve}]
    else
    let
        val lengthWord = IntInf.orb(IntInf.fromInt size, IntInf.<<(Word8.toLargeInt flags, 0w24))
    in
        [StoreConstToMemory{toStore=lengthWord, address=BaseOffset{offset= ~wordSize, base=output, index=NoIndex}},
         AllocStore{size=size, output=output, saveRegs=preserve}]
    end

    fun createProfileObject _ (*functionName*) =
    let
        (* The profile object is a single mutable with the F_bytes bit set. *)
        open Address
        val profileObject = alloc(0w1, Word8.orb(F_mutable, F_bytes), toMachineWord 0w0);
    in
        toMachineWord profileObject
    end

    datatype argument =
        PReg of int (* A pseudo-register - an abstract register. *)
    |   GeneralReg of genReg (* A real general register. *)
    |   AddressConstant of machineWord (* A constant that is an address. *)
    |   IntegerConstant of LargeInt.int (* A non-address constant.  Will usually be shifted and tagged. *)
    |   MemoryLocation of { base: argument, offset: int } (* A memory location.  Could be the stack. *)

    datatype x86ICode =
        (* Load, store and move, depending on the kinds of arguments.  Only certain combinations
           are supported. *)
        MoveArgument of { source: argument, dest: argument}

    |   Conditional of { test: argument, thenPt: x86ICode list, elsePt: x86ICode list, resultReg: argument}

    |   MoveStackArgToPreg of { offset: int, dest: argument} (* Load an argument or the return address *)
    
        (* Call or jump to a function.  Currently the closure is loaded into edx/rdx as one of the
           regArgs. The result is stored in the destination register.  If isTail is true this is tail-recursive
           and should be a jump. *)
    |   FunctionCall of { regArgs: (argument * genReg) list, stackArgs: argument list, dest: argument, isTail: bool}

        (* Allocate a tuple or a function closure. *)
    |   AllocateMemoryToPreg of { size: int, isMutable: bool, dest: argument}

        (* Signal that a tuple has been fully initialised.  Really a check in the
           low-level code-generator. *)
    |   InitialisationComplete of { dest: argument}

        (* Begin a loop.  A set of loop registers are initialised and the loop is entered.  *)
    |   StartLoop of { arguments: {source: argument, loopReg: argument} list, loopCode: x86ICode list }

        (* Within a loop the loop registers are updated from the source registers and
           a jump is made back to the containing StartLoop *)
    |   JumpLoop of { arguments: {source: argument, loopReg: argument} list, loopDest: argument }
    
    |   RaiseExceptionPacket of { packet: argument, dummyResult: argument }

        (* Reserve a contiguous area on the stack to receive a result tuple. *)
    |   ReserveContainer of { size: int, address: argument }

        (* Set up a handler and execute some code with the handler in place. *)
    |   HandleException of
            { tohandle: x86ICode list, handler: x86ICode list, saveHandle: argument, resultReg: argument }

        (* Indexed case.  This could possibly be simplified. *)
    |   IndexedCaseOperation of { testReg: argument, workReg: argument, default: x86ICode list,
                         cases: (x86ICode list * word) list, exhaustive: bool, min: word, max: word,
                         arbitrary: bool, resultReg: argument}

        (* Lock a mutable cell by turning off the mutable bit. *)
    |   LockMutable of { addr: argument }


    fun argToPReg(PReg n) = n | argToPReg _ = raise InternalError "argToPReg"


    fun codeFunctionToX86({body, localCount, name, argTypes, closure, ...}:bicLambdaForm, debugSwitches, closureOpt) =
    let
        val profileObject = createProfileObject name
        val pregCounter = ref 0
        fun newPReg() = (PReg(!pregCounter)) before pregCounter := !pregCounter + 1
        val locToPregArray = Array.array(localCount, NONE: argument option)

        val numFunctionArgs = List.length argTypes
        val argRegsUsed =
            if numFunctionArgs >= List.length argRegs
            then argRegs
            else List.take(argRegs, numFunctionArgs)

        (* Pseudo-regs for the result, the closure and the args that were passed in real regs. *)
        val resultTarget = newPReg()
        val closureRegAddr = newPReg()
        val argPRegs = map (fn _ => newPReg()) argRegsUsed

        val pushClosureReg =
            case closure of [] => [] | _ => [MoveArgument{source=GeneralReg edx, dest=closureRegAddr}]
        val pushArgs =
            ListPair.foldrEq (fn (r, m, l) => MoveArgument{source=GeneralReg r, dest=m} :: l) [] (argRegsUsed, argPRegs)
        (* Number of arguments on the stack and not in registers. *)
        val currentStackArgs = Int.max(0, numFunctionArgs - List.length argRegs)

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

        (* First pass - turn the ML code into ICode.  All targets are pseudo-registers. *)
        local
            fun codeToICode(BICNewenv (bindings, exp), context, isTail, target) =
                let
                    fun doBinding(BICDeclar{value, addr, ...}, l) =
                        let
                            val dest = newPReg()
                            val code = codeToICode(value, context, false, dest)
                            val () = Array.update(locToPregArray, addr, SOME dest)
                        in
                            l @ code
                        end

                    |   doBinding(BICRecDecs [{lambda, addr, ...}], l) =
                        (* We shouldn't have single entries in RecDecs but it seems to occur at the moment. *)
                        let
                            val dest = newPReg()
                            val code = codeToICode(BICLambda lambda, context, false, dest)
                            val () = Array.update(locToPregArray, addr, SOME dest)
                        in
                            l @ code
                        end

                    |   doBinding(BICRecDecs decs, l) =
                        let
                            val destRegs = map (fn _ => newPReg()) decs

                            (* First build the closures as mutable cells containing zeros.  Set the
                               entry in the address table to the register containing the address. *)
                            fun makeClosure({lambda={closure, ...}, addr, ...}, dest, c) =
                            let
                                val () = Array.update(locToPregArray, addr, SOME dest)
                                val sizeClosure = List.length closure + 1
                                fun clear n =
                                    if n = sizeClosure
                                    then [AllocateMemoryToPreg{size=sizeClosure, isMutable=true, dest=dest}]
                                    else
                                    let
                                        val d = newPReg()
                                    in
                                        MoveArgument{source=IntegerConstant(tag 0), dest=d} ::
                                            (clear (n+1) @
                                                [MoveArgument{source=d, dest=MemoryLocation{offset=n*wordSize, base=dest}}])
                                    end
                            in
                                c @ clear 0 @ [InitialisationComplete{dest=dest}]
                            end
                            
                            val allocClosures = ListPair.foldlEq makeClosure [] (decs, destRegs)
                            
                            fun setClosure({lambda as {closure, ...}, ...}, dest, l) =
                            let
                                val codeAddr = codeFunctionToX86(lambda, debugSwitches, NONE)
                                (* Basically the same as tuple except we load the address of
                                   the closure we've made.  It's complicated because
                                   MoveArgument to MemoryLocation assumes that the top of the stack is
                                   the address of the allocated memory and the items below
                                   are the values to store. *)
                                val dstCopy = newPReg()
                                fun loadFields([], _) = [MoveArgument{source=dest, dest=dstCopy}]
                                |   loadFields(f :: rest, n) =
                                    let
                                        val fReg = newPReg()
                                        val code = codeToICode(f, context, false, fReg)
                                        val restAndAlloc = loadFields(rest, n+1)
                                        val storeValue = [MoveArgument{ source=fReg, dest=MemoryLocation{offset=n*wordSize, base=dstCopy} }]
                                    in
                                        code @ restAndAlloc @ storeValue
                                    end
                                val setFields = loadFields(BICConstnt(toMachineWord codeAddr, []) :: closure, 0)
                            in
                                l @ setFields @ [LockMutable{addr=dest}]
                            end
                            val setClosures = ListPair.foldlEq setClosure [] (decs, destRegs)
                        in
                            l @ allocClosures @ setClosures
                        end

                    |   doBinding(BICNullBinding exp, l) =
                        let
                            val dest = newPReg()
                        in
                            l @ codeToICode(exp, context, false, dest) (* And discard result. *)
                        end
                    val code =
                        List.foldl doBinding [] bindings @ codeToICode(exp, context, isTail, target)
                in
                    code
                end
                
            |   codeToICode(BICConstnt(value, _), _, _, target) =
                let
                    val arg =
                        if isShort value
                        then IntegerConstant(tag(Word.toLargeIntX(toShort value)))
                        else AddressConstant value
                in
                    [MoveArgument{source=arg, dest=target}]
                end

            |   codeToICode(BICExtract(BICLoadLocal l, _(*lastRef*)), _, _, target) =
                    [MoveArgument{source=valOf(Array.sub(locToPregArray, l)), dest=target}]

            |   codeToICode(BICExtract(BICLoadArgument a, _(*lastRef*)), _, _, target) =
                    if a < numArgRegs
                    then (* It was originally in a register.  It's now in a preg. *)
                        [MoveArgument{source=List.nth(argPRegs, a), dest=target}]
                    else (* Pushed before call. *)
                        [MoveStackArgToPreg{offset=List.length argTypes - a, dest=target}]
            
            |   codeToICode(BICExtract(BICLoadClosure c, _(*lastRef*)), _, _, target) =
                let
                    val () = if c >= List.length closure then raise InternalError "BICExtract: closure" else ()
                    val baseReg = newPReg()
                    (* N.B.  We need to add one to the closure entry because zero is the code address. *)
                in
                    (* We could just use the closureRegAddr as the base but the code-generation
                       for MoveArgument with MemoryLocation as the source assumes that the base is on the top of the stack. *)
                    [
                        MoveArgument {source=closureRegAddr, dest=baseReg},
                        MoveArgument {source=MemoryLocation{offset=(c+1)*wordSize, base=baseReg}, dest=target}
                    ]
                end

            |   codeToICode(BICExtract(BICLoadRecursive, _(*lastRef*)), _, _, target) =
                (
                    (* If the closure is empty we must use the constant.  We can't guarantee that
                       the caller will actually load the closure register if it knows the closure
                       is empty. *)
                    case closure of
                        [] => [MoveArgument{source=AddressConstant(toMachineWord(valOf closureOpt)), dest=target}]
                    |   _ => [MoveArgument {source=closureRegAddr, dest=target}]
                )
            
            |   codeToICode(BICField{base, offset}, context, _, target) =
                let
                    val baseR = newPReg()
                in
                    codeToICode(base, context, false, baseR) @
                        [MoveArgument{source=MemoryLocation{offset=offset*wordSize, base=baseR}, dest=target}]
                end
            
            |   codeToICode(BICEval {function, argList, ...}, context, isTail, target) =
                let
                    val numArgs = List.length argList
                    (* Create pregs for the closure and each argument. *)
                    val clPReg = newPReg()
                    val argPRegs = List.map (fn _ => newPReg()) argList
                    val codeArgs =
                        ListPair.foldlEq(fn (arg, pr, l) => l @ codeToICode(arg, context, false, pr)) []
                            (function :: List.map #1 argList, clPReg :: argPRegs)
                    (* Load the closure and the argument registers.  This returns the
                       shorter of the actual arguments and those in registers. *)
                    val regArgs = ListPair.zip (clPReg::argPRegs, edx :: argRegs)
                    (* The stack arguments are the later arguments in the list i.e. those
                       most recently pushed to the stack. *)
                    val stackArgs =
                        if numArgs > List.length argRegs
                        then List.drop(argPRegs, List.length argRegs)
                        else []
                in
                    codeArgs @ [FunctionCall{regArgs=regArgs, stackArgs=stackArgs, dest=target, isTail=isTail}]
                end

            |   codeToICode(BICBuiltIn(rtsNo, args), context, isTail, target) =
                    (* For the moment just call the RTS function. *)
                    codeToICode(
                        BICEval{function=BICConstnt(ioOp rtsNo, []),
                                argList = map (fn c => (c, GeneralType)) args, resultType=GeneralType}, context, isTail, target)

            |   codeToICode(BICLambda(lambda as { closure = [], ...}), _, _, target) =
                (* Empty closure - create a constant closure for any recursive calls. *)
                let
                    val closure = Address.alloc(0w1, Word8.orb (F_mutable, F_words), Address.toMachineWord 0w0)
                    val codeAddr = codeFunctionToX86(lambda, debugSwitches, SOME closure)
                    open Address
                in
                    assignWord(closure, 0w0, toMachineWord codeAddr);
                    lock closure;
                    [MoveArgument{source=AddressConstant(toMachineWord closure), dest=target}]
                end

            |   codeToICode(BICLambda(lambda as { closure, ...}), context, isTail, target) =
                (* Non-empty closure.  Ignore stack closure option at the moment. *)
                let
                    val codeAddr = codeFunctionToX86(lambda, debugSwitches, NONE)
                in
                    (* Treat it as a tuple with the code as the first field. *)
                    codeToICode(BICTuple(BICConstnt(toMachineWord codeAddr, []) :: closure), context, isTail, target)
                end

            
            |   codeToICode(BICCond(test, thenPt, elsePt), context, isTail, target) =
                let
                    val testReg = newPReg()
                    (* Put the result in the target register. *)
                    val thenCode = codeToICode(thenPt, context, isTail, target)
                    val elseCode = codeToICode(elsePt, context, isTail, target)
                in
                    codeToICode(test, context, false, testReg) @
                        [Conditional{test=testReg, thenPt=thenCode, elsePt=elseCode, resultReg=target}]
                end
            
            |   codeToICode(BICCase { cases, test, caseType, default}, context, isTail, target) =
                let
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
                    val testReg = newPReg() and workReg = newPReg()
                    val testCode = codeToICode(test, context, false, testReg)
                    (* Put in the default case.  Even when the case is exhaustive one entry is
                       always treated as a default and not included in the list of cases. *)
                    val defaultCode = codeToICode(default, context, isTail, target)
                    val codeCases =
                        map (fn (c, w) => (codeToICode(c, context, isTail, target), w)) cases
                in
                    testCode @ [IndexedCaseOperation{testReg=testReg, workReg=workReg, default=defaultCode,
                                            cases=codeCases, exhaustive=isExhaustive, min=min, max=max,
                                            arbitrary=caseType = CaseInt, resultReg=target}]
                end
            
            |   codeToICode(BICBeginLoop {loop, arguments}, context, isTail, target) =
                let
                    (* First evaluate the initial values for the arguments to new pregs.
                       These are only used for the initial values. *)
                    val argPRegs = map(fn _ => newPReg()) arguments
                    val codeArgs =
                        ListPair.foldlEq(fn (({value, ...}, _), pr, l) => l @ codeToICode(value, context, false, pr)) []
                            (arguments, argPRegs)
                    (* Create loop arguments.  These are associated with the loop variables. *)
                    fun makeLoopReg ({addr, ...}, _) =
                        let val pr = newPReg() val () = Array.update(locToPregArray, addr, SOME pr) in pr end
                    val loopRegs = map makeLoopReg arguments
                    val loopBody = codeToICode(loop, {loopArgs=SOME(loopRegs, target)}, isTail, target)
                    val args = ListPair.mapEq(fn (s, l) => {source=s, loopReg=l}) (argPRegs, loopRegs)
                in
                    codeArgs @ [StartLoop{arguments=args, loopCode=loopBody}]
                end
            
            |   codeToICode(BICLoop args, context as {loopArgs=SOME(loopRegs, loopDest), ...}, _, _) =
                let
                    (* Registers to receive the evaluated arguments.  We can't put the
                       values into the loop variables yet because the values could depend
                       on the current values of the loop variables. *)
                    val argPRegs = map(fn _ => newPReg()) args
                    val codeArgs =
                        ListPair.foldlEq(fn ((arg, _), pr, l) => l @ codeToICode(arg, context, false, pr)) []
                            (args, argPRegs)
                    val jumpArgs = ListPair.mapEq(fn (s, l) => {source=s, loopReg=l}) (argPRegs, loopRegs)
                in
                    codeArgs @ [JumpLoop{arguments=jumpArgs, loopDest=loopDest}]
                end

            |   codeToICode(BICLoop _, {loopArgs=NONE, ...}, _, _) =
                    raise InternalError "BICLoop without BICBeginLoop"

            |   codeToICode(BICKillItems{expression, ...}, context, isTail, target) =
                    codeToICode(expression, context, isTail, target)

            |   codeToICode(BICRaise exc, context, _, target) =
                let
                    val excReg = newPReg()
                in
                    codeToICode(exc, context, false, excReg) @ [RaiseExceptionPacket{packet=excReg, dummyResult=target}]
                end
            
            |   codeToICode(BICLdexc, _, _, target) = (* The argument is in eax. *)
                    [MoveArgument{source=GeneralReg eax, dest=target}]
            
            |   codeToICode(BICHandle {exp, handler}, context, isTail, target) =
                let
                    (* The expression cannot be treated as a tail because the handler has
                       to be removed after *)
                    val expCode = codeToICode(exp, context, false (* Not tail *), target)
                    val handleCode = codeToICode(handler, context, isTail, target)
                in
                    [HandleException { tohandle = expCode, handler=handleCode, saveHandle=newPReg(), resultReg=target }]
                end
            
            |   codeToICode(BICTuple fields, context, _, target) =
                let
                    fun loadFields([], n) =
                            [AllocateMemoryToPreg{size=n, isMutable=false, dest=target}]
                    |   loadFields(f :: rest, n) =
                        let
                            val fReg = newPReg()
                            val code = codeToICode(f, context, false, fReg)
                            val restAndAlloc = loadFields(rest, n+1)
                            val storeValue = [MoveArgument{ source=fReg, dest=MemoryLocation{offset=n*wordSize, base=target} }]
                        in
                            code @ restAndAlloc @ storeValue
                        end
                in
                    loadFields(fields, 0) @ [InitialisationComplete{dest=target}]
                end
            
            |   codeToICode(BICContainer size , _, _, target) = [ReserveContainer{size=size, address=target}]

                (* Copy the source tuple into the container.
                   TODO: If the tuple is BICTuple we should copy the individual fields
                   and never create the heap tuple. *)
            |   codeToICode(BICSetContainer{container, tuple, filter}, context, _, target) =
                let
                    val containerTarget = newPReg() and tupleTarget = newPReg()
                    val codeContainer = codeToICode(container, context, false, containerTarget)
                    and codeTuple = codeToICode(tuple, context, false, tupleTarget)
                    
                    val filterLength = BoolVector.length filter

                    fun copyContainer(sourceWord, destWord) =
                        if sourceWord = filterLength
                        then []
                        else if BoolVector.sub(filter, sourceWord)
                        then
                        let
                            (* This is a little more complicated than we really need because the code for
                               MoveArgument from MemoryLocation and to MemoryLocation assume the arguments are in
                               specific locations on the stack. *)
                            val srcCopy = newPReg() and loadReg = newPReg() and destCopy = newPReg()
                            val srcCode = MoveArgument{source=tupleTarget, dest=srcCopy}
                            val load = MoveArgument{source=MemoryLocation{offset=sourceWord*wordSize, base=srcCopy}, dest=loadReg}
                            val destCode = MoveArgument{source=containerTarget, dest=destCopy}
                            val storeCode = MoveArgument{source=loadReg, dest=MemoryLocation{offset=destWord*wordSize, base=destCopy}}
                        in
                            srcCode :: load :: destCode :: storeCode :: copyContainer(sourceWord+1, destWord+1)
                        end
                        else copyContainer(sourceWord+1, destWord)
                in
                    (* Add a dummy result in here to keep everything neat. *)
                    codeContainer @ codeTuple @ copyContainer(0, 0) @ [MoveArgument{source=IntegerConstant(tag 0), dest=target}]
                end

            |   codeToICode(BICTagTest{test, tag, ...}, context, isTail, target) =
                let
                    (* Convert this into a simple equality function. *)
                    val code =
                        BICEval {
                            function = BICConstnt(ioOp RuntimeCalls.POLY_SYS_word_eq, []),
                            argList=[(test, GeneralType), (BICConstnt(Address.toMachineWord tag, []), GeneralType)],
                            resultType=GeneralType }
                in
                    codeToICode (code, context, isTail, target)
                end

        in
            val icode = pushClosureReg @ pushArgs @ codeToICode(body, {loopArgs=NONE}, true, resultTarget)
        end
        
        (* Second pass.  Currently this generates the code. *)
        local
            val maxStack = ref 0
            
            datatype pregLoc = InRax | OnStack of int
            
            type state = {preg: int, loc: pregLoc} list
            
            (* Return the location on the real stack immediately above the top item. *)
            fun stackPtr [] = 0
            |   stackPtr ({loc=OnStack sp, ...} :: _) = sp+1
            |   stackPtr (_ :: stack) = stackPtr stack
       
            fun findEntry (_, []: state) = raise InternalError "findEntry: not found"
            |   findEntry (reg, {preg, loc} :: rest) = if reg = preg then loc else findEntry(reg, rest)
        
            fun loadSource(source, dest, state) =
                case findEntry (source, state) of
                    InRax => if dest = eax then [] else [MoveRR { source=eax, output=dest }]
                |   OnStack s => loadStack(dest, (stackPtr state - s - 1) * wordSize)

            (* If the top of the stack is in RAX push it otherwise do nothing. *)
            fun pushRax ({preg, loc=InRax} :: stack : state) =
                let
                    val sp = stackPtr stack
                    val () = if sp >= !maxStack then maxStack := sp+1 else ()
                in
                    (* Return the updated state with the value pushed. *)
                    ({preg=preg, loc=OnStack sp} :: stack, [PushR eax])
                end
            |   pushRax stack = (stack, [])
            
            
            (* Reset the stack to value in the original state except that
               the target register should be moved into the result state. *)
            fun resetStack(state, oldState, target) =
            let
                (* Check that the target is on the top of the stack and in rax. *)
                val () =
                    case state of
                        {preg, loc=InRax } :: _ =>
                            if preg = target then () else raise InternalError "resetStack"
                    |   _ => raise InternalError "resetStack"
                (* Check that the top of the old state is not in a register. *)
                val () =
                    case oldState of
                        {loc=InRax, ...} :: _ => raise InternalError "resetStack"
                    |   _ => ()
                val currentSp = stackPtr state
                and oldSp = stackPtr oldState
                val newState = {preg=target, loc=InRax} :: oldState
                val code =
                    if currentSp = oldSp then [] else [ResetStack(currentSp - oldSp)]
            in
                (newState, code)
            end
            
            (* Process the code until the end.  The code list is built up in reverse order. *)
            (* If we reach the end we haven't exited or reached a JumpLoop so we just return the
               final state. *)
            fun codeICode{icode=[], state: state, code, ...} = (state, code)

                (* MoveArgument - this is used for loads, stores and moves.  *)
                (* Create a new Preg containing the contents of another Preg.
                   We only ever want the top item to be in RAX so we push the top of the stack. *)
            |   codeICode{icode=MoveArgument{source=PReg srcReg, dest=PReg destReg} :: rest, state, code, context} =
                let
                    val (pushedState, pushCode) = pushRax state
                    val load = loadSource(srcReg, eax, pushedState)
                in
                    codeICode{
                        icode=rest, state={preg=destReg, loc=InRax} :: pushedState,
                        code=load @ pushCode @ code, context=context}
                end

            |   codeICode{icode=MoveArgument{source=GeneralReg srcReg, dest=PReg destReg} :: rest, state: state, code, context} =
                (* Move a specific register to the stack.  If it is anything but RAX we push it immediately. *)
                let
                    val (pushedState, pushCode) = pushRax state
                in
                    if srcReg = eax
                    then codeICode{
                            icode=rest, state={preg=destReg, loc=InRax} :: pushedState,
                            code=pushCode @ code, context=context}
                    else
                    let
                        val sp = stackPtr pushedState
                    in
                        codeICode{
                            icode=rest, state={preg=destReg, loc=OnStack sp} :: pushedState,
                            code=PushR srcReg :: (pushCode @ code), context=context}
                    end
                end
                
                (* Move a constant to a preg.  First push anything in RAX then load the constant into RAX. *)
            |   codeICode{icode=MoveArgument{source=AddressConstant srcValue, dest=PReg destReg} :: rest, state, code, context} =
                let
                    val (pushedState, pushCode) = pushRax state
                    (* The constant should be an address. *)
                    val _ = isShort srcValue andalso raise InternalError "AddressConstant: short"
                in
                    codeICode{
                        icode=rest, state={preg=destReg, loc=InRax} :: pushedState,
                        code=MoveLongConstR{source=srcValue, output=eax} :: (pushCode @ code),
                        context=context}
                end

                (* Move a short constant to a preg.  First push anything in RAX then load the constant into RAX. *)
            |   codeICode{icode=MoveArgument{source=IntegerConstant srcValue, dest=PReg destReg} :: rest, state, code, context} =
                let
                    val (pushedState, pushCode) = pushRax state
                in
                    codeICode{
                        icode=rest, state={preg=destReg, loc=InRax} :: pushedState,
                        code=MoveConstR{source=srcValue, output=eax} :: (pushCode @ code),
                        context=context}
                end

                (* Load a value from a piece of memory, typically the offset in a tuple. *)
            |   codeICode{icode=MoveArgument{ source=MemoryLocation{offset, base}, dest=PReg destReg} :: rest, state, code, context} =
                let
                    val bReg = argToPReg base
                    (* Currently we always make a new preg for the base and that is
                       on the top of the state.  We can just remove it and replace it
                       with the result. *)
                    val poppedState =
                        case state of
                            { preg, loc=InRax } :: tail =>
                                if preg = bReg then tail else raise InternalError "codeICode: MoveArgument/MemoryLocation"
                        |   _ => raise InternalError "codeICode: MoveArgument/MemoryLocation"
                in
                    codeICode{icode=rest, state={preg=destReg, loc=InRax} :: poppedState,
                        code=loadMemory(eax, eax, offset) @ code, context=context}
                end

                (* Store a pseudo-register in a memory location. *)
            |   codeICode{icode=MoveArgument{ source=PReg srcReg, dest=MemoryLocation{offset, base}} :: rest, state, code, context} =
                let
                    val bReg = argToPReg base
                    (* We currently generate these so that the top of the state is the allocated
                       memory and the next item to store is immediately below it. *)
                    val newState =
                        case state of
                            {preg=tReg, loc=InRax} :: {preg=nReg, loc=OnStack _} :: state =>
                                if tReg = bReg andalso nReg = srcReg
                                then state
                                else raise InternalError "MoveArgument to MemoryLocation"
                        |   _ => raise InternalError "MoveArgument to MemoryLocation"
                    (* Pop the value to store into ebx and store it in the tuple. *)
                    val store = storeMemory(ebx, eax, offset) @ [PopR ebx]
                in
                    codeICode{
                        (* We currently have the base in eax.  Leave it there because it may be used again. *)
                        icode=rest, state={preg=bReg, loc=InRax} :: newState, code=store @ code, context=context}
                end

                (* Other combinations of move aren't used at the moment. *)
            |   codeICode{icode=MoveArgument _ :: _, ...} =
                    raise InternalError "codeICode: MoveArgument"

                (* Load a value from the real stack.  Offset zero is the return address,
                   subsequent values are arguments that were pushed by the caller. *)
            |   codeICode{icode=MoveStackArgToPreg{offset, dest} :: rest, state, code, context} =
                let
                    val destReg = argToPReg dest
                    val (pushedState, pushCode) = pushRax state
                    val load = loadStack(eax, (stackPtr pushedState + offset) * wordSize)
                in
                    codeICode{
                        icode=rest, state={preg=destReg, loc=InRax} :: pushedState,
                        code=load @ pushCode @ code, context=context}
                end
                
            |   codeICode{icode=AllocateMemoryToPreg{ size, isMutable, dest} :: rest, state, code, context} =
                let
                    val destReg = argToPReg dest
                    val (pushedState, pushCode) = pushRax state
                    val alloc = allocateHeap(size, if isMutable then Address.F_mutable else 0w0, eax)
                in
                    codeICode{
                        icode=rest, state={preg=destReg, loc=InRax} :: pushedState,
                        code=alloc @ pushCode @ code, context=context}
                end

                (* Marks the end of setting the fields of a tuple.  This is used in the next
                   level to avoid reordering a new allocation until a previously allocated
                   section of memory has been fully initialised. *)
            |   codeICode{icode=InitialisationComplete _ :: rest, state, code, context} =
                    codeICode{icode=rest, state=state, code=StoreInitialised :: code, context=context}

                (* Conditional - test the test register and execute either the then- or else-part.
                   We need to merge the states at the result. *)
            |   codeICode{icode=Conditional{test, thenPt, elsePt, resultReg} :: rest, state, code, context} =
                let
                    val testReg = argToPReg test and resReg = argToPReg resultReg
                    (* The value on the top of the stack should be in eax and the value we want to test. *)
                    val bodyState =
                        case state of
                            {preg, loc=InRax } :: bodyState =>
                                if preg = testReg then bodyState else raise InternalError "Conditional"
                        |   _ => raise InternalError "Conditional"
                    (* Evaluate the test.  See if it is zero and go to ELSE if it is.
                       If non-zero execute the then-part and branch to the end.  If it
                       is zero execute the else-part. N.B.  This is in reverse. *)
                    val testIt = ArithRConst {opc=CMP, output=eax, source= tag 0 }
                    val (condJump, startElse) = condBranch(JE, PredictNeutral)
                    val (skipToEnd, endIf) = uncondBranch()
                    val (thenState, thenCode) =
                        codeICode{icode=thenPt, state=bodyState, code=condJump @ [testIt] @ code, context=context}
                    (* We have to remove all the stack entries that have been pushed. *)
                    val (_, resetAfterThen) = resetStack(thenState, bodyState, resReg)
                    val (elseState, elseCode) =
                        codeICode{
                            icode=elsePt, state=bodyState,
                            code=forwardJumpLabel startElse @ skipToEnd @ resetAfterThen @ thenCode, context=context}
                    val (resultState, resetAfterElse) = resetStack(elseState, bodyState, resReg)
                    val resCode =
                        forwardJumpLabel endIf @ resetAfterElse @ elseCode
                in
                    codeICode{icode=rest, state=resultState, code=resCode, context=context}
                end
                
                (* Function call. *)
            |   codeICode{icode=FunctionCall{ regArgs, stackArgs, dest, isTail=false} :: rest, state, code, context} =
                let
                    val destReg = argToPReg dest
                    val (pushedState, pushCode) = pushRax state
                    (* We should find the stack arguments occupy the immediately preceding
                       entries on the stack.  If not we've got a problem. *)
                    val numStackArgs = List.length stackArgs
                    val currentSp = stackPtr pushedState

                    fun checkStackArg ([], _) = ()
                    |   checkStackArg (sa::args, num) =
                        let
                            val e = findEntry(argToPReg sa, pushedState)
                        in
                            case e of
                                InRax => raise InternalError "checkStackArg"
                            |   OnStack s => if s = num then () else raise InternalError "checkStackArg";
                            checkStackArg(args, num+1)
                        end
                    val () = checkStackArg(stackArgs, currentSp-numStackArgs)
                    
                    (* Move the register arguments into the correct real registers. *)
                    val doLoad = List.foldl (fn ((pr, gr), l) => l @ loadSource(argToPReg pr, gr, pushedState)) [] regArgs
                    
                    (* After the call the stack arguments will have been removed by the caller. *)
                    (* We can just drop the stack args from the state.  We've checked them already.
                       Actually, we haven't checked that they were immediately preceding on this state list. *)
                    val returnState = List.drop(pushedState, numStackArgs)
                    (* We haven't actually checked that the entries we're dropping really
                       are the register args. *)
                    val poppedState = List.drop(returnState, List.length regArgs)
                    (* Add the result value in Rax to the state *)
                    val (finalState, stackReset) = resetStack({preg=destReg, loc=InRax} :: returnState, poppedState, destReg)
                    val functionCode =
                        stackReset @ [CallFunction FullCall] @ doLoad @ pushCode @ code
                in
                    codeICode{icode=rest, state=finalState, code=functionCode, context=context}
                end

                (* Jump to a function.  The arguments have to be moved into the space currently used
                   by the arguments to this function.  That could result in values being overwritten
                   so we have to save them, especially the return address. *)
            |   codeICode{icode=FunctionCall{ regArgs, stackArgs, dest, isTail=true} :: rest, state, code, context} =
                let
                    val destReg = argToPReg dest
                    val (pushedState, pushCode) = pushRax state
                    val currentSp = stackPtr pushedState

                    (* Load the earlier arguments into registers.  Do that first because
                       they're lower down the stack and we could overwrite them if we wait. *)
                    (* Load until we've done all the arguments or all the registers whichever is earlier.
                       This includes the closure register. *)
                    val doLoad = List.foldl (fn ((pr, gr), l) => l @ loadSource(argToPReg pr, gr, pushedState)) [] regArgs
                    
                    (* Load the return address.  This could be overwritten by an argument. *)
                    val loadReturn = loadStack(edi, currentSp * wordSize)

                    (* Move the stack arguments into the space occupied by this function's args. *)
                    val numArgsToMove = List.length stackArgs
                    val argOffset = currentSp + currentStackArgs - numArgsToMove
                    fun copyArgs (_, nil) = []
                    |   copyArgs (n, r :: rl) =
                            (* N.B. This is reversed *)
                            copyArgs(n-1, rl) @ storeMemory(esi, esp, (argOffset+n)*wordSize) @ loadSource(argToPReg r, esi, pushedState)
                    val moveArgs = copyArgs(numArgsToMove, stackArgs)
                    val storeReturn = storeMemory(edi, esp, argOffset*wordSize)

                    val functionCode =
                        JumpToFunction FullCall :: ResetStack argOffset ::
                            (storeReturn @ moveArgs @ loadReturn @ doLoad @ pushCode @ code)
                in
                    (* We don't really need to worry too much about the state since we've
                       exited.  Add a dummy result just to keep everything neat. *)
                    codeICode{icode=rest, state={preg=destReg, loc=InRax} :: pushedState, code=functionCode, context=context}
                end

                (* Start of a loop.  The loop body contains one or more tail jumps to the start. *)
            |   codeICode{icode=StartLoop{arguments, loopCode} :: rest, state, code, context} =
                let
                    (* The arguments have been evaluated to the stack.  Replace
                       the preg entries in the state with those of the loop variables. *)
                    val (pushedState, pushCode) = pushRax state
                    val numArgs = List.length arguments
                    val headList = List.take(pushedState, numArgs)
                    fun repSourceWithDest({source, loopReg}, {preg, loc}) =
                         if argToPReg source = preg then {preg=argToPReg loopReg, loc=loc} else raise InternalError "codeICode: StartLoop"
                    val newHead = ListPair.mapEq repSourceWithDest (List.rev arguments, headList)
                    val tailList = List.drop(pushedState, numArgs)
                    val bodyState = newHead @ tailList
                    val (startLoopCode, startLoop) = backJumpLabel()
                    val (loopState, loopBody) =
                        codeICode{
                            icode=loopCode, state=bodyState, code=startLoopCode @ pushCode @ code,
                            context={loopArgs=SOME(bodyState, startLoop)}}
                in
                    codeICode{icode=rest, state=loopState, code=loopBody, context=context}
                end

                (* A loop tail jump.  The arguments have been evaluated to the stack. *)
            |   codeICode{icode=JumpLoop{arguments, loopDest} :: nil, state, code,
                          context={loopArgs=SOME(startState, startLoop)}} =
                let
                    val lDestReg = argToPReg loopDest
                    (* Move each argument from the source to the loop variable.  This isn't the same as
                       MoveArgument because the destination already exists. *)
                    fun moveArg{source, loopReg} =
                    let
                        val load = loadSource(argToPReg source, eax, state)
                        val store =
                            case findEntry (argToPReg loopReg, state) of
                                OnStack s => storeMemory(eax, esp, (stackPtr state - s - 1) * wordSize)
                            |   InRax => raise InternalError "codeICode: JumpLoop"
                    in
                        store @ load
                    end
                    val storeAll = List.foldl(fn (arg, l) => l @ moveArg arg) [] arguments
                    val resetCode = [ResetStack(stackPtr state - stackPtr startState)]
                in
                    (* We need to return a state here even though we haven't actually returned. *)
                    ({preg=lDestReg, loc=InRax} :: startState, jumpBackwards startLoop @ resetCode @ storeAll @ code)
                end

            |   codeICode{icode=JumpLoop _ :: _, ...} =
                    raise InternalError "codeICode: JumpLoop out of loop or not final instruction"

                (* Raise an exception using the value in the "packet" register as the exception packet.
                   Since this exits we can ignore any further code and return the code and state. *)
            |   codeICode{icode=RaiseExceptionPacket{packet, dummyResult} :: _, state, code, ...} =
                let
                    val pktReg = argToPReg packet and dRes = argToPReg dummyResult
                    val load = loadSource(pktReg, eax, state)
                    (* Although we don't use the final state we need to get it right for the checks. *)
                    val resultState =
                        case state of
                            {loc=InRax, ...} :: t => t
                        |   s => s
                in
                    ({loc=InRax, preg=dRes} :: resultState, RaiseException :: (load @ code))
                end

                (* Reserve a contiguous area of memory on the stack.  This is used to receive the
                   results of a tuple.  It may be set within the same function or the address
                   may be passed into another function to allow it to return multiple results. *)
            |   codeICode{icode=ReserveContainer{size, address} :: rest, state, code, context} =
                let
                    val addrReg = argToPReg address
                    val (pushedState, pushCode) = pushRax state
                    (* The memory must be cleared in case we have a GC. *)
                    val pushAll = List.tabulate(size, fn _ => PushConst(tag 0))
                    val sp = stackPtr pushedState
                in
                    codeICode{icode=rest, state={loc=InRax, preg=addrReg} :: {loc=OnStack(sp+size-1), preg= ~1} :: pushedState,
                        code=MoveRR{source=esp, output=eax} :: (pushAll @ pushCode @ code), context=context}
                end
                
            |   codeICode{icode=HandleException{ tohandle, handler, resultReg, saveHandle } :: rest, state, code, context} =
                let
                    val resReg = argToPReg resultReg and sHandle = argToPReg saveHandle
                    val (pushedState, pushCode) = pushRax state
                    val sp = stackPtr pushedState
                    fun storeToHandler reg =
                        StoreRegToMemory{
                            toStore=reg, address=BaseOffset{offset=memRegHandlerRegister, base=ebp, index=NoIndex}}
                    (* We actually push two words here.  The first is the old handler, the second is the entry
                       point of the handler.  N.B.  This is reversed. *)
                    val labelRef = ref addrZero
                    val setHandler =
                        [
                            storeToHandler esp, PushR eax,
                            LoadHandlerAddress{ handlerLab=labelRef, output=eax},
                            PushMem{base=ebp, offset=memRegHandlerRegister}
                        ]
                    val startState = {preg=sHandle, loc=OnStack(sp+1) (* Two words *)} :: pushedState
                    val (bodyState, bodyCode) =
                        codeICode{icode=tohandle, state=startState, code=setHandler @ pushCode @ code, context=context}
                    val (_, resetAfterBody) = resetStack(bodyState, startState, resReg)
                    (* Use ebx here to hold the old handler *)
                    val restoreHandle = [storeToHandler ebx, PopR ebx, ResetStack 1]
                    (* Jump round the handler code. *)
                    val (skipHandler, toExit) = uncondBranch()
                    (* Start of handler.  Set the label, reload esp from the handler register, remove the handler
                       address and restore old handler.  N.B.  This is reversed. *)
                    val startHandler =
                        [
                            storeToHandler ebx, ResetStack 2,
                            LoadMemR{ source=BaseOffset{base=esp, offset=wordSize, index=NoIndex}, output=ebx },
                            LoadMemR{ source=BaseOffset{base=ebp, offset=memRegHandlerRegister, index=NoIndex}, output=esp },
                            StartHandler{handlerLab=labelRef}
                        ]
                    val (handleState, handleCode) =
                        codeICode{
                            icode=handler, state=pushedState,
                            code=startHandler @ skipHandler @ restoreHandle @ resetAfterBody @ bodyCode, context=context}
                    (* We're merging the states at this point. *)
                    val (resultState, resetAfterHandle) = resetStack(handleState, pushedState, resReg)
                    val resCode = forwardJumpLabel toExit @ resetAfterHandle @ handleCode
                in
                    codeICode{icode=rest, state=resultState, code=resCode, context=context}
                end

            |   codeICode{icode=IndexedCaseOperation{ testReg, workReg=_, default, cases, exhaustive, min, max,
                                             arbitrary, resultReg} :: rest, state, code, context} =
                let
                    val tReg = argToPReg testReg and resReg = argToPReg resultReg
                    (* We should have the value to test at the top of the stack. *)
                    val bodyState =
                        case state of
                            {preg, loc=InRax } :: bodyState =>
                                if preg = tReg then bodyState else raise InternalError "IndexedCase"
                        |   _ => raise InternalError "IndexedCase"
                    val (indexCaseInstr, caseLabels, defaultLabel) =
                        indexedCase{testReg=eax, workReg=ebx, minCase=min, maxCase=max, 
                                isArbitrary=arbitrary, isExhaustive=exhaustive}

                    (* Put in the default case.  Even when the case is exhaustive one entry is
                       always treated as a default and not included in the list of cases. *)
                    local
                        val startDefault = forwardJumpLabel defaultLabel
                        (* Go down the list of cases and fix up any default labels to come here.
                           Default entries are represented by "holes" in the case list. *)
                        fun genDefaults(indexVal, label :: labelList, cl as ((_, caseLabel) :: cps)) =
                            if indexVal = caseLabel
                            then genDefaults(indexVal+0w1, labelList, cps)
                            else genDefaults(indexVal+0w1, labelList, cl) @ forwardJumpLabel label

                        |   genDefaults(indexVal, label :: labelList, []) =
                                genDefaults(indexVal+0w1, labelList, []) @ forwardJumpLabel label

                        |   genDefaults(_, [], _) = []
                    
                        val otherDefaults = genDefaults(min, caseLabels, cases)
                    
                        val (defaultState, defaultCode) =
                            codeICode{icode=default, state=bodyState,
                                        code=otherDefaults @ startDefault @ indexCaseInstr @ code, context=context}

                        val (finalState, resetAfterDefault) =
                            resetStack(defaultState, bodyState, resReg)
                    
                        val (branchCode, lab) = uncondBranch()
                    in
                        val defaultCode = branchCode @ resetAfterDefault @ defaultCode
                        val exitDefault = lab
                        val finalState = finalState
                    end

                    (* Generate the cases. *)
                    fun genCases(indexVal, label :: labelList, (caseExp, caseLabel) :: cps, incode) =
                        if indexVal <> caseLabel
                        then (* We have a hole.  Skip this item. *)
                            genCases(indexVal+0w1, labelList, (caseExp, caseLabel) :: cps, incode)
                        else (* The index value corresponds to a label. *)
                        let
                            val startCode = forwardJumpLabel label @ incode
                            (* Generate this case. *)
                            val (expState, expCode) =
                                codeICode{icode=caseExp, state=bodyState, code=startCode, context=context}
                            val (_, resetAfterExp) = resetStack(expState, bodyState, resReg)
                        in
                            if null cps
                            then (*  Finished. *) resetAfterExp @ expCode
                            else
                            let
                                val (branchCode, lab) = uncondBranch()
                                val otherCases =
                                    genCases(indexVal+0w1, labelList, cps, branchCode @ resetAfterExp @ expCode)
                            in
                                forwardJumpLabel lab @ otherCases
                            end
                        end
                    | genCases _ = raise InternalError "genCase - null case list"

                    val caseResult = genCases(min, caseLabels, cases, defaultCode)
                in
                    codeICode{icode=rest, state=finalState, code=forwardJumpLabel exitDefault @ caseResult, context=context}
                end

                (* Lock a mutable segment. *)
            |   codeICode{icode=LockMutable{ addr } :: rest, state, code, context} =
                let
                    val (pushedState, pushCode) = pushRax state
                    val load = loadSource(argToPReg addr, eax, pushedState)
                    val lockCode = LockMutableSegment eax :: (load @ pushCode) @ code
                in
                    codeICode{icode=rest, state=pushedState, code=lockCode, context=context}
                end

            val (fnState, fnCode) = codeICode{icode=icode, state=[], code=[], context={loopArgs=NONE}}
            val (_, resetCode) = resetStack(fnState, [], argToPReg resultTarget)
        in
            val ops =
                List.rev fnCode @ resetCode @ [ReturnFromFunction currentStackArgs]

            val stackRequired = !maxStack
        end

        (* The stack limit register is set at least twice this far from the
           end of the stack so we can simply compare the stack pointer with
           the stack limit register if we need less than this much. Setting
           it at twice this value means that functions that use up to this
           much stack and do not call any other functions do not need to
           check the stack at all. *)
        val minStackCheck = 20
        
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
        val newCode = codeCreate (name, profileObject, debugSwitches) 
    in
        createCodeSegment(X86OPTIMISE.optimise(newCode, preludeCode @ ops), newCode)
    end

    fun gencodeLambda(lambda, debugSwitches, closure) =
    let
        open DEBUG Universal
        (*val debugSwitches =
            [tagInject Pretty.compilerOutputTag (Pretty.prettyPrint(print, 70)),
            tagInject assemblyCodeTag true] @ debugSwitches*)
        val codeAddr = codeFunctionToX86(lambda, debugSwitches, SOME closure)
        open Address
    in
        assignWord(closure, 0w0, toMachineWord codeAddr);
        lock closure
    end
    
    structure Foreign = X86FOREIGN
    
end;
