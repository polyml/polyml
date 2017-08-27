(*
    Copyright David C. J. Matthews 2016-17

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

functor X86CodetreeToICode(
    structure BACKENDTREE: BackendIntermediateCodeSig
    structure ICODE: ICodeSig
    structure DEBUG: DEBUGSIG
    structure X86FOREIGN: FOREIGNCALLSIG
    structure ICODETRANSFORM: X86ICODETRANSFORMSIG

    sharing ICODE.Sharing = ICODETRANSFORM.Sharing
): GENCODESIG =
struct
    open BACKENDTREE
    open Address
    open ICODE
    
    exception InternalError = Misc.InternalError

    val argRegs = List.map GenReg (if isX64 then [eax, ebx, r8, r9, r10] else [eax, ebx])
    val numArgRegs = List.length argRegs

    (* tag a short constant *)
    fun tag c = 2 * c + 1
  
    (* shift a short constant, but don't set tag bit *)
    fun semitag c = 2 * c

    (* Reverse a list and append the second.  This is used a lot when converting
       between the reverse and forward list versions. e.g. codeToICode and codeToICodeRev *)
    fun revApp([], l) = l
    |   revApp(hd :: tl, l) = revApp(tl, hd :: l)
    
    
    datatype blockStruct =
        BlockSimple of abstract x86ICode
    |   BlockExit of abstract x86ICode
    |   BlockLabel of int
    |   BlockFlow of controlFlow
    |   BlockBegin of (abstract * reg) list

    local
        open RunCall
        val F_mutable_bytes =  Word.fromLargeWord(Word8.toLargeWord(Word8.orb (F_mutable, F_bytes)))
        fun makeRealConst l =
        let
            val r = allocateByteMemory(0wx8 div bytesPerWord, F_mutable_bytes)
            fun setBytes([], _) = ()
            |   setBytes(hd::tl, n) = (storeByte(r, n, hd); setBytes(tl, n+0wx1))
            val () = setBytes(l, 0w0)
            val () = clearMutableBit r
        in
            r
        end
    in
        (* These are floating point constants used to change and mask the sign bit. *)
        val realSignBit: machineWord = makeRealConst [0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx80]
        and realAbsMask: machineWord = makeRealConst [0wxff, 0wxff, 0wxff, 0wxff, 0wxff, 0wxff, 0wxff, 0wx7f]
    end


    fun codeFunctionToX86({body, localCount, name, argTypes, closure, ...}:bicLambdaForm, debugSwitches, closureOpt) =
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

        val locToPregArray = Array.array(localCount, NONE: abstract option)
        val labelCounter = ref 1 (* Start at 1.  Zero is used for the root. *)
        fun newLabel() = !labelCounter before labelCounter := !labelCounter + 1
        val ccRefCounter = ref 0
        fun newCCRef() = CcRef(!ccRefCounter) before ccRefCounter := !ccRefCounter + 1

        val returnAddressEntry = newStackLoc 1

        local
            val numFunctionArgs = List.length argTypes
            val maxRegArgs = List.length argRegs
            
            fun createArg i = {stackOffset=numFunctionArgs-(i+maxRegArgs), stackReg = newStackLoc 1 }
        in
            val (argRegsUsed, currentStackArgs) =
                if numFunctionArgs >= maxRegArgs
                then (argRegs, numFunctionArgs - List.length argRegs)
                else (List.take(argRegs, numFunctionArgs), 0)
            
            val stackArgVector = Vector.tabulate(currentStackArgs, createArg)
        end
        val stackArguments = Vector.foldr(fn ({stackReg, ...}, l) => stackReg :: l) [] stackArgVector

        (* Pseudo-regs for the result, the closure and the args that were passed in real regs. *)
        val resultTarget = newPReg()
        val closureRegAddr = newPReg()
        val argPRegs = map (fn _ => newPReg()) argRegsUsed

        local
            val clReg = case closure of [] => [] | _ => [(closureRegAddr, GenReg edx)]
            val argRegs = ListPair.zip (argPRegs, argRegsUsed)
        in
            val beginInstruction = BlockBegin(clReg @ argRegs)
        end
        
        (* The return instruction.  This can be added on to various tails but there is always
           one at the end anyway. *)
        fun returnInstruction({stackPtr, ...}, target, tailCode) =
            BlockExit(ReturnResultFromFunction{resultReg=target, numStackArgs=currentStackArgs}) ::
            (if stackPtr <> 0
            then BlockSimple(ResetStackPtr{numWords=stackPtr}) :: tailCode
            else tailCode)

        fun constantAsArgument value =
            if isShort value
            then IntegerConstant(tag(Word.toLargeIntX(toShort value)))
            else AddressConstant value

        (* Create the branch condition from the test, isSigned and jumpOn values.
           (In)equality tests are the same for signed and unsigned values. *)
        local
            open BuiltIns
        in
            fun testAsBranch(TestEqual,         _,      true)       = JE
            |   testAsBranch(TestEqual,         _,      false)      = JNE
                (* Signed tests *)
            |   testAsBranch(TestLess,          true,   true)       = JL
            |   testAsBranch(TestLess,          true,   false)      = JGE
            |   testAsBranch(TestLessEqual,     true,   true)       = JLE
            |   testAsBranch(TestLessEqual,     true,   false)      = JG
            |   testAsBranch(TestGreater,       true,   true)       = JG
            |   testAsBranch(TestGreater,       true,   false)      = JLE
            |   testAsBranch(TestGreaterEqual,  true,   true)       = JGE
            |   testAsBranch(TestGreaterEqual,  true,   false)      = JL
                (* Unsigned tests *)
            |   testAsBranch(TestLess,          false,  true)       = JB
            |   testAsBranch(TestLess,          false,  false)      = JNB
            |   testAsBranch(TestLessEqual,     false,  true)       = JNA
            |   testAsBranch(TestLessEqual,     false,  false)      = JA
            |   testAsBranch(TestGreater,       false,  true)       = JA
            |   testAsBranch(TestGreater,       false,  false)      = JNA
            |   testAsBranch(TestGreaterEqual,  false,  true)       = JNB
            |   testAsBranch(TestGreaterEqual,  false,  false)      = JB
        end

        (* Generally we have an offset in words and no index register. *)
        fun wordOffsetAddress(offset, baseReg: abstract): abstract argument =
            MemoryLocation{offset=offset*wordSize, base=baseReg, index=NoMemIndex}
   
        (* The large-word operations all work on the value within the box pointed at
           by the register.  We generate all large-word operations using this even
           where the X86 instruction requires a register.  This allows the next level
           to optimise cases of cascaded instructions and avoid creating boxes for
           intermediate values. *)
        fun wordAt reg = wordOffsetAddress(0, reg)

        (* This controls what codeAsArgument returns.  Different instructions have different
           requirements.  If an option is set to false the value is instead loaded into a
           new preg.  "const32s" means that it will fit into 32-bits.  Any constant
           satisfies that on X86/32 but on the X86/64 we don't allow addresses because
           we can't be sure whether they will fit or not. *)
        type allowedArgument =
            { anyConstant: bool, const32s: bool, memAddr: bool, existingPreg: bool }
        val allowInMemMove = (* We can move a 32-bit constant into memory but not a long constant. *)
            { anyConstant=false, const32s=true, memAddr=false, existingPreg=true }
        and allowInPReg =
            { anyConstant=false, const32s=false, memAddr=false, existingPreg=true }
        (* AllowDefer can be used to ensure that any side-effects are done before
           something else but otherwise we only evaluate afterwards. *)
        and allowDefer =
            { anyConstant=true, const32s=true, memAddr=true, existingPreg=true }

        datatype destination =
            SpecificPReg of abstract
        |   NoResult
        |   Allowed of allowedArgument

        (* If a preg has been provided, use that, otherwise generate a new one. *)
        fun asTarget(SpecificPReg preg) = preg
        |   asTarget NoResult = newPReg()
        |   asTarget(Allowed _) = newPReg()

        fun moveIfNotAllowed(NoResult, code, arg) = (code, arg, false)

        |   moveIfNotAllowed(Allowed{anyConstant=true, ...}, code, arg as AddressConstant _) = (code, arg, false)
        
        |   moveIfNotAllowed(Allowed{anyConstant=true, ...}, code, arg as IntegerConstant _) = (code, arg, false)
        
        |   moveIfNotAllowed(dest as Allowed{const32s=true, ...}, code, arg as IntegerConstant value) =
            (* This is allowed if the value is within 32-bits *)
                if is32bit value
                then (code, arg, false)
                else moveToTarget(dest, code, arg)

        |   moveIfNotAllowed(dest as Allowed{const32s=true, ...}, code, arg as AddressConstant _) =
                if not isX64
                then (code, arg, false)
                else moveToTarget(dest, code, arg)

        |   moveIfNotAllowed(Allowed{existingPreg=true, ...}, code, arg as RegisterArgument(PReg _)) = (code, arg, false)
        
        |   moveIfNotAllowed(Allowed{memAddr=true, ...}, code, arg as MemoryLocation _) = (code, arg, false)

        |   moveIfNotAllowed(dest, code, arg) = moveToTarget(dest, code, arg)

        and moveToTarget(dest, code, arg) =
            let
                val target = asTarget dest
            in
                (code @ [BlockSimple(LoadArgument{source=arg, dest=target, kind=MoveWord})], RegisterArgument target, false)
            end

        fun moveIfNotAllowedRev(NoResult, code, arg) = (code, arg, false)

        |   moveIfNotAllowedRev(Allowed{anyConstant=true, ...}, code, arg as AddressConstant _) = (code, arg, false)
        
        |   moveIfNotAllowedRev(Allowed{anyConstant=true, ...}, code, arg as IntegerConstant _) = (code, arg, false)
        
        |   moveIfNotAllowedRev(dest as Allowed{const32s=true, ...}, code, arg as IntegerConstant value) =
            (* This is allowed if the value is within 32-bits *)
                if is32bit value
                then (code, arg, false)
                else moveToTargetRev(dest, code, arg)

        |   moveIfNotAllowedRev(dest as Allowed{const32s=true, ...}, code, arg as AddressConstant _) =
                if not isX64
                then (code, arg, false)
                else moveToTargetRev(dest, code, arg)

        |   moveIfNotAllowedRev(Allowed{existingPreg=true, ...}, code, arg as RegisterArgument(PReg _)) = (code, arg, false)
        
        |   moveIfNotAllowedRev(Allowed{memAddr=true, ...}, code, arg as MemoryLocation _) = (code, arg, false)

        |   moveIfNotAllowedRev(dest, code, arg) = moveToTargetRev(dest, code, arg)

        and moveToTargetRev(dest, code, arg) =
            let
                val target = asTarget dest
            in
                (BlockSimple(LoadArgument{source=arg, dest=target, kind=MoveWord}) :: code, RegisterArgument target, false)
            end

        fun codeToICodeTarget(instr, context, isTail, target) =
        (* This is really for backwards compatibility.  *)
        let
            val (code, _, _) = codeToICode(instr, context, isTail, SpecificPReg target)
        in
            code
        end
        
        and codeToPReg(instr, context) =
        let (* Many instructions require an argument in a register.  If it's already in a
               register use that rather than creating a new one. *)
            val (code, result, _) = codeToICode(instr, context, false, Allowed allowInPReg)
            val preg = case result of RegisterArgument pr => pr | _ => raise InternalError "codeToPReg"
        in
            (code, preg)
        end
        
        and codeToPRegRev(instr, context, tailCode) =
        let (* Many instructions require an argument in a register.  If it's already in a
               register use that rather than creating a new one. *)
            val (code, result, _) = codeToICodeRev(instr, context, false, Allowed allowInPReg, tailCode)
            val preg = case result of RegisterArgument pr => pr | _ => raise InternalError "codeToPRegRev"
        in
            (code, preg)
        end

        (* Main function to turn the codetree into ICode.  Optimisation is generally
           left to later passes.  This does detect tail recursion.
           This builds the result up in reverse order.  There was an allocation hotspot in loadFields
           in the BICTuple case which was eliminated by building the list in reverse and then
           reversing the result.  It seems better to build the list in reverse generally but for
           the moment there are too many special cases to do everything. *)
        and codeToICodeRev(BICNewenv (bindings, exp), context as {stackPtr=initialSp, ...} , isTail, destination, tailCode) =
            let
                (* Process a list of bindings.  We need to accumulate the space used by
                   any containers and reset the stack pointer at the end if necessary. *)
                fun doBindings([], context, tailCode) = (tailCode, context)
                    
                |   doBindings(BICDeclar{value, addr, ...} :: decs, context, tailCode) =
                    let
                        val dest = newPReg()
                        val (code, _, _) = codeToICodeRev(value, context, false, SpecificPReg dest, tailCode)
                        val () = Array.update(locToPregArray, addr, SOME dest)
                    in
                        doBindings(decs, context, code)
                    end

                |   doBindings(BICRecDecs [{lambda, addr, ...}] :: decs, context, tailCode) =
                    (* We shouldn't have single entries in RecDecs but it seems to occur at the moment. *)
                    let
                        val dest = newPReg()
                        val (code, _, _) = codeToICodeRev(BICLambda lambda, context, false, SpecificPReg dest, tailCode)
                        val () = Array.update(locToPregArray, addr, SOME dest)
                    in
                        doBindings(decs, context, code)
                    end

                |   doBindings(BICRecDecs recDecs :: decs, context, tailCode) =
                    let
                        val destRegs = map (fn _ => newPReg()) recDecs

                        (* First build the closures as mutable cells containing zeros.  Set the
                           entry in the address table to the register containing the address. *)
                        fun makeClosure({lambda={closure, ...}, addr, ...}, dest, c) =
                        let
                            val () = Array.update(locToPregArray, addr, SOME dest)
                            val sizeClosure = List.length closure + 1
                            fun clear n =
                                if n = sizeClosure
                                then [BlockSimple(AllocateMemoryOperation{size=sizeClosure, flags=Address.F_mutable, dest=dest, saveRegs=[]})]
                                else
                                    (clear (n+1) @
                                        [BlockSimple(StoreArgument{source=IntegerConstant(tag 0), base=dest, offset=n*wordSize, index=NoMemIndex, kind=MoveWord})])
                        in
                            c @ clear 0 @ [BlockSimple InitialisationComplete]
                        end
                    
                        val allocClosures = ListPair.foldlEq makeClosure [] (recDecs, destRegs)
                    
                        fun setClosure({lambda as {closure, ...}, ...}, dest, l) =
                        let
                            val codeAddr = codeFunctionToX86(lambda, debugSwitches, NONE)
                            (* Basically the same as tuple except we load the address of
                               the closure we've made.  It's complicated because
                               StoreArgument to MemoryLocation assumes that the top of the stack is
                               the address of the allocated memory and the items below
                               are the values to store. *)
                            val dstCopy = newPReg()
                            fun loadFields([], _) = [BlockSimple(LoadArgument{source=RegisterArgument dest, dest=dstCopy, kind=MoveWord})]
                            |   loadFields(f :: rest, n) =
                                let
                                    val (code, source, _) = codeToICode(f, context, false, Allowed allowInMemMove)
                                    val restAndAlloc = loadFields(rest, n+1)
                                    val storeValue =
                                        [BlockSimple(StoreArgument{ source=source, base=dstCopy, offset=n*wordSize, index=NoMemIndex, kind=MoveWord })]
                                in
                                    code @ restAndAlloc @ storeValue
                                end
                            val setFields = loadFields(BICConstnt(toMachineWord codeAddr, []) :: map BICExtract closure, 0)
                        in
                            l @ setFields @ [BlockSimple(LockMutable{addr=dest})]
                        end
                        val setClosures = ListPair.foldlEq setClosure [] (recDecs, destRegs)
                        
                        val code = List.rev(allocClosures @ setClosures) 
                    in
                        doBindings(decs, context, code @ tailCode)
                    end

                |   doBindings(BICNullBinding exp :: decs, context, tailCode) =
                    let
                        val (code, _, _) = codeToICodeRev(exp, context, false, NoResult, tailCode) (* And discard result. *)
                    in
                        doBindings(decs, context, code)
                    end
       
                |   doBindings(BICDecContainer{ addr, size } :: decs, {loopArgs, stackPtr}, tailCode) =
                    let
                        val dest = newPReg()
                        val () = Array.update(locToPregArray, addr, SOME dest)
                    in
                        doBindings(decs, {loopArgs=loopArgs, stackPtr=stackPtr+size},
                            BlockSimple(ReserveContainer{size=size, address=dest}) :: tailCode)
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
                            BlockSimple(LoadArgument{source=result, dest=resultReg, kind=MoveWord}) ::
                            BlockSimple(ResetStackPtr{numWords=finalSp-initialSp}) :: codeExp
                in
                    (afterAdjustSp, RegisterArgument resultReg, haveExited)
                end
                else codeToICodeRev(exp, resContext, isTail, destination, codeBindings)
            end

        |   codeToICodeRev(BICConstnt(value, _), _, _, destination, tailCode) =
                moveIfNotAllowedRev(destination, tailCode, constantAsArgument value)

        |   codeToICodeRev(BICExtract(BICLoadLocal l), _, _, destination, tailCode) =
                moveIfNotAllowedRev(destination, tailCode, RegisterArgument(valOf(Array.sub(locToPregArray, l))))

        |   codeToICodeRev(BICExtract(BICLoadArgument a), {stackPtr, ...}, _, destination, tailCode) =
            if a < numArgRegs
            then (* It was originally in a register.  It's now in a preg. *)
                moveIfNotAllowedRev(destination, tailCode, RegisterArgument(List.nth(argPRegs, a)))
            else (* Pushed before call. *)
            let
                val target = asTarget destination
                val {stackOffset, ...} = Vector.sub(stackArgVector, a-numArgRegs)
            in
                (BlockSimple(LoadArgument{
                    source=StackLocation{wordOffset=stackOffset+stackPtr}, dest=target, kind=MoveWord}) :: tailCode,
                 RegisterArgument target, false)
            end
        
        |   codeToICodeRev(BICExtract(BICLoadClosure c), _, _, destination, tailCode) =
            (
                if c >= List.length closure then raise InternalError "BICExtract: closure" else ();
                (* N.B.  We need to add one to the closure entry because zero is the code address. *)
                moveIfNotAllowedRev(destination, tailCode, wordOffsetAddress(c+1, closureRegAddr))
            )

        |   codeToICodeRev(BICExtract BICLoadRecursive, _, _, destination, tailCode) =
                (* If the closure is empty we must use the constant.  We can't guarantee that
                   the caller will actually load the closure register if it knows the closure
                   is empty. *)
                moveIfNotAllowedRev(destination, tailCode,
                    case closure of
                        [] => AddressConstant(toMachineWord(valOf closureOpt))
                    |   _ => RegisterArgument closureRegAddr)
        
        |   codeToICodeRev(BICField{base, offset}, context, _, destination, tailCode) =
            let
                val (codeBase, baseR) = codeToPRegRev(base, context, tailCode)
            in
                moveIfNotAllowedRev(destination, codeBase, wordOffsetAddress(offset, baseR))
            end

        |   codeToICodeRev(BICEval{function, argList, ...}, context, isTail, destination, tailCode) =
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
                            (* If this is a closure we're still compiling or if it's an address
                               of an io function (at the moment) we can't get the code address.
                               However if this is directly recursive we can use the recursive
                               convention. *)
                            val isRecursive =
                                case closureOpt of
                                    SOME a => wordEq(toMachineWord a, addr)
                                |   NONE => false
                        in
                            if isRecursive
                            then (tailCode, [], Recursive)
                            else if flags addrAsAddr <> Address.F_words
                            then (BlockSimple(LoadArgument{source=AddressConstant addr, dest=clPReg, kind=MoveWord}) :: tailCode,
                                      [(RegisterArgument clPReg, GenReg edx)], FullCall)
                            else
                            let
                            
                                val addrLength = length addrAsAddr
                                val _ = addrLength >= 0w1 orelse raise InternalError "BICEval address"
                                val codeAddr = loadWord(addrAsAddr, 0w0)
                                val _ = isCode (toAddress codeAddr) orelse raise InternalError "BICEval address not code"
                            in
                                if addrLength = 0w1
                                then (tailCode, [], ConstantCode codeAddr)
                                else (BlockSimple(LoadArgument{source=AddressConstant addr, dest=clPReg, kind=MoveWord}) :: tailCode,
                                      [(RegisterArgument clPReg, GenReg edx)], ConstantCode codeAddr)
                            end
                        end

                    |   BICExtract BICLoadRecursive =>
                        (
                            (* If the closure is empty we don't need to load rdx *)
                            case closure of
                                [] => (tailCode, [], Recursive)
                            |   _ =>
                                    (BlockSimple(LoadArgument {source=RegisterArgument closureRegAddr, dest=clPReg, kind=MoveWord}) :: tailCode,
                                     [(RegisterArgument clPReg, GenReg edx)], Recursive)
                        )

                    |   function => (* General case. *)
                            (#1 (codeToICodeRev(function, context, false, SpecificPReg clPReg, tailCode)), [(RegisterArgument clPReg, GenReg edx)], FullCall)
                (* Optimise arguments.  We have to be careful with tail-recursive functions because they
                   need to save any stack arguments that could be overwritten.  This is complicated
                   because we overwrite the stack before loading the register arguments.  In some
                   circumstances it could be safe but for the moment leave it.  This should be safe
                   in the new code-transform but not the old codeICode.
                   Currently we don't allow memory arguments at all.  There's the potential for
                   problems later.  Memory arguments could possibly lead to aliasing of the stack
                   if the memory actually refers to a container on the stack.  That would mess
                   up the code that ensures that stack arguments are stored in the right order. *)
                (* We don't allow long constants in stack arguments to a tail-recursive call
                   because we may use a memory move to set them. *)
                val allowInStackArg =
                    Allowed {anyConstant=not isTail, const32s=true, memAddr=false, existingPreg=not isTail }
                and allowInRegArg =
                    Allowed {anyConstant=true, const32s=true, memAddr=false, existingPreg=not isTail }

                (* Load the first arguments into registers and the rest to the stack. *)
                fun loadArgs ([], _, tailCode) = (tailCode, [], [])
                |   loadArgs ((arg, _) :: args, argReg::argRegs, tailCode) =
                    let (* Register argument. *)
                        val (c, r, _) = codeToICodeRev(arg, context, false, allowInRegArg, tailCode)
                        val (code, regArgs, stackArgs) = loadArgs(args, argRegs, c)
                    in
                        (code, (r, argReg) :: regArgs, stackArgs)
                    end
                |   loadArgs ((arg, _) :: args, [], tailCode) =
                    let (* Stack argument. *)
                        val (c, r, _) = codeToICodeRev(arg, context, false, allowInStackArg, tailCode)
                        val (code, regArgs, stackArgs) = loadArgs(args, [], c)
                    in
                        (code, regArgs, r :: stackArgs)
                    end
                val (codeArgs, regArgs, stackArgs) = loadArgs(argList, argRegs, functionCode)
                
                val callInstr =
                    if isTail
                    then
                    let
                        val {stackPtr, ...} = context
                        (* The number of arguments currently on the stack. *)
                        val currentStackArgCount = currentStackArgs
                        val newStackArgCount = List.length stackArgs
                        (* The offset of the first argument or the return address if there are
                           no stack arguments.  N.B. We actually have currentStackArgCount+1
                           items on the stack including the return address.  Offsets can be
                           negative. *)
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
                        val returnEntry = {src=StackLocation{wordOffset=stackPtr}, stack=stackAdjust}
                    in
                        BlockExit(TailRecursiveCall{regArgs=closureEntry @ regArgs, stackArgs=returnEntry :: stackArgs,
                                  stackAdjust = stackAdjust, stackOffset=stackOffset, callKind=callKind})
                    end
                    else BlockSimple(FunctionCall{regArgs=closureEntry @ regArgs, stackArgs=stackArgs, dest=target, callKind=callKind})
            in
                (callInstr :: codeArgs, RegisterArgument target, isTail (* We've exited if this was a tail jump *))
            end

        |   codeToICodeRev(BICGetThreadId, _, _, destination, tailCode) =
            (* Get the ID of the current thread. *)
            let
                val target = asTarget destination
            in
                (BlockSimple(LoadMemReg{offset=memRegThreadSelf, dest=target}) :: tailCode, RegisterArgument target, false)
            end

        |   codeToICodeRev(instr as BICUnary _, context, isTail, destination, tailCode) =
            let
                val (code, dest, haveExited) = codeToICode(instr, context, isTail, destination)
            in
                (revApp(code, tailCode), dest, haveExited)
            end

        |   codeToICodeRev(instr as BICBinary _, context, isTail, destination, tailCode) =
            let
                val (code, dest, haveExited) = codeToICode(instr, context, isTail, destination)
            in
                (revApp(code, tailCode), dest, haveExited)
            end

        |   codeToICodeRev(instr as BICArbitrary _, context, isTail, destination, tailCode) =
            let
                val (code, dest, haveExited) = codeToICode(instr, context, isTail, destination)
            in
                (revApp(code, tailCode), dest, haveExited)
            end

        |   codeToICodeRev(instr as BICAllocateWordMemory _, context, isTail, destination, tailCode) =
            let
                val (code, dest, haveExited) = codeToICode(instr, context, isTail, destination)
            in
                (revApp(code, tailCode), dest, haveExited)
            end

        |   codeToICodeRev(BICLambda(lambda as { closure = [], ...}), _, _, destination, tailCode) =
            (* Empty closure - create a constant closure for any recursive calls. *)
            let
                val closure = Address.allocWordData(0w1, Word8.orb (F_mutable, F_words), Address.toMachineWord 0w0)
                val codeAddr = codeFunctionToX86(lambda, debugSwitches, SOME closure)
                open Address
            in
                assignWord(closure, 0w0, toMachineWord codeAddr);
                lock closure;
                moveIfNotAllowedRev(destination, tailCode, AddressConstant(toMachineWord closure))
            end

        |   codeToICodeRev(BICLambda(lambda as { closure, ...}), context, isTail, destination, tailCode) =
            (* Non-empty closure.  Ignore stack closure option at the moment. *)
            let
                val codeAddr = codeFunctionToX86(lambda, debugSwitches, NONE)
            in
                (* Treat it as a tuple with the code as the first field. *)
                codeToICodeRev(BICTuple(BICConstnt(toMachineWord codeAddr, []) :: map BICExtract closure), context, isTail, destination, tailCode)
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
                (BlockLabel skipElse :: codeElse, (* Unit result *) IntegerConstant(tag 0), false)
            end

        |   codeToICodeRev(BICCond(test, thenPt, elsePt), context, isTail, destination, tailCode) =
            let
                (* Because we may push the result onto the stack we have to create a new preg to
                   hold the result and then copy that to the final result. *)
                (* If this is a tail each arm will exit separately and neither will return a result. *)
                val target = asTarget destination
                val condResult = newPReg()
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
                         [BlockSimple(LoadArgument{source=RegisterArgument condResult, dest=target, kind=MoveWord}),
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
                (thenLabel @ exitElse, RegisterArgument target, isTail orelse thenExited andalso elseExited)
            end

        |   codeToICodeRev(BICCase { cases, test, default, isExhaustive, firstIndex}, context, isTail, destination, tailCode) =
            let
                (* We have to create a new preg for the result in case we need to push
                   it to the stack. *)
                val targetReg = newPReg()
                
                local
                    val initialTestReg = newPReg()
                    val (testCode, _, _) = codeToICodeRev(test, context, false, SpecificPReg initialTestReg, tailCode)
                    (* Subtract the minimum value so the value we're testing is always in the range of
                       (tagged) 0 to the maximum.  It is possible to adjust the value when computing the index
                       but that can lead to overflows during compilation if the minimum is very large or small.
                       We can ignore overflow and allow values to wrap round. *)
                in
                    val (testCode, testReg) =
                        if firstIndex = 0w0
                        then (testCode, initialTestReg)
                        else
                        let
                            val newTestReg = newPReg()
                            val subtract =
                                BlockSimple(ArithmeticFunction{oper=SUB, resultReg=newTestReg, operand1=RegisterArgument initialTestReg,
                                                   operand2=IntegerConstant(semitag(Word.toLargeInt firstIndex)), ccRef=newCCRef()})
                        in
                            (subtract :: testCode, newTestReg)
                        end
                end

                val workReg = newPReg()
               
                (* Unless this is exhaustive we need to add a range check. *)
                val (rangeCheck, extraDefaults) =
                    if isExhaustive
                    then (testCode, [])
                    else
                    let
                        val defLab1 = newLabel() 
                        val tReg1 = newPReg()
                        val ccRef1 = newCCRef()
                        (* Since we've subtracted any minimum we only have to check whether the value is greater (unsigned)
                           than the maximum. *)
                        val numberOfCases = LargeInt.fromInt(List.length cases)
                        val continueLab = newLabel()
                        val testCode2 =
                                BlockLabel continueLab ::
                                BlockFlow(Conditional{ccRef=ccRef1, condition=JNB, trueJump=defLab1, falseJump=continueLab}) ::
                                BlockSimple(WordComparison{arg1=RegisterArgument tReg1, arg2=IntegerConstant(tag numberOfCases), ccRef=ccRef1}) ::
                                BlockSimple(LoadArgument {source=RegisterArgument testReg, dest=tReg1, kind=MoveWord}) :: testCode
                    in
                        (testCode2, [defLab1])
                    end
                
                (* Make a label for each item in the list. *)
                val codeLabels = map (fn _ => newLabel()) cases
                
                (* Create an exit label in case it's needed. *)
                val labelForExit = if isTail then ~1 (* Illegal label. *) else newLabel()

                (* Generate the code for each of the cases and the default.  We need to put an
                   unconditional branch after each to skip the other cases.
                   TODO: This could be replaced by "returns" if we're at the tail. *)
                fun codeCases (SOME c :: otherCases, startLabel :: otherLabels, tailCode) =
                    let
                        val caseTarget = if isTail then newPReg() else targetReg
                        (* Put in the case with a jump to the end of the sequence. *)
                        val (codeThisCase, _, caseExited) =
                            codeToICodeRev(c, context, isTail, SpecificPReg caseTarget,
                                BlockLabel startLabel :: tailCode) 
                        val exitThisCase =
                            if caseExited then codeThisCase
                            else if isTail then returnInstruction(context, caseTarget, codeThisCase)
                            else BlockFlow(Unconditional labelForExit) :: codeThisCase
                    in
                        codeCases(otherCases, otherLabels, exitThisCase)
                    end

                |   codeCases(NONE :: otherCases, _ :: otherLabels, tailCode) = codeCases(otherCases, otherLabels, tailCode)
                        
                |   codeCases ([], [], tailCode) =
                    let
                        (* We need to add labels for all the gaps we filled and also for a "default" label for
                           the indexed-case instruction itself as well as any range checks. *)
                        fun addDefault (startLabel, NONE, l) = BlockLabel startLabel :: l
                        |   addDefault (_, SOME _, l) = l
                        fun asForward l = BlockLabel l
                        val dLabs = map asForward extraDefaults @ tailCode
                        val defLabels = ListPair.foldlEq addDefault dLabs (codeLabels, cases)
                        val defaultTarget = if isTail then newPReg() else targetReg
                        val (defaultCode, _, defaultExited) =
                            codeToICodeRev(default, context, isTail, SpecificPReg defaultTarget, defLabels)
                    in
                        (* Put in the default.  Because this is the last we don't need to
                           jump round it.  However if this is a tail and we haven't exited
                           we put in a return.  That way the case will always have
                           exited if this is a tail. *)
                         if isTail andalso not defaultExited
                         then returnInstruction(context, defaultTarget, defaultCode)
                         else defaultCode
                    end

                |   codeCases _ = raise InternalError "codeCases: mismatch"
                    
                val codedCases =
                    codeCases(cases, codeLabels,
                        BlockFlow(IndexedBr codeLabels) ::
                        BlockSimple(IndexedCaseOperation{testReg=testReg, workReg=workReg}) ::
                        rangeCheck)
                (* We can now copy to the target.  If we need to push the result this load
                   will be converted into a push. *)
                val target = asTarget destination
                val copyToTarget =
                    if isTail then codedCases
                    else BlockSimple(LoadArgument{source=RegisterArgument targetReg, dest=target, kind=MoveWord}) ::
                            BlockLabel labelForExit :: codedCases
            in
                (copyToTarget, RegisterArgument target, isTail (* We have always exited on a tail. *))
            end

        |   codeToICodeRev(BICBeginLoop {loop, arguments}, context as { stackPtr, ...}, isTail, destination, tailCode) =
            let
                val target = asTarget destination
                
                fun codeArgs ([], tailCode) = ([], tailCode)
                |   codeArgs (({value, addr}, _) :: rest, tailCode) =
                    let
                        val pr = newPReg()
                        val () = Array.update(locToPregArray, addr, SOME pr)
                        val (code, _, _) = codeToICodeRev(value, context, false, SpecificPReg pr, tailCode)
                        val (pregs, othercode) = codeArgs(rest, code)
                    in
                        (pr::pregs, othercode)
                    end
                val (loopRegs, argCode) = codeArgs(arguments, tailCode)

                val loopLabel = newLabel()
                val (loopBody, _, loopExited) =
                    codeToICodeRev(loop, {loopArgs=SOME (loopRegs, loopLabel, stackPtr), stackPtr=stackPtr }, isTail, SpecificPReg target,
                        BlockLabel loopLabel :: argCode)
            in
                (loopBody, RegisterArgument target, loopExited)
            end

        |   codeToICodeRev(BICLoop args, context as {loopArgs=SOME (loopRegs, loopLabel, loopSp), stackPtr, ...}, _, destination, tailCode) =
            let
                val target = asTarget destination
                (* Registers to receive the evaluated arguments.  We can't put the
                   values into the loop variables yet because the values could depend
                   on the current values of the loop variables. *)
                val argPRegs = map(fn _ => newPReg()) args
                val codeArgs =
                    ListPair.foldlEq(fn ((arg, _), pr, l) =>
                        #1 (codeToICodeRev(arg, context, false, SpecificPReg pr, l))) tailCode
                        (args, argPRegs)
                val jumpArgs = ListPair.mapEq(fn (s, l) => (RegisterArgument s, l)) (argPRegs, loopRegs)
                (* If we've allocated a container in the loop we have to remove it before jumping back. *)
                val stackReset =
                    if loopSp = stackPtr then codeArgs
                    else BlockSimple(ResetStackPtr{numWords=stackPtr-loopSp}) :: codeArgs
            in
                (BlockFlow(Unconditional loopLabel) :: BlockSimple(JumpLoop{regArgs=jumpArgs, stackArgs=[], checkInterrupt=SOME[]}) :: stackReset, RegisterArgument target, true)
            end

        |   codeToICodeRev(BICLoop _, {loopArgs=NONE, ...}, _, _, _) = raise InternalError "BICLoop without BICBeginLoop"


        |   codeToICodeRev(BICRaise exc, context, _, destination, tailCode) =
            let
                val (code, dest, _) =
                    codeToICodeRev(exc, context, false, Allowed allowDefer, tailCode)
            in
                (BlockExit(RaiseExceptionPacket{packet=dest}) :: code, RegisterArgument(asTarget destination), true (* Always exits *))
            end

        |   codeToICodeRev(BICHandle{exp, handler, exPacketAddr}, context as { stackPtr, loopArgs }, isTail, destination, tailCode) =
            let
                (* As with BICCond and BICCase we need to create a new register for the
                   result in case we need to push it to the stack. *)
                val handleResult = newPReg()
                val handlerLab = newLabel() and startHandling = newLabel()
                val (bodyTarget, handlerTarget) =
                    if isTail then (newPReg(), newPReg()) else (handleResult, handleResult)
                (* TODO: Even if we don't actually want a result we force one in here by
                   using "asTarget".  *)
                (* The expression cannot be treated as a tail because the handler has
                   to be removed after.  It may "exit" if it has raised an unconditional
                   exception.  If it has we mustn't generate a PopExceptionHandler because
                   there won't be any result for resultReg.
                   We need to add two words to the stack to account for the items pushed by
                   PushExceptionHandler.
                   We create an instruction to push the handler followed by a block fork to
                   the start of the code and, potentially the handler, then a label to start
                   the code that the handler is in effect for. *)
                val initialCode =
                    BlockLabel startHandling ::
                    BlockFlow(SetHandler{handler=handlerLab, continue=startHandling}) ::
                    BlockSimple(PushExceptionHandler{workReg=newPReg()}) :: tailCode
                val (expCode, _, expExit) =
                    codeToICodeRev(exp, {stackPtr=stackPtr+2, loopArgs=loopArgs}, false (* Not tail *), SpecificPReg bodyTarget, initialCode)
                (* If this is the tail we can replace the jump at the end of the
                   handled code with returns.  If the handler has exited we don't need
                   a return there.  Otherwise we need to add an unconditional jump to
                   skip the handler. *)
                val (atExpEnd, skipExpLabel) =
                    case (isTail, expExit) of
                        (true, true) => (* Tail and exited. *) (expCode, NONE)
                    |   (true, false) => (* Tail and not exited. *)
                            (returnInstruction(context, bodyTarget,
                                BlockSimple(PopExceptionHandler{workReg=newPReg()}) :: expCode), NONE)
                    |   (false, true) => (* Not tail but exited. *) (expCode, NONE)
                    |   (false, false) =>
                        let
                            val skipHandler = newLabel()
                        in
                            (BlockFlow(Unconditional skipHandler) ::
                             BlockSimple(PopExceptionHandler{workReg=newPReg()}) :: expCode, SOME skipHandler)
                        end
                (* Make a register to hold the exception packet and put eax into it. *)
                val packetAddr = newPReg()
                val () = Array.update(locToPregArray, exPacketAddr, SOME packetAddr)
                val (handleCode, _, handleExit) =
                    codeToICodeRev(handler, context, isTail, SpecificPReg handlerTarget,
                        BlockSimple(BeginHandler{workReg=newPReg(), packetReg=packetAddr}) :: BlockLabel handlerLab :: atExpEnd)
                val target = asTarget destination
                val afterHandler =
                    case (isTail, handleExit) of
                        (true, true) => (* Tail and exited. *) handleCode
                    |   (true, false) => (* Tail and not exited. *)
                            returnInstruction(context, handlerTarget, handleCode)
                    |   (false, _) => (* Not tail. *) handleCode
                
                val addLabel =
                    case skipExpLabel of
                        SOME lab => BlockLabel lab:: afterHandler
                    |   NONE => afterHandler
            in
                (BlockSimple(LoadArgument{source=RegisterArgument handleResult, dest=target, kind=MoveWord}) :: addLabel,
                    RegisterArgument target, isTail)
            end

        |   codeToICodeRev(BICTuple fields, context, _, destination, tailCode) =
            let
                (* TODO: This is a relic of the old fall-back code-generator.  It required
                   the result of a tuple to be at the top of the stack.  It should be changed. *)
                val target = asTarget destination (* Actually we want this. *)
                val memAddr = newPReg()
                fun loadFields([], n, tlCode) =
                        BlockSimple(AllocateMemoryOperation{size=n, flags=0w0, dest=memAddr, saveRegs=[]}) :: tlCode
                |   loadFields(f :: rest, n, tlCode) =
                    let
                        (* Defer the evaluation if possible.  We may have a constant that we can't move
                           directly but it's better to load it after the allocation otherwise we will
                           have to push the register if we need to GC. *)
                        val (code1, source1, _) = codeToICodeRev(f, context, false, Allowed allowDefer, tlCode)
                        val restAndAlloc = loadFields(rest, n+1, code1)
                        val (code2, source, _)  = moveIfNotAllowedRev(Allowed allowInMemMove, restAndAlloc, source1)
                        val storeValue = BlockSimple(StoreArgument{ source=source, offset=n*wordSize, base=memAddr, index=NoMemIndex, kind=MoveWord})
                    in
                        storeValue :: code2
                    end
                val code =
                    BlockSimple InitialisationComplete ::
                        BlockSimple(LoadArgument{source=RegisterArgument memAddr, dest=target, kind=MoveWord}) ::
                        loadFields(fields, 0, tailCode)
            in
                (code, RegisterArgument target, false)
            end

        |   codeToICodeRev(instr as BICSetContainer _, context, isTail, destination, tailCode) =
            let
                val (code, dest, haveExited) = codeToICode(instr, context, isTail, destination)
            in
                (revApp(code, tailCode), dest, haveExited)
            end

        |   codeToICodeRev(instr as BICTagTest _, context, isTail, destination, tailCode) =
                (* Better handled as a conditional *)
                codeAsConditionalRev(instr, context, isTail, destination, tailCode)

        |   codeToICodeRev(instr as BICLoadOperation _, context, isTail, destination, tailCode) =
            let
                val (code, dest, haveExited) = codeToICode(instr, context, isTail, destination)
            in
                (revApp(code, tailCode), dest, haveExited)
            end

        |   codeToICodeRev(instr as BICStoreOperation _, context, isTail, destination, tailCode) =
            let
                val (code, dest, haveExited) = codeToICode(instr, context, isTail, destination)
            in
                (revApp(code, tailCode), dest, haveExited)
            end

        |   codeToICodeRev(instr as BICBlockOperation {kind=BlockOpEqualByte, ...}, context, isTail, destination, tailCode) =
                codeAsConditionalRev(instr, context, isTail, destination, tailCode)

        |   codeToICodeRev(instr as BICBlockOperation _, context, isTail, destination, tailCode) =
            let
                val (code, dest, haveExited) = codeToICode(instr, context, isTail, destination)
            in
                (revApp(code, tailCode), dest, haveExited)
            end

        and codeToICode(BICConstnt(value, _), _, _, destination) =
                moveIfNotAllowed(destination, [], constantAsArgument value)

        |   codeToICode(BICUnary{oper=BuiltIns.NotBoolean, arg1}, context, _, destination) =
            let
                (* TODO: If the argument is something that will be a conditional we would be better off
                   generating a conditional here. *)
                val target = asTarget destination
                val ccRef = newCCRef()
                val (argCode, tReg) = codeToPReg(arg1, context)
            in
                (argCode @
                    [BlockSimple(ArithmeticFunction{oper=XOR, resultReg=target, operand1=RegisterArgument tReg, operand2=IntegerConstant(semitag 1), ccRef=ccRef})],
                        RegisterArgument target, false)
            end

        |   codeToICode(instr as BICUnary{oper=BuiltIns.IsTaggedValue, ...}, context, isTail, destination) =
                codeAsConditional(instr, context, isTail, destination)

        |   codeToICode(BICUnary{oper=BuiltIns.MemoryCellLength, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val argReg1 = newPReg() and argReg2 = newPReg() and argReg3 = newPReg()
                and ccRef1 = newCCRef() and ccRef2 = newCCRef() and ccRef3 = newCCRef()
                (* Get the length of a memory cell (heap object).  We need to mask out the
                   top byte containing the flags and to tag the result.  The mask is 56 bits on
                   64-bit which won't fit in an inline constant.  Since we have to shift it
                   anyway we might as well do this by shifts. *)
                val (argCode, addrReg) = codeToPReg(arg1, context)
            in
                (argCode @
                [BlockSimple(LoadArgument{source=wordOffsetAddress(~1, addrReg), dest=argReg1, kind=MoveWord}),
                 BlockSimple(ShiftOperation{shift=SHL, resultReg=argReg2, operand=RegisterArgument argReg1, shiftAmount=IntegerConstant 8, ccRef=ccRef1 }),
                 BlockSimple(ShiftOperation{shift=SHR, resultReg=argReg3, operand=RegisterArgument argReg2, shiftAmount=IntegerConstant 7 (* 8-tagshift*), ccRef=ccRef2 }),
                 BlockSimple(ArithmeticFunction{oper=OR, resultReg=target, operand1=RegisterArgument argReg3, operand2=IntegerConstant 1, ccRef=ccRef3})], RegisterArgument target, false)
            end

        |   codeToICode(BICUnary{oper=BuiltIns.MemoryCellFlags, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val argReg1 = newUReg()
                val (argCode, addrReg) = codeToPReg(arg1, context)
            in
                (argCode @
                [BlockSimple(LoadArgument{source=MemoryLocation{offset= ~1, base=addrReg, index=NoMemIndex}, dest=argReg1, kind=MoveByte}),
                 BlockSimple(TagValue{ source=argReg1, dest=target })], RegisterArgument target, false)
            end

        |   codeToICode(BICUnary{oper=BuiltIns.ClearMutableFlag, arg1}, context, _, destination) =
            let
                val (argCode, addrReg) = codeToPReg(arg1, context)
                val code = argCode @ [BlockSimple(LockMutable{addr=addrReg})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICUnary{oper=BuiltIns.AtomicIncrement, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val incrReg = newUReg()
                val (argCode, addrReg) = codeToPReg(arg1, context)
            in
                (argCode @
                 [BlockSimple(LoadArgument{source=IntegerConstant(semitag 1), dest=incrReg, kind=MoveWord}),
                  BlockSimple(AtomicExchangeAndAdd{ base=addrReg, source=incrReg }),
                  (* We want the result to be the new value but we've returned the old value. *)
                  BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument incrReg, operand2=IntegerConstant(semitag 1), ccRef=newCCRef()})],
                    RegisterArgument target, false)
            end

        |   codeToICode(BICUnary{oper=BuiltIns.AtomicDecrement, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val incrReg = newUReg()
                val (argCode, addrReg) = codeToPReg(arg1, context)
            in
                (argCode @
                 [BlockSimple(LoadArgument{source=IntegerConstant(semitag ~1), dest=incrReg, kind=MoveWord}),
                  BlockSimple(AtomicExchangeAndAdd{ base=addrReg, source=incrReg }),
                  BlockSimple(ArithmeticFunction{oper=SUB, resultReg=target, operand1=RegisterArgument incrReg, operand2=IntegerConstant(semitag 1), ccRef=newCCRef()})],
                    RegisterArgument target, false)
            end

        |   codeToICode(BICUnary{oper=BuiltIns.AtomicReset, arg1}, context, _, destination) =
            let
                (* This is needed only for the interpreted version where we have a single real
                   mutex to interlock atomic increment and decrement.  We have to use the same
                   mutex to interlock clearing a mutex.  On the X86 we use hardware locking and
                   the hardware guarantees that an assignment of a word will be atomic. *)
                val (argCode, addrReg) = codeToPReg(arg1, context)
                (* Store tagged 1 in the mutex.  This is the unlocked value. *)
                val code = argCode @ [BlockSimple(StoreArgument{source=IntegerConstant(tag 1), base=addrReg, index=NoMemIndex, offset=0, kind=MoveWord})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICUnary{oper=BuiltIns.LongWordToTagged, arg1}, context, _, destination) =
            let (* This is exactly the same as StringLengthWord at the moment.
                   TODO: introduce a new ICode entry so that the next stage can optimise
                   longword operations. *)
                val target = asTarget destination
                val argReg1 = newUReg()
                val (argCode, addrReg) = codeToPReg(arg1, context)
            in
                (argCode @
                [BlockSimple(LoadArgument{source=MemoryLocation{offset=0, base=addrReg, index=NoMemIndex}, dest=argReg1, kind=MoveWord}),
                 BlockSimple(TagValue{ source=argReg1, dest=target })], RegisterArgument target, false)
            end

        |   codeToICode(BICUnary{oper=BuiltIns.SignedToLongWord, arg1}, context, _, destination) =
            let
                val addrReg = newPReg() and argReg1 = newPReg() and untagArg = newUReg()
                val code =
                    codeToICodeTarget(arg1, context, false, argReg1) @
                    [BlockSimple(UntagValue{source=RegisterArgument argReg1, dest=untagArg, isSigned=true}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord,  source=untagArg, dest=addrReg, saveRegs=[]})]
            in
                moveIfNotAllowed(destination, code, RegisterArgument addrReg)
            end

        |   codeToICode(BICUnary{oper=BuiltIns.UnsignedToLongWord, arg1}, context, _, destination) =
            let
                val addrReg = newPReg() and argReg1 = newPReg() and untagArg = newUReg()
                val code =
                    codeToICodeTarget(arg1, context, false, argReg1) @
                    [BlockSimple(UntagValue{source=RegisterArgument argReg1, dest=untagArg, isSigned=false}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord,  source=untagArg, dest=addrReg, saveRegs=[]})]
            in
                moveIfNotAllowed(destination, code, RegisterArgument addrReg)
            end

        |   codeToICode(BICUnary{oper=BuiltIns.RealNeg, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and fpRegSrc = newUReg() and fpRegDest = newUReg() and sse2ConstReg = newUReg()
                (* The SSE2 code uses an SSE2 logical operation to flip the sign bit.  This
                   requires the values to be loaded into registers first because the logical
                   operations require 128-bit operands. *)
                val load = BlockSimple(LoadArgument{source=wordAt aReg1, dest=fpRegSrc, kind=MoveDouble})
                val code =
                    case fpMode of
                        FPModeX87 =>
                            [BlockSimple(X87FPUnaryOps{ fpOp=FCHS, dest=fpRegDest, source=RegisterArgument fpRegSrc})]
                    |   FPModeSSE2 =>
                            [BlockSimple(LoadArgument{source=AddressConstant realSignBit, dest=sse2ConstReg, kind=MoveDouble}),
                             BlockSimple(SSE2FPArith{opc=SSE2Xor, resultReg=fpRegDest, arg1=RegisterArgument fpRegSrc, arg2=RegisterArgument sse2ConstReg})]
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @
                 [load] @ code @ [BlockSimple(BoxValue{boxKind=BoxFloat, source=fpRegDest, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICode(BICUnary{oper=BuiltIns.RealAbs, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and fpRegSrc = newUReg() and fpRegDest = newUReg() and sse2ConstReg = newUReg()
                val load = BlockSimple(LoadArgument{source=wordAt aReg1, dest=fpRegSrc, kind=MoveDouble})
                val code =
                    case fpMode of
                        FPModeX87 => [BlockSimple(X87FPUnaryOps{ fpOp=FABS, dest=fpRegDest, source=RegisterArgument fpRegSrc})]
                    |   FPModeSSE2 =>
                            [BlockSimple(LoadArgument{source=AddressConstant realAbsMask, dest=sse2ConstReg, kind=MoveDouble}),
                             BlockSimple(SSE2FPArith{opc=SSE2And, resultReg=fpRegDest, arg1=RegisterArgument fpRegSrc, arg2=RegisterArgument sse2ConstReg})]
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @
                 [load] @ code @ [BlockSimple(BoxValue{boxKind=BoxFloat, source=fpRegDest, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICode(BICUnary{oper=BuiltIns.FloatFixedInt, arg1}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and untagReg = newUReg() and fpReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @
                 [BlockSimple(UntagValue{source=RegisterArgument aReg1, dest=untagReg, isSigned=true}),
                  BlockSimple(FloatFixedInt{ dest=fpReg, source=RegisterArgument untagReg}),
                  BlockSimple(BoxValue{boxKind=BoxFloat, source=fpReg, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICode(instr as BICBinary{oper=BuiltIns.WordComparison _, ...}, context, isTail, destination) =
                codeAsConditional(instr, context, isTail, destination)

        |   codeToICode(BICBinary {oper=BuiltIns.FixedPrecisionArith oper, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                fun checkOverflow ccRef =
                let
                    val overFlowLab = newLabel() and noOverflowLab = newLabel()
                in
                    [
                        BlockFlow(Conditional{ ccRef=ccRef, condition=JNO, trueJump=noOverflowLab, falseJump=overFlowLab }),
                        BlockLabel overFlowLab,
                        BlockExit(RaiseExceptionPacket{packet=AddressConstant(toMachineWord(Overflow))}),
                        BlockLabel noOverflowLab
                    ]
                end
            in
                (codeFixedPrecisionArith(oper, arg1, arg2, context, target, checkOverflow), RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordArith BuiltIns.ArithAdd, arg1, arg2=BICConstnt(value, _)}, context, _, destination) =
            let
                val target = asTarget destination
                (* If the argument is a constant we can subtract the tag beforehand.
                   N.B. it is possible to have type-incorrect values in dead code. i.e. code that will
                   never be executed because of a run-time check.  *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
            in
                (arg1Code @
                    [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument aReg1, operand2=IntegerConstant constVal, ccRef = newCCRef()})],
                        RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordArith BuiltIns.ArithAdd, arg1=BICConstnt(value, _), arg2}, context, _, destination) =
            let
                val target = asTarget destination
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
            in
                (arg2Code @
                    [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument aReg2, operand2=IntegerConstant constVal, ccRef = newCCRef()})],
                        RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordArith BuiltIns.ArithAdd, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg3 = newPReg()
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
            in
                (arg1Code @ arg2Code @
                    (* Subtract the tag bit from the second argument and do the addition.  No need for overflow check. *)
                    [BlockSimple(ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=RegisterArgument aReg2, operand2=IntegerConstant 1, ccRef=newCCRef()}),
                     BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument aReg1, operand2=RegisterArgument aReg3, ccRef=newCCRef()})],
                        RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordArith BuiltIns.ArithSub, arg1, arg2=BICConstnt(value, _)}, context, _, destination) =
            let
                val target = asTarget destination
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
            in
                (arg1Code @
                    [BlockSimple(ArithmeticFunction{oper=SUB, resultReg=target, operand1=RegisterArgument aReg1, operand2=IntegerConstant constVal, ccRef=newCCRef()})],
                        RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordArith BuiltIns.ArithSub, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg3 = newPReg()
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
            in
                (arg1Code @ arg2Code @
                    (* Do the subtraction and add in the tag bit.  This could be reordered if we have cascaded operations
                       since we don't need to check for overflow. *)
                    [BlockSimple(ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=RegisterArgument aReg1, operand2=RegisterArgument aReg2, ccRef=newCCRef()}),
                     BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument aReg3, operand2=IntegerConstant 1, ccRef=newCCRef()})], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordArith BuiltIns.ArithMult, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and arg1Untagged = newUReg()
                and arg2Untagged = newUReg() and resUntagged = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift one argument and subtract the tag from the other.  It's possible this could be reordered
                       if we have a value that is already untagged. *)
                    [BlockSimple(UntagValue{source=RegisterArgument aReg1, dest=arg1Untagged, isSigned=false}),
                     BlockSimple(ArithmeticFunction{oper=SUB, resultReg=arg2Untagged, operand1=RegisterArgument aReg2, operand2=IntegerConstant 1, ccRef=newCCRef()}),
                     BlockSimple(Multiplication{resultReg=resUntagged, operand1=RegisterArgument arg1Untagged, operand2=RegisterArgument arg2Untagged, ccRef=newCCRef()}),
                     BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument resUntagged, operand2=IntegerConstant 1, ccRef=newCCRef()})],
                        RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordArith BuiltIns.ArithDiv, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val arg1Untagged = newUReg() and arg2Untagged = newUReg()
                val quotient = newUReg() and remainder = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift both of the arguments to remove the tags.  We don't test for zero here - that's done explicitly. *)
                    [BlockSimple(UntagValue{source=RegisterArgument aReg1, dest=arg1Untagged, isSigned=false}),
                     BlockSimple(UntagValue{source=RegisterArgument aReg2, dest=arg2Untagged, isSigned=false}),
                     BlockSimple(Division { isSigned = false, dividend=arg1Untagged, divisor=RegisterArgument arg2Untagged,
                                quotient=quotient, remainder=remainder }),
                     BlockSimple(TagValue { source=quotient, dest=target })], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordArith BuiltIns.ArithMod, arg1, arg2}, context, _, destination) =
            let
                (* Identical to Quot except that the result is the remainder. *)
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val arg1Untagged = newUReg() and arg2Untagged = newUReg()
                val quotient = newUReg() and remainder = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift both of the arguments to remove the tags. *)
                    [BlockSimple(UntagValue{source=RegisterArgument aReg1, dest=arg1Untagged, isSigned=false}),
                     BlockSimple(UntagValue{source=RegisterArgument aReg2, dest=arg2Untagged, isSigned=false}),
                     BlockSimple(Division { isSigned = false, dividend=arg1Untagged, divisor=RegisterArgument arg2Untagged,
                                quotient=quotient, remainder=remainder }),
                     BlockSimple(TagValue { source=remainder, dest=target })], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordArith _, ...}, _, _, _) =
                raise InternalError "codeToICode: WordArith - unimplemented operation"

        |   codeToICode(BICBinary {oper=BuiltIns.WordLogical BuiltIns.LogicalOr, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Or-ing preserves the tag bit. *)
                    [BlockSimple(ArithmeticFunction{oper=OR, resultReg=target, operand1=RegisterArgument aReg1, operand2=RegisterArgument aReg2, ccRef=newCCRef()})], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordLogical BuiltIns.LogicalAnd, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Since they're both tagged the result will be tagged. *)
                    [BlockSimple(ArithmeticFunction{oper=AND, resultReg=target, operand1=RegisterArgument aReg1, operand2=RegisterArgument aReg2, ccRef=newCCRef()})], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordLogical BuiltIns.LogicalXor, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newPReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* We need to restore the tag bit after the operation. *)
                    [BlockSimple(ArithmeticFunction{oper=XOR, resultReg=aReg3, operand1=RegisterArgument aReg1, operand2=RegisterArgument aReg2, ccRef=newCCRef()}),
                     BlockSimple(ArithmeticFunction{oper=OR, resultReg=target, operand1=RegisterArgument aReg3, operand2=IntegerConstant 1, ccRef=newCCRef()})], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.WordShift shift, arg1, arg2}, context, _, destination) =
                (* N.B.  X86 shifts of greater than the word length mask the higher bits.  That isn't what ML wants
                   but that is dealt with at a higher level *)
            let
                open BuiltIns
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val aReg1Untagged = newUReg() and aReg2Untagged = newUReg() and resRegUntagged = newUReg()
                val shiftOp = case shift of ShiftLeft => SHL | ShiftRightLogical => SHR | ShiftRightArithmetic => SAR
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                 (* There are better ways of doing this but it will do for the moment.  We don't need to
                    remove the tag for right shifts provided we OR in a tag afterwards. *)
                 [BlockSimple(UntagValue{source=RegisterArgument aReg1, dest=aReg1Untagged, isSigned=shift=ShiftRightArithmetic}),
                  BlockSimple(UntagValue{source=RegisterArgument aReg2, dest=aReg2Untagged, isSigned=false}),
                  BlockSimple(ShiftOperation{ shift=shiftOp, resultReg=resRegUntagged, operand=RegisterArgument aReg1Untagged, shiftAmount=RegisterArgument aReg2Untagged, ccRef=newCCRef() }),
                  BlockSimple(TagValue{source=resRegUntagged, dest=target})], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.AllocateByteMemory, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val sizeReg = newPReg() and baseReg = newPReg()
                val sizeCode = codeToICodeTarget(arg1, context, false, sizeReg)
                val (flagsCode, flagUntag, flagArg) = codeAsUntaggedValue(arg2, false, context)
            in
                (sizeCode @ flagsCode @
                 [BlockSimple(AllocateMemoryVariable{size=sizeReg, dest=baseReg, saveRegs=[]})] @
                  flagUntag @
                  [BlockSimple(StoreArgument{ source=flagArg, base=baseReg, offset= ~1, index=NoMemIndex, kind=MoveByte}),
                  BlockSimple InitialisationComplete,
                  BlockSimple(LoadArgument{ source=RegisterArgument baseReg, dest=target, kind=MoveWord})], RegisterArgument target, false)
            end

        |   codeToICode(instr as BICBinary{oper=BuiltIns.LargeWordComparison _, ...}, context, isTail, destination) =
                codeAsConditional(instr, context, isTail, destination)

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordArith BuiltIns.ArithAdd, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newUReg()
                val argReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=MoveWord}),
                     BlockSimple(ArithmeticFunction{oper=ADD, resultReg=aReg3, operand1=RegisterArgument argReg, operand2=wordAt aReg2, ccRef=newCCRef()}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordArith BuiltIns.ArithSub, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newUReg()
                val argReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=MoveWord}),
                     BlockSimple(ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=RegisterArgument argReg, operand2=wordAt aReg2, ccRef=newCCRef()}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordArith BuiltIns.ArithMult, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and resValue = newUReg()
                val argReg1 = newUReg() and argReg2 = newUReg()
                (* Temporarily load both arguments into untagged regs.  It's simpler. *)
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg1, kind=MoveWord}),
                     BlockSimple(LoadArgument{source=wordAt aReg2, dest=argReg2, kind=MoveWord}),
                     BlockSimple(Multiplication{resultReg=resValue, operand1=RegisterArgument argReg1, operand2=RegisterArgument argReg2, ccRef=newCCRef()}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=resValue, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordArith BuiltIns.ArithDiv, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val quotient = newUReg() and remainder = newUReg()
                val dividendReg = newUReg() and divisorReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* We don't test for zero here - that's done explicitly. *)
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=dividendReg, kind=MoveWord}),
                     BlockSimple(LoadArgument{source=wordAt aReg2, dest=divisorReg, kind=MoveWord}),
                     BlockSimple(Division { isSigned = false, dividend=dividendReg, divisor=RegisterArgument divisorReg,
                                quotient=quotient, remainder=remainder }),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=quotient, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordArith BuiltIns.ArithMod, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val quotient = newUReg() and remainder = newUReg()
                val dividendReg = newUReg() and divisorReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* We don't test for zero here - that's done explicitly. *)
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=dividendReg, kind=MoveWord}),
                     BlockSimple(LoadArgument{source=wordAt aReg2, dest=divisorReg, kind=MoveWord}),
                     BlockSimple(Division { isSigned = false, dividend=dividendReg, divisor=RegisterArgument divisorReg,
                                quotient=quotient, remainder=remainder }),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=remainder, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordArith _, ...}, _, _, _) =
                raise InternalError "codeToICode: LargeWordArith - unimplemented operation"

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordLogical BuiltIns.LogicalOr, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newUReg()
                val argReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=MoveWord}),
                     BlockSimple(ArithmeticFunction{oper=OR, resultReg=aReg3, operand1=RegisterArgument argReg, operand2=wordAt aReg2, ccRef=newCCRef()}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordLogical BuiltIns.LogicalAnd, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newUReg()
                val argReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=MoveWord}),
                     BlockSimple(ArithmeticFunction{oper=AND, resultReg=aReg3, operand1=RegisterArgument argReg, operand2=wordAt aReg2, ccRef=newCCRef()}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordLogical BuiltIns.LogicalXor, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg() and aReg3 = newUReg()
                val argReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=MoveWord}),
                     BlockSimple(ArithmeticFunction{oper=XOR, resultReg=aReg3, operand1=RegisterArgument argReg, operand2=wordAt aReg2, ccRef=newCCRef()}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.LargeWordShift shift, arg1, arg2}, context, _, destination) =
                (* The shift is always a Word.word value i.e. tagged.  There is a check at the higher level
                   that the shift does not exceed 32/64 bits. *)
            let
                open BuiltIns
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val aReg2Untagged = newUReg() and aReg3 = newUReg()
                val shiftOp = case shift of ShiftLeft => SHL | ShiftRightLogical => SHR | ShiftRightArithmetic => SAR
                val argReg = newUReg()
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                 [BlockSimple(LoadArgument{source=wordAt aReg1, dest=argReg, kind=MoveWord}),
                  BlockSimple(UntagValue{source=RegisterArgument aReg2, dest=aReg2Untagged, isSigned=false}),
                  BlockSimple(ShiftOperation{ shift=shiftOp, resultReg=aReg3, operand=RegisterArgument argReg, shiftAmount=RegisterArgument aReg2Untagged, ccRef=newCCRef() }),
                  BlockSimple(BoxValue{boxKind=BoxLargeWord, source=aReg3, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICode(BICBinary {oper=BuiltIns.RealArith fpOper, arg1, arg2}, context, _, destination) =
            let
                val target = asTarget destination
                val aReg1 = newPReg() and aReg2 = newPReg()
                val fpRegSrc = newUReg() and fpRegDest = newUReg()
                val load = BlockSimple(LoadArgument{source=wordAt aReg1, dest=fpRegSrc, kind=MoveDouble})
                open BuiltIns
                val arith =
                    case fpMode of
                        FPModeX87 =>
                        let
                            val fpOp =
                                case fpOper of
                                    ArithAdd => FADD
                                |   ArithSub => FSUB
                                |   ArithMult => FMUL
                                |   ArithDiv => FDIV
                                |   _ => raise InternalError "codeToICode: RealArith - unimplemented operation"
                        in
                            BlockSimple(X87FPArith{ opc=fpOp, resultReg=fpRegDest, arg1=RegisterArgument fpRegSrc, arg2=wordAt aReg2})
                        end
                    |   FPModeSSE2 =>
                        let
                            val fpOp =
                                case fpOper of
                                    ArithAdd => SSE2Add
                                |   ArithSub => SSE2Sub
                                |   ArithMult => SSE2Mul
                                |   ArithDiv => SSE2Div
                                |   _ => raise InternalError "codeToICode: RealArith - unimplemented operation"
                        in
                            BlockSimple(SSE2FPArith{ opc=fpOp, resultReg=fpRegDest, arg1=RegisterArgument fpRegSrc, arg2=wordAt aReg2})
                        end
            in
                (codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    [load, arith, BlockSimple(BoxValue{boxKind=BoxFloat, source=fpRegDest, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICode(instr as BICBinary{oper=BuiltIns.RealComparison _, ...}, context, isTail, destination) =
                codeAsConditional(instr, context, isTail, destination)
        
        |   codeToICode(BICArbitrary {oper, shortCond, arg1, arg2, longCall}, context, _, destination) =
            let
                val startLong = newLabel() and resultLabel = newLabel()
                val target = asTarget destination
                val condResult = newPReg()
                (* Overflow check - if there's an overflow jump to the long precision case. *)
                fun jumpOnOverflow ccRef =
                let
                    val noOverFlow = newLabel()
                in
                    [BlockFlow(Conditional{ ccRef=ccRef, condition=JO, trueJump=startLong, falseJump=noOverFlow }),
                     BlockLabel noOverFlow]
                end
                val (longCode, _, _) = codeToICode(longCall, context, false, SpecificPReg condResult)
                     (* We could use a tail jump here if this is a tail. *)
           in
                (
                    (* Test the tag bits and skip to the long case if either is clear. *)
                    codeCondition(shortCond, context, false, startLong) @
                    (* Try evaluating as fixed precision and jump if we get an overflow. *)
                    codeFixedPrecisionArith(oper, arg1, arg2, context, condResult, jumpOnOverflow) @
                    (* If we haven't had an overflow jump to the result. *)
                    [BlockFlow(Unconditional resultLabel),
                     (* If we need to use the full long-precision call we come here. *)
                     BlockLabel startLong] @ longCode @
                    [BlockLabel resultLabel,
                     BlockSimple(LoadArgument{source=RegisterArgument condResult, dest=target, kind=MoveWord})],
                    RegisterArgument target, false)
            end

        |   codeToICode(BICAllocateWordMemory {numWords as BICConstnt(length, _), flags as BICConstnt(flagValue, _), initial}, context, _, destination) =
            (* Constant length and flags is used for ref.  We could handle other cases. *)
            if  isShort length andalso isShort flagValue andalso toShort length = 0w1
            then
            let
                val target = asTarget destination (* Force a different register. *)
                val vecLength = Word.toInt(toShort length)
                val flagByte = Word8.fromLargeWord(Word.toLargeWord(toShort flagValue))
                val memAddr = newPReg() and valueReg = newPReg()
                fun initialise n =
                    BlockSimple(StoreArgument{ source=RegisterArgument valueReg, offset=n*wordSize, base=memAddr, index=NoMemIndex, kind=MoveWord})
                val code =
                    codeToICodeTarget(initial, context, false, valueReg) @
                    [BlockSimple(AllocateMemoryOperation{size=vecLength, flags=flagByte, dest=memAddr, saveRegs=[]})] @
                    List.tabulate(vecLength, initialise) @
                    [BlockSimple InitialisationComplete,
                     BlockSimple(LoadArgument{source=RegisterArgument memAddr, dest=target, kind=MoveWord})]
            in
                (code, RegisterArgument target, false)
            end
            else (* If it's longer use the full run-time form. *)
                allocateMemoryVariable(numWords, flags, initial, context, destination)

        |   codeToICode(BICAllocateWordMemory {numWords, flags, initial}, context, _, destination) =
                allocateMemoryVariable(numWords, flags, initial, context, destination)

            (* Copy the source tuple into the container.  If the source is a BICTuple we simply copy
               the fields.  That saves creating the tuple at compile time. *)
        |   codeToICode(BICSetContainer{container, tuple=BICTuple cl, filter}, context, _, destination) =
            let
                val containerTarget = newPReg()
                val codeContainer = codeToICodeTarget(container, context, false, containerTarget)
                (* In theory it's possible that the tuple could contain fields that are not
                   used but nevertheless need to be evaluated for their side-effects.
                   Create all the fields and push to the stack. *)
                fun codeAll [] = ([], [])
                |   codeAll(arg::args) =
                    let
                        val (c, r, _) = codeToICode(arg, context, false, Allowed allowInMemMove)
                        val (regs, code) = codeAll args
                    in
                        (r::regs, c @ code)
                    end
                val (pregs, codeFields) = codeAll cl

                val filterLength = BoolVector.length filter

                fun copyContainer([], _, _) = []
                |   copyContainer(srcReg :: otherRegs, sourceWord, destWord) =
                    if sourceWord < filterLength andalso BoolVector.sub(filter, sourceWord)
                    then
                        BlockSimple(StoreArgument{source=srcReg, offset=destWord*wordSize, base=containerTarget, index=NoMemIndex, kind=MoveWord}) ::
                            copyContainer(otherRegs, sourceWord+1, destWord+1)

                    else copyContainer(otherRegs, sourceWord+1, destWord)
                    
                val code = codeContainer @ codeFields @ copyContainer(pregs, 0, 0)
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICSetContainer{container, tuple, filter}, context, _, destination) =
            let
                val containerTarget = newPReg() and tupleTarget = newPReg()
                val codeContainer = codeToICodeTarget(container, context, false, containerTarget)
                and codeTuple = codeToICodeTarget(tuple, context, false, tupleTarget)
                
                val filterLength = BoolVector.length filter

                fun copyContainer(sourceWord, destWord) =
                    if sourceWord = filterLength
                    then []
                    else if BoolVector.sub(filter, sourceWord)
                    then
                    let
                        val loadReg = newPReg()
                    in
                        BlockSimple(LoadArgument{source=wordOffsetAddress(sourceWord, tupleTarget), dest=loadReg, kind=MoveWord}) ::
                        BlockSimple(StoreArgument{source=RegisterArgument loadReg, offset=destWord*wordSize, base=containerTarget, index=NoMemIndex, kind=MoveWord}) ::
                            copyContainer(sourceWord+1, destWord+1)
                    end
                    else copyContainer(sourceWord+1, destWord)
                val code = codeContainer @ codeTuple @ copyContainer(0, 0)
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreMLWord _, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeAddress(address, false, context)
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument {source=MemoryLocation memLoc, dest=target, kind=MoveWord})], RegisterArgument target, false)
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreMLByte _, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeAddress(address, true, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveByte}),
                     BlockSimple(TagValue {source=untaggedResReg, dest=target})], RegisterArgument target, false)
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreC8, address}, context, _, destination) =
            let
                (* Load a byte from C memory.  This is almost exactly the same as LoadStoreMLByte except
                   that the base address is a LargeWord.word value. *)
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w1, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveByte}),
                     BlockSimple(TagValue {source=untaggedResReg, dest=target})], RegisterArgument target, false)
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreC16, address}, context, _, destination) =
            let
                (* Load a 16-bit value from C memory. *)
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w2, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=Move16Bit}),
                     BlockSimple(TagValue {source=untaggedResReg, dest=target})], RegisterArgument target, false)
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreC32, address}, context, _, destination) =
            let
                (* Load a 32-bit value from C memory.  If this is 64-bit mode we can tag it but
                   if this is 32-bit mode we need to box it. *)
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w4, context)
                val untaggedResReg = newUReg()
                val (boxTagCode, moveKind) =
                    if isX64
                    then (BlockSimple(TagValue {source=untaggedResReg, dest=target}), Move32Bit)
                    else (BlockSimple(BoxValue{boxKind=BoxLargeWord, source=untaggedResReg, dest=target, saveRegs=[]}), MoveWord)
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=moveKind}), boxTagCode], RegisterArgument target, false)
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreC64, address}, context, _, destination) =
            let
                (* Load a 64-bit value from C memory.  This is only allowed in 64-bit mode.  The result
                   is a boxed value. *)
                val _ = isX64 orelse raise InternalError "codeToICode: BICLoadOperation LoadStoreC64 in 32-bit"
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w8, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveWord}),
                     BlockSimple(BoxValue{boxKind=BoxLargeWord, source=untaggedResReg, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreCFloat, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w4, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveFloat}),
                     BlockSimple(BoxValue{boxKind=BoxFloat, source=untaggedResReg, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreCDouble, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeCAddress(address, 0w8, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveDouble}),
                     BlockSimple(BoxValue{boxKind=BoxFloat, source=untaggedResReg, dest=target, saveRegs=[]})], RegisterArgument target, false)
            end

        |   codeToICode(BICLoadOperation {kind=LoadStoreUntaggedUnsigned, address}, context, _, destination) =
            let
                val target = asTarget destination
                val (codeBaseIndex, codeUntag, memLoc) = codeAddress(address, false, context)
                val untaggedResReg = newUReg()
            in
                (codeBaseIndex @ codeUntag @
                    [BlockSimple(LoadArgument { source=MemoryLocation memLoc, dest=untaggedResReg, kind=MoveWord}),
                     BlockSimple(TagValue {source=untaggedResReg, dest=target})], RegisterArgument target, false)
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreMLWord _, address, value}, context, _, destination) =
            let
                val (sourceCode, source, _) = codeToICode(value, context, false, Allowed allowInMemMove)
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeAddress(address, false, context)
                val code =
                    codeBaseIndex @ sourceCode @ codeUntag @
                        [BlockSimple(StoreArgument {source=source, base=base, offset=offset, index=index, kind=MoveWord})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

         |   codeToICode(BICStoreOperation {kind=LoadStoreMLByte _, address, value=BICConstnt(constByte, _)}, context, _, destination) =
            let
                val valueToStore = if isShort constByte then toShort constByte else 0w0
                (* Storing a constant. *)
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeAddress(address, true, context)
                val code =
                    codeBaseIndex @ codeUntag @
                    [BlockSimple(StoreArgument {source=IntegerConstant(Word.toLargeInt valueToStore), base=base, offset=offset, index=index, kind=MoveByte})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreMLByte _, address, value}, context, _, destination) =
            let
                (* We have to untag the value to store. *)
                val valueReg = newPReg() and valueReg1 = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeAddress(address, true, context)
                val code =
                    codeBaseIndex @
                    codeToICodeTarget(value, context, false, valueReg) @
                    [BlockSimple(UntagValue{dest=valueReg1, source=RegisterArgument valueReg, isSigned=false })] @ codeUntag @
                    [BlockSimple(StoreArgument {source=RegisterArgument valueReg1, base=base, offset=offset, index=index, kind=MoveByte})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreC8, address, value}, context, _, destination) =
            let
                (* Store a byte to C memory.  Almost exactly the same as LoadStoreMLByte. *)
                val valueReg = newPReg() and valueReg1 = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeCAddress(address, 0w1, context)
                val code =
                    codeBaseIndex @
                    codeToICodeTarget(value, context, false, valueReg) @
                    [BlockSimple(UntagValue{dest=valueReg1, source=RegisterArgument valueReg, isSigned=false })] @ codeUntag @
                    [BlockSimple(StoreArgument {source=RegisterArgument valueReg1, base=base, offset=offset, index=index, kind=MoveByte})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreC16, address, value}, context, _, destination) =
            let
                (* Store a 16-bit value to C memory. *)
                val valueReg = newPReg() and valueReg1 = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeCAddress(address, 0w2, context)
                val code =
                    codeBaseIndex @
                    codeToICodeTarget(value, context, false, valueReg) @
                    [BlockSimple(UntagValue{dest=valueReg1, source=RegisterArgument valueReg, isSigned=false })] @ codeUntag @
                    [BlockSimple(StoreArgument {source=RegisterArgument valueReg1, base=base, offset=offset, index=index, kind=Move16Bit})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreC32, address, value}, context, _, destination) =
            let
                (* Store a 32-bit value.  If this is 64-bit mode we untag it but if this is 32-bit mode we unbox it. *)
                val valueReg = newPReg() and valueReg1 = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeCAddress(address, 0w4, context)
                val untagUnbox =
                    if isX64
                    then [BlockSimple(UntagValue{dest=valueReg1, source=RegisterArgument valueReg, isSigned=false })]
                    else [BlockSimple(LoadArgument{source=wordAt valueReg, dest=valueReg1, kind=MoveWord})]
                val code =
                    codeBaseIndex @
                    codeToICodeTarget(value, context, false, valueReg) @ untagUnbox @ codeUntag @
                    [BlockSimple(StoreArgument {source=RegisterArgument valueReg1, base=base, offset=offset, index=index, kind=Move32Bit})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreC64, address, value}, context, _, destination) =
            let
                (* Store a 64-bit value. *)
                val _ = isX64 orelse raise InternalError "codeToICode: BICStoreOperation LoadStoreC64 in 32-bit"
                val valueReg = newPReg() and valueReg1 = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeCAddress(address, 0w8, context)
                val code =
                    codeBaseIndex @
                    codeToICodeTarget(value, context, false, valueReg) @ codeUntag @
                    [BlockSimple(LoadArgument{source=wordAt valueReg, dest=valueReg1, kind=MoveWord}),
                     BlockSimple(StoreArgument {source=RegisterArgument valueReg1, base=base, offset=offset, index=index, kind=MoveWord})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreCFloat, address, value}, context, _, destination) =
            let
                val valueReg = newPReg() and floatReg = newUReg() and float2Reg = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeCAddress(address, 0w4, context)
                (* If we're using an SSE2 reg we have to convert it from double to single precision. *)
                val (storeReg, cvtCode) =
                    case fpMode of
                        FPModeSSE2 =>
                            (float2Reg,
                                [BlockSimple(SSE2FPArith{opc=SSE2DoubleToFloat, resultReg=float2Reg, arg1=RegisterArgument floatReg, arg2=RegisterArgument floatReg})])
                    |   FPModeX87 => (floatReg, [])
                val code =
                    codeBaseIndex @
                    codeToICodeTarget(value, context, false, valueReg) @ codeUntag @
                    BlockSimple(LoadArgument{source=wordAt valueReg, dest=floatReg, kind=MoveDouble}) :: cvtCode @
                    [BlockSimple(StoreArgument {source=RegisterArgument storeReg, base=base, offset=offset, index=index, kind=MoveFloat})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreCDouble, address, value}, context, _, destination) =
            let
                val valueReg = newPReg() and floatReg = newUReg()
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeCAddress(address, 0w8, context)
                val code =
                    codeBaseIndex @
                    codeToICodeTarget(value, context, false, valueReg) @ codeUntag @
                    [BlockSimple(LoadArgument{source=wordAt valueReg, dest=floatReg, kind=MoveDouble}),
                     BlockSimple(StoreArgument {source=RegisterArgument floatReg, base=base, offset=offset, index=index, kind=MoveDouble})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(BICStoreOperation {kind=LoadStoreUntaggedUnsigned, address, value}, context, _, destination) =
            let
                (* We have to untag the value to store. *)
                val (codeBaseIndex, codeUntag, {base, offset, index}) = codeAddress(address, false, context)
                (* See if it's a constant.  This is frequently used to set the last word of a string to zero. *)
                (* We have to be a bit more careful on the X86.  We use moves to store constants that
                   can include addresses.  To avoid problems we only use a move if the value is
                   zero or odd and so looks like a tagged value. *)
                val storeAble =
                    case value of
                        BICConstnt(value, _) =>
                            if not(isShort value)
                            then NONE
                            else
                            let
                                val ival = Word.toLargeIntX(toShort value)
                            in
                                if isX64
                                then if is32bit ival then SOME ival else NONE
                                else if ival = 0 orelse ival mod 2 = 1 then SOME ival else NONE
                            end
                    |   _ => NONE
                val (storeVal, valCode) =
                    case storeAble of
                        SOME value => (IntegerConstant value (* Leave untagged *), [])
                    |   NONE =>
                        let
                            val valueReg = newPReg() and valueReg1 = newUReg()
                        in
                            (RegisterArgument valueReg1,
                                codeToICodeTarget(value, context, false, valueReg) @
                                [BlockSimple(UntagValue{dest=valueReg1, source=RegisterArgument valueReg, isSigned=false })])
                        end
                val code =
                    codeBaseIndex @ valCode @ codeUntag @
                    [BlockSimple(StoreArgument {source=storeVal, base=base, offset=offset, index=index, kind=MoveWord})]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(instr as BICBlockOperation {kind=BlockOpEqualByte, ...}, context, isTail, destination) =
                codeAsConditional(instr, context, isTail, destination)

        |   codeToICode(BICBlockOperation {kind=BlockOpCompareByte, sourceLeft, destRight, length}, context, _, destination) =
            let
                (* TODO: If we have a short fixed length comparison we might well be better loading the value into
                   a register and comparing with memory. *)
                val target = asTarget destination
                val vec1Reg = newUReg() and vec2Reg = newUReg()
                val (leftCode, leftUntag, {base=leftBase, offset=leftOffset, index=leftIndex}) =
                    codeAddress(sourceLeft, true, context)
                val (rightCode, rightUntag, {base=rightBase, offset=rightOffset, index=rightIndex}) =
                    codeAddress(destRight, true, context)
                val ccRef = newCCRef()
                val labLess = newLabel() and labGreater = newLabel() and exitLab = newLabel()
                val labNotLess = newLabel() and labNotGreater = newLabel()
                
                val (lengthCode, lengthUntag, lengthArg) = codeAsUntaggedToReg(length, false (* unsigned *), context)

                val code =
                    leftCode @ rightCode @ lengthCode @
                    leftUntag @ [BlockSimple(LoadEffectiveAddress{base=SOME leftBase, offset=leftOffset, index=leftIndex, dest=vec1Reg})] @
                    rightUntag @ [BlockSimple(LoadEffectiveAddress{base=SOME rightBase, offset=rightOffset, index=rightIndex, dest=vec2Reg})] @
                    lengthUntag @
                    [BlockSimple(CompareByteVectors{ vec1Addr=vec1Reg, vec2Addr=vec2Reg, length=lengthArg, ccRef=ccRef }),
                     (* N.B. These are unsigned comparisons. *)
                     BlockFlow(Conditional{ ccRef=ccRef, condition=JB, trueJump=labLess, falseJump=labNotLess }),
                     BlockLabel labNotLess,
                     BlockFlow(Conditional{ ccRef=ccRef, condition=JA, trueJump=labGreater, falseJump=labNotGreater }),
                     BlockLabel labNotGreater,
                     BlockSimple(LoadArgument{ source=IntegerConstant(tag 0), dest=target, kind=MoveWord }),
                     BlockFlow(Unconditional exitLab),
                     BlockLabel labLess,
                     BlockSimple(LoadArgument{ source=IntegerConstant(tag ~1), dest=target, kind=MoveWord }),
                     BlockFlow(Unconditional exitLab),
                     BlockLabel labGreater,
                     BlockSimple(LoadArgument{ source=IntegerConstant(tag 1), dest=target, kind=MoveWord }),
                     BlockLabel exitLab]
            in
                (code, RegisterArgument target, false)
            end

        |   codeToICode(BICBlockOperation {kind=BlockOpMove {isByteMove}, sourceLeft, destRight, length}, context, _, destination) =
            let
                (* TODO: If we have a short fixed length move we might well be better loading the value into
                   a register and storing it. *)
                val lengthReg = newPReg()
                val vec1Reg = newUReg() and vec2Reg = newUReg() and lenReg = newUReg()
                val (leftCode, leftUntag, {base=leftBase, offset=leftOffset, index=leftIndex}) =
                    codeAddress(sourceLeft, isByteMove, context)
                val (rightCode, rightUntag, {base=rightBase, offset=rightOffset, index=rightIndex}) =
                    codeAddress(destRight, isByteMove, context)
                val code =
                    leftCode @ rightCode @ codeToICodeTarget(length, context, false, lengthReg) @
                    leftUntag @ [BlockSimple(LoadEffectiveAddress{base=SOME leftBase, offset=leftOffset, index=leftIndex, dest=vec1Reg})] @
                    rightUntag @ [BlockSimple(LoadEffectiveAddress{base=SOME rightBase, offset=rightOffset, index=rightIndex, dest=vec2Reg})] @
                    [BlockSimple(UntagValue{source=RegisterArgument lengthReg, dest=lenReg, isSigned=false}),
                     BlockSimple(BlockMove{ srcAddr=vec1Reg, destAddr=vec2Reg, length=lenReg, isByteMove=isByteMove })]
            in
                moveIfNotAllowed(destination, code, (* Unit result *) IntegerConstant(tag 0))
            end

        |   codeToICode(instr, context, isTail, destination) =
            let
                val (code, dest, haveExited) = codeToICodeRev(instr, context, isTail, destination, [])
            in
                (List.rev code, dest, haveExited)
            end
        
        and codeConditionRev(BICConstnt(value, _), _, jumpOn, jumpLabel, tailCode) =
            (* Constant - typically part of andalso/orelse.  Either an unconditional branch
               or an unconditional drop-through. *)
            if jumpOn = (toShort value <> 0w0)
            then BlockFlow(Unconditional jumpLabel) :: tailCode
            else tailCode

        |   codeConditionRev(BICTagTest{test, tag=tagValue, ...}, context, jumpOn, jumpLabel, tailCode) =
            (* Check the "tag" word of a union (datatype).  N.B.  Not the same as testing the
               tag bit of a word. *)
            let
                val ccRef = newCCRef()
                val (testCode, tagReg) = codeToPRegRev(test, context, tailCode)
                val noJumpLabel = newLabel()
            in
                BlockLabel noJumpLabel ::
                BlockFlow(Conditional{ccRef=ccRef,
                           condition=if jumpOn then JE else JNE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                BlockSimple(WordComparison{arg1=RegisterArgument tagReg, arg2=IntegerConstant(tag(Word.toLargeInt tagValue)), ccRef=ccRef}) ::
                    testCode
            end

        |   codeConditionRev(BICUnary{oper=BuiltIns.NotBoolean, arg1}, context, jumpOn, jumpLabel, tailCode) =
                (* If we have a "not" we can just invert the jump condition. *)
                codeConditionRev(arg1, context, not jumpOn, jumpLabel, tailCode)

        |   codeConditionRev(BICUnary{oper=BuiltIns.IsTaggedValue, arg1}, context, jumpOn, jumpLabel, tailCode) =
            let
                val ccRef = newCCRef()
                val (testCode, testReg) = codeToPRegRev(arg1, context, tailCode)
                (* Test the tag bit.  This sets the zero bit if the value is untagged. *)
                (* TODO: The X86 supports tests with a memory argument so we don't have to
                   load it into a register.  That's not currently supported by the rest of
                   the code-generator. *)
                val noJumpLabel = newLabel()
            in
                BlockLabel noJumpLabel ::
                BlockFlow(Conditional{ccRef=ccRef, condition=if jumpOn then JNE else JE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                    BlockSimple(TestTagBit{arg=RegisterArgument testReg, ccRef=ccRef}) :: testCode
            end

            (* Comparisons.  Because this is also used for pointer equality and even for exception matching it
               is perfectly possible that the argument could be an address. *)
        |   codeConditionRev(BICBinary{oper=BuiltIns.WordComparison{test, isSigned}, arg1, arg2=BICConstnt(arg2Value, _)},
                    context, jumpOn, jumpLabel, tailCode) =
            let
                val ccRef = newCCRef()
                val (testCode, testReg) = codeToPRegRev(arg1, context, tailCode)
                val arg2Arg = constantAsArgument arg2Value
                val noJumpLabel = newLabel()
            in
                BlockLabel noJumpLabel ::
                BlockFlow(Conditional{ccRef=ccRef,
                           condition=testAsBranch(test, isSigned, jumpOn), trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                BlockSimple(WordComparison{arg1=RegisterArgument testReg, arg2=arg2Arg, ccRef=ccRef}) :: testCode
            end

(*        |   codeCondition(BICBinary{oper=BuiltIns.WordComparison{test, isSigned}, arg1=BICConstnt(arg1Value, _), arg2},
                    context, jumpOn, jumpLabel) =
            let
                val testReg = newPReg() and ccRef = newCCRef()
                val testCode = codeToICodeTarget(arg2, context, false, testReg)
                val arg1Arg = constantAsArgument arg1Value
            in
                testCode @
                    [WordComparison{arg1=arg1Arg, arg2=testReg, ccRef=ccRef},
                     BlockFlow(Conditional{ccRef=ccRef,
                           condition=testAsBranch(test, isSigned, jumpOn), label=jumpLabel}]
            end*)

        |   codeConditionRev(BICBinary{oper=BuiltIns.WordComparison{test, isSigned}, arg1, arg2},
                    context, jumpOn, jumpLabel, tailCode) =
            let
                val ccRef = newCCRef()
                val (arg1Code, arg1Reg) = codeToPRegRev(arg1, context, tailCode)
                val (arg2Code, arg2Reg) = codeToPRegRev(arg2, context, arg1Code)
                val noJumpLabel = newLabel()
            in
                BlockLabel noJumpLabel ::
                BlockFlow(Conditional{ccRef=ccRef,
                           condition=testAsBranch(test, isSigned, jumpOn), trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                BlockSimple(WordComparison{arg1=RegisterArgument arg1Reg, arg2=RegisterArgument arg2Reg, ccRef=ccRef}) :: arg2Code
            end

        |   codeConditionRev(BICBinary{oper=BuiltIns.LargeWordComparison test, arg1, arg2},
                    context, jumpOn, jumpLabel, tailCode) =
            let
                val ccRef = newCCRef()
                val (arg1Code, arg1Reg) = codeToPRegRev(arg1, context, tailCode)
                val (arg2Code, arg2Reg) = codeToPRegRev(arg2, context, arg1Code)
                val argReg = newUReg()
                val noJumpLabel = newLabel()
            in
                BlockLabel noJumpLabel ::
                BlockFlow(Conditional{ccRef=ccRef,
                           condition=testAsBranch(test, false, jumpOn), trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                BlockSimple(WordComparison{arg1=RegisterArgument argReg, arg2=wordAt arg2Reg, ccRef=ccRef}) ::
                BlockSimple(LoadArgument{source=wordAt arg1Reg, dest=argReg, kind=MoveWord}) :: arg2Code
            end

           (* Floating point comparison.  This is complicated because we have different
              instruction sequences for SSE2 and X87.  We also have to get the handling
              of unordered (NaN) values right.  All the tests are treated as false
              if either argument is a NaN.  To combine that test with the other tests
              we sometimes have to reverse the comparison. *)

        |   codeConditionRev(BICBinary{oper=BuiltIns.RealComparison BuiltIns.TestEqual, arg1, arg2},
                    context, jumpOn, jumpLabel, tailCode) =
            let
                val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                val fpReg = newUReg()
                val testReg1 = newUReg() and testReg2 = newUReg() and testReg3 = newUReg()
                val (arg1Code, arg1Reg) = codeToPRegRev(arg1, context, tailCode)
                val (arg2Code, arg2Reg) = codeToPRegRev(arg2, context, arg1Code)
               (* If this is X87 we get the condition into RAX and test it there.  If
                   it is SSE2 we have to treat the unordered result (parity set) specially. *)
                val testCode =
                    BlockSimple(CompareFloatingPt{arg1=RegisterArgument fpReg, arg2=wordAt arg2Reg, ccRef=ccRef1}) ::
                    BlockSimple(LoadArgument{source=wordAt arg1Reg, dest=fpReg, kind=MoveDouble}) :: arg2Code
                val noJumpLabel = newLabel()
            in
                case (fpMode, jumpOn) of
                    (FPModeX87, _) =>
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef2, condition=if jumpOn then JE else JNE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                        BlockSimple(ArithmeticFunction{
                            oper=XOR, resultReg=testReg3, operand1=RegisterArgument testReg2, operand2=IntegerConstant 0x4000, ccRef=ccRef2 }) ::
                        BlockSimple(ArithmeticFunction{
                            oper=AND, resultReg=testReg2, operand1=RegisterArgument testReg1, operand2=IntegerConstant 0x4400, ccRef=newCCRef() }) ::
                        BlockSimple(X87FPGetCondition { ccRef=ccRef1, dest=testReg1 }) :: testCode
                |   (FPModeSSE2, true) =>
                    let
                        val noParityLabel = newLabel()
                    in
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef1, condition=JE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                        BlockLabel noParityLabel ::
                        BlockFlow(Conditional{ccRef=ccRef1, condition=JP, trueJump=noJumpLabel, falseJump=noParityLabel}) :: testCode
                    end
               |    (FPModeSSE2, false) =>
                    let
                        val noParityLabel = newLabel()
                    in
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef1, condition=JNE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                        BlockLabel noParityLabel ::
                        BlockFlow(Conditional{ccRef=ccRef1, condition=JP, trueJump=jumpLabel, falseJump=noParityLabel}) :: testCode
                    end
            end

        |   codeConditionRev(BICBinary{oper=BuiltIns.RealComparison BuiltIns.TestLess, arg1, arg2},
                    context, jumpOn, jumpLabel, tailCode) =
            let
                val arg1Reg = newPReg() and arg2Reg = newPReg()
                val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                val fpReg = newUReg()
                val testReg1 = newUReg() and testReg2 = newUReg()
                val (arg1Code, _, _) = codeToICodeRev(arg1, context, false, SpecificPReg arg1Reg, tailCode)
                val (arg2Code, _, _) = codeToICodeRev(arg2, context, false, SpecificPReg arg2Reg, arg1Code)
                val testCode =
                    BlockSimple(CompareFloatingPt{arg1=RegisterArgument fpReg, arg2=wordAt arg1Reg, ccRef=ccRef1}) :: (* Reverse *)
                    BlockSimple(LoadArgument{source=wordAt arg2Reg, dest=fpReg, kind=MoveDouble}) :: arg2Code
                val noJumpLabel = newLabel()
            in
                case fpMode of
                    FPModeX87 =>
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef2, condition=if jumpOn then JE else JNE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                        BlockSimple(ArithmeticFunction{
                            oper=AND, resultReg=testReg2, operand1=RegisterArgument testReg1, operand2=IntegerConstant 0x4500, ccRef=ccRef2 }) ::
                        BlockSimple(X87FPGetCondition { ccRef=ccRef1, dest=testReg1 }) :: testCode
                |   FPModeSSE2 =>
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef1, condition=if jumpOn then JA else JNA, trueJump=jumpLabel, falseJump=noJumpLabel}) :: testCode
            end

        |   codeConditionRev(BICBinary{oper=BuiltIns.RealComparison BuiltIns.TestGreater, arg1, arg2},
                    context, jumpOn, jumpLabel, tailCode) =
            let
                val arg1Reg = newPReg() and arg2Reg = newPReg()
                val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                val fpReg = newUReg()
                val testReg1 = newUReg() and testReg2 = newUReg()
                val (arg1Code, _, _) = codeToICodeRev(arg1, context, false, SpecificPReg arg1Reg, tailCode)
                val (arg2Code, _, _) = codeToICodeRev(arg2, context, false, SpecificPReg arg2Reg, arg1Code)
                val testCode =
                    BlockSimple(CompareFloatingPt{arg1=RegisterArgument fpReg, arg2=wordAt arg2Reg, ccRef=ccRef1}) :: (* Not reversed. *)
                    BlockSimple(LoadArgument{source=wordAt arg1Reg, dest=fpReg, kind=MoveDouble}) :: arg2Code
                val noJumpLabel = newLabel()
            in
                case fpMode of
                    FPModeX87 =>
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef2, condition=if jumpOn then JE else JNE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                        BlockSimple(ArithmeticFunction{
                            oper=AND, resultReg=testReg2, operand1=RegisterArgument testReg1, operand2=IntegerConstant 0x4500, ccRef=ccRef2 }) ::
                        BlockSimple(X87FPGetCondition { ccRef=ccRef1, dest=testReg1 }) :: testCode
                |   FPModeSSE2 =>
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef1, condition=if jumpOn then JA else JNA, trueJump=jumpLabel, falseJump=noJumpLabel}) :: testCode
            end

        |   codeConditionRev(BICBinary{oper=BuiltIns.RealComparison BuiltIns.TestLessEqual, arg1, arg2},
                    context, jumpOn, jumpLabel, tailCode) =
            let
                val arg1Reg = newPReg() and arg2Reg = newPReg()
                val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                val fpReg = newUReg()
                val testReg1 = newUReg() and testReg2 = newUReg()
                val (arg1Code, _, _) = codeToICodeRev(arg1, context, false, SpecificPReg arg1Reg, tailCode)
                val (arg2Code, _, _) = codeToICodeRev(arg2, context, false, SpecificPReg arg2Reg, arg1Code)
                val testCode =
                    BlockSimple(CompareFloatingPt{arg1=RegisterArgument fpReg, arg2=wordAt arg1Reg, ccRef=ccRef1}) :: (* Reverse *)
                    BlockSimple(LoadArgument{source=wordAt arg2Reg, dest=fpReg, kind=MoveDouble}) :: arg2Code
                val noJumpLabel = newLabel()
            in
                case fpMode of
                    FPModeX87 =>
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef2, condition=if jumpOn then JE else JNE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                        BlockSimple(ArithmeticFunction{
                            oper=AND, resultReg=testReg2, operand1=RegisterArgument testReg1, operand2=IntegerConstant 0x500, ccRef=ccRef2 }) ::
                        BlockSimple(X87FPGetCondition { ccRef=ccRef1, dest=testReg1 }) :: testCode
                |   FPModeSSE2 =>
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef1, condition=if jumpOn then JNB else JB, trueJump=jumpLabel, falseJump=noJumpLabel}) :: testCode
            end

        |   codeConditionRev(BICBinary{oper=BuiltIns.RealComparison BuiltIns.TestGreaterEqual, arg1, arg2},
                    context, jumpOn, jumpLabel, tailCode) =
            let
                val arg1Reg = newPReg() and arg2Reg = newPReg()
                val ccRef1 = newCCRef() and ccRef2 = newCCRef()
                val fpReg = newUReg()
                val testReg1 = newUReg() and testReg2 = newUReg()
                val (arg1Code, _, _) = codeToICodeRev(arg1, context, false, SpecificPReg arg1Reg, tailCode)
                val (arg2Code, _, _) = codeToICodeRev(arg2, context, false, SpecificPReg arg2Reg, arg1Code)
                val testCode =
                    BlockSimple(CompareFloatingPt{arg1=RegisterArgument fpReg, arg2=wordAt arg2Reg, ccRef=ccRef1}) :: (* Not reversed. *)
                    BlockSimple(LoadArgument{source=wordAt arg1Reg, dest=fpReg, kind=MoveDouble}) :: arg2Code
                val noJumpLabel = newLabel()
            in
                case fpMode of
                    FPModeX87 =>
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef2, condition=if jumpOn then JE else JNE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                        BlockSimple(ArithmeticFunction{
                            oper=AND, resultReg=testReg2, operand1=RegisterArgument testReg1, operand2=IntegerConstant 0x500, ccRef=ccRef2 }) ::
                        BlockSimple(X87FPGetCondition { ccRef=ccRef1, dest=testReg1 }) :: testCode
                |   FPModeSSE2 =>
                        BlockLabel noJumpLabel ::
                        BlockFlow(Conditional{ccRef=ccRef1, condition=if jumpOn then JNB else JB, trueJump=jumpLabel, falseJump=noJumpLabel}) :: testCode
            end

        |   codeConditionRev(
                BICBlockOperation{kind=BlockOpEqualByte, sourceLeft, destRight, length}, context, jumpOn, jumpLabel, tailCode) =
            let
                val vec1Reg = newUReg() and vec2Reg = newUReg()
                val ccRef = newCCRef()
                val (leftCode, leftUntag, {base=leftBase, offset=leftOffset, index=leftIndex}) =
                    codeAddressRev(sourceLeft, true, context, tailCode)
                val (rightCode, rightUntag, {base=rightBase, offset=rightOffset, index=rightIndex}) =
                    codeAddressRev(destRight, true, context, leftCode)
                val (lengthCode, lengthUntag, lengthArg) = codeAsUntaggedToRegRev(length, false (* unsigned *), context, rightCode)
                val noJumpLabel = newLabel()
            in
                BlockLabel noJumpLabel ::
                BlockFlow(Conditional{ccRef=ccRef, condition=if jumpOn then JE else JNE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                BlockSimple(CompareByteVectors{ vec1Addr=vec1Reg, vec2Addr=vec2Reg, length=lengthArg, ccRef=ccRef }) ::
                lengthUntag @ BlockSimple(LoadEffectiveAddress{base=SOME rightBase, offset=rightOffset, index=rightIndex, dest=vec2Reg}) ::
                rightUntag @ BlockSimple(LoadEffectiveAddress{base=SOME leftBase, offset=leftOffset, index=leftIndex, dest=vec1Reg}) ::
                leftUntag @ lengthCode
            end

        |   codeConditionRev(BICCond (testPart, thenPart, elsePart), context, jumpOn, jumpLabel, tailCode) =
            let
                val notTest = newLabel() and isThen = newLabel() and notThenElse = newLabel()
                (* Test the condition and jump to the else-part if this is false. *)
                val testTest = codeConditionRev(testPart, context, false, notTest, tailCode)
                (* Test the then-part and jump if the condition we want holds.
                   We don't go to the final label yet. *)
                val testThen = codeConditionRev(thenPart, context, jumpOn, isThen, testTest)
                (* Test the else-part and jump on the inverse of the condition.
                   The destination of this jump is going to be the drop-through
                   case. *)
                (* Branch round the else-part and put in a label for the start of the else *)
                val testElse = codeConditionRev(elsePart, context, not jumpOn, notThenElse,
                    BlockLabel notTest ::
                    BlockFlow(Unconditional notThenElse) :: testThen)
            in
             (* And now the labels for the condition where we don't want to branch and want to
                drop through. *)
                BlockLabel notThenElse ::
                    (* Add a label for the result of the then-part.  Because we branched
                       on the inverse of the test in the else-part we now have both the
                       conditions to take the branch.  Put in an unconditional branch
                       to the final label. *)
                BlockFlow(Unconditional jumpLabel) :: BlockLabel isThen :: testElse
            end

            (* General case.  Load the value into a register and compare it with 1 (true) *)
        |   codeConditionRev(condition, context, jumpOn, jumpLabel, tailCode) =
            let
                val ccRef = newCCRef()
                val (testCode, testReg) = codeToPRegRev(condition, context, tailCode)
                val noJumpLabel = newLabel()
            in
                BlockLabel noJumpLabel ::
                BlockFlow(Conditional{ccRef=ccRef,
                           condition=if jumpOn then JE else JNE, trueJump=jumpLabel, falseJump=noJumpLabel}) ::
                BlockSimple(WordComparison{arg1=RegisterArgument testReg, arg2=IntegerConstant(tag 1), ccRef=ccRef}) ::
                testCode
            end

        and codeCondition(condition, context, jumpOn, jumpLabel) =
            List.rev(codeConditionRev(condition, context, jumpOn, jumpLabel, []))

        (* Some operations that deliver boolean results are better coded as
           if condition then true else false *)
        and codeAsConditionalRev(instr, context, isTail, target, tailCode) =
            codeToICodeRev(
                BICCond(instr, BICConstnt(toMachineWord 1, []), BICConstnt(toMachineWord 0, [])), context, isTail, target, tailCode)

        and codeAsConditional(instr, context, isTail, target) =
            codeToICode(
                BICCond(instr, BICConstnt(toMachineWord 1, []), BICConstnt(toMachineWord 0, [])), context, isTail, target)

        (* The fixed precision functions are also used for arbitrary precision but instead of raising Overflow we
           need to jump to the code that handles the long format. *)
        and codeFixedPrecisionArith(BuiltIns.ArithAdd, arg1, BICConstnt(value, _), context, target, onOverflow) =
            let
                val ccRef = newCCRef()
                (* If the argument is a constant we can subtract the tag beforehand.
                   This should always be a tagged value if the type is correct.  However it's possible for it not to
                   be if we have an arbitrary precision value.  There will be a run-time check that the value is
                   short and so this code will never be executed.  It will generally be edited out by the higher
                   level be we can't rely on that.  Because it's never executed we can just put in zero. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
            in
                arg1Code @
                    [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument aReg1, operand2=IntegerConstant constVal, ccRef=ccRef})] @
                    onOverflow ccRef
            end

        |   codeFixedPrecisionArith(BuiltIns.ArithAdd, BICConstnt(value, _), arg2, context, target, onOverflow) =
            let
                val ccRef = newCCRef()
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
            in
                arg2Code @
                    [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument aReg2, operand2=IntegerConstant constVal, ccRef=ccRef})] @
                    onOverflow ccRef
            end

        |   codeFixedPrecisionArith(BuiltIns.ArithAdd, arg1, arg2, context, target, onOverflow) =
            let
                val aReg3 = newPReg() and ccRef = newCCRef()
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
            in
                arg1Code @ arg2Code @
                    (* Subtract the tag bit from the second argument, do the addition and check for overflow. *)
                    (* TODO: We should really do the detagging in the transform phase.  It can make a better choice of
                       the argument if one of the arguments is already untagged or if we have a constant argument. *)
                    [BlockSimple(ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=RegisterArgument aReg2, operand2=IntegerConstant 1, ccRef=newCCRef()}),
                     BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument aReg1, operand2=RegisterArgument aReg3, ccRef=ccRef})] @
                    onOverflow ccRef
            end

            (* Subtraction.  We can handle the special case of the second argument being a constant but not the first. *)
        |   codeFixedPrecisionArith(BuiltIns.ArithSub, arg1, BICConstnt(value, _), context, target, onOverflow) =
            let
                val ccRef = newCCRef()
                (* If the argument is a constant we can subtract the tag beforehand. Check for short - see comment above. *)
                val constVal =
                    if isShort value
                    then semitag(Word.toLargeIntX(toShort value))
                    else 0
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
            in
                arg1Code @
                    [BlockSimple(ArithmeticFunction{oper=SUB, resultReg=target, operand1=RegisterArgument aReg1, operand2=IntegerConstant constVal, ccRef=ccRef})] @
                    onOverflow ccRef
            end

        |   codeFixedPrecisionArith(BuiltIns.ArithSub, arg1, arg2, context, target, onOverflow) =
            let
                val aReg3 = newPReg()
                val ccRef = newCCRef()
                val (arg1Code, aReg1) = codeToPReg(arg1, context)
                val (arg2Code, aReg2) = codeToPReg(arg2, context)
            in
                arg1Code @ arg2Code @
                    (* Do the subtraction, test for overflow and afterwards add in the tag bit. *)
                    [BlockSimple(ArithmeticFunction{oper=SUB, resultReg=aReg3, operand1=RegisterArgument aReg1, operand2=RegisterArgument aReg2, ccRef=ccRef})] @
                    onOverflow ccRef @
                    [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument aReg3, operand2=IntegerConstant 1, ccRef=newCCRef()})]
            end

        |   codeFixedPrecisionArith(BuiltIns.ArithMult, arg1, arg2, context, target, onOverflow) =
            let
                val aReg1 = newPReg() and aReg2 = newPReg() and arg1Untagged = newUReg()
                and arg2Untagged = newUReg() and resUntagged = newUReg()
                val mulCC = newCCRef()
                (* This is almost the same as the word operation except we use a signed shift and check for overflow. *)
            in
                codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift one argument and subtract the tag from the other.  It's possible this could be reordered
                       if we have a value that is already untagged. *)
                    [BlockSimple(UntagValue{source=RegisterArgument aReg1, dest=arg1Untagged, isSigned=true (* Signed shift here. *)}),
                     BlockSimple(ArithmeticFunction{oper=SUB, resultReg=arg2Untagged, operand1=RegisterArgument aReg2, operand2=IntegerConstant 1, ccRef=newCCRef()}),
                     BlockSimple(Multiplication{resultReg=resUntagged, operand1=RegisterArgument arg1Untagged, operand2=RegisterArgument arg2Untagged, ccRef=mulCC} )] @
                     onOverflow mulCC @
                     [BlockSimple(ArithmeticFunction{oper=ADD, resultReg=target, operand1=RegisterArgument resUntagged, operand2=IntegerConstant 1, ccRef=newCCRef()})]
            end

        |   codeFixedPrecisionArith(BuiltIns.ArithQuot, arg1, arg2, context, target, _) =
            let
                val aReg1 = newPReg() and aReg2 = newPReg()
                val arg1Untagged = newUReg() and arg2Untagged = newUReg()
                val quotient = newUReg() and remainder = newUReg()
            in
                codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift both of the arguments to remove the tags.  We don't test for zero here - that's done explicitly. *)
                    [BlockSimple(UntagValue{source=RegisterArgument aReg1, dest=arg1Untagged, isSigned=true}),
                     BlockSimple(UntagValue{source=RegisterArgument aReg2, dest=arg2Untagged, isSigned=true}),
                     BlockSimple(Division { isSigned = true, dividend=arg1Untagged, divisor=RegisterArgument arg2Untagged,
                                quotient=quotient, remainder=remainder }),
                     BlockSimple(TagValue { source=quotient, dest=target })]
            end

        |   codeFixedPrecisionArith(BuiltIns.ArithRem, arg1, arg2, context, target, _) =
            let
                (* Identical to Quot except that the result is the remainder. *)
                val aReg1 = newPReg() and aReg2 = newPReg()
                val arg1Untagged = newUReg() and arg2Untagged = newUReg()
                val quotient = newUReg() and remainder = newUReg()
            in
                codeToICodeTarget(arg1, context, false, aReg1) @ codeToICodeTarget(arg2, context, false, aReg2) @
                    (* Shift both of the arguments to remove the tags. *)
                    [BlockSimple(UntagValue{source=RegisterArgument aReg1, dest=arg1Untagged, isSigned=true}),
                     BlockSimple(UntagValue{source=RegisterArgument aReg2, dest=arg2Untagged, isSigned=true}),
                     BlockSimple(Division { isSigned = true, dividend=arg1Untagged, divisor=RegisterArgument arg2Untagged,
                                quotient=quotient, remainder=remainder }),
                     BlockSimple(TagValue { source=remainder, dest=target })]
            end

        |   codeFixedPrecisionArith(_, _, _, _, _, _) =
                raise InternalError "codeToICode: FixedPrecisionArith - unimplemented operation"

        (* Code an address.  The index is optional. *)
        and codeAddressRev({base, index=SOME index, offset}, true (* byte move *), context, tailCode) =
            let
                (* Byte address with index.  The index needs to be untagged. *)
                val indexReg1 = newUReg()
                val (codeBase, baseReg) = codeToPRegRev(base, context, tailCode)
                val (codeIndex, indexReg) = codeToPRegRev(index, context, codeBase)
                val untagCode = [BlockSimple(UntagValue{dest=indexReg1, source=RegisterArgument indexReg, isSigned=false })]
                val memResult = {base=baseReg, offset=Word.toInt offset, index=MemIndex1 indexReg1}
            in
                (codeIndex, untagCode, memResult)
            end

        |   codeAddressRev({base, index=SOME index, offset}, false (* word move *), context, tailCode) =
            let
                (* Word address with index.  We can avoid untagging the index by adjusting the
                   multiplier and offset *) 
                val (codeBase, baseReg) = codeToPRegRev(base, context, tailCode)
                val (codeIndex, indexReg) = codeToPRegRev(index, context, codeBase)
                val memResult =
                    if isX64
                    then {base=baseReg, offset=Word.toInt offset-4, index=MemIndex4 indexReg}
                    else {base=baseReg, offset=Word.toInt offset-2, index=MemIndex2 indexReg}
            in
                (codeIndex, [], memResult)
            end

        |   codeAddressRev({base, index=NONE, offset}, _, context, tailCode) =
            let
                val (codeBase, baseReg) = codeToPRegRev(base, context, tailCode)
                val memResult = {offset=Word.toInt offset, base=baseReg, index=NoMemIndex}
            in
                (codeBase, [], memResult)
            end

        and codeAddress(addr, isByte, context) =
        let
            val (code, untag, res) = codeAddressRev(addr, isByte, context, [])
        in
            (List.rev code, untag, res)
        end

        (* C-memory operations are slightly different.  The base address is a LargeWord.word value.
           The index is a byte index so may have to be untagged. *)
        and codeCAddress({base, index=SOME index, offset}, 0w1, context) =
            let
                (* Byte address with index.  The index needs to be untagged. *)
                val untaggedBaseReg = newUReg() and indexReg1 = newUReg()
                val (codeBase, baseReg) = codeToPReg(base, context)
                and (codeIndex, indexReg) = codeToPReg(index, context)
                val untagCode =
                    [BlockSimple(LoadArgument{source=wordAt baseReg, dest=untaggedBaseReg, kind=MoveWord}),
                     BlockSimple(UntagValue{dest=indexReg1, source=RegisterArgument indexReg, isSigned=false })]
                val memResult = {base=untaggedBaseReg, offset=Word.toInt offset, index=MemIndex1 indexReg1}
            in
                (codeBase @ codeIndex, untagCode, memResult)
            end

        |   codeCAddress({base, index=SOME index, offset}, size, context) =
            let
                (* Non-byte address with index.  By using an appropriate multiplier we can avoid
                   having to untag the index. *)
                val untaggedBaseReg = newUReg()
                val (codeBase, baseReg) = codeToPReg(base, context)
                and (codeIndex, indexReg) = codeToPReg(index, context)
                val untagCode = [BlockSimple(LoadArgument{source=wordAt baseReg, dest=untaggedBaseReg, kind=MoveWord})]
                val memResult =
                    case size of
                        0w2 => {base=untaggedBaseReg, offset=Word.toInt offset-1, index=MemIndex1 indexReg}
                    |   0w4 => {base=untaggedBaseReg, offset=Word.toInt offset-2, index=MemIndex2 indexReg}
                    |   0w8 => {base=untaggedBaseReg, offset=Word.toInt offset-4, index=MemIndex4 indexReg}
                    |   _ => raise InternalError "codeCAddress: unknown size"
            in
                (codeBase @ codeIndex, untagCode, memResult)
            end

        |   codeCAddress({base, index=NONE, offset}, _, context) =
            let
                val untaggedBaseReg = newUReg()
                val (codeBase, baseReg) = codeToPReg(base, context)
                val untagCode = [BlockSimple(LoadArgument{source=wordAt baseReg, dest=untaggedBaseReg, kind=MoveWord})]
                val memResult = {offset=Word.toInt offset, base=untaggedBaseReg, index=NoMemIndex}
            in
                (codeBase, untagCode, memResult)
            end

        (* Return an untagged value.  If we have a constant just return it.  Otherwise
           return the code to evaluate the argument, the code to untag it and the
           reference to the untagged register. *)
        and codeAsUntaggedToRegRev(BICConstnt(value, _), isSigned, _, tailCode) =
            let
                (* Should always be short except for unreachable code. *)
                val untagReg = newUReg()
                val cval = if isShort value then toShort value else 0w0
                val cArg = IntegerConstant(if isSigned then Word.toLargeIntX cval else Word.toLargeInt cval) (* Don't tag *)
                val untag = [BlockSimple(LoadArgument{source=cArg, dest=untagReg, kind=MoveWord})]
            in
                (tailCode, untag, untagReg) (* Don't tag. *)
            end
        |   codeAsUntaggedToRegRev(arg, isSigned, context, tailCode) =
            let
                val untagReg = newUReg()
                val (code, srcReg) = codeToPRegRev(arg, context, tailCode)
                val untag = [BlockSimple(UntagValue{source=RegisterArgument srcReg, dest=untagReg, isSigned=isSigned})]
            in
                (code, untag, untagReg)
            end

        and codeAsUntaggedToReg(arg, isSigned, context) =
        let
            val (code, untag, untagReg) = codeAsUntaggedToRegRev(arg, isSigned, context, [])
        in
            (List.rev code, untag, untagReg)
        end

        (* Return the argument as an untagged value.  We separate evaluating the argument from
           untagging because we may have to evaluate other arguments and that could involve a
           function call and we can't save the value to the stack after we've untagged it.
           Currently this is only used for byte values but we may have to be careful if
           we use it for word values on the X86.  Moving an untagged value into a register
           might look like loading a constant address. *)
        and codeAsUntaggedValue(BICConstnt(value, _), isSigned, _) =
            let
                val cval = if isShort value then toShort value else 0w0
                val cArg = IntegerConstant(if isSigned then Word.toLargeIntX cval else Word.toLargeInt cval) (* Don't tag *)
            in
                ([], [], cArg)
            end
        |   codeAsUntaggedValue(arg, isSigned, context) =
            let
                val argReg = newPReg() and untagReg = newUReg()
                val code = codeToICodeTarget(arg, context, false, argReg)
                val untag = [BlockSimple(UntagValue{source=RegisterArgument argReg, dest=untagReg, isSigned=isSigned})]
            in
                (code, untag, RegisterArgument untagReg)
            end

        (* Allocate memory.  This is used both for true variable length cells and also
           for longer constant length cells. *)
        and allocateMemoryVariable(numWords, flags, initial, context, destination) =
        let
            val target = asTarget destination
            (* With the exception of flagReg all these registers are modified by the code.
               So, we have to copy the size value into a new register. *)
            val sizeReg = newPReg() and initReg = newPReg()
            val sizeReg2 = newPReg()
            val untagSizeReg = newUReg() and initAddrReg = newPReg() and allocReg = newPReg()
            val sizeCode = codeToICodeTarget(numWords, context, false, sizeReg)
            and (flagsCode, flagUntag, flagArg) = codeAsUntaggedValue(flags, false, context)
            (* We're better off deferring the initialiser if possible.  If the value is
               a constant we don't have to save it. *)
            val (initCode, initResult, _) = codeToICode(initial, context, false, Allowed allowDefer)
         in
            (sizeCode @ flagsCode @ initCode
              @
             [(* We need to copy the size here because AllocateMemoryVariable modifies the
                 size in order to store the length word.  This is unfortunate especially as
                 we're going to untag it anyway. *)
              BlockSimple(LoadArgument{source=RegisterArgument sizeReg, dest=sizeReg2, kind=MoveWord}),
              BlockSimple(AllocateMemoryVariable{size=sizeReg, dest=allocReg, saveRegs=[]})] @
              flagUntag @
              [BlockSimple(StoreArgument{ source=flagArg, base=allocReg, offset= ~1, index=NoMemIndex, kind=MoveByte}),
              (* We need to copy the address here because InitialiseMem modifies all its arguments. *)
              BlockSimple(LoadArgument{source=RegisterArgument allocReg, dest=initAddrReg, kind=MoveWord}),
              BlockSimple(UntagValue{source=RegisterArgument sizeReg2, dest=untagSizeReg, isSigned=false}),
              BlockSimple(LoadArgument{source=initResult, dest=initReg, kind=MoveWord}),
              BlockSimple(InitialiseMem{size=untagSizeReg, init=initReg, addr=initAddrReg}),
              BlockSimple InitialisationComplete,
              BlockSimple(LoadArgument{source=RegisterArgument allocReg, dest=target, kind=MoveWord})], RegisterArgument target, false)
        end

        (*Turn the codetree structure into icode. *)
        val bodyContext = {loopArgs=NONE, stackPtr=0}
        val (bodyCode, _, bodyExited) =
            codeToICodeRev(body, bodyContext, true, SpecificPReg resultTarget, [beginInstruction])
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
            
            |   splitCode(BlockBegin regArgs :: _, sinceLabel, flow) =
                    (* Final instruction.  Create the initial block and exit. *)
                    createEntry(0,
                        BeginFunction{regArgs=regArgs, stackArgs=stackArguments @ [returnAddressEntry]}::sinceLabel, flow)
            
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

        in
            val () = splitCode(icode, [], ExitCode)
            val resultVector = Array.vector resArray
        end
      
        open ICODETRANSFORM
        
        val pregProperties = Vector.fromList(List.rev(! pregPropList))
    in
        codeICodeFunctionToX86{blocks = resultVector, functionName = name, pregProps = pregProperties,
            argRegsUsed = argRegsUsed, hasFullClosure = not (null closure), currentStackArgs = currentStackArgs,
            debugSwitches = debugSwitches}
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
    
    structure Sharing =
    struct
        type backendIC = backendIC
        and  bicLoadForm = bicLoadForm
        and argumentType = argumentType
    end
    
end;
