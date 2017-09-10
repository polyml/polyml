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

functor X86ICodeToX86Code(


    structure X86CODE: X86CODESIG

    structure X86OPTIMISE:
    sig
        type operation
        type code
        type operations = operation list
        type address = Address.address

        val generateCode: {code: code, ops: operations, labelCount: int} -> address

        structure Sharing:
        sig
            type operation = operation
            type code = code
        end
    end

    structure DEBUG: DEBUGSIG
    
    structure ICODE: ICodeSig
    structure IDENTIFY: X86IDENTIFYREFSSIG
    structure CONFLICTSETS: X86GETCONFLICTSETSIG
    structure INTSET: INTSETSIG
    structure PRETTY: PRETTYSIG

    structure STRONGLY:
        sig
            val stronglyConnectedComponents: {nodeAddress: 'a -> int, arcs: 'a -> int list } -> 'a list -> 'a list list
        end
    
    sharing X86CODE.Sharing = ICODE.Sharing = X86OPTIMISE.Sharing = IDENTIFY.Sharing = CONFLICTSETS.Sharing = INTSET
): X86ICODEGENERATESIG =
struct
    open ICODE
    open IDENTIFY
    open CONFLICTSETS
    open X86CODE
    open INTSET

    open Address
    
    exception InternalError = Misc.InternalError

    (* Temporary datatype to bridge the gap between the concrete icode and the
       direct generation of the low-level assembly code. *)
    datatype llIcode =
        LLLoadArgument of { source: reg regOrMemoryArg, dest: reg, kind: moveKind }
    |   LLStoreArgument of { source: reg regOrMemoryArg, base: genReg, offset: int, index: indexType, kind: moveKind }
    |   LLExchangeRegisters of { regX: reg, regY: reg }
    |   LLFunctionCall of { callKind: callKinds}
    |   LLTailRecursiveCall of { callKind: callKinds }
    |   LLAllocateMemoryOperation of { size: int, flags: Word8.word, dest: reg, saveRegs: reg list }
    |   LLAllocateMemoryVariable of { size: reg, dest: reg, saveRegs: reg list }
    |   LLInitialiseMem of { size: reg, addr: reg, init: reg }
    |   LLInitialisationComplete
    |   LLJumpLoop of { checkInterrupt: reg list option }
    |   LLRaiseExceptionPacket
    |   LLIndexedCaseOperation of { testReg: reg, workReg: reg }
    |   LLLockMutable of { addr: reg }
    |   LLWordComparison of { arg1: reg regOrMemoryArg, arg2: reg regOrMemoryArg }
    |   LLPushExceptionHandler of { workReg: reg }
    |   LLPopExceptionHandler of { workReg: reg }
    |   LLBeginHandler of { packetReg: reg, workReg: reg }
    |   LLReturnResultFromFunction of { resultReg: reg, numStackArgs: int }
    |   LLArithmeticFunction of { oper: arithOp, resultReg: reg, operand1: reg regOrMemoryArg, operand2: reg regOrMemoryArg }
    |   LLTestTagBit of { arg: reg regOrMemoryArg }
    |   LLPushValue of { arg: reg regOrMemoryArg }
    |   LLResetStackPtr of { numWords: int, preserveCC: bool }
    |   LLTagValue of { source: reg, dest: reg }
    |   LLUntagValue of { source: reg regOrMemoryArg, dest: reg, isSigned: bool }
    |   LLLoadEffectiveAddress of { base: reg option, offset: int, index: indexType, dest: reg }
    |   LLShiftOperation of { shift: shiftType, resultReg: reg, operand: reg regOrMemoryArg, shiftAmount: reg regOrMemoryArg }
    |   LLMultiplication of { resultReg: reg, operand1: reg regOrMemoryArg, operand2: reg regOrMemoryArg }
    |   LLDivision of { isSigned: bool, dividend: reg, divisor: reg regOrMemoryArg, quotient: reg, remainder: reg }
    |   LLAtomicExchangeAndAdd of { base: reg, source: reg }
    |   LLCompareByteVectors of { vec1Addr: reg, vec2Addr: reg, length: reg }
    |   LLBlockMove of { srcAddr: reg, destAddr: reg, length: reg, isByteMove: bool }
    |   LLCompareFloatingPt of { arg1: reg regOrMemoryArg, arg2: reg regOrMemoryArg }
    |   LLX87FPGetCondition of { dest: reg }
    |   LLX87FPArith of { opc: fpOps, resultReg: reg, arg1: reg regOrMemoryArg, arg2: reg regOrMemoryArg }
    |   LLX87FPUnaryOps of { fpOp: fpUnaryOps, dest: reg, source: reg regOrMemoryArg }
    |   LLFloatFixedInt of { dest: reg, source: reg regOrMemoryArg }
    |   LLSSE2FPArith of { opc: sse2Operations, resultReg: reg, arg1: reg regOrMemoryArg, arg2: reg regOrMemoryArg }
    
    and llBasicBlock = LLBasicBlock of { block: llIcode list, flow: ICODE.controlFlow }
    
    fun asGenReg(GenReg r) = r
    |   asGenReg _ = raise InternalError "asGenReg"
    
    (* tag a short constant *)
    fun tag c = 2 * c + 1

    val raxAsArg = (GenReg eax)
    and rcxAsArg = (GenReg ecx)
    
    val generalRegisters =
        List.map GenReg
        (if isX64
        then [r14, r13, r12, r11, r10, r9, r8, edi, esi, edx, ecx, ebx, eax]
        else [edi, esi, edx, ecx, ebx, eax])
    
    val floatingPtRegisters =
        case fpMode of
            FPModeSSE2 => List.map XMMReg [xmm6, xmm5, xmm4, xmm3, xmm2, xmm1, xmm0]
        |   FPModeX87 => List.map FPReg [fp0, fp1, fp2, fp3, fp4, fp5, fp6, fp7]

    exception RegisterOverflow of intSet
    
    fun icodeToX86Code{blocks, functionName, regStates, regProps, stackRequired, argRegsUsed, hasFullClosure, debugSwitches} =
    let
        (* Generally we have an offset in words and no index register. *)
        fun wordOffsetAddress(offset, baseReg) =
            MemoryArg{offset=offset*wordSize, base=baseReg, index=NoIndex}
        
        fun argAsGenReg(RegisterArg(GenReg r)) = r
        |   argAsGenReg _ = raise InternalError "argAsGenReg"

        fun sourceAsGenRegOrMem(RegisterArg(GenReg r)) = RegisterArg r
        |   sourceAsGenRegOrMem(MemoryArg{offset, base=baseReg, index}) =
                MemoryArg{base=baseReg, offset=offset, index=index}
        |   sourceAsGenRegOrMem(NonAddressConstArg v) = NonAddressConstArg v
        |   sourceAsGenRegOrMem(AddressConstArg v) = AddressConstArg v
        |   sourceAsGenRegOrMem _ = raise InternalError "sourceAsGenRegOrMem"

        and sourceAsXMMRegOrMem(RegisterArg(XMMReg r)) = RegisterArg r
        |   sourceAsXMMRegOrMem(MemoryArg{offset, base=baseReg, index}) =
                MemoryArg{base=baseReg, offset=offset, index=index}
        |   sourceAsXMMRegOrMem(NonAddressConstArg v) = NonAddressConstArg v
        |   sourceAsXMMRegOrMem(AddressConstArg v) = AddressConstArg v
        |   sourceAsXMMRegOrMem _ = raise InternalError "sourceAsGenRegOrMem"
        
        (* Moves and loads. *)
        fun llLoadArgument{ source, dest=GenReg destReg, kind=MoveWord} =
                MoveToRegister { source=sourceAsGenRegOrMem source, output=destReg }

        |   llLoadArgument{ source=MemoryArg mLoc, dest=GenReg destReg, kind=MoveByte} = (* Load from memory. *)
                LoadNonWord{size=Size8Bit, source=mLoc, output=destReg}

        |   llLoadArgument{ source=MemoryArg mLoc, dest=GenReg destReg, kind=Move16Bit} = (* Load from memory. *)
                LoadNonWord{size=Size16Bit, source=mLoc, output=destReg}
                
        |   llLoadArgument{ source=MemoryArg mLoc, dest=GenReg destReg, kind=Move32Bit} = (* Load from memory. *)
                LoadNonWord{size=Size32Bit, source=mLoc, output=destReg}

                (* Load a floating point value. *)
        |   llLoadArgument{source=MemoryArg{offset, base=baseReg, index}, dest=FPReg fpReg, kind=MoveDouble} =
            let
                val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: Load FPReg <> fp0"
            in
                FPLoadFromMemory{ address={base=baseReg, offset=offset, index=index}, precision=DoublePrecision }
            end

                (* Load or move from an XMM reg. *)
        |   llLoadArgument{source, dest=XMMReg xmmRegReg, kind=MoveDouble} =
                XMMArith { opc= SSE2Move, source=sourceAsXMMRegOrMem source, output=xmmRegReg }

                (* Load a floating point value. *)
        |   llLoadArgument{source=MemoryArg{offset, base=baseReg, index}, dest=FPReg fpReg, kind=MoveFloat} =
            let
                val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: Load FPReg <> fp0"
            in
                FPLoadFromMemory{ address={ base=baseReg, offset=offset, index=index }, precision=SinglePrecision }
            end

                (* Load or move from an XMM reg. *)
        |   llLoadArgument{source, dest=XMMReg xmmRegReg, kind=MoveFloat} =
                XMMArith { opc= SSE2MoveSingle, source=sourceAsXMMRegOrMem source, output=xmmRegReg }

            (* Any other combinations are not allowed. *)
        |   llLoadArgument _ = raise InternalError "codeGenICode: LoadArgument"

                (* Store to memory *)
        fun llStoreArgument{ source=RegisterArg(GenReg sourceReg), base, offset, index, kind=MoveWord} =
                StoreRegToMemory{toStore=sourceReg, address={base=base, offset=offset, index=index}}

        |   llStoreArgument{ source=RegisterArg(GenReg sourceReg), base, offset, index, kind=MoveByte} =
                StoreNonWord{size=Size8Bit, toStore=sourceReg, address={base=base, offset=offset, index=index}} 

        |   llStoreArgument{ source=RegisterArg(GenReg sourceReg), base, offset, index, kind=Move16Bit} =
                StoreNonWord{size=Size16Bit, toStore=sourceReg, address={base=base, offset=offset, index=index}}

        |   llStoreArgument{ source=RegisterArg(GenReg sourceReg), base, offset, index, kind=Move32Bit} =
                StoreNonWord{size=Size32Bit, toStore=sourceReg, address={base=base, offset=offset, index=index}}

                (* Store a short constant to memory *)
        |   llStoreArgument{ source=NonAddressConstArg srcValue, base, offset, index, kind=MoveWord} =
                StoreConstToMemory{toStore=srcValue, address={base=base, offset=offset, index=index}}

        |   llStoreArgument{ source=NonAddressConstArg srcValue, base, offset, index, kind=MoveByte} =
                StoreNonWordConst{size=Size8Bit, toStore=srcValue, address={base=base, offset=offset, index=index}}

                (* Store a long constant to memory *)
        |   llStoreArgument{ source=AddressConstArg srcValue, base, offset, index, kind=MoveWord} =
                StoreLongConstToMemory{toStore=srcValue, address={base=base, offset=offset, index=index}}

                (* Store a floating point value. *)
        |   llStoreArgument{source=RegisterArg(FPReg fpReg), offset, base=baseReg, index, kind=MoveDouble} =
            let
                val _ = fpReg = fp0 orelse raise InternalError "llStoreArgument: Store FPReg <> fp0"
            in
                 FPStoreToMemory{ address={ base=baseReg, offset=offset, index=index}, precision=DoublePrecision, andPop=true }
            end

        |   llStoreArgument{source=RegisterArg(XMMReg xmmRegReg), offset, base=baseReg, index, kind=MoveDouble} =
                 XMMStoreToMemory { toStore=xmmRegReg, address={base=baseReg, offset=offset, index=index}, precision=DoublePrecision }

                (* Store a floating point value. *)
        |   llStoreArgument{source=RegisterArg(FPReg fpReg), offset, base=baseReg, index, kind=MoveFloat} =
            let
                val _ = fpReg = fp0 orelse raise InternalError "llStoreArgument: Store FPReg <> fp0"
            in
                 FPStoreToMemory{address={ base=baseReg, offset=offset, index=index}, precision=SinglePrecision, andPop=true }
            end

        |   llStoreArgument{source=RegisterArg(XMMReg xmmRegReg), offset, base=baseReg, index, kind=MoveFloat} =
                 XMMStoreToMemory { toStore=xmmRegReg, address={base=baseReg, offset=offset, index=index}, precision=SinglePrecision }

        |   llStoreArgument _ = raise InternalError "llStoreArgument: StoreArgument"

        fun llPushValue{ arg = RegisterArg reg, ... } = PushToStack(RegisterArg(asGenReg reg))
        |   llPushValue{ arg = NonAddressConstArg v, ... } = PushToStack(NonAddressConstArg v)
        |   llPushValue{ arg = AddressConstArg v, ... } = PushToStack(AddressConstArg v)
        |   llPushValue{ arg = MemoryArg {offset, base, index}, ... } =
                PushToStack(MemoryArg{base=base, offset=offset, index=index})

        val maxPRegs = Vector.length regStates
        and numBlocks = Vector.length blocks

        (* Allocated registers.  This is set to the real register that is used
           for a specific pseudo-register.  Once a register is allocated that
           is fixed. *)
        val allocatedRegs = Array.array(maxPRegs, NONE: reg option)
        
        (* Hint values.  The idea of hints is that by using a hinted register
           we may avoid an unnecessary move instruction.  realHints is set when
           a pseudo-register is going to be loaded from a specific register
           e.g. a register argument, or moved into one e.g. ecx for a shift.
           friends is set to the other pseudo-registers that may be associated
           with the pseudo-register.  E.g. the argument and destination of
           an arithmetic operation where choosing the same register for
           each may avoid a move. *)
        val realHints = Array.array(maxPRegs, NONE: reg option)

        (* Sources and destinations.  These indicate the registers that are
           the sources and destinations of the indexing register and are used
           as hints.  If a register has been allocated for a source or destination
           we may be able to reuse it.  *)
        val sourceRegs = Array.array(maxPRegs, []: int list)
        and destinationRegs = Array.array(maxPRegs, []: int list)
        
        (* Turn cached stack locations into register arguments. *)
        fun decache(StackLocation{cache=SOME r, ...}) = RegisterArgument r
        |   decache arg = arg

        local
            fun addRealHint(r, reg) =
                case Array.sub(realHints, r) of
                    NONE => Array.update(realHints, r, SOME reg)
                |   SOME _ => ()
        
            fun addSourceAndDestinationHint{src, dst} =
            let
                val {conflicts, ...} = Vector.sub(regStates, src)
            in
                (* If they conflict we can't add them. *)
                if member(dst, conflicts)
                then ()
                else
                let
                    val currentDests = Array.sub(destinationRegs, src)
                    val currentSources = Array.sub(sourceRegs, dst)
                in
                    if List.exists(fn i => i=dst) currentDests
                    then ()
                    else Array.update(destinationRegs, src, dst :: currentDests);
                    if List.exists(fn i => i=src) currentSources
                    then ()
                    else Array.update(sourceRegs, dst, src :: currentSources)
                end
            end
        
        in
            (* Add the hints to steer the register allocation.  The idea is to avoid moves between
               registers by getting values into the appropriate register in advance. *)
            fun addHints{instr=LoadArgument{source, dest=PReg dreg, ...}, ...} =
                (
                    case decache source of
                        RegisterArgument(PReg sreg) => addSourceAndDestinationHint {src=sreg, dst=dreg}
                    |   _ => ()
                )

            |   addHints{instr=StoreArgument{ source, kind, ... }, ...} =
                (
                    case (decache source, kind, isX64) of
                        (* Special case for byte register on X86/32 *)
                        (RegisterArgument(PReg sReg), MoveByte, false) => addRealHint(sReg, GenReg ecx)
                    |   _ => ()
                )
        
            |   addHints{instr=BeginFunction{regArgs, ...}, ...} =
                    List.app (fn (PReg pr, reg) => addRealHint(pr, reg)) regArgs

            |   addHints{instr=TailRecursiveCall{regArgs, ...}, ...} =
                    List.app (fn (arg, reg) => case decache arg of RegisterArgument(PReg pr) => addRealHint(pr, reg) | _ => ()) regArgs
        
            |   addHints{instr=FunctionCall{regArgs, ...}, ...} =
                    List.app (fn (arg, reg) => case decache arg of RegisterArgument(PReg pr) => addRealHint(pr, reg) | _ => ()) regArgs
        
            |   addHints{instr=InitialiseMem{size=PReg sReg, addr=PReg aReg, init=PReg iReg}, ...} =
                    (addRealHint(aReg, GenReg edi); addRealHint(iReg, GenReg eax); addRealHint(sReg, GenReg ecx))
        
            |   addHints{instr=RaiseExceptionPacket{ packetReg=PReg preg }, ...} = addRealHint(preg, GenReg eax)
        
            |   addHints{instr=BeginHandler{packetReg=PReg pReg, workReg=_}, ...} =
                    (* The exception packet is in rax. *) addRealHint(pReg, GenReg eax)
        
            |   addHints{instr=ReturnResultFromFunction { resultReg=PReg resReg, ... }, ...} = addRealHint(resReg, GenReg eax)
        
            |   addHints{instr=ArithmeticFunction{oper=SUB, resultReg=PReg resReg, operand1=RegisterArgument(PReg op1Reg), ...}, ...} =
                    (* Can only be one way round. *)
                    addSourceAndDestinationHint {dst=resReg, src=op1Reg}

            |   addHints{instr=ArithmeticFunction{resultReg=PReg resReg, operand1=RegisterArgument(PReg op1Reg),
                                                operand2=RegisterArgument(PReg op2Reg), ...}, ...} =
                    (addSourceAndDestinationHint {dst=resReg, src=op1Reg}; addSourceAndDestinationHint {dst=resReg, src=op2Reg})
        
            |   addHints{instr=ArithmeticFunction{resultReg=PReg resReg, operand2=RegisterArgument(PReg op2Reg), ...}, ...} =
                    addSourceAndDestinationHint{dst=resReg, src=op2Reg}
        
            |   addHints{instr=CopyToCache{source=PReg sreg, dest=PReg dreg}, ...} =
                    addSourceAndDestinationHint {src=sreg, dst=dreg}
        
            |   addHints{instr=UntagValue{source=RegisterArgument(PReg sReg), dest=PReg dReg, ...}, ...} =
                    addSourceAndDestinationHint{src=sReg, dst=dReg}

            |   addHints{instr=ShiftOperation{resultReg=PReg resReg, operand=RegisterArgument(PReg operReg), shiftAmount=IntegerConstant _, ...}, ...} =
                    addSourceAndDestinationHint{dst=resReg, src=operReg}
        
            |   addHints{instr=ShiftOperation{resultReg=PReg resReg, operand=RegisterArgument(PReg operReg),
                                            shiftAmount=RegisterArgument(PReg shiftReg), ...}, ...} =
                    (addSourceAndDestinationHint{dst=resReg, src=operReg}; addRealHint(shiftReg, GenReg ecx))

            |   addHints{instr=Multiplication{resultReg=PReg resReg, operand1=RegisterArgument(PReg op1Reg),
                                   operand2=RegisterArgument(PReg op2Reg), ...}, ...} =
                    (addSourceAndDestinationHint{dst=resReg, src=op1Reg}; addSourceAndDestinationHint{dst=resReg, src=op2Reg})
            
            |   addHints{instr=Division{dividend=PReg regDivid, quotient=PReg regQuot, remainder=PReg regRem, ...}, ...} =
                    (addRealHint(regDivid, GenReg eax); addRealHint(regQuot, GenReg eax); addRealHint(regRem, GenReg edx))

            |   addHints{instr=CompareByteVectors{vec1Addr=PReg v1Reg, vec2Addr=PReg v2Reg, length=PReg lReg, ...}, ...} =
                    (addRealHint(v1Reg, GenReg esi); addRealHint(v2Reg, GenReg edi); addRealHint(lReg, GenReg ecx))

            |   addHints{instr=BlockMove{srcAddr=PReg sReg, destAddr=PReg dReg, length=PReg lReg, ...}, ...} =
                    (addRealHint(sReg, GenReg esi); addRealHint(dReg, GenReg edi); addRealHint(lReg, GenReg ecx))
        
            |   addHints{instr=X87FPGetCondition{dest=PReg dReg, ...}, ...} = addRealHint(dReg, GenReg eax)
        
            |   addHints{instr=X87FPArith{resultReg=PReg resReg, arg1=RegisterArgument(PReg op1Reg), ...}, ...} =
                    addSourceAndDestinationHint{dst=resReg, src=op1Reg}
        
            |   addHints{instr=X87FPUnaryOps{dest=PReg resReg, source=RegisterArgument(PReg op1Reg), ...}, ...} =
                    addSourceAndDestinationHint{dst=resReg, src=op1Reg}
        
            |   addHints{instr=SSE2FPArith{resultReg=PReg resReg, arg1=RegisterArgument(PReg op1Reg), ...}, ...} =
                    addSourceAndDestinationHint{dst=resReg, src=op1Reg}

            |   addHints _ = ()
        
        end

        (* Find a real register for a preg.
           1.  If a register is already allocated use that.
           2.  Try the "preferred" register if one has been given.
           3.  Try the realHints value if there is one.
           4.  See if there is a "friend" that has an appropriate register
           5.  Look at all the registers and find one. *)
        fun findRegister(r, pref, regSet) =
            case Array.sub(allocatedRegs, r) of
                SOME reg => reg
            |   NONE =>
                let
                    val {conflicts, realConflicts, ...} = Vector.sub(regStates, r)
                    (* Find the registers we've already allocated that may conflict. *)
                    val conflictingRegs =
                        List.mapPartial(fn i => Array.sub(allocatedRegs, i)) (setToList conflicts) @
                            realConflicts

                    fun isFree aReg = not (List.exists(fn i => i=aReg) conflictingRegs)

                    fun tryAReg NONE = NONE
                    |   tryAReg (somePref as SOME prefReg) =
                            if isFree prefReg
                            then (Array.update(allocatedRegs, r, somePref); somePref)
                            else NONE

                    fun findAReg [] = raise RegisterOverflow conflicts
                    |   findAReg (reg::regs) =
                            if isFree reg then (Array.update(allocatedRegs, r, SOME reg); reg)
                            else findAReg regs


                    (* Search the sources and destinations to see if a register has
                       already been allocated or there is a hint. *)
                    fun findAFriend([], [], _) = NONE

                    |   findAFriend(aDest :: otherDests, sources, alreadySeen) =
                        let
                            val possReg =
                                case Array.sub(allocatedRegs, aDest) of
                                    v as SOME _ => tryAReg v
                                |   NONE => tryAReg(Array.sub(realHints, aDest))
                        in
                            case possReg of
                                reg as SOME _ => reg
                            |   NONE =>
                                let
                                    (* Add the destinations of the destinations to the list
                                       if they don't conflict and haven't been seen. *)
                                    fun newFriend f =
                                        not(List.exists (fn n => n=f) alreadySeen) andalso not(member(f, conflicts))
                                    val fOfF = List.filter newFriend (Array.sub(destinationRegs, aDest))
                                in
                                    findAFriend(otherDests @ fOfF, sources, aDest :: alreadySeen)
                                end
                        end

                    |   findAFriend([], aSrc :: otherSrcs, alreadySeen) =
                        let
                            val possReg =
                                case Array.sub(allocatedRegs, aSrc) of
                                    v as SOME _ => tryAReg v
                                |   NONE => tryAReg(Array.sub(realHints, aSrc))
                        in
                            case possReg of
                                reg as SOME _ => reg
                            |   NONE =>
                                let
                                    (* Add the sources of the sources to the list
                                       if they don't conflict and haven't been seen. *)
                                    fun newFriend f =
                                        not(List.exists (fn n => n=f) alreadySeen) andalso not(member(f, conflicts))
                                    val fOfF = List.filter newFriend (Array.sub(sourceRegs, aSrc))
                                in
                                    findAFriend([], otherSrcs @ fOfF, aSrc :: alreadySeen)
                                end
                        end

                    (* See if there is a friend that has a register already or a
                       hint.  Friends are registers that don't conflict and can
                       possibly avoid an extra move. *) 
(*                    fun findAFriend([], _) = NONE
                    |   findAFriend(friend :: tail, old) =
                        let
                            val possReg =
                                case Array.sub(allocatedRegs, friend) of
                                    v as SOME _ => tryAReg v
                                |   NONE => tryAReg(Array.sub(realHints, friend))
                        in
                            case possReg of
                                reg as SOME _ => reg
                            |   NONE =>
                                let
                                    (* Add a friend of a friend to the list if we haven't already
                                       seen it and it doesn't conflict. *)
                                    fun newFriend f =
                                        not(List.exists (fn n => n=f) old) andalso
                                            not(List.exists (fn n => n=f) conflicts)
                                    val fOfF = List.filter newFriend (Array.sub(friends, friend))
                                in
                                    findAFriend(tail @ fOfF, friend :: old)
                                end
                        end*)
                in
                    case tryAReg pref of
                        SOME r => r
                    |   NONE =>
                        (
                            case tryAReg (Array.sub(realHints, r)) of
                                SOME r => r
                            |   NONE =>
                                (
                                    case findAFriend(Array.sub(destinationRegs, r), Array.sub(sourceRegs, r), []) of
                                        SOME r => r
                                        (* Look through the registers to find one that's free. *)
                                    |   NONE => findAReg regSet
                                )
                        )
                end

        fun findGeneralReg r = findRegister(r, NONE, generalRegisters)
        and findFloatReg r = findRegister(r, NONE, floatingPtRegisters)

        fun codeExtArgument(RegisterArgument(PReg r), regSet) = RegisterArg(findRegister(r, NONE, regSet))
        |   codeExtArgument(AddressConstant m, _) = AddressConstArg m
        |   codeExtArgument(IntegerConstant i, _) = NonAddressConstArg i
        |   codeExtArgument(MemoryLocation{base=PReg bReg, offset, index}, _) =
                MemoryArg{base=asGenReg(findGeneralReg bReg), offset=offset, index=codeExtIndex index}
        |   codeExtArgument(StackLocation{wordOffset, cache=NONE, ...}, _) =
                MemoryArg{base=esp, offset=wordOffset*wordSize, index=NoIndex}
        |   codeExtArgument(StackLocation{cache=SOME(PReg r), ...}, regSet) = RegisterArg(findRegister(r, NONE, regSet))
        
        and codeExtArgGeneral arg = codeExtArgument(arg, generalRegisters)
        and codeExtArgFloat arg = codeExtArgument(arg, floatingPtRegisters)

        and codeExtIndex NoMemIndex = NoIndex
        |   codeExtIndex(MemIndex1(PReg r)) = Index1(asGenReg(findGeneralReg r))
        |   codeExtIndex(MemIndex2(PReg r)) = Index2(asGenReg(findGeneralReg r))
        |   codeExtIndex(MemIndex4(PReg r)) = Index4(asGenReg(findGeneralReg r))
        |   codeExtIndex(MemIndex8(PReg r)) = Index8(asGenReg(findGeneralReg r))

        fun moveRR{src, dst} = LLLoadArgument{source=RegisterArg src, dest=dst, kind=MoveWord}

        fun moveIfNecessary{src, dst} =
            if src = dst then [] else [moveRR{src=src, dst=dst}]
       
        datatype llsource =
            StackSource of int
        |   OtherSource of reg regOrMemoryArg

        fun sourceToX86Code(OtherSource r) = r
        |   sourceToX86Code(StackSource wordOffset) = MemoryArg{base=esp, offset=wordOffset*wordSize, index=NoIndex}

        local
            fun indexRegister NoIndex = NONE
            |   indexRegister (Index1 r) = SOME r
            |   indexRegister (Index2 r) = SOME r
            |   indexRegister (Index4 r) = SOME r
            |   indexRegister (Index8 r) = SOME r
            (* The registers are numbered from 0.  Choose values that don't conflict with
               the stack addresses. *)
            fun regNo r = ~1 - nReg r
            type node = {src: llsource, dst: destinations }
            
            fun nodeAddress({dst=RegDest r, ...}: node) = regNo r
            |   nodeAddress({dst=StackDest a, ...}) = a
            
            fun arcs({src=StackSource wordOffset, ...}: node) = [wordOffset]
            |   arcs{src=OtherSource(RegisterArg r), ...} = [regNo r]
            |   arcs{src=OtherSource(MemoryArg{base, index, ...}), ...} =
                    (case indexRegister index of NONE => [regNo(GenReg base)] | SOME r => [regNo(GenReg base), regNo(GenReg r)])
            |   arcs _ = []
        in
            val stronglyConnected = STRONGLY.stronglyConnectedComponents { nodeAddress=nodeAddress, arcs=arcs }
        end
        
        (* This is a general function for moving values into registers or to the stack
           where it is possible that the source values might also be in use as destinations.
           The stack is used for destinations only for tail recursive calls. *)
        fun moveMultipleValues(moves, reservedRegs) =
        let
            fun getWorkRegister(reserved, rest) =
            let
                (* Find a free register.  We cannot use any reserved register, which
                   includes those we've loaded, or any source register that we
                   haven't yet used.  We start with all the registers, after filtering
                   those we can't use, and remove a source register from the list.  *)
                val filteredInUse =
                    List.filter (fn r => not(List.exists (fn i => i = r) reserved)) generalRegisters
                
                fun filterASet(_, []) = [] (* Stop if there's nothing there. *)
                |   filterASet([], regs) = regs
                |   filterASet({src=OtherSource(RegisterArg srcReg), ...} :: rest, regs) =
                        filterASet(rest, List.filter(fn r => r <> srcReg) regs)
                |   filterASet({src=OtherSource(MemoryArg _), ...} :: _, _) = raise InternalError "filterASet - Memory"
                |   filterASet(_ :: rest, regs) = filterASet(rest, regs)
                
                fun filterList(_, []) = []
                |   filterList([], regs) = regs
                |   filterList(set::rest, regs) = filterList(rest, filterASet(set, regs))
                
                (* So far it seems we always find a register this way if we need one.
                   Just in case, fall back to pushing a register, using it and popping it
                   again. *)
            in
                case filterList(rest, filteredInUse) of
                    workReg :: _ => SOME workReg
                |   [] => NONE
            end

            fun moveValues([], _) = [] (* We're done. *)

            |   moveValues(arguments, reservedRegs) =
                let
                    (* stronglyConnectedComponents does two things.  It detects loops where
                       it's not possible to move items without breaking the loop but more
                       importantly it orders the dependencies so that if there are no loops we
                       can load the source and store it in the destination knowing that
                       we won't overwrite anything we might later need. *)
                    
                    val ordered = stronglyConnected arguments
                    
                    fun moveEachValue ([], _) = []

                    |   moveEachValue ([{dst=RegDest reg, src as OtherSource(RegisterArg r)}] :: rest, reserved) =
                            (* Source and dest are both regs - only move if they're different. *)
                            if r = reg
                            then moveEachValue(rest, reg :: reserved)
                            else LLLoadArgument{source=sourceToX86Code src, dest=reg, kind=MoveWord} ::
                                    moveEachValue(rest, reg :: reserved)

                    |   moveEachValue ([{dst=RegDest reg, src}] :: rest, reserved) =
                            (* Load from store or a constant. *)
                            LLLoadArgument{source=sourceToX86Code src, dest=reg, kind=MoveWord} ::
                                moveEachValue(rest, reg :: reserved)

                    |   moveEachValue ([{dst=StackDest _, src=OtherSource(MemoryArg _ )}] :: _, _) =
                            raise InternalError "moveEachValue - MemoryArgument"

                    |   moveEachValue ([{dst=StackDest addr, src as StackSource wordOffset}] :: rest, reserved) =
                            (* Copy a stack location - needs a load and store unless the address is the same. *)
                            if addr = wordOffset
                            then moveEachValue(rest, reserved)
                            else
                            let
                                val loadAndStoreCode =
                                    case getWorkRegister(reserved, rest) of
                                        SOME workReg =>
                                            [LLLoadArgument{source=sourceToX86Code src, dest=workReg, kind=MoveWord},
                                             LLStoreArgument{
                                                    source=RegisterArg(workReg), base=esp, index=NoIndex,
                                                    offset = addr*wordSize, kind=MoveWord}]
                                    |   NONE =>
                                        (* So far it seems we always find a register this way if we need one.
                                           Just in case, fall back to pushing a register, using it and popping it
                                           again. *)
                                        let
                                            val workReg = GenReg eax
                                            val workRegAsArg = RegisterArg workReg
                                            val destAddr = addr+1
                                            val newSrc =
                                                case src of
                                                    StackSource wordOffset => StackSource(wordOffset+1)
                                                |   src => src
                                        in
                                            [
                                            LLPushValue{arg=workRegAsArg},
                                            LLLoadArgument{source=sourceToX86Code newSrc, dest=workReg, kind=MoveWord},
                                            LLStoreArgument{
                                                source=RegisterArg workReg, base=esp, index=NoIndex,
                                                offset = destAddr*wordSize, kind=MoveWord},
                                            LLLoadArgument{source=MemoryArg{base=esp, offset=0, index=NoIndex}, dest=workReg, kind=MoveWord},
                                            LLResetStackPtr{numWords=1, preserveCC=false}
                                            ]
                                        end
                            in
                                loadAndStoreCode @ moveEachValue(rest, reserved)
                            end

                    |   moveEachValue ([{dst=StackDest addr, src}] :: rest, reserved) =
                            (* Store from a register or a constant. *)
                            LLStoreArgument{
                                source=sourceToX86Code src, base=esp, index=NoIndex, offset = addr*wordSize, kind=MoveWord} ::
                                    moveEachValue(rest, reserved)

                    |   moveEachValue(({dst=RegDest regA, src=OtherSource(RegisterArg regB)} :: (cycle as _ :: _)) :: rest, reserved) =
                        (* Cycle with register arguments - swap the registers. *)
                        let
                            (* We need to exchange some of the arguments.  Doing an exchange here will
                               set the destination with the correct source.  However we have to process
                               every subsequent entry with the swapped registers.  That may well mean that
                               one of those entries becomes trivial.  Using XCHG means that we can move
                               N registers in N-1 exchanges.
                               We also need to rerun stronglyConnectedComponents on at least the rest of
                               this cycle.  It's easiest to flatten the rest and do everything. *)
                            val flattened = List.foldl(fn (a, b) => a @ b) [] (cycle :: rest)
                            fun swapRegs r = if r = regA then regB else if r = regB then regA else r
                            fun swapGenRegs r = if GenReg r = regA then asGenReg regB else if GenReg r = regB then asGenReg regA else r
                            fun swapSources{src=OtherSource(RegisterArg r), dst} =
                                    {src=OtherSource(RegisterArg(swapRegs r)), dst=dst}
                            |   swapSources{src=OtherSource(MemoryArg{base, index, offset}), dst} =
                                let
                                    val newIndex =
                                        case index of
                                            NoIndex => NoIndex
                                        |   Index1 r => Index1(swapGenRegs r)
                                        |   Index2 r => Index2(swapGenRegs r)
                                        |   Index4 r => Index4(swapGenRegs r)
                                        |   Index8 r => Index8(swapGenRegs r)
                                in
                                    {src=OtherSource(MemoryArg{base=swapGenRegs base, index=newIndex, offset=offset}), dst=dst}
                                end
                            |   swapSources sd = sd
                        in
                            LLExchangeRegisters{regX=regA, regY=regB} ::
                                moveValues(List.map swapSources flattened, regA :: reserved)
                        end

                    |   moveEachValue(cycle as ({src=originalSrc, ...} :: _ :: _) :: _, reserved) =
                        let
                            (* Anything else.  Break the cycle by getting a work register and
                               loading the source into that.  This is requires more instructions
                               than using an exchange but we don't want to use XCHG between
                               a register and memory.  That generates a memory lock and
                               we don't want the overhead. *)
                            val flattened = List.foldl(fn (a, b) => a @ b) [] cycle
                            val workReg =
                                case getWorkRegister(reserved, cycle) of
                                    SOME workReg => workReg
                                |   _ => raise RegisterOverflow emptySet  (* Force an extra register spill here. *)
                            (* Replace any reference to the source by the new register. *)
                            val replace = OtherSource(RegisterArg workReg)
                            fun swapSources{src, dst} =
                            let
                                val newSource =
                                    case (src, originalSrc) of
                                        (OtherSource(RegisterArg r1), OtherSource(RegisterArg r2)) =>
                                            if r1=r2 then replace else src
                                    |   (StackSource offset1, StackSource offset2) =>
                                            if offset1 = offset2 then replace else src
                                    |   (OtherSource(MemoryArg _), _) => raise InternalError "swapSources: memory loc"
                                    |   (_, OtherSource(MemoryArg _)) => raise InternalError "swapSources: memory loc"
                                    |   _ => src
                            in
                                {src=newSource, dst=dst}
                            end
                        in
                            LLLoadArgument{source=sourceToX86Code originalSrc, dest=workReg, kind=MoveWord} ::
                                (* Process the whole input including this instruction. Don't include
                                   the work reg as reserved.  It's present as a source now so won't
                                   be reused until we've copied it to the real destination. *)
                                moveValues(List.map swapSources flattened, reserved)
                        end

                    |   moveEachValue(([]) :: _, _) = (* This should not happen - avoid warning. *)
                            raise InternalError "moveEachValue - empty set"
                in
                    moveEachValue(ordered, reservedRegs)
                end
        in
            moveValues(moves, reservedRegs)
        end

        (* Where we have multiple specific registers as either source or
           destination there is the potential that a destination register
           if currently in use as a source. *) 
        fun moveMultipleRegisters regPairList =
        let
            val regPairsAsDests =
                List.map(fn {src, dst} => {src=OtherSource(RegisterArg src), dst=RegDest dst}) regPairList
            (* We don't need a work register so this doesn't matter. *)
            val reserveRegs = generalRegisters (* Consider all registers to be in use. *)
        in
            moveMultipleValues(regPairsAsDests, reserveRegs)
        end

        (* Tail recursive calls are complicated because we generally have to overwrite the existing stack.
           That means storing the arguments in the right order to avoid overwriting a
           value that we are using for a different argument. *)

        fun codeTailCall(callKind, arguments: {dst: destinations, src: llsource} list, stackAdjust) =
        if stackAdjust < 0
        then
        let
            (* If the function we're calling takes more arguments on the stack than the
               current function we will have to extend the stack.  Do that by pushing the
               argument whose offset is at -1.  Then adjust all the offsets and repeat. *)
            val {src=argM1, ...} = valOf(List.find(fn {dst=StackDest ~1, ...} => true | _ => false) arguments)
            fun renumberArgs [] = []
            |   renumberArgs ({dst=StackDest ~1, ...} :: args) = renumberArgs args (* Remove the one we've done. *)
            |   renumberArgs ({dst, src} :: args) =
                let
                    val newDest = case dst of StackDest d => StackDest(d+1) | regDest => regDest
                    val newSrc =
                        case src of
                            StackSource wordOffset => StackSource(wordOffset+1)
                        |   other => other
                in
                    {dst=newDest, src=newSrc} :: renumberArgs args
                end
        in
            LLPushValue{arg=sourceToX86Code argM1} :: codeTailCall(callKind, renumberArgs arguments, stackAdjust+1)
        end
        else
        let
            val loadArgs = moveMultipleValues(arguments, [] (* We only need argument registers. *))

            val adjustStack =
                if stackAdjust = 0
                then []
                else [LLResetStackPtr{numWords=stackAdjust, preserveCC=false}]
        in
            loadArgs @ adjustStack @ [LLTailRecursiveCall{callKind=callKind}]
        end

        val outputLabelCount = ref 0
        val blockToLabelMap = Array.array(numBlocks, ~1)

        fun makeLabel() = Label{labelNo = ! outputLabelCount} before outputLabelCount := !outputLabelCount + 1
       
        fun getBlockLabel blockNo =
            case Array.sub(blockToLabelMap, blockNo) of
                ~1 =>
                let
                    val label as Label{labelNo} = makeLabel()
                    val () = Array.update(blockToLabelMap, blockNo, labelNo)
                in label end
            |   n => Label{labelNo=n}

        (* The profile object is a single mutable with the F_bytes bit set. *)
        local
            val v = RunCall.allocateByteMemory(0w1, Word.fromLargeWord(Word8.toLargeWord(Word8.orb(F_mutable, F_bytes))))
            fun clear 0w0 = ()
            |   clear i = (assignByte(v, i-0w1, 0w0); clear (i-0w1))
            val () = clear(Word.fromInt wordSize)
        in
            val profileObject = toMachineWord v
        end
        (* Switch to indicate if we want to trace where live data has been allocated. *)
        val addAllocatingFunction =
            DEBUG.getParameter DEBUG.profileAllocationTag debugSwitches = 1

        (* Check the stack limit "register".  This is used both at the start of a function for genuine
           stack checking but also in a loop to check for an interrupt.  We need to save the registers
           even across an interrupt because it can be used if another thread wants a GC. *)
        fun testRegAndTrap(reg, entryPt, saveRegs) =
        let
            (* Normally we won't have a stack overflow so we will skip the check. *)
            val skipCheckLab = makeLabel()
        in
            (* Need it in reverse order. *)
            [
                JumpLabel skipCheckLab,
                CallRTS{rtsEntry=entryPt, saveRegs=saveRegs},
                ConditionalBranch{test=JNB, predict=PredictTaken, label=skipCheckLab},
                ArithToGenReg{ opc=CMP, output=reg, source=MemoryArg{offset=memRegStackLimit, base=ebp, index=NoIndex} }
            ]
        end
        
        fun codeExtended({instr=LoadArgument{source, dest=PReg dreg, kind}, ...}, code) =
            (
                case decache source of
                    RegisterArgument(PReg sreg) =>
                    (* Register to register move.  Try to use the same register for the source as the destination
                       to eliminate the instruction. *)
                    let
                    in
                        case Array.sub(allocatedRegs, dreg) of
                            NONE => (* The result was never used *) code
                        |   SOME realDestReg =>
                            let
                                val regset =
                                    case kind of
                                        MoveFloat => floatingPtRegisters
                                    |   MoveDouble => floatingPtRegisters
                                    |   _ => generalRegisters
                                (* Get the source register using the current destination as a preference. *)
                                val realSrcReg = findRegister(sreg, SOME realDestReg, regset)
                            in
                                (* If the source is the same as the destination we don't need to do anything. *)
                                moveIfNecessary{src=realSrcReg, dst=realDestReg} @ code
                            end
                    end

                |   source =>
                    (* Loads of constants or from an address. *)
                    let
                        val regSet =
                            case kind of MoveFloat => floatingPtRegisters | MoveDouble => floatingPtRegisters | _ => generalRegisters
                    in
                        (* If we don't have a register that means the result is never used. *)
                        case Array.sub(allocatedRegs, dreg) of
                            SOME regResult => LLLoadArgument{source=codeExtArgument(source, regSet), dest=regResult, kind=kind} :: code
                        |   NONE => code
                    end
            )

        |   codeExtended({instr=StoreArgument{ source, base=PReg bReg, offset, index, kind }, ...}, code) =
            (
                case (decache source, kind) of
                    (RegisterArgument(PReg sReg), MoveByte) =>
                    if isX64
                    then
                        LLStoreArgument{
                            source=codeExtArgGeneral source, base=asGenReg(findGeneralReg bReg), offset=offset, index=codeExtIndex index, kind=MoveByte}
                                :: code
                    else
                    (* This is complicated on X86/32.  We can't use edi or esi for the store registers.  Instead
                       we reserve ecx (see special case in "identify") and use that if we have to. *)
                    let
                        val realStoreReg = findRegister(sReg, SOME(GenReg ecx), generalRegisters)
                        val (moveCode, storeReg) =
                            if realStoreReg = GenReg edi orelse realStoreReg = GenReg esi
                            then (moveIfNecessary{src=realStoreReg, dst=GenReg ecx}, GenReg ecx)
                            else ([], realStoreReg)
                    in
                        moveCode @
                        (LLStoreArgument{
                            source=RegisterArg storeReg, base=asGenReg(findGeneralReg bReg), offset=offset, index=codeExtIndex index, kind=MoveByte}
                                :: code)
                    end
                    
                |   _ =>
                    let
                        val regSet =
                            case kind of MoveFloat => floatingPtRegisters | MoveDouble => floatingPtRegisters | _ => generalRegisters
                    in
                        LLStoreArgument{
                            source=codeExtArgument(source, regSet), base=asGenReg(findGeneralReg bReg), offset=offset, index=codeExtIndex index, kind=kind}
                                :: code
                    end
            )

        |   codeExtended({instr=LoadMemReg { offset, dest=PReg pr}, ...}, code) =
            (* Load from the "memory registers" pointed at by ebp. *)
            (
                (* If we don't have a register that means the result is never used. *)
                case Array.sub(allocatedRegs, pr) of
                    SOME regResult =>
                        LLLoadArgument{source=MemoryArg{base=ebp, offset=offset, index=NoIndex}, dest=regResult, kind=MoveWord} :: code
                |   NONE => code
            )

        |   codeExtended({instr=BeginFunction{regArgs, ...}, ...}, code) =
            let
                (* Look up the register we need to move the argument into.
                   If there's no register then we don't use this argument and
                   can drop it. *)
                fun mkPair(PReg pr, rr) =
                    case Array.sub(allocatedRegs, pr) of
                        NONE => NONE
                    |   SOME regResult => SOME{src=rr,dst=regResult}
                val regPairs = List.mapPartial mkPair regArgs
            in
                moveMultipleRegisters regPairs @ code
            end

        |   codeExtended({instr=TailRecursiveCall{callKind, regArgs=oRegArgs, stackArgs=oStackArgs, stackAdjust, stackOffset}, ...}, code) =
            let
                val regArgs = List.map (fn (arg, reg) => (decache arg, reg)) oRegArgs
                and stackArgs = List.map(fn {src, stack } => {src=decache src, stack=stack}) oStackArgs
                
                (* We must leave stack entries as stack entries for the moment. *)
                fun codeArg(StackLocation{wordOffset, cache=NONE, ...}) = StackSource wordOffset
                |   codeArg arg = OtherSource(codeExtArgGeneral arg)

                val extStackArgs = map (fn {stack, src} => {dst=StackDest(stack+stackOffset), src=codeArg src}) stackArgs
                val extRegArgs = map (fn (a, r) => {src=codeArg a, dst=RegDest r}) regArgs
                val codeTail =
                    codeTailCall(callKind, extStackArgs @ extRegArgs, stackAdjust+stackOffset)
                        handle RegisterOverflow _ =>
                        let
                            (* We've not been able to find enough free registers and have to push something.
                               Find the registers that are needed for the arguments.  This duplicates code in
                               identifyReferences but means we don't need to pass the active set everywhere. *)
                            fun getSourceRegs(RegisterArgument(PReg rarg)) = [rarg]
                            |   getSourceRegs(MemoryLocation { base=PReg base, index=NoMemIndex, ...}) = [base]
                            |   getSourceRegs(MemoryLocation { base=PReg base, index=MemIndex1(PReg index), ...}) = [base, index]
                            |   getSourceRegs(MemoryLocation { base=PReg base, index=MemIndex2(PReg index), ...}) = [base, index]
                            |   getSourceRegs(MemoryLocation { base=PReg base, index=MemIndex4(PReg index), ...}) = [base, index]
                            |   getSourceRegs(MemoryLocation { base=PReg base, index=MemIndex8(PReg index), ...}) = [base, index]
                            |   getSourceRegs _ = []

                            val activeFromRegs =
                                List.foldl(fn((arg, _), srcs) => getSourceRegs arg @ srcs) [] regArgs
                            val active =
                                List.foldl(fn ({src, ...}, srcs) => getSourceRegs src @ srcs) activeFromRegs stackArgs
                        in
                            raise RegisterOverflow(listToSet active)
                        end
            in
                codeTail @ code
            end

        |   codeExtended({instr=FunctionCall{callKind, regArgs=oRegArgs, stackArgs=oStackArgs, dest=PReg dReg}, ...}, code) =
            let
                val regArgs = List.map (fn (arg, reg) => (decache arg, reg)) oRegArgs
                and stackArgs = List.map decache oStackArgs
                
                val destReg = findRegister(dReg, SOME(GenReg eax), generalRegisters)
                
                fun pushStackArgs ([], _) = []
                |   pushStackArgs (arg ::args, argNum) =
                    let
                        (* Have to adjust the offsets of stack arguments. *)
                        val adjusted =
                            case arg of
                                StackLocation {wordOffset, container, field, ...} =>
                                    StackLocation{wordOffset=wordOffset+argNum, container=container, field=field+argNum,
                                                  cache=NONE}
                            |   arg => arg
                    in
                        LLPushValue {arg=codeExtArgGeneral adjusted} :: pushStackArgs(args, argNum+1)
                    end
                val pushedArgs = pushStackArgs(stackArgs, 0)
                (* We have to adjust any stack offset to account for the arguments we've pushed. *)
                val numStackArgs = List.length stackArgs
                
                (* We don't currently allow the arguments to be memory locations and instead
                   force them into registers.  That may be simpler especially if we can get the
                   values directly into the required register. *)
                fun getRegArgs(RegisterArgument(PReg pr), reg) =
                        SOME{dst=reg, src=findRegister(pr, SOME reg, generalRegisters)}
                |   getRegArgs(StackLocation {cache=SOME(PReg pr), ...}, reg) =
                        SOME{dst=reg, src=findRegister(pr, SOME reg, generalRegisters)}
                |   getRegArgs(MemoryLocation _, _) = raise InternalError "FunctionCall - MemoryLocation"
                |   getRegArgs _ = NONE
                val loadRegArgs = moveMultipleRegisters(List.mapPartial getRegArgs regArgs)

                (* These are all items we can load without requiring a source register.
                   That includes loading from the stack. *)
                fun getConstArgs(AddressConstant m, reg) =
                        SOME(LLLoadArgument{source=AddressConstArg m, dest=reg, kind=MoveWord})
                |   getConstArgs(IntegerConstant i, reg) =
                        SOME(LLLoadArgument{source=NonAddressConstArg i, dest=reg, kind=MoveWord})
                |   getConstArgs(StackLocation { wordOffset, ...}, reg) =
                        SOME(LLLoadArgument{source=MemoryArg{offset=(wordOffset+numStackArgs)*wordSize, base=esp, index=NoIndex},
                                          dest=reg, kind=MoveWord})
                |   getConstArgs(RegisterArgument _, _) = NONE
                |   getConstArgs(MemoryLocation _, _) = NONE
                val loadConstArgs = List.mapPartial getConstArgs regArgs
            in
                pushedArgs @ loadRegArgs @ loadConstArgs @
                    (LLFunctionCall{callKind=callKind} ::
                        moveIfNecessary{dst=destReg, src=GenReg eax}) @ code
            end

        |   codeExtended({instr=AllocateMemoryOperation{ size, flags, dest=PReg dReg, saveRegs}, ...}, code) =
            let
                val preserve = List.map(fn (PReg r) => findGeneralReg r) saveRegs
            in
                LLAllocateMemoryOperation{ size=size, flags=flags, dest=findGeneralReg dReg, saveRegs=preserve} :: code
            end

        |   codeExtended({instr=AllocateMemoryVariable{size=PReg sReg, dest=PReg dReg, saveRegs}, ...}, code) =
            let
                (* Simple case - no initialiser. *)
                val preserve = List.map(fn (PReg r) => findGeneralReg r) saveRegs
                val destReg = findGeneralReg dReg
            in
                LLAllocateMemoryVariable{size=findGeneralReg sReg, dest=destReg, saveRegs=preserve} :: code
            end

        |   codeExtended({instr=InitialiseMem{size=PReg sReg, addr=PReg aReg, init=PReg iReg}, ...}, code) =
            let
                (* We are going to use rep stosl/q to set the memory.
                   That requires the length to be in ecx, the initialiser to be in eax and
                   the destination to be edi. *)
                val realAddrReg = findRegister(aReg, SOME(GenReg edi), generalRegisters)
                val realInitReg = findRegister(iReg, SOME(GenReg eax), generalRegisters)
                val realSizeReg = findRegister(sReg, SOME(GenReg ecx), generalRegisters)
            in
                moveMultipleRegisters[
                    {src=realInitReg, dst=GenReg eax}, {src=realSizeReg, dst=GenReg ecx}, {src=realAddrReg, dst=GenReg edi}] @
                    LLInitialiseMem{size=GenReg ecx, addr=GenReg edi, init=GenReg eax} :: code
            end

        |   codeExtended({instr=InitialisationComplete, ...}, code) = LLInitialisationComplete :: code

        |   codeExtended({instr=JumpLoop{regArgs, stackArgs, checkInterrupt}, ...}, code) =
            let
                (* TODO: Make the sources and destinations "friends". *)
                (* We must leave stack entries as stack entries for the moment as with TailCall. *)
                fun codeArg(StackLocation{wordOffset, ...}) = StackSource wordOffset
                |   codeArg arg = OtherSource(codeExtArgGeneral arg)
                val extStackArgs = map (fn (src, stack, _) => {dst=StackDest stack, src=codeArg src}) stackArgs
                val extRegArgs = map (fn (a, PReg r) => {src=codeArg a, dst=RegDest(findGeneralReg r)}) regArgs
                val check = Option.map(List.map(fn (PReg r) => findGeneralReg r)) checkInterrupt
            in
                moveMultipleValues(extStackArgs @ extRegArgs, generalRegisters) @
                    (LLJumpLoop{checkInterrupt=check} :: code)
            end

        |   codeExtended({instr=RaiseExceptionPacket{ packetReg=PReg preg }, ...}, code) =
            let
                (* The argument must be put into rax. *)
                val argReg = findRegister(preg, SOME(GenReg eax), generalRegisters)
            in
                moveIfNecessary{src=argReg, dst=raxAsArg} @ LLRaiseExceptionPacket :: code
            end

        |   codeExtended({instr=ReserveContainer{size, ...}, ...}, code) =
            let
                (* The memory must be cleared in case we have a GC. *)
                val pushAll = List.tabulate(size, fn _ => LLPushValue{arg=NonAddressConstArg(tag 0)})
            in
                pushAll @ code
            end

        |   codeExtended({instr=LoadContainerAddress{stackOffset, dest=PReg aReg, ...}, ...}, code) =
            let
                val addrReg = findRegister(aReg, NONE, generalRegisters)
                val loadAddr =
                    if stackOffset = 0
                    then LLLoadArgument{source=RegisterArg(GenReg esp), dest=addrReg, kind=MoveWord}
                    else LLLoadEffectiveAddress { base=SOME(GenReg esp), offset=stackOffset*wordSize, index=NoIndex, dest=addrReg }
            in
                loadAddr :: code
            end

        |   codeExtended({instr=IndexedCaseOperation{testReg=PReg tReg, workReg=PReg wReg}, ...}, code) =
            let
                val testReg = findRegister(tReg, NONE, generalRegisters)
                val workReg = findRegister(wReg, NONE, generalRegisters)
                val _ = testReg <> workReg orelse raise InternalError "IndexedCaseOperation - same registers"
            in
                LLIndexedCaseOperation{ testReg=testReg, workReg=workReg} :: code
            end

        |   codeExtended({instr=LockMutable{addr=PReg pr}, ...}, code) =
                LLLockMutable{addr=findRegister(pr, NONE, generalRegisters)} :: code

        |   codeExtended({instr=WordComparison{ arg1 as RegisterArgument _, arg2, ... }, ...}, code) =
                LLWordComparison{ arg1=codeExtArgGeneral arg1, arg2=codeExtArgGeneral arg2 } :: code

        |   codeExtended({instr=WordComparison _, ...}, _) = raise InternalError "codeExtended - WordComparison"

            (* Set up an exception handler. *)
        |   codeExtended({instr=PushExceptionHandler{workReg=PReg hReg}, ...}, code) =
                LLPushExceptionHandler{workReg=findGeneralReg hReg} :: code

            (* Pop an exception handler at the end of a handled section.  Executed if no exception has been raised.
               This removes items from the stack. *)
        |   codeExtended({instr=PopExceptionHandler{workReg=PReg wReg, ...}, ...}, code) =
            let
                val realWork = findGeneralReg wReg
            in
                LLPopExceptionHandler{workReg=realWork} :: code
            end

            (* Start of a handler.  Sets the address associated with PushExceptionHandler and
               provides a register for the packet.*) 
        |   codeExtended({instr=BeginHandler{packetReg=PReg pReg, workReg=PReg wReg}, ...}, code) =
            let
                (* The exception packet is in rax. *)
                val realPktReg = findRegister(pReg, SOME(GenReg eax), generalRegisters)
                val realWorkreg = findGeneralReg wReg
            in
                LLBeginHandler{workReg=realWorkreg, packetReg=GenReg eax} ::
                    (moveIfNecessary{src=GenReg eax, dst=realPktReg } @ code)
            end

        |   codeExtended({instr=ReturnResultFromFunction { resultReg=PReg resReg, numStackArgs }, ...}, code) =
            let
                val resultReg = findRegister(resReg, SOME(GenReg eax), generalRegisters)
                (* If for some reason it's not in the right register we have to move it there. *)
                val moveCode = moveIfNecessary{src=resultReg, dst=raxAsArg}
            in
                moveCode @
                    (LLReturnResultFromFunction{resultReg=raxAsArg, numStackArgs=numStackArgs} :: code)
            end

        |   codeExtended({instr=ArithmeticFunction{oper=SUB, resultReg=PReg resReg, operand1=RegisterArgument(PReg op1Reg),
                                            operand2, ...}, ...}, code) =
            (* Subtraction - this is special because it can only be done one way round.  The first argument must
               be in a register. *)
            let
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                (* Try to put the argument into the same register as the result. *)
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, generalRegisters)
                val op2Arg = codeExtArgGeneral operand2
                (* If we couldn't put it in the result register we have to copy it there. *)
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg} @
                    (LLArithmeticFunction{oper=SUB, resultReg=realDestReg, operand1=RegisterArg realDestReg, operand2=op2Arg} :: code)
            end

        |   codeExtended({instr=ArithmeticFunction{oper=SUB, ...}, ...}, _) = raise InternalError "codeExtended - ArithmeticFunction"

        |   codeExtended({instr=ArithmeticFunction{oper, resultReg=PReg resReg, operand1=RegisterArgument(PReg op1Reg),
                                            operand2=RegisterArgument(PReg op2Reg), ...}, ...}, code) =
            (* Arithmetic operation with both arguments as registers.  These operations are all symmetric so
               we can try to put either argument into the result reg and then do the operation on the other arg. *)
            let
               val realDestReg = findRegister(resReg, NONE, generalRegisters)
                
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, generalRegisters)
                and realOp2Reg = findRegister(op2Reg, SOME realDestReg, generalRegisters)
                val (operandReg, moveInstr) =
                    if realOp1Reg = realDestReg
                    then (realOp2Reg, [])
                    else if realOp2Reg = realDestReg
                    then (realOp1Reg, [])
                    else (realOp2Reg, [moveRR{src=realOp1Reg, dst=realDestReg}])
            in
                moveInstr @
                    (LLArithmeticFunction{oper=oper, resultReg=realDestReg, operand1=RegisterArg realDestReg,
                                        operand2=RegisterArg operandReg} :: code)
            end

        |   codeExtended({instr=ArithmeticFunction{oper, resultReg=PReg resReg, operand1=RegisterArgument(PReg op1Reg),
                                            operand2, ...}, ...}, code) =
            (* Arithmetic operation with the first argument in a register and the second a constant or memory location. *)
            let
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, generalRegisters)
                val op2Arg = codeExtArgGeneral operand2
                (* If we couldn't put it in the result register we have to copy it there. *)
                (* TODO: Is there the potential for a problem?  We don't worry about a conflict
                   between the result register and the arguments.  What if the second argument is a memory
                   location with the result reg as a base or index? *)
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg} @
                    (LLArithmeticFunction{oper=oper, resultReg=realDestReg, operand1=RegisterArg realDestReg, operand2=op2Arg} :: code)
            end

        |   codeExtended({instr=ArithmeticFunction{oper, resultReg=PReg resReg, operand1,
                                            operand2=RegisterArgument(PReg op2Reg), ...}, ...}, code) =
            (* Arithemtic operation with the second argument in a register and the first a constant or memory location. *)
            let
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                val realOp2Reg = findRegister(op2Reg, SOME realDestReg, generalRegisters)
                val op1Arg = codeExtArgGeneral operand1
            in
                moveIfNecessary{src=realOp2Reg, dst=realDestReg} @
                    (LLArithmeticFunction{oper=oper, resultReg=realDestReg, operand1=RegisterArg realDestReg, operand2=op1Arg} :: code)
            end

        |   codeExtended({instr=ArithmeticFunction _, ...}, _) =
                raise InternalError "codeExtended - ArithmeticFunction"

        |   codeExtended({instr=TestTagBit{arg, ...}, ...}, code) = LLTestTagBit{ arg=codeExtArgGeneral arg } :: code

        |   codeExtended({instr=PushValue {arg, ...}, ...}, code) = LLPushValue {arg=codeExtArgGeneral arg} :: code

        |   codeExtended({instr=CopyToCache{source=PReg sreg, dest=PReg dreg}, ...}, code) =
            let
                val realDestReg = findRegister(dreg, NONE, generalRegisters)
                (* Get the source register using the current destination as a preference. *)
                val realSrcReg = findRegister(sreg, SOME realDestReg, generalRegisters)
            in
                (* If the source is the same as the destination we don't need to do anything. *)
                moveIfNecessary{src=realSrcReg, dst=realDestReg} @ code
            end

        |   codeExtended({instr=ResetStackPtr {numWords, preserveCC}, ...}, code) =
                LLResetStackPtr{numWords=numWords, preserveCC=preserveCC} :: code

        |   codeExtended({instr=StoreToStack{ source, stackOffset, ... }, ...}, code) =
            let
                val codeSource = codeExtArgument(source, generalRegisters)
            in
                LLStoreArgument{
                    source=codeSource, base=esp, offset=stackOffset*wordSize, index=NoIndex, kind=MoveWord}
                        :: code
            end

        |   codeExtended({instr=TagValue{source=PReg srcReg, dest=PReg dReg}, ...}, code) =
            (
                case Array.sub(allocatedRegs, dReg) of
                    SOME regResult =>
                        let
                            (* If we're using LEA to tag there's we can use any source register. *)
                            val realSReg = asGenReg(findRegister(srcReg, NONE, generalRegisters))
                        in
                            LLLoadEffectiveAddress { base=NONE, offset=1, index=Index2 realSReg, dest=regResult }  :: code
                        end
                |   NONE => code
            )

        |   codeExtended({instr=UntagValue{source=RegisterArgument(PReg sReg), dest=PReg dReg, isSigned}, ...}, code) =
            (* Always generates register argument at the moment.  TODO: This should really just take a single register arg. *)
            (
                case Array.sub(allocatedRegs, dReg) of
                    SOME regResult =>
                        let
                            val realSReg = findRegister(sReg, SOME regResult, generalRegisters)
                        in
                            moveIfNecessary{src=realSReg, dst=regResult} @
                                (LLShiftOperation{shift=if isSigned then SAR else SHR, resultReg=regResult, operand=RegisterArg regResult,
                                    shiftAmount=NonAddressConstArg 1} :: code)
                        end
                |   NONE => code
            )

        |   codeExtended({instr=UntagValue _, ...}, _) = raise InternalError "UntagValue"

        |   codeExtended({instr=LoadEffectiveAddress{base=SOME base, offset=0, index=NoMemIndex, dest}, active, current, kill}, code) =
                (* This should be handled at the higher level. *)
                codeExtended({instr=LoadArgument{source=RegisterArgument base, dest=dest, kind=MoveWord},
                              active=active, current=current, kill=kill}, code)

        |   codeExtended({instr=LoadEffectiveAddress{base, offset, index, dest=PReg dReg}, ...}, code) =
            let
                val destReg = findGeneralReg dReg
                val bReg = case base of SOME(PReg br) => SOME(findGeneralReg br) | NONE => NONE
                val iReg = codeExtIndex index
            in
                LLLoadEffectiveAddress{base=bReg, offset=offset, index=iReg, dest=destReg} :: code
            end

        |   codeExtended({instr=ShiftOperation{shift, resultReg=PReg resReg, operand=RegisterArgument(PReg operReg), shiftAmount=IntegerConstant i, ...}, ...}, code) =
            let
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                val realOpReg = findRegister(operReg, SOME realDestReg, generalRegisters)
            in
                 moveIfNecessary{src=realOpReg, dst=realDestReg} @
                    (LLShiftOperation{shift=shift, resultReg=realDestReg, operand=RegisterArg realDestReg,
                                    shiftAmount=NonAddressConstArg i} :: code)
            end

        |   codeExtended({instr=ShiftOperation{shift, resultReg=PReg resReg, operand=RegisterArgument(PReg operReg),
                                        shiftAmount=RegisterArgument(PReg shiftReg), ...}, ...}, code) =
            let
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                val realShiftReg = findRegister(shiftReg, SOME(GenReg ecx), generalRegisters)
                val realOpReg = findRegister(operReg, SOME realDestReg, generalRegisters)
                (* We want the shift in ecx.  We may not have got it there but the register
                   should be free. *)
            in
                 moveIfNecessary{src=realOpReg, dst=realDestReg} @ moveIfNecessary{src=realShiftReg, dst=GenReg ecx} @
                    (LLShiftOperation{shift=shift, resultReg=realDestReg, operand=RegisterArg realDestReg,
                                    shiftAmount=RegisterArg(GenReg ecx)} :: code)
            end

        |   codeExtended({instr=ShiftOperation _, ...}, _) = raise InternalError "codeExtended - ShiftOperation"

        |   codeExtended({instr=
                Multiplication{resultReg=PReg resReg, operand1=RegisterArgument(PReg op1Reg),
                               operand2=RegisterArgument(PReg op2Reg), ...}, ...}, code) =
            let
                (* Treat exactly the same as ArithmeticFunction. *)
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, generalRegisters)
                and realOp2Reg = findRegister(op2Reg, SOME realDestReg, generalRegisters)
                val (operandReg, moveInstr) =
                    if realOp1Reg = realDestReg
                    then (realOp2Reg, [])
                    else if realOp2Reg = realDestReg
                    then (realOp1Reg, [])
                    else (realOp2Reg, [moveRR{src=realOp1Reg, dst=realDestReg}])
            in
                moveInstr @
                    (LLMultiplication{resultReg=realDestReg, operand1=RegisterArg realDestReg,
                                        operand2=RegisterArg operandReg} :: code)
            end

            (* We currently only generate the register/register case. *)
        |   codeExtended({instr=Multiplication _, ...}, _) = raise InternalError "codeExtended - multiplication TODO"

        |   codeExtended({instr=Division{isSigned, dividend=PReg regDivid, divisor, quotient=PReg regQuot,
                                  remainder=PReg regRem}, ...}, code) =
            let
                (* Division is specific as to the registers.  The dividend must be eax, quotient is
                   eax and the remainder is edx. *)
                val realDiviReg = findRegister(regDivid, SOME(GenReg eax), generalRegisters)
                val realQuotReg = findRegister(regQuot, SOME(GenReg eax), generalRegisters)
                val realRemReg = findRegister(regRem, SOME(GenReg edx), generalRegisters)
                val divisorArg = codeExtArgGeneral divisor
            in
                (* We may need to move one or more of the registers although normally that
                   won't be necessary.  Almost certainly only either the remainder or the
                   quotient will actually be used. *)
                moveIfNecessary{src=realDiviReg, dst=GenReg eax} @
                    LLDivision{isSigned=isSigned, dividend=GenReg eax, divisor=divisorArg,
                              quotient=GenReg eax, remainder=GenReg edx} ::
                    moveMultipleRegisters[{src=GenReg eax, dst=realQuotReg}, {src=GenReg edx, dst=realRemReg}] @ code
            end

        |   codeExtended({instr=AtomicExchangeAndAdd{base=PReg bReg, source=PReg sReg}, ...}, code) =
            let
                val srcReg = findGeneralReg sReg
                val baseReg = findGeneralReg bReg
            in
                LLAtomicExchangeAndAdd{base=baseReg, source=srcReg} :: code
            end

        |   codeExtended({instr=BoxValue{boxKind, source=PReg sReg, dest=PReg dReg, saveRegs}, ...}, code) =
            let
                val preserve = List.map(fn (PReg r) => findGeneralReg r) saveRegs
                val (srcReg, boxSize, moveKind) =
                    case boxKind of
                        BoxLargeWord => (findGeneralReg sReg, 1, MoveWord)
                    |   BoxFloat => (findFloatReg sReg, Int.quot(8, wordSize), MoveDouble)
                val dstReg = findGeneralReg dReg
            in
                LLAllocateMemoryOperation{ size=boxSize, flags=0wx1, dest=dstReg, saveRegs=preserve} ::
                LLStoreArgument{ source=RegisterArg srcReg, offset=0, base=asGenReg dstReg, index=NoIndex, kind=moveKind} ::
                LLInitialisationComplete :: code
            end

        |   codeExtended({instr=CompareByteVectors{vec1Addr=PReg v1Reg, vec2Addr=PReg v2Reg, length=PReg lReg, ...}, ...}, code) =
            let
                val realV1Reg = findRegister(v1Reg, SOME(GenReg esi), generalRegisters)
                val realV2Reg = findRegister(v2Reg, SOME(GenReg edi), generalRegisters)
                val realLengthReg = findRegister(lReg, SOME(GenReg ecx), generalRegisters)
                (* There's a complication here.  CompareByteVectors generates REPE CMPSB to compare
                   the vectors but the condition code is only set if CMPSB is executed at least
                   once.  If the value in RCX/ECX is zero it will never be executed and the
                   condition code will be unchanged.  We want the result to be "equal" in that
                   case so we need to ensure that is the case.  It's quite possible that the
                   condition code has just been set by shifting RCX/ECX to remove the tag in which
                   case it will have set "equal" if the value was zero.  We use CMP R/ECX,R/ECX which
                   is two bytes in 32-bit but three in 64-bit.
                   If we knew the length was non-zero (e.g. a constant) we could avoid this. *)
            in
                moveIfNecessary{src=realV1Reg, dst=GenReg esi} @ moveIfNecessary{src=realV2Reg, dst=GenReg edi} @
                    moveIfNecessary{src=realLengthReg, dst=GenReg ecx} @
                    (LLWordComparison {arg1=RegisterArg rcxAsArg, arg2=RegisterArg rcxAsArg} ::
                     LLCompareByteVectors{vec1Addr=GenReg esi, vec2Addr=GenReg edi, length=GenReg ecx} :: code)
            end

        |   codeExtended({instr=BlockMove{srcAddr=PReg sReg, destAddr=PReg dReg, length=PReg lReg, isByteMove}, ...}, code) =
            let
                val realSrcReg = findRegister(sReg, SOME(GenReg esi), generalRegisters)
                val realDestReg = findRegister(dReg, SOME(GenReg edi), generalRegisters)
                val realLengthReg = findRegister(lReg, SOME(GenReg ecx), generalRegisters)
            in
                moveIfNecessary{src=realSrcReg, dst=GenReg esi} @ moveIfNecessary{src=realDestReg, dst=GenReg edi} @
                    moveIfNecessary{src=realLengthReg, dst=GenReg ecx} @
                    (LLBlockMove{srcAddr=GenReg esi, destAddr=GenReg edi, length=GenReg ecx, isByteMove=isByteMove} :: code)
            end

        |   codeExtended({instr=CompareFloatingPt{arg1 as RegisterArgument _, arg2, ...}, ...}, code) =
                LLCompareFloatingPt{ arg1=codeExtArgFloat arg1, arg2=codeExtArgFloat arg2 } :: code

        |   codeExtended({instr=CompareFloatingPt _, ...}, _) = raise InternalError "codeExtended - CompareFloatingPt"

        |   codeExtended({instr=X87FPGetCondition{dest=PReg dReg, ...}, ...}, code) =
            let
                (* We can only use RAX here. *)
                val destReg = findRegister(dReg, SOME(GenReg eax), generalRegisters)
            in
                LLX87FPGetCondition{dest=raxAsArg} ::
                    moveIfNecessary{dst=destReg, src=GenReg eax} @ code
            end

        |   codeExtended({instr=X87FPArith{opc, resultReg=PReg resReg, arg1=RegisterArgument(PReg op1Reg), arg2}, ...}, code) =
            let
                val realDestReg = findRegister(resReg, NONE, floatingPtRegisters)
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, floatingPtRegisters)
                val op2Arg = codeExtArgFloat arg2
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg} @
                    (LLX87FPArith{opc=opc, resultReg=realDestReg, arg1=RegisterArg realDestReg, arg2=op2Arg} :: code)
            end

        |   codeExtended({instr=X87FPArith _, ...}, _) = raise InternalError "codeExtended - X87FPArith"
    
        |   codeExtended({instr=X87FPUnaryOps{fpOp, dest=PReg resReg, source=RegisterArgument(PReg op1Reg)}, ...}, code) =
            let
                val realDestReg = findRegister(resReg, NONE, floatingPtRegisters)
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, floatingPtRegisters)
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg} @
                    (LLX87FPUnaryOps{fpOp=fpOp, dest=realDestReg, source=RegisterArg realDestReg} :: code)
            end

        |   codeExtended({instr=X87FPUnaryOps _, ...}, _) = raise InternalError "codeExtended - X87FPArith"

        |   codeExtended({instr=FloatFixedInt{dest=PReg resReg, source}, ...}, code) =
            let
                val intSource = codeExtArgGeneral source
                val fpReg = findRegister(resReg, NONE, floatingPtRegisters)
                val floatCode =
                    case fpMode of
                        FPModeX87 => 
                            (* This is complicated.  The integer value has to be in memory not in a
                               register so we have to push it to the stack and then make sure it is
                               popped afterwards.  Because it is untagged it is unsafe to leave it. *)
                            [
                                LLPushValue{arg=intSource},
                                LLFloatFixedInt{dest=fpReg, source=wordOffsetAddress(0, esp)},
                                LLResetStackPtr {numWords=1, preserveCC=false}
                            ]
                    |   FPModeSSE2 => [LLFloatFixedInt{dest=fpReg, source=intSource}]
            in
                floatCode @ code
            end
    
        |   codeExtended({instr=SSE2FPArith{opc, resultReg=PReg resReg, arg1=RegisterArgument(PReg op1Reg), arg2}, ...}, code) =
            let
                val realDestReg = findRegister(resReg, NONE, floatingPtRegisters)
                val realOp1Reg = findRegister(op1Reg, SOME realDestReg, floatingPtRegisters)
                val op2Arg = codeExtArgFloat arg2
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg} @
                    (LLSSE2FPArith{opc=opc, resultReg=realDestReg, arg1=RegisterArg realDestReg, arg2=op2Arg} :: code)
            end
    
        |   codeExtended({instr=SSE2FPArith _, ...}, _) = raise InternalError "codeExtended - SSE2FPArith"
        
        
        (* Depth-first scan. *)
        local
            val emptyBlock = LLBasicBlock { block = [], flow = ExitCode }
        in
            val visited = Array.array(numBlocks, false)
            val resultBlocks = Array.array(numBlocks, emptyBlock)
        end

        fun processBlocks blockNo =
        if Array.sub(visited, blockNo)
        then ()  (* Done or currently being done. *)
        else
        let
            val () = Array.update(visited, blockNo, true)
            val ExtendedBasicBlock { flow, block, passThrough, exports, ...} =
                Vector.sub(blocks, blockNo)
            (* Add the hints for this block before the actual allocation of registers. *)
            val _ = List.app addHints block
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
            local
                (* We assume that anything used later will have been allocated a register.
                   This is generally true except for a loop where the use may occur earlier. *)
                val exported = setToList passThrough @ setToList exports
                fun findAReg r =
                    case Vector.sub(regProps, r) of
                        RegPropStack _ => ()
                    |   _ => ignore(findRegister(r, NONE, generalRegisters))
            in
                val () = List.app findAReg exported
            end
            val codeBlock = List.foldr codeExtended [] block
            
            val resultBlock = LLBasicBlock { block = codeBlock, flow = flow }
        in
            Array.update(resultBlocks, blockNo, resultBlock)
        end

        val () = processBlocks 0
        
        (* We now have the code with concrete registers. *)
        
        (* Generate code from the ICode.  This assumes that all pseudo-registers have been replaced by
           real registers or locations.  Only certain patterns of arguments are accepted. *)

        (* Code-generate a block.  A couple of instructions require the "flow". *)
        fun codeGenBlock(LLBasicBlock{block, flow, ...}, code) =
        let

            (* Turn the icode into machine code.  This produces the code in reverse. *)
            fun codeGenICode([], code) = code

            |   codeGenICode(LLLoadArgument args :: rest, code) =
                    codeGenICode(rest, llLoadArgument args :: code)

                    (* Store to memory *)
            |   codeGenICode(LLStoreArgument args :: rest, code) =
                    codeGenICode(rest, llStoreArgument args :: code)

                (* Exchange two general registers. *)
            |   codeGenICode(LLExchangeRegisters{ regX, regY} :: rest, code) =
                    codeGenICode(rest, XChngRegisters { regX=asGenReg regX, regY=asGenReg regY } :: code)

            |   codeGenICode(LLFunctionCall {callKind, ...} :: rest, code) =
                    codeGenICode(rest, CallFunction callKind :: code)

            |   codeGenICode(LLTailRecursiveCall {callKind, ...} :: rest, code) =
                    codeGenICode(rest, JumpToFunction callKind :: code)

            |   codeGenICode(LLAllocateMemoryOperation { size, flags, dest, saveRegs} :: rest, code) =
                let
                    val toReg = asGenReg dest
                    val preserve = map asGenReg saveRegs

                    (* Allocate memory.  N.B. Instructions are in reverse order. *)
                    fun allocStore{size, flags, output, preserve} =
                    if isX64 andalso flags <> 0w0
                    then
                        [StoreNonWordConst{size=Size8Bit, toStore=Word8.toLargeInt flags, address={offset= ~1, base=output, index=NoIndex}},
                         StoreConstToMemory{toStore=LargeInt.fromInt size, address={offset= ~wordSize, base=output, index=NoIndex}},
                         AllocStore{size=size, output=output, saveRegs=preserve}]
                    else
                    let
                        val lengthWord = IntInf.orb(IntInf.fromInt size, IntInf.<<(Word8.toLargeInt flags, 0w24))
                    in
                        [StoreConstToMemory{toStore=lengthWord, address={offset= ~wordSize, base=output, index=NoIndex}},
                         AllocStore{size=size, output=output, saveRegs=preserve}]
                    end

                    val allocCode =
                        (* If we need to add the profile object *)
                        if addAllocatingFunction
                        then
                            allocStore {size=size+1, flags=Word8.orb(flags, Address.F_profile), output=toReg, preserve=preserve} @
                                [StoreLongConstToMemory{ toStore=profileObject, address={base=toReg, offset=size*wordSize, index=NoIndex}}]
                        else allocStore {size=size, flags=flags, output=toReg, preserve=preserve}
                in
                    codeGenICode(rest, allocCode @ code)
                end

            |   codeGenICode(LLAllocateMemoryVariable{ size, dest, saveRegs} :: rest, code) =
                let
                    val sReg = asGenReg size and dReg = asGenReg dest
                    val _ = sReg <> dReg
                                orelse raise InternalError "codeGenICode-AllocateMemoryVariable"
                    val preserve = map asGenReg saveRegs

                    val allocCode =
                    [
                        (* Store it as the length field. *)
                        StoreRegToMemory{toStore=sReg,
                            address={base=dReg, offset= ~wordSize, index=NoIndex}},
                        (* Untag the length *)
                        ShiftConstant{ shiftType=SHR, output=sReg, shift=0w1},
                        (* Allocate the memory *)
                        AllocStoreVariable{ output=dReg, saveRegs=preserve},
                        (* Compute the number of bytes into dReg. The length in sReg is the number
                           of words as a tagged value so we need to multiply it, add wordSize to
                           include one word for the header then subtract the, multiplied, tag. *)
                        if wordSize = 4
                        then LoadAddress{output=dReg, base=NONE, offset=wordSize-2, index=Index2 sReg }
                        else LoadAddress{output=dReg, base=NONE, offset=wordSize-4, index=Index4 sReg }
                    ]
                in
                    codeGenICode(rest, allocCode @ code)
                end

            |   codeGenICode(LLInitialiseMem{size, addr, init} :: rest, code) =
                let
                    val sReg = asGenReg size and iReg = asGenReg init and aReg = asGenReg addr
                    (* Initialise the memory.  This requires that sReg = ecx, iReg = eax and aReg = edi. *)
                    val _ = sReg = ecx orelse raise InternalError "codeGenICode: InitialiseMem"
                    val _ = iReg = eax orelse raise InternalError "codeGenICode: InitialiseMem"
                    val _ = aReg = edi orelse raise InternalError "codeGenICode: InitialiseMem"
                in
                    codeGenICode(rest, RepeatOperation STOSL :: code)
                end

            |   codeGenICode(LLInitialisationComplete :: rest, code) =
                    codeGenICode(rest, StoreInitialised :: code)

            |   codeGenICode(LLJumpLoop {checkInterrupt, ...} :: rest, code) =
                let
                    val checkCode =
                        case checkInterrupt of
                            NONE => []
                        |   SOME saveRegs => testRegAndTrap (esp, StackOverflowCall, map asGenReg saveRegs)
                in
                    codeGenICode(rest, checkCode @ code)
                end
 
            |   codeGenICode(LLRaiseExceptionPacket :: rest, code) = codeGenICode(rest, RaiseException :: code)

            |   codeGenICode(LLIndexedCaseOperation { testReg, workReg, ...} :: rest, code) =
                let
                    val rReg = asGenReg testReg and wReg = asGenReg workReg
                    val _ = rReg <> wReg orelse raise InternalError "IndexedCaseOperation - same registers"
                    (* This should only be within a block with an IndexedBr flow type. *)
                    val cases =
                        case flow of IndexedBr cases => cases | _ => raise InternalError "codeGenICode: IndexedCaseOperation"
                    val caseLabels = map getBlockLabel cases
                    val startJumpTable = makeLabel()
                    (* Compute the jump address.  The index is a tagged
                       integer so it is already multiplied by 2.  We need to
                       multiply by four to get the correct size. Subtract off the
                       shifted tag. *)
                    val jumpSize = ref JumpSize8
                    (* We use JumpToFunction even though we're not actually going to a new function. *)
                    val resultCode =
                        JumpTable{cases=caseLabels, jumpSize=jumpSize} :: JumpLabel startJumpTable :: JumpToFunction(DirectReg wReg) ::
                            IndexedJumpCalc{ addrReg=wReg, indexReg=rReg, jumpSize=jumpSize } ::
                            LoadLabelAddress{label=startJumpTable, output=wReg} :: code
                in
                    codeGenICode(rest, resultCode)
                end

            |   codeGenICode(LLLockMutable { addr } :: rest, code) =
                    codeGenICode(rest, LockMutableSegment (asGenReg addr) :: code)

            |   codeGenICode(LLWordComparison {arg1=RegisterArg(GenReg r), arg2, ...} :: rest, code) =
                    codeGenICode(rest, ArithToGenReg {opc=CMP, output=r, source=sourceAsGenRegOrMem arg2} :: code)

            |   codeGenICode(LLWordComparison _ :: _, _) =
                    raise InternalError "codeGenICode: TODO WordComparison"

            |   codeGenICode(LLPushExceptionHandler { workReg, ... } :: rest, code) =
                let (* Set up an exception handler. *)
                    (* Although we're pushing this to the stack we need to use LEA on the
                       X86/64 and some arithmetic on the X86/32.  We need a work reg for that. *)
                    val handleReg = asGenReg workReg
                    (* This should only be within a block with a SetHandler flow type. *)
                    val handleLabel =
                        case flow of
                            SetHandler{ handler, ...} => handler
                        |   _ => raise InternalError "codeGenICode: PushExceptionHandler"
                    val labelRef = getBlockLabel handleLabel
                    (* Set up the handler by pushing the old handler to the stack, pushing the
                       entry point and setting the handler address to the current stack pointer. *)
                in
                    codeGenICode(rest,
                        StoreRegToMemory{
                            toStore=esp, address={offset=memRegHandlerRegister, base=ebp, index=NoIndex}} ::
                        PushToStack(RegisterArg handleReg) ::
                        LoadLabelAddress{ label=labelRef, output=handleReg} ::
                        PushToStack(MemoryArg{base=ebp, offset=memRegHandlerRegister, index=NoIndex}) :: code)
                end

            |   codeGenICode(LLPopExceptionHandler { workReg, ... } :: rest, code) =
                let (* Remove an exception handler if no exception was raised. *)
                    val wReg = asGenReg workReg
                in
                    (* The stack pointer has been adjusted to just above the two words that were stored
                       in PushExceptionHandler. *)
                    codeGenICode(rest,
                        StoreRegToMemory{
                            toStore=wReg, address={offset=memRegHandlerRegister, base=ebp, index=NoIndex}} ::
                        PopR wReg ::
                        ResetStack{numWords=1, preserveCC=false} :: code)
                end
 
            |   codeGenICode(LLBeginHandler {workReg, packetReg, ...} :: rest, code) =
                let
                    val _ = asGenReg packetReg = eax orelse raise InternalError "codeGenICode: BeginHandler"
                    val wReg = asGenReg workReg
                in
                    (* The code here is almost the same as PopExceptionHandler.  The only real difference
                       is that PopExceptionHandler needs to pass the result of executing the handled code
                       which could be in any register.  This code needs to transmit the exception packet
                       and that is always in rax. *)
                    codeGenICode(rest,
                        StoreRegToMemory{
                            toStore=wReg, address={offset=memRegHandlerRegister, base=ebp, index=NoIndex}} ::
                        PopR wReg :: ResetStack{numWords=1, preserveCC=false} ::
                        MoveToRegister{ source=MemoryArg{base=ebp, offset=memRegHandlerRegister, index=NoIndex}, output=esp } :: code)
                end

            |   codeGenICode(LLReturnResultFromFunction { numStackArgs, ... } :: rest, code) =
                    codeGenICode(rest, ReturnFromFunction numStackArgs :: code)

            |   codeGenICode(
                    LLArithmeticFunction{
                        oper, resultReg=(GenReg resReg), operand1=RegisterArg(GenReg op1Reg),
                        operand2, ...} :: rest, code) =
                let
                    val _ = resReg = op1Reg orelse raise InternalError "codeGenICode: ArithmeticFunction"
                in
                    codeGenICode(rest, ArithToGenReg { opc=oper, output=resReg, source=sourceAsGenRegOrMem operand2 } :: code)
                end

            |   codeGenICode(LLArithmeticFunction _ :: _, _) =
                    raise InternalError "codeGenICode: TODO codeGenICode - ArithmeticFunction"

            |   codeGenICode(LLTestTagBit {arg = RegisterArg reg, ...} :: rest, code) =
                    codeGenICode(rest, TestTagR(asGenReg reg) :: code)

            |   codeGenICode(LLTestTagBit {arg = MemoryArg {offset, base, index=NoIndex}, ...} :: rest, code) =
                    codeGenICode(rest, TestByteMem{base=base, offset=offset, bits=0w1} :: code)

            |   codeGenICode(LLTestTagBit _ :: _, _) = raise InternalError "codeGenICode: TestTagBit"

            |   codeGenICode(LLPushValue arg :: rest, code) = codeGenICode(rest, llPushValue arg :: code)

            |   codeGenICode(LLResetStackPtr {numWords, preserveCC} :: rest, code) =
                (
                    numWords >= 0 orelse raise InternalError "codeGenICode: ResetStackPtr - negative offset";
                    codeGenICode(rest, ResetStack{numWords=numWords, preserveCC=preserveCC} :: code)
                )

            |   codeGenICode(LLTagValue _ :: _, _) =
                    raise InternalError "codeGenICode: TODO TagValue"

            |   codeGenICode(LLUntagValue _ :: _, _) =
                    raise InternalError "codeGenICode: TODO UntagValue"

            |   codeGenICode(LLLoadEffectiveAddress { base, offset, index, dest } :: rest, code) =
                let
                    val bReg = Option.map asGenReg base
                    val indexR = index
                in
                    codeGenICode(rest, LoadAddress{ output=asGenReg dest, offset=offset, base=bReg, index=indexR } :: code)
                end

            |   codeGenICode(
                    LLShiftOperation{ shift, resultReg, operand, shiftAmount=NonAddressConstArg shiftValue, ...} :: rest, code) =
                let
                    val resReg = asGenReg resultReg and opReg = argAsGenReg operand
                    val _ = resReg = opReg orelse raise InternalError "codeGenICode: ShiftOperation"
                in
                    codeGenICode(rest, ShiftConstant{ shiftType=shift, output=resReg, shift=Word8.fromLargeInt shiftValue } :: code)
                end

            |   codeGenICode(LLShiftOperation { shift, resultReg, operand, shiftAmount, ...} :: rest, code) =
                let
                    val resReg = asGenReg resultReg and opReg = argAsGenReg operand
                    val _ = resReg = opReg orelse raise InternalError "codeGenICode: ShiftOperation"
                    (* The amount to shift must be in ecx.  The shift is masked to 5 or 6 bits so we have to
                       check for larger shift values at a higher level. *)
                    val _ = argAsGenReg shiftAmount = ecx orelse raise InternalError "codeGenICode: ShiftOperation"
                in
                    codeGenICode(rest, ShiftVariable{ shiftType=shift, output=resReg } :: code)
                end

            |   codeGenICode(LLMultiplication { resultReg, operand1, operand2=MemoryArg{offset, base, index=NoIndex}, ... } :: rest, code) =
                let
                    val resReg = asGenReg resultReg and op1Reg = argAsGenReg operand1 and baseReg = base
                    val _ = resReg = op1Reg orelse raise InternalError "codeGenICode: Multiplication"
                in
                    codeGenICode(rest, MultiplyRM { base=baseReg, offset=offset, output=resReg } :: code)
                end

            |   codeGenICode(LLMultiplication { resultReg, operand1, operand2, ... } :: rest, code) =
                let
                    val resReg = asGenReg resultReg and op1Reg = argAsGenReg operand1 and op2Reg = argAsGenReg operand2
                    val _ = resReg = op1Reg orelse raise InternalError "codeGenICode: Multiplication"
                in
                    codeGenICode(rest, MultiplyRR { source=op2Reg, output=resReg } :: code)
                end

            |   codeGenICode(LLDivision { isSigned, dividend, divisor, quotient, remainder } :: rest, code) =
                let
                    val dividendReg = asGenReg dividend and divisorReg = argAsGenReg divisor
                    and quotientReg = asGenReg quotient and remainderReg = asGenReg remainder
                    val _ = dividendReg = eax orelse raise InternalError "codeGenICode: Division"
                    val _ = divisorReg <> eax andalso divisorReg <> edx orelse raise InternalError "codeGenICode: Division"
                    val _ = quotientReg = eax orelse raise InternalError "codeGenICode: Division"
                    val _ = remainderReg = edx orelse raise InternalError "codeGenICode: Division"
                    (* rdx needs to be set to the high order part of the dividend.  For signed
                       division that means sign-extending rdx, for unsigned division we clear it. *)
                    val setRDX =
                        if isSigned then SignExtendForDivide
                        else ArithToGenReg{ opc=XOR, output=edx, source=RegisterArg edx }
                in
                    codeGenICode(rest, DivideAccR {arg=divisorReg, isSigned=isSigned} :: setRDX :: code)
                end

            |   codeGenICode(LLAtomicExchangeAndAdd{ base, source } :: rest, code) =
                let
                    val baseReg = asGenReg base and outReg = asGenReg source
                in
                    codeGenICode(rest, AtomicXAdd{base=baseReg, output=outReg} :: code)
                end

            |   codeGenICode(LLCompareByteVectors { vec1Addr, vec2Addr, length, ... } :: rest, code) =
                let
                    (* The arguments must be in specific registers. *)
                    val _ = asGenReg vec1Addr = esi orelse raise InternalError "CompareByteVectors: esi"
                    val _ = asGenReg vec2Addr = edi orelse raise InternalError "CompareByteVectors: edi"
                    val _ = asGenReg length = ecx orelse raise InternalError "CompareByteVectors: ecx"
                in
                    codeGenICode(rest, RepeatOperation CMPSB :: code)
                end

            |   codeGenICode(LLBlockMove { srcAddr, destAddr, length, isByteMove } :: rest, code) =
                let
                    (* The arguments must be in specific registers. *)
                    val _ = asGenReg srcAddr = esi orelse raise InternalError "BlockMove: esi"
                    val _ = asGenReg destAddr = edi orelse raise InternalError "BlockMove: edi"
                    val _ = asGenReg length = ecx orelse raise InternalError "BlockMove: ecx"
                in
                    codeGenICode(rest, RepeatOperation(if isByteMove then MOVSB else MOVSL) :: code)
                end

            |   codeGenICode(
                    LLCompareFloatingPt {
                        arg1=RegisterArg(FPReg fpReg),
                        arg2=MemoryArg{offset, base=baseReg, index=NoIndex}, ... } :: rest, code) =
                let
                    val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: CompareFloatingPt not fp0"
                    (* This currently pops the value. *)
                    (*val _ = fpMode = FPModeX87 orelse raise InternalError "codeGenICode: FCOMP in SSE2 mode"*)
                in
                    codeGenICode(rest, FPArithMemory{opc=FCOMP, base=baseReg, offset=offset} :: code)
                end

            |   codeGenICode(LLCompareFloatingPt {arg1=RegisterArg(XMMReg xmmReg), arg2, ... } :: rest, code) =
                    codeGenICode(rest, XMMArith { opc= SSE2Comp, output=xmmReg, source=sourceAsXMMRegOrMem arg2} :: code)

            |   codeGenICode(LLCompareFloatingPt _ :: _, _) =
                    raise InternalError "codeGenICode: CompareFloatingPt: TODO"

            |   codeGenICode(LLX87FPGetCondition { dest, ... } :: rest, code) =
                let
                    val _ = asGenReg dest = eax orelse raise InternalError "codeGenICode: GetFloatingPtCondition not eax"
                    (* This currently pops the value. *)
                    (*val _ = fpMode = FPModeX87 orelse raise InternalError "codeGenICode: FPStatusToEAX in SSE2 mode"*)
                in
                    codeGenICode(rest, FPStatusToEAX :: code)
                end

            |   codeGenICode(
                    LLX87FPArith {
                        opc, resultReg=(FPReg fpResReg), arg1=RegisterArg(FPReg fpArgReg),
                        arg2=MemoryArg{offset, base=baseReg, index=NoIndex} } :: rest, code) =
                let
                    val _ = fpResReg = fp0 orelse raise InternalError "codeGenICode: FloatingPointArith not fp0"
                    val _ = fpArgReg = fp0 orelse raise InternalError "codeGenICode: FloatingPointArith not fp0"
                in
                    codeGenICode(rest, FPArithMemory{opc=opc, base=baseReg, offset=offset} :: code)
                end

            |   codeGenICode(LLX87FPArith _ :: _, _) =
                    raise InternalError "codeGenICode: X87FPArith: TODO"

            |   codeGenICode(
                    LLSSE2FPArith {
                        opc, resultReg=(XMMReg xmmResReg), arg1=RegisterArg(XMMReg xmmArgReg), arg2 } :: rest, code) =
                let
                    val _ = xmmResReg = xmmArgReg orelse raise InternalError "codeGenICode: FloatingPointArith - different regs"
                    (* xorpd and andpd require 128-bit arguments with 128-bit alignment. *)
                    val _ =
                        case (opc, arg2) of
                            (SSE2Xor, RegisterArg _) => ()
                        |   (SSE2Xor, _) => raise InternalError "codeGenICode - SSE2Xor not in register"
                        |   (SSE2And, RegisterArg _) => ()
                        |   (SSE2And, _) => raise InternalError "codeGenICode - SSE2And not in register"
                        |   _ => ()
                in
                    codeGenICode(rest, XMMArith{ opc=opc, output=xmmResReg, source=sourceAsXMMRegOrMem arg2} :: code)
                end

            |   codeGenICode(LLSSE2FPArith _ :: _, _) =
                    raise InternalError "codeGenICode: SSE2FPArith: TODO"

            |   codeGenICode(LLX87FPUnaryOps {fpOp, dest=(FPReg fpResReg), source=RegisterArg(FPReg fpArgReg)} :: rest, code) =
                let
                    val _ = fpResReg = fp0 orelse raise InternalError "codeGenICode: X87FPUnaryOps not fp0"
                    val _ = fpArgReg = fp0 orelse raise InternalError "codeGenICode: X87FPUnaryOps not fp0"
                in
                    codeGenICode(rest, FPUnary fpOp :: code)
                end

            |   codeGenICode(LLX87FPUnaryOps _ :: _, _) =
                    raise InternalError "codeGenICode: FloatingPointNeg: TODO"

            |   codeGenICode(LLFloatFixedInt { dest=(XMMReg xmmResReg), source=RegisterArg(GenReg srcReg) } :: rest, code) =
                    codeGenICode(rest, XMMConvertFromInt{ output=xmmResReg, source=srcReg} :: code)

            |   codeGenICode(LLFloatFixedInt { dest=(FPReg fpReg), source=MemoryArg{base, offset, index=NoIndex} } :: rest, code) =
                let
                    val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: FloatFixedInt not fp0"
                in
                    codeGenICode(rest, FPLoadInt{ base=base, offset=offset} :: code)
                end

            |   codeGenICode(LLFloatFixedInt _ :: _, _) =
                    raise InternalError "codeGenICode: FloatFixedInt: TODO"
        in
            codeGenICode(block, code)
        end (* codeGenBlock *)
        
        val minStackCheck = 20
        val inputRegisters = argRegsUsed @ (if hasFullClosure then [GenReg edx] else [])
        val saveRegs = List.mapPartial(fn GenReg r => SOME r | _ => NONE) inputRegisters
        val preludeCode =
            if stackRequired >= minStackCheck
            then
            let
                (* Compute the necessary amount in edi and compare that. *)
                val stackByteAdjust = ~wordSize * stackRequired
                val testEdiCode =
                    testRegAndTrap (edi, StackOverflowCallEx, saveRegs)
            in
                (* N.B. In reverse order. *)
                testEdiCode @ [LoadAddress{output=edi, base=SOME esp, index=NoIndex, offset=stackByteAdjust}]
            end
     
            else testRegAndTrap (esp, StackOverflowCall, saveRegs)

        val newCode = codeCreate (functionName, profileObject, debugSwitches) 
        
        local
            (* processed - set to true when a block has been processed. *)
            val processed = Array.array(numBlocks, false)
            fun haveProcessed n = Array.sub(processed, n)
            
            (* Find the blocks that reference this one.  This isn't essential but
               allows us to try to generate blocks in the order of the control
               flow.  This in turn may allow us to use short branches rather
               than long ones. *)
            val labelRefs = Array.array(numBlocks, [])
            
            fun setReferences(fromLabel, LLBasicBlock{ flow, ...}) =
            let
                val refs =
                    case flow of
                        ExitCode => []
                    |   Unconditional lab => [lab]
                    |   Conditional{trueJump, falseJump, ... } => [trueJump, falseJump]
                    |   IndexedBr labs => labs
                    |   SetHandler { handler, continue } => [handler, continue]
                    |   UnconditionalHandle _ => []
                    |   ConditionalHandle { continue, ...} => [continue]
                
                fun setRefs toLabel =
                    Array.update(labelRefs, toLabel, fromLabel :: Array.sub(labelRefs, toLabel))
            in
                List.app setRefs refs
            end

            val () = Array.appi setReferences resultBlocks
            
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
                        |   Unconditional dest => [UncondBranch(getBlockLabel dest)]
                        |   Conditional { condition, trueJump, falseJump, ...} =>
                                [
                                    UncondBranch(getBlockLabel falseJump),
                                    ConditionalBranch{test=condition, predict=PredictNeutral, label=getBlockLabel trueJump}
                                ]
                        |   SetHandler { continue, ...} => [UncondBranch(getBlockLabel continue)]
                        |   UnconditionalHandle _ => []
                        |   ConditionalHandle { continue, ...} => [UncondBranch(getBlockLabel continue)]
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
                                    then SOME dest
                                    else NONE
                            |   Conditional {trueJump, falseJump, ...} =>
                                    (* Try the falseJump first - this is the usual case.  If that fails
                                       try the trueJump. *)
                                    if not (haveProcessed falseJump) andalso available falseJump
                                    then SOME falseJump
                                    else if not (haveProcessed trueJump) andalso available trueJump
                                    then SOME trueJump
                                    else NONE
                           |    SetHandler { continue, ... } =>
                                    (* We want the continuation if possible.  We'll need a
                                       branch round the handler so that won't help. *)
                                    if not (haveProcessed continue) andalso available continue
                                    then SOME continue
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
                                        SOME c => c
                                    |   NONE => head
                    end
                        
                    val () = Array.update(processed, picked, true)

                    (* Code to terminate the previous block. *)
                    val startCode =
                        case lastFlow of
                            ExitCode => []
                        |   IndexedBr _ => []
                        |   UnconditionalHandle _ => []
                        |   Unconditional dest =>
                                if dest = picked then [] else [UncondBranch(getBlockLabel dest)]
                        |   ConditionalHandle { continue, ...} =>
                                if continue = picked then [] else [UncondBranch(getBlockLabel continue)]
                        |   SetHandler { continue, ... } =>
                                if continue = picked then [] else [UncondBranch(getBlockLabel continue)]
                        |   Conditional { condition, trueJump, falseJump, ...} =>
                            if picked = falseJump (* Usual case. *)
                            then [ConditionalBranch{test=condition, predict=PredictNeutral, label=getBlockLabel trueJump}]
                            else if picked = trueJump
                            then (* We have a jump to the true condition. Invert the jump.
                                    This is more than an optimisation.  Because this immediately precedes the
                                    true block we're not going to generate a label. *)
                            let
                                val revTest =
                                    case condition of
                                        JE  => JNE  |   JNE => JE   |   JA  => JNA  |   JB  => JNB  |   JNA => JA
                                    |   JNB => JB   |   JL  => JGE  |   JG  => JLE  |   JLE => JG   |   JGE => JL
                                    |   JO  => JNO  |   JNO => JO   |   JP  => JNP  |   JNP => JP
                            in
                                [ConditionalBranch{test=revTest, predict=PredictNeutral, label=getBlockLabel falseJump}]
                            end
                            else
                            [
                                UncondBranch(getBlockLabel falseJump),
                                ConditionalBranch{test=condition, predict=PredictNeutral, label=getBlockLabel trueJump}
                            ]

                    val blockToCode as LLBasicBlock{ flow, ...} = Array.sub(resultBlocks, picked)

                    (* Code-generate the body with the code we've done so far
                       at the end.  Add a label at the start if necessary. *)
                    local
                        (* If the previous block dropped through to this and this was
                           the only reference then we don't need a label. *)
                        fun onlyJumpingHere lab =
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
                        val startLabel = if noLabel then [] else [JumpLabel(getBlockLabel picked)]
                    end

                    val bodyCode = codeGenBlock(blockToCode, startLabel @ startCode @ code)

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
        in
            val ops = genCode([0], ExitCode, preludeCode)
        end
    in
        X86OPTIMISE.generateCode{code=newCode, ops=List.rev ops, labelCount= !outputLabelCount}
    end

    val nGenRegs = List.length generalRegisters

    structure Sharing =
    struct
        type intSet             = intSet
        and extendedBasicBlock  = extendedBasicBlock
        and regProperty         = regProperty
        and reg                 = reg
    end

end;
