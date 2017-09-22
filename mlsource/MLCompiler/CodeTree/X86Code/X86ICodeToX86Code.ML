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
    structure INTSET: INTSETSIG
    structure PRETTY: PRETTYSIG

    structure STRONGLY:
        sig
            val stronglyConnectedComponents: {nodeAddress: 'a -> int, arcs: 'a -> int list } -> 'a list -> 'a list list
        end
    
    sharing X86CODE.Sharing = ICODE.Sharing = X86OPTIMISE.Sharing = IDENTIFY.Sharing = INTSET
): X86ICODEGENERATESIG =
struct
    open IDENTIFY
    open ICODE

    open X86CODE

    open Address
    
    exception InternalError = Misc.InternalError

    (* Temporary datatype to bridge the gap between the concrete icode and the
       direct generation of the low-level assembly code. *)
    datatype llIcode =
        LLLoadArgument of { source: reg regOrMemoryArg, dest: reg, kind: moveKind }
    |   LLStoreArgument of { source: reg regOrMemoryArg, base: genReg, offset: int, index: indexType, kind: moveKind }
    |   LLExchange of { reg: reg, arg: reg regOrMemoryArg }
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
     
    fun icodeToX86Code{blocks, functionName, stackRequired, argRegsUsed, hasFullClosure, debugSwitches, allocatedRegisters, ...} =
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

        val numBlocks = Vector.length blocks
        

        
        fun getAllocatedReg r = Vector.sub(allocatedRegisters, r)

        fun codeExtArgument(RegisterArgument(PReg r)) = RegisterArg(getAllocatedReg r)
        |   codeExtArgument(AddressConstant m) = AddressConstArg m
        |   codeExtArgument(IntegerConstant i) = NonAddressConstArg i
        |   codeExtArgument(MemoryLocation{base=PReg bReg, offset, index, cache=NONE}) =
                MemoryArg{base=asGenReg(getAllocatedReg bReg), offset=offset, index=codeExtIndex index}
        |   codeExtArgument(MemoryLocation{cache=SOME(PReg r), ...}) =
                RegisterArg(getAllocatedReg r)
        |   codeExtArgument(StackLocation{wordOffset, cache=NONE, ...}) =
                MemoryArg{base=esp, offset=wordOffset*wordSize, index=NoIndex}
        |   codeExtArgument(StackLocation{cache=SOME(PReg r), ...}) = RegisterArg(getAllocatedReg r)

        and codeExtIndex NoMemIndex = NoIndex
        |   codeExtIndex(MemIndex1(PReg r)) = Index1(asGenReg(getAllocatedReg r))
        |   codeExtIndex(MemIndex2(PReg r)) = Index2(asGenReg(getAllocatedReg r))
        |   codeExtIndex(MemIndex4(PReg r)) = Index4(asGenReg(getAllocatedReg r))
        |   codeExtIndex(MemIndex8(PReg r)) = Index8(asGenReg(getAllocatedReg r))

        fun moveRR{src, dst, kind} = LLLoadArgument{source=RegisterArg src, dest=dst, kind=kind}

        fun moveIfNecessary{src, dst, kind} =
            if src = dst then [] else [moveRR{src=src, dst=dst, kind=kind}]
       
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
        fun moveMultipleValues(moves, workReg: reg option) =
        let
            val _ =
                if List.exists(fn {dst=StackDest _, ...} => true | _ => false) moves andalso not(isSome workReg) then raise InternalError "no work reg" else ()

            fun moveValues [] = [] (* We're done. *)

            |   moveValues arguments =
                let
                    (* stronglyConnectedComponents does two things.  It detects loops where
                       it's not possible to move items without breaking the loop but more
                       importantly it orders the dependencies so that if there are no loops we
                       can load the source and store it in the destination knowing that
                       we won't overwrite anything we might later need. *)
                    
                    val ordered = stronglyConnected arguments
                    
                    fun moveEachValue [] = []

                    |   moveEachValue ([{dst=RegDest reg, src as OtherSource(RegisterArg r)}] :: rest) =
                            (* Source and dest are both regs - only move if they're different. *)
                            if r = reg
                            then moveEachValue rest
                            else LLLoadArgument{source=sourceToX86Code src, dest=reg, kind=MoveWord} ::
                                    moveEachValue rest

                    |   moveEachValue ([{dst=RegDest reg, src}] :: rest) =
                            (* Load from store or a constant. *)
                            LLLoadArgument{source=sourceToX86Code src, dest=reg, kind=MoveWord} ::
                                moveEachValue rest

                    |   moveEachValue ([{dst=StackDest _, src=OtherSource(MemoryArg _ )}] :: _) =
                            raise InternalError "moveEachValue - MemoryArgument"

                    |   moveEachValue ([{dst=StackDest addr, src as StackSource wordOffset}] :: rest) =
                            (* Copy a stack location - needs a load and store unless the address is the same. *)
                            if addr = wordOffset
                            then moveEachValue rest
                            else
                            let
                                val workReg = valOf workReg
                            in
                                LLLoadArgument{source=sourceToX86Code src, dest=workReg, kind=MoveWord} ::
                                LLStoreArgument{source=RegisterArg(workReg), base=esp, index=NoIndex,
                                                offset = addr*wordSize, kind=MoveWord} ::
                                moveEachValue rest
                            end

                    |   moveEachValue ([{dst=StackDest addr, src}] :: rest) =
                            (* Store from a register or a constant. *)
                            LLStoreArgument{
                                source=sourceToX86Code src, base=esp, index=NoIndex, offset = addr*wordSize, kind=MoveWord} ::
                                    moveEachValue rest

                    |   moveEachValue((cycle as first :: _ :: _) :: rest) =
                        (* We have a cycle. *)
                        let
                            (* We need to exchange some of the arguments.  Doing an exchange here will
                               set the destination with the correct source.  However we have to process
                               every subsequent entry with the swapped registers.  That may well mean that
                               one of those entries becomes trivial.  Using XCHG means that we can move
                               N registers in N-1 exchanges.
                               We also need to rerun stronglyConnectedComponents on at least the rest of
                               this cycle.  It's easiest to flatten the rest and do everything. *)
                            (* Try to find either a register-register move or a register-stack move.
                               If not use the first.  If there's a stack-register move there will
                               also be a register-stack so we don't need to look for both. *)
                            val {dst=selectDst, src=selectSrc} =
                                case List.find(fn {src=OtherSource(RegisterArg _), dst=RegDest _} => true | _ => false) cycle of
                                    SOME found => found
                                |   _ =>
                                    (
                                        case List.find(fn {dst=RegDest _, ...} => true | _ => false) cycle of
                                            SOME found => found
                                        |   NONE => first
                                    )
                            (* This includes this entry but after the swap we'll eliminate it. *)
                            val flattened = List.foldl(fn (a, b) => a @ b) [] (cycle :: rest)
                            val destAsSource =
                                case selectDst of
                                    RegDest reg => OtherSource(RegisterArg reg)
                                |   StackDest s => StackSource s

                            (* Source is not an equality type.  We can't currently handle the
                               situation where the source is a memory location. *)
                            fun match(OtherSource(RegisterArg r1), OtherSource(RegisterArg r2)) = r1 = r2
                            |   match(StackSource s1, StackSource s2) = s1 = s2
                            |   match(OtherSource(MemoryArg _), _) = raise InternalError "moveEachValue: cycle"
                            |   match _ = false
                            
                            fun swapSources{src, dst} =
                                if match(src, selectSrc) then {src=destAsSource, dst=dst}
                                else if match(src, destAsSource) then {src=selectSrc, dst=dst}
                                else {src=src, dst=dst}
                            
                            (* Try to use register to register exchange if we can.
                               A register-to-memory exchange involves a bus lock and we'd
                               like to avoid that. *)
                            val exchangeCode =
                                case (selectDst, selectSrc) of
                                    (RegDest regA, OtherSource(RegisterArg regB)) =>
                                        [LLExchange{reg=regA, arg=RegisterArg regB}]

                                |   (RegDest regA, src as StackSource addr) =>
                                    let
                                        val workReg = valOf workReg
                                    in
                                        [LLLoadArgument{source=sourceToX86Code src, dest=workReg, kind=MoveWord},
                                        LLExchange{reg=regA, arg=RegisterArg workReg},
                                        LLStoreArgument{source=RegisterArg(workReg), base=esp, index=NoIndex,
                                                offset = addr*wordSize, kind=MoveWord}]
                                    end

                                |   (StackDest addr, OtherSource(RegisterArg regA)) =>
                                    let
                                        (* This doesn't actually occur because we always find the case above. *)
                                        val workReg = valOf workReg
                                    in
                                        [LLLoadArgument{
                                            source=MemoryArg{base=esp, offset=addr*wordSize, index=NoIndex}, dest=workReg, kind=MoveWord},
                                        LLExchange{reg=regA, arg=RegisterArg workReg},
                                        LLStoreArgument{source=RegisterArg(workReg), base=esp, index=NoIndex,
                                                offset = addr*wordSize, kind=MoveWord}]
                                    end

                                |   (StackDest addr1, StackSource addr2) =>
                                    let
                                        val workReg = valOf workReg
                                    in
                                        [LLLoadArgument{
                                            source=MemoryArg{base=esp, offset=addr1*wordSize, index=NoIndex}, dest=workReg, kind=MoveWord},
                                         LLExchange{reg=workReg, arg=MemoryArg{base=esp, offset=addr2*wordSize, index=NoIndex}},
                                         LLStoreArgument{source=RegisterArg(workReg), base=esp, index=NoIndex,
                                                offset = addr1*wordSize, kind=MoveWord}]
                                    end
                                
                                |   _ => raise InternalError "moveEachValue: cycle"
                                    
                        in
                            exchangeCode @ moveValues(List.map swapSources flattened)
                        end

                    |   moveEachValue(([]) :: _) = (* This should not happen - avoid warning. *)
                            raise InternalError "moveEachValue - empty set"
                in
                    moveEachValue ordered
                end
        in
            moveValues moves
        end

        (* Where we have multiple specific registers as either source or
           destination there is the potential that a destination register
           if currently in use as a source. *) 
        fun moveMultipleRegisters regPairList =
        let
            val regPairsAsDests =
                List.map(fn {src, dst} => {src=OtherSource(RegisterArg src), dst=RegDest dst}) regPairList
        in
            moveMultipleValues(regPairsAsDests, NONE)
        end

        (* Tail recursive calls are complicated because we generally have to overwrite the existing stack.
           That means storing the arguments in the right order to avoid overwriting a
           value that we are using for a different argument. *)

        fun codeTailCall(callKind, arguments: {dst: destinations, src: llsource} list, stackAdjust, workReg) =
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
            LLPushValue{arg=sourceToX86Code argM1} :: codeTailCall(callKind, renumberArgs arguments, stackAdjust+1, workReg)
        end
        else
        let
            val loadArgs = moveMultipleValues(arguments, SOME workReg)

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
    
        (* Return the register part of a cached item.  We must still, unfortunately, ensure that a register is
           allocated for base registers because we're assuming that a register is allocated on the last
           occurrence and this might be it. *)
        fun decache(StackLocation{cache=SOME r, ...}) = RegisterArgument r
        |   decache(MemoryLocation{cache=SOME r, base=PReg bReg, ...}) =
                (getAllocatedReg bReg; RegisterArgument r)
        |   decache arg = arg
        
        fun codeExtended({instr=LoadArgument{source, dest=PReg dreg, kind}, ...}, code) =
            let
                val realDestReg = getAllocatedReg dreg
            in
                case decache source of
                    RegisterArgument(PReg sreg) =>
                    (* Register to register move.  Try to use the same register for the source as the destination
                       to eliminate the instruction. *)
                        (* If the source is the same as the destination we don't need to do anything. *)
                        moveIfNecessary{src=getAllocatedReg sreg, dst=realDestReg, kind=kind} @ code

                |   source => (* Loads of constants or from an address. *)
                        LLLoadArgument{source=codeExtArgument source, dest=realDestReg, kind=kind} :: code
            end

        |   codeExtended({instr=StoreArgument{ source, base=PReg bReg, offset, index, kind }, ...}, code) =
            (
                case (decache source, kind) of
                    (RegisterArgument(PReg sReg), MoveByte) =>
                    if isX64
                    then
                        LLStoreArgument{
                            source=codeExtArgument source, base=asGenReg(getAllocatedReg bReg), offset=offset, index=codeExtIndex index, kind=MoveByte}
                                :: code
                    else
                    (* This is complicated on X86/32.  We can't use edi or esi for the store registers.  Instead
                       we reserve ecx (see special case in "identify") and use that if we have to. *)
                    let
                        val realStoreReg = getAllocatedReg sReg
                        val (moveCode, storeReg) =
                            if realStoreReg = GenReg edi orelse realStoreReg = GenReg esi
                            then (moveIfNecessary{src=realStoreReg, dst=GenReg ecx, kind=MoveWord}, GenReg ecx)
                            else ([], realStoreReg)
                    in
                        moveCode @
                        (LLStoreArgument{
                            source=RegisterArg storeReg, base=asGenReg(getAllocatedReg bReg), offset=offset, index=codeExtIndex index, kind=MoveByte}
                                :: code)
                    end
                    
                |   _ =>
                        LLStoreArgument{
                            source=codeExtArgument source, base=asGenReg(getAllocatedReg bReg), offset=offset, index=codeExtIndex index, kind=kind}
                                :: code
            )

        |   codeExtended({instr=LoadMemReg { offset, dest=PReg pr}, ...}, code) =
            (* Load from the "memory registers" pointed at by ebp. *)
            let
                val regResult = getAllocatedReg pr
            in
                LLLoadArgument{source=MemoryArg{base=ebp, offset=offset, index=NoIndex}, dest=regResult, kind=MoveWord} :: code
            end

        |   codeExtended({instr=BeginFunction{regArgs, ...}, ...}, code) =
            let
                fun mkPair(PReg pr, rr) =
                    {src=rr,dst=getAllocatedReg pr}
                val regPairs = List.map mkPair regArgs
            in
                moveMultipleRegisters regPairs @ code
            end

        |   codeExtended({instr=TailRecursiveCall{callKind, regArgs=oRegArgs, stackArgs=oStackArgs, stackAdjust, stackOffset, workReg=PReg wReg}, ...}, code) =
            let
                val regArgs = List.map (fn (arg, reg) => (decache arg, reg)) oRegArgs
                and stackArgs = List.map(fn {src, stack } => {src=decache src, stack=stack}) oStackArgs
                val workReg = getAllocatedReg wReg
                
                (* We must leave stack entries as stack entries for the moment. *)
                fun codeArg(StackLocation{wordOffset, cache=NONE, ...}) = StackSource wordOffset
                |   codeArg arg = OtherSource(codeExtArgument arg)

                val extStackArgs = map (fn {stack, src} => {dst=StackDest(stack+stackOffset), src=codeArg src}) stackArgs
                val extRegArgs = map (fn (a, r) => {src=codeArg a, dst=RegDest r}) regArgs
            in
                codeTailCall(callKind, extStackArgs @ extRegArgs, stackAdjust+stackOffset, workReg) @ code
            end

        |   codeExtended({instr=FunctionCall{callKind, regArgs=oRegArgs, stackArgs=oStackArgs, dest=PReg dReg}, ...}, code) =
            let
                val regArgs = List.map (fn (arg, reg) => (decache arg, reg)) oRegArgs
                and stackArgs = List.map decache oStackArgs
                
                val destReg = getAllocatedReg dReg
                
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
                        LLPushValue {arg=codeExtArgument adjusted} :: pushStackArgs(args, argNum+1)
                    end
                val pushedArgs = pushStackArgs(stackArgs, 0)
                (* We have to adjust any stack offset to account for the arguments we've pushed. *)
                val numStackArgs = List.length stackArgs
                
                (* We don't currently allow the arguments to be memory locations and instead
                   force them into registers.  That may be simpler especially if we can get the
                   values directly into the required register. *)
                fun getRegArgs(RegisterArgument(PReg pr), reg) =
                        SOME{dst=reg, src=getAllocatedReg pr}
                |   getRegArgs(StackLocation {cache=SOME(PReg pr), ...}, reg) =
                        SOME{dst=reg, src=getAllocatedReg pr}
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
                        moveIfNecessary{dst=destReg, src=GenReg eax, kind=MoveWord}) @ code
            end

        |   codeExtended({instr=AllocateMemoryOperation{ size, flags, dest=PReg dReg, saveRegs}, ...}, code) =
            let
                val preserve = List.map(fn (PReg r) => getAllocatedReg r) saveRegs
            in
                LLAllocateMemoryOperation{ size=size, flags=flags, dest=getAllocatedReg dReg, saveRegs=preserve} :: code
            end

        |   codeExtended({instr=AllocateMemoryVariable{size=PReg sReg, dest=PReg dReg, saveRegs}, ...}, code) =
            let
                (* Simple case - no initialiser. *)
                val preserve = List.map(fn (PReg r) => getAllocatedReg r) saveRegs
                val destReg = getAllocatedReg dReg
            in
                LLAllocateMemoryVariable{size=getAllocatedReg sReg, dest=destReg, saveRegs=preserve} :: code
            end

        |   codeExtended({instr=InitialiseMem{size=PReg sReg, addr=PReg aReg, init=PReg iReg}, ...}, code) =
            let
                (* We are going to use rep stosl/q to set the memory.
                   That requires the length to be in ecx, the initialiser to be in eax and
                   the destination to be edi. *)
                val realAddrReg = getAllocatedReg aReg
                val realInitReg = getAllocatedReg iReg
                val realSizeReg = getAllocatedReg sReg
            in
                moveMultipleRegisters[
                    {src=realInitReg, dst=GenReg eax}, {src=realSizeReg, dst=GenReg ecx}, {src=realAddrReg, dst=GenReg edi}] @
                    LLInitialiseMem{size=GenReg ecx, addr=GenReg edi, init=GenReg eax} :: code
            end

        |   codeExtended({instr=InitialisationComplete, ...}, code) = LLInitialisationComplete :: code

        |   codeExtended({instr=JumpLoop{regArgs, stackArgs, checkInterrupt, workReg}, ...}, code) =
            let
                val workReg = Option.map (fn PReg r => getAllocatedReg r) workReg
                (* TODO: Make the sources and destinations "friends". *)
                (* We must leave stack entries as stack entries for the moment as with TailCall. *)
                fun codeArg(StackLocation{wordOffset, ...}) = StackSource wordOffset
                |   codeArg arg = OtherSource(codeExtArgument arg)
                val extStackArgs = map (fn (src, stack, _) => {dst=StackDest stack, src=codeArg src}) stackArgs
                val extRegArgs = map (fn (a, PReg r) => {src=codeArg a, dst=RegDest(getAllocatedReg r)}) regArgs
                val check = Option.map(List.map(fn (PReg r) => getAllocatedReg r)) checkInterrupt
            in
                moveMultipleValues(extStackArgs @ extRegArgs, workReg) @
                    (LLJumpLoop{checkInterrupt=check} :: code)
            end

        |   codeExtended({instr=RaiseExceptionPacket{ packetReg=PReg preg }, ...}, code) =
            let
                (* The argument must be put into rax. *)
                val argReg = getAllocatedReg preg
            in
                moveIfNecessary{src=argReg, dst=raxAsArg, kind=MoveWord} @ LLRaiseExceptionPacket :: code
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
                val addrReg = getAllocatedReg aReg
                val loadAddr =
                    if stackOffset = 0
                    then LLLoadArgument{source=RegisterArg(GenReg esp), dest=addrReg, kind=MoveWord}
                    else LLLoadEffectiveAddress { base=SOME(GenReg esp), offset=stackOffset*wordSize, index=NoIndex, dest=addrReg }
            in
                loadAddr :: code
            end

        |   codeExtended({instr=IndexedCaseOperation{testReg=PReg tReg, workReg=PReg wReg}, ...}, code) =
            let
                val testReg = getAllocatedReg(tReg)
                val workReg = getAllocatedReg(wReg)
                val _ = testReg <> workReg orelse raise InternalError "IndexedCaseOperation - same registers"
            in
                LLIndexedCaseOperation{ testReg=testReg, workReg=workReg} :: code
            end

        |   codeExtended({instr=LockMutable{addr=PReg pr}, ...}, code) =
                LLLockMutable{addr=getAllocatedReg(pr)} :: code

        |   codeExtended({instr=WordComparison{ arg1 as RegisterArgument _, arg2, ... }, ...}, code) =
                LLWordComparison{ arg1=codeExtArgument arg1, arg2=codeExtArgument arg2 } :: code

        |   codeExtended({instr=WordComparison _, ...}, _) = raise InternalError "codeExtended - WordComparison"

            (* Set up an exception handler. *)
        |   codeExtended({instr=PushExceptionHandler{workReg=PReg hReg}, ...}, code) =
                LLPushExceptionHandler{workReg=getAllocatedReg hReg} :: code

            (* Pop an exception handler at the end of a handled section.  Executed if no exception has been raised.
               This removes items from the stack. *)
        |   codeExtended({instr=PopExceptionHandler{workReg=PReg wReg, ...}, ...}, code) =
            let
                val realWork = getAllocatedReg wReg
            in
                LLPopExceptionHandler{workReg=realWork} :: code
            end

            (* Start of a handler.  Sets the address associated with PushExceptionHandler and
               provides a register for the packet.*) 
        |   codeExtended({instr=BeginHandler{packetReg=PReg pReg, workReg=PReg wReg}, ...}, code) =
            let
                (* The exception packet is in rax. *)
                val realPktReg = getAllocatedReg pReg
                val realWorkreg = getAllocatedReg wReg
            in
                LLBeginHandler{workReg=realWorkreg, packetReg=GenReg eax} ::
                    (moveIfNecessary{src=GenReg eax, dst=realPktReg, kind=MoveWord } @ code)
            end

        |   codeExtended({instr=ReturnResultFromFunction { resultReg=PReg resReg, numStackArgs }, ...}, code) =
            let
                val resultReg = getAllocatedReg resReg
                (* If for some reason it's not in the right register we have to move it there. *)
                val moveCode = moveIfNecessary{src=resultReg, dst=raxAsArg, kind=MoveWord}
            in
                moveCode @
                    (LLReturnResultFromFunction{resultReg=raxAsArg, numStackArgs=numStackArgs} :: code)
            end

        |   codeExtended({instr=ArithmeticFunction{oper=SUB, resultReg=PReg resReg, operand1=RegisterArgument(PReg op1Reg),
                                            operand2, ...}, ...}, code) =
            (* Subtraction - this is special because it can only be done one way round.  The first argument must
               be in a register. *)
            let
                val realDestReg = getAllocatedReg resReg
                (* Try to put the argument into the same register as the result. *)
                val realOp1Reg = getAllocatedReg op1Reg
                val op2Arg = codeExtArgument operand2
                (* If we couldn't put it in the result register we have to copy it there. *)
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg, kind=MoveWord} @
                    (LLArithmeticFunction{oper=SUB, resultReg=realDestReg, operand1=RegisterArg realDestReg, operand2=op2Arg} :: code)
            end

        |   codeExtended({instr=ArithmeticFunction{oper=SUB, ...}, ...}, _) = raise InternalError "codeExtended - ArithmeticFunction"

        |   codeExtended({instr=ArithmeticFunction{oper, resultReg=PReg resReg, operand1=RegisterArgument(PReg op1Reg),
                                            operand2=RegisterArgument(PReg op2Reg), ...}, ...}, code) =
            (* Arithmetic operation with both arguments as registers.  These operations are all symmetric so
               we can try to put either argument into the result reg and then do the operation on the other arg. *)
            let
               val realDestReg = getAllocatedReg resReg
                
                val realOp1Reg = getAllocatedReg op1Reg
                and realOp2Reg = getAllocatedReg op2Reg
                val (operandReg, moveInstr) =
                    if realOp1Reg = realDestReg
                    then (realOp2Reg, [])
                    else if realOp2Reg = realDestReg
                    then (realOp1Reg, [])
                    else (realOp2Reg, [moveRR{src=realOp1Reg, dst=realDestReg, kind=MoveWord}])
            in
                moveInstr @
                    (LLArithmeticFunction{oper=oper, resultReg=realDestReg, operand1=RegisterArg realDestReg,
                                        operand2=RegisterArg operandReg} :: code)
            end

        |   codeExtended({instr=ArithmeticFunction{oper, resultReg=PReg resReg, operand1=RegisterArgument(PReg op1Reg),
                                            operand2, ...}, ...}, code) =
            (* Arithmetic operation with the first argument in a register and the second a constant or memory location. *)
            let
                val realDestReg = getAllocatedReg resReg
                val realOp1Reg = getAllocatedReg op1Reg
                val op2Arg = codeExtArgument operand2
                (* If we couldn't put it in the result register we have to copy it there. *)
                (* TODO: Is there the potential for a problem?  We don't worry about a conflict
                   between the result register and the arguments.  What if the second argument is a memory
                   location with the result reg as a base or index? *)
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg, kind=MoveWord} @
                    (LLArithmeticFunction{oper=oper, resultReg=realDestReg, operand1=RegisterArg realDestReg, operand2=op2Arg} :: code)
            end

        |   codeExtended({instr=ArithmeticFunction{oper, resultReg=PReg resReg, operand1,
                                            operand2=RegisterArgument(PReg op2Reg), ...}, ...}, code) =
            (* Arithemtic operation with the second argument in a register and the first a constant or memory location. *)
            let
                val realDestReg = getAllocatedReg resReg
                val realOp2Reg = getAllocatedReg op2Reg
                val op1Arg = codeExtArgument operand1
            in
                moveIfNecessary{src=realOp2Reg, dst=realDestReg, kind=MoveWord} @
                    (LLArithmeticFunction{oper=oper, resultReg=realDestReg, operand1=RegisterArg realDestReg, operand2=op1Arg} :: code)
            end

        |   codeExtended({instr=ArithmeticFunction _, ...}, _) =
                raise InternalError "codeExtended - ArithmeticFunction"

        |   codeExtended({instr=TestTagBit{arg, ...}, ...}, code) = LLTestTagBit{ arg=codeExtArgument arg } :: code

        |   codeExtended({instr=PushValue {arg, ...}, ...}, code) = LLPushValue {arg=codeExtArgument arg} :: code

        |   codeExtended({instr=CopyToCache{source=PReg sreg, dest=PReg dreg, kind}, ...}, code) =
            let
                val realDestReg = getAllocatedReg dreg
                (* Get the source register using the current destination as a preference. *)
                val realSrcReg = getAllocatedReg sreg
            in
                (* If the source is the same as the destination we don't need to do anything. *)
                moveIfNecessary{src=realSrcReg, dst=realDestReg, kind=kind} @ code
            end

        |   codeExtended({instr=ResetStackPtr {numWords, preserveCC}, ...}, code) =
                LLResetStackPtr{numWords=numWords, preserveCC=preserveCC} :: code

        |   codeExtended({instr=StoreToStack{ source, stackOffset, ... }, ...}, code) =
            let
                val codeSource = codeExtArgument source
            in
                LLStoreArgument{
                    source=codeSource, base=esp, offset=stackOffset*wordSize, index=NoIndex, kind=MoveWord}
                        :: code
            end

        |   codeExtended({instr=TagValue{source=PReg srcReg, dest=PReg dReg}, ...}, code) =
            let
                val regResult = getAllocatedReg(dReg)
                val realSReg = asGenReg(getAllocatedReg(srcReg))
            in
                LLLoadEffectiveAddress { base=NONE, offset=1, index=Index2 realSReg, dest=regResult }  :: code
            end

        |   codeExtended({instr=UntagValue{source=RegisterArgument(PReg sReg), dest=PReg dReg, isSigned}, ...}, code) =
            (* Always generates register argument at the moment.  TODO: This should really just take a single register arg. *)
            let
                val regResult = getAllocatedReg dReg
                val realSReg = getAllocatedReg sReg
            in
                moveIfNecessary{src=realSReg, dst=regResult, kind=MoveWord} @
                    (LLShiftOperation{shift=if isSigned then SAR else SHR, resultReg=regResult, operand=RegisterArg regResult,
                        shiftAmount=NonAddressConstArg 1} :: code)
            end

        |   codeExtended({instr=UntagValue _, ...}, _) = raise InternalError "UntagValue"

        |   codeExtended({instr=LoadEffectiveAddress{base=PReg br, offset, index, dest=PReg dReg}, ...}, code) =
            let
                val destReg = getAllocatedReg dReg
                val bReg = getAllocatedReg br
                val iReg = codeExtIndex index
            in
                LLLoadEffectiveAddress{base=SOME bReg, offset=offset, index=iReg, dest=destReg} :: code
            end

        |   codeExtended({instr=ShiftOperation{shift, resultReg=PReg resReg, operand=RegisterArgument(PReg operReg), shiftAmount=IntegerConstant i, ...}, ...}, code) =
            let
                val realDestReg = getAllocatedReg resReg
                val realOpReg = getAllocatedReg operReg
            in
                 moveIfNecessary{src=realOpReg, dst=realDestReg, kind=MoveWord} @
                    (LLShiftOperation{shift=shift, resultReg=realDestReg, operand=RegisterArg realDestReg,
                                    shiftAmount=NonAddressConstArg i} :: code)
            end

        |   codeExtended({instr=ShiftOperation{shift, resultReg=PReg resReg, operand=RegisterArgument(PReg operReg),
                                        shiftAmount=RegisterArgument(PReg shiftReg), ...}, ...}, code) =
            let
                val realDestReg = getAllocatedReg resReg
                val realShiftReg = getAllocatedReg shiftReg
                val realOpReg = getAllocatedReg operReg
                (* We want the shift in ecx.  We may not have got it there but the register
                   should be free. *)
            in
                 moveIfNecessary{src=realOpReg, dst=realDestReg, kind=MoveWord} @ moveIfNecessary{src=realShiftReg, dst=GenReg ecx, kind=MoveWord} @
                    (LLShiftOperation{shift=shift, resultReg=realDestReg, operand=RegisterArg realDestReg,
                                    shiftAmount=RegisterArg(GenReg ecx)} :: code)
            end

        |   codeExtended({instr=ShiftOperation _, ...}, _) = raise InternalError "codeExtended - ShiftOperation"

        |   codeExtended({instr=
                Multiplication{resultReg=PReg resReg, operand1=RegisterArgument(PReg op1Reg),
                               operand2=RegisterArgument(PReg op2Reg), ...}, ...}, code) =
            let
                (* Treat exactly the same as ArithmeticFunction. *)
                val realDestReg = getAllocatedReg resReg
                
                val realOp1Reg = getAllocatedReg op1Reg
                and realOp2Reg = getAllocatedReg op2Reg
                val (operandReg, moveInstr) =
                    if realOp1Reg = realDestReg
                    then (realOp2Reg, [])
                    else if realOp2Reg = realDestReg
                    then (realOp1Reg, [])
                    else (realOp2Reg, [moveRR{src=realOp1Reg, dst=realDestReg, kind=MoveWord}])
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
                val realDiviReg = getAllocatedReg regDivid
                val realQuotReg = getAllocatedReg regQuot
                val realRemReg = getAllocatedReg regRem
                val divisorArg = codeExtArgument divisor
            in
                (* We may need to move one or more of the registers although normally that
                   won't be necessary.  Almost certainly only either the remainder or the
                   quotient will actually be used. *)
                moveIfNecessary{src=realDiviReg, dst=GenReg eax, kind=MoveWord} @
                    LLDivision{isSigned=isSigned, dividend=GenReg eax, divisor=divisorArg,
                              quotient=GenReg eax, remainder=GenReg edx} ::
                    moveMultipleRegisters[{src=GenReg eax, dst=realQuotReg}, {src=GenReg edx, dst=realRemReg}] @ code
            end

        |   codeExtended({instr=AtomicExchangeAndAdd{base=PReg bReg, source=PReg sReg}, ...}, code) =
            let
                val srcReg = getAllocatedReg sReg
                val baseReg = getAllocatedReg bReg
            in
                LLAtomicExchangeAndAdd{base=baseReg, source=srcReg} :: code
            end

        |   codeExtended({instr=BoxValue{boxKind, source=PReg sReg, dest=PReg dReg, saveRegs}, ...}, code) =
            let
                val preserve = List.map(fn (PReg r) => getAllocatedReg r) saveRegs
                val (srcReg, boxSize, moveKind) =
                    case boxKind of
                        BoxLargeWord => (getAllocatedReg sReg, 1, MoveWord)
                    |   BoxFloat => (getAllocatedReg sReg, Int.quot(8, wordSize), MoveDouble)
                val dstReg = getAllocatedReg dReg
            in
                LLAllocateMemoryOperation{ size=boxSize, flags=0wx1, dest=dstReg, saveRegs=preserve} ::
                LLStoreArgument{ source=RegisterArg srcReg, offset=0, base=asGenReg dstReg, index=NoIndex, kind=moveKind} ::
                LLInitialisationComplete :: code
            end

        |   codeExtended({instr=CompareByteVectors{vec1Addr=PReg v1Reg, vec2Addr=PReg v2Reg, length=PReg lReg, ...}, ...}, code) =
            let
                val realV1Reg = getAllocatedReg v1Reg
                val realV2Reg = getAllocatedReg v2Reg
                val realLengthReg = getAllocatedReg lReg
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
                moveIfNecessary{src=realV1Reg, dst=GenReg esi, kind=MoveWord} @
                    moveIfNecessary{src=realV2Reg, dst=GenReg edi, kind=MoveWord} @
                    moveIfNecessary{src=realLengthReg, dst=GenReg ecx, kind=MoveWord} @
                    (LLWordComparison {arg1=RegisterArg rcxAsArg, arg2=RegisterArg rcxAsArg} ::
                     LLCompareByteVectors{vec1Addr=GenReg esi, vec2Addr=GenReg edi, length=GenReg ecx} :: code)
            end

        |   codeExtended({instr=BlockMove{srcAddr=PReg sReg, destAddr=PReg dReg, length=PReg lReg, isByteMove}, ...}, code) =
            let
                val realSrcReg = getAllocatedReg sReg
                val realDestReg = getAllocatedReg dReg
                val realLengthReg = getAllocatedReg lReg
            in
                moveIfNecessary{src=realSrcReg, dst=GenReg esi, kind=MoveWord} @
                    moveIfNecessary{src=realDestReg, dst=GenReg edi, kind=MoveWord} @
                    moveIfNecessary{src=realLengthReg, dst=GenReg ecx, kind=MoveWord} @
                    (LLBlockMove{srcAddr=GenReg esi, destAddr=GenReg edi, length=GenReg ecx, isByteMove=isByteMove} :: code)
            end

        |   codeExtended({instr=CompareFloatingPt{arg1 as RegisterArgument _, arg2, ...}, ...}, code) =
                LLCompareFloatingPt{ arg1=codeExtArgument arg1, arg2=codeExtArgument arg2 } :: code

        |   codeExtended({instr=CompareFloatingPt _, ...}, _) = raise InternalError "codeExtended - CompareFloatingPt"

        |   codeExtended({instr=X87FPGetCondition{dest=PReg dReg, ...}, ...}, code) =
            let
                (* We can only use RAX here. *)
                val destReg = getAllocatedReg dReg
            in
                LLX87FPGetCondition{dest=raxAsArg} ::
                    moveIfNecessary{dst=destReg, src=GenReg eax, kind=MoveWord} @ code
            end

        |   codeExtended({instr=X87FPArith{opc, resultReg=PReg resReg, arg1=RegisterArgument(PReg op1Reg), arg2}, ...}, code) =
            let
                val realDestReg = getAllocatedReg resReg
                val realOp1Reg = getAllocatedReg op1Reg
                val op2Arg = codeExtArgument arg2
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg, kind=MoveDouble} @
                    (LLX87FPArith{opc=opc, resultReg=realDestReg, arg1=RegisterArg realDestReg, arg2=op2Arg} :: code)
            end

        |   codeExtended({instr=X87FPArith _, ...}, _) = raise InternalError "codeExtended - X87FPArith"
    
        |   codeExtended({instr=X87FPUnaryOps{fpOp, dest=PReg resReg, source=RegisterArgument(PReg op1Reg)}, ...}, code) =
            let
                val realDestReg = getAllocatedReg resReg
                val realOp1Reg = getAllocatedReg op1Reg
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg, kind=MoveDouble} @
                    (LLX87FPUnaryOps{fpOp=fpOp, dest=realDestReg, source=RegisterArg realDestReg} :: code)
            end

        |   codeExtended({instr=X87FPUnaryOps _, ...}, _) = raise InternalError "codeExtended - X87FPArith"

        |   codeExtended({instr=FloatFixedInt{dest=PReg resReg, source}, ...}, code) =
            let
                val intSource = codeExtArgument source
                val fpReg = getAllocatedReg resReg
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
                val realDestReg = getAllocatedReg resReg
                val realOp1Reg = getAllocatedReg op1Reg
                val op2Arg = codeExtArgument arg2
            in
                moveIfNecessary{src=realOp1Reg, dst=realDestReg, kind=MoveDouble} @
                    (LLSSE2FPArith{opc=opc, resultReg=realDestReg, arg1=RegisterArg realDestReg, arg2=op2Arg} :: code)
            end
    
        |   codeExtended({instr=SSE2FPArith _, ...}, _) = raise InternalError "codeExtended - SSE2FPArith"
        
        
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
            |   codeGenICode(LLExchange{reg, arg} :: rest, code) =
                    codeGenICode(rest, XChng { reg=asGenReg reg, arg=sourceAsGenRegOrMem arg } :: code)

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
            
            fun setReferences(fromLabel, ExtendedBasicBlock{ flow, ...}) =
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

            val () = Vector.appi setReferences blocks
            
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
                    local
                        val ExtendedBasicBlock { flow, block, ...} = Vector.sub(blocks, picked)
                        val codeBlock = List.foldr codeExtended [] block
                    in
                        val blockToCode as LLBasicBlock{ flow, ...} = LLBasicBlock { block = codeBlock, flow = flow }
                    end

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
