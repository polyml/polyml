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
    
    sharing X86CODE.Sharing = X86OPTIMISE.Sharing
): ICodeSig =
struct
    open X86CODE

    datatype fpMode = FPModeSSE2 | FPModeX87

    (* For the moment use SSE2 only on X86/64. Not all 32-bit processors support SSE2. *)
    val fpMode: fpMode = if isX64 then FPModeSSE2 else FPModeX87

    open Address

    datatype pregKind =
        PRegGeneral     (* A general register.  This can contain and address and be pushed to the stack. *)
    |   PRegUntagged    (* An untagged general register. Just used for short-term storage. Not valid on the stack. *)

    datatype abstract = PReg of int * pregKind (* A pseudo-register - an abstract register. *)

    datatype 'reg argument =
        RegisterArgument of 'reg
    |   AddressConstant of machineWord (* A constant that is an address. *)
    |   IntegerConstant of LargeInt.int (* A non-address constant.  Will usually be shifted and tagged. *)
    |   MemoryLocation of { base: 'reg, offset: int, index: 'reg memoryIndex } (* A memory location.  Could be the stack. *)
    |   StackLocation of { wordOffset: int, adjustment: int }

    and 'reg memoryIndex =
        NoMemIndex | MemIndex1 of 'reg | MemIndex2 of 'reg | MemIndex4 of 'reg | MemIndex8 of 'reg

    (* Kinds of moves.
       MoveWord - Move a whole word - 64-bits in 64-bit mode, 32-bits in 32-bit mode.
       MoveByte - When loading, load a byte and zero extend.
       Move16Bit - Used for C-memory loads and stores.  Zero extends on load.
       Move32Bit - Used for C-memory loads and stores in 64-bit mode.
       MoveFloat - Load and store a single-precision value
       MoveDouble - Load and store a double-precision value. *)
    datatype moveKind =
        MoveWord | MoveByte | Move16Bit | Move32Bit | MoveFloat | MoveDouble

    datatype iLabel = ILabel of int
    (* The reference to a condition code. *)
    and ccRef = CcRef of int

    datatype boxKind = BoxLargeWord | BoxFloat

    datatype 'reg x86ICode =
        (* Move a value into a register. *)
        LoadArgument of { source: 'reg argument, dest: 'reg, kind: moveKind }
        
        (* Store a value into memory.  The source will usually be a register but could be
           a constant depending on the value. *)
    |   StoreArgument of { source: 'reg argument, base: 'reg, offset: int, index: 'reg memoryIndex, kind: moveKind }

        (* Load an entry from the "memory registers".  Used just for ThreadSelf. *)
    |   LoadMemReg of { offset: int, dest: 'reg }

        (* Exchange two registers. *)
    |   ExchangeRegisters of { regX: 'reg, regY: 'reg }

        (* Start of function.  Set the register arguments. *)
    |   BeginFunction of { regArgs: ('reg * reg) list }

        (* Call a function.  If the code address is a constant it is passed here.
           Otherwise the address is obtained by indirecting through rdx which has been loaded
           as one of the argument registers.  The result is stored in the destination register. *)
    |   FunctionCall of
            { callKind: callKinds, regArgs: ('reg argument * reg) list,
              stackArgs: 'reg argument list, dest: 'reg}

        (* Jump to a tail-recursive function.  This is similar to FunctionCall
           but complicated for stack arguments because the stack and the return
           address need to be overwritten.  We could actually include the
           return address among the stackArgs but leave that for the moment.
           stackAdjust is the number of words to remove (positive) or add
           (negative) to the stack before the call. *)
    |   TailRecursiveCall of
            { callKind: callKinds, regArgs: ('reg argument * reg) list,
              stackArgs: {src: 'reg argument, stack: int} list,
              returnAddr: {srcStack: int, stack: int},
              stackAdjust: int }

        (* Allocate a fixed sized piece of memory.  The size is the number of words
           required.  This sets the length word including the flags bits.
           saveRegs is the list of registers that need to be saved if we
           need to do a garbage collection. *)
    |   AllocateMemoryOperation of { size: int, flags: Word8.word, dest: 'reg, saveRegs: 'reg list }

        (* Allocate a piece of memory whose size is not known at compile-time.  The size
           argument is the number of words. *)
    |   AllocateMemoryVariable of { size: 'reg, dest: 'reg, saveRegs: 'reg list }

        (* Initialise a piece of memory.  N.B. The size is an untagged value containing
           the number of words.  This uses REP STOSL/Q so addr must be rdi, size must be
           rcx and init must be rax. *)
    |   InitialiseMem of { size: 'reg, addr: 'reg, init: 'reg }

        (* Signal that a tuple has been fully initialised.  Really a check in the
           low-level code-generator. *)
    |   InitialisationComplete

        (* Begin a loop.  A set of loop registers are initialised and the loop is entered.
           The loopLabel is the start of the loop and all jumps come back to it. *)
    |   StartLoop of { loopLabel: iLabel }

        (* End a loop.  This is a marker that is used at the higher levels.
           It doesn't generate any code. *)
    |   EndLoop of { loopLabel: iLabel, staticRegs: 'reg list }

        (* Within a loop the loop registers are updated from the source registers and
           a jump is made back to the containing StartLoop *)
    |   JumpLoop of
            { regArgs: ('reg argument * 'reg) list, stackArgs: ('reg argument * int) list,
              stackAdjust: int, loopLabel: iLabel, checkInterrupt: 'reg list option,
              staticRegs: 'reg list }

        (* Raise an exception.  The packet is always loaded into rax. *)
    |   RaiseExceptionPacket of { packet: 'reg argument }

        (* Reserve a contiguous area on the stack to receive a result tuple. *)
    |   ReserveContainer of { size: int, address: 'reg }

        (* Indexed case. *)
    |   IndexedCaseOperation of { testReg: 'reg, workReg: 'reg, cases: iLabel list }

        (* Lock a mutable cell by turning off the mutable bit. *)
    |   LockMutable of { addr: 'reg }

        (* Forward branches. *)
    |   ForwardJumpLabel of { label: iLabel, result: 'reg option }
    
    |   UnconditionalForwardJump of { label: iLabel }

        (* Conditional branch. *)
    |   ConditionalForwardJump of { ccRef: ccRef, condition: branchOps, label: iLabel }

        (* Compare two word values. *)
    |   WordComparison of { arg1: 'reg argument, arg2: 'reg argument, ccRef: ccRef }
    
        (* Exception handling.  - Set up an exception handler. *)
    |   PushExceptionHandler of { workReg: 'reg, handleStart: iLabel }

        (* End of a handled section.  Restore the previous handler. *)
    |   PopExceptionHandler of { resultReg: 'reg option, workReg: 'reg }

        (* Marks the start of a handler.  This sets the stack pointer and
           restores the old handler.  Sets the exception packet register. *) 
    |   BeginHandler of { handleStart: iLabel, packetReg: 'reg, workReg: 'reg }

        (* Return from the function. *)
    |   ReturnResultFromFunction of { resultReg: 'reg, numStackArgs: int }
    
        (* Arithmetic or logical operation.  These can set the condition codes. *)
    |   ArithmeticFunction of
            { oper: arithOp, resultReg: 'reg, operand1: 'reg argument, operand2: 'reg argument, ccRef: ccRef }

        (* Test the tag bit of a word.  Sets the Zero bit if the value is an address i.e. untagged. *)
    |   TestTagBit of { arg: 'reg argument, ccRef: ccRef }

        (* Push a value to the stack.  Added during translation phase. *)
    |   PushValue of { arg: 'reg argument }

        (* Remove items from the stack.  Added during translation phase. *)
    |   ResetStackPtr of { numWords: int }

        (* Tag a value by shifting and setting the tag bit. *)
    |   TagValue of { source: 'reg, dest: 'reg }

        (* Shift a value to remove the tag bit. *)
    |   UntagValue of { source: 'reg argument, dest: 'reg, isSigned: bool }

        (* This provides the LEA instruction which can be used for various sorts of arithmetic.
           The base register is optional in this case. *)
    |   LoadEffectiveAddress of { base: 'reg option, offset: int, index: 'reg memoryIndex, dest: 'reg }

        (* Shift a word by an amount that can either be a constant or a register. *)
    |   ShiftOperation of { shift: shiftType, resultReg: 'reg, operand: 'reg argument, shiftAmount: 'reg argument, ccRef: ccRef }

        (* Multiplication.  We can use signed multiplication for both fixed precision and word (unsigned)
           multiplication.  There are various forms of the instruction including a three-operand
           version. *)
    |   Multiplication of { resultReg: 'reg, operand1: 'reg argument, operand2: 'reg argument, ccRef: ccRef }

        (* Division.  This takes a register pair, always RDX:RAX, divides it by the operand register and
           puts the quotient in RAX and remainder in RDX.  At the abstract level we represent all of
           these by pRegs.  The divisor can be either a register or a memory location. *)
    |   Division of { isSigned: bool, dividend: 'reg, divisor: 'reg argument, quotient: 'reg, remainder: 'reg }

        (* Atomic exchange and addition.   This is executed with a lock prefix and is used
           for atomic increment and decrement for mutexes.
           Before the operation the source contains an increment.  After the operation
           the source contains the old value of the destination and the destination
           has been updated with its old value added to the increment.
           The destination is actually the word pointed at by "base". *)
    |   AtomicExchangeAndAdd of { base: 'reg, source: 'reg }

        (* Create a "box" of a single-word "byte" cell and store the source into it.
           This can be implemented using AllocateMemoryOperation but the idea is to
           allow the transform layer to recognise when a value is being boxed and
           then unboxed and remove unnecessary allocation. *)
    |   BoxValue of { boxKind: boxKind, source: 'reg, dest: 'reg, saveRegs: 'reg list }

        (* Compare two vectors of bytes and set the condition code on the result.
           In general vec1Addr and vec2Addr will be pointers inside memory cells
           so have to be untagged registers. *)
    |   CompareByteVectors of
            { vec1Addr: 'reg, vec2Addr: 'reg, length: 'reg, ccRef: ccRef }

        (* Move a block of bytes (isByteMove true) or words (isByteMove false).  The length is the
           number of items (bytes or words) to move. *)
    |   BlockMove of { srcAddr: 'reg, destAddr: 'reg, length: 'reg, isByteMove: bool }

        (* Floating point comparison. *)
    |   CompareFloatingPt of { arg1: 'reg argument, arg2: 'reg argument, ccRef: ccRef }

        (* The X87 FP unit does not generate condition codes directly.  We have to
           load the cc into RAX and test it there. *)
    |   X87FPGetCondition of { ccRef: ccRef, dest: 'reg }

        (* Binary floating point operations on the X87. *)
    |   X87FPArith of { opc: fpOps, resultReg: 'reg, arg1: 'reg argument, arg2: 'reg argument }

        (* Floating point operations: negate and set sign positive. *)
    |   X87FPUnaryOps of { fpOp: fpUnaryOps, dest: 'reg, source: 'reg argument }

        (* Load a fixed point value as a floating point value. *)
    |   FloatFixedInt of { dest: 'reg, source: 'reg argument }

        (* Binary floating point operations using SSE2 instructions. *)
    |   SSE2FPArith of { opc: sse2Operations, resultReg: 'reg, arg1: 'reg argument, arg2: 'reg argument }

    and blockSource =
        (* Sources for a basic block.  May be the root (start of function), the destination of
           a branch or the start of a handler.  BranchSource includes the list of block nos
           which jump to this. HandleSource includes the block that sets up this handler.
           If the block is unreachable the list may be empty and the handle source ~1. *)
        RootSource | BranchSource of int list | HandleSource of int

        (* Destinations at the end of a basic block. *)
    and controlFlow =
        (* Unconditional branch to a label - should be a merge point. *)
        Unconditional of int
        (* Conditional branch. Jumps to trueJump if the condional is false, falseJump if false. *)
    |   Conditional of { ccRef: ccRef, condition: branchOps, trueJump: int, falseJump: int }
        (* Exit - the last instruction of the block is a return, raise or tailcall. *)
    |   ExitCode
        (* Indexed case - this branches to one of a number of labels *)
    |   IndexedBr of int list
        (* Set up a handler.  This doesn't cause an immediate branch but the state at the
           start of the handler is the state at this point. *)
    |   SetHandler of { handler: int, continue: int }

    and 'reg basicBlock =
        BasicBlock of { block: 'reg x86ICode list, source: blockSource, flow: controlFlow }


    local

        fun printLabel(ILabel i, stream) = stream("L"^Int.toString i)
        
        fun printIndex(NoMemIndex, _, _) = ()
        |   printIndex(MemIndex1 i, stream, printReg) = (stream "["; printReg(i, stream); stream "*1]")
        |   printIndex(MemIndex2 i, stream, printReg) = (stream "["; printReg(i, stream); stream "*2]")
        |   printIndex(MemIndex4 i, stream, printReg) = (stream "["; printReg(i, stream); stream "*4]")
        |   printIndex(MemIndex8 i, stream, printReg) = (stream "["; printReg(i, stream); stream "*8]")

        fun printArg(RegisterArgument reg, stream, printReg) = printReg(reg, stream)
        |   printArg(AddressConstant m, stream, _) = stream(stringOfWord m)
        |   printArg(IntegerConstant i, stream, _) = stream(LargeInt.toString i)
        |   printArg(MemoryLocation{base, offset, index}, stream, printReg) =
            (
                stream(Int.toString offset ^ "(");
                printReg(base, stream);
                stream ")";
                printIndex(index, stream, printReg)
            )
        |   printArg(StackLocation{wordOffset, adjustment}, stream, _) =
                stream(Int.toString wordOffset ^ "+" ^ Int.toString adjustment ^ "(stackptr)")
            
        fun printSaves([], _, _) = ()
        |   printSaves([areg], printReg, stream) = printReg(areg, stream)
        |   printSaves(areg::more, printReg, stream) =
                (printReg(areg, stream); stream ","; printSaves(more, printReg, stream))

        fun printICode(LoadArgument{source, dest, kind}, stream, printReg) =
            (
                case kind of
                    MoveWord => stream "\tLoadWord\t"
                |   MoveByte => stream "\tLoadByte\t"
                |   Move16Bit => stream "\tLoad16Bit\t"
                |   Move32Bit => stream "\tLoad32Bit\t"
                |   MoveFloat => stream "\tLoadFloat\t"
                |   MoveDouble => stream "\tLoadDouble\t";
                printArg(source,  stream, printReg);
                stream " => ";
                printReg(dest, stream)
            )

        |   printICode(StoreArgument{source, base, offset, index, kind}, stream, printReg) =
            (
                case kind of
                    MoveWord => stream "\tStoreWord\t"
                |   MoveByte => stream "\tStoreByte\t"
                |   Move16Bit => stream "\tStore16Bit\t"
                |   Move32Bit => stream "\tStore32Bit\t"
                |   MoveFloat => stream "\tStoreFloat\t"
                |   MoveDouble => stream "\tStoreDouble\t";
                printArg(source,  stream, printReg);
                stream " => ";
                stream(Int.toString offset ^ "(");
                printReg(base, stream);
                stream ")";
                printIndex(index, stream, printReg)
            )

        |   printICode(LoadMemReg { offset, dest}, stream, printReg) =
                ( stream "\tLoadMemReg\t"; stream(Int.toString offset); stream " => "; printReg(dest, stream) )

        |   printICode(ExchangeRegisters { regX, regY}, stream, printReg) =
                ( stream "\tExchangeRegs\t"; printReg(regX, stream); stream " <=> "; printReg(regY, stream) )

        |   printICode(BeginFunction {regArgs}, stream, printReg) =
            (
                stream "\tBeginFunction\t";
                List.app(fn (arg, r) => (stream(regRepr r); stream "="; printReg(arg, stream); stream " ")) regArgs
            )

        |   printICode(FunctionCall{callKind, regArgs, stackArgs, dest}, stream, printReg) =
            (
                stream "\tFunctionCall\t";
                case callKind of
                    Recursive => stream "recursive "
                |   ConstantCode m => (stream(stringOfWord m); stream " ")
                |   FullCall => ()
                |   DirectReg r => (stream(regRepr(GenReg r)); stream " ");
                List.app(fn (arg, r) => (stream(regRepr r); stream "="; printArg(arg, stream, printReg); stream " ")) regArgs;
                List.app(fn arg => (stream "p="; printArg(arg, stream, printReg); stream " ")) stackArgs;
                stream "=> "; printReg(dest, stream)
            )

        |   printICode(TailRecursiveCall{callKind, regArgs, stackArgs, returnAddr={srcStack, stack}, stackAdjust}, stream, printReg) =
            (
                stream "\tTailCall\t";
                case callKind of
                    Recursive => stream "recursive "
                |   ConstantCode m => (stream(stringOfWord m); stream " ")
                |   FullCall => ()
                |   DirectReg r => (stream(regRepr(GenReg r)); stream " ");
                List.app(fn (arg, r) => (stream(regRepr r); stream "="; printArg(arg, stream, printReg); stream " ")) regArgs;
                List.app(fn {src, stack} => (stream (Int.toString stack); stream "<="; printArg(src, stream, printReg); stream " ")) stackArgs;
                stream "ret "; stream(Int.toString stack); stream "<="; stream(Int.toString srcStack);
                stream "adj="; stream(Int.toString stackAdjust)
            )

        |   printICode(AllocateMemoryOperation{size, flags, dest, saveRegs}, stream, printReg) =
            (
                stream "\tAllocateMemory\t";
                stream(concat["s=", Int.toString size, ",f=", Word8.toString flags, " => "]);
                printReg(dest, stream);
                stream " save="; printSaves(saveRegs, printReg, stream)
            )

        |   printICode(AllocateMemoryVariable{size, dest, saveRegs}, stream, printReg) =
            (
                stream "\tAllocateMemory\t";
                stream "s="; printReg(size, stream);
                stream " => "; printReg(dest, stream);
                stream " save="; printSaves(saveRegs, printReg, stream)
            )

        |   printICode(InitialiseMem{size, addr, init}, stream, printReg) =
            (
                stream "\tInitialiseMem\t";
                stream "s="; printReg(size, stream);
                stream ",i="; printReg(init, stream);
                stream ",a="; printReg(addr, stream)
            )

        |   printICode(InitialisationComplete, stream, _) = stream "\tInitComplete"

        |   printICode(StartLoop{loopLabel}, stream, _) = ( printLabel(loopLabel, stream); stream ":\tStartLoop\t" )

        |   printICode(EndLoop{loopLabel, ...}, stream, _) = (stream "\tEndLoop\t"; printLabel(loopLabel, stream))

        |   printICode(JumpLoop{regArgs, stackArgs, loopLabel, stackAdjust, checkInterrupt, ... }, stream, printReg) =
            (
                stream "\tJumpLoop\t";
                List.app(
                    fn (source, loopReg) => (printReg(loopReg, stream); stream "="; printArg(source, stream, printReg); stream " ")
                    ) regArgs;
                List.app(
                    fn (source, stack) => (stream("sp" ^ Int.toString stack); stream "="; printArg(source, stream, printReg); stream " ")
                    ) stackArgs;
                printLabel(loopLabel, stream);
                case checkInterrupt of
                    NONE => ()
                |   SOME saveRegs => (stream " Check:save="; printSaves(saveRegs, printReg, stream));
                stream " adj="; stream(Int.toString stackAdjust)
            )

        |   printICode(RaiseExceptionPacket{packet}, stream, printReg) = (stream "\tRaise\t"; printArg(packet, stream, printReg))

        |   printICode(ReserveContainer{size, address}, stream, printReg) =
            (stream "\tReserveContainer\t"; stream(Int.toString size); stream "=>"; printReg(address, stream))

        |   printICode(IndexedCaseOperation{testReg, workReg, cases}, stream, printReg) =
            (
                stream "\tIndexedCase\t";
                stream "test="; printReg(testReg, stream);
                stream "work="; printReg(workReg, stream);
                List.app(fn l => (stream " "; printLabel(l, stream))) cases
            )

        |   printICode(LockMutable{addr}, stream, printReg) = (stream "\tLockMutable\t"; printReg(addr, stream))
        
        |   printICode(ForwardJumpLabel{label, ...}, stream, _) = (printLabel(label, stream); stream ":\t\t")

        |   printICode(UnconditionalForwardJump{label}, stream, _) = (stream "\tJump\t"; printLabel(label, stream))

        |   printICode(ConditionalForwardJump{condition, label, ...}, stream, _) =
            (
                case condition of
                    JO => stream "\tJumpOverflow\t"
                |   JNO => stream "\tJumpNoOverflow\t"
                |   JE => stream "\tJumpEqual\t"
                |   JNE => stream "\tJumpNotEqual\t"
                |   JL => stream "\tJumpLessSigned\t"
                |   JGE => stream "\tJumpGeqSigned\t"
                |   JLE => stream "\tJumpLeqSigned\t"
                |   JG => stream "\tJumpGrtSigned\t"
                |   JB => stream "\tJumpLessUnsigned\t"
                |   JNB => stream "\tJumpGeqUnsigned\t"
                |   JNA => stream "\tJumpLeqUnsigned\t"
                |   JA => stream "\tJumpGrtUnsigned\t"
                |   JP => stream "\tJumpParitySet"
                |   JNP => stream "\tJumpParityClear\t";
                printLabel(label, stream)
            )

        |   printICode(WordComparison{arg1, arg2, ...}, stream, printReg) =
                (stream "\tWordComparison\t"; printArg(arg1, stream, printReg); stream ","; printArg(arg2, stream, printReg))

        |   printICode(PushExceptionHandler{workReg, handleStart}, stream, printReg) =
            (
                stream "\tPushExcHandler\t";
                printLabel(handleStart, stream);
                stream " with ";
                printReg(workReg, stream)
            )

        |   printICode(PopExceptionHandler{resultReg=_, workReg}, stream, printReg) =
            (
                stream "\tPopExceptionHandler\t";
                stream "with ";
                printReg(workReg, stream)
            )

        |   printICode(BeginHandler{handleStart, packetReg, workReg}, stream, printReg) =
            (
                printLabel(handleStart, stream);
                stream ":\tBeginHandler\t";
                printReg(packetReg, stream);
                stream " with ";
                printReg(workReg, stream)
            )

        |   printICode(ReturnResultFromFunction{resultReg, numStackArgs}, stream, printReg) =
                (stream "\tReturnFromFunction\t"; printReg(resultReg, stream); stream("," ^ Int.toString numStackArgs))

        |   printICode(ArithmeticFunction{oper, resultReg, operand1, operand2, ...}, stream, printReg) =
            (
                case oper of
                    ADD => stream "\tAdd\t"
                |   OR => stream "\tOrBits\t"
                |   AND => stream "\tAndBits\t"
                |   SUB => stream "\tSubtract\t"
                |   XOR => stream "\tExclusiveOrBits\t"
                |   CMP => stream "\tCompare\t";
                printArg(operand1, stream, printReg);
                stream ",";
                printArg(operand2, stream, printReg);
                stream " => ";
                printReg(resultReg, stream)
            )

        |   printICode(TestTagBit{arg, ...}, stream, printReg) = (stream "\tTestTagBit\t"; printArg(arg, stream, printReg))

        |   printICode(PushValue{arg}, stream, printReg) = (stream "\tPushValue\t"; printArg(arg, stream, printReg))

        |   printICode(ResetStackPtr{numWords}, stream, _) = (stream "\tResetStackPtr\t"; stream(Int.toString numWords))

        |   printICode(TagValue{source, dest}, stream, printReg) =
                (stream "\tTagValue\t"; printReg(source, stream); stream " => "; printReg(dest, stream))

        |   printICode(UntagValue{source, dest, isSigned=true}, stream, printReg) =
                (stream "\tUntagSigned\t"; printArg(source, stream, printReg); stream " => "; printReg(dest, stream))
        |   printICode(UntagValue{source, dest, isSigned=false}, stream, printReg) =
                (stream "\tUntagSigned\t"; printArg(source, stream, printReg); stream " => "; printReg(dest, stream))

        |   printICode(LoadEffectiveAddress{base, offset, index, dest}, stream, printReg) =
            (
                stream "\tLoadEffectiveAddr\t"; 
                stream(Int.toString offset ^ "(");
                case base of NONE => stream "_" | SOME b => printReg(b, stream);
                stream ")";
                printIndex(index, stream, printReg);
                stream " => ";
                printReg(dest, stream)
            )

        |   printICode(ShiftOperation{shift, resultReg, operand, shiftAmount, ...}, stream, printReg) =
            (
                case shift of
                    SHL => stream "\tShiftLeft\t"
                |   SHR => stream "\tShiftRLogical\t"
                |   SAR => stream "\tShiftRArith\t";
                printArg(operand, stream, printReg); stream ",";
                printArg(shiftAmount, stream, printReg); stream " => ";
                printReg(resultReg, stream)
            )

        |   printICode(Multiplication{resultReg, operand1, operand2, ...}, stream, printReg) =
            (
                stream "\tMultiplication\t";
                printArg(operand1, stream, printReg);
                stream ",";
                printArg(operand2, stream, printReg);
                stream " => ";
                printReg(resultReg, stream)
            )

        |   printICode(Division{isSigned, dividend, divisor, quotient, remainder}, stream, printReg) =
            (
                stream "\tDivision"; stream(if isSigned then "Signed\t" else "Unsigned\t");
                printReg(dividend, stream); stream " by ";
                printArg(divisor, stream, printReg); stream " => ";
                printReg(quotient, stream); stream " rem ";
                printReg(remainder, stream)
            )

        |   printICode(AtomicExchangeAndAdd{base, source}, stream, printReg) =
            (
                stream "\tAtomicExchangeAndAdd\t";
                stream "addr=0("; printReg(base, stream);
                stream "),with="; printReg(source, stream)
            )

        |   printICode(BoxValue{boxKind, source, dest, saveRegs}, stream, printReg) =
            (
                case boxKind of BoxLargeWord => stream "\tBoxLarge\t" | BoxFloat => stream "\tBoxFloat\t";
                printReg(source, stream);
                stream " => ";
                printReg(dest, stream);
                stream " save="; printSaves(saveRegs, printReg, stream)
            )

        |   printICode(CompareByteVectors{vec1Addr, vec2Addr, length, ...}, stream, printReg) =
            (
                stream "\tCompareByteVectors\t";
                printReg(vec1Addr, stream); stream ",";
                printReg(vec2Addr, stream); stream ",";
                printReg(length, stream)
            )

        |   printICode(BlockMove{srcAddr, destAddr, length, isByteMove}, stream, printReg) =
            (
                stream(if isByteMove then "\tBlockByteMove\t" else "\tBlockWordMove\t");
                stream "src="; printReg(srcAddr, stream);
                stream ",dest="; printReg(destAddr, stream);
                stream ",len="; printReg(length, stream)
            )

        |   printICode(CompareFloatingPt{arg1, arg2, ...}, stream, printReg) =
                (stream "\tCompareFloatingPt\t"; printArg(arg1, stream, printReg); stream ","; printArg(arg2, stream, printReg))

        |   printICode(X87FPGetCondition{dest, ...}, stream, printReg) = (stream "\tX87FPGetCondition\t => "; printReg(dest, stream))

        |   printICode(X87FPArith{opc, resultReg, arg1, arg2}, stream, printReg) =
            (
                case opc of
                    FADD => stream "\tX87FPAdd\t"
                |   FMUL => stream "\tX87FPMul"
                |   FCOM => stream "\tX87FPCompare\t"
                |   FCOMP => stream "\tX87FPComparePop\t"
                |   FSUB => stream "\tX87FPSub\t"
                |   FSUBR => stream "\tX87FPRevSub\t"
                |   FDIV => stream "\tX87FPDiv\t"
                |   FDIVR => stream "\tX87FPRevDiv\t";
                printArg(arg1, stream, printReg); stream ",";
                printArg(arg2, stream, printReg); stream " => ";
                printReg(resultReg, stream)
            )
        
        |   printICode(X87FPUnaryOps{fpOp, dest, source}, stream, printReg) =
            (
                case fpOp of
                    FABS => stream "\tX87FPAbs\t"
                |   FCHS => stream "\tX87FPNegate\t"
                |   FLD1 => stream "\tX87FPLoad1\t"
                |   FLDZ => stream "\tX87FPLoad0\t";
                printArg(source, stream, printReg); stream " => ";
                printReg(dest, stream)
            )
        
        |   printICode(FloatFixedInt{dest, source}, stream, printReg) =
                (stream "\tFloatFixedInt\t"; printArg(source, stream, printReg); stream " => "; printReg(dest, stream))
        
        |   printICode(SSE2FPArith{opc, resultReg, arg1, arg2}, stream, printReg) =
            (
                case opc of
                    SSE2Move => stream "\tSSE2FPMove\t"
                |   SSE2Comp => stream "\tSSE2FPComp\t"
                |   SSE2Add => stream "\tSSE2FPAdd\t"
                |   SSE2Sub => stream "\tSSE2FPSub\t"
                |   SSE2Mul => stream "\tSSE2FPMul\t"
                |   SSE2Div => stream "\tSSE2FPDiv\t"
                |   SSE2Xor => stream "\tSSE2FPXor\t"
                |   SSE2And => stream "\tSSE2FPAnd\t"
                |   SSE2MoveSingle => stream "\tSSE2FPMoveSingle\t"
                |   SSE2DoubleToFloat => stream "\tSSE2FPDoubleToFloat\t";
                printArg(arg1, stream, printReg); stream ",";
                printArg(arg2, stream, printReg); stream " => ";
                printReg(resultReg, stream)
            )
            
        fun printConcreteReg(reg, stream) = stream(regRepr reg)

        and printAbstractReg(PReg(i, PRegGeneral), stream) = stream("G" ^ Int.toString i)
        |   printAbstractReg(PReg(i, PRegUntagged), stream) = stream("U" ^ Int.toString i)
        
        (* Print a basic block. *)
        fun printBlock (printItem, stream) (blockNo, BasicBlock{block, flow, ...}: 'reg basicBlock) =
        (
            (* Put a label on all but the first. *)
            if blockNo <> 0 then stream("L" ^ Int.toString blockNo ^ ":") else ();
            List.app (fn icode => printICode(icode, stream, printItem)) block;
            case flow of
                Unconditional l => stream("\tJump\tL" ^ Int.toString l ^ "\n")
            |   Conditional {condition, trueJump, falseJump, ...} =>
                (
                    case condition of
                        JO => stream "\tJumpOverflow\t"
                    |   JNO => stream "\tJumpNoOverflow\t"
                    |   JE => stream "\tJumpEqual\t"
                    |   JNE => stream "\tJumpNotEqual\t"
                    |   JL => stream "\tJumpLessSigned\t"
                    |   JGE => stream "\tJumpGeqSigned\t"
                    |   JLE => stream "\tJumpLeqSigned\t"
                    |   JG => stream "\tJumpGrtSigned\t"
                    |   JB => stream "\tJumpLessUnsigned\t"
                    |   JNB => stream "\tJumpGeqUnsigned\t"
                    |   JNA => stream "\tJumpLeqUnsigned\t"
                    |   JA => stream "\tJumpGrtUnsigned\t"
                    |   JP => stream "\tJumpParitySet"
                    |   JNP => stream "\tJumpParityClear\t";
                    stream(concat["L", Int.toString trueJump, " else L",  Int.toString falseJump, "\n"])
                )
            |   _ => ()
        )
    in
        fun printICodeConcrete(blockVec, stream) =
            Vector.appi(printBlock(printConcreteReg, stream)) blockVec

        and printICodeAbstract(blockVec, stream) =
            Vector.appi(printBlock(printAbstractReg, stream)) blockVec
    end

    (* We frequently just want to know the register. *)
    fun indexRegister NoMemIndex = NONE
    |   indexRegister (MemIndex1 r) = SOME r
    |   indexRegister (MemIndex2 r) = SOME r
    |   indexRegister (MemIndex4 r) = SOME r
    |   indexRegister (MemIndex8 r) = SOME r
    
    exception InternalError = Misc.InternalError

    (* Generate code from the ICode.  This assumes that all pseudo-registers have been replaced by
       real registers or locations.  Only certain patterns of arguments are accepted. *)
    fun codeAsX86Code{blockCode, stackRequired, inputRegisters: reg list, functionName, debugSwitches} =
    let
        val blockCount = Vector.length blockCode
        val outputLabelCount = ref 0
        val blockToLabelMap = Array.array(blockCount, ~1)

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
        
        fun asGenReg(GenReg r) = r
        |   asGenReg _ = raise InternalError "asGenReg"
        
        fun argAsGenReg(RegisterArgument(GenReg r)) = r
        |   argAsGenReg _ = raise InternalError "argAsGenReg"

        fun memoryAddressAsBaseOffset({offset, base=(GenReg baseReg), index}) =
            {base=baseReg, offset=offset, index=memoryIndexAsIndex index}
        |   memoryAddressAsBaseOffset _ = raise InternalError "memoryAddressAsBaseOffset"

        and memoryIndexAsIndex NoMemIndex = NoIndex
        |   memoryIndexAsIndex(MemIndex1((GenReg iReg))) = Index1 iReg
        |   memoryIndexAsIndex(MemIndex2((GenReg iReg))) = Index2 iReg
        |   memoryIndexAsIndex(MemIndex4((GenReg iReg))) = Index4 iReg
        |   memoryIndexAsIndex(MemIndex8((GenReg iReg))) = Index8 iReg
        |   memoryIndexAsIndex _ = raise InternalError "memoryIndexAsIndex"

        and sourceAsGenRegOrMem(RegisterArgument((GenReg r))) = RegisterArg r
        |   sourceAsGenRegOrMem(MemoryLocation{offset, base=(GenReg baseReg), index}) =
                MemoryArg{base=baseReg, offset=offset, index=memoryIndexAsIndex index}
        |   sourceAsGenRegOrMem(StackLocation{adjustment, wordOffset}) =
                MemoryArg{base=esp, offset=(adjustment+wordOffset)*wordSize, index=NoIndex}
        |   sourceAsGenRegOrMem(IntegerConstant v) = NonAddressConstArg v
        |   sourceAsGenRegOrMem(AddressConstant v) = AddressConstArg v
        |   sourceAsGenRegOrMem _ = raise InternalError "sourceAsGenRegOrMem"

        and sourceAsXMMRegOrMem(RegisterArgument((XMMReg r))) = RegisterArg r
        |   sourceAsXMMRegOrMem(MemoryLocation{offset, base=(GenReg baseReg), index}) =
                MemoryArg{base=baseReg, offset=offset, index=memoryIndexAsIndex index}
        |   sourceAsXMMRegOrMem(StackLocation{adjustment, wordOffset}) =
                MemoryArg{base=esp, offset=(adjustment+wordOffset)*wordSize, index=NoIndex}
        |   sourceAsXMMRegOrMem(IntegerConstant v) = NonAddressConstArg v
        |   sourceAsXMMRegOrMem(AddressConstant v) = AddressConstArg v
        |   sourceAsXMMRegOrMem _ = raise InternalError "sourceAsGenRegOrMem"

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
        
        (* Code-generate a block.  A couple of instructions require the "flow". *)
        fun codeGenBlock(BasicBlock{block, flow, ...}, code) =
        let

            (* Turn the icode into machine code.  This produces the code in reverse. *)
            fun codeGenICode([], code) = code

            |   codeGenICode(
                    (* Load to a general register or move to a general register. *)
                    LoadArgument{ source, dest=GenReg destReg, kind=MoveWord} :: rest, code) =
                        codeGenICode(rest, MoveToRegister { source=sourceAsGenRegOrMem source, output=destReg } :: code)

            |   codeGenICode(
                    (* Load from memory. *)
                    LoadArgument{ source=MemoryLocation mLoc, dest=GenReg destReg, kind=MoveByte} :: rest, code) =
                    codeGenICode(rest, LoadNonWord{size=Size8Bit, source=memoryAddressAsBaseOffset mLoc, output=destReg} :: code)

            |   codeGenICode(
                    (* Load from memory. *)
                    LoadArgument{ source=MemoryLocation mLoc, dest=GenReg destReg, kind=Move16Bit} :: rest, code) =
                    codeGenICode(rest, LoadNonWord{size=Size16Bit, source=memoryAddressAsBaseOffset mLoc, output=destReg} :: code)

            |   codeGenICode(
                    (* Load from memory. *)
                    LoadArgument{ source=MemoryLocation mLoc, dest=GenReg destReg, kind=Move32Bit} :: rest, code) =
                    codeGenICode(rest, LoadNonWord{size=Size32Bit, source=memoryAddressAsBaseOffset mLoc, output=destReg} :: code)

                    (* Store to memory *)
            |   codeGenICode(
                    StoreArgument{ source=RegisterArgument(GenReg sourceReg), base, offset, index, kind=MoveWord} :: rest, code) =
                    codeGenICode(rest,
                        StoreRegToMemory{toStore=sourceReg, address=memoryAddressAsBaseOffset{base=base, offset=offset, index=index}} :: code)

            |   codeGenICode(
                    StoreArgument{ source=RegisterArgument(GenReg sourceReg), base, offset, index, kind=MoveByte} :: rest, code) =
                    codeGenICode(rest,
                        StoreNonWord{size=Size8Bit, toStore=sourceReg, address=memoryAddressAsBaseOffset {base=base, offset=offset, index=index}} :: code)

            |   codeGenICode(
                    StoreArgument{ source=RegisterArgument(GenReg sourceReg), base, offset, index, kind=Move16Bit} :: rest, code) =
                    codeGenICode(rest,
                        StoreNonWord{size=Size16Bit, toStore=sourceReg, address=memoryAddressAsBaseOffset {base=base, offset=offset, index=index}} :: code)

            |   codeGenICode(
                    StoreArgument{ source=RegisterArgument(GenReg sourceReg), base, offset, index, kind=Move32Bit} :: rest, code) =
                    codeGenICode(rest,
                        StoreNonWord{size=Size32Bit, toStore=sourceReg, address=memoryAddressAsBaseOffset {base=base, offset=offset, index=index}} :: code)

                    (* Store a short constant to memory *)
            |   codeGenICode(
                    StoreArgument{ source=IntegerConstant srcValue, base, offset, index, kind=MoveWord} :: rest, code) =
                    codeGenICode(rest,
                        StoreConstToMemory{toStore=srcValue, address=memoryAddressAsBaseOffset {base=base, offset=offset, index=index}} :: code)

            |   codeGenICode(
                    StoreArgument{ source=IntegerConstant srcValue, base, offset, index, kind=MoveByte} :: rest, code) =
                    codeGenICode(rest,
                        StoreNonWordConst{size=Size8Bit, toStore=srcValue, address=memoryAddressAsBaseOffset {base=base, offset=offset, index=index}} :: code)

                    (* Store a long constant to memory *)
            |   codeGenICode(
                    StoreArgument{ source=AddressConstant srcValue, base, offset, index, kind=MoveWord} :: rest, code) =
                    codeGenICode(rest,
                        StoreLongConstToMemory{toStore=srcValue, address=memoryAddressAsBaseOffset {base=base, offset=offset, index=index}} :: code)

                    (* Load a floating point value. *)
            |   codeGenICode(LoadArgument{source=MemoryLocation{offset, base=(GenReg baseReg), index},
                                    dest=FPReg fpReg, kind=MoveDouble} :: rest, code) =
                let
                    val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: Load FPReg <> fp0"
                in
                    codeGenICode(rest, FPLoadFromMemory{ address={base=baseReg, offset=offset, index=memoryIndexAsIndex index}, precision=DoublePrecision } :: code)
                end

                    (* Load or move from an XMM reg. *)
            |   codeGenICode(LoadArgument{source, dest=XMMReg xmmRegReg, kind=MoveDouble} :: rest, code) =
                    codeGenICode(rest, XMMArith { opc= SSE2Move, source=sourceAsXMMRegOrMem source, output=xmmRegReg } :: code)

                    (* Store a floating point value. *)
            |   codeGenICode(StoreArgument{source=RegisterArgument(FPReg fpReg),
                             offset, base=(GenReg baseReg), index, kind=MoveDouble} :: rest, code) =
                let
                    val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: Store FPReg <> fp0"
                in
                    codeGenICode(rest, FPStoreToMemory{ address={ base=baseReg, offset=offset, index=memoryIndexAsIndex index}, precision=DoublePrecision, andPop=true } :: code)
                end

            |   codeGenICode(StoreArgument{source=RegisterArgument(XMMReg xmmRegReg),
                             offset, base=(GenReg baseReg), index, kind=MoveDouble} :: rest, code) =
                    codeGenICode(rest, XMMStoreToMemory { toStore=xmmRegReg, address={base=baseReg, offset=offset, index=memoryIndexAsIndex index}, precision=DoublePrecision } :: code)

                    (* Load a floating point value. *)
            |   codeGenICode(LoadArgument{source=MemoryLocation{offset, base=(GenReg baseReg), index},
                                    dest=FPReg fpReg, kind=MoveFloat} :: rest, code) =
                let
                    val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: Load FPReg <> fp0"
                in
                    codeGenICode(rest, FPLoadFromMemory{ address={ base=baseReg, offset=offset, index=memoryIndexAsIndex index }, precision=SinglePrecision } :: code)
                end

                    (* Load or move from an XMM reg. *)
            |   codeGenICode(LoadArgument{source, dest=XMMReg xmmRegReg, kind=MoveFloat} :: rest, code) =
                    codeGenICode(rest, XMMArith { opc= SSE2MoveSingle, source=sourceAsXMMRegOrMem source, output=xmmRegReg } :: code)

                    (* Store a floating point value. *)
            |   codeGenICode(StoreArgument{source=RegisterArgument(FPReg fpReg),
                             offset, base=(GenReg baseReg), index, kind=MoveFloat} :: rest, code) =
                let
                    val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: Store FPReg <> fp0"
                in
                    codeGenICode(rest, FPStoreToMemory{address={ base=baseReg, offset=offset, index=memoryIndexAsIndex index}, precision=SinglePrecision, andPop=true } :: code)
                end

            |   codeGenICode(StoreArgument{source=RegisterArgument(XMMReg xmmRegReg),
                             offset, base=(GenReg baseReg), index, kind=MoveFloat} :: rest, code) =
                    codeGenICode(rest, XMMStoreToMemory { toStore=xmmRegReg, address={base=baseReg, offset=offset, index=memoryIndexAsIndex index}, precision=SinglePrecision } :: code)

                (* Any other combinations are not allowed. *)
            |   codeGenICode(LoadArgument _ :: _, _) = raise InternalError "codeGenICode: LoadArgument"

            |   codeGenICode(StoreArgument _ :: _, _) = raise InternalError "codeGenICode: StoreArgument"

                (* This should have been transformed into MoveArgument. *)
            |   codeGenICode(LoadMemReg _ :: _, _) = raise InternalError "codeGenICode - LoadMemReg"

                (* Exchange two general registers. *)
            |   codeGenICode(ExchangeRegisters{ regX, regY} :: rest, code) =
                    codeGenICode(rest, XChngRegisters { regX=asGenReg regX, regY=asGenReg regY } :: code)

            |   codeGenICode(BeginFunction _ :: rest, code) = codeGenICode(rest, code) (* Don't need to do anything. *)

            |   codeGenICode(FunctionCall {callKind, ...} :: rest, code) =
                    codeGenICode(rest, CallFunction callKind :: code)

            |   codeGenICode(TailRecursiveCall {callKind, ...} :: rest, code) =
                    codeGenICode(rest, JumpToFunction callKind :: code)

            |   codeGenICode(AllocateMemoryOperation { size, flags, dest, saveRegs} :: rest, code) =
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

            |   codeGenICode(AllocateMemoryVariable{ size, dest, saveRegs} :: rest, code) =
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

            |   codeGenICode(InitialiseMem{size, addr, init} :: rest, code) =
                let
                    val sReg = asGenReg size and iReg = asGenReg init and aReg = asGenReg addr
                    (* Initialise the memory.  This requires that sReg = ecx, iReg = eax and aReg = edi. *)
                    val _ = sReg = ecx orelse raise InternalError "codeGenICode: InitialiseMem"
                    val _ = iReg = eax orelse raise InternalError "codeGenICode: InitialiseMem"
                    val _ = aReg = edi orelse raise InternalError "codeGenICode: InitialiseMem"
                in
                    codeGenICode(rest, RepeatOperation STOSL :: code)
                end

            |   codeGenICode(InitialisationComplete :: rest, code) =
                    codeGenICode(rest, StoreInitialised :: code)

            |   codeGenICode(StartLoop _ :: _, _) = (* Same as ForwardLabel. *)
                    raise InternalError "codeGenICode - StartLoop"

            |   codeGenICode(EndLoop _ :: rest, code) = (* Nothing to do here. *)
                    codeGenICode(rest, code)

            |   codeGenICode(JumpLoop {checkInterrupt, ...} :: rest, code) =
                let
                    val checkCode =
                        case checkInterrupt of
                            NONE => []
                        |   SOME saveRegs => testRegAndTrap (esp, StackOverflowCall, map asGenReg saveRegs)
                in
                    codeGenICode(rest, (*UncondBranch (findLabelRef loopLabel) ::*) checkCode @ code)
                end
 
            |   codeGenICode(RaiseExceptionPacket _ :: rest, code) =
                    codeGenICode(rest, RaiseException :: code)

            |   codeGenICode(IndexedCaseOperation { testReg, workReg, ...} :: rest, code) =
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

            |   codeGenICode(LockMutable { addr } :: rest, code) =
                    codeGenICode(rest, LockMutableSegment (asGenReg addr) :: code)

            |   codeGenICode(ForwardJumpLabel _ :: _, _) =
                    raise InternalError "codeGenICode: ForwardJumpLabel"

            |   codeGenICode(UnconditionalForwardJump _ :: _, _) =
                    raise InternalError "codeGenICode: UnconditionalForwardJump"

            |   codeGenICode(ConditionalForwardJump _ :: _, _) =
                    raise InternalError "codeGenICode: ConditionalForwardJump"

            |   codeGenICode(WordComparison {arg1=RegisterArgument(GenReg r), arg2, ...} :: rest, code) =
                    codeGenICode(rest, ArithToGenReg {opc=CMP, output=r, source=sourceAsGenRegOrMem arg2} :: code)

            |   codeGenICode(WordComparison _ :: _, _) =
                    raise InternalError "codeGenICode: TODO WordComparison"

            |   codeGenICode(PushExceptionHandler { workReg, ... } :: rest, code) =
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

            |   codeGenICode(PopExceptionHandler { workReg, ... } :: rest, code) =
                let (* Remove an exception handler if no exception was raised. *)
                    val wReg = asGenReg workReg
                in
                    (* The stack pointer has been adjusted to just above the two words that were stored
                       in PushExceptionHandler. *)
                    codeGenICode(rest,
                        StoreRegToMemory{
                            toStore=wReg, address={offset=memRegHandlerRegister, base=ebp, index=NoIndex}} ::
                        PopR wReg ::
                        ResetStack 1 :: code)
                end
 
            |   codeGenICode(BeginHandler {workReg, packetReg, ...} :: rest, code) =
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
                        PopR wReg :: ResetStack 1 ::
                        MoveToRegister{ source=MemoryArg{base=ebp, offset=memRegHandlerRegister, index=NoIndex}, output=esp } :: code)
                end

            |   codeGenICode(ReturnResultFromFunction { numStackArgs, ... } :: rest, code) =
                    codeGenICode(rest, ReturnFromFunction numStackArgs :: code)

            |   codeGenICode(
                    ArithmeticFunction{
                        oper, resultReg=(GenReg resReg), operand1=RegisterArgument(GenReg op1Reg),
                        operand2, ...} :: rest, code) =
                let
                    val _ = resReg = op1Reg orelse raise InternalError "codeGenICode: ArithmeticFunction"
                in
                    codeGenICode(rest, ArithToGenReg { opc=oper, output=resReg, source=sourceAsGenRegOrMem operand2 } :: code)
                end

            |   codeGenICode(ArithmeticFunction _ :: _, _) =
                    raise InternalError "codeGenICode: TODO codeGenICode - ArithmeticFunction"

            |   codeGenICode(TestTagBit {arg = RegisterArgument reg, ...} :: rest, code) =
                    codeGenICode(rest, TestTagR(asGenReg reg) :: code)

            |   codeGenICode(TestTagBit {arg = StackLocation {adjustment, wordOffset}, ...} :: rest, code) =
                    (* Since the X86 is little-endian we check the addressed byte. *)
                    codeGenICode(rest, TestByteMem{base=esp, offset=(adjustment+wordOffset)*wordSize, bits=0w1} :: code)

            |   codeGenICode(TestTagBit {arg = MemoryLocation {offset, base, index=NoMemIndex}, ...} :: rest, code) =
                    codeGenICode(rest, TestByteMem{base=asGenReg base, offset=offset, bits=0w1} :: code)

            |   codeGenICode(TestTagBit _ :: _, _) = raise InternalError "codeGenICode: TestTagBit"

            |   codeGenICode(PushValue { arg = RegisterArgument reg } :: rest, code) =
                    codeGenICode(rest, PushToStack(RegisterArg(asGenReg reg)) :: code)

            |   codeGenICode(PushValue { arg = IntegerConstant v } :: rest, code) =
                    codeGenICode(rest, PushToStack(NonAddressConstArg v) :: code)

            |   codeGenICode(PushValue { arg = AddressConstant v } :: rest, code) =
                    codeGenICode(rest, PushToStack(AddressConstArg v) :: code)

            |   codeGenICode(PushValue { arg = StackLocation {adjustment, wordOffset} } :: rest, code) =
                    (* The X86 manual says that the address is computed before the push. *)
                    codeGenICode(rest, PushToStack(MemoryArg{base=esp, offset=(adjustment+wordOffset)*wordSize, index=NoIndex}) :: code)

            |   codeGenICode(PushValue { arg = MemoryLocation {offset, base, index} } :: rest, code) =
                    codeGenICode(rest, PushToStack(MemoryArg{base=asGenReg base, offset=offset, index=memoryIndexAsIndex index}) :: code)

            |   codeGenICode(ResetStackPtr {numWords} :: rest, code) =
                (
                    numWords >= 0 orelse raise InternalError "codeGenICode: ResetStackPtr - negative offset";
                    codeGenICode(rest, ResetStack numWords :: code)
                )

            |   codeGenICode(TagValue _ :: _, _) =
                    raise InternalError "codeGenICode: TODO TagValue"

            |   codeGenICode(UntagValue _ :: _, _) =
                    raise InternalError "codeGenICode: TODO UntagValue"

            |   codeGenICode(LoadEffectiveAddress { base, offset, index, dest } :: rest, code) =
                let
                    val bReg = Option.map asGenReg base
                    val indexR = memoryIndexAsIndex index
                in
                    codeGenICode(rest, LoadAddress{ output=asGenReg dest, offset=offset, base=bReg, index=indexR } :: code)
                end

            |   codeGenICode(
                    ShiftOperation{ shift, resultReg, operand, shiftAmount=IntegerConstant shiftValue, ...} :: rest, code) =
                let
                    val resReg = asGenReg resultReg and opReg = argAsGenReg operand
                    val _ = resReg = opReg orelse raise InternalError "codeGenICode: ShiftOperation"
                in
                    codeGenICode(rest, ShiftConstant{ shiftType=shift, output=resReg, shift=Word8.fromLargeInt shiftValue } :: code)
                end

            |   codeGenICode(ShiftOperation { shift, resultReg, operand, shiftAmount, ...} :: rest, code) =
                let
                    val resReg = asGenReg resultReg and opReg = argAsGenReg operand
                    val _ = resReg = opReg orelse raise InternalError "codeGenICode: ShiftOperation"
                    (* The amount to shift must be in ecx.  The shift is masked to 5 or 6 bits so we have to
                       check for larger shift values at a higher level. *)
                    val _ = argAsGenReg shiftAmount = ecx orelse raise InternalError "codeGenICode: ShiftOperation"
                in
                    codeGenICode(rest, ShiftVariable{ shiftType=shift, output=resReg } :: code)
                end

            |   codeGenICode(Multiplication { resultReg, operand1, operand2=MemoryLocation{offset, base, index=NoMemIndex}, ... } :: rest, code) =
                let
                    val resReg = asGenReg resultReg and op1Reg = argAsGenReg operand1 and baseReg = asGenReg base
                    val _ = resReg = op1Reg orelse raise InternalError "codeGenICode: Multiplication"
                in
                    codeGenICode(rest, MultiplyRM { base=baseReg, offset=offset, output=resReg } :: code)
                end

            |   codeGenICode(Multiplication { resultReg, operand1, operand2, ... } :: rest, code) =
                let
                    val resReg = asGenReg resultReg and op1Reg = argAsGenReg operand1 and op2Reg = argAsGenReg operand2
                    val _ = resReg = op1Reg orelse raise InternalError "codeGenICode: Multiplication"
                in
                    codeGenICode(rest, MultiplyRR { source=op2Reg, output=resReg } :: code)
                end

            |   codeGenICode(Division { isSigned, dividend, divisor, quotient, remainder } :: rest, code) =
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

            |   codeGenICode(AtomicExchangeAndAdd{ base, source } :: rest, code) =
                let
                    val baseReg = asGenReg base and outReg = asGenReg source
                in
                    codeGenICode(rest, AtomicXAdd{base=baseReg, output=outReg} :: code)
                end

            |   codeGenICode(CompareByteVectors { vec1Addr, vec2Addr, length, ... } :: rest, code) =
                let
                    (* The arguments must be in specific registers. *)
                    val _ = asGenReg vec1Addr = esi orelse raise InternalError "CompareByteVectors: esi"
                    val _ = asGenReg vec2Addr = edi orelse raise InternalError "CompareByteVectors: edi"
                    val _ = asGenReg length = ecx orelse raise InternalError "CompareByteVectors: ecx"
                in
                    codeGenICode(rest, RepeatOperation CMPSB :: code)
                end

            |   codeGenICode(BlockMove { srcAddr, destAddr, length, isByteMove } :: rest, code) =
                let
                    (* The arguments must be in specific registers. *)
                    val _ = asGenReg srcAddr = esi orelse raise InternalError "BlockMove: esi"
                    val _ = asGenReg destAddr = edi orelse raise InternalError "BlockMove: edi"
                    val _ = asGenReg length = ecx orelse raise InternalError "BlockMove: ecx"
                in
                    codeGenICode(rest, RepeatOperation(if isByteMove then MOVSB else MOVSL) :: code)
                end

            |   codeGenICode(
                    CompareFloatingPt {
                        arg1=RegisterArgument(FPReg fpReg),
                        arg2=MemoryLocation{offset, base=(GenReg baseReg), index=NoMemIndex}, ... } :: rest, code) =
                let
                    val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: CompareFloatingPt not fp0"
                    (* This currently pops the value. *)
                    (*val _ = fpMode = FPModeX87 orelse raise InternalError "codeGenICode: FCOMP in SSE2 mode"*)
                in
                    codeGenICode(rest, FPArithMemory{opc=FCOMP, base=baseReg, offset=offset} :: code)
                end

            |   codeGenICode(CompareFloatingPt {arg1=RegisterArgument(XMMReg xmmReg), arg2, ... } :: rest, code) =
                    codeGenICode(rest, XMMArith { opc= SSE2Comp, output=xmmReg, source=sourceAsXMMRegOrMem arg2} :: code)

            |   codeGenICode(CompareFloatingPt _ :: _, _) =
                    raise InternalError "codeGenICode: CompareFloatingPt: TODO"

            |   codeGenICode(X87FPGetCondition { dest, ... } :: rest, code) =
                let
                    val _ = asGenReg dest = eax orelse raise InternalError "codeGenICode: GetFloatingPtCondition not eax"
                    (* This currently pops the value. *)
                    (*val _ = fpMode = FPModeX87 orelse raise InternalError "codeGenICode: FPStatusToEAX in SSE2 mode"*)
                in
                    codeGenICode(rest, FPStatusToEAX :: code)
                end

            |   codeGenICode(
                    X87FPArith {
                        opc, resultReg=(FPReg fpResReg), arg1=RegisterArgument(FPReg fpArgReg),
                        arg2=MemoryLocation{offset, base=(GenReg baseReg), index=NoMemIndex} } :: rest, code) =
                let
                    val _ = fpResReg = fp0 orelse raise InternalError "codeGenICode: FloatingPointArith not fp0"
                    val _ = fpArgReg = fp0 orelse raise InternalError "codeGenICode: FloatingPointArith not fp0"
                in
                    codeGenICode(rest, FPArithMemory{opc=opc, base=baseReg, offset=offset} :: code)
                end

            |   codeGenICode(X87FPArith _ :: _, _) =
                    raise InternalError "codeGenICode: X87FPArith: TODO"

            |   codeGenICode(
                    SSE2FPArith {
                        opc, resultReg=(XMMReg xmmResReg), arg1=RegisterArgument(XMMReg xmmArgReg), arg2 } :: rest, code) =
                let
                    val _ = xmmResReg = xmmArgReg orelse raise InternalError "codeGenICode: FloatingPointArith - different regs"
                    (* xorpd and andpd require 128-bit arguments with 128-bit alignment. *)
                    val _ =
                        case (opc, arg2) of
                            (SSE2Xor, RegisterArgument _) => ()
                        |   (SSE2Xor, _) => raise InternalError "codeGenICode - SSE2Xor not in register"
                        |   (SSE2And, RegisterArgument _) => ()
                        |   (SSE2And, _) => raise InternalError "codeGenICode - SSE2And not in register"
                        |   _ => ()
                in
                    codeGenICode(rest, XMMArith{ opc=opc, output=xmmResReg, source=sourceAsXMMRegOrMem arg2} :: code)
                end

            |   codeGenICode(SSE2FPArith _ :: _, _) =
                    raise InternalError "codeGenICode: SSE2FPArith: TODO"

            |   codeGenICode(X87FPUnaryOps {fpOp, dest=(FPReg fpResReg), source=RegisterArgument(FPReg fpArgReg)} :: rest, code) =
                let
                    val _ = fpResReg = fp0 orelse raise InternalError "codeGenICode: X87FPUnaryOps not fp0"
                    val _ = fpArgReg = fp0 orelse raise InternalError "codeGenICode: X87FPUnaryOps not fp0"
                in
                    codeGenICode(rest, FPUnary fpOp :: code)
                end

            |   codeGenICode(X87FPUnaryOps _ :: _, _) =
                    raise InternalError "codeGenICode: FloatingPointNeg: TODO"

            |   codeGenICode(FloatFixedInt { dest=(XMMReg xmmResReg), source=RegisterArgument(GenReg srcReg) } :: rest, code) =
                    codeGenICode(rest, XMMConvertFromInt{ output=xmmResReg, source=srcReg} :: code)

            |   codeGenICode(FloatFixedInt { dest=(FPReg fpReg), source=MemoryLocation{base, offset, index=NoMemIndex} } :: rest, code) =
                let
                    val _ = fpReg = fp0 orelse raise InternalError "codeGenICode: FloatFixedInt not fp0"
                in
                    codeGenICode(rest, FPLoadInt{ base=asGenReg base, offset=offset} :: code)
                end

            |   codeGenICode(FloatFixedInt _ :: _, _) =
                    raise InternalError "codeGenICode: FloatFixedInt: TODO"

                (* ReserveContainer should have been removed by earlier passes. *)
            |   codeGenICode(ReserveContainer _ :: _, _) =
                    raise InternalError "codeGenICode: ReserveContainer"
                
                (* BoxValue should have been removed by earlier passes. *)
            |   codeGenICode(BoxValue _ :: _, _) =
                    raise InternalError "codeGenICode: BoxValue"
        in
            codeGenICode(block, code)
        end (* codeGenBlock *)
        
        val minStackCheck = 20
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
            val processed = Array.array(Vector.length blockCode, false)
            fun haveProcessed n = Array.sub(processed, n)

            fun genCode(toDo, lastFlow, lastBlockNo, code) =
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
                in
                    finalBranch @ code (* Done. *)
                end

            |   stillToDo as head :: _ =>
                (* For the moment just select the first.  This may not be ideal because it means
                   we follow unconditional branches immediately.  If we have if-then-else we will
                   have an unconditional branch at the end of the "then".  We may be better putting
                   in an unconditional branch and processing the "else-part" before joining
                   the two arms. *)
                let
                    local
                        fun available dest =
                            case Vector.sub(blockCode, dest) of
                                BasicBlock { source=BranchSource sources, ...} =>
                                    (* All the sources that lead up to this have already been
                                       processed so if we do this we won't have any backward jumps. *)
                                    List.all haveProcessed sources 
                            |   _ => true (* ??? *)
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
                        |   Unconditional dest =>
                                if dest = picked then [] else [UncondBranch(getBlockLabel dest)]
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

                    val blockToCode as BasicBlock{ flow, source, ...} = Vector.sub(blockCode, picked)
                    
                    (* Code-generate the body with the code we've done so far
                       at the end.  Add a label at the start if necessary. *)
                    val startLabel =
                        case source of
                            HandleSource _ => [JumpLabel(getBlockLabel picked)]
                        |   RootSource => []
                        |   BranchSource [singleton] =>
                                (* The only place this is referred to is the previous block and it wasn't
                                   an indexed branch. *)
                                if singleton = lastBlockNo andalso
                                    (case lastFlow of IndexedBr _ => false | _ => true)
                                then [] else [JumpLabel(getBlockLabel picked)]
                        |   BranchSource _ => [JumpLabel(getBlockLabel picked)]

                    val bodyCode = codeGenBlock(blockToCode, startLabel @ startCode @ code)

                    val addSet =
                        case flow of
                            ExitCode => []
                        |   IndexedBr cases => cases
                        |   Unconditional dest => [dest]
                        |   Conditional {trueJump, falseJump, ...} => [falseJump, trueJump]
                        |   SetHandler { handler, continue } => [handler, continue]

                in
                    genCode(addSet @ stillToDo, flow, picked, bodyCode)
                end
        in
            val ops = genCode([0], ExitCode, ~1, preludeCode)
        end
    in
        X86OPTIMISE.generateCode{code=newCode, ops=List.rev ops, labelCount= !outputLabelCount}
    end
    
    (* Convert the old single list into a list of basic blocks. *)
    fun oldToNew(ops: 'reg x86ICode list, maxLabels): 'reg basicBlock vector =
    let
        (* Count the number of conditional branches and exception handlers.  We are going to add a block for
           the drop-through case of a conditional branch so we have to add those. *)
        val numExtras =
            List.foldl(fn (ConditionalForwardJump _, n) => n+1 | (PushExceptionHandler _, n) => n+1 | (_, n) => n) 0 ops

        local
            val initial = BasicBlock{ source=BranchSource [], block=[], flow=ExitCode }
        in
            val resArray = Array.array(maxLabels+numExtras, initial)
        end
        val additionalLabs = ref maxLabels
        
        (* Create an entry for the block vector.  The code needs to be reversed. *)
        fun createEntry (blockNo, source, block, flow) =
            Array.update(resArray, blockNo,
                BasicBlock{ source=source, block=List.rev block, flow=flow})
        
        fun splitCode([], _, _, _) =
            (* End of code.  We should have had a return or other unconditional transfer. *)
                raise InternalError "splitCode"

        |   splitCode(ForwardJumpLabel{ label=ILabel labelNo ,... } :: rest, index, source, sinceLabel) =
            (* We've found a label that wasn't preceded by an unconditional branch or other exit.
               The action at the end is to jump to this label. *)
            (
                createEntry(index, source, sinceLabel, Unconditional labelNo);
                splitCode(rest, labelNo, BranchSource [], [])
            )

        |   splitCode(StartLoop{ loopLabel=ILabel labelNo,... } :: rest, index, source, sinceLabel) =
            (* A loop label.  Treat this as a normal label. *)
            (
                createEntry(index, source, sinceLabel, Unconditional labelNo);
                splitCode(rest, labelNo, BranchSource [], [])
            )

        |   splitCode(EndLoop _ :: rest, index, source, sinceLabel) =
            (* EndLoop - remove. *)
                splitCode(rest, index, source, sinceLabel)

        |   splitCode(ConditionalForwardJump { ccRef, condition, label=ILabel trueLabel} :: rest, index, source, sinceLabel) =
            (* Conditional jump. This finishes a block but we need a new label for the drop-through case. *)
            let
                val dropLabel = ! additionalLabs before additionalLabs := !additionalLabs + 1;
                val flow =
                    Conditional {ccRef=ccRef, condition=condition, trueJump=trueLabel, falseJump=dropLabel}
                val () = createEntry(index, source, sinceLabel, flow)
            in
                splitCode(rest, dropLabel, BranchSource [], [])
            end

        |   splitCode(UnconditionalForwardJump { label=ILabel labelNo } :: rest, index, source, sinceLabel) =
            (
                createEntry(index, source, sinceLabel, Unconditional labelNo);
                skipCode rest
            )

        |   splitCode((instr as PushExceptionHandler { handleStart=ILabel labelNo, ... }) :: rest, index, source, sinceLabel) =
            (* Set up an exception handler.  We need to include this instruction because it includes the
               work register.  Create a new label for the continuation i.e. the start of the code that the
               handler catches exceptions for. *)
            let
                val dropLabel = ! additionalLabs before additionalLabs := !additionalLabs + 1
                val () = createEntry(index, source, instr :: sinceLabel, SetHandler{ handler=labelNo, continue=dropLabel })
            in
                splitCode(rest, dropLabel, BranchSource [], [])
            end

        |   splitCode((instr as JumpLoop { loopLabel=ILabel labelNo, ... }) :: rest, index, source, sinceLabel) =
            (* Jump to start of loop.  Treat as an unconditional jump but retain the instruction since
               it indicates the registers that need to copied. *)
            (
                createEntry(index, source, instr :: sinceLabel, Unconditional labelNo);
                skipCode rest
            )

        |   splitCode((instr as RaiseExceptionPacket _) :: rest, index, source, sinceLabel) =
            (
                createEntry(index, source, instr :: sinceLabel, ExitCode);
                skipCode rest
            )

        |   splitCode((instr as TailRecursiveCall _) :: rest, index, source, sinceLabel) =
            (
                createEntry(index, source, instr :: sinceLabel, ExitCode);
                skipCode rest
            )

        |   splitCode((instr as ReturnResultFromFunction _) :: rest, index, source, sinceLabel) =
            (
                createEntry(index, source, instr :: sinceLabel, ExitCode);
                skipCode rest
            )

        |   splitCode((instr as IndexedCaseOperation{cases, ...}) :: rest, index, source, sinceLabel) =
            (
                createEntry(index, source, instr :: sinceLabel, IndexedBr(map (fn ILabel i => i) cases));
                skipCode rest
            )

        |   splitCode(instr :: rest, index, source, sinceLabel) =
                splitCode(rest, index, source, instr :: sinceLabel)
                
                
        and skipCode [] = () (* Finished - reached the end. *)

        |   skipCode(ForwardJumpLabel{ label=ILabel labelNo,... } :: rest) =
            (* Found a label after an unconditional transfer. *)
                splitCode(rest, labelNo, BranchSource [], [])

        |   skipCode((instr as BeginHandler { handleStart=ILabel lab, ... }) :: rest) =
            (* Found the start of a handler.  We need to include the old handle instruction
               because that contains register information. *)
                splitCode(rest, lab, HandleSource ~1, [instr])

        |   skipCode(_ :: rest) = skipCode rest
                (* Unreachable code - Can occur with some branches. *)

        val () = splitCode(ops, 0, RootSource, [])
        
        local
            (* The target of a label is usually the label itself except that
               if the target is an unconditional branch with no other code
               we use the target of that branch.  This can also arise if
               we have successive labels.  An infinite loop could
               have a target which is the label itself.  That should not
               happen since we should always have a JumpLoop. *)
            fun findTarget labs labNo =
                case Array.sub(resArray, labNo) of
                    BasicBlock { source=BranchSource _, block=[], flow=Unconditional nextLab, ...} =>
                        if List.exists(fn l => l=nextLab) labs
                        then raise InternalError "findTarget - loop"
                        else findTarget (nextLab :: labs) nextLab
               |    _ => labNo
        in
            val getTarget = findTarget []
        end
        
        local
            (* Find the blocks that reference this one. *)
            val labelRefs = Array.array(maxLabels+numExtras, [])
            
            (* Starting from the root find all the reachable blocks and set the
               block numbers that jump to them. *)
            fun setReferences forLabel =
            let
                (* Find the references this makes. *)
                val BasicBlock{ flow, source, block } = Array.sub(resArray, forLabel)
                fun updateFlow newFlow =
                    Array.update(resArray, forLabel, BasicBlock{flow=newFlow, source=source, block=block })
                val refs =
                    case flow of
                        ExitCode => []
                    |   Unconditional lab =>
                        let
                            val newLab = getTarget lab
                            val () = if lab <> newLab then updateFlow(Unconditional newLab) else ()
                        in
                            [newLab]
                        end
                    |   Conditional{trueJump, falseJump, ccRef, condition } =>
                        let
                            val newTrue = getTarget trueJump and newFalse = getTarget falseJump
                            val () =
                                if newTrue <> trueJump orelse newFalse <> falseJump
                                then updateFlow(Conditional{trueJump=newTrue, falseJump=newFalse, ccRef=ccRef, condition=condition })
                                else ()
                        in
                            [newTrue, newFalse]
                        end
                    |   IndexedBr labs => labs
                    |   SetHandler { handler, continue } =>
                        let
                            (* The handler should not move.  It's referenced by PushExceptionHandler *)
                            val newContinue = getTarget continue
                            val () =
                                if newContinue <> continue
                                then updateFlow(SetHandler { handler=handler, continue=newContinue })
                                else ()
                        in
                            [newContinue, handler]
                        end
                
                (* For each of its references, add this block to the list of
                   blocks it references and, if this is the first time we've visited,
                   do the same for this block. *)
                fun setRefs toLabel =
                let
                    val originalRefs = Array.sub(labelRefs, toLabel)
                    val () = Array.update(labelRefs, toLabel, forLabel :: originalRefs)
                in
                    case originalRefs of
                        [] => (* We hadn't seen this before. *)
                            setReferences toLabel
                    |   _ => ()
                end
            in
                List.app setRefs refs
            end

            val () = setReferences 0
        
            (* Copy the reference information into the result. *)

            fun addRefs(n, BasicBlock{ source=BranchSource _, block, flow }) =
                    BasicBlock{source=BranchSource(Array.sub(labelRefs, n)), block=block, flow=flow}
            |   addRefs(n, BasicBlock{ source=HandleSource _, block, flow }) =
                let
                    (* There should be at most one block that has set up this handler.
                       It's possible there is none if the code to set up the handler was
                       unreachable. *)
                    val handleSource =
                        case Array.sub(labelRefs, n) of
                            [reference] => reference
                        |   [] => ~1
                        |   _ => raise InternalError "More than one handler source"
                in
                    BasicBlock{source=HandleSource handleSource, block=block, flow=flow}
                end
            |   addRefs(_, BasicBlock{ source, block, flow }) =
                    BasicBlock{source=source, block=block, flow=flow}
        in
            val () = Array.modifyi addRefs resArray
        end
    in
        Array.vector resArray
    end
    
    datatype destinations = RegDest of reg | StackDest of int

    structure Sharing =
    struct
        type genReg         = genReg
        and  'reg argument  = 'reg argument
        and  iLabel         = iLabel
        and  'reg x86ICode  = 'reg x86ICode
        and  branchOps      = branchOps
        and  reg            = reg
        and abstract        = abstract
        and destinations    = destinations
        and blockSource     = blockSource
        and controlFlow     = controlFlow
        and 'reg basicBlock = 'reg basicBlock
    end

end;
