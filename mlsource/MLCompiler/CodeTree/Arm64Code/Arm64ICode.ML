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

functor Arm64ICode(

    structure Arm64Code: ARM64PREASSEMBLY

): ARM64ICODE =

struct
    open Arm64Code

    open Address
    datatype preg = PReg of int (* A pseudo-register - an abstract register. *)

    (* If the value is zero we can use X0/W0. *)
    datatype pregOrZero = SomeReg of preg | ZeroReg
    
    (* A location on the stack.  May be more than word if this is a container or a handler entry. *)
    datatype stackLocn = StackLoc of {size: int, rno: int }
    
    (* This combines pregKind and stackLocn.  *)
    datatype regProperty =
        RegPropGeneral      (* A general register. *)
    |   RegPropUntagged     (* An untagged general register. *)
    |   RegPropStack of int (* A stack location or container. *)
    |   RegPropCacheTagged
    |   RegPropCacheUntagged
    |   RegPropMultiple     (* The result of a conditional or case. May be defined at multiple points. *)

    (* The reference to a condition code. *)
    datatype ccRef = CcRef of int
    datatype reg = GenReg of xReg | FPReg of vReg

    datatype callKind = Recursive | ConstantCode of machineWord | FullCall

    (* Function calls can have an unlimited number of arguments so it isn't always
       going to be possible to load them into registers. *)
    datatype fnarg = ArgInReg of preg | ArgOnStack of { wordOffset: int, container: stackLocn, field: int }


    datatype arm64ICode =
        (* Move the contents of one preg to another.  These are always 64-bits. *)
        MoveRegister of { source: preg, dest: preg }

        (* Numerical constant. *)
    |   LoadNonAddressConstant of { source: Word64.word, dest: preg }

        (* Address constant. *)
    |   LoadAddressConstant of { source: machineWord, dest: preg }

        (* Load a value into a register using a constant, signed, byte offset.  The offset
           is in the range of -256 to (+4095*unit size). *)
    |   LoadWithConstantOffset of { base: preg, dest: preg, byteOffset: int, loadType: loadType }

        (* Similarly for FP registers. *)
    |   LoadFPWithConstantOffset of { base: preg, dest: preg, byteOffset: int, floatSize: floatSize }

        (* Load a value into a register using an index register. *)
    |   LoadWithIndexedOffset of { base: preg, dest: preg, index: preg, loadType: loadType }

        (* Ditto for FP. *)
    |   LoadFPWithIndexedOffset of { base: preg, dest: preg, index: preg, floatSize: floatSize }

        (* Returns the current thread ID.  Always a 64-bit value.. *)
    |   GetThreadId of { dest: preg }

        (* Convert a 32-in-64 object index into an absolute address. *)
    |   ObjectIndexAddressToAbsolute of { source: preg, dest: preg }

        (* Convert an absolute address into an object index. *)
    |   AbsoluteToObjectIndex of { source: preg, dest: preg }

        (* Allocate a fixed sized piece of memory and puts the absolute address into dest.
           bytesRequired is the total number of bytes including the length word and any alignment
           necessary for 32-in-64. saveRegs is the list of registers that need to be saved if we
           need to do a garbage collection. *)
    |   AllocateMemoryFixed of { bytesRequired: Word64.word, dest: preg, saveRegs: preg list }

        (* Allocate a piece of memory.  The size argument is an untagged value containing
           the number of words i.e. the same value used for InitialiseMemory and to store
           in the length word. *)
    |   AllocateMemoryVariable of { size: preg, dest: preg, saveRegs: preg list }

        (* Initialise a piece of memory by writing "size" copies of the value
           in "init".  N.B. The size is an untagged value containing the
           number of words. *)
    |   InitialiseMem of { size: preg, addr: preg, init: preg }

        (* Mark the beginning of a loop.  This is really only to prevent the initialisation code being
           duplicated in ICodeOptimise. *)
    |   BeginLoop

        (* Set up the registers for a jump back to the start of a loop. *)
    |   JumpLoop of
            { regArgs: {src: fnarg, dst: preg} list,
              stackArgs: {src: fnarg, wordOffset: int, stackloc: stackLocn} list,
              checkInterrupt: preg list option }

        (* Store a register using a constant, signed, byte offset.  The offset
           is in the range of -256 to (+4095*unit size). *)
    |   StoreWithConstantOffset of { source: preg, base: preg, byteOffset: int, loadType: loadType }

        (* Ditto for FP regs. *)
    |   StoreFPWithConstantOffset of { source: preg, base: preg, byteOffset: int, floatSize: floatSize }

        (* Store a register using an index register. *)
    |   StoreWithIndexedOffset of { source: preg, base: preg, index: preg, loadType: loadType }

        (* and for FP regs. *)
    |   StoreFPWithIndexedOffset of { source: preg, base: preg, index: preg, floatSize: floatSize }

        (* Add/Subtract immediate.  The destination is optional in which case XZero is used.
           ccRef is optional.  If it is NONE the version of the instruction that does not generate
           a condition code is used. immed must be < 0wx1000. *)
    |   AddSubImmediate of { source: preg, dest: preg option, ccRef: ccRef option, immed: word,
                             isAdd: bool, length: opSize }

        (* Add/Subtract register.  As with AddSubImmediate, both the destination and cc are optional. *)
    |   AddSubRegister of { base: preg, shifted: preg, dest: preg option, ccRef: ccRef option,
                            isAdd: bool, length: opSize, shift: shiftType }

        (* Bitwise logical operations.  The immediate value must be a valid bit pattern.  ccRef can
           only be SOME if logOp is LogAnd. *)
    |   LogicalImmediate of { source: preg, dest: preg option, ccRef: ccRef option, immed: Word64.word,
                              logOp: logicalOp, length: opSize }

        (* Register logical operations.  ccRef can only be SOME if logOp is LogAnd.*)
    |   LogicalRegister of { base: preg, shifted: preg, dest: preg option, ccRef: ccRef option,
                             logOp: logicalOp, length: opSize, shift: shiftType }

        (* Shift a word by an amount specified in a register. *)
    |   ShiftRegister of { direction: shiftDirection, dest: preg, source: preg, shift: preg, opSize: opSize }

        (* The various forms of multiply all take three arguments and the general form is
           dest = M * N +/- A..   *)
    |   Multiplication of { kind: multKind, dest: preg, sourceA: preg option, sourceM: preg, sourceN: preg }

        (* Signed or unsigned division.  Sets the result to zero if the divisor is zero. *)
    |   Division of { isSigned: bool, dest: preg, dividend: preg, divisor: preg, opSize: opSize }

        (* Start of function.  Set the register arguments.  stackArgs is the list of
           stack arguments.  If the function has a real closure regArgs includes the
           closure register (X8).  The register arguments include the return register
           (X30). *)
    |   BeginFunction of { regArgs: (preg * xReg) list, stackArgs: stackLocn list }

        (* Call a function.  If the code address is a constant it is passed here.
           Otherwise the address is obtained by indirecting through X8 which has been loaded
           as one of the argument registers.  The result is stored in the destination register.
           The "containers" argument is used to ensure that any container whose address is passed
           as one of the other arguments continues to be referenced until the function is called
           since there's a possibility that it isn't actually used after the function. *)
    |   FunctionCall of
            { callKind: callKind, regArgs: (fnarg * xReg) list,
              stackArgs: fnarg list, dest: preg, saveRegs: preg list, containers: stackLocn list}

        (* Jump to a tail-recursive function.  This is similar to FunctionCall
           but complicated for stack arguments because the stack and the return
           address need to be overwritten.
           stackAdjust is the number of words to remove (positive) or add
           (negative) to the stack before the call.
           currStackSize contains the number of items currently on the stack. *)
    |   TailRecursiveCall of
            { callKind: callKind, regArgs: (fnarg * xReg) list,
              stackArgs: {src: fnarg, stack: int} list,
              stackAdjust: int, currStackSize: int }

        (* Return from the function.  resultReg is the preg that contains the result,
           returnReg is the preg that contains the return address. *)
    |   ReturnResultFromFunction of { resultReg: preg, returnReg: preg, numStackArgs: int }

        (* Raise an exception.  The packet is always loaded into X0. *)
    |   RaiseExceptionPacket of { packetReg: preg }

        (* Push a register to the stack.  This is used both for a normal push, copies=1, and
           also to reserve a container. *)
    |   PushToStack of { source: preg, copies: int, container: stackLocn }

        (* Load a register from the stack.  The container is the stack location identifier,
           the field is an offset in a container. *)
    |   LoadStack of { dest: preg, wordOffset: int, container: stackLocn, field: int }

        (* Store a value into the stack. *)
    |   StoreToStack of { source: preg, container: stackLocn, field: int, stackOffset: int }

        (* Set the register to the address of the container i.e. a specific offset on the stack. *)
    |   ContainerAddress of { dest: preg, container: stackLocn, stackOffset: int }

        (* Remove items from the stack.  Used to remove containers or
           registers pushed to the stack.. *)
    |   ResetStackPtr of { numWords: int }

        (* Tag a value by shifting and setting the tag bit. *)
    |   TagValue of { source: preg, dest: preg, isSigned: bool, opSize: opSize }

        (* Shift a value to remove the tag bit.  The cache is used if this is untagging a
           value that has previously been tagged. *)
    |   UntagValue of { source: preg, dest: preg, isSigned: bool, opSize: opSize }

        (* Box a largeword value.  Stores a value
           into a byte area.  This can be implemented using AllocateMemoryFixed
           but keeping it separate makes optimisation easier.
           The result is always an address and needs to be converted to an
           object index on 32-in-64. *)
    |   BoxLarge of { source: preg, dest: preg, saveRegs: preg list }

        (* Load a value from a box.  This can be implemented using a load but
           is kept separate to simplify optimisation.  The source is always
           an absolute address. *)
    |   UnboxLarge of { source: preg, dest: preg }

        (* Convert a floating point value into a value suitable for storing
           in the heap.  This normally involves boxing except that 32-bit
           floats can be tagged in native 64-bits. *)
    |   BoxTagFloat of { floatSize: floatSize, source: preg, dest: preg, saveRegs: preg list }

        (* The reverse of BoxTagFloat. *)
    |   UnboxTagFloat of { floatSize: floatSize, source: preg, dest: preg }

        (* Load a value with acquire semantics.  This means that any other
           load in this thread after this sees the value of the shared
           memory at this point and not earlier.  This is used for
           references and arrays to ensure that if another thread has
           built a data structure on the heap and then assigns the
           address to a shared ref this thread will see the updated heap
           and not any locally cached previous version. *)
    |   LoadAcquire of { base: preg, dest: preg, loadType: loadType }

        (* Store a value with release semantics.  This ensures that any
           other write completes before this operation and works with
           LoadAcquire. *)
    |   StoreRelease of { base: preg, source: preg, loadType: loadType }

        (* This is a generalised constant shift which includes selection of a
           range of bits. *)
    |   BitFieldShift of { source: preg, dest: preg, isSigned: bool, length: opSize, immr: word, imms: word }

        (*  Copy a range of bits and insert it into another register.  This is the
            only case where a register functions both as a source and a destination. *)
    |   BitFieldInsert of { source: preg, destAsSource: preg, dest: preg,
                            length: opSize, immr: word, imms: word }

        (* Indexed case. *)
    |   IndexedCaseOperation of { testReg: preg }

        (* Exception handling.  - Set up an exception handler. *)
    |   PushExceptionHandler

        (* End of a handled section.  Restore the previous handler. *)
    |   PopExceptionHandler

        (* Marks the start of a handler.  This sets the stack pointer and
           restores the old handler.  Sets the exception packet register. *) 
    |   BeginHandler of { packetReg: preg }

        (* Compare two vectors of bytes and set the condition code on the result.
           The registers are modified by the instruction. *)
    |   CompareByteVectors of
            { vec1Addr: preg, vec2Addr: preg, length: preg, ccRef: ccRef }

        (* Move a block of bytes (isByteMove true) or words (isByteMove false).  The length is the
           number of items (bytes or words) to move. The registers are modified by
           the instruction. *)
    |   BlockMove of { srcAddr: preg, destAddr: preg, length: preg, isByteMove: bool }

        (* Add or subtract to the system stack pointer and optionally return the new value.
           This is used to allocate and deallocate C space. *)
    |   AddSubXSP of { source: preg, dest: pregOrZero, isAdd: bool  }

        (* Ensures the value will actually be referenced although it doesn't generate any code. *)
    |   TouchValue of { source: preg }

        (* Load a value at the address and get exclusive access.  Always loads a
           64-bit value. *)
    |   LoadAcquireExclusive of { base: preg, dest: preg }

        (* Store a value into an address releasing the lock.  Sets the result to
           either 0 or 1 if it succeeds or fails. *)
    |   StoreReleaseExclusive of { base: preg, source: pregOrZero, result: preg }

        (* Insert a memory barrier. dmb ish. *)
    |   MemoryBarrier

        (* Convert an integer to a floating point value. *)
    |   ConvertIntToFloat of { source: preg, dest: preg, srcSize: opSize, destSize: floatSize }

        (* Convert a floating point value to an integer using the specified rounding mode.
           We could get an overflow here but fortunately the ARM generates a value
           that will cause an overflow when we tag it, provided we tag it explicitly. *)
    |   ConvertFloatToInt of { source: preg, dest: preg, srcSize: floatSize, destSize: opSize, rounding: IEEEReal.rounding_mode }

        (* Unary floating point.  This includes conversions between float and double. *)
    |   UnaryFloatingPt of { source: preg, dest: preg, fpOp: fpUnary }

        (* Binary floating point: addition, subtraction, multiplication and division. *)
    |   BinaryFloatingPoint of { arg1: preg, arg2: preg, dest: preg, fpOp: fpBinary, opSize: floatSize }

        (* Floating point comparison. *)
    |   CompareFloatingPoint of { arg1: preg, arg2: preg, ccRef: ccRef, opSize: floatSize }

        (* Destinations at the end of a basic block. *)
    and controlFlow =
        (* Unconditional branch to a label - should be a merge point. *)
        Unconditional of int
        (* Conditional branch. Jumps to trueJump if the condional is true, falseJump if false. *)
    |   Conditional of { ccRef: ccRef, condition: condition, trueJump: int, falseJump: int }
        (* Exit - the last instruction of the block is a return, raise or tailcall. *)
    |   ExitCode
        (* Indexed case - this branches to one of a number of labels *)
    |   IndexedBr of int list
        (* Set up a handler.  This doesn't cause an immediate branch but the state at the
           start of the handler is the state at this point. *)
    |   SetHandler of { handler: int, continue: int }
        (* Unconditional branch to a handler.  If an exception is raised explicitly
           within the scope of a handler. *)
    |   UnconditionalHandle of int
        (* Conditional branch to a handler.  Occurs if there is a call to a
           function within the scope of a handler.  It may jump to the handler. *)
    |   ConditionalHandle of { handler: int, continue: int }

    and basicBlock = BasicBlock of { block: arm64ICode list, flow: controlFlow }

    (* Return the list of blocks that are the immediate successor of this. *)
    fun successorBlocks(Unconditional l) = [l]
    |   successorBlocks(Conditional{trueJump, falseJump, ...}) = [trueJump, falseJump]
    |   successorBlocks ExitCode = []
    |   successorBlocks(IndexedBr cases) = cases
    |   successorBlocks(SetHandler{handler, continue, ...}) = [handler, continue]
        (* We only need "handler" in SetHandler because we may have a handler that is never actually jumped to. *)
    |   successorBlocks(UnconditionalHandle handler) = [handler]
    |   successorBlocks(ConditionalHandle{handler, continue, ...}) = [handler, continue]

    local
        fun printReg(PReg i, stream) = stream("R" ^ Int.toString i)
        and printCC(CcRef ccRef, stream) = stream ("CC" ^ Int.toString ccRef)
        
        fun printOptReg(ZeroReg, stream) = stream "Zero"
        |   printOptReg(SomeReg reg, stream) = printReg(reg, stream)
         
        fun printStackLoc(StackLoc{size, rno}, stream) =
            (stream "S"; stream(Int.toString rno); stream "("; stream(Int.toString size); stream ")")
        
        fun regRepr(XReg w) = "X" ^ Int.toString(Word8.toInt w)
        |   regRepr XZero = "XZ"
        |   regRepr XSP = "SP"

        fun arithRepr OpSize64 = "64"
        |   arithRepr OpSize32 = "32"

        fun printLoadType(Load64, stream) = stream "64"
        |   printLoadType(Load32, stream) = stream "32"
        |   printLoadType(Load16, stream) = stream "16"
        |   printLoadType(Load8, stream) = stream "8"

        fun printSaves([], _) = ()
        |   printSaves([areg], stream) = printReg(areg, stream)
        |   printSaves(areg::more, stream) =
                (printReg(areg, stream); stream ","; printSaves(more, stream))

        fun printArg(ArgInReg reg, stream) = printReg(reg, stream)
        |   printArg(ArgOnStack{wordOffset, container, field, ...}, stream) =
            (
                printStackLoc(container, stream); stream " + ";
                stream(Int.toString field);
                stream " (";  stream(Int.toString wordOffset); stream ")"
            )

        fun printShift(ShiftLSL w, stream) = stream(" LSL " ^ Word8.toString w)
        |   printShift(ShiftLSR w, stream) = stream(" LSR " ^ Word8.toString w)
        |   printShift(ShiftASR w, stream) = stream(" ASR " ^ Word8.toString w)
        |   printShift(ShiftNone, _) = ()

        fun printFloatSize(Float32, stream) = stream "F"
        |   printFloatSize(Double64, stream) = stream "D"

        fun printICode(MoveRegister{ source, dest }, stream) =
            (
                stream "\tMove\t";
                printReg(source,  stream);
                stream " => ";
                printReg(dest, stream)
            )

        |   printICode(LoadNonAddressConstant{ source, dest }, stream) =
            (
                stream "\tLoadNonAddress\t"; stream(Word64.toString source);
                stream " => ";
                printReg(dest, stream)
            )

        |   printICode(LoadAddressConstant{ source, dest }, stream) =
            (
                stream "\tLoadAddress\t"; stream(Address.stringOfWord source);
                stream " => ";
                printReg(dest, stream)
            )

        |   printICode(LoadWithConstantOffset{ base, dest, byteOffset, loadType }, stream) =
            (
                stream "\tLoadConstOffset"; printLoadType(loadType, stream); stream "\t[";
                printReg(base, stream); stream "]+";
                stream(Int.toString byteOffset);
                stream " => ";
                printReg(dest, stream)
            )

        |   printICode(LoadFPWithConstantOffset{ base, dest, byteOffset, floatSize }, stream) =
            (
                stream "\tLoadConstOffset"; printFloatSize(floatSize, stream); stream "\t[";
                printReg(base, stream); stream "]+";
                stream(Int.toString byteOffset);
                stream " => ";
                printReg(dest, stream)
            )

        |   printICode(LoadWithIndexedOffset{ base, dest, index, loadType }, stream) =
            (
                stream "\tLoadIndexed"; printLoadType(loadType, stream); stream "\t[";
                printReg(base, stream); stream "+"; printReg(index, stream);
                stream "] => "; printReg(dest, stream)
            )

        |   printICode(LoadFPWithIndexedOffset{ base, dest, index, floatSize }, stream) =
            (
                stream "\tLoadIndexed"; printFloatSize(floatSize, stream); stream "\t[";
                printReg(base, stream); stream "+"; printReg(index, stream);
                stream "] => "; printReg(dest, stream)
            )

        |   printICode(GetThreadId { dest}, stream) =
                ( stream "\tGetThreadId\t"; stream " => "; printReg(dest, stream) )

        |   printICode(ObjectIndexAddressToAbsolute{ source, dest }, stream) =
            (
                stream "\tObjectAddrToAbs\t";
                printReg(source, stream); stream " => "; printReg(dest, stream)
            )

        |   printICode(AbsoluteToObjectIndex{ source, dest }, stream) =
            (
                stream "\tAbsToObjectAddr\t";
                printReg(source, stream); stream " => "; printReg(dest, stream)
            )

        |   printICode(AllocateMemoryFixed{bytesRequired, dest, saveRegs}, stream) =
            (
                stream "\tAllocateMemory\t";
                stream(Word64.fmt StringCvt.DEC bytesRequired); stream " => ";
                printReg(dest, stream); stream " save="; printSaves(saveRegs, stream)
            )

        |   printICode(AllocateMemoryVariable{size, dest, saveRegs}, stream) =
            (
                stream "\tAllocateMemory\t";
                stream "s="; printReg(size, stream);
                stream " => "; printReg(dest, stream);
                stream " save="; printSaves(saveRegs, stream)
            )

        |   printICode(InitialiseMem{size, addr, init}, stream) =
            (
                stream "\tInitialiseMem\t";
                stream "s="; printReg(size, stream);
                stream ",i="; printReg(init, stream);
                stream ",a="; printReg(addr, stream)
            )

        |   printICode(BeginLoop, stream) = stream "\tBeginLoop"

        |   printICode(JumpLoop{regArgs, stackArgs, checkInterrupt, ... }, stream) =
            (
                stream "\tJumpLoop\t";
                List.app(fn {src, dst} => (printReg(dst, stream); stream "="; printArg(src, stream); stream " ")) regArgs;
                List.app(
                    fn {src, wordOffset, stackloc} =>
                        (printStackLoc(stackloc, stream); stream("(sp" ^ Int.toString wordOffset); stream ")="; printArg(src, stream); stream " ")
                    ) stackArgs;
                case checkInterrupt of
                    NONE => ()
                |   SOME saveRegs => (stream " Check:save="; printSaves(saveRegs, stream))
            )

        |   printICode(StoreWithConstantOffset{ base, source, byteOffset, loadType }, stream) =
            (
                stream "\tStoreConstOffset"; printLoadType(loadType, stream); stream "\t";
                printReg(source, stream); stream " => [";
                printReg(base, stream); stream "+";
                stream(Int.toString byteOffset); stream "]"
            )

        |   printICode(StoreFPWithConstantOffset{ base, source, byteOffset, floatSize }, stream) =
            (
                stream "\tStoreConstOffset"; printFloatSize(floatSize, stream); stream "\t";
                printReg(source, stream); stream " => [";
                printReg(base, stream); stream "+";
                stream(Int.toString byteOffset); stream "]"
            )

        |   printICode(StoreWithIndexedOffset{ base, source, index, loadType }, stream) =
            (
                stream "\tStoreIndexed"; printLoadType(loadType, stream); stream "\t";
                printReg(source, stream); stream " => [";
                printReg(base, stream); stream "+"; printReg(index, stream); stream "]"
            )

        |   printICode(StoreFPWithIndexedOffset{ base, source, index, floatSize }, stream) =
            (
                stream "\tStoreIndexed"; printFloatSize(floatSize, stream); stream "\t";
                printReg(source, stream); stream " => [";
                printReg(base, stream); stream "+"; printReg(index, stream); stream "]"
            )

        |   printICode(AddSubImmediate{ source, dest, ccRef, immed, isAdd, length }, stream) =
            (
                stream (if isAdd then "\tAddImmediate" else "\tSubImmediate"); stream(arithRepr length);
                stream "\t"; printReg(source, stream); stream ",0x"; stream(Word.toString immed);
                stream " => "; case dest of NONE => stream "_" | SOME reg => printReg(reg, stream);
                case ccRef of NONE => () | SOME cc => (stream ", "; printCC(cc, stream))
            )

        |   printICode(AddSubRegister{ base, shifted, dest, ccRef, isAdd, length, shift }, stream) =
            (
                stream (if isAdd then "\tAddRegister" else "\tSubRegister"); stream(arithRepr length);
                stream "\t"; printReg(base, stream); stream ", ";
                printReg(shifted, stream); printShift(shift, stream);
                stream " => "; case dest of NONE => stream "_" | SOME reg => printReg(reg, stream);
                case ccRef of NONE => () | SOME cc => (stream ", "; printCC(cc, stream))
            )

        |   printICode(LogicalImmediate{ source, dest, ccRef, immed, logOp, length }, stream) =
            (
                stream (case logOp of LogAnd => "\tAndImmediate" | LogOr => "\tOrImmediate" | LogXor => "\tXorImmediate");
                stream(arithRepr length);
                stream "\t"; printReg(source, stream); stream ",0x"; stream(Word64.toString immed);
                stream " => "; case dest of NONE => stream "_" | SOME reg => printReg(reg, stream);
                case ccRef of NONE => () | SOME cc => (stream ", "; printCC(cc, stream))
            )

        |   printICode(LogicalRegister{ base, shifted, dest, ccRef, logOp, length, shift }, stream) =
            (
                stream (case logOp of LogAnd => "\tAndRegister"  | LogOr => "\tOrRegister" | LogXor => "\tXorRegister");
                stream(arithRepr length);
                stream "\t"; printReg(base, stream); stream ", ";
                printReg(shifted, stream); printShift(shift, stream);
                stream " => "; case dest of NONE => stream "_" | SOME reg => printReg(reg, stream);
                case ccRef of NONE => () | SOME cc => (stream ", "; printCC(cc, stream))
            )

        |   printICode(ShiftRegister{ direction, dest, source, shift, opSize }, stream) =
            (
                stream (
                    case direction of
                        ShiftLeft => "\tShiftLeft"
                    |   ShiftRightLogical => "\tShiftRightLog"
                    |   ShiftRightArithmetic => "\tShiftRightArith");
                stream(arithRepr opSize);
                stream "\t"; printReg(source, stream); stream " by ";
                printReg(shift, stream);
                stream " => "; printReg(dest, stream)
            )

        |   printICode(Multiplication{ kind, dest, sourceA, sourceM, sourceN }, stream) =
            (
                stream (
                    case kind of
                        MultAdd32 => "\tMultAdd32\t"
                    |   MultSub32 => "\tMultSub32\t"
                    |   MultAdd64 => "\tMultAdd64\t"
                    |   MultSub64 => "\tMultSub64\t"
                    |   SignedMultAddLong => "\tSignedMultAddLong\t"
                    |   SignedMultHigh => "\tSignedMultHigh\t");
                printReg(sourceM, stream); stream " * ";
                printReg(sourceN, stream);
                case sourceA of SOME srcA => (stream " +/- "; printReg(srcA, stream)) | NONE => ();
                stream " => "; printReg(dest, stream)
            )

        |   printICode(Division{ isSigned, dest, dividend, divisor, opSize }, stream) =
            (
                stream (if isSigned then "\tSignedDivide" else "\tUnsignedDivide");
                stream(arithRepr opSize);
                stream "\t"; printReg(dividend, stream); stream " by ";
                printReg(divisor, stream);
                stream " => "; printReg(dest, stream)
            )

        |   printICode(BeginFunction{ regArgs, stackArgs }, stream) =
            (
                stream "\tBeginFunction\t";
                List.app(fn (arg, r) => (stream(regRepr r); stream "="; printReg(arg, stream); stream " ")) regArgs;
                List.app(fn s => printStackLoc(s, stream)) stackArgs
            )

        |   printICode(FunctionCall{callKind, regArgs, stackArgs, dest, saveRegs, containers}, stream) =
            (
                stream "\tFunctionCall\t";
                case callKind of
                    Recursive => stream "recursive "
                |   ConstantCode m => (stream(stringOfWord m); stream " ")
                |   FullCall => ();
                List.app(fn (arg, r) => (stream(regRepr r); stream "="; printArg(arg, stream); stream " ")) regArgs;
                List.app(fn arg => (stream "p="; printArg(arg, stream); stream " ")) stackArgs;
                stream "=> "; printReg(dest, stream);
                stream " save="; printSaves(saveRegs, stream);
                if null containers then ()
                else (stream " containers="; List.app (fn c => (printStackLoc(c, stream); stream " ")) containers)
            )

        |   printICode(TailRecursiveCall{callKind, regArgs, stackArgs, stackAdjust, currStackSize, ...}, stream) =
            (
                stream "\tTailCall\t";
                case callKind of
                    Recursive => stream "recursive "
                |   ConstantCode m => (stream(stringOfWord m); stream " ")
                |   FullCall => ();
                List.app(fn (arg, r) => (stream(regRepr r); stream "="; printArg(arg, stream); stream " ")) regArgs;
                List.app(fn {src, stack} => (stream (Int.toString stack); stream "<="; printArg(src, stream); stream " ")) stackArgs;
                stream "adjust="; stream(Int.toString stackAdjust);
                stream " stackSize="; stream(Int.toString currStackSize)
            )

        |   printICode(ReturnResultFromFunction{ resultReg, returnReg, numStackArgs }, stream) =
            (
                stream "\tReturnFromFunction\t";
                printReg(returnReg, stream); stream " with "; printReg(resultReg, stream);
                stream("," ^ Int.toString numStackArgs)
            )

        |   printICode(RaiseExceptionPacket{ packetReg }, stream) =
            ( stream "\tRaiseException\t"; printReg(packetReg, stream) )

        |   printICode(PushToStack{ source, copies, container }, stream) =
            (
                stream "\tPushToStack\t"; printReg(source, stream);
                if copies > 1 then (stream " * "; stream(Int.toString copies)) else ();
                stream " => "; printStackLoc(container, stream)
            )

        |   printICode(LoadStack{ dest, wordOffset, container, field }, stream) =
            (
                stream "\tLoadStack\t";
                printStackLoc(container, stream); stream " + ";
                stream(Int.toString field);
                stream " (";  stream(Int.toString wordOffset); stream ")";
                stream " => "; printReg(dest, stream)
            )

        |   printICode(StoreToStack{ source, container, field, stackOffset }, stream) =
            (
                stream "\tStoreToStack\t"; printReg(source, stream);
                stream " => "; printStackLoc(container, stream); stream "+";
                stream (Int.toString field); stream "(";
                stream(Int.toString stackOffset); stream ")"
            )

        |   printICode(ContainerAddress{ dest, container, stackOffset }, stream) =
            (
                stream "\tContainerAddress\t";
                stream "@"; printStackLoc(container, stream);
                stream " (";  stream(Int.toString stackOffset); stream ") => ";
                printReg(dest, stream)
            )

        |   printICode(ResetStackPtr{ numWords }, stream) =
            ( stream "\tResetStackPtr\t"; stream(Int.toString numWords) )

        |   printICode(TagValue{ source, dest, isSigned, opSize }, stream) =
            (
                stream "\tTag"; stream(if isSigned then "Signed" else "Unsigned");
                stream(arithRepr opSize); stream "\t";
                printReg(source, stream); stream " => "; printReg(dest, stream)
            )

        |   printICode(UntagValue{ source, dest, isSigned, opSize }, stream) =
            (
                stream "\tUntag"; stream(if isSigned then "Signed" else "Unsigned");
                stream(arithRepr opSize); stream "\t";
                printReg(source, stream); stream " => "; printReg(dest, stream)
            )

        |   printICode(BoxLarge{source, dest, saveRegs}, stream) =
            (
                stream "\tBoxLarge\t";
                printReg(source, stream);
                stream " => ";
                printReg(dest, stream);
                stream " save="; printSaves(saveRegs, stream)
            )

        |   printICode(UnboxLarge{source, dest}, stream) =
            (
                stream "\tUnboxLarge\t";
                printReg(source, stream);
                stream " => ";
                printReg(dest, stream)
            )

        |   printICode(BoxTagFloat{floatSize, source, dest, saveRegs}, stream) =
            (
                stream "\tBoxTagFloat"; printFloatSize(floatSize, stream); stream "\t";
                printReg(source, stream);
                stream " => ";
                printReg(dest, stream);
                stream " save="; printSaves(saveRegs, stream)
            )

        |   printICode(UnboxTagFloat{floatSize, source, dest}, stream) =
            (
                stream "\tUnboxTagFloat"; printFloatSize(floatSize, stream); stream "\t";
                printReg(source, stream);
                stream " => ";
                printReg(dest, stream)
            )

        |   printICode(LoadAcquire{ base, dest, loadType }, stream) =
            (
                stream "\tLoadAcquire"; printLoadType(loadType, stream); stream "\t[";
                printReg(base, stream); stream "] => "; printReg(dest, stream)
            )

        |   printICode(StoreRelease{ base, source, loadType }, stream) =
            (
                stream "\tStoreRelease"; printLoadType(loadType, stream); stream "\t";
                printReg(source, stream); stream " => [";
                printReg(base, stream); stream "]"
            )

        |   printICode(BitFieldShift{ source, dest, isSigned, length, immr, imms }, stream) =
            (
                stream "\tBitShift"; stream(if isSigned then "Signed" else "Unsigned"); stream(arithRepr length); stream "\t";
                printReg(source, stream);
                stream " => "; printReg(dest, stream);
                stream " immr="; stream(Word.fmt StringCvt.DEC immr);
                stream " imms="; stream(Word.fmt StringCvt.DEC imms)
            )

        |   printICode(BitFieldInsert{ source, dest, destAsSource, length, immr, imms }, stream) =
            (
                stream "\tBitInsert";  stream(arithRepr length); stream "\t";
                printReg(source, stream); stream " with "; printReg(destAsSource, stream);
                stream " => "; printReg(dest, stream);
                stream " immr="; stream(Word.fmt StringCvt.DEC immr);
                stream " imms="; stream(Word.fmt StringCvt.DEC imms)
            )

        |   printICode(IndexedCaseOperation{testReg}, stream) =
                ( stream "\tIndexedCase\t"; printReg(testReg, stream) )

        |   printICode(PushExceptionHandler, stream) = stream "\tPushExcHandler"

        |   printICode(PopExceptionHandler, stream) = stream "\tPopExcHandler"

        |   printICode(BeginHandler{packetReg}, stream) =
            (
                stream "\tBeginHandler\t";
                printReg(packetReg, stream)
            )

        |   printICode(CompareByteVectors{vec1Addr, vec2Addr, length, ccRef, ...}, stream) =
            (
                stream "\tCompareByteVectors\t";
                printReg(vec1Addr, stream); stream ",";
                printReg(vec2Addr, stream); stream ",";
                printReg(length, stream);
                stream " => "; printCC(ccRef, stream)
            )

        |   printICode(BlockMove{srcAddr, destAddr, length, isByteMove}, stream) =
            (
                stream(if isByteMove then "\tBlockByteMove\t" else "\tBlockWordMove\t");
                stream "src="; printReg(srcAddr, stream);
                stream ",dest="; printReg(destAddr, stream);
                stream ",len="; printReg(length, stream)
            )

        |   printICode(AddSubXSP{ source, dest, isAdd }, stream) =
            (
                stream(if isAdd then "\tAdd\t" else "\tSubtract\t");
                printReg(source, stream); stream " XSP => ";
                printOptReg(dest, stream)
            )

        |   printICode(TouchValue{ source }, stream) =
            ( stream "\tTouchValue\t"; printReg(source, stream) )

        |   printICode(LoadAcquireExclusive{ base, dest }, stream) =
            (
                stream "\tLoadExclusive\t["; printReg(base, stream);
                stream "] => "; printReg(dest, stream)
            )

        |   printICode(StoreReleaseExclusive{ base, source, result }, stream) =
            (
                stream "\tStoreExclusive\t";
                printOptReg(source, stream); stream " => [";
                printReg(base, stream); stream "] result => ";
                printReg(result, stream)
            )

        |   printICode(MemoryBarrier, stream) = stream "\tMemoryBarrier"

        |   printICode(ConvertIntToFloat{ source, dest, srcSize, destSize}, stream) =
            (
                stream "\tConvert"; stream(arithRepr srcSize); stream "To";
                printFloatSize(destSize, stream); stream "\t";
                printReg(source, stream); stream " => "; printReg(dest, stream)
            )

        |   printICode(ConvertFloatToInt{ source, dest, srcSize, destSize, rounding}, stream) =
            let
                open IEEEReal
            in
                stream "\tConvert"; printFloatSize(srcSize, stream); stream "To";
                stream(arithRepr destSize); stream "\t";
                printReg(source, stream); stream " => "; printReg(dest, stream);
                stream(
                    case rounding of
                        TO_NEAREST => " rounding"
                    |   TO_NEGINF => " rounding down"
                    |   TO_POSINF => " rounding up"
                    |   TO_ZERO => " truncating"
                )
            end

        |   printICode(UnaryFloatingPt{ source, dest, fpOp}, stream) =
            (
                stream(
                    case fpOp of
                        NegFloat => "\tNegFloat\t"
                    |   NegDouble => "\tNegDouble\t"
                    |   AbsFloat => "\tAbsFloat\t"
                    |   AbsDouble => "\tAbsDouble\t"
                    |   ConvFloatToDble => "\tFloatToDble\t"
                    |   ConvDbleToFloat => "\t\t"
                );
                printReg(source, stream); stream " => "; printReg(dest, stream)
            )

        |   printICode(BinaryFloatingPoint{ arg1, arg2, dest, fpOp, opSize}, stream) =
            (
                stream(
                    case fpOp of
                        MultiplyFP => "\tMultiply"
                    |   DivideFP => "\tDivide"
                    |   AddFP => "\tAdd"
                    |   SubtractFP => "\tSubtract"
                );
                printFloatSize(opSize, stream); stream "\t";
                printReg(arg1, stream); stream ", "; printReg(arg2, stream);
                stream " => "; printReg(dest, stream)
            )

        |   printICode(CompareFloatingPoint{ arg1, arg2, opSize, ccRef}, stream) =
            (
                stream "\tCompare"; printFloatSize(opSize, stream); stream "\t";
                printReg(arg1, stream); stream ", "; printReg(arg2, stream);
                stream ", "; printCC(ccRef, stream)
            )

        and printCondition(cond,  stream)  = stream(condToString cond)

        (* Print a basic block. *)
        fun printBlock stream (blockNo, BasicBlock{block, flow, ...}) =
        (
            (* Put a label on all but the first. *)
            if blockNo <> 0 then stream("L" ^ Int.toString blockNo ^ ":") else ();
            List.app (fn icode => (printICode(icode, stream); stream "\n")) block;
            case flow of
                Unconditional l => stream("\tJump\tL" ^ Int.toString l ^ "\n")
            |   Conditional {condition, trueJump, falseJump, ccRef, ...} =>
                (
                    stream "\tJump"; printCondition(condition, stream);
                    stream "\t"; printCC(ccRef, stream);
                    stream " L"; stream (Int.toString trueJump);
                    stream " else L"; stream (Int.toString falseJump); stream "\n"
                )
            |   ExitCode => ()
            |   IndexedBr _ => ()
            |   SetHandler{handler, continue} =>
                    stream(concat["\tSetHandler\tH", Int.toString handler, "\n",
                                  "\tJump\tL", Int.toString continue, "\n"])
            |   UnconditionalHandle handler => stream("\tJump\tH" ^ Int.toString handler ^ "\n")
            |   ConditionalHandle{handler, continue} =>
                    stream(concat["\tJump\tL", Int.toString continue, " or H", Int.toString handler, "\n"])
        )
    in
        fun printICodeAbstract(blockVec, stream) =
            Vector.appi(printBlock stream) blockVec
    end

    (* Only certain bit patterns are allowed in a logical immediate instruction
       but the encoding is complex so it's easiest to inherit the test from
       the assembler layer. *)
    local
        fun optow OpSize32 = WordSize32 | optow OpSize64 = WordSize64
    in
        fun isEncodableBitPattern(v, w) = Arm64Code.isEncodableBitPattern(v, optow w)
    end

    (* This generates a  BitField instruction with the appropriate values for immr and imms. *)
    fun shiftConstant{ direction, dest, source, shift, opSize } =
    let
        val (isSigned, immr, imms) =
            case (direction, opSize) of
                (ShiftLeft, OpSize64) => (false, Word.~ shift mod 0w64, 0w64-0w1-shift)
            |   (ShiftLeft, OpSize32) => (false, Word.~ shift mod 0w32, 0w32-0w1-shift)
            |   (ShiftRightLogical, OpSize64) => (false, shift, 0wx3f)
            |   (ShiftRightLogical, OpSize32) => (false, shift, 0wx1f)
            |   (ShiftRightArithmetic, OpSize64) => (true, shift, 0wx3f)
            |   (ShiftRightArithmetic, OpSize32) => (true, shift, 0wx1f)
    in
        BitFieldShift{ source=source, dest=dest, isSigned=isSigned, length=opSize, immr=immr, imms=imms }
    end

    structure Sharing =
    struct
        type xReg           = xReg
        and  vReg           = vReg
        and  reg            = reg
        and  condition      = condition
        and  shiftType      = shiftType
        and  arm64ICode     = arm64ICode
        and  preg           = preg
        and  pregOrZero     = pregOrZero
        and  controlFlow    = controlFlow
        and  basicBlock     = basicBlock
        and  stackLocn      = stackLocn
        and  regProperty    = regProperty
        and  ccRef          = ccRef
        and  fnarg          = fnarg
        and  closureRef     = closureRef
        and  loadType       = loadType
        and  opSize         = opSize
        and  logicalOp      = logicalOp
        and  callKind       = callKind
        and  floatSize      = floatSize
        and  shiftDirection = shiftDirection
        and  multKind       = multKind
        and  fpUnary        = fpUnary
        and  fpBinary       = fpBinary
   end
end;

