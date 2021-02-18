(*
    Copyright (c) 2021 David C. J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    Licence version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public Licence for more details.
    
    You should have received a copy of the GNU Lesser General Public
    Licence along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

signature Arm64Assembly =
sig
    type closureRef
    type instr
    type machineWord = Address.machineWord
    type labels
    type condition

    eqtype xReg
    val X0:  xReg   and X1:  xReg   and X2:  xReg   and X3: xReg
    and X4:  xReg   and X5:  xReg   and X6:  xReg   and X7: xReg
    and X8:  xReg   and X9:  xReg   and X10: xReg   and X11: xReg
    and X12: xReg   and X13: xReg   and X14: xReg   and X15: xReg
    and X16: xReg   and X17: xReg   and X18: xReg   and X19: xReg
    and X20: xReg   and X21: xReg   and X22: xReg   and X23: xReg
    and X24: xReg   and X25: xReg   and X26: xReg   and X27: xReg
    and X28: xReg   and X29: xReg   and X30: xReg
    
    (* XZero and XSP are both encoded as 31 but the interpretation
       depends on the instruction *)
    val XZero: xReg and XSP: xReg
    
    val X_MLHeapLimit: xReg (* ML Heap limit pointer *)
    and X_MLAssemblyInt: xReg (* ML assembly interface pointer. *)
    and X_MLHeapAllocPtr: xReg (* ML Heap allocation pointer. *)
    and X_MLStackPtr: xReg (* ML Stack pointer. *)
    and X_LinkReg: xReg (* Link reg - return address *)

    (* Condition for conditional branches etc. *)
    val condEqual: condition
    and condNotEqual: condition
    and condCarrySet: condition
    and condCarryClear: condition
    and condNegative: condition
    and condPositive: condition
    and condOverflow: condition
    and condNoOverflow: condition
    and condUnsignedHigher: condition
    and condUnsignedLowOrEq: condition
    and condSignedGreaterEq: condition
    and condSignedLess: condition
    and condSignedGreater: condition
    and condSignedLessEq: condition
    and condAlways: condition
    and condAlwaysNV: condition

    datatype shiftType =
        ShiftLSL of word
    |   ShiftLSR of word
    |   ShiftASR of word
    |   ShiftNone

    datatype wordSize = WordSize32 | WordSize64

    datatype 'a extend =
        ExtUXTB of 'a (* Unsigned extend byte *)
    |   ExtUXTH of 'a (* Unsigned extend byte *)
    |   ExtUXTW of 'a (* Unsigned extend byte *)
    |   ExtUXTX of 'a (* Left shift *)
    |   ExtSXTB of 'a (* Sign extend byte *)
    |   ExtSXTH of 'a (* Sign extend halfword *)
    |   ExtSXTW of 'a (* Sign extend word *)
    |   ExtSXTX of 'a (* Left shift *)

    (* Load/store instructions have only a single bit for the shift.  For byte
       operations this is one bit shift; for others it scales by the size of
       the operand if set. *)
    datatype scale =
        ScaleOrShift
    |   NoScale

    (* Jump to the address in the register and put the address of the
       next instruction into X30. *)
    val genBranchAndLinkReg: xReg -> instr
    (* Jump to the address in the register. *)
    and genBranchRegister: xReg -> instr
    (* Jump to the address in the register and hint this is a return. *)
    and genReturnRegister: xReg -> instr

    (* Move an address constant to a register. *)
    val loadAddressConstant: xReg * machineWord -> instr
    (* Move a constant into a register that is not an address.
       The argument is the actual bit pattern to be copied.
       For tagged integers that means that the value must have
       been shifted and the tag bit set. *)
    and loadNonAddressConstant: xReg * Word64.word -> instr
    
    (* Move a value from one register into another. *)
    val genMoveRegToReg: {sReg: xReg, dReg: xReg} -> instr

    (* Add/subtract an optionally shifted 12-bit immediate (i.e. constant) to/from a register.
       The constant is zero-extended. *)
    val addImmediate: {regN: xReg, regD: xReg, immed: word, shifted: bool} -> instr
    and addSImmediate: {regN: xReg, regD: xReg, immed: word, shifted: bool} -> instr
    and subImmediate: {regN: xReg, regD: xReg, immed: word, shifted: bool} -> instr
    and subSImmediate: {regN: xReg, regD: xReg, immed: word, shifted: bool} -> instr

    (* Add/subtract a shifted register, optionally setting the flags. *)
    val addShiftedReg: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and addSShiftedReg: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and subShiftedReg: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and subSShiftedReg: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    
    (* Add/subtract an extended register, optionally setting the flags. *)
    val addExtendedReg: {regM: xReg, regN: xReg, regD: xReg, extend: word extend} -> instr
    and addSExtendedReg: {regM: xReg, regN: xReg, regD: xReg, extend: word extend} -> instr
    and subExtendedReg: {regM: xReg, regN: xReg, regD: xReg, extend: word extend} -> instr
    and subSExtendedReg: {regM: xReg, regN: xReg, regD: xReg, extend: word extend} -> instr

    (* Logical operations on a shifted register, optionally setting the flags. *)
    val andShiftedReg: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and orrShiftedReg: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and eorShiftedReg: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr
    and andsShiftedReg: {regM: xReg, regN: xReg, regD: xReg, shift: shiftType} -> instr

    (* And a register with a bit pattern, discarding the results but setting the
       condition codes.  The bit pattern must be encodable. *)
    val testBitPattern: xReg * Word64.word -> instr

    (* Check whether a constant can be encoded. *)
    val isEncodableBitPattern: Word64.word * wordSize -> bool

    (* Load/Store an aligned word using a 12-bit offset.  The offset is in units
       of the size of the operand. *)
    val loadRegScaled: {regT: xReg, regN: xReg, unitOffset: int} -> instr
    and storeRegScaled: {regT: xReg, regN: xReg, unitOffset: int} -> instr
    and loadRegScaledByte: {regT: xReg, regN: xReg, unitOffset: int} -> instr
    and storeRegScaledByte: {regT: xReg, regN: xReg, unitOffset: int} -> instr

    (* Load/Store a value using a signed byte offset. *)
    val loadRegUnscaled: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and storeRegUnscaled: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and loadRegUnscaledByte: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and storeRegUnscaledByte: {regT: xReg, regN: xReg, byteOffset: int} -> instr

    (* Load/store with a register offset i.e. an index register. *)
    val loadRegIndexed: {regN: xReg, regM: xReg, regT: xReg, option: scale extend} -> instr
    and storeRegIndexed: {regN: xReg, regM: xReg, regT: xReg, option: scale extend} -> instr
    and loadRegIndexedByte: {regN: xReg, regM: xReg, regT: xReg, option: scale extend} -> instr
    and storeRegIndexedByte: {regN: xReg, regM: xReg, regT: xReg, option: scale extend} -> instr

    (* Load/Store a value using a signed byte offset and post-indexing (post-increment). *)
    (* The terminology is confusing. Pre-indexing means adding the offset into base address
       before loading the value, typically used for push, and post-index means using the
       original value of the base register as the address and adding in the offset after
       the value has been loaded, e.g. pop. *)
    val loadRegPostIndex: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and storeRegPostIndex: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and loadRegPostIndexByte: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and storeRegPostIndexByte: {regT: xReg, regN: xReg, byteOffset: int} -> instr

    (* Load/Store a value using a signed byte offset and pre-indexing (pre-increment). *)
    val loadRegPreIndex: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and storeRegPreIndex: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and loadRegPreIndexByte: {regT: xReg, regN: xReg, byteOffset: int} -> instr
    and storeRegPreIndexByte: {regT: xReg, regN: xReg, byteOffset: int} -> instr

    (* This word is put in after a call to the RTS trap-handler.  All the registers
       are saved and restored across a call to the trap-handler; the register
       mask contains those that may contain an address and so need to be scanned and
       possibly updated if there is a GC. *)
    val registerMask: xReg list -> instr


    (* Create a label. *)
    val createLabel: unit -> labels
    (* Put a label into the code. *)
    val setLabel: labels -> instr
    (* A conditional or unconditional branch. *)
    val putBranchInstruction: condition * labels -> instr
    (* Put the address of a label into a register - used for handlers and cases. *)
    and loadLabelAddress: xReg * labels -> instr

    (* Sets the destination register to the value of the first reg if the
       condition is true otherwise the second register incremented by one. *)
    val conditionalSetIncrement:
        {regD: xReg, regTrue: xReg, regFalse: xReg, cond: condition} -> instr

    (* Various shifts *)
    val logicalShiftLeft: {wordSize: wordSize, shift: word, regN: xReg, regD: xReg} -> instr
    and logicalShiftRight: {wordSize: wordSize, shift: word, regN: xReg, regD: xReg} -> instr
    and unsignedBitfieldInsertinZeros:
        {wordSize: wordSize, lsb: word, width: word, regN: xReg, regD: xReg} -> instr

    (* Logical operations on bit patterns.  The pattern must be valid.
       ANDS is an AND that also sets the flags, typically used for a test. *)
    val bitwiseAndImmediate: {wordSize: wordSize, bits: Word64.word, regN: xReg, regD: xReg} -> instr
    and bitwiseOrImmediate: {wordSize: wordSize, bits: Word64.word, regN: xReg, regD: xReg} -> instr
    and bitwiseXorImmediate: {wordSize: wordSize, bits: Word64.word, regN: xReg, regD: xReg} -> instr
    and bitwiseAndSImmediate: {wordSize: wordSize, bits: Word64.word, regN: xReg, regD: xReg} -> instr

    (* Create the vector of code from the list of instructions and update the
       closure reference to point to it. *)
    val generateCode:
        {instrs: instr list, name: string, parameters: Universal.universal list, resultClosure: closureRef} -> unit

    (* Offsets in the assembly code interface pointed at by X26
       These are in units of 64-bits NOT bytes. *)
    val heapOverflowCallOffset: int
    and stackOverflowCallOffset: int
    and stackOverflowXCallOffset: int
    and exceptionHandlerOffset: int
    and stackLimitOffset: int
    and exceptionPacketOffset: int
    and threadIdOffset: int

    structure Sharing:
    sig
        type closureRef = closureRef
        type instr = instr
        type xReg = xReg
        type labels = labels
        type condition = condition
        type shiftType = shiftType
        type wordSize = wordSize
        type 'a extend = 'a extend
        type scale = scale
    end
end;
