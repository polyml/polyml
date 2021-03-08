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

functor ARM64ASSEMBLY (
    structure Debug: DEBUG
    and       Pretty: PRETTYSIG
    and       CodeArray: CODEARRAYSIG
) : Arm64Assembly =

struct
    open CodeArray Address
    
    exception InternalError = Misc.InternalError

    infix 5 << <<+ <<- >> >>+ >>- ~>> ~>>+ ~>>- (* Shift operators *)
    infix 3 andb orb xorb andbL orbL xorbL andb8 orb8 xorb8
    
    val op << = Word.<< and op >> = Word.>> and op ~>> = Word.~>>
    and op andb = Word.andb and op orb = Word.orb

    val wordToWord8 = Word8.fromLargeWord o Word.toLargeWord
    and word8ToWord = Word.fromLargeWord o Word8.toLargeWord
    
    (* XReg is used for fixed point registers since X0 and W0 are
       the same register. *)
    datatype xReg = XReg of Word8.word | XZero | XSP
    (* VReg is used for the floating point registers since V0, D0 and
       S0 are the same register. *)
    and vReg = VReg of Word8.word

    (* A Label is a ref that is later set to the location.
       Several labels can be linked together so that they are only set
       at a single point.
       Only forward jumps are linked so when we come to finally set the
       label we will have the full list. *)
    type labels = Word.word ref list ref

    (* Condition codes.  The encoding is standard. *)
    datatype condition = CCode of Word8.word
    
    val condEqual           = CCode 0wx0 (* Z=1 *)
    and condNotEqual        = CCode 0wx1 (* Z=0 *)
    and condCarrySet        = CCode 0wx2 (* C=1 *)
    and condCarryClear      = CCode 0wx3 (* C=0 *)
    and condNegative        = CCode 0wx4 (* N=1 *)
    and condPositive        = CCode 0wx5 (* N=0 imcludes zero *)
    and condOverflow        = CCode 0wx6 (* V=1 *)
    and condNoOverflow      = CCode 0wx7 (* V=0 *)
    and condUnsignedHigher  = CCode 0wx8 (* C=1 && Z=0 *)
    and condUnsignedLowOrEq = CCode 0wx9 (* ! (C=1 && Z=0) *)
    and condSignedGreaterEq = CCode 0wxa (* N=V *)
    and condSignedLess      = CCode 0wxb (* N<>V *)
    and condSignedGreater   = CCode 0wxc (* Z==0 && N=V *)
    and condSignedLessEq    = CCode 0wxd (* !(Z==0 && N=V) *)
    (* use unconditional branches for the "always" cases. *)
    (* N.B. On subtraction and comparison the ARM uses an inverted carry
       flag for borrow.  The C flag is set if there is NO borrow.
       This is the reverse of the X86. *)

    (* Offsets in the assembly code interface pointed at by X26
       These are in units of 64-bits NOT bytes. *)
    val heapOverflowCallOffset  = 1
    and stackOverflowCallOffset = 2
    and stackOverflowXCallOffset= 3
    and exceptionHandlerOffset  = 5
    and stackLimitOffset        = 6
    and exceptionPacketOffset   = 7
    and threadIdOffset          = 8
    and heapLimitPtrOffset      = 42
    and heapAllocPtrOffset      = 43
    and mlStackPtrOffset        = 44

    (* 31 in the register field can either mean the zero register or
       the hardware stack pointer.  Which meaning depends on the instruction. *)
    fun xRegOrXZ(XReg w) = w
    |   xRegOrXZ XZero = 0w31
    |   xRegOrXZ XSP = raise InternalError "XSP not valid here"
    
    and xRegOrXSP(XReg w) = w
    |   xRegOrXSP XZero = raise InternalError "XZero not valid here"
    |   xRegOrXSP XSP = 0w31
    
    (* There are cases where it isn't clear. *)
    and xRegOnly (XReg w) = w
    |   xRegOnly XZero = raise InternalError "XZero not valid here"
    |   xRegOnly XSP = raise InternalError "XSP not valid here"

    val X0  = XReg 0w0  and X1  = XReg 0w1  and X2 = XReg 0w2   and X3  = XReg 0w3
    and X4  = XReg 0w4  and X5  = XReg 0w5  and X6 = XReg 0w6   and X7  = XReg 0w7
    and X8  = XReg 0w8  and X9  = XReg 0w9  and X10= XReg 0w10  and X11 = XReg 0w11
    and X12 = XReg 0w12 and X13 = XReg 0w13 and X14= XReg 0w14  and X15 = XReg 0w15
    and X16 = XReg 0w16 and X17 = XReg 0w17 and X18= XReg 0w18  and X19 = XReg 0w19
    and X20 = XReg 0w20 and X21 = XReg 0w21 and X22= XReg 0w22  and X23 = XReg 0w23
    and X24 = XReg 0w24 and X25 = XReg 0w25 and X26= XReg 0w26  and X27 = XReg 0w27
    and X28 = XReg 0w28 and X29 = XReg 0w29 and X30= XReg 0w30
    
    val X_MLHeapLimit       = X25 (* ML Heap limit pointer *)
    and X_MLAssemblyInt     = X26 (* ML assembly interface pointer. *)
    and X_MLHeapAllocPtr    = X27 (* ML Heap allocation pointer. *)
    and X_MLStackPtr        = X28 (* ML Stack pointer. *)
    and X_LinkReg           = X30 (* Link reg - return address *)
    
    fun vReg(VReg v) = v
    (* Only the first eight registers are currently used by ML. *)
    val V0  = VReg 0w0  and V1  = VReg 0w1 and V2 = VReg 0w2   and V3  = VReg 0w3
    and V4  = VReg 0w4  and V5  = VReg 0w5 and V6 = VReg 0w6   and V7  = VReg 0w7

    (* Some data instructions include a possible shift. *)
    datatype shiftType =
        ShiftLSL of word
    |   ShiftLSR of word
    |   ShiftASR of word
    |   ShiftNone

    local
        fun checkImm6 w = if w > 0w63 then raise InternalError "shift > 63" else w
    in 
        fun shiftEncode(ShiftLSL w) = (0w0, checkImm6 w)
        |   shiftEncode(ShiftLSR w) = (0w1, checkImm6 w)
        |   shiftEncode(ShiftASR w) = (0w2, checkImm6 w)
        |   shiftEncode ShiftNone   = (0w0, 0w0)
    end

    (* Other instructions include an extension i.e. a sign- or zero-extended
       value from one of the argument registers.  When an extension is encoded
       there can also be a left shift which applies after the extension.
       I don't understand what difference, if any, there is between UXTX
       and SXTX.
       There's no ExtNone because we need to use either UXTW or UXTX depending
       on the length *)
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

    local
        (* Although there are three bits it seems that the shift is limited to 0 to 4. *)
        fun checkImm3 w = if w > 0w4 then raise InternalError "extend shift > 4" else w
    in
        fun extendArithEncode(ExtUXTB w) = (0w0, checkImm3 w)
        |   extendArithEncode(ExtUXTH w) = (0w1, checkImm3 w)
        |   extendArithEncode(ExtUXTW w) = (0w2, checkImm3 w)
        |   extendArithEncode(ExtUXTX w) = (0w3, checkImm3 w)
        |   extendArithEncode(ExtSXTB w) = (0w4, checkImm3 w)
        |   extendArithEncode(ExtSXTH w) = (0w5, checkImm3 w)
        |   extendArithEncode(ExtSXTW w) = (0w6, checkImm3 w)
        |   extendArithEncode(ExtSXTX w) = (0w7, checkImm3 w)
        
        fun extendLSEncode(ExtUXTB v) = (0w0, v)
        |   extendLSEncode(ExtUXTH v) = (0w1, v)
        |   extendLSEncode(ExtUXTW v) = (0w2, v)
        |   extendLSEncode(ExtUXTX v) = (0w3, v)
        |   extendLSEncode(ExtSXTB v) = (0w4, v)
        |   extendLSEncode(ExtSXTH v) = (0w5, v)
        |   extendLSEncode(ExtSXTW v) = (0w6, v)
        |   extendLSEncode(ExtSXTX v) = (0w7, v)
    end

    datatype wordSize = WordSize32 | WordSize64

    (* Bit patterns on the ARM64 are encoded using a complicated scheme and
       only certain values can be encoded.  An element can be 2, 4, 8, 16, 32 or
       64 bits and must be a sequence of at least one zero bits followed by at
       least one one bit.  This sequence can then be rotated within the element.
       Finally the element is replicated within the register up to 32 or
       64 bits.  All this information is encoded in 13 bits.
       N.B. Bit patterns of all zeros or all ones cannot be encoded. *)

    (* Encode the value if it is possible. *)
    fun encodeBitPattern(value, sf (* size flag *)) =
    (* Can't encode 0 or all ones. *)
    if value = 0w0 orelse value = Word64.notb 0w0
    then NONE
    (* If this is 32-bits we can't encode all ones in the
       low-order 32-bits or any value that won't fit in 32-bits, *)
    else if sf = WordSize32 andalso value >= 0wxffffffff
    then NONE
    else
    let
        val regSize = case sf of WordSize32 => 0w32 | WordSize64 => 0w64
        (* Get the element size.  Look for the repeat of the
           pattern. *)
        fun getElemSize size =
        let
            val ns = size div 0w2
            val mask = Word64.<<(0w1, ns)  - 0w1
        in
            if Word64.andb(value, mask) <> Word64.andb(Word64.>>(value, ns), mask)
            then size
            else if ns <= 0w2
            then ns
            else getElemSize ns
        end
        val elemSize = getElemSize regSize
        fun log2 0w1 = 0w0 | log2 n = 0w1 + log2(Word.>>(n, 0w1))
        val elemBits = log2 elemSize

        (* Find the rotation that puts as many of the zero bits in the
           element at the top. *)
        val elemMask = Word64.>>(Word64.notb 0w0, 0w64-elemSize)
        fun ror elt =
            Word64.orb((Word64.<<(Word64.andb(elt, 0w1), elemSize-0w1),
                Word64.>>(elt, 0w1)))
        and rol elt =
            Word64.orb(Word64.andb(elemMask, Word64.<<(elt, 0w1)),
                Word64.>>(elt, elemSize-0w1))

        fun findRotation(v, n) =
            if ror v < v then findRotation(ror v, (n-0w1) mod elemSize)
            else if rol v < v then findRotation(rol v, n+0w1)
            else (v, n)

        val (rotated, rotation) = findRotation(Word64.andb(value, elemMask), 0w0)

        (* Count out the low order ones.  If the result is zero
           then we;ve got a valid sequence of zeros followed by ones
           but if we discover a zero bit and the result isn't zero
           then we can't encode this. *)
        fun countLowOrderOnes(v, n) =
            if v = 0w0
            then SOME n
            else if Word64.andb(v, 0w1) = 0w1
            then countLowOrderOnes(Word64.>>(v, 0w1), n+0w1)
            else NONE
     in
        case countLowOrderOnes(rotated, 0w0) of
            NONE => NONE
        |   SOME lowOrderOnes =>
            let
                (* Encode the element size. *)
                val elemSizeEnc = 0wx7f - (Word.<<(0w1, elemBits+0w1) - 0w1)
                val n = if Word.andb(elemSizeEnc, 0wx40) = 0w0 then 0w1 else 0w0
                val imms = Word.andb(Word.orb(elemSizeEnc, lowOrderOnes-0w1), 0wx3f)
            in
                SOME{n=n, imms=imms, immr=rotation}
            end
    end;

    (* Decode a pattern for printing. *)
    fun decodeBitPattern{sf, n, immr, imms} =
    let
        (* Find the highest bit set in N:NOT(imms) *)
        fun highestBitSet 0w0 = 0
        |   highestBitSet n = 1+highestBitSet(Word.>>(n, 0w1))
        val len = highestBitSet(Word.orb(Word.<<(n, 0w6), Word.xorb(imms, 0wx3f))) - 1
        val _ = if len < 0 then raise InternalError "decodeBitPattern: invalid" else ()
        val size = Word.<<(0w1, Word.fromInt len)
        val r = Word.andb(immr, size-0w1)
        and s = Word.andb(imms, size-0w1)
        val _ = if s = size-0w1 then raise InternalError "decodeBitPattern: invalid" else ()
        val pattern = Word64.<<(0w1, s+0w1) - 0w1
        (* Rotate right: shift left and put the top bit in the high order bit*)
        fun ror elt =
            Word64.orb((Word64.<<(Word64.andb(elt, 0w1), size-0w1),
                Word64.>>(elt, 0w1)))

        fun rotateBits(value, 0w0) = value
        |   rotateBits(value, n) = rotateBits(ror value, n-0w1)

        val rotated = rotateBits(pattern, r)

        val regSize = if sf = 0w0 then 0w32 else 0w64

        (* Replicate the rotated pattern to fill the register. *)
        fun replicate(pattern, size) =
            if size >= regSize
            then pattern
            else replicate(Word64.orb(pattern, Word64.<<(pattern, size)), size * 0w2)
    in
        replicate(rotated, size)
    end

    val isEncodableBitPattern = isSome o encodeBitPattern


    datatype instr =
        SimpleInstr of word
    |   LoadAddressLiteral of {reg: xReg, value: machineWord}
    |   LoadNonAddressLiteral of {reg: xReg, value: Word64.word}
    |   Label of labels
    |   UnconditionalBranch of labels
    |   ConditionalBranch of { label: labels, jumpCondition: condition, length: brLength ref }
    |   LoadLabelAddress of { label: labels, reg: xReg }
    |   TestBitBranch of { label: labels, bitNo: Word8.word, brNonZero: bool, reg: xReg, length: brLength ref }
    |   CompareBranch of { label: labels, brNonZero: bool, size: wordSize, reg: xReg, length: brLength ref }
    
    and brLength = BrShort | BrExtended

    val nopCode  = 0wxD503201F

    (* Add/subtract an optionally shifted 12-bit immediate (i.e. constant) to/from a register.
       The constant is zero-extended.  The versions that do not set the flags can use XSP as
       the destination; the versions that use the signs can use XZero as the destination i.e.
       they discard the result and act as a comparison. *)
    local
        fun addSubRegImmediate(sf, oper, s, xdOp) ({regN, regD, immed, shifted}) =
        let
            val () =
                if immed >= 0wx1000 then raise InternalError "addSubRegImmediate: immed > 12 bits" else ()
        in
            SimpleInstr(
                0wx11000000 orb (sf << 0w31) orb (oper << 0w30) orb (s << 0w29) orb
                (if shifted then 0wx400000 else 0w0) orb
                (immed << 0w10) orb (word8ToWord(xRegOrXSP regN) << 0w5) orb
                word8ToWord(xdOp regD))
        end
    in
        val addImmediate = addSubRegImmediate(0w1, 0w0, 0w0, xRegOrXSP)
        and addSImmediate = addSubRegImmediate(0w1, 0w0, 0w1, xRegOrXZ)
        and subImmediate = addSubRegImmediate(0w1, 0w1, 0w0, xRegOrXSP)
        and subSImmediate = addSubRegImmediate(0w1, 0w1, 0w1, xRegOrXZ)
    end

    (* Add/subtract a shifted register, optionally setting the flags. *)
    local
        (* X31 is XZ here unlike the extended version.*)
        fun addSubtractShiftedReg (sf, oper, s) ({regM, regN, regD, shift}) =
        let
            val (shift, imm6) = shiftEncode shift
        in
            SimpleInstr(0wx0b000000 orb (sf << 0w31) orb (oper << 0w30) orb (s << 0w29) orb
                (shift << 0w22) orb (word8ToWord(xRegOnly regM) << 0w16) orb
                (imm6 << 0w10) orb (word8ToWord(xRegOrXZ regN) << 0w5) orb
                word8ToWord(xRegOrXZ regD))
        end
    in
        val addShiftedReg = addSubtractShiftedReg(0w1, 0w0, 0w0)
        and addSShiftedReg = addSubtractShiftedReg(0w1, 0w0, 0w1)
        and subShiftedReg = addSubtractShiftedReg(0w1, 0w1, 0w0)
        and subSShiftedReg = addSubtractShiftedReg(0w1, 0w1, 0w1)
    end

    (* Add/subtract an extended register, optionally setting the flags. *)
    local
        (* SP can be used as Xn and also for Xd for the non-flags versions. *)
        fun addSubtractExtendedReg (sf, oper, s, opt, xD) ({regM, regN, regD, extend}) =
        let
            val (option, imm3) = extendArithEncode extend
        in
            SimpleInstr(0wx0b200000 orb (sf << 0w31) orb (oper << 0w30) orb (s << 0w29) orb
                (opt << 0w22) orb (word8ToWord(xRegOnly regM) << 0w16) orb
                (option << 0w13) orb (imm3 << 0w10) orb
                (word8ToWord(xRegOrXSP regN) << 0w5) orb
                word8ToWord(xD regD))
        end
    in
        val addExtendedReg = addSubtractExtendedReg(0w1, 0w0, 0w0, 0w0, xRegOrXSP)
        and addSExtendedReg = addSubtractExtendedReg(0w1, 0w0, 0w1, 0w0, xRegOrXZ)
        and subExtendedReg = addSubtractExtendedReg(0w1, 0w1, 0w0, 0w0, xRegOrXSP)
        and subSExtendedReg = addSubtractExtendedReg(0w1, 0w1, 0w1, 0w0, xRegOrXZ)
    end

    (* Logical operations on a shifted register. *)
    local
        fun logicalShiftedReg (sf, oper, n) ({regM, regN, regD, shift}) =
        let
            val (shift, imm6) = shiftEncode shift
        in
            SimpleInstr(0wx0a000000 orb (sf << 0w31) orb (oper << 0w29) orb
                (shift << 0w22) orb (n << 0w21) orb (word8ToWord(xRegOrXZ regM) << 0w16) orb
                (imm6 << 0w10) orb (word8ToWord(xRegOrXZ regN) << 0w5) orb
                word8ToWord(xRegOrXZ regD))
        end
    in
        val andShiftedReg = logicalShiftedReg(0w1, 0w0, 0w0)
        and orrShiftedReg = logicalShiftedReg(0w1, 0w1, 0w0)
        and eorShiftedReg = logicalShiftedReg(0w1, 0w2, 0w0)
        and andsShiftedReg = logicalShiftedReg(0w1, 0w3, 0w0)
        (* There are also versions that operate with an inverted version
           of the argument. *)
    end

    (* Two-source operations. *)
    local
        fun twoSourceInstr (sf, s, opcode) ({regM, regN, regD}) =
            SimpleInstr(0wx1ac00000 orb (sf << 0w31) orb (s << 0w29) orb
                (word8ToWord(xRegOnly regM) << 0w16) orb (opcode << 0w10) orb
                (word8ToWord(xRegOnly regN) << 0w5) orb
                word8ToWord(xRegOnly regD))
    in
        (* Signed and unsigned division. *)
        val unsignedDivide   = twoSourceInstr(0w1, 0w0, 0wx2)
        and signedDivide     = twoSourceInstr(0w1, 0w0, 0wx3)
        (* Logical shift left Rd = Rn << (Rm mod 0w64) *)
        and logicalShiftLeftVariable = twoSourceInstr(0w1, 0w0, 0wx8)
        (* Logical shift right Rd = Rn >> (Rm mod 0w64) *)
        and logicalShiftRightVariable = twoSourceInstr(0w1, 0w0, 0wx9)
        (* Arithmetic shift right Rd = Rn ~>> (Rm mod 0w64) *)
        and arithmeticShiftRightVariable = twoSourceInstr(0w1, 0w0, 0wxa)
    end

    (* Three source operations.  These are all variations of multiply. *)
    local
        fun threeSourceInstr (sf, op54, op31, o0) ({regM, regA, regN, regD}) =
            SimpleInstr(0wx1b000000 orb (sf << 0w31) orb (op54 << 0w29) orb
                (op31 << 0w21) orb (word8ToWord(xRegOnly regM) << 0w16) orb
                (o0 << 0w15) orb (word8ToWord(xRegOrXZ regA) << 0w10) orb
                (word8ToWord(xRegOnly regN) << 0w5) orb
                word8ToWord(xRegOnly regD))
    in
        (* regD = regA + regN * regM *)
        val multiplyAndAdd = threeSourceInstr(0w1, 0w0, 0w0, 0w0)
        (* regD = regA - regN * regM *)
        and multiplyAndSub = threeSourceInstr(0w1, 0w0, 0w0, 0w1)
        (* Return the high-order part of a signed multiplication. *)
        fun signedMultiplyHigh({regM, regN, regD}) =
            threeSourceInstr(0w1, 0w0, 0w2, 0w0) { regM=regM, regN=regN, regD=regD, regA=XZero}
    end

    (* Loads: There are two versions of this on the ARM.  There is a version that
       takes a signed 9-bit byte offset and a version that takes an unsigned
       12-bit word offset. *)
    
    local
        fun loadStoreRegScaled (size, v, opc, xD) ({regT, regN, unitOffset}) =
        let
            val _ = (unitOffset >= 0 andalso unitOffset < 0x1000)
                orelse raise InternalError "loadStoreRegScaled: value out of range"
        in
            SimpleInstr(0wx39000000 orb (size << 0w30) orb (opc << 0w22) orb
                (v << 0w26) orb (Word.fromInt unitOffset << 0w10) orb
                (word8ToWord(xRegOrXSP regN) << 0w5) orb word8ToWord(xD regT))
        end
    in
        val loadRegScaled = loadStoreRegScaled(0w3, 0w0, 0w1, xRegOrXZ)
        and storeRegScaled = loadStoreRegScaled(0w3, 0w0, 0w0, xRegOrXZ)
        (* (Unsigned) byte operations.  There are also signed versions. *)
        and loadRegScaledByte = loadStoreRegScaled (0w0, 0w0, 0w1, xRegOrXZ)
        and storeRegScaledByte = loadStoreRegScaled (0w0, 0w0, 0w0, xRegOrXZ)
        and loadRegScaled16 = loadStoreRegScaled (0w1, 0w0, 0w1, xRegOrXZ)
        and storeRegScaled16 = loadStoreRegScaled (0w1, 0w0, 0w0, xRegOrXZ)
        and loadRegScaled32 = loadStoreRegScaled (0w2, 0w0, 0w1, xRegOrXZ)
        and storeRegScaled32 = loadStoreRegScaled (0w2, 0w0, 0w0, xRegOrXZ)
        and loadRegScaledDouble = loadStoreRegScaled(0w3, 0w1, 0w1, vReg)
        and storeRegScaledDouble = loadStoreRegScaled(0w3, 0w1, 0w0, vReg)
        and loadRegScaledFloat = loadStoreRegScaled(0w2, 0w1, 0w1, vReg)
        and storeRegScaledFloat = loadStoreRegScaled(0w2, 0w1, 0w0, vReg)
    end    

    local
        (* Loads and stores with a signed byte offset.  This includes simple
           unscaled addresses, pre-indexing and post-indexing. *)
        fun loadStoreByteAddress (op4, xD) (size, v, opc) ({regT, regN, byteOffset}) =
        let
            val _ = (byteOffset >= ~256 andalso byteOffset < 256)
                orelse raise InternalError "loadStoreUnscaled: value out of range"
            val imm9 = Word.fromInt byteOffset andb 0wx1ff
        in
            SimpleInstr(0wx38000000 orb (size << 0w30) orb (opc << 0w22) orb
                (v << 0w26) orb (imm9 << 0w12) orb (op4 << 0w10) orb
                (word8ToWord(xRegOrXSP regN) << 0w5) orb word8ToWord(xD regT))
        end
        
        val loadStoreUnscaled = loadStoreByteAddress (0w0, xRegOrXZ)
        and loadStoreUnscaledSIMD = loadStoreByteAddress (0w0, vReg)
        and loadStorePostIndex = loadStoreByteAddress (0w1, xRegOrXZ)
        and loadStorePreIndex = loadStoreByteAddress (0w3, xRegOrXZ)
    in
        val loadRegUnscaled = loadStoreUnscaled (0w3, 0w0, 0w1)
        and storeRegUnscaled = loadStoreUnscaled (0w3, 0w0, 0w0)
        (* (Unsigned) byte operations.  There are also signed versions. *)
        and loadRegUnscaledByte = loadStoreUnscaled (0w0, 0w0, 0w1)
        and storeRegUnscaledByte = loadStoreUnscaled (0w0, 0w0, 0w0)
        and loadRegUnscaled16 = loadStoreUnscaled (0w1, 0w0, 0w1)
        and storeRegUnscaled16 = loadStoreUnscaled (0w1, 0w0, 0w0)
        and loadRegUnscaled32 = loadStoreUnscaled (0w2, 0w0, 0w1)
        and storeRegUnscaled32 = loadStoreUnscaled (0w2, 0w0, 0w0)
        and loadRegUnscaledFloat = loadStoreUnscaledSIMD (0w2, 0w1, 0w1)
        and storeRegUnscaledFloat = loadStoreUnscaledSIMD (0w2, 0w1, 0w0)
        and loadRegUnscaledDouble = loadStoreUnscaledSIMD (0w3, 0w1, 0w1)
        and storeRegUnscaledDouble = loadStoreUnscaledSIMD (0w3, 0w1, 0w0)

        val loadRegPostIndex = loadStorePostIndex (0w3, 0w0, 0w1)
        and storeRegPostIndex = loadStorePostIndex (0w3, 0w0, 0w0)
        and loadRegPostIndexByte = loadStorePostIndex (0w0, 0w0, 0w1)
        and storeRegPostIndexByte = loadStorePostIndex (0w0, 0w0, 0w0)

        val loadRegPreIndex = loadStorePreIndex (0w3, 0w0, 0w1)
        and storeRegPreIndex = loadStorePreIndex (0w3, 0w0, 0w0)
        and loadRegPreIndexByte = loadStorePreIndex (0w0, 0w0, 0w1)
        and storeRegPreIndexByte = loadStorePreIndex (0w0, 0w0, 0w0)
    end

    (* Load/store with a register offset i.e. an index register. *)
    local
        fun loadStoreRegRegisterOffset (size, v, opc, xD) ({regT, regN, regM, option}) =
        let
            val (opt, s) =
                case extendLSEncode option of
                    (opt, ScaleOrShift) => (opt, 0w1) | (opt, NoScale) => (opt, 0w0)
        in
            SimpleInstr(0wx38200800 orb (size << 0w30) orb (v << 0w26) orb (opc << 0w22) orb
                (word8ToWord(xRegOnly regM) << 0w16) orb (opt << 0w13) orb (s << 0w12) orb
                (word8ToWord(xRegOrXSP regN) << 0w5) orb word8ToWord(xD regT))
        end
    in
        val loadRegIndexed = loadStoreRegRegisterOffset(0w3, 0w0, 0w1, xRegOrXZ)
        and storeRegIndexed = loadStoreRegRegisterOffset(0w3, 0w0, 0w0, xRegOrXZ)
        and loadRegIndexedByte = loadStoreRegRegisterOffset(0w0, 0w0, 0w1, xRegOrXZ)
        and storeRegIndexedByte = loadStoreRegRegisterOffset(0w0, 0w0, 0w0, xRegOrXZ)
        and loadRegIndexed16 = loadStoreRegRegisterOffset(0w1, 0w0, 0w1, xRegOrXZ)
        and storeRegIndexed16 = loadStoreRegRegisterOffset(0w1, 0w0, 0w0, xRegOrXZ)
        and loadRegIndexed32 = loadStoreRegRegisterOffset(0w2, 0w0, 0w1, xRegOrXZ)
        and storeRegIndexed32 = loadStoreRegRegisterOffset(0w2, 0w0, 0w0, xRegOrXZ)
        and loadRegIndexedFloat = loadStoreRegRegisterOffset(0w2, 0w1, 0w1, vReg)
        and storeRegIndexedFloat = loadStoreRegRegisterOffset(0w2, 0w1, 0w0, vReg)
        and loadRegIndexedDouble = loadStoreRegRegisterOffset(0w3, 0w1, 0w1, vReg)
        and storeRegIndexedDouble = loadStoreRegRegisterOffset(0w3, 0w1, 0w0, vReg)
    end

    (* Addresses must go in the constant area at the end of the code where they
       can be found by the GC. *)
    fun loadAddressConstant(xReg, valu) = LoadAddressLiteral{reg=xReg, value=valu}

    (* Non-address constants.  These may or may not be tagged values. *)
    fun loadNonAddressConstant(xReg, valu) = LoadNonAddressLiteral{reg=xReg, value=valu}

    local
        fun moveWideImmediate(sf, opc) {regD, immediate, shift} =
        let
            val hw =
                case (shift, sf) of
                    (0w0, _) => 0w0
                |   (0w16, _) => 0w1
                |   (0w24, 0w1) => 0w2
                |   (0w48, 0w1) => 0w3
                |   _ => raise InternalError "moveWideImmediate: invalid shift"
            val _ =
                immediate <= 0wxffff orelse raise InternalError "moveWideImmediate: immediate too large"
        in
            SimpleInstr(0wx12800000 orb (sf << 0w31) orb (opc << 0w29) orb
                (hw << 0w21) orb (immediate << 0w5) orb word8ToWord(xRegOnly regD))
        end
    in
        val moveNot32 = moveWideImmediate(0w0, 0w0)
        and moveZero32 = moveWideImmediate(0w0, 0w2)
        and moveKeep32 = moveWideImmediate(0w0, 0w3)
        and moveNot = moveWideImmediate(0w1, 0w0)
        and moveZero = moveWideImmediate(0w1, 0w2)
        and moveKeep = moveWideImmediate(0w1, 0w3)
    end

    (* Instructions involved in thread synchonisation. *)
    val yield = SimpleInstr 0wxD503203F (* Yield inside a spin-lock. *)
    and dmbIsh = SimpleInstr 0wxD5033BBF (* Memory barrier. *)
    
    (* Acquire exclusive access to a memory location and load its current value *)
    fun loadAcquireExclusiveRegister{regN, regT} =
        SimpleInstr(0wxC85FFC00 orb (word8ToWord(xRegOrXSP regN) << 0w5) orb
                word8ToWord(xRegOnly regT))
    (* Release exclusive access and test whether it succeeded.  Sets regS to 0
       if successful otherwise 1, in which case we have to repeat the operation. *)
    and storeReleaseExclusiveRegister{regN, regS, regT} =
        SimpleInstr(0wxC800FC00 orb (word8ToWord(xRegOnly regS) << 0w16) orb (word8ToWord(xRegOrXSP regN) << 0w5) orb
                word8ToWord(xRegOnly regT))

    (* Jump to the address in the register and put the address of the
       next instruction into X30. *)
    fun branchAndLinkReg(dest) =
        SimpleInstr(0wxD63F0000 orb (word8ToWord(xRegOnly dest) << 0w5))

    (* Jump to the address in the register. *)
    fun branchRegister(dest) =
        SimpleInstr(0wxD61F0000 orb (word8ToWord(xRegOnly dest) << 0w5))

    (* Jump to the address in the register and hint this is a return. *)
    fun returnRegister(dest) =
        SimpleInstr(0wxD65F0000 orb (word8ToWord(xRegOnly dest) << 0w5))

    (* Put a label into the code. *)
    val setLabel = Label

    (* Create a label. *)
    fun createLabel () = ref [ref 0w0]

    (* A conditional or unconditional branch. *)
    and conditionalBranch(cond, label) = ConditionalBranch{label=label, jumpCondition=cond, length=ref BrExtended }
    and unconditionalBranch label = UnconditionalBranch label
    (* Put the address of a label into a register - used for handlers and cases. *)
    and loadLabelAddress(reg, label) = LoadLabelAddress{label=label, reg=reg}
    (* Test a bit in a register and branch if zero/nonzero *)
    and testBitBranchZero(reg, bit, label) =
        TestBitBranch{label=label, bitNo=bit, brNonZero=false, reg=reg, length=ref BrExtended}
    and testBitBranchNonZero(reg, bit, label) =
        TestBitBranch{label=label, bitNo=bit, brNonZero=true, reg=reg, length=ref BrExtended}
    (* Compare a register with zero and branch if zero/nonzero *)
    and compareBranchZero(reg, size, label) =
        CompareBranch{label=label, brNonZero=false, size=size, reg=reg, length=ref BrExtended}
    and compareBranchNonZero(reg, size, label) =
        CompareBranch{label=label, brNonZero=true, size=size, reg=reg, length=ref BrExtended}
    

    (* Set the destination register to the value of the first reg if the
       condition is true otherwise to a, possibly modified, version of
       the second argument.  There are variants that set it unmodified,
       incremented, inverted and negated. *)
    local
        fun conditionalSelect (sf, opc, op2) {regD, regFalse, regTrue, cond=CCode cond} =
            SimpleInstr(0wx1A800000 orb (sf << 0w31) orb (opc << 0w30) orb
                (word8ToWord(xRegOrXZ regFalse) << 0w16) orb (word8ToWord cond << 0w12) orb
                (op2 << 0w10) orb (word8ToWord(xRegOrXZ regTrue) << 0w5) orb
                word8ToWord(xRegOrXZ regD))
    in
        val conditionalSet = conditionalSelect(0w1, 0w0, 0w0)
        and conditionalSetIncrement = conditionalSelect(0w1, 0w0, 0w1)
        and conditionalSetInverted = conditionalSelect(0w1, 0w1, 0w0)
        and conditionalSetNegated = conditionalSelect(0w1, 0w1, 0w1)
    end

    (* This combines the effect of a left and right shift.  There are various
       derived forms of this depending on the relative values of immr and imms.
       if imms >= immr copies imms-immr-1 bits from bit position immr to the lsb
       bits of the destination.
       if imms < immr copies imms+1 bits from the lsb bit to bit position
       regsize-immr.
       How the remaining bits are affected depends on the instruction.
       BitField instructions do not affect other bits.
       UnsignedBitField instructions zero other bits.
       SignedBitField instructions set the high order bits to a copy of
       the high order bit copied and zero the low order bits. *)
    local
        fun bitfield (sf, opc, n) {immr, imms, regN, regD} =
            SimpleInstr(0wx13000000 orb (sf << 0w31) orb (opc << 0w29) orb (n << 0w22) orb
                (immr << 0w16) orb (imms << 0w10) orb (word8ToWord(xRegOrXZ regN) << 0w5) orb
                word8ToWord(xRegOrXZ regD))

        val signedBitfieldMove32 = bitfield(0w0, 0w0, 0w0)
        and bitfieldMove32 = bitfield(0w0, 0w1, 0w0)
        and unsignedBitfieldMove32 = bitfield(0w0, 0w2, 0w0)
        and signedBitfieldMove64 = bitfield(0w1, 0w0, 0w1)
        and bitfieldMove64 = bitfield(0w1, 0w1, 0w1)
        and unsignedBitfieldMove64 = bitfield(0w1, 0w2, 0w1)
    in
        fun logicalShiftLeft{wordSize=WordSize64, shift, regN, regD} =
                unsignedBitfieldMove64{immr=Word.~ shift mod 0w64,
                    imms=0w64-0w1-shift, regN=regN, regD=regD}
        |   logicalShiftLeft{wordSize=WordSize32, shift, regN, regD} =
                unsignedBitfieldMove32{immr=Word.~ shift mod 0w32,
                    imms=0w32-0w1-shift, regN=regN, regD=regD}

        and logicalShiftRight{wordSize=WordSize64, shift, regN, regD} =
                unsignedBitfieldMove64{immr=shift, imms=0wx3f, regN=regN, regD=regD}
        |   logicalShiftRight{wordSize=WordSize32, shift, regN, regD} =
                unsignedBitfieldMove32{immr=shift, imms=0wx1f, regN=regN, regD=regD}

        and unsignedBitfieldInsertinZeros{wordSize=WordSize64, lsb, width, regN, regD} =
                unsignedBitfieldMove64{immr=Word.~ lsb mod 0w64,
                    imms=width-0w1, regN=regN, regD=regD}
        |   unsignedBitfieldInsertinZeros{wordSize=WordSize32, lsb, width, regN, regD} =
                unsignedBitfieldMove32{immr=Word.~ lsb mod 0w32,
                    imms=width-0w1, regN=regN, regD=regD}

        and arithmeticShiftRight{wordSize=WordSize64, shift, regN, regD} =
                signedBitfieldMove64{immr=shift, imms=0wx3f, regN=regN, regD=regD}
        |   arithmeticShiftRight{wordSize=WordSize32, shift, regN, regD} =
                signedBitfieldMove32{immr=shift, imms=0wx1f, regN=regN, regD=regD}

        and bitfieldInsert{wordSize=WordSize64, lsb, width, regN, regD} =
                bitfieldMove64{immr=Word.~ lsb mod 0w64, imms=width-0w1, regN=regN, regD=regD}
        |   bitfieldInsert{wordSize=WordSize32, lsb, width, regN, regD} =
                bitfieldMove32{immr=Word.~ lsb mod 0w32, imms=width-0w1, regN=regN, regD=regD}
    end

    local
        (* Logical immediates.  AND, OR, XOR and ANDS.  Assumes that the immediate value
           has already been checked as valid.  The non-flags versions can use SP as the
           destination. *)
        fun logicalImmediate (opc, xD) {wordSize, bits, regN, regD} =
        let
            val s = case wordSize of WordSize32 => 0w0 | WordSize64 => 0w1
            val {n, imms, immr} = 
                case encodeBitPattern(bits, wordSize) of
                    NONE => raise InternalError "testBitPattern: unable to encode bit pattern"
                |   SOME res => res
        in
            SimpleInstr(0wx12000000 orb (opc << 0w29) orb (s << 0w31) orb (n << 0w22) orb
                (immr << 0w16) orb (imms << 0w10) orb (word8ToWord(xRegOrXZ regN) << 0w5) orb
                word8ToWord(xD regD))
        end
    in
        val bitwiseAndImmediate = logicalImmediate (0w0, xRegOrXSP)
        and bitwiseOrImmediate = logicalImmediate (0w1, xRegOrXSP)
        and bitwiseXorImmediate = logicalImmediate (0w2, xRegOrXSP)
        and bitwiseAndSImmediate = logicalImmediate (0w3, xRegOrXZ)
        
        (* Test a bit pattern in a register.  If the pattern is within the low-order
           32-bits we use a 32-bit test. *)
        fun testBitPattern(reg, bits) =
        let
            val w = if bits <= 0wxffffffff then WordSize32 else WordSize64
        in
            bitwiseAndSImmediate({wordSize=w, bits=bits, regN=reg, regD=XZero})
        end
    end

    local
        (* Floating point operations - 2 source *)
        fun floatingPoint2Source (pt, opc) {regM, regN, regD} =
            SimpleInstr(0wx1E200800 orb (pt << 0w22) orb (word8ToWord(vReg regM) << 0w16) orb
                (opc << 0w12) orb (word8ToWord(vReg regN) << 0w5) orb word8ToWord(vReg regD))
    in
        val multiplyFloat = floatingPoint2Source(0w0, 0wx0)
        and divideFloat = floatingPoint2Source(0w0, 0wx1)
        and addFloat = floatingPoint2Source(0w0, 0wx2)
        and subtractFloat = floatingPoint2Source(0w0, 0wx3)
        and multiplyDouble = floatingPoint2Source(0w1, 0wx0)
        and divideDouble = floatingPoint2Source(0w1, 0wx1)
        and addDouble = floatingPoint2Source(0w1, 0wx2)
        and subtractDouble = floatingPoint2Source(0w1, 0wx3)
    end

    local
        (* Move between a floating point and a general register with or without conversion. *)
        fun fmoveGeneral (sf, s, ptype, mode, opcode, rN, rD) {regN, regD} =
            SimpleInstr(0wx1E200000 orb (sf << 0w31) orb (s << 0w29) orb (ptype << 0w22) orb
                (mode << 0w19) orb (opcode << 0w16) orb
                (word8ToWord(rN regN) << 0w5) orb word8ToWord(rD regD))
        open IEEEReal
    in
        (* Moves without conversion *)
        val moveGeneralToFloat = fmoveGeneral(0w0, 0w0, 0w0, 0w0, 0w7, xRegOrXZ, vReg)
        and moveFloatToGeneral = fmoveGeneral(0w0, 0w0, 0w0, 0w0, 0w6, vReg, xRegOnly)
        and moveGeneralToDouble = fmoveGeneral(0w1, 0w0, 0w1, 0w0, 0w7, xRegOrXZ, vReg)
        and moveDoubleToGeneral = fmoveGeneral(0w1, 0w0, 0w1, 0w0, 0w6, vReg, xRegOnly)
        (* Moves with conversion - signed.  The argument is a 64-bit value. *)
        and convertIntToFloat = fmoveGeneral(0w1, 0w0, 0w0, 0w0, 0w2, xRegOrXZ, vReg)
        and convertIntToDouble = fmoveGeneral(0w1, 0w0, 0w1, 0w0, 0w2, xRegOrXZ, vReg)

        fun convertFloatToInt TO_NEAREST =
                fmoveGeneral(0w1, 0w0, 0w0, 0w0, 0w4, vReg, xRegOnly) (* fcvtas *)
        |   convertFloatToInt TO_NEGINF =
                fmoveGeneral(0w1, 0w0, 0w0, 0w2, 0w0, vReg, xRegOnly) (* fcvtms *)
        |   convertFloatToInt TO_POSINF =
                fmoveGeneral(0w1, 0w0, 0w0, 0w1, 0w0, vReg, xRegOnly) (* fcvtps *)
        |   convertFloatToInt TO_ZERO =
                fmoveGeneral(0w1, 0w0, 0w0, 0w3, 0w0, vReg, xRegOnly) (* fcvtzs *)

        and convertDoubleToInt TO_NEAREST =
                fmoveGeneral(0w1, 0w0, 0w1, 0w0, 0w4, vReg, xRegOnly) (* fcvtas *)
        |   convertDoubleToInt TO_NEGINF =
                fmoveGeneral(0w1, 0w0, 0w1, 0w2, 0w0, vReg, xRegOnly) (* fcvtms *)
        |   convertDoubleToInt TO_POSINF =
                fmoveGeneral(0w1, 0w0, 0w1, 0w1, 0w0, vReg, xRegOnly) (* fcvtps *)
        |   convertDoubleToInt TO_ZERO =
                fmoveGeneral(0w1, 0w0, 0w1, 0w3, 0w0, vReg, xRegOnly) (* fcvtzs *)
    end

    local
        fun floatingPtCompare(ptype, opc) {regM, regN} =
            SimpleInstr(0wx1E202000 orb (ptype << 0w22) orb
                (word8ToWord(vReg regM) << 0w16) orb (word8ToWord(vReg regN) << 0w5) orb
                (opc << 0w3))
    in
        val compareFloat = floatingPtCompare(0w0, 0w0) (* fcmp *)
        and compareDouble = floatingPtCompare(0w1, 0w0)
        (* It is also possible to compare a single register with zero using opc=1/3 *)
    end

    local
        (* Floating point single source. *)
        fun floatingPtSingle (ptype, opc) {regN, regD} =
            SimpleInstr(0wx1E204000 orb (ptype << 0w22) orb (opc << 0w15) orb
                (word8ToWord(vReg regN) << 0w5) orb word8ToWord(vReg regD))
    in
        val moveFloatToFloat = floatingPtSingle(0w0, 0wx0)
        and absFloat = floatingPtSingle(0w0, 0wx1)
        and negFloat = floatingPtSingle(0w0, 0wx2)
        and convertFloatToDouble = floatingPtSingle(0w0, 0wx5)
        and moveDoubleToDouble = floatingPtSingle(0w1, 0wx0)
        and absDouble = floatingPtSingle(0w1, 0wx1)
        and negDouble = floatingPtSingle(0w1, 0wx2)
        and convertDoubleToFloat = floatingPtSingle(0w1, 0wx4)
    end

    (* This word is put in after a call to the RTS trap-handler.  All the registers
       are saved and restored across a call to the trap-handler; the register
       mask contains those that may contain an address and so need to be scanned and
       possibly updated if there is a GC. *)
    fun registerMask(regs) =
    let
        fun addToMask(r, mask) = mask orb (0w1 << word8ToWord(xRegOnly r))
        val maskWord = List.foldl addToMask 0w0 regs
    in
        SimpleInstr(0wx02000000 (* Reserved instr range. *) orb maskWord)
    end
    

    (* Size of each code word. *)
    fun codeSize (SimpleInstr _) = 1 (* Number of 32-bit words *)
    |   codeSize (LoadAddressLiteral _) = 1
    |   codeSize (LoadNonAddressLiteral _) = 1
    |   codeSize (Label _) = 0
    |   codeSize (UnconditionalBranch _) = 1
    |   codeSize (LoadLabelAddress _) = 1
    |   codeSize (ConditionalBranch { length=ref BrShort, ...}) = 1
    |   codeSize (ConditionalBranch { length=ref BrExtended, ...}) = 2
    |   codeSize (TestBitBranch { length=ref BrShort, ...}) = 1
    |   codeSize (TestBitBranch { length=ref BrExtended, ...}) = 2
    |   codeSize (CompareBranch { length=ref BrShort, ...}) = 1
    |   codeSize (CompareBranch { length=ref BrExtended, ...}) = 2

    (* Store a 32-bit value in the code *)
    fun writeInstr(value, wordAddr, seg) =
    let
        fun putBytes(value, a, seg, i) =
        if i = 0w4 then ()
        else
        (
            byteVecSet(seg, a+i, wordToWord8(value andb 0wxff));
            putBytes(value >> 0w8, a, seg, i+0w1)
        )
    in
        putBytes(value, wordAddr << 0w2, seg, 0w0)
    end
    
    (* Store a 64-bit constant in the code area. *)
    fun write64Bit(value, word64Addr, seg) =
    let
        fun putBytes(value, a, seg, i) =
        if i = 0w8 then ()
        else
        (
            byteVecSet(seg, a+i, Word8.fromLarge(Word64.toLarge value));
            putBytes(Word64.>>(value, 0w8), a, seg, i+0w1)
        )
    in
        putBytes(value, word64Addr << 0w3, seg, 0w0)
    end

    fun genCode(ops, addressConsts, nonAddressConsts) =
    let
        local
            (* First pass - set the labels. *)
            fun setLabels(Label(ref labs) :: ops, ic) = (List.app(fn d => d := ic) labs; setLabels(ops, ic))
            |   setLabels(oper :: ops, ic) = setLabels(ops, ic + Word.fromInt(codeSize oper))
            |   setLabels([], ic) = ic
        in
            val codeSize = setLabels(ops, 0w0) (* Number of 32-bit instructions *)
        end

        val wordsOfCode = (codeSize + 0w1) div 0w2 (* Round up to 64-bits *)
        val paddingWord = if Word.andb(codeSize, 0w1) = 0w1 then [SimpleInstr nopCode] else []
        
        val numNonAddrConsts = Word.fromInt(List.length nonAddressConsts)
        and numAddrConsts = Word.fromInt(List.length addressConsts)

        val segSize = wordsOfCode + numAddrConsts + numNonAddrConsts + 0w4 (* 4 extra words *)
        val codeVec = byteVecMake segSize
        
        fun testBit(bitNo, brNonZero, offset, reg) =
            0wx36000000 orb (if bitNo >= 0w32 then 0wx80000000 else 0w0) orb
                (if brNonZero then 0wx01000000 else 0w0) orb
                (word8ToWord(Word8.andb(bitNo, 0wx3f)) << 0w19) orb
                ((offset andb 0wx3fff) << 0w5) orb word8ToWord(xRegOnly reg)
        and compareBranch(size, brNonZero, offset, reg) =
            0wx34000000 orb (case size of WordSize64 => 0wx80000000 | WordSize32 => 0w0) orb
                (if brNonZero then 0wx01000000 else 0w0) orb
                ((offset andb 0wx7ffff) << 0w5) orb word8ToWord(xRegOnly reg)

        fun genCodeWords([], _ , _, _) = ()

        |   genCodeWords(SimpleInstr code :: tail, wordNo, aConstNum, nonAConstNum) =
            (
                writeInstr(code, wordNo, codeVec);
                genCodeWords(tail, wordNo+0w1, aConstNum, nonAConstNum)
            )

        |   genCodeWords(LoadAddressLiteral{reg, ...} :: tail, wordNo, aConstNum, nonAConstNum) =
            let
                (* The offset is in 32-bit words.  The first of the constants is
                   at offset wordsOfCode+3 *)
                val offsetOfConstant =
                    (wordsOfCode+numNonAddrConsts+0w3+aConstNum)*0w2 - wordNo
                val _ = offsetOfConstant < 0wx100000 orelse raise InternalError "Offset to constant is too large"
                val code = 0wx58000000 orb (offsetOfConstant << 0w5) orb word8ToWord(xRegOnly reg)
            in
                writeInstr(code, wordNo, codeVec);
                genCodeWords(tail, wordNo+0w1, aConstNum+0w1, nonAConstNum)
            end

        |   genCodeWords(LoadNonAddressLiteral{reg, ...} :: tail, wordNo, aConstNum, nonAConstNum) =
            let
                (* The offset is in 32-bit words. *)
                val offsetOfConstant = (wordsOfCode+nonAConstNum)*0w2 - wordNo
                val _ = offsetOfConstant < 0wx100000 orelse raise InternalError "Offset to constant is too large"
                val code = 0wx58000000 orb (offsetOfConstant << 0w5) orb word8ToWord(xRegOnly reg)
            in
                writeInstr(code, wordNo, codeVec);
                genCodeWords(tail, wordNo+0w1, aConstNum, nonAConstNum+0w1)
            end

        |   genCodeWords(Label _ :: tail, wordNo, aConstNum, nonAConstNum) = 
                genCodeWords(tail, wordNo, aConstNum, nonAConstNum) (* No code. *)

        |   genCodeWords(UnconditionalBranch(ref labs) :: tail, wordNo, aConstNum, nonAConstNum) =
            let
                val dest = !(hd labs)
                val offset = Word.toInt dest - Word.toInt wordNo
                val _ = (offset < Word.toInt(0w1 << 0w25) andalso offset >= ~ (Word.toInt(0w1 << 0w25)))
                    orelse raise InternalError "genCodeWords: branch too far";
            in
                writeInstr(0wx14000000 orb (Word.fromInt offset andb 0wx03ffffff), wordNo, codeVec);
                genCodeWords(tail, wordNo+0w1, aConstNum, nonAConstNum)
            end

        |   genCodeWords(ConditionalBranch{ label=ref labs, jumpCondition=CCode cond, length=ref BrShort }:: tail, wordNo,
                            aConstNum, nonAConstNum) =
            let
                val dest = !(hd labs)
                val offset = Word.toInt dest - Word.toInt wordNo
                val _ = (offset < Word.toInt(0w1 << 0w18) andalso offset >= ~ (Word.toInt(0w1 << 0w18)))
                        orelse raise InternalError "genCodeWords: branch too far"
            in
                writeInstr(0wx54000000 orb ((Word.fromInt offset andb 0wx07ffff) << 0w5)
                        orb word8ToWord cond, wordNo, codeVec);
                genCodeWords(tail, wordNo+0w1, aConstNum, nonAConstNum)
            end

        |   genCodeWords(ConditionalBranch{ label=ref labs, jumpCondition=CCode cond, length=ref BrExtended }:: tail, wordNo,
                            aConstNum, nonAConstNum) =
            let (* Long form - put a conditional branch with reversed sense round an unconditional branch. *)
                val dest = !(hd labs)
                val offset = Word.toInt dest - Word.toInt (wordNo + 0w1) (* Next instruction. *)
                val _ = (offset < Word.toInt(0w1 << 0w25) andalso offset >= ~ (Word.toInt(0w1 << 0w25)))
                    orelse raise InternalError "genCodeWords: branch too far"
                val revCond = Word8.xorb(cond, 0w1)
            in
                writeInstr(0wx54000000 orb (0w2 << 0w5) orb word8ToWord revCond, wordNo, codeVec);
                writeInstr(0wx14000000 orb (Word.fromInt offset andb 0wx03ffffff), wordNo+0w1, codeVec);
                genCodeWords(tail, wordNo+0w2, aConstNum, nonAConstNum)
            end

        |   genCodeWords(LoadLabelAddress{label=ref labs, reg} :: tail, wordNo, aConstNum, nonAConstNum) =
            let
                val dest = !(hd labs)
                val offset = Word.toInt dest - Word.toInt wordNo
                val _ = offset < 0x100000 orelse offset >= ~ 0x100000
                    orelse raise InternalError "Offset to label address is too large"
                val code = 0wx10000000 orb ((Word.fromInt offset andb 0wx7ffff) << 0w5) orb word8ToWord(xRegOnly reg)
            in
                writeInstr(code, wordNo, codeVec);
                genCodeWords(tail, wordNo+0w1, aConstNum, nonAConstNum)
            end

        |   genCodeWords(TestBitBranch{label=ref labs, bitNo, brNonZero, reg, length=ref BrExtended} :: tail,
                    wordNo, aConstNum, nonAConstNum) =
            let
                val dest = !(hd labs)
                val offset = Word.toInt dest - Word.toInt (wordNo + 0w1) (* Next instruction *)
                val _ = (offset < Word.toInt(0w1 << 0w25) andalso offset >= ~ (Word.toInt(0w1 << 0w25)))
                    orelse raise InternalError "genCodeWords: branch too far"
                val _ = bitNo <= 0w63 orelse
                    raise InternalError "TestBitBranch: bit number > 63"
                val code = testBit(bitNo, (* Invert test *) not brNonZero, 0w2 (* Skip branch *), reg)
            in
                writeInstr(code, wordNo, codeVec);
                writeInstr(0wx14000000 orb (Word.fromInt offset andb 0wx03ffffff), wordNo+0w1, codeVec);
                genCodeWords(tail, wordNo+0w2, aConstNum, nonAConstNum)
            end

        |   genCodeWords(TestBitBranch{label=ref labs, bitNo, brNonZero, reg, length=ref BrShort} :: tail,
                    wordNo, aConstNum, nonAConstNum) =
            let
                val dest = !(hd labs)
                val offset = Word.toInt dest - Word.toInt wordNo
                val _ = offset < 0x2000 orelse offset >= ~ 0x2000
                    orelse raise InternalError "TestBitBranch: Offset to label address is too large"
                val _ = bitNo <= 0w63 orelse
                    raise InternalError "TestBitBranch: bit number > 63"
                val code = testBit(bitNo, brNonZero, Word.fromInt offset, reg)
            in
                writeInstr(code, wordNo, codeVec);
                genCodeWords(tail, wordNo+0w1, aConstNum, nonAConstNum)
            end

        |   genCodeWords(CompareBranch{label=ref labs, brNonZero, size, reg, length=ref BrExtended} :: tail,
                    wordNo, aConstNum, nonAConstNum) =
            let
                val dest = !(hd labs)
                val offset = Word.toInt dest - Word.toInt (wordNo+0w1)
                val _ = (offset < Word.toInt(0w1 << 0w25) andalso offset >= ~ (Word.toInt(0w1 << 0w25)))
                    orelse raise InternalError "genCodeWords: branch too far"
                val code = compareBranch(size, (* Invert test *) not brNonZero, 0w2, reg)
            in
                writeInstr(code, wordNo, codeVec);
                writeInstr(0wx14000000 orb (Word.fromInt offset andb 0wx03ffffff), wordNo+0w1, codeVec);
                genCodeWords(tail, wordNo+0w2, aConstNum, nonAConstNum)
            end

        |   genCodeWords(CompareBranch{label=ref labs, brNonZero, size, reg, length=ref BrShort} :: tail,
                    wordNo, aConstNum, nonAConstNum) =
            let
                val dest = !(hd labs)
                val offset = Word.toInt dest - Word.toInt wordNo
                val _ = offset < 0x40000 orelse offset >= ~ 0x40000
                    orelse raise InternalError "CompareBranch: Offset to label address is too large"
                val code = compareBranch(size, brNonZero, Word.fromInt offset, reg)
            in
                writeInstr(code, wordNo, codeVec);
                genCodeWords(tail, wordNo+0w1, aConstNum, nonAConstNum)
            end
    in
        genCodeWords (ops @ paddingWord, 0w0, 0w0, 0w0);
        (* Copy in the non-address constants. *)
        List.foldl(fn (cVal, addr) => (write64Bit(cVal, addr, codeVec); addr+0w1)) wordsOfCode nonAddressConsts;
        (codeVec (* Return the completed code. *), wordsOfCode+numNonAddrConsts (* And the size in 64-bit words. *))
    end

    (* Store a 64-bit value in the code *)
    fun set64(value, wordNo, seg) =
    let
        val addrs = wordNo * 0w8
        fun putBytes(value, a, seg, i) =
        if i = 0w8 then ()
        else
        (
            byteVecSet(seg, a+i, Word8.fromInt(value mod 256));
            putBytes(value div 256, a, seg, i+0w1)
        )
    in
        putBytes(value, addrs, seg, 0w0)
    end
    
   
    (* Print the instructions in the code. *)
    fun printCode (codeVec, functionName, wordsOfCode, printStream) =
    let
        val numInstructions = wordsOfCode * 0w2 (* Words is number of 64-bit words *)
    
        fun printHex (v, n) =
        let
            val s = Word.fmt StringCvt.HEX v
            val pad = CharVector.tabulate(Int.max(0, n-size s), fn _ => #"0")
        in
            printStream pad; printStream s
        end

        fun printCondition 0wx0 = printStream "eq"
        |   printCondition 0wx1 = printStream "ne"
        |   printCondition 0wx2 = printStream "cs"
        |   printCondition 0wx3 = printStream "cc"
        |   printCondition 0wx4 = printStream "mi"
        |   printCondition 0wx5 = printStream "pl"
        |   printCondition 0wx6 = printStream "vs"
        |   printCondition 0wx7 = printStream "vc"
        |   printCondition 0wx8 = printStream "hi"
        |   printCondition 0wx9 = printStream "ls"
        |   printCondition 0wxa = printStream "ge"
        |   printCondition 0wxb = printStream "lt"
        |   printCondition 0wxc = printStream "gt"
        |   printCondition 0wxd = printStream "le"
        |   printCondition 0wxe = printStream "al"
        |   printCondition _    = printStream "nv"

        (* Normal XReg with 31 being XZ *)
        fun prXReg 0w31 = printStream "xz"
        |   prXReg r = printStream("x" ^ Word.fmt StringCvt.DEC r)

        (* XReg when 31 is SP *)
        fun prXRegOrSP 0w31 = printStream "sp"
        |   prXRegOrSP r = printStream("x" ^ Word.fmt StringCvt.DEC r)

        (* Normal WReg with 31 being WZ *)
        fun prWReg 0w31 = printStream "wz"
        |   prWReg r = printStream("w" ^ Word.fmt StringCvt.DEC r)

        (* WReg when 31 is WSP *)
        fun prWRegOrSP 0w31 = printStream "wsp"
        |   prWRegOrSP r = printStream("w" ^ Word.fmt StringCvt.DEC r)

        (* Each instruction is 32-bytes. *)
        fun printWordAt wordNo =
        let
            val byteNo = wordNo << 0w2
            val () = printHex(byteNo, 6)  (* Address *)
            val () = printStream "\t"
            val wordValue =
                word8ToWord (codeVecGet (codeVec, byteNo)) orb
                (word8ToWord (codeVecGet (codeVec, byteNo+0w1)) << 0w8) orb
                (word8ToWord (codeVecGet (codeVec, byteNo+0w2)) << 0w16) orb
                (word8ToWord (codeVecGet (codeVec, byteNo+0w3)) << 0w24)
            val () = printHex(wordValue, 8) (* Instr as hex *)
            val () = printStream "\t"
        in
            if (wordValue andb 0wxfffffc1f) = 0wxD61F0000
            then
            let
                val rN = (wordValue andb 0wx3e0) >> 0w5
            in
                printStream "br\tx";
                printStream(Word.fmt StringCvt.DEC rN)
            end

            else if (wordValue andb 0wxfffffc1f) = 0wxD63F0000
            then
            let
                val rN = (wordValue andb 0wx3e0) >> 0w5
            in
                printStream "blr\tx";
                printStream(Word.fmt StringCvt.DEC rN)
            end

            else if (wordValue andb 0wxfffffc1f) = 0wxD65F0000
            then
            let
                val rN = (wordValue andb 0wx3e0) >> 0w5
            in
                printStream "ret\tx";
                printStream(Word.fmt StringCvt.DEC rN)
            end

            else if wordValue = 0wxD503201F
            then printStream "nop"
            else if wordValue = 0wxD503203F
            then printStream "yield"
            else if wordValue = 0wxD5033BBF
            then printStream "dmb\tish"

            else if (wordValue andb 0wx1f800000) = 0wx12800000
            then (* Move of constants.  Includes movn and movk. *)
            let
                val rD = wordValue andb 0wx1f
                val imm16 = Word.toInt((wordValue >> 0w5) andb 0wxffff)
                val isXReg = (wordValue andb 0wx80000000) <> 0w0
                val opc = (wordValue >> 0w29) andb 0w3
                val shift = (wordValue >> 0w21) andb 0w3
            in
                printStream (if opc = 0w3 then "movk\t" else "mov\t");
                printStream (if isXReg then "x" else "w");
                printStream(Word.fmt StringCvt.DEC rD);
                printStream ",#";
                printStream(Int.toString(if opc = 0w0 then ~1 - imm16 else imm16));
                if shift = 0w0
                then ()
                else (printStream ",lsl #"; printStream(Word.fmt StringCvt.DEC (shift*0w16)))
            end

            else if (wordValue andb 0wx3b000000) = 0wx39000000
            then (* Load/Store with unsigned, scaled offset. *)
            let
                (* The offset is in units of the size of the operand. *)
                val size = wordValue >> 0w30
                and v = (wordValue >> 0w26) andb 0w1
                and opc = (wordValue >> 0w22) andb 0w3
                val rT = wordValue andb 0wx1f
                and rN = (wordValue andb 0wx3e0) >> 0w5
                and imm12 = (wordValue andb 0wx3ffc00) >> 0w10
                val (opcode, r, scale) =
                    case (size, v, opc) of
                        (0w0, 0w0, 0w0) => ("strb", "w", 0w0)
                    |   (0w0, 0w0, 0w1) => ("ldrb", "w", 0w0)
                    |   (0w1, 0w0, 0w0) => ("strh", "w", 0w2)
                    |   (0w1, 0w0, 0w1) => ("ldrh", "w", 0w2)
                    |   (0w2, 0w0, 0w0) => ("str", "w", 0w4)
                    |   (0w2, 0w0, 0w1) => ("ldr", "w", 0w4)
                    |   (0w3, 0w0, 0w0) => ("str", "x", 0w8)
                    |   (0w3, 0w0, 0w1) => ("ldr", "x", 0w8)
                    |   (0w2, 0w1, 0w0) => ("str", "s", 0w4)
                    |   (0w2, 0w1, 0w1) => ("ldr", "s", 0w4)
                    |   (0w3, 0w1, 0w0) => ("str", "d", 0w8)
                    |   (0w3, 0w1, 0w1) => ("ldr", "d", 0w8)
                    |   _ => ("??", "?", 0w1)
            in
                printStream opcode; printStream "\t"; printStream r; printStream(Word.fmt StringCvt.DEC rT);
                printStream ",[x"; printStream(Word.fmt StringCvt.DEC rN);
                printStream ",#"; printStream(Word.fmt StringCvt.DEC(imm12*scale));
                printStream "]"
            end

            else if (wordValue andb 0wx3b200c00) = 0wx38000000
            then (* Load/store unscaled immediate *)
            let
                val size = wordValue >> 0w30
                and v = (wordValue >> 0w26) andb 0w1
                and opc = (wordValue >> 0w22) andb 0w3
                val rT = wordValue andb 0wx1f
                and rN = (wordValue andb 0wx3e0) >> 0w5
                and imm9 = (wordValue andb 0wx1ff000) >> 0w12
                val imm9Text =
                    if imm9 > 0wxff
                    then "-" ^ Word.fmt StringCvt.DEC (0wx200 - imm9)
                    else Word.fmt StringCvt.DEC imm9
                val (opcode, r) =
                    case (size, v, opc) of
                        (0w0, 0w0, 0w0) => ("strub", "w")
                    |   (0w0, 0w0, 0w1) => ("ldrub", "w")
                    |   (0w1, 0w0, 0w0) => ("struh", "w")
                    |   (0w1, 0w0, 0w1) => ("ldruh", "w")
                    |   (0w2, 0w0, 0w0) => ("stur", "w")
                    |   (0w2, 0w0, 0w1) => ("ldur", "w")
                    |   (0w3, 0w0, 0w0) => ("stur", "x")
                    |   (0w3, 0w0, 0w1) => ("ldur", "x")
                    |   (0w2, 0w1, 0w0) => ("stur", "s")
                    |   (0w2, 0w1, 0w1) => ("ldur", "s")
                    |   (0w3, 0w1, 0w0) => ("stur", "d")
                    |   (0w3, 0w1, 0w1) => ("ldur", "d")
                    |   _ => ("???", "?")
            in
                printStream opcode; printStream "\t"; printStream r;
                printStream(Word.fmt StringCvt.DEC rT);
                printStream ",[x"; printStream(Word.fmt StringCvt.DEC rN);
                printStream ",#"; printStream imm9Text; printStream "]"
            end

            else if (wordValue andb 0wx3b200c00) = 0wx38000400
            then (* Load/store immediate post-indexed *)
            let
                val size = wordValue >> 0w30
                and v = (wordValue >> 0w26) andb 0w1
                and opc = (wordValue >> 0w22) andb 0w3
                val rT = wordValue andb 0wx1f
                and rN = (wordValue andb 0wx3e0) >> 0w5
                and imm9 = (wordValue andb 0wx1ff000) >> 0w12
                val imm9Text =
                    if imm9 > 0wxff
                    then "-" ^ Word.fmt StringCvt.DEC (0wx200 - imm9)
                    else Word.fmt StringCvt.DEC imm9
                val (opcode, r) =
                    case (size, v, opc) of
                        (0w0, 0w0, 0w0) => ("strb", "w")
                    |   (0w0, 0w0, 0w1) => ("ldrb", "w")
                    |   (0w3, 0w0, 0w0) => ("str", "x")
                    |   (0w3, 0w0, 0w1) => ("ldr", "x")
                    |   _ => ("???", "?")
            in
                printStream opcode; printStream "\t"; printStream r;
                printStream(Word.fmt StringCvt.DEC rT);
                printStream ",[x"; printStream(Word.fmt StringCvt.DEC rN);
                printStream "],#"; printStream imm9Text
            end

            else if (wordValue andb 0wx3b200c00) = 0wx38000c00
            then (* Load/store immediate pre-indexed *)
            let
                val size = wordValue >> 0w30
                and v = (wordValue >> 0w26) andb 0w1
                and opc = (wordValue >> 0w22) andb 0w3
                val rT = wordValue andb 0wx1f
                and rN = (wordValue andb 0wx3e0) >> 0w5
                and imm9 = (wordValue andb 0wx1ff000) >> 0w12
                val imm9Text =
                    if imm9 > 0wxff
                    then "-" ^ Word.fmt StringCvt.DEC (0wx200 - imm9)
                    else Word.fmt StringCvt.DEC imm9
                val (opcode, r) =
                    case (size, v, opc) of
                        (0w0, 0w0, 0w0) => ("strb", "w")
                    |   (0w0, 0w0, 0w1) => ("ldrb", "w")
                    |   (0w3, 0w0, 0w0) => ("str", "x")
                    |   (0w3, 0w0, 0w1) => ("ldr", "x")
                    |   _ => ("???", "?")
            in
                printStream opcode; printStream "\t"; printStream r;
                printStream(Word.fmt StringCvt.DEC rT);
                printStream ",[x"; printStream(Word.fmt StringCvt.DEC rN);
                printStream ",#"; printStream imm9Text; printStream "]!"
            end

            else if (wordValue andb 0wx3b200c00) = 0wx38200800
            then (* Load/store with register offset i.e. an index register. *)
            let
                val size = wordValue >> 0w30
                and v = (wordValue >> 0w26) andb 0w1
                and opc = (wordValue >> 0w22) andb 0w3
                val rT = wordValue andb 0wx1f
                and rN = (wordValue >> 0w5) andb 0wx1f
                and rM = (wordValue >> 0w16) andb 0wx1f
                val option = (wordValue >> 0w13) andb 0w7
                val s = (wordValue andb 0wx1000) <> 0w0 
                val (opcode, r) =
                    case (size, v, opc) of
                        (0w0, 0w0, 0w0) => ("strb", "w")
                    |   (0w0, 0w0, 0w1) => ("ldrb", "w")
                    |   (0w1, 0w0, 0w0) => ("strh", "w")
                    |   (0w1, 0w0, 0w1) => ("ldrh", "w")
                    |   (0w2, 0w0, 0w0) => ("str", "w")
                    |   (0w2, 0w0, 0w1) => ("ldr", "w")
                    |   (0w3, 0w0, 0w0) => ("str", "x")
                    |   (0w3, 0w0, 0w1) => ("ldr", "x")
                    |   (0w2, 0w1, 0w0) => ("str", "s")
                    |   (0w2, 0w1, 0w1) => ("ldr", "s")
                    |   (0w3, 0w1, 0w0) => ("str", "d")
                    |   (0w3, 0w1, 0w1) => ("ldr", "d")
                    |   _ => ("???", "?")
                val (extend, xr) =
                    case option of
                        0w2 => (" uxtw", "w")
                    |   0w3 => if s then (" lsl", "x") else ("", "x")
                    |   0w6 => (" sxtw", "w")
                    |   0w7 => (" sxtx", "x")
                    |   _   => ("?", "?")
                val indexShift =
                    case (size, s) of
                        (0w0, true) => " #1"
                    |   (0w1, true) => " #1"
                    |   (0w2, true) => " #2"
                    |   (0w3, true) => " #3"
                    |   _ => ""
            in
                printStream opcode; printStream "\t"; printStream r;
                printStream(Word.fmt StringCvt.DEC rT);
                printStream ",[x"; printStream(Word.fmt StringCvt.DEC rN);
                printStream ","; printStream xr; printStream(Word.fmt StringCvt.DEC rM);
                printStream extend; printStream indexShift;
                printStream "]"
            end

            else if (wordValue andb 0wxbf800000) = 0wx91000000
            then
            let
                (* Add/Subtract a 12-bit immediate with possible shift. *)
                val rD = wordValue andb 0wx1f
                and rN = (wordValue andb 0wx3e0) >> 0w5
                and imm12 = (wordValue andb 0wx3ffc00) >> 0w10
                and shiftBit = wordValue andb 0wx400000
                val imm = if shiftBit <> 0w0 then imm12 << 0w12 else imm12
                val opr = if (wordValue andb 0wx40000000) = 0w0 then "add" else "sub"
            in
                printStream opr; printStream "\t"; prXRegOrSP rD;
                printStream ","; prXRegOrSP rN;
                printStream ",#"; printStream(Word.fmt StringCvt.DEC imm)
            end

            else if (wordValue andb 0wxff800000) = 0wxF1000000
            then
            let
                (* Subtract a 12-bit immediate with possible shift, setting flags. *)
                val rD = wordValue andb 0wx1f
                and rN = (wordValue andb 0wx3e0) >> 0w5
                and imm12 = (wordValue andb 0wx3ffc00) >> 0w10
                and shiftBit = wordValue andb 0wx400000
                val imm = if shiftBit <> 0w0 then imm12 << 0w12 else imm12
            in
                if rD = 0w31
                then printStream "cmp\t"
                else (printStream "subs\t"; prXReg rD; printStream ",");
                prXRegOrSP rN; printStream ",#"; printStream(Word.fmt StringCvt.DEC imm)
            end

            else if (wordValue andb 0wx7fe0ffe0) = 0wx2A0003E0
            then (* Move reg,reg.  This is a subset of ORR shifted register. *)
            let
                val reg = if (wordValue andb 0wx80000000) <> 0w0 then "x" else "w"
            in
                printStream "mov\t"; printStream reg;
                printStream(Word.fmt StringCvt.DEC(wordValue andb 0wx1f));
                printStream ","; printStream reg;
                printStream(Word.fmt StringCvt.DEC((wordValue >> 0w16) andb 0wx1f))
            end

            else if (wordValue andb 0wx1f000000) = 0wx0A000000
            then
            let
                (* Logical operations with shifted register. *)
                val rD = wordValue andb 0wx1f
                and rN = (wordValue >> 0w5) andb 0wx1f
                and rM = (wordValue >> 0w16) andb 0wx1f
                and imm6 = (wordValue >> 0w10) andb 0wx3f
                and shiftCode = (wordValue >> 0w22) andb 0wx3
                val opc = (wordValue >> 0w29) andb 0wx3
                val nBit = (wordValue >> 0w21) andb 0w1
                val reg = if (wordValue andb 0wx80000000) <> 0w0 then "x" else "w"
                val opcode =
                    case (opc, nBit) of
                        (0w0, 0w0) => "and"
                    |   (0w1, 0w0) => "orr"
                    |   (0w2, 0w0) => "eor"
                    |   (0w3, 0w0) => "ands"
                    |   _ => "??"
            in
                printStream opcode; printStream"\t";
                printStream reg;
                printStream(Word.fmt StringCvt.DEC rD); printStream ",";
                printStream reg; printStream(Word.fmt StringCvt.DEC rN);
                printStream ","; printStream reg; printStream(Word.fmt StringCvt.DEC rM);
                if imm6 <> 0w0
                then
                (
                    case shiftCode of
                        0w0 => printStream ",lsl #"
                    |   0w1 => printStream ",lsr #"
                    |   0w2 => printStream ",asr #"
                    |   _ => printStream ",?? #";
                    printStream(Word.fmt StringCvt.DEC imm6)
                )
                else ()
            end

            else if (wordValue andb 0wx1f200000) = 0wx0B000000
            then
            let
                (* Add/subtract shifted register. *)
                val rD = wordValue andb 0wx1f
                and rN = (wordValue >> 0w5) andb 0wx1f
                and rM = (wordValue >> 0w16) andb 0wx1f
                and imm6 = (wordValue >> 0w10) andb 0wx3f
                and shiftCode = (wordValue >> 0w22) andb 0wx3
                val oper = (wordValue andb 0wx40000000) = 0w0
                val isS = (wordValue andb 0wx20000000) <> 0w0
                val pReg = if (wordValue andb 0wx80000000) <> 0w0 then prXReg else prWReg
            in
                if isS andalso rD = 0w31
                then printStream(if oper then "cmn\t" else "cmp\t")
                else
                (
                    printStream(if oper then "add" else "sub"); printStream(if isS then "s\t" else "\t");
                    pReg rD; printStream ","
                );
                pReg rN;
                printStream ","; pReg rM;
                if imm6 <> 0w0
                then
                (
                    case shiftCode of
                        0w0 => printStream ",lsl #"
                    |   0w1 => printStream ",lsr #"
                    |   0w2 => printStream ",asr #"
                    |   _ => printStream ",?? #";
                    printStream(Word.fmt StringCvt.DEC imm6)
                )
                else ()
            end

            else if (wordValue andb 0wx1fe00000) = 0wx0b200000
            then
            let
                (* Add/subtract extended register. *)
                val rD = wordValue andb 0wx1f
                and rN = (wordValue >> 0w5) andb 0wx1f
                and rM = (wordValue >> 0w16) andb 0wx1f
                and extend = (wordValue >> 0w13) andb 0w7
                and amount = (wordValue >> 0w10) andb 0w7
                and sf = (wordValue >> 0w31) andb 0w1
                and p = (wordValue >> 0w30) andb 0w1
                and s = (wordValue >> 0w29) andb 0w1
            in
                if s = 0w1 andalso rD = 0w31
                then printStream(if p = 0w0 then "cmn\t" else "cmp\t")
                else
                (
                    printStream(if p = 0w0 then "add" else "sub");
                    printStream(if s = 0w1 then "s\t" else "\t");
                    (if sf = 0w1 then prXRegOrSP else prWRegOrSP) rD; printStream ","
                );
                (if sf = 0w1 then prXRegOrSP else prWRegOrSP) rN;
                printStream ",";
                (if extend = 0w3 orelse extend = 0w7 then prXReg else prWReg) rM;
                case extend of
                    0w0 => printStream ",uxtb"
                |   0w1 => printStream ",uxth"
                |   0w2 => if amount = 0w0 andalso sf = 0w0 then () else printStream ",uxtw"
                |   0w3 => if amount = 0w0 andalso sf = 0w1 then () else printStream ",uxtx"
                |   0w4 => printStream ",sxtb"
                |   0w5 => printStream ",sxth"
                |   0w6 => printStream ",sxtw"
                |   0w7 => printStream ",sxtx"
                |   _ => printStream "?";
               
                if amount <> 0w0
                then printStream(" #" ^ Word.fmt StringCvt.DEC amount)
                else ()
            end

            else if (wordValue andb 0wxff000000) = 0wx58000000
            then
            let
                (* Load from a PC-relative address.  This may refer to the
                   address constant area or the non-address constant area. *)
                val rT = wordValue andb 0wx1f
                (* The offset is in 32-bit words *)
                val byteAddr = ((wordValue andb 0wx00ffffe0) >> (0w5-0w2)) + byteNo
                val word64Addr = byteAddr >> 0w3
                (* We must NOT use codeVecGetWord if this is in the non-address
                   area.  It may well not be a tagged value. *)
                local
                    fun getConstant(cVal, 0w0) = cVal
                    |   getConstant(cVal, offset) =
                        let
                            val byteVal =
                                Word64.fromLarge(Word8.toLarge(codeVecGet (codeVec, byteAddr+offset-0w1)))
                        in
                            getConstant(Word64.orb(Word64.<<(cVal, 0w8), byteVal), offset-0w1)
                        end
                in
                    val constantValue =
                        if word64Addr <= wordsOfCode
                        then "0x" ^ Word64.toString(getConstant(0w0, 0w8)) (* It's a non-address constant *)
                        else stringOfWord(codeVecGetWord(codeVec, word64Addr))
                end
            in
                printStream "ldr\tx"; printStream(Word.fmt StringCvt.DEC rT);
                printStream ",0x"; printStream(Word.fmt StringCvt.HEX byteAddr);
                printStream "\t// "; printStream constantValue
            end

            else if (wordValue andb 0wxbf000000) = 0wx10000000
            then
            let
                (* Put a pc-relative address into a register. *)
                val rT = wordValue andb 0wx1f
                val byteOffset =
                    ((wordValue andb 0wx00ffffe0) << (Word.fromInt Word.wordSize - 0w23) ~>>
                        (Word.fromInt Word.wordSize - 0w20)) + ((wordValue >> 0w29) andb 0w3)
            in
                printStream "adr\tx"; printStream(Word.fmt StringCvt.DEC rT);
                printStream ",0x"; printStream(Word.fmt StringCvt.HEX (byteNo+byteOffset))
            end

            else if (wordValue andb 0wxfc000000) = 0wx14000000
            then (* Unconditional branch. *)
            let
                (* The offset is signed and the destination may be earlier. *)
                val byteOffset =
                    (wordValue andb 0wx03ffffff) << (Word.fromInt Word.wordSize - 0w26) ~>>
                        (Word.fromInt Word.wordSize - 0w28)
            in
                printStream "b\t0x";
                printStream(Word.fmt StringCvt.HEX (byteNo+byteOffset))
            end

            else if (wordValue andb 0wxff000000) = 0wx54000000
            then (* Conditional branch *)
            let
                val byteOffset =
                    (wordValue andb 0wx00ffffe0) << (Word.fromInt Word.wordSize - 0w24) ~>>
                        (Word.fromInt Word.wordSize - 0w21)
            in
                printStream "b.";
                printCondition(wordValue andb 0wxf);
                printStream "\t0x";
                printStream(Word.fmt StringCvt.HEX (byteNo+byteOffset))
            end

            else if (wordValue andb 0wx7e000000) = 0wx34000000
            then (* Compare and branch *)
            let
                val byteOffset =
                    (wordValue andb 0wx00ffffe0) << (Word.fromInt Word.wordSize - 0w24) ~>>
                        (Word.fromInt Word.wordSize - 0w21)
                val oper =
                    if (wordValue andb 0wx01000000) = 0w0
                    then "cbz" else "cbnz"
                val r = if (wordValue andb 0wx80000000) = 0w0 then "w" else "x"
            in
                printStream oper; printStream "\t";
                printStream r; printStream(Word.fmt StringCvt.DEC (wordValue andb 0wx1f));
                printStream ",0x";
                printStream(Word.fmt StringCvt.HEX (byteNo+byteOffset))
            end

            else if (wordValue andb 0wx7e000000) = 0wx36000000
            then (* Test bit and branch *)
            let
                val byteOffset =
                    (wordValue andb 0wx00ffffe0) << (Word.fromInt Word.wordSize - 0w19) ~>>
                        (Word.fromInt Word.wordSize - 0w16)
                val oper =
                    if (wordValue andb 0wx01000000) = 0w0
                    then "tbz" else "tbnz"
                val b40 = (wordValue >> 0w19) andb 0wx1f
                val bitNo = b40 orb ((wordValue >> 0w26) andb 0wx20)
                val r = if bitNo < 0w32 then "w" else "x"
            in
                printStream oper; printStream "\t";
                printStream r; printStream(Word.fmt StringCvt.DEC (wordValue andb 0wx1f));
                printStream ",#"; printStream(Word.fmt StringCvt.DEC bitNo); printStream ",0x";
                printStream(Word.fmt StringCvt.HEX (byteNo+byteOffset))
            end

            else if (wordValue andb 0wx3fe00000) = 0wx1A800000
            then
            let
                val sf = wordValue >> 0w31
                val opc = (wordValue >> 0w30) andb 0w1
                val op2 = (wordValue >> 0w10) andb 0w3
                val rT = wordValue andb 0wx1f
                val rN = (wordValue >> 0w5) andb 0wx1f
                val rM = (wordValue >> 0w16) andb 0wx1f
                val cond = (wordValue >> 0w12) andb 0wxf
                val opcode =
                    case (opc, op2) of
                        (0w0, 0w0) => "csel"
                    |   (0w0, 0w1) => "csinc"
                    |   (0w1, 0w0) => "csinv"
                    |   (0w1, 0w1) => "csneg"
                    |   _ => "??"
                val r = if sf = 0w0 then "w" else "x"
            in
                printStream opcode; printStream "\t";
                printStream r; printStream(Word.fmt StringCvt.DEC rT);
                printStream ","; printStream r; printStream(Word.fmt StringCvt.DEC rN);
                printStream ","; printStream r; printStream(Word.fmt StringCvt.DEC rM);
                printStream ","; printCondition cond
            end

            else if (wordValue andb 0wx7f800000) = 0wx13000000
            then (* signed bitfield *)
            let
                val sf = wordValue >> 0w31
                (* N is always the same as sf. *)
                (*val nBit = (wordValue >> 0w22) andb 0w1*)
                val immr = (wordValue >> 0w16) andb 0wx3f
                val imms = (wordValue >> 0w10) andb 0wx3f
                val rN = (wordValue >> 0w5) andb 0wx1f
                val rD = wordValue andb 0wx1f
                val (r, wordSize) = if sf = 0w0 then ("w", 0w32) else ("x", 0w64)
            in
                if imms = wordSize - 0w1
                then printStream "asr\t"
                else printStream "sbfm\t";
                printStream r;
                printStream(Word.fmt StringCvt.DEC rD);
                printStream ",";
                printStream r;
                printStream(Word.fmt StringCvt.DEC rN);
                if imms = wordSize - 0w1
                then (printStream ",#0x"; printStream(Word.toString immr))
                else
                (
                    printStream ",#0x"; printStream(Word.toString immr);
                    printStream ",#0x"; printStream(Word.toString imms)
                )
            end

            else if (wordValue andb 0wx7f800000) = 0wx53000000
            then (* unsigned bitfield move *)
            let
                val sf = wordValue >> 0w31
                (* N is always the same as sf. *)
                (*val nBit = (wordValue >> 0w22) andb 0w1*)
                val immr = (wordValue >> 0w16) andb 0wx3f
                val imms = (wordValue >> 0w10) andb 0wx3f
                val rN = (wordValue >> 0w5) andb 0wx1f
                val rD = wordValue andb 0wx1f
                val (r, wordSize) = if sf = 0w0 then ("w", 0w32) else ("x", 0w64)
            in
                if imms + 0w1 = immr
                then printStream "lsl\t"
                else if imms = wordSize - 0w1
                then printStream "lsr\t"
                else printStream "ubfm\t";
                printStream r;
                printStream(Word.fmt StringCvt.DEC rD);
                printStream ",";
                printStream r;
                printStream(Word.fmt StringCvt.DEC rN);
                if imms + 0w1 = immr
                then (printStream ",#0x"; printStream(Word.toString(wordSize - immr)))
                else if imms = wordSize - 0w1
                then (printStream ",#0x"; printStream(Word.toString immr))
                else
                (
                    printStream ",#0x"; printStream(Word.toString immr);
                    printStream ",#0x"; printStream(Word.toString imms)
                )
            end

            else if (wordValue andb 0wx1f800000) = 0wx12000000
            then (* logical immediate *)
            let
                val sf = wordValue >> 0w31
                val opc = (wordValue >> 0w29) andb 0w3
                val nBit = (wordValue >> 0w22) andb 0w1
                val immr = (wordValue >> 0w16) andb 0wx3f
                val imms = (wordValue >> 0w10) andb 0wx3f
                val rN = (wordValue >> 0w5) andb 0wx1f
                val rD = wordValue andb 0wx1f
                val (opcode, r) =
                    case (sf, opc, nBit) of
                        (0w0, 0w0, 0w0) => ("and", "w")
                    |   (0w0, 0w1, 0w0) => ("orr", "w")
                    |   (0w0, 0w2, 0w0) => ("eor", "w")
                    |   (0w0, 0w3, 0w0) => ("ands", "w")
                    |   (0w1, 0w0, _) => ("and", "x")
                    |   (0w1, 0w1, _) => ("orr", "x")
                    |   (0w1, 0w2, _) => ("eor", "x")
                    |   (0w1, 0w3, _) => ("ands", "x")
                    |   _ => ("??", "?")
            in
                printStream opcode;
                printStream "\t";
                printStream r; printStream(Word.fmt StringCvt.DEC rD); printStream ",";
                printStream r; printStream(Word.fmt StringCvt.DEC rN); printStream ",#0x";
                printStream(Word64.toString(decodeBitPattern{sf=sf, n=nBit, immr=immr, imms=imms}))
            end

            else if (wordValue andb 0wx5fe00000) = 0wx1ac00000
            then (* Two source operations - shifts and divide. *)
            let
                val sf = wordValue >> 0w31
                val s = (wordValue >> 0w29) andb 0w1
                val rM = (wordValue >> 0w16) andb 0wx1f
                val opcode = (wordValue >> 0w10) andb 0wx3f
                val rN = (wordValue >> 0w5) andb 0wx1f
                val rD = wordValue andb 0wx1f
                val (oper, r) =
                    case (sf, s, opcode) of
                        (0w1, 0w0, 0wx2) => ("udiv", "x")
                    |   (0w1, 0w0, 0wx3) => ("sdiv", "x")
                    |   (0w1, 0w0, 0wx8) => ("lsl", "x")
                    |   (0w1, 0w0, 0wx9) => ("lsr", "x")
                    |   (0w1, 0w0, 0wxa) => ("asr", "x")
                    |   _ => ("??", "?")
            in
                printStream oper;
                printStream "\t";
                printStream r; printStream(Word.fmt StringCvt.DEC rD); printStream ",";
                printStream r; printStream(Word.fmt StringCvt.DEC rN); printStream ",";
                printStream r; printStream(Word.fmt StringCvt.DEC rM)
            end

            else if (wordValue andb 0wx1f000000) = 0wx1b000000
            then (* Three source operations - multiply add/subtract. *)
            let
                val sf = wordValue >> 0w31
                val op54 = (wordValue >> 0w29) andb 0w3
                val op31 = (wordValue >> 0w21) andb 0w7
                val o0 = (wordValue >> 0w15) andb 0w1
                val rM = (wordValue >> 0w16) andb 0wx1f
                val rA = (wordValue >> 0w10) andb 0wx1f
                val rN = (wordValue >> 0w5) andb 0wx1f
                val rD = wordValue andb 0wx1f
                val (oper, r) =
                    case (sf, op54, op31, o0, rA) of
                        (0w1, 0w0, 0w0, 0w0, 0w31) => ("mul", "x")
                    |   (0w1, 0w0, 0w0, 0w0, _)    => ("madd", "x")
                    |   (0w1, 0w0, 0w0, 0w1, 0w31) => ("mneg", "x")
                    |   (0w1, 0w0, 0w0, 0w1, _)    => ("msub", "x")
                    |   (0w1, 0w0, 0w2, 0w0, 0w31) => ("smulh", "x")
                    |   _ => ("??", "?")
            in
                printStream oper;
                printStream "\t";
                printStream r; printStream(Word.fmt StringCvt.DEC rD); printStream ",";
                printStream r; printStream(Word.fmt StringCvt.DEC rN); printStream ",";
                printStream r; printStream(Word.fmt StringCvt.DEC rM);
                if rA = 0w31 then ()
                else (printStream ","; printStream r; printStream(Word.fmt StringCvt.DEC rA))
            end

            else if (wordValue andb 0wxfffffc00) = 0wxC85FFC00
            then
            let
                val rN = (wordValue >> 0w5) andb 0wx1f
                val rT = wordValue andb 0wx1f
            in
                printStream "ldaxr\tx"; printStream(Word.fmt StringCvt.DEC rT);
                printStream ".[x"; printStream(Word.fmt StringCvt.DEC rN); printStream "]"
            end

            else if (wordValue andb 0wxffe0fc00) = 0wxC800FC00
            then
            let
                val rS = (wordValue >> 0w16) andb 0wx1f
                val rN = (wordValue >> 0w5) andb 0wx1f
                val rT = wordValue andb 0wx1f
            in
                printStream "stlxr\tw"; printStream(Word.fmt StringCvt.DEC rS);
                printStream ",x"; printStream(Word.fmt StringCvt.DEC rT);
                printStream ".[x"; printStream(Word.fmt StringCvt.DEC rN); printStream "]"
            end
            
            else if (wordValue andb 0wx7f20fc00) = 0wx1E200000
            then (* Moves between floating point and general regs. *)
            let
                val sf = (wordValue >> 0w31) andb 0w1
                and s = (wordValue >> 0w29) andb 0w1
                and ptype = (wordValue >> 0w22) andb 0w3
                and mode = (wordValue >> 0w19) andb 0w3
                and opcode = (wordValue >> 0w16) andb 0w7
                and rN = (wordValue >> 0w5) andb 0wx1f
                and rD = wordValue andb 0wx1f
                val (opc, dr, nr) =
                    case (sf, s, ptype, mode, opcode) of
                        (0w0, 0w0, 0w0, 0w0, 0w7) => ("fmov", "s", "w") (* w -> s *)
                    |   (0w0, 0w0, 0w0, 0w0, 0w6) => ("fmov", "w", "s") (* s -> w *)
                    |   (0w1, 0w0, 0w1, 0w0, 0w7) => ("fmov", "d", "x") (* d -> x *)
                    |   (0w1, 0w0, 0w1, 0w0, 0w6) => ("fmov", "x", "d") (* x -> d *)
                    |   (0w1, 0w0, 0w0, 0w0, 0w2) => ("scvtf", "x", "s")
                    |   (0w1, 0w0, 0w1, 0w0, 0w2) => ("scvtf", "x", "d")
                    |   (0w1, 0w0, 0w0, 0w0, 0w4) => ("fcvtas", "w", "s") (* s -> w *)
                    |   (0w1, 0w0, 0w0, 0w2, 0w0) => ("fcvtms", "w", "s") (* s -> w *)
                    |   (0w1, 0w0, 0w0, 0w1, 0w0) => ("fcvtps", "w", "s") (* s -> w *)
                    |   (0w1, 0w0, 0w0, 0w3, 0w0) => ("fcvtzs", "w", "s") (* s -> w *)
                    |   (0w1, 0w0, 0w1, 0w0, 0w4) => ("fcvtas", "x", "s") (* s -> x *)
                    |   (0w1, 0w0, 0w1, 0w2, 0w0) => ("fcvtms", "x", "s") (* s -> x *)
                    |   (0w1, 0w0, 0w1, 0w1, 0w0) => ("fcvtps", "x", "s") (* s -> x *)
                    |   (0w1, 0w0, 0w1, 0w3, 0w0) => ("fcvtzs", "x", "s") (* s -> x *)
                    |   _ => ("?", "?", "?")
            in
                printStream opc; printStream "\t";
                printStream dr; printStream(Word.fmt StringCvt.DEC rD); printStream ",";
                printStream nr; printStream(Word.fmt StringCvt.DEC rN)
            end
            
            else if (wordValue andb 0wxff200c00) = 0wx1E200800
            then (* Floating point two source operations. *)
            let
                val pt = (wordValue >> 0w22) andb 0w3
                and rM = (wordValue >> 0w16) andb 0wx1f
                and opc = (wordValue >> 0w12) andb 0wxf
                and rN = (wordValue >> 0w5) andb 0wx1f
                and rT = wordValue andb 0wx1f
                val (opcode, r) =
                    case (pt, opc) of
                        (0w0, 0wx0) => ("fmul", "s")
                    |   (0w0, 0wx1) => ("fdiv", "s")
                    |   (0w0, 0wx2) => ("fadd", "s")
                    |   (0w0, 0wx3) => ("fsub", "s")
                    |   (0w1, 0wx0) => ("fmul", "d")
                    |   (0w1, 0wx1) => ("fdiv", "d")
                    |   (0w1, 0wx2) => ("fadd", "d")
                    |   (0w1, 0wx3) => ("fsub", "d")
                    |   _ => ("??", "?")
            in
                printStream opcode; printStream "\t";
                printStream r; printStream(Word.fmt StringCvt.DEC rT); printStream ",";
                printStream r; printStream(Word.fmt StringCvt.DEC rN); printStream ",";
                printStream r; printStream(Word.fmt StringCvt.DEC rM)
            end

            else if (wordValue andb 0wxff207c00) = 0wx1E204000
            then (* Floating point single source. *)
            let
                val pt = (wordValue >> 0w22) andb 0w3
                and opc = (wordValue >> 0w15) andb 0wx3f
                and rN = (wordValue >> 0w5) andb 0wx1f
                and rT = wordValue andb 0wx1f
                val (opcode, rS, rD) =
                    case (pt, opc) of
                        (0w0, 0wx0) => ("fmov", "s", "s")
                    |   (0w0, 0wx1) => ("fabs", "s", "s")
                    |   (0w0, 0wx2) => ("fneg", "s", "s")
                    |   (0w0, 0wx5) => ("fcvt", "s", "d")
                    |   (0w1, 0wx0) => ("fmov", "d", "d")
                    |   (0w1, 0wx1) => ("fabs", "d", "d")
                    |   (0w1, 0wx2) => ("fneg", "d", "d")
                    |   (0w1, 0wx4) => ("fcvt", "d", "s")
                    |   _ => ("??", "?", "?")
            in
                printStream opcode; printStream "\t";
                printStream rD; printStream(Word.fmt StringCvt.DEC rT); printStream ",";
                printStream rS; printStream(Word.fmt StringCvt.DEC rN)
            end

            else if (wordValue andb 0wxff20fc07) = 0wx1E202000
            then (* Floating point comparison *)
            let
                val pt = (wordValue >> 0w22) andb 0w3
                and rM = (wordValue >> 0w16) andb 0wx1f
                and rN = (wordValue >> 0w5) andb 0wx1f
                and opc = (wordValue >> 0w3) andb 0w3
                val (opcode, r) =
                    case (pt, opc) of
                        (0w0, 0wx0) => ("fcmp", "s")
                    |   (0w1, 0wx0) => ("fcmp", "d")
                    |   (0w0, 0wx2) => ("fcmpe", "s")
                    |   (0w1, 0wx2) => ("fcmpe", "d")
                    |   _ => ("??", "?")
            in
                printStream opcode; printStream "\t";
                printStream r; printStream(Word.fmt StringCvt.DEC rN); printStream ",";
                printStream r; printStream(Word.fmt StringCvt.DEC rM)
            end

            else if (wordValue andb 0wx1e000000) = 0wx02000000
            then (* This is an unallocated range.  We use it for the register mask. *)
            let
                fun printMask (0w25, _) = ()
                |   printMask (i, comma) =
                    if ((0w1 << i) andb wordValue) <> 0w0
                    then
                    (
                        if comma then printStream ", " else ();
                        printStream "x";
                        printStream(Word.fmt StringCvt.DEC i);
                        printMask(i+0w1, true)
                    )
                    else printMask(i+0w1, comma)
            in
                printStream "["; printMask(0w0, false); printStream "]"
            end

            else printStream "?"
            ;
            printStream "\n"
        end
        
        fun printAll i =
            if i = numInstructions then ()
            else (printWordAt i; printAll(i+0w1))
    in
        printStream functionName;
        printStream ":\n";
        printAll 0w0
    end

    (* Adds the constants onto the code, and copies the code into a new segment *)
    fun generateCode {instrs, name=functionName, parameters, resultClosure} =
    let
        val printStream = Pretty.getSimplePrinter(parameters, [])
        and printAssemblyCode = Debug.getParameter Debug.assemblyCodeTag parameters
        
        local
            (* Extract the constants. *)
            fun getConsts(LoadAddressLiteral {value, ...}, (addrs, nonAddrs)) = (value::addrs, nonAddrs)
            |   getConsts(LoadNonAddressLiteral {value, ...}, (addrs, nonAddrs)) = (addrs, value::nonAddrs)
            |   getConsts(_, consts) = consts

            val (addrConsts, nonAddrConsts) = List.foldl getConsts ([], []) instrs
        in
            val addressConsts = List.rev addrConsts
            and nonAddressConsts = List.rev nonAddrConsts
        end
        
        val (byteVec, wordsOfCode) = genCode(instrs, addressConsts, nonAddressConsts)

        (* +3 for profile count, function name and constants count *)
        val numOfConst = List.length addressConsts
        val segSize   = wordsOfCode + Word.fromInt numOfConst + 0w4
        val firstConstant = wordsOfCode + 0w3 (* Add 3 for no of consts, fn name and profile count. *)
    
        (* Put in the number of constants. This must go in before
           we actually put in any constants. *)
        local
            val lastWord = segSize - 0w1
        in
            val () = set64(numOfConst + 2, wordsOfCode, byteVec)
            (* Set the last word of the code to the (negative) byte offset of the start of the code area
               from the end of this word. *)
            val () = set64((numOfConst + 3) * ~8, lastWord, byteVec) 
        end

        (* Now we've filled in all the size info we need to convert the segment
           into a proper code segment before it's safe to put in any ML values. *)
        val codeVec = byteVecToCodeVec(byteVec, resultClosure)

        local
            val name     : string = functionName
            val nameWord : machineWord = toMachineWord name
        in
            val () = codeVecPutWord (codeVec, wordsOfCode+0w1, nameWord)
        end
        (* Profile ref.  A byte ref used by the profiler in the RTS. *)
        local
            val v = RunCall.allocateByteMemory(0w1, Word.fromLargeWord(Word8.toLargeWord(Word8.orb(F_mutable, F_bytes))))
            fun clear 0w0 = ()
            |   clear i = (assignByte(v, i-0w1, 0w0); clear (i-0w1))
            val () = clear(wordSize)
        in
            val () = codeVecPutWord (codeVec, wordsOfCode+0w2, toMachineWord v)
        end

        (* and then copy the constants from the constant list. *)
        local
            fun setConstant(value, num) =
            (
                codeVecPutWord (codeVec, firstConstant + num, value);
                num+0w1
            )
        in
            val _ = List.foldl setConstant 0w0 addressConsts
        end
    in
        if printAssemblyCode
        then (* print out the code *)
            (printCode (codeVec, functionName, wordsOfCode, printStream); printStream"\n")
        else ();
        codeVecLock(codeVec, resultClosure)
    end (* copyCode *)


    structure Sharing =
    struct
        type closureRef = closureRef
        type instr = instr
        type xReg = xReg
        type vReg = vReg
        type labels = labels
        type condition = condition
        type shiftType = shiftType
        type wordSize = wordSize
        type 'a extend = 'a extend
        type scale = scale
    end
end;

