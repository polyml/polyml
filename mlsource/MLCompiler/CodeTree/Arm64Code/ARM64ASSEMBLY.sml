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
    
    datatype xReg = XReg of Word8.word | XZero | XSP

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
    and condAlways          = CCode 0wxe (* Any *)
    and condAlwaysNV        = CCode 0wxf (* Any - alternative encoding. *)
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
    |   Branch of { label: labels, jumpCondition: condition }
    |   LoadLabelAddress of { label: labels, reg: xReg }

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
        fun addSubtractShiftedReg (sf, oper, s, xdOp) ({regM, regN, regD, shift}) =
        let
            val (shift, imm6) = shiftEncode shift
        in
            SimpleInstr(0wx0b000000 orb (sf << 0w31) orb (oper << 0w30) orb (s << 0w29) orb
                (shift << 0w22) orb (word8ToWord(xRegOnly regM) << 0w16) orb
                (imm6 << 0w10) orb (word8ToWord(xRegOnly regN) << 0w5) orb
                word8ToWord(xdOp regD))
        end
    in
        val addShiftedReg = addSubtractShiftedReg(0w1, 0w0, 0w0, xRegOrXSP)
        and addSShiftedReg = addSubtractShiftedReg(0w1, 0w0, 0w1, xRegOrXZ)
        and subShiftedReg = addSubtractShiftedReg(0w1, 0w1, 0w0, xRegOrXSP)
        and subSShiftedReg = addSubtractShiftedReg(0w1, 0w1, 0w1, xRegOrXZ)
    end

    (* Add/subtract an extended register, optionally setting the flags. *)
    local
        (* It's not clear whether SP can be used here or how 31 is interpreted.
           For the moment don't allow either. *)
        fun addSubtractExtendedReg (sf, oper, s, opt) ({regM, regN, regD, extend}) =
        let
            val (option, imm3) = extendArithEncode extend
        in
            SimpleInstr(0wx0b200000 orb (sf << 0w31) orb (oper << 0w30) orb (s << 0w29) orb
                (opt << 0w22) orb (word8ToWord(xRegOnly regM) << 0w16) orb
                (option << 0w13) orb (imm3 << 0w10) orb
                (word8ToWord(xRegOnly regN) << 0w5) orb
                word8ToWord(xRegOnly regD))
        end
    in
        val addExtendedReg = addSubtractExtendedReg(0w1, 0w0, 0w0, 0w0)
        and addSExtendedReg = addSubtractExtendedReg(0w1, 0w0, 0w1, 0w0)
        and subExtendedReg = addSubtractExtendedReg(0w1, 0w1, 0w0, 0w0)
        and subSExtendedReg = addSubtractExtendedReg(0w1, 0w1, 0w1, 0w0)
    end


    (* Loads: There are two versions of this on the ARM.  There is a version that
       takes a signed 9-bit byte offset and a version that takes an unsigned
       12-bit word offset. *)
    
    local
        fun loadStoreRegScaled (size, v, opc) ({regT, regN, unitOffset}) =
        let
            val _ = (unitOffset >= 0 andalso unitOffset < 0x1000)
                orelse raise InternalError "loadStoreRegScaled: value out of range"
        in
            SimpleInstr(0wx39000000 orb (size << 0w30) orb (opc << 0w22) orb
                (v << 0w26) orb (Word.fromInt unitOffset << 0w10) orb
                (word8ToWord(xRegOrXSP regN) << 0w5) orb word8ToWord(xRegOnly regT))
        end
    in
        val loadRegScaled = loadStoreRegScaled(0w3, 0w0, 0w1)
        and storeRegScaled = loadStoreRegScaled(0w3, 0w0, 0w0)
        (* (Unsigned) byte operations.  There are also signed versions. *)
        and loadRegScaledByte = loadStoreRegScaled (0w0, 0w0, 0w1)
        and storeRegScaledByte = loadStoreRegScaled (0w0, 0w0, 0w0)
    end    

    local
        (* Loads and stores with a signed byte offset.  This includes simple
           unscaled addresses, pre-indexing and post-indexing. *)
        fun loadStoreByteAddress op4 (size, v, opc) ({regT, regN, byteOffset}) =
        let
            val _ = (byteOffset >= ~256 andalso byteOffset < 256)
                orelse raise InternalError "loadStoreUnscaled: value out of range"
            val imm9 = Word.fromInt byteOffset andb 0wx1ff
        in
            SimpleInstr(0wx38000000 orb (size << 0w30) orb (opc << 0w22) orb
                (v << 0w26) orb (imm9 << 0w12) orb (op4 << 0w10) orb
                (word8ToWord(xRegOrXSP regN) << 0w5) orb word8ToWord(xRegOnly regT))
        end
        
        val loadStoreUnscaled = loadStoreByteAddress 0w0
        and loadStorePostIndex = loadStoreByteAddress 0w1
        and loadStorePreIndex = loadStoreByteAddress 0w3
    in
        val loadRegUnscaled = loadStoreUnscaled (0w3, 0w0, 0w1)
        and storeRegUnscaled = loadStoreUnscaled (0w3, 0w0, 0w0)
        (* (Unsigned) byte operations.  There are also signed versions. *)
        and loadRegUnscaledByte = loadStoreUnscaled (0w0, 0w0, 0w1)
        and storeRegUnscaledByte = loadStoreUnscaled (0w0, 0w0, 0w0)

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
        fun loadStoreRegRegisterOffset (size, v, opc) ({regT, regN, regM, option}) =
        let
            val (opt, s) =
                case extendLSEncode option of
                    (opt, ScaleOrShift) => (opt, 0w1) | (opt, NoScale) => (opt, 0w0)
        in
            SimpleInstr(0wx38200800 orb (size << 0w30) orb (v << 0w26) orb (opc << 0w22) orb
                (word8ToWord(xRegOnly regM) << 0w16) orb (opt << 0w13) orb (s << 0w12) orb
                (word8ToWord(xRegOrXSP regN) << 0w5) orb word8ToWord(xRegOrXSP regT))
        end
    in
        val loadRegIndexed = loadStoreRegRegisterOffset(0w3, 0w0, 0w1)
        and storeRegIndexed = loadStoreRegRegisterOffset(0w3, 0w0, 0w0)
        and loadRegIndexedByte = loadStoreRegRegisterOffset(0w0, 0w0, 0w1)
        and storeRegIndexedByte = loadStoreRegRegisterOffset(0w0, 0w0, 0w0)
    end

    (* Addresses must go in the constant area at the end of the code where they
       can be found by the GC. *)
    fun loadAddressConstant(xReg, valu) = LoadAddressLiteral{reg=xReg, value=valu}

    (* Non-address constants.  These may or may not be tagged values. *)
    local
        fun loadConstantFromCArea(xReg, valu) = LoadNonAddressLiteral{reg=xReg, value=valu}

        (* Move an unsigned constant. *)
        fun genMoveShortConstToReg(xReg, constnt, shiftBits, is64, opc) =
            SimpleInstr((if is64 then 0wx80000000 else 0w0) orb (opc << 0w29) orb 0wx12800000 orb (shiftBits << 0w21) orb
                (constnt << 0w5) orb word8ToWord(xRegOnly xReg))
        val opcMovZ = 0w2 (* Zero the rest of the register. *)
        and opcMovK = 0w3 (* Keep the rest of the register. *)
        and opcMovN = 0w0 (* Invert the value and the rest of the register. *)
    in
        fun loadNonAddressConstant(xReg, valu) =
        (* If this can be encoded using at most two instructions we do that
           otherwise the constant is stored in the non-address constant area. *)
        let
            fun extW h = Word.fromLarge(LargeWord.>>(Word64.toLarge valu, h*0w16)) andb 0wxffff
            val hw0 = extW 0w3 and hw1 = extW 0w2 and hw2 = extW 0w1 and hw3 = extW 0w0
        in
            if hw0 = 0w0 andalso hw1 = 0w0
            then (* If the top 32-bits are zero we can use a 32-bit move. *)
            (
                if hw2 = 0w0
                then genMoveShortConstToReg(xReg, hw3, 0w0, false, opcMovZ)
                else if hw3 = 0w0
                then genMoveShortConstToReg(xReg, hw2, 0w1, false, opcMovZ)
                else if hw2 = 0wxffff
                then genMoveShortConstToReg(xReg, Word.xorb(hw3, 0wxffff), 0w0, false, opcMovN)
                else if hw3 = 0wxffff
                then genMoveShortConstToReg(xReg, Word.xorb(hw2, 0wxffff), 0w1, false, opcMovN)
                else
                (
                    genMoveShortConstToReg(xReg, hw3, 0w0, false, opcMovZ);
                    genMoveShortConstToReg(xReg, hw2, 0w1, false, opcMovK)
                )
            )
            (* TODO: For the moment just handle the simple case. *)
            else if hw0 = 0wxffff andalso hw1 = 0wxffff andalso hw2 = 0wxffff
            then genMoveShortConstToReg(xReg, Word.xorb(hw3, 0wxffff), 0w0, true, opcMovN)

            else loadConstantFromCArea(xReg, valu)
        end
    end



    (* Move a value from one register into another.  This actually uses ORR shifted
       register but for the moment we'll just encode it independently. *)
    fun genMoveRegToReg({sReg, dReg}) =
        SimpleInstr(0wxAA0003E0 orb (word8ToWord(xRegOnly sReg) << 0w16) orb word8ToWord(xRegOnly dReg))

    (* Jump to the address in the register and put the address of the
       next instruction into X30. *)
    fun genBranchAndLinkReg(dest) =
        SimpleInstr(0wxD63F0000 orb (word8ToWord(xRegOnly dest) << 0w5))

    (* Jump to the address in the register. *)
    fun genBranchRegister(dest) =
        SimpleInstr(0wxD61F0000 orb (word8ToWord(xRegOnly dest) << 0w5))

    (* Jump to the address in the register and hint this is a return. *)
    fun genReturnRegister(dest) =
        SimpleInstr(0wxD65F0000 orb (word8ToWord(xRegOnly dest) << 0w5))

    (* Put a label into the code. *)
    val setLabel = Label

    (* Create a label. *)
    fun createLabel () = ref [ref 0w0]

    (* A conditional or unconditional branch. *)
    and putBranchInstruction(cond, label) = Branch{label=label, jumpCondition=cond}
    
    and loadLabelAddress(reg, label) = LoadLabelAddress{label=label, reg=reg}

    (* Sets the destination register to the value of the first reg if the
       condition is true otherwise the second register incremented by one.
       Using XZR for the second register means the result value is one.
       This is used to set the value to either "true" (tagged 1 = 3) or
       "false" (tagged 0 = 1) by setting the result register to 3 and then
       conditionally setting it to XZR incremented. *)
    fun conditionalSetIncrement({regD, regTrue, regFalse, cond=CCode cond}) =
        SimpleInstr(0wx9A800400 orb (word8ToWord(xRegOrXZ regFalse) << 0w16) orb
            (word8ToWord cond << 0w12) orb (word8ToWord(xRegOrXZ regTrue) << 0w5) orb
            word8ToWord(xRegOrXZ regD))

    (* This combines the effect of a left and right shift.  There are various
       derived forms of this depending on the relative values of immr and imms.
       if imms >= immr copies imms-immr-1 bits from bit position immr to the lsb
       bits of the destination.
       if imms < immr copies imms+1 bits from the lsb bit to bit position
       regsize-immr.  Bits not otherwise set are zeroed. *)
    fun unsignedBitfieldMove({wordSize, n, immr, imms, regN, regD}) =
    let
        val sf = case wordSize of WordSize32 => 0w0 | WordSize64 => 0w1 << 0w31
    in
        SimpleInstr(0wx53000000 orb sf orb (n << 0w22) orb
            (immr << 0w16) orb (imms << 0w10) orb (word8ToWord(xRegOnly regN) << 0w5) orb
            word8ToWord(xRegOnly regD))
    end

    fun logicalShiftLeft({wordSize, shift, regN, regD}) =
    let
        val (wordBits, n) = case wordSize of WordSize32 => (0w32, 0w0) | WordSize64 => (0w64, 0w1)
    in
        unsignedBitfieldMove({wordSize=wordSize, n=n, immr=Word.~ shift mod wordBits,
            imms=wordBits-0w1-shift, regN=regN, regD=regD})
    end

    and logicalShiftRight({wordSize, shift, regN, regD}) =
    let
        val (imms, n) = case wordSize of WordSize32 => (0wx1f, 0w0) | WordSize64 => (0wx3f, 0w1)
    in
        unsignedBitfieldMove({wordSize=wordSize, n=n, immr=shift,
            imms=imms, regN=regN, regD=regD})
    end
    
    and unsignedBitfieldInsertinZeros({wordSize, lsb, width, regN, regD}) =
    let
        val (wordBits, n) = case wordSize of WordSize32 => (0w32, 0w0) | WordSize64 => (0w64, 0w1)
    in
        unsignedBitfieldMove({wordSize=wordSize, n=n, immr=Word.~ lsb mod wordBits,
            imms=width-0w1, regN=regN, regD=regD})
    end

    local
        (* Logical immediates.  AND, OR, XOR and ANDS.  Assumes that the immediate value
           has already been checked as valid. *)
        fun logicalImmediate opc ({wordSize, bits, regN, regD}) =
        let
            val s = case wordSize of WordSize32 => 0w0 | WordSize64 => 0w1
            val {n, imms, immr} = 
                case encodeBitPattern(bits, wordSize) of
                    NONE => raise InternalError "testBitPattern: unable to encode bit pattern"
                |   SOME res => res
        in
            SimpleInstr(0wx12000000 orb (opc << 0w29) orb (s << 0w31) orb (n << 0w22) orb
                (immr << 0w16) orb (imms << 0w10) orb (word8ToWord(xRegOrXZ regN) << 0w5) orb
                word8ToWord(xRegOrXZ regD))
        end
    in
        val bitwiseAndImmediate = logicalImmediate 0w0
        and bitwiseOrImmediate = logicalImmediate 0w1
        and bitwiseXorImmediate = logicalImmediate 0w2
        and bitwiseAndSImmediate = logicalImmediate 0w3
        
        (* Test a bit pattern in a register.  If the pattern is within the low-order
           32-bits we use a 32-bit test. *)
        fun testBitPattern(reg, bits) =
        let
            val w = if bits <= 0wxffffffff then WordSize32 else WordSize64
        in
            bitwiseAndSImmediate({wordSize=w, bits=bits, regN=reg, regD=XZero})
        end
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
    

    (* Size of each code word.  All except labels are one word at the moment. *)
    fun codeSize (SimpleInstr _) = 1 (* Number of 32-bit words *)
    |   codeSize (LoadAddressLiteral _) = 1
    |   codeSize (LoadNonAddressLiteral _) = 1
    |   codeSize (Label _) = 0
    |   codeSize (Branch _) = 1
    |   codeSize (LoadLabelAddress _) = 1

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

        |   genCodeWords(Branch{ label=ref labs, jumpCondition=CCode cond }:: tail, wordNo, aConstNum, nonAConstNum) =
            let
                val dest = !(hd labs)
                val offset = Word.toInt dest - Word.toInt wordNo
            in
                if cond = 0wxe orelse cond = 0wxf
                then (* We can use an unconditional branch. *)
                (
                    (offset < Word.toInt(0w1 << 0w25) andalso offset >= ~ (Word.toInt(0w1 << 0w25)))
                        orelse raise InternalError "genCodeWords: branch too far";
                    writeInstr(0wx14000000 orb (Word.fromInt offset andb 0wx03ffffff), wordNo, codeVec)
                )
                else
                (
                    (offset < Word.toInt(0w1 << 0w18) andalso offset >= ~ (Word.toInt(0w1 << 0w18)))
                        orelse raise InternalError "genCodeWords: branch too far";
                    writeInstr(0wx54000000 orb ((Word.fromInt offset andb 0wx07ffff) << 0w5)
                        orb word8ToWord cond, wordNo, codeVec)
                );

                genCodeWords(tail, wordNo+0w1, aConstNum, nonAConstNum)
            end

        |   genCodeWords(LoadLabelAddress{label=ref labs, reg} :: tail, wordNo, aConstNum, nonAConstNum) =
            let
                val dest = !(hd labs)
                val offset = dest - wordNo
                val _ = offset < 0wx100000 orelse offset >= ~ 0wx100000
                    orelse raise InternalError "Offset to label address is too large"
                val code = 0wx10000000 orb ((offset andb 0wx1fffff) << 0w5) orb word8ToWord(xRegOnly reg)
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

            else if (wordValue andb 0wx1f800000) = 0wx12800000
            then (* Move of constants.  Includes movn and movk. *)
            let
                val rD = wordValue andb 0wx1f
                val imm16 = Word.toInt((wordValue andb 0wx1fffe) >> 0w5)
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
                else (printStream ",lsl #"; printStream(Word.fmt StringCvt.HEX (shift*0w16)))
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
                    |   (0w3, 0w0, 0w0) => ("stur", "x")
                    |   (0w3, 0w0, 0w1) => ("ldur", "x")
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
                    |   (0w3, 0w0, 0w0) => ("str", "x")
                    |   (0w3, 0w0, 0w1) => ("ldr", "x")
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
                printStream opr; printStream "\tx"; printStream(Word.fmt StringCvt.DEC rD);
                printStream ",x"; printStream(Word.fmt StringCvt.DEC rN);
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
                else (printStream "subs\tx"; printStream(Word.fmt StringCvt.DEC rD); printStream ",");
                printStream "x"; printStream(Word.fmt StringCvt.DEC rN);
                printStream ",#"; printStream(Word.fmt StringCvt.DEC imm)
            end
            
            else if (wordValue andb 0wx1f800000) = 0wx0B000000
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
                val reg = if (wordValue andb 0wx80000000) <> 0w0 then "x" else "w"
            in
                if isS andalso rD = 0w31
                then printStream(if oper then "cmn\t" else "cmp\t")
                else
                (
                    printStream(if oper then "add" else "sub"); printStream(if isS then "s\t" else "\t");
                    printStream reg;
                    printStream(Word.fmt StringCvt.DEC rD); printStream ","
                );
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

            else if (wordValue andb 0wxff800000) = 0wxF9000000
            then
            let
                (* Load/Store with an unsigned offset.  The offset is in units of 64-bits. *)
                val opc = if (wordValue andb 0wx400000) = 0w0 then "str" else "ldr"
                val rT = wordValue andb 0wx1f
                and rN = (wordValue andb 0wx3e0) >> 0w5
                and imm12 = (wordValue andb 0wx3ffc00) >> 0w10
            in
                printStream opc; printStream "\tx"; printStream(Word.fmt StringCvt.DEC rT);
                printStream ",[x"; printStream(Word.fmt StringCvt.DEC rN);
                printStream ",#"; printStream(Word.fmt StringCvt.DEC(imm12*0w8));
                printStream "]"
            end

            else if (wordValue andb 0wxffe0ffe0) = 0wxAA0003E0
            then (* Move reg,reg.  This is a subset of ORR shifted register. *)
            (
                printStream "mov\tx";
                printStream(Word.fmt StringCvt.DEC(wordValue andb 0wx1f));
                printStream ",x";
                printStream(Word.fmt StringCvt.DEC((wordValue >> 0w16)andb 0wx1f))
            )

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

            else if (wordValue andb 0wxffe00c00) = 0wx9A800400
            then
            let
                val rT = wordValue andb 0wx1f
                val rN = (wordValue >> 0w5) andb 0wx1f
                val rM = (wordValue >> 0w16) andb 0wx1f
                val cond = (wordValue >> 0w12) andb 0wxf
            in
                printStream "csinc\tx"; printStream(Word.fmt StringCvt.DEC rT);
                printStream ",x"; printStream(Word.fmt StringCvt.DEC rN);
                printStream ",x"; printStream(Word.fmt StringCvt.DEC rM);
                printStream ","; printCondition cond
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
        type labels = labels
        type condition = condition
        type shiftType = shiftType
        type wordSize = wordSize
        type 'a extend = 'a extend
        type scale = scale
    end
end;

