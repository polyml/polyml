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
    
    exception Fallback (* Define this here in case we want to raise it. *)

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
    and exceptionHandlerOffset  = 5
    and stackLimitOffset        = 6
    and exceptionPacketOffset   = 7
    and threadIdOffset          = 8

    datatype instr =
        SimpleInstr of word
    |   LoadLiteral of {reg: xReg, cNum: int, isAddress: bool}
    |   Label of labels
    |   Branch of { label: labels, jumpCondition: condition }
    |   CheckStack of { work: xReg, spaceRef: int ref }

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

    (* Many instructions include a possible shift. *)
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

    datatype code =
    Code of 
    {
        instructions:   instr list ref,        (* Code in reverse order. *)
        addressConstVec: machineWord list ref, (* Addresses for the constant area. *)
        nonAddressConstVec: Word64.word list ref, (* Non-address constants. *)
        functionName:   string,               (* Name of the function. *)
        printAssemblyCode:bool,               (* Whether to print the code when we finish. *)
        printStream:    string->unit,         (* The stream to use *)
        maxStackRef:    int ref                 (* Set to the stack space required. *)
    }

    fun codeCreate (name, parameters) = 
    let
        val printStream = Pretty.getSimplePrinter(parameters, [])
    in
        Code
        {
            instructions     = ref nil,
            addressConstVec  = ref [],
            nonAddressConstVec = ref [],
            functionName     = name,
            printAssemblyCode = Debug.getParameter Debug.assemblyCodeTag parameters,
            printStream    = printStream,
            maxStackRef      = ref 0
        }
    end

    fun addInstr (instr, Code{instructions, ...}) =
        instructions := SimpleInstr instr :: ! instructions

    val nopCode  = 0wxD503201F
    
    local
        fun addSubRegConstant({sReg, dReg, cValue, shifted, isAdd}, code) =
        let
            val () =
                if cValue < 0 orelse cValue >= 0x400 then raise InternalError "genAddRegConstant: Value > 12 bits" else ()
        in
            addInstr((if isAdd then 0wx91000000 else 0wxD1000000) orb
                (if shifted then 0wx400000 else 0w0) orb
                (Word.fromInt cValue << 0w10) orb (word8ToWord(xRegOrXSP sReg) << 0w5) orb
                word8ToWord(xRegOrXSP dReg), code)
        end
    in
        fun genAddRegConstant({sReg, dReg, cValue}, code) =
            addSubRegConstant({sReg=sReg, dReg=dReg, cValue=cValue, shifted=false, isAdd=true}, code)
        and genSubRegConstant({sReg, dReg, cValue}, code) =
            addSubRegConstant({sReg=sReg, dReg=dReg, cValue=cValue, shifted=false, isAdd=false}, code)
    end

    (* Subtract a 12-bit constant, possibly shifted by 12 bits and set the
       condition flags.  The destination can be the zero register in which
       case this is a comparison. *)
    fun genSubSRegConstant({sReg, dReg, cValue, shifted}, code) =
    let
        val () =
            if cValue < 0 orelse cValue >= 0x400 then raise InternalError "genAddRegConstant: Value > 12 bits" else ()
    in
        addInstr(0wxF1000000 orb (if shifted then 0wx400000 else 0w0) orb
            (Word.fromInt cValue << 0w10) orb (word8ToWord(xRegOrXSP sReg) << 0w5) orb
            word8ToWord(xRegOrXZ dReg), code)
    end

    (* Subtract regM, after a possible shift, from regN and put the result in regD,
       setting the flags.  This is frequently used as a comparison. *)
    fun subSRegReg({regM, regN, regD, shift}, code) =
    let
        val (shft, imm6) = shiftEncode shift
    in
        addInstr(0wxEB000000 orb (shft << 0w22) orb (word8ToWord(xRegOnly regM) << 0w16) orb
            (imm6 << 0w10) orb (word8ToWord(xRegOnly regN) << 0w5) orb
            word8ToWord(xRegOrXZ regD), code)
    end

    (* Loads: There are two versions of this on the ARM.  There is a version that
       takes a signed 9-bit byte offset and a version that takes an unsigned
       12-bit word offset. *)
    
    (* Load an aligned value using an unsigned offset. *)
    fun loadRegScaled({dest, base, wordOffset}, code) =
    let
        val _ = (wordOffset >= 0 andalso wordOffset < 0x1000)
            orelse raise InternalError "loadRegAligned: value out of range"
    in
        addInstr(0wxF9400000 orb (Word.fromInt wordOffset << 0w10) orb
            (word8ToWord(xRegOrXSP base) << 0w5) orb word8ToWord(xRegOnly dest), code)
    end
    
    (* and corresponding store. *)
    and storeRegScaled({dest, base, wordOffset}, code) =
    let
        val _ = (wordOffset >= 0 andalso wordOffset < 0x1000)
            orelse raise InternalError "storeRegAligned: value out of range"
    in
        addInstr(0wxF9000000 orb (Word.fromInt wordOffset << 0w10) orb
            (word8ToWord(xRegOrXSP base) << 0w5) orb word8ToWord(xRegOnly dest), code)
    end
    
    (* Store a value using a signed byte offset. *)
    fun storeRegUnscaled({dest, base, byteOffset}, code) =
    let
        val _ = (byteOffset >= ~256 andalso byteOffset < 256)
            orelse raise InternalError "storeRegUnaligned: value out of range"
        val imm9 = Word.fromInt byteOffset andb 0wx1ff
    in
        addInstr(0wxF8000000 orb (imm9 << 0w12) orb
            (word8ToWord(xRegOrXSP base) << 0w5) orb word8ToWord(xRegOnly dest), code)
    end
    
    (* Push a register to the ML stack. This uses a pre-increment store to x28 *)
    fun genPushReg(xReg, code) =
        addInstr(0wxF81F8F80 orb word8ToWord(xRegOnly xReg), code)
    
    (* Pop a register from the ML stack. *)
    fun genPopReg(xReg, code) =
        addInstr(0wxF8408780 orb word8ToWord(xRegOnly xReg), code)

    (* Addresses must go in the constant area at the end of the code where they
       can be found by the GC. *)
    fun loadAddressConstant(xReg, valu, Code{instructions, addressConstVec, ...}) =
    let
        val cNum = List.length(!addressConstVec) 
    in
        addressConstVec := valu :: ! addressConstVec;
        instructions := LoadLiteral{reg=xReg, cNum=cNum, isAddress=true} :: ! instructions
    end

    (* Non-address constants.  These may or may not be tagged values. *)
    local
        fun loadConstantFromCArea(xReg, valu, Code{instructions, nonAddressConstVec, ...}) =
        let
            val cNum = List.length(!nonAddressConstVec)
        in
            nonAddressConstVec := valu :: !nonAddressConstVec;
            instructions := LoadLiteral{reg=xReg, cNum=cNum, isAddress=false} :: ! instructions
        end

        (* Move an unsigned constant. *)
        fun genMoveShortConstToReg(xReg, constnt, shiftBits, is64, opc, code) =
            addInstr((if is64 then 0wx80000000 else 0w0) orb (opc << 0w29) orb 0wx12800000 orb (shiftBits << 0w21) orb
                (constnt << 0w5) orb word8ToWord(xRegOnly xReg), code)
        val opcMovZ = 0w2 (* Zero the rest of the register. *)
        and opcMovK = 0w3 (* Keep the rest of the register. *)
        and opcMovN = 0w0 (* Invert the value and the rest of the register. *)
    in
        fun loadNonAddressConstant(xReg, valu, code) =
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
                then genMoveShortConstToReg(xReg, hw3, 0w0, false, opcMovZ, code)
                else if hw3 = 0w0
                then genMoveShortConstToReg(xReg, hw2, 0w1, false, opcMovZ, code)
                else if hw2 = 0wxffff
                then genMoveShortConstToReg(xReg, Word.xorb(hw3, 0wxffff), 0w0, false, opcMovN, code)
                else if hw3 = 0wxffff
                then genMoveShortConstToReg(xReg, Word.xorb(hw2, 0wxffff), 0w1, false, opcMovN, code)
                else
                (
                    genMoveShortConstToReg(xReg, hw3, 0w0, false, opcMovZ, code);
                    genMoveShortConstToReg(xReg, hw2, 0w1, false, opcMovK, code)
                )
            )
            (* TODO: For the moment just handle the simple case. *)
            else if hw0 = 0wxffff andalso hw1 = 0wxffff andalso hw2 = 0wxffff
            then genMoveShortConstToReg(xReg, Word.xorb(hw3, 0wxffff), 0w0, true, opcMovN, code)

            else loadConstantFromCArea(xReg, valu, code)
        end
    end



    (* Move a value from one register into another.  This actually uses ORR shifted
       register but for the moment we'll just encode it independently. *)
    fun genMoveRegToReg({sReg, dReg}, code) =
        addInstr(0wxAA0003E0 orb (word8ToWord(xRegOnly sReg) << 0w16) orb word8ToWord(xRegOnly dReg), code)

    (* Jump to the address in the register and put the address of the
       next instruction into X30. *)
    fun genBranchAndLinkReg(dest, code) =
        addInstr(0wxD63F0000 orb (word8ToWord(xRegOnly dest) << 0w5), code)

    (* Jump to the address in the register. *)
    fun genBranchRegister(dest, code) =
        addInstr(0wxD61F0000 orb (word8ToWord(xRegOnly dest) << 0w5), code)

    (* Jump to the address in the register and hint this is a return. *)
    fun genReturnRegister(dest, code) =
        addInstr(0wxD65F0000 orb (word8ToWord(xRegOnly dest) << 0w5), code)

    (* Put a label into the code. *)
    fun setLabel(label, Code{instructions, ...}) =
        instructions := Label label :: ! instructions
    (* Create a label. *)
    and createLabel () = ref [ref 0w0]

    (* A conditional or unconditional branch. *)
    and putBranchInstruction(cond, label, Code{instructions, ...}) =
        instructions := Branch{label=label, jumpCondition=cond} :: ! instructions

    (* Sets the destination register to the value of the first reg if the
       condition is true otherwise the second register incremented by one.
       Using XZR for the second register means the result value is one.
       This is used to set the value to either "true" (tagged 1 = 3) or
       "false" (tagged 0 = 1) by setting the result register to 3 and then
       conditionally setting it to XZR incremented. *)
    fun conditionalSetIncrement({regD, regTrue, regFalse, cond=CCode cond}, code) =
        addInstr(0wx9A800400 orb (word8ToWord(xRegOrXZ regFalse) << 0w16) orb
            (word8ToWord cond << 0w12) orb (word8ToWord(xRegOrXZ regTrue) << 0w5) orb
            word8ToWord(xRegOrXZ regD), code)

    (* Put in a check for the stack for the function. *)
    fun checkStackForFunction(workReg, Code{instructions, maxStackRef, ...}) =
        instructions := CheckStack{work=workReg, spaceRef=maxStackRef} :: ! instructions

    (* Inserted in a backwards jump to allow loops to be interrupted.  *)
    fun checkForInterrupts(workReg, Code{instructions, ...}) =
        instructions := CheckStack{work=workReg, spaceRef=ref 0} :: ! instructions

    (* This word is put in after a call to the RTS trap-handler.  All the registers
       are saved and restored across a call to the trap-handler; the register
       mask contains those that may contain an address and so need to be scanned and
       possibly updated if there is a GC. *)
    fun registerMask(regs, code) =
    let
        fun addToMask(r, mask) = mask orb (0w1 << word8ToWord(xRegOnly r))
        val maskWord = List.foldl addToMask 0w0 regs
    in
        addInstr(0wx02000000 (* Reserved instr range. *) orb maskWord, code)
    end
    
    

    (* Size of each code word.  All except labels are one word at the moment. *)
    fun codeSize (SimpleInstr _) = 1 (* Number of 32-bit words *)
    |   codeSize (LoadLiteral _) = 1
    |   codeSize (Label _) = 0
    |   codeSize (Branch _) = 1
    |   codeSize (CheckStack _) = 7 (* Fixed size for the moment. *)

    fun foldCode startIc foldFn ops =
    let
        fun doFold(oper :: operList, ic) =
            doFold(operList,
                (* Get the size BEFORE any possible change. *)
                ic + Word.fromInt(codeSize oper) before foldFn(oper, ic))
        |   doFold(_, ic) = ic
    in
        doFold(ops, startIc)
    end

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

    fun genCode(ops, Code {addressConstVec=ref addrConst, nonAddressConstVec=ref nonAddrConsts, ...}) =
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
        
        val numNonAddrConsts = Word.fromInt(List.length nonAddrConsts)
        and numAddrConsts = Word.fromInt(List.length addrConst)

        val segSize = wordsOfCode + numAddrConsts + numNonAddrConsts + 0w4 (* 4 extra words *)
        val codeVec = byteVecMake segSize

        fun genCodeWords(SimpleInstr code, wordNo) = writeInstr(code, wordNo, codeVec)

        |   genCodeWords(LoadLiteral{reg, cNum, isAddress}, wordNo) =
            let
                (* The offset is in 32-bit words.  The first of the constants is
                   at offset wordsOfCode+3 *)
                val offsetOfConstant =
                    (wordsOfCode+(if isAddress then numNonAddrConsts+0w3 else 0w0)+Word.fromInt cNum)*0w2 - wordNo
                val _ = offsetOfConstant < 0wx100000 orelse raise InternalError "Offset to constant is too large"
                val code = 0wx58000000 orb (offsetOfConstant << 0w5) orb word8ToWord(xRegOnly reg)
            in
                writeInstr(code, wordNo, codeVec)
            end

        |   genCodeWords(Label _, _) = () (* No code. *)

        |   genCodeWords(Branch{ label=ref labs, jumpCondition=CCode cond }, wordNo) =
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
                )

            end

        |   genCodeWords(CheckStack{work, spaceRef=ref space}, wordNo) =
            let (* Check for stack space. *)
                (*  ldr x10,[x26,#48]
                    sub x9,x28,#words
                    cmp x9,x10
                    b.cs L1
                    ldr x10,[x26,#24]
                    blr x10
                    0x20000000
                    L1: *)
                val bytesNeeded = space * 8
                val _ = bytesNeeded < 0x400 orelse raise InternalError "CheckStack: too large"
            in
                writeInstr(0wxF9401B4A, wordNo, codeVec);
                writeInstr(0wxD1000389 orb (Word.fromInt bytesNeeded << 0w10), wordNo+0w1, codeVec);
                writeInstr(0wxEB0A013F, wordNo+0w2, codeVec);
                writeInstr(0wx54000082, wordNo+0w3, codeVec);
                writeInstr(0wxF9400F4A, wordNo+0w4, codeVec);
                writeInstr(0wxD63F0140, wordNo+0w5, codeVec);
                (* This is an unallocated instruction.  The lower 25 bits are the registers
                   that must be updated if there is a GC.  *)
                writeInstr(0wx02000000, wordNo+0w6, codeVec) (* Register mask - currently empty *)
            end
    in
        foldCode 0w0 genCodeWords (ops @ paddingWord);
        (* Copy in the non-address constants. *)
        List.foldl(fn (cVal, addr) => (write64Bit(cVal, addr, codeVec); addr+0w1)) wordsOfCode (List.rev nonAddrConsts);
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

            else if (wordValue andb 0wxffe00000) = 0wxF8000000
            then (* Store with 9-bit byte offset. *)
            let
                val rT = wordValue andb 0wx1f
                and rN = (wordValue andb 0wx3e0) >> 0w5
                and imm9 = (wordValue andb 0wx1ff000) >> 0w12
                val imm9Text =
                    if imm9 > 0wxff
                    then "-" ^ Word.fmt StringCvt.DEC (0wx200 - imm9)
                    else Word.fmt StringCvt.DEC imm9
                val opBits = (wordValue >> 0w10) andb 0w3
            in
                printStream(if opBits = 0w0 then "stur\tx" else "str\tx");
                printStream(Word.fmt StringCvt.DEC rT);
                printStream ",[x"; printStream(Word.fmt StringCvt.DEC rN);
                case opBits of
                    0w0 => (printStream ",#"; printStream imm9Text; printStream "]")
                |   0w1 => (printStream "],#"; printStream imm9Text) (* Post-index *)
                |   0w3 => (printStream ",#"; printStream imm9Text; printStream "]!") (* Pre-index *)
                |   _ => printStream "??" (* Not used - store unprivileged. *)
            end

            else if (wordValue andb 0wxffe00c00) = 0wxF8400400
            then
            let
                (* Load with post-indexing *)
                val rT = wordValue andb 0wx1f
                and rN = (wordValue andb 0wx3e0) >> 0w5
                and imm9 = (wordValue andb 0wx1ff000) >> 0w12
                val imm9Text =
                    if imm9 > 0wxff
                    then "-" ^ Word.fmt StringCvt.DEC(0wx200 - imm9)
                    else Word.fmt StringCvt.DEC imm9
            in
                printStream "ldr\tx"; printStream(Word.fmt StringCvt.DEC rT);
                printStream ",[x"; printStream(Word.fmt StringCvt.DEC rN); printStream "],#";
                printStream imm9Text
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
            
            else if (wordValue andb 0wxffe00000) = 0wxEB000000
            then
            let
                (* Subtract a register from another, possibly shifted, setting the flags. *)
                val rD = wordValue andb 0wx1f
                and rN = (wordValue >> 0w5) andb 0wx1f
                and rM = (wordValue >> 0w16) andb 0wx1f
                and imm6 = (wordValue >> 0w10) andb 0wx3f
                and shiftCode = (wordValue >> 0w22) andb 0wx3
            in
                if rD = 0w31
                then printStream "cmp\t"
                else (printStream "subs\tx"; printStream(Word.fmt StringCvt.DEC rD); printStream ",");
                printStream "x"; printStream(Word.fmt StringCvt.DEC rN);
                printStream ",x"; printStream(Word.fmt StringCvt.DEC rM);
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
    fun generateCode {code as
            Code{ instructions = ref instrs, printAssemblyCode, printStream,
                  functionName, addressConstVec=ref addrConsts, maxStackRef, ...},
            maxStack, resultClosure} =
    let
        val () = maxStackRef := maxStack

        local
            val codeList = List.rev instrs
            (* Add a stack check.  We need to do this for native code. *)
        in
            val codeList =
                if maxStack < 128
                then codeList
                else raise InternalError "TODO" (* SimpleCode[opcode_stackSize16, Word8.fromInt maxStack, Word8.fromInt(maxStack div 256)] :: codeList *)
        end

        val (byteVec, wordsOfCode) = genCode(codeList, code)

        (* +3 for profile count, function name and constants count *)
        val numOfConst = List.length addrConsts
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
            val _ = List.foldl setConstant 0w0 (List.rev addrConsts)
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
        type code = code
        type closureRef = closureRef
        type instr = instr
        type xReg = xReg
        type labels = labels
        type condition = condition
        type shiftType = shiftType
    end
end;

