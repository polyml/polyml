(*
    Copyright David C. J. Matthews 1989, 2000, 2009-10, 2012-13, 2015-17
    
    Based on original code:    
    Copyright (c) 2000
        Cambridge University Technical Services Limited

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

(*
    Title:      Code Generator Routines.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1989
*)

(* This module contains the code vector and operations to insert code into
   it. Each procedure is compiled into a separate segment. Initially it is
   compiled into a fixed size segment, and then copied into a segment of the
   correct size at the end.
   This module contains all the definitions of the X86 opCodes and registers.
   It uses "codeseg" to create and operate on the segment itself.
 *)

functor X86OUTPUTCODE (
structure DEBUG: DEBUGSIG
structure PRETTY: PRETTYSIG (* for compilerOutTag *)

) : X86CODESIG =

struct
    open CODE_ARRAY
    open DEBUG;
    open Address
    open Misc;

    val isX64 = wordSize = 8 (* Generate X64 instructions if the word length is 8. *)

    infix 5 << <<+ <<- >> >>+ >>- ~>> ~>>+ ~>>- (* Shift operators *)
    infix 3 andb orb xorb andbL orbL xorbL andb8 orb8 xorb8
    
    val op << = Word.<< and op >> = Word.>>
    val (*op <<+ = LargeWord.<< and *) op >>+ = LargeWord.>>
    val op <<- = Word8.<< and op >>- = Word8.>>

    val op orb8 = Word8.orb
    val op andb8 = Word8.andb

    val op andb = Word.andb (* and op andbL = LargeWord.andb *)
    and op orb  = Word.orb

    val wordToWord8 = Word8.fromLargeWord o Word.toLargeWord
    (*and word8ToWord = Word.fromLargeWord o Word8.toLargeWord*)

    val exp2_16 =        0x10000
    val exp2_31 =        0x80000000: LargeInt.int

    (* Returns true if this a 32-bit machine or if the constant is within 32-bits.
       This is exported to the higher levels.  N.B.  The test for not isX64
       avoids a significant overhead with arbitrary precision arithmetic on
       X86/32. *)
    fun is32bit v = not isX64 orelse ~exp2_31 <= v andalso v < exp2_31

    (* tag a short constant *)
    fun tag c = 2 * c + 1;

    fun is8Bit n = ~ 0x80 <= n andalso n < 0x80
    fun is8BitL (n: LargeInt.int) = ~ 0x80 <= n andalso n < 0x80

    local
        val shift =
            if wordSize = 4
            then 0w2
            else if wordSize = 8
            then 0w3
            else raise InternalError "Invalid word size for x86_32 or x86+64"
    in
        fun wordsToBytes n = n << shift
        and bytesToWords n = n >> shift
    end

    infix 6 addrPlus addrMinus;
  
    (* All indexes into the code vector have type "addrs" *)
    type addrs = Word.word
  
    (* + is defined to add an integer to an address *)
    fun a addrPlus b = a + Word.fromInt b;
      
    (* The difference between two addresses is an integer *)
    fun a addrMinus b = Word.toInt a - Word.toInt b
  
    val addrZero = 0w0;
    val addrLast = wordsToBytes maxAllocation (* A large address. *)
    val addrUnsetLabel = addrLast (* An invalid address *)

    (* The "value" points at the jump instruction, or rather at the
       jump offset part of it.  It is a ref because we may have to change
       it if we have to put in a jump with a 32-bit offset. *)
    datatype jumpFrom =
        Jump8From  of addrs
    |   Jump32From of addrs 
  
    (* This is the list of outstanding labels. *)
    type labList = jumpFrom list
    (* This is the external label type used when constructing operations.
       The ref int is just an identifier for convenience when printing. *)
    datatype label =
        Labels of
        {
            forward: labList ref,
            reverse: addrs ref,
            labId: int ref,
            uses: int ref,
            chain: label option ref
        }

    fun mkLabel() =
        Labels{forward = ref [], reverse=ref addrUnsetLabel, labId = ref 0, uses = ref 0, chain=ref NONE}
   
  (* Constants which are too large to go inline in the code are put in
     a list and put at the end of the code. They are arranged so that
     the garbage collector can find them and change them as necessary.
     A reference to a constant is treated like a forward reference to a
     label. *)

  (* A code list is used to hold a list of code-vectors which must have the
     address of this code-vector put into it. *)

    datatype const =
        WVal of machineWord        (* an existing constant *)
    |   SelfVal (* The address of the start of the code. *)

    and ConstPosn =
        InlineAbsolute      (* The constant is within the code. *)
    |   InlineRelative      (* The constant is within the code but is PC relative (call or jmp). *)
    |   ConstArea of int    (* The constant is in the constant area (64-bit only). *)

    and nonAddressConsts =
        NonAddressReal of machineWord
    |   NonAddressInt of LargeInt.int

  and code = Code of 
    { codeVec:        cseg,           (* This segment is used as a buffer. When the
                                         procedure has been code generated it is
                                         copied into a new segment of the correct size *)
      ic:             addrs ref,      (* Pointer to first free location in "codevec" *)
      constVec:                       (* Constants used in the code *)
           {const: const, addrs: addrs, posn: ConstPosn} list ref,
      numOfConsts:    word ref,        (* size of constVec *)
      nonInlineConsts: int ref,
      nonAddressConstants: {const: nonAddressConsts, addrs: addrs} list ref,
      procName:       string,         (* Name of the procedure. *)
      printAssemblyCode:bool,            (* Whether to print the code when we finish. *)
      printStream:    string->unit,   (* The stream to use *)
      lowLevelOptimise: bool,         (* Whether to do the low-level optimisation pass *)
      profileObject   : machineWord,  (* The profile object for this code. *)
      inAllocation:   bool ref        (* Whether we have an incomplete allocation. *)
    }

    (* Exported functions *)
    fun lowLevelOptimise(Code{lowLevelOptimise, ...}) = lowLevelOptimise

  (* EBP/RBP points to a structure that interfaces to the RTS.  These are
     offsets into that structure.  *)
    val memRegLocalMPointer       = 0 (* Not used in 64-bit *)
    and memRegHandlerRegister     = wordSize
    and memRegLocalMbottom        = 2 * wordSize
    and memRegStackLimit          = 3 * wordSize
    and memRegExceptionPacket     = 4 * wordSize
    and memRegCStackPtr           = 6 * wordSize
    and memRegThreadSelf          = 7 * wordSize
    and memRegStackPtr            = 8 * wordSize
    and memRegHeapOverflowCall    = 10 * wordSize
    and memRegStackOverflowCall   = 11 * wordSize
    and memRegStackOverflowCallEx = 12 * wordSize
    
    (* This can probably be much smaller now. *)
    and memRegSize                = if isX64 then 144 else 56 (* Size of area on the stack. *)

  (* Several operations are not generated immediately but recorded and
     generated later.  Labels (i.e. the destination of a branch) are recorded
     in just_come_from.  Adjustments to the real stack pointer are recorded
     in stack_reset.
     The order in which these "instructions" are assumed to happen is of
     course significant.  If just_come_from is not empty it is assumed to
     have happened before anything else. After that the stack pointer is 
     adjusted and finally the next instruction is executed.
  *)

    val initialCodeSize = 0w15 (* words. Initial size of segment. *)

    (* create and initialise a code segment *)
    fun codeCreate (name : string, profObj, parameters) : code =
    let
        val printStream = PRETTY.getSimplePrinter(parameters, [])
    in
        Code
        { 
            codeVec        = csegMake initialCodeSize, (* a byte array *)
            ic             = ref addrZero,
            constVec       = ref [],
            numOfConsts    = ref 0w0,
            nonInlineConsts = ref 0,
            nonAddressConstants = ref [],
            procName       = name,
            printAssemblyCode = DEBUG.getParameter DEBUG.assemblyCodeTag parameters,
            printStream    = printStream,
            lowLevelOptimise = DEBUG.getParameter DEBUG.lowlevelOptimiseTag parameters,
            profileObject  = profObj,
            inAllocation   = ref false
          }
    end
           

    (* Put 1 unsigned byte at a given offset in the segment. *)
    fun set8u (b, addr, seg) = csegSet (seg, addr,  b)

    (* Put 1 signed byte at a given offset in the segment. *)
    fun set8s (b : int, addr, seg) =
    let
        val a = addr;
        val b' = if b < 0 then b + 0x100 else b;
    in
        csegSet (seg, a, Word8.fromInt b')
    end;

    (* Get 1 unsigned byte from the given offset in the segment. *)
    fun get8u (a: word, seg: cseg) : Word8.word = csegGet (seg, a);

    (* Get 1 signed byte from the given offset in the segment. *)
    fun get8s (a: word, seg: cseg) : int = Word8.toIntX (csegGet (seg, a));
 
    (* Put 4 bytes at a given offset in the segment. *)
    (* b0 is the least significant byte. *)
    fun set4Bytes (b3, b2, b1, b0, addr, seg) =
    let
        val a = addr;
    in
        (* Little-endian *)
        csegSet (seg, a,     b0);
        csegSet (seg, a + 0w1, b1);
        csegSet (seg, a + 0w2, b2);
        csegSet (seg, a + 0w3, b3)
    end;

    (* Put 1 unsigned word at a given offset in the segment. *)
    fun set32u (ival: LargeWord.word, addr: addrs, seg) : unit =
    let
        val b3       = Word8.fromLargeWord (ival >>+ 0w24)
        val b2       = Word8.fromLargeWord (ival >>+ 0w16)
        val b1       = Word8.fromLargeWord (ival >>+ 0w8)
        val b0       = Word8.fromLargeWord ival
    in
        set4Bytes (b3, b2, b1, b0, addr, seg)
    end

    (* Put 1 signed word at a given offset in the segment. *)
    fun set32s (ival: LargeInt.int, addr: addrs, seg) : unit =
        set32u(LargeWord.fromLargeInt ival, addr, seg)

    fun setBytes(_, _, _, 0) = ()
    |   setBytes(seg, ival, offset, count) =
        (
            csegSet(seg, offset, Word8.fromLargeInt(ival mod 256));
            setBytes(seg, ival div 256, offset+0w1, count-1)
        )

    fun setWordU (ival: LargeInt.int, addr: addrs, seg) : unit =
        setBytes(seg, ival, addr, wordSize)
     
    (* Get 1 signed 32 bit word from the given offset in the segment. *)
    fun get32s (a: word, seg: cseg) : LargeInt.int =
    let
        val b0  = Word8.toLargeInt (csegGet (seg, a));
        val b1  = Word8.toLargeInt (csegGet (seg, a + 0w1));
        val b2  = Word8.toLargeInt (csegGet (seg, a + 0w2));
        val b3  = Word8.toLargeInt (csegGet (seg, a + 0w3));
        val b3' = if b3 >= 0x80 then b3 - 0x100 else b3;
        val topHw    = (b3' * 0x100) + b2;
        val bottomHw = (b1 * 0x100) + b0;
    in
        (topHw * exp2_16) + bottomHw
    end
 
    fun get64s (a: word, seg: cseg) : LargeInt.int =
    let
        val b0  = Word8.toLargeInt (csegGet (seg, a));
        val b1  = Word8.toLargeInt (csegGet (seg, a + 0w1));
        val b2  = Word8.toLargeInt (csegGet (seg, a + 0w2));
        val b3  = Word8.toLargeInt (csegGet (seg, a + 0w3));
        val b4  = Word8.toLargeInt (csegGet (seg, a + 0w4));
        val b5  = Word8.toLargeInt (csegGet (seg, a + 0w5));
        val b6  = Word8.toLargeInt (csegGet (seg, a + 0w6));
        val b7  = Word8.toLargeInt (csegGet (seg, a + 0w7));
        val b7' = if b7 >= 0x80 then b7 - 0x100 else b7;
    in
        ((((((((b7' * 0x100 + b6) * 0x100 + b5) * 0x100 + b4) * 0x100 + b3)
             * 0x100 + b2) * 0x100) + b1) * 0x100) + b0
    end

    local
        (* Code-generate a byte. *)
        fun genByte (ival: Word8.word, Code {ic, codeVec, ...}) : unit =
            set8u (ival, !ic, codeVec) before ic := !ic + 0w1
    in
        fun genBytes(l, c) = List.app (fn i => genByte(i, c)) l
    end
    
    fun byteSigned ival =
        if ~0x80 <= ival andalso ival < 0x80
        then Word8.fromInt ival
        else raise InternalError "byteSigned: invalid byte"
    
    (* Convert a large-word value to a little-endian byte sequence. *)
    fun largeWordToBytes(_, 0) = []
    |   largeWordToBytes(ival: LargeWord.word, n) =
            Word8.fromLargeWord ival :: largeWordToBytes(ival >>+ 0w8, n-1)

    fun word32Unsigned(ival: LargeWord.word) = largeWordToBytes(ival, 4)

    fun int32Signed(ival: LargeInt.int) =
        if is32bit ival
        then word32Unsigned(LargeWord.fromLargeInt ival)
        else raise InternalError "int32Signed: invalid word"
    
    (* wordUnsigned is 8 bytes on 64-bits or 4-bytes on 32-bits. *)
    fun wordUnsigned(ival: LargeWord.word) = largeWordToBytes(ival, wordSize)

    (* Add a constant to the list along with its address.  We mustn't put
       the constant directly in the code since at this stage the code is
       simply a byte segment and if we have a garbage collection the value
       won't be updated. *)
    fun addConstToVec (valu: const, posn: ConstPosn,
                       Code{numOfConsts, constVec, ic, nonInlineConsts, ...}, prefixBytes) =
    let
      (* Inline constants are in the body of the code.  Non-inline constants are
         stored in the constant vector at the end of the code.  The value that goes
         in here is the PC-relative offset of the constant. *)
        val realPosn =
            case posn of
                ConstArea _ => (nonInlineConsts := ! nonInlineConsts + 1; ConstArea(!nonInlineConsts))
            |  p => p
        val isInline =
            case posn of ConstArea _ => false | _ => true
    in
	    numOfConsts := ! numOfConsts + 0w1;
        constVec    := {const = valu, addrs = !ic addrPlus List.length prefixBytes, posn = realPosn} :: ! constVec;
        (* We must put a valid tagged integer in here because we might
           get a garbage collection after we have copied this code into
           the new code segment but before we've put in the real constant.
           If this is a relative branch we need to point this at itself.
           Until it is set to the relative offset of the destination it
           needs to contain an address within the code and this could
           be the last instruction. *)
        if isInline andalso wordSize = 8
        then prefixBytes @ wordUnsigned(LargeWord.fromLargeInt(tag 0))
        else prefixBytes @ int32Signed(case posn of InlineRelative => ~5 | _ => tag 0)
    end

    fun addNonAddressConstant(valu: nonAddressConsts, Code{ic, nonAddressConstants, ...}, prefixBytes) =
    (
        nonAddressConstants  := {const = valu, addrs = !ic addrPlus List.length prefixBytes} :: ! nonAddressConstants;
        prefixBytes @ int32Signed(tag 0)
    )

    (* Registers. *)
    datatype genReg = GeneralReg of Word8.word * bool
    and fpReg = FloatingPtReg of Word8.word
    and xmmReg = SSE2Reg of Word8.word
    
    datatype reg =
        GenReg of genReg
    |   FPReg of fpReg
    |   XMMReg of xmmReg

    (* These are the real registers we have.  The AMD extension encodes the
       additional registers through the REX prefix. *)
    val eax = GeneralReg (0w0, false)
    val ecx = GeneralReg (0w1, false)
    val edx = GeneralReg (0w2, false)
    val ebx = GeneralReg (0w3, false)
    val esp = GeneralReg (0w4, false)
    val ebp = GeneralReg (0w5, false)
    val esi = GeneralReg (0w6, false)
    val edi = GeneralReg (0w7, false)
    val r8  = GeneralReg (0w0, true)
    val r9  = GeneralReg (0w1, true)
    val r10 = GeneralReg (0w2, true)
    val r11 = GeneralReg (0w3, true)
    val r12 = GeneralReg (0w4, true)
    val r13 = GeneralReg (0w5, true)
    val r14 = GeneralReg (0w6, true)
    val r15 = GeneralReg (0w7, true)

    (* Floating point "registers".  Actually entries on the floating point stack.
       The X86 has a floating point stack with eight entries. *)
    val fp0 = FloatingPtReg 0w0
    and fp1 = FloatingPtReg 0w1
    and fp2 = FloatingPtReg 0w2
    and fp3 = FloatingPtReg 0w3
    and fp4 = FloatingPtReg 0w4
    and fp5 = FloatingPtReg 0w5
    and fp6 = FloatingPtReg 0w6
    and fp7 = FloatingPtReg 0w7

    (* SSE2 Registers.  These are used for floating point in 64-bity mode.
       We only use XMM0-6 because the others are callee save and we don't
       currently save them. *)
    val xmm0 = SSE2Reg 0w0
    and xmm1 = SSE2Reg 0w1
    and xmm2 = SSE2Reg 0w2
    and xmm3 = SSE2Reg 0w3
    and xmm4 = SSE2Reg 0w4
    and xmm5 = SSE2Reg 0w5
    and xmm6 = SSE2Reg 0w6

    val regClosure  = edx (* Addr. of closure for fn. call goes here. *)

    fun getReg (GeneralReg r) = r
    fun mkReg  n      = GeneralReg n  (* reg.up   *)
  
    (* The maximum size of the register vectors and masks.  Although the
       X86/32 has a floating point stack with eight entries it's much simpler
       to treat it as having seven "real" registers.  Items are pushed to the
       stack and then stored and popped into the current location.  It may be
       possible to improve the code by some peephole optimisation. *)
    val regs = 30 (* Include the X86/64 registers even if this is 32-bit. *)

    (* The nth register (counting from 0). *)
    (* Profiling shows that applying the constructors here creates a lot of
       garbage.  Create the entries once and then use vector indexing instead. *)
    local
        fun regN i =
            if i < 8
            then GenReg(GeneralReg(Word8.fromInt i, false))
            else if i < 16
            then GenReg(GeneralReg(Word8.fromInt(i-8), true))
            else if i < 23
            then FPReg(FloatingPtReg(Word8.fromInt(i-16)))
            else XMMReg(SSE2Reg(Word8.fromInt(i-23)))
        val regVec = Vector.tabulate(regs, regN)
    in
        fun regN i = Vector.sub(regVec, i) handle Subscript => raise InternalError "Bad register number"
    end
 
    (* The number of the register. *)
    fun nReg(GenReg(GeneralReg(r, false))) = Word8.toInt r
    |   nReg(GenReg(GeneralReg(r, true))) = Word8.toInt r + 8
    |   nReg(FPReg(FloatingPtReg r)) = Word8.toInt r + 16
    |   nReg(XMMReg(SSE2Reg r)) = Word8.toInt r + 23
        
    datatype opsize = SZByte | SZWord | SZDWord | SZQWord
    
    val sz32_64 = if isX64 then SZQWord else SZDWord

    fun genRegRepr(GeneralReg (0w0, false), SZByte) = "al"
    |   genRegRepr(GeneralReg (0w1, false), SZByte) = "cl"
    |   genRegRepr(GeneralReg (0w2, false), SZByte) = "dl"
    |   genRegRepr(GeneralReg (0w3, false), SZByte) = "bl"
    |   genRegRepr(GeneralReg (0w4, false), SZByte) = "ah" (* TODO: May be different if there's a rex code *)
    |   genRegRepr(GeneralReg (0w5, false), SZByte) = "ch"
    |   genRegRepr(GeneralReg (0w6, false), SZByte) = "dh"
    |   genRegRepr(GeneralReg (0w7, false), SZByte) = "bh"
    |   genRegRepr(GeneralReg (reg, true),  SZByte) = "r" ^ Int.toString(Word8.toInt reg +8) ^ "b"
    |   genRegRepr(GeneralReg (0w0, false), SZDWord) = "eax"
    |   genRegRepr(GeneralReg (0w1, false), SZDWord) = "ecx"
    |   genRegRepr(GeneralReg (0w2, false), SZDWord) = "edx"
    |   genRegRepr(GeneralReg (0w3, false), SZDWord) = "ebx"
    |   genRegRepr(GeneralReg (0w4, false), SZDWord) = "esp"
    |   genRegRepr(GeneralReg (0w5, false), SZDWord) = "ebp"
    |   genRegRepr(GeneralReg (0w6, false), SZDWord) = "esi"
    |   genRegRepr(GeneralReg (0w7, false), SZDWord) = "edi"
    |   genRegRepr(GeneralReg (reg, true),  SZDWord) = "r" ^ Int.toString(Word8.toInt reg +8) ^ "d"
    |   genRegRepr(GeneralReg (0w0, false), SZQWord) = "rax"
    |   genRegRepr(GeneralReg (0w1, false), SZQWord) = "rcx"
    |   genRegRepr(GeneralReg (0w2, false), SZQWord) = "rdx"
    |   genRegRepr(GeneralReg (0w3, false), SZQWord) = "rbx"
    |   genRegRepr(GeneralReg (0w4, false), SZQWord) = "rsp"
    |   genRegRepr(GeneralReg (0w5, false), SZQWord) = "rbp"
    |   genRegRepr(GeneralReg (0w6, false), SZQWord) = "rsi"
    |   genRegRepr(GeneralReg (0w7, false), SZQWord) = "rdi"
    |   genRegRepr(GeneralReg (reg, true),  SZQWord) = "r" ^ Int.toString(Word8.toInt reg +8)
    |   genRegRepr(GeneralReg (0w0, false), SZWord) = "ax"
    |   genRegRepr(GeneralReg (0w1, false), SZWord) = "cx"
    |   genRegRepr(GeneralReg (0w2, false), SZWord) = "dx"
    |   genRegRepr(GeneralReg (0w3, false), SZWord) = "bx"
    |   genRegRepr(GeneralReg (0w4, false), SZWord) = "sp"
    |   genRegRepr(GeneralReg (0w5, false), SZWord) = "bp"
    |   genRegRepr(GeneralReg (0w6, false), SZWord) = "si"
    |   genRegRepr(GeneralReg (0w7, false), SZWord) = "di"
    |   genRegRepr(GeneralReg (reg, true),  SZWord) = "r" ^ Int.toString(Word8.toInt reg +8) ^ "w"
    |   genRegRepr _ = "unknown" (* Suppress warning because word values are not exhaustive. *)

    and fpRegRepr(FloatingPtReg n) = "fp" ^ Word8.toString n
    
    and xmmRegRepr(SSE2Reg n) = "xmm" ^ Word8.toString n

    fun regRepr(GenReg r) = genRegRepr (r, sz32_64)
    |   regRepr(FPReg r) = fpRegRepr r
    |   regRepr(XMMReg r) = xmmRegRepr r

    (* Install a pretty printer.  This is simply for when this code is being
       run under the debugger.  N.B. We need PolyML.PrettyString here. *)
    val () = PolyML.addPrettyPrinter(fn _ => fn _ => fn r => PolyML.PrettyString(regRepr r))
    
    datatype argType = ArgGeneral | ArgFP

    structure RegSet =
    struct
        (* Implement a register set as a bit mask. *)
        datatype regSet = RegSet of word
        fun singleton r = RegSet(0w1 << Word.fromInt(nReg r))
        fun regSetUnion(RegSet r1, RegSet r2) = RegSet(Word.orb(r1, r2))
        fun regSetIntersect(RegSet r1, RegSet r2) = RegSet(Word.andb(r1, r2))

        local
            fun addReg(acc, n) =
                if n = regs then acc else addReg(regSetUnion(acc, singleton(regN n)), n+1)
        in
            val allRegisters = addReg(RegSet 0w0, 0)
        end

        val noRegisters = RegSet 0w0

        fun inSet(r, rs) = regSetIntersect(singleton r, rs) <> noRegisters
        
        fun regSetMinus(RegSet s1, RegSet s2) = RegSet(Word.andb(s1, Word.notb s2))
        
        val listToSet = List.foldl (fn(r, rs) => regSetUnion(singleton r, rs)) noRegisters

        val generalRegisters = (* Registers checked by the GC. *)
            if isX64
            then listToSet(map GenReg [eax, ecx, edx, ebx, esi, edi, r8, r9, r10, r11, r12, r13, r14])
            else listToSet(map GenReg [eax, ecx, edx, ebx, esi, edi])
        
        val floatingPtRegisters =
            listToSet(map FPReg [fp0, fp1, fp2, fp3, fp4, fp5, fp6(*, fp7*)])

        val sse2Registers =
            listToSet(map XMMReg [xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6])

        fun isAllRegs rs = rs = allRegisters

        fun setToList (RegSet regSet)=
        let
            fun testBit (n, bit, res) =
                if n = regs
                then res
                else testBit(n+1, bit << 0w1, 
                        if (regSet andb bit) <> 0w0
                        then regN n :: res else res)
        in
            testBit(0, 0w1, [])
        end

        val cardinality = List.length o setToList

        (* Choose one of the set.  This chooses the least value which means that
           the ordering of the registers is significant.  This is a hot-spot
           so is coded directly with the word operations. *)
        fun oneOf(RegSet regSet) =
        let
            fun find(n, bit) =
                if n = Word.fromInt regs then raise InternalError "oneOf: empty"
                else if Word.andb(bit, regSet) <> 0w0 then n
                else find(n+0w1, Word.<<(bit, 0w1))
        in
            regN(Word.toInt(find(0w0, 0w1)))
        end
        
        fun regSetRepr regSet =
        let
            val regs = setToList regSet
        in
            "[" ^ String.concatWith "," (List.map regRepr regs) ^ "]"
        end
        
        (* Install a pretty printer for when this code is being debugged. *)
        val () = PolyML.addPrettyPrinter(fn _ => fn _ => fn r => PolyML.PrettyString(regSetRepr r))
     end

    open RegSet

    datatype arithOp = ADD | OR (*|ADC | SBB*) | AND | SUB | XOR | CMP
  
    fun arithOpToWord ADD = 0w0: Word8.word
    |   arithOpToWord OR  = 0w1
    |   arithOpToWord AND = 0w4
    |   arithOpToWord SUB = 0w5
    |   arithOpToWord XOR = 0w6
    |   arithOpToWord CMP = 0w7

    fun arithOpRepr ADD = "Add"
    |   arithOpRepr OR  = "Or"
    |   arithOpRepr AND = "And"
    |   arithOpRepr SUB = "Sub"
    |   arithOpRepr XOR = "Xor"
    |   arithOpRepr CMP = "Cmp"

    datatype shiftType = SHL | SHR | SAR

    fun shiftTypeToWord SHL = 0w4: Word8.word
    |   shiftTypeToWord SHR = 0w5
    |   shiftTypeToWord SAR = 0w7

    fun shiftTypeRepr SHL = "Shift Left Logical"
    |   shiftTypeRepr SHR = "Shift Right Logical"
    |   shiftTypeRepr SAR = "Shift Right Arithemetic"

    datatype repOps = CMPSB | MOVSB | MOVSL | STOSB | STOSL
    
    fun repOpsToWord CMPSB = 0wxa6: Word8.word
    |   repOpsToWord MOVSB = 0wxa4
    |   repOpsToWord MOVSL = 0wxa5
    |   repOpsToWord STOSB = 0wxaa
    |   repOpsToWord STOSL = 0wxab

    fun repOpsRepr CMPSB = "CompareBytes"
    |   repOpsRepr MOVSB = "MoveBytes"
    |   repOpsRepr MOVSL = "MoveWords"
    |   repOpsRepr STOSB = "StoreBytes"
    |   repOpsRepr STOSL = "StoreWords"

    datatype fpOps = FADD | FMUL | FCOM | FCOMP | FSUB | FSUBR | FDIV | FDIVR

    fun fpOpToWord FADD  = 0w0: Word8.word
    |   fpOpToWord FMUL  = 0w1
    |   fpOpToWord FCOM  = 0w2
    |   fpOpToWord FCOMP = 0w3
    |   fpOpToWord FSUB  = 0w4
    |   fpOpToWord FSUBR = 0w5
    |   fpOpToWord FDIV  = 0w6
    |   fpOpToWord FDIVR = 0w7

    fun fpOpRepr FADD  = "FPAdd"
    |   fpOpRepr FMUL  = "FPMultiply"
    |   fpOpRepr FCOM  = "FPCompare"
    |   fpOpRepr FCOMP = "FPCompareAndPop"
    |   fpOpRepr FSUB  = "FPSubtract"
    |   fpOpRepr FSUBR = "FPReverseSubtract"
    |   fpOpRepr FDIV  = "FPDivide"
    |   fpOpRepr FDIVR = "FPReverseDivide"

    datatype fpUnaryOps = FCHS | FABS | FLD1 | FLDZ
    
    fun fpUnaryToWords FCHS   = {rm=0w0:Word8.word, nnn=0w4: Word8.word}
    |   fpUnaryToWords FABS   = {rm=0w1, nnn=0w4}
    |   fpUnaryToWords FLD1   = {rm=0w0, nnn=0w5}
    |   fpUnaryToWords FLDZ   = {rm=0w6, nnn=0w5}

    fun fpUnaryRepr FCHS   = "FPChangeSign"
    |   fpUnaryRepr FABS   = "FPAbs"
    |   fpUnaryRepr FLD1   = "FPLoadOne"
    |   fpUnaryRepr FLDZ   = "FPLoadZero"

    datatype branchOps = JO | JNO | JE | JNE | JL | JGE | JLE | JG | JB | JNB | JNA | JA | JP | JNP

    fun branchOpToWord JO   = 0wx0: Word8.word
    |   branchOpToWord JNO  = 0wx1
    |   branchOpToWord JB   = 0wx2
    |   branchOpToWord JNB  = 0wx3
    |   branchOpToWord JE   = 0wx4
    |   branchOpToWord JNE  = 0wx5
    |   branchOpToWord JNA  = 0wx6
    |   branchOpToWord JA   = 0wx7
    |   branchOpToWord JP   = 0wxa
    |   branchOpToWord JNP  = 0wxb
    |   branchOpToWord JL   = 0wxc
    |   branchOpToWord JGE  = 0wxd
    |   branchOpToWord JLE  = 0wxe
    |   branchOpToWord JG   = 0wxf
 
    fun branchOpRepr JO = "JumpOverflow"
    |   branchOpRepr JNO = "JumpNotOverflow"
    |   branchOpRepr JE = "JumpEqual"
    |   branchOpRepr JNE = "JumpNotEqual"
    |   branchOpRepr JL = "JumpLess"
    |   branchOpRepr JGE = "JumpGreaterOrEqual"
    |   branchOpRepr JLE = "JumpLessOrEqual"
    |   branchOpRepr JG = "JumpGreater"
    |   branchOpRepr JB = "JumpBefore"
    |   branchOpRepr JNB= "JumpNotBefore"
    |   branchOpRepr JNA = "JumpNotAfter"
    |   branchOpRepr JA = "JumpAfter"
    |   branchOpRepr JP = "JumpParity"
    |   branchOpRepr JNP = "JumpNoParity"

    datatype sse2Operations =
        SSE2Move | SSE2Comp | SSE2Add | SSE2Sub | SSE2Mul | SSE2Div | SSE2Xor |
        SSE2And | SSE2MoveSingle | SSE2DoubleToFloat
    
    fun sse2OpRepr SSE2Move = "SSE2Move"
    |   sse2OpRepr SSE2Comp = "SSE2Comp"
    |   sse2OpRepr SSE2Add  = "SSE2Add"
    |   sse2OpRepr SSE2Sub  = "SSE2Sub"
    |   sse2OpRepr SSE2Mul  = "SSE2Mul"
    |   sse2OpRepr SSE2Div  = "SSE2Div"
    |   sse2OpRepr SSE2Xor  = "SSE2Xor"
    |   sse2OpRepr SSE2And  = "SSE2And"
    |   sse2OpRepr SSE2MoveSingle = "SSE2MoveSingle"
    |   sse2OpRepr SSE2DoubleToFloat = "SSE2DoubleToFloat"

    (* Primary opCodes.  N.B. only opCodes actually used are listed here.
       If new instruction are added check they will be handled by the
       run-time system in the event of trap. *)
    datatype opCode =
        Group1_8_A
    |   Group1_32_A
    |   Group1_8_a
    |   JMP_8
    |   JMP_32
    |   CALL_32
    |   MOVL_A_R
    |   MOVL_R_A
    |   MOVL_R_A16
    |   MOVL_R_A32
    |   MOVB_R_A of {forceRex: bool}
    |   PUSH_R of Word8.word
    |   POP_R  of Word8.word
    |   Group5
    |   NOP
    |   LEAL
    |   MOVL_32_64_R of Word8.word
    |   MOVL_32_A
    |   MOVB_8_A
    |   POP_A
    |   RET
    |   RET_16
    |   CondJump of branchOps
    |   CondJump32 of branchOps
    |   Arith of arithOp * Word8.word
    |   Group3_A
    |   Group3_a
    |   Group2_8_A
    |   Group2_CL_A
    |   Group2_1_A
    |   PUSH_8
    |   PUSH_32
    |   TEST_ACC8
    |   LOCK_XADD
    |   FPESC of Word8.word
    |   XCHNG
    |   REP (* Rep prefix *)
    |   MOVZB (* Needs escape code. *)
    |   MOVZW (* Needs escape code. *)
    |   MOVL_A_R32 (* As MOVL_A_R but without RAX.W *)
    |   IMUL (* Needs escape code. *)
    |   SSE2StoreSingle (* movss with memory destination - needs escape sequence. *)
    |   SSE2StoreDouble (* movsd with memory destination - needs escape sequence. *)
    |   CQO_CDQ (* Sign extend before divide.. *)
    |   SSE2Ops of sse2Operations (* SSE2 instructions. *)
    |   CVTSI2SD
    |   HLT     (* End of code marker. *)

    fun opToInt Group1_8_A    =  0wx83
    |   opToInt Group1_32_A   =  0wx81
    |   opToInt Group1_8_a    =  0wx80
    |   opToInt JMP_8         =  0wxeb
    |   opToInt JMP_32        =  0wxe9
    |   opToInt CALL_32       =  0wxe8
    |   opToInt MOVL_A_R      =  0wx8b
    |   opToInt MOVL_R_A      =  0wx89
    |   opToInt MOVL_R_A16    =  0wx89 (* Also has an OPSIZE prefix. *)
    |   opToInt MOVL_R_A32    =  0wx89 (* Suppresses the REX.W prefix. *)
    |   opToInt (MOVB_R_A _)  =  0wx88
    |   opToInt (PUSH_R reg)  =  0wx50 + reg
    |   opToInt (POP_R  reg)  =  0wx58 + reg
    |   opToInt Group5        =  0wxff
    |   opToInt NOP           =  0wx90
    |   opToInt LEAL          =  0wx8d
    |   opToInt (MOVL_32_64_R reg) =  0wxb8 + reg
    |   opToInt MOVL_32_A     =  0wxc7
    |   opToInt MOVB_8_A      =  0wxc6
    |   opToInt POP_A         =  0wx8f
    |   opToInt RET           = 0wxc3
    |   opToInt RET_16        = 0wxc2
    |   opToInt (CondJump opc) = 0wx70 + branchOpToWord opc
    |   opToInt (CondJump32 opc) = 0wx80 + branchOpToWord opc (* Needs 0F prefix *)
    |   opToInt (Arith (ao,dw)) = arithOpToWord ao * 0w8 + dw
    |   opToInt Group3_A      = 0wxf7
    |   opToInt Group3_a      = 0wxf6
    |   opToInt Group2_8_A    = 0wxc1
    |   opToInt Group2_1_A    = 0wxd1
    |   opToInt Group2_CL_A   = 0wxd3
    |   opToInt PUSH_8        = 0wx6a
    |   opToInt PUSH_32       = 0wx68
    |   opToInt TEST_ACC8     = 0wxa8
    |   opToInt LOCK_XADD     = 0wxC1 (* Needs lock and escape prefixes. *)
    |   opToInt (FPESC n)     = 0wxD8 orb8 n
    |   opToInt XCHNG         = 0wx87
    |   opToInt REP           = 0wxf3
    |   opToInt MOVZB         = 0wxb6 (* Needs escape code. *)
    |   opToInt MOVZW         = 0wxb7 (* Needs escape code. *)
    |   opToInt MOVL_A_R32    = 0wx8b
    |   opToInt IMUL          = 0wxaf (* Needs escape code. *)
    |   opToInt SSE2StoreSingle     = 0wx11 (* Needs F3 0F escape. *)
    |   opToInt SSE2StoreDouble     = 0wx11 (* Needs F2 0F escape. *)
    |   opToInt CQO_CDQ       = 0wx99
    |   opToInt (SSE2Ops SSE2Move) = 0wx10 (* Needs F2 0F escape. *)
    |   opToInt (SSE2Ops SSE2Comp) = 0wx2E (* Needs 66 0F escape. *)
    |   opToInt (SSE2Ops SSE2Add)  = 0wx58 (* Needs F2 0F escape. *)
    |   opToInt (SSE2Ops SSE2Sub)  = 0wx5c (* Needs F2 0F escape. *)
    |   opToInt (SSE2Ops SSE2Mul)  = 0wx59 (* Needs F2 0F escape. *)
    |   opToInt (SSE2Ops SSE2Div)  = 0wx5e (* Needs F2 0F escape. *)
    |   opToInt (SSE2Ops SSE2And)  = 0wx54 (* Needs 66 0F escape. *)
    |   opToInt (SSE2Ops SSE2Xor)  = 0wx57 (* Needs 66 0F escape. *)
    |   opToInt (SSE2Ops SSE2MoveSingle)  = 0wx5A (* Needs F3 0F escape. *)
    |   opToInt (SSE2Ops SSE2DoubleToFloat)  = 0wx5A (* Needs F2 0F escape. *)
    |   opToInt CVTSI2SD      = 0wx2a (* Needs F2 0F escape. *)
    |   opToInt HLT           = 0wxf4

    datatype mode =
        Based0   (* mod = 0 *)
    |   Based8   (* mod = 1 *)
    |   Based32  (* mod = 2 *)
    |   Register (* mod = 3 *) ;

    (* Put together the three fields which make up the mod r/m byte. *)
    fun modrm (md : mode, rg: Word8.word, rm : Word8.word) : Word8.word =
    let
        val _ = if rg > 0w7 then raise InternalError "modrm: bad rg" else ()
        val _ = if rm > 0w7 then raise InternalError "modrm: bad rm" else ()
        val modField: Word8.word = 
            case md of 
                Based0   => 0w0
            |   Based8   => 0w1
            |   Based32  => 0w2
            |   Register => 0w3
    in
        (modField <<- 0w6) orb8 (rg <<- 0w3) orb8 rm
    end

    (* REX prefix *)
    fun rex {w,r,x,b} =
        0wx40 orb8 (if w then 0w8 else 0w0) orb8 (if r then 0w4 else 0w0) orb8
            (if x then 0w2 else 0w0) orb8 (if b then 0w1 else 0w0)

    (* The X86 has the option to include an index register and to scale it. *)
    datatype indexType =
        NoIndex | Index1 of genReg | Index2 of genReg | Index4 of genReg | Index8 of genReg

    (* Put together the three fields which make up the s-i-b byte. *)
    fun sib (s : indexType, b : genReg option) : Word8.word =
    let
        val sizeField =
            case s of
                NoIndex  => 0w4 <<- 0w3 (* No index reg. *)
            |   Index1 i => (0w0 <<- 0w6) orb8 (#1 (getReg i) <<- 0w3)
            |   Index2 i => (0w1 <<- 0w6) orb8 (#1 (getReg i) <<- 0w3)
            |   Index4 i => (0w2 <<- 0w6) orb8 (#1 (getReg i) <<- 0w3)
            |   Index8 i => (0w3 <<- 0w6) orb8 (#1 (getReg i) <<- 0w3)
        val baseField =
            case b of SOME r => #1 (getReg r) | NONE => 0w5 (* No base *)
    in
       sizeField orb8 baseField
    end

    (* Makes a new label. *)
    fun makeLongLabel (addr: addrs, _) : jumpFrom = Jump32From addr

    (* Fix up branches by patching the offset. *)
    fun fixup (labs, Code{codeVec=cseg, ic, ...}) =
    let
        fun fixBranch(Jump8From addr) =
            let
                val offset = get8s(addr, cseg)
                val diff = (!ic addrMinus addr) - 1
            in
                if offset <> 0
                then raise InternalError "fixup: jump already patched"
                else set8s (diff, addr, cseg)
            end
        |   fixBranch(Jump32From addr) =
            let
                val offset = get32s (addr, cseg)
                val diff = (!ic addrMinus addr) - 4
            in
                if offset <> 0
                then raise InternalError "fixup: jump already patched"
                else set32s (LargeInt.fromInt diff, addr, cseg)
            end
    in
        List.app fixBranch labs
    end
    
    (* Lock, Opsize and REPNE prefixes come before the REX. *)
    fun opcodePrefix LOCK_XADD                  = [0wxF0] (* Requires LOCK prefix. *)
    |   opcodePrefix MOVL_R_A16                 = [0wx66] (* Requires OPSIZE prefix. *)
    |   opcodePrefix SSE2StoreSingle            = [0wxf3]
    |   opcodePrefix SSE2StoreDouble            = [0wxf2]
    |   opcodePrefix(SSE2Ops SSE2Comp)          = [0wx66]
    |   opcodePrefix(SSE2Ops SSE2And)           = [0wx66]
    |   opcodePrefix(SSE2Ops SSE2Xor)           = [0wx66]
    |   opcodePrefix(SSE2Ops SSE2MoveSingle)    = [0wxf3]
    |   opcodePrefix(SSE2Ops _)                 = [0wxf2]
    |   opcodePrefix CVTSI2SD                   = [0wxf2]
    |   opcodePrefix _                          = []

    (* A few instructions require an escape.  Escapes come after the REX. *)
    fun escapePrefix MOVZB                      = [0wx0f]
    |   escapePrefix MOVZW                      = [0wx0f]
    |   escapePrefix LOCK_XADD                  = [0wx0f]
    |   escapePrefix IMUL                       = [0wx0f]
    |   escapePrefix(CondJump32 _)              = [0wx0f]
    |   escapePrefix SSE2StoreSingle            = [0wx0f]
    |   escapePrefix SSE2StoreDouble            = [0wx0f]
    |   escapePrefix(SSE2Ops SSE2Comp)          = [0wx0f]
    |   escapePrefix(SSE2Ops SSE2And)           = [0wx0f]
    |   escapePrefix(SSE2Ops SSE2Xor)           = [0wx0f]
    |   escapePrefix(SSE2Ops SSE2MoveSingle)    = [0wx0f]
    |   escapePrefix(SSE2Ops _)                 = [0wx0f]
    |   escapePrefix CVTSI2SD                   = [0wx0f]
    |   escapePrefix _                          = []

    (* Generate an opCode byte after doing any pending operations. *)
    fun opCodeBytes(opb:opCode, rx) =
    let
        val rexByte = 
            case rx of
                NONE => []
            |   SOME rxx =>
                if isX64 then [rex rxx]
                else raise InternalError "opCodeBytes: rex prefix in 32 bit mode";
    in
        opcodePrefix opb @ rexByte @ escapePrefix opb @ [opToInt opb]
    end

    (* Generate an effective address. *)
    fun eACode (offset: LargeInt.int, rb: Word8.word, r: Word8.word) =
    let
        val offsetCode =
            (* don't generate [ebp] (use [ebp+0]) 'cos it doesn't exist! *)
            if offset = 0 andalso rb <> 0w5 
            then Based0  (* no disp field *)
            else if is8BitL offset
            then Based8  (* use 8-bit disp field *)
            else Based32 (* use 32-bit disp field *)
        val modRmbytes =
            if rb = 0w4 (* Code for esp and r12 *)
            then (* Need to use s-i-b byte. *)
            let
                (* Normally we will have a non-zero offset for esp.  The
                   exception is computing the maximum stack in the prelude. *)
                val mdrmByte = modrm (offsetCode, r, 0w4 (* use SIB *))
                val sibByte = sib(NoIndex, SOME esp)
            in
                [mdrmByte, sibByte]
            end
            else [modrm(offsetCode, r, rb)]

        val offsetBytes =
            (* generate the disp field (if any) *)
            case offsetCode of
                Based8  => [byteSigned(LargeInt.toInt offset)]
            |   Based32 => int32Signed offset
            |   _       => []
    in
        modRmbytes @ offsetBytes
    end

    fun rexByte(opb, rrX, rbX, riX) =
    let
        (* We need a rex prefix if we need to set the length to 64-bit. *)
        val need64bit =
            case opb of
                Group1_8_A => isX64 (* Arithmetic operations - must be 64-bit *)
            |   Group1_32_A => isX64 (* Arithmetic operations - must be 64-bit *)
            |   Group2_1_A => isX64 (* 1-bit shifts - must be 64-bit *)
            |   Group2_8_A => isX64 (* n-bit shifts - must be 64-bit *)
            |   Group2_CL_A => isX64 (* Shifts by value in CL *)
            |   Group3_A => isX64 (* Test, Not, Mul etc. *)
            |   Arith _ => isX64
            |   MOVL_A_R => isX64 (* Needed *)
            |   MOVL_R_A => isX64 (* Needed *)
            |   XCHNG => isX64
            |   LEAL => isX64 (* Needed to ensure the result is 64-bits *)
            |   MOVZB => isX64 (* Needed to ensure the result is 64-bits *)
            |   MOVZW => isX64 (* Needed to ensure the result is 64-bits *)
            |   MOVL_32_64_R _ => isX64 (* Needed *)
            |   MOVL_32_A => isX64 (* Needed *)
            |   IMUL => isX64 (* Needed to ensure the result is 64-bits *)
            |   LOCK_XADD => isX64 (* Needed to ensure the result is 64-bits *)
            |   CQO_CDQ => isX64 (* It's only CQO if there's a Rex prefix. *)
            |   CVTSI2SD => isX64 (* This affects the size of the integer source. *)
            (* Group5 - We only use 2/4/6 and they don't need prefix *)
            |   _ => false
        (* If we are using MOVB_R_A with SIL or DIL we need to force a REX prefix.
           That's only possible in 64-bit mode. *)
        val forceRex =
            case opb of
                MOVB_R_A {forceRex=true} => (* This is allowed in X86/64 but not in X86/32. *)
                    if isX64
                    then true
                    else raise InternalError "rexByte: MOVB_R_A accessing low order byte of ESI/EDI"
            |   _ => false
    in
        if need64bit orelse rrX orelse rbX orelse riX orelse forceRex
        then [rex{w=need64bit, r=rrX, b=rbX, x = riX}]
        else []
    end

    local
        (* Generate a opcode plus a modrm byte.  *)
        fun opEAGen(opb:opCode, offset: LargeInt.int, rb: genReg, (rrC, rrX)) =
        let
            val (rbC, rbX) = getReg rb
        in
            (* Any lock or opsize prefix comes before any REX prefix. *)
            opcodePrefix opb @ rexByte(opb, rrX, rbX, false) @ escapePrefix opb @ [opToInt opb] @
                eACode(offset, rbC, rrC)
        end
    in
        fun opEA(opb, offset, rb, r) = opEAGen(opb, offset, rb, getReg r)
        and mMXEA(opb:opCode, offset: LargeInt.int, rb: genReg, SSE2Reg rrC) =
            opEAGen(opb, offset, rb, (rrC, false))
        (* Generate a opcode plus a second modrm byte but where the "register" field in
           the modrm byte is actually a code.  *)
        and opPlus2(opb, offset, rb, op2) = opEAGen(opb, offset, rb, (op2, false))
    end

    (* Register/register operation. *)
    fun opReg(opb:opCode, rd: genReg, rs: genReg) =
    let
        val (rbC, rbX) = getReg rs
        val (rrC, rrX) = getReg rd
        val pref = opcodePrefix opb (* Any opsize or lock prefix. *)
        val rex = rexByte(opb, rrX, rbX, false)
        val esc = escapePrefix opb (* Generate the ESCAPE code if needed. *)
        val opc = opToInt opb
        val mdrm = modrm(Register, rrC, rbC)
    in
        pref @ rex @ esc @ [opc, mdrm]
    end

    fun opRegPlus2(opb:opCode, rd: genReg, op2: Word8.word) =
    let
        val (rrC, rrX) = getReg rd
        val rex = rexByte(opb, false, rrX, false)
        val opc = opToInt opb
        val mdrm = modrm(Register, op2, rrC)
    in
        rex @ [opc, mdrm]
    end

    local
        (* Similar to genEA, but used when there is an index register.
           rb may be NONE if no base register is required (used
           with leal to tag values). *)
        fun opIndexedGen (opb:opCode, offset: LargeInt.int, rb: genReg option, ri: indexType, (rrC, rrX)) =
        let
            val (rbC, rbX) = case rb of NONE => (0w0, false) | SOME rb => getReg rb

            val (_, riX) = 
                case ri of
                    (* We should call a non-indexed function if there is no index. *)
                    NoIndex  => raise InternalError "opIndexedGen: No index"
                |   Index1 i => getReg i
                |   Index2 i => getReg i
                |   Index4 i => getReg i
                |   Index8 i => getReg i

            val (offsetCode, basefield) =
                case rb of
                    NONE => (Based0, NONE (* no base register *))
                |   SOME rb =>
                    let
                        val base =
                            if offset = 0 andalso rbC <> 0wx5
                            then Based0    (* no disp field *)
                            else if is8BitL offset
                            then Based8   (* use 8-bit disp field *)
                            else Based32; (* use 32-bit disp field *)
                    in
                        (base, SOME rb)
                    end

            val pref = opcodePrefix opb (* Any opsize or lock prefix. *)
            val rex = rexByte(opb, rrX, rbX, riX);
            val esc = escapePrefix opb
            val opc = opToInt opb
            val mdrm = modrm(offsetCode, rrC, 0w4 (* s-i-b *))
            val sibByte = sib(ri, basefield)
    
            (* generate the disp field (if any) *)
            val dispField =
                case (offsetCode, rb) of
                    (Based8, _)  => [Word8.fromLargeInt offset]
                |   (Based32, _) => int32Signed offset
                |   (_, NONE)    => (* 32 bit absolute used as base *) int32Signed offset
                |    _ => []
        in
            pref @ rex @ esc @ [opc, mdrm, sibByte] @ dispField
        end
    in
        fun opIndexed (opb, offset, rb, ri, rd) =
           opIndexedGen(opb, offset, rb, ri, getReg rd) 
        and mMXIndexed(opb, offset, rb, ri, SSE2Reg rrC) =
            opIndexedGen(opb, offset, SOME rb, ri, (rrC, false))
        and opIndexedPlus2(opb, offset, rb, ri, op2) =
            opIndexedGen(opb, offset, SOME rb, ri, (op2, false))
    end

    fun immediateOperand (opn: arithOp, rd: genReg, imm: LargeInt.int, cvec) =
    if is8BitL imm
    then (* Can use one byte immediate *)
        opRegPlus2(Group1_8_A, rd, arithOpToWord opn) @ [Word8.fromLargeInt imm]
    else if is32bit imm
    then (* Need 32 bit immediate. *)
        opRegPlus2(Group1_32_A, rd, arithOpToWord opn) @ int32Signed imm
    else (* It won't fit in the immediate; put it in the non-address area. *)
    let
        val (rc, rx) = getReg rd
        val opb = opCodeBytes(Arith (opn, 0w3 (* r/m to reg *)), SOME{w=true, r=rx, b=false, x = false})
        val mdrm = modrm (Based0, rc, 0w5 (* PC-relative *))
    in
        addNonAddressConstant(NonAddressInt imm, cvec, opb @ [mdrm])
    end
    
    fun arithOpReg(opn: arithOp, rd: genReg, rs: genReg) = opReg (Arith (opn, 0w3 (* r/m to reg *)), rd, rs)

    (* Called when we have a memory operand and a constant that is an address.
       This is either a move or a comparison. *)
    fun memoryConstant (cnstnt, opcode, arithOp, offset, rb, ri, cvec) =
    let
        val opb =
            case ri of
                NoIndex => opPlus2 (opcode, offset, rb, arithOp)
            |   ri => opIndexed(opcode, offset, SOME rb, ri, mkReg(arithOp, false))
    in
        addConstToVec(WVal cnstnt, InlineAbsolute, cvec, opb)
    end

    type handlerLab = addrs ref

    datatype callKinds =
        Recursive           (* The function calls itself. *)
    |   ConstantCode of machineWord (* A function that doesn't need a closure *)
    |   FullCall            (* Full closure call *)
    |   DirectReg of genReg  (* Currently used within ForeignCall to call the RTS *)

    fun floatingPtOp{escape, md, nnn, rm} =
        opCodeBytes(FPESC escape, NONE) @ [(md <<- 0w6) orb8 (nnn <<- 0w3) orb8 rm]

    datatype trapEntries =
        StackOverflowCall
    |   StackOverflowCallEx
    |   HeapOverflowCall

    (* RTS call.  We need to save any registers that may contain addresses to the stack.
       All the registers are preserved but not seen by the GC. *)
    fun rtsCall(rtsEntry, regSet) =
        let
            val entry =
                case rtsEntry of
                    StackOverflowCall   => memRegStackOverflowCall
                |   StackOverflowCallEx => memRegStackOverflowCallEx
                |   HeapOverflowCall    => memRegHeapOverflowCall
            val regSet = List.foldl(fn (r, a) => (0w1 << Word.fromInt(nReg(GenReg r))) orb a) 0w0 regSet
            val callInstr =
                opPlus2(Group5, LargeInt.fromInt entry, ebp, 0w2 (* call *))
            val regSetInstr =
                if regSet >= 0w256
                then [0wxca, (* This is actually a FAR RETURN *)
                        wordToWord8 regSet, (* Low byte*) wordToWord8 (regSet >> 0w8) (* High byte*)]
                else if regSet <> 0w0
                then [0wxcd, (* This is actually INT n *) wordToWord8 regSet]
                else []
        in
            callInstr @ regSetInstr
        end

    (* Operations. *)
    type cases = word * label

    type memoryAddress = { base: genReg, offset: int, index: indexType }

    datatype branchPrediction = PredictNeutral | PredictTaken | PredictNotTaken

    datatype 'reg regOrMemoryArg =
        RegisterArg of 'reg
    |   MemoryArg of memoryAddress
    |   NonAddressConstArg of LargeInt.int
    |   AddressConstArg of machineWord
    
    datatype nonWordSize = Size8Bit | Size16Bit | Size32Bit
    and fpSize = SinglePrecision | DoublePrecision

    datatype operation =
        MoveToRegister of { source: genReg regOrMemoryArg, output: genReg }
    |   LoadNonWord of { size: nonWordSize, source: memoryAddress, output: genReg }
    |   PushToStack of genReg regOrMemoryArg
    |   PopR of genReg
    |   ArithToGenReg of { opc: arithOp, output: genReg, source: genReg regOrMemoryArg }
    |   ArithMemConst of { opc: arithOp, offset: int, base: genReg, source: LargeInt.int }
    |   ArithMemLongConst of { opc: arithOp, offset: int, base: genReg, source: machineWord }
    |   ShiftConstant of { shiftType: shiftType, output: genReg, shift: Word8.word }
    |   ShiftVariable of { shiftType: shiftType, output: genReg } (* Shift amount is in ecx *)
    |   ConditionalBranch of { test: branchOps, label: label, predict: branchPrediction }
    |   LockMutableSegment of genReg
    |   LoadAddress of { output: genReg, offset: int, base: genReg option, index: indexType }
    |   TestTagR of genReg
    |   TestByteMem of { base: genReg, offset: int, bits: word }
    |   CallRTS of {rtsEntry: trapEntries, saveRegs: genReg list }
    |   StoreRegToMemory of { toStore: genReg, address: memoryAddress }
    |   StoreConstToMemory of { toStore: LargeInt.int, address: memoryAddress }
    |   StoreLongConstToMemory of { toStore: machineWord, address: memoryAddress }
    |   StoreNonWord of { size: nonWordSize, toStore: genReg, address: memoryAddress }
    |   StoreNonWordConst of { size: nonWordSize, toStore: LargeInt.int, address: memoryAddress }
    |   AllocStore of { size: int, output: genReg, saveRegs: genReg list }
    |   AllocStoreVariable of { output: genReg, saveRegs: genReg list }
    |   StoreInitialised
    |   CallFunction of callKinds
    |   JumpToFunction of callKinds
    |   ReturnFromFunction of int
    |   RaiseException
    |   UncondBranch of label
    |   ResetStack of int
    |   JumpLabel of label
        (* Some of these operations are higher-level and should be reduced. *)
    |   LoadHandlerAddress of { handlerLab: addrs ref, output: genReg }
    |   StartHandler of { handlerLab: addrs ref }
    |   IndexedCase of { testReg: genReg, workReg: genReg, min: word, cases: label list }
    |   FreeRegisters of regSet
    |   RepeatOperation of repOps
    |   DivideAccR of {arg: genReg, isSigned: bool }
    |   DivideAccM of {base: genReg, offset: int, isSigned: bool }
    |   AtomicXAdd of {base: genReg, output: genReg}
    |   FPLoadFromMemory of { address: memoryAddress, precision: fpSize }
    |   FPLoadFromFPReg of { source: fpReg, lastRef: bool }
    |   FPLoadFromConst of real
    |   FPStoreToFPReg of { output: fpReg, andPop: bool }
    |   FPStoreToMemory of { address: memoryAddress, precision: fpSize, andPop: bool }
    |   FPArithR of { opc: fpOps, source: fpReg }
    |   FPArithConst of { opc: fpOps, source: machineWord }
    |   FPArithMemory of { opc: fpOps, base: genReg, offset: int }
    |   FPUnary of fpUnaryOps
    |   FPStatusToEAX
    |   FPLoadInt of { base: genReg, offset: int }
    |   FPFree of fpReg
    |   MultiplyRR of { source: genReg, output: genReg }
    |   MultiplyRM of { base: genReg, offset: int,output: genReg }
    |   XMMArith of { opc: sse2Operations, source: xmmReg regOrMemoryArg, output: xmmReg }
    |   XMMStoreToMemory of { toStore: xmmReg, address: memoryAddress, precision: fpSize }
    |   XMMConvertFromInt of { source: genReg, output: xmmReg }
    |   SignExtendForDivide
    |   XChngRegisters of { regX: genReg, regY: genReg }
    |   Negative of { output: genReg }

    type operations = operation list

    fun printOperation(operation, stream) =
    let
        fun printGReg r = stream(genRegRepr(r, sz32_64))
        val printFPReg = stream o fpRegRepr
        and printXMMReg = stream o xmmRegRepr
        fun printBaseOffset(b, x, i) =
        (
            stream(Int.toString i); stream "("; printGReg b; stream ")";
            case x of
                NoIndex => ()
            |   Index1 x => (stream "["; printGReg x; stream "]")
            |   Index2 x => (stream "["; printGReg x; stream "*2]")
            |   Index4 x => (stream "["; printGReg x; stream "*4]")
            |   Index8 x => (stream "["; printGReg x; stream "*8]")
        )
        fun printMemAddress({ base, offset, index }) = printBaseOffset(base, index, offset)
        
        fun printRegOrMemoryArg printReg (RegisterArg r) = printReg r
        |   printRegOrMemoryArg _ (MemoryArg{ base, offset, index }) = printBaseOffset(base, index, offset)
        |   printRegOrMemoryArg _ (NonAddressConstArg c) = stream(LargeInt.toString c)
        |   printRegOrMemoryArg _ (AddressConstArg c) = stream(Address.stringOfWord c)
        
        fun printCallKind Recursive = stream "Recursive"
        |   printCallKind (ConstantCode w) = (stream "code="; stream(stringOfWord w))
        |   printCallKind FullCall = stream "via ClosureReg"
        |   printCallKind (DirectReg reg) = printGReg reg
        
        fun printSize Size8Bit = "Byte"
        |   printSize Size16Bit = "16Bit"
        |   printSize Size32Bit = "32Bit"
     in
        case operation of
            MoveToRegister { source, output } =>
                (stream "MoveRR "; printGReg output; stream " <= "; printRegOrMemoryArg printGReg source)

        |   LoadNonWord { size, source, output } =>
                (stream "Load"; printSize size; stream " "; printGReg output; stream " <= "; printMemAddress source )

        |   ArithToGenReg { opc, output, source } =>
                (stream (arithOpRepr opc ^ "RR "); printGReg output; stream " <= "; printRegOrMemoryArg printGReg source )

        |   ArithMemConst { opc, offset, base, source } =>
            (
                stream (arithOpRepr opc ^ "MC "); printBaseOffset(base, NoIndex, offset);
                stream " "; stream(LargeInt.toString source)
            )

        |   ArithMemLongConst { opc, offset, base, source } =>
            (
                stream (arithOpRepr opc ^ "MC ");
                printBaseOffset(base, NoIndex, offset);
                stream " <= "; stream(Address.stringOfWord source)
            )

        |   ShiftConstant { shiftType, output, shift } =>
            (
                stream(shiftTypeRepr shiftType); stream " "; printGReg output;
                stream " by "; stream(Word8.toString shift)
            )

        |   ShiftVariable { shiftType, output } => (* Shift amount is in ecx *)
            (
                stream(shiftTypeRepr shiftType); stream " "; printGReg output; stream " by ECX"
            )

        |   ConditionalBranch { test, label=Labels{labId=ref lab, ...}, predict } =>
            (
                stream(branchOpRepr test); stream " L"; stream(Int.toString lab);
                case predict of
                    PredictNeutral => ()
                |   PredictTaken => stream " PredictTaken"
                |   PredictNotTaken => stream " PredictNotTaken"
            )

        |   LockMutableSegment reg => (stream "LockMutableSegment "; printGReg reg)

        |   PushToStack source => (stream "Push "; printRegOrMemoryArg printGReg source)

        |   PopR dest => (stream "PopR "; printGReg dest)

        |   StoreRegToMemory { toStore, address } =>
            (
                stream "StoreRegToMemory "; printMemAddress address;
                stream " <= "; printGReg toStore
            )

        |   StoreConstToMemory { toStore, address } =>
            (
                stream "StoreConstToMemory "; printMemAddress address;
                stream " <= "; stream(LargeInt.toString toStore)
            )

        |   StoreLongConstToMemory { address, toStore } =>
            (
                stream "StoreLongConstToMemory "; printMemAddress address; stream " <= "; stream(Address.stringOfWord toStore)
            )

        |   StoreNonWord { size, toStore, address } =>
            (
                stream "Store"; printSize size; stream " "; printMemAddress address;
                stream " <= "; stream(genRegRepr(toStore, SZByte))
            )

        |   StoreNonWordConst { size, toStore, address } =>
            (
                stream "StoreConst"; printSize size; stream " "; printMemAddress address;
                stream " <= "; stream(LargeInt.toString toStore)
            )

        |   LoadAddress{ output, offset, base, index } =>
            (
                stream "LoadAddress ";
                case base of NONE => () | SOME r => (printGReg r; stream " + ");
                stream(Int.toString offset);
                case index of
                    NoIndex => ()
                |   Index1 x => (stream " + "; printGReg x)
                |   Index2 x => (stream " + "; printGReg x; stream "*2 ")
                |   Index4 x => (stream " + "; printGReg x; stream "*4 ")
                |   Index8 x => (stream " + "; printGReg x; stream "*8 ");
                stream " => "; printGReg output
            )

        |   TestTagR reg => ( stream "TestTagR "; printGReg reg )

        |   TestByteMem { base, offset, bits } =>
                ( stream "TestByteMem "; printBaseOffset(base, NoIndex, offset); stream " 0x"; stream(Word.toString bits) )

        |   CallRTS {rtsEntry, ...} =>
            (
                stream "CallRTS ";
                case rtsEntry of
                    StackOverflowCall => stream "StackOverflowCall"
                |   HeapOverflowCall => stream "HeapOverflow"
                |   StackOverflowCallEx => stream "StackOverflowCallEx"
            )

        |   AllocStore { size, output, ... } =>
                (stream "AllocStore "; stream(Int.toString size); stream " => "; printGReg output )

        |   AllocStoreVariable { output, ...} => (stream "AllocStoreVariable "; printGReg output )
        
        |   StoreInitialised => stream "StoreInitialised"

        |   CallFunction callKind => (stream "CallFunction "; printCallKind callKind)

        |   JumpToFunction callKind => (stream "JumpToFunction "; printCallKind callKind)

        |   ReturnFromFunction argsToRemove =>
                (stream "ReturnFromFunction "; stream(Int.toString argsToRemove))

        |   RaiseException =>
                stream "RaiseException"
        |   UncondBranch(Labels{labId=ref lab, ...})=>
                (stream "UncondBranch L"; stream(Int.toString lab))
        |   ResetStack i =>
                (stream "ResetStack "; stream(Int.toString i))
        |   JumpLabel(Labels{labId=ref lab, ...}) =>
                (stream "L"; stream(Int.toString lab); stream ":")
        |   LoadHandlerAddress { handlerLab=_, output=_ } =>
                stream "LoadHandlerAddress"
        |   StartHandler { handlerLab=_ } =>
                stream "StartHandler"
        |   IndexedCase { testReg, workReg, min, cases } =>
            (
                stream "IndexedCase "; printGReg testReg; stream " with "; printGReg workReg;
                stream "\n";
                List.foldl(fn(Labels{labId=ref lab, ...}, v) =>
                    (stream(Word.toString v); stream " => L"; stream(Int.toString lab); stream "\n"; v+0w1))
                    min cases;
                ()
            )
        |   FreeRegisters regs => (stream "FreeRegister "; stream(regSetRepr regs))
        |   RepeatOperation repOp => (stream "Repeat "; stream(repOpsRepr repOp))
        |   DivideAccR{arg, isSigned} => ( stream(if isSigned then "DivideSigned" else "DivideUnsigned"); stream " "; printGReg arg)
        |   DivideAccM{base, offset, isSigned} => ( stream(if isSigned then "DivideSigned" else "DivideUnsigned"); stream " "; printBaseOffset(base, NoIndex, offset))
        |   AtomicXAdd{base, output} => (stream "LockedXAdd ("; printGReg base; stream ") <=> "; printGReg output)
        |   FPLoadFromMemory{address, precision=DoublePrecision} => (stream "FPLoadDouble "; printMemAddress address)
        |   FPLoadFromMemory{address, precision=SinglePrecision} => (stream "FPLoadSingle "; printMemAddress address)
        |   FPLoadFromFPReg {source, lastRef} =>
                (stream "FPLoad "; printFPReg source; if lastRef then stream " (LAST)" else())
        |   FPLoadFromConst const => (stream "FPLoad "; stream(Real.toString const) )
        |   FPStoreToFPReg{ output, andPop } =>
                (if andPop then stream "FPStoreAndPop => " else stream "FPStore => "; printFPReg output)
        |   FPStoreToMemory{ address, precision=DoublePrecision, andPop: bool } =>
            (
                if andPop then stream "FPStoreDoubleAndPop => " else stream "FPStoreDouble => ";
                printMemAddress address
            )
        |   FPStoreToMemory{ address, precision=SinglePrecision, andPop: bool } =>
            (
                if andPop then stream "FPStoreSingleAndPop => " else stream "FPStoreSingle => ";
                printMemAddress address
            )
        |   FPArithR{ opc, source } => (stream(fpOpRepr opc); stream " "; printFPReg source)
        |   FPArithConst{ opc, source } => (stream(fpOpRepr opc); stream(Address.stringOfWord source))
        |   FPArithMemory{ opc, base, offset } => (stream(fpOpRepr opc); stream " "; printBaseOffset(base, NoIndex, offset))
        |   FPUnary opc => stream(fpUnaryRepr opc)
        |   FPStatusToEAX => (stream "FPStatus "; printGReg eax)
        |   FPLoadInt { base, offset} => (stream "FPLoadInt "; printBaseOffset(base, NoIndex, offset))
        |   FPFree reg => (stream "FPFree "; printFPReg reg)
        |   MultiplyRR {source, output } => (stream "MultiplyRR"; stream " "; printGReg source; stream " *=>"; printGReg output)
        |   MultiplyRM {base, offset, output } => (stream "MultiplyRM"; stream " "; printBaseOffset(base, NoIndex, offset); stream " *=>"; printGReg output)
        |   XMMArith { opc, source, output } =>
            (
                stream (sse2OpRepr opc ^ "RM "); printXMMReg output; stream " <= "; printRegOrMemoryArg printXMMReg source
            )
        |   XMMStoreToMemory { toStore, address, precision=DoublePrecision } =>
            (
                stream "MoveDouble "; printXMMReg toStore; stream " => "; printMemAddress address
            )
        |   XMMStoreToMemory { toStore, address, precision=SinglePrecision } =>
            (
                stream "MoveSingle "; printXMMReg toStore; stream " => "; printMemAddress address
            )
        |   XMMConvertFromInt { source, output } =>
            (
                stream "ConvertFromInt "; printGReg source; stream " => "; printXMMReg output
            )
        |   SignExtendForDivide => stream "SignExtendForDivide"
        |   XChngRegisters { regX, regY } => (stream "XChngRegisters "; printGReg regX; stream " <=> "; printGReg regY)
        |   Negative { output } => (stream "Negative "; printGReg output)
        ;
 
        stream "\n"
    end

    datatype implement = ImplementGeneral | ImplementLiteral of machineWord


    (* Previously the jump table was a vector of destination addresses.
       Now changed to use a vector of jump instructions.  These are padded
       out to 8 bytes with no-ops.  The reason for the change is to ensure
       that the code segment only contains instructions so that we can scan
       for addresses within the code.  It also simplifies and speeds up
       the indexed jump at the expense of doubling the size of the table
       itself.  *)
 
    fun indexedCase (r1:genReg, r2:genReg, min:word, cases, code as Code{ic, codeVec, ...}) =
    let
        val (rc2, rx2) = getReg r2
        (* Load the address of the jump table.  We can use Pc-relative addressing on
           X86/64 but on X86/32 we have to load the start of the code and add
           the offset. *)
        val loadAddress =
            if isX64
            then
            let
                val opb = opCodeBytes(LEAL, SOME {w=true, r=rx2, b=false, x=false})
                val mdrm = modrm(Based0, rc2, 0w5 (* Immediate address. *))
            in
                opb @ [mdrm]
            end
            else
            let
                val opb1 = opCodeBytes(MOVL_32_64_R rc2, NONE)
                val opb2 = opRegPlus2(Group1_32_A, r2, arithOpToWord ADD)
            in
                addConstToVec (SelfVal, InlineAbsolute, code, opb1) @ opb2
            end
        val () = genBytes(loadAddress @ int32Signed 0, code) (* Patched with address. *)
        val startJumpTable = !ic addrPlus ~4
    in
        (* Compute the jump address.  The index is a tagged
           integer so it is already multiplied by 2.  We need to
           multiply by four to get the correct size. We subtract off
           the minimum value and also the shifted tag. *)
		let
			val adjustment = Word.toLargeIntX min * ~8 - 4
		in
			(* In 64-bit mode this may not fit in a 32-bit value.  It will always
			   fit in 32-bit mode so we avoid an unnecessary long integer test. *)
			(* We don't need to consider any possible overflow in the execution 
			   because we've already checked that the value is within the range. *)
			if is32bit adjustment
			then
			(
                genBytes(immediateOperand(ADD, r2, adjustment, code), code);
			 	genBytes(opIndexed(LEAL, 0, SOME r2, Index4 r1, r2), code)
			)
        	else genBytes(opIndexed(LEAL, adjustment, SOME r2, Index4 r1, r2), code)
		end;
        (* Jump into the jump table. *)
        let
            val opb = opCodeBytes(Group5, if rx2 then SOME{w=false, r=false, b=rx2, x=false} else NONE)
            val mdrm = modrm(Register, 0w4 (* jmp *), #1 (getReg r2))
        in
            genBytes(opb @ [mdrm], code)
        end;

        let
            fun addJump(Labels{forward, reverse=ref reverse, ...}) =
            let
                val _ = reverse = addrUnsetLabel orelse raise InternalError "addJump";
            in
                genBytes(opToInt JMP_32 :: word32Unsigned 0w0, code);
                forward := [(Jump32From (!ic addrPlus ~4))]  @ ! forward;
                (* Add no-ops to make it 8 bytes. *)
                genBytes([opToInt NOP, opToInt NOP, opToInt NOP], code)
            end
        in
            (* On X86/64 this is a relative offset on X86/32 it is absolute. *)
            set32u(Word.toLargeWord (if isX64 then !ic - startJumpTable - 0w4 else !ic), startJumpTable, codeVec);
            List.app addJump cases
        end
    end;

    fun printLowLevelCode(ops, Code{printAssemblyCode, printStream, procName, ...}) =
        if printAssemblyCode
        then
        let
            (* Set the label fields so it will be clearer. *)
            fun setLabels(JumpLabel(Labels{labId, ...}), labNo) = (labId := labNo; labNo+1)
            |   setLabels(_, labNo) = labNo
            val _ = List.foldl setLabels 1 ops
        in
            if procName = "" (* No name *) then printStream "?" else printStream procName;
            printStream ":\n";
            List.app(fn i => printOperation(i, printStream)) ops;
            printStream "\n"
        end
        else ()

    (* Code generate a list of operations.  The list is in reverse order i.e. last instruction first. *)
    fun codeGenerate (ops, code as Code{ic, ...}) =
    let
        val () = printLowLevelCode(ops, code)

        fun cgOp(LockMutableSegment baseReg) =
                (* Remove the mutable bit from the flag byte. *)(*andb CONST(0xff-0x40),-1[Reax]*)
                genBytes(opPlus2(Group1_8_a, ~1, baseReg, arithOpToWord AND) @ [0wxff - 0wx40], code)

        |   cgOp(MoveToRegister{ source=RegisterArg source, output }) =
                (* Move from one general register to another. *)
                genBytes(opReg(MOVL_R_A, source, output), code)

        |   cgOp(MoveToRegister{ source=NonAddressConstArg source, output}) =
            if isX64
            then
            (
                if source >= ~0x40000000 andalso source < 0x40000000
                then (* Signed 32-bits. *)
                    (* This is not scanned in 64-bit mode because 32-bit values aren't
                       big enough to contain addresses. *)
                    genBytes(opRegPlus2(MOVL_32_A, output, 0w0) @ int32Signed source, code)
                else (* Too big for 32-bits; put it in the non-word area. *)
                let
                    val (rc, rx) = getReg output
                    val opb = opCodeBytes(MOVL_A_R, SOME{w=true, r=rx, b=false, x = false})
                    val mdrm = modrm (Based0, rc, 0w5 (* PC-relative *))
                in
                    genBytes(addNonAddressConstant(NonAddressInt source, code, opb @ [mdrm]), code)
                end
            )
            else (* 32-bit mode. *)
            (
                (* The RTS scans for possible addresses in MOV instructions so we
                   can only use MOV if this is a tagged value.  If it isn't we have
                   to use something else such as XOR/ADD.  In particular this is used
                   before LOCK XADD for atomic inc/dec.
                   TODO: There are various special cases such as setting to -1 can
                   be done with an OR whatever the initial value.  INC can be used
                   in 32-bit mode instead of ADD for 1. LEAL isn't a good idea. *)
                if source mod 2 = 0
                then
                (
                    genBytes(arithOpReg(XOR, output, output), code);
                    if source = 0 then ()
                    else genBytes(immediateOperand(ADD, output, source, code), code)
                )
                else
                let
                    val (rc, _) = getReg output
                    val opb = opCodeBytes(MOVL_32_64_R rc, NONE)
                in
                    genBytes(opb @ int32Signed source, code)
                end
            )

        |   cgOp(MoveToRegister{ source=AddressConstArg source, output }) =
            let
                val (rc, rx) = getReg output
                val opb =
                    opCodeBytes(MOVL_32_64_R rc,
                            if isX64 then SOME {w=true, r=false, b=rx, x=false} else NONE)
            in
                genBytes(addConstToVec (WVal source, InlineAbsolute, code, opb), code)
            end

        |   cgOp(MoveToRegister{source=MemoryArg{base, offset, index}, output }) =
            (
                case index of
                    NoIndex => genBytes(opEA(MOVL_A_R, LargeInt.fromInt offset, base, output), code)
                |   _ => genBytes(opIndexed(MOVL_A_R, LargeInt.fromInt offset, SOME base, index, output), code)
            )

        |   cgOp(LoadNonWord{size=Size8Bit, source={base, offset, index}, output }) =
            (
                case index of
                    NoIndex => genBytes(opEA(MOVZB (* 2 byte opcode *), LargeInt.fromInt offset, base, output), code)
                |   _ => genBytes(opIndexed(MOVZB, LargeInt.fromInt offset, SOME base, index, output), code)
            )

        |   cgOp(LoadNonWord{size=Size16Bit, source={base, offset, index}, output }) =
            (
                case index of
                    NoIndex => genBytes(opEA(MOVZW (* 2 byte opcode *), LargeInt.fromInt offset, base, output), code)
                |   _ => genBytes(opIndexed(MOVZW, LargeInt.fromInt offset, SOME base, index, output), code)
            )

        |   cgOp(LoadNonWord{size=Size32Bit, source={base, offset, index}, output }) =
            (
                case index of
                    NoIndex => genBytes(opEA(MOVL_A_R32, LargeInt.fromInt offset, base, output), code)
                |   _ => genBytes(opIndexed(MOVL_A_R32, LargeInt.fromInt offset, SOME base, index, output), code)
            )

        |   cgOp(LoadAddress{ offset, base, index, output }) =
            (
                (* This provides a mixture of addition and multiplication in a single
                   instruction. *)
                case (index, base) of
                    (NoIndex, SOME base) => genBytes(opEA(LEAL, LargeInt.fromInt offset, base, output), code)
                |   (NoIndex, NONE) => raise InternalError "LoadAddress: no base or index"
                |   _ => genBytes(opIndexed(LEAL, LargeInt.fromInt offset, base, index, output), code)
            )

        |   cgOp(ArithToGenReg{ opc, output, source=RegisterArg source }) =
                genBytes(arithOpReg (opc, output, source), code)

        |   cgOp(ArithToGenReg{ opc, output, source=NonAddressConstArg source }) =
                genBytes(immediateOperand(opc, output, source, code), code)

        |   cgOp(ArithToGenReg{ opc, output, source=AddressConstArg source }) =
            (* This is only used for opc=CMP to compare addresses for equality. *)
            let
                val (rc, rx) = getReg output
            in
                if isX64
                then
                let
                    val opb = opCodeBytes(Arith (opc, 0w3), SOME {w=true, r=rx, b=false, x=false})
			        val mdrm = modrm(Based0, rc, 0w5 (* Immediate address. *))
                in
                    genBytes(addConstToVec (WVal source, ConstArea 0, code, opb @ [mdrm]), code)
                end
                else
                let
                    val opb = opCodeBytes(Group1_32_A (* group1, 32 bit immediate *), NONE)
                    val mdrm = modrm(Register, arithOpToWord opc, rc)
                in
                    genBytes(addConstToVec (WVal source, InlineAbsolute, code, opb @ [mdrm]), code)
                end
            end

        |   cgOp(ArithToGenReg{ opc, output, source=MemoryArg{offset, base, index=NoIndex} }) =
                genBytes(opEA(Arith (opc, 0w3), LargeInt.fromInt offset, base, output), code)

        |   cgOp(ArithToGenReg _) = raise InternalError "cgOp: ArithToGenReg"

        |   cgOp(ArithMemConst{ opc, offset, base, source }) =
                if is8BitL source
                then (* Can use one byte immediate *) 
                    genBytes(opPlus2(Group1_8_A (* group1, 8 bit immediate *),
                               LargeInt.fromInt offset, base, arithOpToWord opc) @ [Word8.fromLargeInt source], code)
                else (* Need 32 bit immediate. *)
                    genBytes(opPlus2(Group1_32_A (* group1, 32 bit immediate *), 
                               LargeInt.fromInt offset, base, arithOpToWord opc) @ int32Signed source, code)

        |   cgOp(ArithMemLongConst{ opc, offset, base, source }) =
                (* Currently this is always a comparison. *)
                genBytes(memoryConstant(source, Group1_32_A, arithOpToWord opc, LargeInt.fromInt offset, base, NoIndex, code), code)

        |   cgOp(ShiftConstant { shiftType, output, shift }) =
                if shift = 0w1
                then genBytes(opRegPlus2(Group2_1_A, output, shiftTypeToWord shiftType), code)
                else genBytes(opRegPlus2(Group2_8_A, output, shiftTypeToWord shiftType) @ [shift], code)

        |   cgOp(ShiftVariable { shiftType, output }) =
                genBytes(opRegPlus2(Group2_CL_A, output, shiftTypeToWord shiftType), code)

        |   cgOp(TestTagR reg) =
            let
            (* Test the bottom bit and jump depending on its value.  This is used
               for tag tests in arbitrary precision operations and also for testing
               for short/long values. *)
                val (regNum, rx) = getReg reg
            in
                if reg = eax
                then (* Special instruction for testing accumulator.  Can use an 8-bit test. *)
                    genBytes(opCodeBytes(TEST_ACC8, NONE) @ [0w1], code)
                else if isX64
                then 
                let
                    (* We can use a REX code to force it to always use the low order byte. *)
                    val opb = opCodeBytes(Group3_a,
                        if rx orelse regNum >= 0w4 then SOME{w=false, r=false, b=rx, x=false} else NONE)
                    val mdrm = modrm (Register, 0w0 (* test *), regNum)
                in
                    genBytes(opb @ [mdrm, 0w1], code)
                end
                else if reg = ebx orelse reg = ecx orelse reg = edx (* can we use an 8-bit test? *)
                then (* Yes. The register value refers to low-order byte. *)
                let
                    val opb = opCodeBytes(Group3_a, NONE)
                    val mdrm = modrm(Register, 0w0 (* test *), regNum)
                in
                    genBytes(opb @ [mdrm, 0w1], code)
                end
                else
                let
                    val opb = opCodeBytes(Group3_A, NONE)
                    val mdrm = modrm (Register, 0w0 (* test *), regNum)
                in
                    genBytes(opb @ mdrm :: word32Unsigned 0w1, code)
                end
            end

        |   cgOp(TestByteMem{base, offset, bits}) =
                (* Test the tag bit and set the condition code. *)
                genBytes(opPlus2(Group3_a, LargeInt.fromInt offset, base, 0w0 (* test *)) @ [wordToWord8 bits], code)

        |   cgOp(ConditionalBranch{ test=opc, label=Labels{forward, reverse=ref reverse, ...}, ... }) =
                (* Almost always conditional branches are forward but it is possible that a
                   conditional branch has been combined with a loop jump to give a backwards jump. *)
                if reverse = addrUnsetLabel (* Destination is after this. *)
                then
                let
                    val opb = opCodeBytes(CondJump32 opc, NONE)
                in
                    genBytes(opb @ word32Unsigned 0w0, code);
                    forward := makeLongLabel (!ic addrPlus ~4, code) :: !forward
                end
                else
                let
                    val offset  = reverse addrMinus (!ic) (* Negative *)
                    val offset2 = offset - 2
                in
                    if is8Bit offset2
                    then genBytes(opCodeBytes(CondJump opc, NONE) @ [Word8.fromInt offset2], code)
                    else genBytes(opCodeBytes(CondJump32 opc, NONE) @ int32Signed(LargeInt.fromInt(offset - 6)), code)
                end

        |   cgOp(CallRTS{rtsEntry, saveRegs}) = genBytes(rtsCall(rtsEntry, saveRegs), code)

        |   cgOp(RepeatOperation repOp) =
            let
                (* We don't explicitly clear the direction flag.  Should that be done? *)
                val opb = opCodeBytes(REP, NONE)
                (* Put in a rex prefix to force 64-bit mode. *)
                val optRex =
                    if isX64 andalso (case repOp of STOSL => true | MOVSL => true | _ => false)
                    then [rex{w=true, r=false, b=false, x=false}]
                    else []
                val repOp = repOpsToWord repOp
            in
                genBytes(opb @ optRex @ [repOp], code)
            end

        |   cgOp(DivideAccR{arg, isSigned}) = genBytes(opRegPlus2(Group3_A, arg, if isSigned then 0w7 else 0w6), code)

        |   cgOp(DivideAccM{base, offset, isSigned}) =
                genBytes(opPlus2(Group3_A, LargeInt.fromInt offset, base, if isSigned then 0w7 else 0w6), code)

        |   cgOp(AtomicXAdd{base, output}) =
                (* Locked exchange-and-add.  We need the lock prefix before the REX prefix. *)
                genBytes(opEA(LOCK_XADD, 0, base, output), code)

        |   cgOp(PushToStack(RegisterArg reg)) =
            let
                val (rc, rx) = getReg reg
            in
                (* Always 64-bit but a REX prefix may be needed for the register. *)
                genBytes(opCodeBytes(PUSH_R rc, if rx then SOME{w=false, b = true, x=false, r = false } else NONE), code)
            end

        |   cgOp(PushToStack(MemoryArg{base, offset, index=NoIndex})) =
                genBytes(opPlus2(Group5, LargeInt.fromInt offset, base, 0w6 (* push *)), code)

        |   cgOp(PushToStack(MemoryArg{base, offset, index})) =
                genBytes(opIndexed(Group5, LargeInt.fromInt offset, SOME base, index, mkReg(0w6 (* push *), false)), code)

        |   cgOp(PushToStack(NonAddressConstArg constnt)) = 
                if is8BitL constnt
                then genBytes(opCodeBytes(PUSH_8, NONE) @ [Word8.fromLargeInt constnt], code)
                else if is32bit constnt
                then genBytes(opCodeBytes(PUSH_32, NONE) @ int32Signed constnt, code)
                else (* It won't fit in the immediate; put it in the non-address area. *)
                let
                    val opb = opCodeBytes(Group5, NONE)
                    val mdrm = modrm(Based0, 0w6 (* push *), 0w5 (* PC rel *))
                in
                    genBytes(addNonAddressConstant(NonAddressInt constnt, code, opb @ [mdrm]), code)
                end

        |   cgOp(PushToStack(AddressConstArg constnt)) = 
                if isX64
                then (* Put it in the constant area. *)
		        let
                    val opb = opCodeBytes(Group5, NONE)
                    val mdrm = modrm(Based0, 0w6 (* push *), 0w5 (* PC rel *));
                in
                    genBytes(addConstToVec(WVal constnt, ConstArea 0, code, opb @ [mdrm]), code)
                end
                else (* 32-bit *)
                let
                    val opb = opCodeBytes(PUSH_32, NONE)
                in
                    genBytes(addConstToVec(WVal constnt, InlineAbsolute, code, opb), code)
                end

        |   cgOp(PopR reg ) =
                let
                    val (rc, rx) = getReg reg
                in
                    (* Always 64-bit but a REX prefix may be needed for the register. *)
                    genBytes(opCodeBytes(POP_R rc, if rx then SOME{w=false, b = true, x=false, r = false } else NONE), code)
                end

        |   cgOp(StoreRegToMemory{toStore, address={offset, base, index=NoIndex}}) =
                genBytes(opEA(MOVL_R_A, LargeInt.fromInt offset, base, toStore), code) 
          
        |   cgOp(StoreRegToMemory{toStore, address={offset, base, index}}) =
                genBytes(opIndexed(MOVL_R_A, LargeInt.fromInt offset, SOME base, index, toStore), code)

        |   cgOp(StoreConstToMemory{ toStore=toStore, address={offset, base, index} }) =
            (
                (* Short constant.  In 32-bit mode this is scanned as a possible address.  That means
                   we can't have an untagged constant in it.  That's not a problem in 64-bit mode.
                   There's a special check for using this to set the length word on newly allocated
                   memory. *)
                isX64 orelse toStore = 0 orelse toStore mod 2 = 1 orelse offset = ~wordSize
                    orelse raise InternalError "cgOp: StoreConstToMemory not tagged";
                case index of
                    NoIndex =>
                        genBytes(opPlus2(MOVL_32_A, LargeInt.fromInt offset, base, 0w0) @ int32Signed toStore, code)
                |   index =>
                        genBytes(opIndexed(MOVL_32_A, LargeInt.fromInt offset, SOME base, index, mkReg(0w0, false)) @
                            int32Signed toStore, code)
            )

        |   cgOp(StoreLongConstToMemory{ toStore=toStore, address={offset, base, index} }) =
                genBytes(memoryConstant(toStore, MOVL_32_A, 0w0, LargeInt.fromInt offset, base, index, code), code)

        |   cgOp(StoreNonWord{ size = Size8Bit, toStore, address }) =
            let
                val (rrC, _) = getReg toStore
                (* In 64-bit mode we can specify the low-order byte of RSI/RDI but we
                   must use a REX prefix.  This isn't possible in 32-bit mode. *)
                val opcode = MOVB_R_A{forceRex= rrC >= 0w4}
                val _ = isX64 orelse rrC < 0w4 orelse raise InternalError "High byte register"
            in
                case address of
                    {offset, base, index=NoIndex} =>
                        genBytes(opEA(opcode, LargeInt.fromInt offset, base, toStore), code) 
                |   {offset, base, index} =>
                        genBytes(opIndexed(opcode, LargeInt.fromInt offset, SOME base, index, toStore), code)
            end

        |   cgOp(StoreNonWord{ size = Size16Bit, toStore, address={offset, base, index=NoIndex}}) =
                genBytes(opEA(MOVL_R_A16, LargeInt.fromInt offset, base, toStore), code) 

        |   cgOp(StoreNonWord{ size = Size16Bit, toStore, address={offset, base, index}}) =
                genBytes(opIndexed(MOVL_R_A16, LargeInt.fromInt offset, SOME base, index, toStore), code)

        |   cgOp(StoreNonWord{ size = Size32Bit, toStore, address={offset, base, index=NoIndex}}) =
                genBytes(opEA(MOVL_R_A32, LargeInt.fromInt offset, base, toStore), code) 
            
        |   cgOp(StoreNonWord{ size = Size32Bit, toStore, address={offset, base, index}}) =
                genBytes(opIndexed(MOVL_R_A32, LargeInt.fromInt offset, SOME base, index, toStore), code)

        |   cgOp(StoreNonWordConst{ size=Size8Bit, toStore=toStore, address={offset, base, index=NoIndex}}) =
                genBytes(opPlus2(MOVB_8_A, LargeInt.fromInt offset, base, 0w0) @ [Word8.fromLargeInt toStore], code)

        |   cgOp(StoreNonWordConst{ size=Size8Bit, toStore=toStore, address={offset, base, index}}) =
                genBytes(opIndexed(MOVB_8_A, LargeInt.fromInt offset, SOME base, index, mkReg(0w0, false)) @
                    [Word8.fromLargeInt toStore], code)

        |   cgOp(StoreNonWordConst{ size=Size16Bit, ... }) =
                raise InternalError "StoreNonWordConst: 16Bit"

        |   cgOp(StoreNonWordConst{ size=Size32Bit, ... }) =
                raise InternalError "StoreNonWordConst: 32Bit"

            (* Allocation is dealt with by expanding the code. *)
        |   cgOp(AllocStore _) = raise InternalError "cgOp: AllocStore"

        |   cgOp(AllocStoreVariable _) = raise InternalError "cgOp: AllocStoreVariable"

        |   cgOp StoreInitialised = raise InternalError "cgOp: StoreInitialised"

        |   cgOp(CallFunction Recursive) =
                genBytes(opCodeBytes (CALL_32, NONE) @ int32Signed (~(Word.toLargeInt(!ic) + 5)), code)
     
        |   cgOp(CallFunction FullCall) =
            let
                val mdrm = modrm(Based0, 0w2 (* call *), #1 (getReg regClosure))
            in
                genBytes(opCodeBytes (Group5, NONE) @ [mdrm], code)
            end

        |   cgOp(CallFunction(ConstantCode w)) =
            if isX64
            then
            let
                val opc = opCodeBytes(Group5, NONE)
                val mdrm = modrm(Based0, 0w2 (* call *), 0w5 (* PC rel *))
            in
                genBytes(addConstToVec (WVal w, ConstArea 0, code, opc @ [mdrm]), code)
            end
            else genBytes(addConstToVec (WVal w, InlineRelative, code, opCodeBytes (CALL_32, NONE)), code)
            
        |   cgOp(CallFunction(DirectReg reg)) =
            let
                val (rc, rx) = getReg reg
                val opb = opCodeBytes (Group5, if rx then SOME{w=false, r=false, b=rx, x=false} else NONE)
                val mdrm = modrm(Register, 0w2 (* call *), rc)
            in
                genBytes(opb @ [mdrm], code)
            end

        |   cgOp(JumpToFunction Recursive) =
                (* Jump to the start of the current function. *)
                genBytes(opCodeBytes (JMP_32, NONE) @ int32Signed (~(Word.toLargeInt(!ic) + 5)), code)
           
        |   cgOp(JumpToFunction FullCall) =
            let (* Full closure call *)
                val opb = opCodeBytes (Group5, NONE)
                val mdrm = modrm(Based0, 0w4 (* jmp *), #1 (getReg regClosure))
            in
                genBytes(opb @ [mdrm], code)
            end

        |   cgOp(JumpToFunction (ConstantCode w)) =
            if isX64
            then
            let
                val opb = opCodeBytes (Group5, NONE)
                val mdrm = modrm(Based0, 0w4 (* jmp *), 0w5 (* PC rel *))
            in
                genBytes(addConstToVec (WVal w, ConstArea 0, code, opb @ [mdrm]), code)
            end
            else genBytes(addConstToVec (WVal w, InlineRelative, code, opCodeBytes (JMP_32, NONE)), code)
            
        |   cgOp(JumpToFunction (DirectReg _ )) = raise InternalError "jumpToFunction-DirectReg" (* Can't be tail-recursive *)
    
        |   cgOp(ReturnFromFunction args) =
            if args = 0
            then genBytes(opCodeBytes(RET, NONE), code)
            else
            let
                val offset = Word.fromInt(args * wordSize)
            in
                genBytes(opCodeBytes(RET_16, NONE) @ [wordToWord8 offset, wordToWord8(offset >> 0w8)], code)
            end

        |   cgOp RaiseException =
            let
                (* Load the current handler into ebx.  Any register will do since we
                   don't preserve registers across exceptions. *)
                val loadInstr = opEA(MOVL_A_R, LargeInt.fromInt memRegHandlerRegister, ebp, ebx)
                val opb = opCodeBytes(Group5, NONE)
                val mdrm = modrm (Based0, 0w4 (* jmp *), #1 (getReg ebx))
            in
                genBytes(loadInstr @ opb @ [mdrm], code)
            end

        |   cgOp(UncondBranch(Labels{forward, reverse, ...})) =
            (* This may be a forward jump, in which case we don't have the destination and
               can just record it, or it may be a backward jump in which case we already
               have the destination. *)
            if !reverse = addrUnsetLabel
            then
            let
                val brOp = opToInt JMP_32 :: word32Unsigned 0w0
                val br = makeLongLabel (!ic addrPlus (List.length brOp - 4), code)
            in
                forward := br :: ! forward;
                genBytes(brOp, code)
            end
            else 
            let
                val offset  = !reverse addrMinus !ic (* Negative *)
                val offset2 = offset - 2;
            in
                if is8Bit offset2
                then genBytes(opCodeBytes(JMP_8, NONE) @ [Word8.fromInt offset2], code)
                else genBytes(opCodeBytes(JMP_32, NONE) @ int32Signed(LargeInt.fromInt(offset - 5)), code)
            end

        |   cgOp(ResetStack count) =
            let
                val sr = Word.toInt(wordsToBytes(Word.fromInt count)) (* Offset in bytes. *)
            in
                if is8Bit sr
                then (* Can use one byte immediate *)
                    genBytes(
                        opRegPlus2(Group1_8_A (* group1, 8-bit immediate *), esp, arithOpToWord ADD) @
                            [Word8.fromInt sr], code)
                else (* Need 32 bit immediate. *)
                    genBytes(opRegPlus2(Group1_32_A (* group1, 32-bit immediate *), esp, arithOpToWord ADD) @
                        int32Signed(LargeInt.fromInt sr), code)
            end

        |   cgOp(JumpLabel(Labels{forward=ref forward, reverse, ...})) =
            let
            in
                fixup(forward, code); (* Fix up any forward branches to here. *)
                (* Set the address. *)
                reverse := !ic
            end

        |   cgOp(LoadHandlerAddress{ handlerLab, output }) =
            let
                val (rc, rx) = getReg output
            in
                (* On X86/64 we can use pc-relative addressing to set the start of the handler.
                   On X86/32 we have to load the address of the start of the code and add
                   an offset. *)
                if isX64
                then
                let
                    val opb = opCodeBytes(LEAL, SOME {w=true, r=rx, b=false, x=false})
                    val mdrm = modrm(Based0, rc, 0w5 (* Immediate address. *))
                in
                    genBytes(opb @ [mdrm], code)
                end
                else
                let
                    val opb = opCodeBytes(MOVL_32_64_R rc, NONE)
                in
                    genBytes(addConstToVec (SelfVal, InlineAbsolute, code, opb), code);
                    genBytes(opRegPlus2(Group1_32_A, output, arithOpToWord ADD), code)
                end;
                handlerLab := !ic;
                genBytes(int32Signed 0, code)
            end

        |   cgOp(StartHandler{ handlerLab }) =
            let
                val Code{ic, codeVec, ...} = code
            in
                (* On X86/64 this is a relative offset on X86/32 it is absolute. *)
                set32u(Word.toLargeWord (if isX64 then !ic - !handlerLab - 0w4 else !ic), !handlerLab, codeVec)
            end

        |   cgOp(IndexedCase { testReg, workReg, min, cases }) =
                indexedCase(testReg, workReg, min, cases, code)

        |   cgOp (FreeRegisters _ ) = ()

        |   cgOp (FPLoadFromMemory {address={ base, offset, index }, precision}) =
            let
                val loadInstr =
                    case precision of
                        DoublePrecision => FPESC 0w5
                    |   SinglePrecision => FPESC 0w1
            in
                case index of
                    NoIndex => genBytes(opPlus2(loadInstr, LargeInt.fromInt offset, base, 0wx0), code)
                |   index => genBytes(opIndexedPlus2(loadInstr, LargeInt.fromInt offset, base, index, 0wx0), code)
            end

        |   cgOp (FPLoadFromFPReg{source=FloatingPtReg fp, ...}) =
                (* Assume there's nothing currently on the stack. *)
                genBytes(floatingPtOp({escape=0w1, md=0w3, nnn=0w0, rm= fp + 0w0}), code) (* FLD ST(r1) *)

        |   cgOp (FPLoadFromConst realValue ) =
            let
                open Real
                infix ==
            in
                (* Treat +/- 0,1 as special cases. *)
                if realValue == 0.0 (* This is also true for -0.0 *)
                then
                (
                    genBytes(floatingPtOp({escape=0w1, md=0w3, nnn=0w5, rm=0w6}), code); (* FLDZ *)
                    if signBit realValue
                    then genBytes(floatingPtOp({escape=0w1, md=0w3, nnn=0w4, rm=0w0}), code)
                    else ()
                )
                else if realValue == 1.0
                then genBytes(floatingPtOp({escape=0w1, md=0w3, nnn=0w5, rm=0w0}), code) (* FLD1 *)
                else if realValue == ~1.0
                then
                (
                    genBytes(floatingPtOp({escape=0w1, md=0w3, nnn=0w5, rm=0w0}), code); (* FLD1 *)
                    genBytes(floatingPtOp({escape=0w1, md=0w3, nnn=0w4, rm=0w0}), code) (* FCHS *)
                )
                else
                (* The real constant here is actually the address of an 8-byte memory
                   object.  FLD takes the address as the argument and in 32-bit mode
                   we use an absolute address.  In 64-bit mode we need to put the
                   constant at the end of the code segment and use PC-relative
                   addressing which happens to be encoded in the same way. *)
                let
                    val opb = opCodeBytes(FPESC 0w5, NONE) (* FLD [Constant] *)
                    val mdrm = modrm (Based0, 0w0, 0w5 (* constant address/PC-relative *))
                in
                    if isX64
                    then genBytes(addNonAddressConstant(NonAddressReal(toMachineWord realValue), code, opb @ [mdrm]), code)
                    else genBytes(addConstToVec(WVal(toMachineWord realValue), InlineAbsolute, code, opb @ [mdrm]), code)
                end
            end

        |   cgOp (FPStoreToFPReg{ output=FloatingPtReg dest, andPop }) =
                (* Assume there's one item on the stack. *)
                genBytes(floatingPtOp({escape=0w5, md=0w3, nnn=if andPop then 0wx3 else 0wx2,
                               rm = dest+0w1(* One item *)}), code) (* FSTP ST(n+1) *)

        |   cgOp (FPStoreToMemory{address={ base, offset, index}, precision, andPop }) =
            let
                val storeInstr =
                    case precision of
                        DoublePrecision => FPESC 0w5
                    |   SinglePrecision => FPESC 0w1
                val subInstr = if andPop then 0wx3 else 0wx2
            in
                case index of
                    NoIndex => genBytes(opPlus2(storeInstr, LargeInt.fromInt offset, base, subInstr), code)
                |   index => genBytes(opIndexedPlus2(storeInstr, LargeInt.fromInt offset, base, index, subInstr), code)
            end

        |   cgOp (FPArithR{ opc, source = FloatingPtReg src}) =
                genBytes(floatingPtOp({escape=0w0, md=0w3, nnn=fpOpToWord opc,
                        rm=src + 0w1 (* One item already there *)}), code)

        |   cgOp (FPArithConst{ opc, source }) =
                (* See comment on FPLoadFromConst *)
            let
                val opb = opCodeBytes(FPESC 0w4, NONE) (* FADD etc [constnt] *)
                val mdrm = modrm (Based0, fpOpToWord opc, 0w5 (* constant address *))
            in
                if isX64
                then genBytes(addNonAddressConstant(NonAddressReal source, code, opb @ [mdrm]), code)
                else genBytes(addConstToVec(WVal source, InlineAbsolute, code, opb @ [mdrm]), code)
            end

        |   cgOp (FPArithMemory{ opc, base, offset }) =
                genBytes(opPlus2(FPESC 0w4, LargeInt.fromInt offset, base, fpOpToWord opc), code) (* FADD/FMUL etc [r2] *)

        |   cgOp (FPUnary opc ) =
            let
                val {rm, nnn} = fpUnaryToWords opc
            in
                genBytes(floatingPtOp({escape=0w1, md=0w3, nnn=nnn, rm=rm}), code) (* FCHS etc *)
            end

        |   cgOp (FPStatusToEAX ) =
                genBytes(opCodeBytes(FPESC 0w7, NONE) @ [0wxe0], code) (* FNSTSW AX *)

        |   cgOp (FPFree(FloatingPtReg reg)) =
                genBytes(floatingPtOp({escape=0w5, md=0w3, nnn=0w0, rm=reg}), code) (* FFREE FP(n) *)

        |   cgOp (FPLoadInt{base, offset}) =
                (* fildl (esp) in 32-bit mode or fildq (esp) in 64-bit mode. *)
                if isX64
                then genBytes(opPlus2(FPESC 0w7, LargeInt.fromInt offset, base, 0w5), code)
                else genBytes(opPlus2(FPESC 0w3, LargeInt.fromInt offset, base, 0w0), code)

        |   cgOp (MultiplyRR {source, output}) =
                (* We use the 0F AF form of IMUL rather than the Group3 MUL or IMUL
                   because the former allows us to specify the destination register.
                   The Group3 forms produce double length results in RAX:RDX/EAX:EDX
                   but we only ever want the low-order half. *)
                genBytes(opReg(IMUL (* 2 byte opcode *), output, source), code)

        |   cgOp (MultiplyRM {base, offset, output}) =
                (* We use the 0F AF form of IMUL rather than the Group3 MUL or IMUL
                   because the former allows us to specify the destination register.
                   The Group3 forms produce double length results in RAX:RDX/EAX:EDX
                   but we only ever want the low-order half. *)
                genBytes(opEA(IMUL (* 2 byte opcode *), LargeInt.fromInt offset, base, output), code)

        |   cgOp (XMMArith { opc, source=MemoryArg{base, offset, index=NoIndex}, output }) =
                genBytes(mMXEA(SSE2Ops opc, LargeInt.fromInt offset, base, output), code)

        |   cgOp (XMMArith { opc, source=MemoryArg{base, offset, index}, output }) =
                genBytes(mMXIndexed(SSE2Ops opc, LargeInt.fromInt offset, base, index, output), code)

        |   cgOp (XMMArith { opc, source=AddressConstArg constVal, output=SSE2Reg rrC }) =
            let
               (* The real constant here is actually the address of an 8-byte memory
                   object.  The SSE2 instruction takes the address as the argument and in 32-bit mode
                   we use an absolute address.  In 64-bit mode we need to put the
                   constant at the end of the code segment and use PC-relative
                   addressing which happens to be encoded in the same way. *)
                val opb = opCodeBytes(SSE2Ops opc, NONE)
                val mdrm = modrm (Based0, rrC, 0w5 (* constant address/PC-relative *))
            in
                if isX64
                then genBytes(addNonAddressConstant(NonAddressReal constVal, code, opb @ [mdrm]), code)
                else genBytes(addConstToVec(WVal constVal, InlineAbsolute, code, opb @ [mdrm]), code)
            end

        |   cgOp (XMMArith { opc, source=RegisterArg(SSE2Reg rrS), output=SSE2Reg rrC }) =
            let
                val oper = SSE2Ops opc
                val pref = opcodePrefix oper
                val esc = escapePrefix oper
                val opc = opToInt oper
                val mdrm = modrm(Register, rrC, rrS)
            in
                genBytes(pref @ esc @ [opc, mdrm], code)
            end

        |   cgOp (XMMArith _) = raise InternalError "cgOp: XMMArith"

        |   cgOp (XMMStoreToMemory { toStore, address={base, offset, index}, precision }) =
            let
                val oper =
                    case precision of
                        DoublePrecision => SSE2StoreDouble
                    |   SinglePrecision => SSE2StoreSingle
            in
                case index of
                    NoIndex => genBytes(mMXEA(oper, LargeInt.fromInt offset, base, toStore), code)
                |   index => genBytes(mMXIndexed(oper, LargeInt.fromInt offset, base, index, toStore), code)
            end

        |   cgOp (XMMConvertFromInt { source, output=SSE2Reg rrC }) =
            let
                (* The source is a general register and the output a XMM register. *)
                val (rbC, rbX) = getReg source
            in
                genBytes(
                    opcodePrefix CVTSI2SD @ rexByte(CVTSI2SD, false, rbX, false) @
                    escapePrefix CVTSI2SD @ [opToInt CVTSI2SD, modrm(Register, rrC, rbC)], code)
            end

        |   cgOp (SignExtendForDivide ) =
                genBytes(opCodeBytes(CQO_CDQ, if isX64 then SOME {w=true, r=false, b=false, x=false} else NONE), code)
            
        |   cgOp (XChngRegisters { regX, regY}) =
                (* We only use register-to-register exchange.  That doesn't cause a lock. *)
                genBytes(opReg(XCHNG, regX, regY), code)

        |   cgOp (Negative {output}) =
                genBytes(opRegPlus2(Group3_A, output, 0w3 (* neg *)), code)
    in
        List.app cgOp ops
    end

    (* It is convenient to have AllocStore and AllocStoreVariable as primitives at the higher
       level but at this point it's better to expand them into their basic instructions. *)
    local
        (* A number of bugs have turned out to be related to starting a second allocation before the
           first area is fully initialised. This is intended to avoid them. *)
        val inAllocation = ref false

        (* On X86/64 the local pointer is in r15.  On X86/32 it's in memRegs. *)
        val localPointer =
            if isX64 then RegisterArg r15 else MemoryArg{base=ebp, offset=memRegLocalMPointer, index=NoIndex}

        fun allocStoreCommonCode (resultReg, isVarAlloc, regSaveSet: genReg list) =
        let
            val compare =
                ArithToGenReg{opc=CMP, output=resultReg, source=MemoryArg{base=ebp, offset=memRegLocalMbottom, index=NoIndex}}
            (* Normally we won't have run out of store so we want the default
               branch prediction to skip the test here. However doing that
               involves adding an extra branch which lengthens the code so
               it's probably not worth while. *)
            (* Just checking against the lower limit can fail
               in the situation where the heap pointer is at the low end of
               the address range and the store required is so large that the
               subtraction results in a negative number.  In that case it
               will be > (unsigned) lower_limit so in addition we have
               to check that the result is < (unsigned) heap_pointer.
               This actually happened on Windows with X86-64.
               In theory this can happen with fixed-size allocations as
               well as variable allocations but in practice fixed-size
               allocations are going to be small enough that it's not a
               problem.  *)
            val destLabel = mkLabel()
            val branches =
                if isVarAlloc
                then
                let
                    val extraLabel = mkLabel()
                in
                    [ConditionalBranch{test=JB, label=extraLabel, predict=PredictNotTaken},
                     ArithToGenReg{opc=CMP, output=resultReg, source=localPointer},
                     ConditionalBranch{test=JB, label=destLabel, predict=PredictTaken},
                     JumpLabel extraLabel]
                end
                else [ConditionalBranch{test=JNB, label=destLabel, predict=PredictTaken}]
            val callRts = CallRTS{rtsEntry=HeapOverflowCall, saveRegs=regSaveSet}
            val fixup = JumpLabel destLabel
            (* Update the heap pointer now we have the store.  This is also
               used by the RTS in the event of a trap to work out how much
               store was being allocated. *)
            val update =
                if isX64 then MoveToRegister{source=RegisterArg resultReg, output=r15}
                else StoreRegToMemory{toStore=resultReg, address={base=ebp, offset=memRegLocalMPointer, index=NoIndex}}
        in
            compare :: branches @ [callRts, fixup, update]
        end
        
        fun doExpansion([], code) = code

        |   doExpansion(AllocStore {size, output, saveRegs} :: instrs, code) =
            let
                val _ = !inAllocation andalso raise InternalError "doExpansion: Allocation started but not complete"
                val _ = inAllocation := true
                val () = if List.exists (fn r => r = output) saveRegs then raise InternalError "AllocStore: in set" else ()
                val bytes = (size + 1) * wordSize
                val startCode =
                    if isX64
                    then [LoadAddress{output=output, offset = ~ bytes, base=SOME r15, index=NoIndex}]
                             (* TODO: What if it's too big to fit? *)
                    else
                        [MoveToRegister{source=MemoryArg{base=ebp, offset=memRegLocalMPointer, index=NoIndex}, output=output},
                         LoadAddress{output=output, offset = ~ bytes, base=SOME output, index=NoIndex}]
                val resultCode = startCode @ allocStoreCommonCode(output, false, saveRegs)
            in
                doExpansion(instrs, (List.rev resultCode) @ code)
            end

        |   doExpansion(AllocStoreVariable {output, saveRegs} :: instrs, code) =
            let
                (* Allocates memory whose size as a number of bytes is in the output register. *)
                val _ = !inAllocation andalso raise InternalError "doExpansion: Allocation started but not complete"
                val _ = inAllocation := true
                val () = if List.exists (fn r => r = output) saveRegs then raise InternalError "AllocStore: in set" else ()
                (* Negate the length and add it to the current heap pointer. *)
                val startCode =
                    [Negative{output=output}, ArithToGenReg{opc=ADD, output=output, source=localPointer}]
                val resultCode = startCode @ allocStoreCommonCode(output, true, saveRegs)
            in
                doExpansion(instrs, (List.rev resultCode) @ code)
            end

        |   doExpansion(StoreInitialised :: instrs, code) =
                (inAllocation := false; doExpansion(instrs, code))

        |   doExpansion(instr :: instrs, code) = doExpansion(instrs, instr::code)
    in
        fun expandComplexOperations instrs = List.rev(doExpansion(instrs, []))
    end


    fun printCode (Code{procName, printStream, ...}, seg) =
    let
        val print = printStream
        val ptr = ref 0w0;
        (* prints a string representation of a number *)
        fun printValue v =
            if v < 0 then (print "-"; print(LargeInt.toString  (~ v))) else print(LargeInt.toString v)

        infix 3 +:= ;
        fun (x +:= y) = (x := !x + (y:word));

        fun get16s (a, seg: cseg) : int =
        let
            val b0  = Word8.toInt (csegGet (seg, a));
            val b1  = Word8.toInt (csegGet (seg, a + 0w1));
            val b1' = if b1 >= 0x80 then b1 - 0x100 else b1;
        in
            (b1' * 0x100) + b0
        end
 
        fun print32 () = printValue (get32s (!ptr, seg)) before (ptr +:= 0w4)
        and print64 () = printValue (get64s (!ptr, seg)) before (ptr +:= 0w8)
        and print16 () = printValue (LargeInt.fromInt(get16s (!ptr, seg)) before (ptr +:= 0w2))
        and print8 () = printValue (LargeInt.fromInt(get8s (!ptr, seg)) before (ptr +:= 0w1))
 
        fun printJmp () =
        let
            val valu = get8s (!ptr, seg)  before ptr +:= 0w1
        in
            print (Word.fmt StringCvt.HEX (Word.fromInt valu + !ptr))
        end
 
        (* Print an effective address.  The register field may designate a general register
           or an xmm register depending on the instruction. *)
        fun printEAGeneral printRegister (rex, sz) =
        let
            val modrm = csegGet (seg, !ptr)
            val () = ptr +:= 0w1
            (* Decode the Rex prefix if present. *)
            val rexX = (rex andb8 0wx2) <> 0w0
            val rexB = (rex andb8 0wx1) <> 0w0
            val prefix =
                case sz of
                    SZByte  => "byte ptr "
                |   SZWord  => "word ptr "
                |   SZDWord => "dword ptr "
                |   SZQWord => "qword ptr "
        in
            case (modrm >>- 0w6, modrm andb8 0w7, isX64) of
                (0w3, rm, _) => printRegister(rm, rexB, sz)
      
            |   (md, 0w4, _) =>
                let (* s-i-b present. *)
                    val sib = csegGet (seg, !ptr)
                    val () = ptr +:= 0w1
                    val ss    = sib >>- 0w6
                    val index = (sib  >>- 0w3) andb8 0w7
                    val base   = sib andb8 0w7
                in
                    print prefix;

                    case (md, base, isX64) of
                        (0w1, _, _) => print8 ()
                    |   (0w2, _, _) => print32 ()
                    |   (0w0, 0w5, _) => print32 () (* Absolute in 32-bit mode.  PC-relative in 64-bit ?? *)
                    |   _ => ();
          
                    print "[";
        
                    if md <> 0w0 orelse base <> 0w5
                    then
                    (
                        print (genRegRepr (mkReg (base, rexB), sz32_64));
                        if index = 0w4 then () else print ","
                    )
                    else ();
        
                    if index = 0w4 (* No index. *)
                    then ()
                    else print (genRegRepr (mkReg(index, rexX), sz32_64) ^ 
                            (if ss = 0w0 then "*1"
                            else if ss = 0w1 then "*2"
                            else if ss = 0w2 then "*4"
                            else "*8"));
        
                    print "]"
                end
      
            |   (0w0, 0w5, false) => (* Absolute address.*) (print prefix; print32 ())

            |   (0w0, 0w5, _) => (* PC-relative in 64-bit  *)
                        (print prefix; print ".+"; print32 ())
            
            |   (md, rm, _) => (* register plus offset. *)
                (
                    print prefix;
                    if md = 0w1 then print8 ()
                    else if md = 0w2 then print32 ()
                    else ();
         
                    print ("[" ^ genRegRepr (mkReg(rm, rexB), sz32_64) ^ "]")
                )
        end
        
        (* For most instructions we want to print a general register. *)
        val printEA =
            printEAGeneral (fn (rm, rexB, sz) => print (genRegRepr (mkReg(rm, rexB), sz)))
        and printEAxmm =
            printEAGeneral (fn (rm, _, _) => print (xmmRegRepr(SSE2Reg rm)))
 
        fun printArith opc =
            print
               (case opc of
                  0 => "add "
                | 1 => "or  "
                | 2 => "adc "
                | 3 => "sbb "
                | 4 => "and "
                | 5 => "sub "
                | 6 => "xor "
                | _ => "cmp "
               )

        fun printGvEv (opByte, rex, rexR, sz) =
        let
            (* Register is in next byte. *)
            val nb = csegGet (seg, !ptr)
            val reg = (nb >>- 0w3) andb8 0w7
        in
            printArith(Word8.toInt((opByte div 0w8) mod 0w8));
            print "\t";
            print (genRegRepr (mkReg(reg, rexR), sz));
            print ",";
            printEA(rex, sz)
        end
        
        fun printMovCToR (opByte, sz, rexB) =
        (
            print "mov \t";
            print(genRegRepr (mkReg (opByte mod 0w8, rexB), sz));
            print ",";
            case sz of SZDWord => print32 () | SZQWord => print64 () | _ => print "???"
        )
        
        fun printShift (opByte, rex, sz) =
        let
            (* Opcode is determined by next byte. *)
            val nb = Word8.toInt (csegGet (seg, !ptr))
            val opc = (nb div 8) mod 8
        in
            print
               (case opc of
                  4 => "shl "
                | 5 => "shr "
                | 7 => "sar "
                | _ => "???"
               );
            print "\t";
            printEA(rex, sz);
            print ",";
            if opByte = opToInt Group2_1_A then print "1"
            else if opByte = opToInt Group2_CL_A then print "cl"
            else print8 ()
        end
        
        fun printFloat (opByte, rex) =
        let
            (* Opcode is in next byte. *)
            val opByte2  = csegGet (seg, !ptr)
            val nnn = (opByte2 >>- 0w3) andb8 0w7
            val escNo = opByte andb8 0wx7
        in
            if (opByte2 andb8 0wxC0) = 0wxC0
            then (* mod = 11 *)
            (
                case (escNo, nnn, opByte2 andb8 0wx7 (* modrm *)) of
                    (0w1, 0w4, 0w0) => print "fchs"
                |   (0w1, 0w4, 0w1) => print "fabs"
                |   (0w1, 0w5, 0w6) => print "fldz"
                |   (0w1, 0w5, 0w1) => print "flf1"
                |   (0w7, 0w4, 0w0) => print "fnstsw\tax"
                |   (0w1, 0w5, 0w0) => print "fld1"
                |   (0w1, 0w6, 0w3) => print "fpatan"
                |   (0w1, 0w7, 0w2) => print "fsqrt"
                |   (0w1, 0w7, 0w6) => print "fsin"
                |   (0w1, 0w7, 0w7) => print "fcos"
                |   (0w1, 0w6, 0w7) => print "fincstp"
                |   (0w1, 0w6, 0w6) => print "fdecstp"
                |   (0w5, 0w2, rno) => print ("fst \tst(" ^ Word8.toString rno ^ ")")
                |   (0w5, 0w3, rno) => print ("fstp\tst(" ^ Word8.toString rno ^ ")")
                |   (0w1, 0w0, rno) => print ("fld \tst(" ^ Word8.toString rno ^ ")")
                |   (0w1, 0w1, rno) => print ("fxch\tst(" ^ Word8.toString rno ^ ")")
                |   (0w0, 0w3, rno) => print ("fcomp\tst(" ^ Word8.toString rno ^ ")")
                |   (0w0, 0w0, rno) => print ("fadd\tst,st(" ^ Word8.toString rno ^ ")")
                |   (0w0, 0w1, rno) => print ("fmul\tst,st(" ^ Word8.toString rno ^ ")")
                |   (0w0, 0w4, rno) => print ("fsub\tst,st(" ^ Word8.toString rno ^ ")")
                |   (0w0, 0w5, rno) => print ("fsubr\tst,st(" ^ Word8.toString rno ^ ")")
                |   (0w0, 0w6, rno) => print ("fdiv\tst,st(" ^ Word8.toString rno ^ ")")
                |   (0w0, 0w7, rno) => print ("fdivr\tst,st(" ^ Word8.toString rno ^ ")")
                |   (0w5, 0w0, rno) => print ("ffree\tst(" ^ Word8.toString rno ^ ")")
                |   _ => (printValue(Word8.toLargeInt opByte); printValue(Word8.toLargeInt opByte2));
                ptr +:= 0w1
            )
            else (* mod = 00, 01, 10 *)
            (
                case (escNo, nnn) of
                    (0w1, 0w0) => (print "fld \t"; printEA(rex, SZDWord)) (* Single precision. *)
                |   (0w1, 0w2) => (print "fst\t"; printEA(rex, SZDWord))
                |   (0w1, 0w3) => (print "fstp\t"; printEA(rex, SZDWord))
                |   (0w3, 0w0) => (print "fildl\t"; printEA(rex, SZQWord))
                |   (0w7, 0w5) => (print "fildq\t"; printEA(rex, SZQWord))
                |   (0w4, 0w0) => (print "fadd\t"; printEA(rex, SZQWord))
                |   (0w4, 0w1) => (print "fmul\t"; printEA(rex, SZQWord))
                |   (0w4, 0w3) => (print "fcomp\t"; printEA(rex, SZQWord))
                |   (0w4, 0w4) => (print "fsub\t"; printEA(rex, SZQWord))
                |   (0w4, 0w5) => (print "fsubr\t"; printEA(rex, SZQWord))
                |   (0w4, 0w6) => (print "fdiv\t"; printEA(rex, SZQWord))
                |   (0w4, 0w7) => (print "fdivr\t"; printEA(rex, SZQWord))
                |   (0w5, 0w0) => (print "fld \t"; printEA(rex, SZQWord)) (* Double precision. *)
                |   (0w5, 0w2) => (print "fst\t"; printEA(rex, SZQWord))
                |   (0w5, 0w3) => (print "fstp\t"; printEA(rex, SZQWord))
                |   _ => (printValue(Word8.toLargeInt opByte); printValue(Word8.toLargeInt opByte2))
            )
        end
        
        fun printJmp32 oper =
        let
            val valu = get32s (!ptr, seg) before (ptr +:= 0w4)
        in
            print oper; print "\t";
            print (Word.fmt StringCvt.HEX (!ptr + Word.fromLargeInt valu))
        end

        fun printMask mask =
        let
            val wordMask = Word.fromInt mask
            fun printAReg n =
                if n = regs then ()
                else
                (
                    if (wordMask andb (0w1 << Word.fromInt n)) <> 0w0
                    then (print(regRepr(regN n)); print " ")
                    else ();
                    printAReg(n+1)
                )
        in
            printAReg 0
        end

    in

        if procName = "" (* No name *) then print "?" else print procName;
        print ":\n";
 
        while get8u (!ptr, seg) <> 0wxf4 (* HLT. *) do
        let
            val () = print (Word.fmt StringCvt.HEX (!ptr)) (* The address in hex. *)
            val () = print "\t"

            (* See if we have a lock prefix. *)
            val () =
                if get8u (!ptr, seg) = 0wxF0
                then (print "lock "; ptr := !ptr + 0w1)
                else ()
                
            val legacyPrefix =
                let
                    val p = get8u (!ptr, seg)
                in
                    if p = 0wxF2 orelse p = 0wxF3 orelse p = 0wx66
                    then (ptr := !ptr + 0w1; p)
                    else 0wx0
                end

            (* See if we have a REX byte. *)
            val rex =
            let
               val b = get8u (!ptr, seg);
            in
               if b >= 0wx40 andalso b <= 0wx4f
               then (ptr := !ptr + 0w1; b)
               else 0w0
            end
        
            val rexW = (rex andb8 0wx8) <> 0w0
            val rexR = (rex andb8 0wx4) <> 0w0
            val rexB = (rex andb8 0wx1) <> 0w0

            val opByte = get8u (!ptr, seg) before ptr +:= 0w1
            
            val sizeFromRexW =
                if rexW then SZQWord else if legacyPrefix = 0wx66 then SZWord else SZDWord
        in
            case opByte of
                0wx03 => printGvEv (opByte, rex, rexR, sizeFromRexW)

            |   0wx0b => printGvEv (opByte, rex, rexR, sizeFromRexW)

            |   0wx0f => (* ESCAPE *)
                let
                    (* Opcode is in next byte. *)
                    val opByte2  = csegGet (seg, !ptr)
                    val () = (ptr +:= 0w1)
                in
                    case legacyPrefix of
                        0w0 =>
                        (
                            case opByte2 of
                                0wxC1 =>
                                let
                                    val nb = csegGet (seg, !ptr);
                                    val reg = (nb >>- 0w3) andb8 0w7
                                in
                                    (* The address argument comes first in the assembly code. *)
                                    print "xadd\t";
                                    printEA (rex, sizeFromRexW);
                                    print ",";
                                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW))
                                end

                            |   0wxB6 =>
                                let
                                    val nb = csegGet (seg, !ptr);
                                    val reg = (nb >>- 0w3) andb8 0w7
                                in
                                    print "movzx\t";
                                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW));
                                    print ",";
                                    printEA (rex, SZByte)
                                end

                            |   0wxB7 =>
                                let
                                    val nb = csegGet (seg, !ptr);
                                    val reg = (nb >>- 0w3) andb8 0w7
                                in
                                    print "movzx\t";
                                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW));
                                    print ",";
                                    printEA (rex, SZWord)
                                end

                            |   0wxAF =>
                                let
                                    val nb = csegGet (seg, !ptr);
                                    val reg = (nb >>- 0w3) andb8 0w7
                                in
                                    print "imul\t";
                                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW));
                                    print ",";
                                    printEA (rex, sizeFromRexW)
                                end

                            |   0wx80 => printJmp32 "jo  "
                            |   0wx81 => printJmp32 "jno "
                            |   0wx82 => printJmp32 "jb  "
                            |   0wx83 => printJmp32 "jnb "
                            |   0wx84 => printJmp32 "je  "
                            |   0wx85 => printJmp32 "jne "
                            |   0wx86 => printJmp32 "jna "
                            |   0wx87 => printJmp32 "ja  " 
                            |   0wx88 => printJmp32 "js  "
                            |   0wx89 => printJmp32 "jns " 
                            |   0wx8a => printJmp32 "jp  "
                            |   0wx8b => printJmp32 "jnp " 
                            |   0wx8c => printJmp32 "jl  "
                            |   0wx8d => printJmp32 "jge "
                            |   0wx8e => printJmp32 "jle "
                            |   0wx8f => printJmp32 "jg  "
                            
                            |   _ => (print "esc\t"; printValue(Word8.toLargeInt opByte2))
                        )
                    
                    |   0wxf2 => (* SSE2 instruction *)
                        let
                            val nb = csegGet (seg, !ptr)
                            val reg = SSE2Reg((nb >>- 0w3) andb8 0w7)
                        in
                            case opByte2 of
                                0wx10 => ( print "movsd\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZQWord) )
                            |   0wx11 => ( print "movsd\t"; printEAxmm(rex, SZQWord); print ","; print(xmmRegRepr reg)  )
                            |   0wx2a => ( print "cvtsi2sd\t"; print(xmmRegRepr reg); print ","; printEA(rex, sizeFromRexW)  )
                            |   0wx58 => ( print "addsd\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZQWord) )
                            |   0wx59 => ( print "mulsd\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZQWord) )
                            |   0wx5a => ( print "cvtsd2ss\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZQWord) )
                            |   0wx5c => ( print "subsd\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZQWord) )
                            |   0wx5e => ( print "divsd\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZQWord) )
                            |   b => (print "F2\n"; print "0F\n"; print(Word8.fmt StringCvt.HEX b))
                        end

                    |   0wxf3 => (* SSE2 instruction. *)
                        let
                            val nb = csegGet (seg, !ptr)
                            val reg = SSE2Reg((nb >>- 0w3) andb8 0w7)
                        in
                            case opByte2 of
                                0wx5a => ( print "cvtss2sd\t"; print(xmmRegRepr reg); print ","; printEA(rex, sizeFromRexW)  )
                            |   0wx11 => ( print "movss\t"; printEAxmm(rex, SZDWord); print ","; print(xmmRegRepr reg)  )
                            |   b => (print "F3\n"; print "0F\n"; print(Word8.fmt StringCvt.HEX b))
                        end

                    |   0wx66 => (* SSE2 instruction *)
                        let
                            val nb = csegGet (seg, !ptr)
                            val reg = SSE2Reg((nb >>- 0w3) andb8 0w7)
                        in
                            case opByte2 of
                                0wx2e => ( print "ucomisd\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZQWord) )
                            |   0wx54 => ( print "andpd\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZQWord) )
                            |   0wx57 => ( print "xorpd\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZQWord) )
                            |   b => (print "66\n"; print "0F\n"; print(Word8.fmt StringCvt.HEX b))
                        end

                    |   _ => (print "esc\t"; printValue(Word8.toLargeInt opByte2))
                end (* ESCAPE *)

            |   0wx13 => printGvEv (opByte, rex, rexR, sizeFromRexW)

            |   0wx1b => printGvEv (opByte, rex, rexR, sizeFromRexW)

            |   0wx23 => printGvEv (opByte, rex, rexR, sizeFromRexW)

            |   0wx2b => printGvEv (opByte, rex, rexR, sizeFromRexW)

            |   0wx33 => printGvEv (opByte, rex, rexR, sizeFromRexW)

            |   0wx3b => printGvEv (opByte, rex, rexR, sizeFromRexW)

                (* Push and Pop.  These are 64-bit on X86/64 whether there is REX prefix or not. *)
            |   0wx50 => print ("push\t" ^  genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx51 => print ("push\t" ^  genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx52 => print ("push\t" ^  genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx53 => print ("push\t" ^  genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx54 => print ("push\t" ^  genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx55 => print ("push\t" ^  genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx56 => print ("push\t" ^  genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx57 => print ("push\t" ^  genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))

            |   0wx58 => print ("pop \t" ^ genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx59 => print ("pop \t" ^ genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx5a => print ("pop \t" ^ genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx5b => print ("pop \t" ^ genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx5c => print ("pop \t" ^ genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx5d => print ("pop \t" ^ genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx5e => print ("pop \t" ^ genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx5f => print ("pop \t" ^ genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))

            |   0wx68 =>(print "push\t"; print32 ())
         
            |   0wx6a =>(print "push\t"; print8 ())

            |   0wx70 => (print "jo  \t"; printJmp())
            |   0wx71 => (print "jno \t"; printJmp())
            |   0wx72 => (print "jb  \t"; printJmp())
            |   0wx73 => (print "jnb \t"; printJmp())
            |   0wx74 => (print "je  \t"; printJmp())
            |   0wx75 => (print "jne \t"; printJmp())
            |   0wx76 => (print "jna \t"; printJmp())
            |   0wx77 => (print "ja  \t"; printJmp())
            |   0wx78 => (print "js  \t"; printJmp())
            |   0wx79 => (print "jns \t"; printJmp())
            |   0wx7a => (print "jp  \t"; printJmp())
            |   0wx7b => (print "jnp \t"; printJmp())
            |   0wx7c => (print "jl  \t"; printJmp())
            |   0wx7d => (print "jge \t"; printJmp())
            |   0wx7e => (print "jle \t"; printJmp())
            |   0wx7f => (print "jg  \t"; printJmp())

            |   0wx80 => (* Group1_8_a *)
                let (* Memory, byte constant *)
                    (* Opcode is determined by next byte. *)
                    val nb = Word8.toInt (csegGet (seg, !ptr))
                in
                    printArith ((nb div 8) mod 8);
                    print "\t";
                    printEA(rex, SZByte);
                    print ",";
                    print8 ()
                end

            |   0wx81 =>
                let (* Memory, 32-bit constant *)
                    (* Opcode is determined by next byte. *)
                    val nb = Word8.toInt (csegGet (seg, !ptr))
                in
                    printArith ((nb div 8) mod 8);
                    print "\t";
                    printEA(rex, sizeFromRexW);
                    print ",";
                    print32 ()
                end

            |   0wx83 =>
                let (* Word memory, 8-bit constant *)
                    (* Opcode is determined by next byte. *)
                    val nb = Word8.toInt (csegGet (seg, !ptr))
                in
                    printArith ((nb div 8) mod 8);
                    print "\t";
                    printEA(rex, sizeFromRexW);
                    print ",";
                    print8 ()
                end

            |   0wx87 =>
                let (* xchng *)
                    (* Register is in next byte. *)
                    val nb = csegGet (seg, !ptr)
                    val reg = (nb >>- 0w3) andb8 0w7
                in
                    print "xchng \t";
                    printEA(rex, sizeFromRexW);
                    print ",";
                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW))
                end

            |   0wx88 =>
                let (* mov eb,gb i.e a store *)
                    (* Register is in next byte. *)
                    val nb = Word8.toInt (csegGet (seg, !ptr));
                    val reg = (nb div 8) mod 8;
                in
                    print "mov \t";
                    printEA(rex, SZByte);
                    print ",";
                    if rexR
                    then print ("r" ^ Int.toString(reg+8) ^ "B")
                    else case reg of
                        0 => print "al"
                    |   1 => print "cl"
                    |   2 => print "dl"
                    |   3 => print "bl"
                         (* If there is a REX byte these select the low byte of the registers. *)
                    |   4 => print (if rex = 0w0 then "ah" else "sil")
                    |   5 => print (if rex = 0w0 then "ch" else "dil")
                    |   6 => print (if rex = 0w0 then "dh" else "bpl")
                    |   7 => print (if rex = 0w0 then "bh" else "spl")
                    |   _ => print ("r" ^ Int.toString reg)
                end

            |   0wx89 =>
                let (* mov ev,gv i.e. a store *)
                    (* Register is in next byte. *)
                    val nb = csegGet (seg, !ptr)
                    val reg = (nb >>- 0w3) andb8 0w7
                in
                    print "mov \t";
                    printEA(rex, sizeFromRexW);
                    print ",";
                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW))
                end
         
            |   0wx8b =>
                let (* mov gv,ev i.e. a load *)
                    (* Register is in next byte. *)
                    val nb = csegGet (seg, !ptr)
                    val reg = (nb >>- 0w3) andb8 0w7
                in
                    print "mov \t";
                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW));
                    print ",";
                    printEA(rex, sizeFromRexW)
                end

            |   0wx8d =>
                let (* lea gv.M *)
                    (* Register is in next byte. *)
                    val nb = csegGet (seg, !ptr)
                    val reg = (nb >>- 0w3) andb8 0w7
                in
                    print "lea \t";
                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW));
                    print ",";
                    printEA(rex, sizeFromRexW)
                end
         
            |   0wx8f => (print "pop \t"; printEA(rex, sz32_64))
            |   0wx90 => print "nop"
            
            |   0wx99 => if rexW then print "cqo" else print "cdq"
            
            |   0wx9e => print "sahf\n"

            |   0wxa4 => (if legacyPrefix = 0wxf3 then print "rep " else (); print "movsb")
            |   0wxa5 => (if legacyPrefix = 0wxf3 then print "rep " else (); print "movsl")
            |   0wxa6 => (if legacyPrefix = 0wxf3 then print "repe " else (); print "cmpsb")

            |   0wxa8 => (print "test\tal,"; print8 ())

            |   0wxaa => (if legacyPrefix = 0wxf3 then print "rep " else (); print "stosb")
            |   0wxab =>
                (
                    if legacyPrefix = 0wxf3 then print "rep " else ();
                    if rexW then print "stosq" else print "stosl"
                )

            |   0wxb8 => printMovCToR (opByte, sizeFromRexW, rexB)
            |   0wxb9 => printMovCToR (opByte, sizeFromRexW, rexB)
            |   0wxba => printMovCToR (opByte, sizeFromRexW, rexB)
            |   0wxbb => printMovCToR (opByte, sizeFromRexW, rexB)
            |   0wxbc => printMovCToR (opByte, sizeFromRexW, rexB)
            |   0wxbd => printMovCToR (opByte, sizeFromRexW, rexB)
            |   0wxbe => printMovCToR (opByte, sizeFromRexW, rexB)
            |   0wxbf => printMovCToR (opByte, sizeFromRexW, rexB)
   
            |   0wxc1 => (* Group2_8_A *) printShift (opByte, rex, sizeFromRexW)

            |   0wxc2 => (print "ret \t"; print16 ())
            |   0wxc3 => print "ret"
         
            |   0wxc6 => (* move 8-bit constant to memory *)
                (
                    print "mov \t";
                    printEA(rex, SZByte);
                    print ",";
                    print8 ()
                )

            |   0wxc7 => (* move 32/64-bit constant to memory *)
                (
                    print "mov \t";
                    printEA(rex, sizeFromRexW);
                    print ",";
                    print32 ()
                )
            
            |   0wxca => (* Register mask *)
                let
                    val mask = get16s (!ptr, seg) before (ptr +:= 0w2)
                in
                    print "SAVE\t";
                    printMask mask
                end

            |   0wxcd => (* Register mask *)
                let
                    val mask = get8s (!ptr, seg) before (ptr +:= 0w1)
                in
                    print "SAVE\t";
                    printMask mask
                end

            |   0wxd1 => (* Group2_1_A *) printShift (opByte, rex, sizeFromRexW)

            |   0wxd3 => (* Group2_CL_A *) printShift (opByte, rex, sizeFromRexW)
           
            |   0wxd8 => printFloat (opByte, rex) (* Floating point escapes *)
            |   0wxd9 => printFloat (opByte, rex)
            |   0wxda => printFloat (opByte, rex)
            |   0wxdb => printFloat (opByte, rex)
            |   0wxdc => printFloat (opByte, rex)
            |   0wxdd => printFloat (opByte, rex)
            |   0wxde => printFloat (opByte, rex)
            |   0wxdf => printFloat (opByte, rex)

            |   0wxe8 =>
                let (* 32-bit relative call. *)
                    val valu = get32s (!ptr, seg) before (ptr +:= 0w4)
                in
                    print "call\t";
                    print (Word.fmt StringCvt.HEX (!ptr + Word.fromLargeInt valu))
                end

            |   0wxe9 =>
                let (* 32-bit relative jump. *)
                    val valu = get32s (!ptr, seg) before (ptr +:= 0w4)
                in
                    print "jmp \t";
                    print (Word.fmt StringCvt.HEX (!ptr + Word.fromLargeInt valu))
                end

            |   0wxeb => (print "jmp \t"; printJmp())
            
            |   0wxf4 => print "hlt" (* Marker to indicate end-of-code. *)
        
            |   0wxf6 => (* Group3_a *)
                let
                    (* Opcode is determined by next byte. *)
                    val nb = Word8.toInt (csegGet (seg, !ptr))
                    val opc = (nb div 8) mod 8
                in
                    print
                      (case opc of
                         0 => "test"
                       | 3 => "neg"
                       | _ => "???"
                      );
                    print "\t";
                    printEA(rex, SZByte);
                    if opc = 0 then (print ","; print8 ()) else ()
                end

            |   0wxf7 => (* Group3_A *)
                let
                    (* Opcode is determined by next byte. *)
                    val nb = Word8.toInt (csegGet (seg, !ptr))
                    val opc = (nb div 8) mod 8
                in
                    print
                      (case opc of
                         0 => "test"
                       | 3 => "neg "
                       | 4 => "mul "
                       | 5 => "imul"
                       | 6 => "div "
                       | 7 => "idiv"
                       | _ => "???"
                      );
                    print "\t";
                    printEA(rex, sizeFromRexW);
                    (* Test has an immediate operand.  It's 32-bits even in 64-bit mode. *)
                    if opc = 0 then (print ","; print32 ()) else ()
                end
         
            |   0wxff => (* Group5 *)
                let
                    (* Opcode is determined by next byte. *)
                    val nb = Word8.toInt (csegGet (seg, !ptr))
                    val opc = (nb div 8) mod 8
                in
                    print
                      (case opc of
                         2 => "call"
                       | 4 => "jmp "
                       | 6 => "push"
                       | _ => "???"
                      );
                    print "\t";
                    printEA(rex, sz32_64) (* None of the cases we use need a prefix. *)
                end
 
            |   _ => print(Word8.fmt StringCvt.HEX opByte);
      
            print "\n"
        end; (* end of while loop *)

        print "\n"

    end (* printCode *);

    (* Adds the constants onto the code, and copies the code into a new segment *)
    fun createCodeSegment (operations, cvec) : address =
    let
        val expanded = expandComplexOperations operations
        val () = codeGenerate(expanded, cvec)
        val () = genBytes(opCodeBytes(HLT, NONE), cvec) (* Marker - this is used by ScanConstants in the RTS. *)

        (* After code generation get the final values of some refs. *)
        val Code{codeVec, ic, constVec = ref constVec, nonInlineConsts = ref constsInConstArea,
                 nonAddressConstants = ref nonAddressConstants,
                 procName, printAssemblyCode, printStream, profileObject, ...} = cvec
    
        (* This aligns ic onto a fullword boundary. *)
        val ()   = while Word.toInt (!ic) mod wordSize <> 0 do genBytes(opCodeBytes(HLT, NONE), cvec)
        
        (* Copy the non-address constants.  These are currently only used for real
           constants in 64-bit mode.  Other constants are left until we have a
           valid code object. *)
        local
            fun putNonAddrConst{const, addrs} =
                let
                    val addrOfConst = ! ic
                    val () =
                        case const of
                            NonAddressReal c =>
                            let
                                val cAsAddr = toAddress c
                                (* For the moment this should always be a real number contained in
                                   a byte segment.  If this changes in the future we may need to
                                   align this back onto a 4/8-byte boundary. *)
                                val cLength = length cAsAddr * Word.fromInt wordSize
                                val _ = (cLength = 0w8 andalso flags cAsAddr = F_bytes) orelse
                                            raise InternalError "putNonAddrConst: Not a real number"
                                fun doCopy n =
                                    if n = cLength then ()
                                    else (genBytes([loadByte(cAsAddr, n)], cvec); doCopy(n+0w1))
                            in
                                doCopy 0w0
                            end
                        |   NonAddressInt imm =>
                            let
                                fun setMem(m, n) = 
                                if n = Word.fromInt wordSize then ()
                                else 
                                (
                                    genBytes([Word8.fromLargeInt(m mod 0x100)], cvec);
                                    setMem(m div 0x100, n+0w1)
                                )
                            in
                                setMem(imm, 0w0)
                            end
                in
                    set32s(Word.toLargeInt(addrOfConst - addrs - 0w4), addrs, codeVec)
                end
        in
            val () = List.app putNonAddrConst nonAddressConstants
        end

        (* +4 for code size, function name, register mask (no longer used) and profile object. *)
        val segSize = !ic div Word.fromInt wordSize + Word.fromInt constsInConstArea + 0w4

        (* Now make the byte segment that we'll turn into the code segment *)
        val seg : cseg = csegMake segSize
    
        (* Copy the code into the new segment. *)
        val _ = csegCopySeg (codeVec, seg, (! ic), 0w0);

        local
            val endOfCode = bytesToWords(! ic)
        in
            (* Put in the number of constants. This must go in before we actually put
               in any constants.  In 32-bit mode there are only two constants: the 
               function name and the register mask. All other constants are in the code. *)
            local
                val addr = wordsToBytes(endOfCode + 0w3 + Word.fromInt constsInConstArea)
            in
                val () = setWordU(LargeInt.fromInt(3 + constsInConstArea), addr, seg)
            end;

             (* Now we've filled in all the C integers; now we need to convert the segment
               into a proper code segment before it's safe to put in any ML values. *)
            val () = csegConvertToCode seg
            val () = csegPutWord (seg, endOfCode, toMachineWord procName)
            val () = csegPutWord (seg, endOfCode + 0w1, toMachineWord 1 (* No longer used. *))
            (* Next the profile object. *)
            val () = csegPutWord (seg, endOfCode + 0w2, profileObject)
        end
    in
        let
            fun putConst {const, addrs, posn, ...} =
            let
                val value =
                    case const of WVal c => c | SelfVal => toMachineWord(csegAddr seg)
            in
                case posn of
                    InlineAbsolute => csegPutConstant (seg, addrs, value, 0w0)
                |   InlineRelative => csegPutConstant (seg, addrs, value, 0w1)
                |   ConstArea nonInlineCount =>
                    (* Not inline.  Put the constant in the constant area and set the original address
                        to be the relative offset to the constant itself. *)
                    let
                        val ref endByte = ic
                        val addrOfConst = endByte addrPlus (nonInlineCount-1 + 2+1) * wordSize
                    in
                        csegPutConstant (seg, addrOfConst, value, 0w0);
                        set32s(Word.toLargeInt(addrOfConst - addrs - 0w4), addrs, seg)
                    end
            end

            val () = List.app putConst constVec

            val () = 
                if printAssemblyCode
                then (* print out the code *)
                (
                    printCode(cvec, seg);
                    printStream "\n\n"
                )
            else ()
        in
            csegLockAndGetExecutable seg 
        end (* the result *)
    end (* copyCode *)
 
    structure Sharing =
    struct
        type code           = code
        and  reg            = reg
        and  genReg         = genReg
        and  fpReg          = fpReg
        and  addrs          = addrs
        and  operation      = operation
        and  regSet         = RegSet.regSet
        and  label          = label
        and  labList        = labList
        and  branchOps      = branchOps
    end

end (* struct *) (* CODECONS *);
