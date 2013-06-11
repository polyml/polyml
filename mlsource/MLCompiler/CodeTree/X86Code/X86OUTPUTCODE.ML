(*
    Copyright David C. J. Matthews 1989, 2000, 2009-10, 2012-13
    
    Based on original code:    
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
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

structure DEBUG :
sig
    val assemblyCodeTag : bool Universal.tag
    val lowlevelOptimiseTag: bool Universal.tag
    val getParameter :
       'a Universal.tag -> Universal.universal list -> 'a
end;

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
    val op <<+ = LargeWord.<< and op >>+ = LargeWord.>>
    val op <<- = Word8.<< and op >>- = Word8.>>

    val op orb = Word.orb and op orbL = LargeWord.orb and op orb8 = Word8.orb
    val op andb8 = Word8.andb

    val op andb = Word.andb (* and op andbL = LargeWord.andb *)

    val wordToWord8 = Word8.fromLargeWord o Word.toLargeWord
    (*and word8ToWord = Word.fromLargeWord o Word8.toLargeWord*)

    val exp2_7  =        0x80
    val exp2_8  =       0x100
    val exp2_16 =     0x10000
    val exp2_30 =        0x40000000
    val exp2_31 =        0x80000000
    val exp2_56 = 0x100000000000000
    val exp2_64 = 0x10000000000000000
  
    (* tag a short constant *)
    fun tag c = 2 * c + 1;

    fun is8Bit n = ~ 0x80 <= n andalso n < 0x80

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
    type labList = jumpFrom ref list
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

    datatype setCodeseg =
        Unset
    |   Set of cseg   (* Used for completing forward references. *)
   
  (* Constants which are too large to go inline in the code are put in
     a list and put at the end of the code. They are arranged so that
     the garbage collector can find them and change them as necessary.
     A reference to a constant is treated like a forward reference to a
     label. *)

  (* A code list is used to hold a list of code-vectors which must have the
     address of this code-vector put into it. *)

    datatype const =
        WVal of machineWord        (* an existing constant *)
    |   CVal of code        (* a forward-reference to another function *)
    |   HVal of addrs ref   (* a handler *)

    and ConstPosn =
        InlineAbsolute      (* The constant is within the code. *)
    |   InlineRelative      (* The constant is within the code but is PC relative (call or jmp). *)
    |   ConstArea of int    (* The constant is in the constant area (64-bit only). *)
    |   NonAddrArea         (* The constant is in the non-address area (64-bit real values). *)

  and code = Code of 
    { codeVec:        cseg,           (* This segment is used as a buffer. When the
                                         procedure has been code generated it is
                                         copied into a new segment of the correct size *)
      ic:             addrs ref,      (* Pointer to first free location in "codevec" *)
      constVec:                       (* Constants used in the code *)
           {const: const, addrs: addrs, posn: ConstPosn} list ref,
      numOfConsts:    word ref,        (* size of constVec *)
      nonInlineConsts: int ref,
      nonAddressConsts: int ref,
      labelList:      labList ref,    (* List of outstanding short branches. *)
      longestBranch:  addrs ref,      (* Address of the earliest 1-byte branch. *)
      procName:       string,         (* Name of the procedure. *)
      completionHooks:                (* Functions to call when we have the final code address. *)
        (code * machineWord -> unit) list ref,
      resultSeg:      setCodeseg ref, (* The segment as the final result. *)
      (* These next two are closely related but kept separate to avoid making big
         changes to the code.  They are only non-empty immediately after JumpLabel instructions.
         justComeFrom accumulates forward branches to the current location.  justComeFromAddrs
         accumulates the labels themselves if they are needed for reverse jumps. *)
      justComeFrom:   labList ref,    (* The label we have just jumped from. *)
      justComeFromAddrs: addrs ref list ref, (* *)
      exited:         bool ref,       (* False if we can fall-through to here *)
      noClosure:      bool,           (* should we make a closure from this? *)
      branchCheck:    addrs ref,      (* the address we last fixed up to.  I added
                                         this to track down a bug and I've left it
                                         in for security.  DCJM 19/1/01. *)
      printAssemblyCode:bool,            (* Whether to print the code when we finish. *)
      printStream:    string->unit,   (* The stream to use *)
      lowLevelOptimise: bool,         (* Whether to do the low-level optimisation pass *)
      profileObject   : machineWord,  (* The profile object for this code. *)
      inAllocation:   bool ref        (* Whether we have an incomplete allocation. *)
    }

    (* Exported functions *)
    fun procName       (Code {procName,...})       = procName
    and lowLevelOptimise(Code{lowLevelOptimise, ...}) = lowLevelOptimise

    (* Add a function to be called when the code is complete and the final address
       is known.  Called immediately if it has already been compiled. *)
    fun addCompletionHook(code as Code{resultSeg = ref(Set seg), ...}, hookFn) =
            hookFn(code, toMachineWord(csegAddr seg))
    |   addCompletionHook(Code{completionHooks, ...}, hookFn) =
            completionHooks := hookFn :: !completionHooks

  (* %ebp points to a structure that interfaces to the RTS.  These are
     offsets into that structure.  *)
    val memRegLocalMPointer       = 0
    and memRegHandlerRegister     = wordSize
    and memRegLocalMbottom        = wordSize*2
    and memRegStackLimit          = wordSize*3
    and memRegHeapOverflowCall    = wordSize*8
    and memRegStackOverflowCall   = wordSize*9
    and memRegStackOverflowCallEx = wordSize*10
    and memRegRaiseException      = wordSize*11
    and memRegRaiseDiv            = wordSize*13
    and memRegArbEmulation        = wordSize*14
    and memRegThreadSelf          = wordSize*15

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

    (* Test for identity of the code segments by testing whether
       the "ic" ref is the same. N.B. NOT its contents. *)
    fun sameCode(Code{ic=a, ...}, Code{ic=b, ...}) = a=b;
  
    (* create and initialise a code segment *)
    fun codeCreate (noClosure : bool, name : string, profObj, parameters) : code =
    let
        val printStream = PRETTY.getSimplePrinter parameters;
    in
        Code
        { 
            codeVec        = csegMake initialCodeSize, (* a byte array *)
            ic             = ref addrZero,
            constVec       = ref [],
            numOfConsts    = ref 0w0,
            nonInlineConsts = ref 0,
            nonAddressConsts = ref 0,
            labelList      = ref [],
            longestBranch  = ref addrLast, (* None so far *)
            procName       = name,
            completionHooks= ref [],
            resultSeg      = ref Unset,   (* Not yet done *)
            justComeFrom   = ref [],
            justComeFromAddrs = ref [],
            exited         = ref false,
            noClosure      = noClosure,
            branchCheck    = ref addrZero,
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
        val b' = if b < 0 then b + exp2_8 else b;
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
    fun set32s (ival: int, addr: addrs, seg) : unit =
        set32u(LargeWord.fromInt ival, addr, seg)

    fun setBytes(_, _, _, 0) = ()
    |   setBytes(seg, ival, offset, count) =
        (
            csegSet(seg, offset, Word8.fromInt(ival mod exp2_8));
            setBytes(seg, ival div exp2_8, offset+0w1, count-1)
        )

    fun setWordU (ival: int, addr: addrs, seg) : unit =
        setBytes(seg, ival, addr, wordSize)
     
    fun set64u (ival: int, addr: addrs, seg) : unit =
        setBytes(seg, ival, addr, 8)
     
    fun set64s (ival: int, addr: addrs, seg) : unit =
    let
        val topByte = (ival div exp2_56) mod exp2_8
    in
        setBytes(seg, ival, addr, 7);
        setBytes(seg, if topByte < 0 then topByte + exp2_8 else topByte, addr + 0w7, 1)
    end

    (* Get 1 signed 32 bit word from the given offset in the segment. *)
    fun get32s (a: word, seg: cseg) : int =
    let
        val b0  = Word8.toInt (csegGet (seg, a));
        val b1  = Word8.toInt (csegGet (seg, a + 0w1));
        val b2  = Word8.toInt (csegGet (seg, a + 0w2));
        val b3  = Word8.toInt (csegGet (seg, a + 0w3));
        val b3' = if b3 >= exp2_7 then b3 - exp2_8 else b3;
        val topHw    = (b3' * exp2_8) + b2;
        val bottomHw = (b1 * exp2_8) + b0;
    in
        (topHw * exp2_16) + bottomHw
    end
 
    fun get64s (a: word, seg: cseg) : int =
    let
        val b0  = Word8.toInt (csegGet (seg, a));
        val b1  = Word8.toInt (csegGet (seg, a + 0w1));
        val b2  = Word8.toInt (csegGet (seg, a + 0w2));
        val b3  = Word8.toInt (csegGet (seg, a + 0w3));
        val b4  = Word8.toInt (csegGet (seg, a + 0w4));
        val b5  = Word8.toInt (csegGet (seg, a + 0w5));
        val b6  = Word8.toInt (csegGet (seg, a + 0w6));
        val b7  = Word8.toInt (csegGet (seg, a + 0w7));
        val b7' = if b7 >= exp2_7 then b7 - exp2_8 else b7;
    in
        ((((((((b7' * exp2_8 + b6) * exp2_8 + b5) * exp2_8 + b4) * exp2_8 + b3)
             * exp2_8 + b2) * exp2_8) + b1) * exp2_8) + b0
    end

    (* Test whether a tagged value will fit into a 32-bit signed constant. *)
    fun isTagged32bitS(a: machineWord) =
    if isShort a
    then let val aI = Word.toIntX(toShort a) in ~exp2_30 <= aI andalso aI < exp2_30 end
    else false
 
    (* Code-generate a byte. *)
    fun gen8u (ival: Word8.word, Code {ic, codeVec, ...}) : unit =
    let
        val icVal = !ic;
    in
        ic := icVal addrPlus 1;
        set8u (ival, icVal, codeVec)  
    end

    (* Used for signed byte values. *)
    fun gen8s (ival: int, Code {ic, codeVec, ...}) =
    if ~exp2_7 <= ival andalso ival < exp2_7
    then
    let
        val icVal = !ic;
    in
        ic := icVal + 0w1;
        set8s (ival, icVal, codeVec)  
    end
    else raise InternalError "gen8s: invalid byte";

    (* Code-generate a 32-bit word. *)
    fun gen32u (ival: LargeWord.word, Code {ic, codeVec, ...}) : unit =
    let
        val icVal = !ic;
    in
        ic := icVal + 0w4;
        set32u (ival, icVal, codeVec)
    end

    fun gen32s (ival: int, Code {ic, codeVec, ...}) : unit =
    (* We really only need to check this on the 64-bit machine and it would otherwise
       be a hot-spot for arbitrary precision arithmetic on 32-bit m/c. *)
    if not isX64 orelse ~exp2_31 <= ival andalso ival < exp2_31
    then
    let
        val icVal = !ic;
    in
        ic := icVal addrPlus 4;
        set32s (ival, icVal, codeVec)
    end
    else raise InternalError "gen32s: invalid word"

    fun gen64u (ival: int, Code {ic, codeVec, ...}) : unit =
    if 0 <= ival andalso (isShort(toMachineWord ival) orelse ival < exp2_64)
    then
    let
        val icVal = !ic;
    in
        ic := icVal addrPlus 8;
        set64u (ival, icVal, codeVec)
    end
    else raise InternalError "gen64u: invalid word"

    fun genWordU(ival, code) =
        if wordSize = 8 then gen64u(LargeWord.toInt ival, code) else gen32u (ival, code)
    
    fun gen64s (ival: int, Code {ic, codeVec, ...}) : unit =
    let
        val icVal = !ic;
    in
        ic := icVal addrPlus 8;
        set64s (ival, icVal, codeVec)
    end

    (* Add a constant to the list along with its address.  We mustn't put
       the constant directly in the code since at this stage the code is
       simply a byte segment and if we have a garbage collection the value
       won't be updated. *)
    fun addConstToVec (valu: const, posn: ConstPosn,
                       cvec as Code{numOfConsts, constVec, ic, nonInlineConsts, ...}): unit =
    let
      (* Inline constants are in the body of the code.  Non-inline constants are
         stored in the constant vector at the end of the code.  The value that goes
         in here is the PC-relative offset of the constant. *)
        val realPosn =
            case posn of
                ConstArea _ => (nonInlineConsts := ! nonInlineConsts + 1; ConstArea(!nonInlineConsts))
            |  p => p
        val isInline =
            case posn of ConstArea _ => false | NonAddrArea => false | _ => true
    in
	    numOfConsts := ! numOfConsts + 0w1;
        constVec    := {const = valu, addrs = !ic, posn = realPosn} :: ! constVec;
        (* We must put a valid tagged integer in here because we might
           get a garbage collection after we have copied this code into
           the new code segment but before we've put in the real constant.
           If this is a relative branch we need to point this at itself.
           Until it is set to the relative offset of the destination it
           needs to contain an address within the code and this could
           be the last instruction. *)
        if isInline andalso wordSize = 8
        then gen64s (tag 0, cvec)
        else gen32s (case posn of InlineRelative => ~5 | _ => tag 0, cvec)
    end


    (* Registers and pseudo-registers. *)
    datatype reg =
        GenReg of Word8.word * bool
    |   FPReg of Word8.word

    (* These are the real registers we have.  The AMD extension encodes the
       additional registers through the REX prefix. *)
    val eax = GenReg (0w0, false)
    val ecx = GenReg (0w1, false)
    val edx = GenReg (0w2, false)
    val ebx = GenReg (0w3, false)
    val esp = GenReg (0w4, false)
    val ebp = GenReg (0w5, false)
    val esi = GenReg (0w6, false)
    val edi = GenReg (0w7, false)
    val r8  = GenReg (0w0, true)
    val r9  = GenReg (0w1, true)
    val r10 = GenReg (0w2, true)
    val r11 = GenReg (0w3, true)
    val r12 = GenReg (0w4, true)
    val r13 = GenReg (0w5, true)
    val r14 = GenReg (0w6, true)
    val r15 = GenReg (0w7, true)

    (* Floating point "registers".  Actually entries on the floating point stack.
       The X86 has a floating point stack with eight entries. *)
    val fp0 = FPReg 0w0
    and fp1 = FPReg 0w1
    and fp2 = FPReg 0w2
    and fp3 = FPReg 0w3
    and fp4 = FPReg 0w4
    and fp5 = FPReg 0w5
    and fp6 = FPReg 0w6
    and fp7 = FPReg 0w7

    val regClosure  = edx (* Addr. of closure for fn. call goes here. *)

    fun getReg (GenReg r) = r
    |   getReg _ = raise InternalError "getReg: not a general register"
    fun mkReg   n      = GenReg n  (* reg.up   *)
  
    (* The maximum size of the register vectors and masks.  Although the
       X86/32 has a floating point stack with eight entries it's much simpler
       to treat it as having seven "real" registers.  Items are pushed to the
       stack and then stored and popped into the current location.  It may be
       possible to improve the code by some peephole optimisation. *)
    val regs = 23 (* Include the X86/64 registers even if this is 32-bit. *)

    (* The nth register (counting from 0). *)
    (* Profiling shows that applying the constructors here creates a lot of
       garbage.  Create the entries once and then use vector indexing instead. *)
    local
        fun regN i =
            if i < 8
            then GenReg(Word8.fromInt i, false)
            else if i < 16
            then GenReg(Word8.fromInt(i-8), true)
            else FPReg(Word8.fromInt(i-16))
        val regVec = Vector.tabulate(regs, regN)
    in
        fun regN i = Vector.sub(regVec, i) handle Subscript => raise InternalError "Bad register number"
    end
 
    (* The number of the register. *)
    fun nReg(GenReg(r, false)) = Word8.toInt r
    |   nReg(GenReg(r, true)) = Word8.toInt r + 8
    |   nReg(FPReg r) = Word8.toInt r + 16

    fun regRepr(r as GenReg _) =
          if r = eax then (if isX64 then "%rax" else "%eax") else
          if r = ebx then (if isX64 then "%rbx" else "%ebx") else
          if r = ecx then (if isX64 then "%rcx" else "%ecx") else
          if r = edx then (if isX64 then "%rdx" else "%edx") else
          if r = esp then (if isX64 then "%rsp" else "%esp") else
          if r = ebp then (if isX64 then "%rbp" else "%ebp") else
          if r = esi then (if isX64 then "%rsi" else "%esi") else
          if r = edi then (if isX64 then "%rdi" else "%edi") else
          (* X86/64 registers *) "%r" ^ Int.toString (nReg r)
    |   regRepr(FPReg n) = "fp" ^ Word8.toString n

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
            then listToSet [eax, ecx, edx, ebx, esi, edi, r8, r9, r10, r11, r12, r13, r14]
            else listToSet [eax, ecx, edx, ebx, esi, edi]
        
        val floatingPtRegisters =
            listToSet [fp0, fp1, fp2, fp3, fp4, fp5, fp6(*, fp7*)]

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

    (* Encode and decode the mask of modified registers stored in compiled code and
       maintained for the RTS.  N.B. This encoding is built into x86asm.asm in the RTS. *)
    local
        val regMap =
        [
            (eax, 0wx000001),
            (ecx, 0wx000002),
            (edx, 0wx000004),
            (ebx, 0wx000008),
            (esi, 0wx000010),
            (edi, 0wx000020),
            (r8,  0wx000040),
            (r9,  0wx000080),
            (r10, 0wx000100),
            (r11, 0wx000200),
            (r12, 0wx000400),
            (r13, 0wx000800),
            (r14, 0wx001000),
            (* Floating point registers. *)
            (fp0, 0wx002000),
            (fp1, 0wx004000),
            (fp2, 0wx008000),
            (fp3, 0wx010000),
            (fp4, 0wx020000),
            (fp5, 0wx040000),
            (fp6, 0wx080000),
            (fp7, 0wx100000)
        ]
    in
        fun getRegisterSet (rSet: Word.word): regSet =
        let
            fun testBit((reg, bit), acc) =
                if (rSet andb bit) = 0w0
                then acc
                else reg :: acc
            val regs = List.foldl testBit [] regMap
        in
            listToSet regs
        end
        
        and encodeRegSet(regSet: regSet): word =
        let
            fun testBit((reg, bit), acc) =
                if inSet(reg, regSet)
                then acc orb bit
                else acc
        in
            List.foldl testBit 0w0 regMap
        end
    end

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

    datatype shiftType = SLL | SRL | SRA

    fun shiftTypeToWord SLL = 0w4: Word8.word
    |   shiftTypeToWord SRL = 0w5
    |   shiftTypeToWord SRA = 0w7

    fun shiftTypeRepr SLL = "Shift Left Logical"
    |   shiftTypeRepr SRL = "Shift Right Logical"
    |   shiftTypeRepr SRA = "Shift Right Arithemetic"

    datatype group3Ops = NOT | NEG | MUL | IMUL | DIV | IDIV
    
    fun group3OpsToWord NOT  = 0w2: Word8.word
    |   group3OpsToWord NEG  = 0w3
    |   group3OpsToWord MUL  = 0w4
    |   group3OpsToWord IMUL = 0w5
    |   group3OpsToWord DIV  = 0w6
    |   group3OpsToWord IDIV = 0w7

    fun group3OpsRepr NOT  = "NOT"
    |   group3OpsRepr NEG  = "NEG"
    |   group3OpsRepr MUL  = "MUL"
    |   group3OpsRepr IMUL = "IMUL"
    |   group3OpsRepr DIV  = "DIV"
    |   group3OpsRepr IDIV = "IDIV"

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

    datatype fpUnaryOps = FCHS | FABS | FSQRT | FSIN | FCOS | FPATAN | FLD1 | FLDZ
    
    fun fpUnaryToWords FCHS   = {rm=0w0:Word8.word, nnn=0w4: Word8.word}
    |   fpUnaryToWords FABS   = {rm=0w1, nnn=0w4}
    |   fpUnaryToWords FSQRT  = {rm=0w2, nnn=0w7}
    |   fpUnaryToWords FSIN   = {rm=0w6, nnn=0w7}
    |   fpUnaryToWords FCOS   = {rm=0w7, nnn=0w7}
    |   fpUnaryToWords FPATAN = {rm=0w3, nnn=0w6}
    |   fpUnaryToWords FLD1   = {rm=0w0, nnn=0w5}
    |   fpUnaryToWords FLDZ   = {rm=0w6, nnn=0w5}

    fun fpUnaryRepr FCHS   = "FPChangeSign"
    |   fpUnaryRepr FABS   = "FPAbs"
    |   fpUnaryRepr FSQRT  = "FPSquareRoot"
    |   fpUnaryRepr FSIN   = "FPSin"
    |   fpUnaryRepr FCOS   = "FPCos"
    |   fpUnaryRepr FPATAN = "FPPartialArctan"
    |   fpUnaryRepr FLD1   = "FPLoadOne"
    |   fpUnaryRepr FLDZ   = "FPLoadZero"

    datatype branchOps = JO | JNO | JE | JNE | JL | JGE | JLE | JG | JB | JNB | JNA | JA

    fun branchOpToWord JO   = 0wx0: Word8.word
    |   branchOpToWord JNO  = 0wx1
    |   branchOpToWord JB   = 0wx2
    |   branchOpToWord JNB  = 0wx3
    |   branchOpToWord JE   = 0wx4
    |   branchOpToWord JNE  = 0wx5
    |   branchOpToWord JNA  = 0wx6
    |   branchOpToWord JA   = 0wx7
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
        

 (* Primary opCodes.  N.B. only opCodes actually used are listed here.
    If new instruction are added check they will be handled by the
    run-time system in the event of trap. *)
  datatype opCode =
    Group1_8_A
  | Group1_32_A
  | Group1_8_a
  | JMP_8
  | JMP_32
  | CALL_32 (* SPF 6/6/95 *)
  | MOVL_A_R
  | MOVL_R_A
  | MOVB_R_A
  | PUSH_R of Word8.word
  | POP_R  of Word8.word
  | Group5
  | NOP
  | LEAL
  | MOVL_32_64_R of Word8.word
  | MOVL_32_A
  | MOVB_8_A
  | ESCAPE
  | POP_A
  | RET
  | RET_16
  | CondJump of branchOps
  | Arith of arithOp * Word8.word
  | STC
  | Group3_A
  | Group3_a
  | Group2_8_A
  | Group2_CL_A
  | Group2_1_A
  | PUSH_8
  | PUSH_32
  | TEST_ACC8
  | LOCK_XADD
  | FPESC of Word8.word
  | XCHNG
  | REP (* Rep prefix *)
  | SAHF
  | MOVZX (* Needs escape code. *)

  fun opToInt opn: Word8.word =
    case opn of
      Group1_8_A    =>  0wx83
    | Group1_32_A   =>  0wx81
    | Group1_8_a    =>  0wx80
    | JMP_8         =>  0wxeb
    | JMP_32        =>  0wxe9
    | CALL_32       =>  0wxe8
    | MOVL_A_R      =>  0wx8b
    | MOVL_R_A      =>  0wx89
    | MOVB_R_A      =>  0wx88
    | PUSH_R reg    =>  0wx50 + reg
    | POP_R  reg    =>  0wx58 + reg
    | Group5        =>  0wxff
    | NOP           =>  0wx90
    | LEAL          =>  0wx8d
    | MOVL_32_64_R reg =>  0wxb8 + reg
    | MOVL_32_A     =>  0wxc7
    | MOVB_8_A      =>  0wxc6
    | ESCAPE        =>  0wx0f
    | POP_A         =>  0wx8f
    | RET           => 0wxc3
    | RET_16        => 0wxc2
    | CondJump opc  => 0wx70 + branchOpToWord opc
    | Arith (ao,dw) => arithOpToWord ao * 0w8 + dw
    | STC           => 0wxf9
    | Group3_A      => 0wxf7
    | Group3_a      => 0wxf6
    | Group2_8_A    => 0wxc1
    | Group2_1_A    => 0wxd1
    | Group2_CL_A   => 0wxd3
    | PUSH_8        => 0wx6a
    | PUSH_32       => 0wx68
    | TEST_ACC8     => 0wxa8
    |   LOCK_XADD   => 0wxC1 (* Needs lock and escape prefixes. *)
    |   FPESC n     => 0wxD8 orb8 n
    |   XCHNG       => 0wx87
    |   REP         => 0wxf3
    |   SAHF        => 0wx9e
    |   MOVZX       => 0wxb6 (* Needs escape code. *)

(* ...

    val eax = Reg  0;  
    val ecx = Reg  1;  
    val edx = Reg  2;
    val ebx = Reg  3;  
    val esp = Reg  4;  (* also used for "SIB used" and "no index" *)
    val ebp = Reg  5;  (* also used for "absolute" *)
    val esi = Reg  6;  
    val edi = Reg  7;

  type basereg  = reg; {0,1,2,3,6,7 only}
  type indexreg = reg; {0,1,2,3,5,6,7 only}
  
The i386 family has a horrendous collection of not-quite-orthogonal addressing modes.

Register mode:
  (1)  reg                   mod = 3; r/m = getReg reg

DS-relative addressing modes:
  (2)  DS:[basereg]          mod = 0; r/m = getReg basereg  
  (3)  DS:[basereg + disp8]  mod = 1; r/m = getReg basereg
  (4)  DS:[basereg + disp32] mod = 2; r/m = getReg basereg

  (2a) DS:[basereg]          mod = 0; r/m = 4; s = ?; i = 4; b = getReg basereg  
  (3a) DS:[basereg + disp8]  mod = 1; r/m = 4; s = ?; i = 4; b = getReg basereg
  (4a) DS:[basereg + disp32] mod = 2; r/m = 4; s = ?; i = 4; b = getReg basereg
  
  (5)  DS:[basereg + (scale * indexreg)]          mod = 0; r/m = 4; s = scale; i = getReg indexreg; b = getReg basereg  
  (6)  DS:[basereg + (scale * indexreg) + disp8]  mod = 1; r/m = 4; s = scale; i = getReg indexreg; b = getReg basereg
  (7)  DS:[basereg + (scale * indexreg) + disp32] mod = 2; r/m = 4; s = scale; i = getReg indexreg; b = getReg basereg

  (8)  DS:disp32             mod = 0; r/m = 5
  (8a) DS:[disp32]           mod = 0; r/m = 4; s = ?; i = 4; b = 5
  
  (9)  DS:[disp32 + (scale * indexreg)]           mod = 0; r/m = 4; s = scale; i = getReg indexreg; b = 5 
  
SS-relative addressing modes:
  (10) SS:[ebp + disp8]      mod = 1; r/m = 5
  (11) SS:[ebp + disp32]     mod = 2; r/m = 5

  (12) SS:[ebp + (scale * indexreg) + disp8]  mod = 1; r/m = 4; s = scale; i = getReg indexreg; b = 5  
  (13) SS:[ebp + (scale * indexreg) + disp32] mod = 2; r/m = 4; s = scale; i = getReg indexreg; b = 5
  
  (14) SS:[esp + (scale * indexreg)]          mod = 0; r/m = 4; s = scale; i = getReg indexreg; b = 4
  (15) SS:[esp + (scale * indexreg) + disp8]  mod = 1; r/m = 4; s = scale; i = getReg indexreg; b = 4  
  (16) SS:[esp + (scale * indexreg) + disp32] mod = 2; r/m = 4; s = scale; i = getReg indexreg; b = 4

... *)


    datatype mode =
        Based0    (* mod = 0 *)
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

    fun genmodrm (md : mode, rg: Word8.word, rm : Word8.word, cvec) : unit =
        gen8u (modrm (md, rg, rm), cvec)

    (* REX prefix *)
    fun rex {w,r,x,b} =
        0wx40 orb8 (if w then 0w8 else 0w0) orb8 (if r then 0w4 else 0w0) orb8
            (if x then 0w2 else 0w0) orb8 (if b then 0w1 else 0w0)

    (* The X86 has the option to include an index register and to scale it. *)
    datatype indexType =
        NoIndex | Index1 of reg | Index2 of reg | Index4 of reg | Index8 of reg

    (* Put together the three fields which make up the s-i-b byte. *)
    fun sib (s : indexType, b : reg option) : Word8.word =
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

    fun gensib (s : indexType, b : reg option, cvec : code) = gen8u (sib (s, b), cvec);

    fun scSet (Set x) = x | scSet _ = raise InternalError "scSet";

    (* Make a reference to another procedure. Usually this will be a forward reference but
     it may have been compiled already, in which case we can put the code address in now. *)
    fun codeConst (Code {resultSeg = ref(Set seg), ... }, isRel, into) =
    (* Already done. *) addConstToVec (WVal (toMachineWord(csegAddr seg)), isRel, into)

    |   codeConst (r, isRel, into as Code{ic = ref constPosition, nonInlineConsts=ref nic, ...} ) = (* forward *)
      (* Forward reference to other code or a recursive reference to this one.
         Add a completion hook to the other code. *)
        let
            fun onCompletion(_, finalAddr) =
            let
                (* This is called after the forward reference is completed but also
                   after this code, the one making the reference, has been completed. *)
                val Code{numOfConsts, resultSeg = ref resultSeg, ic = ref endByte, ...} = into
            in
                (* Fix up the forward reference. *)
                case isRel of
                    InlineRelative =>
                        csegPutConstant (scSet resultSeg, constPosition, finalAddr, true)
                |   InlineAbsolute =>
                        csegPutConstant (scSet resultSeg, constPosition, finalAddr, false)

                |   NonAddrArea => raise InternalError "onCompletion: NonAddrArea"

                |   ConstArea _ =>
                    let
                        val addrOfConst = endByte addrPlus (nic+1-1 + 2+1) * wordSize
                        val seg         = scSet resultSeg
                    in
                        csegPutConstant (seg, addrOfConst, finalAddr, false);
                        set32s(Word.toInt(addrOfConst - constPosition - 0w4), constPosition, seg)
                    end;
                (* decrement the "pending references" count *)
                numOfConsts := !numOfConsts - 0w1;
                (* If this function has no more references we can lock it. *)
                if !numOfConsts = 0w0
                then csegLock (scSet resultSeg)
                else ()
            end

            val _ = addCompletionHook(r, onCompletion)
        in
            addConstToVec (CVal r, isRel, into)
        end

   (* Removes a label from the list when it has been fixed up
      or converted to the long form. *)
   fun removeLabel (lab:addrs, Code{longestBranch, labelList, ... }) : unit = 
   let
     fun removeEntry ([]: labList) : labList = []
       | removeEntry ((ref (Jump32From _)) :: t) =
           removeEntry t (* we discard long jumps *)
         
       | removeEntry ((entry as ref (Jump8From addr)) :: t) =
         if lab = addr
         then removeEntry t
         else
          (
             if addr < !longestBranch
             then longestBranch := addr
             else ();
              
             entry :: removeEntry t
          ) (* removeEntry *);
   in
        (* Must also find the new longest branch. *)
        longestBranch := addrLast;
        labelList     := removeEntry (! labelList)
   end;
 
  (* Fix up the list of labels. *)
  fun reallyFixBranches ([] : labList) _ = ()
    | reallyFixBranches (h::t)        (cvec as Code{codeVec=cseg, ic, branchCheck, ...}) =
   ((case !h of
       Jump8From addr =>
       let
         val offset : int = get8s (addr, cseg);
         val diff : int = (!ic addrMinus addr) - 1;
       in
         branchCheck := !ic;

         if is8Bit diff then () else raise InternalError "jump too large";

         if offset <> 0
         then raise InternalError "reallyFixBranches: jump already patched"
         else set8s (diff, addr, cseg);

         removeLabel (addr, cvec)
       end
       
     | Jump32From addr =>
       let
         val offset : int = get32s (addr, cseg);
         val diff : int = (!ic addrMinus addr) - 4;
       in
         branchCheck := !ic;
         if offset <> 0
         then raise InternalError "reallyFixBranches: jump already patched"
         else
         (* A zero offset is more than simply redundant, it can
            introduce zero words into the code which could be
            taken as markers.  It will not normally be produced
            but can occur in very unusual cases.  The only example
            I've seen is a branch extension in a complicated series
            of andalsos and orelses where the branch extension was
            followed by an unconditional branch which was then backed
            up by check_labs.  We simply fill it with no-ops. *)
          if diff = 0
          then let
            val a    = addr;
            val nop  = opToInt NOP;
          in
            csegSet (cseg, a - 0w1, nop);
            csegSet (cseg, a,     nop);
            csegSet (cseg, a + 0w1, nop);
            csegSet (cseg, a + 0w2, nop);
            csegSet (cseg, a + 0w3, nop)
          end
          else
            set32s (diff, addr, cseg)
       end
    );
   reallyFixBranches t cvec
  )

    (* Makes a new label. *)
    fun makeShortLabel (addr: addrs, Code{longestBranch, labelList ,...}) : jumpFrom ref =
    let
        val lab = ref (Jump8From addr);
    in
        if addr < ! longestBranch
        then longestBranch := addr
        else ();
        labelList := lab :: ! labelList;
        lab
    end;

  (* Apparently fix up jumps - actually just record where we have come from *)
  fun fixup (labs:labList, cvec as Code{justComeFrom, exited, ic, branchCheck, ...}) =
  let
    (* If the jump we are fixing up is immediately preceding, 
       we can remove it.  It is particularly important to remove
       32 bit jumps to the next instruction because they would
       put a word of all zeros in the code, and that could be mistaken
       for a marker word. *)
    fun checkLabs []          = []
      | checkLabs ((lab as ref (Jump8From addr))::labs) =
            if !ic addrMinus addr = 1 andalso !ic <> !branchCheck
            then
             (
                (* It now seems that we can have a !ic = !branchCheck in the situation where
                   we have a handler that does nothing.  Setting the handler entry point sets
                   branchCheck but the branch round the empty handler does nothing.
                   This should be tidied up by peep-hole optimisation. *)
               if !ic <= !branchCheck
               then raise InternalError "Backing up too far (8bit)"
               else ();
               ic := addr addrPlus ~1; (* Back up over the opCode *)
               removeLabel (addr, cvec);
               exited := false;
               checkLabs labs
             )
            else lab :: checkLabs labs
          
      | checkLabs ((lab as ref (Jump32From addr))::labs) =
            if !ic addrMinus addr = 4
            then
             (
               if !ic <= !branchCheck
               then raise InternalError "Backing up too far (32bit)"
               else ();
               ic := addr addrPlus ~1; (* Back up over the opCode *)
               exited := false;
               checkLabs labs
             )
            else lab :: checkLabs labs

     fun doCheck labs =
     (* Repeatedly check the labels until we are no longer backing up.
        We may have several to back up if we have just extended some
        branches and then immediately fix them up.  DCJM 19/1/01. *)
     let
        val lastIc = !ic
        val newLabs = checkLabs labs
     in
        if lastIc = !ic then newLabs
        else doCheck newLabs
     end
  in
    case labs of
      [] => () (* we're not actually jumping from anywhere *)
    | _ =>
       (
        (* Add together the jumps to here and remove redundant jumps. *)
        justComeFrom := doCheck (labs @ !justComeFrom)
      )
  end;


    fun checkBranchList
        (cvec as Code{longestBranch, justComeFrom,
                      exited, ic, labelList, ...}, branched, size) =
    (* If the longest branch is close to going out of range it must
       be converted into a long form. *)
    (* If we have just made an unconditional branch then we make the 
       distance shorter. *)
    let
        (* Generally we only need to extend the nearest short branch but it
           is possible that two branches could be very close together.  In that
           case extending one branch could push another out of range. *)
        val maxDiff =
            Int.min(if branched then 100 else 127, 127 - 5 * List.length (!labelList)) - size;

        (* See if we must extend some branches.  If we are going to fix up a label
           immediately we don't normally extend it.  The exception is if we have
           to extend some other labels in which case we may have to extend this
           because the jumps we add may push this label out of range. *)
        local
            val icOffset =
                if branched then !ic else !ic addrPlus 2 (* Size of the initial branch. *)
            fun checkLab (lab as ref (Jump8From addr), n) =
                if List.exists (fn a => a = lab) (! justComeFrom)
                then n (* Don't include it here. *)
                else if (icOffset addrMinus addr) + n > (100 - size) then n+5 else n
            |   checkLab (_, n) = n
            (* Extending one branch may extend others.  We need to process the list in
               reverse order. *)
        in
            val jumpSpace = List.foldr checkLab 0 (!labelList)
        end

   (* Go down the list converting any long labels, and finding the
      longest remaining. *)
    fun convertLabels ([]:labList) : labList = []
      | convertLabels (lab::labs) =
       let
         (* Process the list starting at the end. The reason for this
            is that more recent labels appear before earlier ones.
            We must put the earliest labels in first because they may
            be about to go out of range. *)
          val convertRest = convertLabels labs
       in
         (* Now do this entry. *)
         case !lab of
           Jump32From _ => raise InternalError "Long jump in label list" (* shouldn't happen *)
           
         | Jump8From addr =>
            (* If we are about to fix this label up we don't need to extend it except that we
               must extend it if we are going to put in more branch extensions which will take
               it out of range. DCJM 9/4/01. *)
            if List.exists (fn a => a = lab) (! justComeFrom)
                andalso (jumpSpace = 0 orelse !ic addrMinus addr < 127 - jumpSpace)
            then lab :: convertRest
            else if !ic addrMinus addr > (100 - size) orelse !ic addrMinus addr > maxDiff
            then (* Getting close - convert it. *)
            (
                reallyFixBranches [lab] cvec; (* fix up short jump to here *)
                gen8u  (opToInt JMP_32, cvec);
                gen32u (0w0, cvec);    (* long jump to final destination *)
                lab := Jump32From (!ic addrPlus ~4);
                (* Return the rest of the list. *)
                convertRest
            )
            else
            (
                (* Not ready to remove this. Just find out if this is an
                   earlier branch and continue. *)
                if addr < ! longestBranch
                then longestBranch := addr
                else ();
           
                lab :: convertRest
            )
       end (* convertLabels *);
    in
        if ! longestBranch <> addrLast andalso !ic addrMinus ! longestBranch > maxDiff
        then
        let         
            (* Must skip round the branches unless we have just taken an
               unconditional branch. *)
            val lab =
              if branched then []
              else
               (
                exited := true;
                gen8u (opToInt JMP_8, cvec);
                gen8u (0w0, cvec);
                [makeShortLabel (!ic addrPlus ~1, cvec)]
               );
        in
            (* Find the new longest branch. *)
            longestBranch := addrLast; (* Initial value. *)
            labelList := convertLabels (!labelList);
            fixup (lab, cvec) (* Continue with normal processing. *)
        end
        else  ()
   end

    (* Do all the outstanding operations including fixing up the branches. *)
    fun doPending (cvec as Code{exited, justComeFromAddrs, branchCheck, ic, ...}, size) : unit =
    let
        (* Deal with a pending fix-up. *)
        fun reallyFixup (Code{justComeFrom=ref [], ... }) = ()
        |   reallyFixup (cvec as Code{justComeFrom=jcf as ref labs, exited, ... }) = 
                (exited := false; reallyFixBranches labs cvec; jcf := []);
    in
        (* If we have not exited and there are branches coming in here
            then we fix them up before jumping round any branch extensions. *)
        if ! exited then () else reallyFixup cvec;
   
        checkBranchList(cvec, ! exited, size);

        exited := false;
        (* Fix up any incoming branches, including a jump round any
           branch extensions. *)
        reallyFixup cvec;
        (* Finally record the current location into any reverse branches. *)
        branchCheck := !ic;
        List.app (fn addr => addr := !ic) (! justComeFromAddrs);
        justComeFromAddrs := []
    end

    (* 12 is maximum size of an instruction.  It's also big
       enough for a comparison and the following conditional
       branch. *)
    val maxInstrSize = if isX64 then 15 else 12

    (* Generate an opCode byte after doing any pending operations. *)
    fun genop(opb:opCode, rx, cvec) =
    (
        doPending (cvec, maxInstrSize);
        case rx of
            NONE => ()
        |   SOME rxx =>
            if isX64 then gen8u(rex rxx, cvec)
            else raise InternalError "genop: rex prefix in 32 bit mode";
        gen8u (opToInt opb, cvec)
    )

    (* This has to be done quite carefully if we are to be able to back-up
       over jumps that point to the next instruction in fixup.  We have to
       guarantee that if we back up we haven't already set a jump to point
       beyond where we're backing up.  See below for more explanation.
       DCJM 19/1/01.*)
    fun putConditional (br: branchOps, cvec as Code{ic, ...}) : jumpFrom ref =
    (
        gen8u (opToInt(CondJump br), cvec); (* Don't use genop. *)
        gen8u (0w0, cvec);
        makeShortLabel (!ic addrPlus ~1, cvec)
    )

    (* Generates an unconditional branch. *)
    fun unconditionalBranch (cvec as Code {justComeFrom, exited, ic, ...}): labList =
    let
        (* If we have just jumped here we may be able to avoid generating a
           jump instruction. *)
        val labs = ! justComeFrom
    in
        justComeFrom := [];
        (* We may get the sequence:   jmp L1; L2: jmp L3.
           If this is the "jmp L3" we can simply remember everything
           that was supposed to jump to L2 and replace it with
           jumps to L3. *)
        (* This code has one disadvantage.  If we have several short branches
           coming here we don't record against the branches themselves that
           they're all going to the same place.  If we have to extend them
           we put in separate long branches for each rather than pointing
           them all at the same branch.  This doesn't increase run-time
           but makes the code larger than it need be.  DCJM 1/1/01. *)
        if ! exited
        then labs
        else
        let
        (* The code here has gone through various versions.  The original
           version always fixed up pending branches so that if we had a
           short branch coming here we might avoid having to extend it.
           A subsequent version separated out long and short branches
           coming here and fixed up short branches but added long ones
           onto the label list.  I discovered a bug with this which
           occurred when we put in branch extension code before an
           unconditional branch and then backed up over the unconditional
           branch and over one of the extended branches.  Since we'd
           already fixed up (really fixed up) the branch round the
           branch extensions we ended up with that branch now pointing into
           the middle of the code we subsequently generated.
           We could get a similar situation if we have a conditional
           branch immediately before this instruction and back up over
           both, for example (if exp then () else (); ...).  In that case
           we have to make sure we haven't already fixed up another branch
           to come here.  Instead we must always add it onto the label list
           so that we only (really) fix it up when we generate something other
           than a branch.  DCJM 19/1/01. *)
            val br =
            (
                gen8u (opToInt JMP_8, cvec); (* Don't use genop. *)
                gen8u (0w0, cvec);
                makeShortLabel (!ic addrPlus ~1, cvec)
            )
        in
            exited := true;
            br :: labs
        end
    
    end (* unconditionalBranch *)

    (* Generate an effective address. *)
    fun genEACode (offset: int, rb: Word8.word, r: Word8.word, cvec) : unit =
    let
        val offsetCode =
            (* don't generate [ebp] (use [ebp+0]) 'cos it doesn't exist! *)
            if offset = 0 andalso rb <> 0w5 
            then Based0  (* no disp field *)
            else if is8Bit offset
            then Based8  (* use 8-bit disp field *)
            else Based32 (* use 32-bit disp field *)
    in
        if rb = 0w4 (* Code for esp and r12 *)
        then (* Need to use s-i-b byte. *)
        (
            (* Normally we will have a non-zero offset for esp.  The
               exception is computing the maximum stack in the prelude. *)
            genmodrm (offsetCode, r, 0w4 (* use SIB *), cvec);
            gensib   (NoIndex, SOME esp, cvec)
        )
        else genmodrm(offsetCode, r, rb, cvec);
     
        (* generate the disp field (if any) *)
        case offsetCode of
            Based8  => gen8s  (offset, cvec)
        |   Based32 => gen32s (offset, cvec)
        |   _       => ()
    end

    (* Generate a opcode plus a modrm byte.  *)
    fun genOpEA(opb:opCode, offset: int, rb: reg, r: reg, cvec): unit =
    let
        val (rbC, rbX) = getReg rb
        val (rrC, rrX) = getReg r
    in
        doPending (cvec, maxInstrSize);
        (* Any lock prefix comes before any REX prefix. *)
        case opb of LOCK_XADD => gen8u(0wxF0, cvec) | _ => ();
        (* For the moment always put in a REX prefix. *)
        if isX64 then gen8u(rex{w=true, r=rrX, b=rbX, x = false}, cvec) else ();
        (* Generate the escape codes for the opcodes that need them. *)
        case opb of
            MOVZX => gen8u(opToInt ESCAPE, cvec)
        |   LOCK_XADD => gen8u(opToInt ESCAPE, cvec)
        |   _     => ();
        (*if offset < 0 andalso rb = esp then raise InternalError "Negative stack offset" else ();*)
        gen8u(opToInt opb, cvec);
        genEACode(offset, rbC, rrC, cvec)
    end

    (* Generate a opcode plus a second modrm byte but where the "register" field in
       the modrm byte is actually a code.  *)
    fun genOpPlus2(opb:opCode, offset: int, rb: reg, op2: Word8.word, cvec): unit =
    let
        val (rbC, rbX) = getReg rb
        val need64bit =
            case opb of
                MOVB_8_A => false
            |   Group3_a => false
            |   FPESC _ => false
            |   Group5 => false (* Call/Jmp/Push - size is intrinsic *)
            |   Group1_8_A => isX64 (* Arithmetic operations - must be 64-bit *)
            |   Group2_1_A => isX64 (* 1-bit shifts - must be 64-bit *)
            |   Group2_8_A => isX64 (* n-bit shifts - must be 64-bit *)
            |   _ => isX64 (* Anything else?  Assume it requires prefix. *)
    in
        doPending (cvec, maxInstrSize);
        (* For the moment always put in a REX prefix. *)
        (* If (opb = Group5 andalso op2 = 6 (* push *) orelse opb = POP_A)
           andalso not rbX  then we don't need it. *)
        if need64bit orelse rbX then gen8u(rex{w=need64bit, r=false, b=rbX, x = false}, cvec) else ();
        gen8u(opToInt opb, cvec);
        genEACode(offset, rbC, op2, cvec)
    end

    (* Register/register operation. *)
    fun genOpReg(opb:opCode, rd: reg, rs: reg, cvec) =
    let
        val (rbC, rbX) = getReg rs
        val (rrC, rrX) = getReg rd
    in
        doPending (cvec, maxInstrSize);
        (* For the moment always put in a REX prefix. *)
        if isX64 then gen8u(rex{w=true, r=rrX, b=rbX, x = false}, cvec) else ();
        gen8u(opToInt opb, cvec);
        genmodrm(Register, rrC, rbC, cvec)
    end

    fun genOpRegPlus2(opb:opCode, rd: reg, op2: Word8.word, cvec) =
    let
        val (rrC, rrX) = getReg rd
    in
        doPending (cvec, maxInstrSize);
        (* For the moment always put in a REX prefix. *)
        if isX64 then gen8u(rex{w=true, r=false, b=rrX, x = false}, cvec) else ();
        gen8u(opToInt opb, cvec);
        genmodrm(Register, op2, rrC, cvec)
    end

    (* Similar to genEA, but used when there is an index register.
     rb may be NONE if no base register is required (used
     with leal to tag values). *)
    fun genOpIndexed (opb:opCode, offset: int, rb: reg option, ri: indexType, rd: reg, cvec) =
    let
        val (rbC, rbX) = case rb of NONE => (0w0, false) | SOME rb => getReg rb

        val (_, riX) = 
            case ri of
                NoIndex  => (0w0, false) (* No index reg. *)
            |   Index1 i => getReg i
            |   Index2 i => getReg i
            |   Index4 i => getReg i
            |   Index8 i => getReg i

        val (rrC, rrX) = getReg rd

        val (offsetCode, basefield) =
        case rb of
            NONE => (Based0, NONE (* no base register *))
        |   SOME rb =>
            let
                val base =
                    if offset = 0 andalso rbC <> 0wx5
                    then Based0    (* no disp field *)
                    else if is8Bit offset
                    then Based8   (* use 8-bit disp field *)
                    else Based32; (* use 32-bit disp field *)
            in
                (base, SOME rb)
            end
    in
        doPending (cvec, maxInstrSize);
        (* For the moment always put in a REX prefix. *)
        if isX64 then gen8u(rex{w=true, r=rrX, b=rbX, x=riX}, cvec) else ();

        (* Generate the ESCAPE code if needed. *)
        case opb of
            MOVZX => gen8u(opToInt ESCAPE, cvec)
        |   _     => ();
        gen8u(opToInt opb, cvec);

        genmodrm (offsetCode, rrC, 0w4 (* s-i-b *), cvec);
        gensib   (ri, basefield, cvec);
    
        (* generate the disp field (if any) *)
        case offsetCode of
            Based8  => gen8s  (offset, cvec)
        |   Based32 => gen32s (offset, cvec)
        |   _       => case rb of NONE =>  (* 32 bit absolute used as base *) gen32s (offset, cvec) | _ => ()
    end

    fun genPushPop(opc, r, cvec) =
    let
        val (rc, rx) = getReg r
    in
        (* These are always 64-bit but a REX prefix may be needed for the register. *)
        genop(opc rc, if rx then SOME{w=false, b = true, x=false, r = false } else NONE, cvec)
    end

    (* Tag the value in register r *)
    fun genTag (r, cvec) = genOpIndexed(LEAL, 1, SOME r, Index1 r, r, cvec)      

    fun genImmed (opn: arithOp, rd: reg, imm: int, cvec) : unit =
    if is8Bit imm
    then (* Can use one byte immediate *) 
    (
       genOpRegPlus2(Group1_8_A, rd, arithOpToWord opn, cvec);
       gen8s (imm, cvec)
    )
    else if not isX64 orelse (~exp2_31 <= imm andalso imm < exp2_31)
    then (* Need 32 bit immediate. *)
    (
       genOpRegPlus2(Group1_32_A, rd, arithOpToWord opn, cvec);
       gen32s(imm, cvec)
    )
    else (* It won't fit in the immediate; put it in the non-address area. *)
    let
        (* For the moment use the same format as real numbers and put
           this into a piece of memory which is then byte-copied into the
           constant area. *)
        open Address
        val mem = alloc(0w1, F_bytes orb8 F_mutable, toMachineWord 0w0)

        fun setMem(m, n) = 
        if n = Word.fromInt wordSize then ()
        else 
        (
            assignByte(mem, n, Word8.fromInt(m mod exp2_8));
            setMem(m div exp2_8, n+0w1)
        )
        val () = setMem(imm, 0w0)
        val () = lock mem
        val (rc, rx) = getReg rd
    in
        genop(Arith (opn, 0w3 (* r/m to reg *)), SOME{w=true, r=rx, b=false, x = false}, cvec);
        genmodrm (Based0, rc, 0w5 (* PC-relative *), cvec);
        addConstToVec(WVal(toMachineWord mem), NonAddrArea, cvec)
    end

    fun genReg (opn: arithOp, rd: reg, rs: reg, cvec) =
        genOpReg (Arith (opn, 0w3 (* r/m to reg *)), rd, rs, cvec)
      
    (* generate padding no-ops to align to n modulo 4 *)
    (* The Intel 64 instruction manual recommends:
       1 byte:  NOP
       2 bytes: 66 NOP
       3 bytes: 0F 1F 00 - Multibyte NOP probably not generally supported. *)
    (* generate padding no-ops to align to n modulo 4 *)
    fun align (n, cvec as Code{ic, ...}) =
        while (n - (!ic)) mod 0w4 <> 0w0
        do genop (NOP, NONE, cvec);

    (* movl offset(rb),rd. *)
    fun genLoad (offset: int, rb: reg, rd: reg, cvec) = genOpEA(MOVL_A_R, offset, rb, rd, cvec)

    (* Called when we have a memory operand and a constant that is an address.
       This is either a move or a comparison. *)
    fun genMemoryConstant (cnstnt, opcode, arithOp, offset, rb, ri, cvec as Code{ic, ...}) =
    let
        val haveIndex = case ri of NoIndex => false | _ => true
    in
      (* We have a little problem here: we have to be very careful that
         we don't end up with a full word of zeros on a word boundary because
         that is used as an end-of-code marker.  This can arise if we have
         zero bytes in the high order part of the offset and zero bytes in
         the low order part of the immediate value.  We can get the former
         if the offset is greater than 127 and we can get the latter if the
         immediate is an address but not if it is a tagged value.  Furthermore
         the garbage collector may change the address in the future so even
         if it is safe now it may not always be.  We add in no-ops to align
         the offset onto a word boundary ensuring that the offset and the
         immediate value never come together in the same word.

         There's also another case.  If the mod-rm byte is zero and aligned
         on a word boundary then this could combine with the immediate value
         if all three low-order words were zero.  It's very unlikely but we
         need to consider it. *)
        if isX64 then raise InternalError "genMemoryConstant" (* We don't have 64-bit immediates. *)
        else if not (is8Bit offset)
        then
        (
            doPending(cvec, maxInstrSize + 2);
            (* We have a sib byte if we either have an index or the base
               register is esp. *)
            align(if haveIndex orelse rb = esp then 0w1 else 0w2, cvec)
        )
        else if offset = 0 andalso rb = eax andalso not haveIndex
        then (* modrm will be zero.  We need to be sure that this is not the
              first byte in a word. *)
        (
            doPending(cvec, maxInstrSize + 1);
            if (!ic) mod 0w4 = 0w3 (* opcode will be the last byte in this word. *)
            then align(0w1, cvec)
            else ()
        )
        else ();
        
        case ri of
            NoIndex => genOpPlus2 (opcode, offset, rb, arithOp, cvec)
        |   ri => genOpIndexed (opcode, offset, SOME rb, ri, mkReg(arithOp, false), cvec);
        addConstToVec (WVal cnstnt, InlineAbsolute, cvec)
    end

    (* Register/register move. *)
    fun genMove (rd, rs, cvec) = genOpReg (MOVL_R_A, rs,rd, cvec)

    (* Add a register to a constant. *)
    fun genLeal (rd, rs, offset, cvec) = genOpEA (LEAL, offset, rs, rd, cvec)

    type handlerLab = addrs ref;
  
    (* Loads the address of the destination of a branch. Used to
     put in the address of the exception handler.
     We used to have pushAddress in place of this which pushed the
     address at the same time.  On this architecture it can save an
     instruction but it's a problem on machines where we have to load
     the address into a register - we don't have a spare checked
     register available. *)
    fun loadHandlerAddress  (rd, lab, cvec) =
    let
        val (rc, rx) = getReg rd
    in
        genop(MOVL_32_64_R rc,
            if isX64 then SOME {w=true, r=false, b=rx, x=false} else NONE, cvec);
        addConstToVec (HVal lab, InlineAbsolute, cvec)
    end

    fun fixupHandler (lab:handlerLab, cvec as Code{exited, ic, branchCheck, ...}) : unit =
    ( 
        (* Make sure anything pending is done first. *)
        (* 15 comes from maximum instruction size + up to 3 nops. *)
        doPending (cvec, maxInstrSize+3); 
    
        (* Ensure the return address is aligned onto a word + 2 byte
           boundary. *)
        align (0w2, cvec);
    
        exited := false;
        branchCheck := !ic;
        lab := !ic
    );

    datatype callKinds =
        Recursive           (* The function calls itself. *)
    |   ConstantClosure of machineWord (* A pre-compiled or io function. *)
    |   ConstantCode of machineWord (* A function that doesn't need a closure *)
    |   CodeFun of code      (* Forward reference to code *)
    |   FullCall            (* Full closure call *)
  
(*****************************************************************************
Calling conventions:
   FullCall:
     the caller loads the function's closure into regClosure and then
     (the code here) does an indirect jump through it.

   Recursive:
     the caller loads its own function's closure/static-link into regClosure
     and the code here does a jump to the start of the code.
     
   ConstantFun:
     a direct or indirect call through the given address.  If possible the
     caller will have done the indirection for us and passed false as the
     indirection value.  The exception is calls to IO functions where the
     address of the code itself is invalid.  If the closure/static-link
     value is needed that will already have been loaded.

   CodeFun:
     the same as ConstantFun except that this is used only for static-link
     calls so is never indirect. 

*****************************************************************************)    
    (* Call a function. *)
    fun callFunction (callKind, cvec as Code {ic, ... }) : unit =
    (
        case callKind of 
            Recursive =>
            (
                (* Call back to the start of the current function. *)
                doPending (cvec, maxInstrSize + 3); 
                align (0w1, cvec);
 
                genop (CALL_32, NONE, cvec);  (* 1 byte  *)
                gen32s (~(Word.toInt(!ic) + 4), cvec)       (* 4 bytes *)
            )
     
        |   FullCall => (* Indirect call through closure reg. *)
            (
                (* Make sure anything pending is done first. *)
                doPending (cvec, maxInstrSize+3); 
                (* Ensure the return address is aligned on
                   a word + 2 byte boundary.  *)
                align (0w0, cvec);
         
                genop (Group5, NONE, cvec);
                genmodrm(Based0, 0w2 (* call *), #1 (getReg regClosure), cvec)
            )

        |   CodeFun c =>
            (
                (* Make sure anything pending is done first. *)
                doPending (cvec, maxInstrSize+3); 

                if isX64
                then
                (
                    align (0w0, cvec);
                    genop (Group5, NONE, cvec);
                    genmodrm(Based0, 0w2 (* call *), 0w5 (* PC rel *), cvec);
                    codeConst (c, ConstArea 0, cvec)
                )
                else
                (
    		        (* Ensure the return address is aligned on
    		           a word + 2 byte boundary.  *)
    		        align (0w1, cvec);
		 
    		        genop (CALL_32, NONE, cvec);
    			    codeConst (c, InlineRelative, cvec)
    		    )
            )

        |   ConstantCode w =>
            (
                (* Make sure anything pending is done first. *)
                doPending (cvec, maxInstrSize+3); 
                if isX64
                then
                (
                    align (0w0, cvec);
                    genop (Group5, NONE, cvec);
                    genmodrm(Based0, 0w2 (* call *), 0w5 (* PC rel *), cvec);
                    addConstToVec (WVal w, ConstArea 0, cvec)
                )
                else
    	 	    (
    		        (* Ensure the return address is aligned on
    		           a word + 2 byte boundary.  *)
    		        align (0w1, cvec);

    		        genop (CALL_32, NONE, cvec);
    			    addConstToVec (WVal w, InlineRelative, cvec)
    		    )
            )

        |   ConstantClosure w =>
            if isX64
            then
            let
                val (rc, rx) = getReg regClosure
            in
                genop  (MOVL_32_64_R rc, SOME {w=true, r=false, b=rx, x=false}, cvec);
                addConstToVec (WVal w, InlineAbsolute, cvec);
                doPending (cvec, maxInstrSize+3); 
		        (* Ensure the return address is aligned on
		            a word + 2 byte boundary.  *)
		        align (0w0, cvec);
                genop (Group5, NONE, cvec);
                genmodrm(Based0, 0w2 (* call *), rc, cvec)
            end
            else
            (
		        (* Make sure anything pending is done first. *)
		        doPending (cvec, maxInstrSize+3); 

		        (* Ensure the return address is aligned on
		           a word + 2 byte boundary.  *)
		        align (0w0, cvec);

			    genop (Group5, NONE, cvec);
			    genmodrm(Based0, 0w2 (* call *), 0w5 (* Immediate address. *), cvec);
			    addConstToVec (WVal w, InlineAbsolute, cvec)
		    );

        if (!ic) mod 0w4 <> 0w2
        then raise InternalError "callFunction: call not aligned"
        else ()
    );
     

  (* Tail recursive jump to a function.
     N.B.  stack checking is used both to ensure that the stack does
     not overflow and also as a way for the RTS to interrupt the code
     at a safe place.  The RTS can set the stack limit "register" at any
     time but the code will only take a trap when it next checks the
     stack.  The only way to break out of infinite loops is for the
     user to type control-C and some time later for the code to do a
     stack check.  We need to make sure that we check the stack in any
     function that my be recursive, directly or indirectly.
  *)
    fun jumpToFunction (callKind, cvec as Code{exited, ic, ...}) =
    (
        case callKind of 
            Recursive =>
            (
                (* Jump to the start of the current function. *)
                genop (JMP_32, NONE, cvec);
                gen32s (~(Word.toInt(!ic) + 4), cvec)
            )
           
        |   FullCall =>
            ( (* Full closure call *)
                genop (Group5, NONE, cvec);
                genmodrm(Based0, 0w4 (* jmp *), #1 (getReg regClosure), cvec)
            )
      
        |   CodeFun c =>
            if isX64
            then
            (
                genop (Group5, NONE, cvec);
                genmodrm(Based0, 0w4 (* jmp *), 0w5 (* PC rel *), cvec);
                codeConst (c, ConstArea 0, cvec)
            )
            else
            (
		        genop (JMP_32, NONE, cvec);
			    codeConst (c, InlineRelative, cvec)
            )

        |   ConstantCode w =>
            if isX64
            then
            (
                genop (Group5, NONE, cvec);
                genmodrm(Based0, 0w4 (* jmp *), 0w5 (* PC rel *), cvec);
                addConstToVec (WVal w, ConstArea 0, cvec)
            )
            else
            (
    		    genop (JMP_32, NONE, cvec);
    			addConstToVec (WVal w, InlineRelative, cvec)
            )

        |   ConstantClosure w =>
		  	(* Indirect jumps are used to call into the RTS. *)
            if isX64
            then
            let
                val (rc, rx) = getReg regClosure
            in
                genop  (MOVL_32_64_R rc, SOME {w=true, r=false, b=rx, x=false}, cvec);
                addConstToVec (WVal w, InlineAbsolute, cvec);
            
                genop (Group5, NONE, cvec);
                genmodrm(Based0, 0w4 (* jmp *), rc, cvec)
            end
            else
	 	    (
			    genop (Group5, NONE, cvec);
			    genmodrm(Based0, 0w4 (* jmp *), 0w5 (* Immediate address. *), cvec);
			    addConstToVec (WVal w, InlineAbsolute, cvec)
		    );

        exited := true (* We're not coming back. *)
    );


    (* Return and remove args. *)
    fun returnFromFunction (args, cvec as Code{exited, ...}) : unit =
    (
        if args = 0
        then genop (RET, NONE, cvec)
        else
        let
            val offset = Word.fromInt(args * wordSize)
        in
            genop (RET_16, NONE, cvec);
            gen8u (wordToWord8 offset, cvec);
            gen8u (wordToWord8(offset >> 0w8), cvec)
        end;
     
        exited := true (* We're not coming back. *)
    )

    (* Backwards jump for loops. *)

    (* Put in a stack check in a loop. This is used to allow the code to be interrupted. *)
    fun stackCheck cvec =
    let
        (* cmp reg,16(%ebp)*)
        val () = genOpEA(Arith (CMP, 0w3), memRegStackLimit, ebp, esp, cvec)
        (* jnb 3 *)
        val lab = [putConditional (JNB, cvec)]
    in
        (* call *)
        genop(Group5, NONE, cvec);
        genmodrm (Based8, 0w2 (* call *), #1 (getReg ebp), cvec);
        gen8u (Word8.fromInt memRegStackOverflowCall, cvec);
        fixup (lab, cvec)
    end

    fun genFloatingPt({escape, md, nnn, rm}, code) =
    (
        genop(FPESC escape, NONE, code);
        gen8u((md <<- 0w6) orb8 (nnn <<- 0w3) orb8 rm, code)
    )

    (* Load a floating point register to the stack.  Because the positions are dependent on
       the number of items already pushed we may need to add an offset. *)
    fun loadFpRegToStack(fpReg, offset, code) =
    let
        val fp = case fpReg of FPReg fp => fp | _ => raise InternalError "fpreg"
    in
        genFloatingPt({escape=0w1, md=0w3, nnn=0w0, rm= fp + offset}, code) (* FLD ST(r1) *)
    end

    (* Pops the top of the stack into a register.  This assumes that there is
       exactly one item on the stack which is why we add one here. *)
    fun storeFpRegFromStack(fpReg, code) =
    let
        val dest = case fpReg of FPReg fp => fp | _ => raise InternalError "fpreg"
    in
        genFloatingPt({escape=0w5, md=0w3, nnn=0w3, rm = dest+0w1(* One item *)}, code) (* FSTP ST(n+1) *)
    end

    (* Allocate store and put the resulting pointer in the result register. *)
    local
        fun allocStoreCommonCode (resultReg, cvec as Code{ic=_, ...}, isVarAlloc) =
        (
            (* Common code.  resultReg contains the possible new address. *)
            genOpEA(Arith (CMP, 0w3 (* r/m to reg *)), memRegLocalMbottom, ebp, resultReg, cvec);

            let
                (* Normally we won't have run out of store so we want the default
                   branch prediction to skip the test here. However doing that
                   involves adding an extra branch which lengthens the code so
                   it's probably not worth while. *)
                (*val lab =
                    let
                        val () = genop(CondJump JB, cvec);
                        val () = gen8u (0w0, cvec);
                        val lab2 = [makeShortLabel (!ic addrPlus ~1, cvec)]
                        val () = genop(JMP_8, cvec);
                        val () = gen8u (0w0, cvec);
                        val lab = [makeShortLabel (!ic addrPlus ~1, cvec)]
                    in
                        fixup(lab2, cvec);
                        lab
                    end*)
                (* Just checking against the lower limit in this way can fail
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
                val lab =
                if isVarAlloc
                then
                let
                    val lab1 = [putConditional(JB, cvec)]
                    val () =
                        if isX64
                        then genReg (CMP, resultReg, r15, cvec)
                        else genOpEA(Arith (CMP, 0w3), memRegLocalMPointer, ebp, resultReg, cvec) 
                    val lab2 = [putConditional(JB, cvec)]
                in
                    fixup(lab1, cvec);
                    lab2
                end
                else [putConditional(JNB, cvec)]
            in
                (* If we don't have enough store for this allocation we call this
                   function. *)
                genop (Group5, NONE, cvec);
                genmodrm(Based8, 0w2 (* call *), #1 (getReg ebp), cvec);
                gen8s (memRegHeapOverflowCall, cvec);
                fixup (lab, cvec)
            end;
            (* Update the heap pointer now we have the store.  This is also
               used by the RTS in the event of a trap to work out how much
               store was being allocated. *)
            if isX64 then genMove(r15, resultReg, cvec)
            else genOpEA (MOVL_R_A, memRegLocalMPointer, ebp, resultReg, cvec)
         )
    in
        fun allocStoreCode (size, resultReg, cvec as Code { inAllocation as ref false, ...}) =
        let
            val _ = inAllocation := true
            val bytes = (size + 1) * wordSize
        in
            if isX64
            then genLeal (resultReg, r15, ~ bytes, cvec) (* TODO: What if it's too big to fit? *)
            else
            (
                (* movl 0(%ebp),r; subl (size+1)*4,r; cmpl r,8(%ebp); jnb 1f;
            	   call 40[%ebp]; 1f: movl r,0(%ebp); movl size,-4(r); *)
                genLoad (memRegLocalMPointer, ebp, resultReg, cvec);
                genLeal (resultReg, resultReg, ~ bytes, cvec)
            );
            allocStoreCommonCode(resultReg, cvec, false)
        end
        |  allocStoreCode _ =
            raise InternalError "Allocation started but not complete"

        and allocStoreVarCode(resultReg, code as Code { inAllocation as ref false, ...}) =
            (* The result reg contains the requested size as a number of bytes
               on entry and returns with the base address. *)
        (
            inAllocation := true;
            (* Turn this into a negative value. *)
            genOpRegPlus2(Group3_A, resultReg, 0w3 (* neg *), code);
            (* Add this negative value to the current heap pointer. *)
            if isX64
            then genReg(ADD, resultReg, r15, code)
            else genOpEA(Arith (ADD, 0w3 (* r/m to reg *)), memRegLocalMPointer, ebp, resultReg, code);
            allocStoreCommonCode(resultReg, code, true)
        )
        | allocStoreVarCode _ =
            raise InternalError "Allocation started but not complete"
    end

    fun allocStoreAndSetSize (size, flag, resultReg, cvec) =
    (
        allocStoreCode (size, resultReg, cvec);
        if isX64
        then
        (
            genOpPlus2(MOVL_32_A, ~wordSize, resultReg, 0w0, cvec);
            (* TODO: What if the length won't fit in 32 bits? *)
            gen32s (size, cvec);
            (* Set the flag byte separately. *)
            if flag <> 0w0
            then
            (
                genOpPlus2(MOVB_8_A, ~1, resultReg, 0w0, cvec);
                gen8s (Word8.toInt flag, cvec)
            )
            else ()
        )
        else
        (
            genOpPlus2 (MOVL_32_A, ~wordSize, resultReg, 0w0, cvec);
            gen32u (LargeWord.fromInt size orbL (Word8.toLargeWord flag <<+ 0w24), cvec)
        )
    )

    (* Operations. *)
    type cases = word * label

    datatype memoryAddress =
        BaseOffset of { base: reg, offset: int, index: indexType }
    |   ConstantAddress of machineWord

    datatype branchPrediction = PredictNeutral | PredictTaken | PredictNotTaken

    datatype operation =
        MoveRR of { source: reg, output: reg }
    |   MoveConstR of { source: int, output: reg }
    |   MoveLongConstR of { source: machineWord, output: reg }
    |   LoadMemR of { source: memoryAddress, output: reg }
    |   LoadByteR of { source: memoryAddress, output: reg }
    |   PushR of reg
    |   PushConst of int
    |   PushLongConst of machineWord
    |   PushMem of { base: reg, offset: int }
    |   PopR of reg
    |   ArithRR of { opc: arithOp, output: reg, source: reg }
    |   ArithRConst of { opc: arithOp, output: reg, source: int }
    |   ArithRLongConst of { opc: arithOp, output: reg, source: machineWord }
    |   ArithRMem of { opc: arithOp, output: reg, offset: int, base: reg }
    |   ArithMemConst of { opc: arithOp, offset: int, base: reg, source: int }
    |   ArithMemLongConst of { opc: arithOp, offset: int, base: reg, source: machineWord }
    |   ShiftConstant of { shiftType: shiftType, output: reg, shift: Word8.word }
    |   ShiftVariable of { shiftType: shiftType, output: reg } (* Shift amount is in ecx *)
    |   ConditionalBranch of { test: branchOps, label: label, predict: branchPrediction }
    |   LockMutableSegment of reg
    |   LoadAddress of { output: reg, offset: int, base: reg option, index: indexType }
    |   LoadCodeRef of { output: reg, code: code }
    |   TestTagR of reg
    |   TestByteMem of { base: reg, offset: int, bits: word }
    |   CallRTS of int
    |   StoreRegToMemory of { toStore: reg, address: memoryAddress }
    |   StoreConstToMemory of { toStore: int, address: memoryAddress }
    |   StoreLongConstToMemory of { toStore: machineWord, address: memoryAddress }
    |   StoreByteRegToMemory of { toStore: reg, address: memoryAddress }
    |   StoreByteConstToMemory of { toStore: Word8.word, address: memoryAddress }
    |   AllocStore of { size: int, output: reg }
    |   AllocStoreVariable of reg
    |   StoreInitialised
    |   CallFunction of callKinds
    |   JumpToFunction of callKinds
    |   ReturnFromFunction of int
    |   RaiseException
    |   UncondBranch of label
    |   ResetStack of int
    |   InterruptCheck
    |   JumpLabel of label
    |   TagValue of { source: reg, output: reg }
        (* Some of these operations are higher-level and should be reduced. *)
    |   LoadHandlerAddress of { handlerLab: addrs ref, output: reg }
    |   StartHandler of { handlerLab: addrs ref }
    |   IndexedCase of { testReg: reg, workReg: reg, min: word, cases: label list }
    |   FreeRegisters of regSet
    |   MakeSafe of reg
    |   RepeatOperation of repOps
    |   Group3Ops of reg * group3Ops
    |   AtomicXAdd of {base: reg, output: reg}
    |   FPLoadFromGenReg of reg
    |   FPLoadFromFPReg of { source: reg, lastRef: bool }
    |   FPLoadFromConst of real
    |   FPStoreToFPReg of { output: reg, andPop: bool }
    |   FPStoreToMemory of { base: reg, offset: int, andPop: bool }
    |   FPArithR of { opc: fpOps, source: reg }
    |   FPArithConst of { opc: fpOps, source: machineWord }
    |   FPArithMemory of { opc: fpOps, base: reg, offset: int }
    |   FPUnary of fpUnaryOps
    |   FPStatusToEAX
    |   FPLoadIntAndPop
    |   FPFree of reg

    type operations = operation list

    fun printOperation(operation, stream) =
    let
        fun printReg r = stream(regRepr r)
        fun printBaseOffset(b, x, i) =
        (
            stream(Int.toString i); stream "("; printReg b; stream ")";
            case x of
                NoIndex => ()
            |   Index1 x => (stream "["; printReg x; stream "]")
            |   Index2 x => (stream "["; printReg x; stream "*2]")
            |   Index4 x => (stream "["; printReg x; stream "*4]")
            |   Index8 x => (stream "["; printReg x; stream "*8]")
        )
        fun printMemAddress(BaseOffset{ base, offset, index }) = printBaseOffset(base, index, offset)
        |   printMemAddress(ConstantAddress addr) = stream(stringOfWord addr)
        
        fun printCallKind Recursive = stream "Recursive"
        |   printCallKind (ConstantClosure w) = (stream "closure="; stream(stringOfWord w))
        |   printCallKind (ConstantCode w) = (stream "code="; stream(stringOfWord w))
        |   printCallKind (CodeFun(Code{procName, ...})) = stream("CODE-" ^ procName)
        |   printCallKind FullCall = stream "via ClosureReg"
     in
        case operation of
            MoveRR { source, output } =>
                (stream "MoveRR "; printReg output; stream " <= "; printReg source)

        |   MoveConstR { source, output } =>
                (stream "MoveCR "; printReg output; stream " <="; stream(Int.toString source))

        |   MoveLongConstR { output, source } =>
                (stream "MoveCR "; printReg output; stream " <= "; stream(Address.stringOfWord source))

        |   LoadMemR { source, output } =>
                (stream "MoveMR "; printReg output; stream " <= "; printMemAddress source )

        |   LoadByteR { source, output } =>
                (stream "MoveByteR "; printReg output; stream " <= "; printMemAddress source )

        |   ArithRR { opc, output, source } =>
                (stream (arithOpRepr opc ^ "RR "); printReg output; stream " <= "; printReg source )

        |   ArithRConst { opc, output, source } =>
                (stream (arithOpRepr opc ^ "RC "); printReg output; stream " <= "; stream(Int.toString source) )

        |   ArithRLongConst { opc, output, source } =>
                (stream (arithOpRepr opc ^ "RC "); printReg output; stream " <= "; stream(Address.stringOfWord source) )

        |   ArithRMem { opc, output, offset, base } =>
                (stream (arithOpRepr opc ^ "RM "); printReg output; stream " <= "; printBaseOffset(base, NoIndex, offset) )

        |   ArithMemConst { opc, offset, base, source } =>
            (
                stream (arithOpRepr opc ^ "MC "); printBaseOffset(base, NoIndex, offset);
                stream " "; stream(Int.toString source)
            )

        |   ArithMemLongConst { opc, offset, base, source } =>
            (
                stream (arithOpRepr opc ^ "MC ");
                printBaseOffset(base, NoIndex, offset);
                stream " <= "; stream(Address.stringOfWord source)
            )

        |   ShiftConstant { shiftType, output, shift } =>
            (
                stream(shiftTypeRepr shiftType); stream " "; printReg output;
                stream " by "; stream(Word8.toString shift)
            )

        |   ShiftVariable { shiftType, output } => (* Shift amount is in ecx *)
            (
                stream(shiftTypeRepr shiftType); stream " "; printReg output; stream " by ECX"
            )

        |   ConditionalBranch { test, label=Labels{labId=ref lab, ...}, predict } =>
            (
                stream(branchOpRepr test); stream " L"; stream(Int.toString lab);
                case predict of
                    PredictNeutral => ()
                |   PredictTaken => stream " PredictTaken"
                |   PredictNotTaken => stream " PredictNotTaken"
            )

        |   LockMutableSegment reg => (stream "LockMutableSegment "; printReg reg)

        |   PushR source => (stream "PushR "; printReg source)

        |   PushConst source => (stream "PushC "; stream(Int.toString source))

        |   PushLongConst source => (stream "PushC "; stream(Address.stringOfWord source))

        |   PushMem{base, offset} => (stream "PushM "; printBaseOffset(base, NoIndex, offset))

        |   PopR dest => (stream "PopR "; printReg dest)

        |   StoreRegToMemory { toStore, address } =>
            (
                stream "StoreRegToMemory "; printMemAddress address;
                stream " <= "; stream(regRepr toStore)
            )

        |   StoreConstToMemory { toStore, address } =>
            (
                stream "StoreConstToMemory "; printMemAddress address;
                stream " <= "; stream(Int.toString toStore)
            )

        |   StoreLongConstToMemory { address, toStore } =>
            (
                stream "StoreLongConstToMemory "; printMemAddress address; stream " <= "; stream(Address.stringOfWord toStore)
            )

        |   StoreByteRegToMemory { toStore, address } =>
            (
                stream "StoreByteRegToMemory "; printMemAddress address;
                stream " <= "; stream(regRepr toStore)
            )

        |   StoreByteConstToMemory { toStore, address } =>
            (
                stream "StoreByteConstToMemory "; printMemAddress address;
                stream " <= 0x"; stream(Word8.toString toStore)
            )

        |   LoadAddress{ output, offset, base, index } =>
            (
                stream "LoadAddress ";
                case base of NONE => () | SOME r => (printReg r; stream " + ");
                stream(Int.toString offset);
                case index of
                    NoIndex => ()
                |   Index1 x => (stream " + "; printReg x)
                |   Index2 x => (stream " + "; printReg x; stream "*2 ")
                |   Index4 x => (stream " + "; printReg x; stream "*4 ")
                |   Index8 x => (stream " + "; printReg x; stream "*8 ");
                stream " => "; printReg output
            )

        |   LoadCodeRef { output, code=Code{procName, ...} } =>
            ( stream "LoadCodeRef "; stream procName; stream " => "; printReg output )

        |   TestTagR reg => ( stream "TestTagR "; printReg reg )
        |   TestByteMem { base, offset, bits } =>
                ( stream "TestByteMem "; printBaseOffset(base, NoIndex, offset); stream " 0x"; stream(Word.toString bits) )
        |   CallRTS entry =>
            (
                stream "CallRTS ";
                if entry = memRegStackOverflowCall then stream "StackOverflowCall"
                else if entry = memRegHeapOverflowCall then stream "HeapOverflow"
                else if entry = memRegStackOverflowCallEx then stream "StackOverflowCallEx"
                else if entry = memRegRaiseException then stream "RaiseException"
                else if entry = memRegRaiseDiv then stream "RaiseDiv"
                else if entry = memRegArbEmulation then stream "ArbEmulation"
                else stream(Int.toString entry)
            )

        |   AllocStore { size, output } =>
                (stream "AllocStore "; stream(Int.toString size); stream " => "; printReg output )

        |   AllocStoreVariable reg => (stream "AllocStoreVariable "; printReg reg )
        
        |   StoreInitialised => stream "StoreInitialised"

        |   TagValue { source, output } =>
                (stream "TagValue "; printReg output; stream " <= "; printReg source)

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
        |   InterruptCheck => stream "InterruptCheck"
        |   JumpLabel(Labels{labId=ref lab, ...}) =>
                (stream "L"; stream(Int.toString lab); stream ":")
        |   LoadHandlerAddress { handlerLab=_, output=_ } =>
                stream "LoadHandlerAddress"
        |   StartHandler { handlerLab=_ } =>
                stream "StartHandler"
        |   IndexedCase { testReg, workReg, min, cases } =>
            (
                stream "IndexedCase "; printReg testReg; stream " with "; printReg workReg;
                stream "\n";
                List.foldl(fn(Labels{labId=ref lab, ...}, v) =>
                    (stream(Word.toString v); stream " => L"; stream(Int.toString lab); stream "\n"; v+0w1))
                    min cases;
                ()
            )
        |   FreeRegisters regs => (stream "FreeRegister "; stream(regSetRepr regs))
        |   MakeSafe reg => ( stream "MakeSafe "; printReg reg)
        |   RepeatOperation repOp => (stream "Repeat "; stream(repOpsRepr repOp))
        |   Group3Ops(reg, ops) => ( stream(group3OpsRepr ops); stream " "; printReg reg)
        |   AtomicXAdd{base, output} => (stream "LockedXAdd ("; printReg base; stream ") <=> "; printReg output)
        |   FPLoadFromGenReg reg => (stream "FPLoad "; printReg reg)
        |   FPLoadFromFPReg {source, lastRef} =>
                (stream "FPLoad "; printReg source; if lastRef then stream " (LAST)" else())
        |   FPLoadFromConst const => (stream "FPLoad "; stream(Real.toString const) )
        |   FPStoreToFPReg{ output, andPop } =>
                (if andPop then stream "FPStoreAndPop => " else stream "FPStore => "; printReg output)
        |   FPStoreToMemory{ base: reg, offset: int, andPop: bool } =>
            (
                if andPop then stream "FPStoreAndPop => " else stream "FPStore => ";
                printBaseOffset(base, NoIndex, offset)
            )
        |   FPArithR{ opc, source } => (stream(fpOpRepr opc); stream " "; printReg source)
        |   FPArithConst{ opc, source } => (stream(fpOpRepr opc); stream(Address.stringOfWord source))
        |   FPArithMemory{ opc, base, offset } => (stream(fpOpRepr opc); stream " "; printBaseOffset(base, NoIndex, offset))
        |   FPUnary opc => stream(fpUnaryRepr opc)
        |   FPStatusToEAX => (stream "FPStatus "; printReg eax)
        |   FPLoadIntAndPop => (stream "FPLoadIntAndPop (%ESP)")
        |   FPFree reg => (stream "FPFree "; printReg reg)

        ;
 
        stream "\n"
    end

    datatype implement = ImplementGeneral | ImplementLiteral of machineWord

    (* Test the bottom bit and jump depending on its value.  This is used
       for tag tests in arbitrary precision operations and also for testing
       for short/long values. *)
    fun testTag(r, cvec) =
    let
        val (regNum, rx) = getReg r
    in
        if r = eax
        then (* Special instruction for testing accumulator.  Can use an 8-bit test. *)
        (
            genop (TEST_ACC8, NONE, cvec);
            gen8u (0w1, cvec)
        )
        else if isX64
        then 
        ( (* We can use a REX code to force it to always use the low order byte. *)
            genop (Group3_a,
                if rx orelse regNum >= 0w4 then SOME{w=false, r=false, b=rx, x=false} else NONE, cvec);
            genmodrm (Register, 0w0 (* test *), regNum, cvec);
            gen8u(0w1, cvec)
        )
        else if r = ebx orelse r = ecx orelse r = edx (* can we use an 8-bit test? *)
        then (* Yes. The register value refers to low-order byte. *)
        (
            genop    (Group3_a, NONE, cvec);
            genmodrm (Register, 0w0 (* test *), regNum, cvec);
            gen8u    (0w1, cvec)
        )
        else
        (
            genop    (Group3_A, NONE, cvec);
            genmodrm (Register, 0w0 (* test *), regNum, cvec);
            gen32u   (0w1, cvec)
        )
    end

    (* Previously the jump table was a vector of destination addresses.
       Now changed to use a vector of jump instructions.  These are padded
       out to 8 bytes with no-ops.  The reason for the change is to ensure
       that the code segment only contains instructions so that we can scan
       for addresses within the code.  It also simplifies and speeds up
       the indexed jump at the expense of doubling the size of the table
       itself.  *)
 
    fun indexedCase (r1:reg, r2:reg, min:word, cases, cvec as Code{exited, ic, ...}) =
    let
        val startJumpTable = ref addrZero
        val nCases = List.length cases
        val (rc2, rx2) = getReg r2
    in
        (* Load the address of the jump table. *)
        genop  (MOVL_32_64_R rc2, if isX64 then SOME {w=true, r=false, b=rx2, x=false} else NONE, cvec);
        addConstToVec (HVal startJumpTable, InlineAbsolute, cvec);
        (* Compute the jump address.  The index is a tagged
           integer so it is already multiplied by 2.  We need to
           multiply by four to get the correct size. We subtract off
           the minimum value and also the shifted tag. *)
		let
			val adjustment = Word.toIntX min * ~8 - 4
		in
			(* In 64-bit mode this may not fit in a 32-bit value.  It will always
			   fit in 32-bit mode so we avoid an unnecessary long integer test. *)
			(* We don't need to consider any possible overflow in the execution 
			   because we've already checked that the value is within the range. *)
			if isX64 andalso (adjustment < ~exp2_31 orelse adjustment >= exp2_31)
			then
			(
                genImmed(ADD, r2, adjustment, cvec);
			 	genOpIndexed(LEAL, 0, SOME r2, Index4 r1, r2, cvec)
			)
        	else genOpIndexed(LEAL, adjustment, SOME r2, Index4 r1, r2, cvec)
		end;
        (* Jump into the jump table.  Since each entry in the table
           is 8 bytes long r2 will still be on a word + 2 byte
           boundary. *)
        genop (Group5, if rx2 then SOME{w=false, r=false, b=rx2, x=false} else NONE, cvec);
        genmodrm(Register, 0w4 (* jmp *), #1 (getReg r2), cvec);

        exited := true;
        (* There's a very good chance that we will now extend the branches for
           the "out of range" checks.  The code to do that doesn't know
           that all these branches will come to the same point so will generate three
           separate long branches. We could combine them but it's hardly worth it. *)
        doPending (cvec, nCases * 8 (* size of table. *) + 3 (* Maximum alignment *));
    
        (* The start address must be on a two byte boundary so that the
           address we've loaded is a valid code address. *)
        while (!ic) mod 0w4 <> 0w2 do genop (NOP, NONE, cvec);

        let
            fun addJump(Labels{forward, reverse=ref reverse, ...}) =
            (
                reverse = addrUnsetLabel orelse raise InternalError "addJump";
                gen8u (opToInt JMP_32, cvec);
                gen32u (0w0, cvec);
                forward := [ref(Jump32From (!ic addrPlus ~4))]  @ ! forward;
                (* Add no-ops to make it 8 bytes. *)
                gen8u (opToInt NOP, cvec);
                gen8u (opToInt NOP, cvec);
                gen8u (opToInt NOP, cvec)
            )
        in
            startJumpTable := !ic;
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

        (* Source and destination checking.  No longer used.  The optimiser removes RegisterStatusChange
           "instructions". *)
        fun checkSources _ = ()
        fun addDests _ = ()
        fun checkIndexSource _ = ()
    
        fun cgOp [] = ()

        (*
        |   cgOp(DataOp{ instr=InstrMulA, operands=[InRegister r1, InRegister r2], output=SOME rd } :: remainder) =
            (* Arbitrary precision multiplication. *)
            let
                val _ = checkSources[r1, r2]; val _ = addDests[rd];
                val addr = tagTest2 (rd, r1, r2, code) (* generates code *)
            in
                (* This is a bit complicated because the result is always placed
                   in the EDX:EAX register pair so we have to save one or both. *)
                 (* If the multiply overflows we need to be able to recover the
                    original arguments in order to emulate the instruction. *)
                if rd <> eax then genPush(eax, code) else ();
                if rd <> edx then genPush(edx, code) else ();
                if r2 = edx
                then
                (
                    (* Untag, but don't shift the multiplicand. *)
                    genLeal (eax, r1, ~1, code);
                    (* Shift down the multiplier to remove the tag. *)
                    genop (Group2_1_A, code);
                    genmodrm(Register, 0w7 (* sar *), getReg edx, code)
                )
                else (* r2 <> edx *)
                (
                    (* Shift down the multiplier. *)
                    if r1 <> edx then genMove(edx, r1, code) else ();
                    genop (Group2_1_A, code);
                    genmodrm(Register, 0w7 (* sar *), getReg edx, code);
                    (* Untag, but don't shift the multiplicand. *)
                    genLeal (eax, r2, ~1, code)
                );
                (* Do the multiplication. *)
                genop (Group3_A, code);
                genmodrm(Register, 0w5 (* imull *), getReg edx, code);
                (* Add back the tag, but don't shift. *)
                genLeal (rd, eax, 1, code);
                (* Restore the saved registers.  N.B. This also has
                   the effect of making sure that both eax and edx contain
                   valid values. *)
                if rd <> edx then genop(POP_R edx, code) else ();
                if rd <> eax then genop(POP_R eax, code) else ();
                genJO8  (addr, code) (* Check for overflow. *);
                cgOp remainder
            end
*) 
        |   cgOp(LockMutableSegment baseReg :: remainder) =
                (* Remove the mutable bit from the flag byte. *)(*andb CONST(0xff-0x40),-1[Reax]*)
            (
                checkSources[baseReg];
                genOpPlus2 (Group1_8_a, ~1, baseReg, arithOpToWord AND, code);
                gen8u(0wxff - 0wx40, code);
                cgOp remainder
            )

        |   cgOp(MoveRR{ source=source, output } :: remainder) =
                (* Move from one general register to another. *)
            (
                checkSources[source]; addDests[output];
                genMove(output, source, code);
                cgOp remainder
            )

        |   cgOp(MoveConstR{ source, output as GenReg _ } :: remainder) =
            (
                addDests[output];
                (* The RTS scans for possible addresses in MOV instructions so we
                   can only use MOV if this is a tagged value.  If it isn't we have
                   to use something else such as XOR/ADD. *)
                if source mod 2 = 0
                then
                (
                    genReg(XOR, output, output, code);
                    if source = 0 then ()
                    else genImmed (ADD, output, source, code)
                )
                else if isX64 andalso isTagged32bitS(toMachineWord source)
                then (* This is better on X64 but longer than a 32 bit immediate on i386 *)
                (
                    genOpRegPlus2 (MOVL_32_A, output, 0w0, code);
                    gen32s (source, code)
                )
                else
                let
                    val (rc, rx) = getReg output
                in
                    genop (MOVL_32_64_R rc,
                            if isX64 then SOME {w=true, r=false, b=rx, x=false} else NONE, code);
                    if isX64 then gen64s (source, code) else gen32s (source, code)
                end;
                cgOp remainder
            )

        |   cgOp(MoveConstR{ source, output as FPReg _ } :: remainder) =
            let
                val _ = addDests[output]
                    (* We seem to get a short zero here as a result of putting in a
                       void value. I think this occurs when a dummy value is put on
                       when one side of a branch raises an exception. *)
                val _ = source = tag 0 orelse raise InternalError "Move LiteralSource to fp reg: invalid source"
            in
                genFloatingPt({escape=0w1, md=0w3, nnn=0w5, rm=0w6}, code); (* FLDZ *)
                storeFpRegFromStack(output, code);
                cgOp remainder
            end

        |   cgOp(MoveLongConstR{ source, output } :: remainder) =
            let
                val (rc, rx) = getReg output
            in
                addDests[output];
                genop(MOVL_32_64_R rc,
                            if isX64 then SOME {w=true, r=false, b=rx, x=false} else NONE, code);
                addConstToVec (WVal source, InlineAbsolute, code); (* Remember this constant and address. *)
                cgOp remainder
            end

        |   cgOp(LoadMemR{ source=BaseOffset{base, offset, index=NoIndex}, output as GenReg _} :: ResetStack count :: remainder) =
            if base = esp andalso offset < count * wordSize
            then (* Can use a pop instruction. *)
            let
                val resetBefore = Int.min(offset div wordSize, count)
            in
                if resetBefore = 0 (* So offset must be zero. *)
                then
                let
                    val _ = offset = 0 orelse raise InternalError "cgOp: offset non-zero"
                    val resetAfter = count - resetBefore - 1
                in
                    checkSources[base]; addDests[output];
                    genPushPop(POP_R, output, code);
                    cgOp(if resetAfter = 0 then remainder else ResetStack resetAfter :: remainder)
                end
                else cgOp(ResetStack resetBefore ::
                          LoadMemR{source=BaseOffset{base=base, offset=offset-resetBefore*wordSize, index=NoIndex}, output=output } ::
                          (if count = resetBefore then remainder else ResetStack(count - resetBefore) :: remainder))
            end
            else
            (
                checkSources[base]; addDests[output];
                genLoad(offset, base, output, code);
                cgOp(ResetStack count :: remainder)
            )

        |   cgOp(LoadMemR{source=BaseOffset{base, offset, index=NoIndex}, output} :: remainder) =
            (
                checkSources[base]; addDests[output];
                genLoad(offset, base, output, code);
                cgOp remainder
            )

        |   cgOp(LoadMemR{source=BaseOffset{base, offset, index}, output } :: remainder) =
            (
                checkSources[base]; checkIndexSource index; addDests[output];
                genOpIndexed(MOVL_A_R, offset, SOME base, index, output, code);
                cgOp remainder
            )

        |   cgOp(LoadMemR{source=ConstantAddress addr, output } :: remainder) =
            (
                addDests[output];
                (* The absolute address form is interpreted as PC relative in 64-bit mode. *)
                if isX64 then raise InternalError "LoadMemR: ConstantAddress" else ();
                genop(MOVL_A_R, NONE, code);
                genmodrm (Based0, #1 (getReg output), 0w5 (* constant address *), code);
                addConstToVec(WVal addr, InlineAbsolute, code);
                cgOp remainder
            )

        |   cgOp(LoadByteR{source=BaseOffset{base, offset, index}, output } :: remainder) =
            (
                checkSources[base]; checkIndexSource index; addDests[output];
                case index of
                    NoIndex => genOpEA (MOVZX (* 2 byte opcode *), offset, base, output, code)
                |   _ => genOpIndexed (MOVZX, 0, SOME base, index, output, code);
                cgOp remainder
            )

        |   cgOp(LoadByteR{source=ConstantAddress addr, output } :: remainder) =
            (
                addDests[output];
                if isX64 then raise InternalError "LoadByteR: ConstantAddress" else ();
                genop (MOVZX, NONE, code);
                genmodrm (Based0, #1(getReg output), 0w5 (* constant address *), code);
                addConstToVec(WVal addr, InlineAbsolute, code);
                cgOp remainder
            )

        |   cgOp(LoadCodeRef{ code=refCode, output } :: remainder) =
            let
                val (rc, rx) = getReg output
            in
                addDests[output];
                genop (MOVL_32_64_R rc,
                        if isX64 then SOME {w=true, r=false, b=rx, x=false} else NONE, code);
                codeConst(refCode, InlineAbsolute, code);
                cgOp remainder
            end

        |   cgOp(LoadAddress{ offset, base, index, output } :: remainder) =
            (
                (* This provides a mixture of addition and multiplication in a single
                   instruction. *)
                addDests[output]; case base of NONE => () | SOME b => checkSources[b];
                case (index, base) of
                    (NoIndex, SOME base) => genOpEA(LEAL, offset, base, output, code)
                |   (NoIndex, NONE) => raise InternalError "LoadAddress: no base or index"
                |   _ => genOpIndexed(LEAL, offset, base, index, output, code);
                cgOp remainder
            )

        |   cgOp(ArithRR{ opc, output, source } :: remainder) =
            (
                case opc of
                    XOR =>
                        (if output = source then () else checkSources[output, source]; addDests[output])
                |   CMP => checkSources[output, source]
                |   _ => (checkSources[output, source]; addDests[output]);
                genReg (opc, output, source, code);
                cgOp remainder
            )

        |   cgOp(ArithRConst{ opc, output, source } :: remainder) =
            (
                checkSources[output]; case opc of CMP => () | _ => addDests[output];
                genImmed (opc, output, source, code);
                cgOp remainder
            )

        |   cgOp(ArithRLongConst{ opc, output, source } :: remainder) =
            (* This is only used for opc=CMP to compare addresses for equality. *)
            let
                val (rc, rx) = getReg output
            in
                checkSources[output]; case opc of CMP => () | _ => addDests[output];
                if isX64
                then
                (
                    genop(Arith (opc, 0w3), SOME {w=true, r=rx, b=false, x=false}, code);
			        genmodrm(Based0, rc, 0w5 (* Immediate address. *), code);
                    addConstToVec (WVal source, ConstArea 0, code)
                )
                else
                (
                    genop (Group1_32_A (* group1, 32 bit immediate *), NONE, code);
                    genmodrm(Register, arithOpToWord opc, rc, code);
                    addConstToVec (WVal source, InlineAbsolute, code) (* Remember this constant and address. *)
                );
                cgOp remainder
            end

        |   cgOp(ArithRMem{ opc, output, offset, base } :: remainder) =
            (
                checkSources[output, base]; case opc of CMP => () | _ => addDests[output];
                genOpEA(Arith (opc, 0w3), offset, base, output, code);
                cgOp remainder
            )

        |   cgOp(ArithMemConst{ opc, offset, base, source } :: remainder) =
                let
                    val () = checkSources[base];
                in
                    if is8Bit source
                    then (* Can use one byte immediate *) 
                    (
                        genOpPlus2(Group1_8_A (* group1, 8 bit immediate *),
                                   offset, base, arithOpToWord opc, code);
                        gen8s (source, code)
                    )
                    else (* Need 32 bit immediate. *)
                    (
                        genOpPlus2(Group1_32_A (* group1, 32 bit immediate *), 
                                   offset, base, arithOpToWord opc, code);
                        gen32s(source, code)
                    );
                    cgOp remainder
                end

        |   cgOp(ArithMemLongConst{ opc, offset, base, source } :: remainder) =
            (
                checkSources[base];
                (* Currently this is always a comparison.  We have to be careful that
                   we don't accidentally get a zero word. *)
                genMemoryConstant(source, Group1_32_A, arithOpToWord opc, offset, base, NoIndex, code);
                cgOp remainder
            )

        |   cgOp(ShiftConstant { shiftType, output, shift } :: remainder) =
            (
                if shift = 0w1
                then genOpRegPlus2(Group2_1_A, output, shiftTypeToWord shiftType, code)
                else
                (
                    genOpRegPlus2(Group2_8_A, output, shiftTypeToWord shiftType, code);
                    gen8u(shift, code)
                );
                cgOp remainder
            )

        |   cgOp(ShiftVariable { shiftType, output } :: remainder) =
            (
                genOpRegPlus2(Group2_CL_A, output, shiftTypeToWord shiftType, code);
                cgOp remainder
            )

        |   cgOp(TestTagR reg :: remainder) =
            (
                checkSources[reg];
                (* Test the tag bit and set the condition code *)
                testTag(reg, code);
                cgOp remainder
            )

        |   cgOp(TestByteMem{base, offset, bits} :: remainder) =
            (
                checkSources[base];
                (* Test the tag bit and set the condition code. *)
                genOpPlus2(Group3_a, offset, base, 0w0 (* test *), code);
                gen8u(wordToWord8 bits, code);
                cgOp remainder
            )

        |   cgOp(ConditionalBranch{ test=opc, label=Labels{forward, reverse, ...}, ... } :: remainder) =
            (
                !reverse = addrUnsetLabel orelse raise InternalError "Conditional jump back";
                genop(CondJump opc, NONE, code);
                gen8u(0w0, code);
                forward := makeShortLabel (!ic addrPlus ~1, code) :: !forward;
                cgOp remainder
            )

        |   cgOp(CallRTS entry :: remainder) =
            (
                genop(Group5, NONE, code);
                genmodrm (Based8, 0w2 (* call *), #1 (getReg ebp), code);
                gen8u (Word8.fromInt entry(*memRegArbEmulation*), code);
                cgOp remainder
            )

        |   cgOp(TagValue{ source, output} :: remainder) =
            (
                (* Convert an untagged integer into a tagged value by shifting and adding 1. *)
                checkSources[source]; addDests[output];
                genOpIndexed (LEAL, 1, SOME source, Index1 source, output, code);
                cgOp remainder
            )

        |   cgOp(RepeatOperation repOp :: remainder) =
            (
                checkSources[edi, ecx]; addDests[edi, ecx];
                case repOp of
                    STOSB => checkSources[eax]
                |   STOSL => checkSources[eax]
                |   _ => (checkSources[esi]; addDests[esi]);
                genop(REP, NONE, code);
                (* Put in a rex prefix to force 64-bit mode. *)
                if isX64 andalso (case repOp of STOSL => true | MOVSL => true | _ => false)
                then gen8u(rex{w=true, r=false, b=false, x = false}, code)
                else ();
                gen8u(repOpsToWord repOp, code);
                cgOp remainder
            )

        |   cgOp(Group3Ops(reg, ops) :: remainder) =
            (
                checkSources[reg];
                case ops of
                    NOT => addDests[reg]
                |   NEG => addDests[reg]
                |   MUL => (checkSources[eax]; addDests[eax, edx])
                |   IMUL => (checkSources[eax]; addDests[eax, edx])
                |   DIV => (checkSources[eax, edx]; addDests[eax, edx])
                |   IDIV => (checkSources[eax, edx]; addDests[eax, edx]);
                genOpRegPlus2(Group3_A, reg, group3OpsToWord ops, code);
                cgOp remainder
            )

        |   cgOp(AtomicXAdd{base, output}:: remainder) =
            (
                checkSources[base, output]; addDests[output];
                (* Locked exchange-and-add.  We need the lock prefix before the REX prefix. *)
                genOpEA (LOCK_XADD, 0, base, output, code); (*0wxF0
                gen8u (0wx0f (* ESCAPE *), code);
                gen8u(0wxC1 (* xaddl *), code);
                genEA(0, base, output, code);*)
                cgOp remainder
            )

        |   cgOp(PushR(reg as GenReg _ ) :: remainder) =
                    (checkSources[reg]; genPushPop(PUSH_R, reg, code); cgOp remainder)
        |   cgOp(PushR(src as FPReg _ ) :: remainder) =
            (
                (* We need to push a fp register to the stack.  This can't be done directly so
                   needs to go through a general register.
                   Push eax (any register would do), allocate memory into that, then
                   swap the value with the top of the stack, restoring the original
                   eax and putting the address of the store onto the stack.
                   It would be better to choose a free register and use that since
                   we wouldn't need to save it. *)
                (* This originally used the XCHNG instruction to do the swap but
                   that is very expensive because it involves a lock.  This version is
                   slightly more complicated but much quicker. *)
                checkSources[src];
                genPushPop (PUSH_R, eax, code);
                genPushPop (PUSH_R, eax, code);
                loadFpRegToStack(src, 0w0, code);
                allocStoreAndSetSize(8 div wordSize, F_bytes, eax, code);
                genOpPlus2(FPESC 0w5, 0, eax, 0wx3, code); (* FSTP [rd] *)
                genOpEA (MOVL_R_A, wordSize, esp, eax, code);
                genPushPop(POP_R, eax, code);
                (* We've completed the allocation. *)
                case code of Code { inAllocation, ...} => inAllocation := false;
                cgOp remainder
            )

        |   cgOp(PushMem{base, offset} :: remainder) =
            (
                checkSources[base];
                genOpPlus2(Group5, offset, base, 0w6 (* push *), code);
                cgOp remainder
            )

        |   cgOp(PushConst constnt :: remainder) = 
            (
                if is8Bit constnt
                then ( genop (PUSH_8, NONE, code); gen8s (constnt, code) )
                else ( genop (PUSH_32, NONE, code); gen32s(constnt, code) );
                cgOp remainder
            )

        |   cgOp(PushLongConst constnt :: remainder) = 
            (
                if isX64
                then (* Put it in the constant area. *)
		        (
                    genop (Group5, NONE, code);
                    genmodrm(Based0, 0w6 (* push *), 0w5 (* PC rel *), code);
                    addConstToVec (WVal constnt, ConstArea 0, code)
                )
                else (* 32-bit *)
                (
                    genop  (PUSH_32, NONE, code);
                    addConstToVec (WVal constnt, InlineAbsolute, code)
		        );
                cgOp remainder
            )

        |   cgOp(PopR reg :: remainder) = (addDests[reg]; genPushPop(POP_R, reg, code); cgOp remainder)

        |   cgOp(StoreRegToMemory{ toStore, address } :: remainder) =
            (
                checkSources[toStore];
                case address of
                    BaseOffset{offset, base, index=NoIndex} =>
                    (
                        checkSources[base];
                        genOpEA(MOVL_R_A, offset, base, toStore, code) 
                    )
                |   BaseOffset{offset, base, index} =>
                    (
                        checkSources[base];
                        checkIndexSource index;
                        genOpIndexed(MOVL_R_A, offset, SOME base, index, toStore, code)
                    )
                |   ConstantAddress address =>
                    (
                        if isX64 then raise InternalError "StoreRegToMemory: ConstantAddress" else ();
                        genop(MOVL_R_A, NONE, code);
                        genmodrm(Based0, #1 (getReg toStore), 0w5 (* constant address *), code);
                        addConstToVec(WVal address, InlineAbsolute, code)
                    );
                cgOp remainder
            )

        |   cgOp(StoreConstToMemory{ toStore=toStore, address } :: remainder) =
            (
                (* Short constant *)
                case address of
                    BaseOffset{offset, base, index=NoIndex} =>
                    (
                        checkSources[base];
                        genOpPlus2(MOVL_32_A, offset, base, 0w0, code) 
                    )
                |   BaseOffset{offset, base, index} =>
                    (
                        checkSources[base];
                        checkIndexSource index;
                        genOpIndexed (MOVL_32_A, offset, SOME base, index, mkReg(0w0, false), code)
                    )
                |   ConstantAddress address =>
                    (
                        if isX64 then raise InternalError "StoreRegToMemory: ConstantAddress" else ();
                        genop (MOVL_32_A, NONE, code);
                        genmodrm (Based0, 0w0, 0w5 (* constant address *), code);
                        addConstToVec(WVal address, InlineAbsolute, code)
                    );
                gen32s (toStore, code);
                cgOp remainder
            )

        |   cgOp(StoreLongConstToMemory{ toStore=toStore, address=BaseOffset{offset, base, index} } :: remainder) =
            (
                checkSources[base]; checkIndexSource index;
                genMemoryConstant(toStore, MOVL_32_A, 0w0, offset, base, index, code);
                cgOp remainder
            )

        |   cgOp(StoreLongConstToMemory{ toStore=toStore, address=ConstantAddress address } :: remainder) =
            (
                (* We have to be careful here that we don't accidentally produce a full word of
                   zeros aligned on a word boundary.  Since we have two addresses here which
                   could potentially have any combination of zeros in the high bytes of one and
                   the low bytes of the next the only safe option is to ensure the addresses are
                   on word boundaries. *)
                if isX64 then raise InternalError "StoreLongConstToMemory: ConstantAddress" else ();
                doPending(code, wordSize*2 + 2);
                align(0w2, code);
                genop (MOVL_32_A, NONE, code);
                genmodrm (Based0, 0w0, 0w5 (* constant address *), code);
                addConstToVec(WVal address, InlineAbsolute, code);
                addConstToVec(WVal toStore, InlineAbsolute, code);
                cgOp remainder
            )

        |   cgOp(StoreByteRegToMemory{ toStore, address } :: remainder) =
            (
                checkSources[toStore];
                case address of
                    BaseOffset{offset, base, index=NoIndex} =>
                    (
                        checkSources[base];
                        genOpEA(MOVB_R_A, offset, base, toStore, code) 
                    )
                |   BaseOffset{offset, base, index} =>
                    (
                        checkSources[base];
                        checkIndexSource index;
                        genOpIndexed(MOVB_R_A, offset, SOME base, index, toStore, code)
                    )
                |   ConstantAddress address =>
                    (
                        if isX64 then raise InternalError "StoreByteRegToMemory: ConstantAddress" else ();
                        genop (MOVB_R_A, NONE, code);
                        genmodrm(Based0, #1 (getReg toStore), 0w5 (* constant address *), code);
                        addConstToVec(WVal address, InlineAbsolute, code)
                    );
                cgOp remainder
            )

        |   cgOp(StoreByteConstToMemory{ toStore=toStore, address } :: remainder) =
            (
                (* Short constant *)
                case address of
                    BaseOffset{offset, base, index=NoIndex} =>
                    (
                        checkSources[base];
                        genOpPlus2(MOVB_8_A, offset, base, 0w0, code) 
                    )
                |   BaseOffset{offset, base, index} =>
                    (
                        checkSources[base];
                        checkIndexSource index;
                        genOpIndexed(MOVB_8_A, offset, SOME base, index, mkReg(0w0, false), code)
                    )
                |   ConstantAddress address =>
                    (
                        if isX64 then raise InternalError "StoreByteConstToMemory: ConstantAddress" else ();
                        genop (MOVB_8_A, NONE, code);
                        genmodrm (Based0, 0w0, 0w5 (* constant address *), code);
                        addConstToVec(WVal address, InlineAbsolute, code)
                    );
                gen8u (toStore, code);
                cgOp remainder
            )

        |   cgOp(AllocStore{ size, output } :: remainder) =
                (addDests[output]; allocStoreCode(size, output, code); cgOp remainder)

        |   cgOp(AllocStoreVariable reg :: remainder) =
                (checkSources[reg]; addDests[reg]; allocStoreVarCode(reg, code); cgOp remainder)

        |   cgOp(StoreInitialised :: remainder) =
            (
                (* This is just for debugging to ensure we have properly initialised a
                   piece of memory before allocating a new one. *)
                case code of
                    Code { inAllocation as ref true, ...} => inAllocation := false
                |   _ => raise InternalError "Found StoreInitialised but not in allocation" ;
                cgOp remainder
            )

        |   cgOp(CallFunction callKind :: remainder) = (callFunction(callKind, code); cgOp remainder)

        |   cgOp(JumpToFunction callKind :: remainder) = (jumpToFunction(callKind, code); cgOp remainder)

        |   cgOp(ReturnFromFunction argsToRemove :: remainder) =
            (
                returnFromFunction(argsToRemove, code);
                cgOp remainder
            )

        |   cgOp(RaiseException :: remainder) =
            (
                checkSources[eax];
                (* Load the current handler into ebx.  Any register will do since we
                   don't preserve registers across exceptions.
                   Call, rather than jump to, the exception code so that we have
                   the address of the caller if we need to produce an exception
                   trace. *)
                genLoad(memRegHandlerRegister, ebp, ebx, code);
                doPending (code, maxInstrSize+3);
                (* Since we're calling we put the "return address" on a word+2 byte
                  boundary.  This is never actually used as a return address but
                  it's probably best to make sure it's properly aligned.  It probably
                  simplifies exception tracing which is the reason it's there. *)
                align (0w3, code);
                genop(Group5, NONE, code);
                genmodrm (Based0, 0w2 (* call *), #1 (getReg ebx), code);
                cgOp remainder
            )

        |   cgOp(UncondBranch(Labels{forward, reverse=ref reverse, ...}) :: remainder) =
            (
                (* This may be a forward jump, in which case we don't have the destination and
                   can just record it, or it may be a backward jump in which case we already
                   have the destination. *)
                if reverse = addrUnsetLabel (* Destination is after this. *)
                then forward := unconditionalBranch code @ ! forward
                else 
                let
                    (* Do any pending instructions before calculating the offset, just
                       in case we put in some instructions first. *)
                    val () = doPending (code, maxInstrSize)
                    val offset  = reverse addrMinus (!ic); (* Negative *)
                    val offset2 = offset - 2;
                in
                    if is8Bit offset2
                    then ( genop (JMP_8, NONE, code); gen8s (offset2, code) )
                    else ( genop  (JMP_32, NONE, code); gen32s (offset - 5, code) )
                end;
                cgOp remainder
            )

        |   cgOp(ResetStack count1 :: ResetStack count2 :: remainder) =
                (* Combine adjacent resets. *)
                cgOp(ResetStack(count1+count2) :: remainder)

        |   cgOp((r as ResetStack _) :: (f as FreeRegisters _) :: remainder) =
                (* Re-order register frees round resets. *)
                cgOp(f :: r :: remainder)

        |   cgOp(ResetStack count :: remainder) =
            let
                val sr = Word.toInt(wordsToBytes(Word.fromInt count)) (* Offset in bytes. *)
            in
                if is8Bit sr
                then (* Can use one byte immediate *) 
                (
                    genOpRegPlus2(Group1_8_A (* group1, 8-bit immediate *), esp, arithOpToWord ADD, code);
                    gen8s(sr, code)
                )
                else (* Need 32 bit immediate. *)
                (
                   genOpRegPlus2(Group1_32_A (* group1, 32-bit immediate *), esp, arithOpToWord ADD, code);
                   gen32s(sr, code)
                );
                cgOp remainder
            end

        |   cgOp(JumpLabel(Labels{forward=ref forward, reverse, ...}) :: remainder) =
            let
                (* This is a bit complicated.  We may have multiple labels at this
                   location and they may be a combination of forward and backward labels.
                   We don't want to put in a branch extension to this location unnecessarily
                   and in particular we really don't want a 32-bit branch immediately before
                   this because that would put in a zero word.  Instead we just record
                   the branches and actually set them when we generate real code. *)
                val Code {justComeFromAddrs, ...} = code
            in
                fixup(forward, code); (* Fix up any forward branches to here. *)
                (* Record the address. *)
                justComeFromAddrs := reverse :: ! justComeFromAddrs;
                cgOp remainder
            end
        
        |   cgOp(InterruptCheck :: remainder) = (stackCheck code; cgOp remainder)

        |   cgOp(LoadHandlerAddress{ handlerLab, output } :: remainder) =
            (
                addDests[output];
                loadHandlerAddress(output, handlerLab, code);
                cgOp remainder
            )

        |   cgOp(StartHandler{ handlerLab } :: remainder) = (fixupHandler(handlerLab, code);  cgOp remainder)

        |   cgOp(IndexedCase { testReg, workReg, min, cases } :: remainder) =
            (
                checkSources[testReg]; addDests[workReg]; (* Check workReg is free? *)
                indexedCase(testReg, workReg, min, cases, code);
                cgOp remainder
            )

        |   cgOp (FreeRegisters _ :: remainder) = cgOp remainder

        |   cgOp (MakeSafe output :: remainder) =
            (
                addDests[output];
                (* The register contains an untagged value.  Clobber it. Because this is
                   executed in the floating point comparison code we need to choose
                   something that doesn't affect the condition codes.  One possibility
                   would be a move from another register if we could find one that was
                   definitely safe. *)
                genTag(output, code);
                cgOp remainder
            )

        |   cgOp (FPLoadFromGenReg source :: remainder) =
            (
                checkSources[source];
                (* The "value" in the general register is actually the address of
                   the memory containing the FP value. *)
                genOpPlus2(FPESC 0w5, 0, source, 0wx0, code); (* FLD [r1] *)
                cgOp remainder
            )

        |   cgOp (FPLoadFromFPReg{source, ...} :: remainder) =
            (
                checkSources[source];
                (* Assume there's nothing currently on the stack. *)
                loadFpRegToStack(source, 0w0, code);
                cgOp remainder
            )

        |   cgOp (FPLoadFromConst realValue :: remainder) =
            let
                open Real
                infix ==
            in
                (* Treat +/- 0,1 as special cases. *)
                if realValue == 0.0 (* This is also true for -0.0 *)
                then
                (
                    genFloatingPt({escape=0w1, md=0w3, nnn=0w5, rm=0w6}, code); (* FLDZ *)
                    if signBit realValue
                    then genFloatingPt({escape=0w1, md=0w3, nnn=0w4, rm=0w0}, code)
                    else ()
                )
                else if realValue == 1.0
                then genFloatingPt({escape=0w1, md=0w3, nnn=0w5, rm=0w0}, code) (* FLD1 *)
                else if realValue == ~1.0
                then
                (
                    genFloatingPt({escape=0w1, md=0w3, nnn=0w5, rm=0w0}, code); (* FLD1 *)
                    genFloatingPt({escape=0w1, md=0w3, nnn=0w4, rm=0w0}, code) (* FCHS *)
                )
                else
                (
                    (* The real constant here is actually the address of an 8-byte memory
                       object.  FLD takes the address as the argument and in 32-bit mode
                       we use an absolute address.  In 64-bit mode we need to put the
                       constant at the end of the code segment and use PC-relative
                       addressing which happens to be encoded in the same way. *)
                    genop(FPESC 0w5, NONE, code); (* FLD [Constant] *)
                    genmodrm (Based0, 0w0, 0w5 (* constant address/PC-relative *), code);
                    addConstToVec(WVal(toMachineWord realValue),
                        if isX64 then NonAddrArea else InlineAbsolute, code)
                );
                cgOp remainder
            end

        |   cgOp (FPStoreToFPReg{ output, andPop } :: remainder) =
            let
                val _ = addDests[output]
                val dest = case output of FPReg fp => fp | _ => raise InternalError "fpreg"
            in
                (* Assume there's one item on the stack. *)
                genFloatingPt({escape=0w5, md=0w3, nnn=if andPop then 0wx3 else 0wx2,
                               rm = dest+0w1(* One item *)}, code); (* FSTP ST(n+1) *)
                cgOp remainder
            end

        |   cgOp (FPStoreToMemory{ base, offset, andPop } :: remainder) =
            (
                checkSources[base];
                genOpPlus2(FPESC 0w5, offset, base, if andPop then 0wx3 else 0wx2, code); (* FST/FSTP [rd] *)
                cgOp remainder 
            )

        |   cgOp (FPArithR{ opc, source } :: remainder) =
            let
                val fp = case source of FPReg fp => fp | _ => raise InternalError "cgOp: FPArithR"
            in
                genFloatingPt({escape=0w0, md=0w3, nnn=fpOpToWord opc,
                        rm=fp + 0w1 (* One item already there *)}, code);
                cgOp remainder 
            end

        |   cgOp (FPArithConst{ opc, source } :: remainder) =
            (
                (* See comment on FPLoadFromConst *)
                genop(FPESC 0w4, NONE, code); (* FADD etc [constnt] *)
                genmodrm (Based0, fpOpToWord opc, 0w5 (* constant address *), code);
                addConstToVec(WVal source,
                    if isX64 then NonAddrArea else InlineAbsolute, code);
                cgOp remainder
            )

        |   cgOp (FPArithMemory{ opc, base, offset } :: remainder) =
            (
                checkSources[base];
                genOpPlus2(FPESC 0w4, offset, base, fpOpToWord opc, code); (* FADD/FMUL etc [r2] *)
                cgOp remainder
            )

        |   cgOp (FPUnary opc :: remainder) =
            let
                val {rm, nnn} = fpUnaryToWords opc
            in
                genFloatingPt({escape=0w1, md=0w3, nnn=nnn, rm=rm}, code); (* FCHS etc *)
                cgOp remainder
            end

        |   cgOp (FPStatusToEAX :: remainder) =
            (
                addDests[eax];
                genop(FPESC 0w7, NONE, code); (* FNSTSW AX *)
                gen8u(0wxe0, code);
                cgOp remainder
            )

        |   cgOp (FPFree reg :: remainder) =
            let
                val dest = case reg of FPReg fp => fp | _ => raise InternalError "fpreg"
            in
                genFloatingPt({escape=0w5, md=0w3, nnn=0w0, rm=dest}, code); (* FFREE FP(n) *)
                cgOp remainder
            end

        |   cgOp (FPLoadIntAndPop :: remainder) =
            (
                (* There are some constraints here: We need to load from memory but the
                   value has to be shifted first.  We need to be able to emulate
                   the instruction. The easiest way to do this is
                   to push the value to the stack. *)
                (* Shift the value we pushed to untag it now we've done any trapping. *)
                genOpPlus2 (Group2_1_A, 0, esp, 0w7 (* sar *), code);
                (* fildl (esp) in 32-bit mode or fildq (esp) in 64-bit mode. *)
                if isX64
                then genOpPlus2(FPESC 0w7, 0, esp, 0w5, code)
                else genOpPlus2(FPESC 0w3, 0, esp, 0w0, code);
                (* Pop the stack.  This value is not a valid tagged value. *)
                cgOp(ResetStack 1 :: remainder) (* Pop the stack. *)
            )
    in
        cgOp ops
    end


    fun printCode (Code{procName, numOfConsts, constVec, printStream, ...}) seg endcode =
    let
        val print = printStream
        val ptr = ref 0w0;
        (* prints a string representation of a number *)
        fun printHex v = print(Int.fmt StringCvt.HEX v)
 
        infix 3 +:= ;
        fun (x +:= y) = (x := !x + (y:word));

        fun print32 () =
        let
            val valu = get32s (!ptr, seg); 
            val () = (ptr +:= 0w4);
        in
            if valu = tag 0 andalso !numOfConsts <> 0w0
            then
                (* May be a reference to a code-segment we haven't generated yet.
                   In that case we try to print the name of the function rather
                   than simply printing "1".  It might be nice to print the
                   function name in other cases but that might be complicated. *)
            let
                val caddr = !ptr - 0w4
                fun findRef [] = (* Not there - probably really tagged 0 *) printHex valu
                |  findRef ({const = CVal(Code{procName, ...}), addrs, ...} :: rest) =
                        if caddr = addrs
                        then print("=" ^ procName)
                        else findRef rest
                |  findRef (_ :: rest) = findRef rest
            in
                findRef(! constVec)
            end
            else printHex valu
        end;

        fun print64 () =
        let
            val valu = get64s(!ptr, seg);
        in
            printHex valu;
            ptr +:= 0w8
        end

        fun get16s (a, seg: cseg) : int =
        let
            val b0  = Word8.toInt (csegGet (seg, a));
            val b1  = Word8.toInt (csegGet (seg, a + 0w1));
            val b1' = if b1 >= exp2_7 then b1 - exp2_8 else b1;
        in
            (b1' * exp2_8) + b0
        end
 
        fun print16 () = printHex(get16s (!ptr, seg) before (ptr +:= 0w2))
        and print8 () = printHex(get8s (!ptr, seg) before (ptr +:= 0w1))
 
        fun printJmp () =
        let
            val valu = get8s (!ptr, seg) 
            val () = ptr +:= 0w1;
        in
            printHex (valu + Word.toInt(!ptr))
        end;
 
        (* Print an effective address. *)
        fun printEA rex =
        let
            val modrm = Word8.toInt (csegGet (seg, !ptr));
            val () = (ptr +:= 0w1);
            val md = modrm div 64;
            val rm = modrm mod 8;
            (* Decode the Rex prefix if present. *)
            val rexX = (rex andb8 0wx2) <> 0w0
            val rexB = (rex andb8 0wx1) <> 0w0
        in
            if md = 3
            then print (regRepr (mkReg(Word8.fromInt rm, rexB)))
      
            else if rm = 4
            then
            let (* s-i-b present. *)
                val sib = Word8.toInt (csegGet (seg, !ptr));
                val () = (ptr +:= 0w1);
                val ss    = sib div 64;
                val index = (sib div 8) mod 8;
                val base   = sib mod 8;
            in
                if md = 1 then print8 ()
                else if md = 2 orelse base = 5 (* andalso md=0 *) 
                then print32 ()
                else ();
          
                print "(";
        
                if md <> 0 orelse base <> 5
                then print (regRepr (mkReg (Word8.fromInt base, rexB)))
                else ();
        
                if index <> 4 (* No index. *)
                then 
                    print ("," ^ regRepr (mkReg(Word8.fromInt index, rexX)) ^ 
                        (if ss = 0 then ",1"
                        else if ss = 1 then ",2"
                        else if ss = 2 then ",4" (* N.B. *not* 3 - bugfix 29/3/95 *)
                        else ",8"))
                else ();
        
                print ")"
            end
      
            else (* no s-i-b. *) if md = 0 andalso rm = 5
            then (* Absolute address. *)
                (print "("; print32 (); print ")")
            else (* register plus offset. *)
            (
                if md = 1 then print8 ()
                else if md = 2 then print32 ()
                else ();
         
                print ("(" ^ regRepr (mkReg(Word8.fromInt rm, rexB)) ^ ")")
            )
        end;
 
        fun printArith opc =
            print
               (case opc of
                  0 => "add"
                | 1 => "or"
                | 2 => "adc"
                | 3 => "sbb"
                | 4 => "and"
                | 5 => "sub"
                | 6 => "xor"
                | _ => "cmp"
               );
    in

        if procName = "" (* No name *) then print "?" else print procName;
        print ":\n";
 
        while !ptr < endcode do
        let
            val () = printHex (Word.toInt(!ptr)) (* The address. *)
            val () = print "\t"

            (* See if we have a lock prefix. *)
            val () =
                if get8u (!ptr, seg) = 0wxF0
                then (print "lock "; ptr := !ptr + 0w1)
                else ()

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
            val rexX = (rex andb8 0wx2) <> 0w0
            val rexB = (rex andb8 0wx1) <> 0w0

            val opByte = get8u (!ptr, seg);
            val () = ptr +:= 0w1;
        in
            if opByte = opToInt Group1_8_A orelse 
                opByte = opToInt Group1_32_A orelse
                opByte = opToInt Group1_8_a
            then
            let
                (* Opcode is determined by next byte. *)
                val nb = Word8.toInt (csegGet (seg, !ptr));
            in
                printArith ((nb div 8) mod 8);
                if opByte = opToInt Group1_8_a
                then print "b" else print "l";
                print "_rev\t";
                printEA rex; (* These are the wrong way round for gas. *)
                print ",";
                if opByte = opToInt Group1_32_A
                then print32 () else print8 ()
            end
         
            else if opByte = opToInt (CondJump JE)
            then (print "je  \t"; printJmp())

            else if opByte = opToInt (CondJump JNE)
            then (print "jne  \t"; printJmp())

            else if opByte = opToInt (CondJump JO)
            then (print "jo  \t"; printJmp())

            else if opByte = opToInt (CondJump JNO)
            then (print "jno  \t"; printJmp())

            else if opByte = opToInt (CondJump JL)
            then (print "jl  \t"; printJmp())

            else if opByte = opToInt (CondJump JG)
            then (print "jg  \t"; printJmp())

            else if opByte = opToInt (CondJump JLE)
            then (print "jle \t"; printJmp())

            else if opByte = opToInt (CondJump JGE)
            then (print "jge \t"; printJmp())

            else if opByte = opToInt (CondJump JB)
            then (print "jb  \t"; printJmp())

            else if opByte = opToInt (CondJump JA)
            then (print "ja  \t"; printJmp())

            else if opByte = opToInt (CondJump JNA)
            then (print "jna \t"; printJmp())

            else if opByte = opToInt (CondJump JNB)
            then (print "jnb \t"; printJmp())

            else if opByte = opToInt JMP_8
            then (print "jmp \t"; printJmp())

            else if opByte = opToInt JMP_32
            then
            let
                val valu     = get32s (!ptr, seg);
                val () = (ptr +:= 0w4);
            in
                print "jmp\t";
                printHex (Word.toInt(!ptr) + valu)
            end
         
            else if opByte = opToInt CALL_32
            then
            let
                val valu     = get32s (!ptr, seg);
                val () = (ptr +:= 0w4);
            in
                print "call\t";
                printHex (Word.toInt(!ptr) + valu)
            end
         
            else if opByte = opToInt MOVL_A_R
            then
            let
                (* Register is in next byte. *)
                val nb = Word8.toInt (csegGet (seg, !ptr));
                val reg = (nb div 8) mod 8;
            in
                print "movl\t";
                printEA rex;
                print ",";
                print (regRepr (mkReg(Word8.fromInt reg, rexR)))
            end
         
            else if opByte mod 0w8 = 0w3 andalso opByte < 0wx3f
            then
            let
                (* Register is in next byte. *)
                val nb = Word8.toInt (csegGet (seg, !ptr));
                val reg = (nb div 8) mod 8;
            in
                printArith(Word8.toInt((opByte div 0w8) mod 0w8));
                print "\t";
                printEA rex;
                print ",";
                print (regRepr (mkReg(Word8.fromInt reg, rexR)))
            end

            else if opByte = opToInt MOVL_R_A
            then
            let
                (* Register is in next byte. *)
                val nb = Word8.toInt (csegGet (seg, !ptr));
                val reg = (nb div 8) mod 8;
            in
                print "movl\t";
                print (regRepr (mkReg(Word8.fromInt reg, rexR)));
                print ",";
                printEA rex
            end

            else if opByte = opToInt XCHNG
            then
            let
                (* Register is in next byte. *)
                val nb = Word8.toInt (csegGet (seg, !ptr));
                val reg = (nb div 8) mod 8;
            in
                print "xchngl\t";
                print (regRepr (mkReg(Word8.fromInt reg, rexR)));
                print ",";
                printEA rex
            end

            else if opByte = opToInt MOVB_R_A
            then
            let
                (* Register is in next byte. *)
                val nb = Word8.toInt (csegGet (seg, !ptr));
                val reg = (nb div 8) mod 8;
            in
                print "movb\t";
                if rexX
                then print ("r" ^ Int.toString(reg+8) ^ "B")
                else case reg of
                    0 => print "%al"
                |   1 => print "%cl"
                |   2 => print "%dl"
                |   3 => print "%bl"
                     (* If there is a REX byte these select the low byte of the registers. *)
                | 4 => print (if rex = 0w0 then "%ah" else "%sil")
                | 5 => print (if rex = 0w0 then "%ch" else "%dil")
                | 6 => print (if rex = 0w0 then "%dh" else "%bpl")
                | 7 => print (if rex = 0w0 then "%bh" else "%spl")
                |   _ => print ("r" ^ Int.toString reg);
                print ",";
                printEA rex
            end


            else if opByte >= opToInt (PUSH_R 0w0) andalso
                    opByte <= opToInt (PUSH_R 0w7)
            then print ("pushl\t" ^  regRepr (mkReg (opByte mod 0w8, rexB)))
      
            else if opByte >= opToInt (POP_R 0w0) andalso
                    opByte <= opToInt (POP_R 0w7)
            then print ("pop\t" ^ regRepr (mkReg (opByte mod 0w8, rexB)))
      
            else if opByte = opToInt NOP
            then print "nop"
      
            else if opByte = opToInt LEAL
            then
            let
                (* Register is in next byte. *)
                val nb = Word8.toInt (csegGet (seg, !ptr));
                val reg = (nb div 8) mod 8;
            in
                print "leal\t";
                printEA rex;
                print ",";
                print (regRepr (mkReg(Word8.fromInt reg, rexR)))
            end

            else if opByte >= opToInt (MOVL_32_64_R(#1(getReg eax))) andalso
                  opByte <= opToInt (MOVL_32_64_R(#1(getReg edi)))
            then
            (
                print "movl\t";
                if rexW then print64 () else print32 ();
                print("," ^ regRepr (mkReg (opByte mod 0w8, rexB)))
            )

            else if opByte = opToInt MOVL_32_A
            then
            (
                print "movl_rev\t";
                printEA rex; (* These are the wrong way round. *)
                print ",";
                print32 ()
            )
         
            else if opByte = opToInt MOVB_8_A
            then
            (
                print "movb_rev\t";
                printEA rex; (* These are the wrong way round. *)
                print ",";
                print8 ()
            )
         
            else if opByte = opToInt PUSH_32
            then (print "push\t"; print32 ())
         
            else if opByte = opToInt PUSH_8
            then (print "push\t"; print8 ())
         
            else if opByte = opToInt Group5
            then
            let
                (* Opcode is determined by next byte. *)
                val nb = Word8.toInt (csegGet (seg, !ptr));
                val opc = (nb div 8) mod 8;
            in
                print
                  (case opc of
                     2 => "call"
                   | 4 => "jmp "
                   | 6 => "push"
                   | _ => "???"
                  );
                print "\t";
                printEA rex
            end
         
            else if opByte = opToInt Group3_A
            then
            let
                (* Opcode is determined by next byte. *)
                val nb = Word8.toInt (csegGet (seg, !ptr));
                val opc = (nb div 8) mod 8;
            in
                print
                  (case opc of
                     0 => "testl"
                   | 3 => "negl"
                   | 4 => "mull"
                   | 5 => "imull"
                   | 6 => "divl"
                   | 7 => "idivl"
                   | _ => "???"
                  );
                print "\t";
                printEA rex;
                if opc = 0 then (print ","; print32 ()) else ()
            end
         
            else if opByte = opToInt Group3_a
            then
            let
                (* Opcode is determined by next byte. *)
                val nb = Word8.toInt (csegGet (seg, !ptr));
                val opc = (nb div 8) mod 8;
            in
                print
                  (case opc of
                     0 => "testb"
                   | 3 => "negb"
                   | _ => "???"
                  );
                print "\t";
                printEA rex;
                if opc = 0 then (print ","; print8 ()) else ()
            end
         
            else if opByte = opToInt Group2_1_A orelse opByte = opToInt Group2_CL_A
                    orelse opByte = opToInt Group2_8_A
            then
            let
                (* Opcode is determined by next byte. *)
                val nb = Word8.toInt (csegGet (seg, !ptr));
                val opc = (nb div 8) mod 8;
            in
                print
                   (case opc of
                      4 => "shl "
                    | 5 => "shr "
                    | 7 => "sar "
                    | _ => "???"
                   );
                print "\t";
                printEA rex;
                print ",";
                (* This is the reverse order from gas which has the shift first. *)
                if opByte = opToInt Group2_1_A then print "1"
                else if opByte = opToInt Group2_CL_A then print "cl"
                else print8 ()
            end
      
            else if opByte = opToInt ESCAPE
            then
            let
                (* Opcode is in next byte. *)
                val opByte2  = Word8.toInt (csegGet (seg, !ptr));
                val () = (ptr +:= 0w1);
            in
                if opByte2 = 0xB6 orelse opByte2 = 0xC1
                then
                let
                    val nb = Word8.toInt (csegGet (seg, !ptr));
                    val reg = (nb div 8) mod 8;
                in
                    print (if opByte2 = 0xB6 then "movzl\t" else "xaddl\t");
                    printEA rex;
                    print ",";
                    print (regRepr (mkReg(Word8.fromInt reg, rexR)))
                end
       
                else if opByte2 >= 0x80 andalso opByte2 <= 0x8f
                then
                let
                    val valu = get32s (!ptr, seg);
                    val () = (ptr +:= 0w4);
                in
                    print(
                        case opByte2 of
                            0x80 => "jo\t"
                        |   0x84 => "je\t"
                        |   0x85 => "jne\t"
                        |   0x8c => "jl\t"
                        |   0x8d => "jge\t"
                        |   0x8e => "jle\t"
                        |   0x8f => "jg\t" 
                        |   0x82 => "jb\t"
                        |   0x83 => "jnb\t"
                        |   0x86 => "jna\t"
                        |   0x87 => "ja\t" 
                        |   _ => "???\t"
                        );
                    printHex (Word.toInt(!ptr) + valu)
                end
       
                else (print "esc\t"; printHex opByte2)
            end (* ESCAPE *)
         
            else if opByte = opToInt POP_A
            then (print "pop\t"; printEA rex)
         
            else if opByte = opToInt RET 
            then print "ret"
      
            else if opByte = opToInt STC
            then print "stc"
         
            else if opByte = opToInt RET_16
            then (print "ret\t"; print16 ())

            else if opByte = opToInt TEST_ACC8
            then (print "testb\t%al,"; print8 ())
            
            else if opByte >= opToInt (FPESC 0w0) andalso opByte <= opToInt (FPESC 0w7)
            then (* Floating point escapes *)
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
                    |   _ => (printHex(Word8.toInt opByte); printHex(Word8.toInt opByte2));
                    ptr +:= 0w1
                )
                else (* mod = 00, 01, 10 *)
                (
                    case (escNo, nnn) of
                        (0w3, 0w0) => print "fildl\t"
                    |   (0w7, 0w5) => print "fildq\t"
                    |   (0w4, 0w0) => print "fadd\t"
                    |   (0w4, 0w1) => print "fmul\t"
                    |   (0w4, 0w3) => print "fcomp\t"
                    |   (0w4, 0w4) => print "fsub\t"
                    |   (0w4, 0w5) => print "fsubr\t"
                    |   (0w4, 0w6) => print "fdiv\t"
                    |   (0w4, 0w7) => print "fdivr\t"
                    |   (0w5, 0w0) => print "fld \t"
                    |   (0w5, 0w2) => print "fst\t"
                    |   (0w5, 0w3) => print "fstp\t"
                    |   _ => (printHex(Word8.toInt opByte); printHex(Word8.toInt opByte2));
                    printEA rex
                )
            end
            
            else if opByte = opToInt SAHF
            then print "sahf\n"

            else if opByte = opToInt REP
            then print "rep\t"

            else if opByte = 0wxA4
            then print "movsb\n"

            else if opByte = 0wxA5
            then print "movsl\n"

            else if opByte = 0wxA6
            then print "cmpsb\n"

            else if opByte = 0wxAA
            then print "stosb\n"

            else if opByte = 0wxAB
            then print "stosl\n"

            else printHex(Word8.toInt opByte);
      
            print "\n"
        end; (* end of while loop *)

        print "\n"

    end (* printCode *);

    (* Adds the constants onto the code, and copies the code into a new segment *)
    fun createCodeSegment (operations, registerSet, cvec) : address =
    let
        val () = codeGenerate(operations, cvec)

        (* After code generation get the final values of some refs. *)
        val Code{codeVec, noClosure,
                 numOfConsts, ic, constVec = ref constVec, nonInlineConsts = ref constsInConstArea,
                 resultSeg, procName, printAssemblyCode, printStream, profileObject, ...} = cvec
    
        (* This aligns ic onto a fullword boundary. *)
        val ()   = while Word.toInt (!ic) mod wordSize <> 0 do genop(NOP, NONE, cvec)
        val endic = !ic (* Remember end *)
        val ()   = genWordU(0w0, cvec) (* Marker - 0 (changes !ic) *)
        (* Byte offset of start of code. (changes !ic) *)
        val () = genWordU(Word.toLargeWord(!ic), cvec)
        
        (* Copy the non-address constants.  These are currently only used for real
           constants in 64-bit mode.  Other constants are left until we have a
           valid code object. *)
        local
            fun putNonAddrConst{const = WVal c, addrs, posn=NonAddrArea, ...} =
                let
                    val addrOfConst = ! ic
                    val cAsAddr = toAddress c
                    (* For the moment this should always be a real number contained in
                       a byte segment.  If this changes in the future we may need to
                       align this back onto a 4/8-byte boundary. *)
                    val cLength = length cAsAddr * Word.fromInt wordSize
                    val _ = (cLength = 0w8 andalso flags cAsAddr = F_bytes) orelse
                                raise InternalError "putNonAddrConst: Not a real number"
                    fun doCopy n =
                        if n = cLength then ()
                        else (gen8u(loadByte(cAsAddr, n), cvec); doCopy(n+0w1))
                    val () = doCopy 0w0
                in
                    set32s(Word.toInt(addrOfConst - addrs - 0w4), addrs, codeVec)
                end
            |   putNonAddrConst _ = ()
        in
            val () = List.app putNonAddrConst constVec
        end

        (* +4 for code size, function name, register mask and profile object. *)
        val segSize = !ic div Word.fromInt wordSize + Word.fromInt constsInConstArea + 0w4

        (* Now make the byte segment that we'll turn into the code segment *)
        val seg : cseg = csegMake segSize
    
        val _ = resultSeg := Set seg;
    
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
                val () = setWordU(3 + constsInConstArea, addr, seg)
            end;

             (* Now we've filled in all the C integers; now we need to convert the segment
               into a proper code segment before it's safe to put in any ML values. *)
            val () = csegConvertToCode seg
            val () = csegPutWord (seg, endOfCode, toMachineWord procName)
            val () = csegPutWord (seg, endOfCode + 0w1, toMachineWord(encodeRegSet registerSet))
            (* Next the profile object. *)
            val () = csegPutWord (seg, endOfCode + 0w2, profileObject)
        end
    in 
        let

            (* constLabels - fill in a constant in the code. *)
            fun constLabels (Code{resultSeg=ref rseg, ...}, addr, value, InlineAbsolute) =
                    csegPutConstant (scSet rseg, addr, value, false)

            |   constLabels (Code{resultSeg=ref rseg, ...}, addr, value, InlineRelative) =
                    csegPutConstant (scSet rseg, addr, value, true)

            |   constLabels (_, _, _, NonAddrArea) = () (* Already done. *)

            |   constLabels (Code{resultSeg=ref rseg, ic = ref endByte, ...},
                           constAddr, value, ConstArea nonInlineCount) =
                    (* Not inline.  Put the constant in the constant area and set the original address
                        to be the relative offset to the constant itself. *)
                    let
                        val addrOfConst = endByte addrPlus (nonInlineCount-1 + 2+1) * wordSize
                        val seg       = scSet rseg (* The address of the segment. *)
                    in
                        csegPutConstant (seg, addrOfConst, value, false);
                        set32s(Word.toInt(addrOfConst - constAddr - 0w4), constAddr, seg)
                    end

            (* and then copy the objects from the constant list. *)
            fun putConst {const = WVal c, addrs, posn, ...} =
                ( (* Can put these in now. *)
                    constLabels (cvec, addrs, c, posn);
                    numOfConsts := ! numOfConsts - 0w1
                )

            |   putConst {const = HVal(ref hv), addrs, posn, ...} =
                let
                    val handlerByteOffset = hv
                    (* The following comment applies to offsetAddr *)
                    (* Special function to add to an address.
                       This only works if the resulting value is 
                       in a code segment and is on a word + 2 byte boundary. *)
                    val handlerAddr : handler = offsetAddr (csegAddr seg, handlerByteOffset);
                in
                    constLabels (cvec, addrs, toMachineWord handlerAddr, posn);
                    numOfConsts := ! numOfConsts - 0w1
                end

                (* forward-reference - fix up later when we compile
                    the referenced code *) 
            |   putConst {const = CVal _, ...} = ()

            val () = List.app putConst constVec
    
            (* Switch off "mutable" bit now if we have no
               forward or recursive references to fix-up *)
            val _ = if ! numOfConsts = 0w0 then csegLock seg else ();
  
            (* Do we need to make a closure, or just return the code? *)
            val addr : address =
                if noClosure
                then csegAddr seg
                else
                let
                    val addr : address = alloc (0w1, F_words, toMachineWord (csegAddr seg))
                    (* Logically unnecessary; however the RTS currently allocates everything
                       as mutable because Dave's code assumed that things were done this
                       way and I'm not completely sure that everything that needs a mutable
                       allocation actually asks for it yet.  *)
                    val () = lock addr
                in
                    addr
                end
  
            (* Now we know the address of this object we can fix up
               any forward references outstanding. This is put in here
               because there may be directly recursive references. *)
            local
                val Code{completionHooks=ref hooks, ...} = cvec
            in
                val () = List.app(fn f => f(cvec, toMachineWord addr)) hooks
            end

            val () = 
                if printAssemblyCode
                then (* print out the code *)
                (
                    printCode cvec seg endic;
                    printStream "Register set = ";
                    printStream(regSetRepr registerSet);
                    printStream "\n\n"
                )
            else ()
        in
            addr 
        end (* the result *)
    end (* copyCode *);

    fun codeAddress (cvec: code) : address option =
    (* This is used to find the register set for a function which was
       originally a forward reference.  If it has now been compiled we
       can get the code. *)
    case cvec of
        Code {resultSeg = ref (Set cseg), ...} => SOME(csegAddr cseg)
    |   Code {resultSeg = ref Unset, ...} =>
         (* We haven't compiled this yet: assume worst case. *) NONE

 
    structure Sharing =
    struct
        type code           = code
        and  reg            = reg
        and  addrs          = addrs
        and  operation      = operation
        and  regSet         = RegSet.regSet
        and  label          = label
        and  labList        = labList
    end

end (* struct *) (* CODECONS *);
