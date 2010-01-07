(*
    Copyright David C. J. Matthews 1989, 2000, 2009
    
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

functor I386CODECONS (

structure DEBUG :
sig
    val assemblyCodeTag : bool Universal.tag
    val getParameter :
       'a Universal.tag -> Universal.universal list -> 'a
end;

structure PRETTY: PRETTYSIG (* for compilerOutTag *)

) : CODECONSSIG =


(*****************************************************************************)
(*                  CODECONS functor body                                    *)
(*****************************************************************************)
struct
    open CodeSeg;
    open DEBUG;
    open PRETTY
    open Address;
    open Misc;

    infix 5 << <<+ <<- >> >>+ >>- ~>> ~>>+ ~>>- (* Shift operators *)
    infix 3 andb orb xorb andbL orbL xorbL andb8 orb8 xorb8
    
    val op << = Word.<< and op >> = Word.>>
    val op <<+ = LargeWord.<< and op >>+ = LargeWord.>>
    val op <<- = Word8.<< and op >>- = Word8.>>

    val op orb = Word.orb and op orbL = LargeWord.orb and op orb8 = Word8.orb
    val op andb8 = Word8.andb

    (*val op andb = Word.andb op andbL = LargeWord.andb *)

    val toInt = Word.toIntX (* This previously just cast the value so continue to treat it as signed. *)
 
    val exp2_3  =         0x8;
    val exp2_6  =        0x40;
    val exp2_7  =        0x80;
    val exp2_8  =       0x100;
    val exp2_16 =     0x10000;
    val exp2_24 =   0x1000000;
    val exp2_30 = 0x040000000;
    val exp2_31 = 0x080000000;
    val exp2_32 = 0x100000000;
    
    (* Let's check that we got them right! *)
    local
        fun exp2 0 = 1
        |   exp2 n = 2 * exp2 (n - 1);
    in
        val _ = 
          (
            exp2_3  = exp2 3  andalso
            exp2_6  = exp2 6  andalso
            exp2_7  = exp2 7  andalso
            exp2_8  = exp2 8  andalso
            exp2_16 = exp2 16 andalso
            exp2_24 = exp2 24 andalso
            exp2_30 = exp2 30 andalso
            exp2_31 = exp2 31 andalso
            exp2_32 = exp2 32
          )
             orelse raise InternalError "Powers of 2 incorrectly specified";
    end;
  
    (* tag a short constant *)
    fun tag c = 2 * c + 1;
  
    (* shift a short constant, but don't set tag bit *)
    fun semitag c = 2 * c;
  
    fun is8Bit n = ~ exp2_7 <= n andalso n < exp2_7;

    local
        val shift =
            if wordSize = 4
            then 0w2
            else raise InternalError "Invalid word size for x86_32"
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
    val addrLast = wordsToBytes(0w2 << 0w24) - 0w1 (* A big number. *)

    (* The "value" points at the jump instruction, or rather at the
       jump offset part of it.  It is a ref because we may have to change
       it if we have to put in a jump with a 32-bit offset. *)
    datatype jumpFrom =
        Jump8From  of addrs
    |   Jump32From of addrs 
     
    type labels = (jumpFrom ref) list;
    val noJump = []:labels; 
  
    (* This is the list of outstanding labels.  Use a separate type from
      "labels" for extra security. *)
    type labList = (jumpFrom ref) list;
  
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
   | CVal of code        (* a forward-reference to another function *)
   | HVal of addrs ref   (* a handler *)

  and code = Code of 
    { codeVec:        cseg,           (* This segment is used as a buffer. When the
                                         procedure has been code generated it is
                                         copied into a new segment of the correct size *)
      ic:             addrs ref,      (* Pointer to first free location in "codevec" *)
      constVec:       (const * addrs * bool) list ref, (* Constants used in the code *)
      numOfConsts:    word ref,        (* size of constVec *)
      stackReset:     word ref,        (* Distance to reset the stack before the next instr. *)
      pcOffset:       word ref,        (* Offset of code in final segment. *)
                                      (* This is used also to test for identity of code segments. *)
      labelList:      labList ref,    (* List of outstanding short branches. *)
      longestBranch:  addrs ref,      (* Address of the earliest 1-byte branch. *)
      procName:       string,         (* Name of the procedure. *)
      otherCodes:     code list ref,  (* Other code vectors with forward references to this vector. *)
      resultSeg:      setCodeseg ref, (* The segment as the final result. *)
      mustCheckStack: bool ref,       (* Set to true if stack check must be done. *)
      justComeFrom:   labels ref,     (* The label we have just jumped from. *)
      exited:         bool ref,       (* False if we can fall-through to here *)
      selfCalls:      addrs list ref, (* List of recursive calls to patch up. *)
      selfJumps:      labels ref,     (* List of recursive tail-calls to patch up. *)
      noClosure:      bool,           (* should we make a closure from this? *)
      branchCheck:    addrs ref,      (* the address we last fixed up to.  I added
                                         this to track down a bug and I've left it
                                         in for security.  DCJM 19/1/01. *)
      printAssemblyCode:bool,            (* Whether to print the code when we finish. *)
      printStream:    string->unit    (* The stream to use *)
    };

  (* procName is exported. *)
  fun procName       (Code {procName,...})       = procName;
  
  
  (* %ebp points to a structure that interfaces to the RTS.  These are
     offsets into that structure.  *)
    val MemRegLocalMPointer       = 0
    and MemRegHandlerRegister     = wordSize
    and MemRegLocalMbottom        = wordSize*2
    and MemRegStackLimit          = wordSize*3
    and MemRegHeapOverflowCall    = wordSize*8
    and MemRegStackOverflowCall   = wordSize*9
    and MemRegStackOverflowCallEx = wordSize*10
    and MemRegRaiseException      = wordSize*11
    and MemRegRaiseDiv            = wordSize*13
    and MemRegArbEmulation        = wordSize*14
    and MemRegThreadSelf          = wordSize*15

  (* Several operations are not generated immediately but recorded and
     generated later.  Labels (i.e. the destination of a branch) are recorded
     in just_come_from.  Adjustments to the real stack pointer are recorded
     in stack_reset.
     The order in which these "instructions" are assumed to happen is of
     course significant.  If just_come_from is not empty it is assumed to
     have happened before anything else. After that the stack pointer is 
     adjusted and finally the next instruction is executed.
  *)

  val codesize = 30; (* bytes. Initial size of segment. *)

  (* Test for identity of the code segments by testing whether
     the pcOffset ref is the same. N.B. NOT its contents. *)
  
  infix is;
  
  fun (Code{pcOffset=a, ...}) is (Code{pcOffset=b, ...}) = a=b;
  
    (* create and initialise a code segment *)
    fun codeCreate (noClosure : bool, name : string, parameters) : code =
    let
        val words = Word.fromInt codesize div Word.fromInt wordSize
        val printStream = PRETTY.getSimplePrinter parameters;
    in
    Code
      { 
        codeVec        = csegMake words, (* a byte array *)
        ic             = ref addrZero,
        constVec       = ref [],
        numOfConsts    = ref 0w0,
        stackReset     = ref 0w0, 
        pcOffset       = ref 0w0, (* only non-zero after code is copied *)
        labelList      = ref [],
        longestBranch  = ref addrLast, (* None so far *)
        procName       = name,
        otherCodes     = ref [],
        resultSeg      = ref Unset,   (* Not yet done *)
        mustCheckStack = ref false,
        justComeFrom   = ref [],
        exited         = ref false,
        selfCalls      = ref [],
        selfJumps      = ref [],
        noClosure      = noClosure,
        branchCheck    = ref addrZero,
        printAssemblyCode = DEBUG.getParameter DEBUG.assemblyCodeTag parameters,
        printStream    = printStream
      }
    end;
           

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
    end;

    (* Put 1 signed word at a given offset in the segment. *)
    fun set32s (ival: int, addr: addrs, seg) : unit =
        set32u(LargeWord.fromInt ival, addr, seg)

    (* Get 1 signed word from the given offset in the segment. *)
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
    end;
 
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
    let
        val icVal = !ic;
    in
        ic := icVal addrPlus 4;
        set32s (ival, icVal, codeVec)
    end

    datatype mode =
        Based0    (* mod = 0 *)
    |   Based8   (* mod = 1 *)
    |   Based32  (* mod = 2 *)
    |   Register (* mod = 3 *) ;


    (* Put together the three fields which make up the mod r/m byte. *)
    fun modrm (md : mode, rg: Word8.word, rm : Word8.word) : Word8.word =
    let
        val modField: Word8.word = 
          case md of 
              Based0   => 0w0
            | Based8   => 0w1
            | Based32  => 0w2
            | Register => 0w3
            ;
    in
        (modField <<- 0w6) orb8 (rg <<- 0w3) orb8 rm
    end;

    fun genmodrm (md : mode, rg: Word8.word, rm : Word8.word, cvec) : unit =
    gen8u (modrm (md, rg, rm), cvec);

  datatype scaleFactor =
    Scale1 (* s = 0 *)
  | Scale2 (* s = 1 *)
  | Scale4 (* s = 2 *)
  | Scale8 (* s = 3 *)
  ;

  (* Put together the three fields which make up the s-i-b byte. *)
  fun sib (s : scaleFactor, i: Word8.word, b : Word8.word) : Word8.word =
  let
    val sizeField =
      case s of
        Scale1 => 0w0 * Word8.fromInt exp2_6
      | Scale2 => 0w1 * Word8.fromInt exp2_6
      | Scale4 => 0w2 * Word8.fromInt exp2_6
      | Scale8 => 0w3 * Word8.fromInt exp2_6
      ;
   in
       sizeField + (i * Word8.fromInt exp2_3) + b
   end;

  fun gensib (s : scaleFactor, i: Word8.word, b : Word8.word, cvec : code) : unit =
    gen8u (sib (s, i, b), cvec);

  fun scSet (Set x) = x | scSet _ = raise InternalError "scSet";

  (* Add a constant to the list along with its address.  We mustn't put
     the constant directly in the code since at this stage the code is
     simply a byte segment and if we have a garbage collection the value
     won't be updated. *)
  fun addConstToVec (valu: const, isRel, cvec as Code{numOfConsts, constVec, ic, ...}): unit =
  (
    numOfConsts := ! numOfConsts + 0w1;
    constVec    := (valu, !ic, isRel) :: ! constVec;
    (* We must put a valid tagged integer in here because we might
       get a garbage collection after we have copied this code into
       the new code segment but before we've put in the real constant.
       If this is a relative branch we need to point this at itself.
       Until it is set to the relative offset of the destination it
       needs to contain an address within the code and this could
       be the last instruction. *)
    gen32s (if isRel then ~5 else tag 0, cvec)
  )


    datatype reg = Reg of Word8.word  (* registers. *)

    (* These are the real registers we have. *)
    val eax = Reg  0w0;  
    val ecx = Reg  0w1;  
    val edx = Reg  0w2;
    val ebx = Reg  0w3;  
    val esp = Reg  0w4;  
    val ebp = Reg  0w5;
    val esi = Reg  0w6;  
    val edi = Reg  0w7;
    
    (* Not real registers. *)
    val regNone    = NONE;  
    val regHandler = Reg 0w9;  

    val regResult   = eax; (* Result of function call goes in here. *)
    val regClosure  = edx; (* Addr. of closure for fn. call goes here. *)
    
    val regStackPtr = esp;
    val regReturn   = NONE;

    fun getReg (Reg r) = r;      (* reg.down *)
    fun mkReg   n      = Reg n;  (* reg.up   *)
  
    (* The number of general registers. Includes result, closure, code,
       return and arg regs but not stackptr, handler, stack limit
       or heap ptrs. *)
    val regs = 6 (* eax, ebx, ecx, edx, esi, edi *);

    (* N.B. The encoding of registers as integers here is used to
       encode the register modification sets.  It MUST match the
       encoding used for IO functions in the registerMaskVector in
       the RTS assembly code section. *)
    (* The nth register (counting from 0). *)
    fun regN i =
        if i < 0 orelse i >= regs then raise InternalError "Bad register number"
        else if i = 4 then esi
        else if i = 5 then edi
        else mkReg(Word8.fromInt i)
  
    (* The number of the register. *)
    fun nReg r =
      if r = esi then 4 else
      if r = edi then 5 else Word8.toInt(getReg r);

    fun regRepr r = 
      if r = eax then "%eax" else
      if r = ebx then "%ebx" else
      if r = ecx then "%ecx" else
      if r = edx then "%edx" else
      if r = esp then "%esp" else
      if r = ebp then "%ebp" else
      if r = esi then "%esi" else
      if r = edi then "%edi" else
         "r" ^ Word8.toString (getReg r);
    
    (* Arguments are passed in eax and ebx. *)
    val argRegs = 2;

    fun argReg i =
      if i = 0 then eax else
      if i = 1 then ebx else
        raise InternalError ("Not arg reg " ^ Int.toString i);

    structure RegSet =
    struct
        (* Implement a register set as a bit mask. *)
        type regSet = word
        fun singleton r = Word.<<(0w1, Word.fromInt(nReg r))
        val regSetUnion = Word.orb

        local
            fun addReg(acc, n) =
                if n = regs then acc else addReg(regSetUnion(acc, singleton(regN n)), n+1)
        in
            val allRegisters = addReg(0w0, 0)
        end
        
        val noRegisters = 0w0

        fun inSet(r, rs) = Word.andb(singleton r, rs) <> 0w0
        
        fun inverseSet s = Word.andb(allRegisters, Word.notb s)
        
        val listToSet = List.foldl (fn(r, rs) => Word.orb(singleton r, rs)) 0w0
        
        fun isAllRegs rs = rs = allRegisters
        
        local
            fun card n 0w0 = n
            |   card n rs = card (n+1) (Word.xorb(rs, Word.andb(rs, ~rs)))
        in
            val cardinality = card 0
        end
     end

    open RegSet

  datatype arithOp =
    ADD
  | OR
  | ADC
  | SBB
  | AND
  | SUB
  | XOR
  | CMP
 ;
  
  fun arithOpToWord ADD = 0w0: Word8.word
    | arithOpToWord OR  = 0w1
    | arithOpToWord ADC = 0w2
    | arithOpToWord SBB = 0w3
    | arithOpToWord AND = 0w4
    | arithOpToWord SUB = 0w5
    | arithOpToWord XOR = 0w6
    | arithOpToWord CMP = 0w7
 ;
  
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
  | PUSH_R of reg
  | POP_R  of reg
  | Group5
  | NOP
  | LEAL
  | MOVL_32_R of reg
  | MOVL_32_A
  | MOVB_8_A
  | ESCAPE
  | POP_A
  | RET
  | RET_16
  | JO
  | JE
  | JNE
  | JL
  | JGE
  | JLE
  | JG
  | JB
  | JNB
  | JNA
  | JA
  | Arith of arithOp * int
  | STC
  | Group3_A
  | Group3_a
  | Group2_8_A
  | Group2_CL_A
  | Group2_1_A
  | PUSH_8
  | PUSH_32
  | TEST_ACC8
  | LOCK
  | FPESC of Word8.word
  ;

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
    | PUSH_R reg    =>  0wx50 + getReg reg
    | POP_R  reg    =>  0wx58 + getReg reg
    | Group5        =>  0wxff
    | NOP           =>  0wx90
    | LEAL          =>  0wx8d
    | MOVL_32_R reg =>  0wxb8 + getReg reg (* opCode$up(0xb8 + r.down) *)
    | MOVL_32_A     =>  0wxc7
    | MOVB_8_A      =>  0wxc6
    | ESCAPE        =>  0wx0f
    | POP_A         =>  0wx8f
    | RET           => 0wxc3
    | RET_16        => 0wxc2
    | JO            =>  0wx70
    | JB            =>  0wx72
    | JNB           =>  0wx73
    | JE            =>  0wx74
    | JNE           =>  0wx75
    | JNA           =>  0wx76
    | JA            =>  0wx77
    | JL            =>  0wx7c
    | JGE           =>  0wx7d
    | JLE           =>  0wx7e
    | JG            =>  0wx7f
    | Arith (ao,dw) => arithOpToWord ao * 0w8 + Word8.fromInt dw
    | STC           => 0wxf9
    | Group3_A      => 0wxf7
    | Group3_a      => 0wxf6
    | Group2_8_A    => 0wxc1
    | Group2_1_A    => 0wxd1
    | Group2_CL_A   => 0wxd3
    | PUSH_8        => 0wx6a
    | PUSH_32       => 0wx68
    | TEST_ACC8     => 0wxa8
    |   LOCK        => 0wxF0
    |   FPESC n     => 0wxD8 orb8 n   
    ;

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


  (* Make a reference to another procedure. Usually this will be a forward reference but
     it may have been compiled already, in which case we can put the code address in now. *)
  fun codeConst (Code {resultSeg = ref(Set seg), ... }, isRel, into) =
    (* Already done. *) addConstToVec (WVal (toMachineWord(csegAddr seg)), isRel, into)

  | codeConst (r as Code {otherCodes, ... }, isRel, into) = (* forward *)
      (* Add the referring procedure onto the list of the procedure
         referred to if it is not already there. This makes sure that when
         the referring procedure is finished and its address is known the
         address will be plugged in to every procedure which needs it. *)
      let
        fun onList _ []      = false
          | onList x (c::cs) = (x is c) orelse onList x cs ;
          
        val codeList = ! otherCodes;
      in
        if onList into codeList then () else otherCodes := into :: codeList;
        addConstToVec (CVal r, isRel, into)
      end;

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
  fun reallyFixBranches ([] : labels) _ = ()
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


  (* Deal with a pending fix-up. *)
  fun reallyFixup (Code{justComeFrom=ref [], ... }) = ()
   |  reallyFixup (cvec as Code{justComeFrom=jcf as ref labs, exited, ... }) = 
       (exited := false; reallyFixBranches labs cvec; jcf := []);

    (* Adds the displacement to the stack pointer before an
     instruction is generated. *)
    fun resetSp (cvec as Code{stackReset, ...}) =
    let
        (* Any pending jumps must be taken first. *)
        val () = reallyFixup cvec;
      
        val sr = Word.toInt(wordsToBytes(!stackReset)) (* Offset in bytes. *)
    in
        stackReset := 0w0;
    
        if is8Bit sr
        then (* Can use one byte immediate *) 
        (
            gen8u(opToInt Group1_8_A (* group1, 8-bit immediate *), cvec);
            genmodrm(Register, arithOpToWord ADD, getReg esp, cvec);
            gen8s(sr, cvec)
        )
        else (* Need 32 bit immediate. *)
        (
           gen8u(opToInt Group1_32_A (* group1, 32-bit immediate *), cvec);
           genmodrm(Register, arithOpToWord ADD, getReg esp, cvec);
           gen32s(sr, cvec)
        )
    end (* resetSp *); 
        
  (* Do any pending instructions, but only fix up branches if there
     are instructions in the pipe-line. *)
  fun flushQueue (Code{stackReset = ref 0w0, ...}) = ()
   |  flushQueue cvec = resetSp cvec;


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
  fun fixup (labs:labels, cvec as Code{justComeFrom, exited, ic, branchCheck, ...}) =
  let
    (* If the jump we are fixing up is immediately preceding, 
       we can remove it.  It is particularly important to remove
       32 bit jumps to the next instruction because they would
       put a word of all zeros in the code, and that could be mistaken
       for a marker word. *)
    fun checkLabs []          = []
      | checkLabs ((lab as ref (Jump8From addr))::labs) =
            if !ic addrMinus addr = 1
            then
             (
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
        (* Any pending stack reset must be done now.
           That may involve fixing up pending jumps because
           so take effect before the stack adjustment. *)
        flushQueue cvec;
          
        (* Add together the jumps to here and remove redundant jumps. *)
        justComeFrom := doCheck (labs @ !justComeFrom)
      )
  end;


    fun checkBranchList
        (cvec as Code{longestBranch, justComeFrom,
                      exited, ic, stackReset, labelList, ...}, branched, size) =
    (* If the longest branch is close to going out of range it must
       be converted into a long form. *)
    (* If we have just made an unconditional branch then we make the 
       distance shorter. *)
    let
        val maxDiff = (if branched then 100 else 127 - 5) - size;

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
          val convertRest = convertLabels labs;
       in
         (* Now do this entry. *)
         case !lab of
           Jump32From _ => (* shouldn't happen? *)
             convertRest
           
         | Jump8From addr =>
            (* If we are about to fix this label up we don't need to extend it except that we
               must extend it if we are going to put in more branch extensions which will take
               it out of range. DCJM 9/4/01. *)
            if List.exists (fn a => a = lab) (! justComeFrom)
                andalso (jumpSpace = 0 orelse !ic addrMinus addr < 127 - jumpSpace)
            then lab :: convertRest
            else if !ic addrMinus addr > (100 - size)
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
        if !ic addrMinus ! longestBranch > maxDiff
        then
        let
            (* Must save the stack-reset, otherwise "fixup" will try
               to reset it. *)
            val sr = !stackReset;
            val _  = stackReset := 0w0;
         
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
            fixup (lab, cvec); (* Continue with normal processing. *)
            stackReset := sr (* Restore old value. *)
        end
        else  ()
   end;


  (* Do all the outstanding operations including fixing up the branches. *)
  fun doPending (cvec as Code{exited, stackReset=ref stackReset, ...}, size) : unit =
  let
    val mustReset = stackReset <> 0w0;
  in
    (* If we have not exited and there are branches coming in here
       then we fix them up before jumping round any branch extensions. *)
     if ! exited then () else reallyFixup cvec;
   
     checkBranchList(cvec, ! exited, if mustReset then size + 6 else size);
            
     (* Fix up any incoming branches, including a jump round any
        branch extensions. *)
     reallyFixup cvec;   
         
     flushQueue cvec
   end;

   (* Generate an opCode byte after doing any pending operations. *)
   fun genop (opb:opCode, cvec) =
     (
       doPending (cvec, 12);
       (* 12 is maximum size of an instruction.  It's also big
          enough for a comparison and the following conditional
          branch. *)
       gen8u (opToInt opb, cvec)
     );

  (* This has to be done quite carefully if we are to be able to back-up
     over jumps that point to the next instruction in fixup.  We have to
     guarantee that if we back up we haven't already set a jump to point
     beyond where we're backing up.  See below for more explanation.
     DCJM 19/1/01.*)
  fun putConditional (br: opCode, cvec as Code{ic, ...}) : jumpFrom ref =
    (
      flushQueue cvec; (* Do any stack adjustments. *)
      gen8u (opToInt br, cvec); (* Don't use genop. *)
      gen8u (0w0, cvec);
      makeShortLabel (!ic addrPlus ~1, cvec)
    );

  (* Generates an unconditional branch. *)
  fun unconditionalBranch (cvec as Code {justComeFrom, exited, ...}): labels =
  let
    (* If we have just jumped here we may be able to avoid generating a
        jump instruction. *)
    val () = flushQueue cvec; (* Do any pending instructions. *)
    val labs = ! justComeFrom;
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
        val br = putConditional (JMP_8, cvec);
    in
        exited := true;
        br :: labs
    end
    
  end; (* unconditionalBranch *)
    
  fun genSelfBranch (cvec as Code{justComeFrom, exited, ic, ... }) : labels =
  let
    (* Do any pending instructions. *)
    val () = flushQueue cvec;
    
    (* Can we get into the prelude with an 8-bit jump? *)
    (* Conservative estimation needs to allow for:
         (1) stack check (10 bytes)
         (2) 1 byte instruction + 1 byte offset (2 bytes)
         (3) possible programmer arithmetic error (6 bytes)
    *)      
    fun isNearPrelude addr = addr <= 0w110;

    (* Is the jump long enough to reach back into the prelude? *)
    fun isLongJump (Jump32From _ )  = true
      | isLongJump (Jump8From addr) = isNearPrelude (addr addrPlus ~1)
      
    val labs       = ! justComeFrom;
    val longJumps  = List.filter (fn r => isLongJump (!r)) labs;
    val shortJumps = List.filter (fn r => not (isLongJump (!r))) labs;

    (* remove the "long enough" 8-bit jumps from the
       list of pending jumps to extend. *)
    fun tidy [] = ()
      | tidy (ref (Jump32From _) :: t) = tidy t
      | tidy (ref (Jump8From a)  :: t) = (removeLabel (a, cvec); tidy t);
      
    val () = tidy longJumps;
    
    (* do we actually need to insert a jump into the codestream? *)
    val needsJump =
       case shortJumps of
         [] => not (! exited) 
        | _ => true;
  in
    if needsJump
    then let
      (* fix up pending short jumps to here *)
      val () = justComeFrom := shortJumps;
      val () = doPending (cvec, 5);
    
      (* Now decide whether we can use an 8-bit jump here. *)
      (* N.B. we use gen8u here, not genop, because the latter
         calls "doPending (cvec, 12)" which could change the
         results of our "isNearPrelude (!ic)" test. *)
      val br =
        if isNearPrelude (!ic)
        then
      (
        gen8u (opToInt JMP_8, cvec);
        gen8u (0w0, cvec);
        ref (Jump8From (!ic addrPlus ~1))
      )
        else
      (
        gen8u  (opToInt JMP_32, cvec);
        gen32u (0w0, cvec);
        ref (Jump32From (!ic addrPlus ~4))
      );
      
    in
      justComeFrom := [];
      exited := true;
      br :: longJumps
    end
    
    else
    (
      justComeFrom := [];
      exited := true;
      longJumps
    )
  end; (* genSelfBranch *)
    

    (* Adds in the reset. Does not actually generate code. *)
    fun resetStack (offset, Code{stackReset, ...}) : unit =
        stackReset := ! stackReset + offset;

  (* Generate an effective address. *)
  fun genEA (offset: int, rb: reg, r : reg, cvec) : unit =
  let
    val offsetCode =
      (* don't generate [ebp] (use [ebp+0]) 'cos it doesn't exist! *)
      if offset = 0 andalso (rb <> ebp) 
        then Based0  (* no disp field *)
      else if is8Bit offset
        then Based8  (* use 8-bit disp field *)
        else Based32; (* use 32-bit disp field *)
  in
    if rb = esp
    then (* Need to use s-i-b byte. *)
      (
        if offset < 0 then raise InternalError "Negative stack offset" else ();
        genmodrm (offsetCode, getReg r, 0w4 (* use SIB *), cvec);
        gensib   (Scale1, 0w4 (* no index *), getReg esp, cvec)
      )
    else genmodrm(offsetCode, getReg r, getReg rb, cvec);
     
    (* generate the disp field (if any) *)
    case offsetCode of
      Based8  => gen8s  (offset, cvec)
    | Based32 => gen32s (offset, cvec)
    | _       => ()
  end;

  (* Similar to genEA, but used when regfield is an extended
     opCode rather than a real register. *)
  fun genop2 (offset: int, rb: reg, opc: Word8.word, cvec) : unit =
  let
    val offsetCode =
      (* don't generate [ebp] (use [ebp+0]) 'cos it doesn't exist! *)
      if offset = 0 andalso (rb <> ebp) 
        then Based0  (* no disp field *)
      else if is8Bit offset
        then Based8  (* use 8-bit disp field *)
        else Based32; (* use 32-bit disp field *)
  in
    if rb = esp
    then (* Need to use s-i-b byte. *)
      (
        if offset < 0 then raise InternalError "Negative stack offset" else ();
        genmodrm (offsetCode, opc, 0w4 (* use s-i-b *), cvec);
        gensib (Scale1, 0w4 (* no index *), getReg esp, cvec)
      )
    else genmodrm(offsetCode, opc, getReg rb, cvec);
     
    (* generate the disp field (if any) *)
    case offsetCode of
      Based8  => gen8s  (offset, cvec)
    | Based32 => gen32s (offset, cvec)
    | _       => ()
  end;


  (* Similar to genEA, but used when there is an index register.
     rb may be NONE if no base register is required (used
     with leal to tag values). *)
  fun genIndexed (offset: int, rb: reg option, ri: reg, size : scaleFactor, rd: reg, cvec) =
  let
    val (offsetCode, basefield) =
        case rb of
            NONE => (Based0, 0w5 (* no base register *))
        |   SOME rb =>
            let
                val base =
                    if offset = 0 andalso (rb <> ebp)
                    then Based0    (* no disp field *)
                    else if is8Bit offset
                    then Based8   (* use 8-bit disp field *)
                    else Based32; (* use 32-bit disp field *)
            in
                (base, getReg rb)
            end
  in
    genmodrm (offsetCode, getReg rd, 0w4 (* s-i-b *), cvec);
    gensib   (size, getReg ri, basefield, cvec);
    
    (* generate the disp field (if any) *)
    case offsetCode of
      Based8  => gen8s  (offset, cvec)
    | Based32 => gen32s (offset, cvec)
    | _       => case rb of NONE =>  (* 32 bit absolute used as base *) gen32s (offset, cvec) | _ => ()
  end;

 (* Tag the value in register r *)
 fun genTag (r : reg, cvec) : unit =
 (
    (* old code (7 bytes) ...
       genop (LEAL, cvec);
       genIndexed (1, regNone, r, Scale2, r, cvec)
    ... *)

   (* new, more compact code (4 bytes) SPF 27/4/95 *)
   genop (LEAL, cvec);
   genIndexed (1, SOME r, r, Scale1, r, cvec)
 )       

    fun genImmed (opn: arithOp, rd: reg, imm: int, cvec) : unit =
    if is8Bit imm
    then (* Can use one byte immediate *) 
      (
       genop (Group1_8_A (* group1, 8 bit immediate *), cvec);
       genmodrm(Register, arithOpToWord opn, getReg rd, cvec);
       gen8s (imm, cvec)
      )
    else (* Need 32 bit immediate. *)
      (
       genop (Group1_32_A (* group1, 32 bit immediate *), cvec);
       genmodrm(Register, arithOpToWord opn, getReg rd, cvec);
       gen32s(imm, cvec)
      );

    fun genReg (opn: arithOp, rd: reg, rs: reg, cvec) : unit =
    (
      genop (Arith (opn, 3 (* r/m to reg *)), cvec);
      genmodrm(Register, getReg rd, getReg rs, cvec)
    );
      
    (* generate padding no-ops to align to n modulo 4 *)
    fun align (n:word, cvec as Code{ic, ...}) =
        case (n - (!ic)) mod 0w4 of
            0w1 => genop    (NOP, cvec)
        |   0w2 => genReg   (OR, eax, eax, cvec)
        |   0w3 => genImmed (OR, eax, 0, cvec)
        |   _ => ()

  (* Exported.  - movl offset(rb),rd. *)
  fun genLoad (offset: int, rb: reg, rd: reg, cvec) : unit =
    if rd = regHandler (* Not a real register. *)
    then 
      (
       (* pushl offset(rb); popl 4(ebp) *)
       (* This only happens when we are popping the handler so we
          could simply pop it straight. *)
       genop (Group5, cvec);
       genop2 (offset, rb, 0w6 (* push *), cvec);
       
       genop (POP_A, cvec);
       genop2 (MemRegHandlerRegister, ebp, 0w0, cvec)
      )
    else
      (
       genop (MOVL_A_R, cvec);
       genEA (offset, rb, rd, cvec)
      );

  (* Store an immediate value at a given address and offset. *)
  fun genStoreIW (cnstnt: machineWord, offset: int, rb: reg, ri: reg option,
                 cvec as Code{ic, ...}) =
    (
  (*  We have a little problem here: we have to be very careful that
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
     ought to consider it. *)
        if isShort cnstnt then () (* No problem. *)
        else if not (is8Bit offset)
        then
        (
            doPending(cvec, 12 + 2);
            align(case ri of NONE => 0w2 | SOME _ => 0w1, cvec)
        )
        else if offset = 0 andalso rb = eax andalso not(isSome ri)
        then (* modrm will be zero.  We need to be sure that this is not the
              first byte in a word. *)
        (
            doPending(cvec, 12 + 1);
            if (!ic) mod 0w4 = 0w3 (* opcode will be the last byte in this word. *)
            then align(0w1, cvec)
            else ()
        )
        else ();
        genop (MOVL_32_A, cvec);
        case ri of
            NONE => genop2 (offset, rb, 0w0, cvec)
        |   SOME ri => genIndexed (offset-2, SOME rb, ri, Scale2, mkReg 0w0, cvec);
        if isShort cnstnt
        then gen32s (Word.toIntX (toShort cnstnt) * 2 + 1, cvec)
        else addConstToVec (WVal cnstnt, false, cvec)
   )


    fun genStoreIB (cnstnt: machineWord, offset: int, rb: reg, ri: reg option, cvec) : unit =
    if not (isShort cnstnt)
    then (* This should never happen. *)
        raise InternalError "genStoreI: storing long constant as a byte"
    else
    let
        val v = toInt (toShort cnstnt)
    in
        case ri of
            NONE =>
            (
                genop (MOVB_8_A, cvec);
                genop2 (offset, rb, 0w0, cvec);
                gen8u (Word8.fromInt v, cvec)
            )

        |   SOME ri =>
            (
                (* Untag the index first. *)
                genop (Group2_1_A, cvec);
                genmodrm(Register, 0w5 (* shr *), getReg ri, cvec);

                genop (MOVB_8_A, cvec);
                genIndexed (offset, SOME rb, ri, Scale1, mkReg 0w0, cvec);
                gen8u (Word8.fromInt v, cvec);
          
                (* Retag the index. *)
                genTag (ri, cvec)
            )
   end


    (* Store a value on the stack.  This is used when the registers need to be
       saved, for more than 4 arguments or to push an exception handler. *)
    fun genPush (r:reg, cvec) : unit =
    if r = regHandler (* Not a real register. *)
    then
    (
        genop (Group5, cvec);
        genop2 (MemRegHandlerRegister, ebp, 0w6 (* push *), cvec)
    )
    else genop (PUSH_R r, cvec);


  (* Exported. Load a value and push it on the stack.  Used when all
     the allocatable registers have run out.
     Also used if preferLoadPush is true. *)
  fun genLoadPush (offset: int, rb: reg, cvec) : unit =
    (
      genop (Group5, cvec);
      genop2 (offset, rb, 0w6 (* push *), cvec)
    );

  val preferLoadPush : bool = true; (* It's cheap. *)

  (* Call the function.  Must ensure that the return address
     is on a word + 2 byte boundary. *)

  (* Call the function.  Must ensure that the return address
     is on a word + 2 byte boundary. *)
  fun genSelfCall (cvec as Code{ic, ...}) : addrs =
  (
    (* Make sure anything pending is done first. *)
    (* 15 comes from the maximum instruction size (12)
       used in genop, together with up to 3 nops. *)
    doPending (cvec, 15); 
    (* Ensure the return address is aligned on
       a word + 2 byte boundary.  *)
    align (0w1, cvec);
 
    genop (CALL_32, cvec);  (* 1 byte  *)
    gen32u (0w0, cvec);       (* 4 bytes *)
    
    if (!ic) mod 0w4 <> 0w2
    then raise InternalError "genSelfCall: call not aligned"
    else ();
    
    !ic addrPlus ~4
  );

  (* Register/register move. *)
  fun genMove (rd:reg, rs:reg, cvec) : unit =
    (
     genop (MOVL_R_A, cvec);
     genmodrm(Register, getReg rs, getReg rd, cvec)
    );

  (* Add a register to a constant. *)
  fun genLeal (rd:reg, rs:reg, offset:int, cvec) : unit =
   (
     genop (LEAL, cvec);
     genEA (offset, rs, rd, cvec)
   );


    (* Exported.  - movl rs,offset(rb) / movb rs,offset(rb) *)
    fun genStoreW (rs: reg, offset: int, rb: reg, ri: reg option, cvec) : unit =
        (
            genop (MOVL_R_A, cvec);
            case ri of
                NONE => genEA (offset, rb, rs, cvec)
            | SOME ri => genIndexed (offset-2, SOME rb, ri, Scale2, rs, cvec)
        )

    fun genStoreB (rs, offset: int, rb, ri: reg option, cvec) =
        let
          (* Only some of the 32 bit registers can be used to store from.
             Eax, Ebx, Ecx and Edx are fine and the code used for those
             registers corresponds to their low-order byte.  The other
             registers can't be used. *)
            val () = if getReg rs <= 0w3 then () else raise InternalError "genStoreB"
        in
            (* The value we store has to be untagged. *)
            genop (Group2_1_A, cvec);
            genmodrm(Register, 0w5 (* shr *), getReg rs, cvec);

            case ri of
                NONE => ()
            |   SOME ri =>
                ( (* Untag the index as well. *)
                    genop (Group2_1_A, cvec);
                    genmodrm(Register, 0w5 (* shr *), getReg ri, cvec)
                );

            genop (MOVB_R_A, cvec);
            
            case ri of
                NONE => genEA (offset, rb, rs, cvec)
            |   SOME ri => genIndexed (offset, SOME rb, ri, Scale1, rs, cvec);

            (* Restore the original value by retagging.
               This ensures we don't have a bad value around and also
               restores the original value since it may still be wanted. *)
            genTag(rs, cvec);

            case ri of
                NONE => ()
            |   SOME ri => (* retag the index as well. *) genTag (ri, cvec)
        end;

    (* Move an immediate value into a register. The immediate value may be
       any 32-bit number. *)
    fun genMoveI (rd:reg, immed:int, cvec) =
    (* There may be better ways e.g. xorl rd,rd; addl immed,rd. *)
    (* Need 32 bit immediate. *)
    (
        genop  (MOVL_32_R rd, cvec); (* TODO: Have different kinds of constants here.  If it's semitagged use xor/add. *)
        gen32s (immed, cvec)
    )

    type handlerLab = addrs ref;
  
  (* Exported. Loads the address of the destination of a branch. Used to
     put in the address of the exception handler.
     We used to have pushAddress in place of this which pushed the
     address at the same time.  On this architecture it can save an
     instruction but it's a problem on machines where we have to load
     the address into a register - we don't have a spare checked
     register available. *)
    fun loadHandlerAddress  (rd, lab, cvec) : unit =
    (
        genop  (MOVL_32_R rd, cvec);
        addConstToVec (HVal lab, false, cvec)
    )

  (* Exported *)
  fun fixupHandler (lab:handlerLab, cvec as Code{exited, ic, branchCheck, ...}) : unit =
  ( 
    (* Make sure anything pending is done first. *)
    (* 15 comes from maximum instruction size + up to 3 nops. *)
    doPending (cvec, 15); 
    
    (* Ensure the return address is aligned onto a word + 2 byte
       boundary. *)
    align (0w2, cvec);
    
    exited := false;
    branchCheck := !ic;
    lab := !ic
  );

  datatype callKinds =
        Recursive           (* The function calls itself. *)
    |   ConstantFun of machineWord * bool (* A pre-compiled or io function. *)
    |   CodeFun of code     (* A static link call. *)
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
  fun callFunction (callKind,
        cvec as Code {selfCalls, mustCheckStack, ic, ... }) : unit =
     (* If we ever call a function we must do a stack check. *)
    (
      mustCheckStack := true;
      
      case callKind of 
       Recursive =>
       let
         val lab : addrs = genSelfCall cvec;
       in
         selfCalls := lab :: ! selfCalls
       end
     
     | FullCall => (* Indirect call through closure reg. *)
          (
            (* Make sure anything pending is done first. *)
            (* 15 comes from the maximum instruction size (12) used in genop,
               together with up to 3 nops. *)
            doPending (cvec, 15); 
            (* Ensure the return address is aligned on
               a word + 2 byte boundary.  *)
            align (0w0, cvec);
         
            genop (Group5, cvec);
            genmodrm(Based0, 0w2 (* call *), getReg regClosure, cvec)
          )

     | CodeFun c =>
          (
            (* Make sure anything pending is done first. *)
            doPending (cvec, 15); 
            (* Ensure the return address is aligned on
               a word + 2 byte boundary.  *)
            align (0w1, cvec);
         
            genop (CALL_32, cvec);
            codeConst (c, true, cvec)
          )

     | ConstantFun(w, false) =>
          (
            (* Make sure anything pending is done first. *)
            doPending (cvec, 15); 
            (* Ensure the return address is aligned on
               a word + 2 byte boundary.  *)
            align (0w1, cvec);
         
            genop (CALL_32, cvec);
            addConstToVec (WVal w, true, cvec)
          )

     | ConstantFun(w, true) =>
          (
            (* Make sure anything pending is done first. *)
            doPending (cvec, 15); 
            (* Ensure the return address is aligned on
               a word + 2 byte boundary.  *)
            align (0w0, cvec);
         
            genop (Group5, cvec);
            genmodrm(Based0, 0w2 (* call *), 0w5 (* Immediate address. *), cvec);
            addConstToVec (WVal w, false, cvec)
          );

        if (!ic) mod 0w4 <> 0w2
        then raise InternalError "callFunction: call not aligned"
        else ()
    );
     

  (* Exported. Tail recursive jump to a function.
     N.B.  stack checking is used both to ensure that the stack does
     not overflow and also as a way for the RTS to interrupt the code
     at a safe place.  The RTS can set the stack limit "register" at any
     time but the code will only take a trap when it next checks the
     stack.  The only way to break out of infinite loops is for the
     user to type control-C and some time later for the code to do a
     stack check.  We need to make sure that we check the stack in any
     function that my be recursive, directly or indirectly.
  *)
    fun jumpToFunction (callKind, returnReg,
                      cvec as Code{selfJumps, exited, mustCheckStack, ...}) =
    ( (* Must push the return register? *)
        case returnReg of
            NONE => ()
        |   SOME returnReg => genop (PUSH_R returnReg, cvec);
            
        case callKind of 
            Recursive =>
            let
                val () = mustCheckStack := true;
                val lab = genSelfBranch cvec;
            in
                selfJumps := lab @ ! selfJumps
            end
           
        |   FullCall =>
            ( (* Full closure call *)
                mustCheckStack := true;
                genop (Group5, cvec);
                genmodrm(Based0, 0w4 (* jmp *), getReg regClosure, cvec)
            )
      
        |   CodeFun c =>
            (
                mustCheckStack := true; (* May be recursive. *)
                genop (JMP_32, cvec);
                codeConst (c, true, cvec)
            )

        |   ConstantFun(w, false) =>
            (      
                mustCheckStack := true; (* May be recursive. *)
                genop (JMP_32, cvec);
                addConstToVec (WVal w, true, cvec)
            )

        |   ConstantFun(w, true) =>
            (
                (* Indirect jumps are used to call into the RTS.  No need
                   to check the stack. *)
                genop (Group5, cvec);
                genmodrm(Based0, 0w4 (* jmp *), 0w5 (* Immediate address. *), cvec);
                addConstToVec (WVal w, false, cvec)
            );

        exited := true (* We're not coming back. *)
    );


    (* Exported. Return and remove args. *)
    fun returnFromFunction (NONE, args, cvec as Code{exited, ...}) : unit =
    (
        if args = 0
        then genop (RET, cvec)
        else
        let
            val offset = args * 4
        in
            genop (RET_16, cvec);
            gen8u (Word8.fromInt(offset mod exp2_8), cvec);
            gen8s (offset div exp2_8, cvec)
        end;
     
        exited := true (* We're not coming back. *)
    )
    |   returnFromFunction _ = raise InternalError "Wrong argument"


    (* Exported. The exception argument has already been loaded into eax *)
    (* Call, rather than jump to, the exception code so that we have
       the address of the caller if we need to produce an exception
       trace. *)
   fun raiseException cvec =
      (
       doPending (cvec, 15);
       (* Since we're calling we put the "return address" on a word+2 byte
          boundary.  This is never actually used as a return address but
          it's probably best to make sure it's properly aligned.  It probably
          simplifies exception tracing which is the reason it's there. *)
       align (0w3, cvec);
       genop(Group5, cvec);
       genmodrm (Based8, 0w2 (* call *), getReg ebp, cvec);
       gen8u (Word8.fromInt MemRegRaiseException, cvec)
      )

    (* Only used for while-loops. *)
    fun jumpback (lab, stackCheck, cvec as Code{exited, ic, ...}) : unit =
    (
      (* Put in a stack check. This is used to allow the code to be interrupted. *)
      if stackCheck
      then
        (
          (* cmp reg,16(%ebp)*)
          genop(Arith (CMP, 3), cvec);
          genmodrm (Based8, getReg esp, getReg ebp, cvec);
          gen8u (Word8.fromInt MemRegStackLimit, cvec);
          (* jnb 3 *)
          let
            val lab = [putConditional (JNB, cvec)]
          in
              (* call *)
              genop(Group5, cvec);
              genmodrm (Based8, 0w2 (* call *), getReg ebp, cvec);
              gen8u (Word8.fromInt MemRegStackOverflowCall, cvec);
              fixup (lab, cvec)
          end

        )
      else ();
    
     (* Do any pending instructions before calculating the offset, just
        in case we put in some instructions first. *)
      doPending (cvec, 12);
    
      let
        val offset  = lab addrMinus (!ic); (* Negative *)
        val offset2 = offset - 2;
      in
        if is8Bit offset2
        then
          (
            genop (JMP_8, cvec);
            gen8s (offset2, cvec)
          )
        else
          (
            genop  (JMP_32, cvec);
            gen32s (offset - 5, cvec)
          )
      end;
      
      exited := true
    );

    (* Allocate store and put the resulting pointer in the result register. *)
    fun allocStore (size, flag, resultReg, cvec) : unit =
    let
        val bytes = (size + 1) * 4;
        val lengthWord = LargeWord.fromInt size orbL (Word8.toLargeWord flag <<+ 0w24); (* Size + mutable. *)
    in
        (* movl 0(%ebp),r; subl (size+1)*4,r; cmpl r,8(%ebp); jnb 1f;
           call 40[%ebp]; 1f: movl r,0(%ebp); movl size,-4(r); *)
        genLoad (MemRegLocalMPointer, ebp, resultReg, cvec);
        genLeal (resultReg, resultReg, ~ bytes, cvec);
    
        genop (Arith (CMP, 3 (* r/m to reg *)), cvec);
        genmodrm(Based8, getReg resultReg, getReg ebp, cvec);
        gen8s (MemRegLocalMbottom, cvec);

        let
           val lab = [putConditional (JNB, cvec)]
        in
            (* If we don't have enough store for this allocation we call this
               function. *)
            genop (Group5, cvec);
            genmodrm(Based8, 0w2 (* call *), getReg ebp, cvec);
            gen8s (MemRegHeapOverflowCall, cvec);
            fixup (lab, cvec)
        end;
    
        genStoreW (resultReg, MemRegLocalMPointer, ebp, regNone, cvec);
    
        (* Set the size field of a newly allocated piece of store. *)
        genop (MOVL_32_A, cvec);
        genmodrm(Based8, 0w0, getReg resultReg, cvec);
        gen8s (~4, cvec);
        gen32u (lengthWord, cvec)
    end;

    (* Operations. *)
    datatype instrs =
        InstrAddA
    |   InstrAddAConst of short
    |   InstrSubA
    |   InstrSubAConst of short
    (*|   InstrRevSubAConst of short*)
    |   InstrMulA
    |   InstrMulAConst2 (* Only constant multiplication by 2 *)
    |   InstrAddW
    |   InstrAddWConst of short
    |   InstrSubW
    |   InstrSubWConst of short
    (*|   InstrRevSubWConst of short*)
    |   InstrMulW
    |   InstrDivW
    |   InstrModW
    |   InstrOrW
    |   InstrOrWConst of short
    |   InstrAndW
    |   InstrAndWConst of short
    |   InstrXorW
    |   InstrXorWConst of short
    |   InstrLoadW
    |   InstrLoadWConstOffset of short
    |   InstrLoadB
    |   InstrLoadBConstOffset of short
    |   InstrUpshiftW    (* logical shift left *)
    |   InstrUpshiftWConst of short
    |   InstrDownshiftW  (* logical shift right *)
    |   InstrDownshiftWConst of short
    |   InstrDownshiftArithW  (* arithmetic shift right *)
    |   InstrDownshiftArithWConst of short
    |   InstrSetStringLength
    |   InstrVeclen
    |   InstrVecflags
    |   InstrGetFirstLong
    |   InstrStringLength
    |   InstrThreadSelf
    |   InstrAtomicAdd of int
    |   InstrStoreW of { offset: short option, toStore: machineWord option }
    |   InstrStoreB of { offset: short option, toStore: machineWord option }
    |   InstrLockSeg
    |   InstrUnaryFP of Word8.word (* Unary floating point operations *)
    |   InstrBinaryFP of Word8.word (* Binary floating point operations *)
    |   InstrIntToFP (* Float an integer. *)
    |   InstrUnimplemented

    (* Exported instructions. *)
    val instrAddA = InstrAddA
    and instrSubA = InstrSubA
    and instrMulA = InstrMulA
    and instrAddW = InstrAddW
    and instrSubW = InstrSubW
    and instrMulW = InstrMulW
    and instrDivW = InstrDivW
    and instrModW = InstrModW
    and instrOrW = InstrOrW
    and instrAndW = InstrAndW
    and instrXorW = InstrXorW
    and instrLoad = InstrLoadW
    and instrLoadB = InstrLoadB
    and instrUpshiftW = InstrUpshiftW    (* logical shift left *)
    and instrDownshiftW = InstrDownshiftW  (* logical shift right *)
    and instrDownshiftArithW = InstrDownshiftArithW  (* arithmetic shift right *)
    and instrSetStringLength = InstrSetStringLength
    and instrVeclen = InstrVeclen
    and instrVecflags = InstrVecflags
    and instrGetFirstLong = InstrGetFirstLong
    and instrStringLength = InstrStringLength
    and instrThreadSelf = InstrThreadSelf
    and instrAtomicIncr = InstrAtomicAdd 1
    and instrAtomicDecr = InstrAtomicAdd ~1
    and instrStoreW = InstrStoreW { offset=NONE, toStore=NONE }
    and instrStoreB = InstrStoreB { offset=NONE, toStore=NONE }
    and instrLockSeg = InstrLockSeg
    (* Floating point operations. *)
    and instrAddFP = InstrBinaryFP 0w0
    and instrSubFP = InstrBinaryFP 0w4
    and instrMulFP = InstrBinaryFP 0w1
    and instrDivFP = InstrBinaryFP 0w6
    and instrNegFP = InstrUnaryFP 0wxE0
    and instrIntToRealFP = InstrIntToFP
    and instrRealToIntFP = InstrUnimplemented
    and instrSqrtFP = InstrUnaryFP 0wxFA
    and instrSinFP = InstrUnaryFP 0wxFE
    and instrCosFP = InstrUnaryFP 0wxFF
    and instrAtanFP = InstrUnaryFP 0wxF3
    and instrExpFP = InstrUnimplemented 
    and instrLnFP  = InstrUnimplemented

    fun prettyInstr(instr, _) =
        case instr of
            InstrAddA                   => PrettyString "AddA"
        |   InstrAddAConst lit          => PrettyString ("AddAConst " ^ Word.toString lit)
        |   InstrSubA                   => PrettyString "SubA"
        |   InstrSubAConst lit          => PrettyString ("SubAConst " ^ Word.toString lit)
        |   InstrMulA                   => PrettyString "MulA"
        |   InstrMulAConst2             => PrettyString "MulAConst2"
        |   InstrAddW                   => PrettyString "AddW"
        |   InstrAddWConst lit          => PrettyString ("AddWConst " ^ Word.toString lit)
        |   InstrSubW                   => PrettyString "SubW"
        |   InstrSubWConst lit          => PrettyString ("SubWConst " ^ Word.toString lit)
        |   InstrMulW                   => PrettyString "MulW"
        |   InstrDivW                   => PrettyString "DivW"
        |   InstrModW                   => PrettyString "ModW"
        |   InstrOrW                    => PrettyString "OrW"
        |   InstrOrWConst lit           => PrettyString ("OrWConst " ^ Word.toString lit)
        |   InstrAndW                   => PrettyString "AndW"
        |   InstrAndWConst lit          => PrettyString ("AndWConst " ^ Word.toString lit)
        |   InstrXorW                   => PrettyString "XorW"
        |   InstrXorWConst lit          => PrettyString ("XorWConst " ^ Word.toString lit)
        |   InstrLoadW                  => PrettyString "LoadW"
        |   InstrLoadWConstOffset lit   => PrettyString ("LoadWConstOffset " ^ Word.toString lit)
        |   InstrLoadB                  => PrettyString "LoadB"
        |   InstrLoadBConstOffset lit   => PrettyString ("LoadBConstOffset " ^ Word.toString lit)
        |   InstrUpshiftW               => PrettyString "UpshiftW"
        |   InstrUpshiftWConst lit      => PrettyString ("UpshiftWConst " ^ Word.toString lit)
        |   InstrDownshiftW             => PrettyString "DownshiftW"
        |   InstrDownshiftWConst lit    => PrettyString ("DownshiftWConst " ^ Word.toString lit)
        |   InstrDownshiftArithW        => PrettyString "DownshiftArithW"
        |   InstrDownshiftArithWConst lit => PrettyString ("DownshiftArithWConst " ^ Word.toString lit)
        |   InstrSetStringLength        => PrettyString "SetStringLength"
        |   InstrVeclen                 => PrettyString "Veclen"
        |   InstrVecflags               => PrettyString "Vecflags"
        |   InstrGetFirstLong           => PrettyString "GetFirstLong"
        |   InstrStringLength           => PrettyString "StringLength"
        |   InstrThreadSelf             => PrettyString "ThreadSelf"
        |   InstrAtomicAdd i            => PrettyString ("AtomicAdd " ^ Int.toString i)
        |   InstrStoreW _               => PrettyString "InstrStoreW"
        |   InstrStoreB _               => PrettyString "InstrStoreB"
        |   InstrLockSeg                => PrettyString "InstrLockSeg"
        |   InstrUnaryFP w              => PrettyString ("InstrUnaryFP " ^ Word8.toString w)
        |   InstrBinaryFP w             => PrettyString ("InstrBinaryFP " ^ Word8.toString w)
        |   InstrIntToFP                => PrettyString "InstrIntToFP"
        |   InstrUnimplemented          => PrettyString "InstrUnimplemented"

    datatype tests = 
        Arb of opCode
    |   ArbConst of opCode * short
    |   Wrd of opCode
    |   WrdConst of opCode * machineWord (* Both long and short. *)
    |   FloatingPt of opCode
    |   Length of opCode
  
    val Short = Length JNE
    val Long  = Length JE

    val testNeqW  = Wrd JNE
    val testEqW   = Wrd JE
    val testGeqW  = Wrd JNB (* These are UNsigned comparisons *)
    val testGtW   = Wrd JA
    val testLeqW  = Wrd JNA
    val testLtW   = Wrd JB

    val testNeqA  = Arb JNE
    val testEqA   = Arb JE
    val testGeqA  = Arb JGE
    val testGtA   = Arb JG
    val testLeqA  = Arb JLE
    val testLtA   = Arb JL

    
    val testNeqFP = FloatingPt JNE
    val testEqFP  = FloatingPt JE
    val testGeqFP = FloatingPt JGE
    val testGtFP  = FloatingPt JG
    val testLeqFP = FloatingPt JLE
    val testLtFP  = FloatingPt JL

    type cases = word * addrs ref

    datatype source =
        LiteralSource of machineWord
    |   InRegister of reg
    |   BaseOffset of reg * int
    |   CodeRefSource of code (* The address of another function *)
    |   StackAddress of int (* Offset within the stack. *)

    datatype operations =
        DataOp of { instr: instrs, operands: source list, output: reg option }
    |   Move of { source: source, output: reg }
    |   CondBranch of { operands: source list, test: tests, label: labels ref }
    |   PushToStack of source
    |   StoreToMemory of { toStore: source, offset: int, base: reg, index: reg option }
    |   AllocStore of { size: int, flags: Word8.word, output: reg }
    |   CallFunction of { callKind: callKinds }
    |   JumpToFunction of { callKind: callKinds, returnReg: reg option }
    |   ReturnFromFunction of { returnReg: reg option, argsToRemove: int }
    |   RaiseException
    |   UncondBranch of { label: labels ref }
    |   ResetStack of int
    |   BackJumpLabel of { dest: addrs ref }
    |   JumpBack of { dest: addrs ref, addStackCheck: bool }
    |   ForwardJumpLabel of { label: labels ref }
    |   LoadHandlerAddress of { handlerLab: addrs ref, output: reg }
    |   StartHandler of { handlerLab: addrs ref }
    |   IndexedCase of
            { testReg: reg, workReg: reg, minCase: word, maxCase: word,
              isArbitrary: bool, isExhaustive: bool, tableAddrRef: addrs ref }
    |   FillJumpTable of
            { tableAddr: addrs ref, cases: cases list, default: addrs ref, min: word, max: word }

    fun prettyOperation(operation, depth) =
        case operation of
            DataOp { instr: instrs, operands=_, output=_ } =>
                PrettyBlock(3, false, [],
                    [
                        PrettyString "DataOp",
                        PrettyBreak(1, 0),
                        prettyInstr(instr, depth-1)
                    ])
        |   Move { source=_, output=_ } =>
                PrettyString "Move"
        |   CondBranch { operands=_, test=_, label=_ } =>
                PrettyString "CondBranch"
        |   PushToStack _(*source*) =>
                PrettyString "PushToStack"
        |   StoreToMemory { toStore=_, offset=_, base=_, index=_ } =>
                PrettyString "StoreToMemory"
        |   AllocStore { size=_, flags=_, output=_ } =>
                PrettyString "AllocStore"
        |   CallFunction { callKind=_ } =>
                PrettyString "CallFunction"
        |   JumpToFunction { callKind=_, returnReg=_ } =>
                PrettyString "JumpToFunction"
        |   ReturnFromFunction { returnReg=_, argsToRemove=_ } =>
                PrettyString "ReturnFromFunction"
        |   RaiseException =>
                PrettyString "RaiseException"
        |   UncondBranch { label=_ } =>
                PrettyString "UncondBranch"
        |   ResetStack _(*int*) =>
                PrettyString "ResetStack"
        |   BackJumpLabel { dest=_ } =>
                PrettyString "BackJumpLabel"
        |   JumpBack { dest=_, addStackCheck=_ } =>
                PrettyString "JumpBack"
        |   ForwardJumpLabel { label=_ } =>
                PrettyString "ForwardJumpLabel"
        |   LoadHandlerAddress { handlerLab=_, output=_ } =>
                PrettyString "LoadHandlerAddress"
        |   StartHandler { handlerLab=_ } =>
                PrettyString "StartHandler"
        |   IndexedCase
                { testReg=_, workReg=_, minCase=_, maxCase=_,
                  isArbitrary=_, isExhaustive=_, tableAddrRef=_ } =>
                PrettyString "IndexedCase"
        |   FillJumpTable
                { tableAddr=_, cases=_, default=_, min=_, max=_ } =>
                PrettyString "FillJumpTable"

    datatype implement = ImplementGeneral | ImplementLiteral of machineWord

    local
        (* Multiplies and divides by powers of two can be implemented as shifts. *)
        fun getPower2 n =
        let
            fun p2(i, log) = if i < n then p2 (i*0w2, log+0w1) else if i = n then SOME log else NONE
        in
            p2(0w1, 0w0)
        end
    in
        (* checkAndReduce checks whether an instruction is implemented and removes literal constant cases.
           It needs to be called before the argument negotiation because on some platforms (not this)
           some instructions are only implemented for certain constant arguments. *)
        (* Nearly all instructions are implemented in the general case. *)
        fun checkAndReduce(InstrAddA, args: 'a list as [arg1, arg2], mapper: 'a -> machineWord option) =
            (
                case List.map mapper args of
                    [_, SOME lit] =>
                        if isShort lit then SOME(InstrAddAConst(toShort lit), [arg1]) else SOME(InstrAddA, args)
                |   [SOME lit, _] =>
                        if isShort lit then SOME(InstrAddAConst(toShort lit), [arg2]) else SOME(InstrAddA, args)
                |   _ => SOME(InstrAddA, args)
            )

        |   checkAndReduce(InstrSubA, args as [arg1, _], mapper) =
            (
                case List.map mapper args of
                    [_, SOME lit] =>
                        if isShort lit then SOME(InstrSubAConst(toShort lit), [arg1]) else SOME(InstrSubA, args)
                (* We don't currently implement reverse subtraction *)
                |   _ => SOME(InstrSubA, args)
            )

        |   checkAndReduce(InstrMulA, args as [arg1, arg2], mapper) =
            (
                (* The only special case we recognise is multiplication by 2. *)
                case List.map mapper args of
                    [_, SOME lit] =>
                        if isShort lit  andalso toShort lit = 0w2
                        then SOME(InstrMulAConst2, [arg1])
                        else SOME(InstrMulA, args)
                |   [SOME lit, _] =>
                        if isShort lit  andalso toShort lit = 0w2
                        then SOME(InstrMulAConst2, [arg2])
                        else SOME(InstrMulA, args)
                |   _ => SOME(InstrMulA, args)
            )

        |   checkAndReduce(InstrAddW, args as [arg1, arg2], mapper) =
            (
                case List.map mapper args of
                    [_, SOME lit] => SOME(InstrAddWConst(toShort lit), [arg1])
                |   [SOME lit, _] => SOME(InstrAddWConst(toShort lit), [arg2])
                |   _ => SOME(InstrAddW, args)
            )

        |   checkAndReduce(InstrSubW, args as [arg1, _], mapper) =
            (
                case List.map mapper args of
                    [_, SOME lit] => SOME(InstrSubWConst(toShort lit), [arg1])
                (* We don't currently implement reverse subtraction *)
                |   _ => SOME(InstrSubW, args)
            )

        |   checkAndReduce(InstrMulW, args as [arg1, arg2], mapper) =
            (
                (* This is implemented for powers of 2. *)
                case List.map (Option.composePartial(getPower2 o toShort, mapper)) args of
                    [_, SOME shift] => SOME(InstrUpshiftWConst shift, [arg1])
                |   [SOME shift, _] => SOME(InstrUpshiftWConst shift, [arg2])
                |   _ => SOME(InstrMulW, args)
            )

        |   checkAndReduce(InstrDivW, args as [arg1, _], mapper) =
            (
                (* This is implemented for powers of 2. *)
                case List.map (Option.composePartial(getPower2 o toShort, mapper)) args of
                    [_, SOME shift] => SOME(InstrDownshiftWConst shift, [arg1])
                |   _ => SOME(InstrDivW, args)
            )

        |   checkAndReduce(InstrModW, args as [arg1, _], mapper) =
            (
                (* This is implemented for powers of 2. *)
                case List.map (Option.composePartial(getPower2 o toShort, mapper)) args of
                    [_, SOME shift] => SOME(InstrAndWConst((0w1 << shift)-0w1), [arg1])
                |   _ => SOME(InstrModW, args)
            )

        |   checkAndReduce(InstrOrW, args as [arg1, arg2], mapper) =
            (
                case List.map mapper args of
                    [_, SOME lit] => SOME(InstrOrWConst(toShort lit), [arg1])
                |   [SOME lit, _] => SOME(InstrOrWConst(toShort lit), [arg2])
                |   _ => SOME(InstrOrW, args)
            )

        |   checkAndReduce(InstrAndW, args as [arg1, arg2], mapper) =
            (
                case List.map mapper args of
                    [_, SOME lit] => SOME(InstrAndWConst(toShort lit), [arg1])
                |   [SOME lit, _] => SOME(InstrAndWConst(toShort lit), [arg2])
                |   _ => SOME(InstrAndW, args)
            )

        |   checkAndReduce(InstrXorW, args as [arg1, arg2], mapper) =
            (
                case List.map mapper args of
                    [_, SOME lit] => SOME(InstrXorWConst(toShort lit), [arg1])
                |   [SOME lit, _] => SOME(InstrXorWConst(toShort lit), [arg2])
                |   _ => SOME(InstrXorW, args)
            )

        |   checkAndReduce(InstrLoadW, args as [arg1, arg2], mapper) =
            (
                case mapper arg2 of
                    SOME lit => SOME(InstrLoadWConstOffset(toShort lit), [arg1])
                |   NONE => SOME(InstrLoadW, args)
            )

        |   checkAndReduce(InstrLoadB, args as [arg1, arg2], mapper) =
            (
                case mapper arg2 of
                    SOME lit => SOME(InstrLoadBConstOffset(toShort lit), [arg1])
                |   NONE => SOME(InstrLoadB, args)
            )

            (* Shift operations are only implemented for constant shifts. *)
        |   checkAndReduce(InstrUpshiftW, args as [arg1, arg2], mapper) =
            (
                case mapper arg2 of
                    SOME lit => SOME(InstrUpshiftWConst(toShort lit), [arg1])
                |   NONE => SOME(InstrUpshiftW, args)
            )

        |   checkAndReduce(InstrDownshiftW, args as [arg1, arg2], mapper) =
            (
                case mapper arg2 of
                    SOME lit => SOME(InstrDownshiftWConst(toShort lit), [arg1])
                |   NONE => SOME(InstrDownshiftW, args)
            )

        |   checkAndReduce(InstrDownshiftArithW, args as [arg1, arg2], mapper) =
            (
                case mapper arg2 of
                    SOME lit => SOME(InstrDownshiftArithWConst(toShort lit), [arg1])
                |   NONE => SOME(InstrDownshiftArithW, args)
            )

        |   checkAndReduce(InstrSetStringLength, [arg1, arg2], _) =
                (* Don't treat constants specially: the string length is untagged so
                   it's not safe to put it inline. *)
                SOME(InstrSetStringLength, [arg1, arg2])

        |   checkAndReduce(InstrVeclen, [arg1], _) = SOME(InstrVeclen, [arg1])

        |   checkAndReduce(InstrVecflags, [arg1], _) = SOME(InstrVecflags, [arg1])

        |   checkAndReduce(InstrGetFirstLong, [arg1], _) = SOME(InstrGetFirstLong, [arg1])

        |   checkAndReduce(InstrStringLength, [arg1], _) = SOME(InstrStringLength, [arg1])

        |   checkAndReduce(InstrThreadSelf, [], _) = SOME(InstrThreadSelf, [])

        |   checkAndReduce(InstrAtomicAdd n, [arg], _) = SOME(InstrAtomicAdd n, [arg])

        |   checkAndReduce(InstrStoreW _, args as [arg1, arg2, arg3], mapper) =
            (
                case List.map mapper args of
                    [_, SOME constOffset, SOME toStore] =>
                        SOME(InstrStoreW{offset=SOME(toShort constOffset), toStore=SOME toStore}, [arg1])
                |   [_, NONE, SOME toStore] =>
                        SOME(InstrStoreW{offset=NONE, toStore=SOME toStore}, [arg1, arg2])
                |   [_, SOME constOffset, NONE] =>
                        SOME(InstrStoreW{offset=SOME(toShort constOffset), toStore=NONE}, [arg1, arg3])
                |   [_, NONE, NONE] => SOME(InstrStoreW{offset=NONE, toStore=NONE}, [arg1, arg2, arg3])
                |   _ => raise InternalError "InstrStoreW: Wrong number of arguments"
            )

        |   checkAndReduce(InstrStoreB _, args as [arg1, arg2, arg3], mapper) =
            (
                case List.map mapper args of
                    [_, SOME constOffset, SOME toStore] =>
                        SOME(InstrStoreB{offset=SOME(toShort constOffset), toStore=SOME toStore}, [arg1])
                |   [_, NONE, SOME toStore] =>
                        SOME(InstrStoreB{offset=NONE, toStore=SOME toStore}, [arg1, arg2])
                |   [_, SOME constOffset, NONE] =>
                        SOME(InstrStoreB{offset=SOME(toShort constOffset), toStore=NONE}, [arg1, arg3])
                |   [_, NONE, NONE] => SOME(InstrStoreB{offset=NONE, toStore=NONE}, [arg1, arg2, arg3])
                |   _ => raise InternalError "InstrStoreB: Wrong number of arguments"
            )

        |   checkAndReduce(InstrLockSeg, [arg1], _) = SOME(InstrLockSeg, [arg1])

        |   checkAndReduce(instr as InstrUnaryFP _, args as [_], _) = SOME(instr, args)
        |   checkAndReduce(instr as InstrBinaryFP _, args as [_,_], _) = SOME(instr, args)
        |   checkAndReduce(instr as InstrIntToFP, args as [_], _) = SOME(instr, args)

        |   checkAndReduce(InstrUnimplemented, _, _) = NONE

        |   checkAndReduce _ = raise InternalError "checkAndReduce: bad arguments"
    end

        (* If we reverse the left and right arguments we reverse the sense of the jump. *)
        fun reverseTestOp JE = JE
        |   reverseTestOp JNE = JNE
        |   reverseTestOp JA = JB
        |   reverseTestOp JB = JA
        |   reverseTestOp JNA = JNB
        |   reverseTestOp JNB = JNA
        |   reverseTestOp JL = JG
        |   reverseTestOp JG = JL
        |   reverseTestOp JLE = JGE
        |   reverseTestOp JGE = JLE
        |   reverseTestOp _ = raise InternalError "reverseTestOp: unknown branch"

    local
    in
        fun checkAndReduceBranches(Arb opcode, args as [arg1, arg2], mapper) =
            let
                (* Because we normalise arbitrary precision values if we are testing
                   a short integer value for equality we don't have to perform a tag
                   test.  We use word equality for those cases. *)
                fun constOp(JE, lit) = WrdConst(JE, lit)
                |   constOp(JNE, lit) = WrdConst(JNE, lit)
                |   constOp(opc, lit) = ArbConst(opc, toShort lit)
            in
                case List.map mapper args of
                    [_, SOME lit] =>
                        if isShort lit
                        then SOME (constOp(opcode, lit), [arg1])
                        else SOME (Arb opcode, args)
                |   [SOME lit, _] => 
                        if isShort lit
                        then SOME (constOp(reverseTestOp opcode, lit), [arg2])
                        else SOME (Arb opcode, args)
                |   _ => SOME (Arb opcode, args)
            end

            (* Word equality is used both as equality on values of type "word" where all the values are
               definitely short but also as a general pointer equality operation. *)
        |   checkAndReduceBranches(Wrd opcode, args as [arg1, arg2], mapper) =
            (
                case List.map mapper args of
                    [_, SOME lit] => SOME (WrdConst(opcode, lit), [arg1])
                |   [SOME lit, _] =>  SOME (WrdConst(reverseTestOp opcode, lit), [arg2])
                |   _ => SOME (Wrd opcode, args)
            )

        |   checkAndReduceBranches(opc as Length _, args as [_], _) = SOME (opc, args)
        
        |   checkAndReduceBranches(opc as FloatingPt _, args as [_, _], _) = SOME (opc, args)

        |   checkAndReduceBranches _ = raise InternalError "checkAndReduceBranches: bad arguments"
    end

    (* Argument negotiation.  The idea is to get the arguments into the "best" locations
       for the instruction. *)

    datatype regHint = UseReg of reg | NoHint | NoResult

    type argReq = source * bool

    datatype argAction =
        ActionDone of (* The output register if any and the final operation. *)
            { outReg: reg option, operation: operations }
    |   ActionLockRegister of (* Lock the register of an argument. *)
            { argNo: int, reg: reg, willOverwrite: bool, next: nextAction }
    |   ActionLoadArg of (* Load an argument into a register. *)
            { argNo: int, regSet: regSet, willOverwrite: bool, next: nextAction }
    |   ActionGetWorkReg of (* Get a work/result register. *)
            { regSet: regSet, setReg: reg -> nextAction }

    withtype nextAction = argReq list -> argAction

    (* Default action for all operations. *)
    local
        (* If we have specified a preferred register use that. *)
        fun prefAsSet(UseReg reg) = singleton reg
        |   prefAsSet _ = allRegisters

        (* We get the output register first because we're going to lock that down. *)
        fun getResultReg(instr, pref: regHint, operands) _ =
            ActionGetWorkReg{regSet=prefAsSet pref, (* Use the preferred destination. *)
                setReg = fn reg => getArg(instr, SOME reg, operands)}
        
        and getInitialResultReg(instr, pref: regHint) args =
            getResultReg(instr, pref: regHint, List.map(fn _ => NONE) args) args

        (* The initial case if we don't need an output register. *)
        and getInitialArg instr args =
            getArg(instr, NONE, List.map(fn _ => NONE) args) args

        and getArg(instr, outputReg, operands) args =
        let
            (* Take each NONE in the operand list and replace it with the appropriate operand. *)
            fun nextArg([], [], _) =
                (* All done *)
                ActionDone{
                    outReg=outputReg,
                    operation=DataOp{ instr = instr, operands = List.map valOf operands, output = outputReg}
                    }
            |   nextArg(NONE :: otherOps, arg :: _, argNo) =
                (
                    case arg of (* Look at the next argument *)
                        (InRegister reg, _) =>
                            (* It's in a register.  Lock it, record it and go on to the next arg. *)
                            ActionLockRegister{argNo=argNo, reg=reg, willOverwrite=false,
                                next=getArg(instr, outputReg,
                                        List.take(operands, argNo) @ (SOME(InRegister reg) :: otherOps))}
                    |   (_, _) =>
                            ActionLoadArg{argNo=argNo, regSet=allRegisters, willOverwrite=false,
                                (* Load into a register - won't modify it afterwards.  Next action is
                                   to look at this again after it's been loaded. *)
                                next=getArg(instr, outputReg, operands)}
                )
            |   nextArg(SOME _ :: otherOps, _ :: otherArgs, argNo) =
                    nextArg(otherOps, otherArgs, argNo+1)
            |   nextArg _ = raise Empty
        in
            nextArg(operands, args, 0)
        end
    in
        fun defaultActions(instr as InstrStoreW _, _) = getInitialArg instr
        |   defaultActions(instr as InstrStoreB _, _) = getInitialArg instr
        |   defaultActions(instr as InstrLockSeg, _) = getInitialArg instr
        |   defaultActions(instr as InstrSetStringLength, _) = getInitialArg instr

        |   defaultActions(instr, pref) = getInitialResultReg(instr, pref)
        
        local
            (* Storing a byte is complicated because we can only store from some registers.
               We get the value into one of the acceptable registers first. *)
            val byteStoreRegs = listToSet[eax, ebx, ecx, edx]
            fun loadStoreArg instr args =
            let
                val argNo = List.length args - 1 (* Last argument. *)
            in
                case List.last args of
                    (InRegister reg, _) =>
                        (* Put it in the right register.  We might record here whether
                           this is the last usage and only retag the result if we really
                           have to. *)
                        if inSet(reg, byteStoreRegs)
                        then ActionLockRegister{argNo=argNo, reg=reg, willOverwrite=false,
                                next=getArg(instr, NONE, List.tabulate(argNo, fn _ => NONE) @ [SOME(InRegister reg)])}
                        else (* Not in the right register - move it. *)
                            ActionLoadArg{argNo=argNo, regSet=byteStoreRegs, willOverwrite=false,
                                next=loadStoreArg instr}
                |   (_, _) =>
                        ActionLoadArg{argNo=argNo, regSet=byteStoreRegs, willOverwrite=false,
                            (* Load into a register - won't modify it afterwards.  Next action is
                               to look at this again after it's been loaded. *)
                            next=loadStoreArg instr}
            end
        in
            val storeByteNegotiator = loadStoreArg
        end
        
        local
            (* Variable shifts always use the cl register i.e. the low order byte of ecx. *)
            val ecxSet = singleton ecx
            fun loadShiftArg (instr, pref) [_, arg2] =
            (
                case arg2 of
                    (InRegister reg, lastRef) =>
                        if reg = ecx andalso lastRef
                        then ActionLockRegister{argNo=1, reg=reg, willOverwrite=true,
                                next=loadResultArg(instr, pref)}
                        else (* Not in the right register - move it. *)
                            ActionLoadArg{argNo=1, regSet=ecxSet, willOverwrite=true,
                                next=loadShiftArg(instr, pref)}
                |   (_, _) =>
                        ActionLoadArg{argNo=1, regSet=ecxSet, willOverwrite=true,
                             next=loadShiftArg(instr, pref)}
            )
            | loadShiftArg _ _ = raise InternalError "loadShiftArg: bad arguments"

            (* Load the first argument into a register we can use for the result. *)
            and loadResultArg (instr, pref) [arg1, _] =
            (
                case arg1 of
                    (InRegister reg, lastRef) =>
                        if lastRef
                        then ActionLockRegister{argNo=0, reg=reg, willOverwrite=true,
                                next=finished(instr, reg)}
                        else (* Can't clobber this register - move the value. *)
                            ActionLoadArg{argNo=0, regSet=allRegisters, willOverwrite=true,
                                next=loadResultArg(instr, pref)}
                |   (_, _) =>
                        ActionLoadArg{argNo=0, regSet=allRegisters, willOverwrite=true,
                             next=loadResultArg(instr, pref)}
            )
            |   loadResultArg _ _ = raise InternalError "loadResultArg: bad arguments"
            
            and finished (instr, resReg) _ =
                ActionDone{
                    outReg=SOME resReg,
                    operation=DataOp{ instr = instr, operands = [InRegister resReg, InRegister ecx], output = SOME resReg}
                    }

        in
            val shiftNegotiator = loadShiftArg
        end
        
        local
            (* Multiply always places the result in EDX:EAX.  Since we're going to modify those
               registers the best arrangement for the arguments is to get the first argument into
               eax and the second into edx. *)
            val eaxSet = singleton eax and edxSet = singleton edx

            fun loadArg1 instr [(InRegister reg, lastRef), _] =
                if reg = eax andalso lastRef
                then ActionLockRegister{argNo=0, reg=reg, willOverwrite=true, next=loadArg2 instr}
                else (* Not in the right register - move it. *)
                    ActionLoadArg{argNo=0, regSet=eaxSet, willOverwrite=true, next=loadArg1 instr}

            |   loadArg1 instr [_, _] =
                    ActionLoadArg{argNo=0, regSet=eaxSet, willOverwrite=true, next=loadArg1 instr}

            |   loadArg1 _ _ = raise InternalError "loadArg1: bad arguments"
            
            and loadArg2 instr [_, (InRegister reg, lastRef)] =
                if reg = edx andalso lastRef
                then ActionLockRegister{argNo=1, reg=reg, willOverwrite=true, next=finished instr}
                else (* Not in the right register - move it. *)
                    ActionLoadArg{argNo=1, regSet=edxSet, willOverwrite=true, next=loadArg2 instr}
                    
            |   loadArg2 instr [_, _] =
                    ActionLoadArg{argNo=1, regSet=edxSet, willOverwrite=true, next=loadArg2 instr}

            |   loadArg2 _ _ = raise InternalError "loadArg1: bad arguments"

            and finished instr _ =
                ActionDone{
                    outReg=SOME eax,
                    operation=DataOp{ instr = instr, operands = [InRegister eax, InRegister edx], output = SOME eax}
                    }
        in
            val multiplyNegotiator = loadArg1
        end
        
        local (* In many cases we want to use the same register for an argument as for the result. *)
            fun isReversible InstrSubW = false
            |   isReversible _ = true

            fun loadDestArg(instr, _) [(InRegister reg, true)] = (* Single argument case *)
                    ActionLockRegister{argNo=0, reg=reg, willOverwrite=true,
                                       next=getArg(instr, SOME reg, [SOME(InRegister reg)])}
            |   loadDestArg(instr, pref) [(_, _)] = (* Not in a register or register can't be modified. *)
                    ActionLoadArg{argNo=0, regSet=prefAsSet pref, willOverwrite=true,
                                  next=loadDestArg(instr, pref)}
                
            |   loadDestArg(instr, _) [(InRegister reg1, true), _] = (* Double arguments *)
                    ActionLockRegister{argNo=0, reg=reg1, willOverwrite=true,
                                       next=getArg(instr, SOME reg1, [SOME(InRegister reg1), NONE])}
                (* For most instructions we can use the second argument if it's in a register. *)
            |   loadDestArg(instr, pref) [_, (InRegister reg2, true)] = (* Could use the second argument *)
                    if isReversible instr
                    then ActionLockRegister{argNo=0, reg=reg2, willOverwrite=true,
                                       next=getArg(instr, SOME reg2, [NONE, SOME(InRegister reg2)])}
                    else ActionLoadArg{argNo=0, regSet=prefAsSet pref, willOverwrite=true,
                                  next=loadDestArg(instr, pref)}
            |   loadDestArg(instr, pref) [(_, _), _] = (* Not in a register or register can't be modified. *)
                    ActionLoadArg{argNo=0, regSet=prefAsSet pref, willOverwrite=true,
                                  next=loadDestArg(instr, pref)}

            |   loadDestArg _ _ = raise InternalError "loadDestArg: not one or two arguments"
        in
            val sharedArgNegotiator = loadDestArg
        end
    end


    (* Select the argument negotiator.  Apart from StoreByte and shifts always use the default. *)
    fun negotiateArguments(instr as InstrStoreB({ toStore=NONE(* Not storing a constant *), ...}), _) =
            storeByteNegotiator instr
    |   negotiateArguments(instr as InstrUpshiftW, pref) = shiftNegotiator(instr, pref)
    |   negotiateArguments(instr as InstrDownshiftW, pref) = shiftNegotiator(instr, pref)
    |   negotiateArguments(instr as InstrDownshiftArithW, pref) = shiftNegotiator(instr, pref)

    |   negotiateArguments(instr as InstrSubW, pref) = sharedArgNegotiator(instr, pref)
    |   negotiateArguments(instr as InstrOrW, pref) = sharedArgNegotiator(instr, pref)
    |   negotiateArguments(instr as InstrAndW, pref) = sharedArgNegotiator(instr, pref)
    |   negotiateArguments(instr as InstrAddAConst _, pref) = sharedArgNegotiator(instr, pref)
    |   negotiateArguments(instr as InstrSubAConst _, pref) = sharedArgNegotiator(instr, pref)
    |   negotiateArguments(instr as InstrOrWConst _, pref) = sharedArgNegotiator(instr, pref)
    |   negotiateArguments(instr as InstrAndWConst _, pref) = sharedArgNegotiator(instr, pref)
    |   negotiateArguments(instr as InstrXorWConst _, pref) = sharedArgNegotiator(instr, pref)
    
    (*|   negotiateArguments(instr as InstrMulA, _) = multiplyNegotiator instr*)
        (* Only use the negotiator for word multiplication at the moment. *)
    |   negotiateArguments(instr as InstrMulW, _) = multiplyNegotiator instr

    |   negotiateArguments(instr, pref) = defaultActions(instr, pref)

    (* Actions for comparisons and tests. *)
    local
        fun getArg(test, label, operands) args =
        if List.length operands = List.length args
        then (* All done *)
            ActionDone{
                outReg=NONE,
                operation= CondBranch { operands=operands, test=test, label=label }
                }
        else
        let
            val argNo = List.length operands
        in
            case List.nth(args, argNo) of (* Look at the next argument *)
                (InRegister reg, _) =>
                    (* It's in a register.  Lock it, record it and go on to the next arg. *)
                    ActionLockRegister{argNo=argNo, reg=reg, willOverwrite=false,
                        next=getArg(test, label, operands @ [InRegister reg])}
            |   (_, _) =>
                    ActionLoadArg{argNo=argNo, regSet=allRegisters, willOverwrite=false,
                        (* Load into a register - won't modify it afterwards.  Next action is
                           to look at this again after it's been loaded. *)
                        next=getArg(test, label, operands)}
        end

        fun getOneArgInReg(test, label) [(InRegister reg1, _), (op2, _)] =
                ActionDone{
                    outReg=NONE,
                    operation= CondBranch { operands=[InRegister reg1, op2], test=test, label=label }
                    }
        |   getOneArgInReg(test, label) [(op1, _), (InRegister reg2, _)] =
            let
                fun reverse(Arb test) = Arb(reverseTestOp test)
                |   reverse(Wrd test) = Wrd(reverseTestOp test)
                |   reverse _ = raise InternalError "reverse"
            in
                ActionDone{
                    outReg=NONE,
                    operation= CondBranch { operands=[InRegister reg2, op1], test=reverse test, label=label }
                    }
            end
        |   getOneArgInReg(test, label) [_, _] =
                ActionLoadArg{argNo=0, regSet=allRegisters, willOverwrite=false,
                    (* Load into a register - won't modify it afterwards.  Next action is
                       to look at this again after it's been loaded. *)
                    next=getOneArgInReg(test, label)}
        |   getOneArgInReg _ _ = raise InternalError "getOneArgInReg: Wrong arguments"

        fun unaryTestActions(test, label) [(InRegister reg, _)] =
                (* It's in a register.  We're done. *)
                ActionDone{
                    outReg=NONE,
                    operation= CondBranch { operands=[InRegister reg], test=test, label=label }
                    }
        |   unaryTestActions(test, label) [_] =
                ActionLoadArg{argNo=0, regSet=allRegisters, willOverwrite=false,
                    (* Load into a register - won't modify it afterwards.  Next action is
                       to look at this again after it's been loaded. *)
                    next=unaryTestActions(test, label)}
        |   unaryTestActions _ _ = raise InternalError "unaryTestActions: Wrong arguments"

        (* Floating point comparisons all require the ax register.  The other arguments
           are loaded into registers. *)
        fun floatPtActions(test, label) _ =
            ActionGetWorkReg{regSet=singleton eax, setReg = fn _ => getArg(test, label, [])}

    in
        (* Select the argument negotiator. *)
        fun negotiateTestArguments test =
        let
            val destLabel = ref noJump
        in
            case test of
                Arb _ => (getArg(test, destLabel, []), destLabel)
            |   ArbConst _ => (unaryTestActions(test, destLabel), destLabel)
            |   Wrd _ => (getOneArgInReg(test, destLabel), destLabel)
            |   WrdConst _ => (unaryTestActions(test, destLabel), destLabel)
            |   Length _ => (unaryTestActions(test, destLabel), destLabel)
            |   FloatingPt _ => (floatPtActions(test, destLabel), destLabel)
        end
    end

    fun isPushI _ = true

    (* Test the bottom bit and jump depending on its value.  This is used
       for tag tests in arbitrary precision operations and also for testing
       for short/long values. *)
    fun testTag(r, cvec) =
        if r = eax
        then (* Special instruction for testing accumulator.  Can use an 8-bit test. *)
        (
            genop (TEST_ACC8, cvec);
            gen8u (0w1, cvec)
        )
        else if r = ebx orelse r = ecx orelse r = edx (* can we use an 8-bit test? *)
        then (* Yes. The register value refers to low-order byte. *)
        (
            genop    (Group3_a, cvec);
            genmodrm (Register, 0w0 (* test *), getReg r, cvec);
            gen8u    (0w1, cvec)
        )
        else
        (
            genop    (Group3_A, cvec);
            genmodrm (Register, 0w0 (* test *), getReg r, cvec);
            gen32u   (0w1, cvec)
        )

    (* Test a single argument and trap if it is long.  The result is
       the instruction address of the trap, and is used to jump back to
       if the instruction overflows. *)
    fun tagTest1 (r: reg, cvec as Code{ic, ...}) =
    let
        val () = testTag(r, cvec)
        val lab = putConditional (JNE, cvec); (* generates code *)
        val jumpback = !ic;
    in
        genop(Group5, cvec);
        genmodrm (Based8, 0w2 (* call *), getReg ebp, cvec);
        gen8u (Word8.fromInt MemRegArbEmulation, cvec);
        fixup ([lab], cvec);
        jumpback
    end (* tagTest1 *)

    (* Test a pair of arguments and trap if either is long.  The result is
       the instruction address of the trap, and is used to jump back to
       if the instruction overflows. *)
    fun tagTest2 (rd:reg, r1:reg, r2:reg, cvec as Code{ic, ...}) : addrs =
    (
        if rd = r1 orelse rd = r2 then raise InternalError "tagTest2: Sharing registers" else ();
        genMove (rd, r1, cvec);
        genReg  (AND, rd, r2, cvec);
        testTag(rd, cvec);
     
        let
            val lab = putConditional (JNE, cvec); (* generates code *)
            val jumpback = !ic;
        in
            genop(Group5, cvec);
            genmodrm (Based8, 0w2 (* call *), getReg ebp, cvec);
            gen8u (Word8.fromInt MemRegArbEmulation, cvec);
            fixup ([lab], cvec);
            jumpback
        end
    )

    (* generate the "jump on overflow" used to implement arbitrary-precision operations. *)
    fun genJO8 (addr, cvec as Code{ic, ...}) = 
    let
        val () = doPending (cvec, 12);
        val here     = !ic;
        (* jump address calculations are relative to the value
           of the program counter *after* the instruction *)
        val offset   = addr addrMinus (here addrPlus 2);
    in
        gen8u (opToInt JO, cvec);
        gen8s (offset, cvec)
    end
  
    (* Common code for div and mod word.  They are identical apart from
       getting the result. *)
    fun divmodWord(isDiv: bool, r1:reg, r2:reg, rd:reg, cvec) : unit =
    let
          (* The divisor needs to be a different register from
             either eax or edx.  It also needs to be different from
             r1 since we're going to modify divReg before we load r1
             into eax. *)
        val divReg =
            if rd <> eax andalso rd <> edx
            then rd
            else if r1 <> ecx
            then ecx
            else esi
    in
        (* This is a bit complicated because the result is always placed
            in the EDX:EAX register pair so we have to save one or both. *)
        if rd <> eax then genPush(eax, cvec) else ();
        if rd <> edx then genPush(edx, cvec) else ();
        if divReg <> rd then genPush(divReg, cvec) else ();
        (* Untag, but don't shift, the divisor and dividend. *)
        if r2 = eax
        then
        (
             if divReg = r1
             then raise InternalError "Assertion failed: Invalid registers"
             else ();
             (* We use move followed by subtraction since that tests the result for zero. *)
             genMove(divReg, r2, cvec);
             genImmed (SUB, divReg, 1, cvec);
             genLeal (eax, r1, ~1, cvec)
        )
        else
        (
             genLeal (eax, r1, ~1, cvec);
             genMove(divReg, r2, cvec);
             genImmed (SUB, divReg, 1, cvec)
        );
        let
            val lab = [putConditional (JNE, cvec)]
        in
            (* call *) (* Use a call so we can get an exception trace. *)
            genop(Group5, cvec);
            genmodrm (Based8, 0w2 (* call *), getReg ebp, cvec);
            gen8u (Word8.fromInt MemRegRaiseDiv, cvec);
            fixup (lab, cvec)
        end;
        (* Do the division. *)
        genReg  (XOR, edx, edx, cvec);
        genop (Group3_A, cvec);
        genmodrm(Register, 0w6 (* div *), getReg divReg, cvec);
         
        if isDiv
        then
        ( (* Tag the result into the result register. *)
             genop (LEAL, cvec);
             genIndexed (1, SOME eax, eax, Scale1, rd, cvec)
        )
        else (* Add the tag back into the remainder. *)
             genLeal (rd, edx, 1, cvec);

        (* Restore the saved registers.  N.B. This also has
           the effect of making sure that both eax and edx contain
           valid values. *)
        if divReg <> rd then genop(POP_R divReg, cvec) else ();
        if rd <> edx then genop(POP_R edx, cvec) else ();
        if rd <> eax then genop(POP_R eax, cvec) else ()
    end


    fun checkAndLimitShift cvec =
    let
        (* The X86 masks the shift value but the ML basis library requires shift values
           greater than the word size to set the value to 0/-1 as appropriate.
           We set the shift to the maximum if it is larger rather than trying to
           set the result to the appropriate value. The shift value is always in ecx. *)
        val maxShift = if wordSize = 4 then tag 31 else tag 63
        val () = genImmed (CMP, ecx, maxShift, cvec)
        val lab = putConditional (JNA, cvec)
    in
        genMoveI (ecx, maxShift, cvec);
        fixup ([lab], cvec)
    end

  (* On this architecture, the jumptable is physically inserted into
     the code as a vector of address offsets. The function "indexedCase"
     generates the space for the table and "makeJumpTable" inserts
     the actual entries, once the addresses are known.
     SPF 23/11/1997

     Now changed to use a vector of jump instructions.  These are padded
     out to 8 bytes with no-ops.  The reason for the change is to ensure
     that the code segment only contains instructions so that we can scan
     for addresses within the code.  It also simplifies and speeds up
     the indexed jump at the expense of doubling the size of the table
     itself. DCJM 1/1/2001
  *)
  
    fun constrCases (p as (_,_)) = p;
  
    type caseList = cases list;

    fun useIndexedCase { min: word, max: word, number: int, isExhaustive: bool } =
        if isExhaustive
        then number > 4
        else number > 7 andalso number >= (Word.toInt max - Word.toInt min) div 3;

    fun indexedCase (r1:reg, r2:reg, min:word, max:word, testArbitrary, isExhaustive,
                     cvec as Code{exited, ic, ...}) =
    let
        (* Typically, an indexed case will arise from an ML case expression on
           values of a datatype although it could arise from an int.  If it
           comes from a datatype it is likely to be almost exhaustive, "almost"
           because the last case to be written is treated as the default.  We
           can't actually find this out, though, because the value constructors
           are essentially opaque and it is only the later phases of the
           optimiser that can put together the constructor discrimination into
           the CASE instruction.  *)
        val taggedMin = tag(Word.toInt min);
        val taggedMax = tag(Word.toInt max);
        
        val rangeCheck =
            if isExhaustive then []
            else
            let
                val l1 =
                    (* If we are testing ints we need to check that the values are
                       short.  If we're testing word values, including datatype tags,
                       we don't need this. *)
                    if testArbitrary
                    then
                    (
                        testTag(r1, cvec);
                        (* Need to check whether the branch is in range. *)
                        [putConditional (JE, cvec)]
                    )
                    else []

                (* Compare with the minimum. *)
                val () = genImmed(CMP, r1, taggedMin, cvec);
                val l2 = putConditional (JL, cvec);
          
                (* Compare with the maximum. *)
                val () = genImmed(CMP, r1, taggedMax, cvec);
                val l3 = putConditional (JG, cvec)
            in
               l1 @ [l2, l3]
            end
      
        val lab = ref addrZero;
    in
        (* Load the address of the jump table. *)
        genop  (MOVL_32_R r2, cvec);
        addConstToVec (HVal lab, false, cvec);
        (* Compute the jump address.  The index is a tagged
           integer so it is already multiplied by 2.  We need to
           multiply by four to get the correct size. We subtract off
           the minimum value and also the shifted tag. *)
        genop (LEAL, cvec);
        genIndexed (Word.toInt min * ~8 - 4, SOME r2, r1, Scale4, r2, cvec);
        (* Jump into the jump table.  Since each entry in the table
           is 8 bytes long r2 will still be on a word + 2 byte
           boundary. *)
        genop (Group5, cvec);
        genmodrm(Register, 0w4 (* jmp *), getReg r2, cvec);

        exited := true;
        (* There's a very good chance that we will now extend the branches for
           the "out of range" checks.  The code to do that doesn't know
           that all these branches will come to the same point so will generate three
           separate long branches. We could combine them but it's hardly worth it. *)
        doPending (cvec,
            (Word.toInt max - Word.toInt min + 1) * 8 (* size of table. *) + 3 (* Maximum alignment *));
    
        (* The start address must be on a two byte boundary so that the
           address we've loaded is a valid code address. *)
        while (!ic) mod 0w4 <> 0w2 do genop (NOP, cvec);

        let
            fun initialiseTable i =
            if i > max then () (* Done *)
            else
            (
                gen8u (opToInt JMP_32, cvec);
                gen32u (0w0, cvec);
                (* Add no-ops to make it 8 bytes. *)
                gen8u (opToInt NOP, cvec);
                gen8u (opToInt NOP, cvec);
                gen8u (opToInt NOP, cvec);
                initialiseTable (i+0w1)
            )
            val here = !ic;
        in
            lab := here;
            initialiseTable min;
            fixup (rangeCheck, cvec); (* The default case comes in here. *)
            here
        end
    end;

    fun makeJumpTable (startTab: addrs, cl:caseList, default:addrs, 
                     min : word, max : word, Code{codeVec, ...}) : unit =
    let
        fun putCase i addr =
        let
            val addrOfJmp = startTab addrPlus (Word.toInt((i - min) * 0w8))
            val jumpOffset = (addr addrMinus addrOfJmp) - 5 (* From end of instr. *)
        in
            set32s(jumpOffset, addrOfJmp addrPlus 1, codeVec)
        end

        (* Initialise to the default. *)
        fun putInDefaults i =
            if i <= max
            then (putCase i default; putInDefaults(i+0w1))
            else ()

        (* Overwrite the defaults by the cases.  N.B.  We've generated
           the list in reverse order so if we have any duplicates we
           will correctly overwrite the later cases with earlier ones. *)
        fun putInCases [] = ()
        |   putInCases ((i, a) :: t) = (putCase i (! a); putInCases t)
        
    in
        putInDefaults min;
        putInCases cl
    end;

    (* Code generate a list of operations.  The list is in reverse order i.e. last instruction first. *)
    fun codeGenerate (ops, code) =
    let
        fun cgOp(DataOp{ instr=InstrVeclen, operands=[InRegister rs], output=SOME rd}) =
            (
              genLoad  (~4, rs, rd, code);
      
              (* length only occupies the least significant 24 bits
                 - the other bits are flags. *)
              genImmed (AND, rd, exp2_24 - 1, code);

              (* Tag the result. *)
              genTag (rd, code)
            )

        |   cgOp(DataOp{ instr=InstrVecflags, operands=[InRegister rs], output=SOME rd}) =  
            (* Load the flags byte. *)
            (
              genop (ESCAPE, code);
              gen8u (0wxB6 (* movzx *), code);
              genEA (~1, rs, rd, code);

              (* Tag the result. *)
              genTag (rd, code)          
            )

        |   cgOp(DataOp{ instr=InstrGetFirstLong, operands=[InRegister rs], output=SOME rd}) =  
            let
                (* Get the first word of a long integer.  We've already
                   checked that it is long. *)
                (* Test the "sign bit" of the object. *)
                val _ =
                    (
                    genop    (Group3_a, code);
                    genmodrm(Based8, 0w0 (* test *), getReg rs, code);
                    gen8s (~1, code);
                    gen8u (0w16, code)
                    )
                (* Load the unsigned, untagged, little-endian value. *)
                val _ = genLoad  (0, rs, rd, code);
                (* Skip if the sign bit wasn't set. *)
                val l1 = [putConditional (JE, code)]
            in
                genop(Group3_A, code);
                genmodrm(Register, 0w3 (* neg *), getReg rd, code);

                fixup(l1, code);
                genTag (rd, code)
            end

        |   cgOp(DataOp{ instr=InstrStringLength, operands=[InRegister rs], output=SOME rd}) =  
            let
                (* If it's tagged the result is 1 otherwise we need to load
                   the length word and tag it. *)
                val () = testTag(rs, code)
                val l1 = [putConditional (JE, code)]
                val _ = genMoveI (rd, tag 1, code)
                val l2 = unconditionalBranch code
            in
                fixup(l1, code);
                genLoad  (0, rs, rd, code); (* Load the length word. *)
                genTag (rd, code); (* And tag the result. *)
                fixup(l2, code)
            end

        |   cgOp(DataOp{ instr=InstrAddAConst constnt, operands=[InRegister rs], output=SOME rd}) =  
            (* Arbitrary precision addition. *)
            let 
                val c = toInt constnt;
                val addr = tagTest1 (rs, code);
            in
                if rd = rs then () else raise InternalError "InstrAddAConst: Bad registers";
                (*genMove  (rd, rs, code);*)
                genImmed (ADD, rd,  semitag c, code);
                genJO8   (addr, code)
            end

        |   cgOp(DataOp{ instr=InstrSubAConst constnt, operands=[InRegister rs], output=SOME rd}) =
            (* Arbitrary precision subtraction. *)
            let 
                val c = toInt constnt
                val addr = tagTest1 (rs, code)
            in
                if rd = rs then () else raise InternalError "InstrSubAConst: Bad registers";
                (*genMove  (rd, rs, code);*)
                genImmed (SUB, rd, semitag c, code);
                genJO8   (addr, code)
            end

        |   cgOp(DataOp{ instr=InstrAddWConst constnt, operands=[InRegister rs], output=SOME rd}) =
            (* Fixed precision addition - doesn't check for overflow. *)
            (* The argument is shifted but not tagged *)
            let
                val c = toInt constnt
            in
                genLeal (rd, rs, semitag c, code)
            end

        |   cgOp(DataOp{ instr=InstrSubWConst constnt, operands=[InRegister rs], output=SOME rd}) =
            (* Fixed precision subtraction - doesn't check for overflow.  *)
            (* The argument is shifted but not tagged. *)
            let
                val c = toInt constnt
            in
                genLeal (rd, rs, ~ (semitag c), code)
            end

        (*  Now removed.  This is no longer safe now that we look for constants
            in the code.
        | InstrRevSubW => 
            (* Fixed precision reverse subtraction - doesn't check for overflow. *)
            (
              genMoveI (rd, semitag c + 2, code);
              genReg   (SUB, rd, rs, code)
            )
        *)

        |   cgOp(DataOp{ instr=InstrOrWConst constnt, operands=[InRegister rs], output=SOME rd}) =
            (* Logical or. *)
            let
                val c = toInt constnt
                val tagged = tag c;
            in
                if rd = rs then () else raise InternalError "InstrOrWConst: Bad registers";
                (*genMove  (rd, rs, code);*)
                genImmed (OR, rd, tagged, code)
            end

        |   cgOp(DataOp{ instr=InstrAndWConst constnt, operands=[InRegister rs], output=SOME rd}) =
            (* Logical and. *)
            let
                val c = toInt constnt
                val tagged = tag c;
            in
                if rd = rs then () else raise InternalError "InstrAndWConst: Bad registers";
                (*genMove  (rd, rs, code);*)
                genImmed (AND, rd, tagged, code)
            end

        |   cgOp(DataOp{ instr=InstrXorWConst constnt, operands=[InRegister rs], output=SOME rd}) =
            (* Constant must be shifted but not tagged. *)
            let
                val c = toInt constnt
            in
                if rd = rs then () else raise InternalError "InstrAndWConst: Bad registers";
                (*genMove  (rd, rs, code);*)
                genImmed (XOR, rd, semitag c, code)
            end

        |   cgOp(DataOp{ instr=InstrUpshiftWConst constnt, operands=[InRegister rs], output=SOME rd}) =
            (* Word shift of more than 31 (unsigned) is defined to return zero
               for the logical shifts and either 0 or all ones for the arithmetic
               shift.  The i386 shift instructions mask the shift value instead. *)
             let
                val c = toInt constnt
            in
                if c < 0 orelse c > 31
                then genMoveI (rd, tag 0, code)
                else if c = 0 (* Multiplying by one??? *)
                then genMove (rd, rs, code)
                else if c = 1
                then (* Multiplying by 2. *)
                    (
                    genop (LEAL, code);
                    (* Using a base and index means the ~1 is just a byte. *)
                    genIndexed (~1, SOME rs, rs, Scale1, rd, code)
                    )
                else if c = 2
                then (* Multiplying by 4 *)
                    (
                    genop (LEAL, code);
                    (* No base register: ~3 is a full word. *)
                    genIndexed (~3, NONE, rs, Scale4, rd, code)
                    )
                else if c = 3
                then (* Multiplying by 8 *)
                    (
                    genop (LEAL, code);
                    genIndexed (~7, NONE, rs, Scale8, rd, code)
                    )
                else (* Other powers of 2. *)
                let
                    fun power2 n = if n = 0 then 1 else 2 * power2 (n-1)
                in
                    genMove (rd, rs, code);
                    genop (Group2_8_A, code);
                    genmodrm(Register, 0w4 (* shl *), getReg rd, code);
                    gen8s (c, code);
                    (* We have shifted the tag bit along with everything else.
                       We need to subtract the shifted tag bit and a tag bit to
                       the end. *)
                    genLeal (rd, rd, 1 - power2 c, code)
                end
            end

        |   cgOp(DataOp{ instr=InstrDownshiftWConst constnt, operands=[InRegister rs], output=SOME rd}) =
            let
                val c = toInt constnt
            in
                if c < 0 orelse c > 31
                then genMoveI (rd, tag 0, code)
                else
                (
                    genMove (rd, rs, code);
                    genop (Group2_8_A, code);
                    genmodrm(Register, 0w5 (* shr *), getReg rd, code);
                    gen8s (c, code);
                    genImmed (OR, rd, tag 0, code) (* Put in the tag *)
                )
            end

        |   cgOp(DataOp{ instr=InstrDownshiftArithWConst constnt, operands=[InRegister rs], output=SOME rd}) =
            (* In this case it's easiest to set the shift to 31. *)
            let
              val c = toInt constnt
            in
              genMove (rd, rs, code);
              genop (Group2_8_A, code);
              genmodrm(Register, 0w7 (* sar *), getReg rd, code);
              gen8s (if c < 0 orelse c > 31 then 31 else c, code);
              genImmed (OR, rd, tag 0, code) (* Put in the tag *)
            end

        |   cgOp(DataOp{ instr=InstrMulAConst2, operands=[InRegister rs], output=SOME rd}) =
            (* We only handle multiplication by two at the moment.  We
               could handle a wider range but it's not that easy particularly
               because the overflow flag is not defined on shifts of more than
               one. *)
            let 
                val addr = tagTest1 (rs, code);
            in
                (* Do the actual operation after removing a tag from one arg. *)
                genLeal (rd, rs, ~1, code);
                genReg  (ADD, rd, rs, code);
                genJO8  (addr, code)
            end

        |   cgOp(DataOp{ instr=InstrLoadWConstOffset constnt, operands=[InRegister rs], output=SOME rd}) =
            let
                val c = toInt constnt
            in
                (* Offset is words so multiply by 4 to get byte offset. *)
                genLoad (c * 4, rs, rd, code)
            end

        |   cgOp(DataOp{ instr=InstrLoadBConstOffset constnt, operands=[InRegister rs], output=SOME rd}) =
    		let
    		    val c = toInt constnt
            in
                genop (ESCAPE, code);
                gen8u (0wxB6 (* movzx *), code);
                genEA (c, rs, rd, code);

                (* Tag the result. *)
                genTag (rd, code)          
            end

        |   cgOp(DataOp{ instr=InstrAtomicAdd incr, operands=[InRegister rs], output=SOME rd}) =
            (* Atomic Increment/Decrement. *)
            (
                genReg(XOR, rd, rd, code); (* Can't use MOVL because it's not tagged. *)
                genImmed (ADD, rd, semitag incr, code);
                genop (LOCK, code);
                gen8u (0wx0f (* ESCAPE *), code);
                gen8u(0wxC1 (* xaddl *), code);
                genEA(0, rs, rd, code);
                (* Since xadd returns the original value we have to add in the value again. *)
                genImmed (ADD, rd, semitag incr, code)
            )        

        |   cgOp(DataOp{ instr=InstrAddA, operands=[InRegister r1, InRegister r2], output=SOME rd }) =
            (* Arbitrary precision addition. *)
            (* If either argument is long, or if both arguments are short
               but the result overflows, the code branches to "addr". This
               executes a trap which gets us into the run-time system which
               then emulates the instructions, using long arithmetic.
               Isn't that cute? To make it work, we have to be sure that
               the source and destination registers are different, because
               otherwise we wouldn't be able to perform the emulation
               following an arithmetic overflow.
          
               Warning: since the RTS can only emulate a few instructions,
               we have to be very careful about what code we generate here.
          
               For example, we use 2-byte "leal" instructions for tagging and
               untagging rather than 1-byte "add" instructions because the
               emulation has to treat these two operations differently.
               SPF 4/1/95 *)
            let
                val addr = tagTest2 (rd, r1, r2, code) (* generates code *)
            in
                (* Do the actual operation after removing a tag from one arg. *)
                genLeal (rd, r1, ~1, code);
                genReg  (ADD, rd, r2, code);
                genJO8  (addr, code)
            end
  
        |   cgOp(DataOp{ instr=InstrSubA, operands=[InRegister r1, InRegister r2], output=SOME rd }) =
            (* Arbitrary precision subtraction. *)
            let
                val addr = tagTest2 (rd, r1, r2, code); (* generates code *)
            in
                (* Do the actual operation after removing a tag from one arg. *)
                genMove (rd, r1, code);
                genReg  (SUB, rd, r2, code);
                genLeal (rd, rd, 1, code); (* Put back the tag. *)
                genJO8  (addr, code)
            end
  
        |   cgOp(DataOp{ instr=InstrMulA, operands=[InRegister r1, InRegister r2], output=SOME rd }) =
            (* Arbitrary precision multiplication. *)
            let
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
                genJO8  (addr, code) (* Check for overflow. *)
            end

        |   cgOp(DataOp{ instr=InstrAddW, operands=[InRegister r1, InRegister r2], output=SOME rd }) =
            (* Fixed precision addition. (Doesn't test for overflow.) *)
            (
                (* Remove the tag from one argument, then add in the other. *)
                (* This could be done using a single leal instruction: leal rd,[r1+r2-1] *)
                genLeal (rd, r2, ~1, code);
                genReg  (ADD, rd, r1, code)
           )

        |   cgOp(DataOp{ instr=InstrSubW, operands=[InRegister r1, InRegister r2], output=SOME rd }) =
            (* Fixed precision subtraction. (Doesn't test for overflow.) *)
            (
                if rd = r1 then () else raise InternalError "InstrSubW: not shared registers";
                (*genMove  (rd, r1, code);*)
                genReg   (SUB, rd, r2, code);
                genImmed (ADD, rd, 1, code)
            )

        |   cgOp(DataOp{ instr=InstrMulW, operands=[InRegister r1, InRegister r2], output=SOME rd }) =
            (* Fixed precision multiplication. (Doesn't test for overflow.) *)
            (
                (* The negotiator puts the arguments into specific registers. *)
                if rd = eax andalso r1 = eax andalso r2 = edx then () else raise InternalError "InstrMulW: bad arguments";
                (* Untag, but don't shift the multiplicand. *)
                genLeal (eax, eax, ~1, code);
                (* Shift down the multiplier. *)
                genop (Group2_1_A, code);
                genmodrm(Register, 0w5 (* shr *), getReg edx, code);
                (* Do the multiplication. *)
                genop (Group3_A, code);
                genmodrm(Register, 0w4 (* mull *), getReg edx, code);
                (* Add back the tag, but don't shift. *)
                genLeal (eax, eax, 1, code);
                genMove (edx, eax, code) (* Copy this into edx so it doesn't have a bad value. *)
            )

        |   cgOp(DataOp{ instr=InstrDivW, operands=[InRegister r1, InRegister r2], output=SOME rd }) =
            (* Fixed precision division. (Doesn't test for overflow.) *)
            divmodWord(true, r1, r2, rd, code)

        |   cgOp(DataOp{ instr=InstrModW, operands=[InRegister r1, InRegister r2], output=SOME rd }) =
            (* Fixed precision remainder. (Doesn't test for overflow.) *)
            divmodWord(false, r1, r2, rd, code)

        |   cgOp(DataOp{ instr=InstrOrW, operands=[InRegister r1, InRegister r2], output=SOME rd }) =
           (* Logical or. *)
           (
                if rd = r1 then genReg  (OR, rd, r2, code)
                else if rd = r2 then genReg  (OR, rd, r1, code)
                else raise InternalError "InstrOrW: not shared registers"
           )

        |   cgOp(DataOp{ instr=InstrAndW, operands=[InRegister r1, InRegister r2], output=SOME rd }) =
           (* Logical and. *)
           (
                if rd = r1 then genReg  (AND, rd, r2, code)
                else if rd = r2 then genReg  (AND, rd, r1, code)
                else raise InternalError "InstrAndW: not shared registers"
           )

        |   cgOp(DataOp{ instr=InstrXorW, operands=[InRegister r1, InRegister r2], output=SOME rd }) =
            (
                (* Must remove the tag from one argument. *)
                genLeal (rd, r2, ~1, code);
                genReg  (XOR, rd, r1, code)
            )

        |   cgOp(DataOp{ instr=InstrUpshiftW, operands=[InRegister r1, InRegister r2], output=SOME rd }) =
            (
                if rd = r1 andalso r2 = ecx then () else raise InternalError "instrUpshiftW: Bad registers";
                checkAndLimitShift code;
                genLeal (rd, rd, ~1, code); (* Remove the tag from the value before we shift. *)
                genop (Group2_1_A, code); (* Untag ecx first. *)
                genmodrm(Register, 0w5 (* shr *), getReg ecx, code);
                genop (Group2_CL_A, code);
                genmodrm(Register, 0w4 (* shl *), getReg rd, code);
                genImmed (OR, rd, tag 0, code); (* Put in the tag *)
                genTag (ecx, code) (* Retag ecx so that it's a valid tagged int. *)
            )

        |   cgOp(DataOp{ instr=InstrDownshiftW, operands=[InRegister r1, InRegister r2], output=SOME rd }) =
            (
                if rd = r1 andalso r2 = ecx then () else raise InternalError "InstrDownshiftW: Bad registers";
                checkAndLimitShift code;
                genop (Group2_1_A, code); (* Untag ecx first. *)
                genmodrm(Register, 0w5 (* shr *), getReg ecx, code);
                genop (Group2_CL_A, code);
                genmodrm(Register, 0w5 (* shr *), getReg rd, code);
                genImmed (OR, rd, tag 0, code); (* Put in the tag *)
                genTag (ecx, code) (* Retag ecx so that it's a valid tagged int. *)
            )

        |   cgOp(DataOp{ instr=InstrDownshiftArithW, operands=[InRegister r1, InRegister r2], output=SOME rd }) =
            (
                if rd = r1 andalso r2 = ecx then () else raise InternalError "InstrDownshiftArithW: Bad registers";
                checkAndLimitShift code;
                genop (Group2_1_A, code); (* Untag ecx first. *)
                genmodrm(Register, 0w5 (* shr *), getReg ecx, code);
                genop (Group2_CL_A, code);
                genmodrm(Register, 0w7 (* sar *), getReg rd, code);
                genImmed (OR, rd, tag 0, code); (* Put in the tag *)
                genTag (ecx, code) (* Retag ecx so that it's a valid tagged int. *)
            )

        |   cgOp(DataOp{ instr=InstrUnaryFP opn, operands=[InRegister reg], output=SOME rd }) =
            (
                (* Allocate space for the result. *)
                allocStore(8 div wordSize, F_bytes, rd, code);
                genop(FPESC 0w5, code); (* FLD [reg] *)
                genop2(0, reg, 0wx0, code);
                (* If this is the FPATAN function we need to push 1.0 here. *)
                if opn = 0wxF3 then (genop(FPESC 0w1, code); gen8u(0wxE8, code)) else ();
                (* We use the change-sign operation here so that this works correctly for +/- zero. *)
                genop(FPESC 0w1, code); (* FCHS/FSIN etc *)
                gen8u(opn, code);
                genop(FPESC 0w5, code); (* FSTP [rd] *)
                genop2(0, rd, 0wx3, code)
            )

        |   cgOp(DataOp{ instr=InstrBinaryFP nnn, operands=[InRegister r1, InRegister r2], output=SOME rd }) =
            (
                (* Allocate space for the result. *)
                allocStore(8 div wordSize, F_bytes, rd, code);
                genop(FPESC 0w5, code); (* FLD [r1] *)
                genop2(0, r1, 0wx0, code);
                genop(FPESC 0w4, code); (* FADD/FMUL etc *)
                genop2(0, r2, nnn, code);
                genop(FPESC 0w5, code); (* FSTP [rd] *)
                genop2(0, rd, 0wx3, code)
            )

        |   cgOp(DataOp{ instr=InstrIntToFP, operands=[InRegister reg], output=SOME rd }) =
            (
                (* We have to load from memory so the easiest way is to push the source register. *)
                genPush(reg, code);
                (* The argument may be long so we need to test the tag bit. *)
                tagTest1(reg, code);
                (* Shift the value we pushed to untag it now we've done any trapping. *)
                genop (Group2_1_A, code);
                genop2(0, esp, 0w7 (* sar *), code);
                genop(FPESC 0w3, code); (* FILD [reg] *)
                genop2(0, esp, 0wx0, code);
                resetStack(0w1, code); (* Have to restore the SP here BEFORE allocating memory. *)
                (* Allocate space for the result. *)
                allocStore(8 div wordSize, F_bytes, rd, code);
                genop(FPESC 0w5, code); (* FSTP [rd] *)
                genop2(0, rd, 0wx3, code)
            )

        |   cgOp(DataOp{ instr=InstrLoadW, operands=[InRegister r1, InRegister r2], output=SOME rd }) =
            (* Load a word. *)
            (
                (* The index is already multiplied by 2, so we need only multiply
                   by two again to give a word offset.  Then we have to subtract
                   2 to account for the tag. *)
                genop      (MOVL_A_R, code);
                genIndexed (~2, SOME r1, r2, Scale2, rd, code)
            )

        |   cgOp(DataOp{ instr=InstrLoadB, operands=[InRegister r1, InRegister r2], output=SOME rd }) =
            (* Load a byte. *)
            (        
                (* mov r2,rd; shrl $1,rd; movzl 0(r1,rd,1),rd; leal 1(,rd,2),rd *)
                genMove (rd, r2, code);
         
                genop (Group2_1_A, code);
                genmodrm(Register, 0w5 (* shr *), getReg rd, code);
         
                genop (ESCAPE, code);
                gen8u (0wxB6 (* movzl *), code);
                genIndexed (0, SOME r1, rd, Scale1, rd, code);
         
                (* Tag the result. *)
                genTag (rd, code)
            )

        |   cgOp(DataOp{ instr=InstrSetStringLength, operands=[InRegister r1, InRegister r2], output=NONE }) =
            (* Set the length word of a string. *)
            (
            (* The length is untagged. *)
                genop (Group2_1_A, code);
                genmodrm(Register, 0w5 (* shr *), getReg r2, code);

                genop (MOVL_R_A, code);
                genEA (0, r1, r2, code);

                (* Restore the original value. This ensures we don't have a
                   bad value around and also restores the original value
                   since it may still be wanted. *)
                genTag(r2, code)
            )

            (* Assignment operators. *)
        |   cgOp(DataOp{ instr=InstrStoreW{offset=NONE, toStore=NONE},
                         operands=[InRegister address, InRegister offset, InRegister toStore], output=_ }) =
                genStoreW(toStore, 0, address, SOME offset, code)
        |   cgOp(DataOp{ instr=InstrStoreW{offset=SOME lit, toStore=NONE},
                         operands=[InRegister address, InRegister toStore], output=_ }) =
                genStoreW(toStore, Word.toInt lit * wordSize, address, NONE, code)
        |   cgOp(DataOp{ instr=InstrStoreW{offset=NONE, toStore=SOME toStore},
                         operands=[InRegister address, InRegister offset], output=_ }) =
                genStoreIW(toStore, 0, address, SOME offset, code)
        |   cgOp(DataOp{ instr=InstrStoreW{offset=SOME constOffset, toStore=SOME toStore},
                         operands=[InRegister address], output=_ }) =
                genStoreIW(toStore, Word.toInt constOffset * wordSize, address, NONE, code)

        |   cgOp(DataOp{ instr=InstrStoreB{offset=NONE, toStore=NONE},
                         operands=[InRegister address, InRegister offset, InRegister toStore], output=_ }) =
                genStoreB(toStore, 0, address, SOME offset, code)
        |   cgOp(DataOp{ instr=InstrStoreB{offset=SOME lit, toStore=NONE},
                         operands=[InRegister address, InRegister toStore], output=_ }) =
                genStoreB(toStore, Word.toInt lit, address, NONE, code)
        |   cgOp(DataOp{ instr=InstrStoreB{offset=NONE, toStore=SOME toStore},
                         operands=[InRegister address, InRegister offset], output=_ }) =
                genStoreIB(toStore, 0, address, SOME offset, code)
        |   cgOp(DataOp{ instr=InstrStoreB{offset=SOME constOffset, toStore=SOME toStore},
                         operands=[InRegister address], output=_ }) =
                genStoreIB(toStore, Word.toInt constOffset, address, NONE, code)
        
        |   cgOp(DataOp{ instr=InstrThreadSelf, operands=[], output=SOME output }) =
                genLoad (MemRegThreadSelf, ebp, output, code) (* movl 60(%ebp),r; *)
                
        |   cgOp(DataOp{ instr=InstrLockSeg, operands=[InRegister baseReg], output=_ }) =
                (* Remove the mutable bit from the flag byte. *)(*andb CONST(0xff-0x40),-1[Reax]*)
                (
                    genop(Group1_8_a (* group1, 8-bit immediate. byte operand *), code);
                    genmodrm (Based8, arithOpToWord AND, getReg baseReg, code);
                    gen8s (~1, code);
                    gen8u(0wxff - 0wx40, code)
                )

        |   cgOp(DataOp{ instr=_, operands=_, output=_ }) =
                raise InternalError "Not implemented"

        |   cgOp(Move{ source=InRegister source, output }) =
                (* Move from one register to another. r2 is ignored *)
                if output = regHandler (* Not a real register. *)
                then genStoreW (source, MemRegHandlerRegister, ebp, regNone, code)
                else genMove  (output, source, code)

        |   cgOp(Move{ source=LiteralSource source, output }) =
                if isShort source
                then
                   (* Load a constant into a register. rs is ignored. *)
                   let
                        val c = toInt (toShort source);
                        val tagged = tag c;
                   in
                        genMoveI (output, tagged, code)
                   end
                else
                (
                    genop  (MOVL_32_R output, code);
                    addConstToVec (WVal source, false, code) (* Remember this constant and address. *)
                )

        |   cgOp(Move{ source=BaseOffset(base, offset), output }) =
                genLoad(offset, base, output, code)

        |   cgOp(Move{ source=CodeRefSource refCode, output }) =
            (
                genop  (MOVL_32_R output, code);
                codeConst (refCode, false, code)
            )

        |   cgOp(Move{ source=StackAddress byteOffset, output }) =
                (* Set a register to a particular offset in the stack. *)
                if byteOffset = 0
                then genMove (output, regStackPtr, code)
                else genLeal (output, regStackPtr, byteOffset, code)

        |   cgOp(CondBranch{ operands=[InRegister r1, InRegister r2], test=Wrd opc, label }) =
            (
                genReg (CMP, r1, r2, code);
                label := [putConditional (opc, code)]
            )

        |   cgOp(CondBranch{ operands=[InRegister r1, InRegister r2], test=Arb opc, label }) =
            let
                (* Because we normalise short precision integers if we are testing for (in)equality
                   we only have to do a full arbitrary precision test if BOTH arguments are long.
                   In all other cases we have to do the full test if EITHER is long. *)
                val isEquality = case opc of JE  => true | JNE => true | _   => false
                val () = testTag(r1, code)
                (* If this is an (in)equality we go straight to the test if this is short. *)
                val l1 = putConditional (if isEquality then JNE else JE, code)
                val () = testTag(r2, code)
                val l2 = putConditional (JNE, code)
                val () = if isEquality then () else fixup ([l1], code)
                val () =
                    (
                        genop(Group5, code);
                        genmodrm (Based8, 0w2 (* call *), getReg ebp, code);
                        gen8u (Word8.fromInt MemRegArbEmulation, code)
                    )
            in
                fixup (if isEquality then [l1, l2] else [l2], code);
                genReg(CMP, r1, r2, code);
                label := [putConditional (opc, code)]
            end

        |   cgOp(CondBranch{ operands=[InRegister left, BaseOffset(base, offset)], test=Wrd opc, label }) =
            (
                genop(Arith (CMP, 3), code);
                genEA(offset, base, left, code);
                label := [putConditional (opc, code)]
            )

        |   cgOp(CondBranch{ operands=[InRegister r], test=Length opc, label }) =
            (
                testTag(r, code); (* Use the standard tag test. *)
                label := [putConditional (opc, code)]
            )

        |   cgOp(CondBranch{ operands=[InRegister r], test=WrdConst(opc, cnstnt), label }) =
            if isShort cnstnt
            then
            let
                val c = toInt(toShort cnstnt)
            in
                genImmed (CMP, r, tag c, code);
                label := [putConditional (opc, code)]
            end
            else (* We may be comparing with an address. *)
            (
                genop (Group1_32_A (* group1, 32 bit immediate *), code);
                genmodrm(Register, arithOpToWord CMP, getReg r, code);
                addConstToVec (WVal cnstnt, false, code); (* Remember this constant and address. *)
                label := [putConditional (opc, code)]
            )

        |   cgOp(CondBranch{ operands=[InRegister r], test=ArbConst(opc, cnstnt), label }) =
            (
                tagTest1 (r, code);
                genImmed (CMP, r, tag(toInt cnstnt), code);
                label := [putConditional (opc, code)]
            )

        |   cgOp(CondBranch{ operands=[InRegister r1, InRegister r2], test=FloatingPt opc, label }) =
            let
                (* Floating point comparisons use the values of the condition bits in AX.
                   We do explicit tests on the bits rather than using the SAHF instruction
                   because not all 64-bit processors support the SAHF instruction. *)
                val c3 = 0wx4000 and c2 = 0wx0400 and c0 = 0wx0100
                val revOrder = opc = JL orelse opc = JLE
                val mask =
                    case opc of
                        JE => c2 orb c3
                    |   JNE => c2 orb c3
                    |   JL => c0 orb c2 orb c3
                    |   JG => c0 orb c2 orb c3
                    |   JLE => c0 orb c2
                    |   JGE => c0 orb c2
                    |   _ => raise InternalError "FloatingPt"
            in
                genop(FPESC 0w5, code); (* FLD [r1] *)
                genop2(0, if revOrder then r2 else r1, 0wx0, code);
                genop(FPESC 0w4, code); (* FCOMP [r2] - compare and pop *)
                genop2(0, if revOrder then r1 else r2, 0w3, code);
                genop(FPESC 0w7, code); (* FNSTSW AX *)
                gen8u(0wxe0, code);

                genImmed(AND, eax, Word.toInt mask, code);
                if opc = JE orelse opc = JNE
                then genImmed (CMP, eax, Word.toInt c3, code)
                else (* AND has set the condition codes. *) ();
                (* EAX currently has an untagged value in it. Make it safe before
                   doing anything else by moving ebx, which ought to be safe, into it. *)
                genMove(eax, ebx, code);
                label := [putConditional (if opc = JNE then JNE else JE, code)]
            end

        |   cgOp(CondBranch{ operands=_, test=_, label=_ }) =
                raise InternalError "Not implemented"

        |   cgOp(PushToStack(InRegister reg)) = genPush(reg, code)

        |   cgOp(PushToStack(BaseOffset(base, offset))) = genLoadPush(offset, base, code)

        |   cgOp(PushToStack(LiteralSource constnt)) = 
                if isShort constnt
                then
                    let
                        val c = toInt (toShort constnt);
                        val tagged = tag c;
                    in
                        if not (is8Bit tagged)
                        then
                        (
                            genop (PUSH_32, code);
                            gen32s(tagged, code)
                        )
                        else
                        (
                            genop (PUSH_8, code);
                            gen8s (tagged, code)
                        )
                    end
                else
                (
                    genop  (PUSH_32, code);
                    addConstToVec (WVal constnt, false, code) (* Remember this constant and address. *)
                )

        |   cgOp(PushToStack _ ) = raise InternalError "Not implemented"

        |   cgOp(StoreToMemory{ toStore=InRegister toStore, offset, base, index }) =
                genStoreW(toStore, offset, base, index, code)

        |   cgOp(StoreToMemory{ toStore=LiteralSource toStore, offset, base, index }) =
                genStoreIW(toStore, offset, base, index, code)

        |   cgOp(StoreToMemory{ toStore=_, offset=_, base=_, index=_ }) =
                raise InternalError "Not implemented"

        |   cgOp(AllocStore{ size, flags, output }) = allocStore(size, flags, output, code)

        |   cgOp(CallFunction{ callKind }) = callFunction(callKind, code)

        |   cgOp(JumpToFunction{ callKind, returnReg }) = jumpToFunction(callKind, returnReg, code)

        |   cgOp(ReturnFromFunction{ returnReg, argsToRemove }) =
                returnFromFunction(returnReg, argsToRemove, code)

        |   cgOp RaiseException = raiseException code

        |   cgOp(UncondBranch{ label }) = label := unconditionalBranch code

        |   cgOp(ResetStack count) =
                if count < 0 then raise InternalError "ResetStack: negative" else resetStack(Word.fromInt count, code)

        |   cgOp(BackJumpLabel{ dest }) =
                (* Backwards jump label.  Used for loops and also for indexed jumps because the
                   jump table is constructed after the code for each case. *)
            let
                val Code {exited, ic, branchCheck, ...} = code
            in
                (* Make sure any pending operations are done. *)
                doPending (code, 0);
                exited := false; (* We may be jumping here. *)
                branchCheck := !ic;
                dest := ! ic (* After any pending operations. *)
            end

        |   cgOp(JumpBack{ dest=ref dest, addStackCheck }) = jumpback(dest, addStackCheck, code)

        |   cgOp(ForwardJumpLabel{ label=ref label }) = fixup(label, code)

        |   cgOp(LoadHandlerAddress{ handlerLab, output }) = loadHandlerAddress(output, handlerLab, code)

        |   cgOp(StartHandler{ handlerLab }) = fixupHandler(handlerLab, code)

        |   cgOp(IndexedCase { testReg, workReg, minCase, maxCase, isArbitrary, isExhaustive, tableAddrRef }) =
                tableAddrRef := indexedCase(testReg, workReg, minCase, maxCase, isArbitrary, isExhaustive, code)

        |   cgOp(FillJumpTable{ tableAddr=ref tableAddr, cases, default=ref default, min, max }) =
                makeJumpTable(tableAddr, cases, default, min, max, code)
    in
        List.app cgOp (List.rev ops)
    end


    fun printCode (Code{procName, numOfConsts, pcOffset: word ref, constVec, printStream, ...}) seg endcode =
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
                |  findRef ((CVal(Code{procName, ...}), addr: word, _) :: rest) =
                        if caddr = addr + ! pcOffset*0w4
                        then print("=" ^ procName)
                        else findRef rest
                |  findRef (_ :: rest) = findRef rest
            in
                findRef(! constVec)
            end
            else printHex valu
        end;

        fun get16s (a, seg: cseg) : int =
        let
            val b0  = Word8.toInt (csegGet (seg, a));
            val b1  = Word8.toInt (csegGet (seg, a + 0w1));
            val b1' = if b1 >= exp2_7 then b1 - exp2_8 else b1;
        in
            (b1' * exp2_8) + b0
        end;
 
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
        fun printEA  () =
        let
            val modrm = Word8.toInt (csegGet (seg, !ptr));
            val () = (ptr +:= 0w1);
            val md = modrm div 64;
            val rm = modrm mod 8;
        in
            if md = 3
            then print (regRepr (mkReg(Word8.fromInt rm)))
      
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
                then print (regRepr (mkReg (Word8.fromInt base)))
                else ();
        
                if index <> 4 (* No index. *)
                then 
                    print ("," ^ regRepr (mkReg(Word8.fromInt index)) ^ 
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
         
                print ("(" ^ regRepr (mkReg(Word8.fromInt rm)) ^ ")")
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
            val () = printHex (Word.toInt(!ptr)); (* The address. *)
            val () = print "\t";

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
                printEA (); (* These are the wrong way round for gas. *)
                print ",";
                if opByte = opToInt Group1_32_A
                then print32 () else print8 ()
            end
         
            else if opByte = opToInt JE
            then (print "je  \t"; printJmp())

            else if opByte = opToInt JNE
            then (print "jne  \t"; printJmp())

            else if opByte = opToInt JO
            then (print "jo  \t"; printJmp())

            else if opByte = opToInt JL
            then (print "jl  \t"; printJmp())

            else if opByte = opToInt JG
            then (print "jg  \t"; printJmp())

            else if opByte = opToInt JLE
            then (print "jle \t"; printJmp())

            else if opByte = opToInt JGE
            then (print "jge \t"; printJmp())

            else if opByte = opToInt JB
            then (print "jb  \t"; printJmp())

            else if opByte = opToInt JA
            then (print "ja  \t"; printJmp())

            else if opByte = opToInt JNA
            then (print "jna \t"; printJmp())

            else if opByte = opToInt JNB
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
                printEA ();
                print ",";
                print (regRepr (mkReg(Word8.fromInt reg)))
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
                printEA ();
                print ",";
                print (regRepr (mkReg(Word8.fromInt reg)))
            end
         
            else if opByte = opToInt MOVL_R_A
            then
            let
                (* Register is in next byte. *)
                val nb = Word8.toInt (csegGet (seg, !ptr));
                val reg = (nb div 8) mod 8;
            in
                print "movl\t";
                print (regRepr (mkReg(Word8.fromInt reg)));
                print ",";
                printEA ()
            end

            else if opByte = opToInt MOVB_R_A
            then
            let
                (* Register is in next byte. *)
                val nb = Word8.toInt (csegGet (seg, !ptr));
                val reg = (nb div 8) mod 8;
            in
                print "movb\t";
                case reg of
                    0 => print "%al"
                |   1 => print "%cl"
                |   2 => print "%dl"
                |   3 => print "%bl"
                |   4 => print "%ah"
                |   5 => print "%ch"
                |   6 => print "%dh"
                |   7 => print "%bh"
                |   _ => print ("r" ^ Int.toString reg);
                print ",";
                printEA ()
            end


            else if opByte >= opToInt (PUSH_R (mkReg 0w0)) andalso
                    opByte <= opToInt (PUSH_R (mkReg 0w7))
            then print ("pushl\t" ^  regRepr (mkReg (opByte mod 0w8)))
      
            else if opByte >= opToInt (POP_R (mkReg 0w0)) andalso
                    opByte <= opToInt (POP_R (mkReg 0w7))
            then print ("pop\t" ^ regRepr (mkReg (opByte mod 0w8)))
      
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
                printEA ();
                print ",";
                print (regRepr (mkReg(Word8.fromInt reg)))
            end

            else if opByte >= opToInt (MOVL_32_R eax) andalso
                  opByte <= opToInt (MOVL_32_R edi)
            then
            (
                print "movl\t";
                print32 ();
                print("," ^ regRepr (mkReg (opByte mod 0w8)))
            )
         
            else if opByte = opToInt MOVL_32_A
            then
            (
                print "movl_rev\t";
                printEA (); (* These are the wrong way round. *)
                print ",";
                print32 ()
            )
         
            else if opByte = opToInt MOVB_8_A
            then
            (
                print "movb_rev\t";
                printEA (); (* These are the wrong way round. *)
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
                printEA ()
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
                printEA ();
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
                printEA ();
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
                printEA ();
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
                    printEA ();
                    print ",";
                    print (regRepr (mkReg(Word8.fromInt reg)))
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
            then (print "pop\t"; printEA ())
         
            else if opByte = opToInt RET 
            then print "ret"
      
            else if opByte = opToInt STC
            then print "stc"
         
            else if opByte = opToInt RET_16
            then (print "ret\t"; print16 ())

            else if opByte = opToInt TEST_ACC8
            then (print "testb\t%al,"; print8 ())
            
            else if opByte = opToInt LOCK
            then print "lock "
            
            else if opByte >= opToInt (FPESC 0w0) andalso opByte <= opToInt (FPESC 0w7)
            then (* Floating point escapes *)
            let
                (* Opcode is in next byte. *)
                val opByte2  = csegGet (seg, !ptr)
                val nnn = (opByte2 >>- 0w3) andb8 0w7
                val escNo = opByte andb8 0wx7
            in
                if (opByte2 andb8 0wxE0) = 0wxE0
                then (* mod = 11 *)
                (
                    case (escNo, opByte2) of
                        (0w1, 0wxE0) => print "fchs"
                    |   (0w1, 0wxE8) => print "fld1"
                    |   (0w1, 0wxF3) => print "fpatan"
                    |   (0w1, 0wxFA) => print "fsqrt"
                    |   (0w1, 0wxFE) => print "fsin"
                    |   (0w1, 0wxFF) => print "fcos"
                    |   (0w7, 0wxE0) => print "fnstsw\tax"
                    |   _ => (printHex(Word8.toInt opByte); printHex(Word8.toInt opByte2));
                    ptr +:= 0w1
                )
                else (* mod = 00, 01, 10 *)
                (
                    case (escNo, nnn) of
                        (0w3, 0w0) => print "fild\t"
                    |   (0w4, 0w0) => print "fadd\t"
                    |   (0w4, 0w1) => print "fmul\t"
                    |   (0w4, 0w3) => print "fcomp\t"
                    |   (0w4, 0w4) => print "fsub\t"
                    |   (0w4, 0w6) => print "fdiv\t"
                    |   (0w5, 0w0) => print "fld \t"
                    |   (0w5, 0w3) => print "fstp\t"
                    |   _ => (printHex(Word8.toInt opByte); printHex(Word8.toInt opByte2));
                    printEA ()
                )
            end

            else printHex(Word8.toInt opByte);
      
            print "\n"
        end; (* end of while loop *)

        print "\n"

    end (* printCode *);

    (* constLabels - fill in a constant in the code. *)
    fun constLabels (Code{resultSeg=ref rseg, pcOffset=ref offset, ...},
                   addr : addrs, value : machineWord, isRel: bool) : unit =
    let
        val seg       = scSet rseg (* The address of the segment. *)
        val constAddr = addr + wordsToBytes offset
    in
        csegPutConstant (seg, constAddr, value, isRel)
    end;

  (* Fix up references from other vectors to this one. *)
  fun fixOtherRefs (refTo as Code{otherCodes=ref otherCodes, ...}, value) =
  let
    fun fixRef (refFrom as
                    Code{numOfConsts = noc, constVec = ref constVec,
                         resultSeg = ref resultSeg, ...}) =
    let      
      fun putConst (CVal cCode, addr, isRel) =
        if cCode is refTo
        then (* A reference to this one. *)
          (
          (* Fix up the forward reference. *)
          constLabels (refFrom, addr, value, isRel);
          (* decrement the "pending references" count *)
          noc := !noc - 0w1
          )
        else ()
      |  putConst _ = ();
    
    in
      (* look down its list of forward references until we find ourselves. *)
      List.app putConst constVec;
      (* If this function has no more references we can lock it. *)
      if !noc = 0w0
      then csegLock (scSet resultSeg)
      else ()
    end (* fixRef *);
  in
    (* For each `code' which needs a forward reference to `refTo' fixing up. *)
    List.app fixRef otherCodes
  end; (* fixOtherRefs *)


    (* Adjust the destinations of recursive branches i.e. tail recursive calls to the current function
       to include the stack check code that has been added to the start. *)
    fun fixRecursiveBranch (Code{codeVec, ...}, stackCheckSize) (ref (Jump8From addr)) = 
        let
            val diff : int = ~ (Word.toInt addr + stackCheckSize + 1)
        in
            if is8Bit diff
            then set8s (diff, addr, codeVec)
            else raise InternalError "fixRecursiveBranches: branch too large"
        end

    |   fixRecursiveBranch (Code{codeVec, ...}, stackCheckSize) (ref (Jump32From addr)) = 
            set32s (~ (Word.toInt addr + stackCheckSize + 4), addr, codeVec)

    (* Adjust the destinations of recursive calls to include the stack check code that has
       been added to the start. *)
    fun fixRecursiveCall (Code{codeVec, ...}, stackCheckSize) addrH = 
    let
        val instr  = get8u(addrH - 0w1, codeVec)
        val diff   = ~(stackCheckSize + Word.toInt addrH + 4)
    in
        if instr <> opToInt CALL_32
        then raise InternalError "fixRecursiveCalls: not a call instruction"
        else set32s (diff, addrH, codeVec)
    end

(***************************************************************************)
(*                              copyCode                                   *)
(***************************************************************************)
  (* The stack limit register is set at least twice this far from the
     end of the stack so we can simply compare the stack pointer with
     the stack limit register if we need less than this much. Setting
     it at twice this value means that procedures which use up to this
     much stack and do not call any other procedures do not need to
     check the stack at all. *)
  val minStackCheck = 20; 
  
    (* Adds the constants onto the code, and copies the code into a new segment *)
    fun copyCode (cvec, operations, stackRequired, registerSet) : address =
    let
        (* TODO: Previously we generated code as we went along and so could only
           create the prelude at the end.  Now that we have the code as a list we
           can generate the prelude first and then put the code into the segment.  *)
    val () = codeGenerate(operations, cvec)

    val Code{pcOffset, codeVec, noClosure, selfCalls = ref selfCalls, selfJumps = ref selfJumps,
             mustCheckStack = ref callsAProc, numOfConsts, ic, constVec = ref constVec,
             resultSeg, procName, printAssemblyCode, printStream, ...} = cvec
    
    (* This aligns ic onto a fullword boundary. *)
    val ()   = align (0w0, cvec);
    val endic      = !ic; (* Remember end *)
    val ()   = gen32u (0w0, cvec); (* Marker - 0 (changes !ic) *)

    (* Prelude consists of 
       1) nops to make it a whole number of words
       2) code to set the pointer to the constant area. 
       3) stack checking code
    *)
    local
      (* little-endian *)
      fun getBytes (0, _) = []
        | getBytes (n, x) = Word8.fromInt(x mod exp2_8) :: getBytes (n - 1, x div exp2_8);
        
      fun testRegAndTrap (reg, entryPt): Word8.word list =
         [
              (* cmp reg,16(%ebp)*)
              opToInt(Arith (CMP, 3)),
              modrm (Based8, getReg reg, getReg ebp),
              Word8.fromInt MemRegStackLimit,
              (* jnb 3 *)
              opToInt JNB,
              0w3,
              (* call *)
              opToInt Group5,
              modrm (Based8, 0w2 (* call *), getReg ebp),
              entryPt
         ];

      val stackCheckCode : Word8.word list =
        if stackRequired >= minStackCheck
        then let
          val stackByteAdjust = ~4 * stackRequired;
          val loadEdiCode : Word8.word list =
            if is8Bit stackByteAdjust
            then
              [
                opToInt LEAL,
                modrm (Based8, getReg edi, 0w4), (* Need s-i-b byte for %esp *)
                sib (Scale1, 0w4 (* no index *), getReg esp)
              ] 
              @ getBytes (1, stackByteAdjust)
              
                else
              [
                opToInt LEAL,
                modrm (Based32, getReg edi, 0w4), (* Need s-i-b byte for %esp *)
                sib (Scale1, 0w4 (* no index *), getReg esp)
              ] 
              @ getBytes (4, stackByteAdjust);
          
          val testEdiCode =
            testRegAndTrap (edi, Word8.fromInt MemRegStackOverflowCallEx)
        in
          (* leal -stack_reqd(%esp),%edi; boundl %edi,16(%ebp); *)
          loadEdiCode @ testEdiCode
           (* The effect of this sequence is to generate an
             overflow trap if sp < sl *)
        end
         
        else if callsAProc (* check for user interrupt *)
        then testRegAndTrap (esp, Word8.fromInt MemRegStackOverflowCall)
               
        else (* no stack check required *)
          []; 

(*****************************************************************************
Functions now have up to 2 entry points:
  (1) Standard entry point
  (2) Self-call entry point - doesn't change %ecx

Entry point 1 is always the first word of the actual code.
Entry point 2 can be at various offsets (if it is needed at all),
but that's OK because it is only used for calls within the procedure
itself.

*****************************************************************************)

         val nopCode : Word8.word list =
            let
            (* Add sufficient No-ops to round this to a full word. *)
                val len = List.length stackCheckCode mod wordSize
            in
                if len = 0
                then []
                else List.tabulate(wordSize - len, fn _ => opToInt NOP)
            end
            
     in
        val preludeCode = nopCode @ stackCheckCode;
        val wordsForPrelude = Word.fromInt(List.length preludeCode) div Word.fromInt wordSize

        (* +4 for code size, profile count, function name and constants count *)
         (* +1 for register mask. *)
        val segSize = ((!ic)) div Word.fromInt wordSize + wordsForPrelude + 0w4 + 0w1;
       
        (* byte offset of L2 label relative to start of post-prelude code. *)
        val stackCheckSize = List.length stackCheckCode;
    end; (* local *)

    (* fix-up all the self-calls to jump/call to the start of the stack check code. *)
    val () = List.app (fixRecursiveCall (cvec, stackCheckSize)) selfCalls
    val () = List.app (fixRecursiveBranch (cvec, stackCheckSize)) selfJumps

    (* Now make the byte segment that we'll turn into the code segment *)
    val seg : cseg = csegMake segSize;
    val offset     = wordsForPrelude;
    
    val _ = resultSeg := Set seg;
    
    (* Copy the code into the new segment. *)
    val _ = pcOffset := offset;
    val _ = csegCopySeg (codeVec, seg, (! ic), offset);

    (* insert prelude code into segment *)
    
    local
        val ptr = ref 0w0;
        (* Generate the prelude. *)
        fun putPrelude (b: Word8.word) : unit =
        let
            val a = !ptr
        in
            csegSet (seg, a, b);
            ptr := a + 0w1
        end;
    in
        val () = List.app putPrelude preludeCode
    end;
    
    local
        val endOfCode = bytesToWords(! ic) + offset;
    in
        (* Byte offset of start of code. *)
        local
            val addr = wordsToBytes endOfCode
        in
            val () = set32u (Word.toLargeWord addr, addr, seg) 
        end;
      
        (* Put in the number of constants. This must go in before we actually put
           in any constants. There are now only two constants: the function name
           and the register mask. All other constants are in the code. *)
        local
            val addr = wordsToBytes(endOfCode + 0w3 + 0w1)
        in
            val () = set32u (0w2, addr, seg) 
        end;
      
        (* Next the profile count. *)
        local
            val addr = wordsToBytes(endOfCode + 0w1)
        in
            val () = set32u (0w0, addr, seg) 
        end;
      
      (* Now we've filled in all the C integers; now we need to convert the segment
         into a proper code segment before it's safe to put in any ML values.
         SPF 13/2/97
      *)
      val () = csegConvertToCode seg;

      local
            (* why do we treat the empty string as a special case? SPF 15/7/94 *)
            (* This is so that profiling can print "<anon>". Note that a
               tagged zero *is* a legal string (it's "\000"). SPF 14/10/94 *)
            val nameWord : machineWord = if procName = "" then toMachineWord 0 else toMachineWord procName;
      in
            val _ = csegPutWord (seg, endOfCode + 0w2, nameWord);
      end;
      local
            (* Encode the register mask.  This encoding must be the same
           as the one used for assembly code segments. *)
        fun encodeReg(r, n: short): short =
        let
            val reg = 0w1 << Word.fromInt (nReg r)
        in
            reg orb n
        end
        val regSet = List.foldl encodeReg 0w0 registerSet
      in
        val () = csegPutWord (seg, endOfCode + 0w3, toMachineWord regSet);
      end;
    end;  (* scope of endOfCode *)
  in 
    let
      (* and then copy the objects from the constant list. *)
      fun putConst (WVal c, addr, isRel) =
            ( (* Can put these in now. *)
              constLabels (cvec, addr, c, isRel);
              numOfConsts := ! numOfConsts - 0w1
            )

        | putConst (HVal(ref hv), addr, isRel) =
          let
            (* on the PC, we don't add the extra 2 (we do on the Sparc) *)
            (* SPF 24/4/95 *)
            val handlerByteOffset = hv + wordsToBytes offset
            (* The following comment applies to offsetAddr *)
            (* Special function to add to an address.
               This only works if the resulting value is 
               in a code segment and is on a word + 2 byte boundary. *)
            val handlerAddr : handler = offsetAddr (csegAddr seg, handlerByteOffset);
          in
            constLabels (cvec, addr, toMachineWord handlerAddr, isRel);
            numOfConsts := ! numOfConsts - 0w1
          end

          (* forward-reference - fix up later when we compile
             the referenced code *) 
        | putConst (CVal _, _, _) = ()

      val _ = List.app putConst constVec;
    
      (* Switch off "mutable" bit now if we have no
         forward or recursive references to fix-up *)
      val _ = if ! numOfConsts = 0w0 then csegLock seg else ();
  
      (* Do we need to make a closure, or just return the code? *)
      val addr : address =
        if noClosure
        then csegAddr seg
        else let
          val addr : address = alloc (0w1, F_words, toMachineWord (csegAddr seg));
          
          (* Logically unnecessary; however the RTS currently allocates everything
             as mutable because Dave's code assumed that things were done this
             way and I'm not completely sure that everything that needs a mutable
             allocation actually asks for it yet. SPF 19/2/97
          *)
          val () = lock addr;
        in
          addr
        end
  
      (* Now we know the address of this object we can fix up
         any forward references outstanding. This is put in here
         because there may be directly recursive references. *)
      val () = fixOtherRefs (cvec, toMachineWord addr);
  
      val () = 
        if printAssemblyCode
        then (* print out the code *)
          (
          printCode cvec seg ((endic) + offset * 0w4);
          printStream "Register set = [";
          List.app (fn r => (printStream " "; printStream(regRepr r))) registerSet;
          printStream "]\n\n"
          )
        else ();
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

  fun traceContext (Code {procName, ic = ref ic, ...}) =
  (* Function name and code offset to help tracing. *)
     procName ^ ":" ^ Word.fmt StringCvt.HEX (ic)


    (* Exported versions of operations datatype *)
    val moveOp = Move
    and pushToStack = PushToStack
    val storeToMemory = StoreToMemory
    val allocStore = AllocStore
    val callFunction = CallFunction
    val jumpToFunction = JumpToFunction
    val returnFromFunction = ReturnFromFunction
    val raiseException = RaiseException
    val resetStack = ResetStack
    val jumpBack = JumpBack
    val forwardJumpLabel = ForwardJumpLabel
    val loadHandlerAddress = LoadHandlerAddress
    val startHandler = StartHandler
    val indexedCase = IndexedCase
    val fillJumpTable = FillJumpTable

    fun uncondBranch() =
    let
        val label = ref noJump
    in
        (UncondBranch{label=label}, label)
    end

    fun backJumpLabel() =
    let
        val loopLabel: addrs ref = ref addrZero
    in
        (BackJumpLabel{dest=loopLabel}, loopLabel)
    end

    structure Sharing =
    struct
        type code       = code
        and  instrs     = instrs
        and  reg        = reg
        and  tests      = tests
        and  addrs      = addrs
        and  operations = operations
        and  source     = source
        and  regHint    = regHint
        and  argAction  = argAction
        and  regSet     = RegSet.regSet
        and  pretty     = pretty
    end

end (* struct *) (* CODECONS *);
