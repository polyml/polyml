(*
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
   This module contains all the definitions of the Sparc opCodes and registers.
   It uses "codeseg" to create and operate on the segment itself.
 *)

(* 6-10/6/94 SPF added abstraction for opCodes, to replace old explicit hex(?) constants *)
(* 13/6/94 SPF started work on i386 version (was SPARC!) *)

functor I386CODECONS (

(*****************************************************************************)
(*                  DEBUG                                                    *)
(*****************************************************************************)
structure DEBUG :
sig
  val assemblyCode:    bool ref;
end;



(*****************************************************************************)
(*                  MISC                                                     *)
(*****************************************************************************)
structure MISC :
sig
  exception InternalError of string
end;

(*****************************************************************************)
(*                  CODECONS sharing constraints                             *)
(*****************************************************************************)

(* removed SPF 24/9/94 ...
sharing type
  ADDRESS.word
= CODESEG.word
       
sharing type
  ADDRESS.short
= CODESEG.short
       
sharing type
  ADDRESS.address
= CODESEG.address
... *)

) :

(*****************************************************************************)
(*                  CODECONS export signature                                *)
(*****************************************************************************)
sig
  type word;  (* added 13/7/94 SPF *)
  type short;
  type code;
  type reg;   (* Machine registers *)
  type address;
  
  val regNone:     reg;
  val regResult:   reg;
  val regClosure:  reg;
  val regStackPtr: reg;
  val regHandler:  reg;
  val regReturn:   reg;
  
  val regs:    int;     (* No of registers. *)
  val argRegs: int;     (* No of args in registers. *)
  
  val regN:   int -> reg;
  val nReg:   reg -> int;
  val argReg: int -> reg;
  
  val regEq:    reg * reg -> bool;
  val regNeq:   reg * reg -> bool;
  
  val regPrint: reg -> unit;

  type addrs

  val codeCreate: bool * string -> code;  (* makes the initial segment. *)

  (* Operations. *)
  type instrs;
  
  val instrMove:       instrs;
  val instrAddA:       instrs;
  val instrSubA:       instrs;
  val instrRevSubA:    instrs;
  val instrMulA:       instrs;
  val instrAddW:       instrs;
  val instrSubW:       instrs;
  val instrRevSubW:    instrs;
  val instrMulW:       instrs;
  val instrDivW:       instrs;
  val instrModW:       instrs;
  val instrOrW:        instrs;
  val instrAndW:       instrs;
  val instrXorW:       instrs;
  val instrLoad:       instrs;
  val instrLoadB:      instrs;
  val instrVeclen:     instrs;
  val instrVecflags:   instrs;
  val instrPush:       instrs;
  val instrUpshiftW:   instrs;
  val instrDownshiftW: instrs;
  val instrDownshiftArithW: instrs;
  val instrGetFirstLong:	instrs;
  val instrStringLength: instrs;
  val instrSetStringLength: instrs;
  val instrBad:        instrs;
  
  (* Can the we use the same register as the source and destination
     of an instructions? (it would be more flexible to make this
      a function of type "instrs -> bool", but a simple flag will
      suffice for now. SPF 17/1/97
  *)
  val canShareRegs : bool;
  
  (* Enquire about operations. *)
  val instrIsRR: instrs -> bool;         (* Is the general form implemented? *)
  val instrIsRI: instrs * word -> bool; (* Is the immediate value ok? *)

  (* Code generate operations. *)
  val genRR: instrs * reg * reg * reg * code -> unit;
  val genRI: instrs * reg * word * reg * code -> unit;

  type tests;
  
  val testNeqW:  tests;
  val testEqW:   tests;
  val testGeqW:  tests;
  val testGtW:   tests;
  val testLeqW:  tests;
  val testLtW:   tests;
  val testNeqA:  tests;
  val testEqA:   tests;
  val testGeqA:  tests;
  val testGtA:   tests;
  val testLeqA:  tests;
  val testLtA:   tests;
  val Short:     tests;
  val Long:      tests;

  type labels; (* The source of a jump. *)

  val noJump: labels;
  
  (* Compare and branch for fixed and arbitrary precision. *)
  
  val isCompRR: tests -> bool;
  val isCompRI: tests * word -> bool;
  
  val compareAndBranchRR: reg * reg  * tests * code -> labels;
  val compareAndBranchRI: reg * word * tests * code -> labels;

  datatype storeWidth = STORE_WORD | STORE_BYTE

  val genLoad:        int * reg * reg * code -> unit;
  val isIndexedStore: storeWidth -> bool
  val genStore:       reg * int * reg * storeWidth * reg * code -> unit;
  val isStoreI:       word * storeWidth * bool -> bool;
  val genStoreI:      word * int * reg * storeWidth * reg * code -> unit;
  val genPush:        reg * code -> unit;
  val genLoadPush:    int * reg * code -> unit;
  val preferLoadPush: bool;
  val genLoadCoderef: code  * reg * code -> unit;
  val genStackOffset: reg * int * code -> unit;

  val allocStore:      int * short * reg * code -> unit;
  val setFlag:         reg * code * short -> unit;
  val completeSegment: code -> unit;

  datatype callKinds =
		Recursive
	|	ConstantFun of word * bool
	|	CodeFun of code
	|	FullCall
  
  val callFunction:       callKinds * code -> unit;
  val jumpToFunction:     callKinds * reg * code -> unit;
  val returnFromFunction: reg * int * code -> unit;
  val raiseException:     code -> unit;

  type cseg;
  
  val copyCode: code * int * reg list -> address;

  val unconditionalBranch: code -> labels;
  
  type handlerLab;
  
  val loadHandlerAddress:  reg * code -> handlerLab;
  val fixupHandler: handlerLab * code -> unit;
  
  val fixup:        labels * code -> unit; (* Fix up a jump. *)

  (* ic - Address for the next instruction in the segment. *)
  val ic: code -> addrs;
  
  val jumpback: addrs * bool * code -> unit; (* Backwards jump. *)

  val resetStack: int * code -> unit; (* Set a pending reset *)
  val procName:   code -> string;      (* Name of the procedure. *)
  
  type cases
  type jumpTableAddrs
  val constrCases : int * addrs -> cases;
  val useIndexedCase: int * int * int * bool -> bool;
  val indexedCase : reg * reg * int * int * bool * code -> jumpTableAddrs;
  val makeJumpTable : jumpTableAddrs * cases list * addrs * int * int * code -> unit;
  
  val inlineAssignments: bool

  val codeAddress: code -> address option

  val traceContext: code -> string;
end (* CODECONS export signature *) =


let

(*****************************************************************************)
(*                  ADDRESS                                                  *)
(*****************************************************************************)
structure ADDRESS :
sig
  type word;    (* NB *not* eqtype, 'cos it might be a closure *)
  eqtype short;
  type address;
  type handler;

  val wordEq : word * word -> bool
  
  val isShort:  'a     -> bool;
  val toShort:  'a     -> short;
  val toWord:   'a     -> word;
  val toInt:     short -> int;

  val Or:  short * short -> short;
  val And: short * short -> short; 
  val << : short * short -> short; 
  val >> : short * short -> short; 
  
  val offsetAddr : address * short -> handler
  
  val alloc:  (short * short * word) -> address
  val F_words : short

  val lock : address -> unit;
  
  val wordSize: int
end = Address;

(*****************************************************************************)
(*                  CODESEG                                                  *)
(*****************************************************************************)
structure CODESEG :
sig
  type word;
  type short;
  type address;
  type cseg;
  
  val csegMake:          int  -> cseg;
  val csegConvertToCode: cseg -> unit;
  val csegLock:          cseg -> unit;
  val csegGet:           cseg * int -> short;
  val csegSet:           cseg * int * short -> unit;
  val csegPutWord:       cseg * int * word -> unit;
  val csegCopySeg:       cseg * cseg * int * int -> unit;
  val csegAddr:          cseg -> address;
  val csegPutConstant:	 cseg * int * word * 'a -> unit;
end = CodeSeg;

in

(*****************************************************************************)
(*                  CODECONS functor body                                    *)
(*****************************************************************************)
struct
  open CODESEG;
  open DEBUG;
  open ADDRESS;
  open MISC;
  
  val short1    = toShort 1;

  val print = TextIO.print;

  (* prints a string representation of a number *)
  fun printHex v = TextIO.print(Int.fmt StringCvt.HEX v)
 
  (* added SPF - take numbers OUT of code *)
  (* These are defined here as explicit constants, so the     *)
  (* code-generator can in-line them as immediates (I think). *)
  val exp2_3  =          8;
  val exp2_6  =         64;
  val exp2_7  =        128;
  val exp2_8  =        256;
  val exp2_16 =      65536;
  val exp2_24 =   16777216;
  val exp2_30 = 1073741824;
  val exp2_31 = 2147483648;
  val exp2_32 = 4294967296;
    
  (* Let's check that we got them right! *)
  local
    fun exp2 0 = 1
      | exp2 n = 2 * exp2 (n - 1);
  in
    val UUU = 
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
  
  
  infix 6 addrPlus addrMinus;
  infix 4 (* ? *) addrLt; (* SPF 5/1/95 *)
  infix 4 (* ? *) addrLe;
  
  (* All indexes into the code vector have type "addrs" *)
  (* This should be an abstype, but it's exported as an eqtype *)
  datatype addrs = Addr of int
  
  (* + is defined to add an integer to an address *)
  fun (Addr a) addrPlus b = Addr (a + b);
    
  (* The difference between two addresses is an integer *)
  fun (Addr a) addrMinus (Addr b) = a - b; 
  
  fun (Addr a) addrLt (Addr b) = a < b; 
  fun (Addr a) addrLe (Addr b) = a <= b; 
  
  fun mkAddr n = Addr n;    (* addr.up   *)
  
  fun getAddr (Addr a) = a; (* addr.down *)
  
  val addrZero = mkAddr 0;
  val addrLast = mkAddr (exp2_30 - 1); (* A big number. *)
  
  
  (* The "value" points at the jump instruction, or rather at the
     jump offset part of it.  It is a ref because we may have to change
     it if we have to put in a jump with a 32-bit offset. *)
     
  datatype jumpFrom =
    Jump8From  of addrs
  | Jump32From of addrs 
     
  type labels = (jumpFrom ref) list;
  val noJump = []:labels; 
  
  (* This is the list of outstanding labels.  Use a separate type from
      "labels" for extra security. *)
  type labList = (jumpFrom ref) list;
  
  datatype setCodeseg =
     Unset
   | Set of cseg   (* Used for completing forward references. *)
   ;
   
  (* Constants which are too large to go inline in the code are put in
     a list and put at the end of the code. They are arranged so that
     the garbage collector can find them and change them as necessary.
     A reference to a constant is treated like a forward reference to a
     label. *)

  (* A code list is used to hold a list of code-vectors which must have the
     address of this code-vector put into it. *)

  datatype const =
     WVal of word        (* an existing constant *)
   | CVal of code        (* a forward-reference to another function *)
   | HVal of addrs ref   (* a handler *)

  and code = Code of 
    { codeVec:        cseg,           (* This segment is used as a buffer. When the
                                         procedure has been code generated it is
                                         copied into a new segment of the correct size *)
      ic:             addrs ref,      (* Pointer to first free location in "codevec" *)
      constVec:       (const * addrs * bool) list ref, (* Constants used in the code *)
      numOfConsts:    int ref,        (* size of constVec *)
      stackReset:     int ref,        (* Distance to reset the stack before the next instr. *)
      pcOffset:       int ref,        (* Offset of code in final segment. *)
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
	  branchCheck:    addrs ref       (* the address we last fixed up to.  I added
                                         this to track down a bug and I've left it
										 in for security.  DCJM 19/1/01. *)
    };

  (* procName is exported. *)
  fun procName       (Code {procName,...})       = procName;
  
  
  (* %ebp points to a structure that interfaces to the RTS.  These are
     offsets into that structure.  *)
  val MemRegLocalMPointer		= 0
  and MemRegHandlerRegister		= wordSize
  and MemRegLocalMbottom		= wordSize*2
  and MemRegStackLimit			= wordSize*3
  and MemRegHeapOverflowCall	= wordSize*8
  and MemRegStackOverflowCall	= wordSize*9
  and MemRegStackOverflowCallEx = wordSize*10
  and MemRegRaiseException      = wordSize*11
  and MemRegRaiseDiv			= wordSize*13
  and MemRegArbEmulation		= wordSize*14

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
  fun codeCreate (noClosure : bool, name : string) : code =
  let
    val words = codesize div 4
  in
    Code
      { 
        codeVec        = csegMake words, (* a byte array *)
        ic             = ref addrZero,
        constVec       = ref [],
        numOfConsts    = ref 0,
        stackReset     = ref 0, 
        pcOffset       = ref 0, (* only non-zero after code is copied *)
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
		branchCheck    = ref addrZero
      }
    end;
           

  (* Put 1 unsigned byte at a given offset in the segment. *)
  fun set8u (b : int, addr, seg) =
  let
    val a = getAddr addr;
  in
    csegSet (seg, a, toShort b)
  end;

  (* Put 1 signed byte at a given offset in the segment. *)
  fun set8s (b : int, addr, seg) =
  let
    val a = getAddr addr;
    val b' = if b < 0 then b + exp2_8 else b;
  in
    csegSet (seg, a, toShort b')
  end;

  (* Get 1 unsigned byte from the given offset in the segment. *)
  fun get8u (a: int, seg: cseg) : int =
  let
    val b = toInt (csegGet (seg, a));
  in
    b
  end;

  (* Get 1 signed byte from the given offset in the segment. *)
  fun get8s (a: int, seg: cseg) : int =
  let
    val b = toInt (csegGet (seg, a));
  in
    if b >= exp2_7 then b - exp2_8 else b
  end;
 
  (* Put 4 bytes at a given offset in the segment. *)
  (* b0 is the least significant byte. *)
  fun set4Bytes (b3, b2, b1, b0, addr, seg) =
  let
    val a = getAddr addr;
  in
    (* Little-endian *)
    csegSet (seg, a,     toShort b0);
    csegSet (seg, a + 1, toShort b1);
    csegSet (seg, a + 2, toShort b2);
    csegSet (seg, a + 3, toShort b3)
  end;

  (* Put 1 unsigned word at a given offset in the segment. *)
  fun set32u (ival: int, addr: addrs, seg) : unit =
  let
    val topHw    = ival div exp2_16;
    val bottomHw = ival mod exp2_16;
    val b3       = topHw div exp2_8;
    val b2       = topHw mod exp2_8;
    val b1       = bottomHw div exp2_8;
    val b0       = bottomHw mod exp2_8;
  in
    set4Bytes (b3, b2, b1, b0, addr, seg)
  end;

  (* Put 1 signed word at a given offset in the segment. *)
  fun set32s (ival: int, addr: addrs, seg) : unit =
  let
    val topHw    = ival div exp2_16;
    val bottomHw = ival mod exp2_16;
    val b3       = topHw div exp2_8;
    val b2       = topHw mod exp2_8;
    val b1       = bottomHw div exp2_8;
    val b0       = bottomHw mod exp2_8;
    val b3'      = if b3 < 0 then b3 + exp2_8 else b3;
  in
    set4Bytes (b3', b2, b1, b0, addr, seg)
  end;

  (* Get 1 signed word from the given offset in the segment. *)
  fun get32s (a: int, seg: cseg) : int =
  let
    val b0  = toInt (csegGet (seg, a));
    val b1  = toInt (csegGet (seg, a + 1));
    val b2  = toInt (csegGet (seg, a + 2));
    val b3  = toInt (csegGet (seg, a + 3));
    val b3' = if b3 >= exp2_7 then b3 - exp2_8 else b3;
    val topHw    = (b3' * exp2_8) + b2;
    val bottomHw = (b1 * exp2_8) + b0;
  in
    (topHw * exp2_16) + bottomHw
  end;
 
  (* Code-generate a byte. *)
  fun gen8u (ival: int, Code {ic, codeVec, ...}) : unit =
    if 0 <= ival andalso ival < exp2_8
    then let
      val icVal = !ic;
    in
      ic := icVal addrPlus 1;
      set8u (ival, icVal, codeVec)  
    end
    else raise InternalError "gen8u: invalid byte";

  (* Used for signed byte values. *)
  fun gen8s (ival: int, Code {ic, codeVec, ...}) =
    if ~exp2_7 <= ival andalso ival < exp2_7
    then let
      val icVal = !ic;
    in
      ic := icVal addrPlus 1;
      set8s (ival, icVal, codeVec)  
    end
    else raise InternalError "gen8s: invalid byte";

  (* Code-generate a 32-bit word. *)
  fun gen32u (ival: int, Code {ic, codeVec, ...}) : unit =
    if 0 <= ival andalso (isShort ival orelse ival < exp2_32)
	(* Note: This previously was  0 <= ival andalso ival < exp2_32
	   The only reason for adding isShort ival is that doing so
	   avoids almost all the arbitrary precision emulation traps
	   that used to be generated here. *)
    then let
      val icVal = !ic;
    in
      ic := icVal addrPlus 4;
      set32u (ival, icVal, codeVec)
    end
    else raise InternalError "gen32u: invalid word";

  fun gen32s (ival: int, Code {ic, codeVec, ...}) : unit =
    (* the Poly compiler generates buggy code for the commented-out test *)
    if true (* ~exp2_31 <= ival andalso ival < exp2_31 *)
    then let
      val icVal = !ic;
    in
      ic := icVal addrPlus 4;
      set32s (ival, icVal, codeVec)
    end
    else raise InternalError "gen32s: invalid word";

  datatype mode =
    Based    (* mod = 0 *)
  | Based8   (* mod = 1 *)
  | Based32  (* mod = 2 *)
  | Register (* mod = 3 *)
  ;


  (* Put together the three fields which make up the mod r/m byte. *)
  fun modrm (md : mode, rg: int, rm : int) : int =
  let
    val modField = 
      case md of 
          Based    => 0 * exp2_6 (* compile-time evaluated! *)
        | Based8   => 1 * exp2_6
        | Based32  => 2 * exp2_6
        | Register => 3 * exp2_6
        ;
  in
    modField + (rg * exp2_3) + rm
  end;

  fun genmodrm (md : mode, rg: int, rm : int, cvec) : unit =
    gen8u (modrm (md, rg, rm), cvec);

  datatype scaleFactor =
    Scale1 (* s = 0 *)
  | Scale2 (* s = 1 *)
  | Scale4 (* s = 2 *)
  | Scale8 (* s = 3 *)
  ;

  (* Put together the three fields which make up the s-i-b byte. *)
  fun sib (s : scaleFactor, i: int, b : int) : int =
  let
    val sizeField =
      case s of
        Scale1 => 0 * exp2_6
      | Scale2 => 1 * exp2_6
      | Scale4 => 2 * exp2_6
      | Scale8 => 3 * exp2_6
      ;
   in
     sizeField + (i * exp2_3) + b
   end;

  fun gensib (s : scaleFactor, i: int, b : int, cvec : code) : unit =
    gen8u (sib (s, i, b), cvec);

  fun scSet (Set x) = x | scSet _ = raise InternalError "scSet";

  (* Add a constant to the list along with its address.  We mustn't put
     the constant directly in the code since at this stage the code is
	 simply a byte segment and if we have a garbage collection the value
	 won't be updated. *)
  fun addConstToVec (valu: const, isRel, cvec as Code{numOfConsts, constVec, ic, ...}): unit =
  (
	numOfConsts := ! numOfConsts + 1;
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


  abstype reg = Reg of int  (* registers. *)
  with
    (* These are the real registers we have. *)
    val eax = Reg  0;  
    val ecx = Reg  1;  
    val edx = Reg  2;
    val ebx = Reg  3;  
    val esp = Reg  4;  
    val ebp = Reg  5;
    val esi = Reg  6;  
    val edi = Reg  7;
    
    (* Not real registers. *)
    val regNone    = Reg  8;  
    val regHandler = Reg  9;  

    val regResult   = eax; (* Result of function call goes in here. *)
    val regClosure  = edx; (* Addr. of closure for fn. call goes here. *)
    
    val regStackPtr = esp;
    val regReturn   = regNone;

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
      else mkReg i
    ;
         
    infix 7 regEq regNeq regLeq regGeq regMinus;

    fun a regEq  b = getReg a  = getReg b;
    fun a regNeq b = getReg a <> getReg b;
    fun a regLeq b = getReg a <= getReg b;
    fun a regGeq b = getReg a >= getReg b;
    fun (Reg a) regMinus (Reg b) = a - b;
  
    (* The number of the register. *)
    fun nReg r =
      if r regEq esi then 4 else
      if r regEq edi then 5 else
        getReg r;

    fun regRepr r = 
      if r regEq eax then "%eax" else
      if r regEq ebx then "%ebx" else
      if r regEq ecx then "%ecx" else
      if r regEq edx then "%edx" else
      if r regEq esp then "%esp" else
      if r regEq ebp then "%ebp" else
      if r regEq esi then "%esi" else
      if r regEq edi then "%edi" else
         "r" ^ Int.toString (getReg r);
         
    fun regPrint r = TextIO.output (TextIO.stdOut, regRepr r);
    
    (* Arguments are passed in eax and ebx. *)
    val argRegs = 2;

    fun argReg i =
      if i = 0 then eax else
      if i = 1 then ebx else
        raise InternalError ("Not arg reg " ^ Int.toString i);
  end (* reg *);


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
  
  fun arithOpToInt ADD = 0
    | arithOpToInt OR  = 1
    | arithOpToInt ADC = 2
    | arithOpToInt SBB = 3
    | arithOpToInt AND = 4
    | arithOpToInt SUB = 5
    | arithOpToInt XOR = 6
    | arithOpToInt CMP = 7
 ;
  
 (* Primary opCodes.  N.B. only opCodes actually used are listed here.
    If new instruction are added check they will be handled by the
    run-time system in the event of trap. *)
  datatype opCode =
    Group1_8_A
  | Group1_32_A
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
  | Group2_1_A
  | PUSH_8
  | PUSH_32
  | TEST_ACC8
  ;

  fun opToInt opn = 
    case opn of
      Group1_8_A    =>  8 * 16 +  3 (* opCode$0x83 *)
    | Group1_32_A   =>  8 * 16 +  1 (* opCode$0x81 *)
    | JMP_8         => 14 * 16 + 11 (* opCode$0xeb *)
    | JMP_32        => 14 * 16 +  9 (* opCode$0xe9 *)
    | CALL_32       => 14 * 16 +  8 (* opCode$0xe8 *)
    | MOVL_A_R      =>  8 * 16 + 11 (* opCode$0x8b *)
    | MOVL_R_A      =>  8 * 16 +  9 (* opCode$0x89 *)
	| MOVB_R_A		=>  8 * 16 +  8 (* opCode$0x88 *)
    | PUSH_R reg    =>  5 * 16 +  0 + getReg reg (* opCode$up(0x50 + r.down) *)
    | POP_R  reg    =>  5 * 16 +  8 + getReg reg (* opCode$up(0x58 + r.down) *)
    | Group5        => 15 * 16 + 15 (* opCode$0xff *)
    | NOP           =>  9 * 16 +  0 (* opCode$0x90 *)
    | LEAL          =>  8 * 16 + 13 (* opCode$0x8d *)
    | MOVL_32_R reg => 11 * 16 +  8 + getReg reg (* opCode$up(0xb8 + r.down) *)
    | MOVL_32_A     => 12 * 16 +  7 (* opCode$0xc7 *)
    | MOVB_8_A      => 12 * 16 +  6 (* opCode$0xc6 *)
    | ESCAPE        =>  0 * 16 + 15 (* opCode$0x0f *)
    | POP_A         =>  8 * 16 + 15 (* opCode$0x8f *)
    | RET           => 12 * 16 +  3 (* opCode$0xc3 *)
    | RET_16        => 12 * 16 +  2 (* opCode$0xc2 *)
    | JO            =>  7 * 16 +  0 (* opCode$0x70 *)
    | JB            =>  7 * 16 +  2 (* opCode$0x72 *)
    | JNB           =>  7 * 16 +  3 (* opCode$0x73 *)
    | JE            =>  7 * 16 +  4 (* opCode$0x74 *)
    | JNE           =>  7 * 16 +  5 (* opCode$0x75 *)
    | JNA           =>  7 * 16 +  6 (* opCode$0x76 *)
    | JA            =>  7 * 16 +  7 (* opCode$0x77 *)
    | JL            =>  7 * 16 + 12 (* opCode$0x7c *)
    | JGE           =>  7 * 16 + 13 (* opCode$0x7d *)
    | JLE           =>  7 * 16 + 14 (* opCode$0x7e *)
    | JG            =>  7 * 16 + 15 (* opCode$0x7f *)
    | Arith (ao,dw) => arithOpToInt ao * 8 + dw (* opCode$up(op.down*8+dw) *)
    | STC           => 15 * 16 +  9 (* opCode$0xf9 *)
    | Group3_A      => 15 * 16 +  7 (* opCode$0xf7 *)
    | Group3_a      => 15 * 16 +  6 (* opCode$0xf6 *)
    | Group2_8_A    => 12 * 16 +  1 (* opCode$0xc1 *)
    | Group2_1_A    => 13 * 16 +  1 (* opCode$0xd1 *)
    | PUSH_8        =>  6 * 16 + 10 (* opCode$0x6a *)
    | PUSH_32       =>  6 * 16 +  8 (* opCode$0x68 *)
	| TEST_ACC8		=> 10 * 16 +  8 (* opCode$0xa8 *)
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
    (* Already done. *) addConstToVec (WVal (toWord(csegAddr seg)), isRel, into)

  | codeConst (r as Code {otherCodes, ... }, isRel, into) = (* forward *)
      (* Add the referring procedure onto the list of the procedure
         referred to if it is not already there. This makes sure that when
         the referring procedure is finished and its address is known the
         address will be plugged in to every procedure which needs it. *)
      let
        fun onList x []      = false
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
       | removeEntry ((entry as ref (Jump32From addr)) :: t) =
           removeEntry t (* we discard long jumps *)
         
       | removeEntry ((entry as ref (Jump8From addr)) :: t) =
         if lab = addr
         then removeEntry t
         else
          (
             if addr addrLt !longestBranch
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
  fun reallyFixBranches ([] : labels) cvec = ()
    | reallyFixBranches (h::t)        (cvec as Code{codeVec=cseg, ic, branchCheck, ...}) =
   ((case !h of
       Jump8From addr =>
       let
         val offset : int = get8s (getAddr addr, cseg);
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
         val offset : int = get32s (getAddr addr, cseg);
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
		    val a    = getAddr addr;
		    val nop  = toShort (opToInt NOP);
		  in
		    csegSet (cseg, a - 1, nop);
		    csegSet (cseg, a,     nop);
		    csegSet (cseg, a + 1, nop);
		    csegSet (cseg, a + 2, nop);
		    csegSet (cseg, a + 3, nop)
		  end
		  else
		    set32s (diff, addr, cseg)
       end
    );
   reallyFixBranches t cvec
  )
 
  fun fixRecursiveBranches (cvec, target, []) = ()
    | fixRecursiveBranches (cvec as Code{codeVec=cseg, ...}, target, addrH :: addrT) = 
   ((case !addrH of
       Jump8From addr =>
       let
		 val offset : int = get8s (getAddr addr, cseg);
		 val diff : int = (target addrMinus addr) - 1;
       in
		 if offset <> 0
		 then raise InternalError "fixRecursiveBranches: already patched"
		 else ();
	       
		 if is8Bit diff
		 then set8s (diff, addr, cseg)
		 else raise InternalError "fixRecursiveBranches: branch too large"
       end

    | Jump32From addr =>
      let
        val offset : int = get32s (getAddr addr, cseg);
		val diff : int = (target addrMinus addr) - 4;
      in
        if offset <> 0
        then raise InternalError "fixRecursiveBranches: already patched"
        else ();

        if diff <> 0
        then set32s (diff, addr, cseg)
        else raise InternalError "fixRecursiveBranches: zero offset"
      end
    );
   fixRecursiveBranches (cvec, target, addrT)
  );

  (* The address is the offset of the offset, not the instruction itself. *)
  fun fixRecursiveCalls (cvec, target, []) = ()
    | fixRecursiveCalls (cvec as Code{codeVec=cseg, ...}, target, addrH :: addrT) = 
    let
      val instr  : int = get8u  (getAddr addrH - 1, cseg);
      val offset : int = get32s (getAddr addrH, cseg);
      val diff   : int = (target addrMinus addrH) - 4;
    in
      if instr <> opToInt CALL_32
		then raise InternalError "fixRecursiveCalls: not a call instruction"
      else if offset <> 0
		then raise InternalError "fixRecursiveCalls: already patched"
      else if diff = 0
		then raise InternalError "fixRecursiveCalls: zero offset"
      else set32s (diff, addrH, cseg);
	 
      fixRecursiveCalls (cvec, target, addrT)
    end;

  (* Deal with a pending fix-up. *)
  fun reallyFixup (cvec as Code{justComeFrom=ref [], ... }) = ()
   |  reallyFixup (cvec as Code{justComeFrom=jcf as ref labs, exited, ... }) = 
       (exited := false; reallyFixBranches labs cvec; jcf := []);

  (* Adds the displacement to the stack pointer before an
     instruction is generated. *)
  fun resetSp (cvec as Code{stackReset, ...}) =
    ( (* Any pending jumps must be taken first. *)
      reallyFixup cvec;
      
      let
        val sr = !stackReset * 4; (* Offset in bytes. *)
      in
        stackReset := 0;
        
        if sr < 0 then raise InternalError "Negative stack reset" else
        if is8Bit sr
        then (* Can use one byte immediate *) 
          (
            gen8u(opToInt Group1_8_A (* group1, 8-bit immediate *), cvec);
            genmodrm(Register, arithOpToInt ADD, getReg esp, cvec);
            gen8s(sr, cvec)
           )
        else (* Need 32 bit immediate. *)
          (
           gen8u(opToInt Group1_32_A (* group1, 32-bit immediate *), cvec);
           genmodrm(Register, arithOpToInt ADD, getReg esp, cvec);
           gen32s(sr, cvec)
          )
      end
     ) (* resetSp *); 
        
  (* Do any pending instructions, but only fix up branches if there
     are instructions in the pipe-line. *)
  fun flushQueue (Code{stackReset = ref 0, ...}) = ()
   |  flushQueue cvec = resetSp cvec;


  (* Makes a new label. (no longer returns a list SPF) *)
  fun makeShortLabel (addr: addrs, Code{longestBranch, labelList ,...}) : jumpFrom ref =
  let
    val lab = ref (Jump8From addr);
  in
    if addr addrLt ! longestBranch
    then longestBranch := addr
    else ();
    
    labelList := lab :: ! labelList;
    
    lab
  end;

  (* Apparently fix up jumps - actually just record where we have come from *)
  fun fixup (labs:labels, cvec as Code{justComeFrom, exited, ic, branchCheck, procName, ...}) =
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
			   if !ic addrLe !branchCheck
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
			   if !ic addrLe !branchCheck
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
					  exited, ic, stackReset, labelList, ...})
  		(branched:bool) (size:int) =
    (* If the longest branch is close to going out of range it must
       be converted into a long form. *)
    (* If we have just made an unconditional branch then we make the 
       distance shorter. *)
  let
    val maxDiff = (if branched then 100 else 127 - 5) - size;

    (* See if we must extend some branches.  If we are going to fix up a label
	   immediately we don't normally extend it.  The exception is if we have
	   to extend some other labels in which case we may have to extend this
	   because the jumps we add may push this label out of range.
	   DCJM 9/4/01. *)
	local
	    val icOffset =
			if branched then !ic else !ic addrPlus 2 (* Size of the initial branch. *)
		fun checkLab (lab as ref (Jump8From addr), n) =
			if List.exists (fn a => a = lab) (! justComeFrom)
			then n (* Don't include it here. *)
			else if (icOffset addrMinus addr) + n > (100 - size) then n+5 else n
		|	checkLab (_, n) = n
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
           Jump32From addr => (* shouldn't happen? *)
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
	       gen32u (0, cvec);    (* long jump to final destination *)
	       lab := Jump32From (!ic addrPlus ~4);
	       (* Return the rest of the list. *)
	       convertRest
	      )
	    else
	      (
	       (* Not ready to remove this. Just find out if this is an
		  earlier branch and continue. *)
	       if addr addrLt ! longestBranch
	       then longestBranch := addr
	       else ();
	       
	       lab :: convertRest
	      )
       end (* convertLabels *);
    in
      if !ic addrMinus ! longestBranch > maxDiff
      then let
        (* Must save the stack-reset, otherwise "fixup" will try
           to reset it. *)
        val sr = !stackReset;
        val _  = stackReset := 0;
         
        (* Must skip round the branches unless we have just taken an
           unconditional branch. *)
        val lab =
          if branched then []
          else
           (
            exited := true;
            gen8u (opToInt JMP_8, cvec);
            gen8u (0, cvec);
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
    val mustReset = stackReset <> 0;
  in
    (* If we have not exited and there are branches coming in here
       then we fix them up before jumping round any branch extensions. *)
     if ! exited then () else reallyFixup cvec;
   
     checkBranchList cvec (! exited) (if mustReset then size + 6 else size);
            
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
      gen8u (0, cvec);
      makeShortLabel (!ic addrPlus ~1, cvec)
    );

  (* Generates an unconditional branch. *)
  fun unconditionalBranch (cvec as Code {justComeFrom, exited, ...}): labels =
  let
    (* If we have just jumped here we may be able to avoid generating a
        jump instruction. *)
    val U : unit = flushQueue cvec; (* Do any pending instructions. *)
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
    val U : unit = flushQueue cvec;
    
    (* Can we get into the prelude with an 8-bit jump? *)
    (* Conservative estimation needs to allow for:
         (1) stack check (10 bytes)
         (2) 1 byte instruction + 1 byte offset (2 bytes)
         (3) possible programmer arithmetic error (6 bytes)
    *)      
    fun isNearPrelude addr = getAddr addr <= 110;

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
      
    val U : unit = tidy longJumps;
	
    (* do we actually need to insert a jump into the codestream? *)
    val needsJump =
       case shortJumps of
		 [] => not (! exited) 
		| _ => true;
  in
    if needsJump
    then let
      (* fix up pending short jumps to here *)
      val U : unit = justComeFrom := shortJumps;
      val U : unit = doPending (cvec, 5);
    
      (* Now decide whether we can use an 8-bit jump here. *)
      (* N.B. we use gen8u here, not genop, because the latter
         calls "doPending (cvec, 12)" which could change the
         results of our "isNearPrelude (!ic)" test. *)
      val br =
        if isNearPrelude (!ic)
        then
	  (
	    gen8u (opToInt JMP_8, cvec);
	    gen8u (0, cvec);
	    ref (Jump8From (!ic addrPlus ~1))
	  )
        else
	  (
	    gen8u  (opToInt JMP_32, cvec);
	    gen32u (0, cvec);
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
    

  (* Exported. Adds in the reset. Does not actually generate code. *)
  fun resetStack (offset, Code{stackReset, ...}) : unit =
    stackReset := ! stackReset + offset;

  (* Generate an effective address. *)
  fun genEA (offset: int, rb: reg, r : reg, cvec) : unit =
  let
    val offsetCode =
      (* don't generate [ebp] (use [ebp+0]) 'cos it doesn't exist! *)
      if offset = 0 andalso (rb regNeq ebp) 
        then Based  (* no disp field *)
      else if is8Bit offset
        then Based8  (* use 8-bit disp field *)
        else Based32; (* use 32-bit disp field *)
  in
    if rb regEq esp
    then (* Need to use s-i-b byte. *)
      (
        if offset < 0 then raise InternalError "Negative stack offset" else ();
        genmodrm (offsetCode, getReg r, 4 (* use SIB *), cvec);
        gensib   (Scale1, 4 (* no index *), getReg esp, cvec)
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
  fun genop2 (offset: int, rb: reg, opc: int, cvec) : unit =
  let
    val offsetCode =
      (* don't generate [ebp] (use [ebp+0]) 'cos it doesn't exist! *)
      if offset = 0 andalso (rb regNeq ebp) 
        then Based  (* no disp field *)
      else if is8Bit offset
        then Based8  (* use 8-bit disp field *)
        else Based32; (* use 32-bit disp field *)
  in
    if rb regEq esp
    then (* Need to use s-i-b byte. *)
      (
        if offset < 0 then raise InternalError "Negative stack offset" else ();
        genmodrm (offsetCode, opc, 4 (* use s-i-b *), cvec);
        gensib (Scale1, 4 (* no index *), getReg esp, cvec)
      )
    else genmodrm(offsetCode, opc, getReg rb, cvec);
     
    (* generate the disp field (if any) *)
    case offsetCode of
      Based8  => gen8s  (offset, cvec)
    | Based32 => gen32s (offset, cvec)
    | _       => ()
  end;


  (* Similar to genEA, but used when there is an index register.
     rb may be regNone if no base register is required (used
     with leal to tag values). *)
  fun genIndexed (offset: int, rb: reg, ri: reg, size : scaleFactor, rd: reg, cvec) =
  let
    val offsetCode =
      if rb regEq regNone orelse (offset = 0 andalso (rb regNeq ebp))
        then Based    (* no disp field *)
      else if is8Bit offset
        then Based8   (* use 8-bit disp field *)
        else Based32; (* use 32-bit disp field *)

    val basefield = 
      if rb regEq regNone
      then 5 (* no base register *)
      else getReg rb;
  in
    genmodrm (offsetCode, getReg rd, 4 (* s-i-b *), cvec);
    gensib   (size, getReg ri, basefield, cvec);
    
    (* generate the disp field (if any) *)
    case offsetCode of
      Based8  => gen8s  (offset, cvec)
    | Based32 => gen32s (offset, cvec)
    | _       => if rb regEq regNone  (* 32 bit absolute used as base *)
                 then gen32s (offset, cvec)
                 else ()
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
   genIndexed (1, r, r, Scale1, r, cvec)
 )       

  fun genImmed (opn: arithOp, rd: reg, imm: int, cvec) : unit =
    if is8Bit imm
    then (* Can use one byte immediate *) 
      (
       genop (Group1_8_A (* group1, 8 bit immediate *), cvec);
       genmodrm(Register, arithOpToInt opn, getReg rd, cvec);
       gen8s (imm, cvec)
      )
    else (* Need 32 bit immediate. *)
      (
       genop (Group1_32_A (* group1, 32 bit immediate *), cvec);
       genmodrm(Register, arithOpToInt opn, getReg rd, cvec);
       gen32s(imm, cvec)
      );

  fun genReg (opn: arithOp, rd: reg, rs: reg, cvec) : unit =
    (
      genop (Arith (opn, 3 (* r/m to reg *)), cvec);
      genmodrm(Register, getReg rd, getReg rs, cvec)
    );
      
  (* generate padding no-ops to align to n modulo 4 *)
  fun align (n:int, cvec as Code{ic, ...}) =
    case (n - getAddr (!ic)) mod 4 of
      1 => genop    (NOP, cvec)
    | 2 => genReg   (OR, eax, eax, cvec)
    | 3 => genImmed (OR, eax, 0, cvec)
    | _ => ()
    ;

  (* Exported.  - movl offset(rb),rd. *)
  fun genLoad (offset: int, rb: reg, rd: reg, cvec) : unit =
    if rd regEq regHandler (* Not a real register. *)
    then 
      (
       (* pushl offset(rb); popl 4(ebp) *)
       (* This only happens when we are popping the handler so we
          could simply pop it straight. *)
       genop (Group5, cvec);
       genop2 (offset, rb, 6 (* push *), cvec);
       
       genop (POP_A, cvec);
       genop2 (MemRegHandlerRegister, ebp, 0, cvec)
      )
    else
      (
       genop (MOVL_A_R, cvec);
       genEA (offset, rb, rd, cvec)
      );

  datatype storeWidth = STORE_WORD | STORE_BYTE
  
  fun isIndexedStore _ = true (* Yes, for both word and byte. *)

  (* Exported - Can we store the value without going through a register? *)
  fun isStoreI (cnstnt: word, _, _) = isShort cnstnt;

  (* Store an immediate value at a given address and offset. *)
  fun genStoreI (cnstnt: word, offset: int, rb: reg, STORE_WORD, ri: reg,
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
		 align(if ri regEq regNone then 2 else 1, cvec)
		)
	  else if offset = 0 andalso rb regEq eax andalso ri regEq regNone
	  then (* modrm will be zero.  We need to be sure that this is not the
	  		  first byte in a word. *)
	  	(
		 doPending(cvec, 12 + 1);
		 if getAddr (!ic) mod 4 = 3 (* opcode will be the last byte in this word. *)
		 then align(1, cvec)
		 else ()
		)
	  else ();
      genop (MOVL_32_A, cvec);
	  if ri regEq regNone then genop2 (offset, rb, 0, cvec)
	  else genIndexed (offset-2, rb, ri, Scale2, mkReg 0, cvec);
	  if isShort cnstnt
	  then gen32s (toInt (toShort cnstnt) * 2 + 1, cvec)
	  else addConstToVec (WVal cnstnt, false, cvec)
   )
  | genStoreI (cnstnt: word, offset: int, rb: reg, STORE_BYTE, ri: reg, cvec) : unit =
  	if not (isShort cnstnt)
	then (* This should never happen. *)
		raise InternalError "genStoreI: storing long constant as a byte"
	else
    let
      val v = toInt (toShort cnstnt)
    in
	  if ri regEq regNone
	  then
	  	(
	      genop (MOVB_8_A, cvec);
		  genop2 (offset, rb, 0, cvec);
	      gen8u (v, cvec)
		)
	  else
	    (
		  (* Untag the index first. *)
          genop (Group2_1_A, cvec);
          genmodrm(Register, 5 (* shr *), getReg ri, cvec);

	      genop (MOVB_8_A, cvec);
		  genIndexed (offset, rb, ri, Scale1, mkReg 0, cvec);
	      gen8u (v, cvec);
		  
		  (* Retag the index. *)
	      genTag (ri, cvec)
		)
   end


  (* Exported. *)
  (* Store a value on the stack.  This is used when the registers need to be
     saved, for more than 4 arguments or to push an exception handler. *)
  fun genPush (r:reg, cvec) : unit =
    if r regEq regHandler (* Not a real register. *)
    then
      (
        genop (Group5, cvec);
        genop2 (MemRegHandlerRegister, ebp, 6 (* push *), cvec)
      )
    else genop (PUSH_R r, cvec);


  (* Exported. Load a value and push it on the stack.  Used when all
     the allocatable registers have run out.
     Also used if preferLoadPush is true. *)
  fun genLoadPush (offset: int, rb: reg, cvec) : unit =
    (
      genop (Group5, cvec);
      genop2 (offset, rb, 6 (* push *), cvec)
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
    align (1, cvec);
 
    genop (CALL_32, cvec);  (* 1 byte  *)
    gen32u (0, cvec);       (* 4 bytes *)
    
    if getAddr (!ic) mod 4 <> 2
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
  fun genStore (rs: reg, offset: int, rb: reg, STORE_WORD, ri, cvec) : unit =
    (
      genop (MOVL_R_A, cvec);
      if ri regEq regNone then genEA (offset, rb, rs, cvec)
	  else genIndexed (offset-2, rb, ri, Scale2, rs, cvec)
    )
  |  genStore (rs: reg, offset: int, rb: reg, STORE_BYTE, ri, cvec) : unit =
    let
	  (* Only some of the 32 bit registers can be used to store from.
	     Eax, Ebx, Ecx and Edx are fine and the code used for those
		 registers corresponds to their low-order byte.  The other
		 registers can't be used.  In the absence of some way of
		 telling the higher-level code-generator about this or finding
		 out if a suitable register is free if we are given one of
		 the other registers we just have to make the best of it. *)
		val regToStore =
			if getReg rs <= 3 then rs (* No problem *)
			else if rb regNeq eax andalso ri regNeq eax then eax
			else if rb regNeq ebx andalso ri regNeq ebx then ebx
			else ecx (* Must be free *)
	 in
	  if rs regEq regToStore then ()
	  else (* Have to move the value into the the right register. *)
		(
		  genPush(regToStore, cvec);
		  genMove(regToStore, rs, cvec)
		);
	  (* The value we store has to be untagged. *)
      genop (Group2_1_A, cvec);
      genmodrm(Register, 5 (* shr *), getReg regToStore, cvec);

	  if ri regEq regNone
	  then ()
	  else
	  	( (* Untag the index as well. *)
          genop (Group2_1_A, cvec);
          genmodrm(Register, 5 (* shr *), getReg ri, cvec)
		);

      genop (MOVB_R_A, cvec);
      if ri regEq regNone then genEA (offset, rb, regToStore, cvec)
	  else genIndexed (offset, rb, ri, Scale1, regToStore, cvec);

	  (* Restore the original value, either by popping or by retagging.
	     This ensures we don't have a bad value around and also
		 restores the original value since it may still be wanted. *)
      if rs regEq regToStore
	  then genTag(rs, cvec)
	  else genop(POP_R regToStore, cvec);

	  if ri regEq regNone
	  then ()
	  else (* retag the index as well. *) genTag (ri, cvec)
    end;


  (* Move an immediate value into a register. The immediate value may be
     any 32-bit number. *)
  fun genMoveI (rd:reg, immed:int, cvec) =
    (* There may be better ways e.g. xorl rd,rd; addl immed,rd. *)
    (* Need 32 bit immediate. *) (* what if it's zero? (looks like marker word) SPF *)
    (
      genop  (MOVL_32_R rd, cvec);
      gen32s (immed, cvec)
    );
  
  (* Exported. *)
  fun genLoadCoderef (c:code, rd:reg, cvec) : unit =
  	(
		genop  (MOVL_32_R rd, cvec);
		codeConst (c, false, cvec)
	)

  type handlerLab = addrs ref;
  
  (* Exported. Loads the address of the destination of a branch. Used to
     put in the address of the exception handler.
	 We used to have pushAddress in place of this which pushed the
	 address at the same time.  On this architecture it can save an
	 instruction but it's a problem on machines where we have to load
	 the address into a register - we don't have a spare checked
	 register available. *)
  fun loadHandlerAddress  (rd, cvec) : handlerLab =
    let
      val lab         = ref addrZero;
    in
	  genop  (MOVL_32_R rd, cvec);
	  addConstToVec (HVal lab, false, cvec);
      lab
    end;

  (* Exported *)
  fun fixupHandler (lab:handlerLab, cvec as Code{exited, ic, branchCheck, ...}) : unit =
  ( 
    (* Make sure anything pending is done first. *)
    (* 15 comes from maximum instruction size + up to 3 nops. *)
    doPending (cvec, 15); 
    
    (* Ensure the return address is aligned onto a word + 2 byte
       boundary. *)
    align (2, cvec);
    
    exited := false;
	branchCheck := !ic;
    lab := !ic
  );

  datatype callKinds =
		Recursive			(* The function calls itself. *)
	|	ConstantFun of word * bool (* A pre-compiled or io function. *)
	|	CodeFun of code		(* A static link call. *)
	|	FullCall			(* Full closure call *)
  
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
		    align (0, cvec);
		 
		    genop (Group5, cvec);
		    genmodrm(Based, 2 (* call *), getReg regClosure, cvec)
		  )

	 | CodeFun c =>
	 	  (
		    (* Make sure anything pending is done first. *)
		    doPending (cvec, 15); 
		    (* Ensure the return address is aligned on
		       a word + 2 byte boundary.  *)
		    align (1, cvec);
		 
		    genop (CALL_32, cvec);
			codeConst (c, true, cvec)
		  )

	 | ConstantFun(w, false) =>
	 	  (
		    (* Make sure anything pending is done first. *)
		    doPending (cvec, 15); 
		    (* Ensure the return address is aligned on
		       a word + 2 byte boundary.  *)
		    align (1, cvec);
		 
		    genop (CALL_32, cvec);
			addConstToVec (WVal w, true, cvec)
		  )

	 | ConstantFun(w, true) =>
	 	  (
		    (* Make sure anything pending is done first. *)
		    doPending (cvec, 15); 
		    (* Ensure the return address is aligned on
		       a word + 2 byte boundary.  *)
		    align (0, cvec);
		 
			genop (Group5, cvec);
			genmodrm(Based, 2 (* call *), 5 (* Immediate address. *), cvec);
			addConstToVec (WVal w, false, cvec)
		  );

    if getAddr (!ic) mod 4 <> 2
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
      if returnReg regNeq regNone
      then  genop (PUSH_R returnReg, cvec)
      else ();
            
     case callKind of 
         Recursive =>
         let
           val U : unit = mustCheckStack := true;
           val lab = genSelfBranch cvec;
         in
           selfJumps := lab @ ! selfJumps
         end
           
     | FullCall =>
          ( (* Full closure call *)
            mustCheckStack := true;
	        genop (Group5, cvec);
	        genmodrm(Based, 4 (* jmp *), getReg regClosure, cvec)
          )
      
	 | CodeFun c =>
	 	  (
            mustCheckStack := true; (* May be recursive. *)
		    genop (JMP_32, cvec);
			codeConst (c, true, cvec)
		  )

	 | ConstantFun(w, false) =>
	 	  (		 
            mustCheckStack := true; (* May be recursive. *)
		    genop (JMP_32, cvec);
			addConstToVec (WVal w, true, cvec)
		  )

	 | ConstantFun(w, true) =>
	 	  (
		  	(* Indirect jumps are used to call into the RTS.  No need
			   to check the stack. *)
			genop (Group5, cvec);
			genmodrm(Based, 4 (* jmp *), 5 (* Immediate address. *), cvec);
			addConstToVec (WVal w, false, cvec)
		  );

     exited := true (* We're not coming back. *)
    );


  (* Exported. Return and remove args. *)
  fun returnFromFunction (resReg, args, cvec as Code{exited, ...}) : unit =
    (if resReg regNeq regNone
        then raise InternalError "Wrong argument"
     else if args = 0
       then genop (RET, cvec)
     else let
       val offset = args * 4
     in
       genop (RET_16, cvec);
       gen8u (offset mod exp2_8, cvec);
       gen8s (offset div exp2_8, cvec)
     end;
     
     exited := true (* We're not coming back. *)
    );


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
	   align (3, cvec);
	   genop(Group5, cvec);
	   genmodrm (Based8, 2 (* call *), getReg ebp, cvec);
	   gen8u (MemRegRaiseException, cvec)
	  )



  (* Exported. Set a register to a particular offset in the stack. *)
  fun genStackOffset (reg, byteOffset, cvec) : unit = 
    if byteOffset = 0
	then genMove (reg, regStackPtr, cvec)
    else genLeal (reg, regStackPtr, byteOffset, cvec)

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
	      gen8u (MemRegStackLimit, cvec);
		  (* jnb 3 *)
		  let
		  	val lab = [putConditional (JNB, cvec)]
		  in
			  (* call *)
			  genop(Group5, cvec);
			  genmodrm (Based8, 2 (* call *), getReg ebp, cvec);
			  gen8u (MemRegStackOverflowCall, cvec);
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
    val lengthWord = size + (toInt flag * exp2_24); (* Size + mutable. *)
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
		genmodrm(Based8, 2 (* call *), getReg ebp, cvec);
		gen8s (MemRegHeapOverflowCall, cvec);
		fixup (lab, cvec)
	end;
    
    genStore (resultReg, MemRegLocalMPointer, ebp, STORE_WORD, regNone, cvec);
    
    (* Set the size field of a newly allocated piece of store. *)
    genop (MOVL_32_A, cvec);
    genmodrm(Based8, 0, getReg resultReg, cvec);
    gen8s (~4, cvec);
    gen32u (lengthWord, cvec)
  end;

  (* Remove the mutable bit by clearing the flag byte. *)
  fun setFlag (baseReg, cvec, flag) : unit =
     genStoreI (toWord flag, ~1, baseReg, STORE_BYTE, regNone, cvec);

  (* Small tuples and closures are created by allocating the space and
     storing into it without setting the mutable bit.  This is safe
     provided there are no traps until all the values have been stored,
     and gcode checks for this by loading all the values, apart from
     constants, into registers.  We have to make sure that we don't mess
     this up by reordering a load instruction (which might cause a
     persistent store trap) before the final store.  Gcode calls
     "completeSegment" after the last store and we flush the queue just
     to be on the safe side. *)
  (* This comment applied to the (very) old persistent store system and
     is no longer relevant.  I've left the comment in because there may
	 be code that assumes that this is still necessary.  DCJM June 2006. *)
  val completeSegment = flushQueue;

  datatype instrs = 
    instrMove
  | instrAddA
  | instrSubA
  | instrRevSubA
  | instrMulA
  | instrAddW
  | instrSubW
  | instrRevSubW
  | instrMulW
  | instrDivW
  | instrModW
  | instrOrW
  | instrAndW
  | instrXorW
  | instrLoad
  | instrLoadB
  | instrVeclen
  | instrVecflags
  | instrPush
  | instrUpshiftW    (* logical shift left *)
  | instrDownshiftW  (* logical shift right *)
  | instrDownshiftArithW  (* arithmetic shift right *)
  | instrGetFirstLong
  | instrStringLength
  | instrSetStringLength
  | instrBad;
   
  (* Can the we use the same register as the source and destination
     of an instructions? On this machine - no. *)
  val canShareRegs : bool = false;

  (* Is there a general register/register operation? Some operations may not
     be implemented because this machine does not have a suitable instruction
     or simply because they have not yet been added to the code generator. It
     is possible for an instruction to be implemented as a register/immediate
     operation but not as a register/register operation (e.g. multiply) *) 
  fun instrIsRR instrUpshiftW   = false (* General shifts require CL register *)
    | instrIsRR instrDownshiftW = false
    | instrIsRR instrDownshiftArithW = false
(*  | instrIsRR instrMulA       = false (* Too complicated to implement *)
    | instrIsRR instrMulW       = false (* Too complicated to implement *)
    | instrIsRR instrDivW       = false
    | instrIsRR instrModW       = false
*)
    | instrIsRR _               = true
    ;
  
  datatype tests = 
     Length of opCode (* always a conditional jump *)
   | Arb of opCode
   | Wrd of opCode;
  
  val Short = Length JNE;
  val Long  = Length JE;

  val testNeqW  = Wrd JNE;
  val testEqW   = Wrd JE;
  val testGeqW  = Wrd JNB; (* These are UNsigned comparisons *)
  val testGtW   = Wrd JA;
  val testLeqW  = Wrd JNA;
  val testLtW   = Wrd JB;
  
  val testNeqA  = Arb JNE;
  val testEqA   = Arb JE;
  val testGeqA  = Arb JGE;
  val testGtA   = Arb JG;
  val testLeqA  = Arb JLE;
  val testLtA   = Arb JL;
  
  (* Is this argument acceptable as an immediate or should it be loaded into a register? *)
  local
     fun isPower2 n =
	 let
	 	fun p2 i = if i < n then p2 (i*2) else i = n
	 in
	    n > 0 andalso p2 1 
	 end
  in
  fun instrIsRI (i, cnstnt) =
    case i of
      instrBad        => false
    | instrRevSubA    => false
	| instrRevSubW	  => false
	| instrMove		  => true (* All constants are immediate. *)
	| instrPush		  => true (* All constants are immediate. *)
    | instrMulA       => isShort cnstnt andalso toInt (toShort cnstnt) = 2
    | instrMulW       => isShort cnstnt andalso isPower2(toInt (toShort cnstnt))
    | instrDivW       => isShort cnstnt andalso isPower2(toInt (toShort cnstnt))
    | instrModW       => isShort cnstnt andalso isPower2(toInt (toShort cnstnt))
	| instrSetStringLength => false (* The string length is untagged and so
									   it's not safe to put it inline. *)
    | _               => isShort cnstnt (* All others must be short *)
  end;
      
  (* Test a single argument and trap if it is long.  The result is
     the instruction address of the trap, and is used to jump back to
     if the instruction overflows. *)
  fun tagTest1 (r: reg, cvec as Code{ic, ...}) =
  let
    val regNum = getReg r;
  in
     if regNum = getReg eax
	 then (* Special instruction for testing accumulator.  Can use an
	         8-bit test. *)
       (
        genop (TEST_ACC8, cvec);
        gen8u (1, cvec)
       )
     else if regNum < 4 (* can we use an 8-bit test? *)
     then (* Yes. The register value refers to low-order byte. *)
       (
        genop    (Group3_a, cvec);
        genmodrm (Register, 0 (* test *), regNum, cvec);
        gen8u    (1, cvec)
       )
     else
       (
        genop    (Group3_A, cvec);
        genmodrm (Register, 0 (* test *), regNum, cvec);
        gen32u   (1, cvec)
       );
     
     let
       val lab = putConditional (JNE, cvec); (* generates code *)
       val jumpback = !ic;
     in
	   genop(Group5, cvec);
	   genmodrm (Based8, 2 (* call *), getReg ebp, cvec);
	   gen8u (MemRegArbEmulation, cvec);
       fixup ([lab], cvec);
       jumpback
     end
  end; (* tagTest1 *)

  (* Test a pair of arguments and trap if either is long.  The result is
    the instruction address of the trap, and is used to jump back to
    if the instruction overflows. *)
  fun tagTest2 (rd:reg, r1:reg, r2:reg, useOr:bool, cvec as Code{ic, ...}) : addrs =
  let
     (* In most cases we have to trap if EITHER is long, and so we AND
       together the arguments.  However if we are comparing two arbitrary
       precision values for (in)equality, we need only trap if BOTH are
       long, since if one is long and the other not then they are both
       definitely different. *)
    val testOP = if useOr then OR else AND;
  in   
     (* If the destination register is not the same as either source
        register we can use that directly, otherwise we have to push it. *)
     (* bug-fixed SPF 3/1/95 - now checks both r1 and r2. *)
    if rd regEq r1
       then 
         (
           genPush (rd, cvec);
           genReg  (testOP, rd, r2, cvec)
         )
     else if rd regEq r2
       then
         (
           genPush (rd, cvec);
           genReg  (testOP, rd, r1, cvec)
         )
       else
         (
           genMove (rd, r1, cvec);
           genReg  (testOP, rd, r2, cvec)
         );

	 if rd regEq eax
	 then (* Special instruction to test the accumulator. *)
	 	(
        genop (TEST_ACC8, cvec);
        gen8u (1, cvec)
		)
     else if (getReg rd) < 4 (* We can only use 8-bit tests with first 4 regs! (SPF) *)
     then (* Use an 8-bit test.  The register value refers to low-order byte. *)
       (
        genop    (Group3_a, cvec);
        genmodrm (Register, 0(* test *), getReg rd, cvec);
        gen8u    (1, cvec)
       )
     else
       (
        genop    (Group3_A, cvec);
        genmodrm (Register, 0(* test *), getReg rd, cvec);
        gen32u   (1, cvec)
       );

     (* restore rd if it's used as a source register *)
     if (rd regEq r1) orelse (rd regEq r2)
     then genop (POP_R rd, cvec)
     else ();
     
     let
       val lab = putConditional (JNE, cvec); (* generates code *)
       val jumpback = !ic;
     in
	   genop(Group5, cvec);
	   genmodrm (Based8, 2 (* call *), getReg ebp, cvec);
	   gen8u (MemRegArbEmulation, cvec);
       fixup ([lab], cvec);
       jumpback
     end
   end;


  (* generate the "jump on overflow" used to
     implement arbitrary-precision operations. *)
  fun genJO8 (addr, cvec as Code{ic, ...}) = 
  let
    val U : unit = doPending (cvec, 12);
    val here     = !ic;
    (* jump address calculations are relative to the value
        of the program counter *after* the instruction *)
    val offset   = addr addrMinus (here addrPlus 2);
  in
    gen8u (opToInt JO, cvec);
    gen8s (offset, cvec)
  end
  

  (* All these can be handled. *)
  fun isCompRR tc = true;

  (* Is this argument acceptable as an immediate or should it be loaded into a register? *) 
  fun isCompRI (tc, cnstnt) =
    case tc of
      Length _ => true
    | _        => isShort cnstnt; (* check!!! SPF *)
    
    
  (* Fixed and arbitrary precision comparisons. *)
  fun compareAndBranchRR (r1, r2, tc, cvec) : labels =
    case tc of
      Wrd opc =>
        (
          genReg (CMP, r1, r2, cvec);
          [putConditional (opc, cvec)]
        )
  
    | Arb opc =>
      let
        (* Test the tags.  If we are testing for equality we can OR the
           tags and only trap if both arguments are long.  Try to use
           eax, ebx, ecx or edx for rd because then we can use a single
           byte test. *)
         val rd = if getReg r1 > 4 then r2 else r1;
         val useOr = 
           case opc of
             JE  => true
           | JNE => true
           | _   => false
           ;
      in
        tagTest2 (rd, r1, r2, useOr, cvec);
        genReg   (CMP, r1, r2, cvec);
        [putConditional (opc, cvec)]
      end
      
    | _ => 
       raise InternalError "Should not have short/long here";
    
    
  fun compareAndBranchRI (r, cnstnt, tc, cvec) : labels =
  let
    val c = toInt (toShort cnstnt);
  in
    case tc of
      Length opc =>
        (
           (* Since we're only interested in the bottom bit we could use an
              8-bit test, provided the value was in eax, ebx, ecx, edx. *)
           genop (Group3_A, cvec);
           genmodrm (Register, 0, getReg r, cvec);
           gen32u (1, cvec);
           [putConditional (opc, cvec)]
        )
  
    | Wrd opc => 
        (
          genImmed (CMP, r, tag c, cvec);
          [putConditional (opc, cvec)]
        )
  
    | Arb opc => 
        (
          (* Can do tests for (in)equality on arbitrary precision values
             without checking the tags since the values we're comparing
             against are short constants. *)
          case opc of
            JE  => ()
          | JNE => ()
          | _   => (tagTest1 (r, cvec) : addrs; ())
          ;

          genImmed (CMP, r, tag c, cvec);
          [putConditional (opc, cvec)]
        )
  end;

  val inlineAssignments = true;
  
  (* Common code for div and mod word.  They are identical apart from
     getting the result. *)
  fun divmodWord(isDiv: bool, r1:reg, r2:reg, rd:reg, cvec) : unit =
        let
	      (* The divisor needs to be a different register from
		     either eax or edx.  It also needs to be different from
			 r1 since we're going to modify divReg before we load r1
			 into eax. *)
	   	  val divReg =
		  	if rd regNeq eax andalso rd regNeq edx
			then rd
			else if r1 regNeq ecx
			then ecx
			else esi
	   in
	 	 (* This is a bit complicated because the result is always placed
		    in the EDX:EAX register pair so we have to save one or both. *)
		 if rd regNeq eax
		 then genPush(eax, cvec) else ();
		 if rd regNeq edx
		 then genPush(edx, cvec) else ();
		 if divReg regNeq rd
		 then genPush(divReg, cvec) else ();
		 (* Untag, but don't shift, the divisor and dividend. *)
		 if r2 regEq eax
		 then
		    (
			 if divReg regEq r1
			 then raise InternalError "Assertion failed: Invalid registers"
			 else ();
			 (* We use move followed by substraction since that tests the
			    result for zero. *)
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
			  genmodrm (Based8, 2 (* call *), getReg ebp, cvec);
			  gen8u (MemRegRaiseDiv, cvec);
			  fixup (lab, cvec)
		  end;
		 (* Do the division. *)
         genReg  (XOR, edx, edx, cvec);
		 genop (Group3_A, cvec);
         genmodrm(Register, 6 (* div *), getReg divReg, cvec);
		 
		 if isDiv
		 then ( (* Tag the result into the result register. *)
			 genop (LEAL, cvec);
			 genIndexed (1, eax, eax, Scale1, rd, cvec)
		 )
		 else (* Add the tag back into the remainder. *)
			 genLeal (rd, edx, 1, cvec);

		 (* Restore the saved registers.  N.B. This also has
		    the effect of making sure that both eax and edx contain
			valid values. *)
		 if divReg regNeq rd
		 then genop(POP_R divReg, cvec) else ();
		 if rd regNeq edx
		 then genop(POP_R edx, cvec) else ();
		 if rd regNeq eax
		 then genop(POP_R eax, cvec) else ()
      end
 

  (* General register/register operation. *)
  fun genRR (inst, r1:reg, r2:reg, rd:reg, cvec) : unit =
  (
    if (rd regEq r1) orelse (rd regEq r2)
    then raise InternalError "Registers must be different"
    else ();
    
    case inst of
      instrMove => 
        (* Move from one register to another. r2 is ignored *)
        if rd regEq regHandler (* Not a real register. *)
        then genStore (r1, MemRegHandlerRegister, ebp, STORE_WORD, regNone, cvec)
        else genMove  (rd, r1, cvec)

   | instrPush =>
       (* Both rd and r2 are ignored. *)
       genPush (r1, cvec)
  
   | instrAddA =>
       (* Arbitrary precision addition. *)
       (* If either argument is long, or if both arguments are short
          but the result overflows, the code branchs to "addr". This
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
         val addr = tagTest2 (rd, r1, r2, false, cvec); (* generates code *)
       in
         (* Do the actual operation after removing a tag from one arg. *)
         genLeal (rd, r1, ~1, cvec);
         genReg  (ADD, rd, r2, cvec);
         genJO8  (addr, cvec)
       end
  
   | instrSubA =>
       (* Arbitrary precision subtraction. *)
       let
         val addr = tagTest2 (rd, r1, r2, false, cvec); (* generates code *)
       in
         (* Do the actual operation after removing a tag from one arg. *)
         genMove (rd, r1, cvec);
         genReg  (SUB, rd, r2, cvec);
         genLeal (rd, rd, 1, cvec); (* Put back the tag. *)
         genJO8  (addr, cvec)
       end
  
   | instrRevSubA =>
       (* Arbitrary precision subtraction. *)
       let
         val addr = tagTest2 (rd, r1, r2, false, cvec); (* generates code *)
       in
         (* Do the actual operation after removing a tag from one arg. *)
         genMove (rd, r2, cvec);
         genReg  (SUB, rd, r1, cvec);
         genLeal (rd, rd, 1, cvec); (* Put back the tag. *)
         genJO8  (addr, cvec)
       end
  
   | instrMulA =>
       (* Arbitrary precision multiplication. *)
       let
         val addr = tagTest2 (rd, r1, r2, false, cvec); (* generates code *)
       in
	 	 (* This is a bit complicated because the result is always placed
		    in the EDX:EAX register pair so we have to save one or both. *)
	     (* If the multiply overflows we need to be able to recover the
		    original arguments in order to emulate the instruction. *)
		 if rd regNeq eax
		 then genPush(eax, cvec) else ();
		 if rd regNeq edx
		 then genPush(edx, cvec) else ();
		 if r2 regEq edx
		 then
		 	(
			 (* Untag, but don't shift the multiplicand. *)
			 genLeal (eax, r1, ~1, cvec);
			 (* Shift down the multiplier. *)
	         genop (Group2_1_A, cvec);
	         genmodrm(Register, 7 (* sar *), getReg edx, cvec)
		    )
		 else (* r2 <> edx *)
		 	(
			 (* Shift down the multiplier. *)
			 if r1 regNeq edx
			 then genMove(edx, r1, cvec) else ();
	         genop (Group2_1_A, cvec);
	         genmodrm(Register, 7 (* sar *), getReg edx, cvec);
			 (* Untag, but don't shift the multiplicand. *)
			 genLeal (eax, r2, ~1, cvec)
		    );
		 (* Do the multiplication. *)
		 genop (Group3_A, cvec);
         genmodrm(Register, 5 (* imull *), getReg edx, cvec);
		 (* Add back the tag, but don't shift. *)
		 genLeal (rd, eax, 1, cvec);
		 (* Restore the saved registers.  N.B. This also has
		    the effect of making sure that both eax and edx contain
			valid values. *)
		 if rd regNeq edx
		 then genop(POP_R edx, cvec)
		 else ();
		 if rd regNeq eax
		 then genop(POP_R eax, cvec)
		 else ();
         genJO8  (addr, cvec) (* Check for overflow. *)
      end

    | instrAddW =>
       (* Fixed precision addition. (Doesn't test for overflow.) *)
       (
         (* Remove the tag from one argument, then add in the other. *)
		 (* This could be done using a single leal instruction:
		    leal rd,[r1+r2-1] *)
         genLeal (rd, r2, ~1, cvec);
         genReg  (ADD, rd, r1, cvec)
       )

   | instrSubW =>
       (* Fixed precision subtraction. (Doesn't test for overflow.) *)
       (
         genMove  (rd, r1, cvec);
         genReg   (SUB, rd, r2, cvec);
         genImmed (ADD, rd, 1, cvec)
       )

   | instrRevSubW =>
       (* Fixed precision subtraction. (Doesn't test for overflow.)  *)
       (
         genMove  (rd, r2, cvec);
         genReg   (SUB, rd, r1, cvec);
         genImmed (ADD, rd, 1, cvec)
       )

   | instrMulW =>
       (* Fixed precision multiplication. (Doesn't test for overflow.) *)
       (
	 	 (* This is a bit complicated because the result is always placed
		    in the EDX:EAX register pair so we have to save one or both. *)
		 if rd regNeq eax
		 then genPush(eax, cvec) else ();
		 if rd regNeq edx
		 then genPush(edx, cvec) else ();
		 if r2 regEq edx
		 then
		 	(
			 (* Untag, but don't shift the multiplicand. *)
			 genLeal (eax, r1, ~1, cvec);
			 (* Shift down the multiplier. *)
	         genop (Group2_1_A, cvec);
	         genmodrm(Register, 5 (* shr *), getReg edx, cvec)
		    )
		 else
		 	(
			 (* Shift down the multiplier. *)
			 if r1 regNeq edx
			 then genMove(edx, r1, cvec) else ();
	         genop (Group2_1_A, cvec);
	         genmodrm(Register, 5 (* shr *), getReg edx, cvec);
			 (* Untag, but don't shift the multiplicand. *)
			 genLeal (eax, r2, ~1, cvec)
		    );
		 (* Do the multiplication. *)
		 genop (Group3_A, cvec);
         genmodrm(Register, 4 (* mull *), getReg edx, cvec);
		 (* Add back the tag, but don't shift. *)
		 genLeal (rd, eax, 1, cvec);
		 (* Restore the saved registers.  N.B. This also has
		    the effect of making sure that both eax and edx contain
			valid values. *)
		 if rd regNeq edx
		 then genop(POP_R edx, cvec)
		 else ();
		 if rd regNeq eax
		 then genop(POP_R eax, cvec)
		 else ()
      )

   | instrDivW =>
       (* Fixed precision division. (Doesn't test for overflow.) *)
	   divmodWord(true, r1, r2, rd, cvec)

   | instrModW =>
       (* Fixed precision remainder. (Doesn't test for overflow.) *)
	   divmodWord(false, r1, r2, rd, cvec)

   | instrOrW =>
       (* Logical or. *)
       (
         genMove (rd, r1, cvec);
         genReg  (OR, rd, r2, cvec)
       )

   | instrAndW =>
       (* Logical and. *)
       (
         genMove (rd, r1, cvec);
         genReg  (AND, rd, r2, cvec)
       )

   | instrXorW =>
       (
         (* Must remove the tag from one argument. *)
         genLeal (rd, r2, ~1, cvec);
         genReg  (XOR, rd, r1, cvec)
       )

   | instrLoad =>
       (* Load a word. *)
       (
         (* The index is already multiplied by 2, so we need only multiply
            by two again to give a word offset.  Then we have to subtract
            2 to account for the tag. *)
         genop      (MOVL_A_R, cvec);
         genIndexed (~2, r1, r2, Scale2, rd, cvec)
       )

   | instrLoadB =>
       (* Load a byte. *)
       (        
         (* mov r2,rd; shrl $1,rd; movzl 0(r1,rd,1),rd; leal 1(,rd,2),rd *)
         genMove (rd, r2, cvec);
         
         genop (Group2_1_A, cvec);
         genmodrm(Register, 5 (* shr *), getReg rd, cvec);
         
         genop (ESCAPE, cvec);
         gen8u (11 * 16 + 6 (* movzl *), cvec);
         genIndexed (0, r1, rd, Scale1, rd, cvec);
         
         (* Tag the result. *)
         genTag (rd, cvec)
       )

   |  instrSetStringLength => (* Set the length word of a string. *)
	   (
	   (* The length is untagged. *)
       genop (Group2_1_A, cvec);
       genmodrm(Register, 5 (* shr *), getReg r2, cvec);

       genop (MOVL_R_A, cvec);
       genEA (0, r1, r2, cvec);

	   (* Restore the original value. This ensures we don't have a
	      bad value around and also restores the original value
		  since it may still be wanted. *)
       genTag(r2, cvec)
	   )

   | _ =>
      (* bad and unimplemented instrs *)
      raise InternalError "Unimplemented instruction"
  ); (* end genRR *)
    

  (* Register/immediate operations.  In many of these operations we have to tag
     the immediate value. *)
  fun genRI (inst, rs, constnt, rd, cvec) : unit =
  let
	(* log2 function for special cases of powers of 2. *)
	fun log2 n =
	let
		fun l2 i j =
			if i < n then l2 (i*2) (j+1) else if i = n then j
			else raise InternalError "Not a power of two"
	in
		if n <= 0 then raise InternalError "Not a power of two"
		else l2 1 0
	end
  in
    if rd regEq rs andalso (case inst of instrPush => false | _ => true) 
    then raise InternalError "Registers must be different"
    else ();
    
    case inst of 
      instrMove =>
	  	if isShort constnt
		then
	       (* Load a constant into a register. rs is ignored. *)
	       let
		     val c = toInt (toShort constnt);
	         val tagged = tag c;
	       in
	         genMoveI (rd, tagged, cvec)
	       end
		else
		(
	      genop  (MOVL_32_R rd, cvec);
		  addConstToVec (WVal constnt, false, cvec) (* Remember this constant and address. *)
		)

   | instrPush =>
   	   if isShort constnt
	   then
	       (* Both rd and rs are ignored. *)
	       let
		     val c = toInt (toShort constnt);
	         val tagged = tag c;
	       in
	         if not (is8Bit tagged)
	         then
	           (
	             genop (PUSH_32, cvec);
	             gen32s(tagged, cvec)
	           )
	         else
	           (
	            genop (PUSH_8, cvec);
	            gen8s (tagged, cvec)
	           )
	       end
	   else
		(
	      genop  (PUSH_32, cvec);
		  addConstToVec (WVal constnt, false, cvec) (* Remember this constant and address. *)
		)
  
    | instrAddA => 
        (* Arbitrary precision addition. *)
        let 
		  val c = toInt (toShort constnt);
          val addr = tagTest1 (rs, cvec);
        in
          genMove  (rd, rs, cvec);
          genImmed (ADD, rd,  semitag c, cvec);
          genJO8   (addr, cvec)
        end
  
    | instrSubA => 
        (* Arbitrary precision subtraction. *)
        let 
		  val c = toInt (toShort constnt);
          val addr = tagTest1 (rs, cvec);
        in
          genMove  (rd, rs, cvec);
          genImmed (SUB, rd, semitag c, cvec); (* needs AHL RTS *)
          genJO8   (addr, cvec)
        end


    | instrAddW => 
        (* Fixed precision addition - doesn't check for overflow. *)
        (* The argument is shifted but not tagged *)
        let
		  val c = toInt (toShort constnt)
		in
          genLeal (rd, rs, semitag c, cvec)
		end
  
    | instrSubW => 
        (* Fixed precision subtraction - doesn't check for overflow.  *)
        (* The argument is shifted but not tagged. *)
        let
		  val c = toInt (toShort constnt)
		in
          genLeal (rd, rs, ~ (semitag c), cvec)
		end
  
    (*  Now removed.  This is no longer safe now that we look for constants
	    in the code.
	| instrRevSubW => 
        (* Fixed precision reverse subtraction - doesn't check for overflow. *)
        (
          genMoveI (rd, semitag c + 2, cvec);
          genReg   (SUB, rd, rs, cvec)
        )
	*)
  
    | instrOrW => 
        (* Logical or. *)
        let
		  val c = toInt (toShort constnt);
          val tagged = tag c;
        in
          genMove  (rd, rs, cvec);
          genImmed (OR, rd, tagged, cvec)
        end
  
    | instrAndW => 
        (* Logical and. *)
        let
		  val c = toInt (toShort constnt);
          val tagged = tag c;
        in
          genMove  (rd, rs, cvec);
          genImmed (AND, rd, tagged, cvec)
        end
  
    | instrXorW => 
        (* Constant must be shifted but not tagged. *)
        let
		  val c = toInt (toShort constnt)
		in
          genMove  (rd, rs, cvec);
          genImmed (XOR, rd, semitag c, cvec)
        end

    | instrUpshiftW =>
		(* Word shift of more than 31 (unsigned) is defined to return zero
		   for the logical shifts and either 0 or all ones for the arithmetic
		   shift.  The i386 shift instructions mask the shift value instead. *)
        let
		  val c = toInt (toShort constnt)
		in
	        if c < 0 orelse c > 31
			then genMoveI (rd, tag 0, cvec)
			else
			let
			  fun power2 n = if n = 0 then 1 else 2 * power2 (n-1)
			in
	          genMove (rd, rs, cvec);
	          genop (Group2_8_A, cvec);
	          genmodrm(Register, 4 (* shl *), getReg rd, cvec);
			  gen8s (c, cvec);
			  (* We have shifted the tag bit along with everything else.
			     We need to subtract the shifted tag bit and a tag bit to
				 the end. *)
	          genLeal (rd, rd, 1 - power2 c, cvec)
	        end
		end

    | instrDownshiftW => 
        let
		  val c = toInt (toShort constnt)
		in
        if c < 0 orelse c > 31
		then genMoveI (rd, tag 0, cvec)
		else
		  (
          genMove (rd, rs, cvec);
          genop (Group2_8_A, cvec);
          genmodrm(Register, 5 (* shr *), getReg rd, cvec);
		  gen8s (c, cvec);
          genImmed (OR, rd, tag 0, cvec) (* Put in the tag *)
          )
		end

    | instrDownshiftArithW =>
		(* In this case it's easiest to set the shift to 31. *)
        let
		  val c = toInt (toShort constnt)
		in
          genMove (rd, rs, cvec);
          genop (Group2_8_A, cvec);
          genmodrm(Register, 7 (* sar *), getReg rd, cvec);
		  gen8s (if c < 0 orelse c > 31 then 31 else c, cvec);
          genImmed (OR, rd, tag 0, cvec) (* Put in the tag *)
        end
 
 	| instrMulA =>
		(* We only handle multiplication by two at the moment.  We
		   could handle a wider range but it's not that easy particularly
		   because the overflow flag is not defined on shifts of more than
		   one. *)
        let 
		  val c = toInt (toShort constnt)
          val addr = tagTest1 (rs, cvec);
        in
		  if c = 2 then () else raise InternalError "Multiply not implemented";
          (* Do the actual operation after removing a tag from one arg. *)
          genLeal (rd, rs, ~1, cvec);
          genReg  (ADD, rd, rs, cvec);
          genJO8  (addr, cvec)
        end

  	| instrMulW =>
		let
		  val c = toInt (toShort constnt)
			(* We only handle multiplication by powers of two at the moment.
			   This is easier than for arbitrary precision multiplication
			   because we don't have to detect overflow. *)
			val log2c = log2 c
		in
			if log2c = 0 (* Multiplying by one??? *)
			then genMove (rd, rs, cvec)
			else if log2c = 1
			then (* Multiplying by 2. *)
				(
				genop (LEAL, cvec);
				genIndexed (~1, rs, rs, Scale1, rd, cvec)
				)
			else if log2c = 2
			then (* Multiplying by 4 *)
				(
				genop (LEAL, cvec);
				genIndexed (~4, regNone, rs, Scale4, rd, cvec)
				)
			else (* Other powers of 2. *)
				(
				genMove (rd, rs, cvec);
				genop (Group2_8_A, cvec);
				genmodrm(Register, 4 (* shl *), getReg rd, cvec);
				gen8s (log2c, cvec);
				genLeal (rd, rd, 1-c, cvec) (* Remove the shifted tag and add the tag. *)
				)
        end

  	| instrDivW =>
		let
		  val c = toInt (toShort constnt)
			(* We only handle division by powers of two at the moment. *)
			val log2c = log2 c
		in
			if log2c = 0 (* Dividing by one??? *)
			then genMove (rd, rs, cvec)
			else (* Other powers of 2. *)
				(
				genMove (rd, rs, cvec);
				genop (Group2_8_A, cvec);
				genmodrm(Register, 5 (* shr *), getReg rd, cvec);
				gen8s (log2c, cvec);
				(* Set the tag bit, which may already be set as
				   a result of shifting a bit into it. *)
 		        genImmed (OR, rd, tag 0, cvec)
				)
        end

  	| instrModW =>
        let
		  val c = toInt (toShort constnt)
		  val ASSERT = log2 c (* Check it's a power of 2. *)
          val tagged = tag (c-1)
        in
          genMove  (rd, rs, cvec);
          genImmed (AND, rd, tagged, cvec)
        end

    | instrLoad => 
	    let
		  val c = toInt (toShort constnt)
		in
          (* Offset is words so multiply by 4 to get byte offset. *)
          genLoad (c * 4, rs, rd, cvec)
		end
  
    | instrLoadB => 
        (* Load a byte. *)
		let
		  val c = toInt (toShort constnt)
        in
          genop (ESCAPE, cvec);
          gen8u (11 * 16 + 6 (* movzx *), cvec);
          genEA (c, rs, rd, cvec);

          (* Tag the result. *)
          genTag (rd, cvec)          
        end
  
    | instrVeclen => 
        (
          genLoad  (~4, rs, rd, cvec);
          
          (* length only occupies the least significant 24 bits
             - the other bits are flags. *)
          genImmed (AND, rd, exp2_24 - 1, cvec);
 
          (* Tag the result. *)
          genTag (rd, cvec)
        )
  
    | instrVecflags => 
        (* Load the flags byte. *)
        (
          genop (ESCAPE, cvec);
          gen8u (11 * 16 + 6 (* movzx *), cvec);
          genEA (~1, rs, rd, cvec);

          (* Tag the result. *)
          genTag (rd, cvec)          
        )
  
    | instrGetFirstLong =>
		let
			(* Get the first word of a long integer.  We've already
			   checked that it is long. *)
			(* Test the "sign bit" of the object. *)
			val _ =
				(
		        genop    (Group3_a, cvec);
			    genmodrm(Based8, 0 (* test *), getReg rs, cvec);
			    gen8s (~1, cvec);
		        gen8u (16, cvec)
				)
			(* Load the unsigned, untagged, little-endian value. *)
			val _ = genLoad  (0, rs, rd, cvec);
			(* Skip if the sign bit wasn't set. *)
			val l1 = [putConditional (JE, cvec)]
		in
			genop(Group3_A, cvec);
			genmodrm(Register, 3 (* neg *), getReg rd, cvec);

			fixup(l1, cvec);
			genTag (rd, cvec)
		end

	| instrStringLength =>
		let
			(* If it's tagged the result is 1 otherwise we need to load
			   the length word and tag it. *)
			val l1 = compareAndBranchRI (rs, toWord 0 (* Unused *), Long, cvec)
			val _ = genMoveI (rd, tag 1, cvec);
			val l2 = unconditionalBranch cvec
		in
			fixup(l1, cvec);
			genLoad  (0, rs, rd, cvec); (* Load the length word. *)
			genTag (rd, cvec); (* And tag the result. *)
			fixup(l2, cvec)
		end

    | _ =>
       (* bad *)
       raise InternalError "Unimplemented instruction"
       
  end; (* genRI *)


  type cases = int * addrs;
  
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
  type jumpTableAddrs = addrs;
  
  fun constrCases (p as (i,a)) = p;
  
  type caseList = cases list;

  fun useIndexedCase (min:int, max:int, numberOfCases:int, exhaustive:bool) =
    isShort min andalso
    isShort max andalso
    numberOfCases > 7 andalso
    numberOfCases >= (max - min) div 3;

  fun indexedCase (r1:reg, r2:reg, min:int, max:int,
                  exhaustive:bool, cvec as Code{exited, ic, ...}) : jumpTableAddrs =
  let 
    val rangeCheck =
      if exhaustive then []
      else let
		val taggedMin = tag min;
		val taggedMax = tag max;
      in
		(* Is it long? *)
		genop (Group3_A, cvec);
		genmodrm(Register, 0(* test *), getReg r1, cvec);
		gen32u(1, cvec);
		
		(* Need to check whether the branch is in range. *)
		let
		  val l1 = putConditional (JE, cvec);
		
		  (* Compare with the minimum. *)
		  val UUU = genImmed(CMP, r1, taggedMin, cvec);
		  val l2 = putConditional (JL, cvec);
		  
		  (* Compare with the maximum. *)
		  val UUU = genImmed(CMP, r1, taggedMax, cvec);
		  val l3 = putConditional (JG, cvec);
		in
		  [l1, l2, l3]
		end
      end;
      
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
    genIndexed (min * ~8 - 4, r2, r1, Scale4, r2, cvec);
    (* Jump into the jump table.  Since each entry in the table
	   is 8 bytes long r2 will still be on a word + 2 byte
	   boundary. *)
	genop (Group5, cvec);
	genmodrm(Register, 4 (* jmp *), getReg r2, cvec);

	exited := true;
	(* There's a very good chance that we will now extend the branches for
	   the "out of range" checks.  The code to do that doesn't know
	   that all these branches will come to the same point so will generate three
	   separate long branches. We could combine them but it's hardly worth it. *)
	doPending (cvec,
		(max - min + 1) * 8 (* size of table. *) + 3 (* Maximum alignment *));
    
	    (* The start address must be on a two byte boundary so that the
		   address we've loaded is a valid code address. *)
	while getAddr (!ic) mod 4 <> 2 do genop (NOP, cvec);

	let
		fun initialiseTable i =
	  	 if i > max then () (* Done *)
		 else
		 	(
			gen8u (opToInt JMP_32, cvec);
			gen32u (0, cvec);
			(* Add no-ops to make it 8 bytes. *)
			gen8u (opToInt NOP, cvec);
			gen8u (opToInt NOP, cvec);
			gen8u (opToInt NOP, cvec);
			initialiseTable (i+1)
			)
      val here = !ic;
    in
      lab := here;
	  initialiseTable min;
      fixup (rangeCheck, cvec); (* The default case comes in here. *)
      here
    end
  end;

  fun makeJumpTable (startTab:jumpTableAddrs, cl:caseList, default:addrs, 
                     min : int, max : int, Code{codeVec, ...}) : unit =
  let
     fun putCase i addr =
	 let
	 	val addrOfJmp = startTab addrPlus ((i - min) * 8)
		val jumpOffset = (addr addrMinus addrOfJmp) - 5 (* From end of instr. *)
	 in
	 	set32s(jumpOffset, addrOfJmp addrPlus 1, codeVec)
	 end

	 (* Initialise to the default. *)
     fun putInDefaults i =
	 	if i <= max
		then (putCase i default; putInDefaults(i+1))
		else ()

	 (* Overwrite the defaults by the cases.  N.B.  We've generated
	    the list in reverse order so if we have any duplicates we
		will correctly overwrite the later cases with earlier ones. *)
	 fun putInCases [] = ()
	   | putInCases ((i, a) :: t) = (putCase i a; putInCases t)
	    
  in
    putInDefaults min;
    putInCases cl
  end;


  fun printCode (Code{procName, numOfConsts, pcOffset, constVec, ...}) seg endcode =
  let 
    val ptr = ref 0;
 
    infix 3 +:= ;
    fun (x +:= y) = (x := !x + (y:int));

    fun print32 () =
    let
      val valu = get32s (!ptr, seg); 
      val U : unit = (ptr +:= 4);
    in
	  if valu = tag 0 andalso !numOfConsts <> 0
	  then
	  	(* May be a reference to a code-segment we haven't generated yet.
		   In that case we try to print the name of the function rather
		   than simply printing "1".  It might be nice to print the
		   function name in other cases but that might be complicated. *)
	  let
		  val caddr = !ptr - 4
		  fun findRef [] = (* Not there - probably really tagged 0 *) printHex valu
		   |  findRef ((CVal(Code{procName, ...}), addr, _) :: rest) =
		   			if caddr = getAddr addr + ! pcOffset*4
					then print("=" ^ procName)
					else findRef rest
		   |  findRef (_ :: rest) = findRef rest
	  in
	  	  findRef(! constVec)
	  end
      else printHex valu
    end;

    fun get16s (a: int, seg: cseg) : int =
    let
      val b0  = toInt (csegGet (seg, a));
      val b1  = toInt (csegGet (seg, a + 1));
      val b1' = if b1 >= exp2_7 then b1 - exp2_8 else b1;
    in
      (b1' * exp2_8) + b0
    end;
 
    fun print16 () =
    let
      val valu = get16s (!ptr, seg); 
      val U : unit = (ptr +:= 2);
    in
      printHex valu
    end;

    fun print8 () =
    let
      val valu = get8s (!ptr, seg); 
      val U : unit = ptr +:= 1;
    in
      printHex valu
    end;
 
    fun printJmp () =
    let
      val valu = get8s (!ptr, seg); 
      val U : unit = ptr +:= 1;
    in
       printHex (valu + !ptr)
    end;
 
    (* Print an effective address. *)
    fun printEA  () =
    let
      val modrm = toInt (csegGet (seg, !ptr));
      val U : unit = (ptr +:= 1);
      val md = modrm div 64;
      val rm = modrm mod 8;
    in
      if md = 3
      then print (regRepr (mkReg rm))
      
      else if rm = 4
      then let (* s-i-b present. *)
        val sib = toInt (csegGet (seg, !ptr));
        val U : unit = (ptr +:= 1);
        val ss    = sib div 64;
        val index = (sib div 8) mod 8;
        val base   = sib mod 8;
      in
        if md = 1
          then print8 ()
        else if md = 2 orelse base = 5 (* andalso md=0 *) 
          then print32 ()
        else ();
          
        print "(";
        
        if md <> 0 orelse base <> 5
        then print (regRepr (mkReg base))
        else ();
        
        if index <> 4 (* No index. *)
          then 
            print ("," ^ regRepr (mkReg index) ^ 
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
         if md = 1
           then print8 ()
         else if md = 2 
           then print32 ()
         else ();
         
         print ("(" ^ regRepr (mkReg rm) ^ ")")
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
      val U : unit = printHex (!ptr); (* The address. *)
      val U : unit = print "\t";

      val opByte : int = get8u (!ptr, seg);
      val U : unit = ptr +:= 1;
    in
      if opByte = opToInt Group1_8_A orelse 
         opByte = opToInt Group1_32_A
      then let
        (* Opcode is determined by next byte. *)
        val nb = toInt (csegGet (seg, !ptr));
      in
        printArith ((nb div 8) mod 8);
        print "_rev\t";
        printEA (); (* These are the wrong way round. *)
        print ",";
        if opByte = opToInt Group1_8_A
        then print8 ()
        else print32 ()
      end : unit
         
      else if opByte = opToInt JE
        then (print "je\t"; printJmp()) : unit

      else if opByte = opToInt JNE
        then (print "jne\t"; printJmp()) : unit

      else if opByte = opToInt JO
        then (print "jo\t"; printJmp()) : unit

      else if opByte = opToInt JL
        then (print "jl\t"; printJmp()) : unit

      else if opByte = opToInt JG
        then (print "jg\t"; printJmp()) : unit

      else if opByte = opToInt JLE
        then (print "jle\t"; printJmp()) : unit

      else if opByte = opToInt JGE
        then (print "jge\t"; printJmp()) : unit

      else if opByte = opToInt JB
        then (print "jb\t"; printJmp()) : unit

      else if opByte = opToInt JA
        then (print "ja\t"; printJmp()) : unit

      else if opByte = opToInt JNA
        then (print "jna\t"; printJmp()) : unit

      else if opByte = opToInt JNB
        then (print "jnb\t"; printJmp()) : unit

      else if opByte = opToInt JMP_8
        then (print "jmp\t"; printJmp()) : unit

      else if opByte = opToInt JMP_32
      then let
        val valu     = get32s (!ptr, seg);
        val U : unit = (ptr +:= 4);
      in
        print "jmp\t";
        printHex (!ptr + valu)
      end : unit
         
      else if opByte = opToInt CALL_32
      then let
        val valu     = get32s (!ptr, seg);
        val U : unit = (ptr +:= 4);
      in
        print "call\t";
        printHex (!ptr + valu)
      end : unit
         
      else if opByte = opToInt MOVL_A_R
      then let
        (* Register is in next byte. *)
        val nb = toInt (csegGet (seg, !ptr));
        val reg = (nb div 8) mod 8;
      in
        print "movl\t";
        printEA ();
        print ",";
        print (regRepr (mkReg reg))
      end : unit
         
      else if opByte mod 8 = 3 andalso
              opByte < 3 * 16 + 15 (* 0x3f *)
      then let
        (* Register is in next byte. *)
        val nb = toInt (csegGet (seg, !ptr));
        val reg = (nb div 8) mod 8;
      in
        printArith ((opByte div 8) mod 8);
        print "\t";
        printEA ();
        print ",";
        print (regRepr (mkReg reg))
      end : unit
         
      else if opByte = opToInt MOVL_R_A
      then let
        (* Register is in next byte. *)
        val nb = toInt (csegGet (seg, !ptr));
        val reg = (nb div 8) mod 8;
      in
        print "movl\t";
        print (regRepr (mkReg reg));
        print ",";
        printEA ()
      end : unit

      else if opByte = opToInt MOVB_R_A
      then let
        (* Register is in next byte. *)
        val nb = toInt (csegGet (seg, !ptr));
        val reg = (nb div 8) mod 8;
      in
        print "movb\t";
		case reg of
			0 => print "%al"
		|	1 => print "%cl"
		|	2 => print "%dl"
		|	3 => print "%bl"
		|	4 => print "%ah"
		|	5 => print "%ch"
		|	6 => print "%dh"
		|	7 => print "%bh"
		|	_ => print ("r" ^ Int.toString reg);
        print ",";
        printEA ()
      end : unit


      else if opByte >= opToInt (PUSH_R (mkReg 0)) andalso
              opByte <= opToInt (PUSH_R (mkReg 7))
        then print ("pushl\t" ^  regRepr (mkReg (opByte mod 8))) : unit
      
      else if opByte >= opToInt (POP_R (mkReg 0)) andalso
              opByte <= opToInt (POP_R (mkReg 7))
        then print ("pop\t" ^ regRepr (mkReg (opByte mod 8))) : unit
      
      else if opByte = opToInt NOP
        then print "nop" : unit
      
      else if opByte = opToInt LEAL
      then let
        (* Register is in next byte. *)
        val nb = toInt (csegGet (seg, !ptr));
        val reg = (nb div 8) mod 8;
      in
        print "leal\t";
        printEA ();
        print ",";
        print (regRepr (mkReg reg))
      end : unit

      else if opByte >= opToInt (MOVL_32_R eax) andalso
              opByte <= opToInt (MOVL_32_R edi)
      then
        (
          print "movl\t";
          print32 ();
          print("," ^ regRepr (mkReg (opByte mod 8)))
        ) : unit
         
      else if opByte = opToInt MOVL_32_A
      then
        (
          print "movl_rev\t";
          printEA (); (* These are the wrong way round. *)
          print ",";
          print32 ()
        ) : unit
         
      else if opByte = opToInt MOVB_8_A
      then
        (
          print "movb_rev\t";
          printEA (); (* These are the wrong way round. *)
          print ",";
          print8 ()
        ) : unit
         
      else if opByte = opToInt PUSH_32
        then (print "push\t"; print32 ()) : unit
         
      else if opByte = opToInt PUSH_8
        then (print "push\t"; print8 ()) : unit
         
      else if opByte = opToInt Group5
      then let
        (* Opcode is determined by next byte. *)
        val nb = toInt (csegGet (seg, !ptr));
        val opc = (nb div 8) mod 8;
      in
        print
          (case opc of
             2 => "call"
           | 4 => "jmp"
           | 6 => "push"
           | _ => "???"
          );
        print "\t";
        printEA ()
      end : unit
         
      else if opByte = opToInt Group3_A
      then let
        (* Opcode is determined by next byte. *)
        val nb = toInt (csegGet (seg, !ptr));
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
      end : unit
         
      else if opByte = opToInt Group3_a
      then let
        (* Opcode is determined by next byte. *)
        val nb = toInt (csegGet (seg, !ptr));
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
      end : unit
         
      else if opByte = opToInt Group2_8_A
      then let
        (* Opcode is determined by next byte. *)
        val nb = toInt (csegGet (seg, !ptr));
        val opc = (nb div 8) mod 8;
      in
        print
          (case opc of
             4 => "shl"
           | 5 => "shr"
           | 7 => "sar"
           | _ => "???"
          );
        print "\t";
        printEA (); (* These are the wrong way round. *)
        print ",";
        print8 ()
      end : unit
         
      else if opByte = opToInt Group2_1_A
      then let
        (* Opcode is determined by next byte. *)
        val nb = toInt (csegGet (seg, !ptr));
        val opc = (nb div 8) mod 8;
      in
	 print
	   (case opc of
	      5 => "shr"
		| 7 => "sar"
	    | _ => "???"
	   );
	 print "\t1,";
	 printEA ()
       end : unit
	  
       else if opByte = opToInt ESCAPE
       then let
	 (* Opcode is in next byte. *)
	 val opByte2  = toInt (csegGet (seg, !ptr));
	 val U : unit = (ptr +:= 1);
       in
	if opByte2 = 11 * 16 + 6 (* 0xb6 *)
	then let
	  val nb = toInt (csegGet (seg, !ptr));
	  val reg = (nb div 8) mod 8;
	in
	  print "movzl\t";
	  printEA ();
	  print ",";
	  print (regRepr (mkReg reg))
	end : unit
	   
	else if opByte2 >= 8 * 16      (* 0x80 *) andalso
	        opByte2 <= 8 * 16 + 15 (* 0x8f *)
	then let
	  val valu = get32s (!ptr, seg);
	  val U : unit = (ptr +:= 4);
        in
	  print
	    (if opByte2 = 8 * 16 (* 0x80 *)
	       then "jo\t"
	     else if opByte2 = 8 * 16 + 4  (* 0x84 *)
	       then "je\t"
	     else if opByte2 = 8 * 16 + 5  (* 0x85 *)
	       then "jne\t"
	     else if opByte2 = 8 * 16 + 12 (* 0x8c *)
	       then "jl\t"
	     else if opByte2 = 8 * 16 + 13 (* 0x8d *)
	       then "jge\t"
	     else if opByte2 = 8 * 16 + 14 (* 0x8e *)
	       then "jle\t"
	     else if opByte2 = 8 * 16 + 15 (* 0x8f *)
	      then "jg\t" 
	     else if opByte2 = 8 * 16 +  2 (* 0x82 *)
	       then "jb\t"
	     else if opByte2 = 8 * 16 +  3 (* 0x83 *)
	       then "jnb\t"
	     else if opByte2 = 8 * 16 +  6 (* 0x86 *)
	       then "jna\t"
	     else if opByte2 = 8 * 16 +  7 (* 0x87 *)
	      then "ja\t" 
	     else "???\t"
	    );
	  printHex (!ptr + valu)
	end : unit
	   
	else (print "esc\t"; printHex opByte2) : unit
      end (* ESCAPE *)
         
      else if opByte = opToInt POP_A
        then (print "pop\t"; printEA ()) : unit
         
      else if opByte = opToInt RET 
        then print "ret" : unit
      
      else if opByte = opToInt STC
        then print "stc" : unit
         
      else if opByte = opToInt RET_16
        then (print "ret\t"; print16 ()) : unit

	  else if opByte = opToInt TEST_ACC8
	    then (print "testb\t%al,"; print8 ())

       else printHex opByte : unit;
      
      print "\n" : unit
    end; (* end of while loop *)
    print "\n"

  end (* printCode *);

  (* constLabels - fill in a constant in the code. *)
  fun constLabels (Code{resultSeg=ref rseg, pcOffset=ref offset, ...},
  				   addr : addrs, value : word, isRel: bool) : unit =
  let
    val seg       = scSet rseg; (* The address of the segment. *)
    val constAddr = addr addrPlus offset*4;
  in
    csegPutConstant (seg, getAddr constAddr, value, isRel)
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
		  noc := !noc - 1
		  )
		else ()
	  |  putConst _ = ();
	
    in
      (* look down its list of forward references until we find ourselves. *)
      List.app putConst constVec;
      (* If this function has no more references we can lock it. *)
      if !noc = 0
      then csegLock (scSet resultSeg)
      else ()
    end (* fixRef *);
  in
    (* For each `code' which needs a forward reference to `refTo' fixing up. *)
    List.app fixRef otherCodes
  end; (* fixOtherRefs *)

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
  fun copyCode (cvec as
  					Code{pcOffset,
						 codeVec,
						 noClosure,
						 selfCalls = ref selfCalls,
						 selfJumps = ref selfJumps,
						 mustCheckStack = ref callsAProc,
						 numOfConsts,
						 ic,
						 constVec = ref constVec,
						 resultSeg,
						 procName,
						 ...},
  			    stackRequired, registerSet) : address =
  let
    
    (* This aligns ic onto a fullword boundary. *)
    val U : unit   = align (0, cvec);
    val endic      = !ic; (* Remember end *)
    val U : unit   = gen32u (0, cvec); (* Marker - 0 (changes !ic) *)

    (* Prelude consists of 
       1) nops to make it a whole number of words
       2) code to set the pointer to the constant area. 
       3) stack checking code
    *)
    local
      (* little-endian *)
      fun getBytes (0, x) = []
        | getBytes (n, x) = (x mod exp2_8) :: getBytes (n - 1, x div exp2_8);
		
	  fun testRegAndTrap (reg, entryPt) =
	     [
		      (* cmp reg,16(%ebp)*)
			  opToInt(Arith (CMP, 3)),
		      modrm (Based8, getReg reg, getReg ebp),
		      MemRegStackLimit,
			  (* jnb 3 *)
			  opToInt JNB,
			  3,
			  (* call *)
			  opToInt Group5,
			  modrm (Based8, 2 (* call *), getReg ebp),
			  entryPt
		 ];

      val stackCheckCode : int list =
        if stackRequired >= minStackCheck
        then let
          val stackByteAdjust = ~4 * stackRequired;
          val loadEdiCode : int list =
            if is8Bit stackByteAdjust
            then
	 	      [
				opToInt LEAL,
				modrm (Based8, getReg edi, 4), (* Need s-i-b byte for %esp *)
				sib (Scale1, 4 (* no index *), getReg esp)
		      ] 
		      @ getBytes (1, stackByteAdjust)
		      
	            else
		      [
				opToInt LEAL,
				modrm (Based32, getReg edi, 4), (* Need s-i-b byte for %esp *)
				sib (Scale1, 4 (* no index *), getReg esp)
		      ] 
		      @ getBytes (4, stackByteAdjust);
	      
		  val testEdiCode : int list =
		  	testRegAndTrap (edi, MemRegStackOverflowCallEx)
        in
          (* leal -stack_reqd(%esp),%edi; boundl %edi,16(%ebp); *)
          loadEdiCode @ testEdiCode
           (* The effect of this sequence is to generate an
             overflow trap if sp < sl *)
        end
         
        else if callsAProc (* check for user interrupt *)
        then testRegAndTrap (esp, MemRegStackOverflowCall)
	           
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

		 val nopCode : int list =
		    let
			(* Add sufficient No-ops to round this to a full word. *)
			    val len = length stackCheckCode mod wordSize
			in
			    if len = 0
				then []
				else List.tabulate(wordSize - len, fn _ => opToInt NOP)
			end
		    
     in
 		val preludeCode = nopCode @ stackCheckCode;
		val wordsForPrelude = length preludeCode div wordSize

 	   (* +4 for code size, profile count, function name and constants count *)
		 (* +1 for register mask. *)
	   val segSize = (getAddr (!ic)) div wordSize + wordsForPrelude + 4 + 1;
       
      (* byte offset of L2 label relative to start of post-prelude code. *)
      val L2Addr = mkAddr (~ (length stackCheckCode));
    end; (* local *)

    (* fix-up all the self-calls *)
    val U : unit = 
      fixRecursiveCalls    (cvec, L2Addr, selfCalls);
       
    val U : unit =
      fixRecursiveBranches (cvec, L2Addr, selfJumps);

    (* Now make the byte segment that we'll turn into the code segment *)
    val seg : cseg = csegMake segSize;
    val offset     = wordsForPrelude;
    
    val _ = resultSeg := Set seg;
    
    (* Copy the code into the new segment. *)
    val _ = pcOffset := offset;
    val _ = csegCopySeg (codeVec, seg, getAddr (! ic), offset);

    (* insert prelude code into segment *)
    local
      val ptr = ref 0;
      (* Generate the prelude. *)
      fun putPrelude (b: int) : unit =
      let
        val a = !ptr
      in
        csegSet (seg, a, toShort b);
        ptr := a + 1
      end;

      fun putPreludeList []      = ()
        | putPreludeList (w::ws) = (putPrelude w; putPreludeList ws);
    in
      val U : unit = putPreludeList preludeCode
    end;
    
    local
      val endOfCode (* words *) = (getAddr (! ic)) div 4 + offset;
    in
      (* Byte offset of start of code. *)
      local
        val byteEndofcode = endOfCode * 4;
        val addr = mkAddr byteEndofcode;
      in
        val U : unit = set32u (byteEndofcode, addr, seg) 
      end;
      
      (* Put in the number of constants. This must go in before we actually put
         in any constants. There are now only two constants: the function name
		 and the register mask. All other constants are in the code. *)
      local
        val addr = mkAddr ((endOfCode + 3 + 1) * 4);
      in
        val U : unit = set32u (2, addr, seg) 
      end;
      
      (* Next the profile count. *)
      local
        val addr = mkAddr ((endOfCode + 1) * 4);
      in
        val U : unit = set32u (0, addr, seg) 
      end;
      
      (* Now we've filled in all the C integers; now we need to convert the segment
         into a proper code segment before it's safe to put in any ML values.
         SPF 13/2/97
      *)
      val U : unit = csegConvertToCode seg;

      local
        (* why do we treat the empty string as a special case? SPF 15/7/94 *)
        (* This is so that profiling can print "<anon>". Note that a
           tagged zero *is* a legal string (it's "\000"). SPF 14/10/94 *)
        val nameWord : word = if procName = "" then toWord 0 else toWord procName;
      in
        val _ = csegPutWord (seg, endOfCode + 2, nameWord);
      end;
      local
	    (* Encode the register mask.  This encoding must be the same
		   as the one used for assembly code segments. *)
		fun encodeReg(r, n: short): short =
		let
		    infix << Or
			val reg = toShort 1 << toShort (nReg r)
		in
			reg Or n
		end
		val regSet = List.foldl encodeReg (toShort 0) registerSet
      in
        val U : unit = csegPutWord (seg, endOfCode + 3, toWord regSet);
      end;
    end;  (* scope of endOfCode *)
  in 
    let
      (* and then copy the objects from the constant list. *)
      fun putConst (WVal c, addr, isRel) =
            ( (* Can put these in now. *)
              constLabels (cvec, addr, c, isRel);
              numOfConsts := ! numOfConsts - 1
            )

		| putConst (HVal(ref hv), addr, isRel) =
          let
            (* on the PC, we don't add the extra 2 (we do on the Sparc) *)
            (* SPF 24/4/95 *)
            val handlerByteOffset = getAddr hv + offset * 4;
            (* The following comment applies to offsetAddr *)
            (* Special function to add to an address.
               This only works if the resulting value is 
               in a code segment and is on a word + 2 byte boundary. *)
            val handlerAddr : handler = 
              offsetAddr (csegAddr seg, toShort handlerByteOffset);
          in
		    constLabels (cvec, addr, toWord handlerAddr, isRel);
            numOfConsts := ! numOfConsts - 1
          end

          (* forward-reference - fix up later when we compile
             the referenced code *) 
		| putConst (CVal _, _, _) = ()

      val _ = List.app putConst constVec;
    
      (* Switch off "mutable" bit now if we have no
         forward or recursive references to fix-up *)
      val _ = if ! numOfConsts = 0 then csegLock seg else ();
  
      (* Do we need to make a closure, or just return the code? *)
      val addr : address =
        if noClosure
        then csegAddr seg
        else let
          val addr : address = alloc (short1, F_words, toWord (csegAddr seg));
          
          (* Logically unnecessary; however the RTS currently allocates everything
             as mutable because Dave's code assumed that things were done this
             way and I'm not completely sure that everything that needs a mutable
             allocation actually asks for it yet. SPF 19/2/97
          *)
          val U : unit = lock addr;
        in
          addr
        end
  
      (* Now we know the address of this object we can fix up
         any forward references outstanding. This is put in here
         because there may be directly recursive references. *)
      val U : unit = fixOtherRefs (cvec, toWord addr);
  
      val U : unit = 
		if !assemblyCode
		then (* print out the code *)
		  (
		  printCode cvec seg ((getAddr endic) + offset * 4);
		  print "Register set = [";
		  List.app (fn r => (print " "; regPrint r)) registerSet;
		  print "]\n\n"
		  )
		else ();
    in
      addr 
    end (* the result *)
  end (* copyCode *);

  (* ic function exported to gencode. Currently only used for backward jumps. *)
  fun ic (cvec as Code {exited, ic=ic', branchCheck, ...}) =
  ( (* Make sure any pending operations are done. *)
    doPending (cvec, 0);
    exited := false; (* We may be jumping here. *)
	branchCheck := !ic';
    ! ic' (* After any pending operations. *)
  );

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
     procName ^ ":" ^ Int.fmt StringCvt.HEX (getAddr ic)

end (* struct *)

end (* CODECONS *);
