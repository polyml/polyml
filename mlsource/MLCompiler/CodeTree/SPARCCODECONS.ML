(*
    Copyright (c) 2000-9
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
   This module contains all the definitions of the Sparc opcodes and registers.
   It uses "codeseg" to create and operate on the segment itself.
 *)

(*
 Linkage conventions:
We ignore the register window and use only the %o, %l and %g register sets.
%i0, the original parameter to the C code, points to a memRegisters structure.
%i1 is currently unused
%i2 and %i3 are unsaved scratch registers.  The difference is that if %i2 is the
target of a sethi/add combination the value is assumed to be an address and can be
updated by the GC whereas if it is %i3 it is not.
%i4 and %i5 are saved unchecked registers.
%i6 and %i7 are never used (%i7 is the return address from ML to C).
%o0 is used for the first argument to a function, and for the result.
%o1-%o3 are used for the next 3 args, any others being passed on the stack.
%o4 is the address of the code being called
%o5 is the closure pointer or static link pointer.
%o6 is never used (the system assumes it points to an area where regs can be stored).
%o7 is the return address i.e. the address of the jmpl instruction.
Return is done by jmp %o7+6. 2 is always added to %o7 on function entry
so that it is distinguishable from other addresses which point at the
start of objects. 
%g3 points to the top exception handler,
%g4 is the stack pointer,
%g5 is the heap limit - previously the stack limit register,
%g6 is the heap pointer,
%g7 is unused. This is used by the pthread library.

%g1 and %g2 are available as work regs, as are the %l registers.

%i4 is used an the arithmetic accumulator, which should contain
only untagged values. By reserving it for this function, we can
hope to minimise the number of untagging operations performed via
a simple caching scheme.

%i5 is used both for untagged arithmetic and as a general scratch
register.

%i4 and %i5 are not always preserved across traps, and
they should never contain pointers, as they are not
examined/adjusted by the garbage-collector.

%i3 is used for stack adjustments, so we don't have to call
doPendingStackAdjustment before we load an immediate.

Other registers MUST contain properly tagged values if there any a possibility
of a garbage collection.
*)

functor SPARCCODECONS (

(*****************************************************************************)
(*                  DEBUG                                                    *)
(*****************************************************************************)
structure DEBUG :
sig
    val assemblyCodeTag : bool Universal.tag
    val getParameter :
       'a Universal.tag -> Universal.universal list -> 'a
end;

(*****************************************************************************)
(*                  PRETTY for compilerOutTag                                *)
(*****************************************************************************)
structure PRETTY: PRETTYSIG

) :

(*****************************************************************************)
(*                  CODECONS export signature                                *)
(*****************************************************************************)
sig
  type machineWord;
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
  
  val regRepr: reg -> string;

  type addrs

  val codeCreate: bool * string * Universal.universal list -> code;  (* makes the initial segment. *)

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
  val instrGetFirstLong:    instrs;
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
  val instrIsRR: instrs -> bool;        (* Is the general form implemented? *)
  val instrIsRI: instrs * machineWord -> bool; (* Is the immediate value ok? *)

  (* Code generate operations. *)
  val genRR: instrs * reg * reg * reg * code -> unit;
  val genRI: instrs * reg * machineWord * reg * code -> unit;

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
  val isCompRI: tests * machineWord -> bool;
  
  val compareAndBranchRR: reg * reg * tests * code -> labels;
  val compareAndBranchRI: reg * machineWord * tests * code -> labels;

  datatype storeWidth = STORE_WORD | STORE_BYTE

  val genLoad:        int * reg * reg * code -> unit;
  val isIndexedStore: storeWidth -> bool
  val genStore:       reg * int * reg * storeWidth * reg * code -> unit;
  val isStoreI:       machineWord * storeWidth * bool -> bool;
  val genStoreI:      machineWord * int * reg * storeWidth * reg * code -> unit;
  val inlineAssignments: bool
  val genPush:        reg * code -> unit;
  val genLoadPush:    int * reg * code -> unit;
  val preferLoadPush: bool;
  val genLoadCoderef: code * reg * code -> unit;

  val allocStore:      int * Word8.word * reg * code -> unit;
  val setFlag:         reg * code * Word8.word -> unit;
  val completeSegment: code -> unit;

  datatype callKinds =
        Recursive
    |   ConstantFun of machineWord * bool
    |   CodeFun of code
    |   FullCall
  
  val callFunction:       callKinds * code -> unit;
  val jumpToFunction:     callKinds * reg * code -> unit;
  val returnFromFunction: reg * int * code -> unit;
  val raiseException:     code -> unit;
  val genStackOffset: reg * int * code -> unit;

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
  val codeAddress: code -> address option
  
  val traceContext: code -> string;
end (* CODECONS export signature *) =

(*****************************************************************************)
(*                  CODECONS functor body                                    *)
(*****************************************************************************)
struct                                                   
  open CodeSeg;
  open DEBUG;
  open Address;
  open Misc;

  val toInt = Word.toIntX (* This previously just cast the value so continue to treat it as signed. *)
  
  fun applyCountList (_, _, [])   = ()
    | applyCountList (f, n, h::t) = 
    let
      val () = f (n, h);
    in
      applyCountList (f, n + 1, t)
    end;
  
  (* added SPF - take numbers OUT of code *)
  (* These are defined here as explicit constants, so the     *)
  (* code-generator can in-line them as immediates (I think). *)
  val exp2_2  =          4;
  val exp2_3  =          8;
  val exp2_4  =         16;
  val exp2_5  =         32;
  val exp2_6  =         64;
  val exp2_7  =        128;
  val exp2_8  =        256;
  val exp2_10 =       1024;
  val exp2_12 =       4096;
  val exp2_13 =       8192;
  val exp2_14 =      16384;
  val exp2_16 =      65536;
  val exp2_19 =     524288;
  val exp2_21 =    2097152;
  val exp2_22 =    4194304;
  val exp2_24 =   16777216;
  val exp2_25 =   33554432;
  val exp2_29 =  536870912;
  val exp2_30 = 1073741824;
  val exp2_31 = 2147483648;
  val exp2_32 = 4294967296;
    
  (* Let's check that we got them right! *)
  local
    fun exp2 0 = 1
      | exp2 n = 2 * exp2 (n - 1);
  in
    val _ : bool = 
      (
        exp2_2  = exp2 2  andalso
        exp2_3  = exp2 3  andalso
        exp2_4  = exp2 4  andalso
        exp2_5  = exp2 5  andalso
        exp2_6  = exp2 6  andalso
        exp2_7  = exp2 7  andalso
        exp2_8  = exp2 8  andalso
        exp2_10 = exp2 10 andalso
        exp2_12 = exp2 12 andalso
        exp2_13 = exp2 13 andalso
        exp2_14 = exp2 14 andalso
        exp2_16 = exp2 16 andalso
        exp2_19 = exp2 19 andalso
        exp2_21 = exp2 21 andalso
        exp2_22 = exp2 22 andalso
        exp2_24 = exp2 24 andalso
        exp2_25 = exp2 25 andalso
        exp2_29 = exp2 29 andalso
        exp2_30 = exp2 30 andalso
        exp2_31 = exp2 31 andalso
        exp2_32 = exp2 32 
      )
         orelse raise InternalError "CodeCons: Powers of 2 incorrectly specified";
  end;

  val mask2bits = 0w3;   (* least significant 2 bits *)
  val mask5bits = 0w31;  (* least significant 5 bits *)
  val mask6bits = 0w63;  (* least significant 6 bits *)
  val mask8bits = 0w255; (* least significant 8 bits *)
  val >> = Word.>> and << = Word.<<
  infix >> <<;

  (* These are defined here as explicit constants, so the     *)
  (* code-generator can in-line them as immediates (I think). *)
  val TAGSHIFT   = 2;
  val TAGMASK    = 3;
  val DATATAG    = 1;
  val CODETAG    = 2;

  (* tag a short constant *)
  fun semiTagged c   = exp2_2 * c;
  fun tagged c       = exp2_2 * c + 1;

(*****************************************************************************)
(*                  Abstype for registers                                    *)
(*****************************************************************************)
  infix 7 regEq regNeq regLeq regGeq regMinus;
 
  abstype reg = Reg of int  (* registers. *)
  with
    val g0 = Reg  0;  
    val g1 = Reg  1;  
    val g2 = Reg  2;
    val g3 = Reg  3;  
    val g4 = Reg  4;  
    val g5 = Reg  5;
    val g6 = Reg  6;  
    val g7 = Reg  7;
    val o0 = Reg  8;  
(*  val o1 = Reg  9;  
    val o2 = Reg 10;
    val o3 = Reg 11; 
    val o4 = Reg 12; *)
    val o5 = Reg 13; 
(*  val o6 = Reg 14; *)
    val o7 = Reg 15;
    val l0 = Reg 16; 
(*  val l1 = Reg 17; 
    val l2 = Reg 18;
    val l3 = Reg 19; 
    val l4 = Reg 20; 
    val l5 = Reg 21; 
    val l6 = Reg 22; *)
    val l7 = Reg 23;
    val i0 = Reg 24; 
(*  val i1 = Reg 25; *)
    val i2 = Reg 26;
    val i3 = Reg 27; 
    val i4 = Reg 28; 
    val i5 = Reg 29; 
(*  val i6 = Reg 30; 
    val i7 = Reg 31;*)

    val psp      = g4;
    val phr      = g3;
    val regResult   = o0;
    val regClosure  = o5;
(*  val regCode     = NONE;*)
    val regStackPtr = g4;
    val regHandler  = g3;
    val regReturn   = o7;
    val regNone     = g0;

    fun getReg (Reg r) = r;      (* reg.down *)
    fun mkReg   n      = Reg n;  (* reg.up   *)
  
    (* The number of general registers.  Includes result, closure, code, return and
       arg regs but not stackptr, handler, stack limit or heap ptrs. *)
    val regs = 17 (* o0-o5, o7-l7, g1 and g2 *);

    (* The nth register (counting from 0). *)
    fun regN i =
      if i < 0 orelse i >= regs
      then let
        val msg =
          concat
            [
              "regN: Bad register number ",
              Int.toString i
            ]
      in
        raise InternalError msg
      end
      else if i = 6 then g1 (* replace o6 *)
      else if i < 16 then mkReg (i + 8) (* o0 - l7 *)
      else g2 (* i = 16 *);
    
         
    fun a regEq  b = getReg a  = getReg b;
    fun a regNeq b = getReg a <> getReg b;
    fun a regLeq b = getReg a <= getReg b;
    fun a regGeq b = getReg a >= getReg b;
    fun (Reg a) regMinus (Reg b) = a - b;
  
    (* The number of the register. *)
    fun nReg r =
      if r regEq g1 then 6
      else if r regGeq o0 andalso r regLeq l7 then getReg r - 8
      else if r regEq g2 then 16
      else let
        val msg =
          concat
            [
              "nReg: Bad register number ",
              Int.toString (getReg r)
            ]
      in
        raise InternalError msg
      end;

    fun regRepr r = 
    ( if r regEq psp then "psp" else if r regEq phr then "phr"
      else if r regLeq g7 then "g" ^ Int.toString (getReg r)      (* use gn *)
      else if r regLeq o7 then "o" ^ Int.toString (r regMinus o0) (* use on *)
      else if r regLeq l7 then "l" ^ Int.toString (r regMinus l0) (* use ln *)
      else "i" ^ Int.toString (r regMinus i0) (* use in *)
    );
         
    val argRegs = 4;

    (* Args 0, 1, 2, 3 correspond to o0, o1, o2, o3. *)
    fun argReg i =
      if i < 4 andalso i >= 0 then mkReg (i + 8)
      else let
        val msg =
          concat
            [
              "argReg: bad register number ",
              Int.toString i
            ]
      in
        raise InternalError msg
      end;
  end; (* reg *)

  datatype CacheState =
    Empty        (* i4 contains no useful value *)
  | Copy of reg; (* i4 contains an untagged copy of reg *)


(*****************************************************************************)
(*                  Abstype for instruction addresses                        *)
(*****************************************************************************)
  infix 6 wordAddrPlus wordAddrMinus;
  infix 4 addrLt;

  (* All indexes into the code vector have type "addrs".
     In this version of the code generator, we're using
     WORD addresses. Earlier versions use BYTE addresses,
     so don't get confused!. SPF 18/2/97
  *)
  abstype addrs = Addr of int
  with
    (* + is defined to add an integer to an address *)
    fun (Addr a) wordAddrPlus b = Addr (a + b);
    
    (* The difference between two addresses is an integer *)
    fun (Addr a) wordAddrMinus (Addr b) = a - b; 

    fun (Addr a) addrLt (Addr b) = a < b; 

    fun mkWordAddr n = Addr n;    (* addr.up   *)
  
    fun getWordAddr (Addr a) = a;
    fun getByteAddr (Addr a) = a * wordSize;
  
    val addrZero = mkWordAddr 0;
  end;
  
  
(*****************************************************************************)
(*                  Types for branch labels                                  *)
(*****************************************************************************)

  type labels = addrs list;
  
  
  val noJump = []:labels; 
  
  datatype const =
     WVal of machineWord        (* an existing constant *)
   | HVal of addrs ref   (* a handler *)
   | CVal of code        (* a forward-reference to another function *)

  (* Constants which are too large to go inline in the code are put in
     a list and put at the end of the code. They are arranged so that
     the garbage collector can find them and change them as necessary.
     A reference to a constant is treated like a forward reference to a
     label. *)

  (* A code list is used to hold a list of code-vectors which must have the
     address of this code-vector put into it. *)

  and setCodeseg =
     Unset
   | Set of cseg * int  (* Code vector, plus size of prelude *)

  and code = Code of 
    { 
      procName:       string,         (* Name of the procedure. *)
      noClosure:      bool,           (* should we make a closure from this? *)
      codeVec:        cseg,           (* This segment is used as a buffer. When the
                                         procedure has been code generated it is
                                         copied into a new segment of the correct size *)
      ic:             addrs ref,      (* Pointer to first free location in "codevec" *)
      constVec:       (const * addrs) list ref, (* Constants used in the code *)
      numOfConsts:    int ref,        (* Number of constants that haven't been fixed up yet *)
      stackReset:     int ref,        (* Distance to reset the stack before the next instr. *)
      otherCodes:     code list ref,  (* Other code vectors with forward references to this vector. *)
      resultSeg:      setCodeseg ref, (* The segment as the final result. *)
      mustCheckStack: bool ref,       (* Set to true if stack check must be done. *)
      justComeFrom:   labels ref,     (* The label(s) we have just jumped from. *)
      exited:         bool ref,       (* False if we can fall-through to here *)
      selfCalls:      labels ref,     (* List of recursive calls to patch up. *)
      selfJumps:      labels ref,     (* List of recursive tail-calls to patch up. *)
      accCache:       CacheState ref, (* i4 contains the untagged value of what register? *)
      constLoads:     (addrs * reg * reg * int) list ref, (* where do we load constants? *)
      printAssemblyCode:bool,            (* Whether to print the code when we finish. *)
      printStream:    string->unit    (* The stream to use *)
    };

  fun codeVec        (Code {codeVec,...})        = codeVec;
  fun ic             (Code {ic,...})             = ic;
  fun constVec       (Code {constVec,...})       = constVec;
  fun numOfConsts    (Code {numOfConsts,...})    = numOfConsts;
  fun stackReset     (Code {stackReset ,...})    = stackReset ;
  fun procName       (Code {procName,...})       = procName;
  fun otherCodes     (Code {otherCodes,...})     = otherCodes;
  fun resultSeg      (Code {resultSeg,...})      = resultSeg;
  fun mustCheckStack (Code {mustCheckStack,...}) = mustCheckStack;
  fun justComeFrom   (Code {justComeFrom,...})   = justComeFrom;
  fun exited         (Code {exited,...})         = exited;
  fun selfCalls      (Code {selfCalls,...})      = selfCalls;
  fun selfJumps      (Code {selfJumps,...})      = selfJumps;
  fun accCache       (Code {accCache,...})       = accCache;
  fun noClosure      (Code {noClosure,...})      = noClosure;

  fun unreachable cvec = 
    case ! (justComeFrom cvec) of
      [] => ! (exited cvec)
    | _  => false;

  val codeSize = 8; (* Words. Initial size of segment. *)
  
  (* %i0 points to a structure that interfaces to the RTS.  These are
     offsets into that structure.  Currently only raiseException and
     the stack limit value (used to interrupt code) are used. *)
  val MemRegRaiseException = 32
  val MemRegStackLimit     = 24


  (* Test for identity of the code segments by testing whether
     the resultSeg ref is the same. N.B. NOT its contents. *)
  infix is;
  fun a is b = (resultSeg a = resultSeg b);

  (* create and initialise a code segment *)
  fun codeCreate (noClosure, name, parameters) : code = 
    Code
      { 
         procName       = name,
         noClosure      = noClosure,
         codeVec        = csegMake codeSize, (* a byte array *)
         ic             = ref addrZero,
         constVec       = ref [],
         numOfConsts    = ref 0,
         stackReset     = ref 0, 
         otherCodes     = ref [],
         resultSeg      = ref Unset,   (* Not yet done *)
         mustCheckStack = ref false,
         justComeFrom   = ref [],
         exited         = ref false,
         selfCalls      = ref [],
         selfJumps      = ref [],
         accCache       = ref Empty,
         constLoads     = ref [],
         printAssemblyCode = DEBUG.getParameter DEBUG.assemblyCodeTag parameters,
         printStream    = PRETTY.getSimplePrinter parameters
      };
  
  datatype quad =  (* the 4 bytes of the instruction word *)
    Quad of short * short * short * short

  (* break an instruction word into 4 bytes; try to do it in a way that *)
  (* will minimise the arithmetic - particularly for long values. *)
  fun toQuad (w : int) : quad =
  let
    val topHw    = toShort (w div exp2_16);
    val bottomHw = toShort (w mod exp2_16);
  in
    Quad (topHw    >> 0w8, Word.andb (mask8bits, topHw),
      bottomHw >> 0w8, Word.andb (mask8bits, bottomHw))
  end;

  (* returns *unsigned* integer *)
  fun fromQuad (Quad (b1,b2,b3,b4)) : int =
  let
    val topHw    = toInt (Word.orb (b1 << 0w8, b2));
    val bottomHw = toInt (Word.orb (b3 << 0w8, b4));
  in
    topHw * exp2_16 + bottomHw
  end;

  (* Put 4 bytes at a given offset in the segment. *)
  (* Write out high order bytes followed by low order.
     Assume all arguments are +ve or zero. *)
  fun setQuad (Quad (b1,b2,b3,b4), addr : addrs, seg : cseg) =
  let
    val a : int = getByteAddr addr;
  in
    csegSet (seg, a,     Word8.fromLargeWord(Word.toLargeWord b1));
    csegSet (seg, a + 1, Word8.fromLargeWord(Word.toLargeWord b2));
    csegSet (seg, a + 2, Word8.fromLargeWord(Word.toLargeWord b3));
    csegSet (seg, a + 3, Word8.fromLargeWord(Word.toLargeWord b4))
  end;

  fun getQuad (addr:addrs, seg:cseg) : quad =
  let
    val a : int = getByteAddr addr;
    val b1  = Word.fromLargeWord(Word8.toLargeWord(csegGet (seg, a)))
    val b2  = Word.fromLargeWord(Word8.toLargeWord(csegGet (seg, a + 1)))
    val b3  = Word.fromLargeWord(Word8.toLargeWord(csegGet (seg, a + 2)))
    val b4  = Word.fromLargeWord(Word8.toLargeWord(csegGet (seg, a + 3)))
  in
    Quad (b1, b2, b3, b4)
  end;

  (* generate a quad and increment instruction counter *)
  fun genCodeQuad (instr : quad, cvec) =
  let
    val addr = ! (ic cvec)
  in  
    setQuad (instr, addr, codeVec cvec);
    ic cvec := addr wordAddrPlus 1
  end;

  fun getCodeQuad (addr : addrs, code : code) : quad =
  let
    val seg = codeVec code;
  in
    getQuad (addr, seg)
  end;

  fun setCodeQuad (instr : quad, addr : addrs, code : code) : unit =
  let
    val seg = codeVec code;
  in
    setQuad (instr, addr, seg)
  end;

  fun scSet (Set x) = x | scSet _ = raise Match;

  (* Test condition codes. *)
  abstype testCode = Test of int 
  with 
    fun eqTest (Test x) (Test y) = x = y;
  
    val never  = Test  0; (* Never true *)
    val e      = Test  1; (* equal *)
    val le     = Test  2; (* less or equal *)
    val l      = Test  3; (* less than *)
    val leu    = Test  4; (* low or same *)
    val cs     = Test  5; (* carry set *)
    val neg    = Test  6; (* minus *)
    val vs     = Test  7; (* overflow *)
    val always = Test  8; (* always true *)
    val ne     = Test  9; (* not equal *)
    val g      = Test 10; (* greater than*)
    val ge     = Test 11; (* greater or equal *)
    val gu     = Test 12; (* high *)
    val cc     = Test 13; (* carry clear *)
    val pos    = Test 14; (* plus *)
    
    fun tstRepr tst =
           if tst = never  then "n"   
      else if tst = e      then "e"   
      else if tst = le     then "le"
      else if tst = l      then "l"   
      else if tst = leu    then "leu" 
      else if tst = cs     then "cs"  
      else if tst = neg    then "neg" 
      else if tst = vs     then "vs"  
      else if tst = always then "a" 
      else if tst = ne     then "ne" 
      else if tst = g      then "g" 
      else if tst = ge     then "ge" 
      else if tst = gu     then "gu" 
      else if tst = cc     then "cc" 
      else if tst = pos    then "pos" 
      else "vc";

    fun getTest (Test t) = t;      (* test.down *)
    fun mkTest   n      = Test n;  (* test.up   *)
  end;
  
  (* effective address modes *)
  (* On the Sparc this is either a register or a 13-bit immediate. *)
  abstype mode13 = Register of reg | Imm13 of int (* 13 bit only *)
  with
    fun getMode13 (Register r) = getReg r
      | getMode13 (Imm13 i)    = 
     let
        (* remove sign *)
       val v = if i < 0 then exp2_13 + i else i
     in 
       (* add "tag" bit *)
       v + exp2_13
     end;
       
     fun isImm13 (Register _) = false
       | isImm13 (Imm13 _)    = true;
  
     fun getImm13 (Register _) = raise InternalError "getImm13"
       | getImm13 (Imm13 i)    = i;
  
    fun eqMode13 (Register x) (Register y) = (x regEq y) 
      | eqMode13 (Imm13 x)    (Imm13 y)    = (x = y) 
      | eqMode13 _            _            = false; 
  
    (* We check for shorts first because that saves us an expensive
       compile-time trap if v is actually a long value.
       SPF 19/12/95 *)
    (* v will fit into 13 bits *)
    fun is13Bit v =  
      isShort v andalso ~ exp2_12 <= v andalso v < exp2_12;

    (* v will fit into 13 bits when tagged *)
    (* NB isTaggable13Bit b implies is13Bit (4*v) ... is13Bit (4*v+3) *)
    fun isTaggable13Bit v = ~ exp2_10 <= v andalso v < exp2_10;

    fun mReg r    = Register r
    fun immed13 i =
      if is13Bit i then Imm13 i
      else let
        val msg = 
          concat
           [
             "immed13: can't convert ",
             Int.toString i,
             " into 13-bit immediate"
           ]
      in
        raise InternalError msg
      end;
  
    (* These are provided for convenience. *)
    val i3M  = mReg i3;
    val i4M  = mReg i4;
    val i5M  = mReg i5;
    val pspM = mReg psp;
    val g6M  = mReg g6;

    val immed13_0  = immed13 0;
    val immed13_4  = immed13 4;
    val immed13_6  = immed13 6;
    val immed13_8  = immed13 8;
    val immed13_24 = immed13 24;
    
    val immed13_TAGSHIFT = immed13 TAGSHIFT;
    val immed13_TAGMASK  = immed13 TAGMASK;
    val immed13_DATATAG  = immed13 DATATAG;
    val immed13_CODETAG  = immed13 CODETAG;

    fun signed13 (imm : int) : int =
     if imm < exp2_12 (* positive? *)
     then imm
     else (imm - exp2_13);
  end (* mode13 *);
      
  (* 30-bit mode for calls *)
  abstype mode30 = Imm30 of int (* 30 bit only *)
  with
    (* Unfortunately, we can't avoid the use of long arithmetic
       in the following code, because if i is negative, the
       result is (should be) >= exp2_29 i.e. it's not a 30-bit short.
       SPF 19/12/95
    *)

    (* But we can work round this by returning a
       (signed) short. SPF 19/12/95
    *)
    fun getMode30Short (Imm30 i) = toShort i;
  
    (* A hack that saves a lot of compile-time traps, assuming
       that the machine we're compiling on uses 30-bit shorts.
       SPF 19/12/95
    *)
    fun is30Bit (v : int) = isShort v;
(* ...
    fun is30Bit v = (* v will fit into 30 bits *)
       ~ exp2_29 <= v andalso v < exp2_29;
... *)

    fun immed30 i = Imm30 i;
    
    val immed30_0 = immed30 0;

    fun signed30 (imm : int) : int =
     if imm < exp2_29 (* positive? *)
     then imm
     else (imm - exp2_30);
  end (* mode30 *);
      
  (* 22-bit mode for jumps *)
  abstype mode22 = Imm22 of int (* 22 bit only *)
  with
    fun getMode22 (Imm22 i) =  (* remove sign *)
      if i < 0 then exp2_22 + i else i;
  
    fun is22Bit v = (* v will fit into 22 bits *)
       ~ exp2_21 <= v andalso v < exp2_21;

    fun immed22 i = Imm22 i;
    
    val immed22_0 = immed22 0;
    
    fun signed22 (imm : int) : int =
     if imm < exp2_21 (* positive? *)
     then imm
     else (imm - exp2_22);
  end (* mode22 *);

  (* break a 32 bit word integer into 22 hi bits (+ve or -ve) + 10 lo bits (+ve) *)
  fun splitHiLo w = (* we assume ~exp2_31 <= w < exp2_31 *)
  let
    val lo = immed13 (w mod exp2_10); (* 0 <= lo < exp2_10 *)
    val hi = immed22 (w div exp2_10); (* ~exp2_21 <= hi < exp2_21 *)
  in
    (hi, lo)
  end;

  (* op2 opcodes *)
  datatype op2 =
    UNIMP    (*  0 *) (* The official unimplemented instruction *)
  | UNIMP1   (*  1 *)
  | Bicc     (*  2 *)
  | UNIMP3   (*  3 *)
  | SETHI    (*  4 *)
  | UNIMP5   (*  5 *)
  | FBfcc    (*  6 *)
  | CBccc    (*  7 *)
 ;
  
  fun op2ToInt UNIMP  = 0
    | op2ToInt UNIMP1 = 1
    | op2ToInt Bicc   = 2
    | op2ToInt UNIMP3 = 3
    | op2ToInt SETHI  = 4
    | op2ToInt UNIMP5 = 5
    | op2ToInt FBfcc  = 6
    | op2ToInt CBccc  = 7
 ;
  
  (* op3 opcodes used when op = 2 *)
  datatype op3_2 =
    ADD      (*  0 *)
  | AND      (*  1 *)
  | OR       (*  2 *)
  | XOR      (*  3 *)
  | SUB      (*  4 *)
  | ANDN     (*  5 *)
  | ORN      (*  6 *)
  | XNOR     (*  7 *)
  | ADDX     (*  8 *)
  | UNIMP9   (*  9 *)
  | UNIMP10  (* 10 *)
  | UNIMP11  (* 11 *)
  | SUBX     (* 12 *)
  | UNIMP13  (* 13 *)
  | UNIMP14  (* 14 *)
  | UNIMP15  (* 15 *)
  | ADDcc    (* 16 *)
  | ANDcc    (* 17 *)
  | ORcc     (* 18 *)
  | XORcc    (* 19 *)
  | SUBcc    (* 20 *)
  | ANDNcc   (* 21 *)
  | ORNcc    (* 22 *)
  | XNORcc   (* 23 *)
  | ADDXcc   (* 24 *)
  | UNIMP25  (* 25 *)
  | UNIMP26  (* 26 *)
  | UNIMP27  (* 27 *)
  | SUBXcc   (* 28 *)
  | UNIMP29  (* 29 *)
  | UNIMP30  (* 30 *)
  | UNIMP31  (* 31 *)

  | TADDcc   (* 32 *)
  | TSUBcc   (* 33 *)
  | TADDccTV (* 34 *)
  | TSUBccTV (* 35 *)
  | MULScc   (* 36 *)
  | SLL      (* 37 *)
  | SRL      (* 38 *)
  | SRA      (* 39 *)
  | RDY      (* 40 *)
  | RDPSR    (* 41 *)
  | RDWIM    (* 42 *)
  | RDTBR    (* 43 *)
  | UNIMP44  (* 44 *)
  | UNIMP45  (* 45 *)
  | UNIMP46  (* 46 *)
  | UNIMP47  (* 47 *)
  | WRY      (* 48 *)
  | WRPSR    (* 49 *)
  | WRWIM    (* 50 *)
  | WRTBR    (* 51 *)
  | FPop1    (* 52 *)
  | FPop2    (* 53 *)
  | CPop1    (* 54 *)
  | CPop2    (* 55 *)
  | JMPL     (* 56 *)
  | RETT     (* 57 *)
  | Ticc     (* 58 *)
  | IFLUSH   (* 69 *)
  | SAVE     (* 60 *)
  | RESTORE  (* 61 *)
  | UNIMP62  (* 62 *)
  | UNIMP63  (* 63 *)
 ;

  fun  op3_2ToInt ADD      =  0 
    |  op3_2ToInt AND      =  1 
    |  op3_2ToInt OR       =  2 
    |  op3_2ToInt XOR      =  3 
    |  op3_2ToInt SUB      =  4 
    |  op3_2ToInt ANDN     =  5 
    |  op3_2ToInt ORN      =  6 
    |  op3_2ToInt XNOR     =  7 
    |  op3_2ToInt ADDX     =  8 
    |  op3_2ToInt UNIMP9   =  9 
    |  op3_2ToInt UNIMP10  = 10 
    |  op3_2ToInt UNIMP11  = 11 
    |  op3_2ToInt SUBX     = 12 
    |  op3_2ToInt UNIMP13  = 13 
    |  op3_2ToInt UNIMP14  = 14 
    |  op3_2ToInt UNIMP15  = 15 
    |  op3_2ToInt ADDcc    = 16 
    |  op3_2ToInt ANDcc    = 17 
    |  op3_2ToInt ORcc     = 18 
    |  op3_2ToInt XORcc    = 19 
    |  op3_2ToInt SUBcc    = 20 
    |  op3_2ToInt ANDNcc   = 21 
    |  op3_2ToInt ORNcc    = 22 
    |  op3_2ToInt XNORcc   = 23 
    |  op3_2ToInt ADDXcc   = 24 
    |  op3_2ToInt UNIMP25  = 25 
    |  op3_2ToInt UNIMP26  = 26 
    |  op3_2ToInt UNIMP27  = 27 
    |  op3_2ToInt SUBXcc   = 28 
    |  op3_2ToInt UNIMP29  = 29 
    |  op3_2ToInt UNIMP30  = 30 
    |  op3_2ToInt UNIMP31  = 31 

    |  op3_2ToInt TADDcc   = 32
    |  op3_2ToInt TSUBcc   = 33
    |  op3_2ToInt TADDccTV = 34
    |  op3_2ToInt TSUBccTV = 35
    |  op3_2ToInt MULScc   = 36
    |  op3_2ToInt SLL      = 37
    |  op3_2ToInt SRL      = 38
    |  op3_2ToInt SRA      = 39
    |  op3_2ToInt RDY      = 40
    |  op3_2ToInt RDPSR    = 41
    |  op3_2ToInt RDWIM    = 42
    |  op3_2ToInt RDTBR    = 43
    |  op3_2ToInt UNIMP44  = 44
    |  op3_2ToInt UNIMP45  = 45
    |  op3_2ToInt UNIMP46  = 46
    |  op3_2ToInt UNIMP47  = 47
    |  op3_2ToInt WRY      = 48
    |  op3_2ToInt WRPSR    = 49
    |  op3_2ToInt WRWIM    = 50
    |  op3_2ToInt WRTBR    = 51
    |  op3_2ToInt FPop1    = 52
    |  op3_2ToInt FPop2    = 53
    |  op3_2ToInt CPop1    = 54
    |  op3_2ToInt CPop2    = 55
    |  op3_2ToInt JMPL     = 56
    |  op3_2ToInt RETT     = 57
    |  op3_2ToInt Ticc     = 58
    |  op3_2ToInt IFLUSH   = 59
    |  op3_2ToInt SAVE     = 60
    |  op3_2ToInt RESTORE  = 61
    |  op3_2ToInt UNIMP62  = 62
    |  op3_2ToInt UNIMP63  = 63
 ;


  datatype op3_3 =
    LD       (*  0 *)
  | LDUB     (*  1 *)
  | LDUH     (*  2 *)
  | LDD      (*  3 *)
  | ST       (*  4 *)
  | STB      (*  5 *)
  | STH      (*  6 *)
  | STD      (*  7 *)
  | UNIMP8   (*  8 *)
  | LDSB     (*  9 *)
  | LDSH     (* 10 *)
  | UNIMP11  (* 11 *)
  | UNIMP12  (* 12 *)
  | LDSTUB   (* 13 *)
  | UNIMP14  (* 14 *)
  | SWAP     (* 15 *)
  | LDA      (* 16 *)
  | LDUBA    (* 17 *)
  | LDUHA    (* 18 *)
  | LDDA     (* 19 *)
  | STA      (* 20 *)
  | STBA     (* 21 *)
  | STHA     (* 22 *)
  | STDA     (* 23 *)
  | UNIMP24  (* 24 *)
  | LDSBA    (* 25 *)
  | LDSHA    (* 26 *)
  | UNIMP27  (* 27 *)
  | UNIMP28  (* 28 *)
  | LDSTUBA  (* 29 *)
  | UNIMP30  (* 30 *)
  | SWAPA    (* 31 *)
 
  | LDF      (* 32 *)
  | LDFSR    (* 33 *)
  | UNIMP34  (* 34 *)
  | LDDF     (* 35 *)
  | STF      (* 36 *)
  | STFSR    (* 37 *)
  | STDFQ    (* 38 *)
  | STDF     (* 39 *)
  | RDY      (* 40 *)
  | UNIMP41  (* 41 *)
  | UNIMP42  (* 42 *)
  | UNIMP43  (* 43 *)
  | UNIMP44  (* 44 *)
  | UNIMP45  (* 45 *)
  | UNIMP46  (* 46 *)
  | UNIMP47  (* 47 *)
  | LDC      (* 48 *)
  | LDCSR    (* 49 *)
  | UNIMP50  (* 50 *)
  | LDDC     (* 51 *)
  | STC      (* 52 *)
  | STCSR    (* 53 *)
  | STDCQ    (* 54 *)
  | STDC     (* 55 *)
  | UNIMP56  (* 56 *)
  | UNIMP57  (* 57 *)
  | UNIMP58  (* 58 *)
  | UNIMP59  (* 69 *)
  | UNIMP60  (* 60 *)
  | UNIMP61  (* 61 *)
  | UNIMP62  (* 62 *)
  | UNIMP63  (* 63 *)
 ;

  fun op3_3ToInt LD      =  0
    | op3_3ToInt LDUB    =  1
    | op3_3ToInt LDUH    =  2
    | op3_3ToInt LDD     =  3
    | op3_3ToInt ST      =  4
    | op3_3ToInt STB     =  5
    | op3_3ToInt STH     =  6
    | op3_3ToInt STD     =  7
    | op3_3ToInt UNIMP8  =  8
    | op3_3ToInt LDSB    =  9
    | op3_3ToInt LDSH    = 10
    | op3_3ToInt UNIMP11 = 11
    | op3_3ToInt UNIMP12 = 12
    | op3_3ToInt LDSTUB  = 13
    | op3_3ToInt UNIMP14 = 14
    | op3_3ToInt SWAP    = 15
    | op3_3ToInt LDA     = 16
    | op3_3ToInt LDUBA   = 17
    | op3_3ToInt LDUHA   = 18
    | op3_3ToInt LDDA    = 19
    | op3_3ToInt STA     = 20
    | op3_3ToInt STBA    = 21
    | op3_3ToInt STHA    = 22
    | op3_3ToInt STDA    = 23
    | op3_3ToInt UNIMP24 = 24
    | op3_3ToInt LDSBA   = 25
    | op3_3ToInt LDSHA   = 26
    | op3_3ToInt UNIMP27 = 27
    | op3_3ToInt UNIMP28 = 28
    | op3_3ToInt LDSTUBA = 29
    | op3_3ToInt UNIMP30 = 30
    | op3_3ToInt SWAPA   = 31
 
    | op3_3ToInt LDF     = 32
    | op3_3ToInt LDFSR   = 33
    | op3_3ToInt UNIMP34 = 34
    | op3_3ToInt LDDF    = 35
    | op3_3ToInt STF     = 36
    | op3_3ToInt STFSR   = 37
    | op3_3ToInt STDFQ   = 38
    | op3_3ToInt STDF    = 39
    | op3_3ToInt RDY     = 40
    | op3_3ToInt UNIMP41 = 41
    | op3_3ToInt UNIMP42 = 42
    | op3_3ToInt UNIMP43 = 43
    | op3_3ToInt UNIMP44 = 44
    | op3_3ToInt UNIMP45 = 45
    | op3_3ToInt UNIMP46 = 46
    | op3_3ToInt UNIMP47 = 47
    | op3_3ToInt LDC     = 48
    | op3_3ToInt LDCSR   = 49
    | op3_3ToInt UNIMP50 = 50
    | op3_3ToInt LDDC    = 51
    | op3_3ToInt STC     = 52
    | op3_3ToInt STCSR   = 53
    | op3_3ToInt STDCQ   = 54
    | op3_3ToInt STDC    = 55
    | op3_3ToInt UNIMP56 = 56
    | op3_3ToInt UNIMP57 = 57
    | op3_3ToInt UNIMP58 = 58
    | op3_3ToInt UNIMP59 = 69
    | op3_3ToInt UNIMP60 = 60
    | op3_3ToInt UNIMP61 = 61
    | op3_3ToInt UNIMP62 = 62
    | op3_3ToInt UNIMP63 = 63
 ;

  (* Specially written to reduce the number of emulation traps required *)
  fun callQuad (disp:mode30) : quad =
  let
    val addrBits : short = getMode30Short disp;
    val b4  : short = Word.andb (addrBits, mask8bits);
    val b3  : short = Word.andb (addrBits >> 0w8,  mask8bits);
    val b2  : short = Word.andb (addrBits >> 0w16, mask8bits);
    val b1  : short = Word.andb (addrBits >> 0w24, mask6bits);
    val opc : short = toShort exp2_6;
  in
    Quad (Word.orb (opc, b1), b2, b3, b4)
  end;
  
  val call0 : quad = callQuad immed30_0;
  
  fun format3Quad (iOp:int, op3:int, rsn:int, mval:int, rdn:int) : quad =
  let
    val iop_short  = toShort iOp;  (*  2 bits *)
    val op3_short  = toShort op3;  (*  6 bits *)
    val rdn_short  = toShort rdn;  (*  5 bits *)
    val rsn_short  = toShort rsn;  (*  5 bits *)
    val mval_short = toShort mval; (* 14 bits *)
  
    val bits30_31 = iop_short << 0w6; (* 2 bits *)
    val bits25_29 = rdn_short << 0w1; (* 5 bits *) 
    val bits24_24 = op3_short >> 0w5; (* 1 bit  *)
    val b1 = Word.orb (Word.orb (bits25_29, bits24_24), bits30_31);
    
    val bits19_23 = Word.andb (op3_short, mask5bits) << 0w3; (* 5 bits *)
    val bits16_18 = rsn_short >> 0w2;                  (* 3 bits *)
    val b2 = Word.orb (bits16_18, bits19_23);
   
    val bits14_15 = Word.andb (rsn_short, mask2bits) << 0w6; (* 2 bits *)
    val bits8_13  = mval_short >> 0w8;                 (* 6 bits *)
    val b3 = Word.orb (bits8_13, bits14_15);
    
    val b4 = Word.andb (mval_short, mask8bits); (* 8 bits *)
  in
    Quad (b1, b2, b3, b4)
  end;

  fun fixupBranch (instr : quad, disp : mode22) : quad =
  let
     val (Quad (b1, b2, b3, b4)) = instr
  in
    if wordEq (0w0, b4) andalso
       wordEq (0w0, b3) andalso
       wordEq (0w0, Word.andb (b2, mask6bits))
    then let
      val d  = toShort (getMode22 disp);     (* 22 bits will be a short *)
      val d4 = Word.andb (mask8bits, d);           (* 8 bits *)
      val d3 = Word.andb (mask8bits, d >> 0w8); (* 8 bits *)
      val d2 = d >> 0w16;                 (* 6 bits *)
    in
      Quad (b1, Word.orb (b2, d2), d3, d4)
    end
    else raise InternalError "fixupBranch: already fixed-up"
  end;
    
  
  (* trap is a variation on format3 *)
  fun trap (cond:testCode, rs:reg, md:mode13) : quad =
    format3Quad (2, op3_2ToInt Ticc, getReg rs, getMode13 md, getTest cond) 

  val tlu24 = trap (cs, g0, immed13_24); (* tlu 24 is a synonym for tcs 24 *)

  fun format2Quad (op2:int, imm:int (* unsigned *), rdn:int) : quad =
  let
    val rdn_short = toShort rdn; (*  5 bits *)
    val op2_short = toShort op2; (*  3 bits *)
    val imm_short = toShort imm; (* 22 bits *)
  
 (* val bits30_31 = 0w0; *)           (* 2 bits *)
    val bits25_29 = rdn_short << 0w1; (* 5 bits *)
    val bits24_24 = op2_short >> 0w2; (* 1 bit  *)
    val b1 = Word.orb (bits24_24, bits25_29);
    
    val bits22_23 = Word.andb (op2_short, mask2bits) << 0w6; (* 2 bits *)
    val bits16_21 = imm_short >> 0w16;                 (* 6 bits *)
    val b2 = Word.orb (bits16_21, bits22_23);
   
    val b3 = Word.andb (imm_short >> 0w8, mask8bits);
    val b4 = Word.andb (imm_short, mask8bits);
  in
    Quad (b1,b2,b3,b4)
  end;

  (* sethi and conditional branches are variations on format 2 *)
  fun sethi (v:mode22, rd:reg) : quad =
    format2Quad (op2ToInt SETHI, getMode22 v, getReg rd);

  (* opcode for a no-op - sethi 0,%0 *)
  val nopQuad : quad = sethi (immed22_0, g0); 

  fun branch (annul:bool, cond:testCode, disp:mode22) : quad = 
  let
    val tn = getTest cond;
    val rn = if annul then tn + exp2_4 else tn;
  in
    format2Quad (op2ToInt Bicc,getMode22 disp, rn)
  end;

  val branchAlwaysQuad : quad =
    branch (false (* no annul *), always, immed22_0);

  fun format3_2 (op3:op3_2, rs:reg, md:mode13, rd:reg) : quad =
    format3Quad (2, op3_2ToInt op3, getReg rs, getMode13 md, getReg rd);
  
  fun format3_3 (op3:op3_3, rs:reg, md:mode13, rd:reg) : quad =
    format3Quad (3, op3_3ToInt op3, getReg rs, getMode13 md, getReg rd);
  
  (* At the moment there are two kinds of operations that can be left pending 
     and are not actually generated immediately. Changing the real stack
     pointer is not generated immediately because often these accumulate and
     sometimes even cancel out. Fixing up a branch is not done immediately in
     case we are going to jump somewhere else, and also because we can often
     use the delay slot of the jump. If both the branch and stack movement are
     deferred the branch is assumed to have happened first. *)

  (* Fix up the list of labels. *)
  fun reallyFixupBranches (_, _, []) = ()
    | reallyFixupBranches (cvec, target, addrH :: addrT) = 
    let
      val wordDiff : int = target wordAddrMinus addrH;
      val jumpInstr  = getCodeQuad (addrH, cvec);
      val () =
        if not (is22Bit wordDiff) (* 22 bit signed number *)
        then raise InternalError "reallyFixupBranches: jump too large"
        else (* Set this addr to point to the destination. *)
      setCodeQuad (fixupBranch (jumpInstr, immed22 wordDiff), addrH, cvec);
    in
      reallyFixupBranches (cvec, target, addrT)
    end;

  fun fixupRecursiveBranches (_, _, _, []) = ()
    | fixupRecursiveBranches (cvec, targetInstr, target, addrH :: addrT) = 
    let
      val nextAddr  : addrs = addrH wordAddrPlus 1;
      val jumpInstr : quad  = getCodeQuad (addrH, cvec);
      val nextInstr : quad  = getCodeQuad (nextAddr, cvec);
    in
      if nextInstr <> nopQuad
      then let
    val wordDiff : int = target wordAddrMinus addrH;
      in
    if not (is22Bit wordDiff)
    then raise InternalError "fixupRecursiveBranches: jump too large"
    else setCodeQuad (fixupBranch (jumpInstr, immed22 wordDiff), addrH, cvec)
      end
      else let (* put targetInstr into the delay slot *)
        val newTarget : addrs = target wordAddrPlus 1;
    val wordDiff : int = newTarget wordAddrMinus addrH;
      in
    if not (is22Bit wordDiff)
    then raise InternalError "fixupRecursiveBranches: jump too large"
    else let
     val () =
       setCodeQuad (fixupBranch (jumpInstr, immed22 wordDiff), addrH, cvec);
    in
      setCodeQuad (targetInstr, nextAddr, cvec)
    end
      end;

      fixupRecursiveBranches (cvec, targetInstr, target, addrT)
    end;

  fun fixupRecursiveCalls (_, _, _, []) = ()
    | fixupRecursiveCalls (cvec, targetInstr, target, addrH :: addrT) = 
    let
      val nextAddr  : addrs = addrH wordAddrPlus 1;
      val callInstr : quad  = getCodeQuad (addrH, cvec);
      val nextInstr : quad  = getCodeQuad (nextAddr, cvec);
    in
      if callInstr <> call0
        then raise InternalError "fixupRecursiveCalls: not a call instruction"
      else if nextInstr <> nopQuad
        then let
          val wordDiff : int = target wordAddrMinus addrH;
        in
          if not (is30Bit wordDiff)
          then raise InternalError "fixupRecursiveCalls: jump too large"
          else setCodeQuad (callQuad (immed30 wordDiff), addrH, cvec)
        end
        else let (* put targetInstr into the delay slot *)
          val newTarget : addrs = target wordAddrPlus 1;
      val wordDiff : int = newTarget wordAddrMinus addrH;
        in
          if not (is30Bit wordDiff)
          then raise InternalError "fixupRecursiveCalls: jump too large"
          else let
            val () =
             setCodeQuad (callQuad (immed30 wordDiff), addrH, cvec);
          in
            setCodeQuad (targetInstr, nextAddr, cvec)
          end
        end;
         
      fixupRecursiveCalls (cvec, targetInstr, target, addrT)
    end;

  (* Deal with a pending fix-up. *)
  fun reallyFixup cvec = 
    let
      val jcf  = justComeFrom cvec;
      val here = ! (ic cvec);
    in
      case ! jcf of
        []   => ()
      | labs => (exited cvec := false; reallyFixupBranches (cvec, here, labs); jcf := [])
    end;

  (* Generate an instruction.  This may involve filling in a delay slot. *)
  (* What if we fill one delay slot, then find a second is already full? *)
  (* OK, since the two jumps will be sent to different destinations.     *)
  fun genInstruction (instr : quad, cvec : code) : unit =
    if unreachable cvec then ()
    else let
      val jcf = justComeFrom cvec
      
      fun fillDelays []             = []
        | fillDelays (addr::addrs)  =
          if getCodeQuad (addr wordAddrPlus 1, cvec) = nopQuad
          then let
             (* Put the instruction in the delay slot *)
            val () = setCodeQuad (instr, addr wordAddrPlus 1, cvec);
          in   
            (* Keep this label in the list for the next instruction. *)
            addr :: fillDelays addrs
          end
          else let (* Fix this up here. *) (* genLoadConstant? SPF *)
            val wordDiff : int = ! (ic cvec) wordAddrMinus addr;
            val jumpInstr = getCodeQuad (addr, cvec);
            val () =
              if not (is22Bit wordDiff) (* 22 bit signed number *)
              then raise InternalError "genInstruction: jump too large"
              else (* Set this addr to point to the destination. *)
                setCodeQuad (fixupBranch (jumpInstr, immed22 wordDiff), addr, cvec);
            (* Must generate the instruction. *)
            val () = exited cvec := false;
          in
            fillDelays addrs
          end;
    in
      case ! jcf of
        []   => ()
      | labs => jcf := fillDelays labs;

      (* If we have managed to use delay slots we can keep defer those jumps for
         one instruction. If we have been unable to use any delay slot, or if we
         could have fallen through to here, we must generate the instruction. *) 
      if not (! (exited cvec)) then genCodeQuad (instr, cvec) else ()
    end (* genInstruction *);

  fun genInstructionList (_, []) = ()
    | genInstructionList (cvec, w::ws) = 
       (genInstruction (w, cvec); genInstructionList (cvec, ws));


(***************************************************************************  
  Functions for maintaining the write-through cache for untagged values.
***************************************************************************)  

  fun cached (reg, cvec) = 
    case ! (accCache cvec) of
      Empty  => false
    | Copy r => r regEq reg

  fun genUntagInstr (reg, cvec) = 
    genInstruction (format3_2 (SUB, reg, immed13_DATATAG, i4), cvec);

  fun genTagInstr (reg, cvec) = 
    genInstruction (format3_2 (ADD, i4, immed13_DATATAG, reg), cvec);

  (* process any pending writes, but remember state *)
  fun clearCache cvec =
  let
    val cache : CacheState ref = accCache cvec;
  in
    cache := Empty
  end;

  (* write the cache into reg *)
  fun writeCache (reg, cvec) =
  let
    val cache : CacheState ref = accCache cvec;
  in
    genTagInstr (reg, cvec);
    
    cache := Copy reg
  end;

  (* load the cache from reg *)
  fun readCache (reg, cvec) =
  let
    val cache : CacheState ref = accCache cvec;
  in
    case !cache of
      Empty   => 
        (
          genUntagInstr (reg, cvec);
          cache := Copy reg
        )

    | Copy r =>
        if r regEq reg then () (* already there *)
        else 
          (
            genUntagInstr (reg, cvec);
            cache := Copy reg
          )
  end;

(***************************************************************************)  


  (* load constant into i3, unless it's an immediate *)
  (* used for heap/stack addressing only *)
  fun immedCodeOffset (v:int) : quad list * mode13 =
    if is13Bit v then ([],immed13 v)
    else let
      val (hi,lo) = splitHiLo v
      val code =
         if eqMode13 lo immed13_0 (* minor optimisation *)
         then [sethi (hi, i3)]
         else [sethi (hi, i3), format3_2 (ADD, i3, lo, i3)]    
    in
      (code, i3M)
    end;

  (* load constant into i5, unless it's an immediate *)
  (* We MUST use i5 for this, because the RTS won't
     understand the tsubcctv instruction in allocate-code
     if we use a different register. SPF 17/5/95 *)
  fun immedCodeData (v:int) : quad list * mode13 =
    if is13Bit v then ([],immed13 v)
    else let
      val (hi,lo) = splitHiLo v
      val code =
    if eqMode13 lo immed13_0 (* minor optimisation *)
    then [sethi (hi, i5)]
    else [sethi (hi, i5), format3_2 (ADD, i5, lo, i5)]    
    in
      (code, i5M)
    end;

  (* Do outstanding operations in the right order. This deals with any
     stack resets which may involve fixing up some branches. *)
  fun doPendingStackAdjustment (cvec : code) : unit =
    case ! (stackReset cvec) of
      0   => ()
    | adj =>
        let
          val (cl,imm) = immedCodeOffset (adj * wordSize);
        in
          stackReset cvec := 0;
        
          (* add %psp,imm,%psp *)
          genInstructionList (cvec, cl);
          genInstruction (format3_2 (ADD, psp, imm, psp), cvec)
        end;

  (* Called when about to generate a jump instruction. If we have a pending
     instruction we may be able to put it in the delay slot. This is called
     instead of doPendingStackAdjustment in those cases. *)
  fun putPendingInDelay (cvec : code) : quad = (* returns instruction to put in delay slot *)
  let
    (* Must take a jump here if it is pending. Can't put a jump in a delay slot. *)
    val () = reallyFixup cvec;
    (* justComeFrom cvec = [] *)
    
    val reset = ! (stackReset cvec) * wordSize;
  in 
    if reset = 0 then nopQuad (* nothing useful to do *)
    else let
       val (cl,imm) = immedCodeOffset reset;
       val () = genInstructionList (cvec, cl);
       val () = stackReset cvec := 0;
    in
      format3_2 (ADD, psp, imm, psp)
    end
  end;

  (* Apparently fix up jumps - actually just record where we have come from *)
  fun fixup (lab : labels, cvec : code) : unit =
  let
    (* If the jump we are fixing up is an immediately preceding unconditional
       jump, we can remove it. This is particularly useful if we have used
       the delay slot for the whole of an else-part so we have nothing to
       jump over. It is also used for reducing the code for "andalso"
       and "orelse" to the minimal forms. N.B. this is only safe because
       the instruction in the delay slot is never itself the target of
       a jump, and because each label is only fixed up once. SPF 5/6/95.
        
       This is also only safe if we've exited, as otherwise the current
       instruction might be the target of a jump backwards, or it
       might be the start of a handler. N.B. this *can* happen
       if the body of the handler is trivial, so that it just falls
       through to the post-handler code. I discovered this the hard way,
       of course. SPF 27/6/95.
    *) 
    fun checkLabs []            = []
      | checkLabs (addr::addrs) =
        if ! (ic cvec) wordAddrMinus addr = 2
        then let
          val instrQuad = getCodeQuad (addr, cvec);
        in
          if instrQuad = branchAlwaysQuad
          then let
            val delayQuad = getCodeQuad (addr wordAddrPlus 1, cvec);
            
            (* skip back two instructions *)
            val () = ic cvec := addr;
            
            (* put the delay slot back into the instruction flow,
               using genCodeQuad not genInstruction to avoid 
               prematurely fixing up any jumps. *)
            val () =
              if delayQuad <> nopQuad
              then genCodeQuad (delayQuad, cvec)
              else ();
              
            (* We're now falling-through (rather than jumping) *)
            val () = exited cvec := false;
          in
            addrs
          end
          else addr :: addrs (* we can't do anything clever *)
        end
        else addr :: checkLabs addrs; (* try the next jump *)
        
    (* We can't optimise if we haven't exited. *)
    fun checkLabsCarefully l =
      if ! (exited cvec) then checkLabs l else l;
  in
    case lab of
      [] => () (* we're not actually jumping from anywhere *)
    | _ =>
       (
    (* Since this is the start of a basic block,
       we must clear any caches that we maintaining.
       If we have a write-back (rather than write-through)
       cache, this may involve generating some code. *)
    clearCache cvec;
    
    (* Any pending stack reset must be done now.
       That may involve fixing up pending jumps. *)
    doPendingStackAdjustment cvec;
      
    (* Add together the jumps to here. *)
    justComeFrom cvec := checkLabsCarefully lab @ !(justComeFrom cvec)
      )
  end;

  (* Adds in the reset. *)
  fun resetStack (offset : int, cvec : code) : unit =
    stackReset cvec := ! (stackReset cvec) + offset;

  (* Generates an unconditional branch. *)
  fun unconditionalBranch (cvec : code) : labels =
    if unreachable cvec then []
    
    else if ! (stackReset cvec) = 0
    then let
      (* If we are branching and we have just arrived from somewhere
         else we can combine this jump with the one we had just made.
         If we had an unconditional branch just before, we don't need
         to actually put a branch. *)
      val lab =
        if !(exited cvec) then !(justComeFrom cvec) (* no branch needed *)
        else
          ( 
            (* Use genCodeQuad not genInstruction to avoid fixing up
               the outstanding branches. *)
            genCodeQuad (branchAlwaysQuad, cvec); (* ba *)
            genCodeQuad (nopQuad, cvec);
            exited cvec := true;
            (!(ic cvec) wordAddrPlus ~2) :: !(justComeFrom cvec)
          );
    in
      justComeFrom cvec := [];
      lab
    end
    
    else let (* Stack needs adjustment - must generate the branch. *)
      val delayQuad = putPendingInDelay cvec; (* may generate code *)
    in
      genInstruction (branchAlwaysQuad, cvec); (* ba - no annul *)
      genInstruction (delayQuad, cvec);
      exited cvec := true;
      [!(ic cvec) wordAddrPlus ~2]
    end;

  (* Generates an unconditional call. *)
  fun genCall (cvec : code)  =
    let
      val ()  = clearCache cvec; (* end of basic block *)
      val delayQuad = putPendingInDelay cvec; (* may generate code *)
    in
      genInstruction (call0, cvec);
      genInstruction (delayQuad, cvec);
      !(ic cvec) wordAddrPlus ~2
    end;

  fun putConditional (test : testCode, cvec : code) : labels =
    if unreachable cvec then [] (* SPF 5/6/95 *)
    else let
      val ()  = clearCache cvec; (* end of basic block *)
      val delayQuad = putPendingInDelay cvec; (* may generate code *)
      
      (* If we have a pending instruction we put it in the delay slot
         and execute it whether or not we have take the branch, otherwise
         we put in a no-op and set the annul bit so that the delay slot
         can be filled in by the code to be executed at the destination
         of the branch. *)
      val annul = (delayQuad = nopQuad);
    in
      genInstruction (branch (annul, test, immed22_0), cvec);
      genInstruction (delayQuad, cvec);
      (* Make a label for this instruction. *)
      [! (ic cvec) wordAddrPlus ~2]
    end;


  (* Most of the arithmetic operations are of this form. *)
  (* Since they won't trap, we don't need to fix-up the stack;
     however, can we be sure that the stack-pointer isn't one
     of the operands? Let's check! SPF 16/5/95 *)
  fun type2 (c2 : op3_2) (r : reg, m : mode13, rd : reg, cvec : code) : unit =
  let
    val () =
      if cached (rd, cvec)
      then clearCache cvec
      else ();
  
    val () = 
      if r  regEq psp orelse
         rd regEq psp orelse
         eqMode13 m pspM
      then doPendingStackAdjustment cvec
      else ();
  in
    genInstruction (format3_2 (c2, r, m, rd), cvec)
  end;

  val genAnd      = type2 AND;
  val genOr       = type2 OR;
  val genXor      = type2 XOR;
  val genAndn     = type2 ANDN;
  val genSub      = type2 SUB; 
  val genAddcc    = type2 ADDcc; 
  val genAndcc    = type2 ANDcc;
  val genSubcc    = type2 SUBcc;
  val genSll      = type2 SLL;
  val genSrl      = type2 SRL;
  val genSra      = type2 SRA;
  
  (* Special case for loading register from stack pointer. SPF 16/5/95 *)
  fun genAdd (r : reg, m : mode13, rd : reg, cvec : code) : unit =
    if r regEq psp andalso rd regNeq psp andalso isImm13 m
    then let
      val m' = getImm13 m + 4 * !(stackReset cvec);
    in
      if is13Bit m'
      then 
        (
          if cached (rd, cvec) then clearCache cvec else ();
          genInstruction (format3_2 (ADD, psp, immed13 m', rd), cvec)
        )
      else type2 ADD (r, m, rd, cvec)
    end
    else type2 ADD (r, m, rd, cvec);

  (* subtract, but "save" result in g0 *)
  fun genCmp (r : reg, m : mode13, cvec : code) : unit =
    genSubcc (r, m, g0, cvec);

  (* or the source with 0 and put it in the destination. *)
  fun genMove (source : mode13, dest : reg, cvec : code) : unit = 
    if eqMode13 source pspM andalso dest regNeq psp
    then let
      val adj = 4 * !(stackReset cvec);
    in
      if is13Bit adj
      then let
        val () =
          if cached (dest, cvec) then clearCache cvec else ();
      in
        genInstruction (format3_2 (ADD, psp, immed13 adj, dest), cvec)
      end
      else genOr (g0, source, dest, cvec)
    end
    else genOr (g0, source, dest, cvec);
  
  (* These instruction may trap, so we must fix-up the stack. *)
  fun type2T (c2 : op3_2) (r : reg, m : mode13, rd : reg, cvec : code) : unit =
    (
      (* The emulation of these instructions sometimes trashes i4,
         so clear the cache. *)
      clearCache cvec; 
      doPendingStackAdjustment cvec;
      genInstruction (format3_2 (c2, r, m, rd), cvec)
    );

  val genTaddcctv = type2T TADDccTV;
  val genTsubcctv = type2T TSUBccTV;

(***************************************************************************  
  Functions that use cached values.
***************************************************************************)  
  
  val genUntagToI4 : reg * code -> unit = readCache;
  val genTagFromI4 : reg * code -> unit = writeCache;
  
  fun genUntagToI5 (fromReg, cvec) =
    (* with a write-back cache, the value may *only* be in i4 *)
    if cached (fromReg, cvec) 
    then genMove (i4M, i5, cvec)
    else genSub (fromReg, immed13_DATATAG, i5, cvec)

(***************************************************************************)

  (* N.B. uses i5, which is never cached, so no "clearCache cvec" needed. *)
  fun genImmedData (cvec : code, v : int) : mode13 =
  let
    val (cl,loc) = immedCodeData v
  in 
    (case cl of
       [] => ()
     | _  => (* no need to adjust stack *) genInstructionList (cvec, cl));
     
    loc
  end;

  fun genImmedOffset (cvec : code, v : int) : mode13 =
  let
    val (cl,loc) = immedCodeOffset v
  in 
    (case cl of
       [] => ()
     | _  => (* no need to adjust stack *) genInstructionList (cvec, cl));
     
    loc
  end;

  fun genLoadImmed (v : int, rd : reg, cvec : code) : unit =
  let
    val () = 
      if cached (rd, cvec)
      then clearCache cvec
      else ();

    (* We shouldn't load immediates into psp, but if we try to,
       we have to ensure that it won't be corrupted by a 
       subsequent stack adjustment. SPF 16/5/95 *)
    val () = 
      if rd regEq psp
      then doPendingStackAdjustment cvec
      else ();
  in
    if is13Bit v
    then genMove (immed13 v, rd, cvec)
    else let
      val (hi,lo) = splitHiLo v;
    in
      (* sethi %hi(v),%rd; add %rd,%lo(v),%rd; *)
      genInstruction (sethi (hi, rd), cvec);
      
      if eqMode13 lo immed13_0
      then () (* minor optimisation *)
      else genAdd (rd, lo, rd, cvec)
    end
  end;

  (* This should be equivalent to the following definition:
     
       fun genLoadLengthWordImmed (flags, wordCount, rd, cvec) =
         genLoadImmed (flags * exp2_24 + wordCount, rd, cvec);
          
     except that the following version has been specially coded
     to avoid the use of compile-time long arithmetic. This little
     hack reduces compile-time for typical programs by several percent! 
        
     SPF 19/12/95
  *)
  (* We should have 0 <= flags < exp2_8; 0 < wordCount < exp2_24 *)
  fun genLoadLengthWordImmed (flags:int, wordCount:int, rd : reg, cvec) : unit =
  let
    val () = 
      if cached (rd, cvec)
      then clearCache cvec
      else ();

    (* We shouldn't load immediates into psp, but if we try to,
       we have to ensure that it won't be corrupted by a 
       subsequent stack adjustment. SPF 16/5/95 *)
    val () = 
      if rd regEq psp
      then doPendingStackAdjustment cvec
      else ();
  in
    if flags = 0 andalso is13Bit wordCount
    then genMove (immed13 wordCount, rd, cvec)
    else let
      (* 8 bits + 14 bits + 10 bits = 32 bits *)
      val hi = immed22 (flags * exp2_14 + wordCount div exp2_10);
      val lo = immed13 (wordCount mod exp2_10);
    in
      (* sethi %hi(v),%rd; add %rd,%lo(v),%rd; *)
      genInstruction (sethi (hi, rd), cvec);
      
      if eqMode13 lo immed13_0
      then () (* minor optimisation *)
      else genAdd (rd, lo, rd, cvec)
    end
  end;

  fun genLoadInstr (iop : op3_3) (rb : reg, m : mode13, rd : reg, cvec : code) : unit =
  let
    val () = 
      if cached (rd, cvec)
      then clearCache cvec
      else ();

    (* Do we need to fix-up the stack pointer? *)
    val () = 
      if rd regEq psp orelse
     rb regEq psp orelse
     eqMode13 m pspM  
      then doPendingStackAdjustment cvec
      else ()
  in
    genInstruction (format3_3 (iop, rb, m, rd), cvec)
  end;

  fun genLoad (offset : int, rb : reg, rd : reg, cvec : code) : unit =
  let
    val () = 
      if cached (rd, cvec)
       then clearCache cvec
      else ();

    (* Do we need to fix-up the stack pointer? *)
    val () = 
      if rd regEq psp
      then doPendingStackAdjustment cvec
      else ();
      
    val adjustedOffset = 
      if rb regEq psp
      then offset + 4 * !(stackReset cvec)
      else offset;
      
    val imm = genImmedOffset (cvec, adjustedOffset);
  in
    genInstruction (format3_3 (LD, rb, imm, rd), cvec)
  end;

  datatype storeWidth = STORE_WORD | STORE_BYTE

  (* NB Load and Store have registers the other way round *)
  fun genStoreOp iop (rd : reg, offset : int, rb : reg, cvec : code) : unit =
  let
    (* Do we need to fix-up the stack pointer?  Yes, if that's the register we're storing. *)
    val () = 
      if rd regEq psp
      then doPendingStackAdjustment cvec
      else ();
      
    val adjustedOffset = 
      if rb regEq psp
      then offset + 4 * !(stackReset cvec)
      else offset;
      
    (* We can guarantee that rd <> %i3, so this is always safe. This wouldn't
       necessarily be the case if we used genImmedData instead of genImmedOffset.
       We might be called with rd = %i5 together with a large (non 13-bit) offset,
       and then everything would go horribly wrong. (Yes, it DID happen.)
       SPF 2/4/97
     *)
    val imm = genImmedOffset (cvec, adjustedOffset);
  in
    genInstruction (format3_3 (iop, rb, imm, rd), cvec)
  end;

  (* General purpose store function.  Since we don't allow indexed stores
     index should always be regNone. *)
  fun genStore (rd : reg, offset : int, rb : reg, STORE_WORD, index: reg, cvec : code) =
    if index regEq regNone
    then genStoreOp ST (rd, offset, rb, cvec)
    else
    (
        (* At present the offset will always be zero if there is an index register. *)
        if offset = 0 then ()
        else raise InternalError "genStore: index register and non-zero offset";
        (* Untag the index. *)
            genUntagToI4 (index, cvec);
        genInstruction (format3_3 (ST, rb, i4M, rd), cvec)
    )
  |   genStore(rd : reg, offset : int, rb : reg, STORE_BYTE, index: reg, cvec : code) =
       (
       (* Untag the value to store. *)
       genSrl (rd, immed13_TAGSHIFT, i5, cvec);
       if index regEq regNone
       then genStoreOp STB (i5, offset, rb, cvec)
       else
        (
        (* At present the offset will always be zero if there is an index register. *)
        if offset = 0 then ()
        else raise InternalError "genStore: index register and non-zero offset";
        (* Shift index to remove tag and get byte-index *)
        clearCache cvec;
            genSra (index, immed13_TAGSHIFT, i4, cvec);
        genInstruction (format3_3 (STB, rb, i4M, i5), cvec)
        )
       );

  val genStoreWord = genStoreOp ST;
  val genStoreByte = genStoreOp STB;

  (* Start inlining assignments. *)
  val inlineAssignments: bool = true
  fun isIndexedStore _ = true
 
  (* Exported - Can we store the value without going through a register?
     Not really.  But if we're going to do a store-byte we have to untag the
     value and it will definitely be better to do this by loading the untagged
     value directly into the register.  This is very infrequent though, so for
     the moment we don't do it. *)
  fun isStoreI _ : bool = false;

  fun genStoreI _ : unit =
    raise InternalError "Not implemented: genStoreI";

  (* Store a value on the stack.  This is used when the registers need to be
     saved, for more than 4 arguments or to push an exception handler. *)
  fun genPush (r : reg, cvec : code) : unit =
  let
    (* If the adjusted byte-offset won't fit into 13 bits, flush the stack reset. *)
    val () =
      if is13Bit (4 * (!(stackReset cvec) - 1)) then ()
      else doPendingStackAdjustment cvec;
      
    (* generate the store *)
    val () = genStoreWord (r, ~4, psp, cvec)
  in
    (* Finally adjust the stack reset (rather than adjusting the stack pointer itself). *)
    stackReset cvec := !(stackReset cvec) - 1
  end;

  (* Load a value and push it on the stack. Used when all
     the allocatable registers have run out. *)
  (* Use i5 as a spare register. *)
  fun genLoadPush (offset : int, base : reg, cvec : code) : unit =
  let
    val () = genLoad (offset, base, i5, cvec);
  in
    genPush (i5, cvec)
  end;

  (* This is false because there's no quicker way than loading
     into a register and then pushing that. *) 
  val preferLoadPush = false;

  fun genJmpl (m : mode13, breg : reg, lreg : reg, cvec : code) : unit =
  let
    val ()  = clearCache cvec;        (* end of basic block *)
    val delayQuad = putPendingInDelay cvec; (* may generate code *)
  in
    (* jmpl breg+m,lreg *)
    genInstruction (format3_2 (JMPL, breg, m, lreg), cvec);
    genInstruction (delayQuad, cvec);
    
    (* we don't come back from a tailcall, so we've exited *)
    if lreg regEq g0 then exited cvec := true else ()
  end;

  (* Make a reference to another procedure. Usually this will be a forward *)
  (* reference but it may have been compiled already, in which case we can *)
  (* put the code address in now. *)
  fun codeConst (Code {resultSeg = ref(Set(seg, _)), ... }, _) =
    (* Already done. *) WVal (toMachineWord(csegAddr seg))
  |  codeConst (r, into)  = (* forward *)
      (* Add the referring procedure onto the list of the procedure
         referred to if it is not already there. This makes sure that when
         the referring procedure is finished and its address is known the
         address will be plugged in to every procedure which needs it. *)
      let
        fun onList _ []      = false
          | onList x (c::cs) = (x is c) orelse onList x cs;
          
        val codeList = ! (otherCodes r);
      in
        if onList into codeList then () else otherCodes r := into :: codeList;
        CVal r
      end


    (* N.B. genLoadConstant must be very careful to do nothing
       for unreachable code, because otherwise genStoreOp won't generate
       any code, but fixupConstantLoad will later try to fixup a non-existant load.
       SPF 28/11/95
     *)
  fun genLoadConstant (cnstnt, destR : reg,
                       cvec as Code{numOfConsts, constVec, ic, ...}) : unit =
      if unreachable cvec then ()
      else
         (  
         (* We can't afford to put this instruction in a delay slot, 
            because we're trying to record its address, so force fix-up now. *)
         reallyFixup cvec;
         numOfConsts := ! numOfConsts + 1;
         constVec := (cnstnt, !ic) :: ! constVec;
         (* Generate a pair of instruction containing tagged 0.  We
            mustn't put in the real value at the moment because this
            is a byte segment and so the value won't be updated as a
            result of any garbage collection. *)
         let
            val (hi,lo) = splitHiLo(tagged 0)
         in
            genInstruction (sethi (hi, destR), cvec);
            genAdd (destR, lo, destR, cvec)
         end
      );

  fun genLoadCoderef (rf : code, destR : reg, cvec : code) : unit =
      if unreachable cvec then ()
      else genLoadConstant (codeConst (rf, cvec), destR, cvec)
  
  type handlerLab = addrs ref;
    
  fun loadHandlerAddress (destR : reg, cvec : code) : handlerLab =
    let
      val lab : handlerLab = ref addrZero;
    in
      genLoadConstant (HVal lab, destR, cvec);
      lab
    end;

  fun fixupHandler (lab : handlerLab, cvec : code) : unit =
  ( 
    clearCache cvec; (* start of basic block *)
    doPendingStackAdjustment cvec; 
    reallyFixup cvec;
    exited cvec := false;
    lab := ! (ic cvec)
  );

  datatype callKinds =
        Recursive           (* The function calls itself. *)
    |   ConstantFun of machineWord * bool (* A pre-compiled or io function. *)
    |   CodeFun of code     (* A static link call. *)
    |   FullCall            (* Full closure call *)

  (* NB this version of the compiler now assumes that the first instruction 
     of the called function is ALWAYS "or %o7,2,%o7". (I had to rewrite
     the run-time system to achieve this.) We skip this instruction whenever
     possible. It would be nice to put it in the delay slot instead, but
     that would give us serious backwards comaptibility problems (our
     code couldn't be called by the Poly version of the compiler).
     The answer is to bootstrap a version of the source that does the
     check in BOTH places, then use it to bootstrap the version that
     only checks in the delay slot. Probably not worth the trouble though.
     SPF 14/10/94

     I don't think this applies any longer.  DCJM 25/7/06.
    *)

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
  (* Call a function. We have to set the stack-check flag 
     to ensure that the called procedure receives its full
     minStackCheck words allocation of "free" stack. *)
  fun callFunction (callKind : callKinds, cvec as Code{constVec, numOfConsts, ...} ) : unit =
    (* Mustn't add to selfCalls list unless we're actually generating code! *)
    if unreachable cvec then ()
    else
     (
     mustCheckStack cvec := true;

     case callKind of 
       Recursive =>
       let
          val lab = genCall cvec;
       in
          selfCalls cvec := lab :: !(selfCalls cvec)
       end
     
     | FullCall =>
        (
          genLoad (0, regClosure, i2, cvec);
          genJmpl (immed13_0, i2, regReturn, cvec)
        )

     | ConstantFun(w, false) =>
        let
           val lab = genCall cvec
        in
           numOfConsts := ! numOfConsts + 1;
           constVec := (WVal w, lab) :: ! constVec
        end
     
     | ConstantFun(w, true) =>
        (
          (* We can't use a CALL instruction here because we're calling
             into the RTS. *)
          (* We have to use i2 here because we want the GC to recognise this
             as an address. *)
          genLoadConstant (WVal w, regClosure, cvec);
          genLoad (0, regClosure, i2, cvec);
          genJmpl (immed13_0, i2, regReturn, cvec)
        )

     | CodeFun c =>
        let
           val lab = genCall cvec
        in
           numOfConsts := ! numOfConsts + 1;
           constVec := (codeConst(c, cvec), lab) :: ! constVec
        end
   )

  (* Tail recursive jump to a function. We have to set the stack-check
     flag to enable the user to break out of loops. *)
  fun jumpToFunction (callKind : callKinds, returnReg : reg, cvec : code) : unit =
    (* Mustn't add to selfJumps list unless we're actually generating code! *)
    if unreachable cvec then ()
    else let
      val () =
      (* Put return address in the correct register. *)
      if returnReg regEq regReturn then ()
      else genMove (mReg returnReg, regReturn, cvec);
    in
      case callKind of
         Recursive =>
         let
           val () = mustCheckStack cvec := true;
           val lab = unconditionalBranch cvec;
         in
           selfJumps cvec := lab @ !(selfJumps cvec)
         end
           
       | FullCall =>
          (
           mustCheckStack cvec := true;
           genLoad (0, regClosure, i2, cvec);
           (* Jump past the or %o7,2,%o7 at the start of every function. *)
           genJmpl (immed13_4, i2, g0, cvec)
          )

       | ConstantFun(w, false) =>
          (
           mustCheckStack cvec := true;
           (* We MUST use i2 here because we want the GC to recognise this
              as an address. *)
           genLoadConstant (WVal w, i2, cvec);
           (* Jump past the or %o7,2,%o7 at the start of every function. *)
           genJmpl (immed13_4, i2, g0, cvec)
          )
    
     | ConstantFun(w, true) =>
          (
           (* Indirect jumps are used to call into the RTS.  No need
              to check the stack. *)
           genLoadConstant (WVal w, regClosure, cvec);
           genLoad (0, regClosure, i2, cvec);
           (* Jump past the or %o7,2,%o7 at the start of every function. *)
           genJmpl (immed13_4, i2, g0, cvec)
          )

     | CodeFun c =>
          (
           mustCheckStack cvec := true;
           (* We MUST use i2 here because we want the GC to recognise this
              as an address. *)
           genLoadCoderef (c, i2, cvec);
           (* Jump past the or %o7,2,%o7 at the start of every function. *)
           genJmpl (immed13_4, i2, g0, cvec)
          )
    end;
      

  (* Return and remove args. *)
  fun returnFromFunction (resReg : reg, args : int, cvec : code) : unit =
   ( 
     resetStack (args, cvec); (* Add in the reset. *)
     genJmpl (immed13_6, resReg, g0, cvec)
   );

  (* The exception argument has already been loaded into o0.
     We initialise o7 (the return address) because this is used by
     the RTS exception handler, when it does a stack trace. *)
  fun raiseException cvec =
     (
          genLoad (MemRegRaiseException, i0, i2, cvec);
          genJmpl (immed13_0, i2, regReturn, cvec)
     )

  (* Exported. Set a register to a particular offset in the stack. *)
  fun genStackOffset (reg, byteOffset, cvec) : unit = 
    genAdd (regStackPtr, genImmedOffset(cvec, byteOffset), reg, cvec);

  (* Trap instruction. Causes trap if garbage-collection is needed or
     when the stack is about to overflow. *)
  fun genTrapls (cvec : code) : unit =
    ( 
      doPendingStackAdjustment cvec;
      genInstruction (tlu24, cvec)
    );


  (* Only used for while-loops. Could make use of the delay slot
     but probably not worth it. *)
  fun jumpback (lab : addrs, stackCheck : bool, cvec : code) : unit =
    (
      clearCache cvec; (* end of basic block (not needed if we exit?) *)
      doPendingStackAdjustment cvec;
    
      (* Put in a stack check. This is used to allow the
         code to be interrupted. *)
      if stackCheck
      then
        (
          (* If we trap the trap handling code examines the previous
             instruction to see how big to make the stack.  That's
             really only relevant at the start of a function but we
             must ensure that the instruction immediately before the
             trap is a comparison.  To do that we must make sure we don't
             move the comparison into a delay slot. *)
          (* That may no longer be a problem now we have to load a reg first. *)
          genLoad (MemRegStackLimit, i0, i3, cvec);
          reallyFixup cvec;
          genCmp (psp, i3M, cvec);
          genTrapls cvec
         )
      else ();
    
      reallyFixup cvec;
    
      let
        val wordOffset : int = lab wordAddrMinus (! (ic cvec)); (* Negative *)
      in
        genInstruction (branch (true, always, immed22 wordOffset), cvec)
      end;
      
      exited cvec := true
    );

  (* Allocate store and put the resulting pointer in the result register. *)
  fun allocStore (length : int, flag : Word8.word, resultReg : reg, cvec : code) : unit =
    if length < 1 orelse exp2_24 <= length
    then raise InternalError "allocStore: bad length"
    else let
      val bytes : int = (length + 1) * wordSize;
    in
      let
        val imm = genImmedData (cvec, bytes); (* may generate code *)
      in
        (* Decrement the store available (-maxint) and trap if it overflows. *)
        (* sub %g6,bytes,%g6; tsubcctv %g5,bytes,%g5 *)
        genSub (g6, imm, g6, cvec);
        genTsubcctv (g5, imm, g5, cvec) (* special case of tsubcctv *)
      end;
     
      (* Set the size field of a newly allocated piece of store
     and include the `mutable' bit. *)
      genLoadLengthWordImmed (Word8.toInt flag, length, i5, cvec);
      genMove  (g6M, resultReg, cvec);
      genStoreWord (i5, ~4, g6, cvec)
    end;

  (* Remove the mutable bit; only save for word objects. *)
  fun setFlag (baseReg : reg, cvec : code, flag : Word8.word) : unit =
  let
    val flagRep = Word8.toInt flag;
  in
    if flagRep = 0
    then genStoreByte (g0, ~4, baseReg, cvec)
    else 
      (
         genLoadImmed (flagRep, i5, cvec);
         genStoreByte (i5, ~4, baseReg, cvec)
      )
  end

  (* Don't need to do anything on the Sparc. *)
  val completeSegment = (fn (_ : code) => ());

  datatype instrs = 
    instrMove
  | instrAddA
  | instrSubA
  | instrRevSubA
  | instrMulA
  | instrAddW (* Various word functions added DCJM 17/4/00 *)
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
  | instrPush  (* added 12/9/94 SPF for v2.08 compiler *)
  | instrUpshiftW    (* logical shift left *)
  | instrDownshiftW  (* arithmetic shift right *)
  | instrDownshiftArithW  (* arithmetic shift right - added 17/4/00 DCJM *)
  | instrGetFirstLong
  | instrStringLength
  | instrSetStringLength
  | instrBad;
   

  (* Can the we use the same register as the source and destination
     of an instructions? On this machine - yes. *)
  val canShareRegs : bool = true;

  (* Is there a general register/register operation? Some operations may not
     be implemented because this machine does not have a suitable instruction
     or simply because they have not yet been added to the code generator. It
     is possible for an instruction to be implemented as a register/immediate
     operation but not as a register/register operation (e.g. multiply) *) 
  fun instrIsRR i = 
    case i of
      instrMulA       => false
    | instrMulW       => false
    | instrDivW       => false
    | instrModW       => false
    | instrVeclen     => false (* immediate form only *)
    | instrVecflags   => false (* immediate form only *)
    | instrUpshiftW   => false
    | instrDownshiftW => false
    | instrDownshiftArithW => false
    | instrGetFirstLong => false (* immediate form only *)
    | instrStringLength => false (* immediate form only *)
    | instrSetStringLength => true
    | _               => true;
  
  datatype tests =
    Short
  | Long
  | Arb of testCode
  | Wrd of testCode;
  
  val testNeqW  = Wrd ne;
  val testEqW   = Wrd e;
  val testGeqW  = Wrd cc; (* Word tests are UNsigned.  DCJM 17/4/00. *)
  val testGtW   = Wrd gu;
  val testLeqW  = Wrd leu;
  val testLtW   = Wrd cs;
  val testLtSignedW = Wrd l; (* These are needed for case checks. *)
  val testGtSignedW = Wrd g;
  val testNeqA  = Arb ne;
  val testEqA   = Arb e;
  val testGeqA  = Arb ge;
  val testGtA   = Arb g;
  val testLeqA  = Arb le;
  val testLtA   = Arb l;
  
  fun isLongTest  Long    = true | isLongTest   _ = false;
  fun isShortTest Short   = true | isShortTest  _ = false;
  fun isArb       (Arb _) = true | isArb        _ = false;
  fun isWrd       (Wrd _) = true | isWrd        _ = false;

  fun tstArb  (Arb x) = x | tstArb   _ = raise Match;
  fun tstWrd  (Wrd x) = x | tstWrd   _ = raise Match;

  (* Is this argument acceptable as an immediate or should it be loaded into a register? *) 
  fun instrIsRI (i : instrs, cnstnt:machineWord) : bool =
    (* For most instructions this is simply a question of whether the
       tagged value will fit in 13 (signed) bits. For the multiply 
       instruction this is a question of whether it is small enough
       to be implemented by repeated addition, but 1024 seems like
       a reasonable limit. *)
    let
        fun isShiftable c = 0 <= c andalso c < (32 - TAGSHIFT)
    in
      case i of
        instrBad => false

      | instrMove => true (* Must always be implemented. *)
          
      | instrPush => false
          
      | instrUpshiftW =>
           isShort cnstnt andalso isShiftable(toInt (toShort cnstnt))

      | instrDownshiftW =>
           isShort cnstnt andalso isShiftable(toInt (toShort cnstnt))

      | instrDownshiftArithW =>
           isShort cnstnt andalso isShiftable(toInt (toShort cnstnt))

      | instrDivW => false

      | instrModW => false

      | instrMulW => false (* At least for the moment *)

      | instrVeclen  => true

      | instrVecflags  => true
       
      | instrLoadB => isShort cnstnt andalso is13Bit(toInt (toShort cnstnt))
      
      | instrLoad  => isShort cnstnt andalso isTaggable13Bit(toInt (toShort cnstnt))

      | instrGetFirstLong => false (* Probably too difficult. *)

      | instrStringLength => true

      | instrSetStringLength => false (* Not at the moment. *)
       
      | _  => isShort cnstnt andalso isTaggable13Bit(toInt (toShort cnstnt))
    end
    
    
   (* untag the arguments into the working registers, returning their locations in order *)
   fun untagBothArgs (r1 : reg, r2 : reg, cvec : code) : reg * mode13 =
(* The following would be unsafe because the RTS assumes that (i4+1) and (i5+1)
   are valid tagged integers whenever we do a taddcctv or tsubcctv with a non-constant
   argument, which wouldn't necessarily be the case if we don't explicitly initialise
   both of them. *)
(* ...
     if r1 regEq r2
     then
       (
         genUntagToI4 (r1, cvec);
         (i4, i4M)
       )
     else 
... *)
     
     if cached (r1, cvec)
     then (* r1 is cached *)
       (
         genUntagToI4 (r1, cvec);  (* "just in case" *)
         genUntagToI5 (r2, cvec);
         (i4, i5M)
       )
     else (* perhaps r2 is cached *)
       (
         genUntagToI4 (r2, cvec);
         genUntagToI5 (r1, cvec);
         (i5, i4M)
       );

   (* useful for symmetric functions only *)
   fun untagOneArg (r1 : reg, r2 : reg, cvec : code) : reg * mode13 =
     if cached (r1, cvec)
     then (* r1 is cached *)
       (
         genUntagToI4 (r1, cvec); (* "just in case" *)
         (r2, i4M)
       )
     else (* perhaps r2 is cached *)
       (
         genUntagToI4 (r2, cvec);
         (r1, i4M)
       );
  (* All comparisons are implemented. *)
  fun isCompRR (tc : tests) : bool =
    case tc of
      Short => false
    | Long  => false
    | Wrd _ => true 
    | Arb _ => true;
  
  (* Is this argument acceptable as an immediate or should it be *)
  (* loaded into a register? *) 
  fun isCompRI (tc : tests, cnstnt : machineWord) : bool =
    isLongTest tc orelse
    isShortTest tc orelse 
   (isShort cnstnt andalso (* Must fit in immediate field. *)
    isTaggable13Bit (toInt (toShort cnstnt)));

  (* Fixed and arbitrary precision comparisons. *)
  fun compareAndBranchRR (r1 : reg, r2 : reg, tc : tests, cvec : code) : labels =
    if isArb tc
    then let (* Must remove the tags before doing the operation. *)
      val (arg1, arg2M) = untagBothArgs (r1, r2, cvec)
    in
      genTsubcctv (arg1, arg2M, g0, cvec); (* both i4 and i5 are "legal" *)
      putConditional (tstArb tc, cvec)
    end
      
    else if isWrd tc
    then (* Fixed *)
      (
        genCmp (r1, mReg r2, cvec);
        putConditional (tstWrd tc, cvec)
      )
      
    else
      raise InternalError "compareAndBranchRR: Unimplemented test";

  fun compareAndBranchRI (r:reg, cnstnt:machineWord, tc : tests, cvec) : labels =
    if isShortTest tc
    then
      ( 
        genAndcc (r, immed13_DATATAG, g0, cvec);
        putConditional (ne, cvec)
      )
    
    else if isLongTest tc
    then
      (
        genAndcc (r, immed13_DATATAG, g0, cvec);
        putConditional (e, cvec)
      )
    
    else let
      val c = toInt (toShort cnstnt);
    in
      if isWrd tc (* Fixed *)
      then
        (
          genCmp (r, immed13 (tagged c), cvec);
          putConditional (tstWrd tc, cvec)
        )
      
      else let (* isArb tc *)
        (* If we are testing for equality with a
            constant then we can simply test directly. *)
        val tk = tstArb tc;
      in
        if eqTest tk e orelse eqTest tk ne
        then genCmp (r, immed13 (tagged c), cvec)
        else  (* Must remove the tags before doing the operation. *)
        ( 
          genUntagToI4 (r, cvec);
          genTsubcctv (i4, immed13 (semiTagged c), g0, cvec)
        );
        
        putConditional (tk, cvec)
      end
    end;

  (* General register/register operation. *)
  fun genRR (inst : instrs, r1 : reg, r2 : reg, rd : reg, cvec : code) : unit =
    case inst of
      instrMove => (* Move from one register to another. r2 is ignored *)
        genMove (mReg r1, rd, cvec)
      
    | instrPush => (* Both rd and r2 are ignored. *)
        genPush (r1, cvec)

    | instrAddA => (* Arbitrary precision addition. *)
      let
        val (arg1, arg2M) = untagBothArgs (r1, r2, cvec)
      in
        genTaddcctv (arg1, arg2M, i4, cvec); (* both i4 and i5 are "legal" *)
    genTagFromI4 (rd, cvec) 
      end
      
    | instrSubA =>  (* Arbitrary precision subtraction. *)
      let
        val (arg1, arg2M) = untagBothArgs (r1, r2, cvec)
      in
        genTsubcctv (arg1, arg2M, i4, cvec); (* both i4 and i5 are "legal" *)
    genTagFromI4 (rd, cvec) 
      end

    | instrRevSubA => (* Arbitrary precision subtraction. *)
      let
        val (arg2, arg1M) = untagBothArgs (r2, r1, cvec)
      in
        genTsubcctv (arg2, arg1M, i4, cvec); (* both i4 and i5 are "legal" *)
    genTagFromI4 (rd, cvec) 
      end

    | instrMulA => (* Arbitrary precision multiplication. *)
        raise InternalError "genRR: unsupported operation - instrMulA"
  
    | instrAddW => (* Word addition. *)
      (* Untag one of the arguments into i4, then
         add the other one to it. We don't care
         which argument gets untagged (addition is a
         symmetric operation), so we can try to be
         clever with the caching. *)
      let (* Word addition. *)
        val (arg1, arg2M) = untagOneArg (r1, r2, cvec)
      in
        genAdd (arg1, arg2M, rd, cvec) (* Ignore overflow. *)
      end
  
    | instrSubW =>  (* Word subtraction. *)
      (* Remove the tag from the second argument, then do the operation. *)
      ( 
        genUntagToI4 (r2, cvec);
        genSub(r1, i4M, rd, cvec) (* Ignore overflow. *)
      )

    | instrRevSubW => (* Fixed precision reverse subtraction. *)
      (
        genUntagToI4 (r1, cvec);
        genSub (r2, i4M, rd, cvec) (* Ignore overflow. *)
      )

    | instrMulW => (* Word multiplication. *)
        raise InternalError "genRR: unsupported operation - instrMulW"

    | instrDivW => (* Word multiplication. *)
        raise InternalError "genRR: unsupported operation - instrDivW"

    | instrModW => (* Word multiplication. *)
        raise InternalError "genRR: unsupported operation - instrModW"

    | instrOrW => (* tag preserved by operation *)
        genOr (r1, mReg r2, rd, cvec) (* Logical or. *)

    | instrAndW => (* tag preserved by operation *)
        genAnd (r1, mReg r2, rd, cvec) (* Logical and. *)

    | instrXorW =>
      let (* Have to remove the tag from one argument. *)
        val (arg1, arg2M) = untagOneArg (r1, r2, cvec)
      in
        genXor (arg1, arg2M, rd, cvec)
      end
    
    | instrUpshiftW =>
        raise InternalError "genRR: unsupported operation - instrUpshiftW"
      
    | instrDownshiftW =>
        raise InternalError "genRR: unsupported operation - instrDownshiftW"

    | instrDownshiftArithW =>
        raise InternalError "genRR: unsupported operation - instrDownshiftArithW"

    | instrLoad => (* Load a word. *)
      ( (* Remove the tag from the index. *)
        genUntagToI4 (r2, cvec);
        genLoadInstr LD (r1, i4M, rd, cvec)
      )

    | instrLoadB => (* Load a byte. Leaves untagged value in i4 - worthwhile? *)
      ( (* Shift index to remove tag and get byte-index *)
        genSra (r2, immed13_TAGSHIFT, i5, cvec);
        genLoadInstr LDUB (r1, i5M, i5, cvec);
        (* Tag the result. *)
        genSll (i5, immed13_TAGSHIFT, i4, cvec);
        genTagFromI4 (rd, cvec) 
      )

    | instrSetStringLength => (* Set the length word of a string. *)
      (
        (* Untag the value to store. *)
        genSra (r2, immed13_TAGSHIFT, i5, cvec);
        (* Store it in the first word of the new string. *)
        genStoreWord (i5, 0, r1, cvec)
        (* We really should set rd to "unit". *)
      )

    | _ =>
        raise InternalError "genRR: unsupported operation"
   (* end genRR *);

  (* Register/immediate operations.  In many of these operations
     we have to tag the immediate value. *)
  fun genRI (inst : instrs, rs : reg, constnt : machineWord, rd : reg, cvec : code) : unit =
    case inst of
      instrMove => (* Load a constant into a register. rs is ignored. *)
         if isShort constnt andalso isTaggable13Bit(toInt (toShort constnt))
         then genLoadImmed (tagged(toInt (toShort constnt)), rd, cvec)
         else genLoadConstant(WVal constnt, rd, cvec)

    | instrAddA => (* Arbitrary precision addition. *)
    let
        val c = toInt (toShort constnt)
    in (* Must remove the tags before doing the operation. *)
      genUntagToI4 (rs, cvec);
      genTaddcctv (i4, immed13 (semiTagged c), i4, cvec); (* add constant to i4 - OK *)
      genTagFromI4 (rd, cvec) (* Put back the tag in the result. *)
    end

    | instrSubA => (* Arbitrary precision subtraction. *)
    let
        val c = toInt (toShort constnt)
    in
      genUntagToI4 (rs, cvec);
      genTsubcctv (i4, immed13 (semiTagged c), i4, cvec); (* subtract constant from i4 - OK *) 
      genTagFromI4 (rd, cvec) 
    end

    | instrRevSubA  => (* Arbitrary precision reverse subtraction. *)
    let
        val c = toInt (toShort constnt)
    in
      genUntagToI4 (rs, cvec);
      
(* The following would be unsafe because the RTS assumes that (i4+1) and (i5+1)
   are valid tagged integers whenever we do a taddcctv or tsubcctv with a non-constant
   argument, which wouldn't necessarily be the case if we don't explicitly initialise
   both of them. *)
(* ...
      if c = 0 then genTsubcctv (g0, i4M, i4, cvec)
      else
... *)
      ( 
        genMove (immed13 (semiTagged c), i5, cvec);
        genTsubcctv (i5, i4M, i4, cvec)  (* both i4 and i5 are "legal" *)
      );
      genTagFromI4 (rd, cvec) 
    end

    | instrMulA => let
        val c = toInt (toShort constnt)
    
(* The following is tricky because the RTS assumes that (i4+1) and (i5+1)
   are valid tagged integers whenever we do a taddcctv or tsubcctv with a non-constant
   arguments. DCJM's version of the code-generator didn't ensure that this was
   always the case. *)

      (* The sparc does not have a multiply instruction but it may well be
         worth doing multiplications by constants by repeated addition.  Do
         this if the constant is small (1024 seems small). *)
      fun mulConst (cn : int) : unit =
      let
        (* i4 must be "legal" on entry; i5 is trashed; res := c * i4; *)
        fun genMult (c, res) =
          if c = 0
          then genLoadImmed (0, res, cvec)
            
          else if c = 1
          then if res regEq i4 then () else genMove (i4M, res, cvec)
            
          else if c mod 8 = 7 (* DCJM's fancy optimisation *)
          then 
          (
            genMult ((c + 1) div 2, i5); (* initialise i5 *)
            genTaddcctv (i5, i5M, i5, cvec);  (* both i4 and i5 are "legal" *)
            genTsubcctv (i5, i4M, res, cvec)  (* both i4 and i5 are "legal" *)
          )
          
          else if c mod 2 = 1
          then
          (
            genMult (c div 2, i5);       (* initialise i5 *)
            genTaddcctv (i5, i5M, i5, cvec);  (* both i4 and i5 are legal *)
            genTaddcctv (i5, i4M, res, cvec)  (* both i4 and i5 are legal *)
          )
          
          else (* c mod 2 = 0 *)
          (
            genMult (c div 2, i5);       (* initialise i5 *)
            genTaddcctv (i5, i5M, res, cvec)  (* both i4 and i5 are legal *)
          );
      in
        if cn = 0 (* It's possible someone might write this. *)
        then genMove (immed13 (tagged 0), rd, cvec)
        
        else if cn = 1 
        then genMove (mReg rs, rd, cvec)
        
        else if cn < 0
        then (* Must negate the multiplicand. *)
        (
          genUntagToI4 (rs, cvec);
          genMult (~ cn, i4);         (* Initialises i4 *)
          genLoadImmed (0, i5, cvec); (* Puts a legal value into i5 *)
          genTsubcctv (i5, i4M, i4, cvec); (* O.K. since both i4 and i5 are legal *)
          genTagFromI4 (rd, cvec)     (* Put the tag in the result. *)
        )
        
        else (* Positive. *)
    ( 
      genUntagToI4 (rs, cvec);
      genMult (cn, i4);
      genTagFromI4 (rd, cvec)     (* Put the tag in the result. *)
    )
      end (* mulConst *);
    in
      mulConst c
    end  (* isInstrMulA *)

    | instrAddW => (* Word addition. *)
       (* The argument is shifted but not tagged. *)
    genAddcc (rs, immed13 (semiTagged(toInt (toShort constnt))), rd, cvec)

    | instrSubW => (* Fixed precision subtraction. *)
        genSubcc (rs, immed13 (semiTagged(toInt (toShort constnt))), rd, cvec)

    | instrRevSubW => (* Fixed precision reverse subtraction. *)
      (
       (* Add one to the second tagged argument then do the subtraction. *)
       genMove (immed13(tagged(toInt (toShort constnt)) + tagged 0), i5, cvec);
       genSubcc (i5,  mReg rs, rd, cvec)
      )

    | instrOrW =>
        genOr (rs, immed13 (tagged(toInt (toShort constnt))), rd, cvec) (* Logical or. *)

    | instrAndW => genAnd (rs, immed13 (tagged(toInt (toShort constnt))), rd, cvec)

      (* Use untagged value to ensure that tag is not changed. *)
    | instrXorW => genXor (rs, immed13 (semiTagged(toInt (toShort constnt))), rd, cvec)

    | instrUpshiftW => (* logical shift left *)
      (
        genUntagToI4 (rs, cvec);
        genSll (i4, immed13(toInt (toShort constnt)), i4, cvec);
        genTagFromI4 (rd, cvec)
      )  
      
    | instrDownshiftW => (* logical shift right *)
      (
        genSrl  (rs, immed13(toInt (toShort constnt)), i5, cvec);
        genAndn (i5, immed13_TAGMASK, i4, cvec); (* remove stray bits from tag positions *)
        genTagFromI4 (rd, cvec)
      ) 
 
    | instrDownshiftArithW => (* logical shift right *)
      (
        genSra  (rs, immed13(toInt (toShort constnt)), i5, cvec);
        genAndn (i5, immed13_TAGMASK, i4, cvec); (* remove stray bits from tag positions *)
        genTagFromI4 (rd, cvec)
      )  

    (* Load a word. Offset is words so multiply by 4 to get byte offset. *)
    | instrLoad =>
        genLoadInstr LD (rs, immed13 ((toInt (toShort constnt)) * wordSize), rd, cvec)

    | instrLoadB => (* Load a byte.  Offset is bytes. *)
      ( 
        genLoadInstr LDUB (rs, immed13(toInt (toShort constnt)), i5, cvec);
        (* Tag the result. *)
        genSll (i5, immed13_TAGSHIFT, i4, cvec);
        genTagFromI4 (rd, cvec)
      )

    | instrVeclen => (* The second arg. is ignored. *)
      ( 
        genLoad (~4, rs, i5, cvec);
        genSll (i5, immed13_8, i5, cvec);  (* Shift up to clear top byte. *)
        genSrl (i5, immed13_6, i4, cvec);
        genTagFromI4 (rd, cvec)
      )

    | instrVecflags => (* The second arg. is ignored. *)
      (
        (* Load the flag byte: Offset -4 on a big-endian machine. *)
        genLoadInstr LDUB (rs, immed13 ~4, i5, cvec);
        (* Tag the result. *)
        genSll (i5, immed13_TAGSHIFT, i4, cvec);
        genTagFromI4 (rd, cvec)
      )

    | instrStringLength => (* The second arg. is ignored. *)
    let
       (* If it's tagged the result is 1 otherwise we need to load
          the length word and tag it. *)
        val l1 = compareAndBranchRI (rs, toMachineWord 0 (* Unused *), Short, cvec)
        val _ = genLoad  (0, rs, i4, cvec); (* Load the length word. *)
            val _ = genSll (i4, immed13_TAGSHIFT, i4, cvec);
        val l2 = unconditionalBranch cvec
    in
        (* In practice the load-immediate gets put into the delay slot of
           the conditional branch and the unconditional branch is optimised
           away. *)
        fixup(l1, cvec);
        genLoadImmed (semiTagged 1, i4, cvec);
        fixup(l2, cvec);
        (* It's probably better to tag the result here because we may be
           able to optimise arithmetic after it.  If we can't will have
           incurred the cost of an extra instruction in the case when we
           had a single character but this is probably worth paying. *)
        genTagFromI4 (rd, cvec) (* And tag the result. *)
    end

    | _ => raise InternalError "genRI: Unimplemented instruction"
  (* genRI *);

  (* this could be made more efficient (and readable??) using shorts *)
  fun printCode (seg : cseg, procName : string, lastAddr : addrs, printStream) : unit =
  let
    (* prints a string representation of a number *)
    fun printHex v = printStream(Int.fmt StringCvt.HEX v)
    fun regPrint r = printStream(regRepr r)

    fun printRimm iOp =
      if (iOp div exp2_13) mod 2 = 1 (* Set if immediate. *)
      then printHex (signed13 (iOp mod exp2_13))
      else regPrint (mkReg (iOp mod exp2_5));
  in
    if procName = "" (* No name *) then printStream "?" else  printStream procName;
    printStream ":\n";
    let
      val ptr = ref addrZero;
    in
      while !ptr addrLt lastAddr do
      let 
    val thisAddr : addrs = !ptr;
    val () = ptr := thisAddr wordAddrPlus 1;
    val instr    : int = fromQuad (getQuad (thisAddr, seg));

    val byteAddr : int = getByteAddr thisAddr;
    val topByte  : int = Word8.toInt (csegGet (seg, byteAddr));
    val format   : int = topByte div exp2_6;
          
        val () = printHex byteAddr; (* The real address. *)
        val () = printStream "\t";
      
        val () =
          if format = 0
          then let
            val op2 = (instr div exp2_22) mod 8; (* op2 field *)
          in
            case op2 of
              4 => (* setHi *)
                if instr = fromQuad nopQuad (* nop is sethi 0,%g0 *)
                then printStream "nop"
                else
                   ( printStream "sethi\t";
                     printHex (instr mod exp2_22);
                     printStream ",";
                     regPrint (mkReg (instr div exp2_25 mod exp2_5))
                   )
                     
            | 2 =>  (* branch *)
              let
                val disp22 = signed22 (instr mod exp2_22);
                val byteDisp = disp22 * wordSize;
              in
                 printStream ("b" ^ tstRepr (mkTest ((instr div exp2_25) mod exp2_4)));
                 if instr div exp2_29 = 1 (* annul bit *) then printStream ",a" else (); 
                 printStream "\t";
                 printHex disp22; (* signed *)
                 printStream "\t\t;to ";
                 printHex (byteAddr + byteDisp)
              end
              
            | 0 => (* unimp *)
                (printStream "unimp";
                 printStream "\t";
                 printHex instr)
                
            | _ => printHex instr
          end
            
          else if format = 1
           then let
             val disp30   = signed30 (instr - exp2_30);
             val byteDisp = disp30 * wordSize;
           in
             printStream "call\t";
             printHex disp30;
             printStream "\t\t;to ";
             printHex (byteAddr + byteDisp)
           end
          
          else if format = 2
          then let
            val op3 = instr div exp2_19 mod exp2_6;
            val rs  = mkReg (instr div exp2_14 mod exp2_5);
            val rd  = mkReg (instr div exp2_25 mod exp2_5);
                        
            local
              val table =
                [
                  (ADD,"add"),
                  (AND,"and"),
                  (OR,"or"),
                  (XOR,"xor"),
                  (ANDN,"andn"),
                  (SUB,"sub"),
                  (ADDcc,"addcc"),
                  (ANDcc,"andcc"),
                  (SUBcc,"subcc"),
                  (TADDccTV,"taddcctv"),
                  (TSUBccTV,"tsubcctv"),
                  (SLL,"sll"),
                  (SRL,"srl"),
                  (SRA,"sra"),
                  (JMPL,"jmpl")
                ]
              
                fun printFromTable _ []         = printHex op3
                  | printFromTable x ((y,s)::t) = 
                     if x = op3_2ToInt y then printStream s else printFromTable op3 t
              
             in
               fun printOp3String x = printFromTable x table
            end
            
            val isMov  = (op3 = op3_2ToInt OR) andalso (rs regEq g0);
            val isTrap = (op3 = op3_2ToInt Ticc);
          in
            if isMov
              then printStream "mov"
            else if isTrap
              then printStream ("t" ^ tstRepr (mkTest ((getReg rd) mod exp2_4)))
            else printOp3String op3;
            
            printStream "\t";
            
            if isMov then () else (regPrint rs; printStream ",");
            
            printRimm instr;
            
            if isTrap then () else (printStream ","; regPrint rd)
          end
          
          else let (* 3 *)
            val op3 = instr div exp2_19 mod exp2_6;
            val rs  = mkReg (instr div exp2_14 mod exp2_5);
            val rd  = mkReg (instr div exp2_25 mod exp2_5);
          in
            if op3 = 0 orelse op3 = 1
            then
            ( (if op3 = 0 then printStream "ld" else printStream "ldub");
              printStream "\t[";
              regPrint rs;
              printStream "+";
              printRimm instr;
              printStream "],";
              regPrint rd
            )
            else if op3 = 4 orelse op3 = 5
            then
            ( printStream (if op3 = 4 then "st" else "stb");
              printStream "\t";
              regPrint rd;
              printStream ",[";
              regPrint rs;
              printStream "+";
              printRimm instr;
              printStream "]" 
            )
            else
            ( printHex op3;
              printStream "\t";
              regPrint rs;
              printStream ",";
              printRimm instr;
              printStream ",";
              regPrint rd
            )
          end;
      in
    printStream "\n"
      end (* while *)
    end (* scope of ptr *)
  end (* printCode *);
        
  (* set the num'th constant in cvec to be value *)
  fun constLabels (cvec : code, addr: addrs, value : machineWord) : unit =
  let
    val (seg : cseg, preludeSize : int) = scSet (!(resultSeg cvec));
    val constAddr : addrs = addr wordAddrPlus preludeSize;
  in
    csegPutConstant (seg, getByteAddr constAddr, value, 0)
  end;
  
  (* Fix up references from other vectors to this one. *)
  fun fixOtherRefs (refTo as Code{otherCodes=ref otherCodes, ...}, value) =
  let
    fun fixRef (refFrom : code) : unit =
    let
      val noc = numOfConsts refFrom;
      
      fun putNonLocalConst (CVal cCode, addr) : unit =
        if cCode is refTo
        then
        (
           (* A reference to this one. *)
           (* Fix up the forward reference. *)
           constLabels (refFrom, addr, value);
           (* decrement the "pending references" count *)
           noc := !noc - 1
        )
        else ()
      | putNonLocalConst _ = ()
        
    in
      (* look down its list of forward references until we find ourselves. *)
      List.app putNonLocalConst (!(constVec refFrom));
      (* If there are no more references, we can lock it. *)
      if !noc = 0
      then csegLock (#1 (scSet (! (resultSeg refFrom))))
      else ()
    end (* fixRef *);
  in
    (* For each `code' which needs a forward reference
       to `refTo' fixing up. *)
    List.app fixRef otherCodes
  end; (* fixOtherRefs *)


  (* The stack limit register is set at least twice this far from the end
     of the stack so we can simply compare the stack pointer with the stack
     limit register if we need less than this much. Setting it at twice
     this value means that procedures which use up to this much stack and
     do not call any other procedures do not need to check the stack at all. *)
  val minStackCheck = 20; 
  
  (* Adds the constants onto the code, and copies the code into a new segment *)
  fun copyCode (cvec as Code{printAssemblyCode, printStream, ...}, stackRequired, registerSet) : address =
  let
    val endIC      = !(ic cvec); (* Remember end *)
    val callsAProc = !(mustCheckStack cvec);

(*****************************************************************************
Functions now have up to 4 entry points:
  (1) Standard entry point
  (2) Tail-call entry point - doesn't change %o7
  (3) Self-call entry point - doesn't change %o4
  (4) Self-tail-call entry point - doesn't change %o7 or %o4

Entry points 1 and 2 are always the first and second words of the actual code.
Entry points 3 and 4 can be at various offsets (if they are needed at all),
but that's OK because they are only used for calls within the procedure
itself. The worst-case code for the prelude would be:

   L1: or    %o7,2,%o7
   L2: Previously this code loaded the address of the constant area.
      
   L3: or    %o7,2,%o7      (It's OK to fall through and do this again)
   L4: sethi yhi,%i5
       add   %i5,ylo,%i5
       ld    [24+%i0],%i3
       sub   %psp,%i5,%psp
       cmp   %i5,%i3
       tlu   16
      
This assumes that both immediates (the offset of the constants section and
the stack required) are larger than 13 bits, which would be unusual.

If we have no self-calls (as opposed to self-jumps), we can produce:

   L1: or    %o7,2,%o7
   L2: 
      
   L4: sethi yhi,%i5
       add   %i5,ylo,%i5
       ld    [24+%i0],%i3
       sub   %psp,%i5,%psp
       cmp   %i5,%i3
       tlu   16

*****************************************************************************)    

    (* Generate the prelude (iterative!) *)
    local 
     (* Or 2 into o7 (the return address) so that it is off-word aligned. Must
        do this before the stack-checking code. Also "jumpToFunction" assumes
        that this is the first instruction and can be skipped if we have already
        added 2 to o7. *)
      val o7Code = [format3_2 (OR, o7, immed13_CODETAG, o7)];
      val lengthO7Code = 1; (* length o7Code *)
    
      val stackCheckCode =
        if stackRequired >= minStackCheck
        then let (* We need to check the stack. *)
          val loadSL = format3_3 (LD, i0, immed13 MemRegStackLimit, i3)
          val (cl,loc) = immedCodeData (stackRequired * wordSize)
        in 
          (* get the "minimum" sp into i5 and trap if it is less 
             than the stack limit register. *)
          loadSL :: cl @
          [format3_2 (SUB, psp, loc, i5), (* N.B. not "ADD" SPF 13/10/94 *)
           format3_2 (SUBcc, i5, i3M, g0),
           tlu24]
        end
           
        (* If we call another function, we have to perform the following
           stack check to ensure that there are at least two minimal
           (minStackCheck words each) stack frames available - one for us,
           one for the (possibly leaf) function that we're calling.
           We don't need to do this if we only make tail calls (our
           caller has reserved a stack frame for us, which we share
           with our callee(, but we'll normally perform the stack
           check anyway, to check for user interrupt. SPF 19/2/97
        *)
        else if callsAProc
        then
          [
           (* trap if current sp is less than stack limit register.
              This check is necessary because user interupts (^C)
              adjust the limit register precisely so this check will
              cause a trap is a "safe" state. *)
           format3_3 (LD, i0, immed13 MemRegStackLimit, i3),
           format3_2 (SUBcc, psp, i3M, g0),
           tlu24
           ]
           
        (* Leaf or tail-call-only function - no stack check required. *)
        else
      []; 
    in
      (* code segment size minimised (iteratively!) SPF 12/8/94 *)
      (* I think this is no longer required now that we don't load
         the constant pointer. DCJM 12/1/01. *)
      fun getPreludeCode spaceForPrelude (* an initial guess! *) =
      let
        (* +6 for zero word, code size, profile count, constants count
           function name and register mask. *)
        val segSize = getWordAddr endIC + spaceForPrelude + 6;
        
      (* Prelude consists of
             1) adding 2 to o7 (the return address reg)
             2) stack checking code
             
         Note: we always have a stack-check, even for self tail-calls
         where we actually know that the stack is big enough. This is
         needed to allow the ^C break-in and profiling mechanisms to work
         smoothly, as otherwise we could get unbreakable loops. *)
      
       infixr 5 @;
       val preludeCode = 
           (* L1 here *) o7Code @ 
           (* L2 here *) [] @
           (* L3 here *) [] @
           (* L4 here *) stackCheckCode;
      in
        (* does it fit? *)
        if List.length preludeCode = spaceForPrelude
        then (spaceForPrelude,segSize,preludeCode)
        else getPreludeCode (spaceForPrelude + 1)
      end
   
      val (spaceForPrelude,segSize,preludeCode) =
       (* iterate to find size of loadConstSegCode *)
       getPreludeCode (lengthO7Code + List.length stackCheckCode); 
      
      (* byte offsets of L3 and L4 labels relative 
      to start of post-prelude code. *)
      val L4Addr = mkWordAddr (~ (List.length stackCheckCode));
      val L4Target = 
    case stackCheckCode of
     (c::_) => c (* never a jump *)
    | []     => nopQuad (* should never get used *)
      
      val L3Addr = mkWordAddr (~ (lengthO7Code + List.length stackCheckCode));
      val L3Target = 
    case o7Code of
     (c::_) => c (* never a jump *)
    | []     => L4Target; (* shouldn't occur *)
    end; (* local *)
    
    (* This is the address of the zero word *)
    val lastAddr : addrs = endIC wordAddrPlus spaceForPrelude;

    (* This is the address of the zero'th constant (the name string)
       +3 for zero word, code size, profile count *)
    val constStartAddr : addrs = lastAddr wordAddrPlus 3;

    (* fix-up all the self-calls *)
    val () = 
      fixupRecursiveCalls (cvec, L3Target, L3Addr, !(selfCalls cvec));
       
    val () =
      fixupRecursiveBranches (cvec, L4Target, L4Addr, !(selfJumps cvec));
      
    (* Now make the byte segment that we'll turn into the code segment *)
    val seg : cseg = csegMake segSize;
    
    (* Copy the code into the new segment. *)
    val () = csegCopySeg (codeVec cvec, seg, getByteAddr endIC, spaceForPrelude);

    (* insert prelude code into segment *)
    local
      fun putPreludeQuad (wordAddr : int, w : quad) =
        setQuad (w, mkWordAddr wordAddr, seg);
    in
      val () = applyCountList (putPreludeQuad, 0, preludeCode);
    end;
    
    (* Zero end-of-code marker *)
    local
      val addr : addrs = lastAddr;
      val quad : quad  = toQuad 0;
    in
      val () = setQuad (quad, addr, seg)
    end;
 
    (* Byte offset of start of code. *)
    local
      val addr : addrs = lastAddr wordAddrPlus 1;
      val quad : quad  = toQuad (getByteAddr addr);
    in
      val () = setQuad (quad, addr, seg)
    end;
    
    (* Next the profile count. *)
    local
      val addr : addrs = lastAddr wordAddrPlus 2;
      val quad : quad  = toQuad 0;
    in
      val () = setQuad (quad, addr, seg)
    end;
    
    (* Put in the number of constants (including the name string). This
       is now always 2 - the name and the register mask. *)
    local
      val addr : addrs = constStartAddr wordAddrPlus 2;
      val quad : quad = toQuad 2;
    in
      val () = setQuad (quad, addr, seg)
    end;
    
    (* Now we've filled in all the C integers; now we need to convert the segment
       into a proper code segment before it's safe to put in any ML values.
       SPF 13/2/97
    *)
    val () = csegConvertToCode seg;
    val () = resultSeg cvec := Set (seg, spaceForPrelude);

    local
        val procName = procName cvec
        val nameWord : machineWord = if procName = "" then toMachineWord 0 else toMachineWord procName;
    in
        val _ = csegPutWord (seg, getWordAddr constStartAddr, nameWord);
    end;

    (* Insert the register set. *)
    local
        val addr : addrs = constStartAddr wordAddrPlus 1;
        fun encodeReg(r, n: short): short =
        let
            open Word
            infix << orb
            val reg = 0w1 << Word.fromInt (nReg r)
        in
            reg orb n
        end
        val regSet = List.foldl encodeReg 0w0 registerSet
    in
        val () = csegPutWord(seg, getWordAddr addr, toMachineWord regSet)
    end;
 
    (* Copy the objects (including the name string) from the constant list. *)
    fun putLocalConst (WVal c, addr) = (* an ordinary (non-short) constant *)
     (
       constLabels (cvec, addr, c);
       numOfConsts cvec := ! (numOfConsts cvec) - 1
     )
    
    |  putLocalConst (HVal href, addr) =      (* a handler *)
    let 
      (* The following comment applies to offsetAddr *)
      (* Special function to add to an address.
         This only works if the resulting value is 
         in a code segment and is on a word  + 2 byte boundary. *)
          val handlerByteOffset : int = getByteAddr ((!href) wordAddrPlus spaceForPrelude);
      val handlerAddr : handler = 
        offsetAddr (csegAddr seg, toShort (handlerByteOffset + CODETAG));
    in
      constLabels (cvec, addr, toMachineWord handlerAddr);
      numOfConsts cvec := ! (numOfConsts cvec) - 1
    end
      
      (* forward-reference - fix up later when we compile
     the referenced code *) 
    |  putLocalConst (CVal _, _) = ();

    val () = List.app putLocalConst (! (constVec cvec));


    (* Switch off "mutable" bit now if we have no
       forward or recursive references to fix-up *)
    val () = 
      if !(numOfConsts cvec) = 0
      then csegLock seg
      else ();

    (* Do we need to make a closure, or just return the code? *)
    val addr : address =
      if noClosure cvec
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
      then let (* print out the code *)
    val () = printCode (seg, procName cvec, lastAddr, printStream);
     in
    printStream "\n"
      end
      else ();
  in
    addr 
  end (* copyCode *);

  (* We need these types although we don't generate indexed cases. *)
  type cases = int * addrs; (* should tag be a short??? *)

(* Dummy implementation ... 
  type jumpTableAddrs = unit;

  fun useIndexedCase (min, max, numberOfCases, exhaustive) : bool =
    false; (* Never use indexed case. *)

  fun indexedCase (reg, reg2, min, max, exhaustive, cvec) : jumpTableAddrs =
    raise InternalError "Not implemented: indexedCase";

  fun makeJumpTable (startTab, cl, default, min, max, cvec) : unit =
    raise InternalError "Not implemented: makeJumpTable";
... *)

  (* On this architecture, the jumptable is physically inserted into
     the code as a vector of address offsets. The function "indexedCase"
     generates the space for the table and "makeJumpTable" inserts
     the actual entries, once the addresses are known.
     SPF 23/11/1997
     
     Note - this will confuse our disassembler. Must fix this some time!
     SPF 12/01/1998
  *)
  type jumpTableAddrs = addrs;
  
  fun constrCases p = p;
  
  type caseList = cases list;

  fun useIndexedCase (min:int, max:int, numberOfCases:int, _:bool) =
    isShort min andalso
    isShort max andalso
    numberOfCases > 7 andalso
    numberOfCases >= (max - min) div 3;

  fun indexedCase (valReg : reg, workReg : reg, min:int, max:int,
                  exhaustive:bool, cvec : code) : jumpTableAddrs =
  let 
    val rangeCheck : labels =
      if exhaustive then []
      else let
    (* Is it long? If so, jump to the default. We need this because we might be 
       doing case-analysis on integers. For other types this is a waste of
       time - we know the tag must be a short - and we should change GCODE
       so we can eliminate this test.
       SPF 23/11/1997
       
       We have to be *very* careful here, though. For optimised data-types,
       we might be comparing a pointer with a (short) integer. This means
       that we're not really testing long/short, we're testing pointer/short
       so we need to keep the test even if we know we're dealing with datatype
       tags (for some kinds of datatypes). This requires more work in the
       high-level code generator to distinguish these cases properly.
       SPF 11/12/1997
    *)
        val l1 : labels = compareAndBranchRI (valReg, toMachineWord 0,   Long, cvec);
        val l2 : labels = compareAndBranchRI (valReg, toMachineWord min, testLtSignedW, cvec);
        val l3 : labels = compareAndBranchRI (valReg, toMachineWord max, testGtSignedW, cvec);
      in
    l1 @ (l2 @ l3)
      end;
      
    (* Get the jumptable address into a register - not easy on this
       machine. Create a handler label and load that.
    *)
    val tableBase : handlerLab = loadHandlerAddress (workReg, cvec);
    (* constR now addresses the jumptable (but with a +2 offset) *)
    
    (* Adjust the index to be relative to the start of the table
       - don't forget to make it "out by 2" to account for the
       CODETAG. No scaling is required - lucky!
    *)
    val adjust : mode13 = genImmedData (cvec, ~ (tagged min + CODETAG));
    val () = genAdd (valReg, adjust, i5, cvec);
    
    (* i5 now contains the index into the jumptable *)
    
    val () = genLoadInstr LD (workReg, i5M, i5, cvec);
    
    (* i5 now contains the distance between the jumptable and our destination,
       (adjusted for the +2 offset).
    *)
    val () = genJmpl (i5M, workReg, g0, cvec);
    
    val () = fixupHandler (tableBase, cvec);
     
    (* We generate the table itself here *)
    val tableBase : addrs = ! (ic cvec);
    val tableWords : int  = max - min + 1;
    val () = ic cvec := tableBase wordAddrPlus tableWords;

(* ...
    (* We haven't really fallen through to here. *)
    val () = exited cvec := true;
... *)
    
    (* The default case comes in here. *)
    val () = fixup (rangeCheck, cvec);
  in
    (* Return the address of the jumptable *)
    tableBase : jumpTableAddrs
  end;

  fun makeJumpTable (startTab:jumpTableAddrs, cl:caseList, default:addrs, 
                     min : int, max : int, cvec : code) : unit =
  let
    fun putCase (entryNo : int) (jumpAddr : addrs) =
    let
      val wordOffset : int   = jumpAddr wordAddrMinus startTab;
      val byteOffset : int   = wordOffset * wordSize;
      val entryAddr  : addrs = startTab wordAddrPlus (entryNo - min);
      val entryQuad  : quad  = toQuad (byteOffset - CODETAG);
    in
      setCodeQuad (entryQuad, entryAddr, cvec)
    end;

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

  (* ic function exported to gencode. Currently only used for backward jumps. *)
  val ic = fn cvec =>
  ( (* Make sure any pending operations are done. *)
    clearCache cvec;
    doPendingStackAdjustment cvec;
    reallyFixup cvec;
    exited cvec := false; (* We may be jumping here. *)
    ! (ic cvec)
  );

  fun codeAddress (cvec: code) : address option =
  (* This is used to find the register set for a function which was
     originally a forward reference.  If it has now been compiled we
     can get the code. *)
    case cvec of
        Code {resultSeg = ref (Set (cseg, _)), ...} => SOME(csegAddr cseg)
    |   Code {resultSeg = ref Unset, ...} =>
         (* We haven't compiled this yet: assume worst case. *) NONE

  fun traceContext (Code {procName, ic = ref ic, ...}) =
  (* Function name and code offset to help tracing. *)
     procName ^ ":" ^ Int.fmt StringCvt.HEX (getByteAddr ic)

end; (* CODECONS functor body *)

