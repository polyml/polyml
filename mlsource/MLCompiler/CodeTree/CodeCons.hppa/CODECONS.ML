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

(* This module contains the code vector and operations to insert code into
   it. Each procedure is compiled into a separate segment. Initially it is
   compiled into a fixed size segment, and then copied into a segment of the
   correct size at the end.
   This module contains all the definitions of the Sparc opcodes and registers.
   It uses "codeseg" to create and operate on the segment itself.
 *)

(*
 Linkage conventions:

; Register Usage
; --------------
; 
; %r0  = pseudo register (constant 0)
; 
; %r1  = scratch register (used as target of ADDIL)
; 
; %r2  = scratch register
; 
; %r3  = regStackPtr  (dedicated)
; %r4  = regStackLim  (dedicated)
; %r5  = regHeapPtr   (dedicated)
; %r6  = regHeapLim   (dedicated)
; %r7  = regHandler   (dedicated)
; 
; %r8  = regCode
; %r9  = regClosure
; 
; %r10 - %r22 = general purpose registers
; 
; %r23 = argReg3
; %r24 = argReg2
; %r25 = argReg1
; %r26 = argReg0
; 
; %r27 = DON'T TOUCH
; 
; %r28 = regResult
; %r29 = scratch register
; 
; %r30 = DON'T TOUCH
; %r31 = regReturn (used as BLE link register)

%r1 is junked by any instruction that may trap; %rt1 and %rt2 are literally preserved
across traps, so they must never contain any tagged valuesm as the GC won't
recognise these as such.

Function Prologues
------------------
Unlike the SPARC, we make it the caller's resposibility to tag the
return address (in the BLE delay slot). This means that functions
have a maximum of 2 entry points:

  (1) L1/L2 Standard entry point - tagged return address is in regReturn
  (2) L3/L4 Self-call entry point - doesn't change regCode

L1, L2: 
        ADDIL   L%<code-size>,regCode
        LDO R%<code-size>(%r1),regCode
L3, L4: 
        <stack-check code>

Note: most code segements will be short, in which case we won't need
the "ADDIL, LDO" sequenece at all.

Calling a Function
------------------
We make the global assumption that we are running in the segment addressed by %sr0.

Call:
    BLE 0(%sr0,regCode)
    DEPI    2,32,2,regReturn    (* tag return address *)

Self-call:
    BL  L3,%regReturn
    DEPI    2,32,2,regReturn    (* tag return address *)

Tail-call:
    BV  %r0(regCode)
    COPY    returnReg,regReturn (* If returnReg <> regReturn *)

Self-tail-call:
    MOVB    returnReg,regReturn,L4  (* If branch is less than 2^11 words *)
    NOP
or
        ADDIL   L%(L4-L1)-<code-size>,regCode (* If we have a long branch *)
        LDO R%(L4-L1)-<code-size>(%r1),%r1
    BV  %r0(%r1)
    COPY    returnReg,regReturn

Note that generating a "long" self-call implies we're going to need a "long" function
prelude.

Return:
    LDO -2(returnReg),%rt1  (* N.B. not regReturn *)
    BV  %r0(%rt1)
    NOP

*)

functor CODECONS (

(*****************************************************************************)
(*                  DEBUG                                                    *)
(*****************************************************************************)
structure DEBUG :
sig
  val assemblyCode:    bool ref;
  
  (* This is true if we are code-generating for the persistent store and
     have to be careful about traps etc. *)
  val persistentStore: bool
  
  (* If we are running with the distributed memory we cannot compile
     in load instructions. *)
  val allowMutableLoads: bool
end;

(*****************************************************************************)
(*                  MISC                                                     *)
(*****************************************************************************)
structure MISC :
  sig
    exception InternalError of string
    val quickSort : ('a -> 'a -> bool) -> 'a list -> 'a list;
  end

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
  val regCode:     reg;
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
  val regRepr:  reg -> string;

  type addrs

  val codeCreate: bool * string -> code;  (* makes the initial segment. *)

  (* Operations. *)
  type instrs;
  
  val instrMove:       instrs;
  val instrAddA:       instrs;
  val instrSubA:       instrs;
  val instrRevSubA:    instrs;
  val instrMulA:       instrs;
  val instrOrW:        instrs;
  val instrAndW:       instrs;
  val instrXorW:       instrs;
  val instrLoad:       instrs;
  val instrLoadB:      instrs;
  val instrVeclen:     instrs;
  val instrPush:       instrs;
  val instrUpshiftW:   instrs;
  val instrDownshiftW: instrs;
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
  
  val compareAndBranchRR: reg * reg * tests * code -> labels;
  val compareAndBranchRI: reg * word * tests * code -> labels;

  val genLoad:        int * reg * reg * code -> unit;
  val genStore:       reg * int * reg * code -> unit;
  val isStoreI:       word -> bool;
  val genStoreI:      word * int * reg * code -> unit;
  val genPush:        reg * code -> unit;
  val genLoadPush:    int * reg * code -> unit;
  val preferLoadPush: bool;
  val genLoadConst:   word * reg * reg * code -> unit;
  val genLoadCoderef: code * reg * reg * code -> unit;

  val allocStore:      int * short * reg * code -> unit;
  val setFlag:         reg * code * short -> unit;
  val completeSegment: code -> unit;

  type callKinds;
  val StaticLink : callKinds;
  val FullCall   : callKinds;
  val IoCall     : callKinds;
  val Recursive  : callKinds;
  val PureCode   : callKinds;
  
  val callFunction:       callKinds * code -> unit;
  val jumpToFunction:     callKinds * reg * code -> unit;
  val returnFromFunction: reg * int * code -> unit;
  val raiseEx:            code -> unit;
  val setSl:              int * code -> unit;

  val copyCode: code * int -> address;

  val unconditionalBranch: code -> labels;
  
  type handlerLab;
  
  val pushAddress:  reg * code -> handlerLab;
  val fixupHandler: handlerLab * code -> unit;
  
  val fixup:        labels * code -> unit; (* Fix up a jump. *)

  (* ic - Address for the next instruction in the segment. *)
  val ic : code -> addrs;
  
  val jumpback: addrs * bool * code -> unit; (* Backwards jump. *)

  val resetStack: int * code -> unit; (* Set a pending reset *)
  val procName:   code -> string;      (* Name of the procedure. *)
  
  type cases
  type jumpTableAddrs
  val constrCases : int * addrs -> cases;
  val useIndexedCase: int * int * int * bool -> bool;
  val indexedCase : reg * reg * int * int * bool * code -> jumpTableAddrs;
  val makeJumpTable : jumpTableAddrs * cases list * addrs * int * int * code -> unit;
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
  val toAddress: 'a -> address

  val wordSize : int; (* still 4, but will change one day *)

  val wordEq : 'a * 'a -> bool
  
  val isShort:  'a     -> bool;
  val toShort:  'a     -> short;
  val toWord:   'a     -> word;
  val toInt:     short -> int;
  val toAddress: 'a -> address

  val loadByte:   address * short -> short 
  val loadWord:   address * short -> word
  val nameOfCode: address -> string
  val unsafeCast: 'a -> 'b

  val Or:  short * short -> short;
  val And: short * short -> short; 
  val << : short * short -> short; 
  val >> : short * short -> short; 
  
  val offsetAddr : address * short -> handler
 
  val alloc:  (short * short * word) -> address
  val length: address -> short
  val flags:  address -> short

  val F_words : short
 
  val isWords : address -> bool;
  val isBytes : address -> bool;
  val isCode  : address -> bool;
  
  val isString : word -> bool;
  val toString : word -> string;

  val lock : address -> unit;
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
  infix 8 >> <<;
  infix 7 And;
  infix 6 Or;

  fun applyList (f, [])   = ()
    | applyList (f, h::t) =
    let
      val U : unit = f h;
    in
      applyList (f, t)
    end;
  
  fun applyCountList (f, n, [])   = ()
    | applyCountList (f, n, h::t) = 
    let
      val U : unit = f (n, h);
    in
      applyCountList (f, n + 1, t)
    end;
    
  fun revOnto ([], l) = l
    | revOnto (h :: t, l) = revOnto (t, h :: l);
  
  fun printString (s: string) = output(std_out,s);
  
  fun reprBool true  = "true"
    | reprBool false = "false";
  
  local
    fun hexDigit (n:int) : string =
      if n < 10 then chr (ord "0" + n) else chr (ord "a" - 10 + n);
  
    fun revHexDigits (width : int, x : int) : string list =
    let
      val lo = x mod 16
      val hi = x div 16
    in
      hexDigit lo ::
      (if x >= 16 orelse width > 1 then revHexDigits (width - 1, hi) else [])
    end;
  
    fun phex (width: int, x : int) : unit =
    let
      val hi = x div 16
      val lo = x mod 16
    in
      if x >= 16 orelse width > 1 then phex (width - 1, hi) else ();
      output (std_out, hexDigit lo)
    end;
  in
     (* prints a string representation of a number, padded to width characters *)
    fun reprHexN (width : int, n : int) : string =
      if n < 0
      then implode ("-" :: rev (revHexDigits (width, ~n)))
      else implode (rev (revHexDigits (width, n)))

    fun reprHex (n : int) : string = reprHexN (0, n);

    (* prints a string representation of a number, padded to width characters *)
    fun printHexN (width : int, n : int) =
      if n < 0
      then (output (std_out, "-"); phex (width - 1, ~ n))
      else phex (width, n)

    fun printHex (n : int) : unit = printHexN (0, n);
  end;

(*****************************************************************************)
(*                  Useful constants                                         *)
(*****************************************************************************)
  (* These are specified as explicit constants, rather than calls to exp2
     so that the code-generator can in-line them when it compiles itself.
  *)
  val exp2_1  =          2;
  val exp2_2  =          4;
  val exp2_3  =          8;
  val exp2_4  =         16;
  val exp2_5  =         32;
  val exp2_6  =         64;
  val exp2_7  =        128;
  val exp2_8  =        256;
  val exp2_10 =       1024;
  val exp2_11 =       2048;
  val exp2_13 =       8192;
  val exp2_16 =      65536;
  val exp2_20 =    1048576;
  val exp2_24 =   16777216;
  val exp2_29 =  536870912;
    
  (* Let's check that we got them right! *)
  local
    fun exp2 0 = 1
      | exp2 n = 2 * exp2 (n - 1);
  in
    val U : bool = 
      (
        exp2_1  = exp2 1  andalso
        exp2_2  = exp2 2  andalso
        exp2_3  = exp2 3  andalso
        exp2_4  = exp2 4  andalso
        exp2_5  = exp2 5  andalso
        exp2_6  = exp2 6  andalso
        exp2_7  = exp2 7  andalso
        exp2_8  = exp2 8  andalso
        exp2_10 = exp2 10 andalso
        exp2_11 = exp2 11 andalso
        exp2_13 = exp2 13 andalso
        exp2_16 = exp2 16 andalso
        exp2_20 = exp2 20 andalso
        exp2_24 = exp2 24 andalso
        exp2_29 = exp2 29
      )
         orelse raise InternalError "CodeCons: Powers of 2 incorrectly specified";
  end;
  
  val short0    = toShort 0;
  val short1    = toShort 1;
  val short2    = toShort 2;
  val short3    = toShort 3;
  val short4    = toShort 4;
  val short5    = toShort 5;
  val short6    = toShort 6;
  val short7    = toShort 7;
  val short8    = toShort 8;
  val short9    = toShort 9;
  val short10   = toShort 10;
  val short11   = toShort 11;
  val short12   = toShort 12;
  val short13   = toShort 13;
  val short14   = toShort 14;
  val short16   = toShort 16;
  val short18   = toShort 18;
  val short19   = toShort 19;
  val short20   = toShort 20;
  val short21   = toShort 21;
  val short22   = toShort 22;
  val short24   = toShort 24;
  val short25   = toShort 25;
  val short26   = toShort 26;
  val short27   = toShort 27;
  val short28   = toShort 28;
  val short30   = toShort 30;
  val short32   = toShort 32;
  val short33   = toShort 33;
  val short34   = toShort 34;
  val short35   = toShort 35;
  val short36   = toShort 36;
  val short37   = toShort 37;
  val short38   = toShort 38;
  val short40   = toShort 40;
  val short44   = toShort 44;
  val short50   = toShort 50;
  val short51   = toShort 51;
  val short52   = toShort 52;
  val short53   = toShort 53;
  val short56   = toShort 56;
  val short57   = toShort 57;
  val short58   = toShort 58;
  
  (* Masks for the least significant n bits *)
  val mask1Bit   = toShort (exp2_1  - 1);
  val mask2Bits  = toShort (exp2_2  - 1);
  val mask3Bits  = toShort (exp2_3  - 1);
  val mask4Bits  = toShort (exp2_4  - 1);
  val mask5Bits  = toShort (exp2_5  - 1);
  val mask6Bits  = toShort (exp2_6  - 1);
  val mask7Bits  = toShort (exp2_7  - 1);
  val mask8Bits  = toShort (exp2_8  - 1);
  val mask10Bits = toShort (exp2_10 - 1);
  val mask11Bits = toShort (exp2_11 - 1);

  (* These are defined here as explicit constants, so the     *)
  (* code-generator can in-line them as immediates (I think). *)
  val TAGSHIFT   = 1;
  val CODETAG    = 2;
  val FLAGLENGTH = 8; (* There are 8 flag bits in the length word *)

  (* tag a short constant *)
  fun semiTagged c   = exp2_1 * c;
  fun tagged c       = exp2_1 * c + 1;

(*****************************************************************************)
(*                 21-bit immediates                                         *)
(*****************************************************************************)
  (* 21-bit signed immediates are used with ADDIL *)
  abstype int21 = Int21 of int
  with
    fun getInt21 (Int21 i) = i;
    
    (* is21Bit is the test for signed 21-bit immediates *) 
    fun is21Bit i = ~exp2_20 <= i andalso i <= exp2_20 - 1;
    
    fun int21 i =
      if is21Bit i
      then Int21 i
      else let
        val msg = 
          implode
           [
             "int21: can't convert ",
             Int.toString i,
             " into a 21-bit signed immediate"
           ]
      in
        raise InternalError msg
      end;
  end;


(*****************************************************************************)
(*                 17-bit immediates                                         *)
(*****************************************************************************)
  (* 17-bit signed immediates are used for unconditional branches *)
  abstype int17 = Int17 of int
  with
    fun getInt17 (Int17 i) = i;
    
    (* We check for shorts first because that saves us an expensive
       compile-time trap if i is actually a long value.
       SPF 11/12/97
    *)
       
    (* is17Bit is the test for signed 17-bit immediates *) 
    fun is17Bit i = 
      isShort i andalso ~exp2_16 <= i andalso i <= exp2_16 - 1;
    
    fun int17 i =
      if is17Bit i
      then Int17 i
      else let
        val msg = 
          implode
           [
             "int17: can't convert ",
             Int.toString i,
             " into a 17-bit signed immediate"
           ]
      in
        raise InternalError msg
      end;
      
    (* various small constants *)
    val int17_0 = Int17 0;
  end;

(*****************************************************************************)
(*                 14-bit immediates                                         *)
(*****************************************************************************)
  (* 14-bit signed immediates are used for load and store instructions *)
  abstype int14 = Int14 of int
  with
    fun getInt14 (Int14 i) = i;
    
    (* is14Bit is the test for signed 14-bit immediates *) 
    fun is14Bit i =
      isShort i andalso ~exp2_13 <= i andalso i <= exp2_13 - 1;
    
    fun int14 i =
      if is14Bit i
      then Int14 i
      else let
        val msg = 
          implode
           [
             "int14: can't convert ",
             Int.toString i,
             " into a 14-bit signed immediate"
           ]
      in
        raise InternalError msg
      end;
      
    (* various small constants *)
    val int14_0      = Int14 0;
    val int14_1      = Int14 1;
    val int14_minus1 = Int14 ~1;
    val int14_minus2 = Int14 ~2;
    val int14_minus4 = Int14 ~4;
  end;
  
(*****************************************************************************)
(*                 12-bit immediates                                         *)
(*****************************************************************************)
  (* 12-bit signed immediates are used for conditional branches *)
  abstype int12 = Int12 of int
  with
    fun getInt12 (Int12 i) = i;
    
    (* is12Bit is the test for signed 12-bit immediates *) 
    fun is12Bit i = 
      isShort i andalso ~exp2_11 <= i andalso i <= exp2_11 - 1;

    fun isPositive12Bit i = 
      isShort i andalso 0 <= i andalso i <= exp2_11 - 1;
    
    fun int12 i =
      if is12Bit i
      then Int12 i
      else let
        val msg = 
          implode
           [
             "int12: can't convert ",
             Int.toString i,
             " into a 12-bit signed immediate"
           ]
      in
        raise InternalError msg
      end;
      
    (* various small constants *)
    val int12_0 = Int12 0;
  end;
  
(*****************************************************************************)
(*                 11-bit immediates                                         *)
(*****************************************************************************)
  (* 11-bit signed immediates are used for arithmetic instructions *)
  abstype int11 = Int11 of int
  with
    fun getInt11 (Int11 i) = i;
    
    (* is11Bit is the test for signed 11-bit immediates *) 
    fun is11Bit i = 
      isShort i andalso ~exp2_10 <= i andalso i <= exp2_10 - 1;
    
    fun int11 i =
      if is11Bit i
      then Int11 i
      else let
        val msg = 
          implode
           [
             "int11: can't convert ",
             Int.toString i,
             " into a 11-bit signed immediate"
           ]
      in
        raise InternalError msg
      end;

    (* various small constants *)
    val int11_minus1 = Int11 ~1;
  end;

(*****************************************************************************)
(*                  5-bit immediates (signed)                                *)
(*****************************************************************************)
  (* 5-bit signed immediates are used by the MOVIB and COMIV instructions. *)
  abstype int5 = Int5 of int
  with
    fun getInt5 (Int5 i) = i;
    
    (* is5Bit is the test for signed 5-bit immediates *) 
    fun is5Bit i = 
      isShort i andalso ~exp2_4 <= i andalso i <= exp2_4 - 1;
    
    fun int5 i =
      if is5Bit i
      then Int5 i
      else let
        val msg = 
          implode
           [
             "int5: can't convert ",
             Int.toString i,
             " into a 5-bit signed immediate"
           ]
      in
        raise InternalError msg
      end;
      
    (* various small constants *)
    val int5_0       = Int5  0;
    val int5_CODETAG = Int5 CODETAG;
  end;
  
(*****************************************************************************)
(*                  5-bit immediates (unsigned)                              *)
(*****************************************************************************)
  (* 5-bit unsigned immediates are used to designate registers and shifts. *)

  abstype nat5 = Nat5 of int
  with
    fun getNat5 (Nat5 i) = i;
    
    (* isUnsigned5Bit is the test for a unsigned 5-bit immediates *) 
    fun isUnsigned5Bit i = 
      isShort i andalso 0 <= i andalso i <= exp2_5 - 1;
    
    fun nat5 i =
      if isUnsigned5Bit i
      then Nat5 i
      else let
        val msg = 
          implode
           [
             "nat5: can't convert ",
             Int.toString i,
             " into a 5-bit unsigned immediate"
           ]
      in
        raise InternalError msg
      end;
      
    (* various small constants *)
    val nat5_TAGSHIFT   = Nat5 TAGSHIFT;
    val nat5_FLAGLENGTH = Nat5 FLAGLENGTH;
    val nat5_FLminusTS  = Nat5 (FLAGLENGTH - TAGSHIFT);
  end; (* nat5 *)
  
(*****************************************************************************)
(*                  splitLeftRight                                           *)
(*****************************************************************************)

  (* Note: we're only using the LSB 11 bits of the int14,
     so we don't have to worry about sign-extension vs zero-extension.
     SPF 19/2/97
  *)
  fun splitLeftRight (n : int) : (int21 * int14) =
  let
    val left  : int = n div exp2_11;
    val right : int = n mod exp2_11;
  in
    (int21 left, int14 right)
  end


(*****************************************************************************)
(*                  Abstype for registers                                    *)
(*****************************************************************************)
  infix 7 regEq regNeq regLeq regGeq regMinus;

  abstype reg = Reg of int  (* registers. *)
  with
    val regZero     = Reg 0;   (* zero-source and dummy destination *)
    val regUnsafe   = Reg 1;   (* a very unsafe temporary - not saved across traps *)
    val regTemp1    = Reg 2;   (* temporary for untagged values *)
    val regStackPtr = Reg 3;   (* current ML stack pointer *)
    val regStackLim = Reg 4;   (* lower limit of stack (pointer) *)
    val regHeapPtr  = Reg 5;   (* current heap allocation pointer *)
    val regHeapLim  = Reg 6;   (* number of available heap bytes (words?) *)
    val regHandler  = Reg 7;   (* pointer into the stack *)
    val regCode     = Reg 8;   (* address of code or constants *)
    val regClosure  = Reg 9;   (* address of closure (or static link) *)
    (* Reg 10 - Reg 22 are general purpose registers *)
    (* Reg 23 - Reg 26 are used for function arguments *)
    (* Reg 27 is the C static closure pointer - DON'T TOUCH *)
    val regResult   = Reg 28;  (* function results *)
    val regTemp2    = Reg 29;  (* temporary for untagged values *)
    (* Reg 30 is the C stack pointer - DON'T TOUCH *)
    val regReturn   = Reg 31;  (* return address *)
 
    val regNone     = regZero; (* Dummy register *)

    fun getReg (Reg r) = r;      (* reg.down *)
    fun mkReg   n      = Reg n;  (* reg.up   *)
  
    fun getReg5 (Reg r) = nat5 r;

    (* The number of general registers.
       Includes result, closure, code, return and arg regs
       but not stackptr, handler, stack limit or heap ptrs. *)
    val regs = 21; (* r8-r26, r28, r31 *)

    (* The nth register (counting from 0). *)
    fun regN i =
      if i < 0 orelse i >= regs
      then let
        val msg =
          implode
            [
              "regN: Bad register number ",
              Int.toString i
            ]
      in
        raise InternalError msg
      end
      else if i = 20 then mkReg 31 
      else if i = 19 then mkReg 28 
      else mkReg (26 - i)
      
    fun a regEq  b = getReg a  = getReg b;
    fun a regNeq b = getReg a <> getReg b;
    fun a regLeq b = getReg a <= getReg b;
    fun a regGeq b = getReg a >= getReg b;
    fun (Reg a) regMinus (Reg b) = a - b;
  
    (* The number of the register. *)
    fun nReg (Reg n) =
      if n = 31 then 20 else
      if n = 28 then 19 else
      if 8 <= n andalso n <= 26 then 26 - n
      else let
        val msg =
          implode
            [
              "nReg: Bad register number ",
              Int.toString n
            ]
      in
        raise InternalError msg
      end;

    fun regRepr (Reg n) = 
      case n of
        3 => "%rsp"
      | 4 => "%rsl"
      | 5 => "%rhp"
      | 6 => "%rhl"
      | 7 => "%rhr"
      | _ => "%r" ^ Int.toString n;

    fun regPrint r = output (std_out, regRepr r);
    val argRegs = 4;

    (* Args 0, 1, 2, 3 correspond to r26, r25, r24, r23. *)
    fun argReg i =
      if 0 <= i andalso i < 4 then mkReg (26 - i)
      else let
        val msg =
          implode
            [
              "argReg: bad register number ",
              Int.toString i
            ]
      in
        raise InternalError msg
      end;
  end; (* reg *)

(*****************************************************************************)
(*                  Abstype for instruction addresses                        *)
(*****************************************************************************)
  infix 6 wordAddrPlus wordAddrMinus byteAddrMinus;
  infix 4 addrLt addrEq;

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
    fun (Addr a) byteAddrMinus (Addr b) = wordSize * (a - b); 

    fun (Addr a) addrLt (Addr b) = a < b; 
    fun (Addr a) addrEq (Addr b) = a = b; 

    fun mkWordAddr n = Addr n;
  
    fun getWordAddr (Addr a) = a;
    fun getByteAddr (Addr a) = a * wordSize;
  
    val addrZero = mkWordAddr 0;
    val addrLast = mkWordAddr (exp2_29 - 1); (* A big number. *)
  end;


(*****************************************************************************)
(*                  The "quad" datatype (used for instruction words)         *)
(*****************************************************************************)

  datatype quad =  (* the 4 bytes of the instruction word *)
    Quad of short * short * short * short

  (* break an instruction word into 4 bytes; try to do it in a way that *)
  (* will minimise the arithmetic - particularly for long values. *)
  fun toQuad (w : int) : quad =
  let
    val topHw    = toShort (w div exp2_16);
    val bottomHw = toShort (w mod exp2_16);
  in
    Quad (topHw    >> short8, mask8Bits And  topHw,
          bottomHw >> short8, mask8Bits And bottomHw)
  end;

  (* returns *unsigned* integer *)
  fun fromQuad (Quad (b1,b2,b3,b4)) : int =
  let
    val topHw    = toInt ((b1 << short8) Or b2);
    val bottomHw = toInt ((b3 << short8) Or b4);
  in
    topHw * exp2_16 + bottomHw
  end;
  
  val zeroQuad : quad = Quad (short0, short0, short0, short0);

(*****************************************************************************)
(*                  Getting and setting quads                                *)
(*****************************************************************************)
  (* We're using a big-endian machine *)
  fun setQuad (Quad (b0, b1, b2, b3), addr : addrs, seg : cseg) : unit =
  let
    val a : int = getByteAddr addr;
    val U : unit = csegSet (seg, a,     b0);
    val U : unit = csegSet (seg, a + 1, b1);
    val U : unit = csegSet (seg, a + 2, b2);
  in
    csegSet (seg, a + 3, b3)
  end;

  fun getQuad (addr : addrs, seg : cseg) : quad =
  let
    val a : int = getByteAddr addr;
    val b0  = csegGet (seg, a);
    val b1  = csegGet (seg, a + 1);
    val b2  = csegGet (seg, a + 2);
    val b3  = csegGet (seg, a + 3);
  in
    Quad (b0, b1, b2, b3)
  end;
  
(*****************************************************************************)
(*                  Types for branch labels                                  *)
(*****************************************************************************)

  datatype 'a option =
    None
  | Some of 'a

  datatype label = 
    Label of addrs option ref;
    
  type labels = label list; (* for now *)

  val noJump : labels = []; 
  
(*****************************************************************************)
(*                  The condition datatype                                   *)
(*****************************************************************************)
  (* We don't use the unsigned conditions, except for the stack check. *)
  datatype condition =
    NEVER
  | EQ
  | LT
  | LE
  | OD
  | TR
  | NE
  | GE
  | GT
  | EV
  ;
  
 fun reprCond (cond : condition) : string =
    case cond of
      NEVER => "NEVER"
    | EQ    => "EQ"
    | LT    => "LT"
    | LE    => "LE"
    | OD    => "OD"
    | TR    => "TR"
    | NE    => "NE"
    | GE    => "GE"
    | GT    => "GT"
    | EV    => "EV"
    ;
  
  (* The tests are set up for an RI comparison, but we actually have an IR instruction *)
  fun swapArgs (cond : condition) : condition =
    case cond of
      NEVER => NEVER
    | EQ    => EQ
    | LT    => GT
    | LE    => GE
    | OD    => OD
    | TR    => TR
    | NE    => NE
    | GE    => LE
    | GT    => LT
    | EV    => EV
    ;

  (* When we do branch extension, we need to invert the test in a COMIB/COMB instruction *)
  fun invert (cond : condition) : condition =
    case cond of
      NEVER => TR
    | EQ    => NE
    | LT    => GE
    | LE    => GT
    | OD    => EV
    | TR    => NEVER
    | NE    => EQ
    | GE    => LT
    | GT    => LE
    | EV    => OD
    ;

  fun isEqTest (cond : condition) : bool =
    case cond of
      EQ => true
    | NE => true
    | _  => false

  (* Use for COMBT, COMIBT instructions *)
  fun encodeArithCond (cond : condition) : (bool * short) =
    case cond of
      NEVER => (true, short0)
    | EQ    => (true, short1)
    | LT    => (true, short2)
    | LE    => (true, short3)
    | OD    => (true, short7)
    | TR    => (false, short0)
    | NE    => (false, short1)
    | GE    => (false, short2)
    | GT    => (false, short3)
    | EV    => (false, short7)
    ;
 
  (* Encoding of c,f fields for arithmetic/logical instructions.
     N.B these encoding are DIFFERENT from the ones we would have to
     use with the Shift/Extract/Deposit conditions. For the logical
     instructions, the encodings are correct, but not all are valid. 
  *)
  val NEVER_short : short = (short0 << short1) Or short0;
  val LT_short    : short = (short2 << short1) Or short0;
  val OD_short    : short = (short7 << short1) Or short0;
  val EV_short    : short = (short7 << short1) Or short1;
  val UGT_short   : short = (short5 << short1) Or short1;
  
(*****************************************************************************)
(*                  The instruction datatype                                 *)
(*****************************************************************************)
  type nullify = bool;
  
  (* Number of instructions between address loaded by LOADCODEADDR and 
     actual JUMPTABLE instruction. *)
  val jumpTableOffset : int = 4;
  
  (* The (synthetic) instructions we'll actually use. *)
  datatype instruction =
    COPY       of reg * reg         (* OR r,%r0,t     *)
  | AND        of reg * reg * reg 
  | OR         of reg * reg * reg 
  | XOR        of reg * reg * reg
  | TESTTAG1   of reg * reg         (* OR,OD     r1,r2,%r0        *) 
  | TESTTAG2   of reg * reg         (* AND,OD    r1,r2,%r0        *) 
  
  | ADDO       of reg * reg * reg
  | ADDL       of reg * reg * reg
  | SUB        of reg * reg * reg
  | ARBSUB     of reg * reg * reg   (* SUBTO,EV  r1,r2,rd *)
  | STACKCHECK of reg               (* SUBT,>>   %rsl,r,%r0       *)
  | HEAPCHECK  of reg               (* SUBT,<    %rhl,r,%rhl      *)

  | LSHR       of nat5 * reg * reg  (* EXTRU r,31-n,32-n,t *)
  | ASHR       of nat5 * reg * reg  (* EXTRS r,31-n,32-n,t *)
  | SHL        of nat5 * reg * reg  (* ZDEP  r,31-n,32-n,t *)
  | TAGCODEREG of int5 * reg        (* DEPI  n,31,2,r *)
 
  | ARBADDI    of int11 * reg * reg (* ADDITO,EV n,r1,rd  *)
  | UNTAGCHECK of reg * reg         (* ADDIT,OD  -1,rs,rd *)
  | HEAPCHECKI of int11             (* ADDIT,<   n,%rhl,%rhl      *)
  
  | ADDIL      of int21 * reg
  | LDO        of int14 * reg * reg
  | LDB        of int14 * reg * reg
  | LDW        of int14 * reg * reg
  | STB        of reg * int14 * reg 
  | STW        of reg * int14 * reg 
  | STWM       of reg * int14 * reg 
  | LOADCONST  of int * reg * reg   (* LDW      <4*n+offset>(b),t *)

  | LDBXSH     of reg * reg * reg   (* we're only interested in the LDWB,S variant *)
  | LDWXSH     of reg * reg * reg   (* we're only interested in the LDWX,S variant *)

  (* Unconditional calls and branches. CALLREG and CALLSELF never nullify. *)
  | BRANCH     of nullify * label   (* BL,?  lab,regZero     *)
  | CALLREG    of reg               (* BLE   0(%sr0,%r)      *)
  | CALLSELF                        (* BL    L3,regReturn    *) 
  | JUMPREG    of nullify * reg     (* BV,?  %r0(%r)         *) 
  | JUMPSELF   of nullify           (* BL,?  L4,regZero      *) 
  
  | LOADCODEADDR of reg             (* BL    0,%r            *)
  | JUMPTABLE   of int * (label list ref)
  | BYTEOFFSET  of addrs * label
  
  (* The "nullify" flag indicates whether we nullify the instruction
     in the delay slot for taken branches. Since this is NOT the
     HPPA semantics (for backwards conditional branches the HPPA
     nullifies the NON-taken branch), we have to ensure that the
     low-level code generator sees only forward branches (where
     our semantics agree with what HPPA  provides). There's
     some code in revExpand that ensures that this is the case.
     SPF 7/3/97
  *)
  | COMB       of condition * nullify * reg * reg * label 
  | COMIB      of condition * nullify * int5 * reg * label
  
  (* Logically speaking, ARBCOMB etc should have nullify flags too, but
     the RTS emulation code isn't good enough to support delay slots
     so we have to ensure that taken branches are ALWAYS nullified.
     SPF 7/3/97
  *)
  | ARBCOMB    of condition * reg * reg * label
  | ARBCOMBAUX of condition * reg * reg * label  (* used when r1 is known to be short *)
  | ARBCOMIB   of condition * int5 * reg * label
   
  (* Pseudo-code for a branch-in point. *)
  | JUMPTARGET of label list
  
(* Not yet used ...  
  | MOVBTR     of nullify * reg * reg * label  (* we're only interested in the MOVB,TR variant *)
  | MOVIBTR    of nullify * int5 * reg * label (* we're only interested in the MOVIB,TR variant *)
... *) 
  ;

(*****************************************************************************)
(*                  Macro-instruction expander                               *)
(*****************************************************************************)

  fun fixupLabel (target : addrs) (Label (r as (ref None))) = r := Some target
    | fixupLabel (target : addrs) (Label (r as (ref (Some _)))) =
        raise InternalError "fixupLabel: label already fixed up"
  
  fun inList (x, [])     = false
    | inList (x, h :: t) = x = h orelse inList (x, t)

  fun remove (x, [])     = []
    | remove (x, h :: t) = if x = h then t else h :: remove (x, t);

  (* instrLength is the number of quads that the instruction will expand to. *)
  fun instrLength (COMB _ )       = raise InternalError "instrLength: COMB has variable length (branch extension possible)"
    | instrLength (COMIB _ )      = raise InternalError "instrLength: COMIB has variable length (branch extension possible)"
    | instrLength (ARBCOMB _ )    = raise InternalError "instrLength: ARBCOMB has variable length (branch extension possible)"
    | instrLength (ARBCOMBAUX _ ) = raise InternalError "instrLength: ARBCOMBAUX has variable length (branch extension possible)"
    | instrLength (JUMPTARGET _ ) = 0
    | instrLength (JUMPTABLE (size, _))  = size
    | instrLength _               = 1
    ;

  fun instrListLengthLoop (n, [])     = n
    | instrListLengthLoop (n, h :: t) = instrListLengthLoop (n + instrLength h, t);
 
  fun instrListLength l = instrListLengthLoop (0, l);
 
  (* Notes:
       (1) Labels are now fixed-up relative to the END of the code.
       (2) Before the code is expanded, the nullify flag in COMB/COMIB
           assumes that we're going to have a forwards jump (this should
           always be the case unless we've chained them onto a backwards
           unconditional branch). After the code is expanded, it has its
           normal HPUX interpretation (but notice that we never create
           any backwards conditional jumps, so we can say that it
           still has the previous "nullify if jump is taken" interpretation).
  *)
  fun revExpandOnto (here : addrs, [], l)     = (l, ~ (getWordAddr here))
    | revExpandOnto (here : addrs, h :: t, l) =
    case h of
      COMB (cond : condition, nullify : bool, r1 : reg, r2 : reg, lab : label) =>
      let
    val isShortForwardJump : bool =
      case lab of
        Label (ref (Some target)) => 
          isPositive12Bit (target wordAddrMinus (here wordAddrPlus 1))
      | _ => 
          false;
      in
        if isShortForwardJump
        then revExpandOnto (here wordAddrPlus ~1, t, h :: l)
        else let
          val newLab : label = Label (ref (Some here));
        in
          revExpandOnto (here wordAddrPlus ~2, t,
            COMB (invert cond, true, r1, r2, newLab) ::
            BRANCH (nullify, lab) ::
            JUMPTARGET [newLab] ::
            l)
        end
      end
    
    | COMIB (cond : condition, nullify : bool, imm : int5, r : reg, lab : label) =>
      let
    val isShortForwardJump : bool =
      case lab of
        Label (ref (Some target)) => 
          isPositive12Bit (target wordAddrMinus (here wordAddrPlus 1))
      | _ => 
          false;
      in
        if isShortForwardJump
        then revExpandOnto (here wordAddrPlus ~1, t, h :: l)
        else let
          val newLab : label = Label (ref (Some here));
        in
          revExpandOnto (here wordAddrPlus ~2, t,
            COMIB (invert cond, true, imm, r, newLab) ::
            BRANCH (nullify, lab) ::
            JUMPTARGET [newLab] ::
            l)
        end
      end
        
    | ARBCOMB (cond : condition, r1 : reg, r2 : reg, lab : label) => 
      let
    val TESTTAGS : reg * reg -> instruction = 
      if isEqTest cond then TESTTAG1 else TESTTAG2;
      
    val isShortForwardJump : bool =
      case lab of
        Label (ref (Some target)) => 
          isPositive12Bit (target wordAddrMinus (here wordAddrPlus 1))
      | _ => 
          false;
      in
        if isShortForwardJump
        then 
      revExpandOnto (here wordAddrPlus ~3, t, 
        TESTTAGS (r1, r2) ::
        UNTAGCHECK (regZero, regZero) :: 
        COMB (cond, true, r1, r2, lab) ::
        l)
        else let
          val newLab : label = Label (ref (Some here));
        in
          revExpandOnto (here wordAddrPlus ~4, t,
        TESTTAGS (r1, r2) ::
        UNTAGCHECK (regZero, regZero) :: 
        COMB (invert cond, true, r1, r2, newLab) ::
            BRANCH (true, lab) ::
            JUMPTARGET [newLab] ::
        l)
        end
      end
    
    | ARBCOMBAUX (cond : condition, r1 : reg, r2 : reg, lab : label) =>
      let
    val isShortForwardJump : bool =
      case lab of
        Label (ref (Some target)) => 
          isPositive12Bit (target wordAddrMinus (here wordAddrPlus 1))
      | _ => 
          false;
      in
        if isShortForwardJump
        then 
      revExpandOnto (here wordAddrPlus ~2, t, 
        UNTAGCHECK (r2, regZero) ::
        COMB (cond, true, r1, r2, lab) :: 
        l)
        else let
          val newLab : label = Label (ref (Some here));
        in
          revExpandOnto (here wordAddrPlus ~3, t,
        UNTAGCHECK (r2, regZero) ::
            COMB (invert cond, true, r1, r2, newLab) ::
            BRANCH (true, lab) ::
            JUMPTARGET [newLab] ::
            l)
        end
      end
 
    | ARBCOMIB (cond : condition, imm : int5, r : reg, lab : label) =>
      let
    val isShortForwardJump : bool =
      case lab of
        Label (ref (Some target)) => 
          isPositive12Bit (target wordAddrMinus (here wordAddrPlus 1))
      | _ => 
          false;
      in
        if isShortForwardJump
        then 
      revExpandOnto (here wordAddrPlus ~2, t, 
        UNTAGCHECK (r, regZero) ::
        COMIB (cond, true, imm, r, lab) :: 
        l)
        else let
          val newLab : label = Label (ref (Some here));
        in
          revExpandOnto (here wordAddrPlus ~3, t,
        UNTAGCHECK (r, regZero) ::
            COMIB (invert cond, true, imm, r, newLab) ::
            BRANCH (true, lab) ::
            JUMPTARGET [newLab] ::
            l)
        end
      end
      
    | JUMPTARGET labs => 
      let
        val U : unit = applyList (fixupLabel here, labs);
      in
        revExpandOnto (here, t, h :: l)
      end
    
      (* The labels are in reverse order (highest index first) *)
    | JUMPTABLE (tableSize, ref labList) =>
      let
        val nextAddr  : addrs = here wordAddrPlus (~ tableSize);
        val tableBase : addrs = nextAddr wordAddrPlus (~ jumpTableOffset);
      
        fun fillTable ([], l) = l
          | fillTable (lab :: labs, l) = fillTable (labs, BYTEOFFSET (tableBase, lab) :: l)
      in
        revExpandOnto (nextAddr, t, fillTable (labList, l))
      end
  
    | _ => revExpandOnto (here wordAddrPlus ~1, t, h :: l);

 
  fun revExpand (l : instruction list) : instruction list * int = revExpandOnto (addrZero, l, []);
  
(*****************************************************************************)
(*                  The pretty-printer (for code)                            *)
(*****************************************************************************)

  fun reprInstruction (segStartAddr : addrs, instr : instruction) : string =
    case instr of
      COPY (r1 : reg, rt : reg) =>
    implode ["COPY (", regRepr r1, ", ", regRepr rt, ")"]
    
    | AND (r1 : reg, r2 : reg, rt : reg) =>
    implode ["AND (", regRepr r1, ", ", regRepr r2, ", ", regRepr rt, ")"]
    
    | OR (r1 : reg, r2 : reg, rt : reg) =>
    implode ["OR (", regRepr r1, ", ", regRepr r2, ", ", regRepr rt, ")"]
    
    | XOR (r1 : reg, r2 : reg, rt : reg) =>
    implode ["XOR (", regRepr r1, ", ", regRepr r2, ", ", regRepr rt, ")"]
    
    | TESTTAG1 (r1 : reg, r2 : reg) => 
    implode ["TESTTAG1 (", regRepr r1, ", ", regRepr r2, ")"]
   
    | TESTTAG2  (r1 : reg, r2 : reg) => (* AND,OD r1,r2,%r0 *) 
    implode ["TESTTAG2 (", regRepr r1, ", ", regRepr r2, ")"]

    | ADDO (r1 : reg, r2 : reg, rt : reg) =>
    implode ["ADDO (", regRepr r1, ", ", regRepr r2, ", ", regRepr rt, ")"]
    
    | ADDL (r1 : reg, r2 : reg, rt : reg) =>
    implode ["ADDL (", regRepr r1, ", ", regRepr r2, ", ", regRepr rt, ")"]
    
    | SUB (r1 : reg, r2 : reg, rt : reg) =>
    implode ["SUB (", regRepr r1, ", ", regRepr r2, ", ", regRepr rt, ")"]
    
    | ARBSUB (r1 : reg, r2 : reg, rt : reg) =>
    implode ["ARBSUB (", regRepr r1, ", ", regRepr r2, ", ", regRepr rt, ")"]
    
    | STACKCHECK (r1 : reg) =>
    implode ["STACKCHECK (", regRepr r1, ")"]
    
    | HEAPCHECK (r1 : reg) =>
    implode ["HEAPCHECK (", regRepr r1, ")"]

    | ARBADDI (imm : int11, r1 : reg, rt : reg) =>
    implode ["ARBADDI (", Int.toString (getInt11 imm), ", ",  regRepr r1, ", ", regRepr rt, ")"]
    
    | UNTAGCHECK (r1 : reg, rt : reg) => (* ADDIT,OD  -1,rs,rd *)
    implode ["UNTAGCHECK (", regRepr r1, ", ", regRepr rt, ")"]
    
    | HEAPCHECKI (imm : int11) => (* ADDIT,<  n,%rhl,%rhl *)
    implode ["HEAPCHECKI (", Int.toString (getInt11 imm), ")"]

    | LSHR (sh : nat5, r1 : reg, rt : reg) =>
    implode ["LSHR (", Int.toString (getNat5 sh), ", ", regRepr r1, ", ", regRepr rt, ")"]
    
    | ASHR (sh : nat5, r1 : reg, rt : reg) => (* EXTRS r,31-n,32-n,t *)
    implode ["ASHR (", Int.toString (getNat5 sh), ", ", regRepr r1, ", ", regRepr rt, ")"]
    
    | SHL (sh : nat5, r1 : reg, rt : reg) => (* ZDEP  r,31-n,32-n,t *)
    implode ["SHL (", Int.toString (getNat5 sh), ", ", regRepr r1, ", ", regRepr rt, ")"]

    | TAGCODEREG (imm : int5, r : reg) =>
    implode ["TAGCODEREG (", Int.toString (getInt5 imm), ", ", regRepr r, ")"]

    | ADDIL (imm : int21, r : reg) =>
    implode ["ADDIL (", Int.toString (getInt21 imm), ", ", regRepr r, ")"]

    | LDO (imm : int14, rb : reg, rt : reg) =>
    implode ["LDO (", Int.toString (getInt14 imm), ", ",  regRepr rb, ", ", regRepr rt, ")"]

    | LDB (imm : int14, rb : reg, rt : reg) =>
    implode ["LDB (", Int.toString (getInt14 imm), ", ",  regRepr rb, ", ", regRepr rt, ")"]

    | LDW (imm : int14, rb : reg, rt : reg) =>
    implode ["LDW (", Int.toString (getInt14 imm), ", ",  regRepr rb, ", ", regRepr rt, ")"]

    | STB (rs : reg, imm : int14, rb : reg) =>
    implode ["STB (", regRepr rs, ", ", Int.toString (getInt14 imm), ", ",  regRepr rb, ")"]

    | STW (rs : reg, imm : int14, rb : reg) =>
    implode ["STW (", regRepr rs, ", ", Int.toString (getInt14 imm), ", ",  regRepr rb, ")"]

    | STWM (rs : reg, imm : int14, rb : reg) =>
    implode ["STWM (", regRepr rs, ", ", Int.toString (getInt14 imm), ", ",  regRepr rb, ")"]

    | LOADCONST (constNum : int, rb : reg, rt : reg) => (* LDW <4*n+offset>(b),t *)
    implode ["LOADCONST (", Int.toString constNum, ", " ,regRepr rb, ", ",  regRepr rt, ")"]
 
    | LDBXSH (rx : reg, rb : reg, rt : reg) =>
    implode ["LDBXSH (", regRepr rx, ", ", regRepr rb, ", ",  regRepr rt, ")"]
    
    | LDWXSH (rx : reg, rb : reg, rt : reg) =>
    implode ["LDWXSH (", regRepr rx, ", ", regRepr rb, ", ",  regRepr rt, ")"]
    
    | BRANCH (nullify : bool, Label (ref None)) =>
    implode ["BRANCH (", reprBool nullify, ", ???label???)"]

    | BRANCH (nullify : bool, Label (ref (Some targetAddr))) =>
      let
    val byteAddr : int = targetAddr byteAddrMinus segStartAddr;
      in
    implode ["BRANCH (", reprBool nullify, ", ", Int.toString byteAddr, ")"]
      end
    
    | JUMPREG (nullify : bool, r : reg) => 
    implode ["JUMPREG (", reprBool nullify, ", ", regRepr r, ")"]
    
    | JUMPSELF (nullify : bool) =>
    implode ["JUMPSELF (", reprBool nullify, ")"]
    
    | LOADCODEADDR (r : reg) =>
    implode ["LOADCODEADDR (", regRepr r, ")"]

    | BYTEOFFSET (tableAddr : addrs, Label (ref None)) =>
      let
    val tableOffset  : int = tableAddr  byteAddrMinus segStartAddr;
      in
    implode ["BYTEOFFSET (", Int.toString tableOffset, ", ???label???)"]
      end
      
    | BYTEOFFSET (tableAddr : addrs, Label (ref (Some targetAddr))) =>
      let
    val tableOffset  : int = tableAddr  byteAddrMinus segStartAddr;
    val targetOffset : int = targetAddr byteAddrMinus segStartAddr;
      in
    implode ["BYTEOFFSET (", Int.toString tableOffset, ", ", Int.toString targetOffset, ")"]
      end

    | CALLREG (r : reg) =>
    implode ["CALLREG (", regRepr r, ")"]
    
    | CALLSELF =>
    "CALLSELF"
    
    | COMB (cond : condition, nullify : bool, r1 : reg, r2 : reg, Label (ref None)) =>
    implode ["COMB (", reprCond cond, ", ", reprBool nullify, ", ", 
         regRepr r1, ", ",  regRepr r2, ", ???label???)"]

    | COMB (cond : condition, nullify : bool, r1 : reg, r2 : reg, Label (ref (Some targetAddr))) =>
      let
    val byteAddr : int = targetAddr byteAddrMinus segStartAddr;
      in
    implode ["COMB (", reprCond cond, ", ", reprBool nullify, ", ", 
         regRepr r1, ", ",  regRepr r2, ", ", Int.toString byteAddr, ")"]
      end

    | COMIB (cond : condition, nullify : bool, imm : int5, r : reg, Label (ref None)) =>
    implode ["COMIB (", reprCond cond, ", ", reprBool nullify, ", ", 
         Int.toString (getInt5 imm) , ", ",  regRepr r, ", ???label???)"]

    | COMIB (cond : condition, nullify : bool, imm : int5, r : reg, Label (ref (Some targetAddr))) =>
      let
    val byteAddr : int = targetAddr byteAddrMinus segStartAddr;
      in
    implode ["COMIB (", reprCond cond, ", ", reprBool nullify, ", ", 
         Int.toString (getInt5 imm) , ", ",  regRepr r, ", ", Int.toString byteAddr, ")"]
      end
      
    | ARBCOMB (cond : condition, r1 : reg, r2 : reg, _) =>
    implode ["ARBCOMB (", reprCond cond, ", ", regRepr r1, ", ",  regRepr r2, ", ???)"]

    | ARBCOMBAUX (cond : condition, r1 : reg, r2 : reg, _) =>
    implode ["ARBCOMBAUX (", reprCond cond, ", ", regRepr r1, ", ",  regRepr r2, ", ???)"]

    | ARBCOMIB (cond : condition, imm : int5, r : reg, _) =>
    implode ["ARBCOMIB (", reprCond cond, ", ",  Int.toString (getInt5 imm) , ", ",  regRepr r, ", ???)"]

    | JUMPTARGET labs =>
        implode ["JUMPTARGET (", Int.toString (length labs), ")"]

    | JUMPTABLE (size, _) =>
        implode ["JUMPTABLE (", Int.toString size, " entries)"]

(* ...
    | MOVBTR (nullify : bool, r1 : reg, rt : reg, Label (ref None)) =>
    implode ["MOVBTR (", reprBool nullify, ", ", 
         regRepr r1, ", ",  regRepr rt, ", ???label???)"]

    | MOVBTR (nullify : bool, r1 : reg, rt : reg, Label (ref (Some targetAddr))) =>
      let
    val byteAddr : int = targetAddr byteAddrMinus segStartAddr;
      in
    implode ["MOVBTR (", reprBool nullify, ", ", 
         regRepr r1, ", ",  regRepr rt, ", ", Int.toString byteAddr, ")"]
      end

    | MOVIBTR (nullify : bool, imm : int5, rt : reg, Label (ref None)) =>
    implode ["MOVIBTR (", reprBool nullify, ", ", 
         Int.toString (getInt5 imm) , ", ",  regRepr rt, ", ???label???)"]

    | MOVIBTR (nullify : bool, imm : int5, rt : reg, Label (ref (Some targetAddr))) =>
      let
    val byteAddr : int = targetAddr byteAddrMinus segStartAddr;
      in
    implode ["MOVIBTR (", reprBool nullify, ", ", 
         Int.toString (getInt5 imm) , ", ",  regRepr rt, ", ", Int.toString byteAddr, ")"]
      end;
... *)

  (* end reprInstruction *) 

(*****************************************************************************)
(*                  The low-level code-generator                             *)
(*****************************************************************************)

  local
    (* encoding of opcode2 field for arithmetic/logical instructions *)
    val ANDop2    : short = short8;
    val ORop2     : short = short9;
    val XORop2    : short = short10; (* 0x0A *)
    val SUBop2    : short = short16; (* 0x10 *)
    val SUBTop2   : short = short19; (* 0x13 *)
    val ADDLop2   : short = short40; (* 0x28 *)
    val SUBTOop2  : short = short51; (* 0x33 *)
    val ADDOop2   : short = short56; (* 0x38 *)
  
    fun mkALUquad (opcode2 : short, r1 : reg, r2 : reg, rt : reg, cf : short) : quad =
    let
      val opcode   : short = short2;
      val r1_short : short = toShort (getReg r1);
      val r2_short : short = toShort (getReg r2);
      val rt_short : short = toShort (getReg rt);
    
      val b0 : short = (opcode << short2) Or ((r2_short >> short3) And mask2Bits);
      val b1 : short = ((r2_short And mask3Bits) << short5) Or r1_short;
      val b2 : short = (cf << short4) Or ((opcode2 >> short2) And mask4Bits);
      val b3 : short = ((opcode2 And mask2Bits) << short6) Or rt_short; (* bit 26 is zero *)
    in
      Quad (b0, b1, b2, b3)
    end;
  
    (* encoding of opcode field for ALU immediate instructions *)
    val ADDITop  : short = short44 (* 0x2C *);
  
    fun mkALUimmedQuad
      (opcode : short, opcode2 : bool,
       imm : int11, r1 : reg, rt : reg, cf : short) : quad =
    let
      (* The only tricky part is that the sign bit of the immediate
     is stored in the LSB (not the nomal MSB) position.
      *)
      val r1_short  : short = toShort (getReg r1);
      val rt_short  : short = toShort (getReg rt);
      val imm_short : short = toShort (getInt11 imm);
      val op2_short : short = if opcode2 then short1 else short0;
      
      val b0 : short = (opcode << short2) Or ((r1_short >> short3) And mask2Bits);
      val b1 : short = ((r1_short And mask3Bits) << short5) Or rt_short;
      val b2 : short = (cf << short4) Or (op2_short << short3) Or ((imm_short >> short7) And mask3Bits);
      val b3 : short = ((imm_short And mask7Bits) << short1) Or ((imm_short >> short10) And mask1Bit);
    in
      Quad (b0, b1, b2, b3)
    end;
  
    (* encoding of opcode field for extract/deposit instructions *)
    val EXTRop   : short = short52 (* 0x34 *);
    val DEPop    : short = short53 (* 0x35 *);
    
    (* encoding of opcode2 field for extract/deposit instructions *)
    val EXTRUop2 : short = short6;
    val EXTRSop2 : short = short7;
    val ZDEPop2  : short = short2;
    val DEPIop2  : short = short7;
  
    fun mkExtrDepQuad 
     (opcode : short, opcode2 : short, 
      r2 : short, r1 : short, pos : short, len : short) : quad =
    let
      (* We generate the c field as zero (NEVER) *)
      val b0 : short = (opcode << short2) Or ((r2 >> short3) And mask2Bits);
      val b1 : short = ((r2 And mask3Bits) << short5) Or r1;
      val b2 : short = (opcode2 << short2) Or ((pos >> short3) And mask2Bits); (* c=0 *)
      val b3 : short = ((pos And mask3Bits) << short5) Or len;
    in
      Quad (b0, b1, b2, b3)
    end
  
    (* encoding of opcode field for load/store instructions *)
    val LDOop  : short = short13; (* 0x0D *)
    val LDBop  : short = short16; (* 0x10 *)
    val LDWop  : short = short18; (* 0x12 *)
    val STBop  : short = short24; (* 0x18 *)
    val STWop  : short = short26; (* 0x1A *)
    val STWMop : short = short27; (* 0x1B *)
  
    fun mkLoadStoreQuad (opcode : short, rb : reg, rt : reg, offset : int14) : quad =
    let
      (* The only tricky part is that the sign bit of the offset
     is stored in the LSB (not the nomal MSB) position.
      *)
      val rb_short  : short = toShort (getReg rb);
      val rt_short  : short = toShort (getReg rt);
      val off_short : short = toShort (getInt14 offset);
  
      val b0 : short = (opcode << short2) Or ((rb_short >> short3) And mask2Bits);
      val b1 : short = ((rb_short And mask3Bits) << short5) Or rt_short;
      val b2 : short = (off_short >> short7) And mask6Bits; (* s field is zero *)
      val b3 : short = ((off_short And mask7Bits) << short1) Or ((off_short >> short13) And mask1Bit);
    in
      Quad (b0, b1, b2, b3)
    end;

    (* encoding of opcode2 field for indexed load instructions *)
    val LDWXop2 : short = short2;
    val LDBXop2 : short = short0;
    
    fun mkLoadXSHquad (opcode2 : short, rx : reg, rb : reg, rt : reg) : quad =
    let
      val opcode    : short = short3;  
      val rb_short  : short = toShort (getReg rb);
      val rx_short  : short = toShort (getReg rx);
      val rt_short  : short = toShort (getReg rt);
  
      (* We generate the s, cc and m fields as zero; bit 19 must also be zero *)
      val b0 : short = (opcode << short2) Or ((rb_short >> short3) And mask2Bits);
      val b1 : short = ((rb_short And mask3Bits) << short5) Or rx_short;
      val b2 : short = (short1 << short5) Or ((opcode2 >> short2) And mask2Bits); (* u=1 *)
      val b3 : short = ((opcode2 And mask2Bits) << short6) Or rt_short;
    in
      Quad (b0, b1, b2, b3)
    end;
    
    (* encoding of opcode field for 17-bit unconditional branch instructions *)
    val BLop  : short = short58; (* 0x3A *)
    val BLEop : short = short57; (* 0x39 *)
    
    (* encoding of opcode2 field for 17-bit unconditional branch instructions *)
    val BLop2 : short = short0;
    (* For BLE the opcode2 field encode the space register. *)
    
    fun mkUncondBranch17Quad (opcode : short, opcode2 : short, r : reg, offset : int17, nullify : bool) : quad =
    let
      (* The tricky part is that the offset gets horribly munged. *)
      val r_short   : short = toShort (getReg r);
      val n_short   : short = if nullify then short1 else short0;
      val off_short : short = toShort (getInt17 offset);
      val w1_short  : short = (off_short >> short11) And mask5Bits; 
      val w2_short  : short = ((off_short And mask10Bits) << short1) Or ((off_short >> short10) And mask1Bit);
      val w3_short  : short = (off_short >> short16) And mask1Bit;
  
      val b0 : short = (opcode << short2) Or ((r_short >> short3) And mask2Bits);
      val b1 : short = ((r_short And mask3Bits) << short5) Or w1_short;
      val b2 : short = (opcode2 << short5) Or ((w2_short >> short6) And mask5Bits);
      val b3 : short = ((w2_short And mask6Bits) << short2) Or (n_short << short1) Or w3_short;
    in
      Quad (b0, b1, b2, b3)
    end;
    
    fun mkBVquad (rx : reg, rb : reg, nullify : bool) : quad =
    let
      val opcode   : short = short58; (* 0x3A *) (* same major opcode as BL *)
      val rb_short : short = toShort (getReg rb);
      val rx_short : short = toShort (getReg rx);
      val n_short  : short = if nullify then short1 else short0;
      val opcode2  : short = short6;             (* BL has 0 here *)
      
      val b0 : short = (opcode << short2) Or ((rb_short >> short3) And mask2Bits);
      val b1 : short = ((rb_short And mask3Bits) << short5) Or rx_short;
      val b2 : short = opcode2 << short5; (* displacement field is zero *)
      val b3 : short = n_short << short1; (* displacement field is zero *)
    in
      Quad (b0, b1, b2, b3)
    end;

    (* encoding of opcode field for conditional branch instructions *)
    val COMBTop  : short = short32; (* 0x20 *)
    val COMBFop  : short = short34; (* 0x22 *)
    val COMIBTop : short = short33; (* 0x21 *)
    val COMIBFop : short = short35; (* 0x23 *)
    val MOVBop   : short = short50; (* 0x32 *)
    val MOVIBop  : short = short51; (* 0x33 *)

    fun mkCondBranchQuad 
       (opcode : short, dest : short, source : short,
        cond : short, offset : int12, nullify : bool) : quad =
    let
      (* The only tricky part is that the top 2 bits of the immediate
     is stored in the LSB (not the normal MSB) positions.
      *)
      val off_short : short = toShort (getInt12 offset);
      val n_short   : short = if nullify then short1 else short0;
  
      val b0 : short = (opcode << short2) Or ((dest >> short3) And mask2Bits);
      val b1 : short = ((dest And mask3Bits) << short5) Or source;
      val b2 : short = (cond << short5) Or ((off_short >> short5) And mask5Bits)
      val b3 : short = ((off_short And mask5Bits) << short3) Or
                       (((off_short >> short10) And mask1Bit) << short2) Or     
                       (n_short << short1) Or
                       ((off_short >> short11) And mask1Bit);
    in
      Quad (b0, b1, b2, b3)
    end;

    fun mkQuad 
       (thisAddr : addrs,      (* relative to end of prelude *)
        instr : instruction,
        constByteOffset : int14,
        selfCallAddr : addrs ) : quad =
      case instr of
        COPY (r1 : reg, rt : reg) =>
      mkALUquad (ORop2 : short, r1, regZero, rt, NEVER_short : short)
      
      | AND (r1 : reg, r2 : reg, rt : reg) =>
      mkALUquad (ANDop2 : short, r1 : reg, r2 : reg, rt : reg, NEVER_short : short)
      
      | OR (r1 : reg, r2 : reg, rt : reg) =>
      mkALUquad (ORop2 : short, r1 : reg, r2 : reg, rt : reg, NEVER_short : short)
      
      | XOR (r1 : reg, r2 : reg, rt : reg) =>
      mkALUquad (XORop2 : short, r1 : reg, r2 : reg, rt : reg, NEVER_short : short)
      
      | TESTTAG1 (r1 : reg, r2 : reg) => (* OR,OD r1,r2,%r0 *) 
      mkALUquad (ORop2 : short, r1 : reg, r2 : reg, regZero : reg, OD_short : short)
     
      | TESTTAG2  (r1 : reg, r2 : reg) => (* AND,OD r1,r2,%r0 *) 
      mkALUquad (ANDop2 : short, r1 : reg, r2 : reg, regZero : reg, OD_short : short)
  
      | ADDO (r1 : reg, r2 : reg, rt : reg) =>
      mkALUquad (ADDOop2 : short, r1 : reg, r2 : reg, rt : reg, NEVER_short : short)
      
      | ADDL (r1 : reg, r2 : reg, rt : reg) =>
      mkALUquad (ADDLop2 : short, r1 : reg, r2 : reg, rt : reg, NEVER_short : short)
      
      | SUB (r1 : reg, r2 : reg, rt : reg) =>
      mkALUquad (SUBop2 : short, r1 : reg, r2 : reg, rt : reg, NEVER_short : short)
      
      | ARBSUB (r1 : reg, r2 : reg, rt : reg) => (* SUBTO,EV  r1,r2,rd *)
      mkALUquad (SUBTOop2 : short, r1 : reg, r2 : reg, rt : reg, EV_short : short)
      
      | STACKCHECK (r1 : reg) => (* SUBT,>>  %rsl,r,%r0 *)
      mkALUquad (SUBTop2 : short, regStackLim : reg, r1 : reg, regZero : reg, UGT_short : short)
      
      | HEAPCHECK (r1 : reg) => (* SUBT,<  %rhl,r,%rhl *)
      mkALUquad (SUBTop2 : short, regHeapLim : reg, r1 : reg, regHeapLim : reg, LT_short : short)
  
      | ARBADDI (imm : int11, r1 : reg, rt : reg) => (* ADDITO,EV n,r1,rd  *)
      mkALUimmedQuad (ADDITop, true, imm, r1, rt, EV_short)
      
      | UNTAGCHECK (r1 : reg, rt : reg) => (* ADDIT,OD  -1,rs,rd *)
      mkALUimmedQuad (ADDITop, false, int11_minus1, r1, rt, OD_short)
      
      | HEAPCHECKI (imm : int11) => (* ADDIT,<  n,%rhl,%rhl *)
      mkALUimmedQuad (ADDITop, false, imm, regHeapLim, regHeapLim, LT_short)
  
  
      | LSHR (sh : nat5, r1 : reg, rt : reg) => (* EXTRU r,31-n,32-n,t *)
    let
      (* p   = 31 - sh;
         len = 32 - sh; clen = 32 - len = sh;
      *)
      val rt_short   : short = toShort (getReg rt);
      val r1_short   : short = toShort (getReg r1);
      val sh_short   : short = toShort (getNat5 sh);
      val p_short    : short = toShort (31 - getNat5 sh);
      val clen_short : short = sh_short;
    in
      mkExtrDepQuad (EXTRop, EXTRUop2, r1_short, rt_short, p_short, clen_short)
    end
      
      | ASHR (sh : nat5, r1 : reg, rt : reg) => (* EXTRS r,31-n,32-n,t *)
    let
      (* p   = 31 - sh;
         len = 32 - sh; clen = 32 - len = sh;
      *)
      val rt_short   : short = toShort (getReg rt);
      val r1_short   : short = toShort (getReg r1);
      val sh_short   : short = toShort (getNat5 sh);
      val p_short    : short = toShort (31 - getNat5 sh);
      val clen_short : short = sh_short;
    in
      mkExtrDepQuad (EXTRop, EXTRSop2, r1_short, rt_short, p_short, clen_short)
    end
      
      | SHL (sh : nat5, r1 : reg, rt : reg) => (* ZDEP  r,31-n,32-n,t *)
    let
      (* p   = 31 - sh; cp   = 31 - p   = sh;
         len = 32 - sh; clen = 32 - len = sh;
      *)
      val rt_short   : short = toShort (getReg rt);
      val r1_short   : short = toShort (getReg r1);
      val sh_short   : short = toShort (getNat5 sh);
      val cp_short   : short = sh_short;
      val clen_short : short = sh_short;
    in
      mkExtrDepQuad (DEPop, ZDEPop2, rt_short, r1_short, cp_short, clen_short)
    end
  
      | TAGCODEREG (imm : int5, rt : reg) => (* DEPI  imm,31,2,rt *)
    let
      (* p   = 31; cp   = 31 - p   = 0;
         len = 2;  clen = 32 - len = 30;
      *)
      val rt_short    : short = toShort (getReg rt);
      val imm5_short  : short = toShort (getInt5 imm);
      val imm5_rep    : short = ((imm5_short And mask4Bits) << short1) Or ((imm5_short >> short4) And mask1Bit);
      val cp_short    : short = short0;
      val clen_short  : short = short30;
    in
      mkExtrDepQuad (DEPop, DEPIop2, rt_short, imm5_rep, cp_short, clen_short)
    end

      | ADDIL (imm : int21, r : reg) =>
    let
      (* The tricky part is that the offset gets horribly munged.
         The following use more instructions that strictly necessary,
         but I value correctness over speed!
         SPF 20/2/97
      *)
      val opcode    : short = short10; (* 0x0A *)
      val r_short   : short = toShort (getReg r);
      val imm_short : short = toShort (getInt21 imm);
      
      val field0 : short = (imm_short >> short20) And mask1Bit;
          val field1 : short = (imm_short >> short9)  And mask11Bits;
          val field2 : short = (imm_short >> short7)  And mask2Bits;
          val field3 : short = (imm_short >> short2)  And mask5Bits;
          val field4 : short =  imm_short             And mask2Bits;
          
          val rep_short : short =
            (field3 << short16) Or
            (field2 << short14) Or
            (field4 << short12) Or
            (field1 << short1)  Or
             field0;

      val b0 : short = (opcode << short2) Or ((r_short >> short3) And mask2Bits);
      val b1 : short = ((r_short And mask3Bits) << short5) Or ((rep_short >> short16) And mask5Bits);
      val b2 : short = (rep_short >> short8) And mask8Bits;
      val b3 : short = rep_short And mask8Bits;
    in
      Quad (b0, b1, b2, b3)
    end

      | LDO (imm : int14, rb : reg, rt : reg) =>
      mkLoadStoreQuad (LDOop, rb, rt, imm)
  
      | LDB (imm : int14, rb : reg, rt : reg) =>
      mkLoadStoreQuad (LDBop, rb, rt, imm)
  
      | LDW (imm : int14, rb : reg, rt : reg) =>
      mkLoadStoreQuad (LDWop, rb, rt, imm)
  
      | STB (rs : reg, imm : int14, rb : reg) =>
      mkLoadStoreQuad (STBop, rb, rs, imm)
  
      | STW (rs : reg, imm : int14, rb : reg) =>
      mkLoadStoreQuad (STWop, rb, rs, imm)
  
      | STWM (rs : reg, imm : int14, rb : reg) =>
      mkLoadStoreQuad (STWMop, rb, rs, imm)
  
      | LOADCONST (constNum : int, rb : reg, rt : reg) => (* LDW <4*n+offset>(b),t *)
        let
          (* It's the responsibility of the prelude-generation code to
             ensure that constByteOffset is sufficiently small that
             this never overflows. SPF 20/2/1997
             
             Unfortunately, if we have too many constants, this may not
             be possible. I need to look at where the LOADCONSTs are
             generated, and change the code to use a temporary register
             to bridge this gap. For now, I'll just make the error
             message more informative. SPF 23/3/1998
          *)
          val imm : int = getInt14 constByteOffset + wordSize * constNum;
        in
          if is14Bit imm
      then mkLoadStoreQuad (LDWop, rb, rt, int14 imm)
      else raise InternalError 
                   (implode ["mkQuad: LOADCONST - constant number ",
                             Int.toString constNum,
                             " requires an offset of ",
                             Int.toString imm,
                             " which is too large for a signed 14-bit field"])
        end
   
      | LDBXSH (rx : reg, rb : reg, rt : reg) =>
          mkLoadXSHquad (LDBXop2, rx, rb, rt)
          
      | LDWXSH (rx : reg, rb : reg, rt : reg) =>
          mkLoadXSHquad (LDWXop2, rx, rb, rt)
      
      | BRANCH (nullify : bool, Label (ref None)) =>
          raise InternalError "mkQuad: BRANCH - jump has not been fixed-up"

      | BRANCH (nullify : bool, Label (ref (Some targetAddr))) => (* BL,? lab,regZero *)
    let
      val wordOffset : int = targetAddr wordAddrMinus (thisAddr wordAddrPlus 2);
    in
      if is17Bit wordOffset
      then mkUncondBranch17Quad (BLop, BLop2, regZero, int17 wordOffset, nullify)
      else raise InternalError "mkQuad: BRANCH - jump exceeds 17 bit range"
    end
      
      | JUMPREG (nullify : bool, destReg : reg) => (* BV,?  regZero(destReg) *) 
          mkBVquad (regZero, destReg, nullify)
      
      | JUMPSELF (nullify : bool) => (* BL,? L4,regZero *)
    let
      (* Offsets are calculated from 2 words beyond the current instruction *)
      val wordOffset : int = selfCallAddr wordAddrMinus (thisAddr wordAddrPlus 2);
    in
      if is17Bit wordOffset
      then mkUncondBranch17Quad (BLop, BLop2, regZero, int17 wordOffset, nullify)
      else raise InternalError "mkQuad: JUMPSELF - jump exceeds 17 bit range"
    end
      
    (* Load the absolute address of here+2 into destReg *)
      | LOADCODEADDR (destReg : reg) => (* BL,? $+2,destReg *)
       mkUncondBranch17Quad (BLop, BLop2, destReg, int17_0, false)
      
        (* Compute the byte offset lab - baseAddr *)
      | BYTEOFFSET (baseAddr : addrs, Label (ref None)) =>
          raise InternalError "mkQuad: BYTEOFFSET - label has not been fixed-up"

      | BYTEOFFSET (tableAddr : addrs, Label (ref (Some targetAddr))) =>
        let
          val byteOffset : int = targetAddr byteAddrMinus tableAddr;
        in
          toQuad byteOffset
        end

      | CALLREG (r : reg) => (* BLE 0(%sr0, %r) *)
          mkUncondBranch17Quad (BLEop, short0 (* %sr0 *), r, int17_0, false)
      
      | CALLSELF => (* BL L3,regReturn *)
    let
      (* Offsets are calculated from 2 words beyond the current instruction *)
      val wordOffset : int = selfCallAddr wordAddrMinus (thisAddr wordAddrPlus 2);
    in
      if is17Bit wordOffset
      then mkUncondBranch17Quad (BLop, BLop2, regReturn, int17 wordOffset, false)
      else raise InternalError "mkQuad: CALLSELF - jump exceeds 17 bit range"
    end
      
      | COMB (cond : condition, nullify : bool, r1 : reg, r2 : reg, Label (ref None)) =>
          raise InternalError "mkQuad: COMB - jump has not been fixed-up"

      | COMB (cond : condition, nullify : bool, r1 : reg, r2 : reg, Label (ref (Some targetAddr))) =>
    let
      val r2_short : short = toShort(getReg r2);
      val r1_short : short = toShort(getReg r1);
      val (posCond : bool, cond_short : short) = encodeArithCond cond;
      val wordOffset : int = targetAddr wordAddrMinus (thisAddr wordAddrPlus 2);
    in
      (* Nullification doesn't work "properly" for backwards condtional branches *)
      if wordOffset < 0 andalso nullify
      then raise InternalError "mkQuad: COMB - can't nullify backwards jump"
    
      else if is12Bit wordOffset
        then if posCond
          then mkCondBranchQuad (COMBTop, r2_short, r1_short, cond_short, int12 wordOffset, nullify)
          else mkCondBranchQuad (COMBFop, r2_short, r1_short, cond_short, int12 wordOffset, nullify)
        else raise InternalError "mkQuad: COMB - jump exceeds 12 bit range"
    end

      | COMIB (cond : condition, nullify : bool, imm : int5, r : reg, Label (ref None)) =>
          raise InternalError "mkQuad: COMIB - jump has not been fixed-up"

      | COMIB (cond : condition, nullify : bool, imm : int5, r : reg, Label (ref (Some targetAddr))) =>
    let
      val r_short   : short = toShort(getReg r);
      val imm_short : short = toShort (getInt5 imm);
      val imm_rep   : short = ((imm_short And mask4Bits) << short1) Or 
                              ((imm_short >> short4) And mask1Bit);
      val (posCond : bool, cond_short : short) = encodeArithCond cond;
      val wordOffset : int = targetAddr wordAddrMinus (thisAddr wordAddrPlus 2);
    in
      if wordOffset < 0 andalso nullify
      then raise InternalError "mkQuad: COMIB - can't nullify backwards jump"
    
      else if is12Bit wordOffset
        then if posCond
          then mkCondBranchQuad (COMIBTop, r_short, imm_rep, cond_short, int12 wordOffset, nullify)
          else mkCondBranchQuad (COMIBFop, r_short, imm_rep, cond_short, int12 wordOffset, nullify)
        else raise InternalError "mkQuad: COMIB - jump exceeds 12 bit range"
    end

      | ARBCOMB _ =>
          raise InternalError "mkQuad: ARBCOMB not expanded"

      | ARBCOMBAUX _ =>
          raise InternalError "mkQuad: ARBCOMBAUX not expanded"

      | ARBCOMIB _ =>
          raise InternalError "mkQuad: ARBCOMIB not expanded"
          
      | JUMPTARGET _ =>
          raise InternalError "mkQuad: JUMPTARGET doesn't generate code"

      | JUMPTABLE _ =>
          raise InternalError "mkQuad: JUMPTABLE not expanded"

(* ...
      | MOVBTR (nullify : bool, r1 : reg, rt : reg, Label (ref None)) =>
          raise InternalError "mkQuad: MOVBTR - jump has not been fixed-up"

      | MOVBTR (nullify : bool, r1 : reg, rt : reg, Label (ref (Some targetAddr))) =>
    let
      val rt_short   : short = toShort (getReg rt);
      val r1_short   : short = toShort (getReg r1);
      val cond_short : short = short4; (* encoding of TR using the shift/extract/deposit table *)
      val wordOffset : int = targetAddr wordAddrMinus (thisAddr wordAddrPlus 2);
    in
      if wordOffset < 0 andalso nullify
      then raise InternalError "mkQuad: MOVBTR - can't nullify backwards jump"

      else if is12Bit wordOffset
        then mkCondBranchQuad (MOVBop, rt_short, r1_short, cond_short, int12 wordOffset, nullify)
        else raise InternalError "mkQuad: MOVBTR - jump exceeds 12 bit range"
    end

      | MOVIBTR (nullify : bool, imm : int5, rt : reg, Label (ref None)) =>
          raise InternalError "mkQuad: MOVIBTR - jump has not been fixed-up"

      | MOVIBTR (nullify : bool, imm : int5, rt : reg, Label (ref (Some targetAddr))) =>
    let
      val rt_short  : short = toShort (getReg rt);
      val imm_short : short = toShort (getInt5 imm);
      val imm_rep   : short = ((imm_short And mask4Bits) << short1) Or 
                              ((imm_short >> short4) And mask1Bit);
      val cond_short : short = short4; (* encoding of TR using the shift/extract/deposit table *)
      val wordOffset : int = targetAddr wordAddrMinus (thisAddr wordAddrPlus 2);
    in
      if wordOffset < 0 andalso nullify
      then raise InternalError "mkQuad: MOVIBTR - can't nullify backwards jump"

      else if is12Bit wordOffset
        then mkCondBranchQuad (MOVIBop, rt_short, imm_rep, cond_short, int12 wordOffset, nullify)
        else raise InternalError "mkQuad: MOVIBTR - jump exceeds 12 bit range"
    end
... *)
    (* end mkQuad *) 
  in 
    
    fun genQuadsToSeg
      (seg : cseg,
       constByteOffset : int14,
       segStartAddr : addrs,
       selfCallAddr : addrs,
       instrList : instruction list) : unit =
    let
      fun genQuadToSeg (wordIndex : int, instr : instruction) : unit =
      let
    (* Relative to start of code segment *)
    val realAddr : addrs = mkWordAddr wordIndex;
    
    (* Relative to end of code segment *)
    val thisAddr : addrs = segStartAddr wordAddrPlus wordIndex;

        (* Quad generation uses addresses that are relative to the end of the segment.
           That's because branch labels are relative to the end of the segment, because
           that makes branches extension in revExpandOnto somewhat easier to implement.
         *)
    val thisQuad : quad  = mkQuad (thisAddr, instr, constByteOffset, selfCallAddr);
    
    val U : unit =
      if (!assemblyCode)
      then let
        val U : unit = printString (Int.toString (getByteAddr realAddr));
        val U : unit = printString "\t";
        val U : unit = printHexN (8, fromQuad thisQuad);
        val U : unit = printString "\t";
        val U : unit = printString (reprInstruction (segStartAddr, instr));
      in
        printString "\n"
      end
      else ()
      in
    setQuad (thisQuad, realAddr, seg)
      end;
      
      fun genQuadLoop (wordIndex, []) = ()
        | genQuadLoop (wordIndex, instr :: rest) =
        case instr of
          JUMPTARGET labs => genQuadLoop (wordIndex, rest)
        | _ =>
      let
        val U : unit = genQuadToSeg (wordIndex, instr)
      in
        genQuadLoop (wordIndex + 1, rest)
      end;
    in
      genQuadLoop (0, instrList)
    end;
    
  end; (* local *)

(***************************************************************************  
  Functions that deal with 32-bit immediates (for convenience). If
  these functions are called with regStackPtr as an argument, it's
  the caller's responsibility to keep the stack-offset caching scheme
  in a consistent state.
***************************************************************************)  

  (* Possibly we could be even more clever with our instruction choice
     for special cases. I'll look at this later. SPF 18/2/97 *)
  fun addImmed (imm : int, ra : reg, rt : reg) : instruction list =
    if imm = 0 then if ra regEq rt then [] else [COPY (ra, rt)]
    else if is14Bit imm
    then [LDO (int14 imm, ra, rt)]
    else let 
      val (imm_left : int21, imm_right : int14) = splitLeftRight imm;
    in
      [
        ADDIL (imm_left, ra),
        LDO   (imm_right, regUnsafe, rt)
      ]
   end;
    
  fun loadImmed (imm : int, rt : reg) : instruction list =
    addImmed (imm, regZero, rt);

(*****************************************************************************)
(*                  The main "code" datatype                                 *)
(*****************************************************************************)

  datatype const =
      WVal of word        (* an existing constant *)
    | CVal of code        (* a forward-reference to another function *)
    | HVal of label       (* a handler *)

  (* Constants which are too far to go inline in the code are put in
     a list and put at the end of the code. They are arranged so that
     the garbage collector can find them and change them as necessary.
     A reference to a constant is treated like a forward reference to a
     label. *)

  (* A code list is used to hold a list of code-vectors which must have the
     address of this code-vector put into it. *)

  and setCodeseg =
      Unset
    | Set of (cseg * addrs)  (* Used for completing forward references. *)

  and code = Code of 
    { 
      procName:       string,         (* Name of the procedure. *)
      noClosure:      bool,           (* should we make a closure from this? *)
      codeVec:        instruction list ref, (* The code we're generating (in reverse order) *)
      constVec:       (int * const) list ref, (* Vector of words to be put at end *)
      numOfConsts:    int ref,        (* Number of constants that haven't been fixed up yet *)
(*    stackReset:     int ref,     *) (* Distance to reset the stack before the next instr. *)
      otherCodes:     code list ref,  (* Other code vectors with forward references to this vector. *)
      resultSeg:      setCodeseg ref, (* The segment as the final result. *)
      mustCheckStack: bool ref,       (* Set to true if stack check must be done. *)
      exited:         bool ref,       (* False if we can fall-through to here *)
      regTemp1Next:   bool ref        (* Used to share work between regTemp1 and regTemp2 *)
    };

(*****************************************************************************)
(*                  Auxiliary functions on "code"                            *)
(*****************************************************************************)

  fun procName       (Code {procName,...})         = procName;
  fun noClosure      (Code {noClosure,...})        = noClosure;
  fun codeVec        (Code {codeVec,...})          = codeVec;
  fun constVec       (Code {constVec,...})         = constVec;
  fun numOfConsts    (Code {numOfConsts,...})      = numOfConsts;
(*fun stackReset     (Code {stackReset ,...})      = stackReset;*)
  fun otherCodes     (Code {otherCodes,...})       = otherCodes;
  fun resultSeg      (Code {resultSeg,...})        = resultSeg;
  fun mustCheckStack (Code {mustCheckStack,...})   = mustCheckStack;
  fun exited         (Code {exited,...})           = exited;
  
  fun chooseTempReg  (Code {regTemp1Next,...}) : reg =
  let
    val regTemp1Now : bool = !regTemp1Next;
    val U : unit = regTemp1Next := not regTemp1Now;
  in
    if regTemp1Now then regTemp1 else regTemp2
  end;

  fun unreachable cvec = ! (exited cvec);

  (* Need not be exhaustive, but must be conservative! *)
  fun noFallThrough (instrList : instruction list) : bool =
   case instrList of
     (BRANCH (true, lab)  :: _ ) => true
   | (JUMPSELF true       :: _ ) => true
   | (JUMPREG (true, reg) :: _ ) => true
   | _ => false;

  (* Test for identity of the code segments by testing whether
     the resultSeg ref is the same. N.B. NOT its contents. *)
  infix is;
  fun a is b = (resultSeg a = resultSeg b);
  
  fun sameConst (WVal w1, WVal w2) = wordEq (w1, w2)
    | sameConst (HVal h1, HVal h2) = h1 = h2
    | sameConst (CVal c1, CVal c2) = c1 is c2
    | sameConst (_,       _)       = false;

  (* create and initialise a code segment *)
  fun codeCreate (noClosure : bool, name : string) : code = 
    Code
      { 
         procName         = name,
         noClosure        = noClosure,
         codeVec          = ref [],
         constVec         = ref [(0, WVal (if name = "" then toWord 0 else toWord name))],
         numOfConsts      = ref 1,
(*       stackReset       = ref 0, *) (* stack adjustment in WORDs *)
         otherCodes       = ref [],
         resultSeg        = ref Unset,   (* Not yet done *)
         mustCheckStack   = ref false,
         exited           = ref false,
         regTemp1Next     = ref true (* ref false would work just as well, I hope! *)
      };

(*****************************************************************************)
(*                  Functions dealing with constants                         *)
(*****************************************************************************)

  fun printConstVec (constants : (int * const) list, segStartAddr : addrs, constStartAddr : addrs) : unit =
    case constants of 
      [] => ()
    | _  =>
    let
      val sortedConstants : (int * const) list =
    quickSort (fn (n1, _) => fn (n2, _) => n1 < n2) constants;
    
      fun printConstCode (a : address) : unit =
    printString ("code:\t" ^ nameOfCode a);
      
      fun printConstClosure (a : address) : unit =
    printString ("clos:\t" ^ nameOfCode a);
      
      fun printWords (a : address) : unit =
    let
      val objLength : int = toInt (ADDRESS.length a)
    in
      if objLength = 1
      then printString ("words:\t1 word")
      else printString ("words:\t" ^ Int.toString objLength ^ " words")
    end;
      
      fun printBytes (a : address) : unit =
    let
      val objLength  : int = toInt (ADDRESS.length a)
    in
      if objLength = 1
      then printString ("bytes:\t1 word")
      else printString ("bytes:\t" ^ Int.toString objLength ^ " words")
    end;
  
      fun printConst (constNum : int, value : const) : unit =
      let
        val constAddr : addrs = constStartAddr wordAddrPlus constNum;
    val U : unit = printString (Int.toString (getByteAddr constAddr));
    val U : unit = printString "\tconst "; 
    val U : unit = printString (Int.toString constNum);
    val U : unit = printString " = \t"; 
    val U : unit = 
      case value of
        CVal c =>
          let
        val U : unit = printString (if noClosure c then "code:\t" else "clos:\t");
          in
        printString (procName c)
          end
          
      | HVal (Label (ref None)) =>
          printString "handle:\t????? (not fixed up)"
          
      | HVal  (Label (ref (Some targetAddr))) =>
          let
        val U : unit = printString "handle:\t";
          in
        printString (Int.toString (targetAddr byteAddrMinus segStartAddr))
          end
          
      | WVal w => 
          if isShort w
          then let
        val value : int = toInt (toShort w);
        val U : unit = printString "short:\t";
        val U : unit = printHex value;
        val U : unit = printString " (";
        val U : unit = printString (Int.toString value);
          in
        printString ")"
          end
          
          else if isString w
          then let
        val U : unit = printString "string:\t";
          in
        printString (String.toString (toString w))
          end
          
          else let
        val a : address = toAddress w;
          in
        if 256 <= toInt (flags a)
          then printString "RTS entry"
        else if isCode a
          then printConstCode a
        else if isBytes a
          then printBytes a
        else if isWords a andalso 1 <= toInt (ADDRESS.length a)
          then let
            val w' : word = loadWord (a, short0)
          in
            if not (isShort w')
            then let
              val a' : address = toAddress w';
            in
              if toInt (flags a') < 256 andalso isCode a'
              then printConstClosure a'
              else printWords a (* First element of tuple is not a code segment *) 
            end
            else printWords a (* First element of tuple is a short *)
          end
          else printWords a (* Not a proper tuple (shouldn't occur) *)
          end;
      in
    printString "\n"
      end (* printConst *)
      
      val U : unit = printString "Constants section:\n";
    in
      applyList (printConst, sortedConstants)
    end; (* printConstVec *)


  (* Find the offset in the constant area of a constant. *)
  (* The first has offset 0.                             *)
  fun addConstToVec (valu : const, cvec : code) : int =
  let
     (* Search the list to see if the constant is already there. Handlers
        are never matched because they are assumed always to be different. *)
    fun findConst (valu, (n, v) :: t) =
         if sameConst (valu, v) then n else findConst (valu, t)
      | findConst (valu, []) =
     let
        val num : int = ! (numOfConsts cvec);
        val U : unit  = numOfConsts cvec := num + 1;
        val U : unit  = constVec cvec := (num, valu) :: ! (constVec cvec);
      in
         num
      end
  in
    findConst (valu, !(constVec cvec))
  end;

  (* Make a reference to another procedure. Usually this will be a forward *)
  (* reference but it may have been compiled already, in which case we can *)
  (* put the code address in now. *)
  fun codeConst (r : code, cvec : code) : int =
    case ! (resultSeg r) of
      Set (seg : cseg, _)  =>
      let
     val addr : address = csegAddr seg;
       in
     addConstToVec (WVal (toWord addr), cvec)
       end
    
    | Unset => (* forward *)
      (* Add the referring procedure onto the list of the procedure
         referred to if it is not already there. This makes sure that when
         the referring procedure is finished and its address is known the
         address will be plugged in to every procedure which needs it.
      *)
      let
        fun onList (x, [])     = false
          | onList (x, c::cs) = (x is c) orelse onList (x, cs);
          
        val codeList = ! (otherCodes r);
        val U : unit =
          if onList (cvec, codeList) then ()
          else otherCodes r := cvec :: codeList;
      in
        addConstToVec (CVal r, cvec)
      end;

(*****************************************************************************)
(*                  Functions dealing with labels                            *)
(*****************************************************************************)

(*****************************************************************************)
(*                  Code-generation functions (1)                            *)
(*****************************************************************************)
 
(* ...
  (* We start by being very simple-minded *)
  fun gen (cvec : code, instr : instruction) : unit =
    if unreachable cvec then ()
    else codeVec cvec := instr :: (! (codeVec cvec))
    
  fun genList (cvec : code, instrList : instruction list) : unit =
    if unreachable cvec then ()
    else codeVec cvec := revOnto (instrList, ! (codeVec cvec))
... *)

  (* A small, very local, optimisation *)
  fun optimisePushes (instrList : instruction list) : instruction list =
    case instrList of
      (STWM (rs1, n1, rb1) :: STWM (rs2, n2, rb2) :: rest) =>
        if rb1 regEq rb2 andalso rb1 regNeq rs1 andalso rb1 regNeq rs2
        then let
          val n : int = getInt14 n1 + getInt14 n2
        in
          if is14Bit n
          then (STWM (rs1, int14 n, rb1) :: STW (rs2, n2, rb2) :: rest)
          else instrList
        end
        else instrList
    | _ => instrList
  
  (* Catch multiple pushes *)
  fun gen (cvec : code, instr : instruction) : unit =
    if unreachable cvec then ()
    else codeVec cvec := optimisePushes (instr :: (! (codeVec cvec)))
    
  fun genList (cvec : code, [] : instruction list) : unit = ()
    | genList (cvec : code, instr :: instrList) : unit =
    let
      val U : unit = gen (cvec, instr)
    in
      genList (cvec, instrList)
    end
  
(*****************************************************************************)
(*              Non-naive jump fixup.                                        *)
(*****************************************************************************)
  (* This code deletes jumps to the immediately following instruction. It assumes
     that we generate a unique label for each jump, so that we can delete the label
     when we delete the jump. SPF 18/3/97
  *)
  fun addLabels ([]   : label list, instrList : instruction list) : instruction list = instrList
    | addLabels (labs : label list, instrList : instruction list) : instruction list =
  let
    val default : instruction list = JUMPTARGET labs :: instrList;
  in
    case instrList of
      []             => default
   | (instr :: rest) =>
        case instr of
          JUMPTARGET labList => addLabels (labs @ labList, rest)

    | COMB (_, true, _, _, lab : label) =>
        if inList (lab, labs) then addLabels (remove (lab, labs), rest) else default
    
    | COMIB (_, true, _, _, lab : label) =>
        if inList (lab, labs) then addLabels (remove (lab, labs), rest) else default
    
    | ARBCOMB (_, _, _, lab : label) => 
        if inList (lab, labs) then addLabels (remove (lab, labs), rest) else default
    
    | ARBCOMBAUX (_, _, _, lab : label) =>
        if inList (lab, labs) then addLabels (remove (lab, labs), rest) else default
     
    | ARBCOMIB  (_, _, _, lab : label) =>
        if inList (lab, labs) then addLabels (remove (lab, labs), rest) else default
          
    | BRANCH (true, lab : label) =>
        if inList (lab, labs) then addLabels (remove (lab, labs), rest) else
       (case rest of
              COMB (cond : condition, true, r1 : reg, r2 : reg, lab2 : label) :: rest2 =>
        if inList (lab2, labs)
        then addLabels (remove (lab2, labs), COMB (invert cond, true, r1, r2, lab) :: rest2)
        else default
    
        | COMIB (cond : condition, true, imm : int5, r : reg, lab2 : label) :: rest2  =>
        if inList (lab2, labs)
        then addLabels (remove (lab2, labs), COMIB (invert cond, true, imm, r, lab) :: rest2)
        else default
    
        | ARBCOMB (cond : condition, r1 : reg, r2 : reg, lab2 : label) :: rest2  => 
        if inList (lab2, labs)
        then addLabels (remove (lab2, labs), ARBCOMB (invert cond, r1, r2, lab) :: rest2)
        else default
        
        | ARBCOMBAUX (cond : condition, r1 : reg, r2 : reg, lab2 : label) :: rest2  => 
        if inList (lab2, labs)
        then addLabels (remove (lab2, labs), ARBCOMBAUX (invert cond, r1, r2, lab) :: rest2)
        else default
     
        | ARBCOMIB (cond : condition, imm : int5, r : reg, lab2 : label) :: rest2 =>
        if inList (lab2, labs)
        then addLabels (remove (lab2, labs), ARBCOMIB (invert cond, imm, r,  lab) :: rest2)
        else default
          
        | _ => default)
    
    | _ => default
   end;
    
  (* addSpecialLabel does NOT attempt to optimise jumps to this label. That's
     because it's only used for the default case for jump tables where
       (1) No optimisations should apply.
       (2) They would be unsafe because we use the same label for multiple
           branch instructions.
  *)

  fun addSpecialLabel (lab : label, instrList : instruction list) : instruction list =
    JUMPTARGET [lab] :: instrList;

(*****************************************************************************)
(* Functions that need to know about the stack-pointer adjustment cache.     *)
(*****************************************************************************)

  (* Exported: adds in the reset. Very naive version - will be changed later *)
  fun resetStack (wordOffset : int, cvec : code) : unit =
    genList (cvec, addImmed (wordOffset * wordSize, regStackPtr, regStackPtr));

  fun genPendingStackAdjustment (cvec : code) : unit = ();

  fun genLimitStackReset (cvec : code) : unit = ();
  
  (* Exported: very naive version - will be changed later! *)
  fun setSl (wordOffset : int, cvec : code) : unit = 
    genList (cvec, addImmed (wordOffset * wordSize, regStackPtr, regClosure));

  (* Exported: very naive version - will be changed later! *)
  fun genPush (r : reg, cvec) : unit =
    gen (cvec, STWM (r, int14 (~ wordSize), regStackPtr));

(* Caching versions - actually appear to generate worse code ...
  (* Adds in the reset. *)
  fun resetStack (wordOffset : int, cvec) : unit =
    stackReset cvec := ! (stackReset cvec) + wordOffset;

  fun genPendingStackAdjustment (cvec : code) : unit =
  let
    val wordAdjustment : int = !(stackReset cvec);
    val byteAdjustment : int = wordAdjustment * wordSize;
    val U : unit = stackReset cvec := 0;
  in
    genList (cvec, addImmed (byteAdjustment, regStackPtr, regStackPtr))
  end;
 
  fun genLimitStackReset (cvec : code) : unit =
  let
    (* Use isSafe12Bit rather than is12Bit to ensure that
       adjustedWordOffset in genPush remains a 12-bit quantity.
       SPF 16/10/1997
    *)
    fun isSafe12Bit (i : int) : bool = 
      ~exp2_11 + 1 <= i andalso i <= exp2_11 - 1;
  in
    if not (isSafe12Bit (!(stackReset cvec)))
    then genPendingStackAdjustment cvec
    else ()
  end;
  
  fun setSl (wordOffset : int, cvec : code) : unit = 
  let
    val U : unit = genLimitStackReset cvec;
    val adjustedWordOffset : int = wordOffset + !(stackReset cvec);
    val adjustedByteOffset : int = adjustedWordOffset * wordSize;
  in
    genList (cvec, addImmed (adjustedByteOffset, regStackPtr, regClosure))
  end;
  
  fun genPush (r : reg, cvec : code) : unit =
  let
    val U : unit = genLimitStackReset cvec;
    val wordOffset : int = !(stackReset cvec);
    val adjustedWordOffset : int = wordOffset - 1;
    val U : unit = stackReset cvec := adjustedWordOffset;
    val adjustedByteOffset : int = adjustedWordOffset * wordSize;
  in
    gen (cvec, STW (r, int14 adjustedByteOffset, regStackPtr))
  end;
  
  (* We want the peephole optimiser to combine the STW for the last push
     with the eventual stack adjustment (giving a STWM instead) but I haven't
     implemented this yet.
     SPF 16/10/1997
  *)
... *)

  (* When we implement code reordering, this will have to change. *)
  fun genBlock (cvec : code, instrList : instruction list) : unit =
  let
    val U : unit = genPendingStackAdjustment cvec;
  in
    genList (cvec, instrList)
  end;
  
  fun fixup (labs : labels, cvec : code) : unit =
    case labs of
      [] => () (* Don't fixup a branch-in from dead code! *)
    | _  =>
      let
        val U : unit = genPendingStackAdjustment cvec;
    val U : unit = exited cvec := false;
      in
    codeVec cvec := addLabels (labs, ! (codeVec cvec))
      end;
  
  (* Used when the label may be the target of several branch instructions *)
  fun fixupSpecialLabel (lab : label, cvec : code) : unit =
  let
    val U : unit = genPendingStackAdjustment cvec;
    val U : unit = exited cvec := false;
  in
    codeVec cvec := addSpecialLabel (lab, ! (codeVec cvec))
  end;

(***************************************************************************  
  Load and Store operations.
***************************************************************************)  
  (* Exported *)
  fun genLoad (byteOffset : int, ra : reg, rt : reg, cvec : code) : unit =
    (* It's safe to have rt = regUnsafe  and genLoadPush uses this fact! *)
    if ra regEq regUnsafe
    then raise InternalError ("genStore: can't use " ^ regRepr regUnsafe)
    else let
      (* Do we need to fix-up the stack pointer? *)
      val U : unit = 
    if rt regEq regStackPtr
    then genPendingStackAdjustment cvec
        else ();
        
      val adjustedOffset : int = byteOffset;
(* ...
      val adjustedOffset : int =
        if ra regEq regStackPtr
        then let
          val U : unit = genLimitStackReset cvec;
          val wordAdjustment : int = !(stackReset cvec);
        in
          byteOffset + wordAdjustment * wordSize
        end
        else byteOffset;
... *)
   in
     if is14Bit adjustedOffset
      then gen (cvec, LDW (int14 adjustedOffset, ra, rt))
      else let
    val (adjustedOffset_left : int21, adjustedOffset_right : int14) =
      splitLeftRight adjustedOffset;
      in
    genList (cvec,
      [
        ADDIL (adjustedOffset_left, ra),
        LDW   (adjustedOffset_right, regUnsafe, rt)
      ])
      end
    end;

  (* Exported *)
  fun genStore (rs : reg, byteOffset : int, ra : reg, cvec : code) : unit =
    if rs regEq regUnsafe orelse ra regEq regUnsafe
    then raise InternalError ("genStore: can't use " ^ regRepr regUnsafe)
    else let
      (* Do we need to fix-up the stack pointer? *)
      val U : unit = 
    if rs regEq regStackPtr
    then genPendingStackAdjustment cvec
        else ();

      val adjustedOffset : int = byteOffset;
(* ...
      val adjustedOffset : int =
        if ra regEq regStackPtr
        then let
          val U : unit = genLimitStackReset cvec;
          val wordAdjustment : int = !(stackReset cvec);
        in
          byteOffset + wordAdjustment * wordSize
        end
        else byteOffset;
... *)
   in
      if is14Bit adjustedOffset
      then gen (cvec, STW (rs, int14 adjustedOffset, ra))
      else let
    val (adjustedOffset_left : int21, adjustedOffset_right : int14) =
      splitLeftRight adjustedOffset;
      in
    genList (cvec,
      [
         ADDIL (adjustedOffset_left, ra),
         STW   (rs, adjustedOffset_right, regUnsafe)
      ])
      end
    end;

  (* Exported - Can we store the value without going through a register? No. *)
  fun isStoreI (cnstnt: word) : bool = false;

  fun genStoreI (cnstnt: word, offset: int, rb: reg, cvec: code) : unit =
    raise InternalError "Not implemented: genStoreI";

  (* Load a value and push it on the stack. Used when all
     the allocatable registers have run out. We must be very careful
     if we try to reorder this code, because it puts a tagged value
     into an untagged register. That's why we use regUnsafe - to
     remind ourseleves of the problem! *)
  fun genLoadPush (offset : int, base : reg, cvec : code) : unit =
  let 
    (* We need to call genLimitStackReset here; otherwise genPush
       might call genPendingStackAdjustment which could corrupt
       regUnsafe. SPF 16/10/1997
    *)
    val U : unit = genLimitStackReset cvec;
    val U : unit = genLoad (offset, base, regUnsafe, cvec);
  in
    genPush (regUnsafe, cvec)
  end;

  (* This is false because there's no quicker way than loading
     into a register and then pushing that. *) 
  val preferLoadPush = false;

(*****************************************************************************)
(*              Functions to generate (non-naive) branch instructions        *)
(*****************************************************************************)

  fun unconditionalBranch (cvec : code) : labels =
    let
      (* We have to do this before we look for old labels,
         as otherwise we might try to combine branches that
         require different stack adjustments!
      *)
      val U : unit = genPendingStackAdjustment cvec;

      val oldLabs : label list =
    case ! (codeVec cvec) of
      JUMPTARGET labs :: rest =>
      let
        (* Ungenerate branch-in *)
        val U : unit = codeVec cvec := rest;
        val U : unit = exited cvec  := noFallThrough rest;
      in
        labs
      end
    | _ => []
    in
      if unreachable cvec then oldLabs
      else let (* branch-out required *)
    val lab : label = Label (ref None);
    val U   : unit  = genBlock (cvec, [BRANCH (true, lab)])
    val U   : unit  = exited cvec := false;
      in
    lab :: oldLabs
      end
    end;
  
(***************************************************************************  
  Functions for handling compiled constants
***************************************************************************)  

  type handlerLab = label option;

  local
    (* Generating pseudo-instructions make this SO much easier. SPF 18/2/97 *)
    fun genLoadConstant (constNum : int, constR : reg, destR : reg, cvec : code) : unit =
      gen (cvec, LOADCONST (constNum, constR, destR));
  in 
    fun genLoadConst (v : word, constR : reg, destR : reg, cvec : code) : unit =
      if unreachable cvec then ()
      else let
    val constNum : int = addConstToVec (WVal v, cvec);
      in
    genLoadConstant (constNum, constR, destR, cvec)
      end;
  
    fun genLoadCoderef (rf : code, constR : reg, destR : reg, cvec : code) : unit =
      if unreachable cvec then ()
      else let
    val constNum : int = codeConst (rf : code, cvec);
      in
    genLoadConstant (constNum, constR, destR, cvec)
      end;
  
    fun pushAddress (constR : reg, cvec : code) : handlerLab =
      if unreachable cvec then None
      else let
    val lab : label = Label (ref None);
    val constNum : int = addConstToVec (HVal lab, cvec);
    val regTemp  : reg = chooseTempReg cvec;
    val U : unit = genLoadConstant (constNum, constR, regTemp, cvec);
    val U : unit = genPush (regTemp, cvec);
      in
    Some lab
      end;
  end;

  (* Less naive now - even allows branch chaining! SPF 27/2/97 *)
  fun fixupHandler (None : handlerLab, cvec : code) : unit = ()
    | fixupHandler (Some lab : handlerLab, cvec : code) : unit =
    fixup ([lab], cvec);

(***************************************************************************  
  Functions calls and returns (plus raising exceptions)
***************************************************************************)  
  datatype callKinds =
    StaticLink   (* Static link call.  *)
  | FullCall     (* Full closure call. Includes calls to the run-time system. *)
  | Recursive    (* The function calls itself. *)
  | PureCode     (* The function calls a pre-compiled function. *);
  
  (* I/O calls are treated as full closure-calls; pure-code calls are
     just static-link calls where the caller hasn't initialised the
     closure.  *)
  val IoCall   : callKinds = FullCall;

(*****************************************************************************
Calling conventions:
   FullCall:
     the caller loads the function's closure into regClosure and then
     (the code here) loads regCode from the closure and jumps to
     that address. Since this code-generator doesn't support
     combined closure/code segments, regCode will always contain a valid
     pointer (unlike the SPARC, where the Poly version of compiler
     use optimised closures, which can cause a lot of grief). 
     
     Note: actually there's still a problem calling RTS functions,
     since the RTS code address is NOT a valid ML value. We'll
     have to ensure that the RTS always zaps it. I discovered
     this problem in the SPARC version, but it occurs here too.
     SPF 18/7/96
     
   IoCall:
     same as FullCall (on this architecture). We could use a
     temporary register rather than regCode, but this wouldn't
     actually gain us anything, as the RTS still has to handle
     the possibility of FullCall entry.
     SPF 18/7/96
     
   StaticLink:
     the caller loads the function's static-link into regClosure and
     its code address into regCode and then (the code here) jumps
     to the code.
     
   Recursive:
     the caller loads its own function's closure/static-link into regClosure
     and its constants address into regCode and then (the code here) jumps
     to the start of its segment.

    (N.B. on architectures which don't have relative calls, we can always
     store the code address in the constants section, fetch it via the
     constants register and jump via a register.
     
   PureCode:
     the caller loads the functions' code address into regCode and then
     (the code here) jumps to the code. regClosure is treated as a
     scratch register.

*****************************************************************************)
  (* Call a function. We have to set the stack-check flag 
     to ensure that the called procedure receives its full
     minStackCheck words allocation of "free" stack. *)
  fun callFunction (callKind, cvec) : unit =
    let
      val U : unit = 
    case callKind of 
      Recursive =>
        genBlock (cvec, [CALLSELF, TAGCODEREG (int5_CODETAG, regReturn)])
        
    | PureCode =>
        genBlock (cvec, [CALLREG regCode, TAGCODEREG (int5_CODETAG, regReturn)])
    
    | StaticLink => 
        genBlock (cvec, [CALLREG regCode, TAGCODEREG (int5_CODETAG, regReturn)])
        
    | FullCall =>
      let
        (* This is NOT generated as part of the following genBlock, because we
           want to allow the code-reordering phase (when written!) to move
           this instruction further back into the basic block. SPF 19/2/97
        *)
        val U : unit = gen (cvec, LDW (int14_0, regClosure, regCode));
      in
        genBlock (cvec, [CALLREG regCode, TAGCODEREG (int5_CODETAG, regReturn)])
      end
    in
      mustCheckStack cvec := true
    end

  (* The exception argument has already been loaded into regResult and the
     address of raiseEx in the ioVector has been loaded into regCode.
     We use a call, not just a jump, so that the RTS exception handler
     can find (the name of) this code segment if it needs to do
     a stack trace. We also must ensure that the stack pointer is
     updated, since otherwise the handler register might point
     beyond the end of the stack. SPF 8/9/95
  *)
  fun raiseEx cvec = 
  let
    val U : unit = genBlock (cvec, [CALLREG regCode, TAGCODEREG (int5_CODETAG, regReturn)]);
  in
    (* we're not coming back, even though we've "called" the handler *)
    exited cvec := true
  end;
  
  (* Tail recursive jump to a function. We have to set the stack-check
     flag to enable the user to break out of loops. Exception: (hack!)
     we don't have to do this if we are calling a pre-compiled function
     (PureCode) because that can't possibly lead to an infinite regress. *)
  fun jumpToFunction (callKind, returnReg, cvec) : unit =
    let
      val U : unit = 
    case callKind of
      Recursive =>
        if returnReg regEq regReturn
        then genBlock (cvec, [JUMPSELF true])
        else genBlock (cvec, [JUMPSELF false, COPY (returnReg, regReturn)])
          
    | PureCode =>
      let
        val U : unit = 
          if returnReg regEq regCode
          then raise InternalError "jumpToFunction: returnReg is regCode"
          else ();
          in
        if returnReg regEq regReturn
        then genBlock (cvec, [JUMPREG (true, regCode)])
        else genBlock (cvec, [JUMPREG (false, regCode), COPY (returnReg, regReturn)])
          end
          
    | StaticLink =>
      let
        val U : unit = 
          if returnReg regEq regCode
          then raise InternalError "jumpToFunction: returnReg is regCode"
          else ();
          in
        if returnReg regEq regReturn
        then genBlock (cvec, [JUMPREG (true, regCode)])
        else genBlock (cvec, [JUMPREG (false, regCode), COPY (returnReg, regReturn)])
          end
        
    | FullCall =>
      let
        val U : unit = 
          if returnReg regEq regCode
          then raise InternalError "jumpToFunction: returnReg is regCode"
          else ();

        val U : unit = gen (cvec, LDW (int14_0, regClosure, regCode));
      in
        if returnReg regEq regReturn
        then genBlock (cvec, [JUMPREG (true, regCode)])
        else genBlock (cvec, [JUMPREG (false, regCode), COPY (returnReg, regReturn)])
      end;
      
      val U : unit =
        case callKind of
          PureCode => () 
        | _        => mustCheckStack cvec := true 
    in
      exited cvec := true
    end;

  (* Return and remove args from stack. *)
  fun returnFromFunction (returnReg : reg, stackArgs : int, cvec : code) : unit =
  let
    val regTemp : reg = chooseTempReg cvec;
    val U : unit = gen (cvec, LDO (int14_minus2, returnReg, regTemp));
    val U : unit = resetStack (stackArgs, cvec); (* Add in the reset. *)
    (* We could try to be clever and put the stack adjustment in the
       delay slot here! However, it might be better to improve the
       peephole optimiser instead. SPF 16/10/1997
    *)
    val U : unit = genBlock (cvec, [JUMPREG (true, regTemp)])
  in
    exited cvec := true
  end;

  fun allocStore (length : int, flag : short, resultReg : reg, cvec : code) : unit =
    if length < 1 orelse exp2_24 <= length
    then raise InternalError "allocStore: bad length"
    else let
      val words : int      = length + 1;
      val bytes : int      = words * wordSize;
      val decrement : int  = ~ bytes;
      val lengthWord : int = length + (toInt flag * exp2_24);
    in
      if is11Bit decrement (* should we use words instead? *)
      then let
        val regTemp : reg = chooseTempReg cvec;
        val U : unit = genBlock (cvec, [HEAPCHECKI (int11 decrement)]);
      in
        genList (cvec,
          loadImmed (lengthWord, regTemp) @
          [
            LDO  (int14 (wordSize + decrement), regHeapPtr, resultReg),
            STWM (regTemp, int14 decrement, regHeapPtr)
          ])
      end
      else let
        val regTempA : reg = chooseTempReg cvec;
        val regTempB : reg = chooseTempReg cvec;
        val U : unit = genList (cvec, loadImmed (bytes, regTempA));
        val U : unit = genBlock (cvec, [HEAPCHECK regTempA]);
      in
        (* Carefully coded to work even if regTempA = regTempB *)
        genList (cvec,
           SUB (regHeapPtr, regTempA, regHeapPtr) ::
          (loadImmed (lengthWord, regTempB) @
          [
           LDO (int14 wordSize, regHeapPtr, resultReg),
           STW (regTempB, int14_0, regHeapPtr)
          ]))
      end
    end;
  
  (* Remove the mutable bit; not safe for code objects. *)
  fun setFlag (baseReg : reg, cvec : code, flag : short) : unit =
  let
    val U : unit = 
      if baseReg regEq regStackPtr
      then raise InternalError "setFlag: can't set flags on stack"
      else ();
    val flagRep : int = toInt flag;
  in
    if flagRep = 0
    then gen (cvec, STB (regZero, int14_minus4, baseReg))
    else let
      val regTemp : reg = chooseTempReg cvec;
    in
      genList (cvec,
        loadImmed (flagRep, regTemp) @
        [STB (regTemp, int14_minus4, baseReg)])
    end
  end;

  (* Don't need to do anything (until we implement code re-ordering). *)
  val completeSegment = (fn code => ());

(***************************************************************************  
  General operations
***************************************************************************)
  datatype instrs = 
    InstrMove
  | InstrAddA
  | InstrSubA
  | InstrRevSubA
  | InstrMulA
  | InstrOrW
  | InstrAndW
  | InstrXorW
  | InstrLoad
  | InstrLoadB
  | InstrVeclen
  | InstrPush
  | InstrUpshiftW    (* logical shift left *)
  | InstrDownshiftW  (* logical shift right *)
  | InstrBad;
  
  (* Can the we use the same register as the source and destination
     of an instructions? On this machine - yes. *)
  val canShareRegs : bool = true;

  fun isInstrBad     InstrBad     = true | isInstrBad      _ = false;

  (* exported versions *)
  val instrMove       = InstrMove;
  val instrAddA       = InstrAddA;
  val instrSubA       = InstrSubA;
  val instrRevSubA    = InstrRevSubA;
  val instrMulA       = InstrMulA;
  val instrOrW        = InstrOrW;
  val instrAndW       = InstrAndW;
  val instrXorW       = InstrXorW;
  val instrLoad       = InstrLoad;
  val instrLoadB      = InstrLoadB;
  val instrVeclen     = InstrVeclen;
  val instrPush       = InstrPush;
  val instrUpshiftW   = InstrUpshiftW;
  val instrDownshiftW = InstrDownshiftW;
  val instrBad        = InstrBad;
  

  
  datatype tests =
    Arb of condition (* Not EV/OD, TR/NEVER *)
  | Wrd of condition (* Not TR/NEVER *)
  | TagTest of bool (* true = Short, false = Long *)
  
  val testNeqW  = Wrd NE;
  val testEqW   = Wrd EQ;
  val testGeqW  = Wrd GE;
  val testGtW   = Wrd GT;
  val testLeqW  = Wrd LE;
  val testLtW   = Wrd LT;
  val testNeqA  = Arb NE;
  val testEqA   = Arb EQ;
  val testGeqA  = Arb GE;
  val testGtA   = Arb GT;
  val testLeqA  = Arb LE;
  val testLtA   = Arb LT;
  
  val Short     = TagTest true;
  val Long      = TagTest false;
  
(***************************************************************************  
  RR implementation of general operations
***************************************************************************)  

  (* Is there a general register/register operation? Some operations may not
     be implemented because this machine does not have a suitable instruction
     or simply because they have not yet been added to the code generator. It
     is possible for an instruction to be implemented as a register/immediate
     operation but not as a register/register operation (e.g. multiply) *) 
  fun instrIsRR (instr : instrs) : bool = 
    case instr of
      InstrMove       => true
    | InstrAddA       => true
    | InstrSubA       => true
    | InstrRevSubA    => true
    | InstrMulA       => false (* for now *)
    | InstrOrW        => true
    | InstrAndW       => true
    | InstrXorW       => true
    | InstrLoad       => allowMutableLoads
    | InstrLoadB      => allowMutableLoads
    | InstrVeclen     => false (* immediate form only *)
    | InstrUpshiftW   => false (* not very useful *)
    | InstrDownshiftW => false (* not very useful *)
    | InstrPush       => true
    | InstrBad        => false
    ;

  (* General register/register operation. *)
  fun genRR (instr : instrs, r1 : reg, r2 : reg, rd : reg, cvec : code) : unit =
  let
    val U : unit =
      (* 
         We shouldn't do arithmetic on the stack pointer,
         but we ought to check, just in case. 
       *)
      if rd regEq regStackPtr orelse 
         r1 regEq regStackPtr orelse 
         r2 regEq regStackPtr
      then genPendingStackAdjustment cvec
      else ();
  in
    case instr of
      InstrMove =>
        gen (cvec, COPY (r1, rd))
    
    | InstrAddA =>
      let
        val regTemp : reg = chooseTempReg cvec;
      in
        if r1 regEq r2
        then
      genBlock (cvec,
      [
        UNTAGCHECK (r1, regTemp),
        ADDO  (r1, regTemp, rd)
      ])
        else
      genBlock (cvec,
      [
        UNTAGCHECK (r2, regTemp),
        UNTAGCHECK (r1, regZero),
        ADDO  (r1, regTemp, rd)
      ])
      end
      
    | InstrSubA =>
        if r1 regEq r2
        then genList (cvec, loadImmed (tagged 0, rd))
    else let
      val regTemp : reg = chooseTempReg cvec;
    in
      genBlock (cvec,
      [
        UNTAGCHECK (r2, regTemp),
        ARBSUB    (r1, regTemp, rd)
      ])
    end
    
    | InstrRevSubA =>
        if r1 regEq r2
        then genList (cvec, loadImmed (tagged 0, rd))
    else let
      val regTemp : reg = chooseTempReg cvec;
    in
      genBlock (cvec,
      [
        UNTAGCHECK (r1, regTemp),
        ARBSUB    (r2, regTemp, rd)
      ])
    end
    
    | InstrOrW =>
        gen (cvec, OR (r1, r2, rd))
    
    | InstrAndW =>
        gen (cvec, AND (r1, r2, rd))
    
    | InstrXorW =>
        if r1 regEq r2
        then genList (cvec, loadImmed (tagged 0, rd))
    else let
          val regTemp : reg = chooseTempReg cvec;
    in
      genList (cvec,
      [
        XOR  (r1, r2, regTemp),
        LDO  (int14_1, regTemp, rd)
      ])
    end

    | InstrLoad =>
      let
        val regTemp : reg = chooseTempReg cvec;
      in
        genList (cvec,
        [
          ASHR   (nat5_TAGSHIFT, r2, regTemp),
          LDWXSH (regTemp, r1, rd)
        ])
      end

    | InstrLoadB =>
      let
        val regTemp : reg = chooseTempReg cvec;
      in
        genList (cvec,
        [
          ASHR    (nat5_TAGSHIFT, r2, regTemp),
          LDBXSH  (regTemp, r1, regTemp),
          SHL     (nat5_TAGSHIFT, regTemp, regTemp),
          LDO     (int14_1, regTemp, rd)
        ])
      end
        
    | InstrPush => (* Both rd and r2 are ignored. *)
        genPush (r1, cvec)
    
    | _ =>
      raise InternalError "genRR: Unimplemented instruction"
  end; (* genRR *)
  
(***************************************************************************  
  RI implementation of general operations
***************************************************************************)  
    
  (* Is this argument acceptable as an immediate
     or should it be loaded into a register? *) 
  fun instrIsRI (instr : instrs, cnstnt : word) : bool =
    isShort cnstnt andalso
    let
      val c : int = toInt (toShort cnstnt);
    in
      case instr of
    InstrMove       => true          (* is32Bit (tagged c) *)
      | InstrAddA       => isShort (~ c) (* is32Bit (semiTagged (~ c)); false for MININT *)
      | InstrSubA       => true          (* is32Bit (semiTagged c) *)
      | InstrRevSubA    => true          (* is32Bit (tagged c)     *)
      | InstrMulA       => (~2 <= c andalso c <= 2) orelse c = 4 (* for now *)
      | InstrOrW        => false (* for now *)
      | InstrAndW       => false (* for now *)
      | InstrXorW       => false (* for now *)
      | InstrLoad       => true
      | InstrLoadB      => true
      | InstrVeclen     => true
      | InstrUpshiftW   => 0 <= c andalso c < (32 - TAGSHIFT)
      | InstrDownshiftW => 0 <= c andalso c < (32 - TAGSHIFT)
      | InstrPush       => false 
      | InstrBad        => false
    end; (* instrIsRI *)  
  
  (* Register/immediate operations.  In many of these operations
     we have to tag the immediate value. *)
  fun genRI (instr : instrs, rs : reg, constnt : word, rd : reg, cvec) : unit =
  let
    (* The constant value will always be a short integer
       so we can convert it directly. *)
    val c : int = toInt (toShort constnt);

    val U : unit =
      (* 
         We shouldn't do arithmetic on the stack pointer,
         but we ought to check, just in case. 
       *)
      if rd regEq regStackPtr orelse 
         rs regEq regStackPtr
      then genPendingStackAdjustment cvec
      else ();
  in
    case instr of
      InstrMove =>
        (* Load a constant into a register. rs is ignored. *)
        genList (cvec, loadImmed (tagged c, rd))

    | InstrAddA => (* Arbitrary precision addition. *)
      let
         val c' : int = semiTagged c;
      in
        if is11Bit c'
        then genBlock (cvec, [ARBADDI (int11 c', rs, rd)])
        else let
          val regTemp : reg = chooseTempReg cvec;
          val U : unit = genList(cvec, loadImmed (semiTagged (~ c), regTemp));
        in
          genBlock (cvec, [ARBSUB  (rs, regTemp, rd)])
        end
      end
      
    | InstrSubA => (* Arbitrary precision subtraction. *)
      let
         val c' : int = semiTagged (~ c);
      in
        if is11Bit c'
        then genBlock (cvec, [ARBADDI (int11 c', rs, rd)])
        else let
          val regTemp : reg = chooseTempReg cvec;
          val U : unit = genList(cvec, loadImmed (semiTagged c, regTemp));
        in
          genBlock (cvec, [ARBSUB  (rs, regTemp, rd)])
        end
      end
      
    | InstrRevSubA => (* Arbitrary precision reverse subtraction. *)
      (* Must be very careful so that this works even if rs contains MININT, or if rs=rd *)
      let
        val regTemp : reg = chooseTempReg cvec;
        val U : unit = genList(cvec, LDO (int14_minus1, rs, regTemp) :: loadImmed (tagged c, rd));
      in
        genBlock (cvec, [ARBSUB (rd, regTemp, rd)])
      end
    
    | InstrMulA =>
      let
      in
        case c of 
          ~2 => (genRI (InstrRevSubA, rs, toWord 0, rd, cvec); genRR (InstrAddA, rd, rd, rd, cvec))
        | ~1 => genRI (InstrRevSubA, rs, toWord 0, rd, cvec)
        |  0 => genRR (InstrMove, regZero, regZero, rd, cvec)
        |  1 => genRR (InstrMove, rs, regZero, rd, cvec)
        |  2 => genRR (InstrAddA, rs, rs, rd, cvec)
        |  4 => (genRR (InstrAddA, rs, rs, rd, cvec); genRR (InstrAddA, rd, rd, rd, cvec))
        |  _ =>
             raise InternalError ("genRI: MulA - bad value " ^ Int.toString c)
      end
        
     (* It might be worth implementing these if the bits form a contiguous field *)
    | InstrOrW =>
        raise InternalError "genRI: InstrOrW is not implemented"

    | InstrAndW =>
        raise InternalError "genRI: InstrAndW is not implemented"
      
    | InstrXorW =>
        raise InternalError "genRI: InstrXorW is not implemented"
    
    | InstrLoad => (* offset is in words *)
        genLoad (wordSize * c, rs, rd, cvec)
        
    | InstrLoadB => (* offset is in bytes *)
        if is14Bit c
        then let
          val regTemp : reg = chooseTempReg cvec;
        in
          genList (cvec,
          [
            LDB  (int14 c, rs, regTemp),
            SHL  (nat5_TAGSHIFT, regTemp, regTemp),
            LDO  (int14_1, regTemp, rd)
          ])
        end
           
        else let
          val (byteOffset_left : int21, byteOffset_right : int14) =
            splitLeftRight c;
          val regTemp : reg = chooseTempReg cvec;
        in
          genList (cvec,
        [
          ADDIL (byteOffset_left, rs),
          LDB   (byteOffset_right, regUnsafe, regTemp),
          SHL   (nat5_TAGSHIFT, regTemp, regTemp),
          LDO   (int14_1, regTemp, rd)
        ])
        end

    | InstrVeclen =>
      let
        val regTemp : reg = chooseTempReg cvec;
      in
    genList (cvec,
    [
      LDW  (int14_minus4, rs, regTemp),
      SHL  (nat5_FLAGLENGTH, regTemp, regTemp),
      LSHR (nat5_FLminusTS, regTemp, regTemp),
      LDO  (int14_1, regTemp, rd)
    ])
      end

    | InstrUpshiftW =>   (* logical shift left *)
      let
        val regTemp : reg = chooseTempReg cvec;
      in
    genList (cvec,
    [
      LSHR (nat5_TAGSHIFT, rs, regTemp),
      SHL  (nat5 (c + TAGSHIFT), regTemp, regTemp),
      LDO  (int14_1, regTemp, rd)
    ])
      end

    | InstrDownshiftW =>  (* logical shift right *)
      let
        val regTemp : reg = chooseTempReg cvec;
      in
    genList (cvec,
    [
      LSHR (nat5 (c + TAGSHIFT), rs, regTemp),
      SHL  (nat5_TAGSHIFT, regTemp, regTemp),
      LDO  (int14_1, regTemp, rd)
    ])
      end
    
    | InstrPush =>
        raise InternalError "genRI: Unimplemented instruction (InstrPush)"
    
    | InstrBad =>
        raise InternalError "genRI: Unimplemented instruction (InstrBad)"
  end; (* genRI *)
  
  
(***************************************************************************  
  RI implementation of comparisons
***************************************************************************)  

  (* Short/Long are not implemented *)
  fun isCompRR tc =
    case tc of
      TagTest _ => false
    | _         => true;

  (* Is this argument acceptable as an immediate or should it be *)
  (* loaded into a register? *) 
  fun isCompRI (tc, cnstnt:word) : bool =
    isShort cnstnt;

  (* If this instruction is unreachable, we want to avoid generating a label
     (which would get fixed up, possibly making some other code spuriously
     reachable), which is why we have the "unreachable" test here. Without
     this test, we would correctly generate no code here but would generate
     code at the target of the non-generated branch, which would be silly.
  *)
  fun compareAndBranchRR (r1 : reg, r2 : reg, test : tests, cvec : code) : labels =
    if unreachable cvec then []
    else case test of
      Wrd cond =>
      let
        val lab : label = Label (ref None);
        val U : unit =
      genBlock (cvec, [COMB (cond, true, r1, r2, lab)])
      in
        [lab]
      end
      
    | Arb cond =>
      let
        val lab : label = Label (ref None);
        val U : unit = genBlock (cvec, [ARBCOMB (cond, r1, r2, lab)]);
      in
        [lab]
      end
      
    | _ =>
       raise InternalError "compareAndBranchRR: Unimplemented test";

   (* GCODE replaces Arb EQ/NE with Word EQ/NE for short constants, so 
      there's no point generating clever code for Arb EQ/NE here.
      SPF 18/2/97
      Oh. Apparently it doesn't actually do this, so let's do it here.
      SPF 21/2/97
   *)
  fun compareAndBranchRI (r : reg, cnstnt : word, test : tests, cvec : code) : labels =
    if unreachable cvec then []
    else case test of 
      Wrd cond =>
      let
        val c : int = tagged (toInt (toShort cnstnt));
        val lab : label = Label (ref None);
        val U : unit =
      if is5Bit c
      then
        genBlock (cvec, [COMIB (swapArgs cond, true, int5 c, r, lab)])
        
      else let
        (* It's safe to load a tagged SHORT into an untagged register *)
        val regTemp : reg = chooseTempReg cvec;
        val U : unit = genList (cvec, loadImmed (c, regTemp));
      in
        genBlock (cvec, [COMB (cond, true, r, regTemp, lab)])
      end
      in
        [lab]
      end
      
    | Arb EQ => compareAndBranchRI (r, cnstnt, Wrd EQ, cvec)
    | Arb NE => compareAndBranchRI (r, cnstnt, Wrd NE, cvec)
    | Arb cond =>
      let
        val c : int = tagged (toInt (toShort cnstnt));
        val lab : label = Label (ref None);
        val U : unit =
      if is5Bit c
      then genBlock (cvec, [ARBCOMIB (swapArgs cond, int5 c, r,  lab)])
        
      else let
        (* It's safe to load a tagged SHORT into an untagged register *)
        val regTemp : reg = chooseTempReg cvec;
        val U : unit = genList (cvec, loadImmed (c, regTemp));
      in
        genBlock (cvec, [ARBCOMBAUX (swapArgs cond, regTemp, r, lab)])
      end
      in
        [lab]
      end
      

    | TagTest b => (* The immediate is ignored *)
      let
        val lab : label = Label (ref None);
        val U : unit =
       genBlock (cvec, [COMB (if b then OD else EV, true, r, regZero, lab)])
     in
       [lab]
     end; (* compareAndBranchRI *)
    
  (* set the num'th constant in seg to be value *)
  fun setConst (seg : cseg, constStartAddr : addrs, constNum : int, value : word) : unit =
  let
    val constAddr : addrs = constStartAddr wordAddrPlus constNum;
  in
    csegPutWord (seg, getWordAddr constAddr, value)
  end

  (* Fix up references from other vectors to this one. *)
  fun fixOtherRefs (refTo : code, value : word) : unit =
  let
    fun fixRef refFrom =
      case ! (resultSeg refFrom) of
        Unset =>
    let (* The "refFrom" code hasn't allocated its code segment yet. *)
      fun replaceNonLocalConst (constEntry as (num : int, const : const)) : int * const =
        case const of
          CVal c => if c is refTo then (num, WVal value) else constEntry
        | _      => constEntry;
    in
      constVec refFrom := map replaceNonLocalConst (! (constVec refFrom))
    end
      
      | Set (seg : cseg, constStartAddr : addrs) =>
    let (* The "refFrom" code has already allocated its code segment. *)
      val noc = numOfConsts refFrom;
      
      fun putNonLocalConst (num : int, const : const) : unit =
        case const of
          CVal c =>
        if c is refTo
        then let (* A reference to this one. *)
          (* Fix up the forward reference. *)
          val U : unit = setConst (seg, constStartAddr, num, value);
        in
          (* decrement the "pending references" count *)
          noc := !noc - 1
        end
        else ()
        | _ => ();
    
      (* look down its list of forward references until we find ourselves. *)
      val U : unit =
        applyList (putNonLocalConst, !(constVec refFrom))
    in
     (* If there are no more references, we can lock it. *)
      if !noc = 0 then csegLock seg else ()
    end (* fixRef *);
  in
    (* For each `code' which needs a forward reference
       to `refTo' fixing up. *)
    applyList (fixRef, !(otherCodes refTo))
  end; (* fixOtherRefs *)


  (* The stack limit register is set at least twice this far from the end
     of the stack so we can simply compare the stack pointer with the stack
     limit register if we need less than this much. Setting it at twice
     this value means that procedures which use up to this much stack and
     do not call any other procedures do not need to check the stack at all. *)
  val minStackCheck = 20; 
  
  (* Create the actual code segment. Fortunately, GCODE passes us the maximum
     stack usage (as calculated by TRANSTAB) so we don't need to recalculate
     it here.
  *)
  fun copyCode (cvec : code, stackRequired : int) : address =
  let
    (* Optimise the instructions for the body of the function *)
    val (bodyCode : instruction list, bodySize : int) = revExpand (! (codeVec cvec));
    val U : unit = codeVec cvec := []; (* To reduce space requirements *)

    val numOfConst = !(numOfConsts cvec);
    val callsAProc = !(mustCheckStack cvec);

    (* Generate the prelude (iterative!) *)
    local 
      val stackCheckCode : instruction list =
        if stackRequired >= minStackCheck
        then let
          val regTemp : reg = chooseTempReg cvec;
        in
          addImmed (~ (stackRequired * wordSize), regStackPtr, regTemp) @
          [STACKCHECK regTemp]
        end
        (* If we call another function, we have to perform the following
           stack check to ensure that there are at least two minimal
           (minStackCheck words each) stack frames available - one for us,
           one for the (possibly leaf) function that we're calling.
           We don't need to do this if we make only tail calls (our
           caller has reserved a stack frame for us, which we share
           with our callee), but we'll normally perform the stack
           check anyway, to check for user interrupt. SPF 19/2/97
        *)
        else if callsAProc
        then
          [STACKCHECK regStackPtr]
           
        (* Leaf or tail-call-only function - no stack check required. *)
        else 
      []; 
    in
      (* code segment size minimised (iteratively!) SPF 12/8/94 *)
      fun getPreludeCode (preludeSize : int) : int * int * instruction list * int * addrs =
      let
        val codeSize : int = preludeSize + bodySize;
      
        (* +4 for zero word, code size, profile count and constants count *)
        val segSize = codeSize + numOfConst + 4;
        
        (* This is the address of the end-of-code marker *)
        val lastAddr : addrs = mkWordAddr codeSize;
       
        (* This is the address of the zero'th constant
          +3 for zero word, code size, profile count *)
        val constStartAddr : addrs = lastAddr wordAddrPlus 3;
      
       (* What's going on here?
         
      We want to be able to address the constants using regCode.
      If we have a short segment, that's no problem. However,
      if we have a long code segment, the 14-bit offset in a
      LDW instruction isn't large enough to reach the constants section
      unless we adjust regCode first. If we need to do this,
      regCode will be pointing into the middle of the code
      segment, so we must tag it as a code pointer, and ensure
      that it address real code (before the zero word) rather
      than data.
       
      Subtracting 16 would skip the 3 "overhead" words:
          end-of-code marker word
          byte offset of start of code segment
          profiling word
       and make regCode point at the last real code word. We then
       subtract another 2 to get the tagging right. (We could add 2
       instead, but I'm happier doing the subtraction, in case I've
       got an off-by-one somewhere.) 
       SPF 21/2/97.
       *)
    
       val (constByteOffset : int, loadConstSegCode : instruction list) =
         if is14Bit (segSize * wordSize)
         then (getByteAddr constStartAddr, [])
         else let
           val offsetFromCodePtr = 18;

           val offsetOfConsts : int =
             getByteAddr constStartAddr - offsetFromCodePtr;
         in
           (offsetFromCodePtr, addImmed (offsetOfConsts, regCode, regCode))
         end;
   
        val preludeCode : instruction list = 
           (* L1,L2 here *) loadConstSegCode @
           (* L3,L4 here *) stackCheckCode;

        (* Branch addresses are relative to the END of the code. SPF 7/3/97 *)
        val segStartAddr : addrs = mkWordAddr (~ codeSize);

        val selfCallAddr : addrs =
          segStartAddr wordAddrPlus (instrListLength loadConstSegCode);
      in
        (* does it fit? *)
        if instrListLength preludeCode = preludeSize
        then (codeSize, segSize, preludeCode, constByteOffset, selfCallAddr)
        else getPreludeCode (preludeSize + 1)
      end
   
      val (codeSize, segSize, preludeCode, constByteOffset, selfCallAddr) =
        (* iterate to find size of loadConstSegCode *)
        getPreludeCode (instrListLength stackCheckCode); 
     end; (* local *)
    
    (* Get the intermediate code *)
    val instrList : instruction list = preludeCode @ bodyCode;
    
    (* Display the intermediate code *)
    val U : unit =
      if !assemblyCode
      then  
        case procName cvec of
           ""   => printString "<anonymous>:\n"
         | name => printString (name ^ ":\n")
      else ();
      
    (* Now make the byte segment that we'll turn into the code segment *)
    val seg : cseg = csegMake segSize;

    (* Perform the low-level code-generation into the byte segment,
       printing the code as we do so. Note that code labels are now
       calculated relative to the END of the code, which explains the
       following.
    *)
    
    val segStartAddr : addrs = mkWordAddr (~ codeSize);
    val U : unit = genQuadsToSeg (seg, int14 constByteOffset, segStartAddr, selfCallAddr, instrList);

    (* This is the address of the zero word, relative to the start of the code segment *)
    val lastAddr : addrs = mkWordAddr codeSize;

    (* This is the address of the zero'th constant (the name string)
       +3 for zero word, code size, profile count *)
    val constStartAddr : addrs = lastAddr wordAddrPlus 3;
    
    (* Print the constants *)
    val U : unit =
      if !assemblyCode
      then let
        val U : unit = printConstVec (! (constVec cvec), segStartAddr, constStartAddr)
      in
        printString "\n"
      end
      else ();

    (* Zero end-of-code marker *)
    local
      val addr : addrs = lastAddr;
      val quad : quad  = zeroQuad;
    in
      val U : unit = setQuad (quad, addr, seg);
    end;
 
    (* Byte offset of start of code. *)
    local
      val addr : addrs = lastAddr wordAddrPlus 1;
      val quad : quad  = toQuad (getByteAddr addr);
    in
      val U : unit = setQuad (quad, addr, seg);
    end;
    
    (* Next the profile count. *)
    local
      val addr : addrs = lastAddr wordAddrPlus 2;
      val quad : quad  = toQuad 0;
    in
      val U : unit = setQuad (quad, addr, seg);
    end;
    
    (* Put in the number of constants (including the name string). *)
    local
      val addr : addrs = constStartAddr wordAddrPlus numOfConst;
      val quad : quad = toQuad numOfConst;
    in
      val U : unit = setQuad (quad, addr, seg);
    end;
    
    (* Now we've filled in all the C integers; now we need to convert the segment
       into a proper code segment before it's safe to put in any ML values.
       SPF 13/2/97
    *)
    val U : unit = csegConvertToCode seg;
    val U : unit = resultSeg cvec := Set (seg, constStartAddr);

    (* Copy the objects (including the name string) from the constant list. *)
    fun putLocalConst (num : int, const : const) : unit =
      case const of
    WVal w => (* an ordinary (non-short) constant *)
    let
      val U : unit = setConst (seg, constStartAddr, num, w);
    in
      numOfConsts cvec := ! (numOfConsts cvec) - 1
    end
    
      (* a handler *)
      | HVal (Label (ref None)) =>
          raise InternalError "putLocalConst: handler not fixed-up"
      
      | HVal (Label (ref (Some targetAddr))) =>
    let 
      (* The following comment applies to offsetAddr *)
      (* Special function to add to an address.
         This only works if the resulting value is 
         in a code segment and is on a word  + 2 byte boundary. *)
      val handlerByteOffset : int = targetAddr byteAddrMinus segStartAddr;
      val handlerAddr : handler = 
        offsetAddr (csegAddr seg, toShort (handlerByteOffset + 2));
      val U :unit = setConst (seg, constStartAddr, num, toWord handlerAddr);
    in
      numOfConsts cvec := ! (numOfConsts cvec) - 1
    end
      
      (* forward-reference - fix up later when we compile
     the referenced code *) 
      | CVal _ => ();

    val U : unit = applyList (putLocalConst, (! (constVec cvec)));
  
    (* Switch off "mutable" bit now if we have no
       forward or recursive references to fix-up *)
    val U : unit = 
      if !(numOfConsts cvec) = 0
      then csegLock seg
      else ();

    (* Do we need to make a closure, or just return the code? *)
    val addr : address =
      if noClosure cvec
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
  in
    addr 
  end (* copyCode *);

  (* Only used for while-loops. *)
  fun jumpback (lab : label, stackCheck : bool, cvec : code) : unit =
  let
    (* Put in a stack check. This is used to allow
       the code to be interrupted. *)
    val U : unit =
      if stackCheck
      then genBlock (cvec, [STACKCHECK regStackPtr])
      else ();
    (* N.B. we don't do branch-chaining here, as otherwise we might
       end up with conditional backward jumps, which we can't nullify properly.
       SPF 27/2/97
    *)
    (* We need genBlock here to force any pending stack adjustment to be taken. *)
    val U : unit = genBlock (cvec, [BRANCH (true, lab)])
  in
    exited cvec := true
  end;

  (* ic function exported to gencode. Currently only used for backward jumps. *)
  (* We use a label (not an addrs), so this doesn't interfere with jump 
     chaining and optimisation.
     SPF 28/2/97
  *)
  val ic : code -> label = 
    fn (cvec : code) => 
    let
      val lab : label = Label (ref None);
      val U : unit = fixupSpecialLabel (lab, cvec);
    in
      lab
    end;
    
  (* The interface to GCODE isn't abstract enough; it thinks
     ic and jumpback manipulate the raw "addrs" type, (which
     is true in older versions of the code-generator). So ...
  *)
  type addrs = label;

  (* We need these types although we don't generate indexed cases. *)
  type cases = int * addrs; (* should tag be a short??? *)

  fun constrCases (p as (i,a)) = p;

(* Dummy implementation ... 
  type jumpTableAddrs = unit;

  fun useIndexedCase (min, max, numberOfCases, exhaustive) : bool =
    false; (* Never use indexed case. *)

  fun indexedCase (reg, reg2, min, max, exhaustive, cvec) : jumpTableAddrs =
    raise InternalError "Not implemented: indexedCase";

  fun makeJumpTable (startTab, cl, default, min, max, cvec) : unit =
    raise InternalError "Not implemented: makeJumpTable";
... *)

  type jumpTableAddrs = int * int * label * (label list ref);

  (* Used in quickSort *)
  fun ltCases ((i1, a1) : cases) ((i2, a2) : cases) = i1 < i2;
  
  fun useIndexedCase (min : int, max : int, numberOfCases : int, exhaustive : bool) : bool =
    isShort min andalso
    isShort max andalso
    isShort (~min + jumpTableOffset) andalso
    isShort (max - min + jumpTableOffset) andalso
    numberOfCases > 7 andalso
    numberOfCases >= (max - min) div 3;

  fun indexedCase (valReg : reg, constReg : reg, min : int, max : int,
                   exhaustive : bool, cvec : code) : jumpTableAddrs =
  let 
    (* Problem: we need multiple copies of defaultLabel, because the
       optimiser assumes that each label is used for only one jump.
       Solution: treat this label very carefully when we fix it up so
       that it doesn't get optimised away.
    *)
    val defaultLabel : label = Label (ref None);
    val entryList : label list ref = ref []; (* To be completed by makeJumpTable *)
    val jumptable : jumpTableAddrs = (min, max, defaultLabel, entryList)
  in
    if unreachable cvec then jumptable
    else let (* we need to generate code *)
      val taggedMin : int = tagged min;
      val taggedMax : int = tagged max;
  
      val rangeCheck : unit =
    if exhaustive then ()
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
      val U : unit = genBlock (cvec, [COMB (EV, true, valReg, regZero, defaultLabel)]);
     
      (* Is it less than the minimum? If so, jump to the default. *)
      val U : unit =
        if is5Bit taggedMin
        then
          genBlock (cvec, [COMIB (GT, true, int5 taggedMin, valReg, defaultLabel)])
        else let
          val regTemp : reg = chooseTempReg cvec;
          val U : unit = genList (cvec, loadImmed (taggedMin, regTemp));
        in
          genBlock (cvec, [COMB (LT, true, valReg, regTemp, defaultLabel)])
        end;
     
      (* Is it greater than the maximum? If so, jump to the default. *)
      val U : unit =
        if is5Bit taggedMax
        then
          genBlock (cvec, [COMIB (LT, true, int5 taggedMax, valReg, defaultLabel)])
        else let
          val regTemp : reg = chooseTempReg cvec;
          val U : unit = genList (cvec, loadImmed (taggedMax, regTemp));
        in
          genBlock (cvec, [COMB (GT, true, valReg, regTemp, defaultLabel)])
        end;
    in
      ()
    end;
    
      val indexReg : reg = chooseTempReg cvec;
      val baseReg  : reg = chooseTempReg cvec;
      
      val U : unit =
    if indexReg regEq baseReg
    then raise InternalError "indexedCase: run out of temporary registers"
    else ();
    
      val entryCount : int = max - min + 1;


      (* We have 3 overhead instructions between the table base and the
         actual JUMPTABLE instruction. This means that we want to map
            min     => 4
            min + 1 => 5
            ....
            max     => max - min + 4
            
         We do this by subtracting (min - 4) from the value. Since
         everything is tagged, we have to tag this value first.
         Since we don't have a subtract immediate instruction, we add
         the negation of this number.
      *)
      val offset : int = min - jumpTableOffset;

      val U : unit =
    genBlock (cvec, 
      addImmed (~ (tagged offset), valReg, indexReg) @
      (* indexReg now contains a value in the range 4 .. max - min + 4, but shifted *)
      [
        LOADCODEADDR baseReg,
        ASHR (nat5_TAGSHIFT, indexReg, indexReg), (* in DELAY slot *)
        (* indexReg now contains an untagged value in the range 3 .. max - min + 3;
           baseReg now contains the address of the base of the table, which is HERE;
           except that its lower 2 bits contain unknown values, so we have
           to mask these out. Were using a temporary register for "baseReg", so
           it doesn't matter that this isn't a legal code-pointer.
           SPF 11/12/1997
        *)
            TAGCODEREG (int5_0, baseReg),
            LDWXSH (indexReg, baseReg, indexReg),
        ADDL   (indexReg, baseReg, baseReg),
        JUMPREG (true, baseReg),
        
        JUMPTABLE (entryCount, entryList)
      ]);
  
      (* We don't fall through the JUMPTABLE instruction *)
      val U : unit = exited cvec := true;
      
      (* The default cases come here *)
      val U : unit = 
    if exhaustive then () else fixupSpecialLabel (defaultLabel, cvec);
    in
      jumptable
    end (* we need to generate code *)
  end; (* indexedCase *)

  fun makeJumpTable (jumptab : jumpTableAddrs, caseList : cases list, default : label,
                     min : int, max : int, cvec : code) : unit =
  let
    val (oldmin : int, oldmax : int, defaultLabel : label, entryList : label list ref)
      = jumptab;
    
    val U : unit = 
      if (min = oldmin andalso max = oldmax) then ()
      else raise InternalError "makeJumpTable: jump table has inconsistent range information"
      
    val U : unit = 
      case !entryList of
        [] => ()
      | _  => raise InternalError "makeJumpTable: jump table already created"
  
    val sortedList = quickSort ltCases caseList;
   
    (* We create the label list in reverse order (highest index first) *)
    fun createLabList (i, cl, labList) =
      if max < i then labList
      else
    case cl of
      [] => createLabList (i + 1, cl, defaultLabel :: labList)
     
    | ((j,caseLabel) :: t) =>
        if i < j 
        then createLabList (i + 1, cl, defaultLabel :: labList)
        else createLabList (i + 1, t,  caseLabel :: labList)
  in
    entryList := createLabList (min, sortedList, [])
  end;
end (* CODECONS functor body *)

end (* structure-level let *)
