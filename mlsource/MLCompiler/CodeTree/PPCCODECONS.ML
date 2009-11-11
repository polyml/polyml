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

r0 scratch register (unsaved?)
r1 - don't touch - dedicated C register (stack - like SPARC %o6) 
r2 - don't touch - dedicated C register (TOC)
r3      used for the first argument to a function, and for the result.
r4-r6  used for the next 3 args, any others being passed on the stack.
 
r24        is the closure pointer or static link pointer (like SPARC %o5)
r25 (rr)   is used as the compiler-visible link register (like SPARC %o7)
r26        is an unsaved temporary
r27 (rsp)  is the ML stack pointer,
r28 (rsl)  is the stack limit,
r29 (rhp)  is the heap pointer,
r30 (rhl)  is the heap limit,
r31 (rhr)  points to the top exception handler.
r13 points to the "MemRegisters" structure.

r7-r10 and r14-r22 (15 registers) are available as general work registers,
as are r3-r6 and r23-r25, when they are not fulfilling their specialised
duties. That's a total of 21 general-purpose registers (as opposed to
17 on the SPARC).

r11, r12 are used as code-generator visible untagged registers.
r26 is used as a compiler-invisible RTS scratch register for
handling traps.

LR is a semi-volatile register used to cache the return address.
Executing a trap can copy rr into LR, even if it wasn't
there before. Furthermore, it can change the tagged status of
LR - it may be tagged after the trap, even if it was untagged
before. What we do guarantee is that if LR cached rr before
the trap, then it caches it afterwards (modulo possible tag bit
discrepancies). Executing any function call (including an RTS
call) invalidates both rr and LR.

CTR is a volatile register - we use it to to return from traps
and normal RTS calls, and it is also used to implement tail-calls.

Note: the RS/6000 follows a callee-saves convention for r13-r31
inclusive, so we'll have to be careful to save these registers
when we first enter ML. We can remove this later if it appears
to be unnecessary.

Function Prologues
------------------
Functions now have up to 3 entry points:
  (1) L1/L2 Standard entry point - return address in LR
  (2) Self-call entry point - doesn't change regCode
  (3) Self-tail-call entry point - doesn't change regReturn or regCode

At all entry points, LR must contain the return address.
At entry point L4, regReturn must contain it too.

L1, L2: (* RetAddrInLR *)
        This previously set regCode to point to the constants area
L3:     (* RetAddrInLR *)
        mflr  regReturn
        ori   regReturn,regReturn,2
L4:     (* Cached *)
        <stack-check code>

The code would be slightly longer if the argument to the addi won't
fit into 16 bits - in that case we need to use a temporary register
to construct the offset, which costs us an extra pair of
instructions.
   
N.B. We must use "ori", not "addi" to adjust regReturn. This is
because we don't know whether or not the value in LR is already tagged.
It will be untagged if it was put there by a "blrl" (or similar)
call instruction, but it will be already tagged if it was put there
by a "mtlr returnReg" tail-call instruction.

Calling a Function
------------------
We cache "LR contains a valid return address".
We also cache "regReturn contains a valid return address".

Call:
    mtlr   regCode
    blrl
        (* Clear LR/RR caches *)
  
Self-call:
    bl     L3
        (* Clear LR/RR caches *)

Tail-call:
    mtctr  regCode
    (* Only if LR doesn't cache the return address *)
    mtlr   returnReg
    bctr
    (* NOTREACHED *)

Self-tail-call:
    (* Only if returnReg <> regReturn *)
    mr     regReturn, returnReg
    (* Only if LR doesn't cache the return address *)
    mtlr   returnReg
    b     L4
    (* NOTREACHED *)

Return:
    (* Only if LR doesn't cache the return address *)
    mtlr   returnReg
    blr
    (* NOTREACHED *)

Write-to-regReturn:
        (* Clear RR cache *)

On-branching:
    (* Clear LR/RR cache if any branch is uncached *)

On-trap:
        (* Clear LR cache state, unless RR cache is set *)

Note: we *don't* have to clear the cache for the stack-check trap
in the prelude because the RTS trap-handler specifically copies
regReturn into LR, which (at that point) is the correct action
i.e. the both LR and RR caches are (implicitly) set at that point.

Note: it's not completely clear that this caching version is a
win (the cost is the extra "mtlr returnReg" instruction in
an uncached (self-)tail-call that's needed to make the initial
state "Cached".) It seems to give a slight overall speed-up though.

*)

functor PPCCODECONS (

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
  val regCode:     reg option;
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
  val instrIsRR: instrs -> bool;         (* Is the general form implemented? *)
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
  val genStackOffset:     reg * int * code -> unit;

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


let

(*****************************************************************************)
(*                  ADDRESS                                                  *)
(*****************************************************************************)
structure ADDRESS :
sig
  type machineWord;    (* NB *not* eqtype, 'cos it might be a closure *)
  type short = Word.word;
  type address;
  type handler;
  val toAddress: 'a -> address

  val wordSize : int; (* still 4, but will change one day *)

  val wordEq : 'a * 'a -> bool
  
  val isShort:  'a     -> bool;
  val toShort:  'a     -> short;
  val toMachineWord:   'a     -> machineWord;

  val loadByte:  address * short -> Word8.word 
  val loadWord:  address * short -> machineWord
  val unsafeCast: 'a -> 'b
  
  val offsetAddr : address * short -> handler
 
  val alloc:  (short * Word8.word * machineWord) -> address
  val length: address -> short
  val flags:  address -> Word8.word

  val F_words : Word8.word
 
  val isWords : address -> bool;
  val isBytes : address -> bool;
  val isCode  : address -> bool;

  val lock : address -> unit;
end = Address;

(*****************************************************************************)
(*                  CODESEG                                                  *)
(*****************************************************************************)
structure CODESEG :
sig
  type machineWord;
  type short = Word.word;
  type address;
  type cseg;
  
  val csegMake:          int  -> cseg;
  val csegConvertToCode: cseg -> unit;
  val csegLock:          cseg -> unit;
  val csegGet:           cseg * int -> Word8.word;
  val csegSet:           cseg * int * Word8.word -> unit;
  val csegPutWord:       cseg * int * machineWord -> unit;
  val csegCopySeg:       cseg * cseg * int * int -> unit;
  val csegAddr:          cseg -> address;
  val csegPutConstant:   cseg * int * machineWord * 'a -> unit;
end = CodeSeg;

in

(*****************************************************************************)
(*                  CODECONS functor body                                    *)
(*****************************************************************************)
struct
  open CODESEG;
  open DEBUG;
  open ADDRESS;
  open Misc;

  val toInt = Word.toIntX (* This previously just cast the value so continue to treat it as signed. *)
  
  fun applyCountList (_, _, [])   = ()
    | applyCountList (f, n, h::t) = 
    let
      val () = f (n, h);
    in
      applyCountList (f, n + 1, t)
    end;

(*****************************************************************************)
(*                  Useful constants                                         *)
(*****************************************************************************)

  (* These are defined here as explicit constants, so the     *)
  (* code-generator can in-line them as immediates (I think). *)
  val TAGBITS = 1; (* Now 1 DCJM 13/11/00. *)

  val exp2_1  =          2;
  val exp2_2  =          4;
  val exp2_3  =          8;
  val exp2_4  =         16;
  val exp2_5  =         32;
  val exp2_6  =         64;
  val exp2_7  =        128;
  val exp2_8  =        256;
  val exp2_9  =        512;
  val exp2_10 =       1024;
  val exp2_11 =       2048;
  val exp2_12 =       4096;
  val exp2_13 =       8192;
  val exp2_14 =      16384;
  val exp2_15 =      32768;
  val exp2_16 =      65536;
  val exp2_19 =     524288;
  val exp2_21 =    2097152;
  val exp2_22 =    4194304;
  val exp2_23 =    8388608;
  val exp2_24 =   16777216;
  val exp2_25 =   33554432;
  val exp2_26 =   67108864;
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
        exp2_1  = exp2 1  andalso
        exp2_2  = exp2 2  andalso
        exp2_3  = exp2 3  andalso
        exp2_4  = exp2 4  andalso
        exp2_5  = exp2 5  andalso
        exp2_6  = exp2 6  andalso
        exp2_7  = exp2 7  andalso
        exp2_8  = exp2 8  andalso
        exp2_9  = exp2 9  andalso
        exp2_10 = exp2 10 andalso
        exp2_11 = exp2 11 andalso
        exp2_12 = exp2 12 andalso
        exp2_13 = exp2 13 andalso
        exp2_14 = exp2 14 andalso
        exp2_15 = exp2 15 andalso
        exp2_16 = exp2 16 andalso
        exp2_19 = exp2 19 andalso
        exp2_21 = exp2 21 andalso
        exp2_22 = exp2 22 andalso
        exp2_23 = exp2 23 andalso
        exp2_24 = exp2 24 andalso
        exp2_25 = exp2 25 andalso
        exp2_26 = exp2 26 andalso
        exp2_29 = exp2 29 andalso
        exp2_30 = exp2 30 andalso
        exp2_31 = exp2 31 andalso
        exp2_32 = exp2 32 
      )
         orelse raise InternalError "CodeCons: Powers of 2 incorrectly specified";
  end;
  
  
  val short0    = toShort 0;
  val short1    = toShort 1;
  val short2    = toShort 2;
  val short3    = toShort 3;
  val short5    = toShort 5;
  val short6    = toShort 6;
  val short7    = toShort 7;
  val short8    = toShort 8;
  
  val short10   = toShort 10;
  val short11   = toShort 11;
  val short14   = toShort 14;
  val short15   = toShort 15;
  val short16   = toShort 16;
  val short18   = toShort 18;
  val short19   = toShort 19;
  val short21   = toShort 21;
  val short22   = toShort 22;
  val short24   = toShort 24;
  val short25   = toShort 25;
  val short26   = toShort 26;
  val short27   = toShort 27;
  val short28   = toShort 28;
  val short29   = toShort 29;
  val short31   = toShort 31;
  val short32   = toShort 32;
  val short34   = toShort 34;
  val short36   = toShort 36;
  val short37   = toShort 37;
  val short38   = toShort 38;
  val short63   = toShort 63;
  val short127  = toShort 127;
  val short255  = toShort 255;
  
  val mask2Bits = short3;   (* least significant 2 bits *)
  val mask3Bits = short7;   (* least significant 3 bits *)
  val mask6Bits = short63;  (* least significant 6 bits *)
  val mask7Bits = short127; (* least significant 7 bits *)
  val mask8Bits = short255; (* least significant 8 bits *)
  val >> = Word.>> and << = Word.<<
  infix >> <<

(*****************************************************************************)
(*                  16-bit immediates                                        *)
(*****************************************************************************)

  (* 
     Most Power immediates are 16 bits. Whether this is interpretted
     as a signed or unsigned number depends on the instruction.
     We'll store everything as unsigned integers because this
     makes the code generation slightly simpler, but we'll
     provide interfaces for both signed and unsigned numbers.
  *)
  abstype int16 = Imm16 of int
  with
    fun isZero16 (Imm16 0) = true
      | isZero16 (Imm16 _) = false;
  
    fun getInt16 (Imm16 i) = i;
    
    (* is16Bit is the test for signed immediates *) 
    fun is16Bit i = ~ exp2_15 <= i andalso i < exp2_15;

    (* isUnsigned16Bit is the test for unsigned immediates *) 
    fun isUnsigned16Bit i = 0 <= i andalso i < exp2_16;
    
    (* tag a short constant *)
    fun semiTagged c = exp2_1 * c;
    fun tagged c = exp2_1 * c + 1;
  
    (* isTaggable16Bit is the test whether the immediate
       will fit into 16 bits when tagged *)
    fun isTaggable16Bit i = ~ exp2_14 <= i andalso i < exp2_14;

    fun isTaggable16BitUnsigned i = 0 <= i andalso i < exp2_15;

    fun int16 i =
      if is16Bit i
      then 
        if i < 0 
        then Imm16 (exp2_16 + i)
        else Imm16 i
      else let
        val msg = 
          concat
           [
             "int16: can't convert ",
             Int.toString i,
             " into a 16-bit signed immediate"
           ]
      in
        raise InternalError msg
      end;
      
    fun unsignedInt16 i =
      if isUnsigned16Bit i
      then Imm16 i
      else let
        val msg = 
          concat
           [
             "unsignedInt16: can't convert ",
             Int.toString i,
             " into a 16-bit unsigned immediate"
           ]
      in
        raise InternalError msg
      end;
      
    val int16_0  = Imm16  0;
    val int16_1  = Imm16  1;
    val int16_2  = Imm16  2;
  end; (* int16 *)
  
  
(*****************************************************************************)
(*                  24-bit immediates                                        *)
(*****************************************************************************)
  (* 
     Unconditional jumps and calls use 24-bit signed offsets.
     As for 16-bit immediates, we turn this into an unsigned
     quantity when we actually generate the code.
  *)
  abstype int24 = Imm24 of int
  with  
    fun getInt24 (Imm24 i) = i;
    
    (* is24Bit is the test for signed 24-bit immediates *) 
    fun is24Bit i = ~ exp2_23 <= i andalso i < exp2_23;
    
    fun int24 i =
      if is24Bit i
      then 
        if i < 0 
        then Imm24 (exp2_24 + i)
        else Imm24 i
      else let
        val msg = 
          concat
           [
             "int24: can't convert ",
             Int.toString i,
             " into a 24-bit signed immediate"
           ]
      in
        raise InternalError msg
      end;
      
     val int24_0 = Imm24 0;
  end; (* int24 *)
  
(*****************************************************************************)
(*                  14-bit immediates                                        *)
(*****************************************************************************)
  (* 
     Conditional jumps use 14-bit signed offsets.
     As for 16-bit immediates, we turn this into an unsigned
     quantity when we actually generate the code.
  *)
  abstype int14 = Imm14 of int
  with  
    fun getInt14 (Imm14 i) = i;
    
    (* is14Bit is the test for signed 14-bit immediates *) 
    fun is14Bit i = ~ exp2_13 <= i andalso i < exp2_13;
    
    fun int14 i =
      if is14Bit i
      then 
        if i < 0 
        then Imm14 (exp2_14 + i)
        else Imm14 i
      else let
        val msg = 
          concat
           [
             "int14: can't convert ",
             Int.toString i,
             " into a 14-bit signed immediate"
           ]
      in
        raise InternalError msg
      end;

    val int14_0  = Imm14 0;
  end; (* int14 *)
  
(*****************************************************************************)
(*                  10-bit immediates                                        *)
(*****************************************************************************)
  (* 
     X-form instructions use 10-bit extended opcodes.
     As for 16-bit immediates, we turn this into an unsigned
     quantity when we actually generate the code.
  *)
  abstype int10 = Imm10 of int
  with  
    fun getInt10 (Imm10 i) = i;
      
    val int10_0   = Imm10   0;
    val int10_4   = Imm10   4;
    val int10_8   = Imm10   8;
    val int10_16  = Imm10  16;
    val int10_23  = Imm10  23;
    val int10_28  = Imm10  28;
    val int10_32  = Imm10  32;
    val int10_87  = Imm10  87;
    val int10_151 = Imm10 151;
    val int10_215 = Imm10 215;
    val int10_235 = Imm10 235
    val int10_266 = Imm10 266;
    val int10_316 = Imm10 316;
    val int10_339 = Imm10 339;
    val int10_444 = Imm10 444;
    val int10_459 = Imm10 459
    val int10_467 = Imm10 467;
    val int10_520 = Imm10 520; (* *unsigned* 520 *)
    val int10_528 = Imm10 528; (* *unsigned* 528 *)
    val int10_747 = Imm10 747; (* *unsigned* 747 *)
    val int10_778 = Imm10 778; (* *unsigned* 778 *)
    val int10_824 = Imm10 824; (* *unsigned* 824 *)
  end; (* int10 *)
  
(*****************************************************************************)
(*                  5-bit immediates                                         *)
(*****************************************************************************)
  (* 
     Various instruction fields use 5-bits. We treat these 
     fields as as *unsigned* here.
  *)
  abstype int5 = Imm5 of int
  with
    fun getInt5 (Imm5 i) = i;
    
    (* is5Bit is the test for signed 5-bit immediates *) 
    fun isUnsigned5Bit i = 0 <= i andalso i < exp2_5;
    
    fun int5 i =
      if isUnsigned5Bit i
      then Imm5 i
      else let
        val msg = 
          concat
           [
             "int5: can't convert ",
             Int.toString i,
             " into a 5-bit unsigned immediate"
           ]
      in
        raise InternalError msg
      end;
      
    (* various small constants *)
    val int5_0  = Imm5  0;
    val int5_1  = Imm5  1;
    val int5_2  = Imm5  2;
    val int5_3  = Imm5  3;
    val int5_4  = Imm5  4;
    val int5_5  = Imm5  5;
    val int5_12 = Imm5 12;
    val int5_13 = Imm5 13;
  end; (* int5 *)
  
(*****************************************************************************)
(*                  splitUnsignedInt                                         *)
(*****************************************************************************)
  (* 
     The logical instructions don't sign-extend their arguments, so
     use this function to split the value into two halves.
  *)
  fun splitUnsignedInt (i:int) : int16 * int16 =
  let
    (* Values with the top bit set appear as negatives even though all
       values of type Word.word are unsigned. *)
    val pi = if i < 0 then exp2_32 + i else i;
    val lo16  = pi mod exp2_16;
    val hi16  = pi div exp2_16;
    
    val () =
      if isUnsigned16Bit hi16
      then ()
      else let
        val msg = 
          concat
           [
             "splitUnsignedInt: can't convert ",
             Int.toString i,
             " into a 32-bit unsigned immediate"
           ]
      in
        raise InternalError msg
      end;
  in
    (unsignedInt16 lo16, unsignedInt16 hi16)
  end;

(*****************************************************************************)
(*                  splitSignedInt                                           *)
(*****************************************************************************)
  (* 
     The addi instruction sign-extends its argument. We have to take
     this into account when we calculate the high-order part (for
     the addis instruction). If we didn't, we would be off by 16
     high-order ones whenever bit15 is set. 
  *)
  fun splitSignedInt (i:int) : int16 * int16 =
  let
    val lo16  = i mod exp2_16;
    val hi16  = i div exp2_16;
    
    val () =
      if is16Bit hi16
      then ()
      else let
        val msg = 
          concat
           [
             "splitSignedInt: can't convert ",
             Int.toString i,
             " into a 32-bit signed immediate"
           ]
      in
        raise InternalError msg
      end;
      
    (* Take account of low-order sign-extension *)
    val hi16' =
      if lo16 < exp2_15
      then hi16
      else hi16 + 1;
      
    (* hi16' may have overflowed 16 bits, so adjust it back. *)
    val hi16'' = hi16' mod exp2_16;
  in
    (unsignedInt16 lo16, unsignedInt16 hi16'')
  end;

(*****************************************************************************)
(*                  Abstype for registers                                    *)
(*****************************************************************************)
  infix 7 regEq regNeq regLeq regGeq regMinus;

  abstype reg = Reg of int  (* registers. *)
  with
    val regNone     = Reg ~1; (* Dummy register *)
    val regZero     = Reg 0;  (* temporary / zero-source *)
    (* Reg 1 is the C stack pointer *)
    (* Reg 2 is the C TOC pointer *)
    val regResult   = Reg 3;  (* argument 1 and function results *)
    (* Reg 4 - Reg 6 are used for arguments 2-4 *)
    (* Reg 7 - Reg 10 are general purpose registers *)
    val regTemp1    = Reg 11; (* temporary *)
    val regTemp2    = Reg 12; (* temporary *)
    (* R13 points to the memRegisters structure. *)
    val regMemRegs  = Reg 13;
    (* Reg 14 - Reg 22 are general purpose registers *)
    val regCode     = NONE; (* No special register required *)
    val regClosure  = Reg 24; (* address of closure (or static link) *)
    val regReturn   = Reg 25; (* return address *)
    (*val regTemp3    = Reg 26;*) (* Unsaved temporary. *)
    val regStackPtr = Reg 27; (* current ML stack pointer *)
    (* r28 is no longer used *)
    val regHeapPtr  = Reg 29; (* current heap allocation pointer *)
    val regHeapLim  = Reg 30; (* number of available heap bytes (words?) *)
    val regHandler  = Reg 31; (* pointer into the stack *)
 
    fun getReg (Reg r) = r;      (* reg.down *)
    fun mkReg   n      = Reg n;  (* reg.up   *)
  
    fun getReg5 (Reg r) = int5 r;

    (* The number of general registers.
       Includes result, closure, code, return and arg regs
       but not stackptr, handler, stack limit or heap ptrs. *)
    val regs = 8 + 12; (* r3-r10, r14-r25 *)

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
      else if i < 8 then mkReg (i + 3) else mkReg (i + 6) 
      
    fun a regEq  b = getReg a  = getReg b;
    fun a regNeq b = getReg a <> getReg b;
  
    (* The number of the register. *)
    fun nReg (Reg n) =
      if  3 <= n andalso n <= 10 then n - 3 else
      if 14 <= n andalso n <= 25 then n - 6 
      else let
        val msg =
          concat
            [
              "nReg: Bad register number ",
              Int.toString n
            ]
      in
        raise InternalError msg
      end;

    fun regRepr (Reg n) = "r" ^ Int.toString n;
    
    val argRegs = 4;

    (* Args 0, 1, 2, 3 correspond to r3, r4, r5, r6. *)
    fun argReg i =
      if 0 <= i andalso i < 4 then mkReg (i + 3)
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

(*****************************************************************************)
(*                  The opCode datatype                                      *)
(*****************************************************************************)

 (* The primary opcode - in the range 0 .. 63 *)
  datatype opCode =
    TWI       (*  3 *)
  | MULLI     (*  7 *)
  | CMPLI     (* 10 *)
  | CMPI      (* 11 *)
  | ADDI      (* 14 *)
  | ADDIS     (* 15 *)
  | BC        (* 16 *)
  | B         (* 18 *)
  | EXT19     (* 19 *)
  | RLWINM    (* 21 *)   
  | ORI       (* 24 *)
  | ORIS      (* 25 *)
  | XORI      (* 26 *)
  | XORIS     (* 27 *)
  | ANDIDOT   (* 28 *)   
  | ANDISDOT  (* 29 *)   
  | EXT31     (* 31 *)
  | LWZ       (* 32 *)
  | LBZ       (* 34 *)
  | STW       (* 36 *)
  | STWU      (* 37 *)
  | STB       (* 38 *)
  ;
  
  fun opToShort (op1 : opCode) : short = 
    case op1 of
      TWI      => short3
    | MULLI    => short7
    | CMPLI    => short10
    | CMPI     => short11
    | ADDI     => short14
    | ADDIS    => short15
    | BC       => short16
    | B        => short18
    | EXT19    => short19
    | RLWINM   => short21
    | ORI      => short24
    | ORIS     => short25
    | XORI     => short26
    | XORIS    => short27
    | ANDIDOT  => short28
    | ANDISDOT => short29
    | EXT31    => short31
    | LWZ      => short32
    | LBZ      => short34
    | STW      => short36
    | STWU     => short37
    | STB      => short38
    ;

  (* EXT19 secondary opcodes *)
  datatype opCode19 =
    BCLR     (*  16 *)
  | BCCTR    (* 528 *)
  ;
  
  fun op19ToInt10 (op2 : opCode19) : int10 =
    case op2 of
      BCLR  => int10_16
    | BCCTR => int10_528
    ;
 
 (* EXT31 secondary opcodes *)
  datatype opCode31 =
    CMP      (*   0 *)
  | TW       (*   4 *)
  | SUBFC    (*   8 *)
  | LWZX     (*  23 *)
  | AND      (*  28 *)
  | CMPL     (*  32 *)
  | LBZX     (*  87 *)
  | STWX     (* 151 *)
  | STBX     (* 215 *)
  | MULLW    (* 235 *)
  | ADD      (* 266 *)
  | XOR      (* 316 *)
  | MFSPR    (* 339 *)
  | OR       (* 444 *)
  | DIVWU    (* 459 *)
  | MTSPR    (* 467 *)
  | SUBFCO   (* 520 (512 + 8) *)
  | MULLWO   (* 747 (512 + 235) *)
  | ADDO     (* 778 (512 + 266) *)
  | SRAWI    (* 824 *)
  ;
  
  fun op31ToInt10 (op2 : opCode31) : int10 =
    case op2 of
      CMP    => int10_0
    | TW     => int10_4
    | SUBFC  => int10_8
    | LWZX   => int10_23
    | AND    => int10_28
    | CMPL   => int10_32
    | LBZX   => int10_87
    | STWX   => int10_151
    | STBX   => int10_215
    | ADD    => int10_266
    | XOR    => int10_316
    | MFSPR  => int10_339
    | OR     => int10_444
    | MTSPR  => int10_467
    | SUBFCO => int10_520
    | ADDO   => int10_778
    | SRAWI  => int10_824
    | MULLW  => int10_235
    | DIVWU  => int10_459
    | MULLWO => int10_747
    ;

(*****************************************************************************)
(*                  The testCode datatype                                    *)
(*****************************************************************************)
  datatype testCode =
    Lt
  | Gt
  | Eq
  | Ge
  | Le
  | Ne
  | SoInv (* used for arbitrary precision overflow only *)
  | NeInv (* used for zero divide detection only *)
  | GeInv (* used for heap and stack limit detection *)
  | EqInv (* used for arbitrary precision tag testing ??? Now removed ??? *)
  ;
  
 fun testToBiBoPair (tc : testCode) : int5 * int5 =
   case tc of
    Lt => (int5_12, int5_0) (* Branch if LT (bit 0) in CR0 is TRUE  *)
  | Gt => (int5_12, int5_1) (* Branch if GT (bit 1) in CR0 is TRUE  *)
  | Eq => (int5_12, int5_2) (* Branch if EQ (bit 2) in CR0 is TRUE  *)
  | Ge => (int5_4,  int5_0) (* Branch if LT (bit 0) in CR0 is FALSE *)
  | Le => (int5_4,  int5_1) (* Branch if GT (bit 1) in CR0 is FALSE *)
  | Ne => (int5_4,  int5_2) (* Branch if EQ (bit 2) in CR0 is FALSE *)
  | SoInv => (int5_13, int5_3) (* Branch if SO (bit 3) in CR0 is TRUE,
                                  but with inverted branch prediction *)
  | NeInv => (int5_5,  int5_2) (* Branch if EQ (bit 2) in CR0 is FALSE,
                                  but with inverted branch prediction *)
  | GeInv => (int5_5,  int5_0) (* Branch if LT (bit 0) in CR0 is FALSE,
                                  but with inverted branch prediction *)
  | EqInv => (int5_13, int5_2) (* Branch if EQ (bit 2) in CR0 is TRUE,
                                  but with inverted branch prediction *)
  ;
  
(*****************************************************************************)
(*                  Disassembler (unfinished)                                *)
(*****************************************************************************)
  fun printHex(v, printStream) = printStream(Int.fmt StringCvt.HEX v)
  (* prints a string representation of a number, padded to width characters *)
  fun printHexN (width : int, n : int, printStream) =
      printStream(StringCvt.padLeft #"0" width (Int.fmt StringCvt.HEX n))

  local

    fun printHex24 (n : int, printStream) = printHexN (6, n, printStream);
    fun printHex16 (n : int, printStream) = printHexN (4, n, printStream);
    fun printHex14 (n : int, printStream) = printHexN (4, n, printStream);
    fun printHex5  (n : int, printStream) = printHexN (2, n, printStream);

    fun splitMform (bits0_31 : int) : int * int * int * int * int * bool =
    let
      val bits0_25  = bits0_31 mod exp2_26;
      val bits0_20  = bits0_25 mod exp2_21;
      val bits0_15  = bits0_20 mod exp2_16;
      val bits0_10  = bits0_15 mod exp2_11;
      val bits0_5   = bits0_10 mod exp2_6;
      val bits0_0   = bits0_5  mod exp2_1;
      
      val bits21_25 = bits0_25 div exp2_21;
      val bits16_20 = bits0_20 div exp2_16;
      val bits11_15 = bits0_15 div exp2_11;
      val bits6_10  = bits0_10 div exp2_6;
      val bits1_5   = bits0_5  div exp2_1;
    in
      (bits21_25, bits16_20, bits11_15, bits6_10, bits1_5, bits0_0 = 1)
    end;
    
    fun splitDform (bits0_31 : int) : int * int * int =
    let
      val bits0_25  = bits0_31 mod exp2_26;
      val bits0_20  = bits0_25 mod exp2_21;
      val bits0_15  = bits0_20 mod exp2_16;
      
      val bits21_25 = bits0_25 div exp2_21;
      val bits16_20 = bits0_20 div exp2_16;
    in
      (bits21_25, bits16_20, bits0_15)
    end;
    
    fun splitBform (bits0_31 : int) : int * int * int * bool * bool =
    let
      val bits0_25  = bits0_31 mod exp2_26;
      val bits0_20  = bits0_31 mod exp2_21;
      val bits0_15  = bits0_20 mod exp2_16;
      val bits0_1   = bits0_31 mod exp2_2;
      val bits0_0   = bits0_1  mod exp2_1;
      
      val bits21_25 = bits0_25 div exp2_21;
      val bits16_20 = bits0_20 div exp2_16;
      val bits2_15  = bits0_15 div exp2_2;
      val bits1_1   = bits0_1  div exp2_1;
    in
      (bits21_25, bits16_20, bits2_15, bits1_1 = 1, bits0_0 = 1)
    end;
    
    fun splitIform (bits0_31 : int) : int * bool * bool =
    let
      val bits0_25  = bits0_31 mod exp2_26;
      val bits0_1   = bits0_25 mod exp2_2;
      val bits0_0   = bits0_1  mod exp2_1;
      
      val bits2_25  = bits0_25 div exp2_2;
      val bits1_1   = bits0_1  div exp2_1;
    in
      (bits2_25, bits1_1 = 1, bits0_0 = 1)
    end;
    
    fun splitXform (bits0_31 : int) : int * int * int * int * bool =
    let
      val bits0_25  = bits0_31 mod exp2_26;
      val bits0_20  = bits0_25 mod exp2_21;
      val bits0_15  = bits0_20 mod exp2_16;
      val bits0_10  = bits0_15 mod exp2_11;
      val bits0_0   = bits0_10 mod exp2_1;
      
      val bits21_25 = bits0_25 div exp2_21;
      val bits16_20 = bits0_20 div exp2_16;
      val bits11_15 = bits0_15 div exp2_11;
      val bits1_10  = bits0_10 div exp2_1;
    in
      (bits21_25, bits16_20, bits11_15, bits1_10, bits0_0 = 1)
    end;

    fun printUnknown (_ : int, printStream) : unit = 
    let
    in
      printStream "??????\t"
    end;
    
    fun printArithmeticDform (name : string, instr : int, printStream) : unit = 
    let
      val (RT : int, RA : int, SI : int) = splitDform instr;
    in
      printStream name;
      printStream "\t";
      printStream(regRepr (mkReg RT));
      printStream ",";
      printStream(regRepr (mkReg RA));
      printStream ",";
      printHex16(SI, printStream)
    end;
    
    fun printArithmeticXform (name : string, instr : int, printStream) : unit = 
    let
      val (RT : int, RA : int, RB : int, _: int, Rc : bool) = 
        splitXform instr;
    in
      printStream name;
      if Rc then printStream "." else ();
      printStream "\t";
      printStream(regRepr (mkReg RT));
      printStream ",";
      printStream(regRepr (mkReg RA));
      printStream ",";
      printStream(regRepr (mkReg RB))
    end;
    
    fun printLogicalDform (name : string, instr : int, printStream) : unit = 
    let
      val (RS : int, RA : int, UI : int) = splitDform instr;
    in
      printStream name;
      printStream "\t";
      printStream(regRepr (mkReg RA));
      printStream ",";
      printStream(regRepr (mkReg RS));
      printStream ",";
      printHex16(UI, printStream)
    end;
    
    fun printLogicalXform (name : string, instr : int, printStream) : unit = 
    let
      val (RS : int, RA : int, RB : int, _: int, Rc : bool) = 
        splitXform instr;
    in
      printStream name;
      if Rc then printStream "." else ();
      printStream "\t";
      printStream(regRepr (mkReg RA));
      printStream ",";
      printStream(regRepr (mkReg RS));
      printStream ",";
      printStream(regRepr (mkReg RB))
    end;
    
    fun printStorageDform (name : string, instr : int, printStream) : unit = 
    let
      val (RS : int, RA : int, D : int) = splitDform instr;
    in
      printStream name;
      printStream "\t";
      printStream(regRepr (mkReg RS));
      printStream ",";
      printStream(regRepr (mkReg RA));
      printStream ",";
      printHex16(D, printStream)
    end;
    
    fun printStorageXform (name : string, instr : int, printStream) : unit = 
    let
      val (RT : int, RA : int, RB : int, _: int, _ : bool) = 
        splitXform instr;
    in
      printStream name;
      printStream "\t";
      printStream(regRepr (mkReg RT));
      printStream ",";
      printStream(regRepr (mkReg RA));
      printStream ",";
      printStream(regRepr (mkReg RB))
    end;
    
    fun printExt19 (instr : int, printStream) : unit = 
    let
      val (BO : int, BI : int, _ : int, OP2 : int, LK : bool) = 
        splitXform instr;
    in
      case OP2 of
         16 => (* BCLR *)
        let
        in
          printStream (if LK then "bclrl\t" else "bclr\t");
          printHex5(BO, printStream);
          printStream ",";
          printHex5(BI, printStream)
        end
         
      | 528 => (* BCCTR *)
        let
        in
          printStream (if LK then "bcctrl\t" else "bcctr\t");
          printHex5(BO, printStream);
          printStream ",";
          printHex5(BI, printStream)
        end

      |   _ =>
         printUnknown(instr, printStream)
    end;
    
    fun printExt31 (instr : int, printStream) : unit = 
    let
      val (F1 : int, F2 : int, F3 : int, OP2 : int, _ : bool) = 
        splitXform instr;
    in
      case OP2 of
          0 => (* CMP *)
      let
      in
    printStream "cmp\t";
    printHex5(F1, printStream);
    printStream ",";
    printStream(regRepr (mkReg F2));
    printStream ",";
    printStream(regRepr (mkReg F3))
      end
      
      |   4 => (* TW *)
      let
      in
    printStream "tw\t";
    printHex5(F1, printStream);
    printStream ",";
    printStream(regRepr (mkReg F2));
    printStream ",";
    printStream(regRepr (mkReg F3))
      end
      
     |   8 => (* SUBFC *)
        printArithmeticXform ("subfc", instr, printStream)
      
      |  23 => (* LWZX *)
        printStorageXform ("lwzx", instr, printStream)
    
      |  28 => (* AND *)
        printLogicalXform ("and", instr, printStream)
    
      |  32 => (* CMPL *)
      let
      in
    printStream "cmpl\t";
    printHex5(F1, printStream);
    printStream ",";
    printStream(regRepr (mkReg F2));
    printStream ",";
    printStream(regRepr (mkReg F3))
      end

      |  87 => (* LBZX *)
        printStorageXform ("lbzx", instr, printStream)
    
      | 151 => (* STWX *)
        printStorageXform ("stwx", instr, printStream)
    
      | 215 => (* STBX *)
        printStorageXform ("stbx", instr, printStream)
    
      | 266 => (* ADD *)
        printArithmeticXform ("add", instr, printStream)

      | 316 => (* XOR *)
        printLogicalXform ("xor", instr, printStream)

      | 339 => (* MFSPR *)
      (
        case (F3, F2) of
          (0, 8) =>
          (
            printStream "mflr\t";
            printStream(regRepr (mkReg F1))
          )
          
        | (0, 9) =>
          (
            printStream "mfctr\t";
            printStream(regRepr (mkReg F1))
          )
          
        | _      =>
      (
        printStream "mfspr\t";
        printStream(regRepr (mkReg F1));
        printStream ",";
        printHex5(F2, printStream);
        printStream ",";
        printHex5(F3, printStream)
      )
      )

      | 444 => (* OR *)
        printLogicalXform ("or", instr, printStream)

      | 467 => (* MTSPR *)
      (
        case (F3, F2) of
          (0, 8) =>
          (
            printStream "mtlr\t";
            printStream(regRepr (mkReg F1))
          )
          
        | (0, 9) =>
          (
            printStream "mtctr\t";
            printStream(regRepr (mkReg F1))
          )
          
        | _      =>
      (
        printStream "mtspr\t";
        printStream(regRepr (mkReg F1));
        printStream ",";
        printHex5(F2, printStream);
        printStream ",";
        printHex5(F3, printStream)
      )
      )

      | 520 => (* SUBFCO *)
        printArithmeticXform ("subfco", instr, printStream)

      | 778 => (* ADDO *)
        printArithmeticXform ("addo", instr, printStream)

      | 824 => (* SRAWI *)
      let
      in
        (* "logical therefore backwards" register ordering *)
    printStream "srawi\t";
    printStream(regRepr (mkReg F2));
    printStream ",";
    printStream(regRepr (mkReg F1));
    printStream ",";
    printHex5(F3, printStream)
      end

      | 235 => (* MULLW *)
        printArithmeticXform ("mullw", instr, printStream)

      | 747 => (* MULLWO *)
        printArithmeticXform ("mullwo", instr, printStream)

      | 459 => (* DIVWU *)
        printArithmeticXform ("divwu", instr, printStream)

      |   _ =>
        printUnknown(instr, printStream)
    end; (* printExt31 *)
  in
    fun printInstr (addr : int, instr : int, printStream) : unit = 
    let
      val OPCD_field : int = instr div exp2_26; (* MSB 6 bits *)
    in
      case OPCD_field of
     3 => (* TWI *)
    let
      val (TO : int, RA : int, SI : int) = splitDform instr;
    in
      printStream "twi\t";
      printHex5(TO, printStream);
      printStream ",";
      printStream(regRepr (mkReg RA));
      printStream ",";
      printHex16(SI, printStream)
    end
     
     | 7 => (* MULLI *)
    let
      val (TO : int, RA : int, SI : int) = splitDform instr;
    in
      printStream "mulli\t";
      printHex5(TO, printStream);
      printStream ",";
      printStream(regRepr (mkReg RA));
      printStream ",";
      printHex16(SI, printStream)
    end
     
      | 10 => (* CMPLI *)
    let
      val (TO : int, RA : int, UI : int) = splitDform instr;
    in
      printStream "cmpli\t";
      printHex5(TO, printStream);
      printStream ",";
      printStream(regRepr (mkReg RA));
      printStream ",";
      printHex16(UI, printStream)
    end

      | 11 => (* CMPI *)
    let
      val (TO : int, RA : int, SI : int) = splitDform instr;
    in
      printStream "cmpi\t";
      printHex5(TO, printStream);
      printStream ",";
      printStream(regRepr (mkReg RA));
      printStream ",";
      printHex16(SI, printStream)
    end
      
      | 14 => (* ADDI *)
      printArithmeticDform ("addi", instr, printStream)
      
      | 15 => (* ADDIS *)
      printArithmeticDform ("addis", instr, printStream)
      
      | 16 => (* BC *)
    let
      val (BO : int, BI : int, BD : int, AA : bool, LK : bool) = 
        splitBform instr;
      val offset = if BD < exp2_13 then BD else BD - exp2_14;
    in
      printStream
        (case (AA, LK) of
           (false, false) => "bc\t"
        |  (false, true)  => "bcl\t"
        |  (true,  false) => "bca\t"
        |  (true,  true)  => "bcla\t");
      printHex5(BO, printStream);
      printStream ",";
      printHex5(BI, printStream);
      printStream ",";
      printHex14(BD, printStream);
      if AA
      then printStream "\t;!!! absolute addressing ???"
      else (printStream "\t;to "; printHex (addr + 4 * offset, printStream))
    end
      
      | 18 => (* B *)
    let
      val (LI : int, AA : bool, LK : bool) = splitIform instr;
      val offset = if LI < exp2_23 then LI else LI - exp2_24;
    in
      printStream
        (case (AA, LK) of
           (false, false) => "b\t"
        |  (false, true)  => "bl\t"
        |  (true,  false) => "ba\t"
        |  (true,  true)  => "bla\t");
      printHex24(LI, printStream);
      if AA
      then printStream "\t\t;!!! absolute addressing ???"
      else (printStream "\t\t;to "; printHex (addr + 4 * offset, printStream))
    end
      
      | 19 =>  (* EXT19 *)
      printExt19(instr, printStream)
      
      | 21 => (* RLWINM *)
    let
      val (RS : int, RA : int, SH : int, MB : int, ME : int, Rc : bool) = 
       splitMform instr;
    in
      printStream (if Rc then "rlwinm.\t" else "rlwinm\t");
      printStream(regRepr (mkReg RA));
      printStream ",";
      printStream(regRepr (mkReg RS));
      printStream ",";
      printHex5(SH, printStream);
      printStream ",";
      printHex5(MB, printStream);
      printStream ",";
      printHex5(ME, printStream)
    end
      
      | 24 => (* ORI *)
      printLogicalDform ("ori", instr, printStream)
     
      | 25 => (* ORIS *)
      printLogicalDform ("oris", instr, printStream)
     
      | 26 => (* XORI *)
      printLogicalDform ("xori", instr, printStream)
     
      | 27 => (* XORIS *)
      printLogicalDform ("xoris", instr, printStream)
     
      | 28 => (* ANDIDOT *)
      printLogicalDform ("andi.", instr, printStream)
     
      | 29 => (* ANDISDOT *)
      printLogicalDform ("andis.", instr, printStream)
     
      | 31 => (* EXT31 *)
      printExt31(instr, printStream)

      | 32 => (* LWZ *)
      printStorageDform ("lwz", instr, printStream)
     
      | 34 => (* LBZ *)
      printStorageDform ("lbz", instr, printStream)
     
      | 36 => (* STW *)
      printStorageDform ("stw", instr, printStream)
     
      | 37 => (* STWU *)
      printStorageDform ("stwu", instr, printStream)
     
      | 38 => (* STB *)
      printStorageDform ("stb", instr, printStream)
     
      |  _ =>
      printUnknown(instr, printStream)
    end; (* printInstr *)
  end;

  
 (* Offsets in the memRegisters structure.  This is pointed at by r13 *)
 val MemRegisterStackLimit      = 24 (* This contains the current base of the stack.  Also
                                        used by the RTS to cause an interrupt by being set
                                        to the end of the stack. *)
 val MemRegisterHeapOverflow    = 28 (* Called when the heap limit is reached. *)
 val MemRegisterStackOverflow   = 32 (* Called when the stack limit is reached. *)
 val MemRegisterStackOverflowEx = 36 (* Called when the stack limit is reached. *)
 val MemRegisterRaiseException  = 40 (* Called to raise an exception with an exception packet. *)
 (*val MemRegisterIOEntry         = 44 *)(* Called to make an IO call.  Not currently used by the code-generator. *)
 val MemRegisterRaiseDiv        = 48 (* Called to raise a divide exception. *)
 val MemRegisterArbEmulation    = 52 (* Called to emulate an arbitrary precision instruction. *)

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
    Quad (topHw    >> short8, Word.andb (mask8Bits, topHw),
          bottomHw >> short8, Word.andb (mask8Bits, bottomHw))
  end;

  (* returns *unsigned* integer *)
  fun fromQuad (Quad (b1,b2,b3,b4)) : int =
  let
    val topHw    = toInt (Word.orb (b1 << short8, b2));
    val bottomHw = toInt (Word.orb (b3 << short8, b4));
  in
    topHw * exp2_16 + bottomHw
  end;

(*****************************************************************************)
(*                  Basic instruction assembly routines                      *)
(*****************************************************************************)
  fun IformQuad (op1: opCode, li : int24, aa : bool, lk : bool) : quad =
  let
    val op1_short = opToShort op1;         (*  6 bits *)
    val li_short  = toShort (getInt24 li); (* 24 bits *)
    val aa_short  = if aa then short1 else short0; (* 1 bit *)
    val lk_short  = if lk then short1 else short0; (* 1 bit *)

    val bits26_31 = op1_short << short2;  (* 6 bits *)
    val bits24_25 = li_short  >> short22; (* 2 bits *) 
    val b1 = Word.orb (bits24_25, bits26_31);
    
    val bits16_23 = Word.andb (li_short >> short14, mask8Bits);
    val b2 = bits16_23;
   
    val bits8_15 = Word.andb (li_short >> short6, mask8Bits);
    val b3 = bits8_15;
    
    val bits2_7  = Word.andb (li_short, mask6Bits) << short2;
    val bits1_1  = aa_short << short1;
    val bits0_0  = lk_short;
    val b4 = Word.orb (bits0_0, Word.orb (bits1_1, bits2_7));
  in
    Quad (b1, b2, b3, b4)
  end;
  
  fun BformQuad (op1: opCode, bo : int5, bi : int5, bd : int14, aa : bool, lk : bool) : quad =
  let
    val op1_short = opToShort op1;         (*  6 bits *)
    val bo_short  = toShort (getInt5 bo);  (*  5 bits *)
    val bi_short  = toShort (getInt5 bi);  (*  5 bits *)
    val bd_short  = toShort (getInt14 bd); (* 14 bits *)
    val aa_short  = if aa then short1 else short0; (* 1 bit *)
    val lk_short  = if lk then short1 else short0; (* 1 bit *)

    val bits26_31 = op1_short << short2;  (* 6 bits *)
    val bits24_25 = bo_short  >> short3;  (* 2 bits *)
    val b1 = Word.orb (bits24_25, bits26_31);
    
    val bits21_23 = Word.andb (bo_short, mask3Bits) << short5 (* 3 bits *)
    val bits16_20 = bi_short;                            (* 5 bits *)
    val b2 = Word.orb (bits16_20, bits21_23);
   
    val bits8_15 = Word.andb (bd_short >> short6, mask8Bits);
    val b3 = bits8_15;
    
    val bits2_7  = Word.andb (bd_short, mask6Bits) << short2;
    val bits1_1  = aa_short << short1;
    val bits0_0  = lk_short;
    val b4 = Word.orb (bits0_0, Word.orb (bits1_1, bits2_7));
  in
    Quad (b1, b2, b3, b4)
  end;
  
  fun DformQuad (op1: opCode, rt : int5, ra : int5,
                 si : int16) : quad =
  let
    val op1_short = opToShort op1;         (* 6 bits *)
    val rt_short  = toShort (getInt5 rt);  (* 5 bits *)
    val ra_short  = toShort (getInt5 ra);  (* 5 bits *)
    val si_short  = toShort (getInt16 si); (* 16 bits *)

    val bits26_31 = op1_short << short2; (* 6 bits *)
    val bits24_25 = rt_short  >> short3; (* 2 bits *) 
    val b1 = Word.orb (bits24_25, bits26_31);
    
    val bits21_23 = Word.andb (rt_short, mask3Bits) << short5 (* 3 bits *)
    val bits16_20 = ra_short;                            (* 5 bits *)
    val b2 = Word.orb (bits16_20, bits21_23);
   
    val bits8_15 = si_short >> short8;  (* 8 bits *)
    val b3 = bits8_15;
    
    val bits0_7   = Word.andb (si_short, mask8Bits); (* 8 bits *)
    val b4 = bits0_7;
  in
    Quad (b1, b2, b3, b4)
  end;
  
  (* field names based on add instruction *)
  fun XformQuad (op1: opCode, rt : int5, ra : int5,
                 rb : int5, xo : int10, rc : bool) : quad =
  let
    val op1_short = opToShort op1;         (* 6 bits *)
    val rt_short  = toShort (getInt5 rt);  (* 5 bits *)
    val ra_short  = toShort (getInt5 ra);  (* 5 bits *)
    val rb_short  = toShort (getInt5 rb);  (* 5 bits *)
    val xo_short  = toShort (getInt10 xo); (* 10 bits *)
    val rc_short  = if rc then short1 else short0; (* 1 bit *)

    val bits26_31 = op1_short << short2; (* 6 bits *)
    val bits24_25 = rt_short  >> short3; (* 2 bits *) 
    val b1 = Word.orb (bits24_25, bits26_31);
    
    val bits21_23 = Word.andb (rt_short, mask3Bits) << short5 (* 3 bits *)
    val bits16_20 = ra_short;                           (* 5 bits *)
    val b2 = Word.orb (bits16_20, bits21_23);
   
    val bits11_15 = rb_short << short3  (* 5 bits *)
    val bits8_10  = xo_short >> short7; (* 3 bits *)
    val b3 = Word.orb (bits8_10, bits11_15);
    
    val bits1_7   = Word.andb (xo_short, mask7Bits) << short1;  (* 7 bits *)
    val bits0_0   = rc_short;                             (* 1 bit *)
    val b4 = Word.orb (bits0_0, bits1_7);
  in
    Quad (b1, b2, b3, b4)
  end;
  
  (* field names based on rlwinm instruction *)
  fun MformQuad (op1: opCode, rs : int5, ra : int5, sh : int5,
                 mb : int5, me : int5, rc : bool) : quad =
  let
    val op1_short = opToShort op1;         (* 6 bits *)
    val rs_short  = toShort (getInt5 rs);  (* 5 bits *)
    val ra_short  = toShort (getInt5 ra);  (* 5 bits *)
    val sh_short  = toShort (getInt5 sh);  (* 5 bits *)
    val mb_short  = toShort (getInt5 mb);  (* 5 bits *)
    val me_short  = toShort (getInt5 me);  (* 5 bits *)
    val rc_short  = if rc then short1 else short0; (* 1 bit *)

    val bits26_31 = op1_short << short2; (* 6 bits *)
    val bits24_25 = rs_short  >> short3; (* 2 bits *) 
    val b1 = Word.orb (bits24_25, bits26_31);
    
    val bits21_23 = Word.andb (rs_short, mask3Bits) << short5 (* 3 bits *)
    val bits16_20 = ra_short;                           (* 5 bits *)
    val b2 = Word.orb (bits16_20, bits21_23);
   
    val bits11_15 = sh_short << short3   (* 5 bits *)
    val bits8_10  = mb_short >> short2;  (* 3 bits *)
    val b3 = Word.orb (bits8_10, bits11_15);
    
    val bits6_7   = Word.andb (mb_short, mask2Bits) << short6;  (* 2 bits *)
    val bits1_5   = me_short << short1;                   (* 5 bits *)
    val bits0_0   = rc_short;                             (* 1 bit *)
    val b4 = Word.orb (bits0_0, Word.orb(bits1_5, bits6_7));
  in
    Quad (b1, b2, b3, b4)
  end;
  
(*****************************************************************************)
(*                  Instruction assembly routines - arithmetic               *)
(*****************************************************************************)
  fun addi  (rt : reg, ra : reg, SI) = 
  let
    val rt' : int5 = getReg5 rt;
    val ra' : int5 = getReg5 ra;
  in
    DformQuad (ADDI,  rt' , ra' , SI)
  end;
  
  fun addis (rt : reg, ra : reg, SI) =
  let
    val rt' : int5 = getReg5 rt;
    val ra' : int5 = getReg5 ra;
  in
    DformQuad (ADDIS, rt', ra', SI)
  end;

  fun sub (rt : reg, ra : reg, rb : reg) : quad =
  let
    val rt' : int5 = getReg5 rt;
    val ra' : int5 = getReg5 ra;
    val rb' : int5 = getReg5 rb;
  in
    (* ra and rb are swapped, because the actual instruction is "subfc" *)
    XformQuad (EXT31, rt', rb', ra', op31ToInt10 SUBFC, false)
  end;
  
  fun cmpl (ra : reg, rb : reg) : quad =
  let
    val ra' : int5 = getReg5 ra;
    val rb' : int5 = getReg5 rb;
  in
    XformQuad (EXT31, int5_0, ra', rb', op31ToInt10 CMPL, false)
  end;

  (* Comparisons only use CR field 0, for now. *)
  fun cmpi (ra : reg, SI : int16) : quad =
  let
    val ra' : int5 = getReg5 ra;
  in
    DformQuad (CMPI, int5_0, ra', SI)
  end;

  fun cmpli (ra : reg, SI : int16) : quad =
  let
    val ra' : int5 = getReg5 ra;
  in
    DformQuad (CMPLI, int5_0, ra', SI)
  end;
    
(*****************************************************************************)
(*                  Instruction assembly routines - logical operations       *)
(*****************************************************************************)
  fun ori (ra : reg, rs : reg, UI : int16) : quad =
  let
    val ra' : int5 = getReg5 ra;
    val rs' : int5 = getReg5 rs;
  in
    (* the RA and RS fields are "backwards" in logical operations *)
    DformQuad (ORI, rs', ra', UI)
  end;
  
  fun oris (ra : reg, rs : reg, UI : int16) : quad =
  let
    val ra' : int5 = getReg5 ra;
    val rs' : int5 = getReg5 rs;
  in
    (* the RA and RS fields are "backwards" in logical operations *)
    DformQuad (ORIS, rs', ra', UI)
  end;
  
  fun xori (ra : reg, rs : reg, UI : int16) : quad =
  let
    val ra' : int5 = getReg5 ra;
    val rs' : int5 = getReg5 rs;
  in
    (* the RA and RS fields are "backwards" in logical operations *)
    DformQuad (XORI, rs', ra', UI)
  end;
  
  fun xoris (ra : reg, rs : reg, UI : int16) : quad =
  let
    val ra' : int5 = getReg5 ra;
    val rs' : int5 = getReg5 rs;
  in
    (* the RA and RS fields are "backwards" in logical operations *)
    DformQuad (XORIS, rs', ra', UI)
  end;
  
  fun andiDot (ra : reg, rs : reg, UI : int16) : quad =
  let
    val ra' : int5 = getReg5 ra;
    val rs' : int5 = getReg5 rs;
  in
    DformQuad (ANDIDOT, rs', ra', UI)
  end;
  
  (* "and" is an ML keyword, so we use "and_" *)
  fun and_ (ra : reg, rs : reg, rb : reg) : quad =
  let
    val ra' : int5 = getReg5 ra;
    val rs' : int5 = getReg5 rs;
    val rb' : int5 = getReg5 rb;
  in
    XformQuad (EXT31, rs', ra', rb', op31ToInt10 AND, false)
  end;
  
  fun rlwinmRc rc (ra : reg, rs : reg, sh : int, mb : int, me : int) : quad =
  let
    val ra' : int5 = getReg5 ra;
    val rs' : int5 = getReg5 rs;
    val sh' : int5 = int5 sh;
    val mb' : int5 = int5 mb;
    val me' : int5 = int5 me;
  in
    (* ra and rs are the "wrong" way round in the rlwinm instruction *)
    MformQuad (RLWINM, rs', ra', sh', mb', me', rc)
  end;
  
  val rlwinm    = rlwinmRc false
  and rlwinmDot = rlwinmRc true

  (* require 0 <= shift < 32 *)
  fun slwi (ra : reg, rs : reg, shift : int) : quad =
    rlwinm (ra, rs, shift, 0, 31 - shift);

  (* Shift and set the condition codes. *)
  fun slwiDot (ra : reg, rs : reg, shift : int) : quad =
    rlwinmDot (ra, rs, shift, 0, 31 - shift);

  (* requires 0 <= shift < 32 *)
  fun srwi (ra : reg, rs : reg, shift : int) : quad =
    rlwinm (ra, rs, if shift = 0 then 0 else 32 - shift, shift, 31);

  fun srawi (ra : reg, rs : reg, shift : int) : quad =
  let
    val ra' : int5 = getReg5 ra;
    val rs' : int5 = getReg5 rs;
    val sh' : int5 = int5 shift;
  in
    XformQuad (EXT31, rs', ra', sh', op31ToInt10 SRAWI, false)
  end;

  fun mulli  (rt : reg, ra : reg, SI) : quad = 
  let
    val rt' : int5 = getReg5 rt;
    val ra' : int5 = getReg5 ra;
  in
    DformQuad (MULLI,  rt' , ra' , SI)
  end;
 
(*****************************************************************************)
(*                  Instruction assembly routines - special registers        *)
(*****************************************************************************)
  local
    (* The 5-bit representation of the special registers *)
    val LR  = int5 8;
    val CTR = int5 9;
  in
    fun mtlr (rs : reg) : quad = 
      XformQuad (EXT31, getReg5 rs, LR, int5_0, op31ToInt10 MTSPR, false);
  
    fun mtctr (rs : reg) : quad = 
      XformQuad (EXT31, getReg5 rs, CTR, int5_0, op31ToInt10 MTSPR, false);
  
    fun mflr (rt : reg) : quad = 
      XformQuad (EXT31, getReg5 rt, LR, int5_0, op31ToInt10 MFSPR, false);
  end;
  
(*****************************************************************************)
(*                  Instruction assembly routines - load and store           *)
(*****************************************************************************)
  fun lwz (rt : reg, ra : reg, SI : int16) : quad =
  let
    val rt' : int5 = getReg5 rt;
    val ra' : int5 = getReg5 ra;
  in
    DformQuad (LWZ, rt', ra', SI)
  end;

  fun lwzx (rt : reg, ra : reg, rb : reg) : quad =
  let
    val rt' : int5 = getReg5 rt;
    val ra' : int5 = getReg5 ra;
    val rb' : int5 = getReg5 rb;
  in
    XformQuad (EXT31, rt', ra', rb', op31ToInt10 LWZX, false)
  end;

  fun lbz (rt : reg, ra : reg, SI : int16) : quad =
  let
    val rt' : int5 = getReg5 rt;
    val ra' : int5 = getReg5 ra;
  in
    DformQuad (LBZ, rt', ra', SI)
  end;

  fun lbzx (rt : reg, ra : reg, rb : reg) : quad =
  let
    val rt' : int5 = getReg5 rt;
    val ra' : int5 = getReg5 ra;
    val rb' : int5 = getReg5 rb;
  in
    XformQuad (EXT31, rt', ra', rb', op31ToInt10 LBZX, false)
  end;

  fun stw (rs : reg, ra : reg, SI : int16) : quad =
  let
    val rs' : int5 = getReg5 rs;
    val ra' : int5 = getReg5 ra;
  in
    DformQuad (STW, rs', ra', SI)
  end;

  fun stwx (rs : reg, ra : reg, rb : reg) : quad =
  let
    val rs' : int5 = getReg5 rs;
    val ra' : int5 = getReg5 ra;
    val rb' : int5 = getReg5 rb;
  in
    XformQuad (EXT31, rs', ra', rb', op31ToInt10 STWX, false)
  end;
  
  fun stb (rs : reg, ra : reg, SI : int16) : quad =
  let
    val rs' : int5 = getReg5 rs;
    val ra' : int5 = getReg5 ra;
  in
    DformQuad (STB, rs', ra', SI)
  end;

  fun stbx (rs : reg, ra : reg, rb : reg) : quad =
  let
    val rs' : int5 = getReg5 rs;
    val ra' : int5 = getReg5 ra;
    val rb' : int5 = getReg5 rb;
  in
    XformQuad (EXT31, rs', ra', rb', op31ToInt10 STBX, false)
  end;

  fun stwu (rs : reg, ra : reg, SI : int16) : quad =
  let
    val rs' : int5 = getReg5 rs;
    val ra' : int5 = getReg5 ra;
  in
    DformQuad (STWU, rs', ra', SI)
  end;
  
(*****************************************************************************)
(*                  Instruction assembly routines - function calls           *)
(*****************************************************************************)
  (* blrl (sets LR) - used for function calls *)
  val blrlQuad : quad =
    XformQuad (EXT19, int5 20, int5_0, int5_0, op19ToInt10 BCLR, true);

  (* bctr (doesn't set LR) - used for tail-calls *)
  val bctrQuad : quad =
    XformQuad (EXT19, int5 20, int5_0, int5_0, op19ToInt10 BCCTR, false);

  (* blr (doesn't set LR) - used for function returns *)
  val blrQuad : quad =
    XformQuad (EXT19, int5 20, int5_0, int5_0, op19ToInt10 BCLR, false);

(*****************************************************************************)
(*                  Higher-level instruction assembly routines               *)
(*****************************************************************************)

  (* unconditional branch, pc-relative, don't set LR *)
  fun uncondBranch (disp : int24) : quad =
    IformQuad (B, disp, false, false);

  (* unconditional call, pc-relative, sets LR *)
  fun uncondCall (disp : int24) : quad =
    IformQuad (B, disp, false, true);

  (* unconditional call, pc-relative, set LR *)
  val call0Quad : quad = uncondCall int24_0;
  val branchAlwaysQuad : quad = uncondBranch int24_0;

  fun condBranch (cond : testCode, disp : int14) : quad =
  let
    val (bo : int5, bi : int5) = testToBiBoPair cond;
  in
    BformQuad (BC, bo, bi, disp, false, false)
  end;
  
  
  fun fixupCondBranch (instr : quad, disp : int14) : quad =
  let
    val (Quad (b1', b2', b3', b4')) = instr;
    val b1    = toInt b1';
    val b2    = toInt b2';
    val b3    = toInt b3';
    val b4    = toInt b4';
    val op1   = b1 div exp2_2;
    val bo    = ((b1 mod exp2_2) * exp2_3) + (b2 div exp2_5);
    val bi    = b2 mod exp2_5;
    val bd    = (b3 * exp2_6) + (b4 div exp2_2);
    val aa_lk = b4 mod exp2_2;
  in
    if op1   = toInt (opToShort BC) andalso
       bd    = 0 andalso
       aa_lk = 0
    then BformQuad (BC, int5 bo, int5 bi, disp, false, false)
    else 
      raise InternalError ("fixupCondBranch: can't fixup " ^ Int.toString (fromQuad instr))
  end;
       
  fun fixupUncondBranch (instr : quad, disp : int24) : quad =
  let
    val (Quad (b1', b2', b3', b4')) = instr;
    val b1    = toInt b1';
    val b2    = toInt b2';
    val b3    = toInt b3';
    val b4    = toInt b4';
    val op1   = b1 div exp2_2;
    val li    = ((b1 mod exp2_2) * exp2_22) + (b2 * exp2_14) +
                (b3 * exp2_6) + (b4 div exp2_2)
    val aa_lk = b4 mod exp2_2;
  in
    if op1   = toInt (opToShort B) andalso
       li    = 0 andalso
       aa_lk = 0
    then uncondBranch disp
    else 
      raise InternalError ("fixupUncondBranch: can't fixup " ^ Int.toString (fromQuad instr))
  end;
       
  fun fixupUncondCall (instr : quad, disp : int24) : quad =
  let
    val (Quad (b1', b2', b3', b4')) = instr;
    val b1    = toInt b1';
    val b2    = toInt b2';
    val b3    = toInt b3';
    val b4    = toInt b4';
    val op1   = b1 div exp2_2;
    val li    = ((b1 mod exp2_2) * exp2_22) + (b2 * exp2_14) +
                (b3 * exp2_6) + (b4 div exp2_2)
    val aa_lk = b4 mod exp2_2;
  in
    if op1   = toInt (opToShort B) andalso
       li    = 0 andalso
       aa_lk = 1
    then uncondCall disp
    else 
      raise InternalError ("fixupUncondCall: can't fixup " ^ Int.toString (fromQuad instr))
  end;

(***************************************************************************  
  Functions that deal with 32-bit immediates (for convenience). If
  these functions are called with regStackPtr as an argument, it's
  the caller's responsibility to keep the stack-offset caching scheme
  in a consistent state.
***************************************************************************)  
  (* The addi and addis instructions treat r0 as 0 when used as a source *)
  fun addImmed (rt : reg, ra  : reg, imm : int) : quad list =
    if rt regEq regZero
    then raise InternalError "addImmed: can't use r0 for add immediate"
    else let
      val (lo, hi) = splitSignedInt imm;
    in
      if isZero16 hi
      then if isZero16 lo andalso rt regEq ra
       then []
       else [addi (rt, ra, lo)]
      else
    addis (rt, ra, hi) ::
    (if isZero16 lo then [] else [addi (rt, rt, lo)])
    end;
    
  fun loadImmed (rt : reg, imm : int) : quad list =
    addImmed (rt, regZero, imm);

  (* Logical operations on immediates. The low-order part will always be
     non-zero for orImmed and andImmed because of the tag bit. *)
  fun orImmed (rt : reg, ra  : reg, imm : int) : quad list =
    let
      val (lo, hi) = splitUnsignedInt imm;
    in
      if isZero16 hi
      then [ori (rt, ra, lo)]
      else [oris (rt, ra, hi), ori (rt, rt, lo)]
    end;

  fun xorImmed (rt : reg, ra  : reg, imm : int) : quad list =
    let
      val (lo, hi) = splitUnsignedInt imm;
    in
      if isZero16 hi
      then if isZero16 lo andalso rt regEq ra
       then []
       else [xori (rt, ra, lo)]
      else
    xoris (rt, ra, hi) ::
    (if isZero16 lo then [] else [xori (rt, rt, lo)])
    end;
    
  (*
     We can't write andImmed in the same way as the above,
     because the andi./andis. instructions both zero-extend
     their immediate argument, which means they can't
     be combined without the use of an extra register.
     We use r0 as the auxiliary register.
   *)
  fun andImmed (rt : reg, ra  : reg, imm : int) : quad list =
    let
      val (lo, hi) = splitUnsignedInt imm;
    in
      if isZero16 hi
      then [andiDot (rt, ra, lo)]
      else [addis (regZero, regZero, hi), (* r0 := hi << 16 *)
            ori   (regZero, regZero, lo), (* r0 := r0 | lo  *)
            and_  (rt, ra, regZero)]      (* rt := ra & r0  *)
    end;

(*****************************************************************************)
(*                  Abstype for instruction addresses                        *)
(*****************************************************************************)
  infix 6 wordAddrPlus wordAddrMinus;
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

    fun (Addr a) addrLt (Addr b) = a < b; 
    fun (Addr a) addrEq (Addr b) = a = b; 

    fun mkWordAddr n = Addr n;
  
    fun getWordAddr (Addr a) = a;
    fun getByteAddr (Addr a) = a * wordSize;
  
    val addrZero = mkWordAddr 0;
    val addrLast = mkWordAddr (exp2_29 - 1); (* A big number. *)
  end;
  
(*****************************************************************************)
(*                  Cache state                                              *)
(*****************************************************************************)
  datatype cacheState =
      Unreachable              (* code is unreachable *)
    | Reachable of bool        (* usable return address in LR *)
    ;
      
  (* This defines the information lattice for the cacheState datatype.
     It's actually a linear order. *)
  fun mergeCacheStates (Unreachable,         state)               = state
    | mergeCacheStates (Reachable lr1, Reachable lr2) = Reachable (lr1 andalso lr2)
    | mergeCacheStates (state,               Unreachable)         = state
    
  (* Does LR cache the return address? *)
  fun LRcacheActive Unreachable         = true (* sic *)
    | LRcacheActive (Reachable lr)      = lr;
    
(*****************************************************************************)
(*                  Types for branch labels                                  *)
(*****************************************************************************)

  (* The addrs is the address of the branch instruction, so we can fix up
     the branch later, NOT the address we're branching to, which we
     don't know when we generate the label. The cacheState indicates whether
     what was cached at the source of the jump.
   *)
  datatype jumpFrom =
    Jump14From of addrs * cacheState  (* branch instruction has 14-bit offset field *)
  | Jump24From of addrs * cacheState; (* branch instruction has 24-bit offset field *)
      
  (* We need a jumpFrom ref, because we may have to indirect short branches
     via long branches if the offset won't fit into 14 bits *)
  type labels = (jumpFrom ref) list;
  
  val noJump : labels = []; 
  
  (* This is the list of outstanding labels.  Use a separate type from
      "labels" for extra security. *)
  type labList = (jumpFrom ref) list;

(*****************************************************************************)
(*                  The main "code" datatype                                 *)
(*****************************************************************************)
  datatype const =
      WVal of machineWord        (* an existing constant *)
    | CVal of code        (* a forward-reference to another function *)
    | HVal of addrs ref   (* a handler *)

  (* Constants which are too far to go inline in the code are put in
     a list and put at the end of the code. They are arranged so that
     the garbage collector can find them and change them as necessary.
     A reference to a constant is treated like a forward reference to a
     label. *)

  (* A code list is used to hold a list of code-vectors which must have the
     address of this code-vector put into it. *)

  and setCodeseg =
      Unset
    | Set of cseg   (* Used for completing forward references. *)

  and code = Code of 
    { codeVec:        cseg,           (* This segment is used as a buffer. When the
                                         procedure has been code generated it is
                                         copied into a new segment of the correct size *)
      ic:             addrs ref,      (* Pointer to first free location in "codevec" *)
      constVec:       (const * addrs) list ref, (* Constants used in the code *)
      numOfConsts:    int ref,        (* size of constVec *)
      stackReset:     int ref,        (* Distance to reset the stack before the next instr. *)
      pcOffset:       int ref,        (* Offset of code in final segment. *)
                                      (* This is used also to test for identity of code segments. *)
      labelList:      labList ref,    (* List of outstanding short branches. *)
      longestBranch:  addrs ref,      (* Address of the earliest short branch.*)
      procName:       string,         (* Name of the procedure. *)
      otherCodes:     code list ref,  (* Other code vectors with forward references to this vector. *)
      resultSeg:      setCodeseg ref, (* The segment as the final result. *)
      mustCheckStack: bool ref,       (* Set to true if stack check must be done. *)
      justComeFrom:   labels ref,     (* The label(s) we have just jumped from. *)
      selfCalls:      addrs list ref, (* List of recursive calls to patch up. *)
      selfJumps:      labels ref,     (* List of recursive tail-calls to patch up. *)
      noClosure:      bool,           (* should we make a closure from this? *)
      cacheState:     cacheState ref,  (* Do we know anything useful here? *)
      needReturn:     bool ref,        (* Do we need to load regReturn in the prelude? *)
      printAssemblyCode:bool,            (* Whether to print the code when we finish. *)
      printStream:    string->unit    (* The stream to use *)
    };
 (* Invariant: the cached information in the justComeFrom list
     does *not* get incorporated into the cacheState until
     the branches are fixed up. This is so we can determine whether
     an instruction is only reachable by branches, in which case
     we may be able to perform the important null-jump optimisation.
     SPF 1/12/95
 *)

(*****************************************************************************)
(*                  Auxiliary functions on "code"                            *)
(*****************************************************************************)

  fun codeVec        (Code {codeVec,...})          = codeVec;
  fun ic             (Code {ic,...})               = ic;
  fun stackReset     (Code {stackReset ,...})      = stackReset;
  fun pcOffset       (Code {pcOffset,...})         = pcOffset;
  fun labelList      (Code {labelList,...})        = labelList;
  fun longestBranch  (Code {longestBranch,...})    = longestBranch;
  fun procName       (Code {procName,...})         = procName;
  fun otherCodes     (Code {otherCodes,...})       = otherCodes;
  fun mustCheckStack (Code {mustCheckStack,...})   = mustCheckStack;
  fun justComeFrom   (Code {justComeFrom,...})     = justComeFrom;
  fun cacheState     (Code {cacheState,...})       = cacheState;

  fun scSet (Set x) = x | scSet _ = raise Match;

  fun unreachable (Code {justComeFrom, cacheState, ...}) = 
    case (justComeFrom, cacheState) of
      (ref [], ref Unreachable) => true
    | _                         => false;

  (* Test for identity of the code segments by testing whether
     the pcOffset ref is the same. N.B. NOT its contents. *)
  infix is;
  fun a is b = (pcOffset a = pcOffset b);

  val codesize = 32; (* bytes. Initial size of segment. *)

  (* create and initialise a code segment *)
  fun codeCreate (noClosure, name, parameters) : code = 
  let
    val words = codesize div 4
  in
    Code
      { 
         codeVec          = csegMake words, (* a byte array *)
         ic               = ref addrZero,
         constVec         = ref [],
         numOfConsts      = ref 0,
         stackReset       = ref 0, (* stack adjustment in WORDs *)
         pcOffset         = ref 0, (* only non-zero after code is copied *)
         labelList        = ref [],
         longestBranch    = ref addrLast, (* None so far *)
         procName         = name,
         otherCodes       = ref [],
         resultSeg        = ref Unset,   (* Not yet done *)
         mustCheckStack   = ref false,
         justComeFrom     = ref [],
         selfCalls        = ref [],
         selfJumps        = ref [],
         noClosure        = noClosure,
         cacheState       = ref (Reachable true),
         needReturn       = ref false,
        printAssemblyCode = DEBUG.getParameter DEBUG.assemblyCodeTag parameters,
        printStream    = PRETTY.getSimplePrinter parameters
      }
  end;
  
(*****************************************************************************)
(*                  Code for LR caching                                      *)
(*****************************************************************************)

  fun mergeCacheStateList (s1, []) = s1
  
    | mergeCacheStateList (s1, ref (Jump14From (_, s2)) :: rest) =
        mergeCacheStateList (mergeCacheStates (s1, s2), rest)

    | mergeCacheStateList (s1, ref (Jump24From (_, s2)) :: rest) =
        mergeCacheStateList (mergeCacheStates (s1, s2), rest);

  (* Called when the preceding instruction causes an unconditional
     transfer of control. Used to reset any caching mechanisms (currently
     only for LR caching). SPF 30/11/95
  *)
  fun cancelFallThrough (Code {cacheState, ...}) : unit =
    cacheState := Unreachable;

  (* Called when control may reach the current point other than by fall-through.
     SPF 30/11/95
  *)
  (* If we clear the LR cache we need to load the return register in the prelude. *)
  fun clearAllCaches (Code {cacheState, needReturn, ...}) : unit =
    (cacheState := Reachable false; needReturn := true);

(* We never actually modify the link-register cache on its own
  fun clearLRCache (Code {cacheState, ...}) : unit =
    (cacheState := LRmodified (!cacheState); needReturn := true);
*)
  
  fun branchInPoint (Code {cacheState, justComeFrom, ...}) : unit =
    cacheState := mergeCacheStateList (!cacheState, !justComeFrom);
  
  (* Does LR contain a usable return address? We mustn't forget to check
     all the jumps-in that we haven't fixed up yet. SPF 30/11/95 *)
  fun returnAddrIsCached (Code {cacheState, justComeFrom, ...}) : bool =
    LRcacheActive (mergeCacheStateList (!cacheState, !justComeFrom));

               
(*****************************************************************************)
(*                  Getting and setting quads                                *)
(*****************************************************************************)

  (* Put 4 bytes at a given offset in the segment. *)
  (* Write out high order bytes followed by low order.
     Assume all arguments are +ve or zero. *)
  fun setQuad (Quad (b1,b2,b3,b4), addr:addrs, seg:cseg) : unit =
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
  fun genCodeQuad (instr : quad, cvec : code) : unit =
  let
    val addr = ! (ic cvec)
  in  
    setQuad (instr, addr, codeVec cvec);
    ic cvec := addr wordAddrPlus 1
  end;

  fun getCodeQuad (addr : addrs, cvec : code) : quad =
  let
    val seg = codeVec cvec;
  in
    getQuad (addr, seg)
  end;

  fun setCodeQuad (instr : quad, addr : addrs, cvec : code) : unit =
  let
    val seg = codeVec cvec;
  in
    setQuad (instr, addr, seg)
  end;

(*****************************************************************************)
(*                  Functions dealing with constants                         *)
(*****************************************************************************)

  (* Make a reference to another procedure. Usually this will be a forward *)
  (* reference but it may have been compiled already, in which case we can *)
  (* put the code address in now. *)
  fun codeConst (Code {resultSeg = ref(Set seg), ... }, _) =
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

(*****************************************************************************)
(*                  Functions dealing with labels                            *)
(*****************************************************************************)
(* 
   The cvec holds a list of short branches so that they can be extended
   to long branches before they go out of range. If we fix up a
   short branch, we must call "removeLabel" to purge it from this list.
   To keep things simple, we call "removeLabel" whenever we fix up
   a jump - if the label is long, or if it doesn't appear in the list
   (which is the case for branhes backwards), we just won't find it
   in the list. SPF 21/9/95
*)
  fun removeLabel (lab : addrs, cvec) : unit = 
  let
    fun removeEntry ([]: labList) : labList = []
      | removeEntry ((ref (Jump24From _)) :: t) =
          removeEntry t (* we discard all long jumps *)
        
      | removeEntry ((entry as ref (Jump14From (addr,_))) :: t) =
        if lab addrEq addr
        then removeEntry t
        else let
          val () =
            if addr addrLt !(longestBranch cvec)
            then longestBranch cvec := addr
            else ();
        in    
          entry :: removeEntry t
        end;
  in
    (* We recompute the longest 14-bit branch. *)
    longestBranch cvec := addrLast;
    labelList cvec     := removeEntry (! (labelList cvec))
  end;

  (* Makes a new short label. Called immediately after
     the jump has been generated, which accounts for the ~1. *)
  fun makeShortLabel (cvec : code) : jumpFrom ref =
  let
    val here : addrs = ! (ic cvec) wordAddrPlus ~1;
    val lab  = ref (Jump14From (here, !(cacheState cvec)));
  in
    if here addrLt !(longestBranch cvec)
    then longestBranch cvec := here
    else ();
    
    labelList cvec := lab :: !(labelList cvec);
    
    lab
  end;

  (* Makes a new long label. Called immediately after
     the jump has been generated, which accounts for the ~1. *)
  fun makeLongLabel (cvec : code) : jumpFrom ref =
  let
    val here  : addrs = ! (ic cvec) wordAddrPlus ~1;
    val lab = ref (Jump24From (here, !(cacheState cvec)));
  in
    (* longestBranch only refers to *short* branches *)
    lab
  end;

  fun getCallAddrs (cvec : code) : addrs =
  let
    val here = ! (ic cvec) wordAddrPlus ~1;
  in
    here
  end;

  (* fix up a call *)
  fun reallyFixCall (addr : addrs, target : addrs, cvec : code) : unit =
  let
    (* Compute offset in words, not bytes *)
    val diff     : int  = target wordAddrMinus addr;
    
    val diff24   : int24 =
      if is24Bit diff
      then int24 diff
      else raise InternalError "call too far (24-bit offset)"
    
    val oldInstr : quad = getCodeQuad (addr, cvec);
    val newInstr : quad = fixupUncondCall (oldInstr, diff24);
  in
    setCodeQuad (newInstr, addr, cvec)
  end
  
  fun reallyFixupBranch (Jump14From (addr, _), target : addrs, cvec : code) : unit =
  let
    (* Compute offset in words, not bytes *)
    val diff     : int  = target wordAddrMinus addr;
    
    val diff14   : int14 =
      if is14Bit diff
      then int14 diff
      else raise InternalError "jump too far (14-bit offset)"
    
    (* 
       We're about to fix up the jump, so remove it from the
       list of pending short jumps.
     *)
    val () = removeLabel (addr, cvec);

    val oldInstr : quad = getCodeQuad (addr, cvec);
    val newInstr : quad = fixupCondBranch (oldInstr, diff14);
  in
    setCodeQuad (newInstr, addr, cvec)
  end
       
    | reallyFixupBranch (Jump24From (addr, _), target : addrs, cvec : code) : unit =
  let
    (* Compute offset in words, not bytes *)
    val diff     : int  = target wordAddrMinus addr;
    
    val diff24   : int24 =
      if is24Bit diff
      then int24 diff
      else raise InternalError "jump too far (24-bit offset)"
    
    val oldInstr : quad = getCodeQuad (addr, cvec);
    val newInstr : quad = fixupUncondBranch (oldInstr, diff24);
  in
    setCodeQuad (newInstr, addr, cvec)
  end;
 
  fun reallyFixupBranches (target : addrs, l : labels, cvec : code) : unit =
    List.app (fn (ref x) => reallyFixupBranch (x, target, cvec)) l;

  (* Recursive branches can be treated just like ordinary branches. *)
  val fixupRecursiveBranches = reallyFixupBranches;

  fun fixupRecursiveCalls (target : addrs, l : addrs list, cvec : code) : unit =
    List.app (fn (x : addrs) => reallyFixCall (x, target, cvec)) l;

  (* Deal with a pending fix-up. *)
  fun reallyFixup cvec = 
    let
      val jcf  = justComeFrom cvec;
      val here = ! (ic cvec);
    in
      case ! jcf of
        []   => ()
      | labs => 
        (
          branchInPoint cvec;
          reallyFixupBranches (here, labs, cvec);
          jcf := []
        )
    end;

(*****************************************************************************)
(*                  Code-generation functions (1)                            *)
(*****************************************************************************)
 
  (*
     At the moment there are two kinds of operations that can be left pending 
     and are not actually generated immediately. Changing the real stack
     pointer is not generated immediately because often these accumulate and
     sometimes even cancel out. Fixing up a branch is not done immediately in
     case we are going to jump somewhere else. If both the branch and stack
     movement are deferred the branch is assumed to have happened first.
  *)

 (* 
     Generate an instruction. This may involve actually fixing up
     any pending branches whose target is immediately prior to 
     the current instruction. It doesn't fix up the stack,
     so it mustn't be used naively. It *does* fix up branches-in,
     so it mustn't be used for branch-out instructions either.
     Also, it doesn't check for pending-jump overflow.
  *)
  fun genRawInstruction (instr : quad, cvec : code) : unit =
    if unreachable cvec then ()
    else let
      val () = reallyFixup cvec;
    in
      genCodeQuad (instr, cvec)
    end;
      
  fun genRawInstructionList (l : quad list, cvec : code) : unit =
    List.app (fn (x : quad) => genRawInstruction (x, cvec)) l;
  
  (* This must use the raw instruction generator, because it's
     used by "fixup", which is called from "checkBranchList, which
     is used by genInstruction. We just have to set maxDiff
     in checkBranchList sufficiently conservatively (small) so that any
     stack adjustment caused by fixup doesn't cause a pending-jump overflow.
  *)
  fun genPendingStackAdjustment cvec : unit =
  let
    val sr  = stackReset cvec;
    val adj = !sr * 4;
    val () = sr := 0;
  in
    genRawInstructionList (addImmed (regStackPtr, regStackPtr, adj), cvec)
  end;

  (* called immediately before a trap instruction is generated *)
  fun genTrapCacheFlush cvec : unit =
    genPendingStackAdjustment cvec;
  
  
(*****************************************************************************)
(*              Functions that implement jump chaining.                      *)
(*****************************************************************************)
  (* Apparently fix up jumps - actually just record where we have come from *)
  fun fixup (lab : labels, cvec : code) : unit =
  let
    (* 
       If the jump we are fixing up is immediately preceding
       we can remove it. This is only safe if we've exited, as
       otherwise the current instruction might be the target of a
       jump backwards, or it might be the start of a handler.
       N.B. this *can* happen if the body of the handler is trivial,
       so that it just falls through to the post-handler code.
       I discovered this the hard way, of course. SPF 27/6/95.
       
       Note: now we've introduced LR caching, we have to merge the
       LR cache state of the label into the current LR cache state.
       (Otherwise we would get problems if we had LR uncached, did an
       unconditional jump, set the LR state cached because the code is
       unreachable then fixed up the jump to arrive at the code.)
       SPF 30/11/95 
       
       I've now delayed the state merging, so null-jump optimisation
       is back on the agenda! (The former version merged the states
       too early, so we couldn't determine whether we had "exited".)
       SPF 1/12/95
     *)
    fun checkLabs []            = []
      | checkLabs (lab :: labs) =
      let
        val icRef : addrs ref = ic cvec;
      in
        case !lab of
          Jump14From  (addr, cachedState) =>
            if !icRef wordAddrMinus addr = 1
            then let
              (* simply skip back one instruction *)
              val () = icRef := addr;
              (* remove the label from the "pending jumps" list *)
              val () = removeLabel (addr, cvec);
              (* update the cached state, to reflect the dummy fixup *)
              val () = 
                cacheState cvec := 
                  mergeCacheStates (!(cacheState cvec), cachedState);
            in
              labs
            end
            else lab :: checkLabs labs
            
        | Jump24From (addr, cachedState) =>
            if !icRef wordAddrMinus addr = 1
            then let
              (* simply skip back one instruction *)
              val () = icRef := addr;
              (* update the cached state, to reflect the dummy fixup *)
              val () = 
                cacheState cvec := 
                  mergeCacheStates (!(cacheState cvec), cachedState);
            in
              labs
            end
            else lab :: checkLabs labs
      end;
        
    (* We can't perform the null-jump optimisation if we haven't exited. *)
    fun checkLabsCarefully l =
      case ! (cacheState cvec) of
        Unreachable => checkLabs l
      | _           => l;
  in
    case lab of
      [] => () (* we're not actually jumping from anywhere *)
    | _ =>
       let
         (* Any pending stack reset must be done now.
            That may involve fixing up pending jumps. *)
         val () = genPendingStackAdjustment cvec;
       in
        (* Add together the jumps to here, updating the cache state *)
        justComeFrom cvec := checkLabsCarefully lab @ !(justComeFrom cvec)
      end
  end;

(*****************************************************************************)
(*              checkBranchList: do we need to extend any short jumps?       *)
(*****************************************************************************)
  (* 
     If the longest branch is close to going out of range it must
     be converted into a long form. This occurs very rarely - how
     often does a conditional branch exceed a 2^13 words displacement?
     Perhaps we should reduce maxDiff just to test the code?
   *)
  fun checkBranchList (needed : int, cvec:code) : unit =
  let
    (* val maxDiff = 100; for testing purposes *)
    val maxDiff : int = (exp2_13 - 1000) - needed;
    
    fun inList _ []     = false
      | inList x (h::t) = (x = h) orelse inList x t;
 
   (* Go down the list converting any long labels, and finding the
      longest remaining. *)
    fun convertLabels ([]:labList) : labList = []
      | convertLabels (lab::labs) =
      (* If we are about to fix up this label there's no point
          in extending it. *)
       if inList lab (! (justComeFrom cvec))
       then lab :: convertLabels labs
       else let
         (* Process the list starting at the end. The reason for this
            is that more recent labels appear before earlier ones.
            We must put the earliest labels in first because they may
            be about to go out of range. *)
          val convertRest = convertLabels labs;
       in
         (* Now do this entry. *)
         case !lab of
           Jump24From _ => (* shouldn't happen? *)
             convertRest
           
         | Jump14From (addr, cachedState) =>
           let
             val here : addrs = !(ic cvec);
           in
             if here wordAddrMinus addr > maxDiff
             then let (* Getting close - convert it. *)
               (* fix up the short branch to here *)
               val () = branchInPoint cvec;
               val () = reallyFixupBranch (!lab, here, cvec);
                         
               (* recompute "here", in case we've generated some code *)
               val here : addrs = !(ic cvec);
               (* long jump to the final destination *)
               val () = genCodeQuad (branchAlwaysQuad, cvec);
               (* alter the jump state on the old label (and discard the new one) *)
               val () = lab := Jump24From (here, cachedState);
               (* We don't fall through from here. *)
               val () = cancelFallThrough cvec;
             in
               convertRest
             end
             else let
               (* Not ready to remove this. Just find out if
                  this is an earlier branch and continue. *)
               val () =
                 if addr addrLt !(longestBranch cvec)
                 then longestBranch cvec := addr
                 else ();
             in
               lab :: convertRest
             end
          end
       end (* convertLabels *);
    in
      if !(ic cvec) wordAddrMinus !(longestBranch cvec) > maxDiff (* rare *)
      then let
        (* Must save the stack-reset, otherwise "fixup" will try
           to reset it. *)
        val sr       = ! (stackReset cvec);
        val () = stackReset cvec := 0;
         
        (* Must skip round the branches unless we have just
           taken an unconditional branch. *)
        val lab = 
          if unreachable cvec then []
          else let
            val () = genRawInstruction (branchAlwaysQuad, cvec);
            val lab = [makeLongLabel cvec];
            val () = cancelFallThrough cvec;
          in
            lab
          end
          
        (* Find the new longest branch while converting the labels *)
        val () = longestBranch cvec := addrLast;
        val () = labelList cvec := convertLabels (! (labelList cvec));
        val () = fixup (lab, cvec); (* Continue with normal processing. *)
      in
        stackReset cvec := sr (* Restore old value. *)
      end
      else  ()
   end;

(*****************************************************************************)
(*                  Code-generation functions (2)                            *)
(*****************************************************************************)
  (* 
     genInstruction is like genRawInstruction, except that it
     checks for pending-jump overflow.
  *)
  fun genInstruction (instr : quad, cvec : code) : unit =
  let
    (* fix up any pending-overflow jumps *)
    val () = checkBranchList (0, cvec);
  in
    genRawInstruction (instr, cvec)
  end;
      
  fun genInstructionList (l : quad list, cvec : code) =
    List.app (fn (x : quad) => genInstruction (x, cvec)) l;

  (* These functions are stack-naive. *)
  fun genAddImmed (rt: reg, ra : reg, imm : int, cvec) =
    genInstructionList (addImmed (rt, ra, imm), cvec)

  fun genLoadImmed (rt: reg, imm : int, cvec) =
    genInstructionList (loadImmed (rt, imm), cvec)
 
(*****************************************************************************)
(* Functions that need to know about the stack-pointer adjustment cache.     *)
(*****************************************************************************)
 
  (* Adds in the reset. *)
  fun resetStack (offset, cvec) : unit =
    stackReset cvec := ! (stackReset cvec) + offset;
  
  (* genMove can be safely used with regStackPtr *)
  fun genMove (rt : reg, ra :reg, cvec) =
  let
    val adj = 
      if ra regEq regStackPtr
      then 4 * !(stackReset cvec)
      else 0;
      
    val () = 
      if rt regEq regStackPtr
        then stackReset cvec := 0
      else ();
  in
    genInstructionList (addImmed  (rt, ra, adj), cvec)
  end;

  (* Exported. Set a register to a particular offset in the stack. *)
  fun genStackOffset (reg, byteOffset, cvec) : unit = 
  let
    val adj = byteOffset + (! (stackReset cvec)) * 4;
  in
    genInstructionList (addImmed (reg, regStackPtr, adj), cvec)
  end;
  
(*****************************************************************************)
(*              Functions to generate (non-naive) branch instructions        *)
(*****************************************************************************)
  (* Generates an unconditional branch. *)
  fun unconditionalBranch (cvec : code) : labels =
    if unreachable cvec then []
    else let
      (* generate stack adjustment (if necessary) *)
      val () = genPendingStackAdjustment cvec;
 
       (*
          If we are branching and we have just arrived from somewhere
          else we can combine this jump with the one we had just made.
          If we had an unconditional branch just before, we don't need
          to actually put a branch. 
          Note that this can only happen if the stack adjustment is zero;
          otherwise the call to genPendingStackAdjustment will have
          already fixed up the jumps when it generated the stack-adjustment
          code. 
       *)
       
      (* Save the just-come-from list *)
      val oldLab = !(justComeFrom cvec);
       
      (* 
         Zap the original list, so we don't fix it up. 
         We don't merge the states here (though that would be safe);
         instead we'll eventually do it when we get to the target of
         the jumps.
         SPF 1/12/95
      *)
      val () = justComeFrom cvec := [];
     
      (* 
         The following code is very delicate. If we've just made the
         code unreachable (by redirecting the incoming jumps), we
         must *not* generate a new jump/address pair, since the
         lower-level routines would suppress the generation of the jump,
         but would still to fix it up later via its label.
         SPF 1/12/95
      *)
      val lab : labels = 
    if unreachable cvec
    then oldLab
        else let
      (* generate the actual branch *)
      val () = genInstruction (branchAlwaysQuad, cvec);
    in
      makeLongLabel cvec :: oldLab
    end;

      (* 
         This must be *after* we've generated the label, as otherwise
         the label would propagate the wrong cacheState forwards.
         SPF 1/12/95
      *)
      val () = cancelFallThrough cvec;
    in
      lab
    end;

  fun putConditional (test : testCode, cvec : code) : labels =
    if unreachable cvec then [] (* SPF 5/6/95 *)
    else let
      val () = genPendingStackAdjustment cvec; (* may generate code *)
      val () = genInstruction (condBranch (test, int14_0), cvec);
    in
      (* Make a label for this instruction. *)
      [makeShortLabel cvec]
    end;


(***************************************************************************  
  Functions for logical and arithmetic operations.
***************************************************************************)  
  
  (* Most of the arithmetic operations are of this form. *)
  (* Since they won't trap, we don't need to fix-up the stack;
     however, can we be sure that the stack-pointer isn't one
     of the operands? Let's check! SPF 16/5/95
     
     We now have to check whether we have to clear the RR cache too.
     SPF 1/1/95.
  *)
  fun genX31 (rt : reg, ra : reg, rb : reg, op2 : opCode31,
              rc : bool, cvec : code) : unit =
  let
    val () = 
      if rt regEq regStackPtr orelse
         ra regEq regStackPtr orelse
         rb regEq regStackPtr
      then genPendingStackAdjustment cvec
      else ();

    (* Get the 5-bit representation of the registers. *)
    val rt'  : int5  = getReg5 rt;  
    val ra'  : int5  = getReg5 ra;  
    val rb'  : int5  = getReg5 rb;
    val op2' : int10 = op31ToInt10 op2
  in
    genInstruction (XformQuad (EXT31, rt', ra', rb', op2', rc), cvec)
  end;
  
  (* The "logical" instructions have the fields in a funny order *)
  fun genAnd (ra : reg, rs : reg, rb : reg, cvec : code) : unit =
    genX31 (rs, ra, rb, AND, false, cvec);
    
  fun genOr (ra : reg, rs : reg, rb : reg, cvec : code) : unit =
    genX31 (rs, ra, rb, OR, false, cvec);
    
  fun genXor (ra : reg, rs : reg, rb : reg, cvec : code) : unit =
    genX31 (rs, ra, rb, XOR, false, cvec);
    
  (* The arithmetic instructions don't. *) 
  fun genAdd (rt : reg, ra : reg, rb : reg, cvec : code) : unit =
    genX31 (rt, ra, rb, ADD, false, cvec);
    
  (* We have a subtract-from instruction, not a straight subtract. *) 
  fun genSubfc (rt : reg, ra : reg, rb : reg, cvec : code) : unit =
    genX31 (rt, ra, rb, SUBFC, false, cvec);
    
  (* These versions set the condition codes, including SO *)
  fun genAddoDot (rt : reg, ra : reg, rb : reg, cvec : code) : unit =
    genX31 (rt, ra, rb, ADDO, true, cvec);
    
  fun genSubfcoDot (rt : reg, ra : reg, rb : reg, cvec : code) : unit =
    genX31 (rt, ra, rb, SUBFCO, true, cvec);
    
  fun genCmp (ra : reg, rb : reg, cvec : code) : unit =
    genX31 (regZero, ra, rb, CMP, false, cvec);

  fun genCmpl (ra : reg, rb : reg, cvec : code) : unit =
    genX31 (regZero, ra, rb, CMPL, false, cvec);


(***************************************************************************  
  Moves to and from special registers.
***************************************************************************)  
  fun genMtlr  (rs : reg, cvec : code) : unit = 
    genInstruction (mtlr rs, cvec);
    
  fun genMtctr (rs : reg, cvec : code) : unit =
    genInstruction (mtctr rs, cvec);
    
(* ...
  fun genMfctr (rt : reg, cvec : code) : unit =
    genInstruction (mfctr rt, cvec);
.... *)
    
(***************************************************************************  
  Load and Store operations.
***************************************************************************)  
  
  (* 
     The order of the parameters for "genLoad" and "genStore" is fixed
     by the generic interface, which makes it rather unnatural for
     this machine. C'est la vie. 
   *)
     
  (* genLoad corrupts regTemp1 *)
  fun genLoad (offset : int, ra : reg, rt : reg, cvec : code) : unit =
    if rt regEq regTemp1 orelse ra regEq regTemp1
    then raise InternalError ("genLoad: can't use " ^ regRepr regTemp1)
    else let
      (* Do we need to fix-up the stack pointer? *)
      val () = 
    if rt regEq regStackPtr
      then genPendingStackAdjustment cvec
      else ();
    
      val adjustedOffset : int = 
    if ra regEq regStackPtr
    then offset + 4 * !(stackReset cvec)
    else offset;
    in
      if is16Bit adjustedOffset
      then genInstruction (lwz (rt, ra, int16 adjustedOffset), cvec)
      else
    (
      genLoadImmed (regTemp1, adjustedOffset, cvec);
      genInstruction (lwzx (rt, ra, regTemp1), cvec)
    )
    end;

  datatype storeWidth = STORE_WORD | STORE_BYTE

  (* genStore corrupts regTemp1 *)
  fun genStoreWord (rs : reg, offset : int, ra : reg, index: reg, cvec : code) : unit =
    if rs regEq regTemp1 orelse ra regEq regTemp1
    then raise InternalError ("genStore: can't use " ^ regRepr regTemp1)
    else let
      (* Do we need to fix-up the stack pointer? *)
      val () = 
        if rs regEq regStackPtr
        then genPendingStackAdjustment cvec
        else ();
    
      val adjustedOffset : int = 
        if ra regEq regStackPtr
        then offset + 4 * !(stackReset cvec)
        else offset
    in
      if index regNeq regNone
      then
        (
          if adjustedOffset <> 0 
          then raise InternalError "genStore: index with non-zero offset"
          else ();
          (* Shift to form word index, masking off the tag bits. *)
          genInstruction (rlwinm (regTemp1, index, 2 - TAGBITS, 0, 31-2), cvec);
          genInstruction (stwx (rs, ra, regTemp1), cvec)
        )
      else if is16Bit adjustedOffset
      then
        (* If we're pushing onto the stack we can update the stack pointer at the
           same time.  DCJM 22/12/00 *)
        if ra regEq regStackPtr andalso offset = 0 andalso index regEq regNone
        then
            (
            genInstruction (stwu(rs, ra, int16 adjustedOffset), cvec);
            stackReset cvec := 0
            )
        else genInstruction (stw(rs, ra, int16 adjustedOffset), cvec)
      else
        (
          genLoadImmed (regTemp1, adjustedOffset, cvec);
          genInstruction (stwx (rs, ra, regTemp1), cvec)
        )
    end;

  (* genStoreByte corrupts regTemp1 and regTemp2 *)
  fun genStoreByte (rs : reg, ra : reg, offset : int, index: reg, cvec : code) : unit =
    if rs regEq regTemp1 orelse ra regEq regTemp1
    then raise InternalError ("genStoreByte: can't use " ^ regRepr regTemp1)
    else let
      (* Do we need to fix-up the stack pointer? *)
      val () = 
    if rs regEq regStackPtr
    then genPendingStackAdjustment cvec
    else ();
    
      val adjustedOffset : int = 
    if ra regEq regStackPtr
    then offset + 4 * !(stackReset cvec)
    else offset;
    in
      (* Untag the value to store. *)
      genInstruction (srawi (regTemp2, rs, TAGBITS), cvec);
      if index regNeq regNone
      then
     (
      if adjustedOffset <> 0 
      then raise InternalError "genStoreByte: index with non-zero offset"
      else ();
      (* Untag the index. *)
      genInstruction (srawi (regTemp1, index, TAGBITS), cvec);
      genInstruction (stbx (regTemp2, ra, regTemp1), cvec)
     )
      else if is16Bit adjustedOffset
      then genInstruction (stb (regTemp2, ra, int16 adjustedOffset), cvec)
      else
    (
      genLoadImmed (regTemp1, adjustedOffset, cvec);
      genInstruction (stbx (rs, ra, regTemp1), cvec)
    )
    end;

  (* General purpose store function. *)
  fun genStore (rs : reg, offset : int, ra : reg, STORE_WORD, index: reg, cvec : code) =
        genStoreWord (rs, offset, ra, index, cvec) 
  |   genStore(rs : reg, offset : int, ra : reg, STORE_BYTE, index: reg, cvec : code) =
        genStoreByte (rs, ra, offset, index, cvec);

  (* Inline assignments since the RTS now supports it. *)
  val inlineAssignments: bool = true
  fun isIndexedStore _ = true (* for both word and byte. *)
 
  (* Exported - Can we store the value without going through a register?
     No. *)
  fun isStoreI _ : bool = false;

  fun genStoreI _ : unit =
    raise InternalError "Not implemented: genStoreI";

  (* Store a value on the stack.  This is used when the registers need to be
     saved, for more than 4 arguments or to push an exception handler. *)
  fun genPush (r, cvec) : unit =
  let
    (* Just adjust stackReset to decrement the virtual stack pointer;
       genStore handles everything else. *)
    val sr = stackReset cvec;
    val () = sr := !sr - 1;
  in
    genStoreWord (r, 0, regStackPtr, regNone, cvec) (* corrupts regTemp1 *)
  end;

  (* Load a value and push it on the stack. Used when all
     the allocatable registers have run out. *)
  (* Corrupts regTemp1 and regZero. *)
  fun genLoadPush (offset : int, base : reg, cvec : code) : unit =
    (
      genLoad (offset, base, regZero, cvec); (* corrupts regTemp1 *)
      genPush (regZero, cvec)                (* corrupts regTemp1 *)
    );

  (* This is false because there's no quicker way than loading
     into a register and then pushing that. *) 
  val preferLoadPush = false;

(***************************************************************************  
  Functions for handling compiled constants
***************************************************************************)  

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
         reallyFixup cvec; (* Probably not needed on PPC but can't hurt. *)
         numOfConsts := ! numOfConsts + 1;
         (* Generate a pair of instruction containing tagged 0.  We
            mustn't put in the real value at the moment because this
            is a byte segment and so the value won't be updated as a
            result of any garbage collection. *)
         let
            val (lo,hi) = splitSignedInt(tagged 0)
         in
           (* The g.c. and set_code_constant need to be able to combine
              the upper and lower halves of the constant. To simplify this
              make sure that we don't fix up a branch between the two.  Also
              SetCodeConstant in the RTS assumes that the "ic" value points
              at an addis instruction and not a branch. *)
            checkBranchList (2, cvec);
            (* Record the address after any branch. *)
            constVec := (cnstnt, !ic) :: ! constVec;
            genInstruction (addis (destR, regZero, hi), cvec);
            genInstruction (ori (destR, destR, lo), cvec)
         end
      );

  fun genLoadCoderef (rf : code, destR, cvec) : unit =
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

  fun fixupHandler (lab, cvec) : unit =
  ( 
    genPendingStackAdjustment cvec; 
    clearAllCaches cvec;
    reallyFixup cvec;
    lab := ! (ic cvec)
  );

(***************************************************************************  
  Functions calls and returns (plus raising exceptions)
***************************************************************************)  
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

  (* Call a function. We have to set the stack-check flag 
       to ensure that the called procedure receives its full
       minStackCheck words allocation of "free" stack. *)
  fun callFunction (callKind,
            cvec as Code {selfCalls, mustCheckStack, ... }) : unit =
      (* Mustn't add to selfCalls list unless we're actually generating code! *)
      if unreachable cvec then ()
      else
      (
      case callKind of 
        Recursive =>  (* bl L3 *)
        (
          genPendingStackAdjustment cvec;
          genInstruction (call0Quad, cvec);
          selfCalls := getCallAddrs cvec :: ! selfCalls
        )
          
      | FullCall =>  (* lwz regZero,0(regClosure); mtlr regZero; blrl *)
        (
          genLoad (0, regClosure, regZero, cvec);
          genMtlr (regZero, cvec);
          genPendingStackAdjustment cvec;
          genInstruction (blrlQuad, cvec)
        )

      | ConstantFun(w, false) =>
         (
          genLoadConstant (WVal w, regZero, cvec);
          genMtlr (regZero, cvec);
          genPendingStackAdjustment cvec;
          genInstruction (blrlQuad, cvec)
         )

      | ConstantFun(w, true) =>
        ( (* Indirect call.  Used to call the RTS. *)
          genLoadConstant (WVal w, regClosure, cvec);
          genLoad (0, regClosure, regZero, cvec);
          genMtlr (regZero, cvec);
          genPendingStackAdjustment cvec;
          genInstruction (blrlQuad, cvec)
        )
      
      | CodeFun c =>
          (
          genLoadCoderef (c, regZero, cvec);
          genMtlr (regZero, cvec);
          genPendingStackAdjustment cvec;
          genInstruction (blrlQuad, cvec)
          );
        
      clearAllCaches cvec; (* Clear the cache *)
      mustCheckStack := true
      );

    (* Call, rather than jump to, the exception code so that we have
       the address of the caller if we need to produce an exception
       trace.  Unlike most RTS calls we call without an indirection. *)
    (* TODO: callFunction will cause us to generate a stack check even
       if we don't actually need it. *)
    fun raiseException cvec =
        (
         genLoad(MemRegisterRaiseException, regMemRegs, regZero, cvec);
         genMtlr(regZero, cvec);
         genInstruction(blrlQuad, cvec);
         (* That's the end of this basic block,
            even though we've "called" the handler *)
         cancelFallThrough cvec
        )

  
  (* Tail recursive jump to a function. We have to set the stack-check
     flag to enable the user to break out of loops. Exception: (hack!)
     we don't have to do this if we are calling a pre-compiled function
     (PureCode) because that can't possibly lead to an infinite regress. *)
  fun jumpToFunction (callKind, returnReg,
            cvec as Code{selfJumps, mustCheckStack, ...}) : unit =
    (* Mustn't add to selfJumps list unless we're actually generating code! *)
    if unreachable cvec then ()
    else
    (
    case callKind of
      Recursive =>   (* mr regReturn,returnReg; mtlr returnReg; b L4 *)
        (
          mustCheckStack := true;
          (* initialise regReturn and LR *)
          if regReturn regEq returnReg
          then ()
          else genMove (regReturn, returnReg, cvec);
          if returnAddrIsCached cvec then () else genMtlr (returnReg, cvec);
          genPendingStackAdjustment cvec;
          selfJumps := unconditionalBranch cvec @ !selfJumps
        )
        
      | FullCall =>    (* lwz regZero,0(regClosure); mtctr regZero;  mtlr returnReg; bctr *)
        (
          mustCheckStack := true;
          genLoad  (0, regClosure, regZero, cvec);
          genMtctr (regZero, cvec);
          if returnAddrIsCached cvec then () else genMtlr (returnReg, cvec);
          genPendingStackAdjustment cvec;
          genInstruction (bctrQuad, cvec)
        )

     
      | ConstantFun(w, false) => (* mtctr regZero; mtlr returnReg; bctr *)
        (
          mustCheckStack := true;
          genLoadConstant (WVal w, regZero, cvec);
          genMtctr (regZero, cvec);
          if returnAddrIsCached cvec then () else genMtlr (returnReg, cvec);
          genPendingStackAdjustment cvec;
          genInstruction (bctrQuad, cvec)
        )

      | ConstantFun(w, true) =>
        ( (* Indirect jumps are used to call into the RTS.  No need
             to check the stack. *)
          genLoadConstant (WVal w, regClosure, cvec);
          genLoad (0, regClosure, regZero, cvec);
          genMtctr (regZero, cvec);
          if returnAddrIsCached cvec then () else genMtlr (returnReg, cvec);
          genPendingStackAdjustment cvec;
          genInstruction (bctrQuad, cvec)
        )

      | CodeFun c =>
        (
          mustCheckStack := true;
          genLoadCoderef (c, regZero, cvec);
          genMtctr (regZero, cvec);
          if returnAddrIsCached cvec then () else genMtlr (returnReg, cvec);
          genPendingStackAdjustment cvec;
          genInstruction (bctrQuad, cvec)
        );

      (* That's the end of this basic block *)
      cancelFallThrough cvec
    );

  (* Return and remove args from stack. *)
  fun returnFromFunction (returnReg, args, cvec) : unit =
  let  (* mtlr resReg; blr *)
    val () = resetStack (args, cvec); (* Add in the reset. *)
    val () =
      if returnAddrIsCached cvec then () else genMtlr (returnReg, cvec);
    val () = genPendingStackAdjustment cvec;
    val () = genInstruction (blrQuad, cvec);
  in
    (* That's the end of this basic block *)
    cancelFallThrough cvec
  end;

  (* Only used for while-loops. *)
  fun jumpback (lab, stackCheck, cvec) : unit =
  let
    val ()     = genPendingStackAdjustment cvec;
    
    (* Put in a stack check. This is used to allow
       the code to be interrupted. *)
    val () =
      if stackCheck
      then
         let
            val () = genLoad(MemRegisterStackLimit, regMemRegs, regZero, cvec);
            val () = genCmpl(regStackPtr, regZero, cvec);
            val skipTrap = putConditional (GeInv, cvec)
         in
            genLoad(MemRegisterStackOverflow, regMemRegs, regZero, cvec);
            genMtlr(regZero, cvec);
            genInstruction(blrlQuad, cvec);
            fixup(skipTrap, cvec);
            clearAllCaches cvec
         end
      else ();
    val ()     = reallyFixup cvec;
    val wordOffset : int = lab wordAddrMinus (! (ic cvec));
    val ()     = genInstruction (uncondBranch (int24 wordOffset), cvec)
  in
    cancelFallThrough cvec
  end;

  (* Allocate store and put the resulting pointer in the result register. *)
  (* Corrupts regTemp1 and regTemp2 *)
  fun allocStore (length : int, flag : Word8.word,
          resultReg : reg, cvec : code) : unit =
    if length < 1 orelse exp2_24 <= length
    then raise InternalError "allocStore: bad length"
    else let
        val bytes : int      = (length + 1) * 4;
        val lengthWord : int = length + (Word8.toInt flag * exp2_24);
        val ()         = genPendingStackAdjustment cvec;
        (* Load r12 with the length word first.  This simplifies recovering if we
           get a trap since we then know how much space we actually wanted. *)
        val () = genLoadImmed(regTemp2, lengthWord, cvec);
        (* addi rhp, rhp, -bytes ; cmpl rhp,rhl; bge+ L1 *)
        val () = genInstructionList (addImmed (regHeapPtr,  regHeapPtr, ~bytes), cvec)
        val () = genCmpl (regHeapPtr, regHeapLim, cvec);
        val skipTrap = putConditional (GeInv, cvec)
      in
        genLoad(MemRegisterHeapOverflow, regMemRegs, regZero, cvec);
        genMtlr(regZero, cvec);
        genInstruction(blrlQuad, cvec);
        fixup(skipTrap, cvec);
        clearAllCaches cvec; (* If we've taken the trap LR won't be valid. *)
        genAddImmed(resultReg, regHeapPtr, 4, cvec);
        genInstruction(stw(regTemp2, regHeapPtr, int16_0), cvec)
    end; (* allocStore *)
  
  (* Remove the mutable bit; only safe for word objects. *)
  (* corrupts regTemp1 and regTemp2 *)
  fun setFlag (baseReg : reg, cvec : code, flag : Word8.word) : unit =
  let
    val flagRep : int = Word8.toInt flag;
  in
    genLoadImmed (regTemp2, flagRep, cvec);
    genStoreByte (regTemp2, baseReg, ~4, regNone, cvec) (* corrupts regTemp1 *)
  end

  (* Don't need to do anything on this machine. *)
  val completeSegment = (fn _ => ());

(***************************************************************************  
  General operations
***************************************************************************)
  datatype instrs = 
    InstrMove
  | InstrAddA
  | InstrSubA
  | InstrRevSubA
  | InstrMulA
  | InstrAddW (* Added and reinstated word functions DCJM 17/4/00. *)
  | InstrSubW
  | InstrRevSubW
  | InstrMulW
  | InstrDivW
  | InstrModW
  | InstrOrW
  | InstrAndW
  | InstrXorW
  | InstrLoad
  | InstrLoadB
  | InstrVeclen
  | InstrVecflags
  | InstrPush  (* added 12/9/94 SPF for v2.08 compiler *)
  | InstrUpshiftW    (* logical shift left *)
  | InstrDownshiftW  (* logical shift right *)
  | InstrDownshiftArithW  (* arithmetic shift right *)
  | InstrGetFirstLong
  | InstrStringLength
  | InstrSetStringLength
  | InstrBad;
   
  (* Can the we use the same register as the source and destination
     of an instructions? On this machine - yes. *)
  val canShareRegs : bool = true;

  (* exported versions *)
  val instrMove       = InstrMove;
  val instrAddA       = InstrAddA;
  val instrSubA       = InstrSubA;
  val instrRevSubA    = InstrRevSubA;
  val instrMulA       = InstrMulA;
  val instrAddW       = InstrAddW;
  val instrSubW       = InstrSubW;
  val instrMulW       = InstrMulW;
  val instrDivW       = InstrDivW;
  val instrModW       = InstrModW;
  val instrRevSubW    = InstrRevSubW;
  val instrOrW        = InstrOrW;
  val instrAndW       = InstrAndW;
  val instrXorW       = InstrXorW;
  val instrLoad       = InstrLoad;
  val instrLoadB      = InstrLoadB;
  val instrVeclen     = InstrVeclen;
  val instrVecflags   = InstrVecflags;
  val instrPush       = InstrPush;
  val instrUpshiftW   = InstrUpshiftW;
  val instrDownshiftW = InstrDownshiftW;
  val instrDownshiftArithW = InstrDownshiftArithW;
  val instrGetFirstLong = InstrGetFirstLong;
  val instrStringLength = InstrStringLength;
  val instrSetStringLength = InstrSetStringLength
  val instrBad        = InstrBad;

  datatype tests =
    Short
  | Long
  | Arb of testCode
  | Wrd of testCode;
  
  val testNeqW  = Wrd Ne;
  val testEqW   = Wrd Eq;
  val testGeqW  = Wrd Ge;
  val testGtW   = Wrd Gt;
  val testLeqW  = Wrd Le;
  val testLtW   = Wrd Lt;
  val testNeqA  = Arb Ne;
  val testEqA   = Arb Eq;
  val testGeqA  = Arb Ge;
  val testGtA   = Arb Gt;
  val testLeqA  = Arb Le;
  val testLtA   = Arb Lt;

(***************************************************************************  
  Auxilariary functions
***************************************************************************)  

  (* Test a single argument and trap if it is long.  The result is
     the instruction address of the trap, and is used to jump back to
     if the instruction overflows. *)
  fun genTagTest1 (r : reg, cvec : code) : addrs =
  let
    (* 
        slwi.    r0,r,31 - extract tag bit
        bne+     lab1
        lab:
        lwz      r0,16(r13)
        mtlr     r0
        blrl
        lab1:
    *)

    val ()    = genInstruction (slwiDot  (regZero, r, 31), cvec);
    (* Jump round the trap.  We invert the prediction flag here because the
       default for a forward jump is "not taken" and this normally will be. *)
    val skipTrap    = putConditional (NeInv, cvec)
    val ()    = genTrapCacheFlush cvec;
    val lab : addrs = ! (ic cvec);
    val () = genLoad(MemRegisterArbEmulation, regMemRegs, regZero, cvec);
    val () = genMtlr(regZero, cvec);
    val () = genInstruction(blrlQuad, cvec);
    val () = fixup(skipTrap, cvec);
    val () = clearAllCaches cvec; (* If we've taken the trap LR won't be valid. *)
  in
    lab
  end;

  (* Test a pair of arguments and trap if either is long.  The result is
     the instruction address of the trap, and is used to jump back to
     if the instruction overflows. *)
  fun genTagTest2 (rx : reg, ry : reg, cvec : code) : addrs =
    if rx regEq ry
    then genTagTest1 (rx, cvec)
    else let
      val () = genAnd (regTemp1, rx, ry, cvec);
    in
      genTagTest1 (regTemp1, cvec)
    end;

  fun genOverflowTest (lab : addrs, cvec : code) : unit =
  let
    val here : addrs = ! (ic cvec);
    val wordOffset : int = lab wordAddrMinus here;
  in
    (* Jump back to the RTS trap in the tag test code.  We need to
       invert the prediction test here because the default for a
       backwards jump is to assume it is taken and this normally
       won't be. *)
    genInstruction (condBranch (SoInv, int14 wordOffset), cvec)
  end; (* genOverflowTest *)


  
(***************************************************************************  
  RI implementation of comparisons
***************************************************************************)  

  (* All are implemented. *)
  fun isCompRR _ = true;


  (* Is this argument acceptable as an immediate or should it be *)
  (* loaded into a register? *) 
  fun isCompRI (tc, cnstnt:machineWord) : bool =
    case tc of
      Short => true
    | Long  => true
    | Arb _ => isShort cnstnt andalso isTaggable16Bit (toInt (toShort cnstnt))
    | Wrd _ => isShort cnstnt andalso isTaggable16BitUnsigned (toInt (toShort cnstnt))
    ;

  fun genCmpi (ra : reg, SI : int16, cvec : code) : unit =
    genInstruction (cmpi (ra, SI), cvec);  

  fun genCmpli (ra : reg, UI : int16, cvec : code) : unit =
    genInstruction (cmpli (ra, UI), cvec);  
    

  (* Fixed and arbitrary precision comparisons. *)
  fun compareAndBranchRR (r1, r2, tc, cvec) : labels =
    case tc of
      Wrd test =>
      let
        val () = genCmpl (r1, r2, cvec); (* Word comparisons are unsigned. *)
      in
        putConditional (test, cvec)
      end
      
    | Arb test =>
      let
       (* 
          We generate the following code:
       
              and/or rtemp1, r1, r2
              slwiDot   rtemp1, rtemp1, 31
              bne+   L1
              lwz  r0,52(r13)
              mtlr r0
              blrl
              cmp    r1, r2
              bc<test> label
              
          If either (or, both, for an equality test) argument is long,
          then rtemp1 becomes zero, which means we take the
          trap which causes the RTS to emulate the immediately following
          comparison.
       *)  
        val () =
          case test of
            Eq => genOr  (regTemp1, r1, r2, cvec)
          | Ne => genOr  (regTemp1, r1, r2, cvec)
          | _  => genAnd (regTemp1, r1, r2, cvec);
        val () = genInstruction (slwiDot (regTemp1, regTemp1, 31), cvec);
        val () = genTrapCacheFlush cvec;
        val skipTrap = putConditional (NeInv, cvec)
        val () = genLoad(MemRegisterArbEmulation, regMemRegs, regZero, cvec);
        val () = genMtlr(regZero, cvec);
        val () = genInstruction(blrlQuad, cvec);
        val () = fixup(skipTrap, cvec);

        val () = genCmp (r1, r2, cvec);
        val () = clearAllCaches cvec; (* If we've taken the trap LR won't be valid. *)
      in
        putConditional (test, cvec)
      end
      
    | _ => 
       raise InternalError "compareAndBranchRR: Unimplemented test";
  
  
  fun compareAndBranchRI (r:reg, cnstnt:machineWord, tc, cvec) : labels =
    case tc of
      Short =>
      let
        val () = genInstruction (andiDot (regTemp1, r, int16_1), cvec);
      in
        (* jump if the result is non-zero *)
        putConditional (Ne, cvec)
      end
        
    | Long =>
      let
        val () = genInstruction (andiDot (regTemp1, r, int16_1), cvec);
      in
        (* jump if the result is zero *)
        putConditional (Eq, cvec)
      end
        
    | Wrd test =>
      let
        val c  : int   = toInt (toShort cnstnt);
        val ui : int16 = unsignedInt16 (tagged c);
        val ()  = genCmpli (r, ui, cvec); (* Word comparisons are unsigned. *)
      in
        putConditional (test, cvec)
      end
    
    | Arb test =>
      let
        val c  : int   = toInt (toShort cnstnt);
        val si : int16 = int16 (tagged c);
      in
        (* If we are testing for equality with a
           constant then we can simply test directly. *)
        case test of
          Eq => (genCmpi (r, si, cvec); putConditional (test, cvec))
        | Ne => (genCmpi (r, si, cvec); putConditional (test, cvec))
        | _  =>
          let
           (* 
              We generate the following code:
           
                  slwi   rtemp1, r, 31
                  slwiDot   rtemp1, r, 31
                  bne+   L1
                  lwz  r0,52(r13)
                  mtlr r0
                  blrl
                  cmpi   r, si
                  bc<test> label
                  
              If the argument is long, then rtemp1 becomes zero, which
              means we take the trap which causes the RTS to emulate the
              immediately following comparison.
           *)  
            val () = genInstruction (slwiDot (regTemp1, r, 31), cvec);
            val () = genTrapCacheFlush cvec;
            val skipTrap = putConditional (NeInv, cvec)
            val () = genLoad(MemRegisterArbEmulation, regMemRegs, regZero, cvec);
            val () = genMtlr(regZero, cvec);
            val () = genInstruction(blrlQuad, cvec);
            val () = fixup(skipTrap, cvec);
            val () = genCmpi (r, si, cvec);
            val () = clearAllCaches cvec; (* If we've taken the trap LR won't be valid. *)
          in
            putConditional (test, cvec)
          end
      end; (* compareAndBranchRI *)
    
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
    | InstrMulA       => true
    | InstrAddW       => true
    | InstrSubW       => true
    | InstrRevSubW    => true
    | InstrMulW       => true
    | InstrDivW       => true
    | InstrModW       => true
    | InstrOrW        => true
    | InstrAndW       => true
    | InstrXorW       => true
    | InstrLoad       => true
    | InstrLoadB      => true
    | InstrVeclen     => false (* immediate form only *)
    | InstrVecflags   => false (* immediate form only *)
    | InstrUpshiftW   => false
    | InstrDownshiftW => false
    | InstrDownshiftArithW => false
    | InstrGetFirstLong => false (* immediate form only *)
    | InstrStringLength => false (* immediate form only *)
    | InstrSetStringLength => true
    | InstrPush       => true
    | InstrBad        => false
    ;

  (* General register/register operation. *)
  fun genRR (instr : instrs, r1 : reg, r2 : reg, rd : reg, cvec : code) : unit =
  let
    val () =
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
    genMove (rd, r1, cvec)
    
    | InstrAddA =>
    let
      (* Untag one of the arguments. *)
      val () = genAddImmed (regTemp2, r2, ~1, cvec)
      val lab : addrs = genTagTest2 (r1, r2, cvec)
    in
      genAddoDot  (rd, r1, regTemp2, cvec);
      genOverflowTest (lab, cvec)
    end
    
    | InstrSubA =>
    let
      (* Untag one of the arguments. *)
      val () = genAddImmed (regTemp2, r2, ~1, cvec)
      val lab : addrs = genTagTest2 (r1, r2, cvec)
    in
      genSubfcoDot (rd, regTemp2, r1, cvec);
      genOverflowTest (lab, cvec)
    end
    
    | InstrRevSubA =>
    let
      val () = genAddImmed (regTemp2, r1, ~1, cvec)
      val lab : addrs = genTagTest2 (r1, r2, cvec);
    in
      genSubfcoDot (rd, regTemp2, r2, cvec);
      genOverflowTest (lab, cvec)
    end

    | InstrMulA =>
    let
      (* Remove the tag from one of the args. *)
      val () = genAddImmed (regTemp2, r2, ~1, cvec);
      (* Now test the tags. *)
      val lab : addrs = genTagTest2 (r1, r2, cvec)
    in
      (* Shift the other arg. *)
      genInstruction (srawi (regTemp1, r1, TAGBITS), cvec);
      (* Do the multiplication and test the tags. *)
      genX31(rd, regTemp2, regTemp1, MULLWO, true, cvec);
      genOverflowTest (lab, cvec);
      (* Set the correct tag. *)
      genAddImmed (rd, rd, 1, cvec)
    end
    

    | InstrAddW =>
        ((* calculate (badly-tagged) result in rt2 *)
        genAdd (regTemp2, r1, r2, cvec);
        (* move result to rd, restoring correct tag *)
        genAddImmed (rd, regTemp2, ~1, cvec)
        )
    
    | InstrSubW =>
        ((* calculate (badly-tagged) result in rt2 *)
        genSubfc (regTemp2, r2, r1, cvec);
        (* move result to rd, restoring correct tag *)
        genAddImmed (rd, regTemp2, 1, cvec)
        )
    
    | InstrRevSubW =>
        (
        (* calculate (badly-tagged) result in rt2 *)
        genSubfc (regTemp2, r1, r2, cvec);
        (* move result to rd, restoring correct tag *)
        genAddImmed (rd, regTemp2, 1, cvec)
        )
    
    | InstrMulW =>
       (* Fixed precision multiplication. (Doesn't test for overflow.) *)
       (
         (* Untag one argument. *)
         genInstruction (srawi (regTemp2, r1, TAGBITS), cvec);
         (* Untag, but don't shift the multiplicand. *)
         genAddImmed(rd, r2, ~1, cvec);
         (* Do the multiplication. *)
         genX31 (rd, rd, regTemp2, MULLW, false, cvec);
         (* Add back the tag, but don't shift. *)
         genAddImmed(rd, rd, 1, cvec)
      )

    | InstrDivW =>
      let
         (* Test for zero.  The addi instruction doesn't set the condition code. *)
         val () = genCmpi (r2, int16_1, cvec); 
         val skipException = putConditional(NeInv, cvec)
      in
         (* Raise the divide exception: lwz r0,48(r13); mtlr r0; blrl; *)
         genLoad(MemRegisterRaiseDiv, regMemRegs, regZero, cvec);
         genMtlr(regZero, cvec);
         genInstruction(blrlQuad, cvec); (* We could use a conditional branch-and-link*)

         fixup(skipException, cvec); (* We jump here if it was non-zero. *)
         (* Subtract the tag from the arguments. *)
         genAddImmed(regTemp2, r2, ~1, cvec);
         genAddImmed(regTemp1, r1, ~1, cvec);
         (* Do the division. *)
         genX31 (regTemp1, regTemp1, regTemp2, DIVWU, false, cvec);
         (* Tag the result. *)
         genInstruction (slwi (regTemp1, regTemp1, TAGBITS), cvec);
         genInstructionList (orImmed (rd, regTemp1, 1), cvec)
      end

    | InstrModW =>
       (* Fixed precision remainder. *)
       (* We can't do this as a single instruction.  Instead we have to
          use division, multiplication and subtraction. *)
      let
         (* Test for zero.  The addi instruction doesn't set the condition code. *)
         val () = genCmpi (r2, int16_1, cvec); 
         val skipException = putConditional(NeInv, cvec)
      in
         (* Raise the divide exception: lwz r0,48(r13); mtlr r0; blrl; *)
         genLoad(MemRegisterRaiseDiv, regMemRegs, regZero, cvec);
         genMtlr(regZero, cvec);
         genInstruction(blrlQuad, cvec); (* We could use a conditional branch-and-link*)
         
         fixup(skipException, cvec); (* We jump here if it was non-zero. *)
         (* Just subtract the tags from the args. *)
         genAddImmed(regTemp1, r1, ~1, cvec);
         genAddImmed(regTemp2, r2, ~1, cvec);
         (* Do the division. *)
         genX31 (regTemp1, regTemp1, regTemp2, DIVWU, false, cvec);
         (* Multiply the result back again. *)
         genX31 (regTemp1, regTemp1, regTemp2, MULLW, false, cvec);
         (* Subtract this from the original, leaving us the tagged remainder. *)
         genSubfc (rd, regTemp1, r1, cvec)
      end

    | InstrOrW =>
        genOr (rd, r1, r2, cvec)
    
    | InstrAndW =>
        genAnd (rd, r1, r2, cvec)
    
    | InstrXorW =>
      let
        val () = genXor (regTemp2, r1, r2, cvec)
      in
        (* restore tag bit *)
        genInstructionList (orImmed (rd, regTemp2, 1), cvec)
      end
        
    | InstrLoad =>
      let
    val FIRSTBIT = 0;
    val LASTBIT  = 31 - 2;
    
    (* Shift to form word index, masking off the tag bits. *)
    val rlwinmQuad : quad = 
      rlwinm (regTemp2, r2, 2 - TAGBITS, FIRSTBIT, LASTBIT);
      
    (* load the word *)
    val lwzxQuad : quad = lwzx (rd, r1, regTemp2);

      in
        genInstruction (rlwinmQuad, cvec);
        genInstruction (lwzxQuad, cvec)
      end
        
    | InstrLoadB =>
        (
        (* Untag the byte index *)
        genInstruction (srawi (regTemp2, r2, TAGBITS), cvec);
        (* Load the byte *)
        genInstruction (lbzx (regTemp1, r1, regTemp2), cvec);
        (* Shift up *)
        genInstruction (slwi (regTemp1, regTemp1, TAGBITS), cvec);
        (* Tag the result *)
        genInstructionList (orImmed (rd, regTemp1, 1), cvec)
        )
        
    | InstrPush => (* RetAddrInBoth rd and r2 are ignored. *)
        genPush (r1, cvec)

    | InstrSetStringLength =>
      (
      (* Untag the value to store. *)
      genInstruction (srawi (regTemp2, r2, TAGBITS), cvec);
      (* Store it in the first word of the new string. *)
      genInstruction (stw (regTemp2, r1, int16_0), cvec)
      )

    | _ =>
      raise InternalError "genRR: Unimplemented instruction"
  end; (* genRR *)
  
(***************************************************************************  
  RI implementation of general operations
***************************************************************************)  
    
  (* Is this argument acceptable as an immediate
     or should it be loaded into a register? *) 
  fun instrIsRI (instr : instrs, cnstnt : machineWord) : bool =
     let
        fun isShiftable c = 0 < c andalso c < (32 - TAGBITS)
     in
      case instr of
        InstrMove       => true (* Always. *)
      | InstrAddA       => isShort cnstnt
      | InstrSubA       => isShort cnstnt
      | InstrRevSubA    => isShort cnstnt
      | InstrMulA       =>
            isShort cnstnt andalso ~1 <= toInt (toShort cnstnt) andalso
            toInt (toShort cnstnt) <= 2 (* for now *)
      | InstrAddW       =>
            isShort cnstnt andalso isTaggable16Bit(toInt (toShort cnstnt)-1)
      | InstrSubW       =>
            isShort cnstnt andalso isTaggable16Bit(1 - toInt (toShort cnstnt))
      | InstrRevSubW    =>
            isShort cnstnt andalso isShort (tagged(toInt (toShort cnstnt))+ 1)
      | InstrMulW       =>
            isShort cnstnt andalso is16Bit(toInt (toShort cnstnt))
      | InstrDivW       => false (* For now *)
      | InstrModW       => false (* For now *)
      | InstrOrW        => isShort cnstnt
      | InstrAndW       => isShort cnstnt
      | InstrXorW       => isShort cnstnt
      | InstrLoad       =>
            isShort cnstnt andalso isTaggable16Bit(toInt (toShort cnstnt))
      | InstrLoadB      =>
            isShort cnstnt andalso is16Bit(toInt (toShort cnstnt))
      | InstrVeclen     => true (* Constant is ignored. *)
      | InstrVecflags   => true (* Constant is ignored. *)
      | InstrUpshiftW   =>
            isShort cnstnt andalso isShiftable(toInt (toShort cnstnt))
      | InstrDownshiftW =>
            isShort cnstnt andalso isShiftable(toInt (toShort cnstnt))
      | InstrDownshiftArithW =>
            isShort cnstnt andalso isShiftable(toInt (toShort cnstnt))
      | InstrPush       => false 
      | InstrGetFirstLong => false (* For the moment. *)
      | InstrStringLength => true (* Constant is ignored. *)
      | InstrSetStringLength => false (* Not at the moment. *)
      | InstrBad        => false
    end; (* instrIsRI *)  
  
  (* Register/immediate operations.  In many of these operations
     we have to tag the immediate value. *)
  (* Register/immediate operations.  In many of these operations
     we have to tag the immediate value. *)
  fun genRI (instr : instrs, rs : reg, constnt : machineWord, rd : reg, cvec) : unit =
  let
    val () =
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
        if isShort constnt
        then genLoadImmed (rd, tagged(toInt (toShort constnt)), cvec)
        else genLoadConstant(WVal constnt, rd, cvec)

    | InstrAddA => (* Arbitrary precision addition. *)
      let
        val c = toInt (toShort constnt)
        val ()    = genLoadImmed (regTemp2, tagged c - 1, cvec);
        val lab : addrs = genTagTest1 (rs, cvec);
      in
        genAddoDot (rd, rs, regTemp2, cvec);
        genOverflowTest (lab, cvec)
      end
      
    | InstrSubA => (* Arbitrary precision subtraction. *)
      let
        val c = toInt (toShort constnt)
        val ()    = genLoadImmed (regTemp2, tagged c - 1, cvec);
        val lab : addrs = genTagTest1 (rs, cvec);
      in
        genSubfcoDot (rd, regTemp2, rs, cvec);
        genOverflowTest (lab, cvec)
      end
      
    | InstrRevSubA => (* Arbitrary precision reverse subtraction. *)
      let
        val c = toInt (toShort constnt)
        val ()    = genLoadImmed (regTemp2, tagged c + 1, cvec);
        val lab : addrs = genTagTest1 (rs, cvec);
      in
        genSubfcoDot (rd, rs, regTemp2, cvec);
        genOverflowTest (lab, cvec)
      end
      
    | InstrMulA =>
      let
        val c = toInt (toShort constnt)
      in
        case c of 
          ~1 => genRI (InstrRevSubA, rs, toMachineWord 0, rd, cvec)
        |  0 => genRI (InstrMove, regZero, toMachineWord 0, rd, cvec)
        |  1 => genRR (InstrMove, rs, regZero, rd, cvec)
        |  2 => genRR (InstrAddA, rs, rs, rd, cvec)
        |  _ =>
             raise InternalError ("genRI: MulA - bad value " ^ Int.toString c)
      end

    | InstrAddW =>
      let
          val c = toInt (toShort constnt)
      in
         (* pre-adjust tag of operand *)
        genAddImmed (rd, rs, tagged c - 1, cvec)
      end
    
    | InstrSubW =>
      let
          val c = toInt (toShort constnt)
      in
        (* pre-adjust tag of operand *)
        genAddImmed (rd, rs, ~(tagged c - 1), cvec)
      end
    
    | InstrRevSubW =>
      let
          val c = toInt (toShort constnt)
      in
        (* pre-adjust tag of operand *)
        genLoadImmed (regTemp1, tagged c + 1, cvec);
        genSubfc (rd, rs, regTemp1, cvec)
      end
    
    | InstrMulW =>
      let
          val c = toInt (toShort constnt)
      in
        (* Subtract off the tag. *)
        genAddImmed (regTemp1, rs, ~1, cvec);
        (* Multiply by the untagged value. *)
        genInstruction(mulli(regTemp1, regTemp1, int16 c), cvec);
        (* Add back the tag. *)
        genAddImmed (rd, regTemp1, 1, cvec)
      end

    | InstrDivW =>
        raise InternalError "genRI: Unimplemented instruction (InstrDivW)"

    | InstrModW =>
        raise InternalError "genRI: Unimplemented instruction (InstrModW)"

    | InstrOrW =>
        genInstructionList (orImmed (rd, rs, tagged(toInt (toShort constnt))), cvec)
    
    | InstrAndW =>
        genInstructionList (andImmed (rd, rs, tagged(toInt (toShort constnt))), cvec)
      
    | InstrXorW =>
        genInstructionList (xorImmed (rd, rs, semiTagged(toInt (toShort constnt))), cvec)
    
    | InstrLoad => (* offset is in words *)
      let
          val c = toInt (toShort constnt)
      in
        genInstruction (lwz (rd, rs, int16 (4 * c)), cvec)
      end
    
    | InstrLoadB => (* offset is in bytes *)
      let
          val c = toInt (toShort constnt)
      in
        genInstruction (lbz (regTemp2, rs, int16 c), cvec);
        genInstruction (slwi (regTemp2, regTemp2, TAGBITS), cvec);
        genInstruction (ori (rd, regTemp2, int16_1), cvec)
      end

    | InstrVeclen =>
      let
    val FIRSTBIT = 8 - TAGBITS;
    val LASTBIT  = 31 - TAGBITS;
    
    (* Get the 24-bit length field, upshifted by TAGBITS bits. *)
    val rlwinmQuad : quad = 
      rlwinm (regTemp2, regTemp2, TAGBITS, FIRSTBIT, LASTBIT);

      in
         genLoad (~4, rs, regTemp2, cvec);
         genInstruction (rlwinmQuad, cvec);
         genInstruction (ori (rd, regTemp2, int16_1), cvec)
      end

    | InstrVecflags =>
      (* Get the flag byte.  N.B.  This is only correct if we
         are running in big endian mode.  DCJM 26/10/00. *)
      let
        val offset = ~4 (* ~1 on a little-endian. *)
      in
        genInstruction (lbz (regTemp2, rs, int16 offset), cvec);
        genInstruction (slwi (regTemp2, regTemp2, TAGBITS), cvec);
        genInstruction (ori (rd, regTemp2, int16_1), cvec)
      end

    | InstrUpshiftW =>   (* logical shift left *)
      let
          val c = toInt (toShort constnt)
          val FIRSTBIT = 0;
          val LASTBIT  = (31 - TAGBITS) - c;
    
    (* The actual shift (must have 0 <= c < 30). *)
    (* This saves an instruction compared with
       removing the tagbits, then performing a normal
       left shift, but only works if we actually
       want part of the answer, rather than shifting evrything
       out. *)
          val rlwinmQuad : quad = 
              rlwinm (regTemp2, rs, c, FIRSTBIT, LASTBIT);

      in
         genInstruction (rlwinmQuad, cvec);
         genInstruction (ori (rd, regTemp2, int16_1), cvec)
      end
    
    | InstrDownshiftW =>  (* logical shift right *)
      let
          val c = toInt (toShort constnt)
    val FIRSTBIT = 0;
    val LASTBIT  = (31 - TAGBITS);
    
    (* The actual shift (must have 0 <= c < 32). *)
    val srwiQuad : quad = 
      srwi (regTemp2, rs, c);
      
    (* Remove stray bits from the tag positions. *)
    val rlwinmQuad : quad = 
      rlwinm (regTemp2, regTemp2, 0, FIRSTBIT, LASTBIT);

      in
         genInstruction (srwiQuad, cvec);
         genInstruction (rlwinmQuad, cvec);
         genInstruction (ori (rd, regTemp2, int16_1), cvec)
      end

    | InstrDownshiftArithW =>  (* arithmetic shift right *)
      let
          val c = toInt (toShort constnt)
    val FIRSTBIT = 0;
    val LASTBIT  = (31 - TAGBITS);
    
    (* The actual shift (must have 0 <= c < 32). *)
    val srawiQuad : quad = 
      srawi (regTemp2, rs, c);
      
    (* Remove stray bits from the tag positions. *)
    val rlwinmQuad : quad = 
      rlwinm (regTemp2, regTemp2, 0, FIRSTBIT, LASTBIT);

      in
         genInstruction (srawiQuad, cvec);
         genInstruction (rlwinmQuad, cvec);
         genInstruction (ori (rd, regTemp2, int16_1), cvec)
      end
    
    | InstrStringLength => (* The second arg. is ignored. *)
        let
            (* If it's tagged the result is 1 otherwise we need to load
               the length word and tag it. *)
            val l1 = compareAndBranchRI (rs, toMachineWord 0 (* Unused *), Long, cvec)
            val _ = genLoadImmed (rd, tagged 1, cvec);
            val l2 = unconditionalBranch cvec
        in
            fixup(l1, cvec);
            (* Load the length word. *)
            genInstruction (lwz (regTemp2, rs, int16_0), cvec);
            genInstruction (slwi (regTemp2, regTemp2, TAGBITS), cvec);
            genInstruction (ori (rd, regTemp2, int16_1), cvec);
            fixup(l2, cvec)
        end

   | _ =>
        raise InternalError "genRI: Unimplemented instruction"
    
  end; (* genRI *)
  
  fun printCode (seg, procName : string, lastAddr : addrs, printStream) : unit =
  let
    val ptr = ref addrZero;
  
  in
    if procName = "" (* No name *) then printStream "?" else printStream procName;
    printStream ":\n";
    while !ptr addrLt lastAddr do
    let 
      val thisAddr : addrs = !ptr;
      val () = ptr := thisAddr wordAddrPlus 1;
      
      val byteAddr : int = getByteAddr thisAddr;
      val instr : int = fromQuad (getQuad (thisAddr, seg));
      val () = printHex(byteAddr, printStream);    (* The address. *)
      val () = printStream "\t";
      val () = printHexN (8, instr, printStream);  (* The naked instruction. *)
      val () = printStream "\t";
      val () = printInstr (byteAddr, instr, printStream); (* The decoded instruction. *)
    in
      printStream "\n"
    end
  end; (* printCode *)
 
  (* constLabels - fill in a constant in the code. *)
  fun constLabels (Code{resultSeg=ref rseg, pcOffset=ref offset, ...},
                   addr: addrs, value:machineWord) =
  let
    val seg       = scSet rseg;
    val constAddr : addrs = addr wordAddrPlus offset;
  in
    csegPutConstant (seg, getByteAddr constAddr, value, 0)
  end;
  
  (* Fix up references from other vectors to this one. *)
  fun fixOtherRefs (refTo as Code{otherCodes=ref otherCodes, ...}, value) =
  let
    fun fixRef (refFrom as
                    Code{numOfConsts = noc, constVec = ref constVec,
                         resultSeg = ref resultSeg, ...}) =
    let      
      fun putConst (CVal cCode, addr) =
        if cCode is refTo
        then (* A reference to this one. *)
          (
          (* Fix up the forward reference. *)
          constLabels (refFrom, addr, value);
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



  (* The stack limit register is set at least twice this far from the end
     of the stack so we can simply compare the stack pointer with the stack
     limit register if we need less than this much. Setting it at twice
     this value means that procedures which use up to this much stack and
     do not call any other procedures do not need to check the stack at all. *)
  val minStackCheck = 20; 
  
  (* Adds the constants onto the code, and copies the code into a new segment *)
  fun copyCode (cvec as
                    Code{needReturn,
                         pcOffset,
                         codeVec,
                         noClosure,
                         selfCalls = ref selfCalls,
                         selfJumps = ref selfJumps,
                         numOfConsts,
                         ic,
                         constVec = ref constVec,
                         resultSeg,
                         procName,
                         printAssemblyCode,
                         printStream,
                         ...},
                stackRequired, registerSet) : address =
   let
    val callsAProc = !(mustCheckStack cvec);
    val endIC      = !ic; (* Remember end *)

    (* Generate end-of-code marker *)
    val () = genCodeQuad (toQuad 0, cvec);  (* changes !ic *)
    
(*****************************************************************************
N.B.  The calling sequence has been simplified since it's no longer necessary
to turn the register that points to the start of the code into a constant
pointer.  Some of the following may no longer apply.

Function Prologues
------------------
Functions now have up to 3 entry points:
  (1) L1/L2 Standard entry point - return address is in LR
  (2) Self-call entry point - doesn't change regCode
  (3) Self-tail-call entry point - doesn't change regReturn or regCode

At all entry points, LR must contain the return address.
At entry point L4, regReturn must contain it too.

L1, L2: (* Return address in LR only *)
L3:     (* Return address in LR only *)
        mflr  regReturn
        ori   regReturn,regReturn,2
L4:     (* Return address in both LR and regReturn *)
        <stack-check code>
        (* Return address in both LR and regReturn *)
        
N.B. We must use "ori", not "addi" to adjust regReturn. This is
because we don't know whether or not the value in LR is already tagged.
It will be untagged if it was put there by a "blrl" (or similar)
call instruction, but it will be already tagged if it was put there
by a "mtlr returnReg" tail-call instruction.

Stack-check code
----------------
The stack limit register is a pointer, below which rsp must
not be decremented.

(1) "No" stack check.

        cmpl   rsp,rsl
        bge+   .+4
        lwz    r0,16(r13)
        mtlr   r0
        blrl

(2) "Big" stack check
        ... load rsp-<frame-size> into rtemp1 ...
        cmpl   rtemp1,rsl
        bge+   .+4
        lwz    r0,16(r13)
        mtlr   r0
        blrl

*****************************************************************************)
     
    (* Generate the prelude (iterative!) *)
    local 
      val regReturnCode =
        if ! needReturn orelse stackRequired >= minStackCheck orelse callsAProc
        then (* If we need to do a stack check we have to preserve the LR. *)
        [
          mflr regReturn,
          ori (regReturn, regReturn, int16_2)
        ]
        else [];
        
      val stackCheckCode =
        if stackRequired >= minStackCheck
        then let (* We need to check the stack. *)
          val stackBytes = stackRequired * 4;
        in 
          (* 
             get the "minimum" sp into rt1 and trap if it is less 
             than the stack limit register. There's probably
             a better way to construct this code - we should
             just subtract the constant using immediates, not
             load it into regTemp1 first.
          *)
          loadImmed (regTemp1, stackBytes) @
          [
            lwz(regZero, regMemRegs, int16 MemRegisterStackLimit),
            sub (regTemp1, regStackPtr, regTemp1),
            cmpl (regTemp1, regZero),
            condBranch(GeInv, int14 4),
            (* Call the "extended" entry. *)
            lwz(regZero, regMemRegs, int16 MemRegisterStackOverflowEx),
            mtlr(regZero),
            blrlQuad
           ]
        end
           
        else if callsAProc (* check for user interrupt *)
        then
          [
           (* trap if current sp is less than stack limit register.
              This check is necessary because user interupts (^C)
              adjust the limit register precisely so this check will
              cause a trap is a "safe" state. *)
            lwz(regZero, regMemRegs, int16 MemRegisterStackLimit),
            cmpl (regStackPtr, regZero),
            condBranch(GeInv, int14 4),
            (* Call the "normal" entry. *)
            lwz(regZero, regMemRegs, int16 MemRegisterStackOverflow),
            mtlr(regZero),
            blrlQuad
         ]
           
        else (* no stack check required *)
          []; 


      (* code segment size minimised (iteratively!) SPF 12/8/94 *)
      fun getPreludeCode spaceForPrelude (* an initial guess! *) =
      let
        (* +5 for code size, profile count, function name, register mask
            and constants count *)
        val segSize = getWordAddr (!ic) + spaceForPrelude + 5;
        
        (* Prelude consists of
             1) moving or(lr,2) to regReturn
             2) stack checking code
        *)
       
       val preludeCode = 
          (* L1, L2 here *)
          (* L3 here *)     regReturnCode @
          (* L4 here *)     stackCheckCode;
      in
        (* does it fit? *)
        if List.length preludeCode = spaceForPrelude
        then (spaceForPrelude,segSize,preludeCode)
        else getPreludeCode (spaceForPrelude + 1)
      end
    in
      val (spaceForPrelude,segSize,preludeCode) =
        (* iterate to find size of loadConstSegCode *)
        getPreludeCode (List.length regReturnCode + List.length stackCheckCode); 
        
      (* byte offsets of L3 and L4 labels relative 
         to start of post-prelude code. *)
      val L4Addr = mkWordAddr (~ (List.length stackCheckCode));
      val L3Addr = mkWordAddr (~ (List.length regReturnCode + List.length stackCheckCode));
    end; (* local *)
    
    (* fix-up all the self-calls *)
    val () = fixupRecursiveCalls    (L3Addr, selfCalls, cvec);
    val () = fixupRecursiveBranches (L4Addr, selfJumps, cvec);
    
    (* Now make the byte segment that we'll turn into the code segment *)
    val seg : cseg = csegMake segSize;
    val offset     = spaceForPrelude;

    val () = resultSeg := Set seg;
    
    (* Copy the code into the new segment. *)
    val () = pcOffset := offset;
    val () = csegCopySeg (codeVec, seg, getByteAddr (! ic), offset);

    (* insert prelude code into segment *)
    local
      fun putPreludeQuad (wordAddr : int, w : quad) =
        setQuad (w, mkWordAddr wordAddr, seg);
    in
      val () = applyCountList (putPreludeQuad, 0, preludeCode);
    end;
    
    local
      val endOfCode : addrs = (! ic) wordAddrPlus offset;
    in
      (* Byte offset of start of code. *)
      local
        val quad = toQuad (getByteAddr endOfCode);
        val addr = endOfCode;
      in
        val () = setQuad (quad, addr, seg)
      end;
      
      (* Put in the number of constants. This must go in before
         we actually put in any constants. *)
      local
        val quad = toQuad 2; (* Just two constants now. *)
        val addr = endOfCode wordAddrPlus 4;
      in
        val () = setQuad (quad, addr, seg)
      end;
      
      (* Next the profile count. *)
      local
        val quad = toQuad 0;
        val addr = endOfCode wordAddrPlus 1;
      in
        val () = setQuad (quad, addr, seg)
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
        val name     : string = procName
        val nameWord : machineWord   = if name = "" then toMachineWord 0 else toMachineWord name;
        val addr     : addrs  = endOfCode wordAddrPlus 2;
      in
        val () = csegPutWord (seg, getWordAddr addr, nameWord)
      end
      local
        (* Encode the register mask.  This encoding must be the same
           as the one used for assembly code segments. *)
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
        val () = csegPutWord (seg, 
                getWordAddr(endOfCode wordAddrPlus 3), toMachineWord regSet);
      end;
    end; (* scope of endofcode *)
  in
    let
      (* and then copy the objects from the constant list. *)
      fun putConst (WVal c, addr) =
            ( (* Can put these in now. *)
              constLabels (cvec, addr, c);
              numOfConsts := ! numOfConsts - 1
            )
        | putConst (HVal(ref hv), addr) =
          let
              (* The following comment applies to offsetAddr *)
              (* Special function to add to an address.
                 This only works if the resulting value is 
                 in a code segment and is on a word  + 2 byte boundary. *)
              val handlerByteOffset : int = getByteAddr (hv wordAddrPlus offset);
              val handlerAddr : handler = 
              offsetAddr (csegAddr seg, toShort (handlerByteOffset + 2));
      in
              constLabels (cvec, addr, toMachineWord handlerAddr);
              numOfConsts := ! numOfConsts - 1
          end

          (* forward-reference - fix up later when we compile
             the referenced code *) 
        | putConst (CVal _, _) = ()

      val _ = List.app putConst constVec;
  
      (* Switch off "mutable" bit now if we have no
         forward or recursive references to fix-up *)
      val () = 
        if ! numOfConsts = 0 then csegLock seg else ();
  
      (* Do we need to make a closure, or just return the code? *)
      val addr : address =
        if noClosure
        then csegAddr seg
        else let
          val addr : address = alloc (short1, F_words, toMachineWord (csegAddr seg));
          
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
          (* endcode is the address of the end-of-code marker word. *)
          val lastAddr : addrs = endIC wordAddrPlus offset;
        in
          printCode (seg, procName, lastAddr, printStream);
          printStream "Register set = [";
          List.app (fn r => (print " "; printStream(regRepr r))) registerSet;
          printStream "]\n\n"
        end
        else ();
    in
      addr 
    end (* the result *)
    
  end (* copyCode *);

  (* ic function exported to gencode. Currently only used for backward jumps. *)
  val ic = fn cvec =>
  ( (* Make sure any pending operations are done. *)
    genPendingStackAdjustment cvec;
    reallyFixup cvec;
    clearAllCaches cvec; (* We may be jumping here. *)
    ! (ic cvec)
  );


   (* We need these types although we don't generate indexed cases. *)
   type cases = {tag: int, addr: addrs}; (* should tag be a short??? *)

   type jumpTableAddrs = unit;
   
   fun constrCases (i, a) : cases = {tag = i, addr = a}
   
   fun useIndexedCase _ : bool =
     false; (* Never use indexed case. *)

   fun indexedCase _ : jumpTableAddrs =
     raise InternalError "Not implemented: indexedCase";

   fun makeJumpTable _ : unit =
     raise InternalError "Not implemented: makeJumpTable";

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
     procName ^ ":" ^ Int.fmt StringCvt.HEX (getByteAddr ic)

end (* CODECONS functor body *)

end (* structure-level let *)
