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
TODO: This duplicates some of the Word structure although it adds certain
"unsafe" and Poly-specific functions.  It probably should be rewritten now
that we have the standard basis library.  DCJM June 2000.
*)

structure Address :

(*****************************************************************************)
(*                  Address export signature                                 *)
(*****************************************************************************)
sig
  type word
  type address
  type handler
  eqtype short
  
  val wordEq : 'a * 'a -> bool

  val isShort   : 'a -> bool

  exception Cast of string;
  val unsafeCast: 'a -> 'b
  val toWord    : 'a    -> word
  val toInt     : short -> int
  val toShort   :  'a -> short
  val toAddress : 'a -> address
    
  val loadByte:   (address * short) -> short 
  val loadWord:   (address * short) -> word

  val assignByte: (address * short * short) -> unit
  val assignWord: (address * short * word)  -> unit

  val alloc:  (short * short * word) -> address
  val lock:   address -> unit
  val length: address -> short
  val flags:  address -> short

  val setFlags: address * short -> unit
  val getFlags: address -> short
  val wordSize: int
  
  val Or:  short * short -> short
  val And: short * short -> short 
  val Xor: short * short -> short 
  val << : short * short -> short 
  val >> : short * short -> short 
  
(* Some stuff for short arithmetic could be added here eventually *)
  
  val F_words        : short
  val F_bytes        : short
  val F_code         : short
  val F_stack        : short
  val F_negative     : short
  val F_mutable      : short
  val F_gc           : short;

  val isWords : address -> bool;
  val isBytes : address -> bool;
  val isCode  : address -> bool;
  val isStack : address -> bool;
  val isMutable:address -> bool;

  val call: (address * word) -> word
  
  val isIoAddress : address -> bool (* Added DCJM 24/10/00. *)

(* The following function is VERY unsafe and should only be *)
(* used by the Poly/ML code generator (ML version).         *)    
  val offsetAddr : address * short -> handler
end =

let
  open RuntimeCalls;
in
(*****************************************************************************)
(*                  Address structure body                                   *)
(*****************************************************************************)
struct
  abstype word    = Word    of int (* a legal ML object (tag = 0 or 1) *)
      and address = Addr    of int (* a normal pointer (tag = 0) *)
      and handler = Handler of int (* pointer to exception handler (tag = 2!) *)
  with
  end; (* abstype *)
  local
    datatype short = Short of int; (* a 30-bit int  (tag = 1) *)
  in
     type short = short; (* short is an eqtype, the others aren't *)
   end;

  (* pointer equality *)
  	local
		val eqWord : word*word->bool = RunCall.run_call2 POLY_SYS_word_eq
	in
		fun wordEq(x: 'a ,y: 'a): bool = RunCall.unsafeCast eqWord (x,y)
	end

  val unsafeCast : 'a -> 'b = RunCall.unsafeCast;

  local
  	  val short : word->bool = RunCall.run_call1 POLY_SYS_is_short
  in
  	  fun isShort (x:'a): bool = unsafeCast short x
  end

  (* The following casts are always safe *)
  val toWord : 'a    -> word = unsafeCast;
  val toInt  : short -> int  = unsafeCast;
  
  (* The following casts need checking *)
  exception Cast of string;
  
  fun toShort   (w:'a) : short   = 
    if isShort w then unsafeCast w else raise Cast "toShort";
   
  fun toAddress (w:'a) : address =
    if isShort w then raise Cast "toAddress" else unsafeCast w;
  
  (* Note: 
       assignByte should *not* be used with word-objects
	 (we might copy half a pointer into the object,
	  then call the garbage collector)
       
       loadWord should *not* be used with byte-objects
	 (we might load somthing that's not a valid ML value,
	  then call the garbage collector)
	  
    Violating these assertions may corrupt the heap and cause unpredictable
    behaviour.
    
    It's safe to use assignWord with a byte-object or loadByte
    with a word-object but it may not do what you expect.
    
      One difference is that loadWord / assignWord leave the tag bits
      unchanged but loadByte / assignByte strip and replace them.
    
      Another difference is that the offset for the
      "Word" functions is in words, whereas the offset for the
      "Byte" functions is in bytes.
  *)
  
  val loadByte:   (address * short) -> short = 
    RunCall.run_call2 POLY_SYS_load_byte;
    
  val loadWord:   (address * short) -> word  =
    RunCall.run_call2 POLY_SYS_load_word;

  val assignByte: (address * short * short) -> unit =
    RunCall.run_call3 POLY_SYS_assign_byte;
  
  val assignWord: (address * short * word)  -> unit =
    RunCall.run_call3 POLY_SYS_assign_word;

  val alloc:  (short * short * word) -> address =
    RunCall.run_call3 POLY_SYS_alloc_store;
  
  val lock:   address -> unit =
    RunCall.run_call1 POLY_SYS_lockseg;
  
  val getFlags: address -> short =
    RunCall.run_call1 POLY_SYS_get_flags;

  val setFlags: address * short -> unit =
    RunCall.run_call2 POLY_SYS_set_flags;

  val wordSize: int =
    RunCall.run_call0 POLY_SYS_bytes_per_word ();

  val length: address -> short =
     RunCall.run_call1 POLY_SYS_get_length;
  
  val flags:  address -> short  =
    RunCall.run_call1 POLY_SYS_get_flags;
  
  (* "call" added SPF 7/7/94, corrected 13/7/94, 8/9/94 *)
  (* call takes the address of the code of a function [N.B. *NOT*  *)
  (* the closure for the function] that uses Poly parameter        *)
  (* conventions (values in registers e.g. an ML secondary entry   *)
  (* point) and applies it to the address of a single ML tuple.    *)
  (* N.B. it MUST be a tuple, even if there's only one parameter.  *)
  (* However, since unit is a "short" not an "address", the type   *)
  (* of the second parameter has to be "word" (not "address").     *)
  
  (* The run-time system functions all use Poly convention. *)
  val call: (address * word) -> word =
    RunCall.run_call1 POLY_SYS_callcode_tupled;

(* Used for flag manipulations(?) *)
  val Or:  short * short -> short = RunCall.run_call2 POLY_SYS_or_word;
  val And: short * short -> short = RunCall.run_call2 POLY_SYS_and_word; 
  val Xor: short * short -> short = RunCall.run_call2 POLY_SYS_xor_word; 
  val << : short * short -> short = RunCall.run_call2 POLY_SYS_shift_left_word; 
  val >> : short * short -> short = RunCall.run_call2 POLY_SYS_shift_right_word; 
  
(* Some stuff for short arithmetic could be added here eventually *)
  
  val F_words        : short = unsafeCast   0;
  val F_bytes        : short = unsafeCast   1;
  val F_code         : short = unsafeCast   2;
  val F_stack        : short = unsafeCast   8;
  val F_negative     : short = unsafeCast  32;
  val F_mutable      : short = unsafeCast  64;
  val F_gc           : short = unsafeCast 128;

  local
		val doCall: int*address -> bool
			 = RunCall.run_call2 RuntimeCalls.POLY_SYS_process_env
  in
		fun isIoAddress (a: address) : bool = doCall(102, a)
  end


(* The following function is VERY unsafe and should only be *)
(* used by the Poly/ML code generator (ML version).         *)    
  val offsetAddr : address * short -> handler =
    RunCall.run_call2 POLY_SYS_offset_address;
  
  local
    val typeMask : short = unsafeCast 255;
    nonfix And
    nonfix Or

    val isType : short -> address -> bool = 
      fn (t:short) => fn (a:address) => 
        wordEq (And (flags a, typeMask), t);
      
  in
    val isWords = isType F_words;
    val isBytes = isType F_bytes;
    val isCode  = isType F_code;
    val isStack = isType F_stack;

	(* The mutable flag may be used with any of the others.
	   Added DCJM 26/1/01. *)
	fun isMutable a = wordEq(And(flags a, F_mutable), F_mutable)
  end;
end

end; (* open RuntimeCalls *)
