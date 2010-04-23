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

structure Address :>

(*****************************************************************************)
(*                  Address export signature                                 *)
(*****************************************************************************)
sig
  type machineWord
  type address
  type handler
  type short = Word.word
  
  val wordEq : 'a * 'a -> bool

  val isShort   : 'a -> bool

  exception Cast of string;
  val unsafeCast: 'a -> 'b
  val toMachineWord    : 'a    -> machineWord
  val toShort   :  'a -> short
  val toAddress : 'a -> address
    
  val loadByte:   (address * Word.word) -> Word8.word 
  val loadWord:   (address * Word.word) -> machineWord

  val assignByte: (address * Word.word * Word8.word) -> unit
  val assignWord: (address * Word.word * machineWord)  -> unit

  val alloc:  (short * Word8.word * machineWord) -> address
  val maxAllocation: word

  val lock:   address -> unit
  val length: address -> short
  val flags:  address -> Word8.word

  val setFlags: address * Word8.word -> unit
  val getFlags: address -> Word8.word
  val wordSize: int
  
    val F_words        : Word8.word
    val F_bytes        : Word8.word
    val F_code         : Word8.word
    val F_stack        : Word8.word
    val F_negative     : Word8.word
    val F_mutable      : Word8.word
    val F_gc           : Word8.word
    val F_noOverwrite  : Word8.word
    val F_weak         : Word8.word

  val isWords : address -> bool;
  val isBytes : address -> bool;
  val isCode  : address -> bool;
  val isStack : address -> bool;
  val isMutable:address -> bool;

  val call: (address * machineWord) -> machineWord

  val isIoAddress : address -> bool

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
  type machineWord    = word (* a legal ML object (tag = 0 or 1) *)
  and address  = word (* a normal pointer (tag = 0) *)
  and handler  = word (* pointer to exception handler (tag = 2!) *)
  and short    = word; (* a 31/63-bit int  (tag = 1) *)

  (* pointer equality *)
  val wordEq = PolyML.pointerEq
  val unsafeCast : 'a -> 'b = RunCall.unsafeCast;

  local
      val short : machineWord->bool = RunCall.run_call1 POLY_SYS_is_short
  in
      fun isShort (x:'a): bool = unsafeCast short x
  end

  (* The following cast is always safe *)
  val toMachineWord : 'a    -> machineWord = unsafeCast;

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
     (we might load something that's not a valid ML value,
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
  
  val loadByte:   (address * Word.word) -> Word8.word = 
    RunCall.run_call2 POLY_SYS_load_byte;
    
  val loadWord:   (address * Word.word) -> machineWord  =
    RunCall.run_call2 POLY_SYS_load_word;

  val assignByte: (address * Word.word * Word8.word) -> unit =
    RunCall.run_call3 POLY_SYS_assign_byte;
  
  val assignWord: (address * Word.word * machineWord)  -> unit =
    RunCall.run_call3 POLY_SYS_assign_word;


    val maxAllocation: word = RunCall.run_call2 POLY_SYS_process_env(100, ())

    fun alloc(len: word, flags: Word8.word, initial: machineWord): address =
        (* Zero sized-objects are not allowed.  Check that the size is within
           the acceptable range. *)
        if len = 0w0 orelse len >= maxAllocation
        then raise Size
        else RunCall.run_call3 POLY_SYS_alloc_store(len, flags, initial)
  
  val lock:   address -> unit =
    RunCall.run_call1 POLY_SYS_lockseg;
  
  val getFlags: address -> Word8.word =
    RunCall.run_call1 POLY_SYS_get_flags;

  val setFlags: address * Word8.word -> unit =
    RunCall.run_call2 POLY_SYS_code_flags;

  val wordSize: int =
    RunCall.run_call0 POLY_SYS_bytes_per_word ();

  val length: address -> Word.word =
     RunCall.run_call1 POLY_SYS_get_length;
  
  val flags:  address -> Word8.word  =
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
  val call: (address * machineWord) -> machineWord =
    RunCall.run_call1 POLY_SYS_callcode_tupled;
  
    val F_words        : Word8.word = 0wx00 (* word object - contains pointers and/or tagged values. *)
    val F_bytes        : Word8.word = 0wx01 (* byte object (contains no pointers) *)
    val F_code         : Word8.word = 0wx02 (* code object (mixed bytes and words) *)
    val F_stack        : Word8.word = 0wx03 (* stack object - may contain internal pointers *)
    val F_noOverwrite  : Word8.word = 0wx08 (* don't overwrite when loading - mutables only. *)
    val F_negative     : Word8.word = 0wx10 (* sign bit for arbitrary precision ints *)
    val F_weak         : Word8.word = 0wx20 (* object contains weak references to option values. *)
    val F_mutable      : Word8.word = 0wx40 (* object is mutable *)
    val F_gc           : Word8.word = 0wx80 (* object is (pointer or depth) tombstone *)

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
    val typeMask : Word8.word = 0wx03;

    fun isType (t: Word8.word) (a: address):bool = 
        Word8.andb(flags a, typeMask) = t

  in
    val isWords = isType F_words;
    val isBytes = isType F_bytes;
    val isCode  = isType F_code;
    val isStack = isType F_stack;

    (* The mutable flag may be used with any of the others. *)
    fun isMutable a = Word8.andb(flags a, F_mutable) = F_mutable
  end;
end

end; (* open RuntimeCalls *)
