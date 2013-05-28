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


signature AddressSig =
sig
    type machineWord
    type address
    type handler
    type short = Word.word

    val stringOfWord: machineWord -> string

    val wordEq : 'a * 'a -> bool

    val isShort   : machineWord -> bool

    exception Cast of string
    val toMachineWord: 'a    -> machineWord
    val toShort:         machineWord -> Word.word
    val toAddress:       machineWord -> address

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
    val F_negative     : Word8.word
    val F_mutable      : Word8.word
    val F_gc           : Word8.word
    val F_noOverwrite  : Word8.word
    val F_weak         : Word8.word
    val F_profile      : Word8.word

    val isWords : address -> bool
    val isBytes : address -> bool
    val isCode  : address -> bool
    val isMutable:address -> bool

    val call: (address * machineWord) -> machineWord

    val isIoAddress : address -> bool

    (* The following function is VERY unsafe and should only be
    used by the Poly/ML code generator *)    
    val offsetAddr : address * short -> handler
end

structure Address :> AddressSig =

struct
    type machineWord    = word (* a legal ML object (tag = 0 or 1) *)
    and address  = word (* a normal pointer (tag = 0) *)
    and handler  = word (* pointer to exception handler (tag = 2!) *)
    and short    = word (* a 31/63-bit int  (tag = 1) *)


    open RuntimeCalls

  (* pointer equality *)
  val wordEq = PolyML.pointerEq
  val unsafeCast : 'a -> 'b = RunCall.unsafeCast

  val isShort : machineWord->bool = RunCall.run_call1 POLY_SYS_is_short

  (* The following cast is always safe *)
  val toMachineWord : 'a    -> machineWord = unsafeCast

  (* The following casts need checking *)
  exception Cast of string
   
  fun toAddress (w: machineWord) : address =
    if isShort w then raise Cast "toAddress" else unsafeCast w

    fun toShort (w: machineWord) : Word.word =
        if isShort w then unsafeCast w else raise Cast "toShort"

  
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
    RunCall.run_call2 POLY_SYS_load_byte
    
  val loadWord:   (address * Word.word) -> machineWord  =
    RunCall.run_call2 POLY_SYS_load_word

  val assignByte: (address * Word.word * Word8.word) -> unit =
    RunCall.run_call3 POLY_SYS_assign_byte
  
  val assignWord: (address * Word.word * machineWord)  -> unit =
    RunCall.run_call3 POLY_SYS_assign_word


    val maxAllocation: word = RunCall.run_call2 POLY_SYS_process_env(100, ())

    fun alloc(len: word, flags: Word8.word, initial: machineWord): address =
        (* Zero sized-objects are not allowed.  Check that the size is within
           the acceptable range. *)
        if len = 0w0 orelse len >= maxAllocation
        then raise Size
        else RunCall.run_call3 POLY_SYS_alloc_store(len, flags, initial)
  
  val lock:   address -> unit =
    RunCall.run_call1 POLY_SYS_lockseg
  
  val getFlags: address -> Word8.word =
    RunCall.run_call1 POLY_SYS_get_flags

  val setFlags: address * Word8.word -> unit =
    RunCall.run_call2 POLY_SYS_code_flags

  val wordSize: int =
    RunCall.run_call0 POLY_SYS_bytes_per_word ()

  val length: address -> Word.word =
     RunCall.run_call1 POLY_SYS_get_length
  
  val flags:  address -> Word8.word  =
    RunCall.run_call1 POLY_SYS_get_flags
  
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
    RunCall.run_call1 POLY_SYS_callcode_tupled
  
    val F_words        : Word8.word = 0wx00 (* word object - contains pointers and/or tagged values. *)
    val F_bytes        : Word8.word = 0wx01 (* byte object (contains no pointers) *)
    val F_code         : Word8.word = 0wx02 (* code object (mixed bytes and words) *)
    val F_noOverwrite  : Word8.word = 0wx08 (* don't overwrite when loading - mutables only. *)
    val F_negative     : Word8.word = 0wx10 (* sign bit for arbitrary precision ints (byte objects) *)
    val F_profile      : Word8.word = 0wx10 (* object has a profile pointer (word objects) *)
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
    RunCall.run_call2 POLY_SYS_offset_address
  
  local
    val typeMask : Word8.word = 0wx03

    fun isType (t: Word8.word) (a: address):bool = 
        Word8.andb(flags a, typeMask) = t

  in
    val isWords = isType F_words
    val isBytes = isType F_bytes
    val isCode  = isType F_code

    (* The mutable flag may be used with any of the others. *)
    fun isMutable a = Word8.andb(flags a, F_mutable) = F_mutable
  end

    local
        val rtsTable =
            [
            (POLY_SYS_exit,"POLY_SYS_exit"),
            (POLY_SYS_chdir,"POLY_SYS_chdir"),
            (POLY_SYS_alloc_store,"POLY_SYS_alloc_store"),
            (POLY_SYS_alloc_uninit,"POLY_SYS_alloc_uninit"),
            (POLY_SYS_raisex,"POLY_SYS_raisex"),
            (POLY_SYS_get_length,"POLY_SYS_get_length"),
            (POLY_SYS_get_flags,"POLY_SYS_get_flags"),
            (POLY_SYS_str_compare,"POLY_SYS_str_compare"),
            (POLY_SYS_teststrgtr,"POLY_SYS_teststrgtr"),
            (POLY_SYS_teststrlss,"POLY_SYS_teststrlss"),
            (POLY_SYS_teststrgeq,"POLY_SYS_teststrgeq"),
            (POLY_SYS_teststrleq,"POLY_SYS_teststrleq"),
            (POLY_SYS_exception_trace,"POLY_SYS_exception_trace"),
            (POLY_SYS_give_ex_trace,"POLY_SYS_give_ex_trace"),
            (POLY_SYS_exception_trace_fn,"POLY_SYS_exception_trace_fn"),
            (POLY_SYS_give_ex_trace_fn,"POLY_SYS_give_ex_trace_fn"),
            (POLY_SYS_lockseg,"POLY_SYS_lockseg"),
            (POLY_SYS_emptystring,"POLY_SYS_emptystring"),
            (POLY_SYS_nullvector,"POLY_SYS_nullvector"),
            (POLY_SYS_network,"POLY_SYS_network"),
            (POLY_SYS_os_specific,"POLY_SYS_os_specific"),
            (POLY_SYS_eq_longword,"POLY_SYS_eq_longword"),
            (POLY_SYS_neq_longword,"POLY_SYS_neq_longword"),
            (POLY_SYS_geq_longword,"POLY_SYS_geq_longword"),
            (POLY_SYS_leq_longword,"POLY_SYS_leq_longword"),
            (POLY_SYS_gt_longword,"POLY_SYS_gt_longword"),
            (POLY_SYS_lt_longword,"POLY_SYS_lt_longword"),
            (POLY_SYS_io_dispatch,"POLY_SYS_io_dispatch"),
            (POLY_SYS_signal_handler,"POLY_SYS_signal_handler"),
            (POLY_SYS_atomic_reset,"POLY_SYS_atomic_reset"),
            (POLY_SYS_atomic_incr,"POLY_SYS_atomic_incr"),
            (POLY_SYS_atomic_decr,"POLY_SYS_atomic_decr"),
            (POLY_SYS_thread_self,"POLY_SYS_thread_self"),
            (POLY_SYS_thread_dispatch,"POLY_SYS_thread_dispatch"),
            (POLY_SYS_plus_longword,"POLY_SYS_plus_longword"),
            (POLY_SYS_minus_longword,"POLY_SYS_minus_longword"),
            (POLY_SYS_mul_longword,"POLY_SYS_mul_longword"),
            (POLY_SYS_div_longword,"POLY_SYS_div_longword"),
            (POLY_SYS_mod_longword,"POLY_SYS_mod_longword"),
            (POLY_SYS_andb_longword,"POLY_SYS_andb_longword"),
            (POLY_SYS_orb_longword,"POLY_SYS_orb_longword"),
            (POLY_SYS_xorb_longword,"POLY_SYS_xorb_longword"),
            (POLY_SYS_kill_self,"POLY_SYS_kill_self"),
            (POLY_SYS_shift_left_longword,"POLY_SYS_shift_left_longword"),
            (POLY_SYS_shift_right_longword,"POLY_SYS_shift_right_longword"),
            (POLY_SYS_shift_right_arith_longword,"POLY_SYS_shift_right_arith_longword"),
            (POLY_SYS_longword_to_tagged,"POLY_SYS_longword_to_tagged"),
            (POLY_SYS_signed_to_longword,"POLY_SYS_signed_to_longword"),
            (POLY_SYS_unsigned_to_longword,"POLY_SYS_unsigned_to_longword"),
            (POLY_SYS_profiler,"POLY_SYS_profiler"),
            (POLY_SYS_full_gc,"POLY_SYS_full_gc"),
            (POLY_SYS_stack_trace,"POLY_SYS_stack_trace"),
            (POLY_SYS_timing_dispatch,"POLY_SYS_timing_dispatch"),
            (POLY_SYS_objsize,"POLY_SYS_objsize"),
            (POLY_SYS_showsize,"POLY_SYS_showsize"),
            (POLY_SYS_is_short,"POLY_SYS_is_short"),
            (POLY_SYS_quotrem,"POLY_SYS_quotrem"),
            (POLY_SYS_aplus,"POLY_SYS_aplus"),
            (POLY_SYS_aminus,"POLY_SYS_aminus"),
            (POLY_SYS_amul,"POLY_SYS_amul"),
            (POLY_SYS_adiv,"POLY_SYS_adiv"),
            (POLY_SYS_amod,"POLY_SYS_amod"),
            (POLY_SYS_aneg,"POLY_SYS_aneg"),
            (POLY_SYS_xora,"POLY_SYS_xora"),
            (POLY_SYS_equala,"POLY_SYS_equala"),
            (POLY_SYS_ora,"POLY_SYS_ora"),
            (POLY_SYS_anda,"POLY_SYS_anda"),
            (POLY_SYS_Real_str,"POLY_SYS_Real_str"),
            (POLY_SYS_Real_geq,"POLY_SYS_Real_geq"),
            (POLY_SYS_Real_leq,"POLY_SYS_Real_leq"),
            (POLY_SYS_Real_gtr,"POLY_SYS_Real_gtr"),
            (POLY_SYS_Real_lss,"POLY_SYS_Real_lss"),
            (POLY_SYS_Real_eq,"POLY_SYS_Real_eq"),
            (POLY_SYS_Real_neq,"POLY_SYS_Real_neq"),
            (POLY_SYS_Real_Dispatch,"POLY_SYS_Real_Dispatch"),
            (POLY_SYS_Add_real,"POLY_SYS_Add_real"),
            (POLY_SYS_Sub_real,"POLY_SYS_Sub_real"),
            (POLY_SYS_Mul_real,"POLY_SYS_Mul_real"),
            (POLY_SYS_Div_real,"POLY_SYS_Div_real"),
            (POLY_SYS_Abs_real,"POLY_SYS_Abs_real"),
            (POLY_SYS_Neg_real,"POLY_SYS_Neg_real"),
            (POLY_SYS_Repr_real,"POLY_SYS_Repr_real"),
            (POLY_SYS_conv_real,"POLY_SYS_conv_real"),
            (POLY_SYS_real_to_int,"POLY_SYS_real_to_int"),
            (POLY_SYS_int_to_real,"POLY_SYS_int_to_real"),
            (POLY_SYS_sqrt_real,"POLY_SYS_sqrt_real"),
            (POLY_SYS_sin_real,"POLY_SYS_sin_real"),
            (POLY_SYS_cos_real,"POLY_SYS_cos_real"),
            (POLY_SYS_arctan_real,"POLY_SYS_arctan_real"),
            (POLY_SYS_exp_real,"POLY_SYS_exp_real"),
            (POLY_SYS_ln_real,"POLY_SYS_ln_real"),
            (POLY_SYS_stdin,"POLY_SYS_stdin"),
            (POLY_SYS_stdout,"POLY_SYS_stdout"),
            (POLY_SYS_process_env,"POLY_SYS_process_env"),
            (POLY_SYS_set_string_length,"POLY_SYS_set_string_length"),
            (POLY_SYS_get_first_long_word,"POLY_SYS_get_first_long_word"),
            (POLY_SYS_poly_specific,"POLY_SYS_poly_specific"),
            (POLY_SYS_bytevec_eq, "POLY_SYS_bytevec_eq"),
            (POLY_SYS_io_operation,"POLY_SYS_io_operation"),
            (POLY_SYS_set_code_constant,"POLY_SYS_set_code_constant"),
            (POLY_SYS_move_words,"POLY_SYS_move_words"),
            (POLY_SYS_shift_right_arith_word,"POLY_SYS_shift_right_arith_word"),
            (POLY_SYS_move_bytes,"POLY_SYS_move_bytes"),
            (POLY_SYS_code_flags,"POLY_SYS_code_flags"),
            (POLY_SYS_shrink_stack,"POLY_SYS_shrink_stack"),
            (POLY_SYS_stderr,"POLY_SYS_stderr"),
            (POLY_SYS_callcode_tupled,"POLY_SYS_callcode_tupled"),
            (POLY_SYS_foreign_dispatch,"POLY_SYS_foreign_dispatch"),
            (POLY_SYS_XWindows,"POLY_SYS_XWindows"),
            (POLY_SYS_is_big_endian,"POLY_SYS_is_big_endian"),
            (POLY_SYS_bytes_per_word,"POLY_SYS_bytes_per_word"),
            (POLY_SYS_offset_address,"POLY_SYS_offset_address"),
            (POLY_SYS_shift_right_word,"POLY_SYS_shift_right_word"),
            (POLY_SYS_word_neq,"POLY_SYS_word_neq"),
            (POLY_SYS_not_bool,"POLY_SYS_not_bool"),
            (POLY_SYS_string_length,"POLY_SYS_string_length"),
            (POLY_SYS_int_geq,"POLY_SYS_int_geq"),
            (POLY_SYS_int_leq,"POLY_SYS_int_leq"),
            (POLY_SYS_int_gtr,"POLY_SYS_int_gtr"),
            (POLY_SYS_int_lss,"POLY_SYS_int_lss"),
            (POLY_SYS_load_byte_immut,"POLY_SYS_load_byte_immut"),
            (POLY_SYS_load_word_immut,"POLY_SYS_load_word_immut"),
            (POLY_SYS_mul_word,"POLY_SYS_mul_word"),
            (POLY_SYS_plus_word,"POLY_SYS_plus_word"),
            (POLY_SYS_minus_word,"POLY_SYS_minus_word"),
            (POLY_SYS_div_word,"POLY_SYS_div_word"),
            (POLY_SYS_or_word,"POLY_SYS_or_word"),
            (POLY_SYS_and_word,"POLY_SYS_and_word"),
            (POLY_SYS_xor_word,"POLY_SYS_xor_word"),
            (POLY_SYS_shift_left_word,"POLY_SYS_shift_left_word"),
            (POLY_SYS_mod_word,"POLY_SYS_mod_word"),
            (POLY_SYS_word_geq,"POLY_SYS_word_geq"),
            (POLY_SYS_word_leq,"POLY_SYS_word_leq"),
            (POLY_SYS_word_gtr,"POLY_SYS_word_gtr"),
            (POLY_SYS_word_lss,"POLY_SYS_word_lss"),
            (POLY_SYS_word_eq,"POLY_SYS_word_eq"),
            (POLY_SYS_load_byte,"POLY_SYS_load_byte"),
            (POLY_SYS_load_word,"POLY_SYS_load_word"),
            (POLY_SYS_assign_byte,"POLY_SYS_assign_byte"),
            (POLY_SYS_assign_word,"POLY_SYS_assign_word")
            ]
    in
        val rtsNames =
            Vector.tabulate(256,
                fn n => case List.find(fn (rtsNo, _) => rtsNo=n) rtsTable of
                    SOME(_, name) => name | _ => " RTS" ^ Int.toString n)
    end

    local
        val doCall: int*machineWord -> string
            = RunCall.run_call2 RuntimeCalls.POLY_SYS_process_env
    in
        fun functionName w = doCall(105, w)
    end

    fun stringOfWord w =
    if isShort w
    then "LIT" ^ Word.toString (unsafeCast w)

    else if isIoAddress(toAddress w)
    then (* RTS call - print the number. *)
        let
            val ioOp : int -> machineWord = RunCall.run_call1 RuntimeCalls.POLY_SYS_io_operation

            fun matchIo n =
                if n = 256 then raise Fail "Unknown RTS entry"
                else if wordEq (w, ioOp n)
                then Vector.sub(rtsNames, n)
                else matchIo (n+1)
        in
            matchIo 0
        end

    else if isCode(toAddress w)
    then "CODE \"" ^ functionName w ^ "\""
    
    else if isWords(toAddress w) andalso Word.toInt(length(toAddress w)) >= 1
    then (* If it's the closure of a function try to print that. *)
        let
            val firstWord = loadWord(toAddress w, 0w0)
        in
            if not (isShort firstWord) andalso isCode(toAddress firstWord)
            then "FUN \"" ^ functionName firstWord ^ "\"" (* Get the function name. *)
            else "LIT <long>"
        end
    else "LIT <long>"

end;

(* Add a print function for machineWord.  This is really only for
   the debugger but prevents addresses being printed as Word.word values. *)
local
    open PolyML Address
    fun printMachineWord _ _ w = PrettyString(stringOfWord w)
in
    val () = addPrettyPrinter printMachineWord
end;

