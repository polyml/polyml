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
    Title:      ML prelude file.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

(* 10/2/94 SPF merged prelude files from AHL's v2.07X and Dave's v2.08 *)
(* 10/2/94 SPF inlined signatures to avoid name-space pollution        *)

(* For ML97 this prelude has been reduced to virtually nothing.  Almost
   everything is defined in the Standard Basis library. *)

(*****************************************************************************)
(*                  Infixes                                                  *)
(*****************************************************************************)
infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

(* Set up the overloading first so we can use it in the rest of the
   prelude. Setting up overloads is messy because of the need to capture
   the inline code for the functions. *)
 
  (* Conversion functions.  At this stage we have installed a simple
     conversion function for strings and ints. *)
  val it: string->real = RunCall.run_call1 RuntimeCalls.POLY_SYS_conv_real;
  RunCall.addOverload it "convReal";

  (* Comparisons.  These are overloaded on strings, chars and word as well as int and real. *)

  val it: int*int->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_int_geq;
  RunCall.addOverload it ">=";
  val it: string*string->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_teststrgeq;
  RunCall.addOverload it ">=";
  val it: real*real->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_Real_geq;
  RunCall.addOverload it ">=";
  val it: char*char->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_geq;
  RunCall.addOverload it ">=";
  val it: word*word->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_geq;
  RunCall.addOverload it ">=";

  val it: int*int->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_int_leq;
  RunCall.addOverload it "<=";
  val it: string*string->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_teststrleq;
  RunCall.addOverload it "<=";
  val it: real*real->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_Real_leq;
  RunCall.addOverload it "<=";
  val it: char*char->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_leq;
  RunCall.addOverload it "<=";
  val it: word*word->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_leq;
  RunCall.addOverload it "<=";

  val it: int*int->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_int_gtr;
  RunCall.addOverload it ">";
  val it: string*string->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_teststrgtr;
  RunCall.addOverload it ">";
  val it: real*real->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_Real_gtr;
  RunCall.addOverload it ">";
  val it: char*char->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_gtr;
  RunCall.addOverload it ">";
  val it: word*word->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_gtr;
  RunCall.addOverload it ">";
  
  val it: int*int->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_int_lss;
  RunCall.addOverload it "<";
  val it: string*string->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_teststrlss;
  RunCall.addOverload it "<";
  val it: real*real->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_Real_lss;
  RunCall.addOverload it "<";
  val it: char*char->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_lss;
  RunCall.addOverload it "<";
  val it: word*word->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_lss;
  RunCall.addOverload it "<";
  
  (* Binary operations. *)
  val it: int*int->int = RunCall.run_call2 RuntimeCalls.POLY_SYS_aplus;
  RunCall.addOverload it "+";
  val it: real*real->real = RunCall.run_call2 RuntimeCalls.POLY_SYS_Add_real;
  RunCall.addOverload it "+";
  val it: word*word->word = RunCall.run_call2 RuntimeCalls.POLY_SYS_plus_word;
  RunCall.addOverload it "+";

  val it: int*int->int = RunCall.run_call2 RuntimeCalls.POLY_SYS_aminus;
  RunCall.addOverload it "-";
  val it: real*real->real = RunCall.run_call2 RuntimeCalls.POLY_SYS_Sub_real;
  RunCall.addOverload it "-";
  val it: word*word->word = RunCall.run_call2 RuntimeCalls.POLY_SYS_minus_word;
  RunCall.addOverload it "-";

  val it: int*int->int = RunCall.run_call2 RuntimeCalls.POLY_SYS_amul;
  RunCall.addOverload it "*";
  val it: real*real->real = RunCall.run_call2 RuntimeCalls.POLY_SYS_Mul_real;
  RunCall.addOverload it "*";
  val it: word*word->word = RunCall.run_call2 RuntimeCalls.POLY_SYS_mul_word;
  RunCall.addOverload it "*";

  (* Unary operations. *)
  val it: int->int = RunCall.run_call1 RuntimeCalls.POLY_SYS_aneg;
  RunCall.addOverload it "~";
  val it: real->real = RunCall.run_call1 RuntimeCalls.POLY_SYS_Neg_real;
  RunCall.addOverload it "~";
  local
    val zero: word = RunCall.run_call1 RuntimeCalls.POLY_SYS_int_to_word 0
  in
    fun it x = zero - x
  end;
  RunCall.addOverload it "~";

  fun it (i: int): int = if i >= 0 then i else ~ i;
  RunCall.addOverload it "abs";
  fun it (x: real): real = if x >= 0.0 then x else ~ x;
  RunCall.addOverload it "abs";
  local
  	val zero: word = RunCall.run_call1 RuntimeCalls.POLY_SYS_int_to_word 0
  in
    fun it x = if x >= zero then x else ~ x
  end;
  RunCall.addOverload it "abs";
	
  (* Equality functions.  In this case we
     are not actually adding overloadings (equality is
	 handled specially by the type system) but telling
	 the compiler about functions which provide faster
	 implementation of equality than the general-purpose
	 structure equality. *)
  (* POLY_SYS_equala performs equality checking on both short and long
     versions of arbitrary precision integers. *)
  val it: int*int->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_equala;
  RunCall.addOverload it "=";
  (* POLY_SYS_int_eq does a simple bitwise comparison on its
     arguments which is sufficient if the values are always short. *)
  val it: bool*bool->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_int_eq;
  RunCall.addOverload it "=";
  val it: char*char->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_int_eq;
  RunCall.addOverload it "=";
  val it: string*string->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_teststreq;
  RunCall.addOverload it "=";
  (* POLY_SYS_word_eq is the same as POLY_SYS_int_eq in the memory mapped
     version of Poly/ML.  It used to be different in the persistent store
	 version because it was possible for two pointers to the same object
	 to have different values.  *)
  (* Adding an overload for equality on 'a ref has the effect of allowing
     equality on all ref types. *)
  local
    (* This is even more messy than usual because we have a type variable
	   in the type. *)
    val f : word*word->bool =
		RunCall.run_call2 RuntimeCalls.POLY_SYS_word_eq
  in
   fun it (x: 'a ref, y: 'a ref) = RunCall.unsafeCast f (x,y)
  end;
  RunCall.addOverload it "=";
  val it : word * word -> bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_eq;
  RunCall.addOverload it "=";


  local
  	val not = RunCall.run_call1 RuntimeCalls.POLY_SYS_not_bool
  in
    fun it(i:int, j: int): bool = not(RunCall.run_call2 RuntimeCalls.POLY_SYS_equala(i, j))
  end;
  RunCall.addOverload it "<>";
  val it: bool*bool->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_int_neq;
  RunCall.addOverload it "<>";
  val it: char*char->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_int_neq;
  RunCall.addOverload it "<>";
  val it: string*string->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_teststrneq;
  RunCall.addOverload it "<>";
  local
    (* This is even more messy than usual because of the types. *)
    val f : word*word->bool =
		RunCall.run_call2 RuntimeCalls.POLY_SYS_word_neq
  in
   fun it (x: 'a ref, y: 'a ref) = RunCall.unsafeCast f (x,y)
  end;
  RunCall.addOverload it "<>";
  val it : word * word -> bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_neq;
  RunCall.addOverload it "<>";

  (* div, mod and / are overloaded in ML97.  *)
  val it: real * real -> real = RunCall.run_call2 RuntimeCalls.POLY_SYS_Div_real;
  RunCall.addOverload it "/";
  
	local
	  infix 7 quot
	  val op quot: int * int -> int = RunCall.run_call2 RuntimeCalls.POLY_SYS_adiv;
	in
	  fun it (x, y) =
	  let
	    (* If the signs differ the normal quot operation will give the wrong
	       answer. We have to round the result down by subtracting either y-1 or
	       y+1. This will round down because it will have the opposite sign to x *)
	    
	(* ...
	    val d = x - (if (y >= 0) = (x >= 0) then 0 else if y > 0 then y-1 else y+1)
	... *)
	    val xpos = x >= 0;
	    val ypos = y >= 0;
	
	    val d =
	      if xpos = ypos 
	        then x
	      else if ypos
	        then (x - (y - 1))
	        else (x - (y + 1))
	  in
	    d quot y (* may raise Div for divide-by-zero *)
	  end;
	end;
  RunCall.addOverload it "div";
 
	local
	  infix 7 rem
	  val op rem:  int * int -> int = RunCall.run_call2 RuntimeCalls.POLY_SYS_amod;
	in
	  fun it(x, y) =
	  let
	    val r = (x rem y) (* must handle divide-by-zero *)
	      (* handle Div => raise Mod; *)
	  in
	    if r = 0 orelse (y >= 0) = (r >= 0) then r else r + y
	  end;
	
	end;
  RunCall.addOverload it "mod";

  val it : word*word->word = RunCall.run_call2 RuntimeCalls.POLY_SYS_div_word;
  RunCall.addOverload it "div";
  val it : word*word->word = RunCall.run_call2 RuntimeCalls.POLY_SYS_mod_word;
  RunCall.addOverload it "mod";

local
	(* We need to use the same identifier for this that we used when
	   compiling the compiler, particularly "make". *)
    structure Fail =
      RunCall.Run_exception1
        (
          type ex_type = string;
          val ex_iden  = RuntimeCalls.EXC_Fail
        )
in
    exception Fail = Fail.ex
end;

(* A few useful functions which are in the top-level environment.
   Others are added later. *)
val op ^ : string * string -> string = 
	  RunCall.run_call2 RuntimeCalls.POLY_SYS_strconcat;

(* TODO: This need not be built into the RTS. *)
val size : string -> int =
	  RunCall.run_call1 RuntimeCalls.POLY_SYS_string_length;

val str: char ->string = RunCall.unsafeCast;
val ord: char -> int = RunCall.unsafeCast;
val not: bool -> bool = RunCall.run_call1 RuntimeCalls.POLY_SYS_not_bool;
val use: string -> unit = RunCall.Inner.use;

local
	val System_setw: (* 'a ref*int*'a*) word*int*word ->unit  =
			RunCall.run_call3 RuntimeCalls.POLY_SYS_assign_word
in
	fun (var: 'a ref) := (v: 'a) : unit =
			(RunCall.unsafeCast System_setw (var, 0, v); ())
end;

(* The following version of "o" currently gets optimised better. *)
fun (f o g) = fn x => f (g x); (* functional composition *)

fun length l =
	let
	(* Tail-recursive function. *)
	fun len [] i = i
	 |  len (_::l) i = len l (i+1)
	in
	len l 0
	end

local
	(* Temporary conversion function for characters. This is replaced in
	   the Char structure. *)
	open RuntimeCalls;

    structure Conversion =
      RunCall.Run_exception1
        (
          type ex_type = string;
          val ex_iden  = EXC_conversion
        );
    exception Conversion = Conversion.ex;

	val isShortString   : string -> bool = RunCall.run_call1 POLY_SYS_is_short

	fun convChar (s: string) : char =
	let
		val convS = PolyML.convString s
	in
		(* Should be exactly a single character i.e. a single short value. *)
		if isShortString convS then RunCall.unsafeCast convS
		else raise Conversion "Bad character"
	end;

	(* Temporary conversion function for word.  This is replaced in the Word
	   structure. *)
	val wordFromInt: int->word = RunCall.run_call1 POLY_SYS_int_to_word

    fun string_sub(s: string, i: int): string =
	   RunCall.run_call2 RuntimeCalls.POLY_SYS_string_sub(s, i+1)

	(* Highly inefficient version of substring but we will be
	   replacing this function shortly. *)
	fun substring(s: string, i: int, n: int): string =
		if n = 0 then ""
		else string_sub(s, i) ^ substring(s, i+1, n-1)

	(* We just remove the 0w at the start and convert it as an integer. *)
	fun convWord (s: string) : word =
		if size s < 3 then raise Conversion "Too short"
		else wordFromInt(PolyML.convInt(substring(s, 2, size s - 2)))
in
	val it = RunCall.addOverload convChar "convChar";
	val it = RunCall.addOverload convWord "convWord";	
end;

(* Print functions.  Some of these are replaced by functions in the Basis library and
   are installed here merely so that we can get useful output if we get a failure while
   compiling it. *)
local
	open RuntimeCalls

    val ord0 = ord #"0";
	val chr = RunCall.unsafeCast
  
    fun soi acc 0 = acc
    |   soi acc n = soi (chr (n mod 10 + ord0) ^ acc) (n div 10);
	
	fun stringOfInt 0 = "0"
    |   stringOfInt n = if n < 0 then "~" ^ soi "" (~n) else soi "" n;

	fun print_int (put, beg, brk, nd) depth _ (i: int) =
		put(stringOfInt i)

	fun print_bool (put, beg, brk, nd) depth _ (b: bool) =
		put(if b then "true" else "false")

	fun print_string (put, beg, brk, nd) depth _ (s: string) =
		put(s) (* Not escaped at the moment. *)

	fun print_char (put, beg, brk, nd) depth _ (c: char) =
		put("#" ^ (RunCall.unsafeCast c))

	fun print_list (put, beg, brk, nd) depth printEl (l: 'a list) =
		let
		(* Print the list as [<elem>, <elem>, etc ]. Replace the
		   rest of the list by ... once the depth reaches zero. *)
	      fun plist [] depth = ()
	       |  plist _ 0 = put "..."
		   |  plist [h]    depth = printEl (h, depth) 
		   |  plist (h::t) depth =
			      ( printEl (h, depth);
					put ",";
					brk (1, 0);
					plist t (depth - 1)
			      )
	    in
	      beg (3, false); (* Wrap this in a begin-end block to keep it together. *)
	      put "[";
	      if depth <= 0 then put "..." else plist l depth;
	      put "]";
	      nd ()
	    end

in
  	val it = PolyML.Inner.install_pp print_int;
  	val it = PolyML.Inner.install_pp print_bool;
  	val it = PolyML.Inner.install_pp print_string;
  	val it = PolyML.Inner.install_pp print_char;
  	(*val it = PolyML.Inner.install_pp print_real;*)
  	val it = PolyML.Inner.install_pp print_list
end;
