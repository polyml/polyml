(*
    Copyright (c) 2000-2010
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
local
    val cReal: string->real = RunCall.run_call1 RuntimeCalls.POLY_SYS_conv_real
in
    val () = RunCall.addOverload cReal "convReal"
end;

(* Comparisons.  These are overloaded on strings, chars and word as well as int and real. *)
local
    val iOp: int*int->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_int_geq
    val sOp: string*string->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_teststrgeq
    val rOp: real*real->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_Real_geq
    val cOp: char*char->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_geq
    val wOp: word*word->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_geq
in
    val () = RunCall.addOverload iOp ">="
    val () = RunCall.addOverload sOp ">="
    val () = RunCall.addOverload rOp ">="
    val () = RunCall.addOverload cOp ">="
    val () = RunCall.addOverload wOp ">="
end;

local
    val iOp: int*int->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_int_leq
    val sOp: string*string->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_teststrleq
    val rOp: real*real->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_Real_leq
    val cOp: char*char->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_leq
    val wOp: word*word->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_leq
in
    val () = RunCall.addOverload iOp "<="
    val () = RunCall.addOverload sOp "<="
    val () = RunCall.addOverload rOp "<="
    val () = RunCall.addOverload cOp "<="
    val () = RunCall.addOverload wOp "<="
end;

local
    val iOp: int*int->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_int_gtr
    val sOp: string*string->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_teststrgtr
    val rOp: real*real->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_Real_gtr
    val cOp: char*char->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_gtr
    val wOp: word*word->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_gtr
in
    val () = RunCall.addOverload iOp ">"
    val () = RunCall.addOverload sOp ">"
    val () = RunCall.addOverload rOp ">"
    val () = RunCall.addOverload cOp ">"
    val () = RunCall.addOverload wOp ">"
end;

local
    val iOp: int*int->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_int_lss
    val sOp: string*string->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_teststrlss
    val rOp: real*real->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_Real_lss
    val cOp: char*char->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_lss
    val wOp: word*word->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_lss
in
    val () = RunCall.addOverload iOp "<"
    val () = RunCall.addOverload sOp "<"
    val () = RunCall.addOverload rOp "<"
    val () = RunCall.addOverload cOp "<"
    val () = RunCall.addOverload wOp "<"
end;

(* Binary operations. *)
local
    val iPlus: int*int->int = RunCall.run_call2 RuntimeCalls.POLY_SYS_aplus
    val rPlus: real*real->real = RunCall.run_call2 RuntimeCalls.POLY_SYS_Add_real
    val wPlus: word*word->word = RunCall.run_call2 RuntimeCalls.POLY_SYS_plus_word
in
    val () = RunCall.addOverload iPlus "+"
    val () = RunCall.addOverload rPlus "+"
    val () = RunCall.addOverload wPlus "+"
end;

local
    val iSub: int*int->int = RunCall.run_call2 RuntimeCalls.POLY_SYS_aminus
    val rSub: real*real->real = RunCall.run_call2 RuntimeCalls.POLY_SYS_Sub_real
    val wSub: word*word->word = RunCall.run_call2 RuntimeCalls.POLY_SYS_minus_word
in
    val () = RunCall.addOverload iSub "-"
    val () = RunCall.addOverload rSub "-"
    val () = RunCall.addOverload wSub "-"
end;

local
    val iMul: int*int->int = RunCall.run_call2 RuntimeCalls.POLY_SYS_amul
    val rMul: real*real->real = RunCall.run_call2 RuntimeCalls.POLY_SYS_Mul_real
    val wMul: word*word->word = RunCall.run_call2 RuntimeCalls.POLY_SYS_mul_word
in
    val () = RunCall.addOverload iMul "*"
    val () = RunCall.addOverload rMul "*"
    val () = RunCall.addOverload wMul "*"
end;

(* Unary operations. *)
local
    val iNeg: int->int = RunCall.run_call1 RuntimeCalls.POLY_SYS_aneg
    val rNeg: real->real = RunCall.run_call1 RuntimeCalls.POLY_SYS_Neg_real
    
    val zero: word = RunCall.unsafeCast 0
    fun wNeg x = zero - x
in
    val () = RunCall.addOverload iNeg "~"
    val () = RunCall.addOverload rNeg "~"
    val () = RunCall.addOverload wNeg "~"
end;

local
    fun iAbs (i: int): int = if i >= 0 then i else ~ i

    fun rAbs (x: real): real = if x >= 0.0 then x else ~ x

    val zero: word = RunCall.unsafeCast 0
    fun wAbs x = if x >= zero then x else ~ x
in
    val () = RunCall.addOverload iAbs "abs"
    val () = RunCall.addOverload rAbs "abs"
    val () = RunCall.addOverload wAbs "abs"
end;


(* div, mod and / are overloaded in ML97.  *)
local
    val rDiv: real * real -> real = RunCall.run_call2 RuntimeCalls.POLY_SYS_Div_real
in
    val () = RunCall.addOverload rDiv "/"
end;
  
    local
      infix 7 quot
      val op quot: int * int -> int = RunCall.run_call2 RuntimeCalls.POLY_SYS_adiv;

      fun iDiv (x, y) =
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
      end
    in
        val () = RunCall.addOverload iDiv "div"
    end;

    local
      infix 7 rem
      val op rem:  int * int -> int = RunCall.run_call2 RuntimeCalls.POLY_SYS_amod;

      fun iMod(x, y) =
      let
        val r = (x rem y) (* must handle divide-by-zero *)
          (* handle Div => raise Mod; *)
      in
        if r = 0 orelse (y >= 0) = (r >= 0) then r else r + y
      end;
    in
        val () = RunCall.addOverload iMod "mod"
    end;

    local
        val wDiv : word*word->word = RunCall.run_call2 RuntimeCalls.POLY_SYS_div_word
        val wMod: word*word->word = RunCall.run_call2 RuntimeCalls.POLY_SYS_mod_word
    in
        val () = RunCall.addOverload wDiv "div"
        val () = RunCall.addOverload wMod "mod"
    end;

    (* We need to use the same identifier for this that we used when
       compiling the compiler, particularly "make". *)
    exception Fail = RunCall.Fail

(* A few useful functions which are in the top-level environment.
   Others are added later. *)

(* This is built in because the length of a string is stored as
   an untagged integer.  The result needs to be a tagged integer.  Actually
   this might be better written using a separate function POLY_SYS_get_string_length
   (by analogy with POLY_SYS_set_string_length) and then
   fun size (s: string): int = if isShort s then 1 else POLY_SYS_get_string_length s;
   The advantage would be that the . *)
val size : string -> int =
      RunCall.run_call1 RuntimeCalls.POLY_SYS_string_length;

val str: char ->string = RunCall.unsafeCast;
val ord: char -> int = RunCall.unsafeCast;
val not: bool -> bool = RunCall.run_call1 RuntimeCalls.POLY_SYS_not_bool;
val use: string -> unit = Bootstrap.use; (* This will be replaced. *)

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

    val isShortString   : string -> bool = RunCall.run_call1 POLY_SYS_is_short

    fun convChar (s: string) : char =
    let
        val convS = Bootstrap.convString s
    in
        (* Should be exactly a single character i.e. a single short value. *)
        if isShortString convS then RunCall.unsafeCast convS
        else raise RunCall.Conversion "Bad character"
    end;

in
    val () = RunCall.addOverload convChar "convChar";
end;

(* Print functions.  Some of these are replaced by functions in the Basis library and
   are installed here merely so that we can get useful output if we get a failure while
   compiling it. *)
local
    open RuntimeCalls PolyML

    fun print_bool _ _ (b: bool) =
        PrettyString(if b then "true" else "false")

    fun print_string _ _ (s: string) = PrettyString s (* Not escaped at the moment. *)

    fun print_char _ _ (c: char) =
        PrettyBlock (0, false, [], [PrettyString "#", PrettyString(RunCall.unsafeCast c)])

      fun nil @ y = y (* This is redefined later. *)
      |  (a::b) @ y = a :: (b @ y)

    fun print_list depth printEl (l: 'a list) =
        let
        (* Print the list as [<elem>, <elem>, etc ]. Replace the
           rest of the list by ... once the depth reaches zero. *)
          fun plist [] _ = []
           |  plist _ 0 = [PrettyString "..."]
           |  plist [h]    depth = [printEl (h, depth)]
           |  plist (h::t) depth =
                    printEl (h, depth) ::
                    PrettyString "," ::
                    PrettyBreak (1, 0) ::
                    plist t (depth - 1)
                    
        in
          PrettyBlock (1, false, [], (* Wrap this in a begin-end block to keep it together. *)
            PrettyString "[" ::
                ((if depth <= 0 then [PrettyString "..."] else plist l depth) @
                [PrettyString "]"]
                )
            )
        end

    fun print_int _ _ (i: int) =
    let
        fun pr i =
           if i < 0 then PrettyString "~" :: pr (~ i)
           else if i < 10 then [PrettyString(RunCall.unsafeCast(i + RunCall.unsafeCast #"0"))]
           else pr(i div 10) @ [PrettyString(RunCall.unsafeCast(i mod 10 + 48))]
    in
        PrettyBlock(1, false, [], pr i)
    end
in
    val () = addPrettyPrinter print_bool
    val () = addPrettyPrinter print_string
    val () = addPrettyPrinter print_char
    val () = addPrettyPrinter print_list
    val () = addPrettyPrinter print_int
end;
