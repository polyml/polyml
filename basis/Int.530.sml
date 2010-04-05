(*
    Title:      Standard Basis Library: Int and LargeInt structures
    Author:     David Matthews
    Copyright   David Matthews 1999

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

(* G&R 2004 status: done. Very minor change to type of "scan". *)

structure Int:
  sig
    (* This signature is almost the same as INTEGER except that
       occurrences of Int and LargeInt are removed.  The type is the
       same in all cases so that doesn't matter. *)
    eqtype  int
    val toLarge : int -> (*LargeInt.*)int
    val fromLarge : (*LargeInt.*)int -> int
    val toInt : int -> (*Int.*)int
    val fromInt : (*Int.*)int -> int
    val precision : (*Int.*)int option

    val minInt : int option
    val maxInt : int option

    val ~ : int -> int
    val * : (int * int) -> int
    val div : (int * int) -> int
    val mod : (int * int) -> int
    val quot : (int * int) -> int
    val rem : (int * int) -> int
    val + : (int * int) -> int
    val - : (int * int) -> int
    val compare : (int * int) -> General.order

    val > : (int * int) -> bool
    val >= : (int * int) -> bool
    val < : (int * int) -> bool
    val <= : (int * int) -> bool

    val abs : int -> int
    val min : (int * int) -> int
    val max : (int * int) -> int
    val sign : int -> (*Int.*)int
    val sameSign : (int * int) -> bool
    val fmt : StringCvt.radix -> int -> string
    val toString : int -> string
    val fromString : string -> int option
    val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader
  end
=
struct
    open RuntimeCalls; (* for POLY_SYS and EXC numbers *)
    
    (* In Poly/ML we use arbitrary precision for the normal integer.
       That uses a short representation which is typically 30 or 31 bit
       and a long representation which is a pointer to a segment of bytes. *)
    type int = int (* Underlying type *)
    
    (* Since LargeInt and Int are the same these are identity functions. *)
    fun toLarge i = i
    and fromLarge i = i
    and toInt i = i
    and fromInt i = i
    
    val precision = NONE (* Arbitrary precision. *)
    and minInt = NONE
    and maxInt = NONE
    
    infix 7 quot rem
    val op quot: int * int -> int = RunCall.run_call2 POLY_SYS_adiv
    and op rem:  int * int -> int = RunCall.run_call2 POLY_SYS_amod

    (* TODO: There was a bug in the i386 RTS which caused the wrong
       exception to be raised for divide-by-zero.  It's been fixed in
       the Windows version.  Check other RTS's, in particular Linux. *)
    fun x mod y =
        let
        val r = x rem y (* must handle divide-by-zero *)
        (* NB: Unlike ML 90 this function raises Div if y is zero, not Mod *)
        in
        if r = 0 orelse (y >= 0) = (r >= 0) then r else r + y
        end;

    fun x div y =
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

    fun compare (i, j) =
        if i < j then General.LESS
        else if i > j then General.GREATER else General.EQUAL
    
    fun abs i = if i >= 0 then i else ~ i
    
    fun min (i, j) = if i < j then i else j
    and max (i, j) = if i > j then i else j
    
    fun sign i = if i = 0 then 0 else if i < 0 then ~1 else 1
    
    (* It might be possible to do something clever by xor-ing the 
       words together when both values are short. *)
    fun sameSign(i, j) =
        if i = 0 then j = 0
        else if i < 0 then j < 0
        else (* i > 0 *) j > 0

    (* Local function *)
    fun baseOf StringCvt.BIN = 2
     |  baseOf StringCvt.OCT = 8
     |  baseOf StringCvt.DEC = 10
     |  baseOf StringCvt.HEX = 16

    local
        open LibrarySupport
        val System_lock: string -> unit   = RunCall.run_call1 POLY_SYS_lockseg;
        val System_setb: string * word * char -> unit   = RunCall.run_call3 POLY_SYS_assign_byte;

        (* Int.toChars turned out to be a major allocation hot-spot in some Isabelle
           examples.  The old code created a list of the characters and then concatenated
           them.  This cost 3 words for each character before the actual string was
           created.  This version avoids that problem. *)
        
        fun toChar digit =
            if digit < 10 then Char.chr(Char.ord(#"0") + digit)
            else (* Hex *) Char.chr(Char.ord(#"A") + digit - 10)

        fun toChars(base, i, negative, chars: word) =
        let
            val digit = i rem base
            val ch = toChar digit
        in
            if i >= base
            then (* More to do *)
            let
                val (result, pos) = toChars(base, i quot base, negative, chars+0w1)
            in
                System_setb(result, pos, ch);
                (result, pos+0w1)
            end
            (* Finished.  Allocate the string. *)
            else if negative
            then
            let
                val res = allocString(chars+0w2)
            in
                System_setb(res, wordSize, #"~");
                System_setb(res, wordSize+0w1, ch);
                (res, wordSize+0w2)
            end
            else
            let
                val res = allocString(chars+0w1)
            in
                System_setb(res, wordSize, ch);
                (res, wordSize+0w1)
            end
        end
    in
        fun fmt radix i =
        let
            val base = baseOf radix
        in
            if i >= 0 andalso i < base
            then (* This will be a single character.  Treat specially. *)
                RunCall.unsafeCast(toChar i) : string
            else (* Multiple characters. *)
            let
                val (result, _) = toChars(base, abs i, i < 0, 0w0)
            in
                System_lock result;
                result
            end
        end
    end
    
    val toString = fmt StringCvt.DEC
    
    fun scan radix getc src =
        let
        val base = baseOf radix
        
        (* Read the digits, accumulating the result in acc.  isOk is true
           once we have read a valid digit. *)
        fun read_digits src acc isOk =
            case getc src of
                NONE => if isOk then SOME(acc, src) else NONE
              | SOME(ch, src') =>
                if Char.ord ch >= Char.ord #"0"
                   andalso Char.ord ch < (Char.ord #"0" + base)
                then read_digits src'
                        (acc*base + Char.ord ch - Char.ord #"0") true
                else (* Invalid character - either end of number or bad no. *)
                    if isOk then SOME(acc, src) else NONE
                    
        fun read_hex_digits src acc isOk =
            case getc src of
                NONE => if isOk then SOME(acc, src) else NONE
              | SOME(ch, src') =>
                if Char.ord ch >= Char.ord #"0"
                   andalso Char.ord ch <= Char.ord #"9"
                then read_hex_digits src'
                        (acc*16 + Char.ord ch - Char.ord #"0") true
                else if Char.ord ch >= Char.ord #"A"
                   andalso Char.ord ch <= Char.ord #"F"
                then read_hex_digits src'
                        (acc*16 + Char.ord ch - Char.ord #"A" + 10) true
                else if Char.ord ch >= Char.ord #"a"
                   andalso Char.ord ch <= Char.ord #"f"
                then read_hex_digits src'
                        (acc*16 + Char.ord ch - Char.ord #"a" + 10) true
                else (* Invalid character - either end of number or bad no. *)
                    if isOk then SOME(acc, src) else NONE

        (*
           There is a special case with hex numbers.  A hex number MAY begin
           with 0x or 0X e.g. 0x1f0 but need not.  So "0x " and "0xg" are
           both valid and represent the value 0 with "x " and "xg" as the
           continuations of the input.
        *)
        fun read_number src =
            if base = 16
            then (* Hex. *)
                (
                case getc src of
                    NONE => NONE
                  | SOME(ch, src') =>
                        if ch <> #"0"
                        then read_hex_digits src 0 false
                        else
                            (
                            case getc src' of
                                NONE => SOME(0, src') (* Accept the 0 *)
                              | SOME(ch, src'') =>
                                    if ch = #"x" orelse ch = #"X"
                                    then
                                        (
                                        (*
                                           See if the characters after the 0x
                                           form a valid hex number.  If so return
                                           that, if not return the 0 and treat
                                           the rest of the string as starting
                                           with the x. 
                                        *)
                                        case read_hex_digits src'' 0 false of
                                            NONE => SOME(0, src') (* Accept the 0 *)
                                          | res => res
                                        )
                                    else (* Start from the 0. *)
                                        read_hex_digits src 0 false
                            )
                )
            else (* Binary, octal and decimal *) read_digits src 0 false
        in
        case getc src of
            NONE => NONE
         |  SOME(ch, src') =>
            if Char.isSpace ch (* Skip white space. *)
            then scan radix getc src' (* Recurse *)
            else if ch = #"+" (* Remove the + sign *)
            then read_number src'
            else if ch = #"-" orelse ch = #"~"
            then
                (
                case read_number src' of
                    NONE => NONE
                  | SOME(i, r) => SOME(~i, r)
                )
            else (* See if it's a valid digit. *)
                read_number src
        end
    
    (* TODO: Implement this directly? *)
    val fromString = StringCvt.scanString (scan StringCvt.DEC)

    (* Converter to int values. This replaces the basic conversion
       function for ints installed in the bootstrap process. In
       particular this converter can handle hexadecimal. *)
    local
        (* The string may be either decimal or hex. *)
        (* TODO: We could rewrite scan so that it raises Conversion with
           a string giving more information and then handle.  It's
           possibly not worth it since the lexical analyser should only
           pass in a syntactically valid string. *)
        fun convInt s =
            let
            val radix =
                if String.size s >= 3 andalso String.substring(s, 0, 2) = "0x"
                   orelse String.size s >= 4 andalso String.substring(s, 0, 3) = "~0x"
                then StringCvt.HEX else StringCvt.DEC
            in
                case StringCvt.scanString (scan radix) s of
                    NONE => raise RunCall.Conversion "Invalid integer constant"
                  | SOME res => res
            end
            
    in
        (* Install this as a conversion function for integer literals.
           Unlike other overloaded functions there's no need to
           ensure that overloaded conversion functions are installed
           at the top-level.  The compiler has type "int" built in
           and will use this conversion function for literals of the
           form nnn... in preference to any other if unification does
           not give an explicit type. *)
        val () = RunCall.addOverload convInt "convInt"
    end 
    
    (* These are overloaded functions and are treated specially. *)
    (* Since they aren't overloaded in this structure
       we can pick up the underlying RTS functions. *)
    val ~ : int->int = RunCall.run_call1 POLY_SYS_aneg
    and op * : int*int->int = RunCall.run_call2 POLY_SYS_amul
    and op + : int*int->int = RunCall.run_call2 POLY_SYS_aplus
    and op - : int*int->int = RunCall.run_call2 POLY_SYS_aminus
    
    val op < : int*int->bool = RunCall.run_call2 POLY_SYS_int_lss
    and op > : int*int->bool = RunCall.run_call2 POLY_SYS_int_gtr
    and op <= : int*int->bool = RunCall.run_call2 POLY_SYS_int_leq
    and op >= : int*int->bool = RunCall.run_call2 POLY_SYS_int_geq
end;

local
    (* Install the pretty printer for int *)
    fun prettyInt _ _ x = PolyML.PrettyString(Int.toString x)
in
    val () = PolyML.addPrettyPrinter prettyInt
end;

structure LargeInt = Int
and Position = Int;
