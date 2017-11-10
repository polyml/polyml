(*
    Title:      Standard Basis Library: Word32 Structure
    Author:     David Matthews
        Achim D. Brucker
    Copyright   David Matthews 1999, 2017
                Achim D. Brucker 2006 
    
        This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

(* 
  This is a hacked version of Word32 - a 32bit Word implementation for
  PolyML 5. It's neither well tested nor efficiently implemented.
  Nevertheless, it works well enough for the XML parser fxp
  (http://www2.in.tum.de/~berlea/Fxp/).
*)

(* This version is for the 32-in-64 architecture.
   We have to use LargeWord here because Word is only 31-bits. *)

(* We need to declare a Word64 structure as well. *)
structure Word64 = LargeWord;

val () = if Word64.wordSize <> 64 then raise Fail "Not 64-bit" else ()

structure Word32 :> WORD =
struct
    open LargeWord
    
    (* Values of type Word32.word can be in the range 0.. 4294967295 *)
    val wordSize = 32
    val maxWord: LargeInt.int = 4294967295
    val maxWordAsWord: word = (LargeWord.fromLargeInt maxWord)
    infix 8 << >> ~>> ;

    (* Comparison operations, min, max and compare, fmt, toString,
       orb, andb, xorb can be inherited directly from LargeWord.
       Similarly div, mod and >> since the results will always be no
       larger than the arguments. *)

    (* Not the same as Word.notb because it only affects the bottom 32 bits.  *)
    fun notb x = xorb(maxWordAsWord, x)

    (* Internal function to convert from Word.word. *)
    fun fromWord (w: LargeWord.word) = andb(w, maxWordAsWord)

    (* Converting from LargeWord.word.  First convert to LargeWord.word and
       then mask. *)
    val fromLargeWord = fromWord o LargeWord.fromLargeWord
    and fromInt = fromWord o LargeWord.fromInt
    and fromLargeInt = fromWord o LargeWord.fromLargeInt

    val fromLarge = fromLargeWord

            (* Arithmetic shift - sign extends. *) 
    (* Shift the "sign" bit into the real sign bit position then
       shift right down again. *)
    local
        val toSignBit = (Word.fromInt(Int.-(LargeWord.wordSize,wordSize)))
    in
        fun op ~>> (a: word, b: Word.word): word = 
            fromWord(LargeWord.~>>(LargeWord.<<(a, toSignBit), Word.+(b, toSignBit)))
    
        (* Convert to a large word by sign extending. *)
        fun toLargeWordX (w: word): LargeWord.word =
            LargeWord.~>>(LargeWord.toLargeWordX(LargeWord.<<(w, toSignBit)), toSignBit)
    end
    val toLargeX = toLargeWordX
        
    (* Conversion to signed integer. *)
    fun toIntX (x: word) : int = LargeWord.toIntX(toLargeWordX x)
    and toLargeIntX (x: word) : LargeInt.int = LargeWord.toLargeIntX(toLargeWordX x)
    
    (* Use Word.scan but check that the result is in the range. *)
    val wordScan = scan;

    fun scan radix getc src =
        case wordScan radix getc src of
            NONE => NONE
         |  SOME(res, src') =>
                if res > maxWordAsWord
                then raise General.Overflow
                else SOME(res, src')

    val fromString = StringCvt.scanString (scan StringCvt.HEX)

    (* TODO: Replace by built-ins? *)
    fun op + (a, b) = fromWord(LargeWord.+(a, b))
    and op - (a, b) = fromWord(LargeWord.-(a, b))
    and op * (a, b) = fromWord(LargeWord.*(a, b))
    and op << (a, b) = fromWord(LargeWord.<<(a, b))

    fun ~ x = 0w0 - x

end;



(* Because we are using opaque signature matching we have to install
   type-dependent functions OUTSIDE the structure. *)
local
        (* The string may be either 0wnnn or 0wxXXX *)
        fun convWord s : Word32.word =
                let
                val radix =
                        (* The word value must consist of at least 0w and a digit. *)
                        if String.sub(s, 2) = #"x" then StringCvt.HEX else StringCvt.DEC
                in
                        case StringCvt.scanString (Word32.scan radix) s of
                                NONE => raise RunCall.Conversion "Invalid Word32.word constant"
                          | SOME res => res
                end

        (* Install the pretty printer for Word32.word *)
        fun pretty _ _ x = PolyML.PrettyString("0wx" ^ Word32.toString x)
in
        val () = RunCall.addOverload convWord "convWord"
        val () = PolyML.addPrettyPrinter pretty
end;




(* Add the overloaded operators. *)
val () = RunCall.addOverload Word32.~ "~";
val () = RunCall.addOverload Word32.+ "+";
val () = RunCall.addOverload Word32.- "-";
val () = RunCall.addOverload Word32.* "*";
val () = RunCall.addOverload Word32.div "div";
val () = RunCall.addOverload Word32.mod "mod";
val () = RunCall.addOverload Word32.< "<";
val () = RunCall.addOverload Word32.> ">";
val () = RunCall.addOverload Word32.<= "<=";
val () = RunCall.addOverload Word32.>= ">=";
