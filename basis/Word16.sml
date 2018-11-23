(*
    Title:      Standard Basis Library: Word16 Structure
    Author:     Domagoj Stolfa
    Copyright   Domagoj Stolfa 2018

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
 * This file is *heavily* based on David Matthews' work on Word8.
 *)

structure Word16 :> WORD =
struct
    open Word

    (* 16-bit words have values that range from 0...65535 and like Word8.word
       are implemented using tagged integers. *)
    val wordSize = 16
    val maxWord = 65535
    val maxWordAsWord: word = RunCall.unsafeCast maxWord

    infix 8 << >> ~>>

    (* Comparison operations, min, max and compare, fmt, toString,
       orb, andb, xorb can be inherited directly from Word.
       Similarly div and mod since the results will always be no
       larger than the arguments. *)

    (* This only works for the bottom 16 bits *)
    fun notb x = xorb(maxWordAsWord, x)

    (* Internal function to convert from Word.word. *)
    fun fromWord (w: Word.word) = andb(w, maxWordAsWord)

    (* Converting from LargeWord.word.  First convert to Word.word and
       then mask. *)
    val fromLargeWord = fromWord o Word.fromLargeWord
    and fromInt = fromWord o Word.fromInt
    and fromLargeInt = fromWord o Word.fromLargeInt

    val fromLarge = fromLargeWord

    local
        val toSignBit = Word.fromInt(Int.-(Word.wordSize,wordSize))
    in
        fun op ~>> (a: word, b: Word.word): word =
            fromWord(Word.~>>(Word.<<(a, toSignBit), Word.+(b, toSignBit)))
    end

    fun op << (a: word, b: Word.word): word = andb(Word.<<(a,b), maxWordAsWord)

    val toInt: word->int = RunCall.unsafeCast

    (* As with Word8.toIntX, this could be implemented with a logical shift
       followed by an arithmetic shift *)
    fun toIntX (x: word) : int =
        let
            val intx = toInt x
            open Int
        in
            if intx >= 32768
            then intx-maxWord-1
            else intx
        end

    val toLargeInt = LargeInt.fromInt o toInt
    and toLargeIntX = LargeInt.fromInt o toIntX

    fun toLargeWordX (w: word): LargeWord.word =
        LargeWord.fromInt(toIntX w);
    val toLargeX = toLargeWordX

    val wordScan = scan;

    fun scan radix getc src =
        case wordScan radix getc src of
            NONE => NONE
         |  SOME(res, src') =>
                if res > maxWordAsWord
                then raise General.Overflow
                else SOME(res, src')

    val fromString = StringCvt.scanString (scan StringCvt.HEX)

    fun op + (a, b) = fromWord(Word.+(a, b))
    and op - (a, b) = fromWord(Word.-(a, b))
    and op * (a, b) = fromWord(Word.*(a, b))

    fun ~ x = 0w0 - x

end;

(* Because we are using opaque signature matching we have to install
   type-dependent functions OUTSIDE the structure. *)
local
    fun convWord s : Word16.word =
        let
        val radix =
            if String.sub(s, 2) = #"x" then StringCvt.HEX else StringCvt.DEC
        in
            case StringCvt.scanString (Word16.scan radix) s of
                NONE => raise RunCall.Conversion "Invalid Word16 constant"
              | SOME res => res
        end

    fun pretty _ _ x = PolyML.PrettyString("0wx" ^ Word16.toString x)
in
    val () = RunCall.addOverload convWord "convWord"
    val () = PolyML.addPrettyPrinter pretty
end;

val () = RunCall.addOverload Word16.~ "~";
val () = RunCall.addOverload Word16.+ "+";
val () = RunCall.addOverload Word16.- "-";
val () = RunCall.addOverload Word16.* "*";
val () = RunCall.addOverload Word16.div "div";
val () = RunCall.addOverload Word16.mod "mod";
val () = RunCall.addOverload Word16.< "<";
val () = RunCall.addOverload Word16.> ">";
val () = RunCall.addOverload Word16.<= "<=";
val () = RunCall.addOverload Word16.>= ">=";
