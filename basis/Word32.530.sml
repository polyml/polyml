(*
    Title:      Standard Basis Library: Word32 Structure
    Author:     David Matthews
        Achim D. Brucker
    Copyright   David Matthews 1999
                Achim D. Brucker 2006 
    
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
  This is a hacked version of Word32 - a 32bit Word implementation for
  PolyML 5. It's neither well tested nor efficiently implemented.
  Nevertheless, it works well enough for the XML parser fxp
  (http://www2.in.tum.de/~berlea/Fxp/).
*)

structure Word32 :> WORD =
struct
    (* This structure is largely derived from LargeWord but we use opaque
       signature matching so that it is a different type. *)
    open LargeWord
    
    (* Values of type Word32.word can be in the range 0.. 4294967295 *)

    val wordSize = 32
    val maxWord = 4294967295
    val maxWordAsWord: word = (LargeWord.fromInt maxWord)
    infix 8 << >> ~>> ;

    (* Comparison operations, min, max and compare, fmt, toString,
       orb, andb, xorb can be inherited directly from LargeWord.
       Similarly div, mod and >> since the results will always be no
       larger than the arguments. *)

    (* Not the same as Word.notb because it only affects the bottom 32 bits.  *)
    fun notb x = xorb(maxWordAsWord, x)

    (* Converting from LargeWord.word.  First convert to Word.word and
       then mask. *)
    fun fromLargeWord (w: LargeWord.word) =   andb(w, maxWordAsWord)
    val fromLarge = fromLargeWord

    fun fromInt (i: int): word = fromLargeWord(LargeWord.fromInt i)

            (* Arithmetic shift - sign extends. *) 
    (* Shift the "sign" bit into the real sign bit position then
       shift right down again. *)
    local
        val toSignBit = (Word.fromInt(Int.-(LargeWord.wordSize,wordSize)))
    in
        fun op ~>> (a: word, b: Word.word): word = 
            fromLargeWord(LargeWord.~>>(LargeWord.<<(a, toSignBit), Word.+(b, toSignBit)))  
    
        (* Convert to a large word by sign extending. *)
        fun toLargeWordX (w: word): LargeWord.word =
            LargeWord.~>>(LargeWord.<<(toLargeWord w, toSignBit), toSignBit)
    end
    val toLargeX = toLargeWordX
        
    (* Conversion to signed integer. *)
    fun toIntX (x: word) : int = LargeWord.toIntX(toLargeWordX x)
    val toLargeIntX = toIntX
    
    (* Use LargeWord.scan but check that the result is in the range. *)
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
    fun op + (a, b) = fromLargeWord(LargeWord.+(a, b))
    and op - (a, b) = fromLargeWord(LargeWord.-(a, b))
    and op * (a, b) = fromLargeWord(LargeWord.*(a, b))
    and op << (a, b) = fromLargeWord(LargeWord.<<(a, b))

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
                                NONE => raise RunCall.Conversion "Invalid word8 constant"
                          | SOME res => res
                end

        (* Install the pretty printer for Word32.word *)
        fun pretty _ _ x = PolyML.PrettyString("0wx" ^ Word32.toString x)
in
        val () = RunCall.addOverload convWord "convWord"
        val () = PolyML.addPrettyPrinter pretty
end;




(* Add the overloaded operators. *)
RunCall.addOverload Word32.~ "~";
RunCall.addOverload Word32.+ "+";
RunCall.addOverload Word32.- "-";
RunCall.addOverload Word32.* "*";
RunCall.addOverload Word32.div "div";
RunCall.addOverload Word32.mod "mod";
RunCall.addOverload Word32.< "<";
RunCall.addOverload Word32.> ">";
RunCall.addOverload Word32.<= "<=";
RunCall.addOverload Word32.>= ">=";
