(*
    Title:      Standard Basis Library: Word and LargeWord Structure
    Author:     David Matthews
    Copyright   David Matthews 1999, 2005

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

(* G&R 2004 status: updated. *)

(*
This file contains definitions of both LargeWord and Word.  SysWord is
defined to be LargeWord.
The only purpose of LargeWord is so that it can be used, as SysWord, to
hold the full machine word values for certain operating-system calls.
*)

(* This uses the global definition of type "word" made in the compiler.
   That type has special status as the default for literals of the form
   0wn in the absence of any other type information. *)
local
    open RuntimeCalls;
    type largeword = {hi: word, lo: word}

    (* Extract a word value from a character stream. *)
    (* There's a complication here which is similar to that with 0x for
       Int.scan.  A word value may, optionally, be preceded by 0w or
       for hex values 0wx, 0wX, 0x or 0X.  Since this is optional it is
       possible for the value after the 0w to be anything, not just a
       valid number, in which case the result is the 0 and the continuation
       is w... *)
    fun scanWord radix getc src =
        let
        (* Some of this code duplicates code in Int.scan.  It would
           be better to avoid that if we could. The difficulty is that
           Int.scan allows the number to begin with a sign and also
           another 0x for hex values. *)
        val base =
            case radix of
                StringCvt.BIN => 2
              | StringCvt.OCT => 8
              | StringCvt.DEC => 10
              | StringCvt.HEX => 16
        
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

        fun read_number src =
            case radix of
                StringCvt.HEX => read_hex_digits src 0 false
              | _ => (* Binary, octal and decimal *) read_digits src 0 false
        in
        case getc src of
            NONE => NONE
         |  SOME(#"0", src') =>
            let (* May be the start of the number or may be 0w, 0x etc. *)
                val after0 = 
                    case getc src' of
                        NONE => NONE
                      | SOME(ch, src'') =>
                        if ch = #"w"
                        then if radix = StringCvt.HEX
                        then (* Is it 0wx, 0wX ? *)
                            (
                            case getc src'' of
                                NONE => NONE
                              | SOME(ch, src''') =>
                                if ch = #"x" orelse ch = #"X"
                                then read_number src''' (* Skip the 0wx *)
                                else read_number src'' (* Skip the 0w *)
                            )
                        else read_number src'' (* Skip the 0w *)
                        else if (ch = #"x" orelse ch = #"X") andalso radix = StringCvt.HEX
                        then read_number src''
                        else read_number src (* Include the 0 in the input *)
            in
                (* If the string *)
                case after0 of
                    NONE => (* No valid number after it, return the zero .*)
                        SOME(0, src')
                  | res => res
            end

         |  SOME(ch, src') =>
                if Char.isSpace ch (* Skip white space. *)
                then scanWord radix getc src' (* Recurse *)
                else (* See if it's a valid digit. *)
                    read_number src
        end (* scanWord *)

    (* Conversion from integer may involve extracting the low-order word
       from a long-integer representation.  *)
    local
        (* Load the first word of a long form arbitrary precision
           number which is always little-endian, tag it and negate
           it if necessary. *)
        val getFirstWord: int -> word =
            RunCall.run_call1 POLY_SYS_get_first_long_word
    in
        (* We previously had a single RTS function to do this.  I've
           replaced that by this code.  Since most of the time we're
           going to be converting short integers this will avoid
           making an RTS call.  getFirstWord can be implemented in
           the code-generator fairly easily on little-endian machines
           but it's too difficult to do it on the Sparc. *)
        fun wordFromInt (i: int): word =
            if LibrarySupport.isShortInt i
            then RunCall.unsafeCast i
            else getFirstWord i
    end

    (* The maximum word value is given by the smallest integer power of two
       which when converted to word gives zero, less one. *)
    val zero = (* 0w *) wordFromInt 0

    local
        fun power2' n 0 = n
         |  power2' n i = power2' (2*n) (i-1)
        val power2 = power2' 1
        fun findlong i =
            if wordFromInt(power2 i) = zero then i else findlong(i+1)
    in
        val wordSize = findlong 1
        val maxWordP1 = power2 wordSize
        val maxWord = maxWordP1 - 1
        val maxLongWord = power2 (wordSize*2) - 1
        val maxWordAsWord = wordFromInt maxWord
        val wordSizeAsWord = wordFromInt wordSize
    end

    structure Words :>
    sig
        (* The result signatures are quite complicated because the general
           signature includes LargeWord.word and Word.word explicitly in a
           few cases.  We have to make variants of them to handle the fact that
           we are declaring Word and LargeWord. *)
        type smallword = word
        type largeword
        structure Word:
        sig
            type  word = word
            val wordSize : int
            val toLarge : word -> largeword
            val toLargeX : word -> largeword
            val toLargeWord : word -> largeword
            val toLargeWordX : word -> largeword
            val fromLarge : largeword -> word
            val fromLargeWord : largeword -> word
            val toLargeInt : word -> LargeInt.int
            val toLargeIntX : word -> LargeInt.int
            val fromLargeInt : LargeInt.int -> word
            val toInt : word -> Int.int
            val toIntX : word -> Int.int
            val fromInt : Int.int -> word
            val orb : (word * word) -> word
            val xorb : (word * word) -> word
            val andb : (word * word) -> word
            val notb : word -> word
            val << : (word * word) -> word
            val >> : (word * word) -> word
            val ~>> : (word * word) -> word
            val + : (word * word) -> word
            val - : (word * word) -> word
            val * : (word * word) -> word
            val div : (word * word) -> word
            val mod : (word * word) -> word
            val ~ : word -> word
            val compare : (word * word) -> General.order
            val > : (word * word) -> bool
            val < : (word * word) -> bool
            val >= : (word * word) -> bool
            val <= : (word * word) -> bool
            val min : (word * word) -> word
            val max : (word * word) -> word
            val fmt : StringCvt.radix -> word -> string
            val toString : word -> string
            val fromString : string -> word option
            val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> 'a -> (word * 'a) option
        end
        and LargeWord:
        sig
            eqtype  word
            val wordSize : int
            val toLarge : word -> word
            val toLargeX : word -> word
            val toLargeWord : word -> word
            val toLargeWordX : word -> word
            val fromLarge : word -> word
            val fromLargeWord : word -> word
            val toLargeInt : word -> LargeInt.int
            val toLargeIntX : word -> LargeInt.int
            val fromLargeInt : LargeInt.int -> word
            val toInt : word -> Int.int
            val toIntX : word -> Int.int
            val fromInt : Int.int -> word
            val orb : (word * word) -> word
            val xorb : (word * word) -> word
            val andb : (word * word) -> word
            val notb : word -> word
            val << : (word * smallword) -> word
            val >> : (word * smallword) -> word
            val ~>> : (word * smallword) -> word
            val + : (word * word) -> word
            val - : (word * word) -> word
            val * : (word * word) -> word
            val div : (word * word) -> word
            val mod : (word * word) -> word
            val ~ : word -> word
            val compare : (word * word) -> General.order
            val > : (word * word) -> bool
            val < : (word * word) -> bool
            val >= : (word * word) -> bool
            val <= : (word * word) -> bool
            val min : (word * word) -> word
            val max : (word * word) -> word
            val fmt : StringCvt.radix -> word -> string
            val toString : word -> string
            val fromString : string -> word option
            val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> 'a -> (word * 'a) option
        end

        (* These sharing constraints ensure that although we are using opaque
           matching we retain the correct sharing. *)
        sharing type LargeWord.word = largeword (* Abstract *)
    end =
    struct
        type largeword = largeword and smallword = word
        structure Word =
        struct
            
            (* Word.word is represented using the short (tagged) integer format.
               It is, though, unsigned so large word values are represented in the
               same form as negative integers.  *)
            type word = word
            val fromInt = wordFromInt
            and wordSize = wordSize

            (* Conversion to signed integer is simple. *)
            val toIntX: word->int = RunCall.unsafeCast
            
            (* Conversion to unsigned integer has to treat values with the sign bit
               set specially. *)
            fun toInt x =
                let
                    val signed = toIntX x
                in
                    if signed < 0 then maxWordP1+signed else signed
                end

            fun scan radix getc src =
                case scanWord radix getc src of
                    NONE => NONE
                |   SOME(res, src') =>
                        if res > maxWord then raise General.Overflow
                        else SOME(fromInt res, src')
    
            (* TODO: Implement this directly? *)
            val fromString = StringCvt.scanString (scan StringCvt.HEX)
        
            val toLargeInt : word -> LargeInt.int = toInt
            and toLargeIntX : word -> LargeInt.int = toIntX
            and fromLargeInt : LargeInt.int -> word = fromInt

            infix >> << ~>>

            val op + : word*word->word = RunCall.run_call2 POLY_SYS_plus_word
            and op - : word*word->word = RunCall.run_call2 POLY_SYS_minus_word
            and op * : word*word->word = RunCall.run_call2 POLY_SYS_mul_word
            and op div : word*word->word = RunCall.run_call2 POLY_SYS_div_word
            and op mod : word*word->word = RunCall.run_call2 POLY_SYS_mod_word
            and orb : word*word->word = RunCall.run_call2 POLY_SYS_or_word
            and andb : word*word->word = RunCall.run_call2 POLY_SYS_and_word
            and xorb : word*word->word = RunCall.run_call2 POLY_SYS_xor_word
            and op >> : word*word->word = RunCall.run_call2 POLY_SYS_shift_right_word
            and op << : word*word->word = RunCall.run_call2 POLY_SYS_shift_left_word
            and op ~>> : word*word->word = RunCall.run_call2 POLY_SYS_shift_right_arith_word
            
            local
                val signShift = fromInt(Int.-(wordSize,1))
            in
                fun toLargeWord x = {hi=zero, lo=x}
                and toLargeWordX x = (* Sign extend. *)
                    {hi = x ~>> signShift, lo = x}
                and fromLargeWord (x: largeword) = #lo x

                fun ~ x = zero - x
            end
            val toLarge = toLargeWord and toLargeX = toLargeWordX and fromLarge = fromLargeWord
        
            fun notb x = xorb(maxWordAsWord, x)
            
            (* We can format the result using the integer format function. *)
            fun fmt radix i = Int.fmt radix (toInt i)
            val toString = fmt StringCvt.HEX
            
            val op > : word*word->bool = RunCall.run_call2 POLY_SYS_word_gtr
            and op < : word*word->bool = RunCall.run_call2 POLY_SYS_word_lss
            and op >= : word*word->bool = RunCall.run_call2 POLY_SYS_word_geq
            and op <= : word*word->bool = RunCall.run_call2 POLY_SYS_word_leq
        
            fun compare (i, j) =
                if i < j then General.LESS
                else if i > j then General.GREATER else General.EQUAL
            
            fun min (i, j) = if i < j then i else j
            and max (i, j) = if i > j then i else j
        end (* Word *)

        structure LargeWord =
        struct
            type word = largeword
            val wordSize = 2*wordSize;

            (* As this is LargeWord we don't need to do anything here. *)
            fun toLargeWord x = x
            and toLargeWordX x = x
            and fromLargeWord x = x
            val toLarge = toLargeWord and toLargeX = toLargeWordX and fromLarge = fromLargeWord

            (* The values are treated as lo + 2^n * hi *)
             (* Avoid doing long precision arithmetic unnecessarily. *)
            fun fromInt x = { lo = Word.fromInt x,
                              hi = if LibrarySupport.isShortInt x then if x < 0 then 0w0-0w1 else 0w0
                                   else Word.fromInt (Int.div(x, maxWordP1)) }
            and toIntX { lo, hi=0w0 } = Word.toInt lo
            |   toIntX { lo, hi } = Word.toInt lo + Word.toIntX hi * maxWordP1
            and toInt { lo, hi=0w0 } = Word.toInt lo
            |   toInt { lo, hi } = Word.toInt lo + Word.toInt hi * maxWordP1

            val toLargeInt = toInt
            val toLargeIntX = toIntX
            val fromLargeInt = fromInt

            fun scan radix getc src =
                case scanWord radix getc src of
                    NONE => NONE
                |   SOME(res, src') =>
                        if res > maxLongWord then raise General.Overflow
                        else SOME(fromInt res, src')
    
            (* TODO: Implement this directly? *)
            val fromString = StringCvt.scanString (scan StringCvt.HEX)

            (* Logical operations are fairly simple. *)
            fun orb ({hi, lo}, {hi=hi', lo=lo'}) =
                {hi=Word.orb(hi, hi'), lo=Word.orb(lo, lo')}
            and xorb ({hi, lo}, {hi=hi', lo=lo'}) =
                {hi=Word.xorb(hi, hi'), lo=Word.xorb(lo, lo')}
            and andb ({hi, lo}, {hi=hi', lo=lo'}) =
                {hi=Word.andb(hi, hi'), lo=Word.andb(lo, lo')}
            and notb {hi, lo} = {hi=Word.notb hi, lo=Word.notb lo}

            (* Shifts are a bit more difficult. *)
            fun op << ({hi, lo}, x) =
                if Word.>=(x, wordSizeAsWord)
                then {hi=Word.<<(lo, Word.-(x, wordSizeAsWord)), lo=zero}
                else {lo= Word.<<(lo, x),
                      hi = Word.orb(Word.<<(hi, x),
                             Word.>>(lo, Word.-(wordSizeAsWord, x)))}

            and op >> ({hi, lo}, x) =
                if Word.>=(x, wordSizeAsWord)
                then {lo=Word.>>(hi, Word.-(x, wordSizeAsWord)), hi=zero}
                else {hi= Word.>>(hi, x),
                      lo= Word.orb(Word.>>(lo, x),
                            Word.<<(hi, Word.-(wordSizeAsWord, x)))}

            and op ~>> ({hi, lo}, x) =
                if Word.>=(x, wordSizeAsWord)
                then {lo=Word.~>>(hi, Word.-(x, wordSizeAsWord)),
                      hi=Word.~>>(hi, wordSizeAsWord) (* Just leaves the sign bit. *)}
                else {hi= Word.~>>(hi, x),
                      (* No sign propagation into low-order word. *)
                      lo= Word.orb(Word.>>(lo, x),
                             Word.<<(hi, Word.-(wordSizeAsWord, x)))}

            (* Arithmetic is most easily done just by converting to integer and
               back again. *)
            fun op + (x, y) = fromInt(Int.+(toInt x, toInt y))
            and op - (x, y) = fromInt(Int.-(toInt x, toInt y))
            and op * (x, y) = fromInt(Int.*(toInt x, toInt y))
            and op div (x, y) = fromInt(Int.quot(toInt x, toInt y))
            and op mod (x, y) = fromInt(Int.rem(toInt x, toInt y))
            and ~ x = fromInt(Int.~(toInt x))

            fun op > ({hi, lo}, {hi=hi', lo=lo'}) =
                case Word.compare (hi, hi') of
                    General.GREATER => true
                |   General.EQUAL => Word.>(lo, lo')
                |   General.LESS => false

            and op < ({hi, lo}, {hi=hi', lo=lo'}) =
                case Word.compare (hi, hi') of
                    General.GREATER => false
                |   General.EQUAL => Word.<(lo, lo')
                |   General.LESS => true

            and op >= ({hi, lo}, {hi=hi', lo=lo'}) =
                case Word.compare (hi, hi') of
                    General.GREATER => true
                |   General.EQUAL => Word.>=(lo, lo')
                |   General.LESS => false

            and op <= ({hi, lo}, {hi=hi', lo=lo'}) =
                case Word.compare (hi, hi') of
                    General.GREATER => false
                |   General.EQUAL => Word.<=(lo, lo')
                |   General.LESS => true

            and compare({hi, lo}, {hi=hi', lo=lo'}) =
                case Word.compare (hi, hi') of
                    General.EQUAL => Word.compare(lo, lo')
                |   compHi => compHi

            fun min (i, j) = if i < j then i else j
            and max (i, j) = if i > j then i else j

            (* We can format the result using the integer format function. *)
            fun fmt radix i = Int.fmt radix (toInt i)
            val toString = fmt StringCvt.HEX
        end;
    
    end (* Words *)
    local
        (* Install the pretty printer for Word.word *)
        fun prettyWord _ _ x =
            PolyML.PrettyString("0wx" ^ Words.Word.toString x)
        and prettyLarge _ _ x =
            PolyML.PrettyString("0wx" ^ Words.LargeWord.toString x)
    in
        val () = PolyML.addPrettyPrinter prettyWord
        val () = PolyML.addPrettyPrinter prettyLarge
    end

in
    structure Word = Words.Word;
    structure LargeWord = Words.LargeWord;
end;

(* Converter to word values.  These must be installed outside the structure
   because they depend on the type identifiers. *)
local

    (* The string may be either 0wnnn or 0wxXXX *)
    fun getRadix s =
        if String.size s > 2 andalso String.sub(s, 2) = #"x"
        then StringCvt.HEX else StringCvt.DEC

    fun convWord s =
        let
        val radix = getRadix s
        in
            case StringCvt.scanString (Word.scan radix) s of
                NONE => raise RunCall.Conversion "Invalid word constant"
              | SOME res => res
        end
    and convLarge s =
        let
        val radix = getRadix s
        in
            case StringCvt.scanString (LargeWord.scan radix) s of
                NONE => raise RunCall.Conversion "Invalid word constant"
              | SOME res => res
        end

in
    (* Install this as a conversion function for word literals.
       Unlike other overloaded functions there's no need to
       ensure that overloaded conversion functions are installed
       at the top-level.  The compiler has type "word" built in
       and will use this conversion function for literals of the
       form 0w... in preference to any other (e.g. for Word8.word)
       if unification does not give an explicit type.
       However, because LargeWord.word is abstract we have to
       install the convertor outside the structure. *)
    val () = RunCall.addOverload convWord "convWord"
    val () = RunCall.addOverload convLarge "convWord"
end;



(* Add the overloaded operators.  Do this outside the structure so
   that we can capture the inline code.  We've already done this for
   word (=Word.word) in the prelude. *)

RunCall.addOverload LargeWord.~ "~";
RunCall.addOverload LargeWord.+ "+";
RunCall.addOverload LargeWord.- "-";
RunCall.addOverload LargeWord.* "*";
RunCall.addOverload LargeWord.div "div";
RunCall.addOverload LargeWord.mod "mod";
RunCall.addOverload LargeWord.< "<";
RunCall.addOverload LargeWord.> ">";
RunCall.addOverload LargeWord.<= "<=";
RunCall.addOverload LargeWord.>= ">=";


(* This signature is defined in terms of Word and LargeWord so 
   we have to define it after the structures.  *)
signature WORD =
  sig
    eqtype  word
    val wordSize : int
    val toLarge : word -> LargeWord.word
    val toLargeX : word -> LargeWord.word
    val toLargeWord : word -> LargeWord.word
    val toLargeWordX : word -> LargeWord.word
    val fromLarge : LargeWord.word -> word
    val fromLargeWord : LargeWord.word -> word
    val toLargeInt : word -> LargeInt.int
    val toLargeIntX : word -> LargeInt.int
    val fromLargeInt : LargeInt.int -> word
    val toInt : word -> Int.int
    val toIntX : word -> Int.int
    val fromInt : Int.int -> word
    val orb : (word * word) -> word
    val xorb : (word * word) -> word
    val andb : (word * word) -> word
    val notb : word -> word
    val << : (word * Word.word) -> word
    val >> : (word * Word.word) -> word
    val ~>> : (word * Word.word) -> word
    val + : (word * word) -> word
    val - : (word * word) -> word
    val * : (word * word) -> word
    val div : (word * word) -> word
    val mod : (word * word) -> word
    val ~ : word -> word
    val compare : (word * word) -> General.order
    val > : (word * word) -> bool
    val < : (word * word) -> bool
    val >= : (word * word) -> bool
    val <= : (word * word) -> bool
    val min : (word * word) -> word
    val max : (word * word) -> word
    val fmt : StringCvt.radix -> word -> string
    val toString : word -> string
    val fromString : string -> word option
    val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (word, 'a) StringCvt.reader
  end;


