(*
    Title:      Standard Basis Library: String Structure
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

(*
    This file declares Char, String and CharVector.  String and CharVector
    are simply different views on the same underlying structure.
*)

(* G&R 2004 status: Done *)

structure Char = struct type char = char end; (* We need this for "scan". *)
structure String = struct type string = string end; (* Needed for toString, fromString etc. *)

signature CHAR =
  sig
    eqtype  char
    eqtype  string
    
    val minChar : char
    val maxChar : char
    val maxOrd : int
    
    val ord : char -> int
    val chr : int -> char
    val succ : char -> char
    val pred : char -> char
    
    val < : (char * char) -> bool
    val <= : (char * char) -> bool
    val > : (char * char) -> bool
    val >= : (char * char) -> bool

    val compare : (char * char) -> General.order

    val contains : string -> char -> bool
    val notContains : string -> char -> bool

    val toLower : char -> char
    val toUpper : char -> char
    val isAlpha : char -> bool
    val isAlphaNum : char -> bool
    val isAscii : char -> bool
    val isCntrl : char -> bool
    val isDigit : char -> bool
    val isGraph : char -> bool
    val isHexDigit : char -> bool
    val isLower : char -> bool
    val isPrint : char -> bool
    val isSpace : char -> bool
    val isPunct : char -> bool
    val isUpper : char -> bool

    val fromString : String.string -> char option
    (* The argument to scan should be the global *)
    val scan : (Char.char, 'a) StringCvt.reader -> (char, 'a) StringCvt.reader
    val toString : char -> String.string
    val fromCString : String.string -> char option
    val toCString : char -> String.string

  end;


signature STRING =
  sig

    eqtype  string
    eqtype char

    val maxSize : int
    val size : string -> int
    val sub : (string * int) -> char
    val extract : (string * int * int option) -> string
    val substring : (string * int * int) -> string
    val concat : string list -> string
    val concatWith: string -> string list -> string
    val ^ : (string * string) -> string
    val str : char -> string
    val implode : char list -> string
    val explode : string -> char list
    val map : (char -> char) -> string -> string
    val translate : (char -> string) -> string -> string
    val tokens : (char -> bool) -> string -> string list
    val fields : (char -> bool) -> string -> string list
    val isPrefix : string -> string -> bool
    val isSubstring : string -> string -> bool
    val isSuffix : string -> string -> bool


    val compare : (string * string) -> General.order
    val collate : ((char * char) -> General.order) ->
                     (string * string) -> General.order
    val < : (string * string) -> bool
    val <= : (string * string) -> bool
    val > : (string * string) -> bool
    val >= : (string * string) -> bool

    val toString : string -> String.string
    val scan: (Char.char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader
    val fromString : String.string -> string option
    val toCString : string -> String.string
    val fromCString : String.string -> string option

  end;

signature SUBSTRING =
  sig
    type  substring
    eqtype char
    eqtype string
    val size : substring -> int
    val base : substring -> (string * int * int)
    val isEmpty : substring -> bool

    val sub : (substring * int) -> char
    val getc : substring -> (char * substring) option
    val first : substring -> char option
    
    val extract : (string * int * int option) -> substring
    val substring : (string * int * int) -> substring
    val slice : (substring * int * int option) -> substring
    val full: string -> substring
    val string : substring -> string
    
    val concat: substring list ->string
    val concatWith: string -> substring list ->string

    val explode : substring -> char list
    val translate : (char -> string) -> substring -> string
    val app : (char -> unit) -> substring -> unit
    val foldl : ((char * 'a) -> 'a) -> 'a -> substring -> 'a
    val foldr : ((char * 'a) -> 'a) -> 'a -> substring -> 'a
    val tokens : (char -> bool) -> substring -> substring list
    val fields : (char -> bool) -> substring -> substring list
    val isPrefix: string -> substring -> bool
    val isSubstring: string -> substring -> bool
    val isSuffix: string -> substring -> bool

    val compare : (substring * substring) -> General.order
    val collate : ((char * char) -> General.order) ->
                     (substring * substring) -> General.order

    val triml : int -> substring -> substring
    val trimr : int -> substring -> substring
    val splitl : (char -> bool) -> substring -> (substring * substring)
    val splitr : (char -> bool) -> substring -> (substring * substring)
    val splitAt : (substring * int) -> (substring * substring)
    val dropl : (char -> bool) -> substring -> substring
    val dropr : (char -> bool) -> substring -> substring
    val takel : (char -> bool) -> substring -> substring
    val taker : (char -> bool) -> substring -> substring
    val position : string -> substring -> (substring * substring)
    val span : (substring * substring) -> substring
  end;

(* The overloads for char and string for the relational operators have
   already been set up in the prelude.  *)
  
local
    open RuntimeCalls; (* for POLY_SYS and EXC numbers *)
    open LibrarySupport;

    val System_lock: string -> unit   = RunCall.run_call1 POLY_SYS_lockseg;
    val System_loadb: string*word->char = RunCall.run_call2 POLY_SYS_load_byte;
    val System_setb: string * word * char -> unit   = RunCall.run_call3 POLY_SYS_assign_byte;

    (* Redefine these as functions on the abstract type. *)
    val System_loadbA: address*word->char = RunCall.run_call2 POLY_SYS_load_byte;
    val System_setbA: address * word * char -> unit   = RunCall.run_call3 POLY_SYS_assign_byte;
    val System_move_bytesA:
        address*word*address*word*word->unit = RunCall.run_call5 POLY_SYS_move_bytes

    (* If a vector/string is short (i.e. has an integer tag) it must be the character
       itself rather than a pointer to a segment. *)
    val System_isShort   : string -> bool = RunCall.run_call1 POLY_SYS_is_short
    val wordSize : word = LibrarySupport.wordSize;

    local
        val mem_move: string*word*string*word*word -> unit = 
            RunCall.run_call5 POLY_SYS_move_bytes
    in
        fun bcopy(src: string, dst: string, src_off: word, dst_off: word, length: word) =
            mem_move(src, src_off, dst, dst_off, length)
    end

    (* These two functions are used to convert between single character
       strings and the character representation. *)
    val vecAsChar: string->char = RunCall.unsafeCast
    val charAsVec: char->string = RunCall.unsafeCast

    (* Casts between int and word. *)
    val intAsWord: int -> word = RunCall.unsafeCast
    and wordAsInt: word -> int = RunCall.unsafeCast

    (* String concatenation is currently built into the RTS although
       it doesn't need to be. *)
(*  val op ^ : string * string -> string = op ^ *)(* In prelude. *) 
    fun op ^ (a: string, b: string): string =
        let
            val a_length = sizeAsWord a
            and b_length = sizeAsWord b
        in
            (* Handle the special cases where one of the strings is
               empty.  As well as saving on duplicating storage it
               also means we don't have to consider the special
               case when the result string is a single character. *)
            if a_length = 0w0 then b
            else if b_length = 0w0 then a
            else (* Normal case *)
            let
                val vec = allocString(a_length + b_length)
            in
                if a_length = 0w1
                then System_setb (vec, wordSize, vecAsChar a)
                else bcopy(a, vec, wordSize, wordSize, a_length);
                if b_length = 0w1
                then System_setb (vec, wordSize+a_length, vecAsChar b)
                else bcopy(b, vec, wordSize, wordSize+a_length, b_length);
                System_lock vec;
                vec
            end
        end (* op ^ *)

    (* This can be used where we have already checked the range. *)
    val unsafeSub = LibrarySupport.unsafeStringSub;

    (* String comparison function used in isPrefix and isSuffix.
       N.B.  The caller must make sure that neither string is a single character. *)
    local
        val byteVecEq: string * word * string * word * word -> bool =
            RunCall.run_call5 POLY_SYS_bytevec_eq
    in
        fun byteMatch s1 s2 i j l =
            byteVecEq(s1, i+wordSize, s2, j+wordSize, l)
    end

    (* There's an irritating dependency here. Char uses StringCvt.reader
       which means that StringCvt depends on Char so String depends on
       StringCvt.  That means we can't define StringCvt in terms of String
       which would be easiest.  I've solved this by putting common code
       into LibrarySupport. *)
    structure Char =
        struct

        type char = char and string = string
        val maxOrd = 255 (* Range from 0 to 255 *)
        
        (* Single characters are represented by the number so we only need
           to check the argument and then convert it. *) 
        fun chr i : char =
            if i < 0 orelse i > maxOrd
            then raise General.Chr else RunCall.unsafeCast i
            
        val ord: char -> int = RunCall.unsafeCast
    
        val minChar = chr 0 and maxChar = chr maxOrd
        
        fun succ c = if ord c = maxOrd then raise Chr else chr(ord c + 1)
        and pred c = if ord c = 0 then raise Chr else chr(ord c - 1)
    
        fun isUpper c = #"A" <= c andalso c <= #"Z" 
        fun isLower c = #"a" <= c andalso c <= #"z" 
        fun isDigit c = #"0" <= c andalso c <= #"9" 
        fun isAlpha c = isUpper c orelse isLower c 
        fun isAlphaNum c = isAlpha c orelse isDigit c 
        fun isHexDigit c = 
            isDigit c orelse (#"a" <= c andalso c <= #"f")
                 orelse (#"A" <= c andalso c <= #"F") 
        fun isGraph c = #"!" <= c andalso c <= #"~" 
        fun isPrint c = isGraph c orelse c = #" " 
        fun isPunct c = isGraph c andalso not (isAlphaNum c)
        (* NOTE: The web page includes 0 <= ord c but all chars satisfy that. *)
        fun isAscii c = c <= chr 127 
        (* NOTE: The web page defines isCtrl not isCntrl *)
        fun isCntrl c = isAscii c andalso not (isPrint c) 
        (* NOTE: There's a mistake in the web page.  It says c <= #"\ " *)
        fun isSpace c = (#"\t" <= c andalso c <= #"\r") orelse c = #" "
        fun toLower c = if isUpper c then chr (ord c + 32) else c 
        fun toUpper c = if isLower c then chr (ord c - 32) else c 

        (* TODO: More efficient versions.
           Probably best to use comparison for short strings and table
           look-up for longer ones.  *)
        fun contains s =
            let
            fun match 0w0 _ = false
              | match i c = unsafeSub(s, i-0w1) = c orelse match (i-0w1) c
            in
            match (sizeAsWord s)
            end
            
        fun notContains s c = not (contains s c)

                
        (* Convert the first i digits as a hex number.  Check the result is
           in the range before returning it. *)
        local
        fun readHex' _    str 0 res =
                if res > maxOrd then NONE else SOME(chr res, str)
          | readHex' getc str i res = 
                case getc str of
                    NONE => (* No char available.  That's ok if we are converting
                               as many chars as we can and have already converted one
                               but not if we are converting n chars and haven't got
                               them *)
                        if i >= ~1 orelse res > maxOrd then NONE else SOME(chr res, str)
                  | SOME(ch, str') =>
                        if #"0" <= ch andalso ch <= #"9"
                        then readHex' getc str' (i-1) (res*16 + ord ch - ord #"0")
                        else if #"a" <= ch andalso ch <= #"f"
                        then readHex' getc str' (i-1) (res*16 + ord ch - ord #"a" + 10)
                        else if #"A" <= ch andalso ch <= #"F"
                        then readHex' getc str' (i-1) (res*16 + ord ch - ord #"A" + 10)
                        else (* Not a hex char. Ok if we are converting as many as we can. *)
                            if i >= ~1 orelse res > maxOrd then NONE else SOME(chr res, str)
        in
        fun readHexN getc str i = readHex' getc str i 0
        and readHex getc str = readHex' getc str ~1 0
        end
    
        (* Convert the first i digits as a decimal. There must be exactly i digits. *)  
        fun readDec _    str 0 res =
                if res > maxOrd then NONE else SOME(chr res, str)
          | readDec getc str i res = 
                case getc str of
                    NONE =>
                        if res > maxOrd orelse i > 0 (* not enough chars *) then NONE
                        else SOME(chr res, str)
                  | SOME(ch, str') =>
                        if #"0" <= ch andalso ord #"9" >= ord ch
                        then readDec getc str' (i-1) (res*10 + ord ch - ord #"0")
                        else (* Not enough valid digits. *) NONE

        (* Convert up to i digits as an octal number.  There may be fewer than i digits. *)
        fun readOct _    str 0 res =
                if res > maxOrd then NONE else SOME(chr res, str)
          | readOct getc str i res = 
                case getc str of
                    NONE =>
                        if res > maxOrd then NONE
                        else SOME(chr res, str)
                  | SOME(ch, str') =>
                        if #"0" <= ch andalso ord #"7" >= ord ch
                        then readOct getc str' (i-1) (res*8 + ord ch - ord #"0")
                        else (* Stop here. *) if res > maxOrd then NONE
                        else SOME(chr res, str)

            (* This function is used as the basis of Char.scan and String.scan.  There is a
               crucial difference between Char.scan and String.scan in that Char.scan returns
               NONE if it cannot read a single character whereas String.scan returns NONE only
               if it encounters a bad escape before reading any valid input, which includes a
               format sequence (\<whitespace>\).  This function returns NONE if it encounters
               a bad escape but SOME("", strm) if it encounters end-of-stream or has read a
               format sequence. *)
            fun scanBase (getc: (char, 'a) StringCvt.reader) (str :'a) : (string * 'a) option =
            case getc str of (* Read the first character. *)
                NONE => SOME("", str) (* Just end-of-stream. *)
              | SOME(ch, str') =>
                    if ch < chr 32 orelse chr 126 < ch
                    then NONE (* Non-printable character. *)
                    else if ch = #"\\"
                    then (* escape *)
                        (
                        case getc str' of
                            NONE => NONE
                          | SOME(#"a", str'') => SOME("\a", str'')
                          | SOME(#"b", str'') => SOME("\b", str'')
                          | SOME(#"t", str'') => SOME("\t", str'')
                          | SOME(#"n", str'') => SOME("\n", str'')
                          | SOME(#"v", str'') => SOME("\v", str'')
                          | SOME(#"f", str'') => SOME("\f", str'')
                          | SOME(#"r", str'') => SOME("\r", str'')
                          | SOME(#"\\", str'') => SOME("\\", str'')
                          | SOME(#"\"", str'') => SOME("\"", str'')
                          | SOME(#"^", str'') => (* Control char *)
                                    (
                                    case getc str'' of
                                        NONE => NONE
                                      | SOME(ch'', str''') =>
                                            if ord ch'' >= 64 andalso 95 >= ord ch''
                                            then SOME(charAsVec(chr(ord ch'' - 64)), str''')
                                            else NONE
                                    )
                          | SOME(#"u", str'') =>
                                (* Hex encoding: Read 4 hex digits *)
                                    (* NOTE: There's a contradiction in the web page:
                                       It says both 4 hex digits and also "the longest
                                       sequence of such characters"
                                     *)
                                     (case readHexN getc str'' 4 of NONE => NONE | SOME(s, str) => SOME(charAsVec s, str))
                          | SOME(ch', str'') =>
                                if isSpace ch'
                                then (* Remove \f...f\ and then recurse. *)
                                    (
                                    case getc (StringCvt.skipWS getc str'') of
                                        NONE => NONE
                                      | SOME(ch'', str''') =>
                                          if ch'' <> #"\\" then NONE (* Bad format *)
                                          else SOME("", str''') (* Return an empty string. *)
                                    )
                                else if #"0" <= ch' andalso ch' <= #"2"
                                then (* Decimal encoding *)
                                    (* NOTE: There's a contradiction in the web page:
                                       It says both 3 digits and also "the longest
                                       sequence of such characters".
                                       The tests insist on 3 digits so we go with
                                       that. *)
                                    (case readDec getc str' 3 0 of NONE => NONE | SOME(s, str) => SOME(charAsVec s, str))
                                else (* Unknown escape *) NONE
                        )
                    else SOME(charAsVec ch, str') (* Result is the character. *)

        fun scan (getc: (char, 'a) StringCvt.reader) (str :'a) : (char * 'a) option =
            case scanBase getc str of
                NONE => NONE
            |   SOME("", strm') => (* May be end-of-string or we may have read a format sequence. *)
                    (case getc strm' of NONE => (* end-of-string *) NONE | _ => scan getc strm')
            |   SOME(s, strm') => SOME(vecAsChar s, strm') (* Only ever a single character *)
    
        (* Convert C escapes *)
        (* This function does not appear in the CHAR signature but we need
           to export it from Char to use it in String.fromCString. *)
        fun scanC (getc: (char, 'a) StringCvt.reader) (str :'a) : (char * 'a) option =
            case getc str of (* Read the first character. *)
                NONE => NONE
              | SOME(ch, str') =>
                    if ch < chr 32 orelse chr 126 < ch
                    then NONE (* Non-printable character. *)
                    else if ch = #"\\"
                    then (* escape *)
                        (
                        case getc str' of
                            NONE => NONE
                          | SOME(#"a", str'') => SOME((*#"\a"*) chr 7, str'')
                          | SOME(#"b", str'') => SOME((*#"\b"*) chr 8, str'')
                          | SOME(#"t", str'') => SOME(#"\t", str'')
                          | SOME(#"n", str'') => SOME(#"\n", str'')
                          | SOME(#"v", str'') => SOME((*#"\v" *) chr 11, str'')
                          | SOME(#"f", str'') => SOME((*#"\f"*) chr 12, str'')
                          | SOME(#"r", str'') => SOME((*#"\r"*) chr 13, str'')
                          | SOME(#"?", str'') => SOME(#"?", str'')
                          | SOME(#"\\", str'') => SOME(#"\\", str'')
                          | SOME(#"\"", str'') => SOME(#"\"", str'')
                          | SOME(#"'", str'') => SOME(#"'", str'')
                          | SOME(#"^", str'') => (* Control char *)
                                    (
                                    case getc str'' of
                                        NONE => NONE
                                      | SOME(ch'', str''') =>
                                            if ord ch'' >= 64 andalso 95 >= ord ch''
                                            then SOME(chr(ord ch'' - 64), str''')
                                            else NONE
                                    )
                        (* Note: the web page says \u here but it seems it should
                           be \x.  That's confirmed by the latest version of
                           the library definition. *)
                          | SOME(#"x", str'') => (* Hex encoding. *)
                                     readHex getc str''
                          | SOME(ch', _) =>
                                if #"0" <= ch' andalso ch' <= #"7"
                                then (* Octal encoding *) readOct getc str' 3 0
                                else (* Unknown escape *) NONE
                        )
                    else SOME(ch, str') (* Result is the character. *)
    
        (* Convert from a string. *)
        (* TODO: More efficient conversion using the string directly rather
           than scanString ? *)
        val fromString = StringCvt.scanString scan
        and fromCString = StringCvt.scanString scanC
        
        (* Convert to printable string. *)
        local
            local
                (* Conversion to octal has now been defined to generate
                   three octal digits in the same way as conversion to
                   integer. *)
                fun octIntRepr base digs (i: int) =
                    if digs = 0 then ""
                    else octIntRepr base (digs-1) (i div base) ^
                            str(chr(i mod base + ord #"0"))
            in
                val intRepr = octIntRepr 10 3
                val octalRepr = octIntRepr 8 3
            end
        in
    
            (* Conversion to ML escapes. *)
            fun toString ch =
                (* First handle the special cases *)
                if ch = #"\\" then "\\\\"
                else if ch = #"\"" then "\\\""
                else if isPrint ch (* Other printable characters *)
                then str ch
                else (* Control chars: Special cases first *)
                    if ch = chr 7 then "\\a"
                else if ch = chr 8 then "\\b"
                else if ch = chr 9 then "\\t"
                else if ch = chr 10 then "\\n"
                else if ch = chr 11 then "\\v"
                else if ch = chr 12 then "\\f"
                else if ch = chr 13 then "\\r"
                else if ch < chr 32 (* Other chars must be escaped. *)
                then "\\^" ^ str(chr(ord ch + 64))
                else (* Use 3 digit notation. *)
                (* Note: Web site assumes ASCII, not Unicode. *)
                    "\\" ^ intRepr(ord ch)
    
            (* Conversion to C escapes. *)
            fun toCString ch =
                (* First handle the special cases *)
                if ch = #"\\" then "\\\\"
                else if ch = #"\"" then "\\\""
                else if ch = #"?" then "\\?"
                else if ch = #"'" then "\\'"
                else if isPrint ch (* Other printable characters *)
                then str ch
                else (* Control chars: Special cases first *)
                    if ch = chr 7 then "\\a"
                else if ch = chr 8 then "\\b"
                else if ch = chr 9 then "\\t"
                else if ch = chr 10 then "\\n"
                else if ch = chr 11 then "\\v"
                else if ch = chr 12 then "\\f"
                else if ch = chr 13 then "\\r"
                else (* Use octal notation. *)
                (* Note: Web site assumes ASCII, not Unicode. *)
                    "\\" ^ octalRepr(ord ch)
        end;
            
        (* Install conversion and print functions. *)
        local
            (* It might be worth rewriting scan to raise Conversion with
               a string argument so we can pass back information about
               why an escape code was invalid. *)
            fun convChar s =
                let
                val len = sizeAsWord s
                fun rdr i =
                    if i = len then NONE
                    else SOME(unsafeSub(s, i), i+0w1)
                in
                    case scan rdr 0w0 of
                        NONE => raise RunCall.Conversion "Invalid character constant"
                      | SOME(res, index') =>
                            (* Check that we have converted all the string. *)
                            if index' <> len
                            then raise RunCall.Conversion "Not exactly one character"
                            else res
                end

            fun print_char _ _ (c: char) =
                PolyML.PrettyString("#\"" ^ toString c ^ "\"")
        in
            val () = RunCall.addOverload convChar "convChar";
            val () = PolyML.addPrettyPrinter print_char
        end
    
        (* Define the type-specific inequalities. *)
        val op < : char * char -> bool = op <
        val op <= : char * char -> bool = op <=
        val op > : char * char -> bool = op >
        val op >= : char * char -> bool = op >=
    
        fun compare (ch, ch') =
            if ch < ch' then General.LESS
            else if ch > ch' then General.GREATER else General.EQUAL
    
        end; (* structure Char *)

    
    structure String =
    (* This structure is the basis of both String and CharVector. *)
        struct

        type string = string
        and vector = string
        and elem = char
        and char = char

        val size : string -> int = size (* In prelude *)
    
        (* A one character string is simply the character itself. *)
        val str: char ->string = charAsVec
    
        val maxSize: int =
            RunCall.run_call2 RuntimeCalls.POLY_SYS_process_env (101, ())
                    
        val alloc = LibrarySupport.allocString
    
        (* Concatentate a list of strings. *)
        fun concat [] = ""
         |  concat [s] = s (* Handle special case to reduce copying. *)
            (* Could also handle the case of concat(""::s) = concat s *)
         |  concat L =
            let
                fun total n []     = n
                 | total n (H::T) = total (n + size H) T
                (* How many characters do we have to implode?   This could
                   possibly be long (although we would probably have run out
                   of memory long before) so we have to add these as integers
                   and then raise an exception if it's not short. *)
                val chars : int = total 0 L;
            in
                if chars = 0
                then ""
                else if chars = 1
                then let  (* Special case for single char *)
                    (* Find the character by first removing any empty strings. *)
                    fun getChar []        = raise Fail ""  (* Should never occur *)
                      | getChar ("" :: T) = getChar T
                      | getChar (H :: _) = H (* Should be a single character *)
                in
                    getChar L
                end
                else (* Normal case *)
                let
                    val chs = unsignedShortOrRaiseSize chars (* Check it's short. *)
                    val vec = alloc chs
                  
                    fun copy (_, []:string list) = ()
                     | copy (i, H :: T) =
                        let
                        val src_len = sizeAsWord H
                        in
                        if src_len = 0w1
                        then (* single character strings are ints *)
                            System_setb (vec, i, vecAsChar H)
                        else bcopy(H, vec, wordSize, i, src_len);
                        copy(i+src_len, T)
                        end
                in
                copy (wordSize, L);
                System_lock vec;
                vec
                end
            end (* concat *)

        fun concatWith _ [] = ""
         |  concatWith _ [one] = one
         |  concatWith s (hd :: tl) =
            let
                fun mk [] = []
                  | mk (h::t) = s :: h :: mk t
            in
                concat(hd :: mk tl)
            end
        
        (* implode is very similar to concat, in fact it could be defined
           as a cast version of it.  Since we need it for StringCvt.splitl
           it's defined in LibrarySupport. *)
        val implode = LibrarySupport.stringImplode

        (* This was previously built-in because of the way it worked in
           the Poly language.  It could be defined as concat[a,b] but we
           define it separately for efficiency. *)
        val op ^ : string * string -> string = op ^

        (* This replaces the string_sub function in the RTS.  It's probably
           worthwhile because that function indexes from 1 so we need a
           wrap-around anyway. *)
        fun sub (s: string, i: int): char =
            if i < 0 orelse i >= size s
            then raise General.Subscript
            else if System_isShort s
            then vecAsChar s
            else System_loadb(s, intAsWord i + wordSize);
    
        (* Explode a string into a list of characters. *)
        fun explode (s : string) : char list = stringExplode(s, 0w0, sizeAsWord s)
            
        (* TODO: Could be defined more efficiently, perhaps by copying
           it into an array. *)
        (* This would be easier if we could process the string twice as we
           do with toString but we need to be careful to call f only once
           for each character in case it has a side-effect. *)
        fun translate f s =
        let
            val len = sizeAsWord s
        in
            if len = 0w1 (* Handle special case of single character. *)
            then f(vecAsChar s)
            else
            let
                (* Accumulate the characters into a list. *)
                fun mapChars i l =
                    if i = len then l
                    else mapChars (i+0w1) (f(System_loadb(s, i+wordSize)) :: l)
            in
                (* Reverse the list and concatenate it. *)
                concat(List.rev(mapChars 0w0 []))
            end
        end
        
        fun substring (s, i, j) =
        let
            val len = sizeAsWord s
            (* Check that the index and length are both non-negative. *)
            val i' = unsignedShortOrRaiseSubscript i
            and j' = unsignedShortOrRaiseSubscript j
        in
            if i'+j' > len
            then raise Subscript
            else unsafeSubstring(s, i', j')
        end
    
        fun extract (s, i, NONE) = substring (s, i, size s - i)
         |  extract (s, i, SOME j) = substring (s, i, j)
    
        (* tokens and fields are very similar except that tokens does not return
           empty strings for adjacent delimiters whereas fields does.  *)
        fun tokens p s =
            let
            val length = size s
            fun tok' i l = (* i is the character to examine.  l is the start of a token *)
                if i = length
                then (* Finished the input.  Return any partially completed string. *)
                    (
                    if l = i then [] else [substring (s, l, i-l)]
                    )
                else if p (sub(s, i)) (* TODO: We don't need sub to do the range check here *)
                then (* It's a delimiter.  If we have more than one character in the
                        string we create a string otherwise we just continue. *)
                    (
                    if l = i then tok' (i+1) (i+1)
                    else substring (s, l, i-l) :: tok' (i+1) (i+1)
                    )
                else (* Token: Keep accumulating characters. *) tok' (i+1) l
            in
            tok' 0 0
            end
    
        fun fields p s =
            let
            val length = size s
            
            fun field' i l = (* i is the character to examine.  l is the start of a token *)
                if i = length
                then (* Finished the input.  Return any partially completed string. *)
                    [substring (s, l, i-l)]
                else if p (unsafeSub(s, intAsWord i))
                then (* It's a delimiter.  Finish the partially completed string and
                        start another. *)
                    substring (s, l, i-l) :: field' (i+1) (i+1)
                else (* Field: Keep accumulating characters. *) field' (i+1) l
            in
            field' 0 0
            end
    
        (* Generate escape characters. *)
        local
            fun toStrings convert s =
            let
                val len = sizeAsWord s
            in
                (* Handle the special case of a single character string first. *)
                if len = 0w1
                then convert(vecAsChar s)
                else
                let
                    (* First pass - find out the size of the result string. *)
                    fun getSize i n =
                        if i = len then n
                        else getSize (i+0w1)
                                (n + size(convert(System_loadb(s, i+wordSize))))
                    (* The result could possibly be long so we add the lengths
                       as integers and convert and check when we've finished. *)
                    val newSize = unsignedShortOrRaiseSize (getSize 0w0 0)
                in
                    (* If the size is the same we can return the original string.
                       This relies on the fact that the conversions either return
                       the character unchanged or return a longer escape sequence. *)
                    if newSize = len
                    then s
                    else
                    let
                        (* Second pass: create the output string and copy to it. *)
                        val newVec = allocString newSize
                        fun copyToOut i j =
                        if i = len then ()
                        else
                        let
                            val conv = convert(System_loadb(s, i+wordSize))
                            val convSize = sizeAsWord conv
                        in
                            if convSize = 0w1
                            then System_setb (newVec, j, vecAsChar conv)
                            else bcopy(conv, newVec, wordSize, j, convSize);
                            copyToOut (i+0w1) (j+convSize)
                        end
                    in
                        copyToOut 0w0 wordSize;
                        System_lock newVec;
                        newVec
                    end
                end
            end
        in
            val toString = toStrings Char.toString
            and toCString = toStrings Char.toCString
        end
        
        (* Convert escapes. *)
        fun scan (getc: (char, 'a) StringCvt.reader) (str :'a) : (string * 'a) option =
            let
            fun scanString str l haveRead =
                case Char.scanBase getc str of
                    NONE => (* Invalid escape sequence *)
                        if haveRead then SOME(concat(rev l), str) else NONE
                |   SOME("", strm') => (* End of input or read a format sequence. *)
                        (case getc strm' of NONE => SOME(concat(rev l), strm') | _ => scanString strm' l true)
                |   SOME(s, strm') => scanString strm' (s :: l) true (* More to do. *)
            in
                scanString str [] false
            end
                
        val fromString = StringCvt.scanString scan          
    
        (* TODO: More efficient version. *)
        fun fromCString "" = SOME "" (* Special case *)
          | fromCString s =
            let
            val len = sizeAsWord s
            fun rdr i =
                if i = len then NONE
                else SOME(unsafeSub(s, i), i+0w1)
            (* Repeatedly convert escape sequences and accumulate the
               results in a list. *)
            fun convChar i =
                case Char.scanC rdr i of
                    NONE => []
                  | SOME(res, j) => res :: convChar j
            in
            (* If we couldn't even get a single character we return NONE. *)
            case convChar 0w0 of
                [] => NONE
              | res => SOME(implode res)
            end
    
        (* Install conversion and print functions. *)
        local
            (* It might be worth rewrite scan to raise Conversion with
               a string argument so we can pass back information about
               why an escape code was invalid. *)
            (* Unlike fromString which returns as much of the input string
               as could be converted this raises an exception if the
               input contains any invalid character. *)
            fun convString s =
                let
                val len = sizeAsWord s
                fun rdr i =
                    if i = len then NONE
                    else SOME(unsafeSub(s, i), i+0w1)
                (* Repeatedly convert escape sequences and accumulate the
                   results in a list. *)
                fun convChars i =
                    if i = len then [] (* Finished *)
                    else case Char.scan rdr i of
                        NONE => (* Bad conversion *)
                            raise RunCall.Conversion "Invalid string constant"
                      | SOME(res, j) => res :: convChars j
                in
                    implode(convChars 0w0)
                end

            fun print_string _ _ (s: string) =
                PolyML.PrettyString(concat["\"", toString s, "\""])
        in
            val () = RunCall.addOverload convString "convString";
            val () = PolyML.addPrettyPrinter print_string
        end
        
        (* True if s1 is a prefix of s2 *)
        (* G&R now says that a string is a prefix of itself.  *)
        fun isPrefix s1 s2 =
        let
            val size_s1 = size s1 and size_s2 = size s2
        in
            if size_s1 <= size_s2
            then if size_s1 = 1 (* We have to deal with the case of single chars. *)
            then if size_s2 = 1 then vecAsChar s1 = vecAsChar s2
            else vecAsChar s1 = System_loadb(s2, wordSize)
            else byteMatch s1 s2 0w0 0w0 (intAsWord size_s1)
            else false
        end

        (* True if s1 is a suffix of s2 *)
        fun isSuffix s1 s2 =
        let
            val size_s1 = size s1 and size_s2 = size s2
        in
            if size_s1 <= size_s2
            then if size_s1 = 1 (* We have to deal with the case of single chars. *)
            then if size_s2 = 1 then vecAsChar s1 = vecAsChar s2
            else vecAsChar s1 = System_loadb(s2, wordSize+intAsWord(size_s2-1))
            else byteMatch s1 s2 0w0 (intAsWord (size_s2 - size_s1)) (intAsWord size_s1)
            else false
        end

        (* True if s1 is a substring of s2 *)
        fun isSubstring s1 s2 =
        let
            val size_s1 = size s1 and size_s2 = size s2
            (* Start at the beginning and compare until we get a match. *)
            fun doMatch i s =
            if s < size_s1 then false (* The remainder of the string is too small to match. *)
            else if byteMatch s1 s2 0w0 i (intAsWord size_s1)
            then true
            else doMatch (i+0w1) (s-1)
        in
            if size_s1 = 1
            then Char.contains s2 (vecAsChar s1) 
            else doMatch 0w0 size_s2
        end
        
        
        (* Functions specific to CharVector, apart from map which is common. *)
        fun tabulate (0, _) : vector = "" (* Must not try to lock it. *)
         |  tabulate (1, f) : vector = charAsVec(f 0)
         |  tabulate (length: int , f : int->elem): vector =
        let
            val len = unsignedShortOrRaiseSize length (* Raises Size if length < 0 *)
            val vec = alloc len
            (* Initialise it to the function values. *)
            fun init i = 
                if len <= i then ()
                else (System_setb(vec, i+wordSize, f(wordAsInt i)); init(i+0w1))
        in
            init 0w0;
            System_lock vec;
            vec
        end

        (* Create the other functions. *)
        structure VectorOps =
            VectorOperations(
                struct
                    type vector = vector and elem = elem
                    val length = sizeAsWord
                    fun unsafeSub(s, i) =
                        if System_isShort s then vecAsChar s else System_loadb(s, i + wordSize);
                    fun unsafeSet(_, _, _) = raise Fail "Should not be called"
                end);
    
        open VectorOps;

        fun map f vec =
        let
            val len = sizeAsWord vec
        in
            if len = 0w0 then ""
            else if len = 0w1 (* Special case. Single character strings. *)
            then charAsVec(f(vecAsChar vec))
            else (* len > 1 *)
            let
                (* Allocate a new vector. *)
                val new_vec = alloc len
                val byte_limit = len + wordSize
                    
                fun domap i =
                    if i >= byte_limit then ()
                    else (System_setb(new_vec, i, f(System_loadb(vec, i))); domap(i+0w1))
            in
                domap wordSize;
                System_lock new_vec;
                new_vec
            end
        end
    
        fun mapi f vec =
        let
            val len = sizeAsWord vec
        in
            if len = 0w0 then ""
            else if len = 0w1 (* Special case. Single character strings. *)
            then charAsVec(f (0, vecAsChar vec))
            else (* len >= 2 *)
            let
                (* Allocate a new vector. *)
                val new_vec = alloc len
                    
                fun domap j =
                    if j >= len then ()
                    else (System_setb(new_vec, j+wordSize,
                            f(wordAsInt(j), System_loadb(vec, j+wordSize)));
                          domap(j+0w1))
            in
                domap 0w0;
                System_lock new_vec;
                new_vec
            end
        end
        
        (* Return a copy of the string with a particular character replaced *)
        fun update (v, i, c) =
            if i < 0 orelse i >= size v
            then raise Subscript
            else mapi (fn (j, s) => if j = i then c else s) v

        (* Name changes needed for CharVector. *)
        val maxLen = maxSize
        val fromList = implode
        val length = size
        
        (* Finally the comparison operations since defining these removes
           the overloading. *)
        val op < : (string * string) -> bool = op <
        val op <= : (string * string) -> bool = op <=
        val op > : (string * string) -> bool = op >
        val op >= : (string * string) -> bool = op >=
    
        local
        (* The previous version of compare involved two tests.  This
           uses a single string comparison and then tests the result. *)
            val strCompare: string*string->int =
                RunCall.run_call2 POLY_SYS_str_compare
        in
            fun compare (s1, s2) =
            let
                val c = strCompare(s1, s2)
            in
                if c = 0
                then General.EQUAL
                else if c = 1
                then General.GREATER
                else General.LESS
            end
        end
                    
        end (* String *)

in

    (* CharArray is very similar to Word8Array and most of the code is duplicated. *)
    structure CharArray : MONO_ARRAY =
    struct
        (* We can't use the segment length for the length of the vector
           as we do for "normal" arrays and vectors.  There are two ways
           of handling this.  We could implement arrays in the same
           way as strings, with a length word in the first word, or we
           could store the length separately.  The former has the advantage
           of using less store but the latter allows the byte vector to be
           used for other purposes and is probably faster.  *)
        type address = LibrarySupport.address
        datatype array = datatype LibrarySupport.CharArray.array
        (* N.B.  This representation is hard-wired into TextIO.  Don't
           change this representation without changing that as well. *)

        type vector = string and elem = char

        infix 9 sub (* For what it's worth *)
                
        val maxLen = String.maxLen (* Use the same maximum as string. *)
        val alloc = LibrarySupport.allocBytes
    
        fun length(Array(l, _)) = wordAsInt l
        
        fun array (length, ini) =
        let
            (* The array is allocated containing zeros.  Some versions of
               the RTS will allow byte vectors to be allocated with other
               values but other versions don't.  For the moment assume
               that we have to initialise the array separately. *)
            val len = unsignedShortOrRaiseSize length
            val vec = alloc len
            fun init i = 
                if len <= i then ()
                else (System_setbA(vec, i, ini); init(i+0w1))
        in
            init 0w0;
            Array(len, vec)
        end
    
        fun op sub (Array(l, v), i: int): elem =
        let
            val iW =
                if isShortInt i andalso i >= 0
                then intAsWord i
                else raise General.Subscript
        in
            if iW >= l then raise General.Subscript
            else System_loadbA (v, iW)
        end
    
        fun update (Array (l, v), i: int, new) : unit =
        let
            val iW =
                if isShortInt i andalso i >= 0
                then intAsWord i
                else raise General.Subscript
        in
            if iW >= l
            then raise General.Subscript
            else System_setbA (v, iW, new)
        end;
    
        (* Create an array from a list. *)
        local
            fun fromList' (l : char list) : word*address =
            let
                val length = unsignedShortOrRaiseSize (List.length l);
                    
                (* Make a array initialised to zero. *)
                val vec = alloc length;
                
                (* Copy the list elements into the array. *)
                fun init (v, i, a :: l) = (System_setbA(v, i, a); init(v, i + 0w1, l))
                |  init (_, _, []) = ();
                
            in
                init(vec, 0w0, l);
                (length, vec)
            end
        in
            fun fromList (l : elem list) : array = Array(fromList' l)
        end
            
        fun tabulate (length: int , f : int->elem): array =
        let
            val len = unsignedShortOrRaiseSize length
            val vec = alloc len
            (* Initialise it to the function values. *)
            fun init i = 
                if len <= i then ()
                else (System_setbA(vec, i, f(wordAsInt i)); init(i+0w1))
        in
            init 0w0;
            Array(len, vec)
        end
        
        fun vector (Array(len, vec)) =
            if len = 0w0 then ""
            else if len = 0w1
            then (* Single character string is the character itself. *)
                charAsVec (System_loadbA (vec, 0w0))
            else
            let
                (* Make an array initialised to zero. *)
                val new_vec = String.alloc len
            in
                System_move_bytesA(vec, 0w0, RunCall.unsafeCast new_vec, wordSize, len);
                System_lock new_vec;
                new_vec
            end
    
        (* Copy an array into another.  It's possible for the arrays to be
           the same but in that case diW must be zero and the copy is a no-op. *)
        fun copy {src=Array (len, s), dst=Array (dlen, d), di: int} =
            let
                val diW = unsignedShortOrRaiseSubscript di
            in
                if diW+len > dlen
                then raise General.Subscript
                else System_move_bytesA(s, 0w0, d, diW, len)
        end
    
        (* Copy avector into an array. *)
        (* Since the source is actually a string we have to start the
           copy from si+wordSize. *)
        fun copyVec {src, dst=Array (dlen, d), di: int} =
            let
                val len = LibrarySupport.sizeAsWord src
                val diW = unsignedShortOrRaiseSubscript di
            in
                if diW + len > dlen
                then raise General.Subscript
                else if System_isShort src (* i.e. String.length s = 1 *)
                then (* Single character strings are represented by the character
                        so we just need to insert the character into the array. *)
                    System_setbA(d, diW, vecAsChar src)
                else System_move_bytesA(RunCall.unsafeCast src, wordSize, d, diW, len)
            end
            
        (* Create the other functions. *)
        structure ArrayOps =
            VectorOperations(
                struct
                    type vector = array and elem = elem
                    fun length(Array(len, _)) = len
                    fun unsafeSub(Array(_, v), i) = System_loadbA(v, i)
                    and unsafeSet(Array(_, v), i, c) = System_setbA(v, i, c)
                end);
    
        open ArrayOps;
    
        local
            (* Install the pretty printer for CharArray.array *)
            (* We may have to do this outside the structure if we
               have opaque signature matching. *)
            fun pretty _ _ x =
                PolyML.PrettyString(String.concat["\"", String.toString(vector x), "\""])
        in
            val () = PolyML.addPrettyPrinter pretty
        end
    end;

    structure Substring :>
            sig
            type  substring
            eqtype char
            eqtype string
            val size : substring -> int
            val base : substring -> (string * int * int)
            val isEmpty : substring -> bool
        
            val sub : (substring * int) -> char
            val getc : substring -> (char * substring) option
            val first : substring -> char option
            
            val extract : (string * int * int option) -> substring
            val substring : (string * int * int) -> substring
            (*val slice : (substring * int * int option) -> substring*)
            val full: string -> substring
            val string : substring -> string
            
            val concat: substring list ->string
            val concatWith: string -> substring list ->string
        
            val explode : substring -> char list
            val translate : (char -> string) -> substring -> string
            val app : (char -> unit) -> substring -> unit
            val foldl : ((char * 'a) -> 'a) -> 'a -> substring -> 'a
            val foldr : ((char * 'a) -> 'a) -> 'a -> substring -> 'a
            val tokens : (char -> bool) -> substring -> substring list
            val fields : (char -> bool) -> substring -> substring list
            val isPrefix: string -> substring -> bool
            val isSubstring: string -> substring -> bool
            val isSuffix: string -> substring -> bool
        
            val compare : (substring * substring) -> General.order
            val collate : ((char * char) -> General.order) ->
                             (substring * substring) -> General.order
        
            val triml : int -> substring -> substring
            val trimr : int -> substring -> substring
            val splitl : (char -> bool) -> substring -> (substring * substring)
            val splitr : (char -> bool) -> substring -> (substring * substring)
            val splitAt : (substring * int) -> (substring * substring)
            val dropl : (char -> bool) -> substring -> substring
            val dropr : (char -> bool) -> substring -> substring
            val takel : (char -> bool) -> substring -> substring
            val taker : (char -> bool) -> substring -> substring
            val position : string -> substring -> (substring * substring)
            val span : (substring * substring) -> substring

            type vector
            type elem
            type slice
            
            val length : slice -> int
            val subslice: slice * int * int option -> slice
            val slice: vector * int * int option -> slice
            val vector: slice -> vector
            val getItem: slice -> (elem * slice) option
            val appi : ((int * elem) -> unit) -> slice -> unit
            val mapi : ((int * elem) -> elem) -> slice -> vector
            val map : (elem -> elem) -> slice -> vector
            val foldli : ((int * elem * 'a) -> 'a) -> 'a -> slice -> 'a
            val foldri : ((int * elem * 'a) -> 'a) -> 'a -> slice -> 'a
            val findi: (int * elem -> bool) -> slice -> (int * elem) option
            val find: (elem -> bool) -> slice -> elem option
            val exists: (elem -> bool) -> slice -> bool
            val all: (elem -> bool) -> slice -> bool
            sharing type slice = substring
            end
            where type elem = char where type vector = string where type char = char where type string = string =
    struct
        type vector = string and elem = char

        structure VectorSliceOps =
            VectorSliceOperations(
                struct
                    type vector = vector and elem = char
                    val vecLength = sizeAsWord
                    fun unsafeVecSub(s, i: word) =
                        if System_isShort s then vecAsChar s else System_loadb(s, i + wordSize)
                    fun unsafeVecUpdate _ = raise Fail "Should not be called" (* Not applicable *)
                end);
    
        open VectorSliceOps;

        (* vector: get the slice out.  Since the underlying vector is implemented using the basic
           string type we can use substring here. *)
        fun vector slice : vector =
        let
            val (vector, start, length) = base slice
        in
            unsafeSubstring(vector, intAsWord start, intAsWord length)
        end;
        
        (* It would be more efficient to do these as single operations but it's probably too complicated. *)
        fun concat L = String.concat(List.map vector L)
        fun concatWith s L = String.concatWith s (List.map vector L)
        fun map f slice = String.map f (vector slice)
        fun mapi f slice = String.mapi f (vector slice)
        
        (* Substring operations. *)
        type substring = slice
        type char = elem
        type string = vector
        
        val size = length
    
        (* Since we've already checked the bounds we don't need to do it here. *)
        fun string(Slice{vector=s, start=i, length=l}) = unsafeSubstring(s, i, l)
    
        (* Check that the index and length are valid. *)
        fun substring(s, i, j) =
            if i < 0 orelse j < 0 orelse String.size s < i+j
            then raise General.Subscript
            else Slice{vector=s, start=intAsWord i, length=intAsWord j}

        fun extract(s, i, NONE) = substring(s, i, String.size s-i)
         |  extract(s, i, SOME j) = substring(s, i, j)

        fun triml k = 
            if k < 0 then raise General.Subscript
            else fn (Slice{vector=s, start=i, length=l}) =>
                if k > wordAsInt l then Slice{vector=s, start=i+l, length=0w0}
                else Slice{vector=s, start=i + intAsWord k, length=l - intAsWord k}
            
        fun trimr k =
            if k < 0 then raise General.Subscript
            else fn (Slice{vector=s, start=i, length=l}) =>
                if k > wordAsInt l then Slice{vector=s, start=i, length=0w0}
                else Slice{vector=s, start=i, length=l - intAsWord k}

        fun explode (Slice{vector=s, start=i, length=l}) : char list = LibrarySupport.stringExplode(s, i, l)
    
        (* Compare two strings.  We could define compare in terms of collate and it
           would be just as efficient provided we set PolyML.Compiler.maxInlineSize
           to a large enough value that collate was inlined, and hence Char.compare
           would be inlined.  *)
        fun compare (Slice{vector=s, start=j, length=l}, Slice{vector=s', start=j', length=l'}) =
            let
            fun comp' i =
                if i = l
                then
                    (
                    if l = l' then General.EQUAL
                    else (* l < l' *) General.LESS
                    )
                else if i = l' (* and not l *) then General.GREATER
                else
                    case Char.compare(unsafeSub(s, i+j), unsafeSub(s', i+j')) of
                        General.EQUAL => comp' (i+0w1)
                      | General.LESS => General.LESS
                      | General.GREATER => General.GREATER
            in
            comp' 0w0
            end

        fun isPrefix (s1: string) (Slice{vector=s2, start=i, length=l}) =
        let
            val size_s1 = sizeAsWord s1
        in
            if size_s1 > l
            then false
            else if size_s1 = 0w1
            then if System_isShort s2
            then vecAsChar s1 = vecAsChar s2
            else vecAsChar s1 = System_loadb(s2, i + wordSize)
            else byteMatch s1 s2 0w0 i size_s1
        end

        (* True if s1 is a suffix of s2 *)
        fun isSuffix s1 (Slice{vector=s2, start=i, length=l}) =
        let
            val size_s1 = sizeAsWord s1
        in
            if size_s1 > l
            then false
            else if size_s1 = 0w1
            then if System_isShort s2
            then vecAsChar s1 = vecAsChar s2
            else vecAsChar s1 = System_loadb(s2, i + l - 0w1 + wordSize)
            else byteMatch s1 s2 0w0 (l + i - size_s1) size_s1
        end

        (* True if s1 is a substring of s2 *)
        fun isSubstring s1 (Slice{vector=s2, start, length}) =
        let
            val size_s1 = sizeAsWord s1
            (* Start at the beginning and compare until we get a match. *)
            fun doMatch i s =
            if s < size_s1 then false (* The remainder of the string is too small to match. *)
            else if (
               if size_s1 = 0w1
               then vecAsChar s1 = System_loadb(s2, i + wordSize)
               else byteMatch s1 s2 0w0 i size_s1
               )
            then true
            else doMatch (i+0w1) (s-0w1)
        in
            if System_isShort s2
            then size_s1 = 0w0 orelse (size_s1 = 0w1 andalso vecAsChar s1 = vecAsChar s2)
            else doMatch start length
        end

        (* TODO: This would be quicker with an RTS function to scan for a
           character in a string. *)
        fun splitl f (Slice{vector=s, start=i, length=l}) =
            let
            fun find j =
                if j = i+l
                then (* All chars satisfy f *) (Slice{vector=s, start=i, length=l}, Slice{vector=s, start=j, length=0w0})
                else if f(unsafeSub(s, j)) then find (j+0w1)
                else (* Found a separator *)
                    (Slice{vector=s, start=i, length=j-i}, Slice{vector=s, start=j, length=l+i-j})
            in
            find i
            end
    
        (* TODO: This would be quicker with an RTS function to scan for a
           character in a string. *)
        fun splitr f (Slice{vector=s, start=i, length=l}) =
            let
            fun find j =
                if j = i
                then (* All chars satisfy f *) (Slice{vector=s, start=j, length=0w0}, Slice{vector=s, start=i, length=l})
                else if f(unsafeSub(s, j-0w1)) then find (j-0w1)
                else (* Found a separator *)
                    (Slice{vector=s, start=i, length=j-i}, Slice{vector=s, start=j, length=l+i-j})
            in
            find (i+l)
            end
            
        fun splitAt (Slice{vector=s, start=i, length=l}, j) =
        let
            val j' = unsignedShortOrRaiseSubscript j
        in
            if j' > l then raise General.Subscript
            else (Slice{vector=s, start=i, length=j'}, Slice{vector=s, start=i+j', length=l-j'})
        end
        
        (* TODO: Define these directly rather than via split.  It's not so expensive
           doing it this way for substrings because we don't actually copy the strings. *)
        fun takel p s = #1(splitl p s)
        and dropl p s = #2(splitl p s)
        and taker p s = #2(splitr p s)
        and dropr p s = #1(splitr p s)

        (* NOTE: There's an error in the web page.  The example function uses "trim"
           rather than "triml".
           QUESTION: The check i'+n' >= i does not guarantee that ss is to the left of ss',
           merely that the end of ss' is to the right of the beginning of ss. 
           I can't remember my reasoning about this at the moment.  *)
        val equal_ptr: string*string->bool = RunCall.run_call2 POLY_SYS_word_eq
        
        fun span (Slice{vector=s, start=i, length=_}, Slice{vector=s', start=i', length=n'}) =
            (* First check with pointer equality and only if that fails do we use the
               string equality function. *)
            if (equal_ptr(s, s') orelse s = s') andalso i'+n' >= i
            then Slice{vector=s, start=i, length=i'+n'-i}
            else raise General.Span 
           
        (* tokens and fields are very similar except that tokens does not return
           empty strings for adjacent delimiters whereas fields does.
           This definition is almost the same as String.tokens and String.fields. *)
        (* QUESTION: Are these defined always to return the results as substrings
           of the original base string?  That's important if we want to be able to
           use "span" to join them up again.  *)
        fun tokens p (Slice{vector=s, start=j, length}) =
            let
            val ends = j+length
            fun tok' i l = (* i is the character to examine.  l is the start of a token *)
                if i = ends
                then (* Finished the input.  Return any partially completed string. *)
                    (
                    if l = i then [] else [Slice{vector=s, start=l, length=i-l}]
                    )
                else if p (unsafeSub(s, i))
                then (* It's a delimiter.  If we have more than one character in the
                        string we create a string otherwise we just continue. *)
                    (
                    if l = i then tok' (i+0w1) (i+0w1)
                    else Slice{vector=s, start=l, length=i-l} :: tok' (i+0w1) (i+0w1)
                    )
                else (* Token: Keep accumulating characters. *) tok' (i+0w1) l
            in
            tok' j j
            end
    
        fun fields p (Slice{vector=s, start=j, length}) =
            let
            val ends = j+length
            
            fun field' i l = (* i is the character to examine.  l is the start of a token *)
                if i = ends
                then (* Finished the input.  Return any partially completed string. *)
                    [Slice{vector=s, start=l, length=i-l}]
                else if p (unsafeSub(s, i))
                then (* It's a delimiter.  Finish the partially completed string and
                        start another. *)
                    Slice{vector=s, start=l, length=i-l} :: field' (i+0w1) (i+0w1)
                else (* Field: Keep accumulating characters. *) field' (i+0w1) l
            in
            field' j j
            end
    
        (* TODO: Could be defined more efficiently. *)
        (* map and translate are defined to apply f from left to right. *)
        fun translate f s = String.concat(List.map f (explode s))
        
        fun position s (Slice{vector=s', start=i, length=n}) =
        let
            val m = sizeAsWord s (* Length of string to match. *)
            fun pos k =
                if k > n-m then (* No match *) (Slice{vector=s', start=i, length=n}, Slice{vector=s', start=i+n, length=0w0})
                else if compare(full s, Slice{vector=s', start=i+k, length=m}) = EQUAL
                then (* Match *) (Slice{vector=s', start=i, length=k}, Slice{vector=s', start=k+i, length=n-k})
                else pos (k+0w1)
        in
            (* Because m and n are word values n-m is UNSIGNED so we have to check
               this before we call "pos". *)
            if m > n then (Slice{vector=s', start=i, length=n}, Slice{vector=s', start=i+n, length=0w0})
            else pos 0w0
        end

        (* Return the first character of the string together with the rest of the
           string.  *)
        fun getc(Slice{length=0w0, ...}) = NONE
          | getc(Slice{vector=s, start=i, length=l}) = SOME(unsafeSub(s, i), Slice{vector=s, start=i+0w1, length=l-0w1})
    
        fun first(Slice{length=0w0, ...}) = NONE
          | first(Slice{vector=s, start=i, length=_}) = SOME(unsafeSub(s, i))
        
    end;

    (* CharVectorSlice. *)
    structure CharVectorSlice: MONO_VECTOR_SLICE where type elem = char where type vector = string = Substring;

    structure Substring : SUBSTRING =
        struct open Substring;
        val slice = subslice
        end
        
    local
        (* Install the pretty printer for CharVector.slice (and substring) *)
        (* We may have to do this outside the structure if we
           have opaque signature matching. *)
        fun pretty _ _ s =
            PolyML.PrettyString(String.concat["\"", String.toString(Substring.string s), "\""])
    in
        val _ = PolyML.addPrettyPrinter pretty
    end;

    structure CharArraySlice:> MONO_ARRAY_SLICE where type elem = char where type vector = string
                    where type vector_slice = CharVectorSlice.slice where type array = CharArray.array =
    struct
        type elem = char
        type vector = string
        datatype array = datatype LibrarySupport.CharArray.array
        (* N.B.  This representation is hard-wired into TextIO.  Don't
           change this representation without changing that as well. *)
        type vector_slice = CharVectorSlice.slice

        structure ArraySliceOps =
            VectorSliceOperations(
                struct
                    type vector = array and elem = char
                    fun unsafeVecSub(Array(_, s: LibrarySupport.address), i) = System_loadbA(s, i)
                    and unsafeVecUpdate(Array(_, s), i, x) = System_setbA (s, i, x)
                    and vecLength(Array(l, _)) = l
                end);
    
        open ArraySliceOps;

        (* vector: get the slice out. *)
        fun vector slice: vector =
            let
                val (Array(_, vec), start, length) = base slice
            in
                if length = 0 then ""
                else if length = 1
                then (* Single character string is the character itself. *)
                    RunCall.unsafeCast (System_loadbA (vec, intAsWord start))
                else
                let
                    val len = intAsWord length
                    (* Make an array initialised to zero. *)
                    val new_vec = String.alloc len
                in
                    System_move_bytesA(vec, intAsWord start, RunCall.unsafeCast new_vec, wordSize, len);
                    System_lock new_vec;
                    new_vec
                end
            end

        (* Copy a slice into an array. N.B. The arrays could be the same. *)
        fun copy {src, dst, di: int} =
        let
            val (src, start, length) = base src
        in
            if di < 0 orelse di+length > CharArray.length dst
            then raise General.Subscript
            else (* We can't use System_move_bytes because of the potential overlap problem.
                    Instead we use explicit copying choosing to copy up or down depending
                    on the index whether the source and destination are the same or not. *)
            let
                fun copyUp n =
                if n = length then ()
                else (CharArray.update(dst, n+di, CharArray.sub(src, n+start)); copyUp(n+1))
                
                and copyDown n =
                if n < 0 then ()
                else (CharArray.update(dst, n+di, CharArray.sub(src, n+start)); copyDown(n-1))
            in
                if di > start then copyDown(length-1) else copyUp 0
            end (* System_move_bytesA(s, intAsWord start, d, intAsWord di, intAsWord length) *)
        end
    
        (* Copy a vector slice into an array. *)
        fun copyVec {src: CharVectorSlice.slice, dst=Array (dlen, d), di: int} =
            let
                val (source, i, l) = CharVectorSlice.base src
                val len = intAsWord l and offset = intAsWord i
                val diW = unsignedShortOrRaiseSubscript di
            in
                if diW + len > dlen
                then raise General.Subscript
                else if System_isShort source (* i.e. length s = 1 *)
                then (* Single character strings are represented by the character
                        so we just need to insert the character into the array. *)
                    System_setbA(d, diW + offset, vecAsChar source)
                    (* The source is represented by a string whose first word is the length. *)
                else System_move_bytesA(RunCall.unsafeCast source, offset + wordSize, d, diW, len)
            end
        
    end (* CharArraySlice *);
    
    local
        (* Install the pretty printer for CharArraySlice.slice *)
        (* We may have to do this outside the structure if we
           have opaque signature matching. *)
        fun pretty _ _ x =
            PolyML.PrettyString(String.concat["\"", CharArraySlice.vector x, "\""])
    in
        val _ = PolyML.addPrettyPrinter pretty
    end

    structure CharVector: MONO_VECTOR = String
    structure String: STRING = String
    structure Char: CHAR = Char

end;

(* Values available unqualified at the top level. *)
val ord : char -> int = Char.ord 
val chr : int -> char = Char.chr 
val concat : string list -> string =String.concat 
val implode : char list -> string = String.implode 
val explode : string -> char list = String.explode 
val substring : string * int * int -> string = String.substring;
val op ^ : string * string -> string = String.^;
type substring = Substring.substring;

(* These are declared in the prelude. *)
(* val size : string -> int = String.size 
   val str : char -> string = String.str *)
