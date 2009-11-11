(*
    Title:      Standard Basis Library: StringCvt Structure
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

(* G&R 2004 status: checked, no change required. *)

signature STRING_CVT =
  sig
    datatype radix = BIN | OCT | DEC | HEX

    datatype realfmt
      = SCI of int option
      | FIX of int option
      | GEN of int option
      | EXACT

    type  ('a, 'b) reader = 'b -> ('a * 'b) option    

    val padLeft : char -> int -> string -> string
    val padRight : char -> int -> string -> string
    val splitl : (char -> bool) -> (char, 'a) reader ->'a -> (string * 'a)
    val takel : (char -> bool) -> (char, 'a) reader ->'a -> string
    val dropl : (char -> bool) -> (char, 'a) reader ->'a -> 'a
    val skipWS : (char, 'a) reader -> 'a -> 'a
    type  cs
    val scanString : ((char, cs) reader -> ('a, cs) reader) -> string -> 'a option

  end;

structure StringCvt : STRING_CVT =
    struct
    (* Note: Both the String and Char structures use StringCvt.reader .
       This means that they depend on this structure so we have to
       put declarations we need for both in LibrarySupport. *)
    open RuntimeCalls; (* for POLY_SYS and EXC numbers *)
    open LibrarySupport

    val chToString: char->string = RunCall.unsafeCast
    and stringToCh: string->char = RunCall.unsafeCast

    val System_lock: string -> unit   = RunCall.run_call1 POLY_SYS_lockseg;
    val System_setb: string * word * char -> unit   = RunCall.run_call3 POLY_SYS_assign_byte;
    val mem_move: string*word*string*word*word -> unit = 
                RunCall.run_call5 POLY_SYS_move_bytes

    datatype radix = BIN | OCT | DEC | HEX

    datatype realfmt
      = SCI of int option
      | FIX of int option
      | GEN of int option
      | EXACT
      
    type  ('a, 'b) reader = 'b -> ('a * 'b) option        

    fun padLeft c i s =
    if i <= 0 (* unsignedShortOrRaiseSize raises Size if i < 0 which isn't right here. *)
    then s
    else
    let
        val len: word = sizeAsWord s
        val iW = unsignedShortOrRaiseSize i (* checks that i is a short. *)
    in
        if len >= iW then s
        else if iW = 0w1 (* and therefore size s = 0 *)
        then chToString c (* return single character string. *)
        else 
        let
            val extra = iW - len
            val str = allocString iW
            fun setCh n =
                if n = extra then ()
                (* Set the character part of the string. *)
                else ( System_setb(str, n+wordSize, c); setCh(n+0w1) )
        in
            setCh 0w0;
            (* Copy the character part of the string over. *)
            if len = 0w1
            then System_setb(str, extra + wordSize, stringToCh s)
            else mem_move(s, wordSize, str, extra + wordSize, len);
            System_lock str;
            str
        end
    end
    
    fun padRight c i s =
    if i <= 0 (* unsignedShortOrRaiseSize raises Size if i < 0 which isn't right here. *)
    then s
    else
    let
        val len = sizeAsWord s
        val iW = unsignedShortOrRaiseSize i (* checks that i is a short. *)
    in
        if len >= iW then s
        else if iW = 0w1 (* and therefore size s = 0 *)
        then chToString c (* return single character string. *)
        else 
        let
            val str = allocString iW
            fun setCh n =
                if n = iW then ()
                (* Set the character part of the string. *)
                else ( System_setb(str, n+wordSize, c); setCh(n+0w1) )
        in
            (* Copy the character part of the string over. *)
            if len = 0w1
            then System_setb(str, wordSize, stringToCh s)
            else mem_move(s, wordSize, str, wordSize, len);
            setCh len;
            System_lock str;
            str
        end
    end

    (* p is described as a predicate.  That implies that it is
       side-effect free.  If it is we could use it e.g. twice, once to work out
       the length of the string and then to create the string itself. 
       Assume that it may have side-effects and that we can only execute it
       once. *)

    local
        (* We have to define rev here because it isn't defined until
           we compile List. *)
        fun rev l [] = l
          | rev l (a::b) = rev (a::l) b

        fun split' p f res src =
            case f src of
                NONE => (stringImplode(rev [] res), src) (* Not available. *)
              | SOME (ch, src') => (* Char available *)
                    if p ch
                    then (* It matches - include in the result *)
                        split' p f (ch :: res) src'
                    else (stringImplode(rev [] res), src) (* No match *)
    in
        fun splitl p f src = split' p f [] src
    end

    (* It may be worth defining takel independently but it doesn't add
       much overhead by contrast with dropl *)
    fun takel p f s = #1(splitl p f s)
    (* fun dropl p f s = #2(splitl p f s) *)
    
    (* This is probably as efficient as it can be. *)
    fun dropl p f src =
        case f src of
            NONE => src (* Not available. *)
          | SOME (ch, src') => (* Char available *)
                if p ch
                then dropl p f src'
                else src (* No match *)

    (* Copied isSpace from Char structure to avoid circular dependency. *)
    fun skipWS f src =
        case f src of
            NONE => src (* Not available. *)
          | SOME (ch, src') => (* Char available *)
                if (#"\t" <= ch andalso ch <= #"\r") orelse ch = #" "
                then skipWS f src'
                else src (* No match *)

    datatype cs = Index of word
    
    (* Index into the string. *)
    fun scanString cvt s =
        let
        val len = sizeAsWord s
        fun rdr (Index i) =
            if i = len then NONE
            (* Since we know the index is between 0 and len-1 we can use
               the unsafe subscript function here. *)
            else SOME(LibrarySupport.unsafeStringSub(s, i), Index(i+0w1))
        in
        case cvt rdr (Index 0w0) of
            NONE => NONE
          | SOME(res, _) => SOME res
        end

    end;
