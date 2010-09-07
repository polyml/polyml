(*
    Title:      Standard Basis Library: Int32 Structure
    Author:     Vesa Karvonen
    Copyright   David Matthews 1999
                Vesa Karvonen 2007

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
  This is a hacked version of Int32 - a 32bit Int implementation for
  PolyML 5. It's neither well tested nor efficiently implemented.
*)

structure Int32 :> INTEGER = struct
   open Int

   val precision = 32
   val minInt = ~(IntInf.<< (1, Word.fromInt (precision - 1)))
   val maxInt = ~1-minInt

   fun check i =
       if i < minInt orelse maxInt < i
       then raise Overflow
       else i

   val precision = SOME precision
   val minInt = SOME minInt
   val maxInt = SOME maxInt

   val fromLarge = check o fromLarge
   val fromInt = check o fromInt

   val ~ = check o ~
   val op * = check o op *
   val op + = check o op +
   val op - = check o op -
   val op div = check o op div
   val op mod = check o op mod
   val quot = check o quot
   val rem = check o rem

   val abs = check o abs
   fun scan' r g s =
       case scan r g s
        of NONE => NONE
         | SOME (i, s) => SOME (check i, s)
   val scan = scan'
   val fromString = Option.map check o fromString
end;

local
   fun convInt s = let
      val radix =
      if String.size s >= 3 andalso String.substring(s, 0, 2) = "0x"
         orelse String.size s >= 4 andalso String.substring(s, 0, 3) = "~0x"
      then StringCvt.HEX else StringCvt.DEC
   in
      case StringCvt.scanString (Int32.scan radix) s of
     NONE => raise RunCall.Conversion "Invalid integer constant"
       | SOME res => res
   end
   fun pretty _ _ x = PolyML.PrettyString (Int32.toString x)
in
   val () = RunCall.addOverload convInt "convInt"
   val () = PolyML.addPrettyPrinter pretty
end;

val () = RunCall.addOverload Int32.~ "~";
val () = RunCall.addOverload Int32.+ "+";
val () = RunCall.addOverload Int32.- "-";
val () = RunCall.addOverload Int32.* "*";
val () = RunCall.addOverload Int32.div "div";
val () = RunCall.addOverload Int32.mod "mod";
val () = RunCall.addOverload Int32.< "<";
val () = RunCall.addOverload Int32.> ">";
val () = RunCall.addOverload Int32.<= "<=";
val () = RunCall.addOverload Int32.>= ">=";
val () = RunCall.addOverload Int32.abs "abs";
