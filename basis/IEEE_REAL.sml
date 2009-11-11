(*
    Title:      Standard Basis Library: IEEE_REAL Signature.
    Author:     David Matthews
    Copyright   David Matthews 2000, 2005

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

signature IEEE_REAL =
sig
    exception Unordered
    datatype real_order = LESS | EQUAL | GREATER | UNORDERED
    datatype float_class
       = NAN
       | INF
       | ZERO
       | NORMAL
       | SUBNORMAL
    datatype rounding_mode
       = TO_NEAREST
       | TO_NEGINF
       | TO_POSINF
       | TO_ZERO
    val setRoundingMode : rounding_mode -> unit 
    val getRoundingMode : unit -> rounding_mode 
    type decimal_approx =
        { class : float_class, sign : bool, digits : int list, exp : int }
    val toString : decimal_approx -> string
    val scan : (char, 'a) StringCvt.reader -> (decimal_approx, 'a) StringCvt.reader
    val fromString : string -> decimal_approx option
end;
