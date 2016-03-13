(*
    Title:      Standard Basis Library: INTEGER signature
    Copyright   David C.J. Matthews 1999, 2016

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

signature INTEGER =
sig
    eqtype  int
    val toLarge : int -> LargeInt.int
    val fromLarge : LargeInt.int -> int
    val toInt : int -> Int.int
    val fromInt : Int.int -> int
    val precision : Int.int option

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
    val sign : int -> Int.int
    val sameSign : (int * int) -> bool
    val fmt : StringCvt.radix -> int -> string
    val toString : int -> string
    val fromString : string -> int option
    val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader
end;
