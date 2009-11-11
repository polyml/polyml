(*
    Title:      Standard Basis Library: Option Structure
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

(* G&R 2004 status: updated, added "app" function. *)

signature OPTION =
  sig
    datatype 'a option = NONE | SOME of 'a
    exception Option
    val getOpt : ('a option * 'a) -> 'a
    val isSome : 'a option -> bool
    val valOf : 'a option -> 'a
    val filter : ('a -> bool) -> 'a -> 'a option
    val join : 'a option option -> 'a option
    val app : ('a -> unit) -> 'a option -> unit
    val map : ('a -> 'b) -> 'a option -> 'b option
    val mapPartial : ('a -> 'b option) -> 'a option -> 'b option
    val compose : (('a -> 'b) * ('c -> 'a option)) -> 'c -> 'b option
    val composePartial : (('a -> 'b option) * ('c -> 'a option)) -> 'c -> 'b option
  end;

structure Option : OPTION =
    struct
    (* datatype 'a option = NONE | SOME of 'a *)
    datatype option = datatype option
    exception Option
    
    fun getOpt (SOME a, _) = a
     |  getOpt (NONE, b) = b
     
    fun isSome (SOME _) = true
     |  isSome (NONE) = false

    fun valOf (SOME a) = a
     |  valOf (NONE) = raise Option

    fun filter f a = if f a then SOME a else NONE

    fun join (SOME v) = v
      | join NONE = NONE
      
    fun app f (SOME v) = f v
      | app _ NONE = ()

    fun map f (SOME v) = SOME (f v)
      | map _ NONE = NONE

    fun mapPartial f (SOME v) = f v
      | mapPartial _ NONE = NONE


    fun compose (f, g) a =
        case g a of
            NONE => NONE
          | SOME v => SOME (f v)

    fun composePartial (f, g) a =
        case g a of
            NONE => NONE
          | SOME v => f v
    end;

(* Export some of these to the top-level. *)
local
    structure dummy:
        sig
        (* datatype 'a option = NONE | SOME of 'a *)
        exception Option
        val getOpt : ('a option * 'a) -> 'a
        val isSome : 'a option -> bool
        val valOf : 'a option -> 'a
        end = Option
in
    open dummy
end;
