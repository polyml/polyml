(*
    Title:      Standard Basis Library: MONO_VECTOR signature
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

signature MONO_VECTOR =
sig
    type  vector
    type  elem
    val maxLen : int
    val fromList : elem list -> vector
    val tabulate : (int * (int -> elem)) -> vector
    val length : vector -> int
    val sub : (vector * int) -> elem
    val update: vector * int * elem -> vector
    val concat : vector list -> vector
    val mapi : ((int * elem) -> elem) -> vector -> vector
    val map : (elem -> elem) -> vector -> vector
    val appi : ((int * elem) -> unit) -> vector -> unit
    val app : (elem -> unit) -> vector -> unit
    val foldli : ((int * elem * 'a) -> 'a) -> 'a -> vector -> 'a
    val foldri : ((int * elem * 'a) -> 'a) -> 'a -> vector -> 'a
    val foldl : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a
    val foldr : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a
    val findi: (int * elem -> bool) -> vector -> (int * elem) option
    val find: (elem -> bool) -> vector -> elem option
    val exists: (elem -> bool) -> vector -> bool
    val all: (elem -> bool) -> vector -> bool
    val collate: (elem * elem -> order) -> vector * vector -> order
end;
