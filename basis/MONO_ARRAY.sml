(*
    Title:      Standard Basis Library: Mono_Array signature
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

signature MONO_ARRAY =
sig
    eqtype array
    type elem
    type vector
    val maxLen : int
    val array : (int * elem) -> array
    val fromList : elem list -> array
    val vector: array -> vector
    val tabulate : (int * (int -> elem)) -> array
    val length : array -> int
    val sub : (array * int) -> elem
    val update : (array * int * elem) -> unit
    val copy : {src : array, dst : array, di : int} -> unit
    val copyVec : {src : vector, dst : array, di : int} -> unit
    val appi : ((int * elem) -> unit) -> array -> unit
    val app : (elem -> unit) -> array -> unit
    val foldli : ((int * elem * 'b) -> 'b) -> 'b -> array -> 'b
    val foldri : ((int * elem * 'b) -> 'b) -> 'b -> array -> 'b
    val foldl : ((elem * 'b) -> 'b) -> 'b -> array -> 'b
    val foldr : ((elem * 'b) -> 'b) -> 'b -> array -> 'b
    val modifyi : ((int * elem) -> elem) -> array -> unit
    val modify : (elem -> elem) -> array -> unit
    val findi: (int * elem -> bool) -> array -> (int * elem) option
    val find: (elem -> bool) -> array -> elem option
    val exists: (elem -> bool) -> array -> bool
    val all: (elem -> bool) -> array -> bool
    val collate: (elem * elem -> order) -> array * array -> order
end;
