(*
    Title:      Standard Basis Library: MONO_ARRAY_SLICE signature
    Author:     David Matthews
    Copyright   David Matthews 2005

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

(* G&R 2004 status: new. *)

signature MONO_ARRAY_SLICE =
sig
    type elem
    type array
    type slice
    type vector
    type vector_slice
    
    val length : slice -> int
    val sub : (slice * int) -> elem
    val update: slice * int * elem -> unit
    val full: array -> slice
    val slice: array * int * int option -> slice
    val subslice: slice * int * int option -> slice
    val base: slice -> array * int * int
    val vector: slice -> vector
    val copy : {src : slice, dst : array, di : int} -> unit
    val copyVec : {src : vector_slice, dst : array, di : int} -> unit
    val isEmpty: slice -> bool
    val getItem: slice -> (elem * slice) option
    val appi : ((int * elem) -> unit) -> slice -> unit
    val app : (elem -> unit) -> slice -> unit
    val modifyi : (int * elem -> elem) -> slice -> unit
    val modify : (elem -> elem) -> slice -> unit
    val foldli : ((int * elem * 'a) -> 'a) -> 'a -> slice -> 'a
    val foldri : ((int * elem * 'a) -> 'a) -> 'a -> slice -> 'a
    val foldl : ((elem * 'a) -> 'a) -> 'a -> slice -> 'a
    val foldr : ((elem * 'a) -> 'a) -> 'a -> slice -> 'a
    val findi: (int * elem -> bool) -> slice -> (int * elem) option
    val find: (elem -> bool) -> slice -> elem option
    val exists: (elem -> bool) -> slice -> bool
    val all: (elem -> bool) -> slice -> bool
    val collate: (elem * elem -> order) -> slice * slice -> order
end;
