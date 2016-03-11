(*
    Title:      Standard Basis Library: Array Slice Signature
    Author:     David Matthews
    Copyright   David Matthews 1999, 2005, 2016

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

(* This uses the 'a array and 'a vector types defined at the top level.
   It also uses the VectorSlice structure. *)
signature ARRAY_SLICE =
sig
    type 'a slice
    val length : 'a slice -> int
    val sub : 'a slice * int -> 'a
    val update : 'a slice * int * 'a -> unit
    val full: 'a array -> 'a slice
    val slice: 'a array * int * int option -> 'a slice
    val subslice: 'a slice * int * int option -> 'a slice
    val base: 'a slice -> 'a array * int * int
    val vector: 'a slice -> 'a vector
    val copy : {src : 'a slice, dst : 'a array, di : int} -> unit
    val copyVec : {src : 'a VectorSlice.slice, dst : 'a array, di : int} -> unit
    val isEmpty: 'a slice -> bool
    val getItem: 'a slice -> ('a * 'a slice) option

    val appi : (int * 'a -> unit) -> 'a slice -> unit
    val app : ('a -> unit) -> 'a slice -> unit

    val modifyi : (int * 'a -> 'a) -> 'a slice -> unit
    val modify : ('a -> 'a) -> 'a slice -> unit

    val foldli : ((int * 'a * 'b) -> 'b) -> 'b -> 'a slice -> 'b
    val foldri : ((int * 'a * 'b) -> 'b) -> 'b -> 'a slice -> 'b
    val foldl : (('a * 'b) -> 'b) -> 'b -> 'a slice -> 'b
    val foldr : (('a * 'b) -> 'b) -> 'b -> 'a slice -> 'b
    
    val findi: (int * 'a -> bool) -> 'a slice -> (int * 'a) option
    val find: ('a -> bool) -> 'a slice -> 'a option
    
    val exists: ('a -> bool) -> 'a slice -> bool
    val all:  ('a -> bool) -> 'a slice -> bool
    val collate: ('a * 'a -> order) -> 'a slice * 'a slice -> order
end;
