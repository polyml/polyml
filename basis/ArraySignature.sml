(*
    Title:      Standard Basis Library: Array Signature
    Author:     David Matthews
    Copyright   David Matthews 2016

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

signature ARRAY =
sig
    eqtype 'a array
    type 'a vector

    val maxLen : int
    val array : (int * 'a) -> 'a array
    val fromList : 'a list -> 'a array
    val vector: 'a array -> 'a vector
    val tabulate : (int * (int -> 'a)) -> 'a array
    val length : 'a array -> int
    val sub : ('a array * int) -> 'a
    val update : ('a array * int * 'a) -> unit
    val copy : {src : 'a array, dst : 'a array, di : int} -> unit
    val copyVec : {src : 'a vector, dst : 'a array, di : int} -> unit

    val appi : ((int * 'a) -> unit) -> 'a array -> unit
    val app : ('a -> unit) -> 'a array -> unit

    val foldli : ((int * 'a * 'b) -> 'b) -> 'b -> 'a array -> 'b
    val foldri : ((int * 'a * 'b) -> 'b) -> 'b -> 'a array -> 'b
    val foldl : (('a * 'b) -> 'b) -> 'b -> 'a array -> 'b
    val foldr : (('a * 'b) -> 'b) -> 'b -> 'a array -> 'b

    val modifyi : ((int * 'a) -> 'a) -> 'a array -> unit
    val modify : ('a -> 'a) -> 'a array -> unit

    val findi: (int * 'a -> bool) -> 'a array -> (int * 'a) option
    val find: ('a -> bool) -> 'a array -> 'a option
    val exists: ('a -> bool) -> 'a array -> bool
    val all: ('a -> bool) -> 'a array -> bool
    val collate: ('a * 'a -> order) -> 'a array * 'a array -> order
end;
