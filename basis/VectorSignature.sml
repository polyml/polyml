(*
    Title:      Standard Basis Library: Vector Signature
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

signature VECTOR =
sig
    eqtype  'a vector
    val maxLen : int
    val fromList : 'a list -> 'a vector
    val tabulate : (int * (int -> 'a)) -> 'a vector
    val length : 'a vector -> int
    val sub : ('a vector * int) -> 'a
    val update: 'a vector * int * 'a -> 'a vector
    
    val concat : 'a vector list -> 'a vector
    val mapi : ((int * 'a) -> 'b) -> 'a vector -> 'b vector
    val map : ('a -> 'b) -> 'a vector -> 'b vector

    val appi : ((int * 'a) -> unit) -> 'a vector -> unit
    val app : ('a -> unit) -> 'a vector -> unit

    val foldli : ((int * 'a * 'b) -> 'b) -> 'b -> 'a vector -> 'b
    val foldri : ((int * 'a * 'b) -> 'b) -> 'b -> 'a vector -> 'b
    val foldl : (('a * 'b) -> 'b) -> 'b -> 'a vector -> 'b
    val foldr : (('a * 'b) -> 'b) -> 'b -> 'a vector -> 'b
    
    val findi: (int * 'a -> bool) -> 'a vector -> (int * 'a) option
    val find: ('a -> bool) -> 'a vector -> 'a option
    val exists: ('a -> bool) -> 'a vector -> bool
    val all: ('a -> bool) -> 'a vector -> bool
    val collate: ('a * 'a -> order) -> 'a vector * 'a vector -> order
end;
