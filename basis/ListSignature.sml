(*
    Title:      Standard Basis Library: List Signature
    Author:     David Matthews
    Copyright   David Matthews 1999, 2005, 2016

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

signature LIST =
sig
    datatype list = datatype list
    (* G&R include the definition of list below in their "Interface".  This is illegal. *)
    (*datatype 'a list = nil | :: of 'a * 'a list *)
    exception Empty
    val null : 'a list -> bool
    val length : 'a list -> int
    val @ : ('a list * 'a list) -> 'a list
    val concat : 'a list list -> 'a list
    val revAppend : ('a list * 'a list) -> 'a list
    val tabulate : (int * (int -> 'a)) -> 'a list   
    val hd : 'a list -> 'a
    val tl : 'a list -> 'a list
    val last : 'a list -> 'a
    val getItem : 'a list -> ('a * 'a list) option
    val nth : ('a list * int) -> 'a
    val take : ('a list * int) -> 'a list
    val drop : ('a list * int) -> 'a list
    val rev : 'a list -> 'a list

    val app : ('a -> unit) -> 'a list -> unit
    val map : ('a -> 'b) -> 'a list -> 'b list
    val mapPartial : ('a -> 'b option) -> 'a list -> 'b list
    val find : ('a -> bool) -> 'a list -> 'a option
    val filter : ('a -> bool) -> 'a list -> 'a list
    val partition : ('a -> bool) -> 'a list -> ('a list * 'a list)
    val foldl : (('a * 'b) -> 'b) -> 'b -> 'a list -> 'b
    val foldr : (('a * 'b) -> 'b) -> 'b -> 'a list -> 'b
    val exists : ('a -> bool) -> 'a list -> bool
    val all : ('a -> bool) -> 'a list -> bool

    val collate: ('a * 'a -> order) -> 'a list * 'a list -> order
end;
