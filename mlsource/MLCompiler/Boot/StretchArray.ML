(*
	Copyright (c) 2000
		Cambridge University Technical Services Limited

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

(*
    Title:      StretchArray.ML
    Author:     Simon Finn, Abstract Hardware Ltd.
*)


(* HACKS: imports Array implicitly *)

(* Version of array which expands as new elements are assigned.
   Used for tables which have no obvious upper limit. *)
structure StretchArray :
sig
  type 'a stretchArray;
  
  exception Subscript;
  exception Size;
  
  val stretchArray : int * '_a -> '_a stretchArray;
  val update : '_a stretchArray * int * '_a -> unit;
  val length : 'a stretchArray -> int;
  val sub    : 'a stretchArray * int -> 'a;
end =

struct
  val expansionFactor = 3; (* Factor by which to increase size. *)

  exception Subscript = (*Array.*)Subscript and Size = (*Array.*)Size;

  (* use Array to hold the contents - less efficient than making
     this a primitive, but does it matter? *)
  type 'a stretchArray =
    {
      initialVal : 'a,
      contents   : 'a Array.array ref
    }
     
  fun stretchArray (originalSize: int, initialVal: '_a)
    :'_a stretchArray =
    {
      initialVal = initialVal,
      contents   = ref (Array.array (originalSize, initialVal))
    };

  
  fun length (sa: 'a stretchArray) : int =
     Array.length (! (#contents sa));
 
  (* Returns the value if there is one, otherwise returns the default. *)
  infix 9 sub;
  fun (sa :'a stretchArray) sub (index : int) : 'a =
  let
    val a : 'a Array.array = ! (#contents sa);
  in
    if index < Array.length a
    then Array.sub (a, index) (* may raise Array.Subscript *)
    else #initialVal sa
  end; 
  
  fun max (x:int) (y:int) : int = if x <= y then y else x;      
	     
  (* Sets the appropriate entry. *)
  fun update (sa: '_a stretchArray, index: int, value: '_a) : unit =
    Array.update (! (#contents sa), index, value)
       handle Subscript => 
	 if index < 0 then raise Subscript
	 else let
	   (* The new vector must be big enough to hold the new item
	      and expanded by the expansion factor. *)
	   val oldSize = length sa;
	   val newSize = max (oldSize * expansionFactor) (index + 1);
	   val newVec = Array.array(newSize, #initialVal sa);
	   (* val newVec  = Array.tabulate (newSize, fn i => sa sub i); *)
	 in
	   Array.copy{src= !(#contents sa), dst=newVec, di=0};
	   #contents sa := newVec;
	   Array.update (newVec, index, value)
	 end;
end;

