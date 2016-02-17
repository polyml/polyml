(*
    Title:      Standard Basis Library: List Structure
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

structure List: LIST =
    struct
    datatype list = datatype list
    exception Empty
    
    fun null [] = true | null (_::_) = false
    
    val length = length (* Declared in prelude. *)
    
    (* ...
    fun   nil @ M = M   (* append *)
     | (x::L) @ M = x :: (L @ M);
    ... *)
    
    (* Dave's improved(?) version SPF 10/2/94 *)
    (* Taken from the prelude.  The idea is to avoid rebuilding the
       list if the second list is empty. *)
    fun x @ nil = x  (* append *)
      | x @ y =
        let
        fun app nil = y
         | app (a :: b) = a :: app b
        in
        app x
        end;

    fun hd (a::_) = a | hd _ = raise Empty
    and tl (_::a) = a | tl _ = raise Empty
    
    (* TODO: We could avoid the test for nil in the recursive cases. *)
    fun last [] = raise Empty
      | last [a] = a
      | last (_::b) = last b
      
    fun getItem [] = NONE
      | getItem (a::b) = SOME(a, b)
    
    (* We could raise subscript immediately if i < 0 and we probably
       would have to if we were using fixed precision arithmetic. *)
    fun nth([], _) = raise General.Subscript
     |  nth(a::_, 0) = a
     |  nth(_::l, i) = nth(l, i-1)
    
    (* TODO: Many of these functions involve recursing down the list and
       so require stack space proportional to the length of the list.
       Would it be more efficient to build the lists in reverse and then
       reverse the result?  That would save on stack space at the expense
       of constructing the list twice. *)
    
    fun take(_, 0) = []
     |  take([], _) = raise General.Subscript
     |  take(a::b, i) = a :: take(b, i-1)
     
    fun drop(l, 0) = l
     |  drop([], _) = raise General.Subscript
     |  drop(_::l, i) = drop(l, i-1)
     
    fun revAppend([], a) = a
     |  revAppend(x::y, a) = revAppend(y, x::a)
     
    fun rev l = revAppend(l, [])

    fun concat [] = []
     |  concat (a::b) = a @ concat b
     
    fun app _ [] = ()
     |  app f (h::t) = (f h; app f t)

    fun map _ [] = []
      | map f (a::b) = f a :: map f b;

    fun mapPartial _ [] = []
      | mapPartial f (a::b) = 
          case f a of
              SOME r => r :: mapPartial f b
            | NONE => mapPartial f b

    fun find _ [] = NONE
      | find f (a::b) = if f a then SOME a else find f b
      
    fun filter _ [] = []
      | filter f (a::b) = if f a then a :: filter f b else filter f b
    
    (* This is defined to evaluate f from left to right.  *)
    (* TODO: This involves returning a pair and creating new pairs
       which allocates storage in Poly/ML.  Is there a more efficient
       implementation?  e.g. recurse down the list and then reverse it. *)
    fun partition _ [] = ([], [])
      | partition f (a::b) =
            let
            val test = f a
            and (x, y) = partition f b
            in
            if test then (a::x, y) else (x, a::y)
            end
            
    fun foldl _ b [] = b
      | foldl f b (x::y) = foldl f (f(x, b)) y

    fun foldr _ b [] = b
      | foldr f b (x::y) = f(x, foldr f b y)

    fun exists _ [] = false
      | exists f (a::b) = if f a then true else exists f b
      
    fun all _ [] = true
      | all f (a::b) = if f a then all f b else false

    (* tabulate a function. *)
    local
        fun tabF max n f =
            if n = max then []
            else f n :: tabF max (n+1) f
    in
        fun tabulate(n, f) =
            if n < 0 then raise Size
            else tabF n 0 f
    end

    (* Lexicographic comparison.  *)
    fun collate _   ([], []) = General.EQUAL
     |  collate _   ([], _) = General.LESS
     |  collate _   (_, []) = General.GREATER
     |  collate cmp (a::b, c::d) =
            (case cmp (a, c) of General.EQUAL => collate cmp (b, d) | notEqual => notEqual)
    end;

(* Values available at the top level. *)
exception Empty = List.Empty
val null : 'a list -> bool = List.null 
val hd : 'a list -> 'a = List.hd 
val tl : 'a list -> 'a list = List.tl 
val length : 'a list -> int = List.length 
val rev : 'a list -> 'a list = List.rev 
val op @ : ('a list * 'a list) -> 'a list = List.@ 
val app : ('a -> unit) -> 'a list -> unit = List.app 
val map : ('a -> 'b) -> 'a list -> 'b list = List.map 
val foldr: ('a*'b->'b)-> 'b -> 'a list -> 'b = List.foldr 
val foldl: ('a*'b->'b)-> 'b -> 'a list -> 'b = List.foldl;
