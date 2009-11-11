(*
    Title:      Standard Basis Library: ListPair Structure
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
(*
   G&R 2004 status: Done.
*)
signature LIST_PAIR =
  sig
    exception UnequalLengths
    val zip : ('a list * 'b list) -> ('a * 'b) list
    val zipEq : ('a list * 'b list) -> ('a * 'b) list
    val unzip : ('a * 'b) list -> ('a list * 'b list)
    val app : ('a * 'b -> unit) -> ('a list * 'b list) -> unit
    val appEq : ('a * 'b -> unit) -> ('a list * 'b list) -> unit
    val map : ('a * 'b -> 'c) -> ('a list * 'b list) -> 'c list
    val mapEq : ('a * 'b -> 'c) -> ('a list * 'b list) -> 'c list
    val foldl : (('a * 'b * 'c) -> 'c) -> 'c -> ('a list * 'b list) -> 'c
    val foldr : (('a * 'b * 'c) -> 'c) -> 'c -> ('a list * 'b list) -> 'c
    val foldlEq : (('a * 'b * 'c) -> 'c) -> 'c -> ('a list * 'b list) -> 'c
    val foldrEq : (('a * 'b * 'c) -> 'c) -> 'c -> ('a list * 'b list) -> 'c
    val all : ('a * 'b -> bool) -> ('a list * 'b list) -> bool
    val exists : ('a * 'b -> bool) -> ('a list * 'b list) -> bool
    val allEq : ('a * 'b -> bool) -> ('a list * 'b list) -> bool
  end;

structure ListPair : LIST_PAIR =
    struct
    exception UnequalLengths
    
    fun zip (h::t, h'::t') = (h, h') :: zip(t, t')
     |  zip (_, _) = [] (* Stop as soon as either list is exhausted. *)
     
    fun zipEq (h::t, h'::t') = (h, h') :: zipEq(t, t')
     |  zipEq ([], []) = []
     |  zipEq (_, _) = raise UnequalLengths

    fun unzip ((a, b) :: l) =
        let
        (* TODO: This is quite inefficient in Poly/ML.  It might be
           better to unzip each of the lists separately. *)
        val (x, y) = unzip l
        in
        (a :: x, b :: y)
        end
     | unzip [] = ([], [])
     
    fun map f (h::t, h'::t') = f(h, h') :: map f (t, t')
      | map _ _ = []

    fun mapEq f (h::t, h'::t') = f(h, h') :: mapEq f (t, t')
      | mapEq _ ([], []) = []
      | mapEq _ _ = raise UnequalLengths

    fun app f (h::t, h'::t') = (f(h, h'); app f (t, t'))
      | app _ _ = ()

    fun appEq f (h::t, h'::t') = (f(h, h'); appEq f (t, t'))
      | appEq _ ([], []) = ()
      | appEq _ _ = raise UnequalLengths

    fun foldl f b (h::t, h'::t') = foldl f (f(h, h', b)) (t, t')
      | foldl _ b _ = b

    fun foldr f b (h::t, h'::t') = f(h, h', foldr f b (t, t'))
      | foldr _ b _ = b

    fun foldlEq f b (h::t, h'::t') = foldlEq f (f(h, h', b)) (t, t')
      | foldlEq _ b ([], []) = b
      | foldlEq _ _ _ = raise UnequalLengths

    fun foldrEq f b (h::t, h'::t') = f(h, h', foldrEq f b (t, t'))
      | foldrEq _ b ([], []) = b
      | foldrEq _ _ _ = raise UnequalLengths

    fun exists f (h::t, h'::t') = if f(h, h') then true else exists f (t, t')
      | exists _ _ = false

    (* all and allEq differ in the way they handle lists of different lengths.
       all returns true if the predicate holds up to the shorter of the lists whereas
       allEq returns false if the lists have different lengths. *)
    fun all f (h::t, h'::t') = if f(h, h') then all f (t, t') else false
      | all _ _ = true

    (* Is it better to check the lengths first? *)
    fun allEq f (h::t, h'::t') = if f(h, h') then allEq f (t, t') else false
      | allEq _ ([], []) = true
      | allEq _ _ = false

    end;
