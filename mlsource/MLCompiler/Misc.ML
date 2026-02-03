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

structure Misc :
(*****************************************************************************)
(*                  Misc exports signature                                  *)
(*****************************************************************************)
sig
  (* This isn't handled at all (except generically) *)
  exception InternalError of string; (* compiler error *)

  val quickSort : ('a -> 'a -> bool) -> 'a list -> 'a list

  val lookupDefault : ('a -> 'b option) -> ('a -> 'b option) -> 'a -> 'b option
end  =

(*****************************************************************************)
(*                  Misc structure body                                      *)
(*****************************************************************************)
struct
  (* This isn't handled at all (except generically) *)
  exception InternalError of string; (* compiler error *)

    (* Simple definition of quicksort.  This is fairly efficient on time but is very
       bad for memory allocation and is only suitable for small cases.
       The call to List.partition builds O(N.log N) cells, if the list is random or
       O(N.N) if it is already sorted.  It has been improved from the original
       which used append (@) to build the sorted list which added a second O(N.N)
       phase. *)
    fun quickSort (leq: 'a -> 'a -> bool) (l: 'a list) =
    let
        fun qs ([], tail) = tail
        |   qs ([h], tail) = h :: tail
        |   qs (h::t, tail) =
            let
                val (after, befor) = List.partition (leq h) t
            in
                qs(befor, h :: qs(after, tail))
            end
    in
        qs(l, [])
    end

  fun lookupDefault first second = (fn key => case first key of NONE => second key | v => v);

end (* Misc *);
