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

structure IntSet :

(*****************************************************************************)
(*                  IntSet export signature                                  *)
(*****************************************************************************)
sig
  type set;
  
  val empty: set;

  val plus:  set * set -> set;
  val minus: set * set -> set;
  val star:  set * set -> set;

  val mk: int -> set;
  
  val upto : int * int -> set;
  val inside: int * set -> bool;
end  =

(*****************************************************************************)
(*                  IntSet structure body                                    *)
(*****************************************************************************)
struct
  abstype set = Set of int -> bool
  with
    val empty : set = Set (fn _ => false);

(* ...  
    fun plus  (Set f, Set g) = Set (fn x => f x orelse g x);
... *)
    (* Experiment! *) 
    fun plus  (Set f, Set g) = Set (fn x => g x orelse f x);
    
    fun star  (Set f, Set g) = Set (fn x => f x andalso g x);
    fun minus (Set f, Set g) = Set (fn x => f x andalso not (g x));
    
    fun mk (i : int) = Set (fn j => j = i)
  
    infix 8 upto;
    
    fun first upto last = Set (fn i => first <= i andalso i <= last);
    
    infix 5 inside eq neq;
    
    fun i inside (Set f) = f i;
  end;
end (* IntSet *);

