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
    Title:  Table of printing functions for user-defined types.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1990
*)


(* implicitly imports Boot.Misc, Boot.PrettyPrinter, StructVals and Address *)

(*
    The original purpose of this module was to allow for type-dependent
    print functions to be installed by the user.  That requires a special
    mechanism to allow a function to be installed in a child database without
    needing to be able to write to the top-level database containing the
    compiler.
    This has been extended to deal with SML97 overloading for both functions
    and literal constants and also for ref-like types which support pointer
    equality even though these cases are intended only for the implementors
    of libraries which would probably be in the top-level database.
*)

      
functor PRINT_TABLE (

structure CODETREE:
sig
    type machineWord;
    type codetree
end;

(*****************************************************************************)
(*                  STRUCTVALS                                               *)
(*****************************************************************************)
structure STRUCTVALS :
sig
  type typeId;
  type typeConstrs
  val sameTypeId:   typeId * typeId -> bool;
  val tcIdentifier: typeConstrs -> typeId
end;

(*****************************************************************************)
(*                  PRETTY                                                   *)
(*****************************************************************************)
structure PRETTY : PRETTYSIG

):

(*****************************************************************************)
(*                  PRINTTABLE export signature                              *)
(*****************************************************************************)
sig
    type typeConstrs
    type codetree
  
    val addOverload: string * typeConstrs * codetree -> unit
    val getOverloads: string -> (typeConstrs * codetree) list
    val getOverload: string * typeConstrs * (unit->codetree) -> codetree
  
    structure Sharing:
    sig
        type typeConstrs = typeConstrs
        type codetree = codetree
    end
end =

(*****************************************************************************)
(*                  PRINTTABLE functor body                                  *)
(*****************************************************************************)
struct
  open STRUCTVALS;
  type pretty = PRETTY.pretty
  open CODETREE
  type overloadEntry = string * typeConstrs * codetree;

  (* Create a ref to hold the list *)
  val overloadTable : overloadEntry list ref = ref []
    
  (* The most recent ref refers to the current level of the hierarchy,
     so is the one we should update. *)
  fun addOverload (name, cons, overload) = 
    let
        (* Remove any existing occurrences of the type. The only reason
           is to allow any existing function to be garbage-collected. *)
        fun filter [] = []
          | filter ((this as (n, t, _)) :: rest) =
                if n = name andalso
                     sameTypeId (tcIdentifier cons, tcIdentifier t)
                then filter rest
                else this :: filter rest
    in
        overloadTable := (name, cons, overload) :: filter (!overloadTable)
    end
  
  (* Return all the overloads for a particular identifier. *)
  fun getOverloads name =
  let
      fun searchList []         = []
      |   searchList ((n,t,v)::rest) = 
            if name = n then (t, v) :: searchList rest else searchList rest
  in
      searchList (! overloadTable) 
  end;

  (* Return the first matching overload or call mkDefault.  *)
  fun getOverload(name, constr, mkDefault) =
  let
      fun searchList []         = mkDefault()
      |   searchList ((n,t,v)::rest) = 
            if name = n andalso
                sameTypeId (tcIdentifier constr, tcIdentifier t)
            then v else searchList rest
  in
      searchList (! overloadTable) 
  end;
  
    structure Sharing =
    struct
        type codetree = codetree
        and  typeConstrs = typeConstrs
    end
  
end;

