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
    Title: 	Table of printing functions for user-defined types.
    Author: 	Dave Matthews, Cambridge University Computer Laboratory
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

(*****************************************************************************)
(*                  CREATEDATABASE                                           *)
(*****************************************************************************)
structure CREATEDATABASE :
sig
  type 'a dbList
  val dbListCreate : (unit -> '_a) * '_a list -> '_a dbList
  val dbListToList : 'a dbList -> 'a list
end

(*****************************************************************************)
(*                  STRUCTVALS                                               *)
(*****************************************************************************)
structure STRUCTVALS :
sig
  type word
  type typeId;
  type typeConstrs
  type codetree
  
  val sameTypeId:   typeId * typeId -> bool;
  val tcIdentifier: typeConstrs -> typeId
end;

(*****************************************************************************)
(*                  PRETTYPRINTER                                            *)
(*****************************************************************************)
structure PRETTYPRINTER :
sig
  type prettyPrinter;
end;

(*****************************************************************************)
(*                  MISC                                                     *)
(*****************************************************************************)
structure MISC :
sig
  exception InternalError of string;
  exception ValueMissing  of string;
end;
) :


(*****************************************************************************)
(*                  PRINTTABLE export signature                              *)
(*****************************************************************************)
sig
  type word
  type typeId;
  type prettyPrinter;
  type typeConstrs
  type codetree
  
  val addPp:    typeId * 
  					(prettyPrinter -> int -> word -> word -> unit) -> unit;
  val getPrint: typeId ->
  					(prettyPrinter -> int -> word -> word -> unit);
  val addOverload: string * typeConstrs * codetree -> unit
  val getOverloads: string -> (typeConstrs * codetree) list
  val getOverload: string * typeConstrs * (unit->codetree) -> codetree
end =

(*****************************************************************************)
(*                  PRINTTABLE functor body                                  *)
(*****************************************************************************)
struct
  open STRUCTVALS;
  open CREATEDATABASE;
  open PRETTYPRINTER;
  open MISC;
  type overloadEntry = string * typeConstrs * codetree;
  
  (* On entering a new database level, create a new printable ref *)
  fun newOverloadLevel () : overloadEntry list ref = ref [] 

  (* Create a dbList of refs. The number of refs in the list depends on which
     level of the database hierarchy we're running in.  *)
  val overloadTable : overloadEntry list ref dbList = 
    dbListCreate (newOverloadLevel, [newOverloadLevel ()]);
    
  (* The most recent ref refers to the current level of the hierarchy,
     so is the one we should update. *)
  fun addOverload (name, cons, overload) = 
    case dbListToList overloadTable of
      [] => raise InternalError "Overload table initialisation error"
    | (thisLevel :: otherLevels) =>
		let
			(* Remove any existing occurrences of the type
			   at this level. The only reason is to allow any
			   existing function to be garbage-collected. *)
			fun filter [] = []
			  | filter ((this as (n, t, _)) :: rest) =
			  		if n = name andalso
						 sameTypeId (tcIdentifier cons, tcIdentifier t)
					then filter rest
					else this :: filter rest
		in
			thisLevel := (name, cons, overload) :: filter (!thisLevel)
		end
  
  (* Return all the overloads for a particular identifier. *)
  fun getOverloads name =
  let
    fun search []                         = []
      | search (thisLevel :: otherLevels) =
	    let
	      fun searchList []         = search otherLevels
	      |   searchList ((n,t,v)::rest) = 
	            if name = n then (t, v) :: searchList rest else searchList rest
	    in
	      searchList (!thisLevel)
	    end
  in
    search (dbListToList overloadTable) 
  end;

  (* Return the first matching overload or call mkDefault.  *)
  fun getOverload(name, constr, mkDefault) =
  let
    fun search []                         = mkDefault()
      | search (thisLevel :: otherLevels) =
	    let
	      fun searchList []         = search otherLevels
	      |   searchList ((n,t,v)::rest) = 
	            if name = n andalso
					sameTypeId (tcIdentifier constr, tcIdentifier t)
				then v else searchList rest
	    in
	      searchList (!thisLevel)
	    end
  in
    search (dbListToList overloadTable) 
  end;
  	
  (* TODO: Treat the pretty print functions as just another form of
     overloading except that we only want the most recent occurrence. *)
  type printEntry = typeId *
  						(prettyPrinter -> int -> word -> word -> unit);
  
  (* On entering a new database level, create a new printable ref *)
  fun newLevel () : printEntry list ref = ref [] 

  (* Create a dbList of refs. The number of refs in the list depends on which
     level of the database hierarchy we're running in.  *)
  val printTable : printEntry list ref dbList = 
    dbListCreate (newLevel, [newLevel ()]);    
  (* The most recent ref refers to the current level of the hierarchy,
     so is the one we should update. *)
  fun addPp (consid, pproc) = 
    case dbListToList printTable of
      [] => raise InternalError "Print table initialisation error"
    | (thisLevel :: otherLevels) =>
		let
			(* Remove any existing occurrences of the identifier
			   at this level. The only reason is to allow any
			   existing function to be garbage-collected. *)
			fun filter [] = []
			  | filter ((this as (i, _)) :: rest) =
			  		if sameTypeId (i, consid)
					then filter rest
					else this :: filter rest
		in
			thisLevel := (consid,pproc) :: filter (!thisLevel)
		end
  
  (* However, we should search ALL the refs when we lookup a constructor *)
  fun getPrint id =
  let
    fun search []                         = raise ValueMissing ""
      | search (thisLevel :: otherLevels) =
    let
      fun searchList []         = search otherLevels
      |   searchList ((i,p)::t) = 
            if sameTypeId (i, id) then p else searchList t;
    in
      searchList (!thisLevel)
    end
  in
    search (dbListToList printTable) 
  end;
  
end;

