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
    Title: 	Parse Types.
    Author: 	Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

functor PARSE_TYPE (

(*****************************************************************************)
(*                  SYMBOLS                                                  *)
(*****************************************************************************)
structure SYMBOLS :
sig
  type sys;
  
  val leftParen:  sys;
  val typeIdent:  sys;
  val comma:      sys;
  val rightParen: sys;
  val asterisk:   sys;
  val arrow:      sys;
  val leftCurly:  sys;
  val rightCurly: sys;
  val colon:      sys;
  val ident:      sys;
  
  val eq: sys * sys -> bool
end;
  
(*****************************************************************************)
(*                  SYMSET                                                   *)
(*****************************************************************************)
structure SYMSET :
sig
  type symset;
  type sys;
  
  val empty:        symset;
  val comma:        symset;
  val rightParen:   symset;
  val asterisk:     symset;
  val arrow:        symset;
  val rightCurly:   symset;
  val colon:        symset;
  val ident:        symset;
  val startTypeSys: symset;
  
  val inside: sys * symset -> bool;
  val ++ :    symset * symset -> symset
end;

(*****************************************************************************)
(*                  LEX                                                      *)
(*****************************************************************************)
structure LEX :
sig
  type lexan;
  type sys;
  
  val insymbol:     lexan -> unit;
  val sy:           lexan -> sys;
  val id:           lexan -> string;
  val lineno:       lexan -> int;
  val errorMessage: lexan * int * string -> unit;
end;

(*****************************************************************************)
(*                  SKIPS                                                    *)
(*****************************************************************************)
structure SKIPS :
sig
  type sys;
  type lexan;
  type symset;
  
  val badsyms:  sys * lexan -> unit;
  val testfor:  sys * symset * lexan -> bool;
  val getsym:   sys * lexan -> unit;
  val skipon:   symset * symset * string * lexan -> unit;
  val getid:    symset * symset * lexan -> string;
  val getLabel: symset * lexan -> string;
  val getList:  sys * symset * lexan * (unit -> 'a) -> 'a list
end;
  
(*****************************************************************************)
(*                  STRUCTVALS                                               *)
(*****************************************************************************)
structure STRUCTVALS :
sig
  type types;
  type typeConstrs;
  
  val badType:   types;
  val undefType: typeConstrs;
end;

(*****************************************************************************)
(*                  TYPETREE                                                 *)
(*****************************************************************************)
structure TYPETREE :
sig
  type types;
  type typeConstrs;
  
  val mkTypeConstruction: string * typeConstrs * types list -> types;
  val mkProductType:      types list -> types;
  val mkFunctionType:     types * types -> types;
  val mkLabelled:         {name: string, typeof: types } list * bool -> types;
  val mkLabelEntry:       string * types -> {name: string, typeof: types };
  val sortLabels:         {name: string, typeof: types } list * (string -> unit) ->
  								{name: string, typeof: types } list;
  val unitType:           types
end;

(*****************************************************************************)
(*                  PARSETYPE sharing constraints                            *)
(*****************************************************************************)

sharing type
  SYMBOLS.sys
= SYMSET.sys
= SKIPS.sys
= LEX.sys

sharing type
  SYMSET.symset
= SKIPS.symset

sharing type
  LEX.lexan
= SKIPS.lexan

sharing type
  STRUCTVALS.types
= TYPETREE.types

sharing type
  STRUCTVALS.typeConstrs
= TYPETREE.typeConstrs
                  
) : 
                  
(*****************************************************************************)
(*                  PARSETYPE export signature                               *)
(*****************************************************************************)
sig
  type symset;
  type lexan;
  type types;
     
  val parseType: symset * lexan * {lookupTvar:string -> types} -> types;
end =
     

(*****************************************************************************)
(*                  PARSETYPE functor body                                   *)
(*****************************************************************************)
struct

  open LEX;
  open SYMSET;
  open SKIPS;
  open TYPETREE;
  open SYMBOLS;
  open STRUCTVALS;
    
   
    infix 7 eq;
    infix 8 ++;
    infix 8 inside;
    
 (* defined here to save re-evaluation (SPF 24/9/94) *)
 val tyseqSyntax = SYMSET.comma ++ SYMSET.rightParen;
 val lrSyntax    = SYMSET.comma ++ SYMSET.rightCurly;
   
    fun parseType (fsys, lex, env as {lookupTvar}) =
    let
      fun tupleType fsys =
      let
	fun basicType fsys =
	let (* First part may be a type sequence. *)
	  val tySeq = 
	    if sy lex eq leftParen
	    then (* sequence of types *)
	    let
	      fun processList () =
	      let
	        val thisType = 
	         if sy lex inside startTypeSys
	         then parseType (fsys ++ tyseqSyntax, lex, env)
	          else
		   (
		     badsyms (typeIdent, lex);
		     badType (* not there *)
		   );
	      in (* Check for any more *)
	        if testfor (SYMBOLS.comma, startTypeSys, lex)
	        then thisType :: processList() (* get some more *)
	        else [thisType] (* that's it *)
	      end (* processList *);
	      
	      (* code bugfixed SPF 19/2/94 *)
	      val UUU      = insymbol lex;  (* Remove opening bracket *)
	      val sequence = processList(); (* read list of items *)
	    in
	      getsym (SYMBOLS.rightParen, lex);
	      sequence
	    end

	  else if sy lex eq leftCurly
	  then
	  (
	    insymbol lex; (* Remove opening bracket *)
	    
	    if testfor (SYMBOLS.rightCurly, empty, lex)
	    then [unitType]
	    
	    else let (* All the labels should be the same sort. *)
	      val l = 
	        getList (SYMBOLS.comma, empty, lex,
		   fn () =>
		   let
		     val name = getLabel (fsys ++ SYMSET.colon, lex);
		   in
		     getsym (SYMBOLS.colon, lex);
		     mkLabelEntry (name, parseType (fsys ++ lrSyntax, lex, env))
		   end);
            in
              getsym (SYMBOLS.rightCurly, lex);
              [mkLabelled 
                 (sortLabels (l, fn msg => errorMessage (lex, lineno lex, msg)),
                  true) (* frozen *)]
            end
          )
                    
          else if sy lex eq typeIdent
          then let (* type variable *)
	    val ty =
	      [#lookupTvar env (id lex)];
	  in
	    getsym (typeIdent, lex);
	    ty
	  end
	  
	  else if sy lex eq SYMBOLS.ident
	  (* Constructor such as `int' *)
	  then [mkTypeConstruction (getid (SYMSET.ident, fsys, lex), undefType, [])]
	  else
	  (
	    badsyms (SYMBOLS.ident, lex);
	    []
	  );
	in
	  (* Type sequence read. Can now have some type constructors. *)
	  if sy lex eq SYMBOLS.ident
	  then
	  let (* Returns the type made from the constructors. *)
	    fun constructors args =
	    let
	      val constructed = mkTypeConstruction (id lex, undefType, args);
	    in
	      insymbol lex;
	      if sy lex eq SYMBOLS.ident
	      then constructors [constructed]
	      else constructed
	    end;
	  in
	    constructors tySeq
	  end

	 (* no constructor - get the first part of the sequence
	    and check that that's all. *)
	 else
	   case tySeq of
	     []     => badType
	   | [t]    => t
	   | (t::_) => (badsyms (SYMBOLS.ident, lex); t)
       end (* basicType *);

       (* ty * .. * ty  *)
       val fsys' = fsys ++ SYMSET.asterisk;
       val firstPart = basicType fsys';
     in
       if testfor (SYMBOLS.asterisk, empty, lex)
       then let
	 fun parseRest () =
	   basicType fsys' ::
	        (if testfor (SYMBOLS.asterisk, empty, lex)
	         then parseRest ()
	         else []
		);
       in
	 mkProductType (firstPart :: parseRest ())
       end
       else firstPart
     end;  (* tupleType *)(* ty -> ty *)
   
     val firstType = tupleType (fsys ++ SYMSET.arrow);
   in
     if testfor (SYMBOLS.arrow, empty, lex)
     then mkFunctionType (firstType, parseType (fsys, lex, env))
     else
     (
       skipon (fsys, empty, "End of type", lex);
       firstType
     )
   end;
end;
