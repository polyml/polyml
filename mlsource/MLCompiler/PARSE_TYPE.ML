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
    Title:  Parse Types.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
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
structure LEX : LEXSIG

(*****************************************************************************)
(*                  SKIPS                                                    *)
(*****************************************************************************)
structure SKIPS :
sig
  type sys;
  type lexan;
  type symset;
  type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }
  
  val badsyms:  sys * lexan -> unit;
  val testfor:  sys * symset * lexan -> bool;
  val getsym:   sys * lexan -> unit;
  val skipon:   symset * symset * string * lexan -> unit;
  val getid:    symset * symset * lexan -> string * location;
  val getLabel: symset * lexan -> string * location;
  val getList:  sys * symset * lexan * (unit -> 'a * location) -> 'a list * location;
end;

(*****************************************************************************)
(*                  UTILITIES                                                *)
(*****************************************************************************)
structure UTILITIES :
sig
    val noDuplicates: (string * 'a * 'a -> unit) -> 
                       { apply: (string * 'a -> unit) -> unit,
                         enter:  string * 'a -> unit,
                         lookup: string -> 'a option };
end

structure TYPETREE : TYPETREESIG

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
                  
) : 
                  
(*****************************************************************************)
(*                  PARSETYPE export signature                               *)
(*****************************************************************************)
sig
    type symset;
    type lexan;
    type types;
    type typeParsetree;
    type typeVarForm
    type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }
     
    val parseType: symset * lexan * {lookupTvar:string -> typeVarForm} -> typeParsetree * location;
end =
     

(*****************************************************************************)
(*                  PARSETYPE functor body                                   *)
(*****************************************************************************)
struct
    open TYPETREE;
    open LEX;
    open SYMSET;
    open SKIPS;
    open SYMBOLS;    
    open UTILITIES;
   
    infix 7 eq;
    infix 8 ++;
    infix 8 inside;
    
    (* defined here to save re-evaluation (SPF 24/9/94) *)
    val tyseqSyntax = SYMSET.comma ++ SYMSET.rightParen;
    val lrSyntax    = SYMSET.comma ++ SYMSET.rightCurly;
   
    fun parseType (fsys, lex, env) =
    let
        fun tupleType fsys =
        let
            fun basicType fsys =
            let (* First part may be a type sequence. *)
                val sym = sy lex and startLocn = location lex
                val (tySeq, seqLocn) = 
                    if sym eq leftParen
                    then (* sequence of types *)
                    let
                        fun processList () =
                        let
                            val thisType = 
                                if sy lex inside startTypeSys
                                then #1 (parseType (fsys ++ tyseqSyntax, lex, env))
                                else
                                (
                                    badsyms (typeIdent, lex);
                                    ParseTypeBad (* not there *)
                                );
                        in (* Check for any more *)
                            if testfor (SYMBOLS.comma, startTypeSys, lex)
                            then thisType :: processList() (* get some more *)
                            else [thisType] (* that's it *)
                        end (* processList *);

                        val ()      = insymbol lex;  (* Remove opening bracket *)
                        val sequence = processList(); (* read list of items *)
                        val endLocn = location lex (* Should be the loc. of the close paren. *)
                    in
                        getsym (SYMBOLS.rightParen, lex);
                        (sequence, locSpan(startLocn, endLocn))
                    end

                    else if sym eq leftCurly
                    then
                    let
                        val () = insymbol lex; (* Remove opening bracket *)
                        val posEnd = location lex
                    in
        
                        if testfor (SYMBOLS.rightCurly, empty, lex)
                        then
                        let
                            val locs = locSpan(startLocn, posEnd)
                        in
                            ([unitTree locs], locs)
                        end
        
                        else
                        let
                            (* The same label name should not be used more than once. *)
                            fun reportDup (name, newLoc, _) =
                                errorMessage (lex, newLoc, "Label (" ^ name ^ ") appears more than once.")
                            val dupCheck = noDuplicates reportDup
                            (* All the labels should be the same sort. *)
                            val (l, _) = 
                                getList (SYMBOLS.comma, empty, lex,
                                    fn () =>
                                    let
                                        val nameAndLoc as (_, nameLoc) =
                                            getLabel (fsys ++ SYMSET.colon, lex);
                                        val () = #enter dupCheck nameAndLoc;
                                        val () = getsym (SYMBOLS.colon, lex);
                                        val (types, typeLoc) = parseType (fsys ++ lrSyntax, lex, env)
                                        val fullLoc = locSpan(nameLoc, typeLoc)
                                    in
                                        ((nameAndLoc, types, fullLoc), fullLoc)
                                    end);
                            val locs = locSpan(startLocn, location lex) (* Include '}' *)
                        in
                            getsym (SYMBOLS.rightCurly, lex);
                            ([makeParseTypeLabelled(l, true, locs) (* frozen *)], locs)
                        end
                    end
                    
                    else if sym eq typeIdent
                    then
                    let (* type variable *)
                        val ty = #lookupTvar env (id lex);
                    in
                        getsym (typeIdent, lex);
                        ([makeParseTypeId(ty, startLocn)], startLocn)
                    end
      
                    else if sym eq SYMBOLS.ident
                    (* Constructor such as `int' *)
                    then
                    let
                        val idLocn as (_, locn) = getid (SYMSET.ident, fsys, lex)
                    in
                        ([makeParseTypeConstruction (idLocn, ([], locn), locn)], locn)
                    end
                    else
                    (
                        badsyms (SYMBOLS.ident, lex);
                        ([], startLocn)
                    );
            in
                (* Type sequence read. Can now have some type constructors. *)
                if sy lex eq SYMBOLS.ident
                then
                let (* Returns the type made from the constructors. *)
                    fun constructors(args, argLoc) =
                    let
                        val idAndLoc as (_, idLoc) = (id lex, location lex)
                        val loc = locSpan(argLoc, idLoc)
                        val constructed = makeParseTypeConstruction(idAndLoc, (args, argLoc), loc);
                    in
                        insymbol lex;
                        if sy lex eq SYMBOLS.ident
                        then constructors([constructed], loc)
                        else (constructed, loc)
                    end;
                in
                    constructors(tySeq, seqLocn)
                end

                (* no constructor - get the first part of the sequence
                   and check that that's all. *)
                else
                case tySeq of
                  []     => (ParseTypeBad, seqLocn)
                | [t]    => (t, seqLocn)
                | (t::_) => (badsyms (SYMBOLS.ident, lex); (t, seqLocn))
            end (* basicType *);

            (* ty * .. * ty  *)
            fun getProduct () =
            let
                val fsys' = fsys ++ SYMSET.asterisk;
                val (firstPart, firstLocn) = basicType fsys'
            in
                if testfor (SYMBOLS.asterisk, empty, lex)
                then
                let
                    val (rest, restLocn) = getProduct ()
                in
                    (firstPart :: rest, locSpan(firstLocn, restLocn))
                end
                else ([firstPart], firstLocn)
            end
        in
            case getProduct () of
                ([notProduct], locn) => (notProduct, locn)
            |   (product, locn) => (makeParseTypeProduct(product, locn), locn)
        end;  (* tupleType *)(* ty -> ty *)
   
        val (firstType, firstLoc) = tupleType (fsys ++ SYMSET.arrow);
    in
        if testfor (SYMBOLS.arrow, empty, lex)
        then
        let
            val (resType, resLocn) = parseType (fsys, lex, env)
            val locs = locSpan(firstLoc, resLocn)
        in
            (makeParseTypeFunction (firstType, resType, locs), locs)
        end
        else
        (
            skipon (fsys, empty, "End of type", lex);
            (firstType, firstLoc)
        )
   end;
end;
