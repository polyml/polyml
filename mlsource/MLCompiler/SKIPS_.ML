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
    Title:      Auxiliary Parsing Functions.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

functor SKIPS_ (

(*****************************************************************************)
(*                  SYMBOLS                                                  *)
(*****************************************************************************)
structure SYMBOLS :
sig
  type sys;
    
  val stringConst:  sys;
  val integerConst: sys;
  val eq :          sys * sys -> bool;
  val repr:         sys -> string;
end;

(*****************************************************************************)
(*                  LEX                                                      *)
(*****************************************************************************)
structure LEX :
sig
  type lexan;
  type sys;

  val sy:           lexan -> sys;
  val id:           lexan -> string;
  val insymbol:     lexan -> unit;
  val lineno:       lexan -> int;
  val errorMessage: lexan * int * string -> unit;
end;


(*****************************************************************************)
(*                  SYMSET                                                   *)
(*****************************************************************************)
structure SYMSET :
sig
  type symset;
  type sys;

  val inside:           sys * symset -> bool;
  val ++ :              symset * symset -> symset;
  val ident:            symset;
  val typeIdent:        symset;
  val integerConst:     symset;
  val realConst:        symset;
  val declarableVarSys: symset;
end;

(*****************************************************************************)
(*                  SKIPS sharing constraints                                *)
(*****************************************************************************)

sharing type
  SYMBOLS.sys
= LEX.sys
= SYMSET.sys

) : 

(*****************************************************************************)
(*                  SKIPS export signature                                   *)
(*****************************************************************************)
sig
  type sys;
  type lexan;
  type symset;
    
  val notfound: string * lexan -> unit;
  val badsyms:  sys * lexan -> unit;
  val getsym:   sys * lexan -> unit;
  val skipon:   symset * symset * string * lexan -> unit;
  val testfor:  sys * symset * lexan -> bool;
  val getid:    symset * symset * lexan -> string;
  val getLabel: symset * lexan -> string;
  val getList:  sys * symset * lexan * (unit -> 'a) -> 'a list;
end =

(*****************************************************************************)
(*                  SKIPS functor body                                       *)
(*****************************************************************************)
struct
  infix 5 inside;
  infix 6 ++;
  infix 8 eq;

  open SYMBOLS LEX SYMSET;
        
  local
    (* Result is in ``id'' *)    
    val idSys = ident ++ typeIdent ++ integerConst ++ realConst;
  in
    (* Try to recreate the original lexical item. *)
    fun notfound (expected, lex) = 
    let
      val found = 
        if sy lex eq SYMBOLS.stringConst 
          then "\"" ^ id lex ^ "\""
        else if sy lex inside idSys 
              then id lex
        else repr (sy lex)
    in
      errorMessage (lex, lineno lex,
         expected ^ " expected but " ^ found ^ " was found")
    end;
  end;

  fun badsyms (sym, lex) = 
    (notfound (repr sym, lex); insymbol lex);

  fun getsym (sym, lex) = 
     if (sy lex) eq sym then insymbol lex else notfound (repr sym, lex);

  infix 5 notin;
  
  fun a notin b = not (a inside b);

  (* used at the end of a syntactic unit to check that the current symbol
      is a suitable terminator *)
  fun skipon (a, b, errmss, lex) = 
    if (sy lex) notin a
    then
      (
        notfound (errmss, lex);
        while sy lex notin (a ++ b) do insymbol lex
      )
    else ();

  (* returns an identifier *)
  fun getid (syms, fsys, lex) = 
    if (sy lex) inside syms
    then let
      val iden = id lex
    in
      insymbol lex;
      iden end
    else
      (
        notfound ("Identifier", lex);
        while sy lex notin fsys do insymbol lex;
        ""
      );

  (* Read a label and check that it is valid if numeric. *)
  fun getLabel (fsys, lex) = 
    if (sy lex) eq SYMBOLS.integerConst
    then let
      val iden = id lex;
      val firstCh = String.str(String.sub(iden, 0));
    in
      insymbol lex;
      if firstCh = "~" orelse firstCh = "0"
      then errorMessage (lex, lineno lex, "Labels must be 1,2,3,....")
      else ();
      iden
    end
    else getid (declarableVarSys, fsys, lex);
  
  (* Tests for a symbol and removes it if it is there.
     startsys is the set of symbols which can start each element. *)
  fun testfor (sym, startsys, lex) =
      (* repeat if the separator or a starting sym is found *)
      if sy lex eq sym
        then (insymbol lex; true)
      else if sy lex inside startsys
        then (badsyms (sym, lex); true)
      else false;

  fun getList (separator, startsys, lex, each) =
  let
    fun forList () = 
      each () ::
      (if testfor (separator, startsys, lex) then forList () else []);
  in
    forList ()
  end;
end (* SKIPS *);
