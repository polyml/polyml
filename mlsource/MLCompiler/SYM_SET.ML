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
    Title:      Symbols Sets.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)


functor SYM_SET () : 

(*****************************************************************************)
(*                  SYMSET export signature                                  *)
(*****************************************************************************)
sig
  type symset;
  type sys;
   
  val inside: sys * symset -> bool;
  
  val ++ :symset * symset -> symset;
   
  val abortParse:   symset;
  val ident:        symset;
  val abstypeSy:    symset;
  val andSy:        symset;
  val andalsoSy:    symset;
  val asSy:         symset;
  val caseSy:       symset;
  val datatypeSy:   symset;
  val doSy:         symset;
  val elseSy:       symset;
  val endSy:        symset;
  val exceptionSy:  symset;
  val fnSy:         symset;
  val funSy:        symset;
  val handleSy:     symset;
  val ifSy:         symset;
  val inSy:         symset;
  val infixSy:      symset;
  val infixrSy:     symset;
  val letSy:        symset;
  val localSy:      symset;
  val nonfixSy:     symset;
  val ofSy:         symset;
  val opSy:         symset;
  val openSy:       symset;
  val orelseSy:     symset;
  val raiseSy:      symset;
  val recSy:        symset;
  val thenSy:       symset;
  val typeSy:       symset;
  val valSy:        symset;
  val withSy:       symset;
  val whileSy:      symset;
  val leftParen:    symset;
  val rightParen:   symset;
  val leftBrack:    symset;
  val rightBrack:   symset;
  val comma:        symset;
  val colon:        symset;
  val semicolon:    symset;
  val thickArrow:   symset;
  val verticalBar:  symset;
  val equalsSign:   symset;
  val underline:    symset;
  val typeIdent:    symset;
  val stringConst:  symset;
  val integerConst: symset;
  val asterisk:     symset;
  val arrow:        symset;
  val realConst:    symset;
  val wordConst:    symset;
  val charConst:    symset;
  val leftCurly:    symset;
  val rightCurly:   symset;
  val dot:          symset;
  val threeDots:    symset;
  val colonGt:      symset;
  val hashSign:     symset;
  val structureSy:  symset;
  val signatureSy:  symset;
  val structSy:     symset;
  val sigSy:        symset;
  val sharingSy:    symset;
  val functorSy:    symset;
  val withtypeSy:   symset;
  val eqtypeSy:     symset;
  val includeSy:    symset;
  val whereSy:      symset;
  val othersy:      symset;
  val empty:        symset;
  
  val variableSys:        symset;
  val constructorSys:     symset;
  val startAtomicSys:     symset;
  val startPatternSys:    symset;
  val startMatchSys:      symset;
  val startExpressionSys: symset;
  val startDecSys:        symset;
  val declarableVarSys:   symset;
  val startTypeSys:       symset;
  val startSigSys:        symset;
  val startTopSys:        symset;
end (* SYMSET export signature *) =

let

(*****************************************************************************)
(*                  SYMBOLS                                                  *)
(*****************************************************************************)
structure SYMBOLS :
sig
  type sys;
   
  val down: sys -> int;
   
  val abortParse:   sys;
  val ident:        sys;
  val abstypeSy:    sys;
  val andSy:        sys;
  val andalsoSy:    sys;
  val asSy:         sys;
  val caseSy:       sys;
  val datatypeSy:   sys;
  val doSy:         sys;
  val elseSy:       sys;
  val endSy:        sys;
  val exceptionSy:  sys;
  val fnSy:         sys;
  val funSy:        sys;
  val handleSy:     sys;
  val ifSy:         sys;
  val inSy:         sys;
  val infixSy:      sys;
  val infixrSy:     sys;
  val letSy:        sys;
  val localSy:      sys;
  val nonfixSy:     sys;
  val ofSy:         sys;
  val opSy:         sys;
  val openSy:       sys;
  val orelseSy:     sys;
  val raiseSy:      sys;
  val recSy:        sys;
  val thenSy:       sys;
  val typeSy:       sys;
  val valSy:        sys;
  val withSy:       sys;
  val whileSy:      sys;
  val leftParen:    sys;
  val rightParen:   sys;
  val leftBrack:    sys;
  val rightBrack:   sys;
  val comma:        sys;
  val colon:        sys;
  val semicolon:    sys;
  val thickArrow:   sys;
  val verticalBar:  sys;
  val equalsSign:   sys;
  val underline:    sys;
  val typeIdent:    sys;
  val stringConst:  sys;
  val integerConst: sys;
  val asterisk:     sys;
  val arrow:        sys;
  val realConst:    sys;
  val wordConst:    sys;
  val charConst:    sys;
  val leftCurly:    sys;
  val rightCurly:   sys;
  val dot:          sys;
  val threeDots:    sys;
  val colonGt:      sys;
  val hashSign:     sys;
  val structureSy:  sys;
  val signatureSy:  sys;
  val structSy:     sys;
  val sigSy:        sys;
  val sharingSy:    sys;
  val functorSy:    sys;
  val withtypeSy:   sys;
  val eqtypeSy:     sys;
  val includeSy:    sys;
  val whereSy:      sys;
  val othersy:      sys;
  val maxsym:       sys;
end = Symbols
     
(*****************************************************************************)
(*                  SYMS                                                     *)
(*****************************************************************************)
structure SYMS :
sig
  type set;
  
  val empty: set;

  val plus:  set * set -> set;
  
  val mk: int -> set;
  
  val inside: int * set -> bool;
end = Syms

in

(*****************************************************************************)
(*                  SYMSET functor body                                      *)
(*****************************************************************************)
struct
  type sys = SYMBOLS.sys;
  val down = SYMBOLS.down;
  
  type symset = SYMS.set;
  val empty   = SYMS.empty;
  fun mk sym  = SYMS.mk (down sym);
  
  infix 5 inside;
  infix 6 ++;

  fun sym inside s = SYMS.inside (down sym,s);
  
(* fun a ++ b = SYMS.plus (a,b); *)
  val op ++ = SYMS.plus; (* redefined to reduce garbage SPF 26/9/94 *)
  
  val abortParse     = mk SYMBOLS.abortParse;
  val ident          = mk SYMBOLS.ident;
  val abstypeSy      = mk SYMBOLS.abstypeSy;
  val andSy          = mk SYMBOLS.andSy;
  val andalsoSy      = mk SYMBOLS.andalsoSy;
  val asSy           = mk SYMBOLS.asSy;
  val caseSy         = mk SYMBOLS.caseSy;
  val datatypeSy     = mk SYMBOLS.datatypeSy;
  val doSy           = mk SYMBOLS.doSy;
  val elseSy         = mk SYMBOLS.elseSy;
  val endSy          = mk SYMBOLS.endSy;
  val exceptionSy    = mk SYMBOLS.exceptionSy;
  val fnSy           = mk SYMBOLS.fnSy;
  val funSy          = mk SYMBOLS.funSy;
  val handleSy       = mk SYMBOLS.handleSy;
  val ifSy           = mk SYMBOLS.ifSy;
  val inSy           = mk SYMBOLS.inSy;
  val infixSy        = mk SYMBOLS.infixSy;
  val infixrSy       = mk SYMBOLS.infixrSy;
  val letSy          = mk SYMBOLS.letSy;
  val localSy        = mk SYMBOLS.localSy;
  val nonfixSy       = mk SYMBOLS.nonfixSy;
  val ofSy           = mk SYMBOLS.ofSy;
  val opSy           = mk SYMBOLS.opSy;
  val openSy         = mk SYMBOLS.openSy;
  val orelseSy       = mk SYMBOLS.orelseSy;
  val raiseSy        = mk SYMBOLS.raiseSy;
  val recSy          = mk SYMBOLS.recSy;
  val thenSy         = mk SYMBOLS.thenSy;
  val typeSy         = mk SYMBOLS.typeSy;
  val valSy          = mk SYMBOLS.valSy;
  val withSy         = mk SYMBOLS.withSy;
  val whileSy        = mk SYMBOLS.whileSy;
  val structureSy    = mk SYMBOLS.structureSy;
  val signatureSy    = mk SYMBOLS.signatureSy;
  val structSy       = mk SYMBOLS.structSy;
  val sigSy          = mk SYMBOLS.sigSy;
  val sharingSy      = mk SYMBOLS.sharingSy;
  val functorSy      = mk SYMBOLS.functorSy;
  val withtypeSy     = mk SYMBOLS.withtypeSy;
  val eqtypeSy       = mk SYMBOLS.eqtypeSy;
  val includeSy      = mk SYMBOLS.includeSy;
  val whereSy        = mk SYMBOLS.whereSy;
  val leftParen      = mk SYMBOLS.leftParen;
  val rightParen     = mk SYMBOLS.rightParen;
  val leftBrack      = mk SYMBOLS.leftBrack;
  val rightBrack     = mk SYMBOLS.rightBrack;
  val comma          = mk SYMBOLS.comma;
  val colon          = mk SYMBOLS.colon;
  val semicolon      = mk SYMBOLS.semicolon;
  val verticalBar    = mk SYMBOLS.verticalBar;
  val equalsSign     = mk SYMBOLS.equalsSign;
  val thickArrow     = mk SYMBOLS.thickArrow;
  val underline      = mk SYMBOLS.underline;
  val typeIdent      = mk SYMBOLS.typeIdent;
  val stringConst    = mk SYMBOLS.stringConst;
  val integerConst   = mk SYMBOLS.integerConst;
  val realConst      = mk SYMBOLS.realConst;
  val wordConst      = mk SYMBOLS.wordConst;
  val charConst      = mk SYMBOLS.charConst;
  val asterisk       = mk SYMBOLS.asterisk;
  val arrow          = mk SYMBOLS.arrow;
  val leftCurly      = mk SYMBOLS.leftCurly;
  val rightCurly     = mk SYMBOLS.rightCurly;
  val dot            = mk SYMBOLS.dot;
  val threeDots      = mk SYMBOLS.threeDots;
  val colonGt        = mk SYMBOLS.colonGt;
  val hashSign       = mk SYMBOLS.hashSign;
  val othersy        = mk SYMBOLS.othersy;
  
  (* Collections of symbol sets for various syntactic elements *)

  (* Symbols which can be declared as identifiers *)
  val declarableVarSys = ident ++ asterisk;

  (* Symbols which can be variables *)
  val variableSys = declarableVarSys ++ equalsSign;

  (* Symbols which can be constructors *)
  val constructorSys =
    declarableVarSys ++ stringConst ++ integerConst ++
    realConst ++ wordConst ++ charConst;

  (* The symbols which can start an atomic expression *)
  val startAtomicSys = 
    letSy ++ opSy ++ leftBrack ++ leftParen ++ leftCurly ++ 
    variableSys ++ constructorSys ++ hashSign;

  (* The symbols which can start a pattern, Note: "=" is not among them. *)
  (* real constants are not allowed in patterns in ML97.  We leave them
     in and sort it out later. *)
  val startPatternSys =
    underline ++ declarableVarSys ++ constructorSys ++ 
    leftParen ++ leftBrack ++ leftCurly ++ opSy;

  (* The symbols which can start a match *)
  val startMatchSys = startPatternSys;

  (* The symbols which can start an expression *)
  val startExpressionSys = 
    raiseSy ++ ifSy ++ whileSy ++ caseSy ++ fnSy ++ startAtomicSys;

  (* The symbols which can start a declaration *)
  val startDecSys =
    valSy ++ typeSy ++ abstypeSy ++ exceptionSy ++ localSy ++ 
    infixSy ++ infixrSy ++ nonfixSy ++ openSy ++ funSy ++ datatypeSy;

  (* Symbols which can start a type. *)
  val startTypeSys = typeIdent ++ ident ++ leftParen ++ leftCurly;

  (* Symbols which can start a signature.  Strictly speaking
     "sharing" cannot start a signature. *)
  val startSigSys =
    datatypeSy ++ typeSy ++ eqtypeSy ++ valSy ++ exceptionSy ++ 
    structureSy ++ sharingSy ++ includeSy;
 
  val startTopSys =
    structureSy ++ functorSy ++ signatureSy ++ startDecSys ++ startExpressionSys
end (* SYMSET functor body *)

end (* structure-level let *);
