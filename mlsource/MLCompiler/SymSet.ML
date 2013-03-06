(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited
        
    Cleaned up: David C.J. Matthews 2010

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

(*
These symbol sets are used in the parsing process.  The only way they
are actually used is the "inside" function which tests if a symbol
is in the set.  The representation used minimises the memory needed
to implement each set union (++ operator).
*)

structure SymSet :> SymsetSig where type sys = Symbols.sys =

(*****************************************************************************)
(*                  SYMSET functor body                                      *)
(*****************************************************************************)
struct
    open Symbols
  
    datatype symset = Set of sys -> bool

    val empty = Set (fn _ => false);
    infix 6 ++;
(* ...  
    fun plus  (Set f, Set g) = Set (fn x => f x orelse g x);
... *)
    (* Experiment! *) 
    fun (Set f) ++ (Set g) = Set (fn x => g x orelse f x);
  
    infix 5 inside

    fun sym inside (Set s) = s sym

    val abortParse     = Set(fn AbortParse => true | _ => false)
    val ident          = Set(fn Ident => true | _ => false)
    val abstypeSy      = Set(fn AbstypeSy => true | _ => false)
    val andSy          = Set(fn AndSy => true | _ => false)
    val andalsoSy      = Set(fn AndalsoSy => true | _ => false)
    val asSy           = Set(fn AsSy => true | _ => false)
    val caseSy         = Set(fn CaseSy => true | _ => false)
    val datatypeSy     = Set(fn DatatypeSy => true | _ => false)
    val doSy           = Set(fn DoSy => true | _ => false)
    val elseSy         = Set(fn ElseSy => true | _ => false)
    val endSy          = Set(fn EndSy => true | _ => false)
    val exceptionSy    = Set(fn ExceptionSy => true | _ => false)
    val fnSy           = Set(fn FnSy => true | _ => false)
    val funSy          = Set(fn FunSy => true | _ => false)
    val handleSy       = Set(fn HandleSy => true | _ => false)
    val ifSy           = Set(fn IfSy => true | _ => false)
    val inSy           = Set(fn InSy => true | _ => false)
    val infixSy        = Set(fn InfixSy => true | _ => false)
    val infixrSy       = Set(fn InfixrSy => true | _ => false)
    val letSy          = Set(fn LetSy => true | _ => false)
    val localSy        = Set(fn LocalSy => true | _ => false)
    val nonfixSy       = Set(fn NonfixSy => true | _ => false)
    val ofSy           = Set(fn OfSy => true | _ => false)
    val opSy           = Set(fn OpSy => true | _ => false)
    val openSy         = Set(fn OpenSy => true | _ => false)
    val orelseSy       = Set(fn OrelseSy => true | _ => false)
    val raiseSy        = Set(fn RaiseSy => true | _ => false)
    val recSy          = Set(fn RecSy => true | _ => false)
    val thenSy         = Set(fn ThenSy => true | _ => false)
    val typeSy         = Set(fn TypeSy => true | _ => false)
    val valSy          = Set(fn ValSy => true | _ => false)
    val withSy         = Set(fn WithSy => true | _ => false)
    val whileSy        = Set(fn WhileSy => true | _ => false)
    val structureSy    = Set(fn StructureSy => true | _ => false)
    val signatureSy    = Set(fn SignatureSy => true | _ => false)
    val structSy       = Set(fn StructSy => true | _ => false)
    val sigSy          = Set(fn SigSy => true | _ => false)
    val sharingSy      = Set(fn SharingSy => true | _ => false)
    val functorSy      = Set(fn FunctorSy => true | _ => false)
    val withtypeSy     = Set(fn WithtypeSy => true | _ => false)
    val eqtypeSy       = Set(fn EqtypeSy => true | _ => false)
    val includeSy      = Set(fn IncludeSy => true | _ => false)
    val whereSy        = Set(fn WhereSy => true | _ => false)
    val leftParen      = Set(fn LeftParen => true | _ => false)
    val rightParen     = Set(fn RightParen => true | _ => false)
    val leftBrack      = Set(fn LeftBrack => true | _ => false)
    val rightBrack     = Set(fn RightBrack => true | _ => false)
    val comma          = Set(fn Comma => true | _ => false)
    val colon          = Set(fn Colon => true | _ => false)
    val semicolon      = Set(fn Semicolon => true | _ => false)
    val verticalBar    = Set(fn VerticalBar => true | _ => false)
    val equalsSign     = Set(fn EqualsSign => true | _ => false)
    val thickArrow     = Set(fn ThickArrow => true | _ => false)
    val underline      = Set(fn Underline => true | _ => false)
    val typeIdent      = Set(fn TypeIdent => true | _ => false)
    val stringConst    = Set(fn StringConst => true | _ => false)
    val integerConst   = Set(fn IntegerConst => true | _ => false)
    val realConst      = Set(fn RealConst => true | _ => false)
    val wordConst      = Set(fn WordConst => true | _ => false)
    val charConst      = Set(fn CharConst => true | _ => false)
    val asterisk       = Set(fn Asterisk => true | _ => false)
    val arrow          = Set(fn Arrow => true | _ => false)
    val leftCurly      = Set(fn LeftCurly => true | _ => false)
    val rightCurly     = Set(fn RightCurly => true | _ => false)
    val threeDots      = Set(fn ThreeDots => true | _ => false)
    val colonGt        = Set(fn ColonGt => true | _ => false)
    val hashSign       = Set(fn HashSign => true | _ => false)
    val othersy        = Set(fn Othersy => true | _ => false)
  
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
end;

