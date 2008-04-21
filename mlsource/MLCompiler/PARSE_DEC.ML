(*
	Copyright (c) 2000-7
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
    Title:      Parse Expressions and Declarations.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)


functor PARSE_DEC (
   
(*****************************************************************************)
(*                  SYMBOLS                                                  *)
(*****************************************************************************)
structure SYMBOLS :
sig
  type sys
  
  val abortParse:   sys
  val ident:        sys
  val abstypeSy:    sys
  val andSy:        sys 
  val andalsoSy:    sys 
  val asSy:         sys 
  val caseSy:       sys
  val datatypeSy:   sys 
  val doSy:         sys 
  val elseSy:       sys 
  val endSy:        sys 
  val exceptionSy:  sys 
  val fnSy:         sys 
  val funSy:        sys
  val handleSy:     sys 
  val ifSy:         sys 
  val inSy:         sys 
  val infixSy:      sys 
  val infixrSy:     sys 
  val letSy:        sys 
  val localSy:      sys
  val nonfixSy:     sys 
  val ofSy:         sys 
  val opSy:         sys 
  val openSy:       sys 
  val orelseSy:     sys 
  val raiseSy:      sys 
  val recSy:        sys 
  val thenSy:       sys
  val typeSy:       sys 
  val valSy:        sys 
  val withSy:       sys 
  val whileSy:      sys 
  val leftParen:    sys 
  val rightParen:   sys 
  val leftBrack:    sys
  val rightBrack:   sys 
  val comma:        sys 
  val colon:        sys 
  val semicolon:    sys 
  val thickArrow:   sys 
  val verticalBar:  sys
  val equalsSign:   sys 
  val underline:    sys 
  val typeIdent:    sys 
  val realConst:    sys
  val stringConst:  sys
  val integerConst: sys 
  val charConst: 	sys 
  val wordConst: 	sys 
  val asterisk:     sys 
  val arrow:        sys 
  val leftCurly:    sys
  val rightCurly:   sys 
  val threeDots:    sys 
  val colonGt:      sys 
  val hashSign:     sys 
  val structureSy:  sys 
  val signatureSy:  sys
  val structSy:     sys 
  val sigSy:        sys 
  val sharingSy:    sys 
  val functorSy:    sys 
  val withtypeSy:   sys 
  val eqtypeSy:     sys
  val includeSy:    sys 
  val whereSy:		sys;
  
  val eq:   sys * sys -> bool
  val neq:  sys * sys -> bool
end;

(*****************************************************************************)
(*                  SYMSET                                                   *)
(*****************************************************************************)
structure SYMSET :
sig
  type symset
  type sys
   
  val inside: sys * symset -> bool
  
  val ++ :symset * symset -> symset
   
  val ident:        symset
  val andSy:        symset
  val andalsoSy:    symset
  val asSy:         symset
  val doSy:         symset
  val elseSy:       symset
  val endSy:        symset
  val handleSy:     symset
  val inSy:         symset
  val letSy:        symset
  val localSy:      symset
  val ofSy:         symset
  val opSy:         symset
  val orelseSy:     symset
  val thenSy:       symset
  val withSy:       symset
  val leftParen:    symset
  val rightParen:   symset
  val rightBrack:   symset
  val comma:        symset
  val colon:        symset
  val colonGt:      symset
  val semicolon:    symset
  val thickArrow:   symset
  val verticalBar:  symset
  val equalsSign:   symset
  val typeIdent:    symset
  val rightCurly:   symset
  val structureSy:  symset
  val signatureSy:  symset
  val structSy:     symset
  val sigSy:        symset
  val sharingSy:    symset
  val functorSy:    symset
  val withtypeSy:   symset
  val whereSy:      symset
  val empty:        symset
  
  val variableSys:        symset
  val startAtomicSys:     symset
  val startPatternSys:    symset
  val startMatchSys:      symset
  val startExpressionSys: symset
  val startDecSys:        symset
  val declarableVarSys:   symset
  val startTypeSys:       symset
  val startSigSys:        symset
  val startTopSys:        symset
end;

(*****************************************************************************)
(*                  LEX                                                      *)
(*****************************************************************************)
structure LEX :
sig
  type lexan
  type sys
     
  val insymbol: lexan -> unit
     
  (* insymbol sets sy and id which are exported as "read-only" *)
     
  val sy:     lexan -> sys
  val id:     lexan -> string
  val lineno: lexan -> int

  (* pushBackSymbol is a hack.  It pushes a symbol back into
     the lexical analysis stream. *)
  val pushBackSymbol: lexan * sys -> unit;

  (* Error handling *)
  val errorMessage:   lexan * int * string -> unit
  val warningMessage: lexan * int * string -> unit

  val debugParams: lexan -> Universal.universal list
end;

(*****************************************************************************)
(*                  SKIPS                                                    *)
(*****************************************************************************)
structure SKIPS :
sig
  type sys
  type lexan
  type symset
    
  val testfor:  sys * symset * lexan -> bool
  val getsym:   sys * lexan -> unit
  val badsyms:  sys * lexan -> unit
  val notfound: string * lexan -> unit
  val skipon:   symset * symset * string * lexan -> unit
  val getid:    symset * symset * lexan -> string
  val getLabel: symset * lexan -> string
  val getList:  sys * symset * lexan * (unit -> 'a) -> 'a list
end;

(*****************************************************************************)
(*                  STRUCTVALS                                               *)
(*****************************************************************************)
structure STRUCTVALS :
sig
  type types

  type values;
  type typeConstrs;
  type structVals;

  val badType:   types
  val emptyType: types

  val generalisable: int

  (* Infix status *)

  datatype fixStatus = 
    Infix of int
  | InfixR of int
  | Nonfix;

  val undefinedValue: values;
  val undefinedStruct: structVals;
end;
  
(*****************************************************************************)
(*                  TYPETREE                                                 *)
(*****************************************************************************)
structure TYPETREE :
sig
  type types

  val unitType:       types
  val mkTypeVar:      int * bool * bool * bool -> types
  val mkLabelled:     {name: string, typeof: types } list * bool -> types;
  val mkLabelEntry:   string * types -> {name: string, typeof: types };
  val mkFunctionType: types * types -> types;

  (* added 6/12/95 SPS *)
  val badType:        types;
end;

(*****************************************************************************)
(*                  PARSETREE                                                *)
(*****************************************************************************)
structure PARSETREE :
sig
  type types
  type fixStatus
  
 (********* parsetree definition copied here to reduce garbage ***************)

  type typeConstrs;
  type values;
  type structVals;

  (* An identifier is just a name. In the second pass it is associated
     with a particular declaration and the type is assigned into the
     type field. The type of this identifier is needed to deal with
     overloaded operators. If we have an occurence of ``='', say, the
     type of the value will be 'a * 'a -> bool but the type of a particular
     occurence, i.e. the type of the identifier must be int * int -> bool,
     say, after all the unification has been done. *)
          
  type parsetree and valbind and fvalbind and fvalclause and typebind
  and datatypebind and exbind and structureIdentForm;
   
  val isIdent : parsetree -> bool;

  val mkIdent  : string -> parsetree; 
  val mkString : string -> parsetree;
  val mkInt    : string -> parsetree;
  val mkReal   : string -> parsetree;
  val mkChar   : string -> parsetree; 
  val mkWord   : string -> parsetree; 
  val mkApplic : parsetree * parsetree -> parsetree;
  
  val mkCond   : parsetree * parsetree * parsetree -> parsetree;
  val mkTupleTree : parsetree list -> parsetree;
  
  val mkValDeclaration : 
       valbind list *
       {
		 lookup: string -> types option,
		 apply: (string * types -> unit) -> unit
       } *
       {
		 lookup: string -> types option,
		 apply: (string * types -> unit) -> unit
       } ->  parsetree;
  
  val mkFunDeclaration : 
       fvalbind list *
       {
		 lookup: string -> types option,
		 apply: (string * types -> unit) -> unit
       } *
       {
		 lookup: string -> types option,
		 apply: (string * types -> unit) -> unit
       } ->  parsetree;
	
  val mkOpenTree : structureIdentForm list -> parsetree;
  val mkStructureIdent : string -> structureIdentForm;
  val mkValBinding : parsetree * parsetree * int -> valbind; 
  val recValbind: valbind;
  val mkClausal : fvalclause list -> fvalbind;
  val mkClause : parsetree * parsetree * int -> fvalclause;
  val mkList : parsetree list -> parsetree;
  val mkConstraint : parsetree * types -> parsetree; 
  val mkLayered : parsetree * parsetree -> parsetree; 
  val mkFn : parsetree list -> parsetree;
  val mkMatchTree : parsetree * parsetree * int -> parsetree; 
  val mkLocalDeclaration :
  	(parsetree * int) list * (parsetree * int) list * bool -> parsetree;
  val mkTypeDeclaration : typebind list -> parsetree;
  val mkDatatypeDeclaration : datatypebind list * typebind list -> parsetree;
  val mkDatatypeReplication : string * string -> parsetree;
  val mkAbstypeDeclaration :
  	datatypebind list * typebind list * (parsetree * int) list -> parsetree;
  val mkTypeBinding : string * types list * types * bool -> typebind;
  val mkDatatypeBinding : string * types list * (string*types) list -> datatypebind;
  val mkExBinding : string * parsetree * types -> exbind;
  val mkLabelledTree : (string * parsetree) list * bool -> parsetree; 
  val mkSelector : string -> parsetree;
  val mkRaise : parsetree -> parsetree;
  val mkHandleTree : parsetree * parsetree list -> parsetree; 
  val mkWhile : parsetree * parsetree -> parsetree;
  val mkCase : parsetree * parsetree list -> parsetree;
  val mkAndalso : parsetree * parsetree -> parsetree;
  val mkOrelse : parsetree * parsetree -> parsetree;
  val mkDirective : string list * fixStatus -> parsetree; 
  val mkExpseq : (parsetree * int) list -> parsetree;
  val mkExDeclaration  : exbind list -> parsetree;
  val unit      : parsetree;
  val wildCard  : parsetree;
  val emptyTree : parsetree;
end;
   
(*****************************************************************************)
(*                  STRUCTURES                                               *)
(*****************************************************************************)
structure STRUCTURES :
sig
  (* Structures form the global name spaces. *)
  type structs
  type types
  type parsetree
  type sigBind and functorBind and structBind

  val emptyStruct:        structs  (* added SPF 6/2/94 *)
  val mkStructureDec:     structBind list -> structs
  val mkStruct:           structs list -> structs
  val mkSignatureDec:     sigBind list -> structs
  val mkSig:              structs list -> structs
  val mkFunctorDec:       functorBind list -> structs
  val mkInclude:          structs list -> structs
  val mkLocaldec:         structs list * structs list * bool * int -> structs
  val mkTopLevel:         parsetree * int -> structs
  val mkStructureBinding: string * structs * bool * structs * int -> structBind
  val mkStructIdent:      string -> structs
  val mkSigIdent:         string -> structs
  val mkSignatureBinding: string * structs * int -> sigBind
  val mkValSig:           string * types * int -> structs
  val mkExSig:            string * types * int -> structs
  val mkFunctorAppl:      string * structs -> structs
  val mkFormalArg:        string * structs -> structs
  val mkFunctorBinding:   string * structs * bool * structs * structs * int -> functorBind
  val mkSharing:          bool * string list * int -> structs
  val mkWhereType:		  structs * types list * string * types * int -> structs
  val mkSigConstraint:    structs * structs * bool -> structs
end; (* STRUCTURES *)

(*****************************************************************************)
(*                  PARSETYPE                                                *)
(*****************************************************************************)
structure PARSETYPE :
sig
  type symset
  type lexan
  type types
     
  val parseType: symset * lexan * {lookupTvar:string -> types} -> types
end;

(*****************************************************************************)
(*                  UTILITIES                                                *)
(*****************************************************************************)
structure UTILITIES :
sig
  type lexan

  val searchList: unit -> { apply: (string * 'a -> unit) -> unit,
                            enter:  string * 'a -> unit,
                            lookup: string -> 'a option }
    
  val checkForDots: string * lexan * int -> unit
    
end;

(*****************************************************************************)
(*                  MISC                                                     *)
(*****************************************************************************)
structure MISC :
sig
  val lookupDefault : ('a -> 'b option) -> ('a -> 'b option) -> 'a -> 'b option
end;

(*****************************************************************************)
(*                  DEBUG                                                    *)
(*****************************************************************************)
structure DEBUG :
sig
    val ml90Tag: bool Universal.tag
    val getParameter :
           'a Universal.tag -> Universal.universal list -> 'a 
end;

(*****************************************************************************)
(*                  PARSEDEC sharing constraints                             *)
(*****************************************************************************)

sharing type
  SYMBOLS.sys
= SYMSET.sys
= SKIPS.sys
= LEX.sys

sharing type
  SYMSET.symset
= SKIPS.symset
= PARSETYPE.symset

sharing type
  LEX.lexan
= SKIPS.lexan
= PARSETYPE.lexan
= UTILITIES.lexan

sharing type
  STRUCTVALS.types
= TYPETREE.types
= PARSETREE.types
= PARSETYPE.types
= STRUCTURES.types

sharing type
  STRUCTVALS.fixStatus
= PARSETREE.fixStatus
   
sharing type
  PARSETREE.parsetree
= STRUCTURES.parsetree

sharing type
  STRUCTVALS.values
= PARSETREE.values

sharing type
  STRUCTVALS.structVals
= PARSETREE.structVals

sharing type
  STRUCTVALS.typeConstrs
= PARSETREE.typeConstrs
) : 

(*****************************************************************************)
(*                  PARSEDEC export signature                                *)
(*****************************************************************************)
sig
  type lexan;
  type symset;
  type fixStatus;
  type structs;
  
  val parseDec: symset * lexan * { enterFix:  string * fixStatus -> unit,
                                   lookupFix: string -> fixStatus option } -> structs list;
end =

(*****************************************************************************)
(*                  PARSEDEC functor body                                    *)
(*****************************************************************************)
struct
 open MISC;
 
 (* Cannot open SYMBOLS due to name clashes with SYMSET *)
 
 val eq  = SYMBOLS.eq;   infix 5 eq;
 val neq = SYMBOLS.neq;  infix 5 neq;
 
 open SYMSET;   infix 9 ++; infix 4 inside;
 open LEX;
 open SKIPS;
 open STRUCTVALS;
 open TYPETREE;
 open PARSETREE;
 open STRUCTURES;
 open PARSETYPE;
 open UTILITIES;
 open DEBUG;

 (* constant sets defined here to reduce run-time garbage SPF 24/9/94 *)
 val structureLocalSy             = structureSy ++ localSy;
 val structureLocalStartDecSy     = structureSy ++ localSy ++ startDecSys;
 val commaRightCurlySy            = comma ++ rightCurly;
 val declarableVarOpSy            = declarableVarSys ++ opSy;
 val declarableVarLetSy           = declarableVarSys ++ letSy;
 val startTypeDeclarableVarOpSy   = startTypeSys ++ declarableVarOpSy;
 val startDecStructureSy          = startDecSys ++ structureSy;
 val ofVerticalBarSy              = ofSy ++ verticalBar;
 val semicolonStartDecSy          = semicolon ++ startDecSys;
 val semicolonStartDecStructureSy = semicolonStartDecSy ++ structureSy;
 val commaRightBrackSy            = comma ++ rightBrack;
 val rightParenCommaSy            = rightParen ++ comma;
 val rightParenSemicolonSy        = rightParen ++ semicolon;
 val rightParenSemicolonCommaSy   = rightParenSemicolonSy ++ comma;
 val rightParenEqualsSignAndSy    = rightParen ++ equalsSign ++ andSy;
 val colonAsSy                    = colon ++ asSy;
 val colonEqualsSignSy            = colon ++ colonGt ++ equalsSign;
 val thenStartExpressionSy        = thenSy ++ startExpressionSys;
 val elseStartExpressionSy        = elseSy ++ startExpressionSys;
 val ofStartMatchSy               = ofSy ++ startMatchSys;
 val semicolonEndSy               = semicolon ++ endSy
 val andalsoColonSy               = andalsoSy ++ colon;
 val withTypeWithSy               = withtypeSy ++ withSy;
 val ofEqualsSignSy               = ofSy ++ equalsSign;
 val inEndSy                      = inSy ++ endSy;
 val startSigEndSy                = startSigSys ++ endSy;
 val startSigEndAndSy             = startSigEndSy ++ andSy;
 val endAndSy                     = endSy ++ andSy;
 val semicolonStartSigSys		  = startSigSys ++ semicolon;
  
 val topdecStartSy                = functorSy ++ signatureSy ++ structureLocalStartDecSy;

  (* added SPF 6/2/94 *)
  fun mkLocalFixEnv {enterFix,lookupFix,lookupTvar} =
  let
    val newFixEnv   = searchList ();
  in
    {
      enterFix      = #enter newFixEnv,
      lookupFix     = lookupDefault (#lookup newFixEnv) lookupFix,
      lookupTvar    = lookupTvar
    }
  end;
  
  (* added SPF 6/2/94 *)
  fun mkLocalBodyFixEnv {enterFix,lookupFix,lookupTvar} outerEnterFix =
    {
      enterFix      = fn (p as (n, v)) => (enterFix p; outerEnterFix p),
      lookupFix     = lookupFix,
      lookupTvar    = lookupTvar
    }

  (* added SPF 17/4/96 *)
  fun getLongId (kind, fsys, lex) =
    getid (kind, fsys, lex);

  (* added SPF 17/4/96 *)
  fun getShortId (kind, fsys, lex) =
  let
    val iden = getid (kind, fsys, lex);
    val U : unit = checkForDots (iden, lex, lineno lex);
  in
    iden
  end;

 (* Attributes of type variables. *)
   
 fun isEqtype name =
 	size name > 1 andalso String.str(String.sub(name, 1)) = "'";

 (* All explicit type variables are weak in ML97.  This provides
    compatibility with ML90.  *)
 fun isWeak (name, lex) =
 	not (getParameter ml90Tag (debugParams lex)) orelse
	(size name > 1 andalso String.str(String.sub(name, 1)) = "_") orelse
    (isEqtype name andalso size name > 2
		andalso String.str(String.sub(name, 2)) = "_");


 (* Global declarations *)
   
 fun parseDec (fsys, lex, {enterFix,lookupFix}) : structs list =
 let
   (* These procedures to parse type declarations are used in both
      signature and expression parsing. *)

   fun getTypeVars (isDatatype, {apply,enter,lookup}) =
   let
      (* Optional type identifier or sequence of type identifiers.  Used
	     in type and datatype declarations and also in val and fun
		 declarations. *)
      (* The type identifiers must be remembered since they will occur
         subsequently in the components. This is the only case where type
         variables are actually bound. *)
      fun getTypeVar () : types =
        (* Read a type variable and return it. *)
	if (sy lex) neq SYMBOLS.typeIdent
	then (badsyms (SYMBOLS.typeIdent, lex); badType) (* SPF 6/12/95 *)
	else let
	   val iden = id lex;
	   
	   (* Each type variable must be distinct. *)
	   val U : unit = 
	     apply
	       (fn (nm,_) => 
		  if nm = iden (* Same name ? *)
		  then errorMessage (lex, lineno lex, 
				     nm ^ " has already been used.")
		  else ()
	       );
	   (* Construct a type variable and enter it.  Equality and weakness
	      are only set if this is a datatype (or abstype). The type variable
	      should be non-unifiable to get value-constructor signature
	      checking right.*)
	   (* DCJM 11/2/00.  isDatatype is now true for tyvarseqs in fun and val.
	      I don't think it matters what it is set to in datatypes. *)
	   val isEqtype = isDatatype andalso isEqtype iden;
	   val isWeak = isDatatype andalso isWeak (iden, lex);
	   val tyVar = mkTypeVar (generalisable, isEqtype, true, isWeak);
	in
	   enter (iden, tyVar);
	   insymbol lex;
	   tyVar
	end; (* getTypeVar *)
   in
      (* May be one type variable or a bracketed sequence. *)
      if (sy lex) eq SYMBOLS.typeIdent
      then [getTypeVar()] (* One type var. *)
      else if (sy lex) eq SYMBOLS.leftParen
      then (* Sequence. *)
	  	 (
		 insymbol lex;
		 (* There is an awkward parsing problem here if we have either
		    val (a, b) = ... or fun (a X b) = ... We only know that we
			haven't got a tyvarseq once we find a symbol that isn't a
			tyvar.  The easiest way round this is to push the parenthesis
			back into the lex stream and return an empty tyvarseq. *)
		 if (sy lex) neq SYMBOLS.typeIdent
		 then (pushBackSymbol(lex, SYMBOLS.leftParen); [] )
		 else
	        let
	          val t = getList (SYMBOLS.comma, typeIdent, lex, getTypeVar);
	        in
	          getsym (SYMBOLS.rightParen, lex);
	          t
	        end
		 )
      else [] (* None at all. *)
   end; (* getTypeVars *)

   fun getShortNonInfix opThere sys fsys lex {enterFix,lookupFix,lookupTvar} =
   let
      (* op followed by a (short) constructor *)
      val id = getShortId (sys, fsys, lex);
      
      val isInfix =
        case lookupFix id of SOME (Infix _) => true | SOME (InfixR _)  => true | _ => false
           (* It is infix if we find it and it has been declared
              as infix. If it hasn't been declared then it isn't infix.*)

      val U : unit =
        if isInfix andalso not opThere
        then warningMessage (lex, lineno lex,
               "(" ^ id ^") has infix status but was not preceded by op.")
        else ();
   in
      id
   end;
   
   fun getLongNonInfix opThere sys fsys lex {enterFix,lookupFix,lookupTvar} =
   let
      (* op followed by a (long) variable *)
      val id = getLongId (sys, fsys, lex);
      val isInfix =
        case lookupFix id of SOME (Infix _) => true | SOME (InfixR _)  => true | _ => false
   in
      if isInfix andalso not opThere
      then warningMessage (lex, lineno lex,
               "(" ^ id ^") has infix status but was not preceded by op.")
      else ();
      id
   end;

   fun andBindings fsys p =
   (* Handles a sequence of non-recursive declarations separated by "and". *)
   	  p (fsys ++ andSy) ::
	  	(if testfor (SYMBOLS.andSy, empty, lex)
         then andBindings fsys p
         else []);

   fun genTypeVarEnv {lookup,enter,apply} =
   {
      (* All type variables used on the right-hand side of the type
         binding must have been declared before the new type constructor *)
      lookupTvar = 
         fn name =>
            (case lookup name of
               SOME t => t
             | NONE =>
                 (
                   errorMessage (lex, lineno lex, 
                        name ^  " has not been declared in type declaration");
                  badType
                 )
             )
   } (* genTypeVarEnv *);

   fun typeBinding isSpec isEqtype {enterFix,lookupFix,lookupTvar} fsys =
   let
      val newTVenv  = searchList ();
      val typeVars = getTypeVars (false, newTVenv);
      (* The name of the type *)
      val typeName   = getShortId (ident, fsys ++ equalsSign, lex);
	  (*
	  val typeVarEnv =
		{ lookupTvar = lookupDefault (#lookup newTVenv) lookupTvar }
	  *)
      val typeVarEnv = genTypeVarEnv newTVenv;
	  val matchedType =
	  	(* If this is part of a signature we do not need to have an
		   "= ty" after it. If it is an eqtype we must not have one. *)
		if (isSpec andalso (sy lex) neq SYMBOLS.equalsSign) orelse isEqtype
		then emptyType
		else
			(
			getsym (SYMBOLS.equalsSign, lex);
			(* Followed by a type or a sequence of constructors *)
			skipon (startTypeSys, fsys, "type", lex);
			parseType (fsys, lex, typeVarEnv)
			)

   in
      mkTypeBinding (typeName, typeVars, matchedType, isEqtype)
      
   end (* typeBinding *);


   fun datatypeDecOrRepl fsys env isSpecification =
   (* "datatype" has been read.  This may be followed by tycon = datatype ...
      if it is a datatype replication or by tyvarseq tycon = vid ... if it
	  is a datatype binding.  We can only distinguish the two when we reach
	  either the second datatype or an identifier.
	  This is used both for declarations and for specifications. *)
   	let
		val U = insymbol lex;
		val newTVenv = searchList ();
		(* The type variables will be empty if this is a replication. *)
		val typeVars = getTypeVars (true, newTVenv);
		(* The name of the type *)
		val typeName = getShortId (ident, fsys ++ equalsSign, lex);
		
		val U : unit = getsym (SYMBOLS.equalsSign, lex);
   	in
		if (sy lex) eq SYMBOLS.datatypeSy
		then (* Replication *)
			let
				(* Check that the type var sequence was empty. *)
				val U: unit =
					case typeVars of
						[] => ()
					 |  _ => errorMessage (lex, lineno lex,
					 				"Datatype replication must not contain type variables");
				val U = insymbol lex;
				val originalTypeName = getLongId (ident, fsys, lex);
			in
				mkDatatypeReplication(typeName, originalTypeName)
			end
		else (* Binding *)
			let
				(* Process the rest of this binding. *)
				val db = 
					datatypeBind (fsys ++ withtypeSy ++ andSy)
						env typeName typeVars newTVenv isSpecification;
				(* Process any others *)
				val dbs =
					if testfor (SYMBOLS.andSy, empty, lex)
					then andBindings (fsys ++ withtypeSy)
						(fn fsys => datatypeBinding fsys env isSpecification)
					else []
	            val withtypes =
	               if testfor (SYMBOLS.withtypeSy, empty, lex)
	               then andBindings fsys (typeBinding false false env)
	               else [];
			in
				mkDatatypeDeclaration (db :: dbs, withtypes)
          	end
	end
	
   and datatypeBind fsys env typeName typeVars newTVenv isSpecification =
   (* Process the rest of a binding. *)
   		let
			(* Followed by a type or a sequence of constructors *)
			val U = skipon (startTypeDeclarableVarOpSy, fsys, "type", lex);

			(* In ML 90 all type variables on the right hand side of a datbind
			   had to appear in the tyvarseq on the left.  That restriction
			   appears to have been removed for declarations, but not specifications,
			   in ML97. This appears, though, to have been a mistake so I'm
			   reinstating the old behaviour. *)
			(*
			val typeVarEnv =
			{ lookupTvar = lookupDefault (#lookup newTVenv) (#lookupTvar env) }
			*)
			val typeVarEnv = genTypeVarEnv newTVenv;

			fun constrs fsys =
			let
				val opThere = (sy lex) eq SYMBOLS.opSy;
				val U = if opThere then insymbol lex else ();
				(* Identifier - name of constructor *)
				val constrName = getShortNonInfix (opThere orelse isSpecification) declarableVarSys
				                              (fsys ++ ofVerticalBarSy) lex env;
				  
				(* If there is an "of" after this then the constructor is
				    a function from the type following the "of" to the type
				    being declared. Otherwise it is a constant of the type 
				    being declared. *)
				val component = 
				   if (sy lex) eq SYMBOLS.ofSy
				   then (insymbol lex;(* Followed by a type. *)
				         (constrName, parseType (fsys ++ verticalBar, lex, typeVarEnv)))
				   else (constrName, emptyType);
			in
			 component :: 
			   (if testfor (SYMBOLS.verticalBar, empty, lex)
			    then constrs fsys
			    else [])
			end
		in
			mkDatatypeBinding (typeName, typeVars, constrs fsys)
		end

   and datatypeBinding fsys env isSpecification =
   (* Datatype and abstype declarations and datatype specifications. *)
   let
      val newTVenv = searchList ();
      val typeVars = getTypeVars (true, newTVenv);
      (* The name of the type *)
      val typeName = getShortId (ident, fsys ++ equalsSign, lex);

      val U : unit = getsym (SYMBOLS.equalsSign, lex);
	in
		datatypeBind fsys env typeName typeVars newTVenv isSpecification
	end;


   fun makeTypeVarEnv() =
   (* Make an environment for type variables. A type variable is put into the
      environment if it is not already there. Type variables are always put into
      the most local scope first and then tidied up in the second pass. *)
   let
       val {enter,lookup,apply} = searchList ();
       
       (* Type variables used here should go in the scope of the
           most local val or fun. *)
       fun lookupT name =
       let
         (* These type variables are not unifiable until they are generalised. *)
         val newTypeVar =
           mkTypeVar (generalisable, isEqtype name, true, isWeak(name, lex));
           
         val U : unit = enter (name, newTypeVar);
       in
         newTypeVar
       end;
   in
     { lookupTvar = fn s => case lookup s of SOME t => t | NONE => lookupT s,
       lookup     = lookup,
       apply      = apply,
	   enter	  = enter }
   end (* makeTypeVarEnv *);


   fun dec fsys lex decOnly (env as {enterFix,lookupFix,lookupTvar}) =
   let
      (* Sequence of declarations optionally separated by semicolons. *)
      fun decSequence fsys env : (parsetree * int) list=
         if (sy lex) eq SYMBOLS.semicolon
         then (* Semicolons are optional. *)
            (insymbol lex; decSequence fsys env)
         else if (sy lex) inside startDecSys
         then
		 	let
				val lno = lineno lex
			in
				(dec (fsys ++ semicolonStartDecSy) lex true env, lno) ::
					decSequence fsys env
			end
         else (* May be empty *) [];

      (* Constraints *)
      fun constraint exp fsys (env as {enterFix,lookupFix,lookupTvar}) =
         if testfor (SYMBOLS.colon, empty, lex)
         then constraint 
                (mkConstraint (exp, 
                     parseType (fsys ++ colon, lex, {lookupTvar=lookupTvar})))
                fsys
                env
         else exp;

      fun getConstant mkConst =
      let
        (* Return the string. *)
        val data = id lex; (* Save it before insymbol. *)
        val U : unit = insymbol lex;
      in
         mkConst data
      end;

      fun parseInfix fsys opSys startSys atomic
         {enterFix: string * fixStatus -> unit,
          lookupFix: string -> fixStatus option,
          lookupTvar: string -> types } =
      let
         (* Infix operators have a precedence value associated with them,
            the larger the value the more tightly they bind. *)

         val opStartSy = opSy ++ startSys;

         fun parseApplication fsys pproc =
         (* Applies a function to an argument and then tries to apply
            that to the next expression/pattern. *)
            if (sy lex) inside startSys
            then (* Read an argument and apply the constructor *)
               if (sy lex) inside opSys andalso
                  (
                    (* It is infix if we find it and it has been declared
                       as infix. If it hasn't been declared then it isn't
                       infix. *)
                    case lookupFix(id lex) of SOME (Infix _) => true | SOME (InfixR _) => true | _ => false
                  )
               then (* it's an infix operator - don't treat it as an arg. *)
                  pproc
               else let
                 val arg = atomic (fsys ++ startSys)
               in
                 parseApplication fsys (mkApplic (pproc, arg))
               end
            else pproc; (* end parseApplication *)


         fun readNextOps () =
         (* Gets the operand and the following operator (if any) *)
         let
           val express = (* Procedure applications *)
             parseApplication (fsys ++ opSys) (atomic (fsys ++ opStartSy));
           
           val operator = 
              if (sy lex) inside opSys
              then getLongId (opSys, fsys, lex)
              else "";

           val fix = 
             if operator = ""
             then Nonfix
             else valOf(lookupFix operator);

           val preclevl = (* ~1 if not infix or infixr *)
		   		case fix of
					Infix prec => prec
				  | InfixR prec => prec
				  | Nonfix => ~1 (* Not infix *);

           val right = 
		   	  case fix of InfixR _ => true | _ => false; (* undefined if not infix or infixr *)
         in
           {express=express,operator=operator,fix=fix,preclevl=preclevl,right=right}
         end;


         fun nextLevel {express,operator,fix,preclevl,right}
                       returnLevel   (* if we get an operator with this level then we must return. *)
                       rightOperator (* except if we have this operator (ml90 only). *) =
         let
            val next = readNextOps(); (* get the next operator and operand.*)
			
			val U: unit =
				(* In ML97 two operators of the same precedence must both be
				   left associative or both right associative. *)
				if getParameter ml90Tag (debugParams lex) then ()
				else if #preclevl next = preclevl andalso right <> #right next
				then errorMessage (lex, lineno lex,
					"Mixed right and left associative operators of the same precedence.")
				else ();

            val rightOp =
               if #preclevl next > preclevl orelse
                  (* next operator is more binding-it must be processed first *)
                  right andalso
				  	(if getParameter ml90Tag (debugParams lex)
					 then #operator next = operator
					 else #preclevl next = preclevl) 
                  (* next operator is same as previous and right assoc.
                     The ML standard (ML90) says that different operators of the same
                     precedence are always associate to the left (even if they
                     are both right associative operators). *)
				  (* This has changed in ML97.  Now right operators of the
				     same precedence associate to the right. *)
               then nextLevel next (preclevl+1) (if right then operator else ".")
               else next;

            (* At this point we are either at the end of the expression or
               ``rightOp'' contains an operator which is as weak or weaker
               than the ``previous''. We can therefore apply the previous 
               operator to the previous operand and the ``rightOp''
               operand. *)

            val oper = mkIdent operator;

            val applied = 
              { express  = mkApplic (oper, mkTupleTree [express,#express rightOp]),
                operator = #operator rightOp,
                fix      = #fix      rightOp,
                preclevl = #preclevl rightOp,
                right    = #right    rightOp };
            (* If the right operator is stronger than the ``returnLimit''
               (i.e. stronger than the operator before this series) then
                repeat else return this result. *)
         in
            if #preclevl rightOp >= returnLevel orelse
               #operator rightOp  = rightOperator
            then nextLevel applied returnLevel rightOperator
            else applied
         end (* nextLevel *);

         (* parseInfix *)
         val ops = readNextOps (); (* Get the first item. *)
      in
         if #preclevl ops < 0 (* no operator *)
         then #express ops
         else #express (nextLevel ops 0 ".")
      end (* parseInfix *);

      fun pattern fsys lex (env as {enterFix,lookupFix,lookupTvar}) =
      (* Parse a pattern or a fun name apat ... apat sequence. *)
      let
         fun constraintOrLayered pat fsys =
         let
            val isVar = isIdent pat;
            val constr = constraint pat (fsys ++ asSy) env;
         in
            if testfor (SYMBOLS.asSy, empty, lex)
            then
            ( (* Layered pattern *)
              if not isVar
              then errorMessage (lex, lineno lex,
                                 "Expected id or id:ty before `as'")
              else ();
              mkLayered (constr, pattern fsys lex env)
            )
            else constr
         end;

         fun atomicPattern fsys =
         let
            val sym = sy lex;
         in
           if sym eq SYMBOLS.underline (* wild card *)
           then let
            val U : unit = insymbol lex;
           in
             wildCard
           end
         
           else if sym eq SYMBOLS.leftBrack (* list - may be empty *)
           then let
	     val U : unit = insymbol lex;
	     val p = if (sy lex) neq SYMBOLS.rightBrack (* may be empty *)
	     then
	       let
		 fun varsList() =
		    pattern (fsys ++ commaRightBrackSy) lex env ::
		     (if testfor (SYMBOLS.comma, empty, lex)
		      then varsList()
		      else []);
	       in
		 varsList()
	       end
	     else [];
	     val U : unit = getsym (SYMBOLS.rightBrack, lex);
	   in
	     mkList p
	   end
         
           (* bracketed pattern or unit value. *)
           else if sym eq SYMBOLS.leftParen
           then let
	     val U = insymbol lex;
	     val p = if (sy lex) neq SYMBOLS.rightParen
	     then
	       let
		 val first = pattern (fsys ++ rightParenCommaSy) lex env;
		 (* May be a tuple *)
	       in
		 if testfor (SYMBOLS.comma, empty, lex)
		 then
		   let  
		    (* It is a tuple - read the other patterns
		       and make the tuple. *)
		     fun tuples () = 
		       pattern (fsys ++ rightParenCommaSy) lex env ::
			 (if testfor (SYMBOLS.comma, empty, lex)
			  then tuples ()
			  else []);
		   in
		     mkTupleTree (first :: tuples())
		   end
		 else first (* just one *)
	       end
	     else unit;
	    val U : unit = getsym (SYMBOLS.rightParen, lex);
	   in
	      p
	   end
         
           (* Either a labelled record or unit. *)
           else if (sy lex) eq SYMBOLS.leftCurly
           then let
             val U : unit = insymbol lex;
           in
             if testfor (SYMBOLS.rightCurly, empty, lex)
             then unit (* Empty parentheses denote unit *)
             else
               let (* lab1 = pat1, __ , labn = patn <<, ... >>*)
                 fun getLabels () =
                   if testfor (SYMBOLS.threeDots, empty, lex)
                   then
                     {frozen = false,result = []}
                   else let
                     val fsys  = fsys ++ commaRightCurlySy;
                     val ident = getLabel (fsys ++ equalsSign, lex);
                     val patt  =
                       if testfor (SYMBOLS.equalsSign, empty, lex)
                       (* Simple case -- lab = pat *)
                       then pattern fsys lex env
                       else (* sugared form - label is also identifier *)
                       (
                         (* Sugared form not allowed for numeric labels. *)
                         if 0 < size ident
                         andalso String.str(String.sub(ident, 0)) >= "1" 
                         andalso String.str(String.sub(ident, 0)) <= "9"
                         then errorMessage (lex, lineno lex,
                                 " = pat expected after numeric label")
                         else ();
                         (* May have constraint and/or be layered. *)
                         constraintOrLayered (mkIdent ident) fsys
                       );
                     val this = (ident, patt);
                   in
                     if testfor (SYMBOLS.comma, declarableVarSys, lex)
                     then let
                         val getRest = getLabels ();
                       in
                         {frozen = #frozen getRest,
                          result = this :: #result getRest}
                       end
                     else (* Finished. *)
                       {frozen = true,result = [this]}
                   end (* getLabels *);
                 
                 val {frozen, result} = getLabels ();
                 val U : unit = getsym (SYMBOLS.rightCurly, lex);
               in
                 mkLabelledTree (result, frozen)
               end
           end

            (* Constants *)

           else if sym eq SYMBOLS.stringConst
           then getConstant mkString

           else if sym eq SYMBOLS.integerConst
           then getConstant mkInt

           else if sym eq SYMBOLS.realConst
           then
		   	   (
			   (* Real literals were allowed in patterns in ML90. *)
			   if getParameter ml90Tag (debugParams lex) then ()
			   else errorMessage (lex, lineno lex,
			   			"Real constants not allowed in patterns");
			   getConstant mkReal
			   )

           else if sym eq SYMBOLS.charConst
           then getConstant mkChar

           else if sym eq SYMBOLS.wordConst
           then getConstant mkWord

           else if (sy lex) inside declarableVarOpSy   (* Identifiers *)
           then
             let
               val opThere = (sy lex) eq SYMBOLS.opSy;
               val U : unit = if opThere then insymbol lex else ();
             in
               mkIdent (getLongNonInfix opThere declarableVarSys fsys lex env)
             end

           else
             (skipon (empty, fsys, "Pattern", lex); emptyTree)

         end (* atomicPattern *);
       
         (* pattern *)
         
         val U = skipon (startPatternSys, fsys, "Pattern", lex);

         val pat = 
           constraintOrLayered 
             (parseInfix (fsys ++ colonAsSy) declarableVarSys
                startPatternSys atomicPattern env)
           fsys;
           
         val U : unit = skipon (fsys, empty, "End of pattern", lex);
      in
        pat
      end;

      fun expression fsys (env as {enterFix,lookupFix,lookupTvar}) =
      (* Parse an expression *)
      let

         fun expressions fsys separator env =
         (* Sequence of expressions separated by semicolons or commas. Returns the list. *)
            getList (separator, empty, lex, fn () => expression fsys env);

		 fun expressionLineList fsys separator env : (parsetree * int) list =
		 (* Similar to "expressions" except it returns parsetree X line pairs. *)
		 let
		    fun getExpLine () =
			let
				val lno = lineno lex
			in
				(expression fsys env, lno)
			end
		 in
            getList (separator, empty, lex, getExpLine)
		 end;

         fun match fsys =
         (* vs1.exp1 | .. | vsn.expn *)
         let
            val U : unit = skipon (startMatchSys, fsys, "Match", lex);
            val lno = lineno lex;

            (* Read the pattern. *)
            val vars = pattern (fsys ++ thickArrow) lex env;

            val U : unit =
				(* We expect to get a => here but a common problem is to confuse
				   matches with fun declarations and use a = here.  We report it as
				   an error but swallow it as though it was what we wanted. *)
		      if (sy lex) eq SYMBOLS.thickArrow then insymbol lex
			  else
			  	 (
				 notfound ("=>", lex);
				 if (sy lex) eq SYMBOLS.equalsSign then insymbol lex else ()
				 )
            
            (* And now the expression. *)
            val exp = expression (fsys ++ verticalBar) env;

            (* Deal with any other alternatives by recursing. *)

            (* Construct this node, and append any more. *)
            val res = 
              mkMatchTree (vars, exp, lno) ::
                (if testfor (SYMBOLS.verticalBar, empty, lex)
                 then match fsys
                 else []);
         in
            skipon (fsys, empty, "End of match", lex);
            res
         end;

         fun atomicExpression fsys =
           if (sy lex) eq SYMBOLS.leftBrack
           then let
	     val U : unit = insymbol lex;
	     val p = 
	       if (sy lex) neq SYMBOLS.rightBrack (* may be empty *)
	        then expressions (fsys ++ commaRightBrackSy) SYMBOLS.comma env
	        else [];
	    val U : unit = getsym (SYMBOLS.rightBrack, lex);
	   in
	     mkList p
	   end

           (* A parenthesised expression, a tuple,
              a sequence or a unit value *)
           else if (sy lex) eq SYMBOLS.leftParen
           then let
             val U : unit =  insymbol lex;
           in
             if testfor (SYMBOLS.rightParen, empty, lex)
             then unit (* Empty parentheses denote unit *)
             else let
			   val firstLine = lineno lex;
		       val firstExp = expression (fsys ++ rightParenSemicolonCommaSy) env;
		       
		       val exps = 
				 if testfor (SYMBOLS.comma, empty, lex)
				 then
				   (* It is a tuple - read the other expressions
				      and make the tuple. *)
				   mkTupleTree (firstExp :: expressions (fsys ++ rightParenCommaSy) SYMBOLS.comma env)
				 else if testfor (SYMBOLS.semicolon, empty, lex)
				 then
				   mkExpseq ((firstExp, firstLine) ::
				   	expressionLineList (fsys ++ rightParenSemicolonSy) SYMBOLS.semicolon env)
				 else (* Only one *) firstExp;
			 
		       val U : unit = getsym (SYMBOLS.rightParen, lex);
		     in
		       exps
		     end
           end

           (* Either a labelled record or unit. *)
           else if (sy lex) eq SYMBOLS.leftCurly
           then let
             val U : unit =  insymbol lex;
           in
             if testfor (SYMBOLS.rightCurly, empty, lex)
             then unit (* Empty parentheses denote unit *)
             else
               let (* lab1 = exp1, __ , labn = expn *)
                 val labelled = 
                   mkLabelledTree 
                     (getList (SYMBOLS.comma, empty, lex,
                          fn () => 
                          let
                            val ident = getLabel (fsys ++ equalsSign, lex);
                           in
                             getsym (SYMBOLS.equalsSign, lex);
                             (ident,
                                expression (fsys ++ commaRightCurlySy) env)
                           end),
                      true) (* expressions cannot contain ``...'' *);
                 val U : unit = getsym (SYMBOLS.rightCurly, lex);
               in
                 labelled
               end
           end

           (* local declaration *)
           else if (sy lex) eq SYMBOLS.letSy
           then let
	     val U : unit = insymbol lex;
	     val newEnv   = mkLocalFixEnv env
	     val decs     = decSequence (fsys ++ inSy) newEnv;
	     val U : unit = getsym (SYMBOLS.inSy, lex);
	     val exp      = expressionLineList (fsys ++ semicolonEndSy) SYMBOLS.semicolon newEnv;
	     val U : unit = getsym (SYMBOLS.endSy, lex);
	     val U : unit = skipon (fsys, empty, "End of let expression", lex);
	   in
	     mkLocalDeclaration (decs, exp, false) (* "let" rather than "local"*)
	   end

           (* ordinary expression - qualified names allowed *)
           else let
	     val opThere = (sy lex) eq SYMBOLS.opSy;
	     val U       = if opThere then insymbol lex else ();
	     val sym     = sy lex;
	   in
	     if sym inside variableSys 
	     then mkIdent (getLongNonInfix opThere variableSys fsys lex env)

	     else if sym eq SYMBOLS.hashSign (* Selector. *)
	     then 
		 	(
			insymbol lex;
	       	mkSelector (getLabel (fsys, lex))
			)

	     else if sym eq SYMBOLS.stringConst
	     then getConstant mkString

	     else if sym eq SYMBOLS.integerConst
	     then getConstant mkInt

	     else if sym eq SYMBOLS.realConst
	     then getConstant mkReal

	     else if sym eq SYMBOLS.wordConst
	     then getConstant mkWord

	     else if sym eq SYMBOLS.charConst
	     then getConstant mkChar

	     else   (* Expected something e.g. an identifier. *)
	       (badsyms (SYMBOLS.ident, lex); emptyTree)
             end

         (* end atomicExpression *);


         fun keyWordExp fsys =
         (* Expressions introduced by keywords, atomic expressions or
            infixed expressions. Expressions introduced by keywords (e.g. if)
            swallow all of the rest of the expression but they can appear
            within other keyword expressions or after "andalso" and "orelse". *)
         let
           val sym = (sy lex);
         in
            (* if expression *)
            if sym eq SYMBOLS.ifSy
            then let
	      val U : unit = insymbol lex;
	      val test   = expression (fsys ++ thenStartExpressionSy) env;
	      val U : unit = getsym (SYMBOLS.thenSy, lex);
	      val thenPt = expression (fsys ++ elseStartExpressionSy) env;
	      val U : unit = getsym (SYMBOLS.elseSy, lex);
	      val elsePt = expression fsys env;
	    in
	      mkCond (test, thenPt, elsePt)
	    end

            (* while expression *)
            else if sym eq SYMBOLS.whileSy
            then let
	      val U : unit = insymbol lex;
	      val test     = expression (fsys ++ doSy) env;
	    in
	      if (sy lex) eq SYMBOLS.doSy
              then let
	        val U : unit = insymbol lex;
	      in
		mkWhile (test, expression fsys env)
	      end
	      else
		(badsyms (SYMBOLS.doSy, lex); emptyTree)
	    end

            (* case expression *)
            else if sym eq SYMBOLS.caseSy
            then let
	      val U : unit = insymbol lex;
	      val exp = expression (fsys ++ ofStartMatchSy) env;
	      val U : unit = getsym (SYMBOLS.ofSy, lex);
	    in
	      mkCase (exp, match fsys)
	    end
	    
            (* raise exception *)
            else if sym eq SYMBOLS.raiseSy
            then let
	      val U : unit = insymbol lex;
            in
              mkRaise (expression fsys env)
            end

            (* fn expression *)
            else if (sy lex) eq SYMBOLS.fnSy
            then let
	      val U : unit = insymbol lex;
            in
              mkFn (match (fsys ++ semicolon))
            end

            (* type constraint, or similar *)
            else let
              val exp = parseInfix (fsys ++ andalsoColonSy) variableSys startAtomicSys atomicExpression env
            in
              constraint exp (fsys ++ andalsoSy) env
            end
         end (* keyWordExp *);

         fun parseAndalso fsys =
         (* EXP1 andalso EXP2 = if EXP1 then EXP2 else false *)
         let
           val first = keyWordExp (fsys ++ andalsoSy);
           (* N.B. If the expression had been introduced by a keyword (e.g. if)
              then the "else" part would have swallowed any "andalso". *)
         in
           if (sy lex) eq SYMBOLS.andalsoSy
           then
             (insymbol lex; mkAndalso (first, parseAndalso fsys))
           else first
         end;

         fun parseOrelse fsys =
         (* EXP1 orelse EXP2  = if EXP1 then true else EXP2 *)
         let
           val first = parseAndalso (fsys ++ orelseSy);
         in
           if (sy lex) eq SYMBOLS.orelseSy
           then
             (insymbol lex; mkOrelse (first, parseOrelse fsys))
           else first
         end;

       in
         skipon (startExpressionSys, fsys, "Expression", lex);

         if (sy lex) inside startExpressionSys
         then
           let
             val exp = parseOrelse (fsys ++ handleSy);
           in
             if (sy lex) eq SYMBOLS.handleSy
             then
               (insymbol lex; (* Remove "handle" *) mkHandleTree (exp, match fsys))
             else exp
           end
         else emptyTree (* No expression *)

      end; (* expression *)
   in
      (* One declaration. "decOnly" is true if the derived form exp => val it = exp is not allowed here. *)
      if decOnly orelse (sy lex) inside startDecSys
      then  let
        val sym = (sy lex);
      in
          if sym eq SYMBOLS.valSy
          then let
            val U : unit = insymbol lex;
            (* Create two different scopes, for explicitly declared
			   type variables and those implicitly declared. *)
            val implicitTvars = makeTypeVarEnv()
			and explicitTvars = makeTypeVarEnv();
            val newEnv   = {enterFix   = #enterFix  env,
                            lookupFix  = #lookupFix env,
                            lookupTvar =
								(* Look up type variables in the explicit
								   environment, otherwise look them up and
								   add them to the implicit environment. *)
								fn s => case #lookup explicitTvars s of
                                    SOME t => t | NONE => #lookupTvar implicitTvars s};
                            
			(* Tyvarseq *)
			val U = getTypeVars(true,
						{ enter = #enter explicitTvars,
						  lookup = #lookup explicitTvars,
						  apply = #apply explicitTvars});

            (* Processes a value binding. *)
            (* We check for qualified names in the second pass *)
            fun valB fsys =
				let
					val recursive = (sy lex) eq SYMBOLS.recSy
				in
					while (sy lex) eq SYMBOLS.recSy
					do insymbol lex; (* Could be more than one *)
					if recursive then recValbind :: valB fsys
					else
						let
							val lno = lineno lex;
			              	(* Pattern *)
			              	val vars = pattern (fsys ++ equalsSign) lex newEnv;
			              	(* = *)
			              	val U : unit = getsym (SYMBOLS.equalsSign, lex);
			              	(* expression *)
			              	val exp = expression fsys newEnv;
 						in
							mkValBinding (vars, exp, lno) ::
								(if testfor (SYMBOLS.andSy, empty, lex)
						         then valB fsys
						         else [])								
						end
				end;

            val bindings = valB (fsys ++ andSy)
          in
            mkValDeclaration (bindings,
				{lookup= #lookup explicitTvars, apply= #apply explicitTvars},
				{lookup= #lookup implicitTvars, apply= #apply implicitTvars})
          end

          else if sym eq SYMBOLS.funSy
          then let
            val U = insymbol lex;
            (* Create two different scopes, for explicitly declared
			   type variables and those implicitly declared. *)
            val implicitTvars = makeTypeVarEnv()
			and explicitTvars = makeTypeVarEnv();
            val newEnv   = {enterFix   = #enterFix  env,
                            lookupFix  = #lookupFix env,
                            lookupTvar =
                              fn s => case #lookup explicitTvars s of
                                  SOME t => t | NONE => #lookupTvar implicitTvars s};
                            
			(* Tyvarseq *)
			val U = getTypeVars(true,
						{ enter = #enter explicitTvars,
						  lookup = #lookup explicitTvars,
						  apply = #apply explicitTvars});

            fun funB fsys =
            (* Processes a fun binding. *)
            (* We check for qualified names in the second pass *)
            let
              fun bindings() =
              let
                val lno = lineno lex;
                (* Pattern - This isn't really a pattern but we can parse it as
				   that and sort it out later. *)
                val vars = pattern (fsys ++ equalsSign) lex newEnv;
                (* = *)
				(* We expect an equals sign here but a common problem is
				   to confuse fun declarations with matches and use a =>
				   here.  Report the error but swallow the =>. *)
                val U : unit =
			      if (sy lex) eq SYMBOLS.equalsSign then insymbol lex
				  else
				  	 (
					 notfound ("=", lex);
					 if (sy lex) eq SYMBOLS.thickArrow then insymbol lex else ()
					 );
	                 (* expression *)
                val exp  = expression (fsys ++ verticalBar) newEnv;
                val bind = mkClause (vars, exp, lno);
                (* Followed by a vertical bar and another binding ? *)
                val rest = 
                  if testfor (SYMBOLS.verticalBar, empty, lex)
                  then bindings ()
                  else []
              in
                bind :: rest
              end;
            in
              mkClausal (bindings ())
            end (* funB *);

            val bindings = andBindings fsys funB;
          in
            mkFunDeclaration (bindings,
				{lookup= #lookup explicitTvars, apply= #apply explicitTvars},
				{lookup= #lookup implicitTvars, apply= #apply implicitTvars})
          end

          else if sym eq SYMBOLS.typeSy
          then
			(
			insymbol lex;
			mkTypeDeclaration (andBindings fsys (typeBinding false false env))
			)

          else if sym eq SYMBOLS.datatypeSy
          then datatypeDecOrRepl fsys env false

          else if sym eq SYMBOLS.abstypeSy
          then let
            val U         = insymbol lex;
            val tb        = 
              andBindings (fsys ++ withTypeWithSy) 
                (fn fsys => datatypeBinding fsys env false);
                
            val withtypes = 
               if testfor (SYMBOLS.withtypeSy, empty, lex)
               then andBindings (fsys ++ withSy) (typeBinding false false env)
               else [];
               
            val U : unit  = getsym (SYMBOLS.withSy, lex);
            val decs      = decSequence (fsys ++ endSy) env;
          in
            getsym (SYMBOLS.endSy, lex);
            mkAbstypeDeclaration (tb, withtypes, decs)
          end

          else if sym eq SYMBOLS.exceptionSy
          then let
            (* Declares exception identifiers and their types. *)
            val U = insymbol lex;

            (* Get an exception binding. Qualified names prohibited. *)
            fun exceptionBinding fsys =
            let
              (* First the identifier. *)
              val iden = getShortId (variableSys, fsys ++ ofEqualsSignSy, lex);
            in
              (* Either   excon of ty   or   excon = excon' *)
              if testfor (SYMBOLS.ofSy, empty, lex)
              then
                mkExBinding (iden, emptyTree, 
                    parseType (fsys ++ equalsSign, lex, 
                               {lookupTvar= #lookupTvar env}))
              else if testfor (SYMBOLS.equalsSign, empty, lex)
              then
                let (* Must be   = excon' *)
                  val oldIden = getLongId (variableSys, fsys, lex);
                in
                  mkExBinding (iden, mkIdent oldIden, emptyType)
                end
              else mkExBinding (iden, emptyTree, emptyType)
            end;
          in
            mkExDeclaration (andBindings fsys exceptionBinding)
          end

          else if sym eq SYMBOLS.localSy
          then let
            val U       = insymbol lex;
            (* Infix status have this scope. Type-variables have the scope of the enclosing val or fun. *)
            val newEnv  = mkLocalFixEnv env
            (* The local declaration *)
            val ins     = decSequence (fsys ++ inEndSy) newEnv;
            val U : unit = getsym (SYMBOLS.inSy, lex);
            (* Decs are added to both the local and surrounding environment. *)
            val resultEnv = mkLocalBodyFixEnv newEnv enterFix

            val body    = decSequence (fsys ++ endSy) resultEnv;
          in
            getsym (SYMBOLS.endSy, lex);
            mkLocalDeclaration (ins, body, true) (*"local" rather than "let"*)
          end

          else if sym eq SYMBOLS.infixSy orelse
                  sym eq SYMBOLS.infixrSy orelse
                  sym eq SYMBOLS.nonfixSy
          then let
            val U = insymbol lex;

            val fixForm =
            if sym eq SYMBOLS.nonfixSy
            then Nonfix
            else
              let
                val precNo =
                if (sy lex) eq SYMBOLS.integerConst
                then (* Read a precedence number *)
                  let
                    val num = valOf(Int.fromString (id lex))
				  in
                    if num < 0 orelse num > 9
                    then errorMessage (lex, lineno lex,
                                      "Precedence " ^ id lex ^ 
                                      " not allowed, must be between 0 and 9")
                    else ();
                    insymbol lex;
                    num
                  end
                else 0 (* default is zero *);
              in
                if sym eq SYMBOLS.infixSy
                then Infix  precNo (* infix *)
                else InfixR precNo (* infixr *)
              end;

            (* Should now be at least one variable. *)
            val U = skipon (variableSys, fsys, "Variable", lex);

            (* Read the variables and put them in the environ
               with their fix status. Qualified names prohibited. *)
            fun vars() =
              if (sy lex) inside variableSys
              then
                let
                  val iden = getShortId (variableSys, fsys, lex);
                  val U : unit = #enterFix env (iden, fixForm);
                in
                  iden :: vars()
                end
              else [];
          in
            mkDirective (vars (), fixForm)
          end

          (* "open" declaration - qualified names allowed *)
          else if testfor (SYMBOLS.openSy, empty, lex)
          then let
            fun vars() =
              if (sy lex) inside variableSys
              then
                let
                  val iden = getLongId (variableSys, fsys, lex);
                in
                  mkStructureIdent iden :: vars()
                end
              else [];
          in
            if (sy lex) inside variableSys
            then mkOpenTree (vars())
            else (* Identifier missing. *)
              (badsyms (SYMBOLS.ident, lex); emptyTree)
          end

        else emptyTree (* Empty declaration. *)

     end

      else let (* Single expression allowed - short for  val it = exp *)
        val newTvars = makeTypeVarEnv();
        val explicitTvars = makeTypeVarEnv();(* This will always be empty. *)
        val newEnv   = {enterFix   = #enterFix  env,
                        lookupFix  = #lookupFix env,
                        lookupTvar = #lookupTvar newTvars};
        val lno = lineno lex;
        val exp = expression fsys newEnv
      in
        mkValDeclaration ([mkValBinding (mkIdent "it", exp, lno)],
           {lookup= #lookup explicitTvars,apply= #apply explicitTvars},
           {lookup= #lookup newTvars,apply= #apply newTvars})
      end
   end (* dec *);

  (* Parses a signature. *)
  fun parseSignature (fsys : symset) (lex : lexan) (env as {enterFix,lookupFix,lookupTvar}) : structs =
  let  (* May be either a signature name or a sig spec .. spec end seq
          followed by multiple  where type  expressions. *)
    val U : unit = skipon (declarableVarSys ++ sigSy, fsys, "Start of signature", lex)

	val sigexp : structs =
		if testfor (SYMBOLS.sigSy, empty, lex)
		then let (* sig *)
			val sgn = mkSig (signatureSpec (fsys ++ endSy ++ whereSy ++ semicolon) lex env)
		in
			getsym (SYMBOLS.endSy, lex);
			sgn
    	end

    	else if (sy lex) eq SYMBOLS.ident
		then mkSigIdent (getShortId (declarableVarSys, fsys ++ whereSy, lex))

		else (* Only if parse error which will have been reported in skipon. *)
			emptyStruct;

	fun getWhereTypes sigexp =
		let
			(* This is similar to a type binding but with the possibility
			   that the type is a longtycon. *)
			val U: unit = getsym(SYMBOLS.typeSy, lex);
			val lno = lineno lex
			val newTVenv  = searchList ();
		    val typeVars = getTypeVars (false, newTVenv);
			val typeName  = getLongId (ident, fsys ++ equalsSign, lex);
			val typeVarEnv = genTypeVarEnv newTVenv;
			val _ = getsym (SYMBOLS.equalsSign, lex);
			(* Followed by a type or a sequence of constructors *)
			val _ = skipon (startTypeSys, fsys, "type", lex);
			val constrainedSig = mkWhereType(sigexp, typeVars, typeName,
					parseType (fsys ++ whereSy ++ andSy, lex, typeVarEnv),
					lno)
		in
			if testfor (SYMBOLS.whereSy, empty, lex)
				(* Recurse to handle any other wheres. *)
			then getWhereTypes constrainedSig

			else if testfor (SYMBOLS.andSy, empty, lex)
			then
				(* There are two possibilities here.  It may be the start of another
				   type abbreviation or it may be the start of another signature. *)
				if sy lex eq SYMBOLS.typeSy
				then getWhereTypes constrainedSig
				else (* Push the "and" back into the lexer so it can be picked out later. *)
					(
					pushBackSymbol(lex, SYMBOLS.andSy);
					constrainedSig
					)
			else constrainedSig
		end
  in
  	if testfor (SYMBOLS.whereSy, empty, lex)
	then getWhereTypes sigexp
	else sigexp
  end (* parseSignature *)


 (* Sequence of "specs" *)
 and signatureSpec (fsys : symset) (lex : lexan) (env as {enterFix,lookupFix,lookupTvar}) : structs list =
 let
   val signatureTvars = makeTypeVarEnv();

   fun parseSigEntries () : structs list =
   let
	   val U : unit = skipon (fsys ++ semicolonStartSigSys, fsys, "Signature", lex)
       val sym = sy lex;
       val thisSig =
         if sym eq SYMBOLS.datatypeSy
         then
		 	let
		       val lno = lineno lex
			   val sys = fsys ++ startSigEndSy
			   val newenv =
			   	{enterFix = #enterFix env, lookupFix = #lookupFix env,
				(* All type variables on the right hand side of a datatype
				   specification must appear on the left. *)
				 lookupTvar =
				 	fn name =>
						(
			            errorMessage (lex, lineno lex, 
			                        name ^  " has not been declared in type declaration");
			            badType
			            )
				}
			in
				[mkTopLevel (datatypeDecOrRepl sys newenv true, lno)]
			end

         else if sym eq SYMBOLS.typeSy
		 then
			 (* It isn't obvious whether specifications of the form
			    type s and t = int * int (i.e. mixed specifications and
				abbreviations) are allowed.  For the moment allow them. *)
			let
				val lno = lineno lex
				val sys = fsys ++ startSigEndSy
			in
				insymbol lex;
				[mkTopLevel (mkTypeDeclaration
							(andBindings sys (typeBinding true false env)), lno)]
			end

         else if sym eq SYMBOLS.eqtypeSy
		 then
			let
				val lno = lineno lex
				val sys = fsys ++ startSigEndSy
			in
				insymbol lex;
				[mkTopLevel (mkTypeDeclaration
							(andBindings sys (typeBinding true true env)), lno)]
			end
         else if sym eq SYMBOLS.valSy
           then let
             val UUU = insymbol lex
             
             fun doVal () =
               let
                 val lno = lineno lex;
                 val id  = getShortId (declarableVarSys, fsys ++ colon, lex);
                 val U : unit = getsym (SYMBOLS.colon, lex);
                 val ty  = 
                   parseType (fsys ++ startSigEndAndSy, lex, 
                      {lookupTvar = #lookupTvar signatureTvars}); 
                   (* bugfixed SPF 19/2/94 - was #lookup signatureTvars *)
               in
                 mkValSig (id, ty, lno)
               end
           in
             getList (SYMBOLS.andSy, empty, lex, doVal)
           end (* val *)

         else if sym eq SYMBOLS.exceptionSy
           then let(* exception id1 of ty1 and _ and idn of tyn *)
             val U : unit = insymbol lex
             
             fun doEx () =
               let
                 val lno = lineno lex
                 val id  = getShortId (variableSys, fsys ++ ofSy, lex)
                 val ty =
                   if testfor (SYMBOLS.ofSy, empty, lex)
                   then parseType (fsys ++ startSigEndAndSy, lex,
                                  {lookupTvar = lookupTvar})
                   else (* Nullary *) emptyType;
               in
                 mkExSig (id, ty, lno)
               end
           in
             getList (SYMBOLS.andSy, empty, lex, doEx)
           end (* exception *)
           
         else if sym eq SYMBOLS.structureSy
           then let
             val U : unit = insymbol lex

             fun doStructure () =
               let
                 val lno = lineno lex
                 val id  = getShortId (variableSys, empty, lex)
                 val U : unit = getsym (SYMBOLS.colon, lex)
                 val sgn = parseSignature (fsys ++ startSigEndAndSy) lex env
               in
                 mkStructureBinding (id, sgn, false, emptyStruct, lno)
               end
           in
             [mkStructureDec (getList (SYMBOLS.andSy, empty, lex, doStructure))]
           end

         else if sym eq SYMBOLS.includeSy
           then let
			(* In ML 97 we can have "include sigexp" but in addition as
			   a derived form we can have "include ident...ident".
			   Presumably this is for backwards compatibility.
			   sigexp may be a single identifier but could
			   also be an identifier with a "where type" constraint.
			   I hate this sort of inconsistency. 
			   The simplest way to deal with this is to parse the
			   first one as a general signature and then allow multiple
			   identifiers.  That is rather more general than the syntax
			   allows and perhaps we should check that the first signature
			   was simply an identifier. *)
             val UUU = insymbol lex
			 val U : unit =
			 	skipon (declarableVarSys ++ sigSy, fsys, "Start of signature", lex)

			 val firstSig =
			 	parseSignature (fsys ++ startSigEndSy ++ declarableVarSys)
					lex env

			 fun sigids () =
				if (sy lex) eq SYMBOLS.ident
				then mkSigIdent (getShortId (declarableVarSys, fsys, lex))
					:: sigids()
				else []

           in
             [mkInclude (firstSig :: sigids())]
           end

         else if sym eq SYMBOLS.sharingSy
		 then let (* sharing *)
             val UUU = insymbol lex
             (* Now the types or structures. *)
             fun doSharing () : structs =
               let
                 val lno    = lineno lex
                 val isType = testfor (SYMBOLS.typeSy, empty, lex)
                 fun getShare () : string =
                    (getLongId (declarableVarSys,
                            fsys ++ rightParenEqualsSignAndSy,
                            lex));
                 val shares = getShare ()
                 val U : unit = getsym (SYMBOLS.equalsSign, lex)
				 val share: structs =
	                 mkSharing
	                   (isType,
	                   shares :: getList (SYMBOLS.equalsSign, ident, lex, getShare),
	                   lno)
               in
			   	  if (sy lex) eq SYMBOLS.andSy andalso not (getParameter ml90Tag (debugParams lex))
				  then errorMessage (lex, lineno lex,
                                 "sharing ... and ... is not allowed in ML97")
				  else ();
			   	  share
               end (* doSharing *)
           in
             getList (SYMBOLS.andSy, declarableVarSys, lex, doSharing)
           end

		else [] (* Empty. *)
           (* end of parse of thisSig *)
             
        (* continue until the `end' *)
        val UUU = if (sy lex) eq SYMBOLS.semicolon then insymbol lex else ();
    in 
		if (sy lex) inside semicolonStartSigSys
        then thisSig @ parseSigEntries ()
		else thisSig
    end (* parseSigEntries *)
  in
    parseSigEntries ()
  end (* signatureSpec *);


  fun signatureDec (fsys : symset) (lex : lexan) (env as {enterFix,lookupFix,lookupTvar}) : structs =
  let
    val UUU = insymbol lex
    fun doSigDec () =
     let
       val lno = lineno lex
       val id  = getShortId (variableSys, empty, lex);
       val U : unit = getsym (SYMBOLS.equalsSign, lex)
       val sgn = parseSignature (fsys ++ endAndSy) lex env
     in
       mkSignatureBinding (id, sgn, lno)
     end 
  in
    mkSignatureDec (getList (SYMBOLS.andSy, empty, lex, doSigDec))
  end
       
    
  fun structVal (fsys : symset) (lex : lexan) (env as {enterFix,lookupFix,lookupTvar}) : structs =
  let
      (* Series of declarations inside struct...end or (...) in functor
         application. *)
    fun structEnd (fsys: symset) : structs =
    let
      (* Infix declarations are local to struct ... end. *)
      val structEnv = mkLocalFixEnv env
    in
      mkStruct (strDec (fsys ++ endSy) lex structEnv)
    end (* structEnd *)

    val UUU = skipon (structSy ++ declarableVarLetSy,
                      fsys, "struct or functor application", lex);
    
    val fsysPcolon = fsys ++ colon ++ colonGt

    val strExp =
        if testfor (SYMBOLS.structSy, empty, lex)
        then let(* It's a new structure *)
          val str = structEnd fsysPcolon
          val U : unit = getsym (SYMBOLS.endSy, lex)
        in
          str
        end
    
        else if testfor (SYMBOLS.letSy, empty, lex)
        then let
         (* Fixity is local. *)
          val lno = lineno lex;
          val newEnv = mkLocalFixEnv env
          (* The local declaration *)
          val ins  = strDec (fsysPcolon ++ inEndSy) lex newEnv
          val U : unit = getsym (SYMBOLS.inSy, lex)
          val body = [structVal (fsysPcolon ++ endSy) lex newEnv]
          val U : unit = getsym (SYMBOLS.endSy, lex)
        in
          mkLocaldec (ins, body, (* "let" rather than "local" *) false,  lno)
        end
    
        else let (* Either a structure path or a functor application *)
          val iden = getLongId (declarableVarSys, fsysPcolon ++ leftParen, lex);
        in
          if testfor (SYMBOLS.leftParen, empty, lex)
          then let (* functor application *)
            (* Functor names shouldn't be qualified, so we ought to
               check this here using:
                    checkForDots (iden, lex, lineno lex)
               Unfortunately, this breaks the Poly/ML prelude, which
               needs the functor RunCall.Run_exception1, so I've
               taken this check out again! SPF 17/4/96
            *)
            val parameter =
              if (sy lex) eq SYMBOLS.rightParen (* Empty parameter list *)
              then mkStruct []
              else let
                val tsys = fsysPcolon ++ rightParenCommaSy
              in
                (* May be either a structure value or a sequence
                   of declarations. *)
                if (sy lex) inside startDecStructureSy
                then structEnd tsys (* implied struct...end *)
                else structVal tsys lex env
              end
            val U : unit = getsym (SYMBOLS.rightParen, lex)
          in
            mkFunctorAppl (iden, parameter)
          end
          else mkStructIdent iden
        end

  (* We may have one or more constraints. *)
      fun doConstraints strExp =
          if testfor (SYMBOLS.colon, empty, lex)
          then doConstraints(mkSigConstraint(strExp, parseSignature fsysPcolon lex env, false))
          else if testfor (SYMBOLS.colonGt, empty, lex)
          then doConstraints(mkSigConstraint(strExp, parseSignature fsysPcolon lex env, true))
          else strExp
  in
      doConstraints strExp
  end (* structVal *)

  and structureDec (fsys : symset) (lex : lexan) (env as {enterFix,lookupFix,lookupTvar}) : structs =

    if testfor (SYMBOLS.structureSy, empty, lex)
    then let
      fun doStrDec () =
      let (* Read strId <<: sig>> = str *)
        val lno = lineno lex
        (* First the identifier *)
        val strId = getShortId (declarableVarSys, fsys ++ colonEqualsSignSy, lex);
        (* Next the signature if there is one. *)
	    val isOpaque = (sy lex) eq SYMBOLS.colonGt;
        val sgn =
           if testfor (SYMBOLS.colon, empty, lex) orelse testfor (SYMBOLS.colonGt, empty, lex)
           then parseSignature (fsys ++ equalsSign) lex env
           else emptyStruct
        (* Now the equals sign *)
        val U : unit = getsym (SYMBOLS.equalsSign, lex)
      in
        (* And finally the structure value. *)
        mkStructureBinding (strId, sgn, isOpaque, structVal fsys lex env, lno)
      end
    in  
      mkStructureDec (getList (SYMBOLS.andSy, structSy, lex, doStrDec))
    end

    else if testfor (SYMBOLS.localSy, empty, lex)
    then let
      val lno = lineno lex
      val newEnv  = mkLocalFixEnv env
      (* The local declaration *)
      val ins = strDec (fsys ++ inEndSy) lex newEnv
      val U : unit = getsym (SYMBOLS.inSy, lex)
      (* Decs are added to both the local and surrounding environment. *)
      val resultEnv = mkLocalBodyFixEnv newEnv enterFix
      val body = strDec (fsys ++ endSy) lex resultEnv
      val U : unit = getsym (SYMBOLS.endSy, lex)
    in
      mkLocaldec (ins, body, true, lno)
    end
       
    else emptyStruct
  (* end of structureDec *)

  (* Functor declarations. *)
  and functorDec (fsys : symset) (lex : lexan) (env as {enterFix,lookupFix,lookupTvar}) : structs =
  let
    val UUU = insymbol lex; (* remove ``functor'' *)
    fun doFunctDec () : functorBind =
    let (* Read fncId (<<paramSpec>> ) <<: sig>> = str *)
      (* First the identifier *)
      val lno = lineno lex
      val strId = getShortId (declarableVarSys, fsys ++ colonEqualsSignSy, lex);
      val U : unit = getsym (SYMBOLS.leftParen, lex);
      (* Now the parameters *)
      val tsys = fsys ++ rightParenCommaSy;

       val parameter = (* empty | name:sigexp | spec *)
         if (sy lex) eq SYMBOLS.rightParen
         (* empty *)
         then mkFormalArg ("", mkSig [])
            
         else if (sy lex) inside startSigSys
           (* spec *)
         then mkFormalArg ("", mkSig (signatureSpec tsys lex env))
         
         (* name : sigexp *)
         else let
           val strId = getShortId (declarableVarSys, tsys ++ colon, lex);
           val U : unit = getsym (SYMBOLS.colon, lex)
 
           (* Next the signature. *)
           val sgn = parseSignature (tsys ++ sharingSy) lex env
         in
           mkFormalArg (strId, sgn)
         end (* parameter *)

       val U : unit = getsym (SYMBOLS.rightParen, lex)
       
       (* Next the signature if there is one. *)
	   val isOpaque = (sy lex) eq SYMBOLS.colonGt;
	   
       val sgn =
         if testfor (SYMBOLS.colon, empty, lex) orelse testfor (SYMBOLS.colonGt, empty, lex)
         then parseSignature (fsys ++ equalsSign) lex env
         else emptyStruct
           
       (* Now the equals sign *)
       val U : unit = getsym (SYMBOLS.equalsSign, lex)
     in
       (* And finally the functor value. *)
       mkFunctorBinding (strId, sgn, isOpaque, structVal fsys lex env, parameter, lno)
     end (* doFunctDec *)
  in
    mkFunctorDec (getList (SYMBOLS.andSy, structSy, lex, doFunctDec))
  end (* functorDec *)


  and strDec (fsys : symset) (lex : lexan) (env as {enterFix,lookupFix,lookupTvar}) : structs list =
   (* A sequence of declarations, optionally separated by semicolons. *)
  let
    fun getDecs () : structs list =
    let 
      val tsys = fsys ++ semicolonStartDecStructureSy;
    in
      (* Semicolons are optional. *)
      if (sy lex) eq SYMBOLS.semicolon
      then let
        val UUU = insymbol lex
      in 
        getDecs ()
      end
      else if (sy lex) inside structureLocalSy
        then (structureDec tsys lex env) :: getDecs()
      else if (sy lex) inside startDecSys
        then let
          val lno = lineno lex
        in
          (mkTopLevel (dec tsys lex true env, lno)) :: getDecs()
        end
      else (* May be empty *) []
    end (* getDecs *)
  in
    (* Return the declarations. *)
    getDecs ()
  end (* strDec *);

   val globalEnv =
      (* Extend the fixity environment with a type var environment which traps
         top-level type variables in exceptions. *)
    { enterFix   = enterFix,
      lookupFix  = lookupFix,
      lookupTvar =
        fn (s : string) => 
        let
          val U : unit =
            errorMessage (lex, lineno lex, "Free type variables not allowed");
        in
          mkTypeVar (generalisable, false, true, true)
        end}

  (* May be structure/functor dec, signature dec or top-level dec. Treat
     "local" as a structure dec even if it is actually declaring a value
     or type. *)
  val tsys = fsys ++ startTopSys;

  fun doStrDecs () =
    if (sy lex) eq SYMBOLS.structureSy orelse
       (sy lex) eq SYMBOLS.localSy
    (* Structure or local declarations.  Treat all local
       declarations as strDecs since this includes the case of
       local dec in dec. *)
     then structureDec tsys lex globalEnv
     else let
       val lno = lineno lex
     in
       mkTopLevel (dec tsys lex true globalEnv, lno)
     end   
         
  fun parseTopDecs (okStartSet : symset) : structs list =
  let
    val startSym = sy lex;
  in
    if startSym eq SYMBOLS.semicolon orelse
       startSym eq SYMBOLS.abortParse
    then []
    
    else if startSym inside topdecStartSy
    then let
      (* Is this the same kind of topdec as the previous one?
	     This is no longer required in ML97. *)
      val U : unit =
        if not (startSym inside okStartSet) andalso getParameter ml90Tag (debugParams lex)
        then warningMessage (lex, lineno lex,
               "ML Standard requires ';' between different kinds of top-level declaration.")
	    else ();

      val (aDec : structs, newOkStartSet : symset) =
        if startSym eq SYMBOLS.functorSy
          then (functorDec tsys lex globalEnv, functorSy)
          
        else if startSym eq SYMBOLS.signatureSy
          then (signatureDec tsys lex globalEnv, signatureSy)
          
        else if startSym eq SYMBOLS.structureSy
          then (structureDec tsys lex globalEnv, structureLocalStartDecSy)
          
         (* Local declarations are ambiguous; we treat them as strDecs *)
        else if startSym eq SYMBOLS.localSy
          then (structureDec tsys lex globalEnv, structureLocalStartDecSy)
          
        (* let, val, fun etc. *)
        else let
          val lno : int = lineno lex;
        in
          (mkTopLevel (dec tsys lex true globalEnv, lno), structureLocalStartDecSy)
        end;
        
    in
      aDec :: parseTopDecs newOkStartSet
    end
     
    else let
      val U : unit = notfound (";", lex);
    in
      []
    end
  end; (* parseTopDecs *)

in (* body of parseDec *)
  (* topdecs are either fundecs, sigdecs, strDecs (including decs) or a
     single expression.
     
     We now handle everything except the single expression in "parseTopDecs".
     This makes it easier to produce warning messages for missing semi-colons
     that the ML Standard requires between different kinds of topdec.
     SPF 18/7/96
  *)

  if (sy lex) inside topdecStartSy
    then parseTopDecs topdecStartSy
  
  else let (* expression - only one allowed. *)
    val lno : int = lineno lex;
    val aDec : structs = mkTopLevel (dec tsys lex false globalEnv, lno);
    val U : unit = 
       if (sy lex) neq SYMBOLS.semicolon andalso
          (sy lex) neq SYMBOLS.abortParse
       then notfound (";", lex)
       else ()
  in
    [aDec]
  end
end; (* parseDec *)

end (* PARSEDEC *);

