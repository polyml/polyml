(*
    Copyright (c) 2000-7
        Cambridge University Technical Services Limited

    Further Development Copyright 2009 David C.J. Matthews.

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
  val charConst:     sys 
  val wordConst:     sys 
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
  val whereSy:        sys;
  
  val eq:   sys * sys -> bool
  val neq:  sys * sys -> bool
end;

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

structure LEX : LEXSIG

structure SKIPS :
sig
  type sys
  type lexan
  type symset
  type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }
    
  val testfor:  sys * symset * lexan -> bool
  val getsym:   sys * lexan -> unit
  val badsyms:  sys * lexan -> unit
  val notfound: string * lexan -> unit
  val skipon:   symset * symset * string * lexan -> unit
  val getid:    symset * symset * lexan -> string * location
  val getLabel: symset * lexan -> string * location
  val getList:  sys * symset * lexan * (unit -> 'a * location) -> 'a list * location;
end;

structure STRUCTVALS : STRUCTVALSIG;

structure TYPETREE : TYPETREESIG;

structure PARSETREE : PARSETREESIG
   
structure STRUCTURES : STRUCTURESSIG
structure SIGNATURES: SIGNATURESSIG

structure PARSETYPE :
sig
    type symset;
    type lexan;
    type types;
    type typeParsetree;
    type typeVarForm
    type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }
     
    val parseType: symset * lexan * {lookupTvar:string -> typeVarForm} -> typeParsetree * location;
end;

structure UTILITIES :
sig
    type lexan
    type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }

    val searchList: unit -> { apply: (string * 'a -> unit) -> unit,
                            enter:  string * 'a -> unit,
                            lookup: string -> 'a option }
    
    val checkForDots: string * lexan * location -> unit

    val noDuplicates: (string * 'a * 'a -> unit) -> 
                       { apply: (string * 'a -> unit) -> unit,
                         enter:  string * 'a -> unit,
                         lookup: string -> 'a option };
    
end;

structure MISC :
sig
  val lookupDefault : ('a -> 'b option) -> ('a -> 'b option) -> 'a -> 'b option
end;

sharing STRUCTVALS.Sharing = TYPETREE.Sharing = PARSETREE.Sharing = STRUCTURES.Sharing
      = LEX.Sharing = SIGNATURES.Sharing
      = UTILITIES

sharing SYMBOLS = SYMSET = SKIPS = LEX.Sharing


sharing type
  PARSETYPE.types =
  STRUCTVALS.Sharing.types

sharing type
  PARSETYPE.typeVarForm =
  STRUCTVALS.Sharing.typeVarForm

sharing type
  PARSETYPE.typeParsetree =
  TYPETREE.typeParsetree

sharing type
  SYMSET.symset
= PARSETYPE.symset

sharing type
  LEX.lexan
= PARSETYPE.lexan
) : 

(*****************************************************************************)
(*                  PARSEDEC export signature                                *)
(*****************************************************************************)
sig
  type lexan;
  type symset;
  type fixStatus;
  type program
  
  val parseDec: symset * lexan * { enterFix:  string * fixStatus -> unit,
                                   lookupFix: string -> fixStatus option } -> program;
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
 open SIGNATURES

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
 val rightParenEqualsSignSy       = rightParen ++ equalsSign;
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
 val semicolonStartSigSys          = startSigSys ++ semicolon;
  
 val topdecStartSy                = functorSy ++ signatureSy ++ structureLocalStartDecSy;

  fun mkLocalFixEnv {lookupFix,lookupTvar, ...} =
  let
    val newFixEnv   = searchList ();
  in
    {
      enterFix      = #enter newFixEnv,
      lookupFix     = lookupDefault (#lookup newFixEnv) lookupFix,
      lookupTvar    = lookupTvar
    }
  end;
  
  fun mkLocalBodyFixEnv {enterFix,lookupFix,lookupTvar} outerEnterFix =
    {
      enterFix      = fn p => (enterFix p; outerEnterFix p),
      lookupFix     = lookupFix,
      lookupTvar    = lookupTvar
    }

  fun getLongId (kind, fsys, lex): string * location =
    getid (kind, fsys, lex);

  fun getShortId (kind, fsys, lex): string * location =
  let
    val idLoc as (iden, location) = getid (kind, fsys, lex);
    val () = checkForDots (iden, lex, location);
  in
    idLoc
  end;

 (* Attributes of type variables. *)
   
 fun isEqtype name =
     size name > 1 andalso String.str(String.sub(name, 1)) = "'";

 (* Global declarations *)
   
 fun parseDec (fsys, lex, {enterFix,lookupFix}) : program =
 let
   (* These procedures to parse type declarations are used in both
      signature and expression parsing. *)

   fun getTypeVars (isDatatype, {apply,enter,...}) =
   let
        (* Optional type identifier or sequence of type identifiers.  Used
           in type and datatype declarations and also in val and fun
           declarations. *)
        (* The type identifiers must be remembered since they will occur
           subsequently in the components. This is the only case where type
           variables are actually bound. *)
        fun getTypeVar (): typeVarForm * location =
        (* Read a type variable and return it. *)
        if (sy lex) neq SYMBOLS.typeIdent
        then
        (
            badsyms (SYMBOLS.typeIdent, lex);
            (makeTv {value=emptyType, level=generalisable, equality=true, nonunifiable=true, printable=false},
             location lex)
        )
        else
        let
            val iden = id lex;
            val locn = location lex
       
            (* Each type variable must be distinct. *)
            val () = 
            apply
                (fn (nm,_) => 
                    if nm = iden (* Same name ? *)
                    then errorMessage (lex, location lex, 
                        nm ^ " has already been used.")
                    else ()
                );
            (* Construct a type variable and enter it.  Equality is
               only set if this is a datatype (or abstype). The type variable
               should be non-unifiable to get value-constructor signature
               checking right.*)
            (* DCJM 11/2/00.  isDatatype is now true for tyvarseqs in fun and val.
               I don't think it matters what it is set to in datatypes. *)
            val isEqtype = isDatatype andalso isEqtype iden;
            val tyVar =
                makeTv {value=emptyType, level=generalisable, equality=isEqtype, nonunifiable=true, printable=false}
        in
            enter (iden, tyVar);
            insymbol lex;
            (tyVar, locn)
        end; (* getTypeVar *)
   in
      (* May be one type variable or a bracketed sequence. *)
      if (sy lex) eq SYMBOLS.typeIdent
      then [#1(getTypeVar())] (* One type var. *)
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
              val (t, _) = getList (SYMBOLS.comma, typeIdent, lex, getTypeVar);
            in
              getsym (SYMBOLS.rightParen, lex);
              t
            end
         )
      else [] (* None at all. *)
   end; (* getTypeVars *)
   
   fun getLongNonInfix opThere sys fsys lex {lookupFix,...} =
   let
      (* op followed by a (long) variable *)
      val idLoc as (id, location) = getLongId (sys, fsys, lex);
      val isInfix =
        case lookupFix id of SOME (Infix _) => true | SOME (InfixR _)  => true | _ => false
   in
      if isInfix andalso not opThere
      then warningMessage (lex, location,
               "(" ^ id ^") has infix status but was not preceded by op.")
      else ();
      idLoc
   end;

    fun andBindings(fsys, p: symset -> 'a * location) : 'a list * location =
    (* Handles a sequence of non-recursive declarations separated by "and".
       Returns the list plus the spanning location. *)
    let
        val (item, itemLocn) = p (fsys ++ andSy)
    in
        if testfor (SYMBOLS.andSy, empty, lex)
        then
        let
            val (rest, restLocn) = andBindings(fsys, p)
        in
            (item::rest, locSpan(itemLocn, restLocn))
        end
        else ([item], itemLocn)
    end

   fun genTypeVarEnv {lookup,...} =
   {
      (* All type variables used on the right-hand side of the type
         binding must have been declared before the new type constructor *)
      lookupTvar = 
         fn name =>
            (case lookup name of
               SOME t => t
             | NONE =>
                 (
                   errorMessage (lex, location lex, 
                        name ^  " has not been declared in type declaration");
                   makeTv {value=emptyType, level=generalisable, equality=false,
                           nonunifiable=true, printable=false}
                 )
             )
   } (* genTypeVarEnv *);

    fun typeBinding (isSpec, isEqtype, _) fsys =
    let
        val newTVenv  = searchList ();
        val typeVars = getTypeVars (false, newTVenv);
        (* The name of the type *)
        val (typeName, idLocn)   = getShortId (ident, fsys ++ equalsSign, lex);
        (*
        val typeVarEnv =
            { lookupTvar = lookupDefault (#lookup newTVenv) lookupTvar }
        *)
        val typeVarEnv = genTypeVarEnv newTVenv;
        val (matchedType, endLocn) =
          (* If this is part of a signature we do not need to have an
           "= ty" after it. If it is an eqtype we must not have one. *)
            if (isSpec andalso (sy lex) neq SYMBOLS.equalsSign) orelse isEqtype
            then (NONE, idLocn)
            else
            let
                val () = getsym (SYMBOLS.equalsSign, lex);
                (* Followed by a type or a sequence of constructors *)
                val () = skipon (startTypeSys, fsys, "type", lex);
                val (t, l) = parseType (fsys, lex, typeVarEnv)
            in
                (SOME t, l)
            end
        val bindLocn = locSpan(idLocn, endLocn)
    in
        (mkTypeBinding (typeName, typeVars, matchedType, isEqtype, idLocn, bindLocn), bindLocn)
    end (* typeBinding *);


   fun datatypeDecOrRepl(fsys, env, isSpecification, startLocn) =
   (* "datatype" has been read.  This may be followed by tycon = datatype ...
      if it is a datatype replication or by tyvarseq tycon = vid ... if it
      is a datatype binding.  We can only distinguish the two when we reach
      either the second datatype or an identifier.
      This is used both for declarations and for specifications. *)
       let
        val () = insymbol lex;
        val newTVenv = searchList ();
        (* The type variables will be empty if this is a replication. *)
        val typeVars = getTypeVars (true, newTVenv);
        (* The name of the type *)
        val (typeName, idLocn) = getShortId (ident, fsys ++ equalsSign, lex);
        
        val () = getsym (SYMBOLS.equalsSign, lex);
       in
        if (sy lex) eq SYMBOLS.datatypeSy
        then (* Replication *)
            let
                (* Check that the type var sequence was empty. *)
                val () =
                    case typeVars of
                        [] => ()
                     |  _ => errorMessage (lex, location lex,
                                     "Datatype replication must not contain type variables");
                val () = insymbol lex;
                val (originalTypeName, repLocn) = getLongId (ident, fsys, lex);
                val fullLocn = locSpan(startLocn, repLocn)
            in
                (mkDatatypeReplication{newType=typeName, oldType=originalTypeName,
                    newLoc=idLocn, oldLoc=repLocn, location=fullLocn}, fullLocn)
            end
        else (* Binding *)
            let
                (* Process the rest of this binding. *)
                val (db, dbLocn) = 
                    datatypeBind (fsys ++ withtypeSy ++ andSy,
                        typeName, typeVars, newTVenv, idLocn, isSpecification);
                (* Process any others *)
                val (dbs, dbsLocn) =
                    if testfor (SYMBOLS.andSy, empty, lex)
                    then andBindings
                            (fsys ++ withtypeSy, datatypeBinding isSpecification)
                    else ([], dbLocn)
                val (withtypes, lastLocn) =
                   if testfor (SYMBOLS.withtypeSy, empty, lex)
                   then andBindings(fsys, typeBinding(false, false, env))
                   else ([], dbsLocn);
                val fullLocn = locSpan(startLocn, lastLocn)
            in
                (mkDatatypeDeclaration (db :: dbs, withtypes, fullLocn), fullLocn)
            end
    end
    
    and datatypeBind (fsys, typeName, typeVars, newTVenv, idLocn, isSpecification) =
    (* Process the rest of a binding. *)
    let
        (* Followed by a type or a sequence of constructors *)
        val () = skipon (startTypeDeclarableVarOpSy, fsys, "type", lex);

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
            val () =
                if (sy lex) eq SYMBOLS.opSy
                then
                (
                    if isSpecification
                    then warningMessage (lex, location lex,
                                     "``op'' may not appear before a constructor in a specification.")
                    else ();
                    insymbol lex
                )
                else ();
            (* Identifier - name of constructor *)
            val (constrName, idLocn) = getShortId (declarableVarSys, fsys ++ ofVerticalBarSy, lex)
              
            (* If there is an "of" after this then the constructor is
                a function from the type following the "of" to the type
                being declared. Otherwise it is a constant of the type 
                being declared. *)
            val (component, componentLoc) = 
               if (sy lex) eq SYMBOLS.ofSy
               then
                    let
                        val () = insymbol lex;(* Followed by a type. *)
                        val (theType,typeLocn) = parseType (fsys ++ verticalBar, lex, typeVarEnv)
                    in
                        ({constrName=constrName, constrArg=SOME theType, idLocn=idLocn}, locSpan(idLocn, typeLocn))
                    end
               else ({constrName=constrName, constrArg=NONE, idLocn=idLocn}, idLocn);
        in
            if testfor (SYMBOLS.verticalBar, empty, lex)
            then
            let
                val (tail, locn) = constrs fsys
            in
                (component :: tail, locSpan(componentLoc, locn))
            end
            else ([component], componentLoc)
         end
         
         val (constrs, constrsLocn) = constrs fsys
         val bindLocn = locSpan(idLocn, constrsLocn)
    in
        (mkDatatypeBinding (typeName, typeVars, constrs, idLocn, bindLocn), bindLocn)
    end

    and datatypeBinding isSpecification fsys =
    (* Datatype and abstype declarations and datatype specifications. *)
    let
        val newTVenv = searchList ();
        val typeVars = getTypeVars (true, newTVenv);
        (* The name of the type *)
        val (typeName, idLocn) = getShortId (ident, fsys ++ equalsSign, lex);

        val () = getsym (SYMBOLS.equalsSign, lex);
    in
        datatypeBind (fsys, typeName, typeVars, newTVenv, idLocn, isSpecification)
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
            makeTv {value=emptyType, level=generalisable, equality=isEqtype name,
                    nonunifiable=true, printable=false}
           
         val () = enter (name, newTypeVar);
       in
         newTypeVar
       end;
   in
     { lookupTvar = fn s => case lookup s of SOME t => t | NONE => lookupT s,
       lookup     = lookup,
       apply      = apply,
       enter      = enter }
   end (* makeTypeVarEnv *);


    fun dec (fsys, lex, decOnly, env as {enterFix,...}): parsetree * location =
    let
        (* Sequence of declarations optionally separated by semicolons. *)
        fun decSequence(fsys, env) : parsetree list =
        if (sy lex) eq SYMBOLS.semicolon
        then (* Semicolons are optional. *)
            (insymbol lex; decSequence(fsys, env))
        else if (sy lex) inside startDecSys
        then
            #1(dec(fsys ++ semicolonStartDecSy, lex, true, env)) :: decSequence(fsys, env)
        else (* May be empty *) [];

        (* Constraints *)
        fun constraint (exp, expLoc) fsys (env as {lookupTvar, ...}) =
        if testfor (SYMBOLS.colon, empty, lex)
        then
        let
            val (constrType, typeLoc) = parseType (fsys ++ colon, lex, {lookupTvar=lookupTvar})
            val locs = locSpan(expLoc, typeLoc)
        in
            constraint (mkConstraint (exp, constrType, locs), locs) fsys env
        end
        else (exp, expLoc);

        fun getConstant mkConst =
        let
            (* Return the string. *)
            val data = id lex; (* Save it before insymbol. *)
            val loc  = location lex
        in
            insymbol lex;
            mkConst(data, loc)
        end;

        fun parseInfix fsys opSys startSys atomic{lookupFix: string -> fixStatus option, ... } =
        let
            (* Infix operators have a precedence value associated with them,
               the larger the value the more tightly they bind. *)

            val opStartSy = opSy ++ startSys;

            fun parseApplication fsys (funExp, funLoc) : parsetree * location =
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
                (funExp, funLoc)
            else
                let
                    val (arg, argLoc) = atomic (fsys ++ startSys)
                    val appLoc = locSpan(funLoc, argLoc)
                in
                    parseApplication fsys (mkApplic (funExp, arg, appLoc, false), appLoc)
                end
            else (funExp, funLoc); (* end parseApplication *)


            fun readNextOps () =
            (* Gets the operand and the following operator (if any) *)
            let
                val express = (* function applications *)
                    parseApplication (fsys ++ opSys) (atomic (fsys ++ opStartSy));
           
                val (operator, loc, fix) = 
                    if (sy lex) inside opSys
                    then
                    let
                        val (id, loc) = getLongId (opSys, fsys, lex)
                    in
                        (id, loc, valOf(lookupFix id))
                    end
                    else ("", nullLocation, Nonfix);

               val (preclevl, right) = (* ~1 if not infix or infixr *)
                    case fix of
                        Infix prec => (prec, false)
                      | InfixR prec => (prec, true)
                      | Nonfix => (~1, false) (* Not infix *);
            in
                {express=express,operator=(operator,loc),preclevl=preclevl,right=right}
            end;


            fun nextLevel {express: parsetree*location,operator: string*location,preclevl,right}
                           (returnLevel, lastRight, lastOp) =
            let
                val next = readNextOps(); (* get the next operator and operand.*)
                
                (* In ML97 two operators of the same precedence must both be
                   left associative or both right associative.
                   We actually have to check this in two different places depending
                   on whether these are consecutive operators or there is a higher precedence
                   operator in the middle. *)
                fun checkAssociativity(op1, op2 (* Type includes unused express field. *)) =
                    if #preclevl op1 = #preclevl op2 andalso #right op1 <> #right op2
                    then errorMessage (lex, location lex,
                            concat["Operators \"", #1(#operator op1), "\" and \"", #1 (#operator op2),
                                "\" have the same precedence but \"", #1(#operator op1), "\" is ",
                                if #right op1 then "right" else "left", "-associative while \"",
                                #1 (#operator op2), "\" is ",
                                if #right op2 then "right" else "left", "-associative."])
                    else ();

                val () = checkAssociativity({right=right, operator=operator, preclevl=preclevl}, next)

                val rightOp =
                    if #preclevl next > preclevl orelse
                        (* next operator is more binding-it must be processed first *)
                        right andalso #preclevl next = preclevl 
                    then nextLevel next (preclevl, right, operator)
                    else next;

                (* At this point we are either at the end of the expression or
                   ``rightOp'' contains an operator which is as weak or weaker
                   than the ``previous''. We can therefore apply the previous 
                   operator to the previous operand and the ``rightOp''
                   operand. *)

                val oper = mkIdent operator;
                val appLocn = locSpan(#2 express, #2 (#express rightOp))

                val applied = 
                  { express  = (mkApplic (oper, mkTupleTree([#1 express, #1 (#express rightOp)], appLocn), appLocn, true), appLocn),
                    operator = #operator rightOp,
                    preclevl = #preclevl rightOp,
                    right    = #right    rightOp };
                (* If the right operator is stronger than the ``returnLimit''
                   (i.e. stronger than the operator before this series) then
                    repeat else return this result. *)
            in
                checkAssociativity({right=lastRight, operator=lastOp, preclevl=returnLevel}, rightOp);

                if #preclevl rightOp > returnLevel orelse
                    #preclevl rightOp = returnLevel andalso lastRight
                then nextLevel applied (returnLevel, lastRight, lastOp)
                else applied
            end (* nextLevel *);

            (* parseInfix *)
            val ops = readNextOps (); (* Get the first item. *)
        in
            if #preclevl ops < 0 (* no operator *)
            then #express ops
            else #express (nextLevel ops (~1, false, ("", nullLocation)))
        end (* parseInfix *);

        fun pattern fsys lex env =
        (* Parse a pattern or a fun name apat ... apat sequence. *)
        let
            fun constraintOrLayered (pat, patLoc) fsys =
            let
                val isVar = isIdent pat;
                val (constr, constrLoc) = constraint (pat, patLoc) (fsys ++ asSy) env;
            in
                if testfor (SYMBOLS.asSy, empty, lex)
                then
                let (* Layered pattern *)
                    val () =
                        if not isVar
                        then errorMessage (lex, location lex,
                                         "Expected id or id:ty before `as'")
                        else ();
                    val (lPatt, lPattLoc) = pattern fsys lex env
                    val layeredLoc = locSpan(patLoc, lPattLoc)
                in
                    (mkLayered (constr, lPatt, layeredLoc), layeredLoc)
                end
                else (constr, constrLoc)
            end;

            fun atomicPattern fsys: parsetree * location =
            let
                val sym = sy lex;
                val startLocn = location lex
            in
                if sym eq SYMBOLS.underline (* wild card *)
                then ( insymbol lex; (wildCard startLocn, startLocn) )
         
                else if sym eq SYMBOLS.leftBrack (* list - may be empty *)
                then
                let
                    val () = insymbol lex;
                    val p = if (sy lex) neq SYMBOLS.rightBrack (* may be empty *)
                        then
                        let
                            fun varsList() =
                            let
                                val (p, _) = pattern (fsys ++ commaRightBrackSy) lex env
                            in
                                if testfor (SYMBOLS.comma, empty, lex)
                                then p :: varsList()
                                else [p]
                            end
                        in
                            varsList()
                        end
                        else [];
                    val locs = locSpan(startLocn, location lex)
                    val () = getsym (SYMBOLS.rightBrack, lex);
                in
                    (mkList(p, locs), locs)
                end

                (* bracketed pattern or unit value. *)
                else if sym eq SYMBOLS.leftParen
                then
                let
                    val () = insymbol lex;
                    val p = if (sy lex) neq SYMBOLS.rightParen
                        then
                        let
                            val (first,_) = pattern (fsys ++ rightParenCommaSy) lex env;
                            (* May be a tuple *)
                        in
                            if testfor (SYMBOLS.comma, empty, lex)
                            then
                            let  
                                (* It is a tuple - read the other patterns
                                   and make the tuple. *)
                                fun tuples () =
                                let
                                    val (p, _) = pattern (fsys ++ rightParenCommaSy) lex env
                                in
                                    if testfor (SYMBOLS.comma, empty, lex)
                                    then p :: tuples()
                                    else [p]
                                end
                             in
                                mkTupleTree (first :: tuples(), locSpan(startLocn, location lex))
                            end
                            else (* just one *)
                                mkParenthesised(first, locSpan(startLocn, location lex))
                        end
                        else unit(locSpan(startLocn, location lex));
                    val locs = locSpan(startLocn, location lex)
                    val () = getsym (SYMBOLS.rightParen, lex);
                in
                    (p, locs)
                end
         
               (* Either a labelled record or unit. *)
               else if (sy lex) eq SYMBOLS.leftCurly
               then
               let
                    val () = insymbol lex;
                    val posEnd = location lex
               in
                    if testfor (SYMBOLS.rightCurly, empty, lex)
                    then (* Empty brackets denote unit *)
                        let val locs = locSpan(startLocn, posEnd) in (unit locs, locs) end
                    else
                    let (* lab1 = pat1, __ , labn = patn <<, ... >>*)
                        (* The same label name should not be used more than once. *)
                        fun reportDup (name, newLoc, _) =
                            errorMessage(lex, newLoc, "Label (" ^ name ^ ") appears more than once.")
                        val dupCheck = noDuplicates reportDup

                        fun getLabels () =
                        if testfor (SYMBOLS.threeDots, empty, lex)
                        then {frozen = false, result = []}
                        else
                        let
                            val fsys  = fsys ++ commaRightCurlySy;
                            val (ident, idLoc) = getLabel (fsys ++ equalsSign, lex);
                            val () = #enter dupCheck (ident, idLoc) (* Check for dups. *)
                            val (patt, pattLoc) =
                                if testfor (SYMBOLS.equalsSign, empty, lex)
                                (* Simple case -- lab = pat *)
                                then pattern fsys lex env
                                else (* sugared form - label is also identifier *)
                                (
                                    (* Sugared form not allowed for numeric labels. *)
                                    if 0 < size ident
                                        andalso String.str(String.sub(ident, 0)) >= "1" 
                                        andalso String.str(String.sub(ident, 0)) <= "9"
                                    then errorMessage (lex, location lex,
                                        " = pat expected after numeric label")
                                    else ();
                                    (* May have constraint and/or be layered. *)
                                    constraintOrLayered (mkIdent (ident, idLoc), idLoc) fsys
                                );
                            val labEntry = mkLabelRecEntry(ident, idLoc, patt, locSpan(idLoc, pattLoc))
                        in
                            if testfor (SYMBOLS.comma, declarableVarSys, lex)
                            then
                            let
                                val getRest = getLabels ();
                            in
                                {frozen = #frozen getRest, result = labEntry :: #result getRest}
                            end
                            else (* Finished. *)
                                {frozen = true, result = [labEntry]}
                        end (* getLabels *);
                 
                        val {frozen, result} = getLabels ();
                        val locs = locSpan(startLocn, location lex)
                        val () = getsym (SYMBOLS.rightCurly, lex);
                    in
                        (mkLabelledTree (result, frozen, locs), locs)
                    end
                end

                (* Constants *)

                else if sym eq SYMBOLS.stringConst
                then (getConstant mkString, startLocn)

                else if sym eq SYMBOLS.integerConst
                then (getConstant mkInt, startLocn)

                else if sym eq SYMBOLS.realConst
                then
                (
                    (* Real literals were allowed in patterns in ML90. *)
                    errorMessage (lex, location lex,
                               "Real constants not allowed in patterns");
                    (getConstant mkReal, startLocn)
                )

                else if sym eq SYMBOLS.charConst
                then (getConstant mkChar, startLocn)

                else if sym eq SYMBOLS.wordConst
                then (getConstant mkWord, startLocn)

                else if (sy lex) inside declarableVarOpSy   (* Identifiers *)
                then
                let
                    val opThere = (sy lex) eq SYMBOLS.opSy;
                    val () = if opThere then insymbol lex else ();
                    val idLoc as (_, endLoc) = getLongNonInfix opThere declarableVarSys fsys lex env
                in
                    (mkIdent idLoc, locSpan(startLocn, endLoc))
                end

                else (skipon (empty, fsys, "Pattern", lex); (emptyTree, startLocn))

            end (* atomicPattern *);
       
            (* pattern *)
         
            val () = skipon (startPatternSys, fsys, "Pattern", lex);

            val patAndLoc = 
                constraintOrLayered 
                    (parseInfix (fsys ++ colonAsSy) declarableVarSys
                        startPatternSys atomicPattern env)
                    fsys;
           
            val () = skipon (fsys, empty, "End of pattern", lex);
        in
            patAndLoc
        end (* pattern *);

        fun expression fsys env: parsetree * location =
        (* Parse an expression *)
        let

            fun expressionList(fsys, separator, env): parsetree list =
            (* Sequence of expressions separated by semicolons or commas. Returns the list and strips
               the locations. *)
                #1 (getList (separator, empty, lex, fn () => expression fsys env));

            fun match fsys: matchtree list * location =
            (* vs1.exp1 | .. | vsn.expn *)
            let
                val () = skipon (startMatchSys, fsys, "Match", lex);

                (* Read the pattern. *)
                val (vars, varLoc) = pattern (fsys ++ thickArrow) lex env;

                val () =
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
                val (exp, expLoc) = expression (fsys ++ verticalBar) env;

                (* Construct this node, and append any more. *)
                val thisLocn = locSpan(varLoc, expLoc)
                val thisMatch = mkMatchTree (vars, exp, thisLocn)
                val res =
                    if testfor (SYMBOLS.verticalBar, empty, lex)
                    then
                    let
                        val (m, mloc) = match fsys
                    in
                        (thisMatch :: m, locSpan(thisLocn, mloc))
                    end
                    else ([thisMatch], thisLocn)
            in
                skipon (fsys, empty, "End of match", lex);
                res
            end (* end match *);

            fun atomicExpression fsys: parsetree * location =
            let
                val startSym = sy lex and startLocn = location lex
            in
                if startSym eq SYMBOLS.leftBrack
                then
                let
                    val () = insymbol lex;
                    val p = 
                        if sy lex neq SYMBOLS.rightBrack (* may be empty *)
                        then expressionList (fsys ++ commaRightBrackSy, SYMBOLS.comma, env)
                        else [];
                    val locs = locSpan(startLocn, location lex)
                    val () = getsym (SYMBOLS.rightBrack, lex);
                in
                    (mkList(p, locs), locs)
                end

                (* A parenthesised expression, a tuple, a sequence or a unit value *)
                else if startSym eq SYMBOLS.leftParen
                then
                let
                    val () = insymbol lex;
                    val posEnd = location lex
                in
                    if testfor (SYMBOLS.rightParen, empty, lex)
                    then (* Empty parentheses denote unit *)
                        let val locs = locSpan(startLocn, posEnd) in (unit locs, locs) end
                    else
                    let
                        val (firstExp, _) = expression (fsys ++ rightParenSemicolonCommaSy) env;
       
                        val (exps, fullLocn) = 
                            if testfor (SYMBOLS.comma, empty, lex)
                            then (* Tuple *)
                                let
                                    val expressions =
                                        firstExp :: expressionList (fsys ++ rightParenCommaSy, SYMBOLS.comma, env)
                                    val locs = locSpan(startLocn, location lex)
                                in
                                    (mkTupleTree (expressions, locs), locs)
                                end
                            else if testfor (SYMBOLS.semicolon, empty, lex)
                            then (* Expression sequence. *)
                                let
                                    val expressions =
                                        firstExp :: expressionList (fsys ++ rightParenSemicolonSy, SYMBOLS.semicolon, env)
                                    val locs = locSpan(startLocn, location lex)
                                in
                                    (mkExpseq (expressions, locs), locs)
                                end
                            else (* Only one *)
                                let
                                    val locs = locSpan(startLocn, location lex)
                                in
                                    (mkParenthesised(firstExp, locs), locs)
                                end;

                        val () = getsym (SYMBOLS.rightParen, lex);
                    in
                        (exps, fullLocn)
                    end
                end

                (* Either a labelled record or unit. *)
                else if startSym eq SYMBOLS.leftCurly
                then
                let
                    val () = insymbol lex;
                    val posEnd = location lex
                in
                    if testfor (SYMBOLS.rightCurly, empty, lex)
                    then  (* Empty brackets denote unit *)
                        let val locs = locSpan(startLocn, posEnd) in (unit locs, locs) end
                    else
                    let (* lab1 = exp1, __ , labn = expn *)
                        (* The same label name should not be used more than once. *)
                        fun reportDup (name, newLoc, _) =
                            errorMessage(lex, newLoc, "Label (" ^ name ^ ") appears more than once.")
                        val dupCheck = noDuplicates reportDup

                        fun getEntry () =
                        let
                            val (ident, idLoc) = getLabel (fsys ++ equalsSign, lex);
                            val () = #enter dupCheck (ident, idLoc) (* Check for dups. *)
                            val () = getsym (SYMBOLS.equalsSign, lex);
                            val (labExp, labLoc) = expression (fsys ++ commaRightCurlySy) env
                            val locs = locSpan(idLoc, labLoc)
                        in
                            (mkLabelRecEntry(ident, idLoc, labExp, locs), locs)
                        end
                        val (labs, _) = getList (SYMBOLS.comma, empty, lex, getEntry)
                        val locs = locSpan(startLocn, location lex) (* Include brackets. *)
                        val labelled = mkLabelledTree (labs, true (* always frozen *), locs)
                        val () = getsym (SYMBOLS.rightCurly, lex);
                    in
                        (labelled, locs)
                    end
                end

                (* local declaration *)
                else if startSym eq SYMBOLS.letSy
                then
                let
                    val ()     = insymbol lex;
                    val newEnv = mkLocalFixEnv env
                    val decs   = decSequence (fsys ++ inSy, newEnv);
                    val ()     = getsym (SYMBOLS.inSy, lex);
                    val exp    = expressionList (fsys ++ semicolonEndSy, SYMBOLS.semicolon, newEnv);
                    val locs   = locSpan(startLocn, location lex)
                    val ()     = getsym (SYMBOLS.endSy, lex);
                    val ()     = skipon (fsys, empty, "End of let expression", lex);
                in
                    (mkLocalDeclaration (decs, exp, locs, false) (* "let" rather than "local"*), locs)
                end

                (* ordinary expression - qualified names allowed *)
                else
                let
                    val opThere = startSym eq SYMBOLS.opSy;
                    val ()      = if opThere then insymbol lex else ();
                    val sym     = sy lex;
                    val symLoc  = location lex
                in
                    if sym inside variableSys 
                    then
                    let
                        val (ident, idLoc) = getLongNonInfix opThere variableSys fsys lex env
                    in
                        (mkIdent (ident, idLoc), locSpan(startLocn, idLoc))
                    end

                    else if sym eq SYMBOLS.hashSign (* Selector. *)
                    then 
                     let
                        val () = insymbol lex;
                        val (lab, labLoc) = getLabel (fsys, lex)
                        val locs = locSpan(startLocn, labLoc)
                    in
                        (mkSelector(lab, locs), locs)
                    end

                    else if sym eq SYMBOLS.stringConst
                    then (getConstant mkString, locSpan(startLocn, symLoc))

                    else if sym eq SYMBOLS.integerConst
                    then (getConstant mkInt, locSpan(startLocn, symLoc))

                    else if sym eq SYMBOLS.realConst
                    then (getConstant mkReal, locSpan(startLocn, symLoc))

                    else if sym eq SYMBOLS.wordConst
                    then (getConstant mkWord, locSpan(startLocn, symLoc))

                    else if sym eq SYMBOLS.charConst
                    then (getConstant mkChar, locSpan(startLocn, symLoc))

                    else   (* Expected something e.g. an identifier. *)
                       (badsyms (SYMBOLS.ident, lex); (emptyTree, symLoc))
                end
            end(* end atomicExpression *);


            fun keyWordExp fsys: parsetree * location =
            (* Expressions introduced by keywords, atomic expressions or
               infixed expressions. Expressions introduced by keywords (e.g. if)
               swallow all of the rest of the expression but they can appear
               within other keyword expressions or after "andalso" and "orelse". *)
            let
                val sym = sy lex;
                val startLocn = location lex
            in
                (* if expression *)
                if sym eq SYMBOLS.ifSy
                then
                let
                    val () = insymbol lex;
                    val (test, _) = expression (fsys ++ thenStartExpressionSy) env;
                    val () = getsym (SYMBOLS.thenSy, lex);
                    val (thenPt, _) = expression (fsys ++ elseStartExpressionSy) env;
                    val () = getsym (SYMBOLS.elseSy, lex);
                    val (elsePt, elseLocn) = expression fsys env;
                    val locs = locSpan(startLocn, elseLocn)
                in
                    (mkCond (test, thenPt, elsePt, locs), locs)
                end

                (* while expression *)
                else if sym eq SYMBOLS.whileSy
                then
                let
                    val () = insymbol lex;
                    val (test, testLocn) = expression (fsys ++ doSy) env;
                 in
                    if (sy lex) eq SYMBOLS.doSy
                    then
                        let
                            val () = insymbol lex;
                            val (doExp, doLocn) = expression fsys env
                            val locs = locSpan(startLocn, doLocn)
                        in
                            (mkWhile (test, doExp, locs), locs)
                        end
                    else (badsyms (SYMBOLS.doSy, lex); (test, testLocn))
                end

                (* case expression *)
                else if sym eq SYMBOLS.caseSy
                then
                let
                    val () = insymbol lex;
                    val (exp, _) = expression (fsys ++ ofStartMatchSy) env;
                    val () = getsym (SYMBOLS.ofSy, lex);
                    val (m, matchLoc) = match (fsys ++ semicolon)
                    val locs = locSpan(startLocn, matchLoc)
                in
                    (mkCase (exp, m, locs, matchLoc), locs)
                end
        
                (* raise exception *)
                else if sym eq SYMBOLS.raiseSy
                then
                let
                    val () = insymbol lex;
                    val (exp, expLoc) = expression fsys env
                    val locs = locSpan(startLocn, expLoc)
                in
                    (mkRaise (exp, locs), locs)
                end

                (* fn expression *)
                else if (sy lex) eq SYMBOLS.fnSy
                then
                let
                    val () = insymbol lex;
                    val (m, matchLoc) = match (fsys ++ semicolon)
                    val locs = locSpan(startLocn, matchLoc)
                in
                    (mkFn (m, locs), locs)
                end

                (* type constraint, or similar *)
                else
                let
                    val exp = parseInfix (fsys ++ andalsoColonSy) variableSys startAtomicSys atomicExpression env
                in
                    constraint exp (fsys ++ andalsoSy) env
                end
            end (* keyWordExp *);

            fun parseAndalso fsys =
            (* EXP1 andalso EXP2 = if EXP1 then EXP2 else false *)
            let
                val (first, firstLoc) = keyWordExp (fsys ++ andalsoSy);
                (* N.B. If the expression had been introduced by a keyword (e.g. if)
                   then the "else" part would have swallowed any "andalso". *)
            in
                if (sy lex) eq SYMBOLS.andalsoSy
                then
                let
                    val () = insymbol lex;
                    val (right, rightLoc) = parseAndalso fsys
                    val locs = locSpan(firstLoc, rightLoc)
                in
                    (mkAndalso (first, right, locs), locs)
                end
                else (first, firstLoc)
            end;

           fun parseOrelse fsys =
           (* EXP1 orelse EXP2  = if EXP1 then true else EXP2 *)
           let
                val (first, firstLoc) = parseAndalso (fsys ++ orelseSy);
           in
                if (sy lex) eq SYMBOLS.orelseSy
                then
                let
                    val () = insymbol lex;
                    val (right, rightLoc) = parseOrelse fsys
                    val locs = locSpan(firstLoc, rightLoc)
                in
                    (mkOrelse (first, right, locs), locs)
                end
                else (first, firstLoc)
           end;

        in
            skipon (startExpressionSys, fsys, "Expression", lex);

            if (sy lex) inside startExpressionSys
            then
            let
                val (exp, expLoc) = parseOrelse (fsys ++ handleSy);
            in
                if (sy lex) eq SYMBOLS.handleSy
                then
                let
                    val () = insymbol lex; (* Remove "handle" *)
                    val (m, mLoc) = match fsys
                    val locs = locSpan(expLoc, mLoc)
                in
                    (mkHandleTree (exp, m, locs, mLoc), locs)
                end
                else (exp, expLoc)
            end
            else (emptyTree (* No expression *), location lex)

        end; (* expression *)

    in
        (* One declaration. "decOnly" is true if the derived form exp => val it = exp is not allowed here. *)
        if decOnly orelse (sy lex) inside startDecSys
        then
        let
            val sym = sy lex;
            val startLocn = location lex
        in
            if sym eq SYMBOLS.valSy
            then
            let
                val () = insymbol lex;
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
                val _ = getTypeVars(true,
                            { enter = #enter explicitTvars,
                              lookup = #lookup explicitTvars,
                              apply = #apply explicitTvars});

                (* Processes a value binding. *)
                (* We check for qualified names in the second pass *)
                fun valB(fsys, isRec) =
                if (sy lex) eq SYMBOLS.recSy
                then (insymbol lex; valB(fsys, true))
                else
                let
                    (* Pattern *)
                    val (vars, varLoc) = pattern (fsys ++ equalsSign) lex newEnv;
                    (* = *)
                    val () = getsym (SYMBOLS.equalsSign, lex);
                    (* expression *)
                    val (exp, expLoc) = expression fsys newEnv;
                    (* Other declarations. *)
                    val (tail, tailLocn) =
                        if testfor (SYMBOLS.andSy, empty, lex)
                        then valB(fsys, isRec)
                        else ([], expLoc)
                in
                     (mkValBinding (vars, exp, isRec, locSpan(varLoc, expLoc)) :: tail, tailLocn)
                end

                val (bindings, bindLocns) = valB (fsys ++ andSy, false)
                val fullLocn = locSpan(startLocn, bindLocns)
            in
                (mkValDeclaration (bindings,
                    {lookup= #lookup explicitTvars, apply= #apply explicitTvars},
                    {lookup= #lookup implicitTvars, apply= #apply implicitTvars},
                    fullLocn),
                fullLocn)
            end

            else if sym eq SYMBOLS.funSy
            then
            let
                val () = insymbol lex;
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
                val _ = getTypeVars(true,
                            { enter = #enter explicitTvars,
                              lookup = #lookup explicitTvars,
                              apply = #apply explicitTvars});

                fun funB fsys =
                (* Processes a fun binding. *)
                (* We check for qualified names in the second pass *)
                let
                    fun bindings soFar =
                    let
                        (* Pattern - This isn't really a pattern but we can parse it as
                           that initially.  That results in accepting some invalid syntax
                           so we need to check the parsed code.  *)
                        val (vars, varLoc) = pattern (fsys ++ equalsSign) lex newEnv;
                        (* Get the name and number of args. *)
                        val (funPattern, funName, argCount) = mkFunPattern(vars, lex)
                        val () =
                            case soFar of
                                SOME(prevName, prevCount) =>
                                (
                                    if prevName = funName
                                    then ()
                                    else errorMessage (lex, location lex,
                                            "This clause defines function ``" ^ funName ^
                                            "'' but previous clause(s) defined ``" ^
                                            prevName ^ "''");
                                    if prevCount = argCount
                                    then ()
                                    else errorMessage (lex, location lex,
                                            "This clause has " ^ Int.toString argCount ^
                                            " arguments but previous clause(s) had " ^
                                            Int.toString prevCount)
                                )
                            |   NONE => () (* This was first. *)
                        (* = *)
                        (* We expect an equals sign here but a common problem is
                           to confuse fun declarations with matches and use a =>
                           here.  Report the error but swallow the =>. *)
                        val () =
                            if (sy lex) eq SYMBOLS.equalsSign then insymbol lex
                            else
                            (
                                notfound ("=", lex);
                                if (sy lex) eq SYMBOLS.thickArrow then insymbol lex else ()
                            );
                            (* expression *)
                        val (exp, expLoc)  = expression (fsys ++ verticalBar) newEnv;
                        val bind = mkClause (funPattern, exp, locSpan(varLoc, expLoc));
                        (* Followed by a vertical bar and another binding ? *)
                        val (rest, endLoc) = 
                            if testfor (SYMBOLS.verticalBar, empty, lex)
                            then bindings(SOME(funName, argCount))
                            else ([], expLoc)
                    in
                        (bind :: rest, locSpan(varLoc, endLoc))
                    end;
                    
                    val (bindings, bindLocns) = bindings NONE
                in
                    (mkClausal (bindings, bindLocns), bindLocns)
                end (* funB *);

                val (bindings, bindLocns) = andBindings(fsys, funB);
                val fullLocn = locSpan(startLocn, bindLocns)
            in
                (mkFunDeclaration (bindings,
                    {lookup= #lookup explicitTvars, apply= #apply explicitTvars},
                    {lookup= #lookup implicitTvars, apply= #apply implicitTvars},
                    fullLocn),
                fullLocn)
            end

            else if sym eq SYMBOLS.typeSy
            then
            let
                val () = insymbol lex;
                val (bindings, bindLocns) = andBindings(fsys, typeBinding(false, false, env))
                val fullLocn = locSpan(startLocn, bindLocns)
            in
                (mkTypeDeclaration (bindings, fullLocn), fullLocn)
            end

            else if sym eq SYMBOLS.datatypeSy
            then datatypeDecOrRepl(fsys, env, false, startLocn)

            else if sym eq SYMBOLS.abstypeSy
            then
            let
                val ()         = insymbol lex;
                val (tb, _) = 
                    andBindings (fsys ++ withTypeWithSy, datatypeBinding false);
                
                val (withtypes, _) = 
                   if testfor (SYMBOLS.withtypeSy, empty, lex)
                   then andBindings (fsys ++ withSy, typeBinding(false, false, env))
                   else ([], startLocn);
               
                val ()  = getsym (SYMBOLS.withSy, lex);
                val decs = decSequence (fsys ++ endSy, env);
                val fullLocn = locSpan(startLocn, location lex)
            in
                getsym (SYMBOLS.endSy, lex);
                (mkAbstypeDeclaration (tb, withtypes, decs, fullLocn), fullLocn)
            end

            else if sym eq SYMBOLS.exceptionSy
            then
            let
                (* Declares exception identifiers and their types. *)
                val () = insymbol lex;

                (* Get an exception binding. Qualified names prohibited. *)
                fun exceptionBinding fsys =
                let
                    (* Allow an "op" here but don't produce a warning if it's absent. *)
                    val () = if (sy lex) eq SYMBOLS.opSy then insymbol lex else ()
                    (* First the identifier. *)
                    val (iden, idLoc) = getShortId (variableSys, fsys ++ ofEqualsSignSy, lex);
                in
                    (* Either   excon of ty   or   excon = excon' *)
                    if testfor (SYMBOLS.ofSy, empty, lex)
                    then
                    let
                        val (theType, typeLocn) =
                            parseType (fsys ++ equalsSign, lex, {lookupTvar= #lookupTvar env})
                        val fullLoc = locSpan(idLoc, typeLocn)
                    in
                        (mkExBinding (iden, emptyTree, SOME theType, idLoc, fullLoc), fullLoc)
                    end
                    else if testfor (SYMBOLS.equalsSign, empty, lex)
                    then
                    let (* Must be   = excon' *)
                        (* Allow an "op" here but don't produce a warning if it's absent. *)
                        val () = if (sy lex) eq SYMBOLS.opSy then insymbol lex else ()
                        val (oldIden, oldIdenLoc) = getLongId (variableSys, fsys, lex);
                        val fullLoc = locSpan(idLoc, oldIdenLoc)
                    in
                        (mkExBinding (iden, mkIdent(oldIden, oldIdenLoc), NONE, idLoc, fullLoc), fullLoc)
                    end
                    else (mkExBinding (iden, emptyTree, NONE, idLoc, idLoc), idLoc)
                end;
                val (bindings, bindLocns) = andBindings(fsys, exceptionBinding)
                val fullLocn = locSpan(startLocn, bindLocns)
            in
                (mkExDeclaration (bindings, fullLocn), fullLocn)
            end

            else if sym eq SYMBOLS.localSy
            then let
                val ()      = insymbol lex;
                (* Infix status have this scope. Type-variables have the scope of the enclosing val or fun. *)
                val newEnv  = mkLocalFixEnv env
                (* The local declaration *)
                val ins     = decSequence (fsys ++ inEndSy, newEnv);
                val ()      = getsym (SYMBOLS.inSy, lex);
                (* Decs are added to both the local and surrounding environment. *)
                val resultEnv = mkLocalBodyFixEnv newEnv enterFix

                val body    = decSequence (fsys ++ endSy, resultEnv)
                
                val locs = locSpan(startLocn, location lex)
            in
                getsym (SYMBOLS.endSy, lex);
                (mkLocalDeclaration (ins, body, locs, true), (*"local" rather than "let"*) locs)
            end

            else if sym eq SYMBOLS.infixSy orelse
                    sym eq SYMBOLS.infixrSy orelse
                    sym eq SYMBOLS.nonfixSy
            then
            let
                val () = insymbol lex;

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
                                then errorMessage (lex, location lex,
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
                val () = skipon (variableSys, fsys, "Variable", lex);

                (* Read the variables and put them in the environ
                   with their fix status. Qualified names prohibited. *)
                fun vars endLoc =
                if (sy lex) inside variableSys
                then
                let
                    val (iden, idLoc) = getShortId (variableSys, fsys, lex);
                    val () = #enterFix env (iden, fixForm);
                    val (tail, endLoc) = vars idLoc
                in
                    (iden :: tail, endLoc)
                end
                else ([], endLoc);
                
                val (variables, endLoc) = vars startLocn
                val fullLocn = locSpan(startLocn, endLoc)
            in
                (mkDirective (variables, fixForm, fullLocn), fullLocn)
            end

            (* "open" declaration - qualified names allowed *)
            else if testfor (SYMBOLS.openSy, empty, lex)
            then
            let
                fun vars endLoc =
                if (sy lex) inside variableSys
                then
                let
                    val (id, idLoc) = getLongId (variableSys, fsys, lex);
                    val (tail, tailLoc) = vars idLoc
                in
                    (mkStructureIdent(id, idLoc) :: tail, tailLoc)
                end
                else ([], endLoc);
            in
                if (sy lex) inside variableSys
                then
                let
                    val (vars, varLocns) = vars startLocn
                in
                    (mkOpenTree(vars, varLocns), varLocns)
                end
                else (* Identifier missing. *)
                    (badsyms (SYMBOLS.ident, lex); (emptyTree, startLocn))
          end

            else (emptyTree, startLocn) (* Empty declaration. *)

        end

        else
        let (* Single expression allowed - short for  val it = exp *)
            val newTvars = makeTypeVarEnv();
            val explicitTvars = makeTypeVarEnv();(* This will always be empty. *)
            val newEnv   = {enterFix   = #enterFix  env,
                            lookupFix  = #lookupFix env,
                            lookupTvar = #lookupTvar newTvars};
            val (exp, expLoc) = expression fsys newEnv
        in
            (mkValDeclaration ([mkValBinding (mkIdent ("it", nullLocation), exp, false, expLoc)],
                {lookup= #lookup explicitTvars,apply= #apply explicitTvars},
                {lookup= #lookup newTvars,apply= #apply newTvars},
                expLoc), expLoc)
        end
    end (* dec *);

    (* Parses a signature. *)
    fun parseSignature (fsys : symset) (lex : lexan) env : sigs * location =
    let  (* May be either a signature name or a sig spec .. spec end seq
          followed by multiple  where type  expressions. *)
        val () = skipon (declarableVarSys ++ sigSy, fsys, "Start of signature", lex)

        val startLocn = location lex

        val sigexp : sigs * location =
            if testfor (SYMBOLS.sigSy, empty, lex)
            then
            let (* sig *)
                val sigs = signatureSpec (fsys ++ endSy ++ whereSy ++ semicolon) lex env
                val locs = locSpan(startLocn, location lex)
            in
                getsym (SYMBOLS.endSy, lex);
                (mkSig (sigs, locs), locs)
            end

            else if (sy lex) eq SYMBOLS.ident
            then
            let
                val ident as (_, locs) = getShortId (declarableVarSys, fsys ++ whereSy, lex)
            in
                (mkSigIdent ident, locs)
            end

            else (* Only if parse error which will have been reported in skipon. *)
                (mkSigIdent("error", location lex), location lex);

        fun getWhereTypes(sigexp, sigLoc) =
        let
            (* This is similar to a type binding but with the possibility
               that the type is a longtycon. *)
            val () = getsym(SYMBOLS.typeSy, lex);
            val newTVenv  = searchList ();
            val typeVars = getTypeVars (false, newTVenv);
            val (typeName, nameLoc)  = getLongId (ident, fsys ++ equalsSign, lex);
            val typeVarEnv = genTypeVarEnv newTVenv;
            val () = getsym (SYMBOLS.equalsSign, lex);
            (* Followed by a type or a sequence of constructors *)
            val () = skipon (startTypeSys, fsys, "type", lex);
            val (theType, typeLoc) = parseType (fsys ++ whereSy ++ andSy, lex, typeVarEnv)
            val constrainedSig =
                (mkWhereType(sigexp, typeVars, typeName, theType, nameLoc),
                 locSpan(sigLoc, typeLoc))
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
 and signatureSpec (fsys : symset) (lex : lexan) (env as {lookupTvar, ...}) : specs list =
 let
   val signatureTvars = makeTypeVarEnv();

   fun parseSigEntries () : specs list =
   let
       val () = skipon (fsys ++ semicolonStartSigSys, fsys, "Signature", lex)
       val sym = sy lex and startLocn = location lex
       val thisSig =
         if sym eq SYMBOLS.datatypeSy
         then
             let
               val startLocn = location lex
               val sys = fsys ++ startSigEndSy
               val newenv =
                   {enterFix = #enterFix env, lookupFix = #lookupFix env,
                (* All type variables on the right hand side of a datatype
                   specification must appear on the left. *)
                 lookupTvar =
                     fn name =>
                        (
                        errorMessage (lex, location lex, 
                                    name ^  " has not been declared in type declaration");
                        badType
                        )
                }
            in
                [mkCoreType (datatypeDecOrRepl(sys, newenv, true, startLocn))]
            end

         else if sym eq SYMBOLS.typeSy
         then
             (* It isn't obvious whether specifications of the form
                type s and t = int * int (i.e. mixed specifications and
                abbreviations) are allowed.  For the moment allow them. *)
            let
                val sys = fsys ++ startSigEndSy
                val () = insymbol lex;
                val (bindings, bindLocns) = andBindings(sys, typeBinding(true, false, env))
            in
                [mkCoreType (mkTypeDeclaration(bindings, bindLocns), locSpan(startLocn, bindLocns))]
            end

         else if sym eq SYMBOLS.eqtypeSy
         then
            let
                val sys = fsys ++ startSigEndSy
                val () = insymbol lex;
                val (bindings, bindLocns) = andBindings(sys, typeBinding(true, true, env))
            in
                [mkCoreType (mkTypeDeclaration(bindings, bindLocns), locSpan(startLocn, bindLocns))]
            end

         else if sym eq SYMBOLS.valSy
           then
           let
                val () = insymbol lex
             
                fun doVal () =
                let
                    val idAndLoc as (_, idLoc)  = getShortId (declarableVarSys, fsys ++ colon, lex);
                    val () = getsym (SYMBOLS.colon, lex);
                    val (ty, tyLoc)  = 
                        parseType (fsys ++ startSigEndAndSy, lex, 
                            {lookupTvar = #lookupTvar signatureTvars});
                    val locs = locSpan(idLoc, tyLoc)
               in
                    (mkValSig (idAndLoc, ty, locs), locs)
               end
           in
             #1 (getList (SYMBOLS.andSy, empty, lex, doVal))
           end (* val *)

         else if sym eq SYMBOLS.exceptionSy
            then
            let(* exception id1 of ty1 and _ and idn of tyn *)
                val () = insymbol lex
             
                fun doEx () =
                let
                    val idAndLoc as (_, idLoc)  = getShortId (variableSys, fsys ++ ofSy, lex)
                    val (ty, locs) =
                        if testfor (SYMBOLS.ofSy, empty, lex)
                        then
                            let
                                val (types, tyLoc) =
                                    parseType (fsys ++ startSigEndAndSy, lex, {lookupTvar = lookupTvar})
                            in
                                (SOME types, locSpan(idLoc, tyLoc))
                            end
                        else (* Nullary *) (NONE, idLoc);
               in
                    (mkExSig (idAndLoc, ty, locs), locs)
               end
            in
                #1 (getList (SYMBOLS.andSy, empty, lex, doEx))
            end (* exception *)
           
         else if sym eq SYMBOLS.structureSy
            then
            let
                val () = insymbol lex

                fun doStructure () =
                let
                    val idAndLoc as (_, idLoc)  = getShortId (variableSys, empty, lex)
                    val () = getsym (SYMBOLS.colon, lex)
                    val (sgn, sgnLoc) = parseSignature (fsys ++ startSigEndAndSy) lex env
                    val locs = locSpan(idLoc, sgnLoc)
                in
                    (mkStructureSigBinding (idAndLoc, (sgn, false, sgnLoc), locs), locs)
                end
            in
                [mkStructureSig(getList(SYMBOLS.andSy, empty, lex, doStructure))]
            end

         else if sym eq SYMBOLS.includeSy
            then
            let
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
                val () = insymbol lex
                val () =
                    skipon (declarableVarSys ++ sigSy, fsys, "Start of signature", lex)

                val (firstSig, firstLoc) =
                    parseSignature (fsys ++ startSigEndSy ++ declarableVarSys) lex env

                fun sigids locs =
                if (sy lex) eq SYMBOLS.ident
                then
                let
                    val nameLoc as (_, loc) = getShortId (declarableVarSys, fsys, lex)
                    val (rest, lastLoc) = sigids loc
                in
                    (mkSigIdent nameLoc :: rest, lastLoc)
                end
                else ([], locs)

                val (otherSigs, finalLoc) = sigids firstLoc
            in
                [mkInclude (firstSig :: otherSigs, locSpan(startLocn, finalLoc))]
            end

         else if sym eq SYMBOLS.sharingSy
         then
            let (* sharing *)
                val startLocn = location lex
                val () = insymbol lex
                val isType = testfor (SYMBOLS.typeSy, empty, lex)
                fun getShare () =
                let
                    val (id, loc) =  getLongId (declarableVarSys, fsys ++ rightParenEqualsSignSy, lex)
                in
                    (* We want to include the location in the list as well as in the result here. *)
                    ((id, loc), loc)
                end
                val (shares, _) = getShare ()
                val () = getsym (SYMBOLS.equalsSign, lex)
                val (shareRest, shareLocn) = getList (SYMBOLS.equalsSign, ident, lex, getShare)
                val fullLoc = locSpan(startLocn, shareLocn)
                val share = mkSharing (isType, shares :: shareRest, fullLoc)
            in
                [share]
            end

        else [] (* Empty. *)
           (* end of parse of thisSig *)
             
        (* continue until the `end' *)
        val () = if (sy lex) eq SYMBOLS.semicolon then insymbol lex else ();
    in 
        if (sy lex) inside semicolonStartSigSys
        then thisSig @ parseSigEntries ()
        else thisSig
    end (* parseSigEntries *)
  in
    parseSigEntries ()
  end (* signatureSpec *);


    fun signatureDec (fsys : symset) (lex : lexan) env : topdec =
    let
        val startLocn = location lex
        val () = insymbol lex
        fun doSigDec () =
        let
            val idAndLoc as (_, idLoc) = getShortId (variableSys, empty, lex);
            val () = getsym (SYMBOLS.equalsSign, lex)
            val (sgn, sigLoc) = parseSignature (fsys ++ endAndSy) lex env
            val locs = locSpan(idLoc, sigLoc)
        in
            (mkSignatureBinding (idAndLoc, sgn, locs), locs)
        end
        
        val (sigs, sigLoc) = getList (SYMBOLS.andSy, empty, lex, doSigDec)
    in
        mkSignatureDec (sigs, locSpan(startLocn, sigLoc))
    end
       
    
  fun structVal (fsys : symset) (lex : lexan) env : structValue * location =
  let
      (* Series of declarations inside struct...end or (...) in functor
         application. *)

    val () = skipon (structSy ++ declarableVarLetSy,
                      fsys, "struct or functor application", lex);
    
    val fsysPcolon = fsys ++ colon ++ colonGt
    val startLocn = location lex

    val strExp =
        if testfor (SYMBOLS.structSy, empty, lex)
        then
        let(* It's a new structure *)
            (* Infix declarations are local to struct ... end. *)
            val structEnv = mkLocalFixEnv env
            val str = strDec (fsysPcolon ++ endSy) lex structEnv
            val locs = locSpan(startLocn, location lex)
            val () = getsym (SYMBOLS.endSy, lex)
        in
            (mkStruct(str, locs), locs)
        end
    
        else if testfor (SYMBOLS.letSy, empty, lex)
        then let
         (* Fixity is local. *)
          val newEnv = mkLocalFixEnv env
          (* The local declaration *)
          val ins  = strDec (fsysPcolon ++ inEndSy) lex newEnv
          val () = getsym (SYMBOLS.inSy, lex)
          val body = #1 (structVal (fsysPcolon ++ endSy) lex newEnv)
          val endLoc = location lex
          val () = getsym (SYMBOLS.endSy, lex)
          val locs = locSpan(startLocn, endLoc)
        in
          (mkLetdec (ins, body, locs), locs)
        end
    
        else
        let (* Either a structure path or a functor application *)
            val (iden, idLoc) = getLongId (declarableVarSys, fsysPcolon ++ leftParen, lex);
            val startLoc = location lex
        in
            if testfor (SYMBOLS.leftParen, empty, lex)
            then
            let (* functor application *)
                (* Functor names must not be qualified. *)
                val () = checkForDots (iden, lex, idLoc);
                val parameter =
                    if (sy lex) eq SYMBOLS.rightParen (* Empty parameter list *)
                    then mkStruct([], locSpan(startLoc, location lex))
                    else
                    let
                        val tsys = fsysPcolon ++ rightParenCommaSy
                    in
                        (* May be either a structure value or a sequence
                           of declarations. *)
                        if (sy lex) inside startDecStructureSy
                        then (* implied struct...end *)
                        let
                            val structEnv = mkLocalFixEnv env
                            val str = strDec tsys lex structEnv
                            val locs = locSpan(startLoc, location lex)
                        in
                            mkStruct(str, locs)
                        end
                        else #1 (structVal tsys lex env)
                    end
                val endPos = location lex
                val () = getsym (SYMBOLS.rightParen, lex)
                val locs = locSpan(idLoc, endPos)
            in
                (mkFunctorAppl (iden, parameter, idLoc, locs), locs)
            end
            else (mkStructIdent (iden, idLoc), idLoc)
        end

        (* We may have one or more constraints. *)
        fun doConstraints (strExp, strExpLoc) =
        let
            val isOpaque = (sy lex) eq SYMBOLS.colonGt;
        in
            if testfor (SYMBOLS.colon, empty, lex) orelse testfor (SYMBOLS.colonGt, empty, lex)
            then
            let
                val (sign, sigLoc) = parseSignature fsysPcolon lex env
            in
                doConstraints(mkSigConstraint(strExp, sign, isOpaque, sigLoc), locSpan(strExpLoc, sigLoc))
            end
            else (strExp, strExpLoc)
        end
  in
      doConstraints strExp
  end (* structVal *)

    and structureDec (fsys : symset) (lex : lexan) (env as {enterFix, ...}) : structDec =
    let
        val startLocn = location lex
    in
        if testfor (SYMBOLS.structureSy, empty, lex)
        then
        let
            fun doStrDec () =
            let (* Read strId <<: sig>> = str *)
                (* First the identifier *)
                val idAndLoc as (_, idLoc) = getShortId (declarableVarSys, fsys ++ colonEqualsSignSy, lex);
                (* Next the signature if there is one. *)
                val isOpaque = (sy lex) eq SYMBOLS.colonGt;
                val sgn =
                    if testfor (SYMBOLS.colon, empty, lex) orelse testfor (SYMBOLS.colonGt, empty, lex)
                    then
                        let
                            val (sign, sigLoc) = parseSignature (fsys ++ equalsSign) lex env
                        in
                            SOME (sign, isOpaque, sigLoc)
                        end
                    else NONE
                (* Now the equals sign *)
                val () = getsym (SYMBOLS.equalsSign, lex)
                val (strVal, strLoc) = structVal fsys lex env
                val locs = locSpan(idLoc, strLoc)
            in
                (* And finally the structure value. *)
                (mkStructureBinding (idAndLoc, sgn, strVal, locs), locs)
            end
            val (strs, strLocs) = getList (SYMBOLS.andSy, structSy, lex, doStrDec)
        in  
            mkStructureDec (strs, locSpan(startLocn, strLocs))
        end

        else 
        let
            val () = getsym (SYMBOLS.localSy, lex)
            val startLoc = location lex
            val newEnv  = mkLocalFixEnv env
            (* The local declaration *)
            val ins = strDec (fsys ++ inEndSy) lex newEnv
            val () = getsym (SYMBOLS.inSy, lex)
            (* Decs are added to both the local and surrounding environment. *)
            val resultEnv = mkLocalBodyFixEnv newEnv enterFix
            val body = strDec (fsys ++ endSy) lex resultEnv
            val endLoc = location lex
            val () = getsym (SYMBOLS.endSy, lex)
        in
            mkLocaldec (ins, body, locSpan(startLoc, endLoc))
        end

    end (* end of structureDec *)

    (* Functor declarations. *)
    and functorDec (fsys : symset) (lex : lexan) env : topdec =
    let
        val startLocn = location lex
        val () = insymbol lex; (* remove ``functor'' *)

        fun doFunctDec () : functorBind * location =
        let (* Read fncId (<<paramSpec>> ) <<: sig>> = str *)
            (* First the identifier *)
            val (strId, idLocn) = getShortId (declarableVarSys, fsys ++ colonEqualsSignSy, lex);
            val () = getsym (SYMBOLS.leftParen, lex);
            (* Now the parameters *)
            val tsys = fsys ++ rightParenCommaSy;

            val parameter = (* empty | name:sigexp | spec *)
                if (sy lex) eq SYMBOLS.rightParen
                    (* empty *)
                then mkFormalArg ("", mkSig([], location lex))
            
                else if (sy lex) inside startSigSys
                (* spec *)
                then
                let
                    val startLocn = location lex
                    val sigs = signatureSpec tsys lex env
                in
                    mkFormalArg ("", mkSig (sigs, locSpan(startLocn, location lex)))
                end
         
                (* name : sigexp *)
                else
                let
                    val (strId, _) = getShortId (declarableVarSys, tsys ++ colon, lex);
                    val () = getsym (SYMBOLS.colon, lex)
 
                    (* Next the signature. *)
                    val (sgn, _) = parseSignature (tsys ++ sharingSy) lex env
                in
                    mkFormalArg (strId, sgn)
                end (* parameter *)

            val () = getsym (SYMBOLS.rightParen, lex)
       
            (* Next the signature if there is one. *)
            val isOpaque = (sy lex) eq SYMBOLS.colonGt;
       
            val sigOpt =
                if testfor (SYMBOLS.colon, empty, lex) orelse testfor (SYMBOLS.colonGt, empty, lex)
                then
                let
                    val (sign, sigLoc) = parseSignature (fsys ++ equalsSign) lex env
                in
                    SOME(sign, isOpaque, sigLoc)
                end
                else NONE
           
            (* Now the equals sign *)
            val () = getsym (SYMBOLS.equalsSign, lex)
            (* And finally the functor value. *)
            val (strVal, strLoc) = structVal fsys lex env
            val locs = locSpan(idLocn, strLoc)
        in
            (mkFunctorBinding (strId, idLocn, sigOpt, strVal, parameter, locs), locs)
        end (* doFunctDec *)
        
        val (functs, functLoc) = getList (SYMBOLS.andSy, structSy, lex, doFunctDec)
    in
        mkFunctorDec (functs, locSpan(startLocn, functLoc))
    end (* functorDec *)


    and strDec (fsys : symset) (lex : lexan) env : structDec list =
    (* A sequence of declarations, optionally separated by semicolons. *)
    let
        fun getDecs () : structDec list =
        let 
            val tsys = fsys ++ semicolonStartDecStructureSy;
        in
            (* Semicolons are optional. *)
            if (sy lex) eq SYMBOLS.semicolon
            then
            let
                val () = insymbol lex
            in 
                getDecs ()
            end
            else if (sy lex) inside structureLocalSy
            then (structureDec tsys lex env) :: getDecs()
            else if (sy lex) inside startDecSys
            then (mkCoreLang (dec(tsys, lex, true, env))) :: getDecs()
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
        fn _ => 
        let
          val () =
            errorMessage (lex, location lex, "Free type variables not allowed");
        in
          makeTv {value=emptyType, level=generalisable, equality=false, nonunifiable=true, printable=false}
        end}

    (* May be structure/functor dec, signature dec or top-level dec. Treat
     "local" as a structure dec even if it is actually declaring a value
     or type. *)
    val tsys = fsys ++ startTopSys;
         
    fun parseTopDecs (_ : symset) : topdec list * location =
    let
        val startSym = sy lex and startLoc = location lex;
    in
        if startSym eq SYMBOLS.semicolon orelse
           startSym eq SYMBOLS.abortParse
        then ([], startLoc)
    
        else if startSym inside topdecStartSy
        then
        let
            val (aDec : topdec, newOkStartSet : symset) =
                if startSym eq SYMBOLS.functorSy
                then (functorDec tsys lex globalEnv, functorSy)
          
                else if startSym eq SYMBOLS.signatureSy
                then (signatureDec tsys lex globalEnv, signatureSy)
          
                else if startSym eq SYMBOLS.structureSy
                then (mkTopDec(structureDec tsys lex globalEnv), structureLocalStartDecSy)
          
                (* Local declarations are ambiguous; we treat them as strDecs *)
                else if startSym eq SYMBOLS.localSy
                then (mkTopDec(structureDec tsys lex globalEnv), structureLocalStartDecSy)
          
                (* let, val, fun etc. *)
                else (mkTopDec(mkCoreLang (dec(tsys, lex, true, globalEnv))), structureLocalStartDecSy);

            val (rest, locRest) = parseTopDecs newOkStartSet
        in
            (aDec :: rest, locSpan(startLoc, locRest))
        end
     
        else (notfound (";", lex); ([], startLoc))
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
    then mkProgram(parseTopDecs topdecStartSy)
  
    else
    let (* expression - only one allowed. *)
        val startLoc = location lex;
        val aDec = mkCoreLang (dec(tsys, lex, false, globalEnv));
        val () = 
            if (sy lex) neq SYMBOLS.semicolon andalso
              (sy lex) neq SYMBOLS.abortParse
            then notfound (";", lex)
            else ()
    in
        mkProgram([mkTopDec aDec], locSpan(startLoc, location lex))
    end
end; (* parseDec *)

end (* PARSEDEC *);

