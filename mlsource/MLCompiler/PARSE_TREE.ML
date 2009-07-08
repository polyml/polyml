(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further development:
    Copyright (c) 2000-9 David C.J. Matthews

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
    Title:      Parse Tree Structure and Operations.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

functor PARSE_TREE (

structure LEX : LEXSIG
structure CODETREE : CODETREESIG
structure STRUCTVALS : STRUCTVALSIG;
structure EXPORTTREE: EXPORTTREESIG;
structure TYPETREE : TYPETREESIG
structure VALUEOPS : VALUEOPSSIG;
structure PRETTY : PRETTYSIG
structure DEBUGGER : DEBUGGERSIG
structure COPIER: COPIERSIG
structure TYPEIDCODE: TYPEIDCODESIG

structure UTILITIES :
sig
  type lexan;
  type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }

  val noDuplicates: (string * 'a * 'a -> unit) -> 
                       { apply: (string * 'a -> unit) -> unit,
                         enter:  string * 'a -> unit,
                         lookup: string -> 'a option};
    
  val searchList: unit -> { apply: (string * 'a -> unit) -> unit,
                            enter:  string * 'a -> unit,
                            lookup: string -> 'a option };

  val checkForDots:  string * lexan * location -> unit;

  val splitString: string -> { first:string,second:string }

    structure Sharing:
    sig
        type lexan = lexan
    end
end;

structure UNIVERSALTABLE:
sig
  type universal = Universal.universal;
  type univTable;
  
  val univFold: univTable * (string * universal * 'a -> 'a) * 'a -> 'a;
end;

structure DEBUG :
sig
    val debugTag: bool Universal.tag
    val errorDepthTag : int Universal.tag
    val fileNameTag: string Universal.tag
    val getParameter :
           'a Universal.tag -> Universal.universal list -> 'a 
end;

structure MISC :
sig
  (* These are handled in the compiler *)
  exception Conversion of string;     (* string to int conversion failure *)
  
  (* This isn't handled at all (except generically) *)
  exception InternalError of string; (* compiler error *)

  val quickSort : ('a -> 'a -> bool) -> 'a list -> 'a list;
  
  val lookupDefault :  ('a -> 'b option) -> ('a -> 'b option) -> 'a -> 'b option
end (* MISC *);

structure ADDRESS :
sig
  type machineWord;    (* any legal bit-pattern (tag = 0 or 1) *)
  val toMachineWord: 'a -> machineWord
end;

(*****************************************************************************)
(*                  PARSETREE sharing constraints                            *)
(*****************************************************************************)

sharing LEX.Sharing = TYPETREE.Sharing = STRUCTVALS.Sharing = COPIER.Sharing
       = VALUEOPS.Sharing = EXPORTTREE.Sharing = UTILITIES.Sharing
       = DEBUGGER.Sharing = PRETTY.Sharing = TYPEIDCODE.Sharing
       = ADDRESS = CODETREE.Sharing

) : PARSETREESIG =
   
(*****************************************************************************)
(*                  PARSETREE functor body                                   *)
(*****************************************************************************)
struct 
  open MISC;
  open LEX;
  open CODETREE;
  open STRUCTVALS;
  open TYPETREE;
  open VALUEOPS;
  open UTILITIES;
  open DEBUG;
  open UNIVERSALTABLE;
  open Universal;
  open PRETTY;
  open ADDRESS;
  open EXPORTTREE
  open TYPEIDCODE
  open RuntimeCalls; (* for POLY_SYS numbers *)
  
  val ioOp : int -> machineWord = RunCall.run_call1 POLY_SYS_io_operation;
          
  infix 9 sub;
  
 (********* types constructors copied here to reduce garbage ***************)
  val emptyType            = EmptyType;
  val badType              = BadType;

 (************************************************************)
  
  val makeRaise = CODETREE.mkRaise; (* to avoid confusion! *)
  val makeWhile = CODETREE.mkWhile; (* to avoid confusion! *)

    datatype parsetree = 
        Ident               of
      (* An identifier is just a name. In the second pass it is associated
         with a particular declaration and the type is assigned into the
         type field. The type of this identifier is needed to deal with
         overloaded operators. If we have an occurence of ``='', say, the
         type of the value will be 'a * 'a -> bool but the type of a particular
         occurence, i.e. the type of the identifier must be int * int -> bool,
         say, after all the unification has been done. *)
        { name: string, expType: types ref, value: values ref, location: location }

    |   Literal             of
           (* Literal constants may be overloaded on more than one type. The
              types are specified by installing appropriate conversion functions:
              convInt, convReal, convChar, convString and convWord. *)
            { converter: values, expType: types ref, literal: string, location: location }

    |   Applic              of
            (* Function application *)
            { f: parsetree, arg: parsetree, location: location, isInfix: bool, expType: types ref }

    |   Cond                of
            (* Conditional *)
            { test: parsetree, thenpt: parsetree, elsept: parsetree, location: location } 

    |   TupleTree           of parsetree list * location

    |   ValDeclaration      of valDecForm

    |   FunDeclaration      of funDecForm

    |   OpenDec             of
            (* Open a structure. *)
            { decs: structureIdentForm list, variables: values list ref, location: location }

    |   Constraint          of
           (* Constraint (explicit type given) *)
           (* A constraint has a value and a type. The actual type, will, however
              be the unification of these two and not necessarily the given type. *)
            { value: parsetree, given: typeParsetree, location: location }

    |   Layered             of
          (* Layered pattern. Equivalent to an ordinary pattern except that the
             variable is given the name of the object which is to be matched. *)
            { var: parsetree, pattern: parsetree, location: location }

    |   Fn                  of parsetree list * location
    |   MatchTree           of matchForm
    |   Localdec            of localdecForm
    |   TypeDeclaration     of typebind list * location
    |   AbstypeDeclaration  of abstypeDeclarationForm
    |   DatatypeDeclaration of abstypeDeclarationForm
    |   DatatypeReplication of datatypeReplicationForm
    |   ExpSeq              of parsetree list * location

    |   Directive           of
            (* Directives are infix, infixr and nonfix. They are processed by the
               parser itself and only appear in the parse tree for completeness. *)
            { tlist: string list, fix: fixStatus, location: location } 

    |   ExDeclaration       of exbind list * location

    |   Raise               of parsetree * location

    |   HandleTree          of
            (* Execute an expression and catch any exceptions. *)
            { exp: parsetree, hrules: parsetree list, location: location }

    |   While               of
            (* Ordinary while-loop *)
            { test: parsetree, body: parsetree, location: location } 

    |   Case                of
            (* Case-statement *)
            { test: parsetree, match: parsetree list, location: location }

    |   Andalso             of { first: parsetree, second: parsetree, location: location } 

    |   Orelse              of { first: parsetree, second: parsetree, location: location }

    |   Labelled            of
        (* Labelled record & the entry in the list. "frozen" is false if it's
           a pattern with "...". *)
            { recList: labelRecEntry list, frozen: bool, expType: types ref, location: location }

    |   Selector            of
            { name: string, labType: types, typeof: types, location: location }

    |   List                of
            { elements: parsetree list, location: location, expType: types ref }
    |   EmptyTree
    |   WildCard            of location
    |   Unit                of location
    |   Parenthesised       of parsetree * location
   
   and valbind = (* Value bindings are either a binding or a list
                       of recursive bindings.*)
         ValBind of (* Consists of a declaration part (pattern) and an expression. *)
             {
               dec: parsetree,
               exp: parsetree,
               line: location
             } 
        | RecValBind
    
   and fvalbind = (* Function binding *)
   (* `Fun' bindings *)
      (* A function binding is a list of clauses, each of which uses a
         valBinding to hold the list of patterns and the corresponding function
         body. The second pass extracts the function variable and the number of
         patterns in each clause. It checks that they are the same in each
         clause. *)
       FValBind of
         {
           clauses:     fvalclause list, 
           numOfPatts:  int ref,
           functVar:    values ref,
           argType:     types ref,
           resultType:  types ref,
           location:    location
         }

    and fvalclause = (* Clause within a function binding. *)
        FValClause of { dec: funpattern, exp: parsetree, line: location }
        
    and typebind = (* Non-generative type binding *)
        TypeBind of
         {
           name: string,
           typeVars: typeVarForm list,
           decType: typeParsetree option,
           isEqtype: bool, (* True if this was an eqtype in a signature. *)
           nameLoc:  location,
           fullLoc:  location
         } 

    and datatypebind = (* Generative type binding *)
        DatatypeBind of
         {
           name:          string,
           typeVars:      typeVarForm list,
           constrs:       {constrName: string, constrArg: typeParsetree option, idLocn: location} list,
           tcon:          typeConstrs ref,
           valueConstrs:  values list ref,
           nameLoc:       location,
           fullLoc:  location
         }

   and exbind = (* An exception declaration. It has a name and
                   optionally a previous exception and a type. *)
        ExBind of
         {
           name:         string,
           previous:     parsetree,
           ofType:       typeParsetree option,
           value:        values ref,
           nameLoc:      location,
           fullLoc:      location
         } 

       (* Val and fun declarations. *)
   withtype valDecForm = 
     {
       dec:    valbind list,
       explicit: {lookup: string -> typeVarForm option,
               apply: (string * typeVarForm -> unit) -> unit },
       implicit: {lookup: string -> typeVarForm option,
               apply: (string * typeVarForm -> unit) -> unit },
       variables: values list ref, (* list of variables declared *)
       location: location
     } 

   and funDecForm = 
     {
       dec:    fvalbind list,
       explicit: {lookup: string -> typeVarForm option,
               apply: (string * typeVarForm -> unit) -> unit },
       implicit: {lookup: string -> typeVarForm option,
               apply: (string * typeVarForm -> unit) -> unit },
       location: location
     } 

       (* Name of a structure. Used only in an ``open'' declaration. *)
   and structureIdentForm = 
     {
       name:   string,
       value:  structVals ref,
       location: location
     } 

   (* A match is a pattern and an expression. If the pattern matches then
     the expression is evaluated in the environment of the pattern. *)
   and matchForm = 
     {
       vars: parsetree,
       exp: parsetree,
       location: location,
       argType: types ref,
       resType: types ref
     } 

    (* Used for local dec in dec and let dec in exp. *)
   and localdecForm = 
     {
       decs: parsetree  list,
       body: parsetree list,
       isLocal: bool,
       varsInBody: values list ref, (* Variables in the in..dec part
                                      of a local declaration. *)
       location: location
     } 

   (* Datatype and Abstract Type declarations *)
   and abstypeDeclarationForm = 
     {
       typelist:  datatypebind list,
       withtypes: typebind list,
       declist:   parsetree list,
       location:  location
     }
     
   and datatypeReplicationForm =
        {
         newType:  string,
         oldType:  string,
         oldLoc:   location,
         newLoc:   location,
         location: location
        }

    (* An entry in a label record in an expression or a pattern. *)
    and labelRecEntry =
    {
        name: string,
        nameLoc: location,
        valOrPat: parsetree,
        fullLocation: location,
        expType: types ref
    }
    
    and funpattern = (* The declaration part of a fun binding. *)
        { ident: { name: string, expType: types ref, location: location },
          isInfix: bool, args: parsetree list, constraint: typeParsetree option }

  (*****************************************************************************
              Pretty Printing
  ******************************************************************************)
  
    fun isIdent               (Ident _)               = true | isIdent _               = false;

    fun isEmptyTree           EmptyTree               = true | isEmptyTree _           = false;
  
    val unit      = Unit;
    val wildCard  = WildCard;
    val emptyTree = EmptyTree;

    (* A general type variable for an expression.  This is used to record the type. *)
    fun makeGeneralTypeVar() = mkTypeVar(generalisable, false, false)
  
    fun mkIdent (name, loc) : parsetree = 
      Ident
        {
          name   = name,
          expType = ref EmptyType,
          value  = ref undefinedValue,
          location = loc
        };
    
    local    
       (* Make overloaded functions for the conversions. *)
       (* For the moment we make the type string->t and raise an exception
          if the constant cannot be converted. *)
       val ty      = mkOverloadSet[]
       val funType = mkFunctionType (stringType, ty);
       fun mkOverloaded name : values = makeOverloaded (name, funType, TypeDep)
    in
        val convString = mkOverloaded "convString"
        and convInt = mkOverloaded "convInt"
        and convWord = mkOverloaded "convWord"
        and convChar = mkOverloaded "convChar"
        and convReal = mkOverloaded "convReal"
    end;

    fun mkString(s: string, loc): parsetree =
        Literal{converter=convString, literal=s, expType=ref EmptyType, location=loc};
    
    fun mkInt  (i : string, loc) : parsetree =
        Literal{converter=convInt, literal=i, expType=ref EmptyType, location=loc};
    
    fun mkReal (r : string, loc) : parsetree =
        Literal{converter=convReal, literal=r, expType=ref EmptyType, location=loc};
    
    fun mkChar (c : string, loc) : parsetree = 
        Literal{converter=convChar, literal=c, expType=ref EmptyType, location=loc};

    fun mkWord (w : string, loc) : parsetree =
        Literal{converter=convWord, literal=w, expType=ref EmptyType, location=loc};
    
    fun mkApplic (f, arg, loc, isInfix) : parsetree  =
      Applic
        {
          f   = f,
          arg = arg,
          location = loc,
          isInfix = isInfix,
          expType = ref EmptyType
        };
    
    fun mkCond (test, thenpt, elsept, location) : parsetree  = 
      Cond  
       { test   = test,
         thenpt = thenpt,
         elsept = elsept,
         location = location
       };
       
    val mkTupleTree : parsetree list * location -> parsetree = TupleTree;
    
    fun mkValDeclaration (dec, explicit, implicit, location) : parsetree = 
      ValDeclaration 
        {
            dec   = dec,
            explicit = explicit,
            implicit = implicit,
            variables = ref [],
            location = location
        };
    
    fun mkFunDeclaration (dec, explicit, implicit, location) : parsetree =
      FunDeclaration
        {
            dec=dec,
            explicit = explicit,
            implicit = implicit,
            location = location
        };
    
    fun mkOpenTree(ptl : structureIdentForm list, location): parsetree =
        OpenDec{decs=ptl, variables=ref [], location = location};
    
    fun mkStructureIdent (name, location) : structureIdentForm =
        { 
          name  = name,
          value = ref undefinedStruct,
          location = location
        }; 
 
    fun mkValBinding (dec, exp, line) : valbind = 
      ValBind
        {
          dec  = dec,
          exp  = exp,
          line = line
        };

    val recValbind = RecValBind;

    fun mkClausal(clauses, location) : fvalbind =
       FValBind
         { 
           clauses    = clauses,
           numOfPatts = ref 0,
           functVar   = ref undefinedValue,
           argType    = ref badType,
           resultType = ref badType,
           location   = location
         }; 

    (* A clause for a clausal function is initially parsed as a pattern because that is
       the easiest way to handle it but that's actually more general than the syntax allows.
       Process it at this point to check for some validity. *)
    fun mkFunPattern (fPat, lex): funpattern * string * int =
    let
        fun makeId(name, loc) =
            {name = name, expType = ref EmptyType, location = loc }

        fun unpick (Applic{ f, arg, location, isInfix, ... }) =
                (* "Application" of function to a parameter. *)
            let
                val () =
                (* This could be an infixed application and since it has been parsed using the
                   normal infix handler the arguments could be prefixed constructor applications
                   or infixed constructor applications with a higher precedence.  These are not
                   allowed because the arguments are supposed to just be "atpats".  Any
                   applications should have been parenthesised. *)
                    case (isInfix, arg) of
                        (true, TupleTree([Applic _, _], location)) =>
                            errorMessage(lex, location,
                                "Constructor applications in fun bindings must be parenthesised.")
                    |   (true, TupleTree([_, Applic _], location)) =>
                            errorMessage(lex, location,
                                "Constructor applications in fun bindings must be parenthesised.")
                    |   _ => ();
                val { ident, isInfix, args, ... } = unpick f
            in
                { ident=ident, isInfix=isInfix, args = args @ [arg], constraint = NONE }
            end

        |   unpick (Ident{ name, location, ...}) =
            {
                ident={ name = name, location = location, expType = ref EmptyType},
                isInfix=false, args = [], constraint = NONE
            }

        |   unpick (Parenthesised(p as Applic{ f = Ident { name, location, ...}, isInfix=true, arg, ... }, _)) =
            {
                ident={ name = name, location = location, expType = ref EmptyType},
                isInfix=true, args = [arg], constraint = NONE
            }

        |   unpick (Parenthesised(_, location)) =
                (* Only the bottom (i.e. first) application may be parenthesised and then
                   only if the application is infixed. *)
                (
                    errorMessage(lex, location,
                        "Parentheses are only allowed for infixed applications in fun bindings.");
                    { ident=makeId("", location), isInfix=false, args = [], constraint = NONE }
                )

        |   unpick _ =
                (
                    errorMessage(lex, location lex,
                        "Syntax error: fun binding is not an identifier applied to one or more patterns.");
                    { ident=makeId("", location lex), isInfix=false, args = [], constraint = NONE }
                )

        val unpicked as { ident = { name, ...}, args, ...} =
            (* The "pattern" may have a single constraint giving the result
               type of the function.  Otherwise it must be a set of one or more,
               possibly infixed, applications. *)
            case fPat of
                Constraint { value = value as Applic _, given, ... } =>
                let
                    val { ident, isInfix, args, ... } = unpick value
                in
                    { ident = ident, isInfix = isInfix, args = args, constraint = SOME given }
                end

            |   Constraint { value = value as Parenthesised(Applic _, _), given, ... } =>
                let
                    val { ident, isInfix, args, ... } = unpick value
                in
                    { ident = ident, isInfix = isInfix, args = args, constraint = SOME given }
                end

            |   fPat as Parenthesised(Applic _, _) =>
                    unpick fPat

            |   fPat as Applic _ =>
                    unpick fPat

            |   _ =>
                (
                    errorMessage(lex, location lex,
                        "Syntax error: fun binding is not an identifier applied to one or more patterns.");
                    { ident=makeId("", location lex), isInfix=false, args = [], constraint = NONE }
                )
    in
        (unpicked, name, List.length args)
    end;

    fun mkClause (dec, exp, line) : fvalclause =
        FValClause
        {
          dec  = dec,
          exp  = exp,
          line = line
        }

    fun mkList(elem, loc) = List{ elements = elem, location = loc, expType = ref EmptyType }
    
    fun mkConstraint (value, given, location) : parsetree = 
      Constraint 
        { 
          value = value,
          given = given,
          location = location
        };
      
    fun mkLayered (var, pattern, location) : parsetree = 
      Layered
        {
          var     = var,
          pattern = pattern,
          location = location
        };
    
    val mkFn : parsetree list * location -> parsetree = Fn;
    
    fun mkMatchTree (vars, exp, location) : parsetree = 
      MatchTree 
        {
          vars = vars,
          exp  = exp,
          location = location,
          argType = ref badType,
          resType = ref badType
        };
  
    fun mkLocalDeclaration (decs, body, location, isLocal) : parsetree =
      Localdec 
        {
          decs = decs,
          body = body,
          isLocal  = isLocal,
          varsInBody = ref [],
          location = location
        };
      
    val mkTypeDeclaration : typebind list * location -> parsetree = TypeDeclaration;

    fun mkDatatypeDeclaration (typelist, withtypes, location) : parsetree =
     DatatypeDeclaration
       {
            typelist  = typelist,
            withtypes = withtypes,
            declist   = [],
            location  = location
       };
    
    fun mkAbstypeDeclaration (typelist, withtypes, declist, location) : parsetree =
      AbstypeDeclaration
        {
            typelist  = typelist,
            withtypes = withtypes,
            declist   = declist,
            location  = location
        };

    val mkDatatypeReplication = DatatypeReplication
    
    fun mkTypeBinding (name, typeVars, decType, isEqtype, nameLoc, fullLoc) : typebind =
      TypeBind 
        {
          name     = name,
          typeVars = typeVars,
          decType  = decType,
          isEqtype = isEqtype,
          nameLoc = nameLoc,
          fullLoc = fullLoc
        };
    
    fun mkDatatypeBinding (name, typeVars, constrs, typeNameLoc, fullLoc) : datatypebind =
      DatatypeBind
        {
          name         = name,
          typeVars     = typeVars,
          constrs      = constrs,
          tcon         = ref undefType,
          valueConstrs = ref [],
          nameLoc      = typeNameLoc,
          fullLoc = fullLoc
        };
   
    fun mkExBinding (name, previous, typeof, nameLoc, fullLoc) : exbind =
      ExBind 
        {
          name        = name,
          previous    = previous,
          ofType      = typeof,
          value       = ref undefinedValue,
          nameLoc     = nameLoc,
          fullLoc     = fullLoc
        };

    fun mkLabelledTree (recList, frozen, location) : parsetree = 
     Labelled
       {
         recList = recList,
         frozen  = frozen,
         expType  = ref EmptyType,
         location = location
       };
       
    fun mkLabelRecEntry (name, nameLoc, valOrPat, fullLocation) =
    {
        name = name,
        nameLoc = nameLoc,
        valOrPat = valOrPat,
        fullLocation = fullLocation,
        expType = ref EmptyType
    }

    fun mkSelector(name, location) : parsetree =
    let
        (* Make a type for this.  It's equivalent to
          fn { name = exp, ...} => exp. *)
      val resType   = makeGeneralTypeVar();
      val entryType = mkLabelEntry (name, resType);
      val labType   = mkLabelled ([entryType], false) (* Not frozen*);
    in
      Selector
        {
          name      = name,
          labType   = labType,
          typeof    = mkFunctionType (labType, resType),
          location  = location
        }
    end;
    
    val mkRaise : parsetree * location -> parsetree = Raise;
    
    fun mkHandleTree (exp, hrules, location) : parsetree = 
       HandleTree
         { 
           exp    = exp,
           hrules = hrules,
           location = location
         };
       
    fun mkWhile (test, body, location) : parsetree =
      While
        { 
            test = test,
            body = body,
            location = location
        };
      
    fun mkCase (test, match, location) : parsetree =
      Case
        {
            test  = test,
            match = match,
            location = location
        };
      
    fun mkAndalso (first, second, location) : parsetree =
      Andalso
        {
          first  = first,
          second = second,
          location = location
        };
      
    fun mkOrelse (first, second, location) : parsetree =
      Orelse
        {
          first  = first,
          second = second,
          location = location
        };
      
    fun mkDirective (tlist, fix, location) : parsetree = 
      Directive
        {
          tlist = tlist,
          fix   = fix,
          location = location
        };
       
    val mkExpseq  : parsetree list * location -> parsetree = ExpSeq;
    
    val mkExDeclaration  : exbind list * location -> parsetree = ExDeclaration;  
    
    val mkParenthesised = Parenthesised
  
  (* This pretty printer is used to format the parsetree
     for error messages (Error near ...) and also for
     debugging.  There is a quite different pretty printer
     in VALUEOPS that is used to format values produced as
     a result of compiling and executing an expression or
     declaration. *) 

    fun printList (doPrint: 'a*int->pretty) (c: 'a list, separator, depth): pretty list =
        if depth <= 0 then [PrettyString "..."]
        else
        case c of
            []      => []
        |   [v]     => [doPrint (v, depth)]
        |   (v::vs) =>
                PrettyBlock (0, false, [],
                    [
                        doPrint (v, depth),
                        PrettyBreak
                           (if separator = "," orelse separator = ";" orelse separator = "" then 0 else 1, 0),
                        PrettyString separator
                    ]
                    ) ::
                PrettyBreak (1, 0) ::
                printList doPrint (vs, separator, depth - 1)
  
   (* Generates a pretty-printed representation of a piece of tree. *)
    fun ptDisplay (c      : parsetree, (* The value to print. *)
                   depth  : int) : pretty = (* The number of levels to display. *)
    let
        val displayList: parsetree list * string * int -> pretty list = printList ptDisplay
        
        (* type bindings and datatype bindings are used in several cases *)
        fun printTypeBind (TypeBind{name, typeVars, decType, ...}, depth) =
            PrettyBlock (3, true, [],
                displayTypeVariables (typeVars, depth) @
                (
                    PrettyString name ::
                    (* The type may be missing if this is a signature. *)
                    (case decType of
                        NONE => []
                    |   SOME t =>
                            [
                                PrettyBreak (1, 0),
                                PrettyString "=",
                                PrettyBreak (1, 0),
                                display (typeFromTypeParse t, depth, emptyTypeEnv)
                            ]
                    )
                )
            )

        and printDatatypeBind (DatatypeBind{name, typeVars, constrs, ...}, depth) =
            PrettyBlock (3, true, [],
                displayTypeVariables (typeVars, depth) @
                    (
                        PrettyString name ::
                        PrettyBreak (1, 0) ::
                        PrettyString "=" ::
                        PrettyBreak (1, 0) ::
                        printList printConstructor (constrs, "|", depth - 1)
                    )
                )

        and printConstructor ({constrName, constrArg, ...}, depth) =
            PrettyBlock (2, false, [],
                PrettyString constrName ::
                (
                    case constrArg of
                        NONE => []
                    |   SOME argType =>
                        [
                            PrettyBreak (1, 0),
                            PrettyString "of",
                            PrettyBreak (1, 0),
                            display (typeFromTypeParse argType, depth, emptyTypeEnv)
                        ]
                )
            )
        
    in
      if depth <= 0 (* elide further text. *)
        then PrettyString "..."

      else case c of
      
        Ident {name, ...} =>
          PrettyString name
          
      | Literal{literal, converter, ...} =>
            let
               val convName = valName converter
             val lit =
                 if convName = "convString"
                then concat["\"" , literal, "\""]
                else literal 
          in
             PrettyString lit
          end

      | Applic { f, arg = TupleTree([left, right], _), isInfix = true, ...} =>
            (* Infixed application. *)
            PrettyBlock (0, false, [],
                [
                    ptDisplay (left, depth - 1),
                    PrettyBreak (1, 0),
                    ptDisplay (f, depth), (* Just an identifier. *)
                    PrettyBreak (1, 0),
                    ptDisplay (right, depth - 1)
                ]
            )

      | Applic {f, arg, ...} => (* Function application. *)
            PrettyBlock (0, false, [],
                [
                    ptDisplay (f, depth - 1),
                    PrettyBreak (1, 0),
                    ptDisplay (arg, depth - 1)
                ]
            )

      | Cond {test, thenpt, elsept, ...} => (* if..then..else.. *)
            PrettyBlock (0, false, [],
                [
                    PrettyString "if",
                    PrettyBreak (1, 0),
                    ptDisplay (test, depth - 1),
                    PrettyBreak (1, 0),
                    PrettyString "then",
                    PrettyBreak (1, 0),
                    ptDisplay (thenpt, depth - 1),
                    PrettyBreak (1, 0),
                    PrettyString "else",
                    PrettyBreak (1, 0),
                    ptDisplay (elsept, depth - 1)
                ]
            )

      | TupleTree(ptl, _) =>
            PrettyBlock (3, true, [],
                (
                    PrettyString "(" ::
                    PrettyBreak (0, 0) ::
                    displayList (ptl, ",", depth - 1)
                ) @ [PrettyBreak (0, 0), PrettyString ")"]
            )

      | ValDeclaration {dec, explicit, ...} =>
        let
            (* We can't use printList here because we don't want an
               "and" after a "rec". *)
            fun printValBind ([], _) = []

              | printValBind (RecValBind :: rest, depth) =
                    PrettyString "rec" :: PrettyBreak (1, 0) :: printValBind(rest, depth)

              | printValBind (ValBind{dec, exp, ...} :: rest, depth) =
                    if depth <= 0
                    then [PrettyString "..."]
                    else
                    let
                        val pValBind =
                            PrettyBlock (3, false, [],
                                [
                                    ptDisplay (dec, depth - 1),
                                    PrettyBreak (1, 0),
                                    PrettyString "=",
                                    PrettyBreak (1, 0),
                                    ptDisplay (exp, depth - 1)
                                ]
                            )
                    in
                        case rest of
                            [] => [pValBind]
                        |   _ => PrettyBlock (0, false, [], [pValBind, PrettyBreak(1, 0), PrettyString "and"]) ::
                                      PrettyBreak(1, 0) :: printValBind(rest, depth-1)
                    end
        in
            PrettyBlock (3, true, [],
                PrettyString "val" ::
                PrettyBreak (1, 0) ::
                (* TODO: Display the explicit type variables. *)
                (* displayTypeVariables (explicit, depth); *)
                printValBind (dec, depth - 1)
            )
        end

      | FunDeclaration {dec, explicit={apply, ...}, ...} =>
          let
            fun printfvalbind (FValBind{clauses, ...}, depth) =
                PrettyBlock(3, true, [], printList printClause (clauses, "|", depth - 1))
            and printClause (FValClause{dec, exp, ...}, depth) =
                PrettyBlock (3, true, [],
                    [
                        printDec (dec, depth - 1),
                        PrettyBreak (1, 0),
                        PrettyString "=",
                        PrettyBreak (1, 0),
                        ptDisplay (exp, depth - 1)
                    ]
                )
            and printDec(
                    { ident = { name, ... }, isInfix=true, args=[TupleTree([left, right], _)], constraint }, depth) =
                (* Single infixed application. *)
                PrettyBlock (0, false, [],
                    [
                        ptDisplay (left, depth - 1),
                        PrettyBreak (1, 0),
                        PrettyString name,
                        PrettyBreak (1, 0),
                        ptDisplay (right, depth - 1)
                    ] @ printConstraint (constraint, depth-1)
                )
            |   printDec(
                    { ident = { name, ... }, isInfix=true,
                      args=TupleTree([left, right], _) :: args, constraint }, depth) =
                (* Infixed application followed by other arguments. *)
                PrettyBlock (0, false, [],
                    [
                        PrettyString "(",
                        PrettyBreak (0, 0),
                        ptDisplay (left, depth - 1),
                        PrettyBreak (1, 0),
                        PrettyString name,
                        PrettyBreak (1, 0),
                        ptDisplay (right, depth - 1),
                        PrettyBreak (0, 0),
                        PrettyString ")"
                    ] @ displayList (args, "", depth - 1) @ printConstraint(constraint, depth-2)
                )
            |   printDec({ ident = { name, ...}, args, constraint, ... }, depth) =
                (* Prefixed application. *)
                PrettyBlock (0, false, [],
                    [ PrettyString name, PrettyBreak (1, 0) ] @
                        displayList (args, "", depth - 1) @ printConstraint(constraint, depth-2)
                )
            and printConstraint(NONE, _) = []
            |   printConstraint(SOME given, depth) =
                [
                    PrettyBreak (1, 0),
                    PrettyString ":",
                    PrettyBreak (1, 0),
                    display (typeFromTypeParse given, depth, emptyTypeEnv)
                ]
         in
            PrettyBlock (3, true, [],
                PrettyString "fun" ::
                PrettyBreak (1, 0) ::
                (* TODO: Display the explicit type variables. *)
                (* displayTypeVariables (explicit, depth); *)
                printList printfvalbind (dec, "and", depth - 1)
            )
        end

      | OpenDec {decs, ...} =>
        let
            fun printStrName ({name, ...}: structureIdentForm, _) = PrettyString name
        in
            PrettyBlock (3, true, [],
                PrettyString "open" ::
                PrettyBreak (1, 0) ::
                printList printStrName (decs, "", depth - 1)
            )
        end

      | List {elements, ...} =>
            PrettyBlock (3, true, [],
                PrettyString "[" ::
                PrettyBreak (0, 0) ::
                displayList (elements, ",", depth - 1) @
                [PrettyBreak (0, 0), PrettyString "]" ]
            )

      | Constraint {value, given, ...} =>
            PrettyBlock (3, false, [],
                [
                    ptDisplay (value, depth - 1),
                    PrettyBreak (1, 0),
                    PrettyString ":",
                    PrettyBreak (1, 0),
                    display (typeFromTypeParse given, depth, emptyTypeEnv)
                ]
            )

      | Layered {var, pattern, ...} =>
            PrettyBlock (3, true, [],
                [
                    ptDisplay (var, depth - 1),
                    PrettyBreak (1, 0),
                    PrettyString "as",
                    PrettyBreak (1, 0),
                    ptDisplay (pattern, depth - 1)
                ]
            )

      | MatchTree {vars, exp, ...} =>
            PrettyBlock (0, false, [],
                [
                    ptDisplay (vars, depth - 1),
                    PrettyBreak (1, 0),
                    PrettyString "=>",
                    PrettyBreak (1, 0),
                    ptDisplay (exp, depth - 1)
                ]
            )

      | Fn (ptl, _) =>
            PrettyBlock (3, true, [],
                PrettyString "fn" ::
                PrettyBreak (1, 0) ::
                displayList (ptl, "|", depth - 1)
            )

      | Unit _ =>
            PrettyString "()"

      | WildCard _ =>
            PrettyString "_"

      | Localdec {isLocal, decs, body, ...} =>
            PrettyBlock (3, false, [],
                PrettyString (if isLocal then "local" else "let") ::
                PrettyBreak (1, 0) ::
                displayList (decs, ";", depth - 1) @
                [PrettyBreak (1, 0), PrettyString "in", PrettyBreak (1, 0)] @
                displayList (body, ";", depth - 1) @
                [PrettyBreak (1, 0), PrettyString "end"]
            )

      | TypeDeclaration(ptl, _) =>
        let
            (* This is used both for type bindings and also in signatures.
               In a signature we may have "eqtype". *)
            val typeString =
                case ptl of
                    TypeBind {isEqtype=true, ...} :: _ => "eqtype"
                |   _ => "type"
        in
            PrettyBlock (3, true, [],
                PrettyString typeString ::
                PrettyBreak (1, 0) ::
                printList printTypeBind (ptl, "and", depth - 1)
            )
        end

      | DatatypeDeclaration {typelist, withtypes, ...} =>
            PrettyBlock (3, true, [],
                PrettyString "datatype" ::
                PrettyBreak (1, 0) ::
                printList printDatatypeBind (typelist, "and", depth - 1) @
                (
                    if null withtypes then []
                    else
                        PrettyBreak (1, 0) ::
                        PrettyString "withtype" ::
                        PrettyBreak (1, 0) ::
                        printList printTypeBind (withtypes, "and", depth - 1)
                 )
             )

      | DatatypeReplication {newType, oldType, ...} =>
            PrettyBlock (3, true, [],
                [
                    PrettyString "datatype",
                    PrettyBreak (1, 0),
                    PrettyString newType,
                    PrettyBreak (1, 0),
                    PrettyString "=",
                    PrettyBreak (1, 0),
                    PrettyString "datatype",
                    PrettyBreak (1, 0),
                    PrettyString oldType
                ]
            )

       | AbstypeDeclaration {typelist, withtypes, declist, ...} =>
            PrettyBlock (3, true, [],
                PrettyString "abstype" ::
                PrettyBreak (1, 0) ::
                printList printDatatypeBind (typelist, "and", depth - 1) @
                [ PrettyBreak (1, 0) ] @
                (
                    if null withtypes then []
                    else
                        PrettyString "withtype" ::
                        PrettyBreak (1, 0) ::
                        printList printTypeBind (withtypes, "and", depth - 1) @
                        [PrettyBreak (1, 0)]
                ) @
                [
                    PrettyString "with",
                    PrettyBreak (1, 0),
                    PrettyBlock (3, true, [],
                        displayList (declist, ";", depth - 1))
                ]
            )
                

      | ExpSeq(ptl, _) =>
            PrettyBlock (3, true, [],
                PrettyString "(" ::
                PrettyBreak (0, 0) ::
                displayList (ptl, ";", depth - 1) @
                [ PrettyBreak (0, 0), PrettyString ")"]
            )

      | Directive {fix, tlist, ...} =>
            PrettyBlock (3, true, [],
                displayFixStatus fix ::
                PrettyBreak (1, 0) ::
                printList (fn (name, _) => PrettyString name) (tlist, "", depth - 1)
            )

      | ExDeclaration(pt, _) =>
          let
            fun printExBind (ExBind {name, ofType, previous, ...}, depth) =
                PrettyBlock (0, false, [],
                    PrettyString name ::
                    (case ofType of NONE => []
                        | SOME typeof =>
                        [
                            PrettyBreak (1, 0),
                            PrettyString "of",
                            PrettyBreak (1, 0),
                            display (typeFromTypeParse typeof, depth, emptyTypeEnv)
                        ]
                    ) @
                    (if isEmptyTree previous then []
                    else 
                    [
                        PrettyBreak (1, 0),
                        PrettyString "=",
                        PrettyBreak (1, 0),
                        ptDisplay (previous, depth - 1)
                    ])
                )
         in
            PrettyBlock (3, true, [],
                PrettyString "exception" ::
                PrettyBreak (1, 0) ::
                printList printExBind (pt, "and", depth - 1)
            )
        end

      | Raise (pt, _) =>
            PrettyBlock (0, false, [],
                [
                    PrettyString "raise",
                    PrettyBreak (1, 0),
                    ptDisplay (pt, depth - 1)
                ]
            )

      | HandleTree {exp, hrules, ...} =>
            PrettyBlock (0, false, [],
                [
                    ptDisplay (exp, depth - 1),
                    PrettyBreak (1, 0),
                    PrettyBlock (3, true, [],
                        PrettyString "handle" ::
                        PrettyBreak (1, 0) ::
                        displayList (hrules, "|", depth - 1)
                    )
                ]
            )

      | While {test, body, ...} =>
            PrettyBlock (0, false, [],
                [
                    PrettyString "while",
                    PrettyBreak (1, 0),
                    ptDisplay (test, depth - 1),
                    PrettyBreak (1, 0),
                    PrettyString "do",
                    PrettyBreak (1, 0),
                    ptDisplay (body, depth - 1)
                ]
            )

      | Case {test, match, ...} =>
            PrettyBlock (3, true, [],
                PrettyBlock (0, false, [],
                    [
                        PrettyString "case",
                        PrettyBreak (1, 0),
                        ptDisplay (test, depth - 1),
                        PrettyBreak (1, 0),
                        PrettyString "of"
                    ]
                ) ::
                PrettyBreak (1, 0) ::
                displayList (match, "|", depth - 1)
            )

      | Andalso {first, second, ...} =>
            PrettyBlock (3, true, [],
                [
                    ptDisplay (first, depth - 1),
                    PrettyBreak (1, 0),
                    PrettyString "andalso",
                    PrettyBreak (1, 0),
                    ptDisplay (second, depth - 1)
                ]
            )

      | Orelse {first, second, ...} =>
            PrettyBlock (3, true, [],
                [
                    ptDisplay (first, depth - 1),
                    PrettyBreak (1, 0),
                    PrettyString "orelse",
                    PrettyBreak (1, 0),
                    ptDisplay (second, depth - 1)
                ]
            )

      | Labelled {recList, frozen, ...} =>
        let
            fun displayRecList (c, depth): pretty list =
            if depth <= 0 then [PrettyString "..."]
            else
              case c of
                []      => []
              | [{name, valOrPat, ...}]     =>
                    [
                        PrettyBlock (0, false, [],
                            [
                                PrettyString (name ^ " ="),
                                PrettyBreak (1, 0),
                                ptDisplay (valOrPat, depth - 1)
                            ]
                        )
                    ]
                | ({name, valOrPat, ...}::vs) =>
                    PrettyBlock (0, false, [],
                        [
                             PrettyBlock (0, false, [],
                                [
                                    PrettyString (name ^ " ="),
                                    PrettyBreak (1, 0),
                                    ptDisplay (valOrPat, depth - 1)
                                ]
                            ),
                            PrettyBreak (0, 0),
                            PrettyString ","
                        ]
                    ) ::
                    PrettyBreak (1, 0) ::
                    displayRecList (vs, depth - 1)
             (* end displayRecList *)
        in
            PrettyBlock (2, false, [],
                PrettyString "{" ::
                displayRecList (recList, depth - 1) @
                (if frozen then [PrettyString "}"]
                else [PrettyString (if null recList then "...}" else ", ...}")])
            )
        end

      | Selector {name, ...} =>
          PrettyString ("#" ^ name)

      | EmptyTree =>
         PrettyString "<Empty>"
         
      | Parenthesised(p, _) =>
            PrettyBlock(0, false, [],
                [
                    PrettyString "(",
                    PrettyBreak (0, 0),
                    ptDisplay (p, depth),
                    PrettyBreak (0, 0),
                    PrettyString ")"
                ]
            )
        
    end (* ptDisplay *)

    fun getExportTree(navigation, p: parsetree) =
    let
        (* Common properties for navigation and printing. *)
        val commonProps = exportNavigationProps navigation @ [PTprint(fn d => ptDisplay(p, d))]

        fun asParent () = getExportTree(navigation, p)

         (* Put all these into a common list.  That simplifies navigation between
            the various groups in abstypes and datatypes. *)
        datatype lType = DataT of datatypebind | TypeB of typebind | Decl of parsetree
       
        (* Common code for datatypes, abstypes and type bindings. *)
        fun exportTypeBinding(navigation, this as DataT(DatatypeBind{name, nameLoc, fullLoc, constrs, ...})) =
            let
                fun asParent () = exportTypeBinding(navigation, this)
                (* Ignore any type variables before the type name. *)
                fun getName () =
                    getStringAsTree({parent=SOME asParent, previous=NONE, next=SOME getConstrs}, name, nameLoc, [])
                and getConstrs () =
                    let
                        fun exportConstrs(navigation, {constrName, idLocn, constrArg, ... }) =
                            (* TODO: the constructor type. *)
                            getStringAsTree(navigation, constrName, idLocn, [])
                    in
                        (fullLoc, (* TODO: We need a separate location for the constrs. *)
                            exportList(exportConstrs, SOME asParent) constrs @    
                                exportNavigationProps {parent=SOME asParent, previous=SOME getName, next=NONE})
                    end
            in
                (fullLoc, PTfirstChild getName :: exportNavigationProps navigation)
            end

        |   exportTypeBinding(navigation,
                this as TypeB(TypeBind{name, nameLoc, decType = SOME decType, fullLoc, ...})) =
            let
                fun asParent () = exportTypeBinding(navigation, this)
                (* Ignore any type variables before the type name. *)
                fun getName () =
                    getStringAsTree({parent=SOME asParent, previous=NONE, next=SOME getType}, name, nameLoc, [])
                and getType () =
                    typeExportTree({parent=SOME asParent, previous=SOME getName, next=NONE}, decType)
            in
                (fullLoc, PTfirstChild getName :: exportNavigationProps navigation)
            end

           (* TypeBind is also used in a signature in which case decType could be NONE. *)
        |   exportTypeBinding(navigation,
                this as TypeB(TypeBind{name, nameLoc, decType = NONE, fullLoc, ...})) =
            let
                fun asParent () = exportTypeBinding(navigation, this)
                (* Ignore any type variables before the type name. *)
                (* Retain this as a child entry in case we decide to add the type vars later. *)
                fun getName () =
                    getStringAsTree({parent=SOME asParent, previous=NONE, next=NONE}, name, nameLoc, [])
            in
                (fullLoc, PTfirstChild getName :: exportNavigationProps navigation)
            end

        |   exportTypeBinding(navigation, Decl dec) =
                (* Value declarations in an abstype. *) getExportTree(navigation, dec)
    in
        case p of
            Ident{location, expType=ref expType, value, ...} =>
            let
                (* Include the type and declaration properties if these
                   have been set. *)
                val decProp =
                    case value of
                        ref (Value{name = "<undefined>", ...}) => []
                    |   ref (Value{locations, ...}) => mapLocationProps locations
            in
                (location, PTtype expType :: decProp @ commonProps)
            end

        |   Literal {location, expType=ref expType, ...} => (location, PTtype expType :: commonProps)

            (* Infixed application.  For the purposes of navigation we treat this as
               three entries in order. *)
        |   Applic{location, f, arg = TupleTree([left, right], _), isInfix = true, expType=ref expType, ...} =>
                (location,
                    PTtype expType :: exportList(getExportTree, SOME asParent) [left, f, right] @ commonProps)

            (* Non-infixed application. *)
        |   Applic{location, f, arg, expType=ref expType, ...} =>
                (location, PTtype expType :: exportList(getExportTree, SOME asParent) [f, arg] @ commonProps)

        |   Cond{location, test, thenpt, elsept, ...} =>
                (location, exportList(getExportTree, SOME asParent) [test, thenpt, elsept] @ commonProps)

        |   TupleTree(entries, location) =>
                (location, exportList(getExportTree, SOME asParent) entries @ commonProps)

        |   ValDeclaration{location, dec, ...}  =>
            let
                (* TODO: This is a mess.  "rec" is not really an entry in the list. *)
                fun exportVB(navigation, vb as ValBind{dec, exp, line}) =
                    let
                        val vbProps = exportNavigationProps navigation
                        (* First child should give the pattern *)
                        (* Second child should give the expression *)
                        fun exportThis () = exportVB(navigation, vb)
                        val asChild = exportList(getExportTree, SOME exportThis) [dec, exp]
                    in
                        (line, asChild @ vbProps)
                    end
                |   exportVB(navigation, RecValBind) = (nullLocation, [])

                val expChild = exportList(exportVB, SOME asParent) dec
            in
                (* We need a special case for a top-level expression.  This has been converted
                   by the parser into val it = exp but the "val it = " takes up no space.
                   We need to go directly to the expression in that case. *)
                case dec of
                    [ValBind{dec=Ident{name="it", location=itLoc, ...}, exp, ...}]
                    => if #startPosition itLoc = #endPosition itLoc andalso
                          #startLine itLoc = #endLine itLoc
                       then getExportTree(navigation, exp)
                       else (location, expChild @ commonProps)
                | _ => (location, expChild @ commonProps)
            end

        |   FunDeclaration{location, dec, ...}  =>
            let
                (* It's easiest to put these all together into a single list. *)
                datatype funEntry =
                    FunIdent of { name: string, expType: types ref, location: location }
                |   FunPtree of parsetree
                |   FunConstraint of typeParsetree
                |   FunInfixed of funEntry list * location

                fun exportFunEntry(navigation, FunIdent{name, expType=ref expType, location}) =
                        (location, PTtype expType :: PTdeclaredAt location :: exportNavigationProps navigation)
                |   exportFunEntry(navigation, FunPtree pt) = getExportTree(navigation, pt)
                |   exportFunEntry(navigation, FunConstraint typ) = typeExportTree(navigation, typ)

                |   exportFunEntry(navigation, this as FunInfixed(inf, location)) =
                    let
                        fun asParent () = exportFunEntry(navigation, this)
                        val expChild = exportList(exportFunEntry, SOME asParent) inf
                    in
                        (location, expChild @ exportNavigationProps navigation)
                    end

                fun exportAClause(
                        FValClause{dec = {ident, isInfix, args, constraint}, exp, ...}, exportThis) =
                let
                    (* The effect of this is to have all the elements of the clause as
                       a single level except that if we have an infixed application of
                       the function (e.g. fun f o g = ...) then this is a subnode. *)
                    val funAndArgs =
                        case (isInfix, args) of
                            (true, TupleTree([left, right], loc) :: otherArgs) => (* Infixed. *)
                                FunInfixed([FunPtree left, FunIdent ident, FunPtree right], loc)
                                    :: map FunPtree otherArgs
                        |   (_, args) => (* Normal prefixed form. *)
                                FunIdent ident :: map FunPtree args

                    val constraint = case constraint of NONE => [] |SOME typ => [FunConstraint typ]
                in
                    exportList(exportFunEntry, SOME exportThis) (funAndArgs @ constraint @ [FunPtree exp])
                end

                fun exportFB(navigation, fb as FValBind{clauses=[clause], location, ...}) =
                    (* If there's just one clause go straight to it.  Otherwise we have an
                       unnecessary level of navigation. *)
                    let
                        val fbProps = exportNavigationProps navigation
                        val asChild = exportAClause(clause, fn () => exportFB(navigation, fb))
                    in
                        (location, asChild @ fbProps)
                    end
                
                |   exportFB(navigation, fb as FValBind{clauses, location, ...}) =
                    let
                        val fbProps = exportNavigationProps navigation
                        (* Each child gives a clause. *)
                        (* First child should give the pattern *)
                        (* Second child should give the expression *)
                        fun exportThis () = exportFB(navigation, fb)
                        
                        fun exportClause(navigation, clause as FValClause{ line, ...}) =
                        let
                            val clProps = exportNavigationProps navigation
                            val asChild = exportAClause(clause, fn () => exportClause(navigation, clause))
                        in
                            (line, asChild @ clProps)    
                        end
                            
                        val asChild = exportList(exportClause, SOME exportThis) clauses
                    in
                        (location, asChild @ fbProps)
                    end

                val expChild = exportList(exportFB, SOME asParent) dec
            in
                (location, expChild @ commonProps)
            end

        |   OpenDec{location, decs, ...} =>
            let
                fun exportStructIdent(navigation, { name, value=ref value, location} ) =
                    let
                        (* Include the declaration properties if it has been set. *)
                        val siProps = exportNavigationProps navigation @
                            (
                                if isUndefinedStruct value
                                then []
                                else mapLocationProps(structLocations value)
                            )
                    in
                        (location, siProps)
                    end

                val expChild = exportList(exportStructIdent, SOME asParent) decs
            in
                (location, expChild @ commonProps)
            end

        |   Constraint{location, value, given, ...} =>
            let
                (* The first position is the expression, the second the type *)
                fun getExpr () =
                    getExportTree({parent=SOME asParent, previous=NONE, next=SOME getType}, value)
                and getType () =
                    typeExportTree({parent=SOME asParent, previous=SOME getExpr, next=NONE}, given)
            in
                (location, PTfirstChild getExpr :: commonProps)
            end

        |   Layered{location, var, pattern, ...} =>
                (location, exportList(getExportTree, SOME asParent) [var, pattern] @ commonProps)

        |   Fn(ptl,location) =>
                (location, exportList(getExportTree, SOME asParent) ptl @ commonProps)

        |   MatchTree{location, vars, exp, ...} =>
                (location, exportList(getExportTree, SOME asParent) [vars, exp] @ commonProps)

        |   Localdec{location, decs, body, ...} =>
                (location, exportList(getExportTree, SOME asParent) (decs @ body) @ commonProps)

        |   TypeDeclaration(tbl, location) =>
            let
                val allItems = List.map TypeB tbl
            in
                (location, exportList(exportTypeBinding, SOME asParent) allItems @ commonProps)
            end

        |   AbstypeDeclaration { location, typelist, withtypes, declist, ... } =>
            let
                val allItems =
                    List.map DataT typelist @ List.map TypeB withtypes @ List.map Decl declist
            in
                (location, exportList(exportTypeBinding, SOME asParent) allItems @ commonProps)
            end

        |   DatatypeDeclaration { location, typelist, withtypes, ... } =>
            let
                val allItems =
                    List.map DataT typelist @ List.map TypeB withtypes
            in
                (location, exportList(exportTypeBinding, SOME asParent) allItems @ commonProps)
            end

        |   DatatypeReplication{location, ...} => (* TODO *) (location, commonProps)

        |   ExpSeq(ptl, location) =>
                (location, exportList(getExportTree, SOME asParent) ptl @ commonProps)

        |   Directive{location, ...} =>
                (* No need to process the individual identifiers. *)
                (location, commonProps)

        |   ExDeclaration(exbinds, location) =>
            let
                (* There are three possibilities here.  exception exc; exception exc of ty; exception exc = exc' *)
                fun exportExdec(navigation, eb as ExBind{name, previous=EmptyTree, ofType=NONE, nameLoc, ...}) =
                        (* Simple, generative exception with no type. *)
                        getStringAsTree(navigation, name, nameLoc, [PTtype exnType])

                |   exportExdec(navigation,
                        eb as ExBind{name, previous=EmptyTree, ofType=SOME ofType, nameLoc, fullLoc, ...}) =
                        (* exception exc of type. *)
                    let
                        fun asParent () = exportExdec (navigation, eb)
                        fun getName () =
                            getStringAsTree({parent=SOME asParent, next=SOME getOfType, previous=NONE},
                                name, nameLoc, [(* Type could be in here? *)])
                        and getOfType () =
                            typeExportTree({parent=SOME asParent, previous=SOME getName, next=NONE}, ofType)
                    in
                        (fullLoc, PTfirstChild getName :: exportNavigationProps navigation)
                    end

                |   exportExdec(navigation,
                        eb as ExBind{name, previous, (* ofType=NONE, *) nameLoc, fullLoc, ...}) =
                    let
                        fun asParent () = exportExdec (navigation, eb)
                        fun getName () =
                            getStringAsTree({parent=SOME asParent, next=SOME getPreviousExc, previous=NONE},
                                name, nameLoc, [(* Type could be in here? *)])
                        and getPreviousExc () =
                            getExportTree({parent=SOME asParent, previous=SOME getName, next=NONE}, previous)
                    in
                        (fullLoc, PTfirstChild getName :: exportNavigationProps navigation)
                    end

                val expChild = exportList(exportExdec, SOME asParent) exbinds
            in
                (location, expChild @ commonProps)
            end

        |   Raise(raiseExp, location) =>
            let
                fun getExp () = getExportTree({parent=SOME asParent, next=NONE, previous=NONE}, raiseExp)
            in
               (location, [PTfirstChild getExp] @ commonProps)
            end

        |   HandleTree{location, exp, hrules, ...} =>
                (* For the moment just treat the expression and each of the handlers as
                   siblings.  It might be better to treat the expression and the
                   set of handlers as siblings and go down a level to process the
                   individual handlers.  That really requires the parser to
                   give a separate location for the handler set. *)
                (location, exportList(getExportTree, SOME asParent) (exp :: hrules) @ commonProps)

        |   While{location, test, body, ...}           =>
                (location, exportList(getExportTree, SOME asParent) [test, body] @ commonProps)

        |   Case{location, test, match, ...}            =>
                (* As with HandleTree it may be better to treat the expression being tested
                   and the branches of the case at separate levels. *)
                (location, exportList(getExportTree, SOME asParent) (test :: match) @ commonProps)

        |   Andalso {location, first, second, ...} =>
                (location, exportList(getExportTree, SOME asParent) [first, second] @ commonProps)

        |   Orelse{location, first, second, ...} =>
                (location, exportList(getExportTree, SOME asParent) [first, second] @ commonProps)

        |   Labelled{location, expType=ref expType, recList, ...} =>
            let
                (* It's convenient to be able to click on the label part and get
                   the type of the expression or pattern on the right of the '='. *)
                fun exportField(navigation,
                        label as {name, nameLoc, valOrPat, expType=ref expType, fullLocation, ...}) =
                let
                    val patTree as (patLocation, _) = getExportTree(navigation, valOrPat)
                in
                    if patLocation = fullLocation
                    then
                        (* The parser rewrites { name, ...} as { name=name, ... } (more generally
                           { name: ty as pat, ...} as { name = name: ty as pat).
                           To avoid having nodes that overlap we return only the pattern part here. *)
                        patTree
                    else
                    let
                        (* The first position is the label, the second the type *)
                        fun asParent () = exportField (navigation, label)
                        fun getLab () =
                            getStringAsTree({parent=SOME asParent, next=SOME getExp, previous=NONE},
                                name, nameLoc, [PTtype expType])
                        and getExp () =
                            getExportTree({parent=SOME asParent, previous=SOME getLab, next=NONE}, valOrPat)
                    in
                        (fullLocation, PTfirstChild getLab :: exportNavigationProps navigation)
                    end
                end

                val expChild = exportList(exportField, SOME asParent) recList
            in
                (location, PTtype expType :: (expChild @ commonProps))
            end

        |   Selector{location, typeof, ...} => (location, PTtype typeof :: commonProps)

        |   List{elements, location, expType = ref expType, ...} =>
                (location,
                    PTtype expType :: exportList(getExportTree, SOME asParent) elements @ commonProps)

        |   EmptyTree                      => (nullLocation, commonProps)

        |   WildCard location              => (location, commonProps)

        |   Unit location                  => (location, PTtype unitType :: commonProps)

        |   Parenthesised(p, _) => getExportTree(navigation, p)
    end
    
    fun getLocation c = #1 (getExportTree({parent=NONE, next=NONE, previous=NONE}, c))

    (* Error message routine.  Used in both pass 2 and pass 3. *)
    fun errorNear (lex, hard, near, line, message) =
    let
        val parameters = debugParams lex
        val errorDepth = getParameter errorDepthTag parameters
    in
    (* Puts out an error message and then prints the piece of tree. *)
        reportError lex
        {
            hard = hard,
            location = line,
            message = PrettyBlock (0, false, [], [PrettyString message]),
            context = SOME(ptDisplay (near, errorDepth))
        }
     end;

    (* Extract the declaration location from the location list. *)
    fun declaredAt [] = LEX.nullLocation
    |   declaredAt (DeclaredAt loc :: _) = loc
    |   declaredAt (_::l) = declaredAt l

(*****************************************************************************
                                PASS 2
                  Identifier matching and type checking
******************************************************************************)



   (* Second pass of ML parse tree. *)
   
    (* This is pass 2 of the compiler. It walks over the parse tree
       generated by pass 1 and looks up identifiers to match them to
       declarations. It performs the type checking. "makeTypeId" is used
       to construct unique identifiers for types depending on the context
       (i.e. in a signature, structure or functor). *)
    fun pass2 (v, makeTypeId, env, lex) =
    let
      (* Returns a function which can be passed to unify or apply to
         print a bit of context info. *)
        fun foundNear v () =
        let
            val parameters = debugParams lex
            val errorDepth = getParameter errorDepthTag parameters
        in
            ptDisplay (v, errorDepth)
        end;

      (* A simpler error message routine for lookup_... where the message
         does not involve pretty-printing anything. *)
      fun giveError (v, lex, line)  =
        fn message => errorNear (lex, true, v, line, message);

      fun checkForBuiltIn (name, v, lex, lineno, isConstr) =
      (* ML97 does not allow the standard constructors to be rebound and does
         not allow "it" to become a constructor. *)
         if name = "true" orelse name = "false" orelse name = "nil"
         orelse name = "::" orelse name = "ref" orelse (isConstr andalso name = "it")
         then errorNear(lex, true, v, lineno,
                     "Rebinding or specifying \"" ^ name ^ "\" is illegal")
         else ()

        fun errorDepth lex =
        let
            open DEBUG
            val parameters = LEX.debugParams lex
        in
            getParameter errorDepthTag parameters
        end

        (* Turn a result from unifyTypes into a pretty structure so that it
           can be included in a message. *)
        fun unifyErrorReport(lex, typeEnv) = unifyTypesErrorReport(lex, typeEnv, typeEnv, "unify")

        (* Error message for incompatible types.  Displays both expressions and their types. *)
        fun typeMismatch (title, left, right, detail, lex : lexan, location, moreInfo) =
        let
            val errorDepth = errorDepth lex
            val message =
                PrettyBlock(3, true, [],
                    [
                        PrettyString title,
                        PrettyBreak(1, 0), left,
                        PrettyBreak(1, 0), right,
                        PrettyBreak(1, 0),
                        PrettyBlock(0, false, [],
                            [PrettyString "Reason:", PrettyBreak(1, 3), detail])
                    ])
        in
            reportError lex
            {
                location = location,
                hard = true,
                message = message,
                context = SOME (moreInfo ())
            }
        end;

        (* Error message for single expressions with the wrong type. e.g. "if" not followed
           by a "bool". *)
        fun typeWrong (title, value, detail, lex : lexan, location, moreInfo) =
        let
            val errorDepth = errorDepth lex
            val message =
                PrettyBlock(3, true, [],
                    [
                        PrettyString title,
                        PrettyBreak(1, 0), value,
                        PrettyBreak(1, 0),
                        PrettyBlock(0, false, [],
                            [ PrettyString "Reason:", PrettyBreak(1, 3), detail])
                    ])
        in
            reportError lex
            {
                location = location,
                hard = true,
                message = message,
                context = SOME (moreInfo ())
            }
        end;

        (* Display a value and its type as part of an error message. *)
        fun valTypeMessage (lex, typeEnv) (title, value, valType) =
        let
            val errorDepth = errorDepth lex
        in
            PrettyBlock(3, false, [],
                [
                    PrettyString title,
                    PrettyBreak(1, 0),
                    ptDisplay (value, errorDepth),
                    PrettyBreak(1, 0),
                    PrettyString ":",
                    PrettyBreak(1, 0),
                    display(valType, 10000 (* All of it *), typeEnv)
                ])
        end

        (* Old error message and unification functions.  These will eventually be
           removed.  *)
        fun matchError 
            (error: matchResult, lex : lexan, location : LEX.location, moreInfo : unit -> pretty, typeEnv) : unit =
            reportError lex
            {
                location = location,
                hard = true,
                message = unifyErrorReport(lex, typeEnv) error,
                context = SOME (moreInfo ())
           }

        fun unify (alpha, beta, lex, location, moreInfo, typeEnv) =
            case unifyTypes (alpha, beta) of
                NONE => ()
            |   SOME error =>
                    matchError (error, lex, location, moreInfo, typeEnv)

        fun apply (f, arg, lex, location, moreInfo, typeEnv) =
            case eventual f of
                FunctionType {arg=farg, result} =>
                (
                    unify (farg, arg, lex, location, moreInfo, typeEnv);
                    result
                )
            |   ef => (* Type variables etc. - Use general case. *)
                let  (* Make arg->'a, and unify with the function. *)
                    val resType  = mkTypeVar (generalisable, false, false);
                    val fType    = mkFunctionType (arg, resType);
      
                    (* This may involve more than just assigning the type to "ef". *)
                    val () = unify (ef, fType, lex, location, moreInfo, typeEnv);
                in
                    resType (* The result is the type variable unified to the result. *)
                end

        (* These cases currently use the "apply" or "unify" and may need to be improved in
           order to produce better messages.
           apply:
              Literals.  The conversion functions are applied to the string literal.  In effect this produces the set
              of overloadings of the literal.  This should never produce an error message.
              Constructors in patterns to their args.
              "case": the patterns are "applied" to the value to be tested.

           unify:
              Layered patterns, to set the variable. Also checks the pattern against any explicit type.
              Handlers: the handling patterns are unified against a function from exn -> the result type of the
              expression being handled.
         *)

    fun assignValues (level, letDepth, env, near, v)  =
    let
        val typeEnv = { lookupType = #lookupType env, lookupStruct = #lookupStruct env }
         (* Process each item of the sequence and return the type of the
            last item. A default item is returned if the list is empty. *)
        fun assignSeq env depth (l: parsetree list) =
        let
          fun applyList last []       = last
            | applyList last (h :: t) = 
              applyList (assignValues(level, depth, env, v, h)) t
        in
          applyList badType l
        end;

        fun ptAssignTypes t near =
            assignTypes
                (t,
                fn (s, line) => 
                    lookupTyp 
                        ({lookupType = #lookupType env, lookupStruct = #lookupStruct env},
                        s, giveError (near, lex, line)),
                lex);

        (* Makes a type for an instance of an identifier. *)
        fun instanceType (v as Value{access=Overloaded _, ...}) =
          (* Look up the current overloading for this function. *)
                overloadType(v, false)

        | instanceType v =
            (* The types of constructors and variables are copied 
               to create new instances of type variables. *)
              generalise (valTypeOf v);

        fun processPattern(pat, enterResult, level, notConst, isRec) =
        let
            val mapProcessPattern =
                map (fn x => processPattern(x, enterResult, level, notConst, isRec));
        in
            case pat of
                Ident {name, value, expType, location, ...} => (* Variable or nullary constructor. *)
                let
                    (* Look up the name. If it is a constructor then use it,
                        otherwise return `undefined'. If it is a qualified name,
                        i.e. it contains a full-stop, we assume it is a constructor
                        and give an error message if it does not exist. *)
                    (* In ML 97 recursive declarations such as val rec f = ...
                         override constructor status.  If this is a recursive declaration
                         we don't check for constructor status. *)
                    val names   = splitString name;
                    val nameVal =
                        if isRec
                        then undefinedValue
                        else if #first names = ""
                        then (* Not qualified - may be a variable. *)
                            getOpt (#lookupVal env name, undefinedValue) 
              
                        else (* Qualified - cannot be a variable. *)
                            lookupValue
                                ("Constructor",
                                {lookupVal= #lookupVal env, lookupStruct= #lookupStruct env},
                                name,
                                giveError (pat, lex, location));
            
                   
                    val instanceType = 
                        (* If the result is a constructor use it. *)
                        if isConstructor nameVal (* exceptions. *)
                        then
                        ( 
                            if notConst
                            then errorNear (lex, true, pat, location,
                                    "Identifier before `as' must not be a constructor")
                            else ();
                 
                            (* set this value in the record *)
                            value := nameVal;
      
                            (* Must be a nullary constructor otherwise it should
                               have been applied to something. *)
                            let
                                val isNullary =
                                    case nameVal of
                                        Value{class=Constructor{nullary, ...}, ...} => nullary
                                    |   Value{typeOf, ...} => (* exception *) not (isSome(getFnArgType typeOf))
                            in
                                if isNullary then ()
                                else errorNear (lex, true, pat, location,
                                                "Constructor must be applied to something.")
                            end;
                
                            instanceType nameVal
                        )
      
                        (* If undefined or another variable, construct a new variable. *)
                        else
                        let
                            val var = 
                                mkVar (name, mkTypeVar (level, false, false), [DeclaredAt location]);
                        in
                            checkForDots (name, lex, location); (* Must not be qualified *)
                            (* Must not be "true", "false" etc. *)
                            checkForBuiltIn (name, v, lex, location, false);
                            enterResult (name, var);
                            value := var;
                            valTypeOf var (* and return its type *)
                        end;
                in
                    expType := instanceType; (* Record the instance type.*)
                    instanceType
                end
    
            |   Literal{converter, expType, location, ...} =>
                let
                    (* Find out the overloadings on this converter and
                       construct an instance of it.  The converters are
                       all functions from string to the result type. *)
                    val instanceType = overloadType(converter, true)
                    (* Apply the converter to string to get the type of the
                       literal. *)
                    val instance =
                        apply(instanceType, stringType, lex, location, foundNear pat, typeEnv)
                in
                    expType := instance; (* Record the instance type.*)
                    instance
                end

            |   Applic {f = con, arg, location, expType, ...} =>
                let
                    (* Apply the function to the argument and return the result. *)
                    (* Function must be a constructor. *)
                    val conType = 
                        case con of
                            Ident {name, value, location, expType, ...} =>
                            let (* Look up the value and return the type. *)
                                val constrVal =
                                    lookupValue 
                                        ("Constructor",
                                        {lookupVal   = #lookupVal env, lookupStruct = #lookupStruct env},
                                        name, giveError (pat, lex, location));
                            in
                                if isConstructor constrVal
                                then
                                let
                                    val cType = instanceType constrVal
                                in
                                    value := constrVal;
                                    expType := cType; (* Record the instance type.*)
                                    cType
                                end
                                else (* Undeclared or a variable. *)
                                (
                                    if isUndefinedValue constrVal then ()
                                    else errorNear (lex, true, pat, location, name ^ " is not a constructor");
                                    badType
                                )
                            end
        
                        |   _ => (* con is not an Ident *)
                            (
                                errorNear (lex, true, pat, location,
                                    "Constructor in a pattern was not an identifier");
                                badType
                            )
    
                    val patType = processPattern(arg, enterResult, level, notConst, isRec);
                    (* Apply to the pattern type. *)
                    val resultType = apply (conType, patType, lex, location, foundNear pat, typeEnv)
                in
                    expType := resultType; (* Record the instance type.*)
                    resultType
                end (* Applic *)

            |   TupleTree(ptl, _) =>
                (* Construct the type obtained by mapping "processPattern"
                   onto each element of the tuple. *)
                mkProductType (mapProcessPattern ptl)

            |   Labelled {recList, frozen, expType, location, ...} =>
                let (* Process each item in the list. *)

                    fun mapLabels [] = []
                    |   mapLabels ({name, valOrPat, expType, fullLocation, ...}::T) =
                        (* Type is a label entry with the label name
                           and the type of the pattern. *)
                        let
                            val ty = processPattern(valOrPat, enterResult, level, notConst, isRec)
                        in
                            expType := ty;
                            mkLabelEntry(name, ty) :: mapLabels T
                        end;
                    val patType = mkLabelled (sortLabels(mapLabels recList), frozen);
                in
                    expType := patType;
                    patType
                end

            |   (aList as List{elements, location, expType}) =>
                let
                    (* Applies "processPattern" to every element of a list and
                       unifies the types. Returns a type variable if the list
                       is empty *)
                    fun processElement elem =
                        processPattern(elem, enterResult, level, notConst, isRec)
                    val elementType =
                        assignList processElement
                            ("Elements in a list have different types.", "Item", ",", location, aList) elements
                    val resType =
                        if isBadType elementType
                        then badType
                        else mkTypeConstruction ("list", listType, [elementType], [DeclaredAt inBasis])
                in
                    expType := resType;
                    resType
                end

            |   aConstraint as Constraint {value, given, location} =>
                let
                    val valType  = processPattern(value, enterResult, level, notConst, isRec);
                    val theType = typeFromTypeParse given
                    val () = ptAssignTypes theType pat;
                in
                    (* These must be unifiable. *)
                    case unifyTypes(valType, theType) of
                        NONE => () (* OK. *)
                    |   SOME report =>
                            typeMismatch("Type mismatch in type constraint.",
                                valTypeMessage (lex, typeEnv) ("Value:", value, valType),
                                PrettyBlock(0, false, [],
                                    [
                                        PrettyString "Constraint:",
                                        PrettyBreak(1, 0),
                                        display(theType, 10000 (* All of it *), typeEnv)
                                    ]),
                                unifyErrorReport (lex, typeEnv) report,
                                lex, location, foundNear aConstraint);
                    theType
                end

            |   Layered {var, pattern, location} =>
                let
                    (* Unify the variable and the pattern - At this stage that simply
                     involves assigning the type of the pattern to the variable,
                     but it may result in more unification when the variable is
                     used *)
              
                    (* The "variable" must be either id or id: ty but we have to
                     check that the id is not a constructor. *)
                    val varType = processPattern(var,     enterResult, level, true, isRec);
                    val patType = processPattern(pattern, enterResult, level, notConst, isRec)
                    val () = unify (varType, patType, lex, location, foundNear pat, typeEnv);
                in
                    varType
                end

            |   Unit _ => unitType

            |   WildCard _ => mkTypeVar (generalisable, false, false)

            |   Parenthesised(p, _) =>
                    processPattern(p, enterResult, level, notConst, isRec)

            |   _ => (* not a legal pattern *)
                    badType

        end (* processPattern *)

        
        (* Applies "assignValues" or "processPattern" to every element of a list and unifies the
           types. Returns a type variable if the list is empty.
           This is used for lists, function values (fn .. => ...),
           handlers and case expressions. *)
        and assignList _ _ [] = mkTypeVar (generalisable, false, false)
        |   assignList processValue _ [single] = processValue single

        |   assignList processValue (errorMsg, itemName, separator, location, near) (tlist as hd :: tl) =
            let
                val firstType = processValue hd

                fun applyList(ty, _, []) = ty
                |   applyList(ty, n, h::t) =
                    let
                        val typ = processValue h
                    in
                        case unifyTypes (ty, typ) of
                            NONE => applyList(ty, n+1, t)
                        |   SOME report =>
                            let
                                (* We have a type error but we don't know which is correct.
                                   The previous items must have produced a consistent type
                                   otherwise we'd already have reported an error but we
                                   can't identify exactly where the error occurred. *)
                                val errorDepth = errorDepth lex
                                val previousValsAndType =
                                    PrettyBlock(3, false, [],
                                        [
                                            PrettyString (
                                                if n = 1 then itemName ^ " 1:"
                                                else itemName ^ "s 1-" ^ Int.toString n ^ ":"),
                                            PrettyBreak(1, 0),
                                            PrettyBlock(0, false, [],
                                                printList ptDisplay (List.take(tlist, n),
                                                separator, errorDepth)),
                                            PrettyBreak(1, 0),
                                            PrettyString ":",
                                            PrettyBreak(1, 0),
                                            display(ty, 10000 (* All of it *), typeEnv)
                                        ])
                            in
                                typeMismatch(errorMsg,
                                    previousValsAndType,
                                    valTypeMessage(lex, typeEnv) (concat[itemName, " ", Int.toString(n+1), ":"], h, typ),
                                    unifyErrorReport(lex, typeEnv) report, lex, location, foundNear near);
                                (* Continue with "bad" which suppresses further error messages
                                   and return "bad" as the result. *)
                                applyList(badType, n+1, t)
                            end
                    end
            in
                applyList(firstType, 1, tl)
            end

        (* val assValues = assignValues level line env; *)
        and assValues near v =
          case v of
            Ident {name, value, expType, location} =>
            let
                val expValue =
                    lookupValue 
                        ("Value or constructor",
                            {lookupVal = #lookupVal env, lookupStruct = #lookupStruct env},
                            name, giveError (near, lex, location));
                (* Set the value and type found. *)
                val instanceType = instanceType expValue;
            in
                expType := instanceType;
                value  := expValue;
                instanceType (* Result is the instance type. *)
            end

          | Literal{converter, expType, location, ...} =>
            let
                (* Find out the overloadings on this converter and
                   construct an instance of it.  The converters are
                   all functions from string to the result type. *)
                val instanceType = overloadType(converter, true)
                val instance =
                    apply(instanceType, stringType, lex, location, foundNear near, typeEnv)
            in
                expType := instance;
                instance
            end

          | Applic {f, arg, location, expType, ...} => 
            let
                (* Apply the function to the argument and return the result. *)
                val funType = assValues near f;
                val argType = assValues near arg;
                (* Test to see if we have a function. *)
                val fType =
                    case eventual funType of
                        FunctionType {arg, result} => SOME(arg, result)
                    |   ef => (* May be a simple type variable. *)
                        let
                            val funResType = mkTypeVar (generalisable, false, false);
                            val funArgType = mkTypeVar (generalisable, false, false);
                            val fType    = mkFunctionType (funArgType, funResType);
                        in
                            case unifyTypes (fType, funType) of
                                NONE => SOME(funArgType, funResType)
                            |   SOME report =>
                                (
                                    (* It's not a function. *)
                                    typeMismatch("Type error in function application.",
                                        valTypeMessage (lex, typeEnv) ("Function:", f, funType),
                                        valTypeMessage (lex, typeEnv) ("Argument:", arg, argType),
                                        PrettyString "Value being applied does not have a function type",
                                        lex, location, foundNear near);
                                    NONE
                                )
                        end

            in
                case fType of
                    NONE => badType (* Not a function *)
                |   SOME (fArg, fResult) =>
                    (
                        case unifyTypes (fArg, argType) of
                            NONE => ()
                        |   SOME report =>
                                typeMismatch("Type error in function application.",
                                    valTypeMessage (lex, typeEnv) ("Function:", f, funType),
                                    valTypeMessage (lex, typeEnv) ("Argument:", arg, argType),
                                    unifyErrorReport (lex, typeEnv) report, lex, location, foundNear near);
                        expType := fResult; (* Preserve for browsing. *)
                        fResult
                    )
            end

          | Cond {test, thenpt, elsept, location} =>
            let
                (* The test must be bool, and the then and else parts must be the
                   same. The result is either of these two once they have been
                   unified. *)
                val testType = assValues v test;
                val thenType = assValues v thenpt;
                val elseType = assValues v elsept;
            in
                case unifyTypes(testType, boolType) of
                    NONE => ()
                |   SOME report =>
                        typeWrong("Condition in if-statement must have type bool.",
                            valTypeMessage (lex, typeEnv) ("If:", test, testType),
                            unifyErrorReport (lex, typeEnv) report, lex, location, foundNear v);

                case unifyTypes(thenType, elseType) of
                    NONE => thenType (* or equally elseType *)
                |   SOME report =>
                    (
                        typeMismatch("Type mismatch between then-part and else-part.",
                            valTypeMessage (lex, typeEnv) ("Then:", thenpt, thenType),
                            valTypeMessage (lex, typeEnv) ("Else:", elsept, elseType),
                            unifyErrorReport (lex, typeEnv) report, lex, location, foundNear v);
                        badType
                    )
            end

          | TupleTree(ptl, _) =>
            (* Construct the type obtained by mapping "assignValue" onto
               each element of the tuple. *)
              mkProductType (map (assValues near) ptl) (* SPF 22/10/94 *)
          
          | Labelled {recList, frozen, expType, location, ...} =>
            let
                (* Process each item in the list. *)              
                fun labEntryToLabType {name, valOrPat, expType, ...} =
                let
                    val ty = assValues v valOrPat
                in
                    expType := ty;
                    {name = name, typeof = ty }
                end
            
              val expressionType =
                mkLabelled 
                  (sortLabels (map labEntryToLabType recList), frozen) (* should always be true *);
            in
                expType := expressionType;
                expressionType
            end

          | Selector {typeof, ...} =>
              typeof (* Already made. *)

          | ValDeclaration {dec, explicit, implicit, variables, ...} =>
                (assValDeclaration (dec, explicit, implicit, variables); badType (* Should never be used. *))

          | FunDeclaration {dec, explicit, implicit, ...} =>
                (assFunDeclaration(dec, explicit, implicit); badType (* Should never be used. *))

          | OpenDec{decs=ptl, variables, location, ...} =>
                let
                    (* Go down the list of names opening the structures. *)
                    (* We have to be careful because open A B is not the same as
                       open A; open B if A contains a structure called B. *)
                    (* We accumulate the values so that we can produce debugging
                       information if we need to.  Note: we have to be careful if
                       we have the same name in multiple structures. *)
                    val valTable = HashTable.hashMake 10
    
                    (* First get the structures... *)
                    fun findStructure ({name, location, ...}: structureIdentForm) = 
                        lookupStructure
                            ("Structure", {lookupStruct = #lookupStruct env}, name,
                                giveError (v, lex, location))
        
                    val strs : structVals list = map findStructure ptl;
                        
                    (* Value and substructure entries in a structure will generally have
                       "Formal" access which simply gives the offset of the entry within
                       the parent structure.  We need to convert these into "Select"
                       entries to capture the address of the base structure. *)
                    fun copyEntries str =
                    if isUndefinedStruct str then ()
                    else
                    let
                        val openLocs =
                        (* If we have a declaration location for the structure set this as the structure
                           location.  Add in here as the "open location". *)
                            case List.find (fn DeclaredAt _ => true | _ => false) (structLocations str) of
                                SOME (DeclaredAt loc) => [StructureAt loc, OpenedAt location]
                            |   _ => [OpenedAt location]

                        val sigTbl = structSignat str; (* Get the tables. *)
                        
                        (* We have to convert typeIds from Formal into Selected.  We need to do
                           this for all IDs in the structure we're opening and also any within
                           substructures if they use IDs in the parent, which is usually the case.
                           We may, though, have IDs that are local to substructures so we need to check. *)                        
                        val _ =
                            COPIER.fullCopySig 
                            (sigTbl,
                            {
                              enterType   =
                                fn (s,v) => #enterType env (s, v),
                              enterStruct =
                              fn (name, strVal) =>
                                    let
                                        val selectedStruct = 
                                            makeSelectedStruct (strVal, str, openLocs);
                                    in
                                        #enterStruct env (name, selectedStruct)
                                    end,
                              enterVal    =
                                fn (name, value) =>
                                    let
                                        (* If this is a datatype we could use findValueConstructor here
                                           to save constructing a new object.  It's probably not worth it. *)
                                        val selectedVar = 
                                            mkSelectedVar (value, str, openLocs);
                                    in
                                        HashTable.hashSet(valTable, name, selectedVar);
                                        #enterVal env (name, selectedVar)
                                    end
                            },
                            idFromStructure str,
                            (* Add the structure we're opening here to the types of
                               the values.  The name will be removed in messages if the type
                               constructor is in scope but if it has been redefined we can
                               get an identifiable name. *)
                            structName str^".");
                    in
                        ()
                    end
    
                    (* ...then put them into the name space. *)
                    val () = List.app copyEntries strs;
                in
                    variables := HashTable.hashFold valTable (fn _ => fn v => fn t => v :: t) [];
                    badType (* Does not return a type *)
                end
    
          | TypeDeclaration(tlist, _) =>
            let (* This is either a type abbreviation in the core language, in a structure
                   or in a signature or it is a type specification in a signaure. *)
                fun messFn(name, old, new) = 
                    errorNear (lex, true, v, declaredAt(tcLocations new),
                        name ^ " has already been bound in this declaration");
               
                val newEnv = noDuplicates messFn;
              
                (* First match all the types on the right-hand sides. *)
                fun processTypeBody (TypeBind {decType = SOME decType, ...}) =
                let
                    val t = typeFromTypeParse decType
                in
                    ptAssignTypes t v;
                    t
                end
                |   processTypeBody _ = emptyType (* Specification. *)
                
                val resTypes = List.map processTypeBody tlist;
              
                (* Can now declare the new types. *)
                fun processType (TypeBind {name, typeVars, isEqtype, nameLoc, ...}, decType) =
                let
                    (* Construct a type constructor which is an alias of the
                       right-hand side of the declaration.  If we are effectively
                       giving a new name to a type constructor we use the same type
                       identifier.  This is needed to check "well-formedness" in signatures. *)
                    val tcon =
                        if isEmpty decType
                        then (* Type specification *)
                        let
                            val description = { location = nameLoc, name = name, description = "" }
                        in
                            makeFrozenTypeConstrs (name, typeVars,
                                makeTypeId(isEqtype, description), 0, [DeclaredAt nameLoc])
                        end
                        else case typeNameRebinding(typeVars, decType) of
                            SOME typeId =>
                                makeFrozenTypeConstrs (name, typeVars,
                                    typeId, 0, [DeclaredAt nameLoc])
                        |   NONE =>
                                makeTypeAbbreviation (name, typeVars, decType, [DeclaredAt nameLoc]);
                in
                    checkForDots  (name, lex, nameLoc); (* Must not be qualified *)
                    #enter newEnv (name, tcon); (* Check for duplicates. *)
                    #enterType env  (name, tcon)  (* Put in the surrounding scope. *)
                end
                   
                val () = ListPair.app processType (tlist, resTypes);
            in
                badType (* Does not return a type *)
            end
        
          | DatatypeDeclaration 
              {typelist, withtypes, declist, ...} =>
                 assAbsData false typelist withtypes declist
    
          | AbstypeDeclaration 
              {typelist, withtypes, declist, ...} =>
                 assAbsData true typelist withtypes declist

          | DatatypeReplication{oldType, newType, oldLoc, newLoc, ...} =>
                  (* Adds both the type and the constructors to the
                   current environment. *)
              let
            (* Look up the type constructor in the environment. *)
                val oldTypeCons: typeConstrs =
                    lookupTyp 
                         ({lookupType = #lookupType env, lookupStruct = #lookupStruct env},
                          oldType,
                          giveError (near, lex, oldLoc));

                (* If the type name was qualified (e.g. S.t) we need to find the
                   value constructors from the same structure. *)
                val {first = namePrefix, ...} = splitString oldType;
                val baseStruct =
                    if namePrefix = ""
                    then NONE
                    else SOME(lookupStructure("Structure", {lookupStruct = #lookupStruct env},
                                namePrefix, giveError (v, lex, oldLoc)))

                (* Copy the datatype, converting any Formal constructors to Selected. *)
                val newTypeCons = mkSelectedType(oldTypeCons, newType, baseStruct, [DeclaredAt newLoc])

                val newValConstrs = tcConstructors newTypeCons
            in
                (* Check that it has at least one constructor. *)
                case newValConstrs of
                    [] => errorNear (lex, true, v, oldLoc, oldType ^ " is not a datatype")
                |    _ => ();
                (* Enter the value constrs in the environment. *)
                List.app (fn c => (#enterVal env) (valName c, c)) newValConstrs;
                (* Add this type constructor to the environment. *)
                (#enterType env) (newType, newTypeCons);
                badType (* Does not return a type *)
            end

          | (aList as List{elements, location, expType, ...}) =>
            let
                val elementType =
                    assignList(assValues v)("Elements in a list have different types.", "Item", ",", location, aList) elements
                val resType =
                    if isBadType elementType
                    then badType
                    else mkTypeConstruction ("list", listType, [elementType], [DeclaredAt inBasis])
            in
                expType := resType;
                resType
            end

          | Constraint {value, given, location} =>
            let
                val valType = assValues near value;
                val theType = typeFromTypeParse given
                val ()  = ptAssignTypes theType v;
            in
                (* These must be unifiable. *)
                case unifyTypes(valType, theType) of
                    NONE => () (* OK. *)
                |   SOME report =>
                        typeMismatch("Type mismatch in type constraint.",
                            valTypeMessage (lex, typeEnv) ("Value:", value, valType),
                            PrettyBlock(0, false, [],
                                [
                                    PrettyString "Constraint:",
                                    PrettyBreak(1, 0),
                                    display(theType, 10000 (* All of it *), typeEnv)
                                ]),
                            unifyErrorReport (lex, typeEnv) report,
                            lex, location, foundNear v);
                theType
            end

          | (aFun as Fn (ptl, location)) =>  (* Must unify the types of each of the alternatives.*)
              assignList(assValues aFun)("Clauses in fn expression have different types.", "Clause", "|", location, aFun) ptl

          | MatchTree {vars, exp, location, resType, argType} =>
            let 
              (* A match is a function from the pattern to the expression *)
              
              (* Process the pattern looking for variables. *)
        
               (* Construct a new environment for the variables. *)
              fun messFn(name, old, Value{locations, ...}) =  
                    errorNear (lex, true, v, declaredAt locations,
                        name ^ " has already been bound in this match");
              
              val newEnv   = noDuplicates messFn;
              val newLevel = level + 1;
              val decs     = processPattern(vars, #enter newEnv, newLevel, false, false)
        
              (* The identifiers declared in the pattern are available in the
                 body of the function. *)
              val bodyEnv =
                {
                  lookupVal     = lookupDefault (#lookup newEnv) (#lookupVal env),
                  lookupType    = #lookupType env,
                  lookupFix     = #lookupFix env,
                  lookupStruct  = #lookupStruct env,
                  lookupSig     = #lookupSig env,
                  lookupFunct   = #lookupFunct env,
                  lookupTvars   = #lookupTvars env,
                  enterVal      = #enterVal env,
                  enterType     = #enterType env,
                  enterFix      = #enterFix env,
                  enterStruct   = #enterStruct env,
                  enterSig      = #enterSig env,
                  enterFunct    = #enterFunct env
                };
        
              (* Now the body. *)
              val expType = assignValues(newLevel, letDepth, bodyEnv, v, exp);
            in
              resType := expType;
              argType := decs;
              (* Check the type of parameters to the function to make
                 sure they have not been unified with local datatypes.
                 We don't need to check the result type because the check
                 in "Localdec" will do that. *)
              checkForLocalDatatypes(decs, letDepth, giveError (v, lex, location));
              (* Result is a function from the type of the pattern to the type
                 of the body. This previously generalised the resulting type. Why? *)
              mkFunctionType (decs, expType)
            end (* MatchTree *)

          | Unit _ =>
              unitType

          | Localdec {decs, body, isLocal, varsInBody, location} =>
            let (* Local declarations or expressions. *)
              val newValEnv  = searchList();
              val newTypeEnv = searchList();
              val newStrEnv  = searchList();
              val newLetDepth = if isLocal then letDepth else letDepth+1;
              (* The environment for the local declarations. *)
              val localEnv =
                {
                   lookupVal     = lookupDefault (#lookup newValEnv)  (#lookupVal env),
                   lookupType    = lookupDefault (#lookup newTypeEnv) (#lookupType env),
                   lookupFix     = #lookupFix env,
                   (* This environment is needed if we open a 
                  structure which has sub-structures. *)
                   lookupStruct  = lookupDefault (#lookup newStrEnv) (#lookupStruct env),
                   lookupSig     = #lookupSig env,
                   lookupFunct   = #lookupFunct env,
                   lookupTvars   = #lookupTvars env,
                   enterVal      = #enter newValEnv,
                   enterType     = #enter newTypeEnv,
                  (* Fixity has already been dealt with in the parsing process.  The only reason
                     we deal with it here is to ensure that declarations are printed in the
                     correct order.  We simply need to make sure that local fixity declarations
                     are ignored. *)
                   enterFix      = fn _ => (),
                   enterStruct   = #enter newStrEnv,
                   enterSig      = #enterSig env,
                   enterFunct    = #enterFunct env
                };
        
              (* Process the local declarations and discard the result. *)
              val U : types = assignSeq localEnv newLetDepth decs;
        
              (* This is the environment used for the body of the declaration.
             Declarations are added both to the local environment and to
             the surrounding scope. *)
              val bodyEnv =
                { 
                  (* Look-ups come from the local environment *)
                  lookupVal     = #lookupVal localEnv,
                  lookupType    = #lookupType localEnv,
                  lookupFix     = #lookupFix localEnv,
                  lookupStruct  = #lookupStruct localEnv,
                  lookupSig     = #lookupSig localEnv,
                  lookupFunct   = #lookupFunct localEnv,
                  lookupTvars   = #lookupTvars localEnv,
                  enterVal      =
                    fn (pair as (name, v)) =>
                      (varsInBody := v :: ! varsInBody;
                       #enter newValEnv pair;
                       #enterVal env      pair),
                  enterType     =
                    fn (pair as (name, v)) =>
                      (#enter newTypeEnv pair;
                       #enterType env      pair),
                  enterFix      = #enterFix env,
                  enterStruct   =
                    fn (pair as (name, v)) =>
                      (#enter newStrEnv pair;
                       #enterStruct env   pair),
                  enterSig      = #enterSig env,
                  enterFunct    = #enterFunct env
                };
              (* Now the body, returning its result if it is an expression. *)
                val resType = assignSeq bodyEnv newLetDepth body
            in
                (* If this is a let expression we have to check that there
                   are no datatypes escaping. *)
                if isLocal then ()
                else checkForLocalDatatypes(resType, letDepth,
                        giveError (v, lex, location));
                resType
            end (* LocalDec *)

          | ExpSeq (ptl, _) =>
             (* A sequence of expressions separated by semicolons.
                Result is result of last expression. *)
              assignSeq env letDepth ptl

          | ExDeclaration(tlist, _) =>
            let
                fun messFn(name, _, line) =
                    errorNear (lex, true, v, line,
                        name ^ " has already been bound in this declaration");
         
                (* Construct an environment to check for duplicate declarations.
                   Include the declaration location as the value. *)
                val dupEnv = noDuplicates messFn;
  
                fun processException (ExBind {name, previous, ofType, value, nameLoc, ...}) =
                let
                    (* Fill in any types.  If there was no type given the exception has type exn
                       otherwise it has type ty->exn. *)
                    val oldType =
                        case ofType of
                            NONE => exnType
                        |   SOME typeof =>
                            let
                                val t = typeFromTypeParse typeof
                            in
                                ptAssignTypes t v;
                                mkFunctionType(t, exnType)
                            end
    
                    val exValue = 
                        case previous of 
                            EmptyTree => (* Generative binding. *)
                                mkEx (name, oldType, [DeclaredAt nameLoc])
                        |   Ident {name = prevName, value = prevValue, location, expType, ...} =>
                            let 
                                (* ex = ex' i.e. a non-generative binding? *)
                                (* Match up the previous exception. *)
                                val prev = 
                                    lookupValue 
                                        ("Exception",
                                            {lookupVal= #lookupVal env,
                                            lookupStruct= #lookupStruct env},
                                            prevName,
                                            giveError (v, lex, location))
                                val excType = valTypeOf prev
                            in
                                (* Check that it is an exception *)
                                case prev of
                                    Value{class=Exception, ...} => ()
                                |    _ => errorNear (lex, true, v, location, "(" ^ prevName ^ ") is not an exception.");
                                prevValue := prev; (* Set the value of the looked-up identifier. *)
                                expType := excType; (* And remember the type. *)
                                (* The result is an exception with the same type. *)
                                mkEx (name, excType, [DeclaredAt nameLoc])
                            end
                        | _ =>
                            raise InternalError "processException: badly-formed parse-tree"
                in
                    (* Save this value. *)
                    value := exValue;
        
                    (* In the check environment *)
                    #enter dupEnv (name, nameLoc);
        
                    (* Must not be qualified *)
                    checkForDots (name, lex, nameLoc) : unit;
                    (* Must not be "true", "false" etc. *)
                    checkForBuiltIn (name, v, lex, nameLoc, true) : unit;
        
                    (* Put this exception into the env *)
                    #enterVal env (name, exValue) 
                end
  
                val () = List.app processException tlist;
            in
                badType
            end (* ExDeclaration *)
        
          | Raise (pt, line) =>
            let
                val exType = assValues v pt
            in
                (* The exception value must have type exn. *)
                case unifyTypes(exType, exnType) of
                    NONE => ()
                |   SOME report =>
                        typeWrong("Exception to be raised must have type exn.",
                            valTypeMessage (lex, typeEnv) ("Raise:", pt, exType),
                            unifyErrorReport (lex, typeEnv) report, lex, line, foundNear v);
                (* Matches anything *)
                mkTypeVar (generalisable, false, false)
            end
  
        | (aHandler as HandleTree {exp, hrules, location}) =>
            let
                (* If the expression returns type E
                 the handler must be exn -> E *)
                val expType = assValues aHandler exp;
                (* Unify the handler with a function from exn -> expType *)
                val clauses =
                    assignList(assValues aHandler)
                        ("Clauses in handler have different types.", "Clause", "|", location, aHandler) hrules
                (* The result type of the handlers must match the result type of the expression being
                   handled and the arguments must all have type exn. *)
                val () = 
                    unify (clauses, mkFunctionType (exnType, expType), lex, location, foundNear v, typeEnv);
            in
              expType (* Result is expType. *)
            end

          | While {test, body, location} =>
            let
                val testType = assValues v test
            in
                (* Test must be bool. Result is unit *)
                case unifyTypes(testType, boolType) of
                    NONE => ()
                |   SOME report =>
                        typeWrong("Loop condition of while-expression must have type bool.",
                            valTypeMessage (lex, typeEnv) ("While:", test, testType),
                            unifyErrorReport (lex, typeEnv) report, lex, location, foundNear v);
                assValues v body; (* Result of body is discarded. *)
                unitType
            end

          | aCase as Case {test, match, location} =>
            let
                val funType =
                    assignList(assValues aCase)("Clauses in case have different types.", "Clause", "|", location, aCase) match;
                val argType = assValues aCase test;
            in
              (* The matches constitute a function from the test type to
                 the result of the case statement, so we apply the match type
                 to the test. *)
              apply (funType, argType, lex, location, foundNear aCase, typeEnv)
            end

          | anAndAlso as Andalso {first, second, location} =>
            let
                (* Both parts must be bool and the result is bool. *)
                val pairArgs = mkTupleTree([first, second], location)
                val argTypes  = assValues anAndAlso pairArgs;
                val boolStarBool = mkProductType[boolType, boolType]
                val () =
                    case unifyTypes(argTypes, boolStarBool) of
                        NONE => ()
                    |   SOME report =>
                            typeWrong("Arguments of andalso must have type bool*bool.",
                                valTypeMessage (lex, typeEnv) ("Arguments:", pairArgs, argTypes),
                                unifyErrorReport (lex, typeEnv) report, lex, location, foundNear anAndAlso)
            in
                boolType
            end

          | anOrElse as Orelse {first, second, location} =>
            let
                (* Both parts must be bool and the result is bool. *)
                val pairArgs = mkTupleTree([first, second], location)
                val argTypes  = assValues anOrElse pairArgs;
                val boolStarBool = mkProductType[boolType, boolType]
                val () =
                    case unifyTypes(argTypes, boolStarBool) of
                        NONE => ()
                    |   SOME report =>
                            typeWrong("Arguments of orelse must have type bool*bool.",
                                valTypeMessage (lex, typeEnv) ("Arguments:", pairArgs, argTypes),
                                unifyErrorReport (lex, typeEnv) report, lex, location, foundNear anOrElse)
            in
                boolType
            end

          | Directive { tlist, fix, ... } => 
                  (
                (* Infix declarations have already been processed by the parser.  We include
                   them here merely so that we get all declarations in the correct order. *)
                List.app (fn name => #enterFix env (name, fix)) tlist;
                badType
                )

          | WildCard _ => (* Should never occur in an expression. *)
                  raise InternalError "assignTypes: wildcard found"

          | Layered _ => 
                  raise InternalError "assignTypes: layered pattern found"

          | EmptyTree => 
                  raise InternalError "assignTypes: emptytree found"

          | Parenthesised(p, _) => assValues near p
                
            (* end of assValues *)

        and assValDeclaration (valdecs: valbind list, explicit, implicit, variables) =
        (* assignTypes for a val-declaration. *)
        let
            val newLevel = level + 1;
      
            (* Set the scope of explicit type variables. *)
            val () = #apply explicit(fn (_, tv) => setTvarLevel (tv, newLevel));

            (* For each implicit type variable associated with this value declaration,
               link it to any type variable with the same name in an outer
               scope. *)
            val () = 
                #apply implicit
                    (fn (name, tv) =>
                        case #lookupTvars env name of SOME v => linkTypeVars(v, tv) | NONE => setTvarLevel (tv, newLevel));
            (* If it isn't there set the level of the type variable. *)

            (* Construct a new environment for the variables. *)
            val newEnv =
                noDuplicates
                (fn(name, _, Value{locations, ...}) =>
                    errorNear (lex, true, v, declaredAt locations,
                        name ^ " has already been bound in this declaration"));

            (* This environment is those identifiers declared by recursive bindings *)
            val recEnv = searchList ();

            (* If this is a recursive declaration we will have to find all
               the variables declared by the patterns in each binding before
               we can look at the bodies of the bindings. For simplicity we
               process the patterns first even if this is not recursive but
               arrange for the variables to be added to the environment
               after rather than before processing the bodies. The result of
               processing the patterns is a list of their types. Each item
               in the list must be unified with the type of the
               corresponding body. *)

            (* Process the patterns. *)
            local
                fun mapProcess ([], isRec) = []
                |   mapProcess (RecValBind :: tlist, isRec) =
                        (* If we have  val x=1 and rec ... we will have an and-list
                           as the last element of the list. (It must be the last
                           because the inner and-list will swallow all the rest).
                           All those entries will be recursive. *)
                        mapProcess(tlist, true)

                |   mapProcess ((ValBind {dec,line,...}) :: ptl, isRec) =
                    let
                        fun enterVals (pair as (name, v)) =
                        (
                            #enter newEnv pair;
                            if isRec then #enter recEnv pair else ()
                        );
                                   
                        val patType = processPattern(dec, enterVals, newLevel, false, isRec);
                    in
                        patType :: mapProcess(ptl, isRec)
                    end;
            in
                val decs = mapProcess(valdecs, false)
            end

            (* Now the bodies. *)
            local
                (* Check that the types match by going down the list of value
                   bindings and the list of types produced from the patterns,
                   and matching corresponding types. *)
                fun checkTypes (patTypes, RecValBind :: valList, isRec) =
                    checkTypes (patTypes, valList, true)
          
                |   checkTypes (patType :: patTypes, (ValBind {dec, exp, line,...}) :: valList, isRec) =
                    let
                        val newEnv =
                        { (* If this is recursive we find the recursive names
                             and others in the surrounding scope. *)
                            lookupVal     = 
                                if isRec
                                then lookupDefault (#lookup recEnv) (#lookupVal env)
                                else #lookupVal env,
                            lookupType    = #lookupType env,
                            lookupFix     = #lookupFix env,
                            lookupStruct  = #lookupStruct env,
                            lookupSig     = #lookupSig env,
                            lookupFunct   = #lookupFunct env,
                            (* Extend the environment of type variables. *)
                            lookupTvars   =
                                lookupDefault (#lookup explicit)
                                    (lookupDefault (#lookup implicit) (#lookupTvars env)),
                            enterVal      = #enterVal env,
                            enterType     = #enterType env,
                            enterFix      = #enterFix env,
                            enterStruct   = #enterStruct env,
                            enterSig      = #enterSig env,
                            enterFunct    = #enterFunct env
                        }

                        val expType = assignValues(newLevel, letDepth, newEnv, exp, exp);
            
                        val () =
                            case unifyTypes(patType, expType) of
                                NONE => () (* OK*)
                            |   SOME report =>
                                    typeMismatch("Pattern and expression have incompatible types.",
                                        valTypeMessage (lex, typeEnv) ("Pattern:", dec, patType),
                                        valTypeMessage (lex, typeEnv) ("Expression:", exp, expType),
                                        unifyErrorReport (lex, typeEnv) report, lex, line, foundNear v)
        
                        (* true if the expression is a possibly-constrained fn-expression *)
                        fun isConstrainedFn (Constraint {value, ...}) = isConstrainedFn value
                        |   isConstrainedFn (Fn _)  = true
                        |   isConstrainedFn _       = false;
                    in
                        (* Must check that the expression is of the form FN match. *)
                        (* N.B. the code generator assumes this is true. *)
                        if isRec andalso not (isConstrainedFn exp)
                        then errorNear (lex, true, v, line, 
                            "Recursive declaration is not of the form `fn match'")
                        else ();
        
                        checkTypes(patTypes, valList, isRec)
                    end
          
                    |   checkTypes _ = ()
            in
                val () = checkTypes(decs, valdecs, false)
            end;

            (* Variables, constructors and fn are non-expansive.
               [] is a derived form of "nil" so must be included.
               Integer and string constants are also constructors but
               cannot involve imperative type variables. Constrained
               versions are also non-expansive.
               This has been extended and made more explicit in ML 97. *)
            fun nonExpansive (Fn _)   = true
            |   nonExpansive (Ident _) = true
            |   nonExpansive (List{elements = [], ...}) = true
            |   nonExpansive (List{elements, ...}) =
                    List.foldl (fn (v, a) => a andalso nonExpansive v) true elements
            |   nonExpansive (Constraint {value, ...}) = nonExpansive value
            |   nonExpansive (Literal _) = true
            |   nonExpansive (Unit _) = true
            |   nonExpansive (TupleTree(elems, _)) = 
                    List.foldl (fn (v, a) => a andalso nonExpansive v)
                            true elems
            |   nonExpansive (Labelled{recList, ...}) =
                    List.foldl (fn ({valOrPat, ...}, a) => a andalso nonExpansive valOrPat)
                            true recList (* Every element must be non-expansive *)
            |   nonExpansive (Applic{f, arg, ...}) =
                    isNonRefConstructor f andalso nonExpansive arg
            |   nonExpansive (Selector _) = true (* derived from fn {..} => ...*)
            |   nonExpansive (Parenthesised(p, _)) = nonExpansive p
            |   nonExpansive _       = false

            (* An application is non-expansive only if it is a, possibly
               constrained, constructor which is not ref. *)
            and isNonRefConstructor (Ident {value=ref v, name, ...}) =
                (* Rather than looking at the name it might be better to look
                   at the operation.  This is probably ok since we're not allowed
                   to rebind "ref". *)
                    isConstructor v andalso name <> "ref"
            | isNonRefConstructor (Constraint {value, ...}) =
                    isNonRefConstructor value
            | isNonRefConstructor (Parenthesised(p, _)) =
                    isNonRefConstructor p
            | isNonRefConstructor _ = false

            (* Now allow generalisation on the variables being declared.
               For imperative type variables we have to know whether the
               expression is expansive. *)
            fun allowGen (decs, []) = ()
            |   allowGen (decs, RecValBind :: tlist) = allowGen(decs, tlist)
            |   allowGen (d::dl, (ValBind {exp, line,...}) :: ptl) =
                (
                    allowGeneralisation 
                        (d, newLevel, nonExpansive exp, lex, line, foundNear v, typeEnv);
                    (* Check the type to make sure that a local datatype is
                       not escaping.  Checking here is really only needed in the
                       recursive case (where a recursive call inside the body
                       sets the type of the function) because the result will be
                       checked elsewhere. *)
                    checkForLocalDatatypes(d, letDepth, giveError (v, lex, line));
                    allowGen(dl, ptl)
                ) (* allowGen *)
        
            | allowGen _ =
                raise InternalError "allowGen: badly-formed parse-tree";
        
        in
            allowGen(decs, valdecs);
            (* And declare the new names into the surrounding environment. *)
            #apply newEnv (fn nv as (_, var) => (#enterVal env nv; variables := var :: !variables))
        end (* assValDeclaration *)

        and assFunDeclaration (tlist: fvalbind list, explicit, implicit) =
        (* Assigntypes for a fun-declaration. *)
        let
            val funLevel = level + 1; (* Level for function names. *)
      
            (* Set the scope of explicit type variables. *)
            val () =
                #apply explicit(fn (name, tv) => setTvarLevel (tv, funLevel));

            (* For each implicit type variable associated with this value declaration,
               link it to any type variable with the same name in an outer
               scope. *)
            val () = 
                #apply implicit
                  (fn (name, tv) =>
                      case #lookupTvars env name of SOME v => linkTypeVars(v, tv) | NONE => setTvarLevel (tv, funLevel));
            (* If it isn't there set the level of the type variable. *)

            (* Construct a new environment for the variables. *)
            fun msgFn(name, _, Value{locations, ...}) = 
                errorNear (lex, true, v, declaredAt locations,
                    name ^ " has already been bound in this declaration");
           
            val newEnv = noDuplicates msgFn;
           
            (* Since this is a recursive declaration we must get the function
               names first. Because of the way they are parsed they are hidden
               as applications of the function to one or more patterns. There
               may be more than one clause in a function binding but each
               should declare the same function and have the same number of
               patterns. We need to know the number of patterns and the
               function name in the third pass so we save them in the
               function binding. *)

            local
                fun findNameAndPatts (FValBind {clauses = (FValClause {dec, exp, line}::_), numOfPatts, functVar, ...}) =
                let
                    (* Just look at the first clause for the moment. *)
                    val { ident = { name, location, ... }, args, ... } = dec;
                    (* Declare a new identifier with this name. *)
                    val funVar =
                        mkVar (name, mkTypeVar (funLevel, false, false), [DeclaredAt location])

                    val arity = case dec of { args, ...} => List.length args
                    val () = numOfPatts := arity;
                    val () =
                        (* Put the results onto the function binding. *)
                        if arity = 0
                        then errorNear (lex, true, v, line,
                                "Clausal function does not have any parameters.")
                        else ()
                in
                    (* Must not be qualified *)
                    checkForDots (name, lex, line);
                    (* Must not be "true", "false" etc. but may be "it" *)
                    checkForBuiltIn (name, v, lex, line, false);
                    functVar := funVar; (* Save the variable. *)
                    (* Enter it in the environment. *)
                    #enter newEnv (name, funVar)
                end
                |   findNameAndPatts _ = raise InternalError "findNameAndPatts: badly-formed parse-tree";

            in
                val () = List.app findNameAndPatts tlist
            end;

            local
                (* Can now process the clausal functions in the environment 
                   of the function names and using the information about
                   function name and number of patterns we have saved. *)
                fun processBinding
                    (fvalBind as FValBind {clauses, numOfPatts, functVar=ref functVar, argType, resultType, location, ...}) =
                let
                    (* Each fun binding in the declaration may consist of several
                       clauses. Each must have the same function name, the same
                       number of patterns and a unifiable type. *)
                    (* The type information is built up from the bottom so that if there are
                       errors we can report them in the most appropriate place.
                       Build a type to be used for the function.  This will later be unified
                       with the type that we've already created for the function variable. *)
                    val funType = mkTypeVar(generalisable, false, false)

                    fun processClause (clause as FValClause {dec, exp, line}) =
                    let
                        val { ident = ident, args, constraint, ... } = dec
                        val clauseAsTree: parsetree =
                            (* This clause as a parsetree object for error messages. *)
                            mkFunDeclaration([mkClausal([clause], line)], explicit, implicit, line)
                        
                        val () = (* Set the type.  Only in case we look at the export tree. *)
                            #expType ident := valTypeOf functVar

                        fun messFn (name, _, Value{locations, ...}) =
                            errorNear (lex, true, clauseAsTree, declaredAt locations,
                                name ^ " has already been bound in this clause.");
                        (* Construct a new environment for the variables in the patts. *)
                        val varEnv = noDuplicates messFn;
                        val varLevel = funLevel + 1; (* Level for variables. *)

                        (* Process the patterns. *)
                        val argTypeList =
                            List.map (fn arg => processPattern(arg, #enter varEnv, varLevel, false, false))
                                args
                        (* This list is used for the type of the helper function. *)
                        val () = argType :=
                            (case argTypeList of
                                [] => badType (* error *)
                            |   [single] => single
                            |   multiple => mkProductType(List.rev multiple))

                        (* The identifiers declared in the pattern are available in the
                           body of the function. Since it is recursive the function
                           names are also available. *)
                        val bodyEnv =
                        { 
                            lookupVal     = 
                                lookupDefault (#lookup varEnv)
                                    (lookupDefault (#lookup newEnv) (#lookupVal env)),
                            lookupType    = #lookupType env,
                            lookupFix     = #lookupFix env,
                            lookupStruct  = #lookupStruct env,
                            lookupSig     = #lookupSig env,
                            lookupFunct   = #lookupFunct env,
                            (* Extend the environment of type variables. *)
                            lookupTvars   =
                                lookupDefault (#lookup explicit)
                                    (lookupDefault (#lookup implicit) (#lookupTvars env)),
                            enterVal      = #enterVal env,
                            enterType     = #enterType env,
                            enterFix      = #enterFix env,
                            enterStruct   = #enterStruct env,
                            enterSig      = #enterSig env,
                            enterFunct    = #enterFunct env
                        };
           
                        (* Now the body. *)
                        val expTyp = assignValues(varLevel, letDepth, bodyEnv, exp, exp);
                        (* Remember the result type for the debugger. Actually this
                           assigns the result type for each clause in the fun but
                           they'll all be the same. *)
                        val () = resultType := expTyp;
                        (* Check the expression type against any explicit type constraint. *)
                        val () =
                            case constraint of
                                NONE => ()
                            |   SOME given =>
                                let
                                    val theType = typeFromTypeParse given
                                    val ()  = ptAssignTypes theType v
                                in
                                    case unifyTypes(expTyp, theType) of
                                        NONE => () (* OK. *)
                                    |   SOME report =>
                                            typeMismatch("Body of fun binding does not match type constraint.",
                                                valTypeMessage (lex, typeEnv) ("Expression:", exp, expTyp),
                                                PrettyBlock(0, false, [],
                                                    [
                                                        PrettyString "Constraint:",
                                                        PrettyBreak(1, 0),
                                                        display(theType, 10000 (* All *), typeEnv)
                                                    ]),
                                                unifyErrorReport (lex, typeEnv) report,
                                                lex, line, foundNear clauseAsTree)
                                end
                        (* The type of this clause is a function type. *)
                        val clauseType = List.foldr mkFunctionType expTyp argTypeList
                        (* Unify this with the type we're using for the other clauses. *)
                        val () =
                            case unifyTypes(clauseType, funType) of
                                NONE => () (* OK. *)
                            |   SOME report =>
                                    typeMismatch("Type of clause does not match the type of previous clauses.",
                                        valTypeMessage (lex, typeEnv) ("Clause:", clauseAsTree, clauseType),
                                        PrettyBlock(0, false, [],
                                            [
                                                PrettyString "Other clauses:",
                                                PrettyBreak(1, 0),
                                                display(funType, 10000 (* All *), typeEnv)
                                            ]),
                                        unifyErrorReport (lex, typeEnv) report,
                                        lex, line, foundNear clauseAsTree)

                    in (* body of processClause *)
                        ()
                    end
                in (* body of processFun *)
                    List.app processClause clauses;
                    (* Finally unify the function type with the type of the function variable.  If the
                       variable has not yet been used that will simply set its type but if it has been
                       used recursively it may have been given an incompatible type. *)
                    case unifyTypes(funType, valTypeOf functVar) of
                        NONE => () (* OK. *)
                    |   SOME report =>
                        let
                            val fvalAsTree = mkFunDeclaration([fvalBind], explicit, implicit, location)
                        in
                            typeMismatch("Type of function does not match type of recursive application.",
                                valTypeMessage (lex, typeEnv) ("Function:", fvalAsTree, funType),
                                valTypeMessage (lex, typeEnv)
                                    ("Variable:", mkIdent(valName functVar, location), valTypeOf functVar),
                                unifyErrorReport (lex, typeEnv) report,
                                lex, location, foundNear fvalAsTree)
                        end
                end
            in
                val () = List.app processBinding tlist
            end;
      
        in
            (* Now declare the new names into the surrounding environment,
               releasing the copy flags on the type variables. All fun
               bindings are non-expansive. *)
            #apply newEnv 
                (fn (pair as (name, Value{typeOf, locations, ...})) =>
                let
                    val () =
                        allowGeneralisation(typeOf, funLevel, true, lex, declaredAt locations, foundNear v, typeEnv);
                    (* Check the type to make sure that a local datatype is
                       not escaping as a result of a recursive application of
                       the function to a local datatype. *)
                    val () = checkForLocalDatatypes(typeOf, letDepth, giveError (v, lex, declaredAt locations))
                in
                    #enterVal env pair
                end)
        end (* assFunDeclaration *)

        and assAbsData isAbs (typeList : datatypebind list) withtypes declist =
    let
      (* A type declaration causes a type to be entered in the type
         environment, together with some constructors. *)
      fun messFn (name, _, new) = 
        errorNear (lex, true, v, declaredAt(tcLocations new),
           name ^ " has already been bound in this declaration");

      val newEnv = noDuplicates messFn;
      
      (* datatype and abstype declarations are both recursive so we can
         enter the type names into the environment during a first pass,
         and then process the value constructors during a second. *)
        fun enterType(innerType, outerType, typeName) =
        (
            checkForDots  (typeName, lex, declaredAt(tcLocations innerType)); (* Must not be qualified *)
            #enter newEnv (typeName, innerType); (* Check for duplicates. *)
            #enterType env  (typeName, outerType)  (* and put in the enclosing scope *)
        );
       
       (* Make the type constructors and put them in a list. *)
        fun enterTcon (DatatypeBind {name, tcon, typeVars, nameLoc, ...}) =
        let
            (* Make a new ID.  If this is within a let declaration we always make
               a free ID because it is purely local and can't be exported. *)
            val description = { location = nameLoc, name = name, description = "" }
            
            val newId =
                if letDepth = 0
                then makeTypeId(false, description)
                else makeFreeIdEqUpdate (Local{addr = ref 0, level = ref 0}, false, description)
            val tc =
                makeDatatypeConstr(name, typeVars, newId,
                                   letDepth, [DeclaredAt nameLoc])
            val outerType =
                if isAbs
                then
                    (* Outside the with..end section the type is not a datatype.
                       The type constructor we enter in the outer environment
                       uses the same typeId but with the constructors removed. *)
                    makeFrozenTypeConstrs(name, typeVars, newId,
                                          letDepth, [DeclaredAt nameLoc])
                else tc
        in
            tcon := tc;
            enterType(tc, outerType, name);
            tc
        end
      
      val listOfTypes = map enterTcon typeList;

        (* First match all the types on the right-hand sides using the
            datatypes and the existing bindings. *)
        fun processType (TypeBind {decType = SOME decType, ...}) =
        let
            val t = typeFromTypeParse decType
        in
            ptAssignTypes t v;
            t
        end
        |   processType _ = emptyType

        val decTypes = List.map processType withtypes;

        (* Can now enter the `withtypes'. *)
        fun enterWithType (TypeBind {name, typeVars, nameLoc, ...}, decType) =
        let
            (* Construct a type constructor which is an alias of the
               right-hand side of the declaration. *)
            val tcon =
                makeTypeAbbreviation (name, typeVars, decType, [DeclaredAt nameLoc]);
        in
            enterType(tcon, tcon, name)
        end

        val () = ListPair.app enterWithType (withtypes, decTypes);
        
      (* For the constructors *)
      fun messFn (name, _, Value{locations, ...}) =
        errorNear (lex, true, v, declaredAt locations,
          name ^ " has already been used as a constructor in this type");
      
      val consEnv = noDuplicates messFn;
    
      (* Now process the types and generate the constructors. *)
      fun genValueConstrs (DatatypeBind {name, typeVars, constrs, nameLoc, ...}, typ) =
      let
        val numOfConstrs = length constrs;
        
        (* The new constructor applied to the type variables (if any) *)
        val resultType = mkTypeConstruction (name, typ, List.map TypeVar typeVars, [DeclaredAt nameLoc]);

        (* Sort the constructors by name.  This simplifies matching with
           datatypes in signatures. *)
        fun leq {constrName=xname: string, ...} {constrName=yname, ...} = xname < yname;
        val sortedConstrs = quickSort leq constrs;

        fun processConstr ({constrName=name, constrArg, idLocn, ...}, repn) =
        let
          val cons =
            case constrArg of
                NONE =>
                    mkGconstr (name, resultType, repn, true, numOfConstrs, [DeclaredAt idLocn])
            |   SOME argtype =>
                    mkGconstr (name, mkFunctionType (typeFromTypeParse argtype, resultType),
                        repn, false, numOfConstrs, [DeclaredAt idLocn]);
        
          (* Name must not be qualified *)
          val U : unit = checkForDots (name, lex, idLocn);
          (* Must not be "true", "false" etc. *)
          val U : unit = checkForBuiltIn (name, v, lex, idLocn, true) : unit;
          
          (* Put into the environment. *)
          val U : unit = #enter consEnv (name, cons);
        in    
          cons
        end (* processConstr *)

        (* Declares the constructors and return a list of them. *)
        fun decCons cs =
        let
            (* Match up identifiers to type constructors. *)
            (* This side-effects "argtype" to set equivalence fields *)
            (* Return entries for chooseConstrRepr which uses emptyType for nullary constrs. *)
            fun checkValConstr{constrName, constrArg=NONE, ...} = (constrName, emptyType)
            |   checkValConstr{constrName, constrArg=SOME t, ...} =
                let val ty = typeFromTypeParse t in ptAssignTypes ty v; (constrName, ty) end

            val constrTypes = List.map checkValConstr cs

            (* Choose the representation for the constructors. *)
            val reprs = chooseConstrRepr constrTypes
        in
            ListPair.map processConstr (cs,reprs)
        end; (* decCons*)

      in
        tcSetConstructors (typ, decCons sortedConstrs) : unit
      end (* genValueConstrs *)
      
      val U : unit = ListPair.app genValueConstrs (typeList, listOfTypes);


      (* Third pass - Check to see if equality testing is allowed for
         these types. *)
      val () = computeDatatypeEqualities listOfTypes;

      (* If this is a datatype declaration the value constructors should be
         entered in the surrounding scope, otherwise (abstract type
         declaration) we evaluate the declarations in an environment
         containing the value constructors, but the value constructors
         themselves do not appear in the surrounding scope. *)
      val U : unit =
        if not isAbs
        then #apply consEnv (#enterVal env)
        else let   (* Abstract type *)
          (* The declarations have to be evaluated in an environment in
             which the constructors have been declared. When an identifier
             is looked up it may:
               (a) be one of these new declarations, or else
               (b) be a constructor from the type declarations, or else
               (c) be outside the abstract type declaration.
             New declarations are entered in the local environment so that
             they can be found under (a) and in the surrounding environment
             where they will be available after this declaration. *)
          val decEnv =
        let
          val localEnv = searchList ();
          fun enterValFn (pair as (name, v)) = 
            (#enter localEnv pair;
             #enterVal env     pair);
          val lookupValFn = 
            lookupDefault (#lookup localEnv)
             (lookupDefault (#lookup consEnv) (#lookupVal env))
        in
           { 
             lookupVal     = lookupValFn,
             lookupType    = #lookupType env,
             lookupFix     = #lookupFix env,
             lookupStruct  = #lookupStruct env,
             lookupSig     = #lookupSig env,
             lookupFunct   = #lookupFunct env,
             lookupTvars   = #lookupTvars env,
             enterVal      = enterValFn,
             enterType     = #enterType env,
             enterFix      = #enterFix env,
             enterStruct   = #enterStruct env,
             enterSig      = #enterSig env,
             enterFunct    = #enterFunct env
          }
        end;
  
        in
            (* Process the declarations, discarding the result. *)
            assignSeq decEnv letDepth declist;
            ()
        end;
        in
      badType (* Does not return a type *)
    end (* assAbsData *)
      in 
        assValues near v
      end (* assignValues *);

      val Env gEnv = env
      
      val env = 
          {
            lookupVal     = #lookupVal gEnv,
            lookupType    = #lookupType gEnv,
            lookupFix     = #lookupFix gEnv,
            lookupStruct  = #lookupStruct gEnv,
            lookupSig     = #lookupSig gEnv,
            lookupFunct   = #lookupFunct gEnv,
            lookupTvars   = fn name => NONE,
            enterVal      = #enterVal gEnv,
            enterType     = #enterType gEnv,
            enterFix      = #enterFix gEnv,
            enterStruct   = #enterStruct gEnv,
            enterSig      = #enterSig gEnv,
            enterFunct    = #enterFunct gEnv
          };
    in
      assignValues(1, 0, env, v, v)
    end (* pass2 *);



(*****************************************************************************
                                PASS 3
                        Code Tree Generation
******************************************************************************)

          (* Generate code from ML parse tree. *)
  
    datatype environEntry = datatype DEBUGGER.environEntry
  
    local (* gencode *)

      (* Generates a block from a sequence of declarations/statements, unless
         there is only one, in which case it returns that one. *)
      fun mkblock []    = mkEnv [] (* Empty - generate ``void'' *)
        | mkblock [c]   = c
        | mkblock clist = mkEnv clist;

      val singleArg = mkLoad (~1, 0);

      (* Make a tuple out of a set of arguments or return the single
         argument if there is just one. *)
      fun mkArgTuple from downto =
      let (* Makes a list of arguments. *)
        fun mkArgList num =
          if num < downto then []
          else mkLoad (~ num, 0) :: mkArgList (num - 1);
      in
        if from = downto (* "tuple" is a singleton (SPF) *)
        then mkLoad (~ from, 0)
        else mkTuple (mkArgList from)
      end;
   
      (* Load args by selecting from a tuple. *)
      fun loadArgs size arg =
      let  (* Makes arguments by taking apart the tuple. *)
        fun mkargs num =
          if num = size then []
          else mkInd (num, arg) :: mkargs (num + 1);
      in
        if size = 1  (* "tuple" is a singleton (SPF) *)
        then [arg]
        else mkargs 0
      end;

     (* tupleWidth returns the width of a tuple or record or 1 if it
        isn't one.  It is used to detect both argument tuples and results.
        When used for arguments the idea is that frequently a tuple is
        used as a way of passing multiple arguments and these can be
        passed on the stack.  When used for results the idea is to
        create the result tuple  on the stack and avoid garbage collector
        and allocator time.  If we could tell that the caller was simply going
        to explode it we would gain but if the caller needed a
        tuple on the heap we wouldn't.  We wouldn't actually lose
        if we were going to create a tuple and return it but we
        would lose if we exploded a tuple here and then created
        a new one in the caller.
        This version of the code assumes that if we create a tuple
        on one branch we're going to create one on others which may
        not be correct. *)
     fun tupleWidth(TupleTree(l, _)) = List.length l

      |  tupleWidth(Labelled{expType=ref expType, ...}) =
           if recordNotFrozen expType (* An error, but reported elsewhere. *)
           then 1 (* Safe enough *)
           else recordWidth expType

      |  tupleWidth(Cond{thenpt, elsept, ...}) =
              let
                val w = tupleWidth thenpt
            in
                if w = 1
                then tupleWidth elsept
                else w
            end

      |  tupleWidth(Constraint{value, ...}) =
              tupleWidth value

      |  tupleWidth(HandleTree{exp, ...}) =
              (* Look only at the expression and ignore
               the handlers on the, possibly erroneous,
               assumption that they won't normally be
               executed. *)
              tupleWidth exp

      |  tupleWidth(Localdec{body=[], ...}) =
              raise InternalError "tupleWidth: empty localdec"
      |  tupleWidth(Localdec{body, ...}) =
              (* We are only interested in the last expression. *)
              tupleWidth(List.last body)

      |  tupleWidth(Case{match, ...}) =
              let
                fun getWidth(MatchTree{exp, ...}) = tupleWidth exp
                |    getWidth _ = raise InternalError "getWidth"
            in
                List.foldl(fn(v, 1) => getWidth v | (_, s) => s)
                          1 match
            end

      |  tupleWidth(Parenthesised(p, _)) = tupleWidth p

      |  tupleWidth _ = 1
    in
      (* Start of the code-generator itself. *)
      type debugenv = environEntry list * (int->codetree)
      
      (* Code generates the parse tree. `pt' is the tree of declarations,
         `valDeclarations' and `exDeclarations' are the lists of the value
         and exception variables which have been declared at the top level.
         `level' is the initial level which is only non-zero if we are
         declaring a functor. `Addresses' is a variable which contains the
         current address for top-level declarations. It has to be a parameter
         to `gencode' to handle structure declarations containing applications
         of functors. *)
      fun gencode (pt : parsetree, lex: lexan, debugEnv: debugenv, level, 
                   addresses, structName: string) : codetree list * debugenv =
      let
        fun mkAddr () = 
          let
            val addr = !addresses;
          in
            addresses := addr + 1;
            addr
          end;

        val level = ref level;  (* Incremented by one for each nested proc. *)
        val lastLine = ref 0 (* The last line for which we produced debug info. *)

        (* Debugging control and debug function. *)
        val debugging = getParameter debugTag (debugParams lex)
        
        val debuggerFun =
            case List.find (Universal.tagIs DEBUGGER.debuggerFunTag) (LEX.debugParams lex) of
                SOME f => Universal.tagProject DEBUGGER.debuggerFunTag f
            |   NONE => DEBUGGER.nullDebug

        (* Add a call to the debugger. *)
        fun addDebugCall (decName: string, (ctEnv, rtEnv), location) : codetree =
            let
                open DEBUGGER
                val currLine = #startLine location
                val debugger =
                    debugFunction(debuggerFun, DebugStep, decName, location) ctEnv
            in
                lastLine := currLine;
                mkEval(mkConst(toMachineWord debugger), [rtEnv(!level)], false)
            end

        (* Add a debug call if line has changed.  This is used between
           declarations and expression sequences to avoid more than one
           call on a line. *)
        fun changeLine (decName, env, loc) =
            if not debugging orelse #startLine loc = !lastLine then []
            else [addDebugCall(decName, env, loc)]

        fun createDebugEntry (v: values, (ctEnv, rtEnv), loadVal) =
            if not debugging
            then { dec = [], rtEnv = rtEnv, ctEnv = ctEnv }
            else let
                    val newEnv =
                    (* Create a new entry in the environment. *)
                          mkTuple [ loadVal (* Value. *), rtEnv(!level) ]
                    val { dec, load } = multipleUses (newEnv, mkAddr, !level)
                    val ctEntry =
                        case v of
                            Value{class=Exception, name, typeOf, locations, ...} =>
                                EnvException(name, typeOf, locations)
                        |   Value{class=Constructor{nullary, ofConstrs, ...}, name, typeOf, locations, ...} =>
                                EnvVConstr(name, typeOf, nullary, ofConstrs, locations)
                        |    Value{name, typeOf, locations, ...} =>
                                EnvValue(name, typeOf, locations)
                in
                    { dec = dec, rtEnv = load, ctEnv = ctEntry :: ctEnv}
                end

        (* Start a new static level.  This is currently used only to
           distinguish function arguments from the surrounding static
           environment. *)
        fun newDebugLevel (ctEnv, rtEnv) = (EnvStaticLevel :: ctEnv, rtEnv)

        fun makeDebugEntries (vars: values list, debugEnv: debugenv) =
             if debugging
            then
                let
                    fun loadVar (var, (decs, env)) =
                        let
                            val loadVal =
                                codeVal (var, !level, valTypeOf var, lex, nullLocation)
                            val {dec, rtEnv, ctEnv} =
                                createDebugEntry(var, env, loadVal)
                        in
                            (decs @ dec, (ctEnv, rtEnv))
                        end
                in
                    List.foldl loadVar ([], debugEnv) vars
                end
            else ([], debugEnv)

        (* In order to build a call stack in the debugger we need to know about
           function entry and exit.  It would be simpler to wrap the whole function
           in a debug function (i.e. loop the call through the debugger) but that
           would prevent us from using certain call optimisations. *)
        fun wrapFunctionInDebug(body: codetree, name: string, argCode: codetree,
                                argType: types, restype: types, location, (ctEnv, rtEnv)): codetree =
            if not debugging then body (* Return it unchanged. *)
            else
            let
                open DEBUGGER
                (* At the moment we can't deal with function arguments. *)
                fun enterFunction (rtEnv, args) =
                    debugFunction(debuggerFun, DebugEnter(args, argType), name, location) ctEnv rtEnv
                and leaveFunction (rtEnv, result) =
                    (debugFunction(debuggerFun, DebugLeave(result, restype), name, location) ctEnv rtEnv; result)
                and exceptionFunction (rtEnv, exn) =
                    (debugFunction(debuggerFun, DebugException exn, name, location) ctEnv rtEnv; raise exn)

                val entryCode = toMachineWord enterFunction
                and exitCode = toMachineWord leaveFunction
                and exceptionCode = toMachineWord exceptionFunction
                val ldexAddr = mkAddr ()
            in
                mkEnv [
                    (* Call the enter code. *)
                    mkEval(mkConst entryCode, [mkTuple[rtEnv(!level), argCode]], false),
                    (* Call the exit code with the function result. The
                       function is wrapped in a handler that catches all
                       exceptions and calls the exception code. *)
                    mkEval(mkConst exitCode,
                        [mkTuple[rtEnv(!level), mkHandle (body, [CodeZero (* all exceptions *)],
                            mkEnv
                                [
                                    (* Must save the exception packet first. *)
                                    mkDec(ldexAddr, Ldexc),
                                    mkEval(mkConst exceptionCode,
                                        [mkTuple[rtEnv(!level), mkLoad(ldexAddr, 0)]], false)])
                                ]
                        ], false)
                ]
            end
         
        (* datatype 'a option = SOME of 'a | NONE *)

        (* Convert a literal constant. We can only do this once any overloading
           has been resolved. *)
        fun getLiteralValue(converter, literal, instance, line, near): machineWord option =
            let
                val (conv, name) =
                     getOverloadInstance(valName converter, instance, true)
            in
                SOME(RunCall.unsafeCast(evalue conv) literal)
                    handle Match => NONE (* Overload error *)
                      | Conversion s =>
                              (
                              errorNear (lex, true, near, line,
                                      "Conversion exception ("^s^") raised while converting " ^
                                        literal ^ " to " ^ name);
                            NONE
                            )
                      | Overflow => 
                              (
                              errorNear (lex, true, near, line,
                                      "Overflow exception raised while converting " ^
                                    literal ^ " to " ^ name);
                            NONE
                            )
                      | _ =>
                              (
                              errorNear (lex, true, near, line,
                                      "Exception raised while converting " ^
                                    literal ^ " to " ^ name);
                            NONE
                            )
            end

        (* Devised by Mike Fourman, Nick Rothwell and me (DCJM).  First coded
           up by Nick Rothwell for the Kit Compiler. First phase of the match
           compiler. The purpose of this phase is to take a match (a set of
           patterns) and bring together the elements that will be discriminated
           by testing any particular part of the value.  Where a pattern is a
           tuple, for example, it is possible to discriminate on each of the
           fields independently, but it may be more efficient to discriminate
           on one of the fields first, and then on the others. The aim is to
           produce a set of tests that discriminate between the patterns 
           quickly. *)
           
        abstype patSet = PatSet of int list

        with           
          (* Each leaf in the tree contains a number which identifies the
             pattern it came from. As well as linking back to the patterns,
             these numbers represent an ordering, because earlier patterns
             mask out later ones. *)
          (* A set of pattern identifiers. *)
          val empty       = PatSet [];
          fun singleton i = PatSet [i];
          
          fun list (PatSet p) = p;
          
          infix 3 :::;
          
          fun a ::: b = PatSet (a :: list b);

          fun isEmpty (PatSet p) = null p;
          fun first   (PatSet p) = hd p; 
          fun next    (PatSet p) = PatSet (tl p); 
          
          (* Set from i to j inclusive. *)
          fun from i j = if i > j then empty else i ::: from (i + 1) j;
    
          fun stringOfSet p = 
            "[" ^ 
            List.foldl (fn (i, s) => s ^ " " ^ Int.toString i) "" (list p) ^ 
            "]";

          infix 3 plus;
          infix 4 inside;
          infix 5 intersect;
          infix 6 diff;
          infix 7 eq;
          infix 8 eqSc
          infix 9 neq;
          
              (* Union of sets. *)
          fun a plus b =
           if isEmpty a then b
           else if isEmpty b then a
           else if first a = first b then first a ::: (next a plus next b)
           else if first a < first b then first a ::: (next a plus b)
           else first b ::: (a plus next b);
    
              (* Set membership. *)
          fun i inside a =
            if isEmpty a then false
            else if i = first a then true
            else if i < first a then false
            else i inside next a; 
          
          (* Intersection of sets. *) 
          fun a intersect b =
            if isEmpty a orelse isEmpty b
              then empty
            else if first a = first b 
              then first a ::: ((next a) intersect (next b))
            else if first a < first b 
              then (next a) intersect b
            else a intersect next b;
    
          (* Set difference. *)
          fun a diff b =
            if isEmpty a 
              then empty
            else if isEmpty b
              then a
            else if first a = first b
              then (next a) diff (next b) 
            else if first a < first b
              then first a ::: ((next a) diff b)
            else a diff next b;
    
              (* Set equality. *)
          fun a eq b =
            if isEmpty a
               then isEmpty b
            else if isEmpty b
              then false
            else first a = first b andalso next a eq next b;
          
          fun a neq b = not (a eq b);
        
        end (* patSet *);

        datatype aot = 
          Aot of 
           { 
             patts:    aots,       (* Choices made at this point. *)
             defaults: patSet,     (* Patterns that do not discriminate on this node. *)
             width:    int,        (* For cons nodes the no. of constrs in the datatype. *)
             vars:     values list (* The variables bound at this point. *)
           }
                                
        and aots = 
          TupleField of aot list       (* Each element of the list is a field of the tuple. *)
        | Cons       of consrec list   (* List of constructors. *)
        | Excons     of consrec list   (* Exception constructors. *)
        | Scons      of sconsrec list  (* Int, char, string, real. *)
        | Wild                         (* Patterns that do not discriminate at all. *) 
  
        (* Datatype constructors and exception constructors. *)
        withtype consrec =
            {
              constructor: values, (* The constructor itself. *)
              patts: patSet,       (* Patterns that use this constructor *)
              appliedTo: aot       (* Patterns this constructor was applied to. *)
            } 
       
        and sconsrec =
            {
              eqFun:   codetree,    (* Equality functions for this type*)
              specVal: machineWord option,    (* The constant value. NONE here means we had
                                         a conversion error. *)
              patts:   patSet       (* Patterns containing this value. *)
            };
    
        fun makeAot patts defaults width vars =
          Aot 
            { 
              patts    = patts,
              defaults = defaults, 
              width    = width, 
              vars     = vars
            };
                                                    
        fun makeConsrec constructor patts appliedTo = 
            {
              constructor = constructor,
              patts       = patts, 
              appliedTo   = appliedTo
            };
                                                              
        fun makeSconsrec eqFun specVal patts : sconsrec =
            {
              eqFun    = eqFun,
              specVal  = specVal,
              patts    = patts
            };

                   
        fun aVars        (Aot         {vars,...})        = vars;
        
        (* An empty wild card - can be expanded as required. *)
        val aotEmpty = makeAot Wild empty 0 [];

        (* A new wild card entry with the same defaults as a previous entry. *)
        fun wild (Aot {defaults, ...}) = makeAot Wild defaults 0 [];
            
          (* Take a pattern and merge it into an andOrTree. *)
          fun buildAot vars (tree as Aot {patts = treePatts, 
                  defaults = treeDefaults, vars = treeVars, ...}) patNo line =
          let (* Add a default (wild card or variable) to every node in the tree. *)
            fun addDefault (Aot {patts, defaults, width, vars}) patNo =
            let
              fun addDefaultToConsrec {constructor, patts, appliedTo} =
                makeConsrec constructor patts (addDefault appliedTo patNo)
            
              val newPatts =
                case patts of
                  TupleField pl => 
                    TupleField (map (fn a => addDefault a patNo) pl)
                    
                | Cons cl =>
                    Cons (map addDefaultToConsrec cl)
                             
                | Excons cl => 
                    Excons (map addDefaultToConsrec cl)
                  
                | otherPattern => (* Wild, Scons *)
                    otherPattern;
            in
              makeAot newPatts (defaults plus singleton patNo) width vars
            end (* addDefault *);
           
            fun addVar (Aot {patts, defaults, width, vars}) var =
              makeAot patts defaults width (var :: vars);
    
            (* Add a constructor to the tree.  It can only be added to a
               cons node or a wild card. *)
            fun addConstr(cons, noOfConstrs, doArg, tree as Aot {patts, defaults, width, vars}, patNo) =
            let
              val consName = valName cons;
            in
              case patts of
                Wild =>
                let (* Expand out the wildCard into a constructor node. *)          
                  val cr = 
                    makeConsrec cons (singleton patNo) (* Expand the argument *) (doArg (wild tree));
                in
                  makeAot (Cons [cr]) defaults noOfConstrs vars
                end
              
            | Cons pl =>
              let
                (* Merge this constructor with other occurences. *)
                fun addClist [] = (* Not there - add this on the end. *)
                      [makeConsrec cons (singleton patNo) (doArg (wild tree))]
                      
                  | addClist (ccl::ccls) =
                    if valName (#constructor ccl) = consName
                    then (* Merge in. *)
                      makeConsrec 
                        cons 
                        (singleton patNo plus #patts ccl) 
                        (doArg (#appliedTo ccl))
                      :: ccls
                    else (* Carry on looking. *) ccl :: addClist ccls;
              in
                makeAot (Cons (addClist pl)) defaults width vars
              end
              
            | _ =>
              raise InternalError "addConstr: badly-formed and-or tree"
            
            end (* addConstr *);
  
                (* Add a special constructor to the tree.  Very similar to preceding. *)
            fun addSconstr eqFun cval (Aot {patts, defaults, width, vars}) patNo =
              case patts of
                 Wild =>  (* Expand out the wildCard into a constructor node. *)
                   makeAot
                    (Scons [makeSconsrec eqFun cval (singleton patNo)])
                    defaults 0 vars
                    
              | Scons pl =>
                let (* Must be scons *)
                    (* Merge this constructor with other occurrences. *)
                    fun addClist [] = (* Not there - add this on the end. *)
                        [makeSconsrec eqFun cval (singleton patNo)]
                    | addClist (ccl :: ccls) =
                        if (case (cval, #specVal ccl) of
                            (SOME a, SOME b) => structureEq(a, b)
                          | _ => false)
                        (* N.B. It is essential that this equality is done using
                           the general structure equality and NOT using an optimised
                           machineWord*machineWord->bool version.  The constants could be pointers
                           to strings or long integers. *)
                    then (* Merge in. *)
                        makeSconsrec eqFun cval (singleton patNo plus #patts ccl) :: ccls
                    else (* Carry on looking. *) ccl :: addClist ccls;
                in
                    makeAot (Scons (addClist pl)) defaults 0 vars
                end
        
              | _ =>
                raise InternalError "addSconstr: badly-formed and-or tree"
           (* end addSconstr *);
  
            (* Add an exception constructor to the tree.  Similar to the above
               except that exception constructors must be kept in order. *)
            fun addExconstr cons arg (Aot {patts, defaults, width, vars}) patNo =
            let
              val consName = valName cons;
            in
              case patts of
                Wild => (* Expand out the wildCard into a constructor node. *)
                let
                  val cr =
                    makeConsrec 
                      cons 
                      (singleton patNo)
                      (buildAot arg (wild tree) patNo line)
                in
                  makeAot (Excons [cr]) defaults 0 vars
                end
            
            
            | Excons (cl as (h::t)) =>
              let
              (* The exception constructor list is maintained in reverse order.
                 We have to be careful about merging exception constructors.
                 Two exceptions may have different names but actually have the
                 same exception value, or have the same (short) name but come
                 from different structures.  We only add to the last entry in
                 the list if we can tell that it is the same exception. We could
                 be more sophisticated and allow merging with other entries if
                 we could show that the entries we were skipping over were
                 definitely different, but it's probably not worth it. *)
                val newList = 
                  if isTheSameException (#constructor h, cons)
                  then 
                     makeConsrec cons ((singleton patNo) plus (#patts h))
                       (buildAot arg (#appliedTo h) patNo line) :: t
                  else
                     makeConsrec cons (singleton patNo)
                       (buildAot arg (wild tree) patNo line) :: cl;
              in
                makeAot (Excons newList) defaults 0 vars
              end
              
            | _ =>
              raise InternalError "addExconstr: badly-formed and-or tree"
              
            end (* addExconstr *);
          in (* body of buildAot *)
            case vars of 
              Ident {value=ref ident, ... } =>
                  (
                    case ident of
                        Value{class=Constructor {ofConstrs, ...}, ...} =>
                          (* Only nullary constructors. Constructors with arguments
                             will be dealt with by ``isApplic'. *)
                            addConstr(ident, ofConstrs,
                                fn a => buildAot (wildCard nullLocation) a patNo line, tree, patNo)
                    |    Value{class=Exception, ...} =>
                              addExconstr ident (wildCard nullLocation) tree patNo
                    |   _ => (* variable - matches everything. Defaults here and pushes a var. *)
                              addVar (addDefault tree patNo) ident
                )
    
            | TupleTree(ptl, location) => (* Tree must be a wild card or a tuple. *)
             (case treePatts of
                 Wild =>
                 let
                   val tlist =
                     map (fn el => buildAot el (wild tree) patNo location) ptl;
                 in
                  makeAot (TupleField tlist) treeDefaults 0 treeVars 
                 end

          | TupleField pl =>
                let (* Must be tuple already. *)
                (* Merge each field of the tuple in with the corresponding
                   field of the existing tree. *)
                fun mergel []       []     = [] (* Should both finish together *)
                  | mergel (t::tl) (a::al) = buildAot t a patNo line :: mergel tl al
                  | mergel _       _       = raise InternalError "mergel";
                val tlist = mergel ptl pl;
              in
                makeAot (TupleField tlist) treeDefaults 0 treeVars 
              end
          | _ => 
             raise InternalError "pattern is not a tuple in a-o-t")
  
            | Labelled {recList, frozen, expType=ref expType, location, ...} =>
              let
                (* Treat as a tuple, but in the order of the record entries.
                   Missing entries are replaced by wild-cards. The order of
                   the patterns given may bear no relation to the order in
                   the record which will be matched.
                   e.g. case X of (a = 1, ...) => ___ | (b = 2, a = 3) => ___ *)
        
                (* Check that the type is frozen. *)
                (* This check is probably redundant since we now check at the
                   point when we generalise the type (except for top-level
                   expressions - those could be detected in
                   checkForFreeTypeVariables).  Retain it for the moment.
                   DCJM 15/8/2000. *)
                val U =
                  if recordNotFrozen expType
                  then errorNear (lex, true, vars, location,
                          "Can't find a fixed record type.")
                  else ();
    
                (* Make a list of wild cards. *)
                fun buildl 0 = []
                  | buildl n = wildCard nullLocation :: buildl (n-1);
        
                (* Get the maximum number of patterns. *)
                val wilds = buildl (recordWidth expType);
    
                (* Now REPLACE entries from the actual pattern, leaving
                   the defaulting ones behind. *)
                (* Take a pattern and add it into the list. *)
                fun mergen (h :: t) 0 pat = pat :: t
                  | mergen (h :: t) n pat = h :: mergen t (n - 1) pat
                  | mergen []       _ _   = raise InternalError "mergen";
        
                fun enterLabel ({name, valOrPat, ...}, l) = 
                    (* Put this label in the appropriate place in the tree. *)
                    mergen l (entryNumber (name, expType)) valOrPat
              
                val tupleList = List.foldl enterLabel wilds recList;
              in
                 (* And process it as a tuple. *)
                 buildAot (TupleTree(tupleList, location)) tree patNo location
              end
  
            | Applic{f = Ident{value = ref applVal, ...}, arg, location, ...} =>
                (
                    case applVal of
                         Value{class=Constructor{ofConstrs, ...}, ...} =>
                            addConstr(applVal, ofConstrs,
                                fn atree => buildAot arg atree patNo location, tree, patNo)
    
                    |    Value{class=Exception, ...} => addExconstr applVal arg tree patNo

                    |    _ => tree (* Only if error *)
                )
    
            | Applic _ => tree (* Only if error *)

            | Unit _ =>
                (* There is only one value so it matches everything. *)
                addDefault tree patNo
              
            | WildCard _ =>
                addDefault tree patNo (* matches everything *)
              
            | List{elements, location, ...} =>
                let (* Generate suitable combinations of cons and nil.
                    e.g [1,2,3] becomes ::(1, ::(2, ::(3, nil))). *)
                    
                    fun processList [] tree = 
                        (* At the end put in a nil constructor. *)
                        addConstr(nilConstructor, 2,
                            fn a => buildAot (wildCard nullLocation) a patNo location, tree, patNo)
                    | processList (h :: t) tree = (* Cons node. *)
                        let
                            fun mkConsPat (Aot {patts = TupleField [hPat, tPat],
                                      defaults,  width, vars}) =  
                                let   (* The argument is a pair consisting of the
                                         list element and the rest of the list. *)
                                    val tlist = [buildAot h hPat patNo location, processList t tPat];
                                in
                                    makeAot (TupleField tlist) defaults 0 vars
                                end
                           | mkConsPat (tree  as Aot {patts = Wild, defaults,
                                                      width, vars}) =  
                                let
                                    val hPat  = wild tree;
                                    val tPat  = wild tree;
                                    val tlist = [buildAot h hPat patNo location, processList t tPat];
                                in
                                    makeAot (TupleField tlist) defaults 0 vars
                                end
                            | mkConsPat _ = 
                                raise InternalError "mkConsPat: badly-formed parse-tree"
                        in
                            addConstr(consConstructor, 2, mkConsPat, tree, patNo)
                        end
                    (* end processList *);
                in
                    processList elements tree
                end
  
              | Literal{converter, literal, expType=ref expType, location} =>
                let
                    (* At the same time we have to get the equality function
                       for this type to plug into the code.  Literals are overloaded
                       so this may require first resolving the overload to the
                       preferred type. *)
                    val constr = typeConstrFromOverload(expType, true)
                    val equality =
                        equalityForType(mkTypeConstruction(tcName constr, constr, [], []), !level)
                    val litValue: machineWord option =
                        getLiteralValue(converter, literal, expType, location, vars)
                in
                    addSconstr equality litValue tree patNo
                end
            
            | Constraint {value, location, ...} => (* process the pattern *)
                buildAot value tree patNo location
              
            | Layered {var, pattern, location} =>  (* process the pattern *)
              let  
                (* A layered pattern may involve a constraint which
                   has to be removed. *)
                fun getVar pat =
                  case pat of
                    Ident {value, ...}      => !value
                  | Constraint {value, ...} => getVar value
                  | _                       => undefinedValue (* error *);
              in
                addVar (buildAot pattern tree patNo location) (getVar var)
              end

            | Parenthesised(p, location) => buildAot p tree patNo location
    
            | _ =>
               tree (* error cases *)
          end; (* buildAot *)
  
          fun buildTree (patts: parsetree list) =
          let   (* Merge together all the patterns into a single tree. *)
            fun maket []     patNo tree = tree
              | maket ((MatchTree{vars, location, ...})::t) patNo tree =
                     maket t (patNo + 1) (buildAot vars tree patNo location)
              | maket _ _ _ =
                  raise InternalError "maket - badly formed parsetree"
          in
            maket patts 1 aotEmpty 
          end;
    
              (* Find all the variables declared by each pattern. *)
          fun findVars vars varl =
            case vars of
              Ident {value, ...} =>
              let
                val ident = ! value;
              in
                if isConstructor ident
                then varl (* Ignore constructors *)
                else ident :: varl
              end
              
            | TupleTree(ptl, _) =>
                List.foldl (fn (v1, v2) => findVars v1 v2) varl ptl
              
            | Labelled {recList, ...} =>
                List.foldl (fn ({valOrPat, ...}, v) => findVars valOrPat v) varl recList
              
             (* Application of a constructor: only the argument
                can contain vars. *)
            | Applic {f, arg, ...} =>
                findVars arg varl
              
            | List{elements, ...} =>
                List.foldl (fn (v1, v2) => findVars v1 v2) varl elements
              
            | Constraint {value, ...} =>
                findVars value varl
              
            | Layered {var, pattern, ...} =>
                 (* There may be a constraint on the variable
                    so it is easiest to recurse. *)
                findVars pattern (findVars var varl)

            | Parenthesised(p, _) =>
                findVars p varl
               
            | _ =>
                varl (* constants and error cases. *);
  
          val findAllVars =
              map (fn (MatchTree{vars, ...}) => findVars vars []
                      | _ => raise InternalError "findAllVars - badly formed parsetree");
  
          (* Put the arg into a local declaration and set the address of any
             variables to it. We declare all the variables that can be
             declared at this point, even though they may not be in different
             patterns. *)
          fun declareVars (tree : aot, arg : codetree, env: debugenv) 
              : {load: codetree, decs: codetree list, env: debugenv} =
          let
            val addressOfVar = mkAddr ();
            val dec = mkDec (addressOfVar, arg)
            and load = mkLoad (addressOfVar, 0)
            
            fun setAddr (v as Value{access=Local{addr=lvAddr, level=lvLevel}, ...}, (oldDec, oldEnv) ) =
            let (* Set the address of the variable to this and create
                   debug environment entries if required. *)
              val {dec=nextDec, ctEnv, rtEnv} = createDebugEntry(v, oldEnv, load)
            in
              lvAddr  := addressOfVar;
              lvLevel := !level;
              (oldDec @ nextDec, (ctEnv, rtEnv))
            end

            | setAddr _ = raise InternalError "setAddr"

            val (envDec, newEnv) = List.foldl setAddr ([], env) (aVars tree)

          in 
            {decs = dec :: envDec, load = load, env = newEnv}
          end;
  
         (* The code and the pattern from which it came, 0 if the default,
             ~1 if more than one pattern. This is used to remove redundant
             tests that are sometimes put in where we have a wild card above
             a constructor. *)
    
          type patcode = {code: codetree list, pat: int};

          fun makePatcode code pat : patcode = { code = code, pat = pat };
          val matchFailCode  : patcode = makePatcode [MatchFail] 0;

            local
                 (* Raises an exception. *)
                 (* TODO: Set the location of the exception rather than using
                    the null location. *)
                fun raiseException(exName, exIden, line) =
                    makeRaise (mkTuple [exIden, mkStr exName, CodeZero, codeLocation line]);
                (* Create exception values - Small integer values are used for
                   run-time system exceptions. *)
                val bindExceptionVal  = mkConst (toMachineWord EXC_Bind);
                val matchExceptionVal = mkConst (toMachineWord EXC_Match);
            in
                (* Raise match and bind exceptions. *)
                fun raiseMatchCode line : patcode =
                    makePatcode [raiseException("Match", matchExceptionVal, line)] 0
                and raiseBindCode line  : patcode =
                    makePatcode [raiseException("Bind", bindExceptionVal, line)] 0;
            end
 
          (* Code generate a set of patterns.  tree is the aot we are working
             on, arg is the code representing the argument to take apart.
             The set of patterns which are active are held in "active", and
             "othermatches" is a continuation of other patterns when we have
             done this one. "default" is the default code executed if no
             pattern matches and is needed only because of problems with
             exceptions. "isBind" is a flag indicating whether we are
             processing a variable binding. The set of patterns is needed
             primarily for tuples. If we have patterns like
               (A, A) => ..| (B,B) => ... | _ => ... 
             when we have tested that the first field is actually A we are
             only interested in patterns 1 and 3, so that testing for the
             second field being B is unnecessary in this case. Similarly
             when we test for the second field being B we can eliminate
             pattern 1. Actually this does not work properly for exceptions
             because of exception aliasing. e.g.
                X 1 => ... | Y _ => ... | _ => ...
             It is possible that X and Y might be the same exception, so that
             the fact that the constructor matches X does not imply that it
             cannot also match Y.  *)
          fun codePatt 
               (tree as Aot {patts, defaults, width, vars})
               (arg : codetree)
               (active : patSet)
               (othermatches : (patSet * (unit->patcode) * debugenv) -> patcode)
               (default : unit -> patcode)
               (isBind : bool)
               (debugEnv: debugenv)
               : patcode =
          let
            val decl : {load: codetree, decs: codetree list, env: debugenv} =
                declareVars (tree, arg, debugEnv);
            val load : codetree = #load decl;
            (* In several cases below we used "arg".  "arg" is the code used to
               create the value to be taken apart and may well involve several
               indirections.  I've changed them to use "load" since that avoids
               duplication of code.  It probably doesn't matter too much since the
               low level code-generator will probably optimise these anyway.
               DCJM 27/3/01. *)
            
            (* Get the set of defaults which are active. *)
            val activeDefaults : patSet = defaults intersect active;
     
            (* Code-generate a list of constructors. "constrsLeft" is the
               number of constructors left to deal with. If this gets to 1
               we have dealt with all the rest. *)
            fun genConstrs ([]:consrec list) constrsLeft = 
                 (* Come to the end without exhausting the datatype. *)
                  othermatches(activeDefaults, default, #env decl)
                  
              | genConstrs (p :: ps) constrsLeft =
              let
                (* If this is not in the active set we skip it. *)
                val newActive = (#patts p) intersect active;
              in
                (* If the set is empty we don't bother with this constructor. *)
                if newActive eq empty
                  then genConstrs ps constrsLeft (* N.B. NOT "(constrsLeft - 1)", since we haven't matched! *)
                else if constrsLeft = 1
                  then 
                   (* We have put all the other constructors in this
                      datatype out so there is no need to test for this case. *)
                    codePatt (#appliedTo p) (makeInverse (#constructor p, load, !level))
                       (newActive plus activeDefaults) othermatches default isBind (#env decl)
                else let (* Code generate the choice. *)
                  val testCode = makeGuard (#constructor p, load, !level);
                  
                  (* If it succeeds we have to take apart the argument of the
                     constructor. *)
                  val thenCode : patcode = 
                    codePatt (#appliedTo p) (makeInverse (#constructor p, load, !level))
                       (newActive plus activeDefaults)
                       othermatches default isBind (#env decl);
                       
                  (* Otherwise we look at the next constructor in the list. *)
                  val elseCode : patcode = genConstrs ps (constrsLeft - 1);
                in
                  (* 
                     If we are binding a pattern to an expression we have to
                     ensure that the variable bindings remain after the test
                     has returned.  To do this we change the test round so
                     that the else-part, which just raises an exception, is
                     done first, and the then-part is done after the test.
                     e.g. val (a::b) = e  generates code similar to if not
                     (e is ::) then raise Bind; val a = e.0; val b = e.1 
                     
                     Note: the reason bindings are treated differently is
                     that the then-part contains ONLY the matching code,
                     whereas for function-argument and exception-handler
                     matches, the then-part contains ALL the relevant code,
                     including the uses of any matched variables. This means
                     that we have to retain the bindings. The point about the
                     structure of an "if", is that merging the two paths through
                     the if-expression destroys any binding that were only made
                     in one half.
                     
                     SPF  25/11/96
                  *) 
                  if isBind
                    then makePatcode (mkIf (mkNot testCode, mkblock (#code elseCode), CodeNil) :: 
                                              #code thenCode) ~1
                  else if #pat thenCode = #pat elseCode andalso #pat thenCode >= 0
                    then elseCode (* This didn't actually do any discrimination,
                                      probably because a default was above a constructor. *)
                  else makePatcode [mkIf (testCode, mkblock (#code thenCode),
                                            mkblock (#code elseCode))] ~1
                end
              end (* genConstrs *);
              
              
              fun genExnConstrs ([]:consrec list)= 
                 (* Process the feualt matches, if any. *)
                  othermatches(activeDefaults, default, #env decl)
                  
              | genExnConstrs (p :: ps) =
              let
                (* If this is not in the active set we skip it. *)
                val newActive = (#patts p) intersect active;
              in
                (* If the set is empty we don't bother with this constructor. *)
                if newActive eq empty
                  then genExnConstrs ps
                else let (* Code generate the choice. *)
           (* Called if this exception constructor matches, but
              none of the active patterns match, either because
              the values in the datatype do not match (e.g. value
              is A 2, but pattern is A 1), or because of other
              fields in the tuple (e.g. value is (A, 2) but
              pattern is (A, 1)). If this were an ordinary
              constructor we would go straight to the default,
              because if it matches this constructor it could not
              match any of the others, but with exceptions it can
              match other exceptions, so we have to test them.
              
              We do this by generating MatchFail, which jumps
              to the "handler" of the enclosing AltMatch construct.
           *)
                  (* This doesn't work properly for bindings since the values we bind have to
                     be retained after this match.  However, this isn't really a problem.
                     The reason for using AltMatch is to avoid the code blow-up that used
                     to occur with complex matches.  That doesn't happen with bindings
                     because the elseCode simply raises a Bind exception.  DCJM 27/3/01. *)

                  (* If the match fails we look at the next constructor in the list. *)
                  val elseCode : patcode = genExnConstrs ps;

                  fun codeDefault () = 
                        if isBind then elseCode else matchFailCode;
                      
                  val testCode = makeGuard (#constructor p, load, !level);
                  
                  (* If it succeeds we have to take apart the argument of the
                     constructor. *)
                  val thenCode : patcode = 
                    codePatt (#appliedTo p) (makeInverse (#constructor p, load, !level))
                       newActive
                       othermatches codeDefault isBind (#env decl)
                       
                in
                  (* If we are binding a pattern to an expression we have to
                     ensure that the variable bindings remain after the test
                     has returned.  To do this we change the test round so
                     that the else-part, which just raises an exception, is
                     done first, and the then-part is done after the test.
                     e.g. val (a::b) = e  generates code similar to if not
                     (e is ::) then raise Bind; val a = e.0; val b = e.1 *) 
                   (* There was a bug here because the code used an AltMatch which
                      doesn't work properly if the elseCode makes bindings which
                      have to be retained after the AltMatch.  Since a binding can
                      only have a single pattern we don't need to use an AltMatch
                      here.  DCJM 27/3/01. *)
                  if isBind
                  then   
                     makePatcode
                       (mkIf (mkNot testCode, mkblock (#code elseCode), CodeNil):: #code thenCode)
                       ~1
            
                  (* Needed? *)
                  else if #pat thenCode = #pat elseCode andalso #pat thenCode >= 0
                    then elseCode
                    
                  else
                     makePatcode
                      [
                         mkAltMatch
                         (
                            mkIf (testCode, mkblock (#code thenCode), MatchFail),
                            mkblock (#code elseCode)
                         )
                      ]
                      ~1
                end
              end (* genExnConstrs *);
          
            (* Look at the kinds of pattern. - If there is nothing left
               (match is not exhaustive) or if all the active patterns will
               default, we can skip any checks. *)
            val pattCode = 
              if active eq empty orelse active eq activeDefaults
              then othermatches(active, default, #env decl)
              else case patts of
                TupleField [patt] =>
                    codePatt patt load (* optimise unary tuples - no indirection! *)
                      active othermatches default isBind (#env decl)
              
              | TupleField asTuples =>
                let
                    (* A simple-minded scheme would despatch the first column
                       and then do the others. The scheme used here tries to do
                       better by choosing the column that has any wild card
                       furthest down the column. *)
                  val noOfCols = length asTuples;
              
                  fun despatch colsToDo (active, def, env) =
                  let
                    (* Find the "depth" of pattern i.e. the position of
                    any defaults. If one of the fields is itself a
                    tuple find the maximum depth of its fields, since
                    if we decide to discriminate on this field we will
                    come back and choose the deepest in that tuple. *)
                    fun pattDepth (Aot {patts, defaults,...}) =
                      case patts of
                    TupleField pl =>
                     List.foldl (fn (t, d) => Int.max(pattDepth t, d)) 0 pl
                     
                      | _ =>
                    let (* Wild cards, constructors etc. *)
                      val activeDefaults = defaults intersect active;
                    in
                      if activeDefaults eq empty
                      then
                        (* No default - the depth is the number of
                           patterns that will be discriminated. Apart
                           from Cons which could be a complete match,
                           all the other cases will only occur
                           if the match is not exhaustive. *)
                        case patts of 
                          Cons   cl => length cl + 1
                        | Excons cl => length cl + 1
                        | Scons  sl => length sl + 1
                        | _         => 0 (* Error? *)
                      else first activeDefaults
                    end;
        
                    fun findDeepest column bestcol depth =
                      if column = noOfCols (* Finished. *)
                      then bestcol
                      else if column inside colsToDo
                      then let
                    val thisDepth = pattDepth (List.nth(asTuples, column));
                      in
                    if thisDepth > depth
                    then findDeepest (column + 1) column thisDepth
                    else findDeepest (column + 1) bestcol depth
                      end
                      else findDeepest (column + 1) bestcol depth;
                  in
                    (* If we have done all the columns we can stop. (Or if
                       the active set is empty). *)
                    if colsToDo eq empty orelse
                       active eq empty
                    then othermatches(active, def, env)
                    else let
                      val bestcol = findDeepest 0 0 0;
                    in
                      codePatt (List.nth(asTuples, bestcol)) (mkInd (bestcol, load)) active
                           (despatch (colsToDo diff (singleton bestcol)))
                           def isBind env
                    end
                  end (* despatch *);
                in
                  despatch (from 0 (noOfCols-1)) (active, default, #env decl)
                end (* TupleField. *)

              | Cons cl =>
                  genConstrs cl width
    
              | Excons cl =>
                  (* Must reverse the list because exception constructors are
                     in reverse order from their order in the patterns, and
                     ordering matters for exceptions. *)
                genExnConstrs (rev cl)
  
              | Scons sl =>
                 let (* Int, real, string *)
                
                  (* Generate if..then..else for each of the choices. *)
                  fun foldConstrs ([]: sconsrec list) =
                         othermatches(activeDefaults, default, #env decl)
                    | foldConstrs (v :: vs) =
                    let 
                     (* If this pattern is in the active set
                        we discriminate on it. *)
                      val newActive = (#patts v) intersect active;
          
                    in
                      if newActive eq empty
                      then (* No point *) foldConstrs vs
                      else let
                    val constVal =
                        case #specVal v of NONE => CodeZero | SOME w => mkConst w
                    val testCode =
                        mkEval(#eqFun v,
                               [mkTuple[constVal, load]], true)
                           
                    (* If it is a binding we turn the test round - see
                        comment in genConstrs. *)
                    val rest = 
                      othermatches(newActive plus activeDefaults, default, #env decl);
                    
                       (* If we have a handler of the form
                             handle e as Io "abc" => <E1> we will
                      generate a handler which catches all Io exceptions
                      and checks the argument. If it fails to match it
                      generates the other cases as explicit checks. The
                      other cases will generate a new address for "e"
                      (even though "e" is not used in them "declareVars"
                      does all).  We have to make sure that we
                      code-generate <E1> BEFORE we go on to the next
                      case. (i.e. we must call "othermatches" before
                      "foldConstrs"). *)  
                    val elsept = foldConstrs vs;
                      in
                    if isBind
                      then makePatcode (mkIf (mkNot testCode, mkblock (#code elsept),
                                              CodeNil) :: #code rest) ~1
                       (* Match or handler. *)
                    else if (#pat rest) = (#pat elsept) andalso (#pat rest) >= 0
                       then elsept
                    else makePatcode [mkIf (testCode, mkblock (#code rest),
                                mkblock (#code elsept))] ~1
                      end 
                    end (* foldConstrs *);
                in
                  foldConstrs sl
                end
              | _ =>  (* wild - no choices to make here. *)
                    othermatches(activeDefaults, default, #env decl)
          in 
            makePatcode (#decs decl @ #code pattCode) (#pat pattCode)
          end; (* codePatt *)
  
          (* Make an argument list from the variables bound in the pattern. *)
          fun makeArglist []        argno = []
            | makeArglist (Value{access=Local{addr=ref lvAddr, ...}, ...} :: vs) argno =
                mkLoad (lvAddr, 0) :: makeArglist vs (argno - 1) 
            | makeArglist _ argno = raise InternalError "makeArgList"
  
  
          (* Generate variable-bindings (declarations) for each of the
              expressions as functions. *)
          fun cgExps []  varl    base patNo uses decName debugEnv cgExpression lex near = []
            | cgExps (MatchTree {exp, location, ...} ::al) (vl::vll)
                    base patNo uses decName debugEnv cgExpression lex near =
              let
                val noOfArgs = length vl;
                val patNoIndex = patNo - 1;
                open Array
                val pattUses = uses sub patNoIndex;
                
                val U : unit =
                   if pattUses = 0
                   then errorNear (lex, false, near, location,
                          "Pattern " ^ Int.toString patNo ^ " is redundant.")
                   else ();
                
                val U = level := !level + 1; (* For the function. *)

                (* Set the addresses to be suitable for arguments.  At the
                   same time create a debugging environment if required. *)
                fun setAddr (v as Value{access=Local{addr=lvAddr, level=lvLevel}, ...},
                            (argno, oldDec, oldEnv)) =
                  let
                    val load = mkLoad (~argno, 0)
                    val {dec=nextDec, ctEnv, rtEnv} = createDebugEntry(v, oldEnv, load)
                  in
                    lvAddr  := ~argno;
                    lvLevel := !level;
                    (argno - 1, oldDec @ nextDec, (ctEnv, rtEnv))
                  end
                  | setAddr _ = raise InternalError "setAddr"
                  
                val (_, envDec, newEnv) = List.foldl setAddr (noOfArgs, [], debugEnv) vl
                
                val functionBody =
                    mkEnv(envDec @ [cgExpression (exp, newEnv, decName)]);

                val U = level := !level - 1; (* Back to the surroundings. *)
                
                (* Make it an inline function if it only used once. *)
                val theCode = 
                  (if pattUses = 1 then mkInlproc else mkProc)
                  (functionBody, !level, noOfArgs, decName ^ "/" ^ Int.toString patNo);
              in
                mkDec (base + patNoIndex, theCode) ::
                  cgExps al vll base (patNo + 1) uses decName debugEnv cgExpression lex near
              end
            | cgExps _ _ base patNo uses decName debugEnv cgExpression lex near = 
                raise InternalError "cgExps";
    
            fun codeMatch 
               (near : parsetree,
                alt : parsetree list,
                arg : codetree,
                lex : lexan,
                decName : string,
                debugEnv : debugenv,
                cgExpression : parsetree * debugenv * string -> codetree,
                isHandlerMatch : bool)
               : codetree =
            let
              val noOfPats  = length alt;
              val andortree = buildTree alt;
              val allVars   = findAllVars alt;
              val lineNo =
                  case alt of
                    MatchTree {location, ... } :: _ => location
                  | _ => raise Match
              
             (* Save the argument in a variable. *)
             val decCode   = multipleUses (arg, mkAddr, !level);
             
             (* Generate code to load it. *)
             val loadExpCode = #load decCode (!level);
             
             (* Generate a range of addresses for the expressions. *)  
             val baseAddr  = !addresses;  
             val U         = addresses := baseAddr + noOfPats;
              
             (* Make an array to count the number of references to a pattern.
                This is used to decide whether to use a function for certain
                expressions or to make it inline. *)
             val uses = Array.array (noOfPats, 0);
    
             (* Set to false if we find it is not exhaustive. *)
             val exhaustive = ref true;
             
             (* Make some code to insert at defaults. *)
             val codeDefault : unit -> patcode =
               if isHandlerMatch
               then (fn () => makePatcode [makeRaise loadExpCode] 0)
               else (fn () => (exhaustive := false; raiseMatchCode lineNo));
         
             (* Generate the code and also check for redundancy
                and exhaustiveness. *)
             val code : patcode = 
                codePatt andortree loadExpCode (from 1 noOfPats)
                  (fn (pattsLeft, default, env) => 
                    (* This function is called when we done all the discrimination
                       we can. We fire off the first pattern in the set. *)
                    if pattsLeft eq empty
                      then default ()
                    else let
                      val pattChosen = first pattsLeft;
                      val pattChosenIndex = pattChosen - 1; 
                    in
                      (* Increment the count for this pattern. *)
                      Array.update (uses, pattChosenIndex,(Array.sub(uses, pattChosenIndex)) + 1);
                      
                     (* If we have a single pattern it cannot be duplicated
                        so we can put the code in immediately, other cases
                        are made into inline functions and inserted later. *)
                     (* The idea is to avoid the code size blowing up if we
                        have a large expression which occurs multiple times in
                        the resulting code
                        e.g. case x of [1,2,3,4] => exp1 | _ => exp2
                        Here exp2 will be called at several points in the
                        code.  DCJM 13/2/01. *)
                      if noOfPats = 1
                      then
                      (
                          case alt of
                            MatchTree {exp, ... } :: _ =>
                               makePatcode [cgExpression (exp, env, decName)] pattChosen
                          | _ => raise InternalError "codeMatch - badly formed parsetree"
                      )
                      else let
                        val thisVars    = List.nth(allVars, pattChosenIndex);
                        val noOfArgs    = length thisVars;
                        val argsForCall = makeArglist thisVars noOfArgs;
                      in
                        (* Call the appropriate expression function. *)
                        makePatcode 
                          [mkEval 
                            (mkLoad (baseAddr + pattChosenIndex, 0),
                             argsForCall, false)]
                          pattChosen
                      end
                    end
                    )
                   codeDefault
                   false
                   debugEnv;
              (* Report inexhaustiveness if necessary. *)
              val U : unit = 
                if not (!exhaustive)
                then errorNear (lex, false, near, lineNo,
                                "Matches are not exhaustive.")
              else ();
            in
              if noOfPats = 1
              then
                (* Special case to speed up compilation. If we have
                    a single pattern we put the code in immediately. *)
                mkblock (#dec decCode @ #code code)
              else let
                (* Now generate the expressions as functions, inline
                   if only used once. Also checks for redundancy. *)
                val expressionFuns =
                    cgExps alt allVars baseAddr 1 uses decName debugEnv cgExpression lex near;
              in
                (* Return the code in a block. *)
                mkblock (#dec decCode @ (expressionFuns @ #code code))
              end
            end (* codeMatch *);
            
  
            (* Part of a val-binding. *)
            fun codeBind near decl exp lex line debugEnv =
            let
              (* Build a single pattern tree. *)
              val andortree = buildAot decl aotEmpty 1 line;
              
              (* Save the argument in a variable. *)
              val decCode   = multipleUses (exp, mkAddr, !level);
              
              (* Generate code to load it. *)
              val loadExpCode = #load decCode (!level);
              
              val exhaustive  = ref true;
              (* Set to false if we find it is not exhaustive. *)

              (* Make some code to insert at defaults. *)
              fun codeDefault () = (exhaustive := false; raiseBindCode line);
              
              (* Generate the code and also check for redundancy and exhaustiveness. *)
              val code : patcode =
                codePatt andortree loadExpCode (singleton 1)
                   (fn (pattsLeft, default, _) =>
                       if pattsLeft eq empty then (default ())
                       else makePatcode [] ~1
                    )
                   codeDefault
                   true
                   debugEnv;
              (* Report inexhaustiveness if necessary. *)
              val U : unit =
                if not (!exhaustive) andalso (!level) > 0
                then errorNear (lex, false, near, line, "Pattern is not exhaustive.")
                else ();
            in
              #dec decCode @ #code code  (* Return the code *)
            end (* codeBind *);
  
  
        (* Code-generates a sequence of declarations. "decName" is a string
           which is used to identify functions in profiles. "otherdecs" is
           a continuation for the rest of the block. It is needed to deal
           with cases such as let val a :: b = x; val ... in ... end,
           where the code is a test that only includes the rest of the
           declarations and the body if the test for "x" being a cons-cell
           is true. *)
           
        fun codeSequence (dlist: parsetree list, debugEnv: debugenv, decName: string)
             : codetree list * debugenv =
        let
          (* Makes a block from a series of alternatives in a match.
             Used only for functions. *)
          fun codeAlt 
             (near: parsetree,
              alt : parsetree list,
              arg : codetree,
              decName : string,
              debugEnv : debugenv)
              : codetree =
              let
                   (* Insert a call to the debugger in each arm of the match after
                    the variables have been bound but before the body. *)
                   fun cgExp (c: parsetree, debugEnv: debugenv, decName: string) =
                   let
                        val code = codegen(c, debugEnv, decName)
                   in
                        if not debugging
                        then code
                        else mkEnv[addDebugCall(decName, debugEnv, getLocation c), code]
                   end
              in
                codeMatch (near, alt, arg, lex, decName, debugEnv, cgExp, false)
              end

          (* Code-generates a piece of tree. *)
        and codegen (c: parsetree, debugEnv: debugenv, decName: string) : codetree =
        let
            fun codeList debugEnv [] = []
             |  codeList debugEnv (x::tl) =
                 (* Generate any line change code first, then this entry, then the rest. *)
                let
                    val lineChange = changeLine(decName, debugEnv, getLocation x)
                    val code = codegen (x, debugEnv, decName)
                in
                    lineChange @ code :: codeList debugEnv tl
                end
        in
            case c of 
              Ident {value, expType=ref expType, location, ...} =>
                  let
                    val v : values = !value;
                    (* The instance type is not necessarily the same as the type
                       of the value of the identifier. e.g. in the expression
                       1 :: nil, "::" has an instance type of
                       int * list int -> list int but the type of "::" is
                       'a * 'a list -> 'a list. *)
                  in
                      case v of
                        Value{class=Exception, ...} =>
                            codeExFunction (v, !level, expType, lex, location)
                    |    Value{class=Constructor _, ...} =>
                        let
                            (* When using the constructor as a value we just want
                               then second word. *)
                            val constrTuple = codeVal (v, !level, expType, lex, location)
                        in
                            mkInd(1, constrTuple)
                        end
                    |    _ => codeVal (v, !level, expType, lex, location)
                  end
          
              | Literal{converter, literal, expType=ref expType, location} =>
                (
                    case getLiteralValue(converter, literal, expType, location, c) of
                        SOME w => mkConst w
                      | NONE => CodeNil
                )

            | Applic {f, arg, location, ...} =>
              let
                (* The overloaded functions of more than one argument are
                   applied to their arguments rather than to a tuple. *)
                (* The only other optimisation we make is to remove applications
                   of constructors such as ``::'' which are no-ops. *)
                val argument : codetree = codegen (arg, debugEnv, decName);
              in
                (* If the function is an identifier then see if it is a global
                   constructor. If it is not then we must code-generate the
                   whole identifier, not the value it is bound to. *)
                case f of
                  Ident {value = ref function, expType=ref expType, ...} =>
                      applyFunction (function, argument, !level, expType, lex, location)
                | _ => 
                  mkEval (codegen (f, debugEnv, decName), [argument], false) (* not early *)
              end
  
            | Cond {test, thenpt, elsept, location} =>
                mkIf (codegen (test,   debugEnv, decName),
                      codegen (thenpt, debugEnv, decName),
                      codegen (elsept, debugEnv, decName))
  
            | TupleTree([pt], _) => (* can this occur? *)
                codegen (pt, debugEnv, decName) (* optimise unary tuples *)
  
            | TupleTree(ptl, _) =>
              let  (* Construct a vector of objects. *)
                val args = map (fn x => codegen (x, debugEnv, decName)) ptl;
              in
                mkTuple args : codetree
              end
  
            | Labelled {recList = [{valOrPat, ...}], ...} =>
                codegen (valOrPat, debugEnv, decName) (* optimise unary tuples *)
  
            | Labelled {recList, expType=ref expType, location, ...} =>
              let
                (* We must evaluate the expressions in the order they are
                   written. This is not necessarily the order they appear
                   in the record. *)
                val recordSize = length recList; (* The size of the record. *)
                
                (* First declare the values as local variables. *)
                (* We work down the list evaluating the expressions and putting
                   the results away in temporaries. When we reach the end we
                   construct the tuple by asking for each entry in turn. *) 
                fun declist [] look = 
                  let
                    val args = List.tabulate (recordSize, look);
                  in
                    [mkTuple args]
                  end
                  
                  | declist ({name, valOrPat, ...} :: t) look =
                  let
                    val thisDec = 
                      multipleUses (codegen (valOrPat, debugEnv, decName), mkAddr, !level);
                    
                    val myPosition = entryNumber (name, expType);
                    
                    fun lookFn i =
                      if i = myPosition
                      then #load thisDec (!level)
                      else look i
                  in
                    #dec thisDec @ declist t lookFn
                  end (* declist *)
                  in
                 (* Create the record and package it up as a block. *)
                mkEnv (declist recList (fn i => raise InternalError "missing in record"))  : codetree
              end
  
            | Selector {name, labType, location, ...} =>
              let
                (* Check that the type is frozen. *)
                val U =
                   if recordNotFrozen labType
                   then errorNear (lex, true, c, location, "Can't find a fixed record type.")
                   else ();

                val selectorBody : codetree =
                  if recordWidth labType = 1
                  then singleArg (* optimise unary tuples - no indirection! *)
                  else let
                    val offset : int = entryNumber (name, labType);
                  in
                    mkInd (offset, singleArg)
                  end
              in    (* Make an inline function. *)
                mkInlproc (selectorBody, !level + 1, 1, decName ^ "#" ^ name) : codetree
              end
  
            | Unit _ => (* Use zero.  It is possible to have () = (). *)
                CodeZero : codetree
  
            | List{elements, ...} =>
              let  (* Construct a list. *)
                (* At the end of the list put a "nil" *)
                fun consList []       = CodeZero
                  | consList (h :: t) =
                  let
                    val H = codegen (h, debugEnv, decName);
                    val T = consList t;
                  in
                    mkTuple [H,T]
                  end (* consList*);
              in
                consList elements : codetree
              end
    
            | Constraint {value, ...} =>
                (* code gen. the value *)
                codegen (value, debugEnv, decName) : codetree
  
            | Fn(_, location) =>
                (* Function *)
                mkblock (codeProc(c, decName, false, ref 0, location)) : codetree
   
            | Localdec {decs, body, ...} =>
              (* Local expressions only. Local declarations will be handled
                 by codeSequence.*)
              let 
                val (decs, newDebug) = codeSequence (decs, debugEnv, decName);
                val exps = codeList newDebug body;
              in
                mkblock (decs @ exps) : codetree
              end
  
            | ExpSeq(ptl, _) =>
              (* Sequence of expressions. Discard results of all except the
                 last. It isn't clear whether this will work properly since
                 the code-generator does not expect expressions to return
                 results unless they are wanted. It may be necessary to turn
                 all except the last into declarations. *)
                mkblock (codeList debugEnv ptl) : codetree
  
            | Raise (pt, location) =>
                let
                    val {dec, load} = 
                        multipleUses (codegen (pt, debugEnv, decName), mkAddr, !level);
                    val load = load(!level)
                in
                    (* Copy the identifier, name and argument from the packet and add this location. *)
                    makeRaise (
                        mkEnv(dec @
                            [mkTuple[mkInd(0, load), mkInd(1, load), mkInd(2, load), codeLocation location]]))
                end

            | HandleTree {exp, hrules, location, ...} =>
              (* Execute an expression in the scope of a handler *)
              let
                val handleExp : codetree = codegen (exp, debugEnv, decName);
                
                (* 
                   We only bother with matchTags because they allow
                   PolyML.exception_trace to be more discriminating as to where the
                   exception actually came from (by not bothering to match an exception
                   for handlers than only reraise it again). We don't actually need the tags
                   for pattern-matching because the actual handlers now contain all the
                   pattern-matching code that is actually required for correct execution.
                   If there's a default pattern, we just generate the single default tag
                   (CodeZero), since we'll always match one of the user-supplied patterns.
                   
                   Matching twice (in the RTS and in compiled code) is something of an overhead,
                   but I'm prepared to live with it for now - at least it meets the twin
                   aims of a working exception_trace combined with avoiding exponential code
                   blow-up (which the previous version didn't).
                   SPF 25/11/96.
                *)
                val matchTagList : codetree list =
                  case (buildTree hrules) of
                    Aot { patts = Excons exList, defaults, ...} =>
                      if defaults eq empty
                      then map (fn cons : consrec => 
                                   codeVal (#constructor cons, !level, emptyType, lex, location))
                             exList
                      else [CodeZero]
                  | _ => [CodeZero]
                  
                val handlerCode : codetree = 
                  codeMatch (c, hrules, Ldexc, lex, decName, debugEnv, codegen, true)
              in
                mkHandle (handleExp, matchTagList, handlerCode)
              end

            | While {test, body, ...} =>
                makeWhile 
                  (codegen (test, debugEnv, decName),
                   codegen (body, debugEnv, decName)) : codetree
  
            | Case {test, match, location} =>
              (* The matches are made into a series of tests and
                 applied to the test expression. *)
              let
                val testCode = codegen (test, debugEnv, decName)
              in
                codeMatch (c, match, testCode, lex, decName, debugEnv, codegen, false) : codetree
              end
    
            | Andalso {first, second, ...} =>
              (* Equivalent to  if first then second else false *)
                mkCand (codegen (first,  debugEnv, decName),
                        codegen (second, debugEnv, decName)) : codetree
  
            | Orelse {first, second, ...} =>
              (* Equivalent to  if first then true else second *)
                mkCor (codegen (first,  debugEnv, decName),
                       codegen (second, debugEnv, decName)) : codetree
    
            | Parenthesised(p, _) => codegen (p, debugEnv, decName)
 
            | _ => (* empty and any others *)
               CodeNil : codetree
  
          end (* codegen *)
  
           (* Generate a function either as a free standing lambda expression
               or as a declaration. *)
          and codeProc(c, decName, isRecursive, varAddr, location) =
          let
            fun getFnBody (exp : parsetree) : parsetree list = 
              case exp of
                Constraint {value, ...} => getFnBody value
                  | Fn(e, _)  => e
                  | _     => raise InternalError "getFnBody: not a constrained fn-expression";
          
            val f        = getFnBody c;
            val U        = level := !level + 1; (* This function comprises a new declaration level*)
            val oldAddr  = !addresses;
            val U        = addresses := 1;
            val (firstPat, resType, argType) = 
              case f of 
                MatchTree {vars, resType = ref rtype, argType = ref atype, ...} :: _  => (vars, rtype, atype)
              | _ => raise InternalError "codeProc: body of fn is not a clause list";

            val tupleSize = tupleWidth firstPat
          in
              if tupleSize <> 1
            then
            let
              (* If the first pattern is a tuple we make a tuple from the
                 arguments and pass that in. Could possibly treat labelled 
                 records in the same way but we have the problem of
                 finding the size of the record. *) 
   
              val newDecName : string = decName ^ "(" ^ Int.toString tupleSize ^ ")";
              val newDebugEnv = newDebugLevel debugEnv

              val argumentCode = mkArgTuple tupleSize 1
              val mainProc =
                 mkProc
                     (wrapFunctionInDebug
                       (codeAlt (c, f, argumentCode, newDecName, newDebugEnv),
                        newDecName, argumentCode, argType, resType, location, newDebugEnv), 
                    !level, tupleSize, newDecName);
                     
              (* Reset level and addresses *)
              val U = level := !level - 1;
              val U = addresses := oldAddr;
              
              (* Now make a block containing the procedure which expects
                 multiple arguments and an inline procedure which expects
                 a single tuple argument and calls the main procedure after
                 taking the tuple apart. *)
              val thisDec = multipleUses (mainProc, mkAddr, !level);
  
              val resProc = 
              let   (* Result procedure. *)
                val pr =
                  mkInlproc 
                   (mkEval
                      (#load thisDec (!level + 1),
                       loadArgs tupleSize singleArg, 
                       false),
                   !level + 1, 1, decName ^ "(1)");
              in
                if isRecursive then mkDec (!varAddr, pr) else pr
              end;
            in
              #dec thisDec @ [resProc]
            end
            
            else
             let (* Ordinary function. *)
              (* Must set the address to zero to get recursive references right. *)
              val addr = !varAddr;
              val U    = varAddr := 0; 
              val newDecName : string  = decName ^ "(1)";
              val newDebug = newDebugLevel debugEnv
              val alt  = codeAlt (c, f, mkLoad (~1, 0), newDecName, newDebug);
              (* If we're debugging add the debug info before resetting the level. *)
              val wrapped =
                  wrapFunctionInDebug(alt, newDecName, mkLoad (~1, 0), argType, resType, location, newDebug)
            in
              varAddr   := addr;        (* Reset the address *)
              level     := !level - 1;  (* Reset level and addresses *)
              addresses := oldAddr;
              let
                val pr = mkProc (wrapped, !level + 1, 1, newDecName);
              in
                [if isRecursive then mkDec (addr, pr) else pr]
              end
            end
          end (* codeProc *);

         in      (* codeSequence *)
           if null dlist then ([], debugEnv)
           else let
             val c : parsetree = hd dlist (* First in the list. *)

             val (firstDec, firstEnv) = 
               case c of 
                 FunDeclaration {dec = tlist, ...} =>
                 let
                (* Each function may result in either one or two functions
                   actually being generated. If a function is not curried
                   it will generate a single function of one argument, but
                   if it is curried (e.g. fun f a b = ...) it will
                   generate two mutually recursive functions. A function
                   fun f a b = X will be translated into
                   val rec f' = fn(a,b) => X and f = fn a => b => f'(a,b)
                   with the second function (f) being inline. This allows
                   the optimiser to replace references to f with all its
                   arguments by f' which avoids building unneccessary
                   closures. *)

                 (* These are recursive declarations so we must set the
                    address of each variable first. *)
                 local
                   fun setAddr (FValBind{
                                   functVar = ref (Value{access=Local{addr=lvAddr, level=lvLevel}, ...}), ...}) = 
                   let (* Set the addresses of the variables. *)
                     val addr  = mkAddr ();
                     val addr1 = mkAddr ();
                   in
                     lvAddr  := addr;
                     lvLevel := !level
                   end
                   |   setAddr _ = raise InternalError "setAddr"

                 in 
                   val () = List.app setAddr tlist;
                 end

                     (* Now we can process the function bindings. *)
                 fun loadFunDecs []               = []
                   | loadFunDecs ((FValBind{numOfPatts = ref numOfPats,
                                            functVar = ref var, clauses, argType = ref aType,
                                            resultType = ref resType, location, ...})::otherDecs) =
                   let
                     (* Make up the function, and if there are several mutually
                        recursive functions, put it in the vector. *)
                     val address   =
                         case var of
                            Value{access=Local{addr, ...}, ...} => addr
                        |    _ => raise InternalError "lvAddr"
                     val addr      = !address;
                     val procName  = decName ^ valName var;
                    (* Produce a list of the size of any tuples or labelled records
                       in the first clause. Tuples in the first clause are passed as
                       separate arguments. We could look at the other clauses and only
                       pass them as separate arguments if each clause contains a tuple.
                       
                       We can treat labelled records exactly like tuples here - we only
                       need to worry about the mapping from labels to tuple offsets
                       when we create the record (getting the order of evaluation right)
                       and in the pattern-matching code (extracting the right fields).
                       We don't have to worry about that here, because all we're doing
                       is untupling and retupling, taking care always to put the values
                       back at exactly the same offset we got them from.
                       SPF 19/12/96
                     *)
                     val tupleSeq : int list =
                         case clauses of
                             (FValClause{dec= { args, ...}, ...} :: _) => List.map tupleWidth args
                         | _ => raise InternalError "badly formed parse tree";

                     fun getResultTuple(FValClause{exp, ...}) = tupleWidth exp
    
                     val resultTuples =
                         List.foldl(fn(t, 1) => getResultTuple t  | (_, s) => s) 1 clauses

                     (* If we're debugging we want the result of the
                        function so we don't do this optimisation. *)
                     val resultTuple = if debugging then 1 else resultTuples

                     val extraArg = if resultTuple = 1 then 0 else 1

                     (* Count the total number of arguments needed. *)
                     val totalArgs = List.foldl (op +) extraArg tupleSeq 

                     (* The old test was "totalArgs = 1", but that's not really
                        right, because we could have one genuine arg plus a
                        lot of "()" patterns. We now use the normal inlining
                        mechanism to optimise this (unusual) case too.
                        SPF 19/12/96
                     *)
                     val noInlineFunction = numOfPats = 1 andalso totalArgs = 1
                   
                     (* If there is only one pattern and it is not a tuple we
                         generate only one function so we recurse directly. *)
                     val U : unit = if noInlineFunction then address := 0  else (); (* Marks a recursive call. *)
                     
                     (* This function comprises a new declaration level *)
                     val U : unit = level := !level + 1;
                     val oldAddr  = !addresses;
                     val U : unit = addresses := 1;
                     
                    (* Turn the list of clauses into a match. *)
                     val matches = 
                       map (fn FValClause {dec={ args, ...}, exp, line, ...} =>
                            mkMatchTree(if numOfPats = 1 then hd args else TupleTree(args, line), exp, line)
                            )
                        clauses;
                     
                     (* We arrange for the inner function to be called with
                        the curried arguments in reverse order, but the tupled
                        arguments in the normal order. For example, the
                        ML declaration:
                        
                         fun g a b c              = ... gives the order <c,b,a>
                         fun g (a, b, c)          = ... gives the order <a,b,c>
                         fun g (a, b) c (d, e, f) = ... gives the order <d,e,f,c,a,b>
                       
                       We want reverse the order of curried arguments to produce
                       better code. (The last curried argument often gets put
                       into the first argument register by the normal calling
                       mechanism, so we try to ensure that it stays there.)
                       We don't reverse the order of tupled arguments because
                       I'm still a bit confused about when a tuple is an
                       argument tuple (reversed?) and when it isn't (not reversed).
                       
                       Just to confuse matters further, the argument numbering
                       scheme is also reversed, so the first argument is actually
                       the highest numbered!
                       
                       For example: <d,e,f,c,a,b> is numbered <6,5,4,3,2,1>, so
                       we have to produce:
                       
                          <<2,1>, 3, <6, 5, 4>>
                          
                       as our list of loads in "argList".
                       
                       SPF 19/12/96
                     *)
                       
                     fun makeArgs []     _ = []
                       | makeArgs (h::t) n = 
                          mkArgTuple (n + h) (n + 1) :: makeArgs t (n + h);

                     val argList : codetree =
                       if numOfPats = 1
                       then mkArgTuple totalArgs (extraArg+1)
                       else mkTuple (makeArgs tupleSeq extraArg);

                     val innerProcName : string = 
                       concat ([procName,  "(" , Int.toString totalArgs, ")"]);

                     val codeMatches : codetree =
                       codeAlt (c, matches, argList, innerProcName, newDebugLevel debugEnv);

                     (* If the result is a tuple we try to avoid creating it by adding
                        an extra argument to the inline function and setting this to
                        the result. *)
                     val bodyCode =
                         if resultTuple = 1
                        then codeMatches
                        else
                            (* The function sets the extra argument to the result
                               of the body of the function.  We use the last
                               argument (addr = ~1) for the container so that
                               other arguments will be passed in registers in
                               preference.  Since the container is used for the
                               result this argument is more likely to have to be
                               pushed onto the stack within the function than an
                               argument which may have its last use early on. *)
                            mkSetContainer(mkLoad(~1, 0), codeMatches, resultTuple)

                     (* If we're debugging add the debug info before resetting the level. *)
                     val wrapped =
                         wrapFunctionInDebug(bodyCode, procName, argList,
                                             aType, resType, location, newDebugLevel debugEnv)
                     (* Reset level and addresses *)
                     val U = level := !level - 1;
                     val U = addresses := oldAddr;
        
                     val innerFun : codetree = mkProc (wrapped, !level + 1, totalArgs, innerProcName);
                          
                     (* We now have a function which can be applied to the
                        arguments once we have them. If the function is curried 
                        we must make a set of nested inline procedures which
                        will take one of the parameters at a time. If all the
                         parameters are provided at once they will be
                         optimised away. *)
  
                     (* Make into curried functions *)
                     fun makeFuns depth decName parms [] argCount =
                      (* Got to the bottom. - put in a call to the procedure. *)
                        if resultTuple = 1
                        then mkEval (mkLoad (addr + 1, numOfPats), parms, false)
                        else (* Create a container for the result, side-effect
                                     it in the function, then create a tuple from it.
                                     Most of the time this will be optimised away. *)
                            let
                                val {load, dec} =
                                    multipleUses(mkContainer resultTuple, mkAddr, !level)
                                val ld = load(!level)
                            in
                                mkEnv(dec @
                                   [mkEval (mkLoad (addr + 1, numOfPats),
                                               parms @ [ld], false),
                                    mkTupleFromContainer(ld, resultTuple)])
                            end
                         
                       | makeFuns depth decName parms (t::ts) argCount =
                       let (* Make a function. *)
                         (* This function comprises a new declaration level *)
                         val U       = level := !level + 1;
                         val oldAddr = !addresses;
                         val U       = addresses := 1;
                         
                         val newDecName : string = decName ^ "(1)";
                         
                         (* Arguments from this tuple precede older arguments,
                            but order of arguments within the tuple is preserved.
                            SPF 19/12/96
                         *) 
                         val nextParms = loadArgs t (mkLoad (~1, depth)) @ parms;
                            
                         val body =
                            makeFuns (depth - 1) newDecName nextParms ts (argCount + t);
                       in
                         (* Reset level and addresses *)
                         level := !level - 1;
                         addresses := oldAddr;
                         mkInlproc (body, !level + 1, 1, newDecName)
                       end (* end makeFuns *);

                     (* Reset the address of the variable. *)
                     val U : unit = address := addr;
                   in
                     if noInlineFunction
                       then mkDec (addr, innerFun) :: loadFunDecs otherDecs
                     else 
                       (* Return the `inner' procedure and the inline
                          functions as a mutually recursive pair. Try putting
                          the inner function first to see if the optimiser
                          does better this way. *)
                       mkDec (addr + 1, innerFun) :: 
                       mkDec (addr, makeFuns (numOfPats - 1) procName [] tupleSeq 0) ::
                         loadFunDecs otherDecs
                   end (* loadFunDecs *);
             val loaded = loadFunDecs tlist;

             (* Construct the debugging environment by loading all variables.
                This won't be available recursively in the
                functions but it will be in the rest of the scope. *)
             val vars = map (fn(FValBind{functVar, ...}) => !functVar) tlist
             val (decEnv, newDebugEnv) = makeDebugEntries(vars, debugEnv)
           in
             case loaded of
                 [singleton] => (singleton :: decEnv, newDebugEnv)
             |  _ => (* Put the declarations into a package of mutual decs. *)
                       (mkMutualDecs loaded :: decEnv, newDebugEnv)
           end (* FunDeclaration *)
  
           | ValDeclaration {dec = valDec, variables = ref vars, ...} =>
           let
             (* Recursive val declarations. *)
             fun codeRecursive []      = []
               | codeRecursive (RecValBind :: ds) =
                 (* e.g. val rec a = ... and rec b = ... *)
                     codeRecursive ds
               | codeRecursive ((ValBind{dec=vbDec, exp=vbExp, line, ...})::ds) =
               let
                 local
                   (* The pattern being declared may be a variable or a
                      constraint or (perversely) 
                      a layered pattern or a wild-card. *)
                   fun getVars (Constraint{value, ...}) = getVars value
                    |  getVars (Layered{var, pattern, ...}) =
                          getVars var @ getVars pattern
                    |  getVars (pat as (Ident _)) = [pat]
                    |  getVars (Parenthesised(p, _)) = getVars p
                    |  getVars _ = (* wild-card *) [];
                 in
                   val vars = getVars vbDec;
                 end
  
                 local
                   (* Set the addresses of the variables. *)
                   val addr = mkAddr ();
                 
                   (* Set all the variables for this declaration to the same
                      address, since they all refer to the same function.
                      There will normally be precisely one variable. *)
                   fun setAddress (
                           Ident{value = ref(Value{access=Local{addr=lvAddr, level=lvLevel}, ...}),...}) =
                       (
                         lvAddr  := addr;
                         lvLevel := !level
                       )
                       | setAddress _ =
                        raise InternalError "setAddress  - badly formed parsetree"
                 in
                   val () = List.app setAddress vars;
                 end;
                 
                 val rest = codeRecursive ds
               in 
                 (* Recursive declarations must be of the form var = fn ...
                    so the declaration part of the binding must be an
                    identifier pointing to a variable. *)
                    case vars of
                        [] => rest (* ignore it *)
                            (* Perverse but legal:  val rec _ = fn ... *)
                     |  (Ident{name = idname, value=ref idval, ...} :: _) =>
                         let
                        (* Normally precisely one identifier, but may be more
                             if layered. Just create  one - all the others have
                           the same address. *)
                            val lvAddr =
                                case idval of
                                    Value{access=Local{addr, ...}, ...} => addr
                                |    _ => raise InternalError "lvAddr";
                         in
                           (* Must be a function. This returns either a single
                              declaration or possibly a pair of mutually recursive
                              functions. *)
                           (codeProc(vbExp, idname, true, lvAddr, line)) @ rest
                         end
                     | _ => raise InternalError "ValDeclaration - not a variable"
               end
   
             (* Non-recursive val bindings.  Always called initially but will
                call codeRecursive as soon as it finds a "rec" to deal with
                the rest of the bindings. *)
             fun codeDecs []              = []
               | codeDecs (RecValBind :: otherDecs) =
                  let
                   (* Recursive - all the rest must be recursive. *)
                   val loaded = codeRecursive otherDecs;
                 in
                   (* If there is just one (may conceivably be none) *)
                   if null loaded orelse null (tl loaded)
                   then loaded (* Return the declarations (as a list) *)
                   else (* Put the declarations into a package of mutual decs. *)
                     [mkMutualDecs loaded]
                 end
               
               | codeDecs ((ValBind{dec=vbDec, exp=vbExp, line, ...})::otherDecs) =
               let (* A binding. *)
                 (* Codegen and push the declarations. For non-recursive
                    declarations, where a declaration may involve a pattern
                    with more than one variable, we need to process the
                    patterns to get the variables. *)
                     (* First the result of the expression is bound to a
                       variable. *)
                   
                 (* added to improve name generation SPF 18/10/94 *)    
                 fun getName (Ident {name, ...}) = name
                   | getName (Constraint {value, ...}) = getName value
                   | getName (Parenthesised(p, _)) = getName p
                   | getName (Layered {var=vbl, pattern, ...}) =
                     (
                         case vbl of
                            Ident {name, ...} =>
                                name (* could (perversely) be "_" *)
                          | _ => getName pattern
                     )
                   | getName _ = "<pattern>" (* give up *)
                       
                 val name =
                   (* Get the name of this declaration. *)
                   decName ^ getName vbDec ^ "-";

                 val decCode =
                     codeBind c vbDec (codegen (vbExp, debugEnv, name)) lex line debugEnv
               in
                  decCode @ codeDecs otherDecs
               end
             val decCode = codeDecs valDec

             (* Construct the debugging environment by loading all variables. *)
             val (decEnv, env) = makeDebugEntries (vars, debugEnv)
           in
              (decCode @ decEnv, env)
           end (* ValDeclaration *)
  
           | Localdec {decs, body, varsInBody=ref vars, ...} => (* Local declarations only *)
               let
                (* Simply process the declarations in sequence. *)
                val (decCode, decEnv) = codeSequence (decs, debugEnv, decName)
                val (bodyCode, bodyEnv) = codeSequence (body, decEnv, decName)
                (* We can't simply pass through the environment because it
                   would include the declarations in the local part.  Instead
                   we create a new environment here containing only the variables
                   in the in...dec part. *)
                val (decEnv, resEnv) = makeDebugEntries (vars, debugEnv)
            in
               (decCode @ bodyCode @ decEnv, resEnv)
            end
  
           | ExDeclaration(tlist, _) =>
             let
               fun codeEx (ExBind{value=ref exval, previous, ... }) =
                 let
                   val ex     = exval;
                   (* This exception is treated in the same way as a local
                      variable except that the value it contains is created
                      by generating a word on the heap. The address of this word
                      constitutes a unique identifier. Non-generative exception
                      bindings i.e. exception ex=ex'  merely copy the word from
                      the previous exception. *)
                   val (lvAddr, lvLevel, exType) =
                      case ex of
                          Value{access=Local{addr, level}, typeOf, ...} => (addr, level, typeOf)
                       | _ => raise InternalError "lvAddr"
                 in
                   lvAddr  := mkAddr ();
                   lvLevel := !level;
                   
                   mkDec 
                     (! lvAddr,
                      case previous of
                          EmptyTree => 
                            (* Generate a new exception. This is a single
                               mutable word which acts as a token. It is a
                               mutable to ensure that there is precisely one
                               copy of it. It contains a function to print values
                               of the type so when we raise the exception we can print
                               the exception packet without knowing the type. *)
                            mkExIden (exType, !level)
                      | Ident{value=ref prevVal, location, ...} =>
                              (* Copy the previous value. N.B. We want the exception
                               identifier here so we can't call codegen. *)
                            codeVal (prevVal, !level, emptyType, lex, location)
                      | _ => raise InternalError "codeEx"
                     )
                 end  (* codeEx *);

                 val exdecs = map codeEx tlist

                 fun getValue(ExBind{value=ref exval, ...}) = exval
                 val (debugDecs, newDebugEnv) =
                     makeDebugEntries(map getValue tlist, debugEnv)
               in 
                 (exdecs @ debugDecs, newDebugEnv)
               end (* ExDeclaration *)
  
            |   AbstypeDeclaration {typelist, declist, ...} =>
                let (* Code-generate the declarations. *)
                    (* The debugging environment for the declarations should include
                       the constructors but the result shouldn't.  For the moment
                       ignore the constructors. *)
                    val (code, newDebug) = codeSequence (declist, debugEnv, decName);
                    val typeIds =
                        List.map(fn (DatatypeBind {tcon = ref tc, ...}) => tc) typelist
                in
                    (createDatatypeFunctions(typeIds, mkAddr, !level) @ code, newDebug)
                end
  
            |   DatatypeDeclaration {typelist, ...} =>
                   (* We just need the value constructors for the debug env.
                      Later we will include the types as well. *)
                let
                    fun getConstrs(DatatypeBind {tcon = ref tc, ...}, (decs, debugEnv)) =
                    let
                        val (newDecs, newDebug) = makeDebugEntries(tcConstructors tc, debugEnv)
                    in
                        (decs @ newDecs, newDebug)
                    end

                    val (decs: codetree list, newDebugenv: debugenv) =
                        List.foldl getConstrs ([], debugEnv) typelist
                    val typeIds =
                        List.map(fn (DatatypeBind {tcon = ref tc, ...}) => tc) typelist
                in
                    (createDatatypeFunctions(typeIds, mkAddr, !level) @ decs, newDebugenv)
                end

           | OpenDec {variables=ref vars, ...} =>
                   (* All we need to do here is make debugging entries. *)
              makeDebugEntries(vars, debugEnv)

           | _ => (* c is Directive or TypeDeclaration*)
              ([], debugEnv); 

            val (decRest, finalEnv) = codeSequence (tl dlist, firstEnv, decName)
         in    (* Append the remaining declarations. *)
           (firstDec @ decRest, finalEnv)
         end
       end (* codeSequence *);
     in
       codeSequence ([pt], debugEnv, structName)
     end (* gencode *)
   end (* type *)

    (* Types that can be shared. *)
    structure Sharing =
    struct
        type lexan      = lexan
        and  pretty     = pretty
        and  environEntry = environEntry
        and  codetree   = codetree
        and  types      = types
        and  values     = values
        and  typeId     = typeId
        and  structVals = structVals
        and  typeConstrs= typeConstrs
        and  typeVarForm=typeVarForm
        and  env        = env
        and  fixStatus  = fixStatus
        and  structureIdentForm = structureIdentForm
        and  typeParsetree = typeParsetree
        and  parsetree  = parsetree
        and  valbind    = valbind
        and  fvalbind   = fvalbind
        and  fvalclause = fvalclause
        and  typebind   = typebind
        and  datatypebind=datatypebind
        and  exbind     = exbind
        and  labelRecEntry=labelRecEntry
        and  ptProperties = ptProperties
    end

end (* PARSETREE *);
