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
    Title:      Parse Tree Structure and Operations.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

functor PARSE_TREE (

(*****************************************************************************)
(*                  LEX                                                      *)
(*****************************************************************************)
structure LEX :
sig
  type lexan;
  type prettyPrinter;

  val errorProc:     lexan  * int * (prettyPrinter -> unit) -> unit;
  val warningProc:   lexan  * int * (prettyPrinter -> unit) -> unit;
  val filename: lexan -> string;

end;

(*****************************************************************************)
(*                  CODETREE                                                 *)
(*****************************************************************************)

(* This tree represents the intermediate code between the parse tree and
   the final machine code *)

structure CODETREE :
sig
  type word
  type codetree

  val CodeNil:       codetree
  val CodeZero:      codetree
  val Ldexc:         codetree
  val MatchFail:     codetree
  val mkAltMatch:    codetree * codetree -> codetree
  val mkConst:       word -> codetree
  val mkLoad:        int * int -> codetree
  val mkInd:         int * codetree -> codetree
  val mkProc:        codetree * int * int * string -> codetree
  val mkInlproc:     codetree * int * int * string -> codetree
  val mkEval:        codetree * codetree list * bool -> codetree
  val mkStr:         string   -> codetree
  val mkNot:         codetree -> codetree
  val mkCand:        codetree * codetree -> codetree
  val mkCor:         codetree * codetree -> codetree
  val mkRaise:       codetree -> codetree
  val mkDec:         int * codetree -> codetree
  val mkIf:          codetree * codetree * codetree -> codetree
  val mkWhile:       codetree * codetree -> codetree
  val mkEnv:         codetree list -> codetree
  val mkTuple:       codetree list -> codetree
  val mkHandle:      codetree * codetree list * codetree -> codetree
  val mkMutualDecs:  codetree list -> codetree
  val mkContainer:   int -> codetree
  val mkSetContainer: codetree * codetree * int -> codetree
  val mkTupleFromContainer: codetree * int -> codetree

  val multipleUses:  codetree * (unit -> int) * int -> {load: int -> codetree, dec: codetree list}
  val evalue:           codetree -> word;
  val structureEq:      word * word -> bool
end;


(*****************************************************************************)
(*                  STRUCTVALS                                               *)
(*****************************************************************************)
structure STRUCTVALS :
sig
  (* Structures *)

  type word; (* added 23/7/94 SPF *)
  type structVals;
  type signatures;
  type functors;
  type fixStatus;
  type typeId;
  type typeConstrs;
  type typeDependent;
  type codetree
        
  val undefinedStruct:    structVals;
  val isUndefinedStruct:  structVals -> bool;
  val makeSelectedStruct: structVals * structVals -> structVals;
  val structSignat:       structVals -> signatures;
 
  (* Signatures *)
  type univTable
  val sigTab: signatures -> univTable;

  (* Types *)
  val makeVariableId: unit -> typeId;
  val isVariableId:   typeId -> bool;
  val unifyTypeIds:   typeId * typeId -> bool;
  val sameTypeId:     typeId * typeId -> bool;
  
  type 'a possRef
     
  (* A type is the union of these different cases. *)
  type typeVarForm
  datatype types = 
    TypeVar of typeVarForm
    
  | TypeConstruction of (* typeConstructionForm *)
      {
        name:  string,
        value: typeConstrs possRef,
        args:  types list
      }

  | FunctionType of (* functionTypeForm *)
    { 
      arg:    types,
      result: types
    }
  
  | LabelledType  of (* labelledRecForm *)
    { 
      recList: { name: string, typeof: types } list,
      frozen: bool,
	  genericInstance: typeVarForm list
    }

  | OverloadSet	  of (* overloadSetForm *)
  	{
		typeset: typeConstrs list
	}

  | BadType
  
  | EmptyType
  ;

  datatype valAccess =
  	Global   of codetree
  | Local    of { addr: int ref, level: int ref }
  | Selected of { addr: int,     base:  structVals }
  | Formal   of int
  | Overloaded of typeDependent (* Values only. *)

  and values =
  	Value of {
		name: string,
		typeOf: types,
		access: valAccess,
		class: valueClass }

  (* Classes of values. *)
  and valueClass =
  	  SimpleValue
	| Exception
	| Constructor of { nullary: bool }

  val tcName:            typeConstrs -> string;
  val tcEquivalent:      typeConstrs -> types;
  val tcSetConstructors: typeConstrs * values list -> unit;
  val tcTypeVars:        typeConstrs -> types list;
  val tcSetEquality:     typeConstrs * bool -> unit;
  val tcConstructors:    typeConstrs -> values list;
  val makeTypeConstrs:
  	string * types list * types * typeId *  bool * int -> typeConstrs;

  val isConstructor: values -> bool;
  val isUndefinedValue: values -> bool;

  val listType: typeConstrs;
  val undefType: typeConstrs;
  
  val generalisable: int;

  (* Values. *)

  val undefinedValue: values;
  val valName: values -> string
  val valTypeOf: values -> types

  val TypeDep: typeDependent
  val makeOverloaded:    string * types * typeDependent -> values;

  datatype env = Env of
    {
      lookupVal:    string -> values,
      lookupType:   string -> typeConstrs,
      lookupFix:    string -> fixStatus,
      lookupStruct: string -> structVals,
      lookupSig:    string -> signatures,
      lookupFunct:  string -> functors,
      lookupTvars:  string -> types,
      enterVal:     string * values      -> unit,
      enterType:    string * typeConstrs -> unit,
      enterFix:     string * fixStatus   -> unit,
      enterStruct:  string * structVals  -> unit,
      enterSig:     string * signatures  -> unit,
      enterFunct:   string * functors    -> unit
    };

        
  type 'a tagMethods;
  val structVar:     structVals  tagMethods;
  val valueVar:      values      tagMethods;
  val typeConstrVar: typeConstrs tagMethods;

end;

(*****************************************************************************)
(*                  TYPETREE                                                 *)
(*****************************************************************************)
structure TYPETREE :
sig
  type types;
  type values;
  type lexan;
  type prettyPrinter;
  type typeConstrs;

  val unify: types * types * lexan * int * (prettyPrinter -> unit) -> unit;
  val apply: types * types * lexan * int * (prettyPrinter -> unit) -> types;

  val allowGeneralisation: types * int * bool * lexan *
  						   int * (prettyPrinter -> unit) -> unit;
  val generalise: types * bool -> types;
  (* Check for a local datatype "escaping".  Added for ML97. *)
  val checkForLocalDatatypes: types * int * (string -> unit) -> unit;

  val mkProductType:      types list -> types;
  val mkTypeVar:          int * bool * bool * bool -> types;
  val mkTypeConstruction: string * typeConstrs * types list -> types;
  val mkFunctionType:     types * types -> types;
  val mkLabelled:         {name: string, typeof: types } list * bool -> types;
  val mkLabelEntry:       string * types -> {name: string, typeof: types };
  val sortLabels:         {name: string, typeof: types } list * (string -> unit) ->
  								{name: string, typeof: types } list;
  val entryNumber:        string * types -> int;
  val recordNotFrozen:    types -> bool;
  val recordWidth:        types -> int;

  (* Unify two type variables which would otherwise be non-unifiable. *)
  val linkTypeVars: types * types -> unit;
  val setTvarLevel: types * int -> unit;

  (* Gets the lists of constructors from a type. *)
  val getConstrList: types -> values list;

  (* Second pass on type identifiers which associates type constructors with names. *)
  val assignTypes: types * (string -> typeConstrs) * lexan * int -> unit;

  (* Print out a type structure. *)
  val display: types * int * prettyPrinter -> unit;

  (* A list of type variables. *)
  val displayTypeVariables: types list * int * prettyPrinter -> unit;
  
  (* Standard types. *)
  val boolType:   types;
  val intType:    types;
  val charType:   types; (* added 22/8/96 SPF *)
  val stringType: types;
  val realType:   types;
  val unitType:   types;
  val exnType:    types;
  val mkOverloadSet:	  typeConstrs list -> types;

  val genEqualityFunctions: typeConstrs list * (string -> unit) * bool -> unit;

end;

(*****************************************************************************)
(*                  VALUEOPS                                                 *)
(*****************************************************************************)
structure VALUEOPS :
sig
  type word;
  type types;
  type codetree
  type values;
  type structVals;
  type lexan;
  type prettyPrinter;
  type typeConstrs;
  type fixStatus;

  val chooseConstrRepr : (string*types) list -> codetree list

  val overloadType:	  values * bool -> types

  (* Construction functions. *)
  val mkVar:         string * types -> values;
  val mkSelectedVar: values * structVals -> values;
  val mkGconstr:     string * types * codetree * bool -> values;
  val mkEx:          string * types -> values;

  val mkSelectedType: typeConstrs * string * structVals option -> typeConstrs

  (* Standard values *)
  val nilConstructor:  values;
  val consConstructor: values;
  
  (* Print values. *)
  val displayFixStatus:  fixStatus * int * prettyPrinter -> unit;

  (* Code-generate. *)
  val mkExIden:       unit -> codetree
  val codeVal:        values * int * types * lexan * int *
			{ lookupFix: string -> fixStatus, lookupExnById: word -> values option} -> codetree
  val codeExFunction: values * int * types * lexan * int -> codetree
  val applyFunction:  values * codetree * int * types * lexan * int *
			{ lookupFix: string -> fixStatus, lookupExnById: word -> values option} -> codetree
  val getOverloadInstance: string * types * bool * lexan * int -> codetree*string
  val isTheSameException: values * values -> bool
  val raiseBind:      codetree;
  val raiseMatch:     codetree;
  val makeGuard:      values * codetree * int -> codetree 
  val makeInverse:    values * codetree * int -> codetree;
 
  (* Look-up functions. *)

  val lookupStructure:  string * {lookupStruct: string -> structVals} * 
                        string * (string -> unit) -> structVals
                                           
  val lookupValue:   string * {lookupVal: string -> values, lookupStruct: string -> structVals} * 
                     string * (string -> unit) -> values
                                
  val lookupTyp:   {lookupType: string -> typeConstrs,
                    lookupStruct: string -> structVals} * 
                   string * (string -> unit) -> typeConstrs
end;

(*****************************************************************************)
(*                  UTILITIES                                                *)
(*****************************************************************************)
structure UTILITIES :
sig
  type lexan;

  val noDuplicates: (string -> unit) -> 
                       { apply: (string * '_a -> unit) -> unit,
                         enter:  string * '_a -> unit,
                         lookup: string -> '_a };
    
  val searchList: unit -> { apply: (string * '_a -> unit) -> unit,
                            enter:  string * '_a -> unit,
                            lookup: string -> '_a };

  val checkForDots:  string * lexan * int -> unit;

  val splitString: string -> { first:string,second:string }
end;

(*****************************************************************************)
(*                  UNIVERSALTABLE                                           *)
(*****************************************************************************)
structure UNIVERSALTABLE:
sig
  type universal;
  type univTable;
  
  val univFold: univTable * (string * universal * 'a -> 'a) * 'a -> 'a;
end;

(*****************************************************************************)
(*                  DEBUG                                                    *)
(*****************************************************************************)
structure DEBUG :
sig
  val errorDepth: int ref;
  val ml90: bool ref
  val debug: bool ref
end;

(*****************************************************************************)
(*                  MISC                                                    *)
(*****************************************************************************)
structure MISC :
sig
  (* These are handled in the compiler *)
  exception Conversion of string;     (* string to int conversion failure *)
  exception ValueMissing of string;  (* table lookup failure *)
  
  (* This isn't handled at all (except generically) *)
  exception InternalError of string; (* compiler error *)

  val quickSort : ('a -> 'a -> bool) -> 'a list -> 'a list;
  
  val forall  : ('a -> unit) -> 'a list -> unit;
  val revfold : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b;
  
  val isNil  : 'a list -> bool;
  val nth : 'a list -> int -> 'a;
  
  val max : int -> int -> int;
  
  type 'a iter;
  val upto        : int -> int -> int iter;
  val overList    : 'a list -> 'a iter;
  val for         : 'a iter -> ('a -> unit) -> unit;
  val mapIterator : ('a -> 'b) -> 'a iter -> 'b list
  
  val lookupDefault :  ('a -> 'b) -> ('a -> 'b) -> 'a -> 'b;
end (* MISC *);

(*****************************************************************************)
(*                  PRETTYPRINTER                                            *)
(*****************************************************************************)
structure PRETTYPRINTER :
sig
  type prettyPrinter 
  
  val ppAddString  : prettyPrinter -> string -> unit
  val ppBeginBlock : prettyPrinter -> int * bool -> unit
  val ppEndBlock   : prettyPrinter -> unit -> unit
  val ppBreak      : prettyPrinter -> int * int -> unit
end;

(*****************************************************************************)
(*                  UNIVERSAL                                                *)
(*****************************************************************************)
structure UNIVERSAL :
sig
  type universal
  type 'a tagMethods
  
  val makeTag : unit -> '_a tagMethods
  
  val tagIs      : 'a tagMethods -> universal -> bool
  val tagProject : 'a tagMethods -> universal -> 'a
end;

(*****************************************************************************)
(*                  ARRAY                                                    *)
(*****************************************************************************)
structure ARRAY :
sig
  type 'a array;
  
  val array  : int * '_a -> '_a array;
  val update : 'a array * int * 'a -> unit;
  val sub    : 'a array * int -> 'a;
end;

(*****************************************************************************)
(*                  ADDRESS                                                  *)
(*****************************************************************************)
structure ADDRESS :
sig
  type word;    (* any legal bit-pattern (tag = 0 or 1) *)
  val toWord  : 'a -> word;
  val unsafeCast: 'a -> 'b
end;

(*****************************************************************************)
(*                  RUNCALL                                                  *)
(*****************************************************************************)
structure RUNCALL :
sig
  val run_call1 : int -> 'a -> 'b
end;

(*****************************************************************************)
(*                  DEBUGGER                                                  *)
(*****************************************************************************)
structure DEBUGGER :
sig
	type types
	type word

	datatype environEntry =
		EnvValue of string * types
	|	EnvException of string * types
	|	EnvVConstr of string * types * bool
	|	EnvStaticLevel

	val debugFunction:
		string * string * int -> environEntry list -> word list -> unit
	val enterFunction: string -> unit -> unit
	val leaveFunction: types -> word -> word
	val exceptionFunction: exn -> 'a
end;

(*****************************************************************************)
(*                  PARSETREE sharing constraints                            *)
(*****************************************************************************)

sharing type
  LEX.lexan
= UTILITIES.lexan
= TYPETREE.lexan
= VALUEOPS.lexan
= UTILITIES.lexan
  
sharing type
  STRUCTVALS.types
= TYPETREE.types

sharing type
  STRUCTVALS.typeConstrs
= TYPETREE.typeConstrs
= VALUEOPS.typeConstrs

sharing type
  STRUCTVALS.structVals
= VALUEOPS.structVals

sharing type
  STRUCTVALS.types
= VALUEOPS.types
= DEBUGGER.types

sharing type
  STRUCTVALS.values
= VALUEOPS.values
= TYPETREE.values

sharing type
  CODETREE.codetree
= VALUEOPS.codetree

sharing type
  LEX.prettyPrinter
= TYPETREE.prettyPrinter 
= VALUEOPS.prettyPrinter 
= PRETTYPRINTER.prettyPrinter

sharing type
  ADDRESS.word
= CODETREE.word
= STRUCTVALS.word
= DEBUGGER.word
= VALUEOPS.word

sharing type
  UNIVERSALTABLE.univTable
= STRUCTVALS.univTable

sharing type
  STRUCTVALS.tagMethods
= UNIVERSAL.tagMethods;

sharing type
  UNIVERSALTABLE.universal
= UNIVERSAL.universal;

sharing type
  STRUCTVALS.fixStatus
= VALUEOPS.fixStatus

) : 

(*****************************************************************************)
(*                  PARSETREE export signature                               *)
(*****************************************************************************)
sig
  type word;
  type types;
  type fixStatus;
  type lexan;
  type prettyPrinter;
  type typeId;
  type codetree;
  type env;
  
  type typeConstrs;
  type values;
  type structVals;
  type environEntry;
  type structureIdentForm;

  (* An identifier is just a name. In the second pass it is associated
     with a particular declaration and the type is assigned into the
     type field. The type of this identifier is needed to deal with
     overloaded operators. If we have an occurence of ``='', say, the
     type of the value will be 'a * 'a -> bool but the type of a particular
     occurence, i.e. the type of the identifier must be int * int -> bool,
     say, after all the unification has been done. *)
          
  type parsetree and valbind and fvalbind and fvalclause and typebind
  and datatypebind and exbind; 
   
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
		 lookup: string -> types,
		 apply: (string * types -> unit) -> unit
       } *
       {
		 lookup: string -> types,
		 apply: (string * types -> unit) -> unit
       } ->  parsetree;
  
  val mkFunDeclaration : 
       fvalbind list *
       {
		 lookup: string -> types,
		 apply: (string * types -> unit) -> unit
       } *
       {
		 lookup: string -> types,
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
  val mkLocalDeclaration : (parsetree * int) list * (parsetree * int) list * bool -> parsetree;
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

  val pass2: parsetree * (unit -> typeId) * env * lexan * int * string -> types;

  type debugenv = environEntry list * (int->codetree)

  val gencode: parsetree * lexan * debugenv * int * int ref * string * int *
  					{ lookupFix: string -> fixStatus, lookupExnById: word -> values option}
					-> codetree list * debugenv;

  (* only used for debugging and error messages! *)
  val ptDisplay: parsetree * int * prettyPrinter -> unit;

end (* PARSETREE export signature *) =
   
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
  open UNIVERSAL;
  open PRETTYPRINTER;
  open ADDRESS;
  open RUNCALL;
  open ARRAY;
  open DEBUGGER;
  open RuntimeCalls; (* for POLY_SYS numbers *)
  
  val ioOp : int -> word = RUNCALL.run_call1 POLY_SYS_io_operation;
          
  infix 9 sub;
  
 (********* types constructors copied here to reduce garbage ***************)
  val emptyType            = EmptyType;
  val badType              = BadType;

  fun isEmpty             EmptyType           = true
    | isEmpty            _ = false;

 (************************************************************)
  
  val makeRaise = CODETREE.mkRaise; (* to avoid confusion! *)
  val makeWhile = CODETREE.mkWhile; (* to avoid confusion! *)
  
  abstype parsetree = 
     Ident               of identForm
   | Literal             of literalForm
   | Applic              of applicForm
   | Cond                of condForm
   | TupleTree           of parsetree list
   | ValDeclaration      of valDecForm
   | FunDeclaration      of funDecForm
   | OpenDec             of { decs: structureIdentForm list, variables: values list ref }
   | Constraint          of constraintForm
   | Layered             of layeredForm
   | Fn                  of parsetree list
   | MatchTree           of matchForm
   | Localdec            of localdecForm
   | TypeDeclaration     of typebind list
   | AbstypeDeclaration  of abstypeDeclarationForm
   | DatatypeDeclaration of abstypeDeclarationForm
   | DatatypeReplication of datatypeReplicationForm
   | ExpSeq              of (parsetree * int) list
   | Directive           of directiveForm
   | ExDeclaration       of exbind list
   | Raise               of parsetree
   | HandleTree          of handleTreeForm
   | While               of whileForm
   | Case                of caseForm
   | Andalso             of andOrForm
   | Orelse              of andOrForm
   | Labelled            of labelledForm
   | Selector            of selectorForm
   | List                of parsetree list
   | EmptyTree
   | WildCard 
   | Unit
   
   and valbind = (* Value bindings are either a binding or a list
   					of recursive bindings.*)
   	  ValBind of (* Consists of a declaration part (pattern) and an expression. *)
		     {
		       dec: parsetree,
		       exp: parsetree,
		       line: int
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
		   resultType:  types ref
	     }

	and fvalclause = (* Clause within a function binding. *)
		FValClause of { dec: parsetree, exp: parsetree, line: int }
		
	and typebind = (* Non-generative type binding *)
		TypeBind of
	     {
	       name: string,
	       typeVars: types list,
	       decType: types,
		   isEqtype: bool (* True if this was an eqtype in a signature. *)
	     } 

   	and datatypebind = (* Generative type binding *)
		DatatypeBind of
	     {
	       name:          string,
	       typeVars:      types list,
	       constrs:       (string*types) list,
	       tcon:          typeConstrs ref,
	       valueConstrs:  values list ref
	     }

   and exbind = (* An exception declaration. It has a name and
   				   optionally a previous exception and a type. *)
   		ExBind of
     {
       name:         string,
       previous:     parsetree,
       typeof:       types,
       value:        values ref
     } 



  (* An identifier is just a name. In the second pass it is associated
     with a particular declaration and the type is assigned into the
     type field. The type of this identifier is needed to deal with
     overloaded operators. If we have an occurence of ``='', say, the
     type of the value will be 'a * 'a -> bool but the type of a particular
     occurence, i.e. the type of the identifier must be int * int -> bool,
     say, after all the unification has been done. *)
   withtype identForm = 
     {
       name:   string,
       typeof: types ref,
       value:  values ref
     }

   (* Literal constants may be overloaded on more than one type. The
      types are specified by installing appropriate conversion functions:
	  convInt, convReal, convChar, convString and convWord. *)
   and literalForm =
   	 {
	 	converter: values,
        typeof: types ref,
		literal: string
	 }

       (* Function application *)
   and applicForm = 
     {
       f: parsetree,
       arg: parsetree
     } 

       (* Conditional *)
   and condForm = 
     {
       test: parsetree,
       thenpt: parsetree,
       elsept: parsetree
     } 

       (* Val and fun declarations. *)
   and valDecForm = 
     {
       dec:    valbind list,
       explicit: {lookup: string -> types,
               apply: (string * types -> unit) -> unit },
       implicit: {lookup: string -> types,
               apply: (string * types -> unit) -> unit },
	   variables: values list ref (* list of variables declared *)
     } 

   and funDecForm = 
     {
       dec:    fvalbind list,
       explicit: {lookup: string -> types,
               apply: (string * types -> unit) -> unit },
       implicit: {lookup: string -> types,
               apply: (string * types -> unit) -> unit }
     } 

       (* Name of a structure. Used only in an ``open'' declaration. *)
   and structureIdentForm = 
     {
       name:   string,
       value:  structVals ref
     } 

   (* Constraint (explicit type given) *)
   (* A constraint has a value and a type. The actual type, will, however
      be the unification of these two and not necessarily the given type. *)
   and constraintForm = 
     {
       value: parsetree,
       given: types
     } 

  (* Layered pattern. Equivalent to an ordinary pattern except that the
     variable is given the name of the object which is to be matched. *)
   and layeredForm =
     {
       var: parsetree,
       pattern: parsetree
     } 

   (* A match is a pattern and an expression. If the pattern matches then
     the expression is evaluated in the environment of the pattern. *)
   and matchForm = 
     {
       vars: parsetree,
       exp: parsetree,
       line: int,
	   resType: types ref
     } 

    (* Used for local dec in dec and let dec in exp. *)
   and localdecForm = 
     {
       decs: (parsetree * int) list,
       body: (parsetree * int) list,
       loc: bool,
	   varsInBody: values list ref (* Variables in the in..dec part
	                                  of a local declaration. *)
     } 

   (* Datatype and Abstract Type declarations *)
   and abstypeDeclarationForm = 
     {
       typelist:  datatypebind list,
       withtypes: typebind list,
       declist:   (parsetree * int) list
     }
	 
   and datatypeReplicationForm =
   	 {
	 	newType:  string,
		oldType:  string
     }

   (* Directives are infix, infixr and nonfix. They are processed by the
      parser itself and only appear in the parse tree for completeness. *)
   and directiveForm = 
     {
       tlist: string list,
       fix: fixStatus
     } 

   (* Execute an expression and catch any exceptions. *)
   and handleTreeForm = 
     {
       exp: parsetree,
       hrules: parsetree list
     } 

       (* Ordinary while-loop *)
   and whileForm = 
     {
       test: parsetree,
       body: parsetree
     } 

       (* Case-statement *)
   and caseForm = 
     {
       test: parsetree,
       match: parsetree list
     } 

       (* andalso/orelse *)
   and andOrForm = 
     {
       first: parsetree,
       second: parsetree
     } 

   and labelledForm = 
     {
       recList: (string * parsetree) list,
       frozen:  bool,
       typeof:  types ref
     } 

   (* Labelled record & the entry in the list. "frozen" is false if it's
      a pattern with "...". *)
                   
   and selectorForm = 
     {
       name: string,
       labType: types,
       typeof: types
     }
     
  with

  (*****************************************************************************
			  Pretty Printing
  ******************************************************************************)
  
    fun isIdent               (Ident _)               = true | isIdent _               = false;

    fun isApplic              (Applic _)              = true | isApplic _              = false;
    fun isTupleTree           (TupleTree _)           = true | isTupleTree _           = false;
    fun isAbstypeDeclaration  (AbstypeDeclaration  _) = true | isAbstypeDeclaration _  = false;
    fun isEmptyTree           EmptyTree               = true | isEmptyTree _           = false;
  
    val unit      = Unit;
    val wildCard  = WildCard;
    val emptyTree = EmptyTree;
  
    fun mkIdent name : parsetree = 
      Ident
        {
          name   = name,
          typeof = ref badType,
          value  = ref undefinedValue
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

    fun mkString(s: string): parsetree =
		Literal{converter=convString, literal=s, typeof=ref badType};
    
    fun mkInt  (i : string) : parsetree =
		Literal{converter=convInt, literal=i, typeof=ref badType};
    
    fun mkReal (r : string) : parsetree =
		Literal{converter=convReal, literal=r, typeof=ref badType};
    
    fun mkChar (c : string) : parsetree = 
		Literal{converter=convChar, literal=c, typeof=ref badType};

    fun mkWord (w : string) : parsetree =
		Literal{converter=convWord, literal=w, typeof=ref badType};
	
    fun mkApplic (f, arg) : parsetree  =
      Applic
        {
          f   = f,
          arg = arg
        };
    
    fun mkCond (test, thenpt, elsept) : parsetree  = 
      Cond  
       { test   = test,
         thenpt = thenpt,
         elsept = elsept
       };
       
    val mkTupleTree : parsetree list -> parsetree = TupleTree;
    
    fun mkValDeclaration (dec, explicit, implicit) : parsetree = 
      ValDeclaration 
        {
          dec   = dec,
		  explicit = explicit,
          implicit = implicit,
		  variables = ref []
        };
    
    fun mkFunDeclaration (dec, explicit, implicit) : parsetree =
      FunDeclaration
        {
           dec=dec,
		  explicit = explicit,
          implicit = implicit
        };
    
    fun mkOpenTree(ptl : structureIdentForm list): parsetree = OpenDec{decs=ptl, variables=ref []};
    
    fun mkStructureIdent name : structureIdentForm =
        { 
          name  = name,
          value = ref undefinedStruct
        }; 
 
    fun mkValBinding (dec, exp, line) : valbind = 
      ValBind
        {
          dec  = dec,
          exp  = exp,
          line = line
        };

    val recValbind = RecValBind;

    fun mkClausal clauses : fvalbind =
       FValBind
         { 
           clauses    = clauses,
           numOfPatts = ref 0,
           functVar   = ref undefinedValue,
		   resultType = ref badType
         }; 

    fun mkClause (dec, exp, line) : fvalclause = 
      FValClause
        {
          dec  = dec,
          exp  = exp,
          line = line
        };

    val mkList : parsetree list -> parsetree = List;
    
    fun mkConstraint (value, given) : parsetree = 
      Constraint 
        { 
          value = value,
          given = given
        };
      
    fun mkLayered (var, pattern) : parsetree = 
      Layered
        {
          var     = var,
          pattern = pattern
        };
    
    val mkFn : parsetree list -> parsetree = Fn;
    
    fun mkMatchTree (vars, exp, line) : parsetree = 
      MatchTree 
        {
          vars = vars,
          exp  = exp,
          line = line,
		  resType = ref badType
        };
  
    fun mkLocalDeclaration (decs, body, loc) : parsetree =
      Localdec 
        {
          decs = decs,
          body = body,
          loc  = loc,
		  varsInBody = ref []
        };
      
    val mkTypeDeclaration : typebind list -> parsetree = TypeDeclaration;

    fun mkDatatypeDeclaration (typelist, withtypes) : parsetree =
     DatatypeDeclaration
       {
         typelist  = typelist,
         withtypes = withtypes,
         declist   = []
       };
    
    fun mkAbstypeDeclaration (typelist, withtypes, declist) : parsetree =
      AbstypeDeclaration
        {
          typelist  = typelist,
          withtypes = withtypes,
          declist   = declist
        };

    fun mkDatatypeReplication (newType, oldType) : parsetree =
     DatatypeReplication
       {
         oldType = oldType,
		 newType = newType
       };
    
    fun mkTypeBinding (name, typeVars, decType, isEqtype) : typebind =
      TypeBind 
        {
          name     = name,
          typeVars = typeVars,
          decType  = decType,
		  isEqtype = isEqtype
        };
    
    fun mkDatatypeBinding (name, typeVars, constrs) : datatypebind =
      DatatypeBind
        {
          name         = name,
          typeVars     = typeVars,
          constrs      = constrs,
          tcon         = ref undefType,
          valueConstrs = ref []
        };
   
    fun mkExBinding (name, previous, typeof) : exbind =
      ExBind 
        {
          name        = name,
          previous    = previous,
          typeof      = typeof,
          value       = ref undefinedValue
        };

    fun mkLabelledTree (recList, frozen) : parsetree = 
     Labelled
       {
         recList = recList,
         frozen  = frozen,
         typeof  = ref emptyType
       };

    fun mkSelector name : parsetree =
    let
      val resType   = mkTypeVar (generalisable, false, false, false);
      val entryType = mkLabelEntry (name, resType);
      val labType   = mkLabelled ([entryType], false) (* Not frozen*);
    in
      Selector
        {
          name      = name,
          labType   = labType,
          typeof    = mkFunctionType (labType, resType)
        }
    end;
    
    val mkRaise : parsetree -> parsetree = Raise;
    
    fun mkHandleTree (exp, hrules) : parsetree = 
       HandleTree
         { 
           exp    = exp,
           hrules = hrules
         };
       
    fun mkWhile (test, body) : parsetree =
      While
        { 
          test = test,
          body = body
        };
      
    fun mkCase (test, match) : parsetree =
      Case
        {
          test  = test,
          match = match
        };
      
    fun mkAndalso (first, second) : parsetree =
      Andalso
        {
          first  = first,
          second = second
        };
      
    fun mkOrelse (first, second) : parsetree =
      Orelse
        {
          first  = first,
          second = second
        };
      
    fun mkDirective (tlist, fix) : parsetree = 
      Directive
        {
          tlist = tlist,
          fix   = fix
        };
       
    val mkExpseq  : (parsetree * int) list -> parsetree = ExpSeq;
    
    val mkExDeclaration  : exbind list -> parsetree = ExDeclaration;
  
  
  (* We actually have 2 separate pretty printers! This one is
     only used for debugging and error messages. The proper
     top-level pretty-printer is the collection of "display"
     functions in VALUEOPS.ML. This needs sorting!
	 DCJM:  The reason for that is that this pretty printer prints
	 the parse tree whereas the one in Valueops prints values.
  *) 
  
   (* Generates a pretty-printed representation of a piece of tree. *)
    fun ptDisplay (c      : parsetree, (* The value to print. *)
                   depth  : int,       (* The number of levels to display. *)
                   pprint : prettyPrinter) : unit =
    let
        (* Prints a list of items. *)
      fun printList (doPrint: 'a*int*prettyPrinter->unit) (c: 'a list, separator, depth) =
        if depth <= 0 then ppAddString pprint "..."
        else
          case c of
            []      => ()
          | [v]     => doPrint (v, depth, pprint)
          | (v::vs) =>
              (
                ppBeginBlock pprint (0, false);
                doPrint (v, depth, pprint);
                ppBreak pprint
                   (if separator = "," orelse separator = ";" orelse separator = "" then 0 else 1, 0);
                ppAddString pprint separator;
                ppEndBlock pprint  ();
                ppBreak pprint (1, 0);
                printList doPrint (vs, separator, depth - 1)
              )
         (* end printList *);

		val displayList = printList ptDisplay
		
		(* type bindings and datatype bindings are used in several cases *)
		fun printTypeBind (TypeBind{name, typeVars, decType, ...}, depth, pprint) =
		        (
		          ppBeginBlock pprint (3, true);
		          displayTypeVariables (typeVars, depth, pprint);
		          ppAddString pprint name;
				  (* The type may be missing if this is a signature. *)
				  case decType of
				  		EmptyType => ()
				  |	_ =>
					  	(
						ppBreak pprint (1, 0);
						ppAddString pprint "=";
						ppBreak pprint (1, 0);
						display (decType, depth, pprint)
						);
				  ppEndBlock pprint ()
		        )

		and printDatatypeBind (DatatypeBind{name, typeVars, constrs, ...}, depth, pprint) =
		        (
		          ppBeginBlock pprint (3, true);
		          displayTypeVariables (typeVars, depth, pprint);
		          ppAddString pprint name;
		          ppBreak pprint (1, 0);
		          ppAddString pprint "=";
		          ppBreak pprint (1, 0);
          		  printList printConstructor (constrs, "|", depth - 1);
		          ppEndBlock pprint ()
		        )
		and printConstructor ((name, argtype), depth, pprint) =
	        (
	          ppBeginBlock pprint (2, false);
	          ppAddString pprint name;
	          if isEmpty argtype then ()
	          else
	           (
	            ppBreak pprint (1, 0);
	            ppAddString pprint "of";
	            ppBreak pprint (1, 0);
	            display (argtype, depth, pprint)
	           );
	          ppEndBlock pprint ()
	        )
    in
      if depth <= 0 (* elide further text. *)
        then ppAddString pprint "..."

      else case c of
      
        Ident {name, ...} =>
          ppAddString pprint name
          
      | Literal{literal, converter, ...} =>
	  	  let
		  	 val convName = valName converter
			 val lit =
			 	if convName = "convString"
				then concat["\"" , literal, "\""]
				else literal 
		  in
             ppAddString pprint lit
		  end

      | Applic {f, arg} =>
        (
          ppBeginBlock pprint (0, false);
          
          (* No need to parenthesise *)
          if isApplic f orelse isIdent f
          then ptDisplay (f, depth - 1, pprint)
          else
           ( (* Put parentheses round the function. *)
            ppAddString pprint "(";
            ppBreak pprint (0, 0);
            ptDisplay (f, depth - 1, pprint);
            ppBreak pprint (0, 0);
            ppAddString pprint ")"
           );
           
          ppBreak pprint (0, 0);
          
          if isTupleTree arg
          then (* will have parens anyway *)
            ptDisplay (arg, depth - 1, pprint)
          else
           ( (* Put parentheses round the argument. *)
            ppAddString pprint "(";
            ppBreak pprint (0, 0);
            ptDisplay (arg, depth - 1, pprint);
            ppBreak pprint (0, 0);
            ppAddString pprint ")"
           );
           
          ppEndBlock pprint  ()
        )

      | Cond {test, thenpt, elsept} =>
        (
          ppBeginBlock pprint (0, false);
          ppAddString pprint "if";
          ppBreak pprint (1, 0);
          ptDisplay (test, depth - 1, pprint);
          ppBreak pprint (1, 0);
          ppAddString pprint "then";
          ppBreak pprint (1, 0);
          ptDisplay (thenpt, depth - 1, pprint);
          ppBreak pprint (1, 0);
          ppAddString pprint "else";
          ppBreak pprint (1, 0);
          ptDisplay (elsept, depth - 1, pprint);
          ppEndBlock pprint ()
        )

      | TupleTree ptl =>
	(
	 ppBeginBlock pprint (3, true);
	 ppAddString pprint "(";
	 ppBreak pprint (1, 0);
	 displayList (ptl, ",", depth - 1);
	 ppBreak pprint (0, 0);
	 ppAddString pprint ")";
	 ppEndBlock pprint ()
	)

      | ValDeclaration {dec, explicit, ...} =>
	  	let
			fun printValBind (RecValBind, depth, pprint) =
			  		(
		            ppAddString pprint "rec";
		            ppBreak pprint (1, 0)
					)
			  | printValBind (ValBind{dec, exp, ...}, depth, pprint) =
			  		(
					ppBeginBlock pprint (3, true);
					ptDisplay (dec, depth - 1, pprint);
					ppBreak pprint (1, 0);
					ppAddString pprint "=";
					ppBreak pprint (1, 0);
					ptDisplay (exp, depth - 1, pprint);
					ppEndBlock pprint ()
					)
		in
			ppBeginBlock pprint (3, true);
			ppAddString pprint "val";
			ppBreak pprint (1, 0);
			(* TODO: Display the explicit type variables. *)
		    (* displayTypeVariables (explicit, depth, pprint); *)
			(* TODO: This prints val rec f as "val rec  and f". *)
			printList printValBind (dec, "and", depth - 1);
			ppEndBlock pprint ()
		end

      | FunDeclaration {dec, explicit={apply, ...}, ...} =>
	  	let
			fun printfvalbind (FValBind{clauses, ...}, depth, pprint) =
					printList printClause (clauses, "|", depth - 1)
			and printClause (FValClause{dec, exp, ...}, depth, pprint) =
			  		(
					ppBeginBlock pprint (3, true);
					ptDisplay (dec, depth - 1, pprint);
					ppBreak pprint (1, 0);
					ppAddString pprint "=";
					ppBreak pprint (1, 0);
					ptDisplay (exp, depth - 1, pprint);
					ppEndBlock pprint ()
					)
 		in
			ppBeginBlock pprint (3, true);
			ppAddString pprint "fun";
			ppBreak pprint (1, 0);
			(* TODO: Display the explicit type variables. *)
		    (* displayTypeVariables (explicit, depth, pprint); *)
			printList printfvalbind (dec, "and", depth - 1);
			ppEndBlock pprint ()
		end

      | OpenDec {decs, ...} =>
		let
		  fun printStrName ({name, ...}: structureIdentForm, _, pprint) = ppAddString pprint name
		in
		  ppBeginBlock pprint (3, true);
		  ppAddString pprint "open";
		  ppBreak pprint (1, 0);
		  printList printStrName (decs, "", depth - 1);
		  ppEndBlock pprint ()
		end

      | List ptl =>
	(
	  ppBeginBlock pprint (3, true);
	  ppAddString pprint "[";
	  ppBreak pprint (1, 0);
	  displayList (ptl, ",", depth - 1);
	  ppBreak pprint (0, 0);
	  ppAddString pprint "]";
	  ppEndBlock pprint ()
	)

      | Constraint {value, given} =>
        (
          ppBeginBlock pprint (3, false);
          ptDisplay (value, depth - 1, pprint);
          ppBreak pprint (1, 0);
          ppAddString pprint ":";
          ppBreak pprint (1, 0);
          display (given, depth, pprint);
          ppEndBlock pprint ()
        )

      | Layered {var, pattern} =>
        (
          ppBeginBlock pprint (3, true);
          ptDisplay (var, depth - 1, pprint);
          ppBreak pprint (1, 0);
          ppAddString pprint "as";
          ppBreak pprint (1, 0);
          ptDisplay (pattern, depth - 1, pprint);
          ppEndBlock pprint ()
        )

      | MatchTree {vars, exp, ...} =>
        (
          ppBeginBlock pprint (0, false);
          ptDisplay (vars, depth - 1, pprint);
          ppBreak pprint (1, 0);
          ppAddString pprint "=>";
          ppBreak pprint (1, 0);
          ptDisplay (exp, depth - 1, pprint);
          ppEndBlock pprint ()
        )

      | Fn ptl =>
	(
	  ppBeginBlock pprint (3, true);
	  ppAddString pprint "fn";
	  ppBreak pprint (1, 0);
	  displayList (ptl, "|", depth - 1);
	  ppEndBlock pprint ()
	)

      | Unit =>
          ppAddString pprint "()"

      | WildCard =>
          ppAddString pprint "_"

      | Localdec {loc, decs, body, ...} =>
        (
          ppBeginBlock pprint (3, false);
          ppAddString pprint (if loc then "local" else "let");
          ppBreak pprint (1, 0);
          displayList (#1(ListPair.unzip decs), ";", depth - 1);
          ppBreak pprint (1, 0);
          ppAddString pprint "in";
          ppBreak pprint (1, 0);
          displayList (#1(ListPair.unzip body), ";", depth - 1);
          ppBreak pprint (1, 0);
          ppAddString pprint "end";
          ppEndBlock pprint ()
        )

      | TypeDeclaration ptl =>
	  	let
			(* This is used both for type bindings and also in signatures.
			   In a signature we may have "eqtype". *)
			val typeString =
				case ptl of
					TypeBind {isEqtype=true, ...} :: _ => "eqtype"
				|   _ => "type"
		in
			ppBeginBlock pprint (3, true);
			ppAddString pprint typeString;
			ppBreak pprint (1, 0);
			printList printTypeBind (ptl, "and", depth - 1);
			ppEndBlock pprint ()
		end

      | DatatypeDeclaration {typelist, withtypes, ...} =>
        (
          ppBeginBlock pprint (3, true);
          ppAddString pprint "datatype";
          ppBreak pprint (1, 0);
		  printList printDatatypeBind (typelist, "and", depth - 1);
          if isNil withtypes then ()
          else
           (
            ppBreak pprint (1, 0);
            ppAddString pprint "withtype";
            ppBreak pprint (1, 0);
		    printList printTypeBind (withtypes, "and", depth - 1)
           );
          ppEndBlock pprint ()
        )

      | DatatypeReplication {newType, oldType} =>
        (
          ppBeginBlock pprint (3, true);
          ppAddString pprint "datatype";
          ppBreak pprint (1, 0);
          ppAddString pprint newType;
          ppBreak pprint (1, 0);
          ppAddString pprint "=";
          ppBreak pprint (1, 0);
          ppAddString pprint "datatype";
          ppBreak pprint (1, 0);
          ppAddString pprint oldType;
          ppEndBlock pprint ()
        )

       | AbstypeDeclaration {typelist, withtypes, declist} =>
        (
          ppBeginBlock pprint (3, true);
          ppAddString pprint "abstype";
          ppBreak pprint (1, 0);
		  printList printDatatypeBind (typelist, "and", depth - 1);
          ppBreak pprint (1, 0);
          if isNil withtypes then ()
          else
           (
            ppAddString pprint "withtype";
            ppBreak pprint (1, 0);
		    printList printTypeBind (withtypes, "and", depth - 1);
            ppBreak pprint (1, 0)
           );
          ppAddString pprint "with";
          ppBreak pprint (1, 0);
          ppBeginBlock pprint (3, true);
          displayList (#1 (ListPair.unzip declist), ";", depth - 1);
          ppEndBlock pprint ();
          ppEndBlock pprint ()
        )

      | ExpSeq ptl =>
		(
		  ppBeginBlock pprint (3, true);
		  ppAddString pprint "(";
		  ppBreak pprint (0, 0);
		  displayList (#1 (ListPair.unzip ptl), ";", depth - 1);
		  ppBreak pprint (0, 0);
		  ppAddString pprint ")";
		  ppEndBlock pprint ()
		)

      | Directive {fix, tlist} =>
        (
          ppBeginBlock pprint (3, true);
          displayFixStatus (fix, depth, pprint);
          ppBreak pprint (1, 0);
		  printList (fn (name, _, pprint) => ppAddString pprint name) (tlist, "", depth - 1);
          ppEndBlock pprint ()
        )

      | ExDeclaration pt =>
	  	let
			fun printExBind (ExBind {name, typeof, previous, ...}, depth, pprint) =
		        (
		          ppBeginBlock pprint (0, false);
		          ppAddString pprint name;
		          if isEmpty typeof then ()
		          else 
		            (
		              ppBreak pprint (1, 0);
		              ppAddString pprint "of";
		              ppBreak pprint (1, 0);
		              display (typeof, depth, pprint)
		            );
		          if isEmptyTree previous then ()
		          else 
		            (
		              ppBreak pprint (1, 0);
		              ppAddString pprint "=";
		              ppBreak pprint (1, 0);
		              ptDisplay (previous, depth - 1, pprint)
		            );
		          ppEndBlock pprint ()
		        )
 		in
			ppBeginBlock pprint (3, true);
			ppAddString pprint "exception";
			ppBreak pprint (1, 0);
			printList printExBind (pt, "and", depth - 1);
			ppEndBlock pprint ()
		end

      | Raise pt =>
	(
	  ppBeginBlock pprint (0, false);
	  ppAddString pprint "raise";
	  ppBreak pprint (1, 0);
	  ptDisplay (pt, depth - 1, pprint);
	  ppEndBlock pprint ()
	)

      | HandleTree {exp, hrules} =>
        (
          ppBeginBlock pprint (0, false);
          ptDisplay (exp, depth - 1, pprint);
          ppBreak pprint (1, 0);
          ppBeginBlock pprint (3, true);
          ppAddString pprint "handle";
          ppBreak pprint (1, 0);
          displayList (hrules, "|", depth - 1);
          ppEndBlock pprint ();
          ppEndBlock pprint ()
        )

      | While {test, body} =>
        (
          ppBeginBlock pprint (0, false);
          ppAddString pprint "while";
          ppBreak pprint (1, 0);
          ptDisplay (test, depth - 1, pprint);
          ppBreak pprint (1, 0);
          ppAddString pprint "do";
          ppBreak pprint (1, 0);
          ptDisplay (body, depth - 1, pprint);
          ppEndBlock pprint ()
        )

      | Case {test, match} =>
        (
          ppBeginBlock pprint (3, true);
          ppBeginBlock pprint (0, false);
          ppAddString pprint "case";
          ppBreak pprint (1, 0);
          ptDisplay (test, depth - 1, pprint);
          ppBreak pprint (1, 0);
          ppAddString pprint "of";
          ppEndBlock pprint ();
          ppBreak pprint (1, 0);
          displayList (match, "|", depth - 1);
          ppEndBlock pprint ()
        )

      | Andalso {first, second} =>
        (
          ppBeginBlock pprint (3, true);
          ptDisplay (first, depth - 1, pprint);
          ppBreak pprint (1, 0);
          ppAddString pprint "andalso";
          ppBreak pprint (1, 0);
          ptDisplay (second, depth - 1, pprint);
          ppEndBlock pprint ()
        )

      | Orelse {first, second} =>
        (
          ppBeginBlock pprint (3, true);
          ptDisplay (first, depth - 1, pprint);
          ppBreak pprint (1, 0);
          ppAddString pprint "orelse";
          ppBreak pprint (1, 0);
          ptDisplay (second, depth - 1, pprint);
          ppEndBlock pprint ()
        )

      | Labelled {recList, frozen, ...} =>
        let
	      fun displayRecList (c, depth) =
	        if depth <= 0 then ppAddString pprint "..."
	        else
	          case c of
	            []      => ()
	          | [(name, value)]     =>
				  	(
			        ppBeginBlock pprint (0, false);
			        ppAddString pprint (name ^ " =");
			        ppBreak pprint (1, 0);
			        ptDisplay (value, depth - 1, pprint);
			        ppEndBlock pprint ()
					)
  	          | ((name, value)::vs) =>
	              (
	                ppBeginBlock pprint (0, false);
			        ppBeginBlock pprint (0, false);
			        ppAddString pprint (name ^ " =");
			        ppBreak pprint (1, 0);
			        ptDisplay (value, depth - 1, pprint);
			        ppEndBlock pprint ();
	                ppBreak pprint (0, 0);
	                ppAddString pprint ",";
	                ppEndBlock pprint ();
	                ppBreak pprint (1, 0);
	                displayRecList (vs, depth - 1)
	              )
	         (* end displayRecList *)
		 in
          ppBeginBlock pprint (2, false);
          ppAddString pprint "{";
          displayRecList (recList, depth - 1);
          if frozen then ()
          else ppAddString pprint (if isNil recList then "..." else ", ...");
          ppAddString pprint "}";
          ppEndBlock pprint ()
        end

      | Selector {name, ...} =>
          ppAddString pprint ("#" ^ name)

      | EmptyTree =>
         ppAddString pprint "<Empty>"
    end; (* ptDisplay *)


    (* Error message routine.  Used in both pass 2 and pass 3. *)
    fun errorNear (lex, hard, near, line, message) =
    let
      val printProc = if hard then errorProc else warningProc;
    in
     (* Puts out an error message and then prints the piece of tree. *)
     printProc
       (lex,
        line,
        fn (pprint:prettyPrinter) =>
          (ppBeginBlock pprint (0, false);
           ppAddString pprint message;
           ppBreak pprint (3, 0);
           ppBeginBlock pprint (0, false);
           ppAddString pprint "Found near";
           ppBreak pprint (1, 0);
           ptDisplay (near, ! errorDepth, pprint);
           ppEndBlock pprint ();
           ppEndBlock pprint ()))
    end;


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
    fun pass2 (v, makeTypeId, env as Env E, lex, line, strName) =
    let
      (* Returns a function which can be passed to unify or apply to
         print a bit of context info. *)
      fun foundNear v (pprint: prettyPrinter) =
        (
          ppAddString pprint "Found near";
          ppBreak pprint (1, 0);
          ptDisplay (v, ! errorDepth, pprint)
        );

      (* A simpler error message routine for lookup_... where the message
         does not involve pretty-printing anything. *)
      fun giveError (v, lex, line)  =
        fn message => errorNear (lex, true, v, line, message);

	  fun checkForBuiltIn (name, v, lex, lineno, isConstr) =
	  (* ML97 does not allow the standard constructors to be rebound and does
	     not allow "it" to become a constructor. *)
	  	 if !ml90 then ()
	     else if name = "true" orelse name = "false" orelse name = "nil"
		 orelse name = "::" orelse name = "ref" orelse (isConstr andalso name = "it")
		 then errorNear(lex, true, v, lineno,
		 			"Rebinding or specifying \"" ^ name ^ "\" is illegal")
		 else ()

      (* parameters re-ordered SPF 22/10/94 *)
      fun assignValues level letDepth line (env as Env E) near v  =
      let
         (* Process each item of the sequence and return the type of the
            last item. A default item is returned if the list is empty. *)
        fun assignSeq env depth (l: (parsetree * int) list) =
        let
          fun applyList last []       = last
            | applyList last ((h, line) :: t) = 
               applyList (assignValues level depth line env v h) t
        in
          applyList badType l
        end;

        fun ptAssignTypes t near line =
          assignTypes
            (t,
             fn s => 
               lookupTyp 
                 ({lookupType = #lookupType E, lookupStruct = #lookupStruct E},
                  s,
                  giveError (near, lex, line)),
            lex,
            line);

         (* Makes a type for an instance of an identifier. In the case of
            exceptions this means converting the type into a function and
            in the case of values or other constructors of making a copy
            of the type to create new instances of type variables.
            isExp is true if this an expression, false if it is a pattern.
            Generic imperative type variables are turned into ordinary type
            variables in patterns. *)
        fun instanceType (Value{class = Exception, typeOf, ...}) isExp =
          (* If this is an exception the type is either  exn  or t -> exn. *)
          if isEmpty typeOf then exnType else mkFunctionType (typeOf, exnType)

        | instanceType  (v as Value{access=Overloaded _, ...}) isExp =
		  (* Look up the current overloading for this function. *)
				overloadType(v, false)

        | instanceType  (Value{typeOf, ...}) isExp =
            (* The types of constructors and variables are copied 
               to create new instances of type variables. *)
          	generalise (typeOf, isExp);

        fun processPattern pat enterResult level notConst isRec line =
        let
          val mapProcessPattern =
            map (fn x => processPattern x enterResult level notConst isRec line);
        in
          case pat of
            Ident {name, value, typeof} => (* Variable or nullary constructor. *)
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
		    if not (! ml90) andalso isRec
			then undefinedValue
			else if #first names = ""
			then (* Not qualified - may be a variable. *)
			  lookupDefault (#lookupVal E) (fn _ => undefinedValue) name
			  
			else (* Qualified - cannot be a variable. *)
			  lookupValue
			    ("Constructor",
			     {lookupVal= #lookupVal E, lookupStruct= #lookupStruct E},
			     name,
			     giveError (pat, lex, line));
			
			       
	      val instanceType = 
	        (* If the result is a constructor use it. *)
			if isConstructor nameVal (* exceptions. *)
			then
			  ( 
			    if notConst
			    then errorNear (lex, true, pat, line,
			       "Identifier before `as' must not be a constructor")
			    else ();
			     
			    (* set this value in the record *)
			    value := nameVal;
	  
			    (* Must be a nullary constructor otherwise it should
			       have been applied to something. *)
			    let
			      val isNullary =
				  	case nameVal of
						Value{class=Constructor{nullary}, ...} => nullary
					|	Value{typeOf, ...} => (* exception *) isEmpty typeOf
			    in
			      if isNullary then ()
			      else errorNear (lex, true, pat, line,
				      "Constructor must be applied to something.")
			    end;
			    
			    instanceType nameVal false (* pattern *)
			   )
	  
			(* If undefined or another variable, construct a new variable. *)
			 else let
			   val var = 
			     mkVar (name, mkTypeVar (level, false, false, false));
			 in
			   checkForDots (name, lex, line); (* Must not be qualified *)
			   (* Must not be "true", "false" etc. *)
			   checkForBuiltIn (name, v, lex, line, false);
			   enterResult (name, var);
			   value := var;
			   valTypeOf var (* and return its type *)
			 end;
		    in
		      typeof := instanceType;
		      instanceType
		    end
	
		 | Literal{converter, typeof, ...} =>
		  	   let
				(* Find out the overloadings on this converter and
				   construct an instance of it.  The converters are
				   all functions from string to the result type. *)
			      val instanceType = overloadType(converter, true)
				  (* Apply the converter to string to get the type of the
				     literal. *)
				  val instance =
				  	apply(instanceType, stringType, lex, line, foundNear pat)
			   in
			   	  typeof := instance; (* Remember it *)
				  instance
			   end

          | Applic {f = con, arg} =>
		    let
		      (* Apply the function to the argument and return the result. *)
		      (* Function must be a constructor. *)
		      val conType = 
		        case con of
		          Ident {name, value, ...} =>
			  let (* Look up the value and return the type. *)
			    val constrVal =
			      lookupValue 
					("Constructor",
					 {lookupVal   = #lookupVal E, 
					 lookupStruct = #lookupStruct E},
					 name,
					 giveError (pat, lex, line));
			  in
			    if isConstructor constrVal
			    then let
			      val U : unit = value := constrVal;
			    in
			      instanceType constrVal false (* pattern *)
			    end
			    else let (* Undeclared or a variable. *)
			      val U : unit = 
					if isUndefinedValue constrVal then ()
					else errorNear (lex, true, pat, line,
					                name ^ " is not a constructor")
			    in
			      badType
			    end
			  end
		
                | _ => (* con is not an Ident *)
		  let
		    val U : unit = 
		      errorNear (lex, true, pat, line,
			"Constructor in a pattern was not an identifier");
		  in
		    badType
		  end;
		
	      val patType = processPattern arg enterResult level notConst isRec line;
	    in
	      apply (conType, patType, lex, line, foundNear pat)
	    end (* Applic *)

          | TupleTree ptl =>
              (* Construct the type obtained by mapping "processPattern"
                 onto each element of the tuple. *)
              mkProductType (mapProcessPattern ptl)

          | Labelled {recList, frozen, typeof} =>
            let (* Process each item in the list. *)
			  fun mapLabels [] = []
			    | mapLabels ((name, value)::T) =
		               (* Type is a label entry with the label name
		                  and the type of the pattern. *)
		             mkLabelEntry
		                  (name, processPattern value enterResult level notConst isRec line)
						 :: mapLabels T;
              val patType =
                 mkLabelled
                   (sortLabels 
                     (mapLabels recList,
                      fn msg => errorNear (lex, true, pat, line, msg)), 
                    frozen);
            in
              typeof := patType;
              patType
            end

          | List ptl =>
            let
              (* Applies "processPattern" to every element of a list and
                 unifies the types. Returns a type variable if the list
                 is empty *)
              fun processList tlist =
              let
                (* Construct a type variable and unite all the types to that. *)
                val basicType = mkTypeVar (generalisable, false, false, false);
                fun applyList []     = ()
                  | applyList (h::t) =
                  let
                    val typ = processPattern h enterResult level notConst isRec line
                   val U : unit = 
                     unify (typ, basicType, lex, line, foundNear pat);
                  in
                    applyList t
                  end;
              in
                applyList tlist;
                basicType (*Return the type variable - united to all the types*)
              end  (* processList *);
            in
              mkTypeConstruction ("list", listType, [processList ptl])
            end

          | Constraint {value, given} =>
            let
              val valType  = processPattern value enterResult level notConst isRec line;
              val U : unit = ptAssignTypes given pat line;

              (* These must be unifiable. *)
              val U : unit = unify (valType, given, lex, line, foundNear pat);
            in
              given
            end

          | Layered {var, pattern} =>
            let
              (* Unify the variable and the pattern - At this stage that simply
                 involves assigning the type of the pattern to the variable,
                 but it may result in more unification when the variable is
                 used *)
              
              (* The "variable" must be either id or id: ty but we have to
                 check that the id is not a constructor. *)
              val varType = processPattern var     enterResult level true isRec line;
              val patType = processPattern pattern enterResult level notConst isRec line
              val U : unit = unify (varType, patType, lex, line, foundNear pat);
            in
              varType
            end

          | Unit =>
              unitType

          | WildCard =>
              mkTypeVar (generalisable, false, false, false)

          | _ => (* not a legal pattern *)
              badType

        end; (* processPattern *)

        
        (* Applies "assignValues" to every element of a list and unifies the
           types. Returns a type variable if the list is empty. *)
        fun assignList tlist =
        let   (* Construct a type variable and unite all the types to that. *)
          val basicType = mkTypeVar (generalisable, false, false, false);
          fun applyList [] = ()
            | applyList (h::t) =
            let
              val typ      = assValues v h;
              val U : unit = unify (typ,  basicType, lex, line, foundNear v);
            in
              applyList t
            end;
        in
          applyList tlist;
          basicType (* Return the type variable - united to all the types *)
        end (* assignList *)

        (* val assValues = assignValues level line env; *)
        and assValues near v =
          case v of
            Ident {name, value, typeof} =>
	    let
	      val expValue =
		lookupValue 
		  ("Value or constructor",
		   {lookupVal = #lookupVal E, lookupStruct = #lookupStruct E},
		   name,
		   giveError (near, lex, line));
	      (* Set the value and type found. *)
	      val instanceType = instanceType expValue true (* expression *);
	    in
	      value  := expValue;
	      typeof := instanceType;
	      instanceType (* Result is the instance type. *)
	    end

          | Literal{converter, typeof, ...} =>
		  	   let
				(* Find out the overloadings on this converter and
				   construct an instance of it.  The converters are
				   all functions from string to the result type. *)
			      val instanceType = overloadType(converter, true)
				  val instance =
				  	apply(instanceType, stringType, lex, line, foundNear near)
			   in
			   	  typeof := instance; (* Remember it *)
				  instance
			   end

          | Applic {f, arg} => 
            let
              (* Apply the function to the argument and return the result. *)
              val funType = assValues near f;   (* SPF 22/10/94 *)
              val argType = assValues near arg;
            in
              apply (funType, argType, lex, line, foundNear near)
            end

          | Cond {test, thenpt, elsept} =>
            let
	      (* The test must be bool, and the then and else parts must be the
		 same. The result is either of these two once they have been
		 unified. *)
	      val testType = assValues v test;
	      val U : unit = unify (testType, boolType, lex, line, foundNear v);
	      
	      val thenType = assValues v thenpt;
	      val elseType = assValues v elsept;
	      val U : unit = unify (thenType, elseType, lex, line, foundNear v);
	    in
	      thenType (* or equally elseType *)
	    end

          | TupleTree ptl =>
            (* Construct the type obtained by mapping "assignValue" onto
               each element of the tuple. *)
              mkProductType (map (assValues near) ptl) (* SPF 22/10/94 *)
          
          | Labelled {recList, frozen, typeof} =>
            let
              (* Process each item in the list. *)
            
              fun msgFn msg = errorNear (lex, true, v, line, msg);
			  
			  fun labEntryToLabType (name, value) =
			  	{name = name, typeof = assValues v value }
            
              val expType =
                mkLabelled 
                  (sortLabels (map labEntryToLabType recList, msgFn),
                   frozen) (* should always be true *);
            in
              typeof := expType;
              expType
            end

          | Selector {typeof, ...} =>
              typeof (* Already made. *)

          | ValDeclaration {dec, explicit, implicit, variables} =>
              assValDeclaration dec explicit implicit variables

		  | FunDeclaration {dec, explicit, implicit} =>
		      assFunDeclaration dec explicit implicit

		  | OpenDec{decs=ptl, variables} =>
	            let
		      (* Go down the list of names opening the structures. *)
		      (* We have to be careful because open A B is not the same as
			  open A; open B if A contains a structure called B. *)
			  (* We accumulate the values so that we can produce debugging
			     information if we need to.  Note: we have to be careful if
				 we have the same name in multiple structures. *)
			  val valTable = HashTable.hashMake 10
	
		      (* First get the structures... *)
		      fun findStructure ({name, ...}: structureIdentForm) = 
			lookupStructure
			  ("Structure", {lookupStruct = #lookupStruct E}, name,
			    giveError (v, lex, line))
	    
		      val strs : structVals list = map findStructure ptl;

			  (* Value and substructure entries in a structure will generally have
			     "Formal" access which simply gives the offset of the entry within
				 the parent structure.  We need to convert these into "Select"
				 entries to capture the address of the base structure. *)
		      fun copyEntries str =
			if isUndefinedStruct str then ()
			else let
			  val sigTbl = structSignat str; (* Get the tables. *)
			  
			  fun copyEntry (dName, dVal, ()) = 
			    if (tagIs typeConstrVar dVal)
			      then let
				  	(* If this is a datatype we have to apply mkSelectedVar to the
					   value constructors.  It will not do anything if the constructors
					   are global but if we are opening the argument to a functor we
					   need to do this.  It's essential if we subsequently replicate
					   this datatype.  Note: we don't add the value constructors to the
					   environment at this point.  While a signature cannot contain
					   values which "mask" constructors we can have a structure
					   containing values which mask constructors. *)
					val tcons = tagProject typeConstrVar dVal
				  in
				  	#enterType E (dName, 
					  	if null(tcConstructors tcons)
						then tcons (* Not a datatype. *)
						else mkSelectedType(tcons, tcName tcons, SOME str))
				  end
			      
			    else if (tagIs valueVar dVal)
			      then let
				  	(* If this is a datatype we could use findValueConstructor here
					   to save constructing a new object.  It's probably not worth it. *)
					val selectedVar = 
					  mkSelectedVar (tagProject valueVar dVal, str);
			      in
				    HashTable.hashSet(valTable, dName, selectedVar);
					#enterVal E (dName, selectedVar)
			      end
			      
			    else if (tagIs structVar dVal)
			      then let
					val selectedStruct = 
					  makeSelectedStruct (tagProject structVar dVal, str);
			      in
					#enterStruct E (dName, selectedStruct)
			      end
			    else ()
			in
			  univFold (sigTab sigTbl, copyEntry, ())
			end
	
		      (* ...then put them into the name space. *)
		      val U : unit = forall copyEntries strs;
		    in
			  variables := HashTable.hashFold valTable (fn _ => fn v => fn t => v :: t) [];
		      badType (* Does not return a type *)
		    end
	
		  | TypeDeclaration tlist =>
		    let (* Non-generative type binding *)
		      fun messFn name = 
			errorNear (lex, true, v, line,
			   name ^ " has already been bound in this declaration");
			   
		      val newEnv = noDuplicates messFn;
		      
		      (* First match all the types on the right-hand sides. *)
		      fun processTypeBody (TypeBind {decType, ...}) =
		            ptAssignTypes decType v line
		        
		      val U : unit = forall processTypeBody tlist;
		      
		      (* Can now declare the new types. *)
		      fun processType (TypeBind {name, typeVars, decType, isEqtype}) =
		      let
			(* Construct a type constructor which is an alias of the
			   right-hand side of the declaration. If the right-hand
			   side were a type constructor we could use the same unique
			   id, but it is probably not worth it. *)
			val tcon =
			  makeTypeConstrs (strName ^ name, typeVars, decType, makeTypeId (),
			                   isEqtype (* In most cases we look at the equivalent. *),
							   0 (* This is only required for datatypes. *));
		      in
			checkForDots  (name, lex, line); (* Must not be qualified *)
			#enter newEnv (name, tcon); (* Check for duplicates. *)
			#enterType E  (name, tcon)  (* Put in the surrounding scope. *)
		      end
		           
	              val U : unit = forall processType tlist;
		    in
		      badType (* Does not return a type *)
		    end
	    
		  | DatatypeDeclaration 
		      {typelist, withtypes, declist} =>
		         assAbsData false typelist withtypes declist
	
		  | AbstypeDeclaration 
		      {typelist, withtypes, declist} =>
		         assAbsData true typelist withtypes declist

		  | DatatypeReplication{oldType, newType} =>
		  		(* Adds both the type and the constructors to the
				   current environment. *)
	  		let
			(* Look up the type constructor in the environment. *)
				val oldTypeCons: typeConstrs =
					lookupTyp 
             			({lookupType = #lookupType E, lookupStruct = #lookupStruct E},
              			oldType,
              			giveError (near, lex, line));

				(* If the type name was qualified (e.g. S.t) we need to find the
				   value constructors from the same structure. *)
				val {first = namePrefix, ...} = splitString oldType;
				val baseStruct =
					if namePrefix = ""
					then NONE
					else SOME(lookupStructure("Structure", {lookupStruct = #lookupStruct E},
								namePrefix, giveError (v, lex, line)))

				(* Copy the datatype, converting any Formal constructors to Selected. *)
				val newTypeCons = mkSelectedType(oldTypeCons, strName ^ newType, baseStruct)

				val newValConstrs = tcConstructors newTypeCons
			in
				(* Check that it has at least one constructor. *)
				case newValConstrs of
					[] => errorNear (lex, true, v, line, oldType ^ " is not a datatype")
				|	_ => ();
				(* Enter the value constrs in the environment. *)
				List.app (fn c => (#enterVal E) (valName c, c)) newValConstrs;
				(* Add this type constructor to the environment. *)
				(#enterType E) (newType, newTypeCons);
				badType (* Does not return a type *)
			end

          | List ptl =>
              mkTypeConstruction ("list", listType, [assignList ptl])

          | Constraint {value, given} =>
		    let
		      val valType   = assValues near value; (* SPF 22/10/94 *)
		      val U : unit  = ptAssignTypes given v line;
		      (* These must be unifiable. *)
		      val U : unit  = unify (valType, given, lex, line, foundNear v);
		    in
		      given
		    end

          | Fn ptl =>  (* Must unify the types of each of the alternatives.*)
              assignList ptl 

          | MatchTree {vars, exp, line, resType} =>
		    let 
		      (* A match is a function from the pattern to the expression *)
		      
		      (* Process the pattern looking for variables. *)
	    
		       (* Construct a new environment for the variables. *)
		      fun messFn name =  
				errorNear (lex, true, v, line,
				  name ^ " has already been bound in this match");
			  
		      val newEnv   = noDuplicates messFn;
		      val newLevel = level + 1;
		      val decs     = processPattern vars (#enter newEnv) newLevel false false line;
	    
		      (* The identifiers declared in the pattern are available in the
			 body of the function. *)
		      val bodyEnv =
			let
			  val Env E = env
			in
			  Env
			    {
			      lookupVal     = lookupDefault (#lookup newEnv) (#lookupVal E),
			      lookupType    = #lookupType E,
			      lookupFix     = #lookupFix E,
			      lookupStruct  = #lookupStruct E,
			      lookupSig     = #lookupSig E,
			      lookupFunct   = #lookupFunct E,
			      lookupTvars   = #lookupTvars E,
			      enterVal      = #enterVal E,
			      enterType     = #enterType E,
			      enterFix      = #enterFix E,
			      enterStruct   = #enterStruct E,
			      enterSig      = #enterSig E,
			      enterFunct    = #enterFunct E
			    }
			 end;
	    
		      (* Now the body. *)
		      val expType = assignValues newLevel letDepth line bodyEnv v exp;
		    in
			  resType := expType;
			  (* Check the type of parameters to the function to make
			     sure they have not been unified with local datatypes.
				 We don't need to check the result type because the check
				 in "Localdec" will do that. *)
			  checkForLocalDatatypes(decs, letDepth, giveError (v, lex, line));
		      (* Result is a function from the type of the pattern to the type
			 of the body. This previously generalised the resulting type.
			  Why? *)
		      mkFunctionType (decs, expType)
		    end (* MatchTree *)

          | Unit =>
              unitType

          | Localdec {decs, body, loc, varsInBody} =>
		    let (* Local declarations or expressions. *)
		      val newValEnv  = searchList();
		      val newTypeEnv = searchList();
		      val newStrEnv  = searchList();
			  val newLetDepth = if loc then letDepth else letDepth+1;
		      (* The environment for the local declarations. *)
		      val localEnv =
			let
			  val Env E = env
			in
			  Env
			    {
			       lookupVal     = lookupDefault (#lookup newValEnv)  (#lookupVal E),
			       lookupType    = lookupDefault (#lookup newTypeEnv) (#lookupType E),
			       lookupFix     = #lookupFix E,
			       (* This environment is needed if we open a 
				  structure which has sub-structures. *)
			       lookupStruct  = lookupDefault (#lookup newStrEnv) (#lookupStruct E),
			       lookupSig     = #lookupSig E,
			       lookupFunct   = #lookupFunct E,
			       lookupTvars   = #lookupTvars E,
			       enterVal      = #enter newValEnv,
			       enterType     = #enter newTypeEnv,
				  (* Fixity has already been dealt with in the parsing process.  The only reason
				     we deal with it here is to ensure that declarations are printed in the
					 correct order.  We simply need to make sure that local fixity declarations
					 are ignored. *)
			       enterFix      = fn _ => (),
			       enterStruct   = #enter newStrEnv,
			       enterSig      = #enterSig E,
			       enterFunct    = #enterFunct E
			    }
			end;
	    
		      (* Process the local declarations and discard the result. *)
		      val U : types = assignSeq localEnv newLetDepth decs;
	    
		      (* This is the environment used for the body of the declaration.
			 Declarations are added both to the local environment and to
			 the surrounding scope. *)
		      val bodyEnv =
			let
			  val Env L = localEnv
			in
			  Env
			    { 
			      (* Look-ups come from the local environment *)
			      lookupVal     = #lookupVal L,
			      lookupType    = #lookupType L,
			      lookupFix     = #lookupFix L,
			      lookupStruct  = #lookupStruct L,
			      lookupSig     = #lookupSig L,
			      lookupFunct   = #lookupFunct L,
			      lookupTvars   = #lookupTvars L,
			      enterVal      =
					fn (pair as (name, v)) =>
					  (varsInBody := v :: ! varsInBody;
					   #enter newValEnv pair;
					   #enterVal E      pair),
			      enterType     =
					fn (pair as (name, v)) =>
					  (#enter newTypeEnv pair;
					   #enterType E      pair),
			      enterFix      = #enterFix E,
			      enterStruct   =
					fn (pair as (name, v)) =>
					  (#enter newStrEnv pair;
					   #enterStruct E   pair),
			      enterSig      = #enterSig E,
			      enterFunct    = #enterFunct E
			    }
			end;
		      (* Now the body, returning its result if it is an expression. *)
				val resType = assignSeq bodyEnv newLetDepth body
		    in
				(* If this is a let expression we have to check that there
				   are no datatypes escaping. *)
				if loc then ()
				else checkForLocalDatatypes(resType, letDepth,
						giveError (v, lex, line));
				resType
		    end (* LocalDec *)

          | ExpSeq ptl =>
             (* A sequence of expressions separated by semicolons.
                Result is result of last expression. *)
              assignSeq env letDepth ptl

          | ExDeclaration tlist =>
	    let
	      fun messFn name =
	       errorNear (lex, true, v, line,
		 name ^ " has already been bound in this declaration");
		 
	      (* Construct an environment to check for duplicate declarations.
		 There is no need for a value since all this is doing is
		 checking. *)
	      val dupEnv = noDuplicates messFn;
  
          fun processException (ExBind {name, previous, typeof, value}) =
          let
			(* Fill in any types *)
			val U : unit = ptAssignTypes typeof v line;
			val U : unit =
			  if isEmpty typeof then ()
			  else let (* Make the type weak. *)
			    val weakTv = 
			      mkTypeVar (generalisable, false, false, true);
			  in
			    unify (typeof, weakTv, lex, line, foundNear v)
			  end;
	
			val exValue = 
			  case previous of 
			    EmptyTree => (* Generative binding. *)
			      mkEx (name, typeof)
			  | Ident {name = prevName, value = prevValue, ...} =>
				  let 
				    (* ex = ex' i.e. a non-generative binding? *)
				    (* Match up the previous exception. *)
				    val prev = 
				      lookupValue 
					("Exception",
					  {lookupVal= #lookupVal E,
					  lookupStruct= #lookupStruct E},
					  prevName,
					  giveError (v, lex, line))
				  in
				    (* Check that it is an exception *)
					case prev of
							Value{class=Exception, ...} => ()
						|	_ => errorNear (lex, true, v, line, "(" ^ prevName ^ ") is not an exception.");
				    prevValue   := prev; (* Set the value of the looked-up identifier. *)
				    (* The result is an exception with the same type. *)
				    mkEx (name, valTypeOf prev)
				  end
			  | _ =>
			     raise InternalError "processException: badly-formed parse-tree"
	      in
		(* Save this value. *)
		value := exValue;
		
		(* In the check environment *)
		#enter dupEnv (name, emptyType);
		
		(* Must not be qualified *)
		checkForDots (name, lex, line) : unit;
		(* Must not be "true", "false" etc. *)
		checkForBuiltIn (name, v, lex, line, true) : unit;
		
		(* Put this exception into the env *)
		#enterVal E (name, exValue) 
	      end
  
	      val U : unit = forall processException tlist;
	    in
	      badType
	    end (* ExDeclaration *)
	    
	  | Raise pt =>
	    let
	      val exType = assValues v pt;
	      (* The exception value must have type exn. *)
	      val U : unit = unify (exnType, exType, lex, line, foundNear v);
	    in
	      (* Matches anything *)
	      mkTypeVar (generalisable, false, false, false)
	    end
  
	  | HandleTree {exp, hrules} =>
	    let
	      (* If the expression returns type E
	         the handler must be exn -> E *)
	      val expType = assValues v exp;
	     (* Unify the handler with a function from exn -> expType *)
	      val U : unit = 
		 unify (assignList hrules, 
			mkFunctionType (exnType, expType),
			 lex, line, foundNear v);
	    in
	      expType (* Result is expType. *)
	    end

          | While {test, body} =>
	    let
	      val testType = assValues v test;
	      
	      (* Test must be bool. Result is unit *)
	       val U : unit = unify (testType, boolType, lex, line, foundNear v);
	       
	      (* Result of body is discarded. *)
	      val U : types = assValues v body;
	    in
	      unitType
	    end

	  | Case {test, match} =>
	    let
	      val funType = assignList match;
	      val argType = assValues v test;
	    in
	      (* The matches constitute a function from the test type to
		 the result of the case statement, so we apply the match type
		 to the test. *)
	      apply (funType, argType, lex, line, foundNear v)
	    end

          | Andalso {first, second} =>
	    let
	      (* Both parts must be bool and the result is bool. *)
	      val fstType  = assValues v first;
	      val U : unit = unify (fstType, boolType, lex, line, foundNear v);
	      
	      val sndType  = assValues v second;
	      val U : unit = unify (sndType, boolType, lex, line, foundNear v);
	    in
	      boolType
	    end

          | Orelse {first, second} =>
	    let
	      (* Both parts must be bool and the result is bool. *)
	      val fstType  = assValues v first;
	      val U : unit = unify (fstType, boolType, lex, line, foundNear v);
	      
	      val sndType  = assValues v second;
	      val U : unit = unify (sndType, boolType, lex, line, foundNear v);
	    in
	      boolType
	    end

          | Directive { tlist, fix } => 
		  		(
				(* Infix declarations have already been processed by the parser.  We include
				   them here merely so that we get all declarations in the correct order. *)
				List.app (fn name => #enterFix E (name, fix)) tlist;
				badType
				)

          | WildCard => (* Should never occur in an expression. *)
		  		raise InternalError "assignTypes: wildcard found"

          | Layered _ => 
		  		raise InternalError "assignTypes: layered pattern found"

          | EmptyTree => 
		  		raise InternalError "assignTypes: emptytree found"
			(* end of assValues *)

        and assValDeclaration (valdecs: valbind list) explicit implicit variables =
	let
	  val newLevel = level + 1;
	  
	  (* Set the scope of explicit type variables. *)
	  val U: unit =
	    #apply explicit(fn (_, tv) => setTvarLevel (tv, newLevel));

	  (* For each implicit type variable associated with this value declaration,
	     link it to any type variable with the same name in an outer
	     scope. *)
	  val U : unit = 
	    #apply implicit
	      (fn (name, tv) =>
		 (linkTypeVars (#lookupTvars E name, tv)
		    handle ValueMissing _ => setTvarLevel (tv, newLevel)));
		(* If it isn't there set the level of the type variable. *)

	  (* Construct a new environment for the variables. *)
	  val newEnv =
	     noDuplicates
	       (fn name => errorNear (lex, true, v, line,
		name ^ " has already been bound in this declaration"));

	  (* This environment is those identifiers declared by
	     recursive bindings *)
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
	  fun mapProcess [] isRec = []
	    | mapProcess (RecValBind :: tlist) isRec =
	      (* If we have  val x=1 and rec ... we will have an and-list
		  as the last element of the list. (It must be the last
		  because the inner and-list will swallow all the rest).
		  All those entries will be recursive. *)
		mapProcess tlist true

	    | mapProcess ((ValBind {dec,line,...}) :: ptl) isRec =
		      let
				(* To check that every binding actually declares
				   something i.e. to catch val _ = 99, we enter the
				   names through the following environment. *)
				val nothingEntered = ref true;
				fun enterVals (pair as (name, v)) =
				   (nothingEntered := false;
				    #enter newEnv pair;
				    if isRec then #enter recEnv pair else ());
							       
				val patType = processPattern dec enterVals newLevel false isRec line;
				  
				(* Give a warning if no variables are declared, except at
				   top level.  This check was removed in ML97. *)
		      in
				if !nothingEntered andalso level <> 1 andalso !ml90
				then errorNear (lex, false, dec, line,
					      "Pattern does not declare any variables.")
				else ();
				patType :: mapProcess ptl isRec
		      end;

	  val decs = mapProcess valdecs false;

	  (* Now the bodies. *)

	  (* Check that the types match by going down the list of value
	     bindings and the list of types produced from the patterns,
	     and matching corresponding types. *)
	  fun checkTypes [] [] isRec = ()
	    | checkTypes dl (RecValBind :: tlist) isRec =
		checkTypes dl tlist true
	      
	    | checkTypes (d::dl) ((ValBind {dec, exp, line,...}) :: ptl) isRec =
	      let
		val newEnv =
		  let
		    val Env E = env
		  in
		    Env
		     { (* If this is recursive we find the recursive names
			  and others in the surrounding scope. *)
		       lookupVal     = 
			 if isRec
			 then lookupDefault (#lookup recEnv) (#lookupVal E)
			 else #lookupVal E,
		       lookupType    = #lookupType E,
		       lookupFix     = #lookupFix E,
		       lookupStruct  = #lookupStruct E,
		       lookupSig     = #lookupSig E,
		       lookupFunct   = #lookupFunct E,
		       (* Extend the environment of type variables. *)
		       lookupTvars   =
			  	lookupDefault (#lookup explicit)
					(lookupDefault (#lookup implicit) (#lookupTvars E)),
		       enterVal      = #enterVal E,
		       enterType     = #enterType E,
		       enterFix      = #enterFix E,
		       enterStruct   = #enterStruct E,
		       enterSig      = #enterSig E,
		       enterFunct    = #enterFunct E
		     }
		  end


		val typ = assignValues newLevel letDepth line newEnv exp exp;
		    
		val U : unit = unify (d, typ, lex, line, foundNear v);
		
		(* true if the expression is a possibly-constrained fn-expression *)
		fun isConstrainedFn (exp : parsetree) : bool =
		  case exp of
		    Constraint {value, ...} => isConstrainedFn value
		  | Fn _  => true
		  | _     => false;
	      in
		(* Must check that the expression is of the form FN match. *)
		(* N.B. the code generator assumes this is true. *)
		if isRec andalso not (isConstrainedFn exp)
		then errorNear (lex, true, v, line, 
		      "Recursive declaration is not of the form `fn match'")
		else ();
		
		checkTypes dl ptl isRec
	      end
	      
	    | checkTypes decs _ isRec =
		raise InternalError "checkTypes: badly-formed parse-tree";


	   (* Variables, constructors and fn are non-expansive.
	      [] is a derived form of "nil" so must be included.
	      Integer and string constants are also constructors but
	      cannot involve imperative type variables. Constrained
	      versions are also non-expansive.
		  This has been extended and made more explicit in ML 97. *)
	   fun nonExpansive (Fn _)   = true
	     | nonExpansive (Ident _) = true
	     | nonExpansive (List []) = true
		 | nonExpansive (List elems) = not (!ml90) andalso
		 					revfold (fn v => fn a => a andalso nonExpansive v)
								true elems
	     | nonExpansive (Constraint {value, ...}) = nonExpansive value
		 | nonExpansive (Literal _) = true
		 | nonExpansive Unit = true
		 | nonExpansive (TupleTree elems) = 
		 	   not (!ml90)
			   	andalso revfold (fn v => fn a => a andalso nonExpansive v)
							true elems
		 | nonExpansive (Labelled{recList, ...}) =
		 	   not (!ml90)
			   	andalso revfold (fn (n, v) => fn a => a andalso nonExpansive v)
							true recList (* Every element must be non-expansive *)
		 | nonExpansive (Applic{f, arg}) =
		 	   not (!ml90) andalso isNonRefConstructor f andalso nonExpansive arg
		 | nonExpansive (Selector _) = not (!ml90) (* derived from fn {..} => ...*)
	     | nonExpansive _       = false

		(* An application is non-expansive only if it is a, possibly
		   constrained, constructor which is not ref. *)
	  and isNonRefConstructor (Ident {value=ref v, name, ...}) =
	  		(* Rather than looking at the name it might be better to look
			   at the operation.  This is probably ok since we're not allowed
			   to rebind "ref". *)
	  		isConstructor v andalso name <> "ref"
	    | isNonRefConstructor (Constraint {value, ...}) =
				isNonRefConstructor value
		| isNonRefConstructor _ = false

	  (* Now allow generalisation on the variables being declared.
	     For imperative type variables we have to know whether the
	     expression is expansive. *)
	  fun allowGen decs [] = ()
	    | allowGen decs (RecValBind :: tlist) = allowGen decs tlist
	      
	    | allowGen (d::dl) ((ValBind {exp, line,...}) :: ptl) =
	      let
			val U : unit =
			  allowGeneralisation 
			    (d, newLevel, nonExpansive exp, lex, line, foundNear v);
			(* Check the type to make sure that a local datatype is
			   not escaping.  Checking here is really only needed in the
			   recursive case (where a recursive call inside the body
			   sets the type of the function) because the result will be
			   checked elsewhere. *)
			val U: unit =
				checkForLocalDatatypes(d, letDepth, giveError (v, lex, line))
	      in
			allowGen dl ptl
	      end (* allowGen *)
	    
	    | allowGen _ _ =
		raise InternalError "allowGen: badly-formed parse-tree";
		
	  val U : unit = checkTypes decs valdecs false;
	  val U : unit = allowGen decs valdecs;
	  (* And declare the new names into the surrounding environment. *)
	  val U : unit = #apply newEnv
	  	(fn nv as (_, var) => (#enterVal E nv; variables := var :: !variables));
	in
	  badType (* Type should not be used *)
	end (* assValDeclaration *)

        and assFunDeclaration (tlist: fvalbind list) explicit implicit =
	let
	  val funLevel = level + 1; (* Level for function names. *)
	  
	  (* Set the scope of explicit type variables. *)
	  val U: unit =
	    #apply explicit(fn (name, tv) => setTvarLevel (tv, funLevel));

	  (* For each implicit type variable associated with this value declaration,
	     link it to any type variable with the same name in an outer
	     scope. *)
	  val U : unit = 
	    #apply implicit
	      (fn (name, tv) =>
		 (linkTypeVars (#lookupTvars E name, tv)
		    handle ValueMissing _ => setTvarLevel (tv, funLevel)));
		(* If it isn't there set the level of the type variable. *)

	  (* Construct a new environment for the variables. *)
	  fun msgFn name = 
	    errorNear (lex, true, v, line,
	      name ^ " has already been bound in this declaration");
	       
	  val newEnv = noDuplicates msgFn;

	  fun getName pat =
	    case pat of
	      Constraint {value, ...} => getName value
	    | Applic {f, arg}         => getName f
	    | Ident {name, ...}       => name
	    | _  =>  ""; (* error - report it later. *)
	       
	  fun getArity pat =
	    case pat of
	      Constraint {value, ...} => getArity value
	    | Applic {f, arg}         => getArity f + 1
	    | Ident {name, ...}       => 0
	    | _  =>  0; (* error - report it later. *)
	       
	  (* Since this is a recursive declaration we must get the function
	     names first. Because of the way they are parsed they are hidden
	     as applications of the function to one or more patterns. There
	     may be more than one clause in a function binding but each
	     should declare the same function and have the same number of
	     patterns. We need to know the number of patterns and the
	     function name in the third pass so we save them in the
	     function binding. *)
	     
	  (* findNameAndPatts. Find the name and number of patterns in the
	     first of the clauses. The other clauses should be the same.
	     We check that later. *)
	  fun findNameAndPatts (FValBind {clauses = (FValClause {dec, exp, line}::_), numOfPatts, functVar, ...}) =
	  let
	    (* Just look at the first clause for the moment. *)
	    val name  = getName  dec;
	    val arity = getArity dec;
	    
	    (* Declare a new identifier with this name. *)
	    val funVar =
	       mkVar (name, mkTypeVar (funLevel, false, false, false));
	       
	    val U : unit = numOfPatts := arity;

	   val U : unit =
	      (* Put the results onto the function binding. *)
	      if arity = 0
	      then errorNear (lex, true, v, line,
		     "Clausal function does not have any parameters.")
	      else ();
	  in
	    if name <> ""
	    then let
	      (* Must not be qualified *)
	      val U : unit = checkForDots (name, lex, line);
		  (* Must not be "true", "false" etc. but may be "it" *)
		  val U : unit = checkForBuiltIn (name, v, lex, line, false) : unit;

	      (* Look up the name to check it isn't a constructor. *)
		  (* This check no longer applies in ML97. *)
	      val value =
			lookupDefault (#lookupVal E) (fn s => undefinedValue) name;
	      val U :unit =
			if !ml90 andalso isConstructor value
			then errorNear (lex, true, v, line,
			       "Variable " ^ String.toString name ^
				 " already declared as a constructor")
			else ()
		
	      val U : unit = functVar := funVar;
	    in
	      (* Enter it in the environment. *)
	      #enter newEnv (name, funVar)
	    end
	    else ()
	  end
	  | findNameAndPatts _ = 
	      raise InternalError "findNameAndPatts: badly-formed parse-tree";

	  val U : unit = forall findNameAndPatts tlist;
	  
	 (* Can now process the clausal functions in the environment 
	    of the function names and using the information about
	    function name and number of patterns we have saved. *)
	  fun processFun (FValBind {clauses, numOfPatts, functVar, resultType})=
	  let
	    val functVar  = !functVar;

	   (* Each fun binding in the declaration may consist of several
	      clauses. Each must have the same function name, the same
	      number of patterns and a unifiable type. *)
	    fun processClause (FValClause {dec, exp, line}) =
	    let 
	      (* Each clause is a val binding with the function 
		and patterns as the `pattern' and the function body
		as the `expression' *)
	      
	     (* Construct a new environment for the variables
		in the patts. *)
	     fun messFn name =
	       errorNear (lex, true, dec, line,
		 name ^ " has already been bound in this clause.");
	      
	      val varEnv = noDuplicates messFn;
	      
	      val varLevel = funLevel + 1; (* Level for variables. *)

	      (* Processes a single alternative. Similar to processPattern. 
		 A variable for the function being declared is passed as a
		 parameter so that its type can be used for the result. *)
	      fun doPatterns pat numOfPats : types =
		case pat of
		  Constraint {value, given} => 
		  let (* Check the constraint against the returned type. *) 
		    val patType  = doPatterns value numOfPats;
		    val U : unit = ptAssignTypes given pat line;
		    
		    (* These must be unifiable. *)
		    val U : unit =
		      unify (patType, given, lex, line, foundNear dec);
		  in
		    given
		  end

		| Applic {f, arg} =>
		  let 
		    (* Apply the function to this pattern. Return the result
		       type. This will have the effect of making the
		       function we are declaring into a function from the
		       type of the pattern to some other type yet to be
		       determined. This type will be found when we unify
		       with the body of the function. *)
		    val funType = doPatterns f (numOfPats - 1);
		    val argType = 
		      processPattern arg (#enter varEnv) varLevel false false line;
		  in
		    apply (funType, argType, lex, line, foundNear dec)
		  end

		| Ident {name, value, ...} =>
		  let 
		    (* We have presumably reached the function name. *)
		    val functName = valName functVar; 
		  in
		    (* Must check this is the same name. *)
		    if name = functName then ()
		    else errorNear (lex, true, dec, line,
			     "In clausal function one clause defines "^
			      name ^ " and another defines " ^ functName);
		    
		    (* And it has the same number of patterns. *)
		    if numOfPats = 0 then ()
		    else errorNear
			    (lex, true, dec, line,
			     "Clausal function contains clauses with " ^
			     "different numbers of patterns");
		    
		    value := functVar;
		    valTypeOf functVar (* Return function type. *)
		  end
		
		| EmptyTree =>
		    badType
		  
		| _ =>
		  let (* error *)
		    val U : unit = 
		       errorNear (lex, true, pat, line,
			     "Start of clausal function is not a variable")
		  in
		    badType
		  end
	      (* end doPatterns *);

	      val pattType = doPatterns dec (!numOfPatts);

	     (* The identifiers declared in the pattern are available in the
		body of the function. Since it is recursive the function
		names are also available. *)
	      val bodyEnv =
		let
		  val Env E = env
		in
		  Env 
		    { 
		      lookupVal     = 
			lookupDefault (#lookup varEnv)
		       (lookupDefault (#lookup newEnv) (#lookupVal E)),
		      lookupType    = #lookupType E,
		      lookupFix     = #lookupFix E,
		      lookupStruct  = #lookupStruct E,
		      lookupSig     = #lookupSig E,
		      lookupFunct   = #lookupFunct E,
		      (* Extend the environment of type variables. *)
		      lookupTvars   =
			  	lookupDefault (#lookup explicit)
					(lookupDefault (#lookup implicit) (#lookupTvars E)),
		      enterVal      = #enterVal E,
		      enterType     = #enterType E,
		      enterFix      = #enterFix E,
		      enterStruct   = #enterStruct E,
		      enterSig      = #enterSig E,
		      enterFunct    = #enterFunct E
		    }
	       end;
	       
	      (* Now the body. *)
	      (* The type from `doPatterns' is the effect of applying
		 the function to the patterns. This must be unified with
		 the type of the expression which will set the result type
		 of the function. *)
	      val expTyp = 
	        assignValues varLevel letDepth line bodyEnv exp exp;

	    in (* body of processClause *)
		  (* Remember the result type for the debugger. Actually this
		     assigns the result type for each clause in the fun but
			 they will all be the same type because we've used the same
			 variable for the function in each pattern. *)
		  resultType := expTyp;
		  (* Unify the pattern and the clause body. *)
	      unify (pattType, expTyp, lex, line, foundNear dec)
	    end
	  in (* body of processFun *)
	    forall processClause clauses
	  end
	      
	  val U : unit = forall processFun tlist;
	  
	  (* Now declare the new names into the surrounding environment,
	     releasing the copy flags on the type variables. All fun
	     bindings are non-expansive. *)
	  val U : unit = 
	  #apply newEnv 
	    (fn (pair as (name, var)) =>
	     let
		   val ty: types = valTypeOf var
	       val U : unit =
	         allowGeneralisation(ty, funLevel, true, lex, line, foundNear v);
			(* Check the type to make sure that a local datatype is
			   not escaping as a result of a recursive application of
			   the function to a local datatype. *)
			val U: unit =
				checkForLocalDatatypes(ty, letDepth, giveError (v, lex, line))
	     in
	       #enterVal E pair
	     end);
	in
	  badType (* Type should not be used *)
	end (* FunDeclaration *)

        and assAbsData isAbs (typeList : datatypebind list) withtypes declist =
	let
	  (* A type declaration causes a type to be entered in the type
	     environment, together with some constructors. *)
	  fun messFn name = 
	    errorNear (lex, true, v, line,
	       name ^ " has already been bound in this declaration");

	  val newEnv = noDuplicates messFn;
	  
	  (* datatype and abstype declarations are both recursive so we can
	     enter the type names into the environment during a first pass,
	     and then process the value constructors during a second. *)
	  fun enterType tcon typeName =
	   (
	    checkForDots  (typeName, lex, line) : unit; (* Must not be qualified *)
	    #enter newEnv (typeName, tcon); (* Check for duplicates. *)
	    #enterType E  (typeName, tcon)  (* and put in the enclosing scope *)
	   );
	   
	   (* Make the type constructors and put them in a list. *)
	  fun enterTcon (DatatypeBind {name, tcon, typeVars, ...}) =
	    let
		  val tc =
		  	makeTypeConstrs (strName ^ name, typeVars, emptyType,
						     makeTypeId (), false, letDepth)
	    in
	      tcon := tc;
		  enterType tc name;
		  tc
	    end
	  
	  val listOfTypes = map enterTcon typeList;

	 (* First match all the types on the right-hand sides using the
	     datatypes and the existing bindings. *)
	  fun processType (TypeBind {decType, ...}) = ptAssignTypes decType v line
	  val U : unit = forall processType withtypes;

	  (* Can now enter the `withtypes'. *)
	  fun enterWithType (TypeBind {name, typeVars, decType, ...}) =
	    let
	      (* Construct a type constructor which is an alias of the
		 right-hand side of the declaration. *)
	      val tcon =
		 makeTypeConstrs (strName ^ name, typeVars, decType, makeTypeId (), 
		                  false, 0);
	    in
	      enterType tcon name
	    end
	  val U: unit = forall enterWithType withtypes;
	    
	  (* For the constructors *)
	  fun messFn name =
	    errorNear (lex, true, v, line,
	      name ^ " has already been used as a constructor in this type");
	  
	  val consEnv = noDuplicates messFn;
    
	  (* Now process the types and generate the constructors. *)
	  fun genValueConstrs (DatatypeBind {name, typeVars, constrs, ...}, typ) =
	  let
	    val numOfConstrs = length constrs;
	    
	    (* The new constructor applied to the type variables (if any) *)
	    val resultType = mkTypeConstruction (name, typ, typeVars);

		(* Sort the constructors by name.  This simplifies matching with
		   datatypes in signatures. *)
	    fun leq (xname: string, _) (yname, _) = xname < yname;
	    val sortedConstrs = quickSort leq constrs;

	    fun processConstr ((name, argtype), repn) =
	    let
	      val cons = 
			if isEmpty argtype
			then mkGconstr (name, resultType, repn, true)
			else mkGconstr (name, mkFunctionType (argtype, resultType), repn, false);
	    
	      (* Name must not be qualified *)
	      val U : unit = checkForDots (name, lex, line);
		  (* Must not be "true", "false" etc. *)
		  val U : unit = checkForBuiltIn (name, v, lex, line, true) : unit;
	      
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
		  val _ = List.app(fn (_, t) => ptAssignTypes t v line) cs

		  (* Choose the representation for the constructors. *)
	      val reprs = chooseConstrRepr cs;
	    in
	      ListPair.map processConstr (cs,reprs)
	    end; (* decCons*)

	  in
	    tcSetConstructors (typ, decCons sortedConstrs) : unit
	  end (* genValueConstrs *)
	  
	  val U : unit = ListPair.app genValueConstrs (typeList, listOfTypes);


	  (* Third pass - Check to see if equality testing is allowed for
	     these types. No error messages should be produced.  In ML90
		 we did not do this here for datatypes in signatures. Instead
		 all the datatypes in a signature were analysed as a whole. *)
	  val U : unit =
	    genEqualityFunctions(listOfTypes, fn s => raise InternalError s, false) ;

	  (* If this is a datatype declaration the value constructors should be
	     entered in the surrounding scope, otherwise (abstract type
	     declaration) we evaluate the declarations in an environment
	     containing the value constructors, but the value constructors
	     themselves do not appear in the surrounding scope. *)
	  val U : unit =
	    if not isAbs
	    then #apply consEnv (#enterVal E)
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
		  val Env E    = env
		  val localEnv = searchList ();
		  fun enterValFn (pair as (name, v)) = 
		    (#enter localEnv pair;
		     #enterVal E     pair);
		  val lookupValFn = 
		    lookupDefault (#lookup localEnv)
		     (lookupDefault (#lookup consEnv) (#lookupVal E))
		in
		  Env
		   { 
		     lookupVal     = lookupValFn,
		     lookupType    = #lookupType E,
		     lookupFix     = #lookupFix E,
		     lookupStruct  = #lookupStruct E,
		     lookupSig     = #lookupSig E,
		     lookupFunct   = #lookupFunct E,
		     lookupTvars   = #lookupTvars E,
		     enterVal      = enterValFn,
		     enterType     = #enterType E,
		     enterFix      = #enterFix E,
		     enterStruct   = #enterStruct E,
		     enterSig      = #enterSig E,
		     enterFunct    = #enterFunct E
		  }
		end;
  
	      (* Process the declarations, discarding the result. *)
	      val U : types = assignSeq decEnv letDepth declist;
	      
	      fun setConstructors (DatatypeBind {tcon=ref tc, valueConstrs, ...}) = 
		(	
		  tcSetEquality (tc, false);
		  valueConstrs := tcConstructors tc;
		  tcSetConstructors (tc, [])
		)
	    in
	      (* Now clobber the equality operations and the constructor list
		 - equality and printing are not allowed outside the abstract
		 type declaration, and the type should not match a datatype
		 in a signature. The problem is that we need the constructor
		 list for exhaustiveness checking of patterns inside the abstype
		 declaration but we do exhaustiveness checking during the code
		 generation phase. *)
	      forall setConstructors typeList
	    end;
        in
	  badType (* Does not return a type *)
	end (* assAbsData *)
      in 
        assValues near v
      end (* assignValues *);
      
      val env = 
        Env
          {
            lookupVal     = #lookupVal E,
            lookupType    = #lookupType E,
            lookupFix     = #lookupFix E,
            lookupStruct  = #lookupStruct E,
            lookupSig     = #lookupSig E,
            lookupFunct   = #lookupFunct E,
            lookupTvars   = fn name => raise ValueMissing name,
            enterVal      = #enterVal E,
            enterType     = #enterType E,
            enterFix      = #enterFix E,
            enterStruct   = #enterStruct E,
            enterSig      = #enterSig E,
            enterFunct    = #enterFunct E
          };
    in
      assignValues 1 0 line env v v
    end (* pass2 *);



(*****************************************************************************
                                PASS 3
                        Code Tree Generation
******************************************************************************)

          (* Generate code from ML parse tree. *)
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
	 fun tupleWidth(TupleTree l) = List.length l

	  |  tupleWidth(Labelled{typeof, ...}) =
           if recordNotFrozen (! typeof) (* An error, but reported elsewhere. *)
           then 1 (* Safe enough *)
           else recordWidth (! typeof)

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
	  		tupleWidth(#1(List.last body))

	  |  tupleWidth(Case{match, ...}) =
	  		let
				fun getWidth(MatchTree{exp, ...}) = tupleWidth exp
				|	getWidth _ = raise InternalError "getWidth"
			in
				List.foldl(fn(v, 1) => getWidth v | (_, s) => s)
						  1 match
			end

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
                   addresses, structName: string, line, globalEnv) : codetree list * debugenv =
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

		(* Add a call to the debugger. *)
		fun addDebugCall (decName: string, (ctEnv, rtEnv), line: int) : codetree =
			let
				val debugger =
					debugFunction(filename lex, decName, line) ctEnv
			in
				lastLine := line;
				mkEval(mkConst(unsafeCast debugger), [rtEnv(!level)], false)
			end

		(* Add a debug call if line has changed.  This is used between
		   declarations and expression sequences to avoid more than one
		   call on a line. *)
		fun changeLine (decName, env, line) =
			if not(!debug) orelse line = !lastLine then []
			else [addDebugCall(decName, env, line)]

		fun createDebugEntry (v: values, (ctEnv, rtEnv), loadVal) =
			if not (!debug)
			then { dec = [], rtEnv = rtEnv, ctEnv = ctEnv }
			else let
					val newEnv =
					(* Create a new entry in the environment. *)
					  	mkTuple [ loadVal (* Value. *), rtEnv(!level) ]
					val { dec, load } = multipleUses (newEnv, mkAddr, !level)
					val ctEntry =
						case v of
							Value{class=Exception, name, typeOf, ...} =>
								EnvException(name, typeOf)
						|   Value{class=Constructor{nullary}, name, typeOf, ...} =>
								EnvVConstr(name, typeOf, nullary)
						|	Value{name, typeOf, ...} =>
								EnvValue(name, typeOf)
				in
					{ dec = dec, rtEnv = load, ctEnv = ctEntry :: ctEnv}
				end

		(* Start a new static level.  This is currently used only to
		   distinguish function arguments from the surrounding static
		   environment. *)
	    fun newDebugLevel (ctEnv, rtEnv) = (EnvStaticLevel :: ctEnv, rtEnv)

		fun makeDebugEntries (vars: values list, debugEnv: debugenv) =
		 	if ! debug
			then
				let
					fun loadVar (var, (decs, env)) =
						let
							val loadVal =
								codeVal (var, !level, valTypeOf var, lex, line, globalEnv)
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
		fun wrapFunctionInDebug(body: codetree, name: string, restype: types): codetree =
			if not (! debug) then body (* Return it unchanged. *)
			else
			let
				val entryCode = unsafeCast (DEBUGGER.enterFunction name)
				and exitCode = unsafeCast (DEBUGGER.leaveFunction restype)
				and exceptionCode = unsafeCast DEBUGGER.exceptionFunction
			in
				mkEnv [
					(* Call the enter code. *)
					mkEval(mkConst entryCode, [CodeZero (* unit *)], false),
					(* Call the exit code with the function result. The
					   function is wrapped in a handler that catches all
					   exceptions and calls the exception code. *)
					mkEval(mkConst exitCode,
						[mkHandle (body, [CodeZero (* all exceptions *)],
							mkEval(mkConst exceptionCode, [Ldexc], false))
						], false)
				]
			end
 		
		(* datatype 'a option = SOME of 'a | NONE *)

		(* Convert a literal constant. We can only do this once any overloading
		   has been resolved. *)
		fun getLiteralValue(converter, literal, instance, line, near): word option =
			let
		 	   val (conv, name) =
			   	  getOverloadInstance(valName converter, instance, true, lex, line)
			in
				SOME(unsafeCast(evalue conv)(literal))
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

          fun isEmpty (PatSet p) = isNil p;
          fun first   (PatSet p) = hd p; 
          fun next    (PatSet p) = PatSet (tl p); 
          
          (* Set from i to j inclusive. *)
          fun from i j = if i > j then empty else i ::: from (i + 1) j;
    
          fun stringOfSet p = 
            "[" ^ 
            revfold (fn i => fn s => s ^ " " ^ Int.toString i) "" (list p) ^ 
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
			  eqFun:   codetree,	(* Equality functions for this type*)
			  specVal: word option,	(* The constant value. NONE here means we had
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
            fun addConstr cons doArg (tree as Aot {patts, defaults, width, vars}) patNo =
            let
              val consName = valName cons;
            in
              case patts of
                Wild =>
                let (* Expand out the wildCard into a constructor node. *)
                 (* Get the constructor list from the type information
                    of the constructor and put the length of this list
                    into the "width". *)
		  val noOfConstrs = length (getConstrList (valTypeOf cons));
		  
		  val cr = 
		    makeConsrec 
		      cons 
		      (singleton patNo) (* Expand the argument *)
		      (doArg (wild tree));
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
				   word*word->bool version.  The constants could be pointers
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
						Value{class=Constructor _, ...} =>
						  (* Only nullary constructors. Constructors with arguments
						     will be dealt with by ``isApplic'. *)
						  	addConstr ident (fn a => buildAot wildCard a patNo line) tree patNo
					|	Value{class=Exception, ...} =>
					  		addExconstr ident wildCard tree patNo
					|   _ => (* variable - matches everything. Defaults here and pushes a var. *)
					  		addVar (addDefault tree patNo) ident
				)
    
            | TupleTree ptl => (* Tree must be a wild card or a tuple. *)
             (case treePatts of
                 Wild =>
                 let
				   val tlist =
				     map (fn el => buildAot el (wild tree) patNo line) ptl;
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
  
            | Labelled {recList, frozen, typeof} =>
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
		  if recordNotFrozen (! typeof)
		  then errorNear (lex, true, vars, line,
				  "Can't find a fixed record type.")
		  else ();
	
		(* Make a list of wild cards. *)
		fun buildl 0 = []
		  | buildl n = wildCard :: buildl (n-1);
		
		(* Get the maximum number of patterns. *)
		val wilds = buildl (recordWidth (! typeof));
	
		(* Now REPLACE entries from the actual pattern, leaving
		   the defaulting ones behind. *)
		(* Take a pattern and add it into the list. *)
		fun mergen (h :: t) 0 pat = pat :: t
		  | mergen (h :: t) n pat = h :: mergen t (n - 1) pat
		  | mergen []       _ _   = raise InternalError "mergen";
		
		fun enterLabel ((name, value)) l = 
		    (* Put this label in the appropriate place in the tree. *)
		    mergen l (entryNumber (name, ! typeof)) value
		      
		val tupleList = revfold enterLabel wilds recList;
	      in
		(* And process it as a tuple. *)
		buildAot (TupleTree tupleList) tree patNo line
	      end
  
            | Applic{f = Ident{value = ref applVal, ...}, arg} =>
			 (
				 case applVal of
				 	Value{class=Constructor _, ...} =>
						addConstr applVal (fn atree => buildAot arg atree patNo line) tree patNo
	
				 |	Value{class=Exception, ...} => addExconstr applVal arg tree patNo

				 |	_ => tree (* Only if error *)
			)
    
            | Applic _ => tree (* Only if error *)

            | Unit =>
                (* There is only one value so it matches everything. *)
                addDefault tree patNo
              
            | WildCard =>
                addDefault tree patNo (* matches everything *)
              
            | List ptl =>
              let (* Generate suitable combinations of cons and nil.
                    e.g [1,2,3] becomes ::(1, ::(2, ::(3, nil))). *)
                    
		fun processList [] tree = 
		    (* At the end put in a nil constructor. *)
		    addConstr nilConstructor (fn a => buildAot wildCard a patNo line) tree patNo
		  | processList (h :: t) tree = (* Cons node. *)
		let
		  fun mkConsPat (Aot {patts = TupleField [hPat, tPat],
		                      defaults,  width, vars}) =  
		  let   (* The argument is a pair consisting of the
			   list element and the rest of the list. *)
		    val tlist = [buildAot h hPat patNo line, processList t tPat];
		  in
		    makeAot (TupleField tlist) defaults 0 vars
		  end
		   | mkConsPat (tree  as Aot {patts = Wild, defaults,
		                              width, vars}) =  
		  let
		    val hPat  = wild tree;
		    val tPat  = wild tree;
		    val tlist = [buildAot h hPat patNo line, processList t tPat];
		  in
		    makeAot (TupleField tlist) defaults 0 vars
		  end
		   | mkConsPat _ = 
		       raise InternalError "mkConsPat: badly-formed parse-tree"
		in
		  addConstr consConstructor mkConsPat tree patNo
		end
		(* end processList *);
	      in
		processList ptl tree
	      end
  
		  	| Literal{converter, literal, typeof=ref instance} =>
				let
				   (* At the same time we have to get the equality function
				      for this type to plug into the code.  This will find
					  a type-specific equality function if there is one
					  otherwise default to structure equality. *)
			 	   val (equality, _) =
				   	  getOverloadInstance("=", instance, false, lex, line)
				   val litValue: word option =
				      getLiteralValue(converter, literal, instance, line, vars)
				in
					addSconstr equality litValue tree patNo
				end
            
            | Constraint {value, given} => (* process the pattern *)
                buildAot value tree patNo line
              
            | Layered {var, pattern} =>  (* process the pattern *)
              let  
                (* A layered pattern may involve a constraint which
                   has to be removed. *)
		fun getVar pat =
		  case pat of
		    Ident {value, ...}      => !value
                  | Constraint {value, ...} => getVar value
		  | _                       => undefinedValue (* error *);
	      in
		addVar (buildAot pattern tree patNo line) (getVar var)
	      end
    
            | _ =>
               tree (* error cases *)
          end; (* buildAot *)
  
          fun buildTree (patts: parsetree list) =
          let   (* Merge together all the patterns into a single tree. *)
            fun maket []     patNo tree = tree
              | maket ((MatchTree{vars, line, ...})::t) patNo tree =
                 	maket t (patNo + 1) (buildAot vars tree patNo line)
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
              
            | TupleTree ptl =>
                revfold findVars varl ptl
              
            | Labelled {recList, ...} =>
                revfold (fn (_, value) => findVars value) varl recList
              
             (* Application of a constructor: only the argument
                can contain vars. *)
            | Applic {f, arg} =>
                findVars arg varl
              
            | List ptl =>
                revfold findVars varl ptl
              
            | Constraint {value, ...} =>
                findVars value varl
              
            | Layered {var, pattern} =>
                 (* There may be a constraint on the variable
                    so it is easiest to recurse. *)
                findVars pattern (findVars var varl)
               
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
          val raiseMatchCode : patcode = makePatcode [raiseMatch] 0;
          val raiseBindCode  : patcode = makePatcode [raiseBind] 0;
 
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
					 revfold (fn t => fn d => max (pattDepth t) d) 0 pl
					 
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
					val thisDepth = pattDepth (nth asTuples column);
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
				      codePatt (nth asTuples bestcol) (mkInd (bestcol, load)) active
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
            | cgExps (MatchTree {exp, line, ...} ::al) (vl::vll)
					base patNo uses decName debugEnv cgExpression lex near =
              let
                val noOfArgs = length vl;
                val patNoIndex = patNo - 1;
                val pattUses = uses sub patNoIndex;
                
                val U : unit =
                   if pattUses = 0
                   then errorNear (lex, false, near, line,
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
					mkEnv(envDec @ [cgExpression (exp, newEnv, decName, line)]);

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
                cgExpression : parsetree * debugenv * string * int -> codetree,
                isHandlerMatch : bool)
               : codetree =
            let
              val noOfPats  = length alt;
              val andortree = buildTree alt;
              val allVars   = findAllVars alt;
			  val lineNo =
			  	case alt of
					MatchTree {line, ... } :: _ => line
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
             val uses = array (noOfPats, 0);
    
             (* Set to false if we find it is not exhaustive. *)
             val exhaustive = ref true;
             
             (* Make some code to insert at defaults. *)
             val codeDefault : unit -> patcode =
               if isHandlerMatch
               then (fn () => makePatcode [makeRaise loadExpCode] 0)
               else (fn () => (exhaustive := false; raiseMatchCode));
         
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
                      update (uses, pattChosenIndex,(uses sub pattChosenIndex) + 1);
                      
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
							MatchTree {exp, line, ... } :: _ =>
							   makePatcode [cgExpression (exp, env, decName, line)]
							   		pattChosen
						  | _ => raise InternalError "codeMatch - badly formed parsetree"
					  )
                      else let
                        val thisVars    = nth allVars pattChosenIndex;
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
              fun codeDefault () = (exhaustive := false; raiseBindCode);
              
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
           
        fun codeSequence (dlist: (parsetree * int) list, debugEnv: debugenv, decName: string)
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
			  	 fun cgExp (c: parsetree, debugEnv: debugenv, decName: string, line: int) =
				 	if (! debug)
					then mkEnv[addDebugCall(decName, debugEnv, line),
							   codegen(c, debugEnv, decName, line)]
					else codegen(c, debugEnv, decName, line) 
			  in
	            codeMatch (near, alt, arg, lex, decName, debugEnv, cgExp, false)
			  end

          (* Code-generates a piece of tree. *)
          and codegen (c: parsetree, debugEnv: debugenv, decName: string, line: int) : codetree =
          let
            fun codeList debugEnv [] = []
			 |  codeList debugEnv ((x, line)::tl) =
			 	(* Generate any line change code first, then this entry, then the rest. *)
			 		changeLine(decName, debugEnv, line) @ (codegen (x, debugEnv, decName, line)
						:: codeList debugEnv tl)
          in
            case c of 
              Ident {value, typeof, ...} =>
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
							codeExFunction (v, !level, !typeof, lex, line)
					|	Value{class=Constructor _, ...} =>
						let
							(* When using the constructor as a value we just want
							   then second word. *)
							val constrTuple = codeVal (v, !level, !typeof, lex, line, globalEnv)
						in
							mkInd(1, constrTuple)
						end
					|	_ => codeVal (v, !level, !typeof, lex, line, globalEnv)
			      end
		  
		  	| Literal{converter, literal, typeof=ref instance} =>
				(
				case getLiteralValue(converter, literal, instance, line, c) of
					SOME w => mkConst w
				  | NONE => CodeNil
				)

            | Applic {f, arg} =>
		      let
				(* The overloaded functions of more than one argument are
				   applied to their arguments rather than to a tuple. *)
				(* The only other optimisation we make is to remove applications
				   of constructors such as ``::'' which are no-ops. *)
				val argument : codetree = codegen (arg, debugEnv, decName, line);
		      in
				(* If the function is an identifier then see if it is a global
				   constructor. If it is not then we must code-generate the
				   whole identifier, not the value it is bound to. *)
				case f of
				  Ident {value, typeof, ...} =>
				    let
				      val function : values = !value;
				      val instanceType = !typeof;
				    in
				      applyFunction (function, argument,
					     !level, instanceType, lex, line, globalEnv)  : codetree
				    end
				| _ => 
				  mkEval (codegen (f, debugEnv, decName, line), [argument],
				          false) (* not early *) : codetree
		      end
  
            | Cond {test, thenpt, elsept} =>
                mkIf (codegen (test,   debugEnv, decName, line),
                      codegen (thenpt, debugEnv, decName, line),
                      codegen (elsept, debugEnv, decName, line)) : codetree
  
            | TupleTree [pt] => (* can this occur? *)
		        codegen (pt, debugEnv, decName, line) (* optimise unary tuples *)
  
            | TupleTree ptl =>
		      let  (* Construct a vector of objects. *)
				val args = map (fn x => codegen (x, debugEnv, decName, line)) ptl;
		      in
				mkTuple args : codetree
		      end
  
            | Labelled {recList = [(_, value)],  ...} =>
                codegen (value, debugEnv, decName, line) (* optimise unary tuples *)
  
            | Labelled {recList, typeof, ...} =>
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
				    val args = mapIterator look (upto 0 (recordSize - 1));
				  in
				    [mkTuple args]
				  end
				  
				  | declist ((name, value) :: t) look =
				  let
				    val thisDec = 
				      multipleUses (codegen (value, debugEnv, decName, line), mkAddr, !level);
					
				    val myPosition = entryNumber (name, !typeof);
				    
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
  
            | Selector {name, labType, ...} =>
              let
                (* Check that the type is frozen. *)
                val U =
                   if recordNotFrozen labType
                   then errorNear (lex, true, c, line,
                                   "Can't find a fixed record type.")
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
  
            | Unit => (* Use zero.  It is possible to have () = (). *)
                CodeZero : codetree
  
            | List ptl =>
              let  (* Construct a list. *)
                (* At the end of the list put a "nil" *)
                fun consList []       = CodeZero
                  | consList (h :: t) =
                  let
                    val H = codegen (h, debugEnv, decName, line);
                    val T = consList t;
                  in
                    mkTuple [H,T]
                  end (* consList*);
              in
                consList ptl : codetree
              end
    
            | Constraint {value, ...} =>
                (* code gen. the value *)
                codegen (value, debugEnv, decName, line) : codetree
  
            | Fn _ =>
                (* Function *)
                mkblock (codeProc c decName false (ref 0)) : codetree
   
            | Localdec {decs, body, ...} =>
              (* Local expressions only. Local declarations will be handled
                 by codeSequence.*)
              let 
                val (decs, newDebug) = codeSequence (decs, debugEnv, decName);
                val exps = codeList newDebug body;
              in
                mkblock (decs @ exps) : codetree
              end
  
            | ExpSeq ptl =>
              (* Sequence of expressions. Discard results of all except the
                 last. It isn't clear whether this will work properly since
                 the code-generator does not expect expressions to return
                 results unless they are wanted. It may be necessary to turn
                 all except the last into declarations. *)
                mkblock (codeList debugEnv ptl) : codetree
  
            | Raise pt =>
                makeRaise (codegen (pt, debugEnv, decName, line)) : codetree

            | HandleTree {exp, hrules} =>
              (* Execute an expression in the scope of a handler *)
              let
                val handleExp : codetree = codegen (exp, debugEnv, decName, line);
                
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
                                   codeVal (#constructor cons, !level, emptyType, lex,
								   			line, globalEnv))
                             exList
                      else [CodeZero]
                  | _ => [CodeZero]
                  
                val handlerCode : codetree = 
                  codeMatch (c, hrules, Ldexc, lex, decName, debugEnv, codegen, true)
              in
                mkHandle (handleExp, matchTagList, handlerCode) : codetree
              end

            | While {test, body} =>
                makeWhile 
                  (codegen (test, debugEnv, decName, line),
                   codegen (body, debugEnv, decName, line)) : codetree
  
            | Case {test, match} =>
	          (* The matches are made into a series of tests and
			     applied to the test expression. *)
              let
                val testCode : codetree =
                  codegen (test, debugEnv, decName, line)
              in
				codeMatch (c, match, testCode, lex, decName, debugEnv, codegen, false) : codetree
		      end
    
            | Andalso {first, second} =>
              (* Equivalent to  if first then second else false *)
                mkCand (codegen (first,  debugEnv, decName, line),
                        codegen (second, debugEnv, decName, line)) : codetree
  
            | Orelse {first, second} =>
              (* Equivalent to  if first then true else second *)
                mkCor (codegen (first,  debugEnv, decName, line),
                       codegen (second, debugEnv, decName, line)) : codetree
  
            | _ => (* empty and any others *)
               CodeNil : codetree
  
          end (* codegen *)
  
           (* Generate a function either as a free standing lambda expression
               or as a declaration. *)
          and codeProc c decName isRecursive varAddr =
          let
            fun getFnBody (exp : parsetree) : parsetree list = 
		      case exp of
				Constraint {value, ...} => getFnBody value
			      | Fn e  => e
			      | _     => raise InternalError "getFnBody: not a constrained fn-expression";
          
            val f        = getFnBody c;
            val U        = level := !level + 1; (* This function comprises a new declaration level*)
            val oldAddr  = !addresses;
            val U        = addresses := 1;
            val (firstPat, resType) = 
              case f of 
                MatchTree {vars, resType = ref rtype,...} :: _  => (vars, rtype)
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

              val mainProc =
                 mkProc
				 	(wrapFunctionInDebug
	                   (codeAlt (c, f, mkArgTuple tupleSize 1, newDecName, newDebugEnv),
					    newDecName, resType), 
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
            in
              varAddr   := addr;        (* Reset the address *)
              level     := !level - 1;  (* Reset level and addresses *)
              addresses := oldAddr;
              let
                val pr =
					mkProc (
						wrapFunctionInDebug(alt, newDecName, resType),
						!level + 1, 1, newDecName);
              in
                [if isRecursive then mkDec (addr, pr) else pr]
              end
            end
          end (* codeProc *);
  
         in      (* codeSequence *)
           if isNil dlist then ([], debugEnv)
           else let
		     val lineChangeCode = changeLine(decName, debugEnv, #2 (hd dlist))

             val c : parsetree = #1 (hd dlist); (* First in the list. *)
             
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
                   val U = forall setAddr tlist;
                 end

                     (* Now we can process the function bindings. *)
                 fun loadFunDecs []               = []
                   | loadFunDecs ((FValBind{numOfPatts = ref numOfPats,
				   						    functVar = ref var, clauses,
											resultType = ref resType})::otherDecs) =
                   let
                     (* Make up the function, and if there are several mutually
                        recursive functions, put it in the vector. *)
                     val address   =
					 	case var of
							Value{access=Local{addr, ...}, ...} => addr
						|	_ => raise InternalError "lvAddr"
                     val addr      = !address;
                     val procName  = decName ^ valName var;
  
                     (* Make a list of the patterns in the clause such that the lowest
                        pattern in the structure is the first on the list. *)
                     fun getPatts (Constraint {value, ...}) f acc =
					 		getPatts value f acc
					   | getPatts (Applic{f=applF, arg=applA, ...}) f acc =
					   		getPatts applF f (f applA :: acc)
					   | getPatts vars f acc = acc

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
						 	(FValClause{dec, ...} :: _) => getPatts dec tupleWidth []
						 | _ => raise InternalError "badly formed parse tree";

					 fun getResultTuple(FValClause{exp, ...}) = tupleWidth exp
	
					 val resultTuples =
					 	List.foldl(fn(t, 1) => getResultTuple t  | (_, s) => s) 1 clauses

					 (* If we're debugging we want the result of the
						function so we don't do this optimisation. *)
					 val resultTuple = if !debug then 1 else resultTuples

					 val extraArg = if resultTuple = 1 then 0 else 1

                     (* Count the total number of arguments needed. *)
                     val totalArgs =
					 	revfold (fn x => fn y => x + y) extraArg tupleSeq 

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
                       map (fn FValClause {dec=vbDec, exp=vbExp, line, ...} =>
                            let
                              val patList = getPatts vbDec (fn x => x) [];
                            in
                              mkMatchTree 
                                (if numOfPats = 1 then hd patList else TupleTree patList,
                                 vbExp,
                                 line)
                            end)
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
  
                     (* Reset level and addresses *)
                     val U = level := !level - 1;
                     val U = addresses := oldAddr;
        
                     val innerFun : codetree = 
                       mkProc (wrapFunctionInDebug(bodyCode, procName, resType),
					   	!level + 1, totalArgs, innerProcName);
                          
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
			    This won't be won't be available recursively in the
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
               | codeRecursive ((ValBind{dec=vbDec, exp=vbExp, ...})::ds) =
               let
                 local
                   (* The pattern being declared may be a variable or a
                      constraint or (perversely) 
                      a layered pattern or a wild-card. *)
                   fun getVars (Constraint{value, ...}) = getVars value
				    |  getVars (Layered{var, pattern, ...}) =
                          getVars var @ getVars pattern
				 	|  getVars (pat as (Ident _)) = [pat]
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
                   val U = forall setAddress vars;
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
								|	_ => raise InternalError "lvAddr";
		                 in
		                   (* Must be a function. This returns either a single
		                      declaration or possibly a pair of mutually recursive
		                      functions. *)
		                   (codeProc vbExp idname true lvAddr) @ rest
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
                   if isNil loaded orelse isNil (tl loaded)
                   then loaded (* Return the declarations (as a list) *)
                   else (* Put the declarations into a package of mutual decs. *)
                     [mkMutualDecs loaded]
                 end
               
               | codeDecs ((ValBind{dec=vbDec, exp=vbExp, ...})::otherDecs) =
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
				 	codeBind c vbDec (codegen (vbExp, debugEnv, name, line)) lex line debugEnv
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
  
           | ExDeclaration tlist =>
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
				   val (lvAddr, lvLevel) =
				      case ex of
					  	Value{access=Local{addr, level}, ...} => (addr, level)
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
							   copy of it. *)
							mkExIden ()
					  | Ident{value=ref prevVal, ...} =>
				      		(* Copy the previous value. N.B. We want the exception
							   identifier here so we can't call codegen. *)
							codeVal (prevVal, !level, emptyType, lex, line, globalEnv)
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
  
           | AbstypeDeclaration {typelist=typeList, declist, ...} =>
             let (* Code-generate the declarations. *)
               
               (* We have to put the constructors back onto the abstype
                  temporarily so that we can do exhaustiveness checking. *)
               val UUU = 
                  forall
                    (fn (DatatypeBind{tcon=ref tc, valueConstrs = ref vconstrs, ...}) =>
                       tcSetConstructors (tc, vconstrs)
                    )
                     typeList;
               (* The debugging environment for the declarations should include
			      the constructors but the result shouldn't.  For the moment
				  ignore the constructors. *)
               val (code, newDebug) = codeSequence (declist, debugEnv, decName);
              
              (* Now we can take the constructors off again. *)
               val U  : unit = 
                 forall 
                   (fn (DatatypeBind{tcon=ref tc, ...}) => tcSetConstructors (tc, []))
                   typeList;
             in
               (code, newDebug)
             end (* AbstypeDeclaration *)
  
           | DatatypeDeclaration {typelist, ...} =>
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
			 in
		      (decs, newDebugenv)
			 end

		   | OpenDec {variables=ref vars, ...} =>
		   		(* All we need to do here is make debugging entries. *)
              makeDebugEntries(vars, debugEnv)

           | _ => (* c is Directive or TypeDeclaration*)
              ([], debugEnv); 

			val (decRest, finalEnv) = codeSequence (tl dlist, firstEnv, decName)
         in    (* Append the remaining declarations. *)
           (lineChangeCode @ firstDec @ decRest, finalEnv)
         end
       end (* codeSequence *);
     in
       codeSequence ([(pt, line)], debugEnv, structName)
     end (* gencode *)
   end (* type *)
  
  end; (* parsetree abstype *)

end (* PARSETREE *);
