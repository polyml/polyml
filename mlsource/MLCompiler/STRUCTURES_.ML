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
    Title:      Module Structure and Operations.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

functor STRUCTURES_ (

(*****************************************************************************)
(*                  LEX                                                      *)
(*****************************************************************************)
structure LEX :
sig
  type lexan;
  type prettyPrinter;
  
  val errorProc:    lexan * int * (prettyPrinter -> unit) -> unit;
  val errorMessage:   lexan * int * string -> unit;
  val warningProc:  lexan * int * (prettyPrinter -> unit) -> unit;
  val lineno:       lexan -> int;
  val nullLex:      lexan; (* Used when no errors are expected - streams raise exceptions. *)
end;

(*****************************************************************************)
(*                  CODETREE                                                 *)
(*****************************************************************************)
structure CODETREE :
sig
  type word;
  type codetree;
  
  val CodeNil:      codetree;
  val CodeZero:     codetree;
  val mkLoad:       int * int -> codetree;
  val mkConst:      word -> codetree;
  val mkDec:        int * codetree  -> codetree;
  val mkInd:        int * codetree  -> codetree;
  val mkProc:       codetree * int * int * string -> codetree;
  val mkMacroProc:  codetree * int * int * string -> codetree;
  val mkStr:        string   -> codetree;
  val mkRaise:      codetree -> codetree;
  val mkEval:       codetree * codetree list * bool -> codetree;
  val mkTuple:      codetree list -> codetree;
  val mkEnv:        codetree list -> codetree;
  val multipleUses: codetree * (unit -> int) * int -> {load: int -> codetree, dec: codetree list};
end (* CODETREE *);


(*****************************************************************************)
(*                  STRUCTVALS                                               *)
(*****************************************************************************)
structure STRUCTVALS :
sig
  type signatures;
  type typeDependent;
  type codetree;
  type typeId;
  type types

  type 'a tagMethods;

  datatype structVals = 
    NoStruct
  | Struct of
    {
      name:   string,
      signat: signatures,
      access: valAccess
    }

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

  and valAccess =
  	Global   of codetree
  | Local    of { addr: int ref, level: int ref }
  | Selected of { addr: int,     base:  structVals }
  | Formal   of int
  | Overloaded of typeDependent (* Values only. *)

  (* Structures *)
        
  val undefinedStruct:    structVals;
  val isUndefinedStruct:  structVals -> bool;
  val structSignat:       structVals -> signatures;
  val structName:         structVals -> string;
  val structAccess:       structVals -> valAccess;
  
  val structVar:          structVals  tagMethods;

  val makeSelectedStruct: structVals * structVals -> structVals;
  val makeLocalStruct:    string * signatures -> structVals;
  val makeGlobalStruct:   string * signatures * codetree -> structVals;
  val makeFormalStruct:   string * signatures * int -> structVals;

  (* Functors *)
  
  type functors;

  val undefinedFunctor:   functors;
  val isUndefinedFunctor: functors -> bool;
  val functorName:        functors -> string;
  val functorArg:         functors -> structVals;
  val functorResult:      functors -> signatures;
  val functorAccess:      functors -> valAccess;
  
  val makeFunctor: string * structVals * signatures * valAccess -> functors;

 
  (* Signatures *)
  type univTable;
  val sigName:        signatures -> string;
  val sigTab:         signatures -> univTable;
  val sigMinTypes:    signatures -> int;
  val sigMaxTypes:    signatures -> int;
  
  val makeSignatures: string -> signatures;
  val makeCopy:       string * signatures * int * int -> signatures;

  (* type or structure identifiers *)
  val makeFreeId:     unit -> typeId;
  val makeVariableId: unit -> typeId;
  val makeBoundId:    int  -> typeId;
  
  val unsetId:        typeId;
  val isUnsetId:      typeId -> bool;
  val isFreeId:       typeId -> bool;
  val isBoundId:      typeId -> bool;
  val isVariableId:   typeId -> bool;
  val offsetId:       typeId -> int;
  val sameTypeId:     typeId * typeId -> bool;
  val unifyTypeIds:   typeId * typeId -> bool;

  (* Types *)
  
  (* Standard type constructors. *)
  type typeConstrs;
  
  val undefType:         typeConstrs;

  val tcName:            typeConstrs -> string;
  val tcArity:           typeConstrs -> int;
  val tcTypeVars:        typeConstrs -> types list;
  val tcEquality:        typeConstrs -> bool;
  val tcEquivalent:      typeConstrs -> types;
  val tcConstructors:    typeConstrs -> values list;
  val tcSetConstructors: typeConstrs * values list -> unit;
  val tcIdentifier:      typeConstrs -> typeId;
  
  val typeConstrVar:     typeConstrs tagMethods;
  
  val makeTypeConstrs:
  	string * types list * types * typeId * bool * int -> typeConstrs;

  val badType:   types;
  val emptyType: types;
  val isEmpty:   types -> bool;
  
  val makeValueConstr: string * types * bool * valAccess -> values;
  val isConstructor: values -> bool;

  (* Access to values, structures etc. *)

  val makeGlobal:   codetree -> valAccess;
  val makeLocal:    unit -> valAccess;
  val makeFormal:   int  -> valAccess;
  
  val isGlobal:     valAccess -> bool;
  val isLocal:      valAccess -> bool;
  val isFormal:     valAccess -> bool;
  val isSelected:   valAccess -> bool;

  val vaGlobal:     valAccess -> codetree;
  val vaFormal:     valAccess -> int;
  val vaLocal:      valAccess -> { addr: int ref, level: int ref };
  val vaSelected:   valAccess -> { addr: int,     base:  structVals };


  (* Values. *)
  
  val valName:         values -> string;
  val valTypeOf:       values -> types;

  val valueVar:        values      tagMethods;

  (* Infix status *)
  type fixStatus;

  val fixVar: fixStatus tagMethods;

  datatype env = 
    Env of
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

  val makeEnv: signatures -> env;
end (* STRUCTVALS *);

(*****************************************************************************)
(*                  VALUEOPS                                                 *)
(*****************************************************************************)
structure VALUEOPS :
sig
  type word
  type types;
  type codetree;
  type values;
  type structVals;
  type valAccess;
  type lexan;
  type typeConstrs;
  type fixStatus

  val mkGvar:        string * types * codetree -> values;
  val mkGex:         string * types * codetree -> values;
  val mkSelectedVar: values * structVals -> values;
  
  val codeStruct:     structVals * int -> codetree;
  val codeAccess:     valAccess  * int -> codetree
  val codeVal:        values * int * types * lexan * int *
			{ lookupFix: string -> fixStatus, lookupExnById: word -> values option} -> codetree
  val codeExFunction: values * int * types * lexan * int -> codetree
                    
  val lookupAny:  string * (string -> 'a) * (string -> structVals) *
                 (structVals -> string -> 'a) * string * 'a * (string -> unit) -> 'a
                    
  val lookupStructure:  string * {lookupStruct: string -> structVals} * 
                        string * (string -> unit) -> structVals
                                           
  val lookupStructureDirectly: string * {lookupStruct: string -> structVals} * 
                               string * (string -> unit) -> structVals
                                           
                  
  val lookupTyp:   {lookupType: string -> typeConstrs,
                    lookupStruct: string -> structVals} * 
                   string * (string -> unit) -> typeConstrs
end (* VALUEOPS *);


(*****************************************************************************)
(*                  TYPETREE                                                 *)
(*****************************************************************************)
structure TYPETREE :
sig
  type typeConstrs;
  type types;
  type lexan;
  type prettyPrinter;
  type typeId;
  type values;

  val mkTypeConstruction:   string * typeConstrs * types list -> types;
  val mkFunctionType:       types  * types -> types;

  (* Fill in the values of type variables and make checks. *)
  val assignTypes:          types * (string -> typeConstrs) * lexan * int -> unit;

  val exnType:              types;

   (* Match a candidate to a target type. *)
   val matchTypes: types * types * (typeId -> typeConstrs) *
                   lexan * int * (prettyPrinter -> unit) -> unit;


  (* Used to establish sharing constraints between type constructors. *)
  val linkTypeConstructors: typeConstrs * typeConstrs * (string -> unit) -> unit;

  (* Used to link a type constructor to a type as the result of a "where type"
     construction. *)
  val setWhereType: typeConstrs * typeConstrs * (string -> unit) -> unit;

  (* Check that a type constructor permits equality. *)
  val permitsEquality:      typeConstrs -> bool;

  val copyType:             types * (types -> types) * 
                               (typeConstrs -> typeConstrs) -> types;

  val setTypeConstr:        typeConstrs * (typeConstrs -> typeId) -> unit;

  val enterTypeConstrs:     typeConstrs * typeConstrs *
                            { enter: typeId * typeConstrs -> unit, 
                              lookup: typeId -> typeConstrs} -> unit;

  val identical:            types * types -> bool;
  val identicalConstr:      typeConstrs * typeConstrs -> bool;
  val makeEquivalent:       typeConstrs * types list -> types;
  val genEqualityFunctions: typeConstrs list * (string -> unit) * bool -> unit;
  val checkWellFormed:      types * (string -> unit) -> unit;

  val findValueConstructor: values -> values;

  val copyTypeConstr:  typeConstrs * (typeId -> bool) * 
                       (unit -> typeId) *
                       {enter: typeId * typeConstrs -> unit, 
                        lookup: typeId -> typeConstrs} *
						(types -> types) * string -> typeConstrs;

  val display:              types * int * prettyPrinter -> unit;
  val displayTypeConstrs:   typeConstrs * int * prettyPrinter -> unit;
  (* A list of type variables. *)
  val displayTypeVariables: types list * int * prettyPrinter -> unit;
  
  (* added SPF 16/4/95 *)  
  val sameTypeVar : types * types -> bool;

  (* Check for free type variables.  Added for ML97. *)
  val checkForFreeTypeVariables: string * types * lexan -> unit;

end (* TYPETREE *);

(*****************************************************************************)
(*                  PARSETREE                                                *)
(*****************************************************************************)
structure PARSETREE :
sig
  type word
  type parsetree;
  type types;
  type lexan;
  type prettyPrinter;
  type typeId;
  type env;
  type codetree;
  type environEntry
  type fixStatus
  type values

  val ptDisplay: parsetree * int * prettyPrinter -> unit;

  val pass2: parsetree * (unit -> typeId) * env * lexan * int * string -> types;

  type debugenv = environEntry list * (int->codetree)

  val gencode: parsetree * lexan * debugenv * int * int ref * string * int *
  					{ lookupFix: string -> fixStatus, lookupExnById: word -> values option}
					-> codetree list * debugenv
end;

(*****************************************************************************)
(*                 MISC                                                      *)
(*****************************************************************************)
structure MISC :
sig
  exception ValueMissing of string;  (* table lookup failure *)
  exception InternalError of string; (* compiler error *)

  val isNil : 'a list -> bool
  
  val fold   : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
  val revfold : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b

  val min : int -> int -> int
  val max : int -> int -> int

  type 'a iter;
  
  val downto   : int -> int -> int iter
  val for    : 'a iter -> ('a -> 'b) -> unit
  val forall : ('a -> unit) -> 'a list -> unit
  val overList        : 'a list -> 'a iter;
  val revfoldIterator : ('a -> 'b -> 'b) -> 'b -> 'a iter -> 'b
  
  val lookupDefault : ('a -> 'b) -> ('a -> 'b) -> 'a -> 'b
end;

(*****************************************************************************)
(*                  UTILITIES                                                *)
(*****************************************************************************)
structure UTILITIES :
sig
  val noDuplicates: (string -> unit) -> 
         { apply: (string * '_a -> unit) -> unit,
           enter:  string * '_a -> unit,
           lookup: string -> '_a };

  val searchList: unit -> { apply: (string * '_a -> unit) -> unit,
                            enter:  string * '_a -> unit,
                            lookup: string -> '_a };
  val splitString: string -> { first:string,second:string }
end;

(*****************************************************************************)
(*                  UNIVERSAL                                                *)
(*****************************************************************************)
structure UNIVERSAL :

sig
  type universal
  type 'a tagMethods
  
  val tagIs      : 'a tagMethods -> universal -> bool
  val tagProject : 'a tagMethods -> universal -> 'a
end;

(*****************************************************************************)
(*                  UNIVERSALTABLE                                           *)
(*****************************************************************************)
structure UNIVERSALTABLE:
sig
  type universal
  type univTable
  type 'a tagMethods
  
  val univEnter:  univTable * 'a tagMethods * string * 'a -> unit;
  val univLookup: univTable * 'a tagMethods * string -> 'a;
  val univFold:   univTable * (string * universal * 'a -> 'a) * 'a -> 'a;
end;

(*****************************************************************************)
(*                  DEBUG                                                    *)
(*****************************************************************************)
structure DEBUG :
sig
  val errorDepth: int ref;
  val inlineFunctors: bool ref;
  val ml90: bool ref;
end;

(*****************************************************************************)
(*                  ADDRESS                                                  *)
(*****************************************************************************)
structure ADDRESS:
sig
  type word
  val toWord : 'a -> word
end;


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
(*                  ARRAY                                                    *)
(*****************************************************************************)
structure ARRAY :
sig
  type 'a array
  
  val array  : int * '_a -> '_a array
  val update : 'a array * int * 'a -> unit
  val sub    : 'a array * int -> 'a
end;

(*****************************************************************************)
(*                  STRETCHARRAY                                             *)
(*****************************************************************************)
structure STRETCHARRAY :
sig
  type 'a stretchArray
  
  val stretchArray : int * '_a -> '_a stretchArray
  val update : '_a stretchArray * int * '_a -> unit
  val sub    : 'a stretchArray * int -> 'a
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
end;

(*****************************************************************************)
(*                  STRUCTURES sharing constraints                           *)
(*****************************************************************************)

sharing type
  LEX.lexan
= VALUEOPS.lexan
= TYPETREE.lexan
= PARSETREE.lexan

sharing type
  LEX.prettyPrinter
= PARSETREE.prettyPrinter
= TYPETREE.prettyPrinter
= PRETTYPRINTER.prettyPrinter

sharing type
  CODETREE.codetree
= VALUEOPS.codetree
= PARSETREE.codetree
= STRUCTVALS.codetree

sharing type
  STRUCTVALS.types
= VALUEOPS.types
= TYPETREE.types
= PARSETREE.types
= DEBUGGER.types

sharing type
  STRUCTVALS.values
= VALUEOPS.values
= PARSETREE.values
= TYPETREE.values

sharing type
  STRUCTVALS.typeId
= TYPETREE.typeId
= PARSETREE.typeId

sharing type
  STRUCTVALS.structVals
= VALUEOPS.structVals

sharing type
  STRUCTVALS.valAccess
= VALUEOPS.valAccess

sharing type
  STRUCTVALS.typeConstrs
= VALUEOPS.typeConstrs
= TYPETREE.typeConstrs

sharing type
  STRUCTVALS.tagMethods
= UNIVERSALTABLE.tagMethods
= UNIVERSAL.tagMethods
 
sharing type
  STRUCTVALS.env 
= PARSETREE.env
   
sharing type
  ADDRESS.word
= CODETREE.word
= DEBUGGER.word
= VALUEOPS.word
= PARSETREE.word

sharing type
  UNIVERSALTABLE.univTable
= STRUCTVALS.univTable

sharing type 
  UNIVERSALTABLE.universal
= UNIVERSAL.universal;

sharing type
  PARSETREE.environEntry
= DEBUGGER.environEntry

sharing type
  STRUCTVALS.fixStatus
= PARSETREE.fixStatus
= VALUEOPS.fixStatus
) : 

(*****************************************************************************)
(*                  STRUCTURES export signature                              *)
(*****************************************************************************)
sig
  (* Structures form the global name spaces. *)
  type structs;
  type structVals;
  type types;
  type parsetree;
  type lexan;
  type prettyPrinter;
  type values;
  type typeConstrs;
  type codetree;
  type signatures;
  type functors;
  type env;
  type sigBind and functorBind and structBind
  type word
  type fixStatus

  val isEmptyStruct:      structs -> bool;
  val emptyStruct:        structs  (* added 8/2/94 SPF *)
  val mkStructureDec:     structBind list -> structs;
  val mkStruct:           structs list -> structs;
  val mkSignatureDec:     sigBind list -> structs;
  val mkSig:              structs list -> structs;
  val mkFunctorDec:       functorBind list -> structs;
  val mkInclude:          structs list -> structs;
  val mkLocaldec:         structs list * structs list * bool * int -> structs;
  val mkTopLevel:         parsetree * int -> structs;
  val mkStructureBinding: string * structs * bool * structs * int -> structBind;
  val mkStructIdent:      string -> structs;
  val mkSigIdent:         string -> structs;
  val mkSignatureBinding: string * structs * int -> sigBind;
  val mkValSig:           string * types * int -> structs;
  val mkExSig:            string * types * int -> structs;
  val mkFunctorAppl:      string * structs -> structs;
  val mkFormalArg:        string * structs -> structs;
  val mkFunctorBinding:   string * structs * bool * structs * structs * int -> functorBind;
  val mkSharing:          bool * string list * int -> structs;
  val mkWhereType:		  structs * types list * string * types * int -> structs

  val pass2Structs:   structs list * lexan * env -> unit;

  val checkForFreeTypeVars:
  	((string*values->unit)->unit) * ((string*structVals->unit)->unit) *
		((string*functors->unit)->unit) * lexan -> unit

  val pass4Structs:
    codetree * structs list *
     {
       enterVal:     string * values      -> unit,
       enterStruct:  string * structVals  -> unit,
       enterFunct:   string * functors    -> unit,
       enterFix:     string * fixStatus   -> unit,
       enterType:    string * typeConstrs -> unit,
       enterSig:     string * signatures  -> unit
     } -> unit;

  val gencodeStructs: structs list * lexan *
			{ lookupFix: string -> fixStatus, lookupExnById: word -> values option} -> codetree;

  val displayStructs: structs list * int * prettyPrinter -> unit;
end (* STRUCTURES export signature *) =

(*****************************************************************************)
(*                  STRUCTURES functor body                                  *)
(*****************************************************************************)
struct
  open MISC; 
  open PRETTYPRINTER;
  
  open LEX;
  open CODETREE;
  open STRUCTVALS;
  open VALUEOPS;
  open TYPETREE;
  open PARSETREE;
  open UTILITIES;
  open DEBUG;
  open ADDRESS;
  open UNIVERSALTABLE;
  open UNIVERSAL; (* for tagMethods record selectors *)
  open DEBUGGER;

  val displayType = TYPETREE.display;

  (* Union of the various kinds of core language declaration.  Structures are included
     because they can be declared by opening a structure with substructures. *)
  datatype coreDeclaration =
  	CoreValue       of values
  | CoreType        of typeConstrs
  | CoreFix         of string*fixStatus (* Include the name because it isn't part of fixStatus. *)
  | CoreStruct      of structVals

  (* "structs" is the abstract syntax for the module language. *)
  datatype structs =
    StructureDec   of structBind list       (* List of structure decs *)
  | StructureIdent of structureIdentForm (* A structure name *)
  | StructDec      of structDecForm      (* struct ... end *)
  | SignatureDec   of sigBind list       (* List of signature decs *)
  | SignatureIdent of string             (* A signature name *)
  | SigDec         of structs list       (* sig ... end *)
  | ValSig         of valExSig
  | ExSig          of valExSig
  | FunctorDec     of functorBind list       (* List of functor decs. *)
  | FunctorAppl    of functorApplForm    (* Appln of a functor *)
  | Singleton      of singletonForm      (* Any other decln. *)
  | FormalArg      of formalArgStruct    (* Functor arg. *)
  | Sharing        of shareConstraint    (* Sharing constraints. *)
  | WhereType	   of whereTypeStruct    (* type realisation. *)
  | Localdec       of localdecStruct     (* Local/Let. *)
  | IncludeSig     of structs list       (* Include. *)
  | EmptyStruct                          (* Error cases. *)

  (* List of structures. *)
  withtype structBind =
      {
        name:      string,         (* The name of the structure *)
        sigStruct: structs,        (* Its signature *)
		opaque:	   bool,		   (* true if it was :> rather than : *)
        value:     structs,        (* And its value *)
        valRef:    structVals ref, (* The structure variable declared. *)
        line:      int
      } 

  and sigBind =
      {
        name:      string, (* The name of the signature *)
        sigStruct: structs,(* Its value *)
		sigRef:    signatures ref, (* The "value" of the signature. *)
        line:      int
      }   

  (* A reference to a name *)
  and structureIdentForm =
      {
        name:   string,        (* The name *)
        valRef: structVals ref (* The variable found. *)
      } 
  
   (* struct ... end *)
  and structDecForm =
      {
        alist: structs list, (* The list of items in it. *)
        value: signatures    (* The value *)
      }

  and singletonForm =
      {
        dec:   parsetree,           (* The value *)
        vars:  coreDeclaration list ref,     (* The declarations *)
        line:  int
      } 
  
  (* Signature of a value or exception. *)
  and valExSig =
      {
        name:   string,
        typeof: types,
        line:   int
      } 
  
  (* Application of a functor. *)
  and functorApplForm =
      {
        name:   string,
        arg:    structs,
        valRef: functors ref      (* The functor looked up. *)
      }

  (* Functor binding. *)
  and functorBind =
      {
        name:      string,
        sigStruct: structs,
		opaque:	   bool,		   (* true if it was :> rather than : *)
        body:      structs,
        arg:       structs,
        valRef:    functors ref,    (* The functor variable declared. *)
        line:      int
      } 

  and formalArgStruct =
      {
        name:      string,
        sigStruct: structs,
        valRef:    structVals ref
      } (* The structure variable. *) 

  and shareConstraint =
      {
        isType: bool,
        shares: string list,
        line:   int
      } 

  and whereTypeStruct =
      {
        sigExp: structs,
		typeVars: types list,
        typeName: string,
        realisation: types,
		line: int
      }


  (* Used for local strdec in strdec and let strdec in strexp. *)
  and localdecStruct =
      {
        decs:     structs list,
        body:     structs list,
        localDec: bool,
        line:     int
      }
  
  (* with *)
    fun isSignatureIdent (SignatureIdent x) = true | isSignatureIdent _ = false;
    fun isEmptyStruct     EmptyStruct       = true | isEmptyStruct    _ = false;
    
    (* Make a signature for initialisating variables and for
       undeclared signature variables. *)
    val undefinedSignature = makeCopy("UNDEFINED", makeSignatures "UNDEFINED", 0, 0);
  
    (* Construction functions called by the parser. *)
    val emptyStruct    = EmptyStruct; (* added SPF 8/2/94 *)
    
    val mkStructureDec = StructureDec;
    
    fun mkStructureBinding (name, signat, opaque, value, line) = 
        { 
          name      = name,
          sigStruct = signat,
		  opaque	= opaque,
          value     = value,
          valRef    = ref undefinedStruct,
          line      = line
        };
  
    fun mkStructIdent name =
      StructureIdent
        {
          name   = name,
          valRef = ref undefinedStruct
        };
  
  
    (* For struct...end, make a signature to accept the values. *)
    fun mkStruct alist =
      StructDec
        {
          alist = alist,
          value = makeSignatures ""
        };
  
    val mkSignatureDec = SignatureDec;
  
    fun mkSignatureBinding (name, sg, ln) =
        { 
          name     = name,
          sigStruct = sg,
          line      = ln,
		  sigRef   = ref undefinedSignature
        };
  
    val mkSigIdent = SignatureIdent;
  
    val mkSig = SigDec;
  
    fun mkTopLevel (dec, line) =
      Singleton  
        {
          dec   = dec,
          vars  = ref [],
          line  = line
        };
  
    val mkFunctorDec = FunctorDec;
  
    fun mkFunctorBinding (name, signat, opaque, body, arg, line) =
        {
          name      = name,
          sigStruct = signat,
		  opaque	= opaque,
          body      = body,
          arg       = arg,
          valRef    = ref undefinedFunctor,
          line      = line
        };
  
    fun mkFunctorAppl (name, arg) =
      FunctorAppl
        {
          name   = name,
          arg    = arg,
          valRef = ref undefinedFunctor
        };
  
    fun mkValSig (name, typeof, line) = 
      ValSig 
        {
          name   = name,
          typeof = typeof,
          line   = line
        };
  
    fun mkExSig (name, typeof, line) = 
       ExSig
        {
          name   = name,
          typeof = typeof,
          line   = line
        };
  
    fun mkFormalArg (name, signat) =
      FormalArg
        {
          name      = name,
          sigStruct = signat,
          valRef    = ref undefinedStruct
        };
  
    fun mkSharing (isType, shares, line) = 
        Sharing {
          isType = isType,
          shares = shares,
          line   = line
        };

    fun mkWhereType (sigexp, typeVars, name, types, line) = 
        WhereType {
          sigExp      = sigexp,
		  typeVars    = typeVars,
          typeName    = name,
          realisation = types,
          line        = line
        };
  

    fun mkLocaldec (decs, body, localDec, line) =
      Localdec 
         {
           decs     = decs,
           body     = body,
           localDec = localDec,
           line     = line
         };

	val mkInclude = IncludeSig;
       
  (*  end; structs abstype *)

  (* Pretty printing *)

  fun displayStructs 
        (strs : structs list, 
         depth : int,
         pprint: prettyPrinter
        ) : unit =
  let (* Prints a list of items. *)
    fun displayList ([], separator, depth) dodisplay = ()
    
      | displayList ([v], separator, depth) dodisplay =
         if depth <= 0
         then ppAddString pprint "..."
         else dodisplay (v, depth)
      
      | displayList (v::vs, separator, depth) dodisplay =
         if depth <= 0
         then ppAddString pprint "..."
         else let
           val brk = if separator = "," orelse separator = ";" then 0 else 1
         in
           ppBeginBlock pprint (0, false);
           dodisplay (v, depth);
           ppBreak pprint (brk, 0);
           ppAddString pprint separator;
           ppEndBlock pprint ();
           ppBreak pprint (1, 0);
           displayList (vs, separator, depth - 1) dodisplay
         end (* displayList *) 

    fun display (str, depth) =
    ( if depth <= 0 (* elide further text. *)
      then ppAddString pprint "..."

      else case str of
        StructureDec (structList : structBind list) =>
		let
			fun displayStructBind (
					{name, sigStruct, value, opaque, ...}: structBind, depth) =
		        (
		          ppBeginBlock pprint (3, false);
		          ppAddString pprint name;
		          if isEmptyStruct sigStruct then ()
		          else (* Signature is optional *)
		          (
		            ppAddString pprint (if opaque then " :>" else " :");
		            ppBreak pprint (1, 0);
		            display (sigStruct, depth - 1)
		          );
		          if isEmptyStruct value then ()
		          else (* May be a structure signature *)
		          ( 
		            ppAddString pprint " =";
		            ppBreak pprint (1, 0);
		            display (value, depth - 1)
		          );
		          ppEndBlock pprint ()
		        )
		in
          ppBeginBlock pprint (3, false);
          ppAddString pprint "structure";
          ppBreak pprint (1, 0);
          displayList (structList, "and", depth) displayStructBind;
          ppEndBlock pprint ()
        end

      | StructureIdent {name, ...} =>
          ppAddString pprint name

      | StructDec {alist, ...} =>
        (
          ppBeginBlock pprint (1, true);
          ppAddString pprint "struct";
          ppBreak pprint (1, 0);
          displayList (alist, "", depth) display;
          ppBreak pprint (1, 0);
          ppAddString pprint "end";
          ppEndBlock pprint ()
        )

      | SignatureDec (structList : sigBind list) =>
        let
			fun displaySigBind ({name, sigStruct, ...}: sigBind, depth) =
		        (
		          ppBeginBlock pprint (3, false);
		          ppAddString pprint (name ^ " =");
		          ppBreak pprint (1, 0);
		          display (sigStruct, depth - 1);
		          ppEndBlock pprint ()
		        )
		in 
          ppBeginBlock pprint (3, false);
          ppAddString pprint "signature";
          ppBreak pprint (1, 0);
          displayList (structList, "and", depth) displaySigBind;
          ppEndBlock pprint ()
        end

      | SignatureIdent (name : string) =>
          ppAddString pprint name

      | SigDec (structList : structs list) =>
        ( 
          ppBeginBlock pprint (1, true);
          ppAddString pprint "sig";
          ppBreak pprint (1, 0);
          displayList (structList, "", depth) display;
          ppBreak pprint (1, 0);
          ppAddString pprint "end";
          ppEndBlock pprint ()
        )

      | ValSig {name, typeof, ...} =>
        let
        in
          ppBeginBlock pprint (0, false);
          ppAddString pprint "val";
          ppBreak pprint (1, 1);
          ppAddString pprint (name ^ " :");
          ppBreak pprint (1, 0);
          displayType (typeof, depth - 1, pprint);
          ppEndBlock pprint ()
        end

      | ExSig {name, typeof, ...} =>
        let
        in
          ppBeginBlock pprint (0, false);
          ppAddString pprint "exception";
          ppBreak pprint (1, 1);
          ppAddString pprint (name ^ " :");
          ppBreak pprint (1, 0);
          displayType (typeof, depth - 1, pprint);
          ppEndBlock pprint ()
        end

      | FunctorDec (structList : functorBind list) =>
        let
			fun displayFunctBind (
					{name, arg, sigStruct, body, opaque, ...}: functorBind, depth) =
		        (
		          ppBeginBlock pprint (3, false);
		          ppAddString pprint (name ^ "(");
		          ppBreak pprint (1, 0);
		          ppBeginBlock pprint (3, true);
		          display (arg, depth - 1);
		          ppEndBlock pprint ();
		          ppAddString pprint ")";
		          if not (isEmptyStruct sigStruct)
		          then (* Signature is optional *)
		          ( 
		            ppAddString pprint(if opaque then " :>" else " :");
		            ppBreak pprint (1, 0);
		            display (sigStruct, depth - 1)
		          ) 
		          else ();
		          ppBreak pprint (1, 0);
		          ppAddString pprint "=";
		          ppBreak pprint (1, 0);
		          display (body, depth - 1);
		          ppEndBlock pprint ()
		        )
		in 
          ppBeginBlock pprint (3, false);
          ppAddString pprint "functor";
          ppBreak pprint (1, 0);
          displayList (structList, "and", depth) displayFunctBind;
          ppEndBlock pprint ()
        end

      | FunctorAppl {name, arg, ...} =>
        let
        in
          ppBeginBlock pprint (1, false);
          ppAddString pprint (name ^ "(");
          ppBreak pprint (0, 0);
          display (arg, depth);
          ppBreak pprint (0, 0);
          ppAddString pprint ")";
          ppEndBlock pprint ()
        end

      | FormalArg {name, sigStruct, ...} =>
        let
        in
          ppBeginBlock pprint (1, false);
          if name = "" then ()
          else
          ( 
            ppAddString pprint (name ^ " :");
            ppBreak pprint (1, 2)
          );
          display (sigStruct, depth - 1);
          ppEndBlock pprint ()
        end

      | Sharing { isType, shares, ... } =>
        (
			ppBeginBlock pprint (3, false);
			ppAddString pprint "sharing";
			ppBreak pprint (1, 0);
			if not isType then ()
			else
				(
				ppAddString pprint "type";
				ppBreak pprint (1, 0)
				);
			displayList (shares, "=", depth)
				  	(fn (name, depth) => ppAddString pprint name);
			ppEndBlock pprint ()
		)

      | WhereType { sigExp, typeVars, typeName, realisation, ... } =>
        (
			ppBeginBlock pprint (3, false);
            display (sigExp, depth);
			ppBreak pprint (1, 0);
			ppAddString pprint "where";
			ppBreak pprint (1, 0);
			ppAddString pprint "type";
			ppBreak pprint (1, 0);
			displayTypeVariables (typeVars, depth, pprint);
			ppAddString pprint typeName;
			ppBreak pprint (1, 0);
			ppAddString pprint "=";
			ppBreak pprint (1, 0);
            displayType (realisation, depth - 1, pprint);
			ppEndBlock pprint ()
		)

      | Localdec {decs, body, localDec, ...} =>
        (
          ppBeginBlock pprint (3, false);
          ppAddString pprint (if localDec then "local" else "let");
          ppBreak pprint (1, 0);
          displayList (decs, ";", depth - 1) display;
          ppBreak pprint (1, 0);
          ppAddString pprint "in";
          ppBreak pprint (1, 0);
          displayList (body, ";", depth - 1) display;
          ppBreak pprint (1, 0);
          ppAddString pprint "end";
          ppEndBlock pprint ()
        )

      | IncludeSig (structList : structs list) =>
        ( 
          ppBeginBlock pprint (3, true);
          ppAddString pprint "include";
          ppBreak pprint (1, 0);
          displayList (structList, "", depth - 1) display;
          ppEndBlock pprint ()
        )

      | Singleton {dec, ...} =>
          ptDisplay (dec, depth - 1, pprint)

      | EmptyStruct =>
          ppAddString pprint ("<bad>")
    );
  in
    displayList (strs, "", depth)  display
  end (* displayStructs *);


  (* Puts out an error message and then prints the piece of tree. *)
  fun errorNear (lex, hard, near, lno, message) : unit =
  let
    val printProc = if hard then errorProc else warningProc;
  in
    printProc
      (lex,
       lno,
       fn (pprint: prettyPrinter) =>
         (
           ppBeginBlock pprint (0, false);
           ppAddString pprint message;
           ppBreak pprint (3, 0);
           ppBeginBlock pprint (0, false);
           ppAddString pprint "Found near";
           ppBreak pprint (1, 0);
           displayStructs ([near], ! errorDepth, pprint);
           ppEndBlock pprint ();
           ppEndBlock pprint ()
         ))
  end;

  (* Returns a function which can be passed to typetree.match to
     print a bit of context information. *)
  fun foundNear (sVal : structs, name : string) : prettyPrinter -> unit =
    fn (pprint: prettyPrinter) =>
      ( 
        ppAddString pprint ("While checking (" ^ name ^ ") near");
        ppBreak pprint (1, 2);
        displayStructs ([sVal], ! errorDepth, pprint)
      );

  (* Error message routine for lookupType and lookupStructure. *)
  fun giveError (sVal : structs, lno : int, lex : lexan) : string -> unit =
    fn (message : string) => errorNear (lex, true, sVal, lno, message);

  (* Structures and values in signatures. *)
   
  (* Formal paramater to a functor - either value or exception. *)
  fun mkFormal (name : string, class, typ, addr) =
  	Value{class=class, name=name, typeOf=typ, access=Formal addr}

  (* A null map from a structure identifier to anything. *)
  fun nullMap (id : 'a) : 'b = raise ValueMissing "";

  (* Check that a matching has succeeded, and check the value
     constructors if they are datatypes. Used  for both signature
     matching and also for sharing constraints where there is no
     obvious direction of matching. However the rule for signature
     matching requires the types of the constructors to match 
     whereas the rule for sharing only requires that they have the
     same names (type checking in that case could require second-order
     unification). *)
  fun checkTypeConstrs
         (candid,
          target,
          targTypeMap,
          checkTypes,
          lex         : lexan,
          near,
          lno         : int
          ) : unit=
  let
    val candidName : string = tcName candid;
    val targetName : string = tcName target;
  
    fun checkConstrs ([], []) = ()
    
      | checkConstrs (cList, []) =
          errorNear (lex, true, near, lno, 
            "Too many constructors to match type (" ^  candidName ^ ")")
                    
      | checkConstrs ([], tList) =
          errorNear
            (lex, true, near, lno,
             "Not enough constructors to match type (" ^  candidName ^ ")")
                 
      | checkConstrs (cVal :: cList, tVal :: tList) =
        let
          val cName : string = valName cVal;
          val tName : string = valName tVal;
        in
          (* Check they have the same name. *)
          if cName <> tName
          then errorNear 
                 (lex, true, near, lno, 
                  "Looking for constructor " ^ tName ^ 
                  " but found " ^ cName)
          (* Check their types match. *)
          else let
            (* This works correctly for polytypes
             (e.g. datatype 'a t = ...) because the type variables 
             in the signature will be non-unifiable. checkTypes does
             nothing if we are generating a sharing constraint and then
             the rest of the list. *)
            val cType : types = valTypeOf cVal;
            val tType : types = valTypeOf tVal;
            val U : unit = checkTypes (cType, tType, cName);
          in
            checkConstrs (cList, tList)
          end
        end;
  in
    if tcArity candid <> tcArity target
    then () (* Have already given the error message. *)
    else let
      (* Check the type constructors themselves first. This checks
         that the sharing constraints have been satisfied. *)
      val tvars : types list = tcTypeVars target; (* either will do *)
      val U : unit = 
        matchTypes 
          (mkTypeConstruction (candidName, candid, tvars),
           mkTypeConstruction (targetName, target, tvars),
           targTypeMap,
           lex,
           lno,
           foundNear (near, targetName));
          
      val candidConstrs : values list = tcConstructors candid;
      val targetConstrs : values list = tcConstructors target;
    in 
      (* We have already checked for matching a type in the structure
          to a datatype in the signature. *)
      if isNil targetConstrs orelse isNil candidConstrs
      then ()
      else checkConstrs (candidConstrs, targetConstrs)
    end
  end (* checkTypeConstrs *);

  (* Check that a candidate signature (actually the environment part of
     a structure) matches a target signature. The direction is important
     because a candidate is allowed to have more components and more
     polymorphism than the target.  Along with the candidate and target
     signatures we also pass  maps which are applied to the structure and
     type "names" (unique ids) before the matching process. *)

  type 'a map =
    {
      lookup: typeId -> 'a,
      enter:  typeId * 'a -> unit
    };

  fun matchSigs 
       (candidate     : signatures,
        target        : signatures,
        tArgTypeMap   : typeConstrs map, 
        near,
        lno           : int,
        lex           : lexan
       ) : unit =
  let  
    val lookupType = #lookup tArgTypeMap;
 
    (* Match names (unique ids) for types. This is slightly more
	   complicated than simply assigning the stamps. *)
    fun matchNames (candidate, target) : unit=
        univFold 
          (sigTab target,
          (fn (dName, dVal, ()) =>
            if tagIs typeConstrVar dVal
            then let (* See if there is one with the same name. *)
              val target = tagProject typeConstrVar dVal;
              val candid = univLookup (sigTab candidate, typeConstrVar, dName);
            in (* Match up the types. This does certain checks but
                  does not check sharing. Equality is checked for. *)
              if not (isUnsetId (tcIdentifier target)) (* just in case *)
              then
                ( 
                  (* Check for arity and equality - value constructors 
                     are checked later. If the target is a bound identifier
                     in the range it can be matched by a candidate. *)
                  enterTypeConstrs (target, candid, tArgTypeMap);
                
                  if tcArity target <> tcArity candid
                    then errorNear (lex, true, near, lno,
                       "Types (" ^ tcName target ^ ") have different arities.")
                       
                  else if tcEquality target andalso
                       not (permitsEquality candid)
                    then errorNear (lex, true, near, lno,
                       "(" ^ tcName candid ^ ") is not an eqtype")
                       
                  else if not (isNil (tcConstructors target)) andalso
                        isNil (tcConstructors candid)
                    then errorNear (lex, true, near, lno,
                        "(" ^ tcName candid ^ ") is not a datatype")
                       
                  else () 
                )
              else ()
            end (* scope of "handle" increased SPF 11/8/94 *)
               (* If the lookup failed catch the exception. *)
               handle ValueMissing _ =>
                 errorNear (lex, true, near, lno, 
                     "Type (" ^ dName ^ ") missing in structure.")
             
            else if tagIs structVar dVal
              then let (* and sub-structures. *)
                val target = (tagProject structVar) dVal;
                (* For each target structure: find a candidate with the 
                   same name and recursively check them. *)
                val candid = univLookup (sigTab candidate, structVar, dName);
              in
                matchNames (structSignat candid, structSignat target)
              end (* scope of "handle" increased SPF 11/8/94 *)
                handle ValueMissing _ =>
                  errorNear (lex, true, near, lno, 
                     "Structure (" ^ dName ^ ") missing in structure.")

            else () (* not a type or structure *)
          ), (* end of fn *)
          ()  (* default value for fold *)
        ) (* matchNames *);
      
    val U : unit = matchNames (candidate, target);
       
    (* Match the values and exceptions in the signatures.
       This actually does the checking of types. *)
    fun matchVals (candidate, target) : unit =
    (* Map the identifiers first, returning the originals if they are
         not in the map. *)
    let
      val checkTypesAndStructures : unit =
          univFold 
           (sigTab target,
            (fn (dName, dVal, ()) =>
              if tagIs typeConstrVar dVal
              then let (* For each type in the target ... *)
                val target = tagProject typeConstrVar dVal;
                
                (* Find a candidate with the same name. *)
                val candid = univLookup (sigTab candidate, typeConstrVar, dName);
              in 
                (* Now check that the types match. *)
                checkTypeConstrs 
                  (candid,
                   target,
                   lookupType,
                   fn (c, t, n) =>
                     matchTypes (c, t, lookupType, lex, lno, foundNear (near, n)),
                   lex,
                   near,
                   lno)
              end
                (* If the lookup failed catch the exception, but ignore
                   the error - we've already reported it in matchNames *)
                handle ValueMissing _ => ()
               
              else if tagIs structVar dVal
              then let (* and each sub-structure *)
                val target = tagProject structVar dVal;
                val candid = univLookup (sigTab candidate, structVar, dName);
              in
                (* For each target structure: find a candidate with the same
                   name and recursively check them. *)
                matchVals (structSignat candid, structSignat target)
              end
                (* If the lookup failed catch the exception, but ignore
                   the error - we've already reported it in matchNames *)
                handle ValueMissing _ => ()
  
              else ()
            ), (* fn *)
           ()
          );

      val checkValuesAndExceptions : unit =
        (* Finally the values and exceptions. *)
        univFold 
          (sigTab target,
          (fn (dName, dVal, ()) =>
            if tagIs valueVar dVal
            then let
              val v = tagProject valueVar dVal;
            in 
             (* The constructors in the target are ignored since
                they have already been checked. *)
			  case v of
			  	Value{class=Constructor _, ...} => ()
			  | _ =>
			   let
                (* Look up a corresponding value and check the type. *)
                val candid = univLookup (sigTab candidate, valueVar, dName);
                     
                val vIsEx = case v of Value{class=Exception, ...} => true | _ => false
                and cIsEx = case candid of Value{class=Exception, ...} => true | _ => false
              in
                (* Check that exceptions have matched with exceptions 
                   and values with values, and have not mixed. *)
                if not cIsEx andalso vIsEx
                  then errorNear (lex, true, near, lno, 
                        "Value (" ^ dName ^
                        ") must be an exception to match the signature.")
                       
                (* An exception will match a value provided the types are right. *)
                else if cIsEx andalso not vIsEx
                  then let
                    val exType = valTypeOf candid;
                    val candidType = 
                      if isEmpty exType
                      then exnType
                      else mkFunctionType (exType, exnType)
                  in
                    matchTypes
                      (candidType, valTypeOf v, lookupType,
                       lex, lno, foundNear (near, dName))
                  end
                  
                else
                  matchTypes
                    (valTypeOf candid, valTypeOf v, lookupType,
                     lex, lno, foundNear (near, dName))
              end (* scope of "handle" increased SPF 11/8/94 *)
                handle ValueMissing _ =>
                  errorNear (lex, true, near, lno, 
                      "Value (" ^ dName ^ ") missing in structure.")
            end
            else ()
          ),
          ()
         )
      in
         ()
      end (* matchVals *);
  in 
     matchVals (candidate, target)
  end (* matchSigs *);

  fun typeMatchTab (minOffset : int, size : int) : typeConstrs map =
  let
    (* Make a vector with an entry for each bound name. *)
    val v = ARRAY.array (max 0 size, undefType) ;(* was "max 1 size" *)
  in
    (* Return the entry corresponding to the name, unless it is empty
       when an exception is raised. *)
    { 
      lookup =
        (fn id =>
          ( if not (isBoundId id) orelse
              offsetId id < minOffset orelse
              offsetId id >= size
            (* It is possible for the offset to be >= size if the type
               is being shared inside the result signature of a functor. It
               will only happen for types inside structures which are shared. *)
              then raise ValueMissing "" 
              else (); (* Not present if it isn't bound. *)
             let
               (* Must be a bound stamp. *)
                val entry = ARRAY.sub (v, offsetId id);
                               (* SPF 7/6/94 fixed off-by-one *)
              in
                if isUnsetId (tcIdentifier entry) (* undefType *)
                then raise ValueMissing ""
                else entry
              end
            ) 
         ),
            
      enter =
        (fn (id, value) =>
           if isBoundId id andalso
              offsetId id >= minOffset andalso
              offsetId id < size
           then ARRAY.update (v, offsetId id, value)
           else ()
         )
    }
  end;

  type tsvEnv = { enterType:   string * typeConstrs -> unit,
                  enterStruct: string * structVals  -> unit,
                  enterVal   : string * values      -> unit };
  
  fun tsvEnv (Env E) = {enterType   = #enterType   E,
                        enterStruct = #enterStruct E,
                        enterVal    = #enterVal    E};

  (* Copy the signature so that types in different signatures are distinct. *)
  fun copySig 
        (source       : signatures,
         mustCopyType : typeId -> bool,
         makeTypeId   : unit -> typeId,
         typeMap      : typeConstrs map,
		 strName	  : string)
        : signatures = 
  let
      (* Make a new signature. *)
      val newSig = makeSignatures (sigName source);
      (* Copy everything into the new signature. *)
      val tab = sigTab newSig
      val lastAddr =
              fullCopySig 
                (0, source,
                {
                  enterType   = fn (s,v) => univEnter (tab, typeConstrVar, s, v),
                  enterStruct = fn (s,v) => univEnter (tab, structVar,     s, v),
                  enterVal    = fn (s,v) => univEnter (tab, valueVar,      s, v)
                },
                mustCopyType, makeTypeId, typeMap, strName);
  in
	  makeCopy(sigName source, newSig, sigMinTypes newSig, sigMaxTypes newSig)
  end (* copySig *)

  (* Generate new entries for all the elements of the signature. *)
  and fullCopySig 
        (offset        : int, 
         source        : signatures,
         resEnv        : tsvEnv,
         mustCopyType  : typeId -> bool,
         makeTypeId    : unit -> typeId,
         typeMap       : typeConstrs map,
		 strName	   : string) 
        : int =
  let
    fun copyTypeCons (tcon : typeConstrs) : typeConstrs =
      copyTypeConstr (tcon, mustCopyType, makeTypeId, typeMap,
	  	fn x => x, strName);

    fun copyTyp (t : types) : types =
      copyType (t, fn x => x, (* Don't bother with type variables. *) copyTypeCons);

	(* First copy the type constructors in this signature and any substructures.
	   It's inefficient but harmless to do this again for substructures.
	   TODO: Tidy this up. *)
	val u: unit =
		copyTypeConstructors(source, mustCopyType, makeTypeId, typeMap, strName)
  in
    univFold
     (sigTab source,
      (fn (dName: string, dVal: universal, num) =>
        (if tagIs structVar dVal
         then let
           val oldStruct = tagProject structVar dVal;
           val oldSig     = structSignat oldStruct;
           
           (* Make a new sub-structure. *)
           val newSig = 
             copySig (oldSig, mustCopyType, makeTypeId, typeMap,
					  strName ^ dName ^ ".");
               
           val addr = 
             if isFormal (structAccess oldStruct) 
              then vaFormal (structAccess oldStruct) + offset
              else num (* From   sig ... open Global; ... end *);
              
           val newStruct =
             makeFormalStruct (structName oldStruct, newSig, addr);
         in
           #enterStruct resEnv (dName, newStruct);
           max num (addr+1)
         end (* structures *)
                 
         else if tagIs typeConstrVar dVal
         then let (* Types *)
		  val address = ref num
          (* Make a new constructor. *)
           val oldConstr = tagProject typeConstrVar dVal;
           
           (* 
              The new type constructor will use the NEW polymorphic
              type variables. This is because copyTypeCons uses the
              table built by matchSigs which maps OLD constructors to
              NEW ones, and the NEW constructors contain NEW type variables.
           *)
           val newConstr = copyTypeCons oldConstr;
           
           (* We must copy the datatype if any of the value
              constructors have to be copied. The datatype may
              be rigid but some of the value constructors may
              refer to flexible type names. *)
           val mustCopy = ref (not (identicalConstr (newConstr, oldConstr)));
           
           local
             val oldTypeVars : types list = tcTypeVars oldConstr;
             val newTypeVars : types list = tcTypeVars newConstr;
(* 
   We CAN legitimately get different numbers of type variables here,
   it we're trying to recover from a user error that we've already
   diagnosed. We'll just ignore the extra variables. SPF 26/6/96
*)
             fun zipTypeVars (x::xs) (y::ys) = (x, y) :: zipTypeVars xs ys
               | zipTypeVars _  _   = []
                 
             val typeVarTable : (types * types) list = 
               zipTypeVars oldTypeVars newTypeVars;
             
             fun copyTypeVar (t : types) : types =
             let
               fun search [] = t
                 | search ((oldTypeVar, newTypeVar) :: rest) =
                    if sameTypeVar (t, oldTypeVar) then newTypeVar else search rest
             in
               search typeVarTable
             end;
           in
             (* 
                 Dave was wrong - we DO need to copy the polymorphic type variables -
                  at least, we do here! This version hides the old version of
                  copyTyp, which is in the enclosing environment. The entire
                  type/signature matching code needs a thorough overhaul.
                  SPF 16/4/96
             *)
			 (* TODO: If SPF is right we also need to redefine
			 	copyTypeCons. DCJM 17/2/00.  *)
             fun copyTyp (t : types) : types =
               copyType (t, copyTypeVar, copyTypeCons);
           end;
           
           (* 
              Now copy the value constructors. The equality status
              and any equivalence (i.e. type t = ...) will have been
              processed when the constructor was copied.
              
              What's going on here? Copying the type constructor will
              use the NEW polymorphic variables, but copying the rest of
              the type will use the OLD ones, since copyTyp doesn't copy
              individual type variables - what a MESS! I think this means
              that we end up with OLD variables throughout.
              SPF 15/4/96
           *)
           val copiedConstrs =
             map 
              (fn (v as Value{name, typeOf, class, access}) =>
               let
                 (* Copy its type and make a new constructor if the type
                    has changed. *)
                 val newType = copyTyp typeOf;
                 val typeChanged  = not (identical (newType, typeOf));
				 val (newAccess, addressChanged) =
				 	case access of
						Formal addr =>
						let
							val newAddr = addr+offset
						in
							address := max (newAddr+1) (!address);
							(Formal newAddr, offset <> 0)
						end
					  | access => (access, false)
				 (* If this datatype shares with another one we will already have
				    constructors available.  This can happen, in particular, if
					we have a signature constraining the result of a structure.
					There will be sharing between the datatype in the implementing
					structure and the result signature. *)
                 val copy =
                   if typeChanged orelse addressChanged
                   then let
					 val v' = Value{name=name, typeOf=newType, class=class, access=newAccess}
					 (* See if the constructor already exists. *)
                   in
				     let
					 	val original = findValueConstructor v'
					 in
					 	(* We try to use the original if it is global since that
						   allows us to print values of the datatype.  If it is
						   not global we MUSTN'T use the copy.  It may be local
						   and so may not exist later on. *)
					    case original of
							Value{access=Global _, ...} => original
						|	_ => v'
					 end
                   end
                   else v;
               in
                 if typeChanged orelse addressChanged then mustCopy := true else ();
                 copy (* Return the copy. *)
               end)
              (tcConstructors oldConstr);
          in
            if !mustCopy
            then let
              (* If the copied datatype already has constructors on it
                 we must have two datatypes which share. They need not
                 necessarily have the same constructors e.g. datatype 
                 t = X of int t   can share with datatype t = X of int * int
                 or even with datatype t = X of bool . We have to make a new
                 type constructor in that case. We don't need to put this
                 in the typeMap table because we can always return the
                 type that is already in there. This will also work correctly
                 if we have a type constructor which does not itself need to
                  be copied (e.g. it is rigid) but at least one of whose
                  value constructors involves a flexible type. Another  
                  case could be where we have a structure containing a datatype.
                  The type in the signature may be either a datatype or a type. *)
                  
              val newType =
                if not (isNil (tcConstructors newConstr))
                then (* Matched to a datatype. Use the NEW types throughout *)
                  makeTypeConstrs (* Necessary? *)
                      (tcName newConstr, tcTypeVars newConstr,
                       emptyType,
                       tcIdentifier newConstr, tcEquality newConstr, 0)
(* old (16/4/96) ...
                   makeTypeConstrs 
                      (tcName oldConstr, tcTypeVars oldConstr,
                       makeEquivalent (newConstr, tcTypeVars oldConstr),
                       tcIdentifier newConstr, tcEquality newConstr)
... *)          
                else newConstr;
            in
              (* Put the new constructors on the result type *)
              tcSetConstructors (newType, copiedConstrs);
              (* and put it into the table. *)
              #enterType resEnv (dName, newType)
            end
            else #enterType resEnv (dName, newConstr);
            
            max num (!address)
          end
            
          (* Finally the values and exceptions. *)
          else if tagIs valueVar dVal
            then let
              val v = tagProject valueVar dVal;
            in
			  case v of
			   Value {typeOf=oldType, class, name, access=Formal addr, ...} =>
				    let
	                  val newType = copyTyp oldType;
	                  val newAddr = addr + offset;
	                  
	                  (* Make a new entry if the address or type have changed. *)
	                  val res =
	                    if addr <> newAddr orelse not (identical (newType, oldType))
	                    then mkFormal (name, class, newType, newAddr)
	                    else v;
	                in
	                  #enterVal resEnv (name, res);
	                  max num (newAddr+1)
	                end

			  | Value {typeOf, class, name, access, ...} =>
			  	    (* Values in the result signature of a structure may be globals
					   as a result of a call to extractValsToSig.  This applies
					   if we have a functor which returns a global structure
					   e.g. structure S = ...; functor F() = S.
					   We still have to consider the possibility that the types might
					   be different due to an opaque signature e.g. structure S1 :> SIG = S2. *)
				    let
	                  val newType = copyTyp typeOf;
	                  (* Can save creating a new object if the address and type
					     are the same as they were. *)
	                  val res =
	                    if not (identical (newType, typeOf))
	                    then Value {typeOf=newType, class=class, name=name, access=access}
	                    else v
	                in
	                  #enterVal resEnv (name, res);
					  num
	                end
            end 
          else num
        ) 
      ),
      offset
     )
  end (* fullCopySig *)

  (* Make entries for all the type constructors.  The only reason for
     doing this separately from fullCopySig is to try to ensure that the
	 names we give the types are appropriate.  If we do this as part of
	 fullCopySig we could get the wrong name in cases such as
	 sig structure S: sig type t end structure T : sig val x: S.t end end.
	 If fullCopySig happens to process "x" before "S" it will copy "t"
	 and give it the name "T.t" rather than "S.t". *)
  and copyTypeConstructors(
  		 source: signatures, 
         mustCopyType  : typeId -> bool,
         makeTypeId    : unit -> typeId,
         typeMap       : typeConstrs map,
		 strName	   : string): unit =
  let
    fun copyTypeCons (tcon : typeConstrs) : typeConstrs =
      copyTypeConstr (tcon, mustCopyType, makeTypeId, typeMap,
	  	fn x => x, strName);
  in
    univFold
     (sigTab source,
      (fn (dName: string, dVal: universal, ()) =>
        (if tagIs structVar dVal
         then let
           val oldStruct = tagProject structVar dVal;
           val oldSig     = structSignat oldStruct;
		 in
		   copyTypeConstructors(oldSig, mustCopyType, makeTypeId, typeMap,
					  strName ^ dName ^ ".")
         end (* structures *)
                 
         else if tagIs typeConstrVar dVal
         then let (* Types *)
          (* Make a new constructor.  It will be entered in the match table
		     and picked up when we copy the signature. *)
           val oldConstr = tagProject typeConstrVar dVal;
           val newConstr = copyTypeCons oldConstr
          in
           ()
          end
            
		else ()
        ) 
      ),
      ()
     )
	 end;

  (* This used to be checkExplicitness which, as well as replacing variable
     stamps with bound stamps also checked for type explicitness.  That is
	 no longer required since ML97 does not allow types to be redefined. *)
  fun renameVariableAsBound (signat, initTypeId, errorMessage) =
  let
   (* First set every different variable stamp in types to be
      new bound stamps. We may not start at zero
      if this is the result signature of a functor because there
      may be sharing between the argument and the result. *) 
    val typeCounter = ref initTypeId;
    
    fun makeTypeId () =
    let
      val n = !typeCounter;
    in
      typeCounter := n + 1;
      makeBoundId n
    end
    
    fun setStamps source =
       (* Don't make the signature bound yet, we may have
          several shared structures and if we make any of
          them bound we will make them all bound. Process
          all the types. *)
        univFold 
         (sigTab source,
          (fn (dName, dVal, ()) =>
            if tagIs structVar dVal
              then setStamps (structSignat (tagProject structVar dVal))
              
            else if tagIs typeConstrVar dVal
              then setTypeConstr (tagProject typeConstrVar dVal,
                                  fn tc => makeTypeId ())
            else ()
           ),
          ()
         );
    
  in
    setStamps signat;
    (* Set the size of the type table for the signature we return. *)
    makeCopy (sigName signat, signat, initTypeId, !typeCounter)
  end (* renameVariableAsBound *);

  val makeEnv = fn x => let val Env e = makeEnv x in e end;  

  (* Second pass - identify names with values and type-check *)
  fun pass2Structs (strs : structs list, lex : lexan, Env env : env) : unit =
  let 
    fun pass2Struct 
        (strs     : structs list,
         typeNo   : int ref,
         Env env  : env, 
         lno      : int,
		 strName  : string
         ) : unit =
    let
      (* Get the value from a signature-returning expression
         (either the name of a signature or sig ... end.
         The names (type and structure ids) in the signature
         are bound names. *)
      fun sigVal 
            (str           : structs,
             initTypeId    : int,
             Env globalEnv : env,
             lno           : int,
			 strName	   : string
            ) : signatures =
      let
       (* Process a sharing constraint. *)
        fun applySharingConstraint 
              ({shares = tlist, isType, line}: shareConstraint,
               Env tEnv    : env,
               near        : structs)
              : unit =
        let
          fun shareTypes
                (aType : typeConstrs,
                 bType : typeConstrs,
                 lno   : int
                 ) : unit =
		  (* In ML90 we had to check that two datatypes which shared were
		     "consistent" i.e. had the same constructor names (but not
			 necessarily the same types since that would have required
			 second order unification).  That requirement has been removed
			 in ML97. *)
		  linkTypeConstructors (aType, bType, giveError (near, lno, lex));


(********************* Start of SPF's rewrite (incomplete!) **********************)

		(* The purpose of the following code was to fix some bugs in my
		   original structure sharing code for ML90 and also to simplify it.  In
		   particular it detected cyclic sharing constraints more accurately.
		   These were cases of "sharing A = A.B" which were illegal in ML90
		   but are legal in ML97 (it's a short-hand for sharing type A.t = A.B.t).
		   Much of it is no longer relevant since we are only interested in
		   sharing types in ML97. I've simplified it somewhat but it
		   might be worth simplifying it further. DCJM 27/7/00. *)

        (* useful stuff *)
        (* sets as unordered lists *)
        fun member (eq : 'a * 'a -> bool) x []       = false
          | member (eq : 'a * 'a -> bool) x (h :: t) =
              eq (x, h) orelse member eq x t;
        
        fun addToSet (eq : 'a * 'a -> bool) x l =
          if member eq x l then l else x :: l;
        
        fun union (eq : 'a * 'a -> bool) []       l = l
          | union (eq : 'a * 'a -> bool) (h :: t) l =
              if member eq h l then union eq t l else h :: union eq t l;
              
        fun unionMap (eq : 'b * 'b -> bool) (f : 'a -> 'b list) ([] : 'a list) : 'b list = []
          | unionMap (eq : 'b * 'b -> bool) (f : 'a -> 'b list) (h :: t) =
              union eq (f h) (unionMap eq f t)
      
        type virtStruct = signatures list;
        
        (* Find all the substructure names occurring in a single structure *)
        fun subStructureNames (sigVal : signatures) : string list = 
           univFold
            (sigTab sigVal,
             fn (structName, dVal, names) =>
               if tagIs structVar dVal then structName :: names else names,
             []);
  
        (* Find all the type constructor names occurring in a single structure *)
        fun typeConstrNames (sigVal : signatures) : string list = 
           univFold
            (sigTab sigVal,
             fn (typeName, dVal, names) =>
               if tagIs typeConstrVar dVal then typeName :: names else names,
             []);
      
        (* Find all the substructure names occurring in a virtual structure. *)
        fun virtSubStructureNames sigs : string list =
          unionMap (op =) subStructureNames sigs;
         
        (* Find all the type constructor names occurring in a virtual structure. *)
        fun virtTypeConstrNames sigs : string list =
          unionMap (op =) typeConstrNames sigs;
         
        (* Find the named virtual substructure of a virtual structure. *)
        fun getVirtSubStructure sigs (strName : string) : virtStruct =
        let
           (* 
              Look up the name of the substructure. It may not
              be there because not every substructure occurs
              in every structure of the virtual structure.
           *)
          val substrList : signatures list =
            fold 
              (fn (sigVal : signatures) => fn (res : signatures list) =>
	              let
	                val str = univLookup (sigTab sigVal, structVar, strName)
	              in
	                structSignat str ::res
	              end
                handle ValueMissing _ => res)
             []
             sigs;
        in
          substrList
        end;
        
        (* Find the named typed constructors of a virtual structure. *)
        fun getVirtTypeConstrs sigs (typeName : string) : typeConstrs list =
        let
           fun funForFold (sigVal : signatures) (res : typeConstrs list) : typeConstrs list =
		  	  univLookup (sigTab sigVal, typeConstrVar, typeName) :: res
			  	  handle ValueMissing _ => res
        in
          fold funForFold [] sigs
        end;
                
        (* Find all the substructure names occurring in a list of virtual structures *)
        fun listVirtSubStructureNames (virts : virtStruct list) : string list = 
           unionMap (op =) virtSubStructureNames virts;
        
        (* Find all the type constructor names occurring in a list of virtual structures *)
        fun listVirtTypeConstrNames (virts : virtStruct list) : string list = 
           unionMap (op =) virtTypeConstrNames virts;
      
        (* Find all the named virtual substructures occurring in a list of virtual structures *)
        fun listVirtSubStructures (virts : virtStruct list) (strName : string) : virtStruct list = 
        let
          fun funForFold (vs : virtStruct) (res : virtStruct list) : virtStruct list = 
            getVirtSubStructure vs strName :: res
        in
          fold funForFold [] virts 
        end;
        
        (* Find all the named virtual type constructors occurring in a list of virtual structures *)
        fun listVirtTypeConstrs (virts : virtStruct list) (strName : string) : typeConstrs list = 
        let
          fun funForFold (vs : virtStruct) (res : typeConstrs list) : typeConstrs list = 
            (getVirtTypeConstrs vs strName) @ res
        in
          fold funForFold [] virts 
        end;
        
        fun shareVirtStructs ([], _)      = raise InternalError "Empty sharing list"
          | shareVirtStructs (virts,  _)  = 
         let
           (* Share the types *)
           val typeConstrNames : string list = listVirtTypeConstrNames virts;
           
           fun shareVirtTypeConstr (typeName : string) : unit = 
           let
             (* Find all the type constructors with this name *)
             val tcs : typeConstrs list = listVirtTypeConstrs virts typeName;
             
             fun shareWith (tc : typeConstrs) ([] : typeConstrs list) = ()
               | shareWith tc (h :: t) = 
             let
               val U : unit = shareTypes (tc, h, lno);
             in
               shareWith tc t
             end;
             
             fun shareAll ([] : typeConstrs list) = ()
               | shareAll (h :: t) =
             let 
               val U : unit = shareWith h t
             in
               shareAll t
             end;
           in  
             (* Share them all pair-wise (inefficient!) *)
             shareAll tcs
           end;
           
           val U : unit list = map shareVirtTypeConstr typeConstrNames;
           
           (* Share the substructures *)
           val subStrNames : string list = listVirtSubStructureNames virts;
           
           fun shareVirtSubstruct (strName : string) : unit =
             shareVirtStructs (listVirtSubStructures virts strName, lno);
           
         in
			map shareVirtSubstruct subStrNames;
            ()
         end;
         
        
         fun shareStructures (shareList : signatures list, lno : int) : unit =
           shareVirtStructs (map (fn strVal => [strVal]) shareList, lno);

		(* When looking up the structure and type names we look only
		   in the signature in ML97.  We add this to make it clear that
		   we are only looking up in the signature otherwise we get
		   confusing messages such as "type (int) has not been declared". *)
		 fun lookupFailure msg =
		 	giveError (str, line, lex) (msg ^ " in signature.")

        in
              if isType
              then let (* Type sharing. *)
                fun lookupSharing (name: string) = 
                  lookupTyp
                   ({ 
                      lookupType   = #lookupType   tEnv,
                      lookupStruct = #lookupStruct tEnv
                    },
                    name,
                    lookupFailure)
                      
                val first  = lookupSharing (hd tlist);
              in
                if not (isUnsetId (tcIdentifier first))
                then
                  forall
                    (fn typ => shareTypes (lookupSharing typ, first, line))
                    (tl tlist)
                 else ()
              end

              else let (* structure sharing. *)
                fun getStructSignat (name: string) : signatures =
                let
                  val subStr : structVals =
				    lookupStructureDirectly 
				      ("Structure" ,
				       {lookupStruct = #lookupStruct tEnv}, 
				       name,
				       lookupFailure);
					in
					  structSignat subStr
					end
              in  (* Now share all these signatures. *)
                shareStructures (map getStructSignat tlist, line)
              end
        end (* applySharingConstraint *);

(**************************** End of SPF's rewrite *************************)

        (* Look up a signature. Strictly a signature cannot be contained
           in a structure but this allows a structure to be used as a
           general name space. *)
        fun lookSig (name : string, lno : int) : signatures =
        let
          val errorFn    = giveError (str, lno, lex);
          fun lookupFn s = #lookupSig (makeEnv (structSignat s));
        in
          lookupAny 
            (name,
             #lookupSig globalEnv,
             #lookupStruct globalEnv, 
             lookupFn,
             "Signature",
             undefinedSignature,
             errorFn)
        end

		(* Construct a signature. *)
        fun sigValue (str : structs, Env env : env, lno : int, strName: string) =
		let
			(* Make a new signature. *)
			val sigName =
				case str of
					SignatureIdent name => name
				|	_ => ""
			val newSig = makeSignatures sigName;
			(* Copy everything into the new signature. *)
            val structEnv = makeEnv newSig;

			(* ML 97 does not allow multiple declarations in a signature. *)
			fun checkAndEnter enter lookup kind (s: string, v) =
				if !ml90 then enter(s, v)
				else
					(
					let
					val discard = lookup s
					in
					errorNear (lex, true, str, lno, 
                         kind ^ " (" ^ s ^ ") is already present in this signature.")
					end
					handle ValueMissing _ => enter(s, v)
					)

            val checkedStructEnv = 
             {
              lookupVal     = #lookupVal    structEnv,
              lookupType    = #lookupType   structEnv,
              lookupFix     = #lookupFix    structEnv,
              lookupStruct  = #lookupStruct structEnv,
              lookupSig     = #lookupSig    structEnv,
              lookupFunct   = #lookupFunct  structEnv,
              lookupTvars   = #lookupTvars  structEnv,
              enterVal      =
			  	checkAndEnter (#enterVal structEnv) (#lookupVal structEnv) "Value",
              enterType     =
			  	checkAndEnter (#enterType structEnv) (#lookupType structEnv) "Type",
              enterStruct   =
			  	checkAndEnter (#enterStruct structEnv) (#lookupStruct structEnv) "Structure",
			  (* These next three can't occur. *)
              enterFix      =
			  	checkAndEnter (#enterFix structEnv) (#lookupFix structEnv) "Fixity",
              enterSig      =
			  	checkAndEnter (#enterSig structEnv) (#lookupSig structEnv) "Signature",
              enterFunct    =
			  	checkAndEnter (#enterFunct structEnv) (#lookupFunct structEnv) "Functor"
             }
		in
			makeSigInto(str, Env checkedStructEnv, Env env, lno, strName, 0);
			(* Make a copy to freeze it as immutable.*)
			makeCopy(sigName, newSig, sigMinTypes newSig, sigMaxTypes newSig)
		end

		(* Constructs a signature and inserts it into an environment at a given offset.
		   Generally offset will be zero except if we are including a signature. *)
		and makeSigInto(str: structs,
						Env structEnv, (* The immediately enclosing sig. *)
						Env globalEnv, (* The surrounding environment excluding this sig. *)
						lno: int, strName: string,
					    offset: int): int =
          (* Either a named signature or sig ... end or one of
		     these with possibly multiple where type realisations. *)
          case str of
            SignatureIdent name =>
            let
              (* Look up the signature and copy it to get new instances
                 of variables. *)
              val sourceSig = lookSig (name, lno);
                
              val typeMap : typeConstrs map =
                typeMatchTab (sigMinTypes sourceSig, sigMaxTypes sourceSig);
            in
				(* Copy the signature into the result. *)
				fullCopySig(offset, sourceSig, tsvEnv (Env structEnv), 
                        isBoundId, makeVariableId, typeMap, strName)
            end
  
          | SigDec (sigsList : structs list) =>  (* sig .... end *)
          let
            (* Process the entries in the signature and allocate an address
               to each. *)
            fun processSig (signat, offset : int, lno : int) : int =
              case signat of
                StructureDec (structList : structBind list) =>
                let
                  (* Each element in the list should be a structure binding. *)
                  fun pStruct [] offset = offset
                    | pStruct (({name, sigStruct, line, ...}: structBind) :: t) offset =
                    let
					  (* Create a new surrounding environment to include the surrounding
					     structure.  This is the scope for any structures or types.
						 Specifically, if we look up a type defined by a "where type"
						 we use this environment and not the signature we're creating. *)
		              val newEnv = 
			             {
			              lookupVal     = #lookupVal    structEnv,
			              lookupType    =
			                lookupDefault (#lookupType structEnv) (#lookupType globalEnv),
			              lookupFix     = #lookupFix    structEnv,
			              lookupStruct  =
			                lookupDefault (#lookupStruct structEnv) (#lookupStruct globalEnv),
			              lookupSig     = #lookupSig    structEnv,
			              lookupFunct   = #lookupFunct  structEnv,
			              lookupTvars   = #lookupTvars  structEnv,
			              enterVal      = #enterVal structEnv,
			              enterType     = #enterType structEnv,
			              enterStruct   = #enterStruct structEnv,
			              enterFix      = #enterFix structEnv,
			              enterSig      = #enterSig structEnv,
			              enterFunct    = #enterFunct structEnv
			             };
                      val resSig =
					  	sigValue (sigStruct, Env newEnv, line, strName ^ name ^ ".");
                      (* Process the rest of the list before declaring
                         the structure. *)
                      val result = pStruct t (offset + 1);
                      (* Make a structure. *)
                      val resStruct = makeFormalStruct (name, resSig, offset);
                      val U : unit = #enterStruct structEnv (name, resStruct);
                    in
                      result (* One slot for each structure. *)
                    end
                in
                  pStruct structList offset
                end
                
              | ValSig {name, typeof, line} =>
                let
                  val errorFn = giveError (signat, line, lex);
                
                  fun lookup s =
                    lookupTyp
                      ({
                        lookupType   =
		                	lookupDefault (#lookupType structEnv) (#lookupType globalEnv),
                        lookupStruct =
							lookupDefault (#lookupStruct structEnv) (#lookupStruct globalEnv)
                       },
                     s,
                     errorFn);
                in  (* If the type is not found give an error. *)
				  (* Check for rebinding of built-ins.  "it" is allowed here. *)
			  	  if !ml90 then ()
			      else if name = "true" orelse name = "false" orelse name = "nil"
				  orelse name = "::" orelse name = "ref"
				  then errorFn("Specifying \"" ^ name ^ "\" is illegal.")
				  else ();
                  assignTypes (typeof, lookup, lex, line);
				  (* The type is copied before being entered in the environment.
				     This isn't logically necessary but has the effect of removing
					 ref we put in for type constructions. *)
                  #enterVal structEnv (name, mkFormal (name, SimpleValue,
				        copyType (typeof, fn x => x, fn x => x), offset));
                  (offset + 1)
                end
               
              | ExSig {name, typeof, line} =>
                let
                  val errorFn = giveError (signat, line, lex);
                
                  fun lookup s =
                    lookupTyp
                      ({
                        lookupType   =
		                	lookupDefault (#lookupType structEnv) (#lookupType globalEnv),
                        lookupStruct =
							lookupDefault (#lookupStruct structEnv) (#lookupStruct globalEnv)
                       },
                     s,
                     errorFn);
                in  (* If the type is not found give an error. *)
				  (* Check for rebinding of built-ins. "it" is not allowed. *)
			  	  if !ml90 then ()
			      else if name = "true" orelse name = "false" orelse name = "nil"
				  orelse name = "::" orelse name = "ref" orelse name = "it"
				  then errorFn("Specifying \"" ^ name ^ "\" is illegal.")
				  else ();
                  assignTypes (typeof, lookup, lex, line);
                  #enterVal structEnv (name, mkFormal (name, Exception, typeof, offset));
                  (offset + 1)
                end
               
              | IncludeSig (structList : structs list) =>
              let
                (* include sigid ... sigid or include sigexp.  For
				   simplicity we handle the slightly more general case
				   of a list of signature expressions.
				  The contents of the signature are added to the environment. *)
                fun includeSigExp (str: structs, offset) =
					makeSigInto(str, Env structEnv, Env globalEnv, lno, strName, offset)
              in
                List.foldl includeSigExp offset structList
              end

              | Sharing (share : shareConstraint) =>
              (* Sharing constraint. *)
			  let
			     (* In ML90 it was possible to share with any identifier
				    in scope.  In ML97 sharing is restricted to identifiers
					in the "spec". *)
			  	 val envForSharing =
				 	if !ml90
					then Env
			             {
			              lookupVal     = #lookupVal    structEnv,
			              lookupType    =
			                lookupDefault (#lookupType structEnv) (#lookupType globalEnv),
			              lookupFix     = #lookupFix    structEnv,
			              lookupStruct  =
			                lookupDefault (#lookupStruct structEnv) (#lookupStruct globalEnv),
			              lookupSig     = #lookupSig    structEnv,
			              lookupFunct   = #lookupFunct  structEnv,
			              lookupTvars   = #lookupTvars  structEnv,
			              enterVal      = #enterVal structEnv,
			              enterType     = #enterType structEnv,
			              enterStruct   = #enterStruct structEnv,
			              enterFix      = #enterFix structEnv,
			              enterSig      = #enterSig structEnv,
			              enterFunct    = #enterFunct structEnv
			             }
					else Env structEnv
			  in
                 applySharingConstraint (share, envForSharing, str);
                 offset (* No entry *)
              end
                
              | Singleton {dec, line, ...} =>
              let (* datatype or type binding(s) *)
                (* This pass puts the data constructors into the environment. *)
				val addrs = ref offset
				(* Pass2 creates value constructors of datatypes as global values.
				   Rather than complicate pass2 by trying to make formal values
				   in this case it's easier to trap the value constructors at
				   this point. N.B. We may get constructors from a datatype
				   declaration or from datatype replication. *)
				fun enterVal(name, Value{class=class, typeOf, ...}) =
					let
						val addr = !addrs
						val _ = addrs := addr+1
					in
						(#enterVal structEnv)(name,
							Value{class=class, typeOf=typeOf, access=Formal addr, name=name})
					end

				(* Record all the datatypes we declare. *)
				val datatypeList = ref []
				fun enterType(name, tyCons) =
					(
					if isEmpty (tcEquivalent tyCons)
					then datatypeList := tyCons :: !datatypeList else ();
					#enterType structEnv (name, tyCons)
					)

	           val newEnv = 
	             {
	              lookupVal     = #lookupVal    structEnv,
	              lookupType    =
	                lookupDefault (#lookupType structEnv) (#lookupType globalEnv),
	              lookupFix     = #lookupFix    structEnv,
	              lookupStruct  =
	                lookupDefault (#lookupStruct structEnv) (#lookupStruct globalEnv),
	              lookupSig     = #lookupSig    structEnv,
	              lookupFunct   = #lookupFunct  structEnv,
	              lookupTvars   = #lookupTvars  structEnv,
	              enterVal      = enterVal,
	              enterType     = enterType,
	              enterStruct   = #enterStruct structEnv,
	              enterFix      = #enterFix structEnv,
	              enterSig      = #enterSig structEnv,
	              enterFunct    = #enterFunct structEnv
	             };

                val t : types =
                  pass2 (dec, makeVariableId, Env newEnv, lex, line, strName);
				(* Replace the constructor list for the datatype with the modified
				   constructors.  All the constructors should be in the set.  Is
				   it possible that one might not be because of an error? *)
				fun findConstr(v: values): values =
					(#lookupVal structEnv)(valName v) handle ValueMissing _ => v
				fun updateConstrList tyCons =
					tcSetConstructors(tyCons, List.map findConstr (tcConstructors tyCons))
				val _ = List.app updateConstrList (!datatypeList)
              in
                ! addrs
              end
              
              | _ =>
                 raise InternalError "processSig: not a signature"
            (* end processSig *);
          in
              List.foldl
                (fn (signat, offset) => 
                   processSig (signat, offset, lno))
                offset sigsList
          end

		  | WhereType { sigExp, typeVars, typeName, realisation, line } =>
		  let
			  (* We construct the signature into the result signature.  When we apply the
			     "where" we need to look up the types (and structures) only within the
				 signature constrained by the "where" and not in the surrounding signature.
				 e.g. If we have sig type t include S where type t = ... end
				 we need to generate an error if S does not include t.  Of course
				 if it does that's also an error since t would be rebound!
				 Equally, we must look up the right hand side of a where type
				 in the surrounding scope, which will consist of the global environment
				 and the signature excluding the entries we're adding here. *)
			  val findTypes = searchList() and findStructs = searchList()
			  val newEnv =
				{
	                lookupVal     = #lookupVal    structEnv,
	                lookupType    =
						lookupDefault (#lookup findTypes)
							(lookupDefault (#lookupType structEnv) (#lookupType globalEnv)),
	                lookupFix     = #lookupFix    structEnv,
	                lookupStruct  =
						lookupDefault (#lookup findStructs)
							(lookupDefault (#lookupStruct structEnv) (#lookupStruct globalEnv)),
	                lookupSig     = #lookupSig    structEnv,
	                lookupFunct   = #lookupFunct  structEnv,
	                lookupTvars   = #lookupTvars  structEnv,
	                enterVal      = #enterVal structEnv,
		            enterType     = #enter findTypes,
		            enterFix      = #enterFix structEnv,
		            enterStruct   = #enter findStructs,
		            enterSig      = #enterSig structEnv,
		            enterFunct    = #enterFunct structEnv
				}

 			  val resAddr = makeSigInto(sigExp, Env newEnv, Env globalEnv, lno, strName, offset)

			  fun lookupFailure msg =
			 	giveError (str, line, lex) (msg ^ " in signature.")

			  (* Look up the type constructor in the signature. *)
              val typeConstr =
                    lookupTyp
                      ({
                        lookupType   = #lookup findTypes,
                        lookupStruct = #lookup findStructs
                       },
                     typeName,
                     lookupFailure);
			  (* The type, though, is looked up in the surrounding environment. *)
			  fun lookupGlobal s =
                    lookupTyp
                      ({
                        lookupType   =
							lookupDefault (#lookupType structEnv) (#lookupType globalEnv),
                        lookupStruct =
							lookupDefault (#lookupStruct structEnv) (#lookupStruct globalEnv)
                       },
                     s,
                     giveError (str, line, lex))

		  	  (* Process the type, looking up any type constructors. *)
			  val U: unit = assignTypes (realisation, lookupGlobal, lex, line);
			  (* Now build a dummy type constructor whose equivalent is
			     the type on the right hand side. *)
			  val dummyTypeCons =
				  makeTypeConstrs (typeName, typeVars, realisation,
				  				   makeVariableId(), false, 0);
		  in
			  (* Now match these up.  This is very similar to an ML90
			     sharing constraint where one of the types was rigid. *)
			  setWhereType(typeConstr, dummyTypeCons, giveError (str, line, lex));
			  (* Finally we can safely add the new declarations to the surrounding scope. *)
			  #apply findTypes (#enterType structEnv);
			  #apply findStructs (#enterStruct structEnv);
		      resAddr
		  end

          | _ =>
            raise InternalError "makeSigInto: not a SigIdent nor a SigDec"; (* end makeSigInto *)
      in
        case str of 
          SignatureIdent (name : string) =>
          (* We can speed things up because the stamps are already bound.
             N.B. When processing the result signature of a functor we 
             explicitly check for this case. *)
          lookSig (name, lno)
        
        | _ =>
          let (* Anything else has to be copied. *)
           val result = sigValue (str, Env globalEnv, lno, strName);
        
		  (* We used to have code in here to generate equality types for
		     all the datatypes in the signature and also to check that the
			 signature was well formed (that meant that if we had shared
			 a structure in the signature with a global structure then every
			 type and substructure in the shared structure had a counterpart
			 in the global structure).  That has all changed in ML97. *)
         in
           (* Now enumerate the bound names. *)
           renameVariableAsBound(result, initTypeId, giveError (str, lno, lex)) 
         end (* not (isSignatureIdent str) *)
      end (* sigVal *);
                  
      (* Process structure-returning expressions i.e. structure names,
         struct..end values and functor applications.  typeNo is the
         number of the next bound identifier. If < 0 make a free identifier. *)
      fun structValue str typeNo (Env env) lno strName =
      let (* Look up a structure name. *)
        fun lookStr name kind = 
          lookupStructure (kind,{lookupStruct = #lookupStruct env}, 
                           name, giveError (str, lno, lex));

        (* these were anonymous fn; moved here for convenience SPF 11/8/94 *)
        fun newTypeId () =
        let
           val n = !typeNo;
         in
           if n < 0 then makeFreeId () else (typeNo := n + 1; makeBoundId n)
         end;
         
      in
        case str of
          StructureIdent {name, valRef} =>
          let (* Look up the name and save the value. *)
            val result = lookStr name "Structure";
          in
            if isUndefinedStruct result
            then undefinedSignature 
            else (valRef := result; structSignat result ) 
          end
                
        | FunctorAppl {name, arg, valRef} =>
          (* The result structure must be copied to generate a new
             environment. This will make new types so that different
             applications of the functor yield different types. There may be 
             dependencies between the parameters and result signatures so
             copying may have to take that into account. *)
          let 
            (* Look up the name, and copy the structure. Strictly functors
               cannot be contained in structures but this allows structures
               to be used as general name spaces. *)
            val functr =
              lookupAny 
                (name,
                 #lookupFunct env, 
                 #lookupStruct env,
                (fn s => #lookupFunct (makeEnv (structSignat s))),
                "Functor",
                undefinedFunctor,
                giveError (str, lno, lex));
          in
            if isUndefinedFunctor functr
            then undefinedSignature
            else let
              val U : unit = valRef := functr; (* save it till later. *)
              
              val resultSig : signatures = functorResult functr;
              val argStruct : structVals = functorArg functr;
                  
              (* We must ensure that types are correctly shared
                 between the arguments and the result. This table keeps track
                 of which actual parameters have been matched to the formals
                 and are then used in the generation of new stamps for types
                 declared in the functor. *)
              val typeMap : typeConstrs map =
                typeMatchTab (sigMinTypes  resultSig, sigMaxTypes   resultSig);
               
              (* Get the actual parameter value. *)
              val argSig = structValue arg typeNo (Env env) lno "";

              (* Check that the actual arguments match formal arguments,
                 and instantiate the variables. *)
              val U : unit = 
                matchSigs (argSig, structSignat argStruct,
                           typeMap, str, lno, lex);
            in
              (* Finally copy the result signature, incorporating the actual
                 args, and creating new stamps for any generative stamps
                 (i.e. stamps that were not put in the table before. *)
              copySig (resultSig, isBoundId, newTypeId,
                       typeMap, strName)
            end
          end
                           
        | StructDec {alist, value = signat} =>
          let
            (* Collection of declarations packaged into a structure
                or a collection of signatures. *)
            (* Some of the environment, the types and the value constructors,
               is generated during the first pass. Get the environment from
               the structure. *)
            val structEnv = makeEnv signat;
            
            (* Make a result signature. This will have the value and structure
               entries as formals. *)
            val resultSig = makeSignatures (sigName signat);
            val resultEnv = makeEnv resultSig;
            val addrs = ref 0;
            
            (* This environment receives the declarations from calling pass2. *)
            val newEnv = 
              {
                enterType =
                  fn (pair as (name,v)) =>
                    (
                     #enterType structEnv pair;
                     #enterType resultEnv pair
                    ),
                    
               (* Should never occur. *)
                enterSig = 
                  fn (pair as (name,v)) =>
                    (
                     #enterSig  structEnv pair;
                     #enterSig  resultEnv pair
                    ),
                    
               (* Never occurs in normal ML, might happen if we opened a
                  structure that was actually a name space. *)
               enterFunct =
                  fn (pair as (name,v)) =>
                   (
                     #enterFunct structEnv pair;
                     #enterFunct resultEnv pair
                   ),
                   
               (* Enter the value in the environment of the struct..end,
                  but turn local declarations  or selections from other
                  structures into entries for the result vector. *)
              enterVal =
                  fn (pair as (name,v)) =>
                  (
                    #enterVal structEnv pair;

                    case v of
						Value{access=Overloaded _, ...} =>
							#enterVal resultEnv pair (* Just copy. *)
					|	Value{class, typeOf, ...} =>
	                    let (* Local or selected. *)
	                      val isVal = not (isConstructor v);
	                        
	                      val a = !addrs;
	                    in
	                      #enterVal resultEnv (name, mkFormal (name, class, typeOf, a));
	                      addrs := a + 1
	                    end
                  ),
                  
              (* Make entries in the result vector. *)
              enterStruct =
                fn (pair as (name, str)) => 
                let
                  val U = #enterStruct structEnv pair;
                  val resSig = structSignat str;
                  val a = !addrs;
                in
                  #enterStruct resultEnv (name, makeFormalStruct (name, resSig, a));
                  addrs := a + 1
                end,
                
              lookupVal =
                lookupDefault (#lookupVal    structEnv) (#lookupVal    env),
                
              lookupType =
                lookupDefault (#lookupType   structEnv) (#lookupType   env),
                
              lookupStruct =
                lookupDefault (#lookupStruct structEnv) (#lookupStruct env),
                 
              lookupSig    = #lookupSig   env, (* Global *)
              
              lookupFunct  = #lookupFunct env, (* Global *)
              
              lookupFix    = #lookupFix   env,

			  (* Fixity declarations are dealt with in the parsing process.  They
			     are only processed again in this pass in order to get declarations
				 in the right order. *)
              enterFix     = fn _ => (),
              
              lookupTvars  = #lookupTvars env
            };
            
            (* process body of structure *)
            val U : unit =
              pass2Struct (alist, typeNo, Env newEnv, lno, strName);
          in
            resultSig
          end
                
        | Localdec {decs, body = [localStr], line, ...} =>
          let (* let strdec in strexp end *)
            val newEnv = makeEnv (makeSignatures "");
                   
            (* The environment for the local declarations. *)
            val localEnv =
             { 
              lookupVal     =
                lookupDefault (#lookupVal    newEnv) (#lookupVal    env),
              lookupType    =
                lookupDefault (#lookupType   newEnv) (#lookupType   env),
              lookupFix     = #lookupFix    newEnv,
              lookupStruct  =
                lookupDefault (#lookupStruct newEnv) (#lookupStruct env),
              lookupSig     = #lookupSig    env,
              lookupFunct   = #lookupFunct  env,  (* Sigs and functs are global *)
              lookupTvars   = #lookupTvars  newEnv,
              enterVal      = #enterVal     newEnv,
              enterType     = #enterType    newEnv,
			  (* Fixity declarations are dealt with in the parsing process.  At
			     this stage we simply need to make sure that local declarations
				 aren't entered into the global environment. *)
              enterFix      = fn _ => (),
              enterStruct   = #enterStruct  newEnv,
              enterSig      = #enterSig     newEnv,
              enterFunct    = #enterFunct   newEnv
             };
             
            (* Process the local declarations. *)
            val U : unit =
              pass2Struct (decs, typeNo, Env localEnv, line, strName);
                   
          in
            (* There should just be one entry in the "body" list. *)
            structValue localStr typeNo (Env localEnv) line strName
          end
                
        | _ =>
           raise InternalError "structValue: not a value"
      end (* structValue *);
            
      (* compare with newTypeId above!!! *)    
      fun makeTypeId () =
      let
        val n = !typeNo;
      in
        if n < 0 then makeFreeId () else (typeNo := n + 1; makeBoundId n)
      end;
      
      fun pass2StructureDec (str : structs, structList : structBind list) : unit =
      let (* Declaration of structures. *)
        (* The declarations must be made in parallel. i.e.
            structure A = struct ... end and B = A; binds B to the A
            in the PREVIOUS environment, not the A being declared. *)
        val sEnv =  (* The new names. *)
          noDuplicates 
            (fn name => 
              errorNear (lex, true, str, lno, 
                         "Structure " ^ name ^ 
                         " has already been bound in this declaration")
            );
         (* Put the new names into this environment. *)
         
        fun pass2StructureBind ({name, sigStruct, value, valRef, line, opaque}) : unit=
          let (* Each element in the list is a structure binding. *)
            val resSig =
				structValue value typeNo (Env env) line (strName ^ name ^".");
              
            val resultSig = 
              if isEmptyStruct sigStruct
              then resSig (* No signature to match. *)
              else let
               (* Get the explicit signature. If we are inside a functor
                   we have to make any new bound names outside the range
                   we have already used. If !typeNo is
                   less than 0 this means we are not in a functor so we 
                    actually start at 0. *)
                val explicitSig  : signatures = 
                  sigVal 
                    (sigStruct, 
                     max (!typeNo) 0,
                     Env env,
                     line,
					 strName ^ name ^ ".");
                         
                (* Make tables to contain the matched ids. *)
                val typeMap  : typeConstrs map =
                  typeMatchTab (sigMinTypes explicitSig, sigMaxTypes explicitSig);
                 
                fun mustCopyType (s : typeId) : bool = 
                  isBoundId s andalso offsetId s >= sigMinTypes explicitSig;
                  
				(* These maps are used to construct the resultant signature.
				   If we are using transparent matching we use the map constructed
				   from matching the signature to the structure but if we are
				   using opaque matching we need to make new instances of
				   every type. *)
				val resTypeMap = 
					if opaque
					then typeMatchTab (sigMinTypes explicitSig, sigMaxTypes explicitSig)
					else typeMap;
              in (* Match the signature. *)
                matchSigs (resSig, explicitSig, typeMap, str, line, lex);
                (* And copy it to put in the names from the structure. *)
                copySig (explicitSig, mustCopyType, makeTypeId,
                         resTypeMap, strName ^ name ^ ".")
              end;
               
            (* Now make a local structure variable using this signature. *)
            val var = makeLocalStruct (name, resultSig);
            
          in
            #enter sEnv (name, var);
            valRef := var
          end
         
      in 
        forall pass2StructureBind structList;
        (* Put them into the enclosing env. *)
        #apply sEnv (#enterStruct env)
      end; (* pass2StructureDec *)
      
      fun pass2FunctorDec (s: structs, structList : functorBind list) : unit =
      let
        (* There is a restriction that the same name may not be bound twice.
		   As with other bindings functor bindings happen in parallel.
		   DCJM 6/1/00. *)
        val sEnv =  (* The new names. *)
          noDuplicates 
            (fn name => 
              errorNear (lex, true, s, lno, 
                         "Functor " ^ name ^ 
                         " has already been bound in this declaration")
            );

         (* Put the new names into this environment. *)
        fun pass2FunctorBind ({name, arg = FormalArg arg, body, sigStruct,
							   valRef, line, opaque}) =
          let
            (* We must copy the signatures to ensure that arguments with
               the same signature are different. Make an environment to
               contain the arguments. *)
            val argEnv = makeEnv (makeSignatures "");
            
            val {name = argName, sigStruct = argSig, valRef = argVal} = arg;
            
            (* If it is a "spec" it must be wrapped up in sig...end.
               We can't have a functor declaration in another functor
               (at least in the current definition of ML so we can 
               start this at 0, rather than !typeNo). *)
            val signat : signatures =
              let
			  	val spec =
					case argSig of
						SignatureIdent _ => argSig
					|	SigDec _ => argSig
					|	WhereType _ => argSig
					|	_ => mkSig [argSig]
              in
                sigVal (spec, 0, Env env, line, "")
              end;
              
            val resArg = makeLocalStruct (argName, signat);
          in (* Put the copied version in. *)
            if argName <> ""
            then #enterStruct argEnv (argName, resArg)
            else
              (* Open the dummy argument. Similar to "open" in treestruct. *)
              univFold
               (sigTab signat,
                (fn (dName, dVal, ()) =>
                   if tagIs typeConstrVar dVal
                     then
                       #enterType argEnv
                         (dName, tagProject typeConstrVar dVal)
                       
                   else if tagIs valueVar dVal
                     then
                       #enterVal argEnv 
                         (dName, mkSelectedVar (tagProject valueVar dVal, resArg))
                          
                   else if tagIs structVar dVal
                     then
                       #enterStruct argEnv 
                         (dName, makeSelectedStruct (tagProject structVar dVal, resArg))
                         
                   else ()
                 ),
                ()
               );
             
            argVal := resArg;
             
            (* Now process the body of the functor using the environment of
               the arguments to the functor and the global environment. *)
            let
              val envWithArgs = 
               {
                lookupVal     =
                  lookupDefault (#lookupVal    argEnv) (#lookupVal    env),
                lookupType    =
                  lookupDefault (#lookupType   argEnv) (#lookupType   env),
                lookupFix     = #lookupFix    env,
                lookupStruct  =
                  lookupDefault (#lookupStruct argEnv) (#lookupStruct env),
                lookupSig     = #lookupSig    env,
                lookupFunct   = #lookupFunct  env,
                lookupTvars   = #lookupTvars  env,
                enterVal      = #enterVal     env,
                enterType     = #enterType    env,
                enterFix      = fn _ => (),
                enterStruct   = #enterStruct  env,
                enterSig      = #enterSig     env,
                enterFunct    = #enterFunct   env
               };
               
              (* In sigVal we will have allocated a range of bound stamps
                 for the argument signature. We need to extend the range
                 for stamps in the body. *)
              val typeStamps = ref (sigMaxTypes   signat);
              
              fun newTypeId () =
              let
                val n = !typeStamps;
               in
                 typeStamps := n + 1;
                 makeBoundId n
               end;
               
             val resSig =
                structValue body typeStamps
                  (Env envWithArgs) line (strName ^ name ^ "().");
                     
              val resultSig =
                if isEmptyStruct sigStruct
                then resSig (* No signature to match. *)
                else let (* Get the explicit result signature. *)
                  val startTypes = sigMaxTypes   signat;
                  
                  val explicitSig : signatures =
                    sigVal 
                      (sigStruct,
                       startTypes,
                       Env envWithArgs,
                       line, strName ^ name ^ "().");
                      
                  (* Make tables to contain the matched names. If we have
                     a named signature we  won't have copied it so the names
                     will start from zero, but since we can't have any
                     sharing with the argument there isn't a problem.
                     Otherwise we may have some sharing with the argument
                     and we have to keep the names distinct. *)
                  val fromZero = isSignatureIdent sigStruct;
                  
                  val typeMap  : typeConstrs map =
                    typeMatchTab
                      (sigMinTypes   explicitSig, 
                       sigMaxTypes   explicitSig);

                  fun mustCopyType s =   
                    isBoundId s andalso offsetId s >= sigMinTypes explicitSig;
                     
				(* These maps are used to construct the resultant signature.
				   If we are using transparent matching we use the map constructed
				   from matching the signature to the structure but if we are
				   using opaque matching we need to make new instances of
				   every type. *)
				val resTypeMap = 
					if opaque
					then typeMatchTab (sigMinTypes explicitSig, sigMaxTypes explicitSig)
					else typeMap

                in 
                  (* Match the signature. *)
                  matchSigs (resSig, explicitSig, typeMap, s, line, lex);
                  (* And copy it to put in the names from the args and
                     generative names. All the names will normally be in
                     the table already so copySig will not make any new 
                     ones, merely link up the values to their new types.
                     We have to be careful if we have names used in the
                     explicit result signature which have come from the 
                     argument, particularly if there is no type constructor
                     in the result with that name. e.g.
                         functor F(type t end): sig val x: t end = ...
                     To handle that we only copy bound names actually in
                     the explicit signature. New names are generated in
                     exceptional circumstances, mainly if there has been
                     an error in matching. *)
				  (* The above comment was true when we only had transparent
				     matching.  In ML 97, with opaque matching, we
					 generate new names for all types which are not
					 constrained by sharing constraints. *)
                  copySig (explicitSig, mustCopyType, newTypeId,
				  		   resTypeMap, strName ^ name ^ "().")
                end;
                
             (* Now make a local functor variable and put it in the
                name space. Because functors can only be declared at
                the top level the only way it can be used is if we have 
                functor F(..) = ... functor G() = ..F.. with no semicolon
                between them. They will then be taken as a single
                declaration and F will be picked up as a local. *)
              (* Set the size of the type map. *)
              val sig' =
                makeCopy (sigName resultSig, resultSig, 0, !typeStamps);
              
              val var = makeFunctor (name, resArg, sig', makeLocal ());
              
            in
              #enter sEnv (name, var);
              valRef := var
            end
          end
        | pass2FunctorBind _ =
			raise InternalError "pass2FunctorBind"
      in
        (* Each element in the list is a functor binding. *)
        forall pass2FunctorBind structList;
        (* Put them into the enclosing env. *)
        #apply sEnv (#enterFunct env)		
      end; (* pass2FunctorDec *)
      
      fun pass2SignatureDec (str: structs, structList : sigBind list) : unit =
      let
        (* There is a restriction that the same name may not be bound twice.
		   As with other bindings functor bindings happen in parallel.
		   DCJM 6/1/00. *)
        val sEnv =  (* The new names. *)
          noDuplicates 
            (fn name => 
              errorNear (lex, true, str, lno, 
                         "Signature " ^ name ^ 
                         " has already been bound in this declaration")
            );

        fun pass2SignatureBind ({name, sigStruct, line, sigRef}) =
          let (* Each element in the list is a signature binding. *)
            (* Get the signature.  We can't have a declaration of a
               signature inside a functor so we can start at 0
               rather than !typeNo. *)
            val resSig : signatures =
				sigVal (sigStruct, 0, Env env, line, strName ^ name ^ ".");
            (* Generate a signature with the new name and put it
               in the table *)
			val copiedSig = makeCopy (name, resSig, sigMinTypes resSig, sigMaxTypes resSig)
          in
		     sigRef := copiedSig; (* Remember the signature for pass4. *)
             #enter sEnv (name, copiedSig)
           end
      in
        forall pass2SignatureBind structList;
        (* Put them into the enclosing env. *)
        #apply sEnv (#enterSig env)		
      end; (* pass2SignatureDec *)

       fun pass2Localdec (decs : structs list, body : structs list) : unit =
       let
         val newEnv = makeEnv (makeSignatures "");
             
         (* The environment for the local declarations. *)
         val localEnv =
           {
            lookupVal     =
              lookupDefault (#lookupVal    newEnv) (#lookupVal    env),
            lookupType    =
              lookupDefault (#lookupType   newEnv) (#lookupType   env),
            lookupFix     = #lookupFix    newEnv,
            lookupStruct  =
              lookupDefault (#lookupStruct newEnv) (#lookupStruct env),
            lookupSig     = #lookupSig    env,
            lookupFunct   = #lookupFunct  env,
            lookupTvars   = #lookupTvars  newEnv,
            enterVal      = #enterVal     newEnv,
            enterType     = #enterType    newEnv,
            enterFix      = fn _ => (),
            enterStruct   = #enterStruct  newEnv,
            enterSig      = #enterSig     newEnv,
            enterFunct    = #enterFunct   newEnv
           };
        
        (* Process the local declarations. *)
        val U = pass2Struct (decs, typeNo, Env localEnv, lno, strName);
             
        (* This is the environment used for the body of the declaration.
           Declarations are added both to the local environment and to
           the surrounding scope. *)
           
        (* Look-ups come from the local env *)
        val bodyEnv = 
          {
           lookupVal     = #lookupVal    localEnv,
           lookupType    = #lookupType   localEnv,
           lookupFix     = #lookupFix    localEnv,
           lookupStruct  = #lookupStruct localEnv,
           lookupSig     = #lookupSig    localEnv,
           lookupFunct   = #lookupFunct  localEnv,
           lookupTvars   = #lookupTvars  localEnv,
           enterVal      =
             fn (pair as (name, v)) =>
               (
                #enterVal newEnv pair;
                #enterVal env    pair
               ),
           enterType     =
             fn (pair as (name, v)) =>
               (
                #enterType newEnv pair;
                #enterType env    pair
               ),
           enterFix      = #enterFix     localEnv,
           enterStruct   =
             fn (pair as (name, v)) =>
               (
                #enterStruct newEnv pair;
                #enterStruct env    pair
               ),
           enterSig      =
             fn (pair as (name, v)) =>
               (
                #enterSig newEnv pair;
                #enterSig env    pair
               ),
           enterFunct    = #enterFunct   localEnv
          };
      in 
        (* Now the body. *)
        pass2Struct (body, typeNo, Env bodyEnv, lno, strName)
      end; (* pass2Localdec *)
      
      fun pass2Singleton (dec : parsetree, vars, line : int) : unit =
      let (* Single declaration - may declare several names. *)
        (* As well as entering the declarations we must keep a list
            of the value and exception declarations. *)
         val newEnv = 
           {
             lookupVal     = #lookupVal    env,
             lookupType    = #lookupType   env,
             lookupFix     = #lookupFix    env,
             lookupStruct  = #lookupStruct env,
             lookupSig     = #lookupSig    env,
             lookupFunct   = #lookupFunct  env,
             lookupTvars   = #lookupTvars  env,
             (* Must add the entries onto the end in case a declaration
                with the same name is made. e.g.
                   local ... in val a=1; val a=2 end. *)
             enterVal      =
               fn (pair as (name,v)) =>
                 (
                   #enterVal env pair;
                   vars := !vars @ [CoreValue v]
                 ),
             enterType     =
               fn (pair as (name,t)) =>
                 (
                   #enterType env pair;
                   vars := !vars @ [CoreType t]
                 ),
             enterFix      =
               fn (pair as (name,f)) =>
                 (
                   #enterFix env pair;
                   vars := !vars @ [CoreFix pair]
                 ),
             (* This will only be used if we do `open A' where A
                contains sub-structures. *)
             enterStruct   =
               fn (pair as (name,v)) =>
                 (
                   #enterStruct env pair;
                   vars := !vars @ [CoreStruct v]
                 ), 
             enterSig      = #enterSig     env,
             enterFunct    = #enterFunct   env
           };
           
         val discard : types = 
           pass2 (dec, makeTypeId, Env newEnv, lex, line, strName);
       in
         ()
       end; (* pass2Singleton *)

      fun pass2Dec (str : structs) : unit =
        case str of
          StructureDec (structList : structBind list) =>
            pass2StructureDec (str, structList)
           
        | FunctorDec (structList : functorBind list) =>
            pass2FunctorDec (str, structList)
        
        | SignatureDec (structList : sigBind list) =>
            pass2SignatureDec (str, structList)
        
        | Localdec {decs, body, ...} =>
            pass2Localdec (decs, body)
                
        | Singleton {dec, vars, line} =>
            pass2Singleton (dec, vars, line)
                
        | _ => (* empty (we should check this!!!) *)
            ();
    in        
      forall pass2Dec strs (* Process all the top level entries. *)
    end (* pass2Struct *);
		 
  in (* Structures and types at the top level are free identifiers. *)
    pass2Struct (strs, (* make free ids *) ref ~1, Env env, lineno lex, "")
  end (*pass2Structs *);

  (* When we have done all the unification we can we need to check all
     the values for free type variables.  The arguments to this function
	 are functions which yield all the values, structures and functors
	 in the top-level environment. *)
  fun checkForFreeTypeVars(applyVal, applyStruct, applyFunc, lex : lexan) =
  let
		(* Check the type of a value. *)
		fun checkValue(name: string, v: values) =
			checkForFreeTypeVariables(name, valTypeOf v, lex)

		(* Find all the values in the structure. *)
		fun checkStruct(name: string, s: signatures) =
		let
			fun checkEntry(dName: string, dVal: universal, ()) =
				if tagIs structVar dVal
				then checkStruct(name ^ dName ^ ".",
						structSignat((tagProject structVar) dVal))
				else if tagIs valueVar dVal
				then checkValue(name ^ dName, (tagProject valueVar) dVal)
				else ()
		in
			univFold(sigTab s, checkEntry, ())
		end
  in
		applyVal(fn (s: string, v: values) => checkValue(s, v));
		applyStruct(
			fn (n: string, s: structVals) =>
				checkStruct(n^".", structSignat s));
		(* Look at the result signature of the functor. *)
		applyFunc(
			fn (n: string, f: functors) => checkStruct(n^"().", functorResult f))
  end;



  (*							*
   *     Code-generation phase.	*
   *							*)


  (* Generate code from the expressions and arrange to return the results
      so that "pass4" can find them. *)
  fun gencodeStructs (strs, lex, globalEnv) =
  let
    (* Each top level declaration is assigned a distinct address. *)
    val addresses = ref 1;
    fun mkAddr ()  = 
      let
        val addr = !addresses
      in
        addresses := addr + 1;
        addr
      end;
      
    val level = ref 0; (* Only non-zero inside a functor body *)
    
    (* Apply a function which returns a pair of codelists to a list of structs.
	   This now threads the debugging environment through the functions so
	   the name is no longer really appropriate.  DCJM 23/2/01. *)
    fun mapPair
		(f: 'a * debugenv -> {code: codetree list, load: codetree list, debug: debugenv})
		[] debug =
          {
            code = [],
            load = [],
			debug = debug
          }
          
      | mapPair f (h::t) debug =
    let
      (* Process the list in order. In the case of a declaration sequence
         later entries in the list may refer to earlier ones. *)
        val this = f (h, debug);
        val rest = mapPair f t (#debug this);
     in  (* Return the combined code. *)
       { 
         code = #code this @ #code rest,
         load = #load this @ #load rest,
		 debug = #debug rest
       }
     end;

   (* Code-generate a structure value, and return the result 
      after matching it to the required signature. *)
    fun structureCode (str, resultSig, strName, debugEnv) =
    let
       (* Generate a new structure which will match the given signature.
          A structure is represented by a vector of entries, and its
          signature is a map which gives the offset in the vector of 
          each value. When we match a signature the candidate structure
          will in general not have its entries in the same positions as
          the target. We have to construct a new structure from it with
          the entries in the correct positions. In most cases the optimiser
          will simplify this code considerably so there is no harm in using
          a general mechanism. *)
      fun matchStructure (code : codetree, source : signatures, dest : signatures) =
      ( let
          val decs = multipleUses (code, mkAddr, !level);
          val load = #load decs (!level); (* All local *)
          
          (* To save taking apart a structure and then rebuilding it, if the
             structure has not changed we just copy it. *)
          val useOriginal = ref true;
          
          (* We put the entries into this vector and then flatten it. *)
          val resVec   = STRETCHARRAY.stretchArray (10 (* Guess *), CodeZero);
          val maxEntry = ref 0;
          
          fun addToList code addr =
            (
              STRETCHARRAY.update (resVec, addr, code); 
                     (* SPF 7/6/94 fixed off-by-one *)
              if addr >= !maxEntry then maxEntry := addr + 1 else ()
            );
        
          val U : unit =      (* Structures. *)
            univFold
             (sigTab dest,
              (fn (dName, dVal, ()) =>
                if tagIs structVar dVal
                then let
                  val dval = tagProject structVar dVal;
                in
                  if isFormal (structAccess dval)
                  then let
                    val destAddr     = vaFormal (structAccess dval);
                    val sourceStruct =
                       univLookup (sigTab source, structVar, dName);
                         
                    val access = structAccess sourceStruct;
                    (* Since these have come from a signature we might expect all
                       the entries to be "formal". However if the structure is
                       global the entries in the signature may be global, and if
                       the structure is in a "struct .. end" it may be local. *)
                    val code = 
                      if isFormal access
                      then let (* select from the code. *)
                        val U : unit = 
                          if vaFormal access <> destAddr
                          then useOriginal := false
                          else ()
                      in
                        mkInd (vaFormal access, load)
                      end
                      else let
                        val U : unit = useOriginal := false;
                      in
                        codeStruct (sourceStruct, !level)
                      end;
                         
                    val matched =
                      matchStructure 
                        (code,
                         structSignat sourceStruct,
                         structSignat dval);
                        
                    val U : unit = 
                      if not (#unchanged matched)
                      then useOriginal := false
                      else ();
                  in
                    addToList (#code matched) destAddr
                  end
                  else ()
                end

                else if tagIs valueVar dVal
                then let (* values. *)
                  val dval = tagProject valueVar dVal;
                in
				  case dval of
				  	Value{access=Formal addr, ...} =>
                  let
                    val sourceVal =
                      univLookup (sigTab source, valueVar, dName);
                       
                    (* If we have an exception matching a value we have to
                        generate a packet or a function yielding a packet. *)
                     val excBecomesVal =
					 	case (dval, sourceVal) of
							(Value{class=SimpleValue, ...}, Value{class=Exception, ...}) =>
								true
							|	_ => false
					 (* Similarly, if we have a constructor which becomes a value we
					    have to extract the injection function or the value. *)
					 val constrBecomesVal =
					 	case (dval, sourceVal) of
							(Value{class=SimpleValue, ...}, Value{class=Constructor _, ...}) =>
								true
							|	_ => false
                       
                      (* If the entry is from a signature select from the code. *)
                     val code =
					   case sourceVal of
					   	Value{access=Formal svAddr, ...} =>
						   let
	                         val UUU =
	                           if svAddr <> addr orelse excBecomesVal orelse constrBecomesVal
	                           then useOriginal := false
	                           else ();
	                       in
	                         if excBecomesVal
	                         then (* Have to make a packet or a function returning a packet. *)
	                           if isEmpty (valTypeOf dval)
	                           then 
	                             mkTuple [mkInd (svAddr, load), mkStr (valName dval), CodeZero]
	                            else
	                              mkProc 
	                                (mkTuple
	                                   [mkInd (svAddr, #load decs (!level + 1)),
	                                    mkStr (valName dval),
	                                    mkLoad (~1, 0)],
	                                1, 1, "")
	                         else if constrBecomesVal
							 then mkInd(1, mkInd (svAddr, load))
							 else mkInd (svAddr, load)
	                       end

						 | _ =>
	                         let
	                           val UUU = useOriginal := false;
	                         in
	                           if excBecomesVal
							   then codeExFunction(sourceVal, !level, valTypeOf dval, lex, 0 (* line no *))
							   else let
								  val valu =
									 	codeVal (sourceVal, !level, valTypeOf dval, lex, 0, globalEnv)
							   in
							   	  if constrBecomesVal
								  then mkInd(1, valu)
								  else valu
							   end
	                         end;
                  in
		             addToList code addr
                  end
				  | _ => ()
                  end

                else if tagIs typeConstrVar dVal
				then (* We need to process the value constructors.  We may well process them
				        as values anyway but we don't always. *)
					let
		            	val tcons = tagProject typeConstrVar dVal;
						val matchedType = univLookup (sigTab source, typeConstrVar, dName)

						fun processConstructor(Value{access=Formal dstAddr, ...},
											   Value{access=Formal svAddr, ...}) =
							(* Selecting from a signature. *)
			                  (
							  if svAddr <> dstAddr then useOriginal := false else (); 
					          addToList (mkInd (svAddr, load)) dstAddr
			                  )

						  | processConstructor(Value{access=Formal dstAddr, typeOf, ...}, sourceVal) =
						  	  (* Any other source. *)
						 	  (
							  useOriginal := false;
							  addToList (codeVal (sourceVal, !level, typeOf, lex, 0, globalEnv)) dstAddr
							  )

						  | processConstructor _ = ()
					in
						ListPair.app processConstructor (tcConstructors tcons, tcConstructors matchedType)
					end

                else ()
               ),
               ()
              );
        in  
          (* If we have copied the original we can use it unchanged. *)
          if !useOriginal
          then
            {
              code = code,
              unchanged = true
            }
          else (* Put the entries into a list for the vector. *)
          let
            fun foldFn i l = STRETCHARRAY.sub (resVec,i) :: l;
            val codeList  =
              revfoldIterator foldFn [] (downto (!maxEntry - 1) 0); 
                  (* SPF 7/6/94 fixed off-by-one *)
          in
            {
              code      = mkEnv (#dec decs @ [mkTuple codeList]),
              unchanged = false
            }
          end
        end
      );
    in
      case str of
        FunctorAppl {name, arg, valRef} =>
        let
          val functs = !valRef;
          val applyCode : codetree =
            mkEval 
              (codeAccess (functorAccess functs, !level),
               [structureCode (arg, structSignat (functorArg functs), strName, debugEnv)],
               false);
        in
          (* Evaluate the functor and match to the result. *)
          #code (matchStructure (applyCode, functorResult functs, resultSig))
        end

      | StructureIdent {valRef, ...} =>
        let
          val v = !valRef;
          val valCode : codetree =
            codeStruct (v, !level); (* Get the structure. *)
        in
          #code (matchStructure (valCode, structSignat v, resultSig))
        end

      | Localdec {decs, body = [localStr], ...} =>
        let (* let strdec in strexp end *)
         (* Generate the declarations but throw away the loads. *)
		 (* TODO: Get the debug environment correct here. *)
          val coded =
		  	mapPair (fn (str, debug) => codeStrdecs (strName, str, debug))
				decs debugEnv;
        in
          mkEnv (#code coded @
                 [structureCode (localStr, resultSig, strName, #debug coded)])
        end

      | StructDec {alist, value} =>
        let
          val coded = mapStrdecs alist strName debugEnv;
        in 
          (* The result is a block containing the declarations and
             code to load the results. *)
          mkEnv (#code coded @
            [#code (matchStructure (mkTuple (#load coded), value, resultSig))])
        end

      | _ =>
         raise InternalError "structureCode: not a structure"
    end (* structureCode *)

    (* We need to generate code for the declaration and then code to load
       the results into a tuple. *)
    and codeStrdecs (strName, str, debugEnv: debugenv):
		{ code: codetree list, load: codetree list, debug: debugenv} =
      case str of
        StructureDec (structList : structBind list) =>
        let
          fun codeStructureBind ({name, value, valRef, ...}: structBind, debug) =
            let
              (* Set the address of the variable representing this structure. *)
              val addr = mkAddr(); 
              val var  = vaLocal (structAccess (!valRef));
              val U : unit = #addr var  := addr; 
              val U : unit = #level var := !level;
			  val sName = strName ^ name ^ "."
			  val strCode =
			  	structureCode (value, structSignat (!valRef), sName, debug)
            in (* Get the code and save the result in the variable. *)
              {
                code = [mkDec (addr, strCode)],
                (* Load the variable. *)
                load = [mkLoad (addr, 0)],
				debug = debug (* We don't do structures at the moment. *)
              }
            end
        in
          (* Code-generate each declaration. *)
          mapPair codeStructureBind structList debugEnv
        end
         
      | FunctorDec (structList : functorBind list) =>
        let
          fun codeFunctorBind ({name, arg = FormalArg arg, body, valRef, ...}: functorBind,
		  					   debugEnv) =
            let
              val {valRef = argVal, ...} = arg;
            
              (* Go down one level. *)
              val U = level := !level + 1;
              
              (* Save the value and set to 1 *)
              val addr = !addresses;
              val U = addresses := 1;
              val arg = vaLocal (structAccess (!argVal));
              val U = #addr  arg := ~1; 
              val U = #level arg := !level;
              val func = !valRef;
              
              val name : string = strName ^ name;
              
              (* Process the body and make a procedure out of it. *)
              val functorCode : codetree =
                (if !inlineFunctors then mkMacroProc else mkProc)
                (structureCode (body, functorResult func, name ^ "().", debugEnv),
                 !level, 1, name);
                  
              (* Go back down a level and add 1 to address. *)
              val U = level := !level - 1; 
              val U = addresses := addr + 1;
              
              (* Set the address of this variable. Because functors can only
                 be declared at the top level the only way it can be used is
                 if we have 
                    functor F(..) = ... functor G() = ..F..
                 with no semicolon between them. They will then be taken as
                 a single declaration and F will be picked up as a local. *)
              val var = vaLocal (functorAccess func);
              val U   = #addr  var := addr;
              val U   = #level var := !level;
            in
              {
                code = [mkDec (addr, functorCode)],
                load = [mkLoad (addr, 0)], (* Load the variable. *)
				debug = debugEnv
              }
            end
          | codeFunctorBind _ =
              raise InternalError "codeFunctorBind: not a FunctorBind"
        in
          mapPair codeFunctorBind structList debugEnv
        end
 
      | Localdec {decs, body, ...} =>
        let (* Accumulate the code from the declarations,
               and the code to load the results. *)
		  (* TODO: Get the environment right here. *)
          val codeDecs = mapStrdecs decs strName debugEnv;
          val codeBody = mapStrdecs body strName (#debug codeDecs);
        in  (* Combine the lists. *)
          { 
            code = #code codeDecs @ #code codeBody,
            load = #load codeBody, (* Result is just the body. *)
			debug = #debug codeBody (* TODO: This isn't correct. *)
          }
        end
 
      | Singleton {dec, vars, line, ...} =>
        let
          (* Load each variable and exception that has been declared.
             Since value declarations may be mutually recursive we have
             to code-generate the declarations first then return the values. *)
		  fun filterVals (CoreValue v) = SOME(codeVal (v, !level, badType, nullLex, 0, globalEnv))
		   |  filterVals _            = NONE
		  val (code, newDebug) =
		  	gencode (dec, lex, debugEnv, !level, addresses, strName, line, globalEnv)
        in
          { 
            code = code,
            load = List.mapPartial filterVals (!vars),
			debug = newDebug
          }
        end
        
      | _ => (* signature decs *)
        { 
          code = [],
          load = [],
		  debug = debugEnv
        }
    (* end codeStrdecs *)

    and mapStrdecs strs strName debugEnv =
          mapPair (fn (str, debug) => codeStrdecs (strName, str, debug))
		  	strs debugEnv;

    val coded = mapStrdecs strs "" ([], fn _ => CodeZero); (* Process top level list. *)

  in 
    (* The result is code for a vector containing the results of the
       declarations which pass4 can use to pull out the values after
       the code has been run. *)
    mkEnv (#code coded @ [mkTuple (#load coded)])
  end (* gencodeStructs *);

  (* Once the code has been executed the declarations must be added to
     the global scope. The type and infix status environments have already
     been processed so they can be dumped into the global environment
     unchanged. The values and exceptions, however, have to be picked out
     the compiled code. Note: The value constructors are actually produced
     at the same time as their types but are dumped out by enterGlobals. *)
  (* This previously only processed declarations which required some code-generation and
     evaluation (structures, values and functors).  It now includes types, signatures and
	 fixity so that all declarations can be printed in the order of declaration.  DCJM 6/6/02. *)
  fun pass4Structs (results, strs, {enterVal,enterStruct,enterFunct,enterType,enterFix,enterSig}) =
  PolyML.exception_trace(fn () =>
  let
	(* Process the datatypes in the structure and turn their value constructors
	   into Global entries.  We only need this in order to be able to print values
	   of datatypes which have been produced in structures or functors with
	   opaque signatures.  We could do this for other values as well but it's not
	   really necessary.
	   Because of sharing, value constructors from structures or functors
	   without opaque matching will already be global. *)
    fun extractValsToSig (results: codetree, signat: signatures) =
      univFold
       (sigTab signat,
        (fn (dName, dVal, ()) =>
         if tagIs structVar dVal
         then let (* Structures in the signature. *)
           val subStr = tagProject structVar dVal;
           
           (* Process this signature. *)
		   val base =
		   	case structAccess subStr of
				Formal addr => mkInd (addr, results)
			|	Global code => code
			|	_ => raise InternalError "extractValsToSig: bad access";
         in
		 	(* We could create a global substructure here and enter it. *)
           extractValsToSig (base, structSignat subStr)
         end
         
(*         else if tagIs valueVar dVal
         then let (* Values. *)
           val v = tagProject valueVar dVal;
         in
		   case v of
		   	  Value {name, typeOf, class, access = Formal addr } =>
			  	let
					val ind = mkInd (addr, results);
					val globalVal =
						Value{name=name, typeOf=typeOf, class=class, access=Global ind}
				in
			  		univEnter (sigTab signat, valueVar, dName, globalVal)
				end
			| _ => ()
         end
*)
		 else if tagIs typeConstrVar dVal
		 then let (* Types. *)
            val tcons = tagProject typeConstrVar dVal;
			(* Update the constructor list. *)
			fun copyAConstructor(Value{name=cName, typeOf, class, access = Formal addr}) =
				Value{name=cName, typeOf=typeOf, class=class, access=Global(mkInd(addr, results))}
			|	copyAConstructor c = c (* Already a global. *)
         in
		 	tcSetConstructors(tcons, map copyAConstructor (tcConstructors tcons))
        end

         else () (* Anything else *)
        ), (* end fn *)
       ()
      ) (* end extractValsToSig *);

    fun extractStruct str addr =
      case str of
        FunctorDec (structList : functorBind list) =>
        let
          fun extractFunctorBind ({name, valRef, ...}: functorBind) addr =
            let
              val code = mkInd (addr, results);
              val func = !valRef;
              val funcTree = 
                makeFunctor 
                 (functorName func,
                  functorArg func,
                  functorResult func,
                  makeGlobal code);

              val U : unit = enterFunct (name, funcTree);
            in
              addr + 1
            end
        in
          (* Get the functor values. *)
          revfold extractFunctorBind addr structList
        end
 
      | StructureDec (structList : structBind list) =>
        let
          fun extractStructureBind ({name, valRef, ...}: structBind) addr =
            let
              val resultSig = structSignat (!valRef);
			  val structCode = mkInd (addr, results)
            in
			  (* Convert the values to global.  More importantly convert any datatypes
			     with non-global value constructors. *)
			  extractValsToSig (structCode, resultSig);
              (* Make a global structure and put it in the global table. *)
              enterStruct (name, makeGlobalStruct (name, resultSig, structCode));
              addr + 1
            end
        in
          revfold extractStructureBind addr structList
        end
 
      | Localdec {body, ...} =>
          revfold extractStruct addr body
 
      (* Value, exception or type declaration at the top level. *)
      | Singleton {vars, ...} =>
        let (* Enter the values and exceptions. *)
		  fun makeDecs (CoreValue dec) addr =
                 ( case dec of
				 	Value{access=Overloaded _, ...} => (* enter it as it is. *) 
                     	enterVal (valName dec, dec)
					| Value{class, name, typeOf, ...} =>
						(* take the value out of the result vector. *)
	                   let
	                     val codeVal = mkInd (addr, results);
	                     val name    = valName dec;
	                     val val' =
						 	Value{class=class, name=name, typeOf=typeOf, access=Global codeVal}
	                   in
	                     enterVal (name, val')
	                   end;
                   addr + 1
                 )
		  |  makeDecs (CoreStruct dec) addr =
	          (* Any structures will only come from "open A" where A contains
	             sub-structures. "A" must be global otherwise the open would
	             not be global, so all the sub-structures must be global. *)
				 (
				   enterStruct (structName dec, dec);
				   addr
				 )
		  |   makeDecs (CoreFix pair) addr = (enterFix pair; addr)
		  |   makeDecs (CoreType dec) addr =
		  		(enterType(#second(splitString(tcName dec)), dec); addr)
        in
          revfold makeDecs addr (!vars)
        end

     | SignatureDec (structList : sigBind list) =>
		let
			fun decSig ({sigRef=ref s, name, ...}: sigBind) = enterSig(name, s)
		in
		    List.app decSig structList;
			addr
		end

     | _ =>
       addr; (* end extractStruct *)
     
    val lastAddr = revfold extractStruct 0 strs;
  in
    ()
  end (* pass4Structs *))
end;
