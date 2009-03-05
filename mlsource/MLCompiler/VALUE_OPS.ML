(*
	Copyright (c) 2000
		Cambridge University Technical Services Limited

    Modified David C.J. Matthews 2008.

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
    Title:      Operations on global and local values.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1986
*)

functor VALUE_OPS (

(*****************************************************************************)
(*                  LEX                                                      *)
(*****************************************************************************)
structure LEX : LEXSIG;

(*****************************************************************************)
(*                  CODETREE                                                 *)
(*****************************************************************************)
structure CODETREE :
sig
  type machineWord
  type codetree
  
  val CodeNil:          codetree;
  val CodeTrue:         codetree;
  val CodeZero:         codetree;
  val isCodeNil:        codetree -> bool;
  val mkTuple:          codetree list -> codetree;
  val mkRecLoad:        int -> codetree;
  val mkLoad:           int * int -> codetree;
  val mkInd:            int * codetree -> codetree;
  val mkConst:          machineWord -> codetree;
  val mkEnv:            codetree list -> codetree;
  val mkProc:           codetree * int * int * string -> codetree;
  val mkInlproc:        codetree * int * int * string -> codetree;
  val mkEval:           codetree * codetree list * bool -> codetree;
  val mkStr:            string   -> codetree;
  val mkRaise:          codetree -> codetree;
  val mkNot:            codetree -> codetree;
  val mkTestnull:       codetree -> codetree;
  val mkTestnotnull:    codetree -> codetree;
  val mkTestinteq:      codetree * codetree -> codetree;
  val mkTestptreq:      codetree * codetree -> codetree;
  val mkCand:           codetree * codetree -> codetree;
  val mkCor:              codetree * codetree -> codetree;
  val mkMutualDecs:       codetree list -> codetree;
  val mkIf:               codetree * codetree * codetree -> codetree;
  val mkDec:              int * codetree -> codetree;
  val evalue:           codetree -> machineWord;
  
  val structureEq:      machineWord * machineWord -> bool

  val genCode:          codetree * Universal.universal list -> unit -> codetree
end;
    
(*****************************************************************************)
(*                  STRUCTVALS                                               *)
(*****************************************************************************)
structure STRUCTVALS :
sig
  type structVals
  type codetree
  type signatures
  type typeConstrs
  type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }
  val inBasis: location
    
  type univTable
  val sigName: signatures -> string
  val sigTab:  signatures -> univTable
  val sigDeclaredAt: signatures -> location;
    
  type typeId
  val isUnsetId:  typeId -> bool
  val sameTypeId: typeId * typeId -> bool

  type typeVarForm
  
  type 'a possRef
  val pling: 'a possRef -> 'a

  (* A type is the union of these different cases. *)
  datatype types = 
    TypeVar          of typeVarForm
    
  | TypeConstruction of (* typeConstructionForm *)
      {
        name:  string,
        value: typeConstrs possRef,
        args:  types list,
        location: location
      }

  | FunctionType of (* functionTypeForm *)
    { 
      arg: types,
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

  and typeDependent =
    Print
  | PrintSpace
  | MakeString
  | MakeStringSpace
  | InstallPP
  | Equal
  | NotEqual
  | AddOverload
  | TypeDep

  and values =
  	Value of {
		name: string,
		typeOf: types,
		access: valAccess,
		class: valueClass,
        declaredAt: location }

  (* Classes of values. *)
  and valueClass =
  	  SimpleValue
	| Exception
	| Constructor of { nullary: bool }

  val tvValue: typeVarForm -> types
  val sameTv: typeVarForm * typeVarForm -> bool;

  val tcName:         typeConstrs -> string
  val tcTypeVars:     typeConstrs -> types list
  val tcEquivalent:   typeConstrs -> types
  val tcConstructors: typeConstrs -> values list
  val tcIdentifier:   typeConstrs -> typeId
  val tcEquality:     typeConstrs -> bool;
  val tcLetDepth:     typeConstrs -> int;
  val tcSetConstructors: typeConstrs * values list -> unit;

  val makeTypeConstrs:
		string * types list * types * typeId *  bool * int * location -> typeConstrs;

  val isFreeId:       typeId -> bool;
  
  val boolType:   typeConstrs
  val intType:    typeConstrs
  val charType:   typeConstrs
  val stringType: typeConstrs
  val wordType:	  typeConstrs;
  val realType:   typeConstrs
  val unitType:   typeConstrs
  val exnType:    typeConstrs
  val listType:   typeConstrs
  val refType:    typeConstrs
  val undefType:  typeConstrs

  val undefinedStruct:    structVals
  val isUndefinedStruct:  structVals -> bool
  val structSignat:       structVals -> signatures
  val structName:         structVals -> string
  val structAccess:       structVals -> valAccess
  val structDeclaredAt:  structVals -> location;
  val makeSelectedStruct: structVals * structVals -> structVals

  type functors
  val functorName:   functors -> string
  val functorArg:    functors -> structVals
  val functorResult: functors -> signatures
  val functorDeclaredAt:  functors -> location
  
  val undefinedValue: values
   
  val makeValueConstr: string * types * bool * valAccess * location -> values
  val makeGlobalV: string * types * codetree * location -> values
  val makeLocalV: string * types * int ref * int ref * location -> values
  val valName: values -> string
  val valTypeOf: values -> types
    
  datatype fixStatus = 
    Infix of int
  | InfixR of int
  | Nonfix;

  datatype env =
    Env of
      {
        lookupVal:    string -> values option,
        lookupType:   string -> typeConstrs option,
        lookupFix:    string -> fixStatus option,
        lookupStruct: string -> structVals option,
        lookupSig:    string -> signatures option,
        lookupFunct:  string -> functors option,
        enterVal:     string * values      -> unit,
        enterType:    string * typeConstrs -> unit,
        enterFix:     string * fixStatus   -> unit,
        enterStruct:  string * structVals  -> unit,
        enterSig:     string * signatures  -> unit,
        enterFunct:   string * functors    -> unit
      };

  val makeEnv: signatures -> env;
  
  type 'a tag = 'a Universal.tag;

  val signatureVar:  signatures  tag
  val structVar:     structVals  tag
  val typeConstrVar: typeConstrs tag
  val valueVar:      values      tag
  val fixVar:        fixStatus   tag
end;

(*****************************************************************************)
(*                  TYPESTRUCT                                               *)
(*****************************************************************************)
structure TYPESTRUCT :
sig
  type typeConstrs
  type types
  type lexan
  type values
  type pretty
  type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }
  
  val mkTypeConstruction: string * typeConstrs * types list * location -> types
  val mkFunctionType:     types  * types -> types
  val mkProductType:      types list -> types
  
  val generalise: 		  types -> types;
  val overloadError:      types * string * string  * lexan * location -> unit
  val generaliseOverload: types * typeConstrs list * bool -> types;
  val typeConstrFromOverload: types * bool -> typeConstrs;
  val makeEquivalent:     typeConstrs * types list -> types
  val constructorResult:  types * types list -> types
  val display:            types * int -> pretty
  val displayTypeConstrs: typeConstrs * int -> pretty
  val sameTypeVar : 	  types * types -> bool;
  val firstArg:			  types -> types;

  val copyType:types * (types -> types) * (typeConstrs -> typeConstrs) -> types;

  (* Exceptions have either type exn or exn->ty. *)
  val getFnArgType:   types -> types option
end;

(*****************************************************************************)
(*                  PRINTTABLE                                               *)
(*****************************************************************************)
structure PRINTTABLE :
sig
  type machineWord
  type typeId
  type pretty
  type typeConstrs
  type codetree
  
  val addPp:    typeId *  (int -> machineWord -> machineWord -> pretty) -> unit
  val getPrint: typeId -> (int -> machineWord -> machineWord -> pretty)
  val addOverload: string * typeConstrs * codetree -> unit
  val getOverloads: string -> (typeConstrs * codetree) list
  val getOverload: string * typeConstrs * (unit->codetree) -> codetree
end;

(*****************************************************************************)
(*                  UNIVERSALTABLE                                           *)
(*****************************************************************************)
structure UNIVERSALTABLE:
sig
  type universal = Universal.universal
  type univTable
  type 'a iter

  val univOver: univTable -> (string * universal) iter;
end;

(*****************************************************************************)
(*                  DEBUG                                                    *)
(*****************************************************************************)
structure DEBUG :
sig
    val printDepthFunTag : (unit->int) Universal.tag
    val getParameter :
       'a Universal.tag -> Universal.universal list -> 'a
end;

(*****************************************************************************)
(*                  MISC                                                     *)
(*****************************************************************************)
structure MISC :
sig
  exception InternalError of string; (* compiler error *)
  
  val quickSort : ('a -> 'a -> bool) -> 'a list -> 'a list
  
  type 'a iter;
  val for    : 'a iter -> ('a -> 'b) -> unit
  val iterList : 'a iter -> 'a list
end;


(*****************************************************************************)
(*                  PRETTYPRINTER                                            *)
(*****************************************************************************)
structure PRETTY : PRETTYSIG;

structure ADDRESS :
sig
  type machineWord;      (* any legal bit-pattern (tag = 0 or 1) *)
  type address;   (* an object that's represented as a pointer *) 
  type short = Word.word;   (* an object that's represented as a 30-bit int *)

  val wordEq : 'a * 'a -> bool

  val unsafeCast : 'a -> 'b;
  
  val isShort   : 'a -> bool;

  (* The following casts are always safe *)
  val toMachineWord : 'a    -> machineWord;
  
  (* The following casts are checked at run-time *)
  val toShort   : 'a -> short;
  val toAddress : 'a -> address;
  
  val alloc:  (short * Word8.word * machineWord)  -> address;

  val loadByte:   (address * short) -> Word8.word;
  val loadWord:   (address * short) -> machineWord;
  val assignWord: (address * short * machineWord)  -> unit
  val lock:       address -> unit

  val length: address -> short;
   
  val F_words     : Word8.word;
  val F_bytes     : Word8.word;
  val F_mutable   : Word8.word;
end;

(*****************************************************************************)
(*                  UTILITIES                                                *)
(*****************************************************************************)
structure UTILITIES :
sig
  val splitString: string -> { first:string,second:string }
end;

(*****************************************************************************)
(*                  VALUEOPS sharing constraints                             *)
(*****************************************************************************)

sharing type
  CODETREE.codetree
= STRUCTVALS.codetree
= PRINTTABLE.codetree

sharing type
  STRUCTVALS.typeConstrs
= TYPESTRUCT.typeConstrs
= PRINTTABLE.typeConstrs

sharing type
  STRUCTVALS.types
= TYPESTRUCT.types

sharing type
  STRUCTVALS.values
= TYPESTRUCT.values

sharing type
  STRUCTVALS.typeId
= PRINTTABLE.typeId

sharing type
  LEX.lexan
= TYPESTRUCT.lexan

sharing type
  PRINTTABLE.pretty
= TYPESTRUCT.pretty
= PRETTY.pretty

sharing type
  UNIVERSALTABLE.iter
= MISC.iter
    
sharing type
  ADDRESS.machineWord 
= CODETREE.machineWord
= PRINTTABLE.machineWord

sharing type
  UNIVERSALTABLE.univTable
= STRUCTVALS.univTable
) : 
  
(*****************************************************************************)
(*                  VALUEOPS exports signature                               *)
(*****************************************************************************)
sig
  type machineWord
  type lexan
  type codetree
  type types
  type values
  type structVals
  type functors
  type valAccess
  type typeConstrs
  type signatures
  type fixStatus
  type univTable
  type pretty
  type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }
  
  val exnId    : exn -> machineWord
  val exnName  : exn -> string
  val exnValue : exn -> machineWord

  val overloadType:	  values * bool -> types
  
  val chooseConstrRepr : (string*types) list -> codetree list

  (* Construction functions. *)
  val mkGvar:        string * types * codetree * location -> values
  val mkVar:         string * types * location -> values
  val mkSelectedVar: values * structVals -> values
  val mkGconstr:     string * types * codetree * bool* location -> values
  val mkGex:         string * types * codetree* location -> values
  val mkEx:          string * types * location -> values
  
  val mkSelectedType: typeConstrs * string * structVals option * location -> typeConstrs

  (* Standard values *)
  val nilConstructor:  values;
  val consConstructor: values;
  

    type nameSpace =
      { 
        lookupVal:    string -> values option,
        lookupType:   string -> typeConstrs option,
        lookupFix:    string -> fixStatus option,
        lookupStruct: string -> structVals option,
        lookupSig:    string -> signatures option,
        lookupFunct:  string -> functors option,

        enterVal:     string * values      -> unit,
        enterType:    string * typeConstrs -> unit,
        enterFix:     string * fixStatus   -> unit,
        enterStruct:  string * structVals  -> unit,
        enterSig:     string * signatures  -> unit,
        enterFunct:   string * functors    -> unit,

        allVal:       unit -> (string*values) list,
        allType:      unit -> (string*typeConstrs) list,
        allFix:       unit -> (string*fixStatus) list,
        allStruct:    unit -> (string*structVals) list,
        allSig:       unit -> (string*signatures) list,
        allFunct:     unit -> (string*functors) list
      };


  (* Print values. *)
  val displayFixStatus:  fixStatus -> pretty
  val displaySignatures: signatures * int * nameSpace -> pretty
  val displayStructures: structVals * int * nameSpace -> pretty
  val displayFunctors:   functors   * int * nameSpace -> pretty
  val displayValues: values * int * nameSpace -> pretty
  val printValues: values * int * nameSpace -> pretty
  
  val printSpaceTag: nameSpace Universal.tag
  val nullEnvironment : nameSpace
   
  val codeStruct:     structVals * int -> codetree
  val codeAccess:     valAccess  * int -> codetree
  val mkExIden:       unit -> codetree
  val codeVal:        values * int * types * lexan * location -> codetree
  val codeExFunction: values * int * types * lexan * location -> codetree
  val applyFunction:  values * codetree * int * types * lexan * location -> codetree
  val getOverloadInstance: string * types * bool * lexan * location -> codetree*string
  val isTheSameException: values * values -> bool
  val raiseBind:      codetree
  val raiseMatch:     codetree
  val makeGuard:      values * codetree * int -> codetree 
  val makeInverse:    values * codetree * int -> codetree
  
  val lookupAny:  string * (string -> 'a option) * (string -> structVals option) *
                 (structVals -> string -> 'a option) * string * 'a * (string -> unit) -> 'a
                    
  val lookupStructure:  string * {lookupStruct: string -> structVals option} * 
                        string * (string -> unit) -> structVals
                                           
  val lookupStructureDirectly: string * {lookupStruct: string -> structVals option} * 
                               string * (string -> unit) -> structVals
                                           
  val lookupValue:   string * {lookupVal: string -> values option, lookupStruct: string -> structVals option} * 
                     string * (string -> unit) -> values
                                
  val lookupTyp:   {lookupType: string -> typeConstrs option,
                    lookupStruct: string -> structVals option} * 
                   string * (string -> unit) -> typeConstrs

  type representations
  val RefForm:   representations;
  val BoxedForm: representations;
  val EnumForm:  int -> representations;

  val createNullaryConstructor: representations * string -> codetree
  val createUnaryConstructor: representations * string -> codetree

end (* VALUEOPS exports signature *) =

(*****************************************************************************)
(*                  VALUEOPS functor body                                    *)
(*****************************************************************************)
struct
  open MISC; 
  open PRETTY;
  
  open LEX;
  open CODETREE;
  open STRUCTVALS;
  open TYPESTRUCT;
  open PRINTTABLE;
  open DEBUG;
  open ADDRESS;
  open UNIVERSALTABLE;
  open Universal; (* for tag etc. *)
  open RuntimeCalls; (* for POLY_SYS and EXC numbers *)
  open UTILITIES;
  
  val lengthW  = ADDRESS.length;
  val length   = List.length;
  
  (* gets a value from the run-time system; 
    usually this is a closure, but sometimes it's an int.  *)
  val ioOp : int -> machineWord = RunCall.run_call1 POLY_SYS_io_operation;

  val andb = Word8.andb and orb = Word8.orb
  infix 6 andb;
  infix 7 orb;

(************* "types" constructors copied here to reduce garbage *********)
  fun isTypeVar          (TypeVar          _) = true
    | isTypeVar          _ = false;
     
  fun isTypeConstruction (TypeConstruction _) = true
    | isTypeConstruction _ = false;
     
  fun isFunctionType     (FunctionType     _) = true
    | isFunctionType     _ = false;
    
  fun isLabelled         (LabelledType         _) = true
    | isLabelled         _ = false;
    
  fun isEmpty             EmptyType           = true
    | isEmpty            _ = false;
    
  val emptyType            = EmptyType;
  
  val badType              = BadType;

  type typeConstructionForm = 
      {
        name:  string,
        value: typeConstrs ref,
        args:  types list
      }
         

  (* A function type takes two types, the argument and the result. *)
      
  and functionTypeForm = 
    { 
      arg: types,
      result: types
    }
      
  (* An entry in a labelled record type. *)
      
  and labelledRecEntry = 
    { 
      name: string,
      typeOf: types
    }
      
  (* A fixed labelled record. *)
      
  and labelledRecForm = 
    { 
      recList: { name: string, typeof: types } list,
      frozen: bool
    };

  fun typesTypeVar          (TypeVar          x) = x 
    | typesTypeVar          _ = raise Match;
    
  fun typesTypeConstruction (TypeConstruction x) = x 
    | typesTypeConstruction _ = raise Match;
    
  fun typesFunctionType     (FunctionType     x) = x
     | typesFunctionType     _ = raise Match;
     
  fun typesLabelled         (LabelledType         x) = x
    | typesLabelled         _ = raise Match;
  
  fun lreName   ({name,...}   : labelledRecEntry) = name;
  fun lreTypeOf ({typeOf,...} : labelledRecEntry) = typeOf;

  fun lrfFrozen  ({frozen,...} : labelledRecForm)  = frozen;
  fun lrfRecList ({recList,...} : labelledRecForm) = recList;
  
  fun tcfArgs  ({args,...}  : typeConstructionForm)  = args;
  fun tcfName  ({name,...}  : typeConstructionForm)  = name;
  fun tcfValue ({value,...} : typeConstructionForm) = !value;
(*************)

     (* Functions to construct the values. *)

 fun mkGconstr (name, typeof, code, nullary, location) =
   	makeValueConstr (name, typeof, nullary, Global code, location);

     (* Global variable *)
 val mkGvar = makeGlobalV

     (* Local variable - Generated by the second pass. *)
 fun mkVar (name, typeof, location) =  makeLocalV (name, typeof, ref 0, ref 0, location);

     (* Value in a local structure or a functor argument.  May be simple value, exception
	    or constructor. *)
 fun mkSelectedVar (Value { access = Formal addr, name, typeOf, class, declaredAt}, base) =
 		(* If the argument is "formal" set the base to the base structure. *)
 	Value{name=name, typeOf=typeOf, class=class,
          access=Selected{addr=addr, base=base}, declaredAt=declaredAt}

  |  mkSelectedVar(selected, _) = selected (* global or overloaded? *);

 (* Construct a global exception. *)
 fun mkGex (name, typeof, code, location) =
 	Value{ name = name, typeOf = typeof, access = Global code,
           class = Exception, declaredAt = location }
 
 (* Construct a local exception. *)
 fun mkEx (name, typeof, location) = 
 	Value{ name = name, typeOf = typeof,
		   access = Local{addr = ref 0, level = ref 0},
		   class = Exception, declaredAt=location }

 (* Copy a datatype (if necessary), converting the constructors to selections on
    a base structure.  This is used both when opening a structure and also for
	replicating a datatype. *)
 fun mkSelectedType(tcons: typeConstrs, newName: string, baseStruct: structVals option, location): typeConstrs =
	let
		(* Create a new constructor with the same unique ID. *)
		val typeID = tcIdentifier tcons;
		val newTypeCons =
			makeTypeConstrs(newName, tcTypeVars tcons, EmptyType, typeID,
							tcEquality tcons, tcLetDepth tcons, location);
		
		(* Copy the value constructors. *)
		fun copyAConstructor(Value{name=cName, typeOf, class, access, ...}) =
			let
				(* Copy the types of value constructors replacing
				   occurrences of the old type with the new one.
				   This is not strictly necessary but improves printing.
				   e.g. local datatype X = A | B in datatype Y = datatype X end;
				   A; prints  A: Y rather than A: X *)
			    fun copyTypeCons (tcon : typeConstrs) : typeConstrs =
					if sameTypeId(tcIdentifier tcon, typeID)
					then newTypeCons
					else tcon;
	            fun copyTyp (t : types) : types =
	               copyType (t, fn x => x, (* Don't bother with type variables. *)
				   	copyTypeCons);
	            val newType = copyTyp typeOf;
				val newAccess =
					case (access, baseStruct) of
						(* If we are opening a structure we must have a base structure
						   and we turn Formal entries into Selected.  If we are replicating
						   a datatype within a signature the original constructors will
						   be Formal. *)
						(Formal addr, SOME base) => Selected{base=base, addr=addr}
					|	(Formal _, NONE) => access
					|	_ => access; (* Probably already a global. *)
			in
				Value{name=cName, typeOf=newType, class=class, access=newAccess, declaredAt=location}
			end
	
		val newValConstrs = map copyAConstructor (tcConstructors tcons)
	in
		tcSetConstructors(newTypeCons, newValConstrs);
		newTypeCons
	end

(*****************************************************************************)
(*             Representation of Exceptions (incomplete!)                    *)
(*****************************************************************************)
  fun exnId (value : exn) : machineWord = 
    loadWord (toAddress value, 0w0);

  fun exnName (value : exn) : string = 
    unsafeCast (loadWord (toAddress value, 0w1));

  fun exnValue (value : exn) : machineWord = 
    unsafeCast (loadWord (toAddress value, 0w2));

  val toExn : machineWord -> exn = unsafeCast;

(*****************************************************************************)
(*             Representation of Datatype Constructors                       *)
(*****************************************************************************)
  (* These are the possible representations of a value constructor. *)
  datatype representations = 
    RefForm            (* As for OnlyOne but must be a monotype. *)
  | UnitForm           (* If the only value in an enumeration. *)
  | OnlyOne            (* If only one constructor, there is no tag or box. *)
                       (* Could be replaced by "UnboxedForm"? *)
    
  | EnumForm of int    (* Enumeration - argument is the number. (short?) *)
  | BoxedForm          (* Boxed but not tagged (only unary constructor) *)
  | UnboxedForm        (* Unboxed and untagged (only unary constructor) *)

  | ConstForm of machineWord         (* Constant - argument is a tagged value. *)
  | TaggedBox of int          (* Union - tagged and boxed.  i.e. the representation is a
								 pair whose first word is the tag and second is the value. *)
  | TaggedTuple of int * int  (* Union - tagged but with in-line tuple. i.e. for a
  								 tuple of size n the representation is a tuple of size n+1
								 whose first word contains the tag. *)


(* This makes the isConsTest "fn x => not (isShort x)" rather than
   "fn x => x <> 0", but that shouldn't give any worse code.
   If it does, we'll have to reinstate ConsForm as a primitive. *)
  val NilForm  = EnumForm 0;
  val ConsForm = UnboxedForm;
  
(* Similarly, this makes the tests for UnitForm and OnlyOne more
   expensive, but we shouldn't be generating them anyway (especially
   as the tests seem buggy). That's what fancy compilation
   of pattern-matching does for you! Of course, if it doesn't work,
   we can always reinstate them. SPF 22/10/94 *)
   
(* Don't do this ...
  val UnitForm = EnumForm 0;
... because it breaks the (hacky!) pretty-printing of arrays.
    (Array values are represented by a UnitForm value constructor;
    if we use an EnumForm instead, constrMatches fails, so we
    get a (handled) exception inside the pretty-printer. This
    is all very sad. The right fix would be to generalise
    use-defined pretty-printers to handle parameterised types, and
    add one of these for arrays.
    SPF 2/7/96 *)
(* I've done that although whether it fixes this particular problem
   or not I don't know. DCJM Sept 2000. *)

  val arg1     = mkLoad (~1, 0); (* saves a lot of garbage *)
  val arg2     = mkLoad (~2, 0);

(* Don't do this ...
    val OnlyOne  = UnboxedForm;
... because that gives the wrong test if the value isn't boxed. SPF 22/10/94 *)

  local
    val mutableFlags = F_words orb F_mutable;
    
    fun abstract (doIt: codetree -> codetree) (name : string) : codetree =
      mkInlproc (doIt arg1, 0, 1, name);
  
    (* we use toShort to check that the tag actually fits in a 30-bit integer *)
    fun mkTag (tag:int) : codetree = mkConst (toMachineWord (toShort tag));

    (* tag now moved into first word SPF 22/10/94 *)
    (* get the tag from a TaggedBox or ConstForm *)
    fun loadTag (u: machineWord) : machineWord = 
      loadWord (toAddress u, 0w0); (* tag is first field *)

    (* get the data from a TaggedBox *)
    fun loadTaggedBoxedValue (u: machineWord) : machineWord = 
      loadWord (toAddress u, 0w1); (* contents is second field *)

    (* get the data from a BoxedForm or a RefForm *)
    fun loadBoxedValue (u: machineWord) : machineWord = loadWord (toAddress u, 0w0);
    
    fun loadTaggedTupleValue (n: int) (u : machineWord) : machineWord =
    let
      val vec = alloc (toShort n, mutableFlags, toMachineWord 0);
      
      fun copyField i =
      let
        val w : machineWord = loadWord (toAddress u, toShort (i + 1));
      in
        assignWord (toAddress vec, toShort i, w) 
      end;
      
      fun copyFields i =
        if i < n then (copyField i; copyFields (i + 1)) else ();
    
      val U : unit = copyFields 0;
      val U : unit = lock vec;
    in
      toMachineWord vec
    end;

    fun identityApplyCode v = v;  (* no-op *)
    fun boxApplyCode v    = mkTuple [v];

    (* Inject into a union. Generate as mkTuple(arg, tag). *)
    (* we use toShort to check that the tag actually fits in a 30-bit integer *)
    fun tagBoxApplyCode (tag:int) (v : codetree) =
      mkTuple [mkTag tag, v];
              
    (* Inject into a tagged tuple - we could improve this if we knew that
       arg is an explicit tuple already, but the optimiser should do this
       anyway. I have also coded this using inline procedures with explicit
       applications because I can't work out how to make a well-formed
       block (because declartion addresses are *not* local to a block?)
       SPF 25/10/94 *)
    fun tagTupleCode (tag:int) (n:int) (name:string) : codetree =
    let
      (* copy n data fields out of ordinary record *)
      fun getFields i =
	if i < n then mkInd (i, arg1) :: getFields (i + 1) else [];
    in  
      mkInlproc (mkTuple (mkTag tag :: getFields 0), 0, 1, name)
    end;

    fun tagTupleApplyCode (tag:int) (n:int) (arg:codetree) : codetree =
      mkEval (tagTupleCode tag n "", [arg], true);

    fun taggedTupleDestructCode (n:int) (arg:codetree) : codetree =
    let
      (* copy n data fields out of tagged record *)
      fun getFields i =
	if i <= n then mkInd (i, arg1) :: getFields (i + 1) else [];
	
      val proc = mkInlproc (mkTuple (getFields 1), 0, 1, "")
    in  
      mkEval (proc, [arg], true)
    end;

    (* allocate 1 mutable word, initialise to "v"; do not evaluate "early" *)
    fun refApplyCode (v: codetree) =
      mkEval
        (mkConst (ioOp POLY_SYS_alloc_store),
        [mkConst (toMachineWord 1), mkConst (toMachineWord mutableFlags), v],
        false);

    (* If we need to make a "real" functions, here's how *)
    val refCode      = abstract refApplyCode "ref";
(*  val consCode     = abstract identityApplyCode "::" *);
    val identityCode = abstract identityApplyCode;
    val boxCode      = abstract boxApplyCode;
    
    fun tagBoxCode (tag:int) = 
      abstract (tagBoxApplyCode tag);
      
    fun tagTupleCode (tag:int) (n:int) =
       abstract (tagTupleApplyCode tag n);

    fun testTag (tag: int) (v: codetree) =
      mkTestinteq (mkTag tag, v);
      
    fun testBoxedTag (tag: int) (v: codetree) : codetree =
      mkTestinteq (mkTag tag, mkInd (0, v)); (* tag is first field *)
       
    fun taggedBoxDestructCode (box : codetree) : codetree = 
      mkInd (1, box); (* contents is second field of record *)

    (* Use loadWord not indirect because the optimiser reorders indirections. *)
    fun refDestructCode (v : codetree) : codetree =
       mkEval 
         (mkConst (ioOp POLY_SYS_load_word),
         [v, CodeZero],
         false) (* NOT early *)
       
    fun testBoxed (v: codetree) : codetree = (* not (isShort v) *)
      mkEval
        (mkConst (ioOp POLY_SYS_not_bool),
        [mkEval (mkConst (ioOp POLY_SYS_is_short), [v], true)],
        true);
        
    fun boxedDestructCode (box : codetree) : codetree =
      mkInd (0, box);

  (* The run-time representation of the constructor's value *)
  fun constrMake (test: representations) (name: string) : codetree =
    case test of
      UnboxedForm          => identityCode name
    | BoxedForm            => boxCode name
    | TaggedBox tag        => tagBoxCode tag name
    | TaggedTuple (tag, n) => tagTupleCode tag n name
    | RefForm              => refCode
    | ConstForm c          => mkConst c (* tagged value. *)
    | EnumForm tag         => mkConst (toMachineWord (toShort tag))
    | OnlyOne              => identityCode name
    | UnitForm             => CodeZero
    ;

  (* How to apply the constructor at run-time *)
  fun constrApply (test: representations) : codetree -> codetree =
    case test of
      UnboxedForm          => identityApplyCode
    | BoxedForm            => boxApplyCode
    | RefForm              => refApplyCode
    | TaggedBox tag        => tagBoxApplyCode tag
    | TaggedTuple (tag, n) => tagTupleApplyCode tag n
    | OnlyOne              => identityApplyCode
    | _                    => 
        (fn arg => raise InternalError "constant can't be applied")

  (* The run-time test whether a value matches a constructor. *)
  fun constrMatch (test: representations) (value:codetree) : codetree =
    case test of
      UnboxedForm          => testBoxed value
    | BoxedForm            => testBoxed value
    | RefForm              => CodeTrue
    | EnumForm tag         => testTag tag value 
    | TaggedBox tag        => testBoxedTag tag value
    | TaggedTuple (tag, n) => testBoxedTag tag value
    | ConstForm c          => testBoxedTag (Word.toIntX (* May be signed. *) (toShort (loadTag c))) value
    | OnlyOne              => CodeTrue
    | UnitForm             => CodeTrue
    ;

  (* The compile-time test whether a value matches a constructor. *)
(*  fun constrMatches (test: representations) (value: word) : bool =
    case test of
      UnboxedForm          => not (isShort value)
    | BoxedForm            => not (isShort value)
    | RefForm              => true
    | EnumForm tag         => wordEq (value, toMachineWord tag)
    | TaggedBox tag        => wordEq (loadTag value, toMachineWord tag)
    | TaggedTuple (tag, n) => wordEq (loadTag value, toMachineWord tag)
    | ConstForm c          => wordEq (loadTag value, loadTag c)
    | OnlyOne              => true
    | UnitForm             => true
    ;
*)

  (* The run-time code to destruct a construction. *)
  (* shouldn't the CodeZero's raise an exception instead? *)
  fun constrDestruct (test: representations) (value: codetree) : codetree =
    case test of
      UnboxedForm          => identityApplyCode value
    | BoxedForm            => boxedDestructCode value
    | RefForm              => refDestructCode value
    | TaggedBox tag        => taggedBoxDestructCode value
    | TaggedTuple (tag, n) => taggedTupleDestructCode n value
    | OnlyOne              => identityApplyCode value
    | EnumForm tag         => CodeZero (* To keep optimiser happy. *)
    | ConstForm c          => CodeZero (* (rather than raising an exception) *)
    | UnitForm             => CodeZero
    ;

  (* The compile-time function to destruct a construction. *)
(*  fun constrFetch (test: representations) (value: machineWord) : machineWord =
    case test of
      UnboxedForm         => value
    | BoxedForm           => loadBoxedValue value
    | RefForm             => loadBoxedValue value
    | TaggedBox tag       => loadTaggedBoxedValue value
    | TaggedTuple (tag,n) => loadTaggedTupleValue n value
    | OnlyOne             => value
    | EnumForm tag        => loadTag value (* shouldn't occur, but ... *)
    | ConstForm c         => value         (* shouldn't occur, but ... *)
    | UnitForm            => value         (* occurs for arrays! *)
    ;
*)

  in
	 (* Constructors are now represented as run-time values.  A nullary constructor is
	    a pair consisting of a test function and the constructor value.  A unary
		constructor is a triple: a test function, an injection function and a
		projection function.
		Previously constructors were handled entirely at compile with the appropriate
		functions inserted whenever a constructor was used.  This worked fine except
		in one case: when a datatype was used in a structure we couldn't use the optimal
		representation because it might match a datatype in a signature and we needed
		the same representation in both cases.  This arises if we have a datatype
		such as
		    type t = int * int datatype s = X | Y of t
		We can use an optimised representation because we know that Y constructors are
		always boxed.  If we have
		    type t = int datatype s = X | Y of t
		we can't do that and have to create tagged pairs for Y values.  Unfortunately
		we could pass either of these to a functor expecting a signature of the form
			sig type t datatype s = X | Y of t end
		If we handle constructors entirely at compile time we are forced to use the
		same representation for Y constructors in both cases. By passing the
		constructors as run-time values we can use different representations.
		This actually costs very little at run-time because functor are inlined
		so the constructor functions become inserted inline.  DCJM 18/5/01. *)

	  fun createNullaryConstructor (test: representations, name: string): codetree =
	  let
	  	val code =
		  	mkTuple[mkInlproc(constrMatch test arg1, 0, 1, name), (* Test function. *)
				    constrMake test name (* Value. *)]
	  in
	    (* Code generate the tuple now.  This saves us having multiple occurrences of
		   the code but more importantly allows us to be able to print values of
		   this datatype (printstruct uses evalue and that only works if we have
		   a constant). *)
	  	genCode (code, [] (* No debugging output *)) ()
	  end
	
	  fun createUnaryConstructor(test: representations, name: string): codetree =
	  let
	  	val code =
	  	mkTuple[mkInlproc(constrMatch test arg1, 0, 1, name), (* Test function. *)
			    mkInlproc(constrApply test arg1, 0, 1, name), (* Injection function. *)
			    mkInlproc(constrDestruct test arg1, 0, 1, name) (* Projection function. *)]
	  in
	    (* Code generate the tuple now. *)
	  	genCode (code, [] (* No debugging output *)) ()
	  end

 	 (* Construct a nex execption identifier - it's really just a unit ref. *)
	  fun mkExIden () : codetree = refApplyCode CodeZero;
  end;

(* 
     RefForm is used for "ref" (only). We use various representations
     of datatype / abstype constructors.
   
     Nullary constructors are represented as:
        UnitForm     (if it's the only constructor in the datatype)
        EnumForm     (if there's no more than one unary constructor)
         ConstForm    (otherwise)
    
     Unary constructors are represented as:
        OnlyOne      (if it's the only constructor in the datatype)
        UnboxedForm  (if it's the only unary constructor, applied to a tuple)
        TaggedTuple  (if it's not the only unary constructor, applied to a tuple, and we don't need backwards compatibility)
        BoxedForm    (if it's the only unary constructor)
        TaggedBox    (otherwise)
        
     Note that we can't use UnboxedForm/TaggedTuple if the datatype
     might appear in a signature, because the signature might be ambiguous
     about whether the argument is a tuple or not. In these cases, we always
     use BoxedForm/TaggedBox instead.
      
     Note that we use ConstForm, not EnumForm, for nullary constructors
     when the unary constructors are represented as TaggedTuple/TaggedBox
     because that allows the TaggedBox test to be:
       
         fn w => wordEq (loadWord (w,0), tag)
     
     rather than:
     
         fn w => not (isShort w) andalso wordEq (loadWord (w,0), tag)
     
     In my ignorance, I tried combining EnumForms with TaggedBoxes
     *without* changing the TaggedBox test and got a lot of
     core dumps as my reward (the machine didn't like fetching the
     tag from address 5!).
     
     SPF 20/10/94
  *)
  
  
  datatype constructorKind =
    Nullary             (* a nullary constructor *)
  | UnaryGeneric        (* a normal unary constructor *)
  | UnaryFunction       (* unary constructor applied to a function *)
  | UnaryTupled of int  (* a unary constructor applied to a tuple of size n  *)
  ;

  (* Make an object with the appropriate tag. Doing it here means we
     only do it once for this object. *)
  fun genConstForm (n :int) : representations =
  let
    (* In the new datatype format, I've moved the tag word, so
       we only need a one-word object. SPF 26/5/95 *)
    val vec : address = alloc (0w1, F_words, toMachineWord n);
    val U : unit      = lock vec;
  in
    ConstForm (toMachineWord vec)
  end;
    
  (* Choose tags in the sequence 0, ~1, 1, ~2, 2, ... because that
     maximises the chance of the tag being implemented by "immediate"
     data in the low-level generator. Note this means that "bool"
     gets a *different* representation than what its natural datatype
     declaration would give! Of course, we can't do this for the
     "Old" representations anyway. SPF 18/2/1998
  *)
  fun nextTag n = if n < 0 then ~ n else ~ (n + 1);
    
  (* We use this version if all the constructors are nullary (i.e. constants)
     except possibly one.  The (at most one) unary constructor is represented
	 by the boxed value and the nullary constructors by untagged integers. *)
  (* Note that "UnaryTupled 0" (which would arise as a result of a declaration of the
     form  datatype t = A of () | ... ) can't be represented as "UnboxedForm"
     because "{}" is represented as a short (unboxed) integer. *)
  fun chooseOptimisedRepr1 n [] = []
    | chooseOptimisedRepr1 n (h :: t) = 
       case h of
         (Nullary,       name) =>
		 	createNullaryConstructor(EnumForm n, name) :: chooseOptimisedRepr1 (nextTag n) t
       | (UnaryGeneric,  name) =>
	   		createUnaryConstructor(BoxedForm, name) :: chooseOptimisedRepr1 n t
       | (UnaryFunction, name) =>
	   		createUnaryConstructor(UnboxedForm, name)  :: chooseOptimisedRepr1 n t
       | (UnaryTupled 0, name) =>
	   		createUnaryConstructor(BoxedForm, name)    :: chooseOptimisedRepr1 n t
       | (UnaryTupled _, name) =>
	   		createUnaryConstructor(UnboxedForm, name)  :: chooseOptimisedRepr1 n t

  (* We use this version there's more than 1 unary constructor. *)
  (* With this representation constructors of small tuples make tuples of
     size n+1 whose first word is the tag.  Nullary constructors are represented
	 by single word objects containing the tag. *)
  fun chooseOptimisedRepr2 n [] = []
    | chooseOptimisedRepr2 n (h :: t) = 
  let
    val repr = 
      case h of
        (Nullary,       name) => createNullaryConstructor(genConstForm n, name)
      | (UnaryGeneric,  name) => createUnaryConstructor(TaggedBox n, name)
      | (UnaryFunction, name) => createUnaryConstructor(TaggedBox n, name)
      | (UnaryTupled i, name) =>
			createUnaryConstructor(
	  			if i <= 4 (*!maxPacking*) then TaggedTuple (n, i) else TaggedBox n, name)
  in
    repr :: chooseOptimisedRepr2 (nextTag n) t
  end;

	fun getTupleKind t =
	  case t of
	    LabelledType {recList = [{typeof=t', ...}], frozen = true, ...} =>
			(* Singleton records are always represented simply by the value. *)
	      getTupleKind t'
	  
	  | LabelledType {recList, frozen = true, ...} =>
	      UnaryTupled (length recList)
	  
	  | FunctionType _ => 
	      UnaryFunction
	
	  | TypeConstruction {name, value, args, ...} =>
	  	let
		    val cons = pling value
			val equiv = tcEquivalent cons;
		in
	  		(* We may have a type equivalence or this may be a datatype. *)
	  		if not (isEmpty equiv)
			then getTupleKind (makeEquivalent(cons, args))
			else if sameTypeId (tcIdentifier cons, tcIdentifier refType)
			then UnaryGeneric (* A tuple ref is NOT the same as the tuple. *)
			else (* Datatype.  For the moment we only consider datatypes with a
					single constructor since we want to find the width of the
					tuple.  At present we simply return UnaryGeneric for all
					other cases but it might be helpful to return a special
					result when we have a datatype which we know will always
					be boxed. *)
				case tcConstructors cons of
					[Value{typeOf, class=Constructor{nullary=false}, ...}] =>
						(* This may be a polymorphic datatype in which case
						   we have to invert the constructor to find the base type.
						   e.g. we may have an instance (int*int) t where t was
						   declared as datatype 'a t = X of 'a .*)
						getTupleKind(constructorResult(typeOf, args))
				|	_ => UnaryGeneric
		end
	
	  | _ =>
	      UnaryGeneric

  (* This now creates the functions as well as choosing the representation. *)
	fun chooseConstrRepr cs =
	let
		fun checkArgKind (name, EmptyType) = (Nullary, name)
		 |  checkArgKind (name, argType) = (getTupleKind argType, name)
		val kinds = map checkArgKind cs;

		fun chooseRepr [(Nullary, name)]       = [createNullaryConstructor(UnitForm, name)]
	    | chooseRepr [(UnaryGeneric, name)]  = [createUnaryConstructor(OnlyOne, name)]
	    | chooseRepr [(UnaryFunction, name)] = [createUnaryConstructor(OnlyOne, name)]
	    | chooseRepr [(UnaryTupled _, name)] = [createUnaryConstructor(OnlyOne, name)]
	    | chooseRepr l =
	    let
	      val unaryCount = List.foldl(fn((Nullary, _), n) => n | (_,n) => n+1) 0 l
	    in
	      (* tags now allocated from 0 (SPF 22/10/94) *)
	      if unaryCount <= 1
	      then chooseOptimisedRepr1 0 l (* can save the box *)
	      else chooseOptimisedRepr2 0 l (* can use tagged tuples *)
	    end;

   	in
		chooseRepr kinds
	end;
 
   (* RefForm, NilForm and ConsForm are only used for built-in types *)


(*****************************************************************************)
(*             Standard values and exceptions.                               *)
(*****************************************************************************)

  (* Nil and :: are used in parsetree for lists constructed
     using [ ... ] and are also used for initialisation. *)
  local
    val listTypeVars  = tcTypeVars listType;
    val alpha         = hd listTypeVars;
    val alphaList     = mkTypeConstruction ("list", listType, listTypeVars, inBasis);
    val consType      = mkFunctionType (mkProductType [alpha, alphaList], alphaList);
  in
    val nilConstructor  =
		mkGconstr ("nil", alphaList, createNullaryConstructor(NilForm, "nil"),  true, inBasis);
    val consConstructor =
		mkGconstr ("::",  consType,  createUnaryConstructor(ConsForm, "::"), false, inBasis);
  end;
  
  (* Create exception values - Small integer values are used for
     run-time system exceptions but only (currently) up to 22. *)
  val bindExceptionVal  = mkConst (toMachineWord EXC_Bind);
  val matchExceptionVal = mkConst (toMachineWord EXC_Match);
 
(*****************************************************************************)

  (* Look-up functions. *)
  fun mkEnv x = let val Env e = makeEnv x in e end
  
  (* Look up a structure. *)
  fun lookupStructure (kind, {lookupStruct:string -> structVals option},
		       name, errorMessage) =
  let
    fun lookupStr name secondary =
    let
      val {first = prefix, second = suffix} = splitString name;
      val strLookedUp =
        if prefix = "" then lookupStruct suffix
        else let  (* Look up the first part in the structure environment. *)
        val str =
	     lookupStructure
	       ("Structure", {lookupStruct=lookupStruct}, prefix, errorMessage);
        in 
            (* If the structure is not there the value can't be. *)
            if isUndefinedStruct str
            then SOME undefinedStruct
            else secondary str suffix (* Look up in it and select. *)
        end
    in
        case strLookedUp of
           SOME s => s
        |  NONE =>
        	 (* Not declared? *)
                (errorMessage (kind ^ " (" ^ suffix ^  ") has not been declared" ^
        	       (if prefix = "" then "" else " in structure " ^ prefix));
                undefinedStruct)
    end (* lookupStr *) 
  in
    lookupStr name 
      (fn baseStruct =>
       let
           val look = #lookupStruct (mkEnv (structSignat baseStruct));
       in
           fn name => case look name of SOME s => SOME(makeSelectedStruct (s, baseStruct)) | NONE => NONE
       end)
  end;
	
  fun lookupAny
	(name : string,
	 primary:     string -> 'a option,
	 lookupStruct:string -> structVals option,
	 secondary:   structVals -> string -> 'a option,
	 kind,
	 undefined:'a,
	 errorMessage)
	: 'a =
  let
    val {first = prefix, second = suffix} = splitString name;
    val found =
      if prefix = "" then primary suffix
      else let (* Look up the first part in the structure environment. *)
        val str =
            lookupStructure
                ("Structure", {lookupStruct=lookupStruct}, prefix, errorMessage);
      in
            (* If the structure is not there the value can't be. *)
            if isUndefinedStruct str
            then SOME undefined
            else secondary str suffix (* Look up in it and select. *)
      end
  in
      case found of
          SOME v => v
      |   NONE => (* Not declared? *)
            (errorMessage (kind ^ " (" ^ suffix ^ ") has not been declared" ^
                (if prefix = "" then "" else " in structure " ^ prefix));
            undefined)
  end (* lookupAny *) 
 
  (* Look up a structure but return the actual structure even if it is a formal. *)
  fun lookupStructureDirectly (kind, {lookupStruct}, name, errorMessage) =
    lookupAny (name, lookupStruct, lookupStruct, 
	      fn baseStruct => #lookupStruct (mkEnv (structSignat baseStruct)),
	      kind, undefinedStruct, errorMessage);
 
  (* Look up a value, possibly in a structure. If it is in
     a structure we may have to apply a selection. *)
  fun lookupValue (kind, {lookupVal,lookupStruct}, name, errorMessage) =
    lookupAny (name, lookupVal, lookupStruct,
	       fn baseStruct =>
	       let
               val look = #lookupVal (mkEnv (structSignat baseStruct));
	       in
               (fn name => case look name of SOME v => SOME(mkSelectedVar (v, baseStruct)) | NONE => NONE)
	       end,
	      kind, undefinedValue, errorMessage);
 
  fun lookupTyp ({lookupType,lookupStruct}, name, errorMessage) =
    lookupAny (name, lookupType, lookupStruct,
	      (* Types do not require a selection from the source
		 structure since there is no actual value. *)
	       fn s => #lookupType (mkEnv (structSignat s)),
	      "Type constructor", undefType, errorMessage);
 
 
 
 
      (* Printing. *)

    type nameSpace =
      { 
        lookupVal:    string -> values option,
        lookupType:   string -> typeConstrs option,
        lookupFix:    string -> fixStatus option,
        lookupStruct: string -> structVals option,
        lookupSig:    string -> signatures option,
        lookupFunct:  string -> functors option,

        enterVal:     string * values      -> unit,
        enterType:    string * typeConstrs -> unit,
        enterFix:     string * fixStatus   -> unit,
        enterStruct:  string * structVals  -> unit,
        enterSig:     string * signatures  -> unit,
        enterFunct:   string * functors    -> unit,

        allVal:       unit -> (string*values) list,
        allType:      unit -> (string*typeConstrs) list,
        allFix:       unit -> (string*fixStatus) list,
        allStruct:    unit -> (string*structVals) list,
        allSig:       unit -> (string*signatures) list,
        allFunct:     unit -> (string*functors) list
      };

  val nullEnvironment : nameSpace =
     {
        lookupVal = fn (s: string) => NONE,
        lookupType = fn (s: string) => NONE,
        lookupFix = fn (s: string) => NONE,
        lookupStruct = fn (s: string) => NONE,
        lookupSig = fn (s: string) => NONE,
        lookupFunct = fn (s: string) => NONE,
        enterVal = fn _ => (),
        enterType = fn _ => (),
        enterFix = fn _ => (),
        enterStruct = fn _ => (),
        enterSig = fn _ => (),
        enterFunct = fn _ => (),
        allVal = fn () => [],
        allType = fn () => [],
        allFix = fn () => [],
        allStruct = fn () => [],
        allSig = fn () => [],
        allFunct = fn () => []
        }

    (* Debug tag for the name space for the printer environment. *)
    val printSpaceTag: nameSpace tag = Universal.tag()
    
    fun getPrintSpace parameters =
        case List.find (Universal.tagIs printSpaceTag) parameters of
            SOME a => Universal.tagProject printSpaceTag a
        |   NONE => nullEnvironment
    

  (* Checks to see whether a labelled record is in the form of
     a product i.e. 1=, 2=   We only need this for prettyprinting. *)
  fun isProductType(LabelledType{recList, frozen=true, ...}) =
  	let
		fun isRec [] n = true
		 |  isRec ({name, typeof} :: l) n =
		 		name = Int.toString n andalso isRec l (n+1)
	in
		isRec recList 1
	end
    | isProductType _ = false;

    (* Find an exception with the given identifier. *)
    fun findException (allValues: values list, allStructs: structVals list) (w: machineWord): values option =
    let
        fun searchList f [] = NONE
	    |   searchList f (hd::tl) =
			case f hd of
				NONE => searchList f tl
			|	result => result

	  	open ADDRESS

        (* Test a value to see if it's the exception we want. *)
        fun testValue base valu =
		case valu of
			(* Top-level exception *)
			Value{class = Exception, access = Global code, ...} =>
				if wordEq(evalue code, w) then SOME valu else NONE

			(* Exception in a structure. *)
		|	Value{class = Exception, access=Formal addr, ...} =>
				if wordEq(loadWord(toAddress base, toShort addr), w)
				then SOME valu else NONE

		|	_ => NONE

        (* Search for the exception in a structure. *)
        fun searchSpace(u: univTable, base: machineWord) =
        let
	  	  (* Get a list of all the entries in this space. *)
	  	  val decList = iterList (univOver u)

		  fun findItem (s: string, u: universal): values option =
		  	 if tagIs valueVar u (* Values *)
			 then testValue base (tagProject valueVar u)

			 else if tagIs structVar u
			 then (* Search this structure recursively. *)
			 let
			 	val str = tagProject structVar u
				val access = structAccess str
			 in
			 	case access of
					Global code => (* Top-level structures. *)
						searchSpace(sigTab (structSignat str), evalue code)

				|	Formal addr => (* Sub-structures. *)
						searchSpace(sigTab (structSignat str),
							loadWord(toAddress base, toShort addr))

				|	_ => NONE
			 end

			 else NONE (* Not a structure or a value. *)
	  in
	  	  searchList findItem decList
	  end

        val globalBase = ADDRESS.toMachineWord 0 (* Unused. *)
    in
        (* First try the global values. *)
        case searchList (testValue globalBase) allValues of
            ex as SOME _ => ex (* found *)
        |   NONE => (* Not found; try in the structures. *)
                searchList (
                    fn s =>
                        case structAccess s of
                            Global code =>
                                searchSpace(sigTab (structSignat s), evalue code)
                        | _ => NONE (* Should just be globals. *))
                    allStructs
	end

    (* This module prints a structure by following the type structure. *)
    fun printStruct (value:machineWord, types, depth, nameSpace: nameSpace): pretty =
    let
        fun exceptionSearch (w: machineWord) : values option =
        let
            val values = #allVal nameSpace ()
            and strs   = #allStruct nameSpace ()
            (* Filter out the strings.  Order doesn't matter. *)
            fun getVal []            l = l
            |   getVal ((_, v) :: r) l = getVal r (v::l)
        in
            findException(getVal values [], getVal strs []) w
        end

        val { lookupFix, lookupExnById} = { lookupFix = #lookupFix nameSpace, lookupExnById = exceptionSearch }
 
        fun pVec (num, value : machineWord, [], separator, leftPrec, rightPrec, depth) doPrint = []
      
        |   pVec (num, value : machineWord, [t], separator, leftPrec, rightPrec, depth) doPrint =
            if num = 0 (* optimised unary tuples - no indirection! *)
            then [doPrint (value, t, depth, rightPrec)]
            else
            let
                val addr : address = toAddress value;
	            val entryValue : machineWord = loadWord (addr, toShort num);
	        in
	            [doPrint (entryValue, t, depth, rightPrec)]
	        end
	  
        |   pVec (num, value, t::ts, separator, leftPrec, rightPrec, depth) doPrint =
		    if depth <= 0
		    then [PrettyString "..."]
		    else
		    let
		        val addr : address = toAddress value;
		        val entryValue : machineWord = loadWord (addr, toShort num);
		    in
		        doPrint (entryValue, t, depth, leftPrec) ::
                (
		        (* Precede infix ops by a space. *)
		        (if separator <> "," then [PrettyBreak (1, 0)] else []) @
		        [PrettyString separator, PrettyBreak (1, 0)] @
		        pVec (num + 1, value, ts, separator, leftPrec, rightPrec, depth-1) doPrint
                )
		    end (* pVec *);

        fun prints (value : machineWord, types, depth, precedence, objList) : pretty =
        let (* Print out the contents of a tuple or labelled record. *)
	  
            (* Print the constructor in infix notation if appropriate. *)
            fun printInfixed(constrName, argType, args, objList): pretty =
            let
                val maxPrec = 999;
		        val thisPrecedence = getOpt(lookupFix constrName, Nonfix);

                (* Some of these need to be parenthesised.  We replace values at
		           level 1 by "..." rather than printing "(...)". *)
                fun mayParenthesise true (f: int -> pretty list): pretty list =
                    if depth <= 1
                    then [PrettyString "..."]
                    else PrettyString "(" :: (f (depth-1) @ [PrettyString ")"])
                |	mayParenthesise false f = f depth

            in
                PrettyBlock(3, false, [],
        		case (thisPrecedence, argType) of
        			(Infix precNo,
        				LabelledType{recList=recList as [{name="1", ...}, {name="2", ...}], ...}) =>
        			  mayParenthesise (precNo < precedence)
        			  	 (fn depth =>
        				  pVec (0, args, recList, constrName, precNo, precNo + 1, depth)
        				  	(fn (value, {name, typeof}, depth, precedence) =>
        						  prints (value, typeof, depth, precedence, objList)
        					))

        		  | (InfixR precNo,
        				LabelledType{recList=recList as [{name="1", ...}, {name="2", ...}], ...}) =>
        			  mayParenthesise (precNo < precedence)
        			  	 (fn depth =>
        				  pVec (0, args, recList, constrName, precNo + 1, precNo, depth)
        				  	(fn (value, {name, typeof}, depth, precedence) =>
        						  prints (value, typeof, depth, precedence, objList)
        					))

        		  | _ =>
        			  (* This constructor is not infix - print it in prefix notation.
        			     If the constructor is already applied to something we must
        			     parenthesise it. The argument precedence is set to infinity
        			     - i.e. any constructors must be in parentheses. *)
        			  mayParenthesise (precedence = maxPrec)
        			  	 (fn depth =>
        				  	 (
        					  (* Must precede infix constructors by ``op''. *)
        					  (case thisPrecedence of
        						Nonfix => []
        						| _ => [PrettyString "op", PrettyBreak (1, 0)]
                              ) @
        					  [
                                  PrettyString constrName,
                                  PrettyBreak (1, 0),
        					      prints (args, argType, depth, maxPrec, objList)
                              ]
        				     ))
                )
            end (* printInfixed *);
 
            (* Prints out a type construction by undoing the value constructors *)
            fun printConstruction typeArgs []: pretty =
                raise InternalError "none matches" (* Shouldn't happen *)
	     
            | printConstruction typeArgs
			    (Value{name, typeOf, access=Global code, class = Constructor{nullary}, ...} :: constrs) =
                let   (* Try this constructor *)
                    open ADDRESS
			        val base = toAddress (evalue code)
			        val test = loadWord(base, 0w0) (* First word is the test. *)
			        val matches: bool = unsafeCast test value
		        in
		            if not matches (* try the next *) then printConstruction typeArgs constrs
		    
		            (* matches *)
		            else if nullary then (* Just a constant *) PrettyString name
		    
		            (* Not just a constant. *) 
		            else if depth <= 0
		            then PrettyString "..."
		            else
                    let
		                (* The test succeeded so this is the constructor that made
		                   this value  - get the value out. *)
			            val project = loadWord(base, 0w2) (* Third word is projection fn. *)
		                val v : machineWord = unsafeCast project value
		   
            		    (* Find the argument type which gives this result when the
            		       constructor is applied. If we have, for example, a value of
            		       type int list and we have discovered that this is a `::' node
            		       we have to work back by comparing the type of `::' 
            		       ('a * 'a list -> 'a list) to find the argument of the
            		       constructor (int * int list) and hence how to print it.
            		       (Actually `list' is treated specially). *)
		                val resType = constructorResult (typeOf, typeArgs);
            			(* If the value we get back from undoing the constructor
            			   is the same as the constructed value, i.e. applying the
            			   constructor simply returns the argument, we don't want to
            			   add this value to the list.  If we do we won't be able to
            			   print the constructed value. *)
            			val newList =
            				if ADDRESS.wordEq(v, value) then objList
            				else value :: objList
                    in
            		    printInfixed(name, resType, v, newList)
                    end
                end

        	  	(* Normally a datatype constructor will be global. If, though, we
        		   call PolyML.print within a functor on a datatype passed in as
        		   a functor argument the code to test for constructor will be
        		   in the actual argument.  We could generate code to handle that
        		   case but it's probably not worth it. *)
            | printConstruction typeArgs _ = PrettyString "?"
	  		    (* printConstruction *)

        in (* prints *)
    		(* If we have a circular structure we could end up looping until
    		   we reach the maximum depth.  Instead we check for any structure
    		   we've seen before and just print .... We really only need to include
    		   mutable structures in the list because only they can form loops
    		   but it's easier to include everything. *)
    		if List.exists (fn v => ADDRESS.wordEq(value, v)) objList
    		then PrettyString "..."
	
    		else case types of
    			TypeVar tyVar =>
    			let
    			  (* The type variable may be bound to something *)
    			  val tyVal = tvValue tyVar
    			in
    			  if isEmpty tyVal then PrettyString "?"
    			  else prints (value, tyVal, depth, precedence, objList)
    			end
	
    		| TypeConstruction{value=tval, args, ...} =>
                let
    			    val constr = pling tval
    			in
            	  if isUnsetId (tcIdentifier constr)
            	    then PrettyString "?"
        
            	  (* Type-specific printing is all handled by the libraries now. *)
            	  (* Although unit is a type construction it is treated as
            	     equivalent to the empty labelled record.  That means we
            		 can't install a pretty printer for it using addPrettyPrinter. *)
            	  else if sameTypeId (tcIdentifier constr, tcIdentifier unitType)
            	    then PrettyString "()"
        
            	  (* Leave this one, at least for the moment, since we need to be able
            	     to look the exception up in the environment. *)
            	  else if sameTypeId (tcIdentifier constr, tcIdentifier exnType)
            	    then let (* Exception. *)
            	      val exn  : exn    = unsafeCast value;
            	      val name : string = exnName exn;
            		  (* In order to be able to print this exception we need to find the type of
            		     any arguments.  Previously we used the name to search for a global
            			 exception with that name but that doesn't help if the exception is in
            			 a structure.  We now do a search of the complete name space.  *)
            		in
            		  case lookupExnById (exnId exn) of
            		  	SOME exc =>
            			let
            				val typeof = valTypeOf exc
            			in
            				case getFnArgType typeof of
                                NONE => PrettyString name
            				|   SOME typeof => printInfixed(name, typeof, exnValue exn, value::objList)
            			end
            		  |	NONE => PrettyString name (* Just put the name. *)
            	    end
        	    
            	  else (* All the others. *)
            	    let    (* Use the given print function if it is in the table,
            		          otherwise use the default. *)
            			(* If we have a print function installed for this type constructor
            			   we have to pass it the functions to print the argument types
            			   (if any). *)
            			fun makeArg argType =
            		  		let
            					fun printArg(v, depth) = prints(v, argType, depth, ~1, value::objList)
            				in
            					mkConst(toMachineWord printArg)
            				end
            			(* The easiest way to make a tuple is to use Codetree.mkTuple
            			   which makes a tuple immediately if all the arguments
            			   are constants. *)
            			val argTuple =
            				case args of
            					[] => CodeZero
            				  | [t] => makeArg t
            				  | args => mkTuple(map makeArg args)
            		in
            	      getPrint (tcIdentifier constr) depth (evalue argTuple) value
            	      handle Subscript =>
                	      ( if not (null (tcConstructors constr))
                		    then printConstruction args (tcConstructors constr)
                		    else if not (isEmpty (tcEquivalent constr))  (* May be an alias *)
                		    then prints (value, makeEquivalent (constr, args), depth, precedence, objList)
                		    else PrettyString "?"
                		  )
            	    end
            		(* isTypeConstruction *)
                end
      
    		| FunctionType _ => PrettyString "fn"
      
    		| LabelledType {recList, ...} =>
    			if depth <= 0
    			then PrettyString "..."
    			else if isProductType types
    			then (* If it is a record of the form {1=, 2=, ... } *)
                    PrettyBlock (1, false, [], (* Print them as (t1, t2, t3) .... *)
                        PrettyString "(" ::
			            (
                            pVec (0, value, recList, ",",  ~1, ~1, depth)
                				(fn (value, {name, typeof}, depth, precedence) =>
                					prints (value, typeof, depth, precedence, objList)
                				) @
			                [PrettyString ")"]
                        )
                    )
 	
    			else
                    PrettyBlock (1, false, [], (* Print them as ( a = X, b = Y ... ) *)
    			        PrettyString "{" ::
                        (
            			  pVec (0, value, recList, ",", ~1, ~1, depth)
            			  	(fn (value : machineWord, {name, typeof}, depth, precedence) =>
            					PrettyBlock (0, false, [],
                                    [
                                        PrettyString (name ^ " ="),
                                        PrettyBreak (1, 0),
                					    (* Don't add the current value to objList here.  We may have an
                					       optimised unary tuple in which case the value will be the
                						   same as the one we've just had. Since we're only really
                						   concerned about references making loops that should be fine. *)
                                        prints (value, typeof, depth - 1, ~1, objList)
                                    ]
                                )
            				) @
                            [PrettyString "}"]
                         )
                    )
	
    		| _ => PrettyString "<empty>"
        end  (* prints *);
    in
        prints (value, types, depth, ~1, [])
    end (* printStruct *);

    fun displayFixStatus Nonfix = PrettyString "nonfix"
    | displayFixStatus (Infix prec) =
        PrettyBlock(0, false, [],
			[ PrettyString "infix", PrettyBreak (1, 0), PrettyString (Int.toString prec) ])
    | displayFixStatus (InfixR prec) =
        PrettyBlock(0, false, [],
			[ PrettyString "infixr", PrettyBreak (1, 0), PrettyString (Int.toString prec) ])

    (* displays value as a block, with no external formatting *)
    fun displayValues (Value{name, typeOf, class, access, declaredAt}, depth, nameSpace): pretty =
    let
        (* Create the "val X =" part. *)
        fun valPart equOrColon =
            PrettyBlock (0, false, [],
                [
                    PrettyString "val",
                    PrettyBreak (1, 0),
	                PrettyBlock(0, false, [ContextLocation declaredAt], [PrettyString name]),
                    PrettyBreak (1, 0),
                    PrettyString equOrColon
                ]
            )
    in
  
        if depth <= 0 
        then PrettyString "..."

	    else
        case (class, access) of
		(SimpleValue, Global code) => (* Normal top-level values *)
     		PrettyBlock (0, false, [],
                [
                    valPart "=",
     		        PrettyBreak (1, 3),
            		(printStruct (evalue code, typeOf, depth, nameSpace)
            		      handle InternalError _ => PrettyString "<undefined>"),
            		      (* evalue will fail for "undefined". *)
                          (* I think "undefined" here was an old dummy-functor which has since been removed. *)
    		        PrettyBreak (1, 3),
                    PrettyBlock (0, false, [],
                        [ PrettyString ":", PrettyBreak (1, 3), display (typeOf, depth) ])
                 ])

    	|	(SimpleValue, _) => (* Values in structures and overloaded values (from PolyML). *)
                (* We can't get a value to print in this case. *)
     		PrettyBlock (0, false, [],
                [ valPart ":", PrettyBreak (1, 3), display (typeOf, depth) ])

	    |	(Exception, _) => (* exceptions *)
     		PrettyBlock (0, false, [],
                PrettyBlock (0, false, [],
                    [
                        PrettyString "exception",
                        PrettyBreak (1, 0),
    	                PrettyBlock(0, false, [ContextLocation declaredAt], [PrettyString name])
                    ]
                )
                ::
                (
                    case getFnArgType typeOf of
                       NONE => []
                    |  SOME excType =>
                        [ PrettyBreak (1, 1), PrettyString "of", PrettyBreak (1, 3), display (excType, depth) ]
                )
            )
      
        | _ => PrettyString "" (* ??? *)
    end;

  (* Print global values.  This is passed through the bootstrap and used in the debugger. *)
  fun printValues (Value{name, typeOf, class, access, ...}, depth, nameSpace) =
        case (class, access) of
		    (SimpleValue, Global code) =>
                printStruct (evalue code, typeOf, depth, nameSpace)
        | _ => PrettyString "" (* Probably shouldn't occur. *)

  (* Prints "sig ... end" as a block, with no external formatting *)
  fun displaySig (str, depth : int, space : int, nameSpace) : pretty =
  let      
    fun displaySpec (name, value) : pretty list =
      if (tagIs signatureVar value)
      then
        [ PrettyBreak(1,2), displaySignatures (tagProject signatureVar value, depth - 1, nameSpace)]
		   
      else if (tagIs structVar value)
      then
    	[ PrettyBreak(1,2), displayStructures (tagProject structVar value, depth - 1, nameSpace)]
		       
      else if (tagIs typeConstrVar value)
      then 
    	[ PrettyBreak(1,2), displayTypeConstrs (tagProject typeConstrVar value, depth) ]
      
      else if (tagIs valueVar value)
      then let
       (* Only print variables. Constructors are printed with their type. *)
		val value = tagProject valueVar value;
      in
	    case value of
			Value{class = Constructor _, ...} => []
		|	_ =>
		  [ PrettyBreak(1,2),
		  (* We lookup the infix status and any exception in the global environment
		     only.  Infix status isn't a property of a structure and it's too
			 much trouble to look up exceptions in the structure. *)
		    displayValues (value, depth, nameSpace)
		  ]
      end
      
      else if (tagIs fixVar value)
      then 
		[ PrettyBreak(1,2), displayFixStatus (tagProject fixVar value) ]
 
      else []
       (* end displaySpec *)
  in
    PrettyBlock (0, true, [],
        PrettyString "sig" ::
        (
            (
            	if depth <= 1 (* If the depth is 1 each of the calls to displaySpec will
            					 print "..." so we replace them all by a single "..." here. *)
            	then [PrettyBreak (1, 0), PrettyString "..."]
            	else
            	let
            		val declist = ref nil : (string * universal) list ref
            		fun addToList nv = declist := nv :: !declist
            		(* For the moment order them by name.  We may change this to
            		   order primarily by kind and secondarily by name. *)
            		fun order (s1: string, _) (s2: string, _) = s1 <= s2
            	in
            		(* Put all the entries into a list. *)
                	for (univOver (sigTab str)) addToList;
            		(* Sort the list and print it. *)
            		List.foldl
                        (fn (a, l) => displaySpec a @ l)
                        [] (quickSort order (!declist))
            	end
            )
            @ [PrettyBreak (1, 0), PrettyString "end"]
        )
    )
  end (* displaySig *)

  (* Print: signature S = sig .... end *)
  and displaySignatures (str, depth : int, nameSpace) : pretty =
    if depth <= 0 then PrettyString "..."
    else
        PrettyBlock(0, false, [],
            [
                PrettyBlock(0, false, [],
                    [
                        PrettyString "signature",
                        PrettyBreak(1, 0),
                        PrettyBlock(0, false,
                            [ContextLocation(sigDeclaredAt str)],
                            [PrettyString(sigName str)]
                        ),
                        PrettyBreak(1, 0),
                        PrettyString "="
                    ]
                ),
            PrettyBreak (1, 2),
            displaySig (str, depth, 1, nameSpace)
            ])

  (* print structure in a block (no external spacing) *)
    and displayStructures (str, depth, nameSpace): pretty =
    if depth <= 0 then PrettyString "..."
    else if isUndefinedStruct str then PrettyString "<bad>"
    else
    let
        val structureName = structName str;
        val signatureName = sigName (structSignat str);
    in
        PrettyBlock (0, false, [],
        [
            PrettyBlock(0, false, [],
                [
                    PrettyString "structure",
                    PrettyBreak(1, 0),
                    PrettyBlock(0, false,
                        [ContextLocation(structDeclaredAt str)],
                        [PrettyString structureName]
                    ),
                    PrettyBreak(1, 0),
                    PrettyString ":"
                ]
            ),
    	    PrettyBreak(1, 2),
            if signatureName <> ""
    	    then PrettyString signatureName
            else displaySig (structSignat str, depth - 1, 1, nameSpace)
        ])
    end

    fun displayFunctors (funct, depth, nameSpace) =
    if depth <= 0 then PrettyString "..."
    else
        PrettyBlock (0, false, [],
            PrettyBlock(0, false, [],
                [
                    PrettyString "functor",
                    PrettyBreak(1, 0),
                    PrettyBlock(0, false,
                        [ContextLocation(functorDeclaredAt funct)],
                        [PrettyString(functorName funct)]
                    ),
                    PrettyBreak(1, 0),
                    PrettyString "("
                ]) ::
    	    PrettyBreak(0, 0) ::
            (
            (if structName (functorArg funct) <> ""
            then [ PrettyString((structName (functorArg funct)) ^ " :"), PrettyBreak(1, 3) ]
            else []) @
            [
                displaySig (structSignat (functorArg funct), depth - 1, 0, nameSpace),
                PrettyString ") :",
        	    PrettyBreak(1, 3),
                displaySig (functorResult funct, depth - 1, 1, nameSpace)
            ]
            )
        )
   
  (* Code-generation. *)

     (* Code-generate the values. *) 
 fun codeStruct (str, level) =
     (* Global structures have no code value. Instead the
        values are held in the values of the signature. *)
   if isUndefinedStruct str
   then CodeNil
   else codeAccess (structAccess str, level)

 and codeAccess (Global code, _) = code
      
 |  codeAccess (Local{addr=ref locAddr, level=ref locLevel}, level) =
     let
       val levelDiff = level - locLevel;
     in
       if locAddr = 0
       then mkRecLoad (levelDiff - 1)   (* Recursive *)
       else mkLoad (locAddr, levelDiff) (* Argument or local *)
     end
     
 | codeAccess (Selected{addr, base}, level) = (* Select from a structure. *)
       mkInd (addr, codeStruct (base, level))
     
 | codeAccess (acc, level) = raise InternalError "No access"
     (* codeAccess *);

     (* Raises an exception. *)
 fun raiseException exName exIden parm =
   mkRaise (mkTuple [exIden,mkStr exName,parm]);

 (* Raise match and bind exceptions. *)
 val raiseMatch     = raiseException "Match" matchExceptionVal CodeZero;
 val raiseBind      = raiseException "Bind" bindExceptionVal CodeZero;

(*****************************************************************************)
(*                  Equality function compiler                               *)
(*****************************************************************************)

(*
    This generates code to handle equality (i.e. = and <>) by generating functions
	based on the type of the arguments to be compared.  For parameterised datatypes
	(e.g. 'a list) that means generating functions which are parameterised on the
	equality function for 'a.  In a few cases (e.g where the type is a functor
	argument) we can't generate the function and we fall back to the general purpose
	structure equality function.  The optimiser (codetree) does a good job of optimising
	the code and turning the functions into loops. 
*)

 fun genEqualityFunction(instance: types, level: int): codetree =
 let
    (* To reduce the size of the code we pass down the kind of
	   result we want. *)
 	datatype reskind =
		ApplyFun of ({level:int, myAddr: int}->codetree)*({level:int, myAddr: int}->codetree)
	|   MakeFun
	(* If we get a function back it may take a pair as an argument or
	   it may take two arguments. *)
	datatype resfun =
		PairArg of {level:int, myAddr: int} -> codetree
	|	TwoArgs of {level:int, myAddr: int} -> codetree

	val baseLevel = level+1
	local
		val addrs = ref 0
	in
		fun mkaddrs () = (addrs := !addrs + 1; !addrs)
	end

	(* The list of functions.  These are potentially mutually recursive. *)
	val generatedFuns: codetree list ref = ref []
	(* The list of addresses of functions for datatypes.  This allows us
	   to make recursive calls for recursive datatypes and also to avoid
	   generating the same function twice.
	   e.g. datatype t = A of s | B and s = C of t | D. *)
	val datatypeList: (int * typeId) list ref = ref []

	(* If we have a function we either return it or we apply it.  The
	   function will always take a single argument as a tuple. *)
	fun returnFun (f: resfun, MakeFun) = f
	 |  returnFun (TwoArgs f, ApplyFun(a1, a2)) =
			PairArg(fn lA => mkEval(f lA, [a1 lA, a2 lA], true))
	 |  returnFun (PairArg f, ApplyFun(a1, a2)) =
			PairArg(fn lA => mkEval(f lA, [mkTuple[a1 lA, a2 lA]], true))

	(* If we have a piece of code we may need to wrap it in a function.
	   This is generally used to create the code for handling tuples.
	   When creating a function this previously attempted to add the function to
	   the generatedFuns list but that turned out to have a bug.  If we have
	   a tuple inside a parameterised datatype e.g. datatype 'a t = X of 'a * ... then
	   we must make sure that we create the tuple equality function inside the equality
	   function for t otherwise it won't be able to find the equality function for 'a.  *)
	fun returnCode(mkCode, ApplyFun(a1, a2)) =
			PairArg(fn l => mkCode(a1, a2, l))
	  | returnCode(mkCode, MakeFun) =
	  	let
			fun wrappedCode {level, myAddr} =
			let
				val addr = mkaddrs() (* Should never be used since this isn't directly recursive. *)
				val newLevel = level+1
	
				val code = mkCode(fn {level=l, ...} => mkLoad(~1, l-newLevel),
								  fn {level=l, ...} => mkLoad(~2, l-newLevel),
								  {level=newLevel, myAddr=addr});
			in
				mkProc(code, newLevel, 2, "eq{...}(2)")
			end
		in
			TwoArgs wrappedCode
		end

 	val default = PairArg(fn _ => mkConst (toMachineWord structureEq))

 	fun makeEq(ty: types, resKind: reskind,
			   findTyVars: typeVarForm -> resfun): resfun =
	let

		fun equalityForDatatype(constr, vConstrs) : {level:int, myAddr: int} -> codetree =
		let
			val id = tcIdentifier constr
			val typeName = tcName constr
			val addr = mkaddrs()
			(* We need to record this address in the list. *)
			val _ = datatypeList := (addr, id) :: !datatypeList;
			(* If this is a polymorphic type constructor (e.g. 'a list)
			   we have to pass the equality functions for the argument
			   type (e.g. int if we have int list) as arguments to the
			   equality function. *)
			val constructorTypeVars = tcTypeVars constr
			val nTypeVars = List.length constructorTypeVars

			val outerFunLevel = baseLevel+1
			val newLevel =
				if nTypeVars = 0 then outerFunLevel else outerFunLevel+1

			fun newTvFun tv =
			let
				fun findTv [] n = findTyVars tv (* Not in this list. *)
				 |  findTv (TypeVar tv' :: tvs) n =
				 		if sameTv(tv, tv')
						then TwoArgs(fn {level, ...} => mkLoad(n, level-outerFunLevel))
						else findTv tvs (n+1)
				 |  findTv _ _ =
				 		raise InternalError "findTv: not a type variable"
			in
				findTv constructorTypeVars (~nTypeVars)
			end

			(* Filter out the EnumForm constructors.  They arise
			   in situations such as datatype t = A of int*int | B | C
			   i.e. where we have only one non-nullary constructor
			   and it is a tuple.  In this case we can deal with all
			   the nullary constructors simply by testing whether
			   the two arguments are the same.  We don't have to
			   discriminate the individual cases. *)
			fun isEnum(Value{class=Constructor{nullary=true}, access=Global code, ...}) =
				let
					open ADDRESS
				in
					(* If the value is a short integer then we can check
					   for equality using pointer equality. *)
					isShort(loadWord(toAddress(evalue code), 0w1))
				end
			  | isEnum _ = false

			fun processConstrs [] =
					(* The last of the alternatives is false *) CodeZero

			 |	processConstrs ((vConstr as Value{class, access, typeOf, name=tempConstrName, ...}) ::rest) =
			 	if isEnum vConstr then processConstrs rest
				else
			 	let
					val base = codeAccess(access, newLevel)
					fun matches arg =
						mkEval(mkInd(0, base) (* Test function. *), [arg], true)
				in
					case class of
						Constructor{nullary=true} =>
							mkIf(matches arg1, matches arg2, processConstrs rest)
					|	_ => (* We have to unwrap the value. *)
						let
							(* Get the constructor argument given
							   the result type.  We might actually be
							   able to take the argument type off directly
							   but there's some uncertainty about whether
							   we use the same type variables for the
							   constructors as for the datatype. (This only
							   applies for polytypes). *)
							val resType =
								constructorResult(typeOf, constructorTypeVars)

							(* Code to extract the value. *)
							fun destruct argNo {level=l, ...} =
								mkEval(mkInd(2, codeAccess(access, l)) (* projection function. *),
									[mkLoad(argNo, l-newLevel)], true)

							(* Test whether the values match. *)
							val eqValue =
								applyEq(resType, destruct ~1, destruct ~2,
										{level=newLevel, myAddr=addr}, newTvFun)	
						in
							(* We have equality if both values match
							   this constructor and the values within
							   the constructor match. *)
							mkIf(matches arg1,
								mkCand(matches arg2, eqValue),
								processConstrs rest)
						end
				end

            (* We previously only tested for bit-wise (pointer) equality if we had
               at least one "enum" constructor in which case the test would eliminate
               all the enum constructors.  I've now extended this to all cases where
               there is more than one constructor.  The idea is to speed up equality
               between identical data structures. *)
			val eqCode =
                case vConstrs of
                   [vcons] => (* Single constructor. *)
                       if isEnum vcons
                       then CodeTrue (* Return true here: processConstrs would return false. *)
                       else processConstrs vConstrs
                 |  _ => (* More than one constructor: should never be zero. *)
                        mkCor(mkTestptreq(arg1, arg2), processConstrs vConstrs)
			val eqFun =
				mkProc(eqCode, newLevel, 2, "eq-" ^ typeName ^ "(2)")
			(* If this is a monotype we can return it directly otherwise we
			   need to wrap it up in a function to take the equality functions
			   for the argument types. *)
			val resFun =
				if nTypeVars = 0 then eqFun
				else mkProc(eqFun, outerFunLevel, nTypeVars, "eq-" ^ typeName ^ "(1)")
		in
			generatedFuns := !generatedFuns @ [mkDec(addr, resFun)];
			fn {level=l, myAddr} => mkLoad(addr, l-baseLevel)
		end
		
		fun equalityForConstruction(constr, args, vConstrs): resfun =
		(* Generate an equality function for a datatype construction. *)
		let
			(* See if we are currently making this function or
			   have already made it.  If this is recursive we may
			   be able to optimise it. *)
			val id = tcIdentifier constr
			val constrName = tcName constr

			fun searchList [] = NONE
			|	searchList ((addr, t) :: rest) =
					if sameTypeId(t, id) then SOME addr else searchList rest
			val alreadyAddr = searchList (!datatypeList)
			(* Get the equality functions for the argument types.
			   These want to be functions taking two arguments.
			   This applies only to polytypes. *)
			fun getArg (lA as {level, myAddr}) ty : codetree =
			let
				val eqFun = makeEq(ty, MakeFun, findTyVars)
			in
				case eqFun of
					PairArg f =>
							(* Have to make a function which takes two arguments. *)
							mkInlproc(
								mkEval(f{level=level+1, myAddr=myAddr}, [mkTuple[arg1, arg2]], true),
								level+1, 2, "eq-"^constrName^"(...)")
				|	TwoArgs f => f lA
			end
			(* If we are compiling a recursive polytype (e.g. list) and
			   we find a recursive call we can generate better code by
			   calling the inner function directly, provided the recursive
			   call involves the polymorphic type.  This isn't true if we
			   have datatype 'a t = X of int t | Y of 'a where the
			   recursive call is not polymorphic. *)
			 fun sameTypeVars(TypeVar tv, TypeVar tv') = sameTv(tv, tv')
			 |   sameTypeVars _ = false

			 fun recursiveEq (addr, []) {level:int, myAddr: int} =
				if addr = myAddr
				then mkRecLoad(level-baseLevel-1)
				else mkLoad(addr, level-baseLevel)

			 |	recursiveEq (addr, _) (lA as {level:int, myAddr: int}) =
				if addr <> myAddr
				then mkEval(mkLoad(addr, level-baseLevel),
						map (getArg lA) args, true)
				else if ListPair.foldl
							(fn(a, b, t) => t andalso sameTypeVars(a,b))
							true (args, tcTypeVars constr)
				then mkRecLoad(level-baseLevel-2)
				else mkEval(mkRecLoad(level-baseLevel-1),
						map (getArg lA) args, true)
		in
			(* Apply the function we obtained to any type arguments. *)
			case searchList (!datatypeList) of
				SOME addr => TwoArgs(recursiveEq(addr, args))
			|	NONE =>
				let
					val eq = equalityForDatatype(constr, vConstrs)
				in
					case args of
						[] => TwoArgs eq
					|	_ => 
						TwoArgs(fn l =>
							mkEval(eq l, map (getArg l) args, true))
				end
		end
	in
		case ty of
			TypeVar tyVar =>
				let
				  (* The type variable may be bound to something. *)
				  val tyVal = tvValue tyVar
				in
				  (* If we have an unbound type variable it may either
				     be a type constructor argument or it may be a free
					 equality type variable. *)
				  if isEmpty tyVal
				  then returnFun(findTyVars tyVar, resKind)
				  else makeEq(tyVal, resKind, findTyVars)
				end

		|	TypeConstruction{value, args, ...} =>
			let
			    val constr = pling value
				val id = tcIdentifier constr
		 	    (* See if we have a special version of equality for this type.
				   N.B.  The only special functions we have for polytypes are
				   for 'a ref and 'a array.  In these cases the function does
				   pointer equality and is applied directly.  We must not use
				   the normal approach of combining the equality function for
				   the polytype with that for the argument because in this
				   case there may not be an equality function for the argument.
				   e.g. we can use equality on (int->int) ref. *)
				val special = getOverload("=", constr, fn () => CodeNil);
			in
				if not (isCodeNil special) (* There's an overloading. *)
				then returnFun(PairArg(fn _ => special), resKind)
				else (* Not there *)
					if sameTypeId (id, tcIdentifier unitType)
				then (* unit - always true for equality. *)
						returnCode(fn _ => CodeTrue, resKind)
				else case tcConstructors constr of
					[] => (* Not a datatype. *)
					if not (isEmpty (tcEquivalent constr))  (* May be an alias *)
					then makeEq (makeEquivalent (constr, args), resKind, findTyVars)
					else (* An opaque eqtype - probably a functor argument.  N.B. since
							we're returning the structure equality function we mustn't
							apply it to the equality function for the arguments. *)
						returnFun(default, resKind)
				|	Value{access=Formal _, ...} :: _ =>
						(* If the datatype constructor is a parameter to a functor
						   the value constructors will be in parameter structure.
						   It's too complicated to find out where the structure is
						   so we just use structure equality. *)
						returnFun(default, resKind)
				|	vConstrs => (* Datatype. *)
						returnFun(
							equalityForConstruction(constr, args, vConstrs), resKind)
			end

		|   LabelledType {recList=[{typeof=singleton, ...}], ...} =>
				(* Unary tuples are optimised - no indirection. *)
					makeEq(singleton, resKind, findTyVars)

		|   LabelledType {recList, ...} =>
				(* Combine the entries.
					fun eq(a,b) = #1 a = #1 b andalso #2 a = #2 b ... *)
				let
					fun eqTuple(arg1, arg2, lA) =
					let
						fun combineEntries ([], n) = CodeTrue
						|	combineEntries ({typeof, name=_}::t, n) =
								mkCand
								(applyEq(typeof, fn l => mkInd(n, arg1 l),
										 fn l => mkInd(n, arg2 l),
										 lA, findTyVars),
								 combineEntries (t, n+1))
					in
						combineEntries(recList, 0)
					end
				in
					returnCode(eqTuple, resKind)
				end

		|	_ => (* It is actually possible to get an equality function
					on functions in ML97 as a result of sharing constraints
					in a functor. The signature would not be matchable by
					a real structure so the functor could never be applied.
					Nevertheless the functor must compile so we just put
					in structure equality here. *)
				returnFun(default, resKind)
	end

	(* Make an equality function and apply it to the arguments. *)
	and applyEq(ty, arg1, arg2, lA, findTyVars): codetree =
		case makeEq(ty, ApplyFun(arg1, arg2), findTyVars) of
			PairArg c => c lA
		|	TwoArgs _ => raise InternalError "applyEq: wrong result"	

 	(* The instance type is a function so we have to get the first
	   argument. *)
	val argType = firstArg instance
	(* Get the final equality function and generate any which are needed
	   as a side effect. *)
 	val resultCode = makeEq(argType, MakeFun, fn _ => default)
 in
 	(* The final result function must take a single argument.  If we have
	   generated a function the result must be one which takes two arguments.
	   If we have not generated it it must have come from somewhere else so
	   it must take a pair. *)
 	case resultCode of
		PairArg c => c {level=baseLevel,myAddr=0}
	|	TwoArgs c =>
			let
				(* Must call c BEFORE we dereference generatedFuns because
				   the call may generate new functions. *)
				val code = c {level=baseLevel,myAddr=0}
				val funs = ! generatedFuns
				val wrappedFuns =
					case funs of
						[singleton] => singleton
					|	funs => mkMutualDecs funs
			in
				(* We need to wrap this up in a new inline function for two reasons.
				   One is that it needs to take a single pair argument, the other is
				   that we have allocated the addresses from 1 and we may get conflicts
				   with addresses in the surrounding scope. *)
				mkInlproc(CODETREE.mkEnv[wrappedFuns,
						  mkEval(code, [mkInd(0, arg1), mkInd(1, arg1)], true)],
						  baseLevel, 1, "equality")
			end
 end


(*****************************************************************************)
(*                  datatype access functions                                *)
(*****************************************************************************)

 (* Get the appropriate instance of an overloaded function.  If the
    overloading has not resolved to a single type it finds the preferred
	type if possible (i.e. int for most overloadings, but possibly real,
	word, string or char for conversion functions.) *)
 fun getOverloadInstance(name, instance, isConv, lex, lineno): codetree*string =
 	 let
	 val constr = typeConstrFromOverload(instance, isConv)
	 (* If there is no matching type produce a message. That should only
	    happen if we are running in ML90 mode and the overloading has not
		reduced to a single type.  *)
	 fun notFound () =
	 	if name = "=" (* Special case *)
		then mkConst (toMachineWord structureEq)
		else
 		let
			val ops = getOverloads name
			(* Construct a list of the current overloadings. *)
			fun makeOverloadList [] = ""
			 |  makeOverloadList [(last, _)] = tcName last
			 |  makeOverloadList ((h, _)::l) =
			 		tcName h ^ "/" ^ makeOverloadList l
			val overloads = makeOverloadList ops
		in
           overloadError (instance, name, overloads, lex, lineno);
		   CodeNil
		end

	 in
	    (getOverload(name, constr, notFound), tcName constr)
	 end

 (* Code-generate an identifier matched to a value.  N.B. If the value is a
    constructor it returns the pair or triple representing the functions on the
	constructor. *)
 fun codeVal (Value{access = Global code, ...}, _, _, _, _) = code

  |  codeVal (Value{access = Local{addr=ref locAddr, level=ref locLevel}, ...}, level, _, _, _) =
     let
       (* Load the variable. *)
       val levelDiff = level - locLevel;
     in
       if locAddr = 0
       then mkRecLoad (levelDiff - 1)   (* Recursive *)
       else mkLoad (locAddr, levelDiff) (* Argument or local *)
     end

  |  codeVal (Value{access = Selected{addr, base}, ...}, level, _, _, _) =
       (* Select from a structure. *)
       mkInd (addr, codeStruct (base, level))

  |  codeVal (Value{access = Formal _, ...}, level, _, _, _) =
  		raise InternalError "codeVal - Formal"

  |  codeVal (Value{access = Overloaded Print, ...}, level, instance, lex, _) =
        let 
         (* "instance" should be 'a -> 'a. We need to get the 'a. *)
         val argType = case getFnArgType instance of NONE => badType | SOME t => t;
         open DEBUG
         (* The parameter is the reference used to control the print depth
            when the value is actually printed. *)
         val printDepthFun = getParameter printDepthFunTag (LEX.debugParams lex)
         and prettyOut = getPrintOutput (LEX.debugParams lex)
         and printSpace = getPrintSpace (LEX.debugParams lex)

         fun printProc value =
         (
            prettyOut(printStruct (value, argType, printDepthFun(), printSpace));
            value
         );
           (* Coerce the procedure so that it can be put into the code. *)
       in
            mkConst (toMachineWord printProc)
       end 

  |  codeVal (Value{access = Overloaded PrintSpace, ...}, level, instance, _, _) =
        let 
            (* "instance" should be 'a * namespace * printer * int -> 'a. We need to get the 'a. *)
            val argType =
                case getFnArgType instance of
                    SOME (LabelledType { recList = {typeof, ...} ::_, ...}) => typeof
                |   _ => badType;

            fun printProc (value, namespace, printer, depth): unit =
                printer(printStruct (value, argType, depth, namespace))
         in
            mkConst (toMachineWord printProc)
        end 

  |  codeVal (Value{access = Overloaded MakeString, ...}, level, instance, lex, _) =
       let 
         (* "instance" should be 'a -> string. We need to get the 'a. *)
         val argType = case getFnArgType instance of NONE => badType | SOME t => t;

         val printSpace = getPrintSpace (LEX.debugParams lex)

         fun makeString value =
            uglyPrint (printStruct (value, argType, 10000, printSpace));
       in
         mkConst (toMachineWord makeString)
       end

  |  codeVal (Value{access = Overloaded MakeStringSpace, ...}, level, instance, lex, _) =
        let 
            (* "instance" should be 'a * namespace -> string. We need to get the 'a. *)
            val argType =
                case getFnArgType instance of
                    SOME (LabelledType { recList = {typeof, ...} ::_, ...}) => typeof
                |   _ => badType;

            fun printProc (value, namespace) =
                uglyPrint (printStruct (value, argType, 10000, namespace))
        in
            mkConst (toMachineWord printProc)
        end

  |  codeVal (Value{access = Overloaded InstallPP, ...}, level, instance, _, _) =
         let 
         (* "instance" should be ((,,,) -> int-> 'a -> 'b -> unit) -> unit.
             We need to get the 'a and 'b.  This function installs a
			 pretty printer against the type which matches 'b.
			 The type 'a is related to type of 'b as follows:
			 If 'b is a monotype t then 'a is ignored.
			 If 'b is a unary type constructor 'c t then 'a must have
			 type 'c * int -> unit.
			 If 'b is a binary or higher type constructor e.g. ('c, 'd, 'e) t
			 then 'a must be a tuple of functions of the form
			 ('c * int -> unit, 'd * int -> unit, 'e * int -> unit).
			 When the installed function is called it will be passed the
			 appropriate argument functions which it can call to print the
			 argument types.  *)
         fun rmvars (t as TypeVar tv) =
		 	(
			case tvValue tv of EmptyType => t (* Unbound type variable - return it*)
			  |  t' => rmvars t'
			)
		   | rmvars t = t;

         val (installType, argPrints) =
		 	case rmvars instance of
				FunctionType{arg, ...} => (* arg should be int->'a->'b->pretty*)
				(
				case arg of
					FunctionType{result, ...} => (* result should be 'a->'b->pretty*)
					(
					case result of
						FunctionType{arg=aType, result} =>
							(* arg should be 'a, result should be 'b->pretty.*)
						(
						case result of
							FunctionType{arg=bType, ...} => (* arg should be 'b *)
								(rmvars bType, rmvars aType)
						  | _ => (badType, badType)
						)
					  | _ => (badType, badType)
					)
				  | _ => (badType, badType)
				)
			  | _ => (badType, badType)

         fun installPp (pprint:int -> machineWord -> machineWord -> pretty) =
         let  (* Find the last type constructor in the chain. We have to install
                 this against the last in the chain because type constructors in
                 different modules may be at different points in the chain. *)
			  (* This does mean that it's not possible to install a
			     pretty printer for a type constructor rather than a datatype. *)
           fun followTypes (TypeConstruction{value, args, ...}) : typeId =
		       let
			       val constr = pling value
			   in
                   if isEmpty (tcEquivalent constr)
                   then let
                     val typeId : typeId = tcIdentifier constr;
    
    				 (* Check that the argument is a function from the type
    				    variable to unit. *)
    				 fun checkFun(tvar: types, FunctionType{arg, result}) =
    				 	(
    					case rmvars arg of
    						LabelledType{recList=[{name="1", typeof=arg},
    											  {name="2", typeof=depthType}],
    									 frozen=true, ...} =>
    						if sameTypeVar(rmvars tvar, rmvars arg)
    						then
    							(
    							case rmvars depthType of
    								TypeConstruction{value, args=[], ...} =>
    									if sameTypeId (tcIdentifier(pling value),
    												tcIdentifier intType)
    									then ()
    									else raise Fail "Argument printer must have type 'a*int->unit (second arg not int)"
    							  | _ => raise Fail "Argument printer must have type 'a*int->unit (second arg not type int)"
    							)
    					   	else raise Fail "Argument printer must have type 'a*int->unit (mismatched 'a)"
    					  | _ =>
    					  	raise Fail "Argument printer must have type 'a*int->unit (not pair)"
    					)
    				   | checkFun(tvar: types, _) =
    				   		raise Fail "Argument printer must have type 'a*int->unit (not function)"
    				 
    				 fun checkFuns([], []) = ()
    				   | checkFuns(tvar::tRest, {name, typeof}::argRest) =
    				   		(checkFun(tvar, typeof); checkFuns(tRest, argRest))
    				   | checkFuns _ =
    				   		raise Fail "Tuple size does not match type"
    				 	
                   in
    			   	 (* Check that the arity of the type constructor matches
    				    the arity of the tuple. *)
    				 case args of
    				 	[] => (* Simple constructor *)
    						() (* Ignore this for the moment. *)
    				   | [t] => checkFun(t, argPrints)
    				   | tlist =>
    				   		if not (isProductType argPrints)
    						then raise Fail "Argument must be a tuple"
    						else (
    						case argPrints of
    							LabelledType{recList, ...} => checkFuns(tlist, recList)
    						  | _ => ()
    						);
                     (* Check that it's a top-level datatype (NOT in a functor) *)
                       if not (isFreeId typeId)
                       then raise Fail "Invalid type (not free at top-level)"
                       else ();
                     typeId
                   end
                   else followTypes (makeEquivalent (constr, args))
			   end
           | followTypes _ =
				raise Fail "Invalid type (not a type construction) (installPP)";
         in
           addPp (followTypes installType, pprint)
         end (* installPp *);
           
       in
         mkConst (toMachineWord installPp)
       end

  |  codeVal (value as Value{access = Overloaded _, ...}, level, instance, lex, lineno) =
       (* AddOverload, Equal, NotEqual, TypeDep *)
         mkProc
           (applyFunction (value, arg1, level, instance, lex, lineno),
           1, 1, "")
     (* codeVal *)

 (* Some of these have a more efficient way of calling them as functions. *)
 and applyFunction (value as Value{class=Exception, ...}, argument, level, instance, lex, lineno) =
     let
       (* If we are applying it as a function we cannot be after the
          exception id, we must be constructing an exception packet. *)
       (* Get the exception id, put it in the packet with the exception name
          and the argument. *)
         val exIden = codeVal (value, level, instance, lex, lineno);
     in
         mkTuple (exIden :: mkStr (valName value) :: [argument])
     end

  | applyFunction(value as Value{class=Constructor _, ...},
  				  argument, level, instance, lex, lineno) =
	 let
	    (* If this is a value constructor we need to get the construction
		   function and use that. *)
         val constrTriple = codeVal (value, level, instance, lex, lineno);
	 in
	 	(* Don't apply this "early".  It might be the ref constructor and that
		   must not be applied until run-time.  The optimiser should take care
		   of any other cases. *)
		mkEval (mkInd(1, constrTriple), [argument], false)
	 end

  | applyFunction (value as Value{access = Overloaded oper, name = valName, ...},
  				   argument, level, instance, lex, lineno) =
     (
	   case oper of
	   	  Equal =>
			(* See if we have a special implementation for equality on
			   this type.  If not we have to fall back to the default
			   structure equality. *)
			(* Note: the overloadings will normally be inline functions
			   which will unwrap the argument tuple and so elide it away.
			   structureEq, though, is passed here as a pointer to
			   the code so no such optimisation is possible and we will
			   always make a tuple which will then be unwrapped inside
			   structureEq. Two solutions are possible: we could build
			   structureEq into the RTS in which case it would take an
			   argument pair (usually in registers) or we could write it
			   in the prelude and set it as an overload with some special
			   type so that getOverload would return it as the default. *)
			let
			    val code = genEqualityFunction(instance, level)
			in
				mkEval (code, [argument], true) (* evaluate early *)
			end
		
		| NotEqual =>
		   let
		   	 (* Use the "=" function to provide inequality as well as
			 	equality. *)
			 val code = genEqualityFunction(instance, level)
			 val isEqual =
			 	mkEval (code, [argument], true) (* evaluate early *)
	       in
	         mkNot isEqual
	       end
       
        | TypeDep =>
		   let
			 val (code, _) =
			 	getOverloadInstance(valName, instance, false, lex, lineno)
	       in
	         mkEval (code, [argument], true) (* evaluate early *)
	       end

	   | AddOverload =>
	   	(* AddOverload is only intended for use by writers of library modules.
		   It only does limited checking and should be regarded as "unsafe". *)
	   	let
		(* instance should be ('a->'b) -> string -> unit.  For overloadings
		   on most functions (e.g. abs and +) we are looking for the 'a, which
		   may be a pair, but in the case of conversion functions we want the 'b. *)
		   (* rmvars removes type variables put on by unification. *)
			fun rmvars (TypeVar tv) = rmvars(tvValue tv)
		     | rmvars t = t

			fun followTypes(TypeConstruction{value, args, ...}):typeConstrs =
			    let
				    val constr = pling value
				in
                   if isEmpty (tcEquivalent constr)
                   then constr
                   else followTypes (makeEquivalent (constr, args))
                end
			 | followTypes _ =
			 	    raise Fail "Invalid type (not a type construction) (addOverload)"

		   (* In normal use the instance type would be a function and
		      everything would be fine.  It is possible though that we
			  might have something of the form val a = addOverload in
			  which case we want to leave the error until runtime.  This
			  particular case seems to happen as a result of open PolyML
			  when PolyML contains addOverload. *)
		   val (argType, resultType) =
		   	case rmvars instance of
				FunctionType{arg,...} =>
				(
				case arg of
					(* We could do some checking of the type of the
					   function such as checking that we either have
					   something of the form t->t, t*t->t or t*t->bool
					   or string->t in the case of conversion functions.
					   It's probably not worth it since adding overloads
					   is only intended to be done by writers of libraries. *)
					FunctionType{arg, result} =>
						(
						case (rmvars arg) of
							LabelledType{recList=[{typeof, ...}, _], ...} =>
								(rmvars typeof, rmvars result)
						  | t => (rmvars t, rmvars result)
						)
				  | _ => (badType, badType)
				)
			  | _ => (badType, badType)

			fun addOverloading (argCode: codetree) (name: string) =
				let
					val typeToUse =
						if size name > 4 andalso
							String.substring(name, 0, 4) = "conv"
						(* For conversion functions it's the result
						   type we're interested in. For everything
						   else it's the argument type. *)
						then resultType
						else argType
					val tcons = followTypes typeToUse
				in
					addOverload(name, tcons, argCode)
				end

			(* This function is used if we can't get the codetree at
			   compile time. *)
			fun addOverloadGeneral (arg: machineWord) =
				addOverloading(mkConst arg)
		in
		(* This is messy but necessary for efficiency.  If we simply treat
		   addOverload as a function we would be able to pick up the
		   additional overloading as a pointer to a function.  Most overloads
		   are small functions or wrapped calls to RTS functions and so
		   we need to get the inline code for them. *)
		   (
    		   (* evalue raises an exception if "argument" is not a constant,
    		      or more usefully, a global value containing a constant and
    			  possibly a piece of codetree to inline. *)
    		   evalue(argument);
    		   mkConst (toMachineWord (addOverloading argument))
		   )
		   handle SML90.Interrupt => raise SML90.Interrupt
		     | _ =>
		   	mkEval (mkConst (toMachineWord addOverloadGeneral), [argument], false)
		end

	  | _ => (* Print, MakeString, InstallPP *)
         (* Just call as functions. *) (* not early *)
            mkEval (codeVal (value, level, instance, lex, lineno), [argument], false)
            
     ) (* overloaded *)
     
  | applyFunction (value, argument, level, instance, lex, lineno) =
   (* Just call as functions. *)
       mkEval (codeVal (value, level, instance, lex ,lineno), [argument], false)
   (* end applyFunction *)

  (* If the exception is being used as a value we want an exception packet
     or a function to make a packet. If it is a nullary constructor make
     an exception packet now, otherwise generate a function to construct
     an exception packet. *)
 fun codeExFunction (value, level, instance, lex, lineno) =
	case getFnArgType(valTypeOf value) of (* N.B. Not "instance" *)
	    NONE => applyFunction (value, CodeZero, level, instance, lex, lineno)
	|   SOME _ =>
            mkProc 
	          (applyFunction (value, arg1, level + 1, instance, lex, lineno),
	            1, 1, ""); (* shouldn't this function be in-lined??? SPF 20/10/94 *)

    (* Operations to compile code from the representation of a constructor. *)
    (* Code to test whether a value matches a constructor. *)
    fun makeGuard (value as Value{class=Constructor _, ...}, testing, level) =
    	mkEval(mkInd(0, codeVal (value, level, emptyType, nullLex, location nullLex)),
    		[testing], true)

    |   makeGuard (value as Value{class=Exception, ...}, testing, level) =
     (* Should only be an exception. Get the value of the exception identifier 
        and compare with the identifier in the exception packet. *)
     mkTestptreq 
        (mkInd (0, testing),
         codeVal (value, level, emptyType, nullLex, location nullLex))

    |   makeGuard _ = raise InternalError "makeGuard"

    (* Code to invert a constructor. i.e. return the value originally
    used as the argument. *)
    fun makeInverse(value as Value{class=Constructor{nullary=false}, ...}, arg, level): codetree =
    	mkEval(mkInd(2, codeVal (value, level, emptyType, nullLex, location nullLex)),
    		[arg], false) (* NOT "early" - this may be the "ref" constructor. *)

    |  makeInverse(value as Value{class=Constructor{nullary=true}, ...}, arg, level): codetree =
    	(* makeInverse is called even on nullary constructors.  Return zero to keep the
    	   optimiser happy. *) CodeZero

    |   makeInverse (value as Value{class=Exception, ...}, arg, level) =
      (* Exceptions. - Get the parameter from third word *) mkInd (2,arg)

    |   makeInverse _ = raise InternalError "makeInverse"

 (* Get the current overload set for the function and return a new
    instance of the type containing the overload set. *)
 fun overloadType(Value{typeOf, access = Overloaded TypeDep, name, ...}, isConv) =
 	let
		fun getTypes [] = []
		 |  getTypes ((t, _) :: l) = t :: getTypes l

	in
    	generaliseOverload(typeOf, getTypes(getOverloads name), isConv)
	end

 |  overloadType(Value{typeOf, ...}, isConv) =  generalise typeOf

  (* True if the arguments are definitely the same exception.  Used in the
     match compiler to see if we can merge adjacent exception patterns.
     Unlike the constructors of a datatype we can't assume that having the
     same (short) name means that the exceptions are the same, we have to
     look at the address. *)
   fun isTheSameException(Value{access = Global aCode, ...},
   						  Value{access = Global bCode, ...}) : bool =
		wordEq(evalue aCode, evalue bCode)
       
   |  isTheSameException(Value{access = Local{addr=ref aAddr, level=ref aLevel}, ...},
   						 Value{access = Local{addr=ref bAddr, level=ref bLevel}, ...}) : bool =
        (* I don't like this. It assumes that the address and level have
           already been set. *)
		aAddr = bAddr andalso aLevel = bLevel

  |  isTheSameException _ = false (* Forget about "selected" for the moment. *)


end (* body of VALUEOPS *);


