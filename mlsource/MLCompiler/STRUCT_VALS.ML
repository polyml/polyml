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
    Title:      Global and Local values.
    Author:     Dave Matthews,Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

(* This type contains the basic structures of global and local declarations.
   Putting the global declarations in a separate type allows us to install a new
   compiler (particularly to fix bugs) and still be compatible with declarations
   made with the old compiler. It is also convenient to put local values in
   here as well.  *) 
  
functor STRUCT_VALS (

(*****************************************************************************)
(*                  CODETREE                                                 *)
(*****************************************************************************)
structure CODETREE :
sig
  type codetree
  val CodeZero : codetree
end;
  
(*****************************************************************************)
(*                  MISC                                                     *)
(*****************************************************************************)
structure MISC :
sig
  exception InternalError of string;
end;

(*****************************************************************************)
(*                  UNIVERSAL                                                *)
(*****************************************************************************)
structure UNIVERSAL :
sig
  type 'a tag
  
  val tag : unit -> 'a tag
end;

(*****************************************************************************)
(*                  UNIVERSALTABLE                                           *)
(*****************************************************************************)
structure UNIVERSALTABLE :
sig
  type 'a tag;
  type univTable;
  
  val makeUnivTable: unit -> univTable;
  val univEnter:     univTable * 'a tag * string * 'a -> unit;
  val univLookup:    univTable * 'a tag * string -> 'a option;
  
  (* Freeze a mutable table so it is immutable. *)
  val univFreeze:       univTable -> univTable
end;


(*****************************************************************************)
(*                  STRUCTVALS sharing constraints                           *)
(*****************************************************************************)

sharing type
  UNIVERSAL.tag
= UNIVERSALTABLE.tag

) :  

(*****************************************************************************)
(*                  STRUCTVALS export signature                              *)
(*****************************************************************************)
sig
  type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }
  val inBasis: location
  
  (* Structures *)
  type signatures;
  type codetree;
  type typeId;
  (* type identifiers.  In the old (ML90) version these were used for
     structures as well. *)
  
  val unsetId:      typeId;
  val isUnsetId:    typeId -> bool;
  val isFreeId:     typeId -> bool;
  val isBoundId:    typeId -> bool;
  val isVariableId: typeId -> bool;
  val offsetId:     typeId -> int;
  val sameTypeId:   typeId * typeId -> bool;
  val unifyTypeIds: typeId * typeId -> bool;

  val makeFreeId:     unit -> typeId;
  val makeVariableId: unit -> typeId;
  val makeBoundId:    int  -> typeId;
  
  (* Types *)
  
  datatype 'a possRef = FrozenRef of 'a | VariableRef of 'a ref
  val pling: 'a possRef -> 'a
  val updatePR: 'a possRef * 'a -> unit
  
  (* Standard type constructors. *)
  
  type typeVarForm;
  type typeConstrs;

  (* A type is the union of these different cases. *)
  datatype types = 
    TypeVar of typeVarForm
    
  | TypeConstruction of (* typeConstructionForm *)
      {
        name:  string,
        value: typeConstrs possRef,
        args:  types list,
        location: location
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

  and valAccess =
  	Global   of codetree
  | Local    of { addr: int ref, level: int ref }
  | Selected of { addr: int,     base:  structVals }
  | Formal   of int
  | Overloaded of typeDependent (* Values only. *)

  (* Structures. *)
  and structVals = 
    NoStruct
  | Struct of
    {
      name:   string,
      signat: signatures,
      access: valAccess,
      declaredAt: location
    }

  (* Values. *)
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
  ;
      

  val badType:   types;
  val emptyType: types;
  
  val isBad:     types -> bool;
  val isEmpty:   types -> bool;

  val tcName:            typeConstrs -> string;
  val tcArity:           typeConstrs -> int;
  val tcTypeVars:        typeConstrs -> types list;
  val tcEquivalent:      typeConstrs -> types;
  val tcSetEquivalent:   typeConstrs * types -> unit;
  val tcConstructors:    typeConstrs -> values list;
  val tcSetConstructors: typeConstrs * values list -> unit;
  val tcEquality:        typeConstrs -> bool;
  val tcSetEquality:     typeConstrs * bool -> unit;
  val tcIdentifier:      typeConstrs -> typeId;
  val tcLetDepth:        typeConstrs -> int;
  val tcDeclaredAt:      typeConstrs -> location

  (* These are all logically equivalent but include differing numbers of refs *)
  val makeTypeConstrs:
  	string * types list * types * typeId *  bool * int * location -> typeConstrs;
  val makeFrozenTypeConstrs:
  	string * types list * types * typeId *  bool * int * location -> typeConstrs;

  val tvLevel:        typeVarForm -> int;
  val tvEquality:     typeVarForm -> bool;
  val tvNonUnifiable: typeVarForm -> bool;
  val tvWeak:         typeVarForm -> bool;
  val tvValue:        typeVarForm -> types;
  val tvSetValue:     typeVarForm * types -> unit;

  val sameTv: typeVarForm * typeVarForm -> bool;
  
  val makeTv: types * int * bool * bool * bool -> typeVarForm;

  val generalisable: int;
  
  val boolType:   typeConstrs;
  val intType:    typeConstrs;
  val charType:   typeConstrs; (* added 22/8/96 SPF *)
  val stringType: typeConstrs;
  val wordType:	  typeConstrs;
  val realType:   typeConstrs;
  val refType:    typeConstrs;
  val unitType:   typeConstrs;
  val exnType:    typeConstrs;
  val listType:   typeConstrs;
  val undefType:  typeConstrs;

  (* Access to values, structures etc. *)
  
  val isGlobal:   valAccess -> bool;
  val isLocal:    valAccess -> bool;
  val isSelected: valAccess -> bool;
  val isFormal:   valAccess -> bool;

  val makeGlobal:   codetree -> valAccess;
  val makeLocal:    unit -> valAccess;
  val makeSelected: int * structVals -> valAccess;
  val makeFormal:   int -> valAccess;
  
  val vaGlobal:   valAccess -> codetree;
  val vaFormal:   valAccess -> int;
  val vaLocal:    valAccess -> { addr: int ref, level: int ref };
  val vaSelected: valAccess -> { addr: int,     base:  structVals };
  
  val undefinedStruct:   structVals;
  val isUndefinedStruct: structVals -> bool;
  val structSignat:      structVals -> signatures;
  val structName:        structVals -> string;
  val structAccess:      structVals -> valAccess;
  val structDeclaredAt:  structVals -> location;
  
  val makeEmptyGlobal:   string -> structVals;
  val makeGlobalStruct:  string * signatures * codetree * location -> structVals;
  val makeLocalStruct:   string * signatures * location -> structVals;
  val makeFormalStruct:  string * signatures * int * location -> structVals;

  val makeSelectedStruct: structVals * structVals -> structVals;

  (* Functors *)
  
  type functors;

  val undefinedFunctor:   functors;
  val isUndefinedFunctor: functors -> bool;
  val functorName:        functors -> string;
  val functorArg:         functors -> structVals;
  val functorResult:      functors -> signatures;
  val functorAccess:      functors -> valAccess;
  val functorDeclaredAt:  functors -> location
  
  val makeFunctor: string * structVals * signatures * valAccess * location -> functors;

  (* Signatures *)
  
  type univTable;
  val sigName:       signatures -> string;
  val sigTab:        signatures -> univTable;
  val sigMinTypes:   signatures -> int;
  val sigMaxTypes:   signatures -> int;
  val sigDeclaredAt: signatures -> location;

  val makeSignatures: string * location -> signatures;
  val makeCopy: string * signatures * int * int * location -> signatures;

  (* Values. *)
  val valName: values -> string
  val valTypeOf: values -> types
  val undefinedValue: values;
  val isUndefinedValue: values -> bool;
  val isConstructor: values -> bool;
  val isValueConstructor: values -> bool
  
  val makeGlobalV: string * types * codetree * location -> values;
  val makeLocalV: string * types * int ref * int ref * location -> values;
  val makeFormalV: string * types * int * location -> values;  
  val makeFormalEx: string * types * int * location -> values;  
  val makeOverloaded: string * types * typeDependent -> values;
  val makeValueConstr: string * types * bool * valAccess * location -> values;
  
  (* Infix status *)

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

  type 'a tag;
  
  val valueVar:      values      tag;
  val typeConstrVar: typeConstrs tag;
  val fixVar:        fixStatus   tag;
  val structVar:     structVals  tag;
  val signatureVar:  signatures  tag;
  val functorVar:    functors    tag;

end (* STRUCTVALS export signature *) =  

(*****************************************************************************)
(*                  STRUCTVALS functor body                                  *)
(*****************************************************************************)
struct
  open CODETREE;
  
  open MISC;
  open UNIVERSAL;
  open UNIVERSALTABLE;
  
  (* Location for declarations. *)
  type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }

  (* The idea of this is reduce the number of mutable objects. *)
  datatype 'a possRef = FrozenRef of 'a | VariableRef of 'a ref
  fun pling(FrozenRef x) = x | pling(VariableRef(ref x)) = x
  fun updatePR(VariableRef r, x) = r := x | updatePR(FrozenRef _, _) = raise Fail "Assignment to frozen ref"

  abstype uniqueId = Unique of int ref
  with
    fun makeUnique () = Unique (ref 0); (* REF HOTSPOT - 400 *)

    fun sameUnique (Unique a, Unique b) = (a = b);
  end;
   
  (* There are three classes of structure or type identifier. Variables occur
     in signatures and can be assigned to other variables or constants by
     sharing constraints or by signature matching. Free constants arise from
     top level structures or types or those in top level structures. Bound
     constants occur in the arguments to functors or in structures or types
     constructed inside a functor (and therefore generated when the functor
     is applied). *)
      
  abstype typeId =
    Unset
  | Free     of uniqueId
  | Bound    of int
  | Variable of typeId ref
  
  with
    (* Variable stamps: can be set to other stamps whether variable or free. *)
    val unsetId = Unset;
    
    fun makeFreeId () = Free (makeUnique ());
  
    val makeBoundId = Bound;
  
    fun makeVariableId () = Variable (ref Unset); (* REF HOTSPOT - 260 *)
  
    (* If it is a constant or an unset variable return it,
       otherwise return the value the variable is bound to. *)
  
    fun realId id =
    (
      case id of
	Variable (ref v) => (case v of Unset => id | _ => realId v)
      | _                => id
    );
			    
    (* Unset variable. *)
    fun isUnsetId Unset = true | isUnsetId _ = false;
  
    fun isVariableId x = case (realId x) of Variable _ => true | _ => false;
    fun isFreeId     x = case (realId x) of Free     _ => true | _ => false;
    fun isBoundId    x = case (realId x) of Bound    _ => true | _ => false;
	
    (* Find the number - assuming it is bound. *)
    fun offsetId x = 
      case (realId x) of 
	Bound i => i
      | _       => raise InternalError "offsetId: not a Bound";
  
    (* Are two type constructors the same. *)
    fun sameTypeId (cons1, cons2) =
    let
      val id1 = realId cons1;
      val id2 = realId cons2;
    in
      case (id1, id2) of
	(Variable a, Variable b) => (a = b)
      | (Free     a, Free     b) => sameUnique (a, b)
      | (Bound    a, Bound    b) => (a = b)
      | _                        => false
    end;
	  
    (* Do the unification and return whether they are equal. *)
    fun unifyTypeIds (x, y) : bool =
      let
	val x1 = realId x;
	val y1 = realId y;
      in
	case (x1, y1) of
	  (Bound a, Bound b) => 
	     a = b
	
	| (Free a, Free b) => 
	    sameUnique (a, b)
    
	| (Variable x2, Variable y2) => 
	     x2 = y2 orelse (x2 := y1; true)
	  
	| (Variable x2, _) => 
	    (x2 := y1; true)
	  
	| (_, Variable y2) => 
	    (y2 := x1; true)
	  
	| _  =>
	     false
      end;
   end; (* typeId abstype *)      
    
  
  (* Used for both signatures of local structures and for global structures 
     (name spaces). Strictly signatures do not contain fix-status functors
     or signatures but as we use these structures for top-level name-spaces
     we have to have tables for these. *)
  abstype signatures =
    Signatures of
      { 
        name:       string,
        tab:        univTable,
        minTypes:   int,
        maxTypes:   int,
        declaredAt: location
      }
  with
    fun sigName       (Signatures {name,...})       = name;
    fun sigTab        (Signatures {tab,...})        = tab;
    fun sigMinTypes   (Signatures {minTypes,...})   = minTypes;
    fun sigMaxTypes   (Signatures {maxTypes,...})   = maxTypes;
    fun sigDeclaredAt (Signatures {declaredAt,...}) = declaredAt;
  
    fun makeSignatures (name, location) = 
		Signatures { name       = name,
			   tab        = makeUnivTable(),
			   minTypes   = 0, 
			   maxTypes   = 0,
               declaredAt = location };
       
    (* Used when we want to give a name to a signature. *)
    fun makeCopy (name, copy, minTypes, maxTypes, location) =
    	Signatures { name       = name,
			   tab        = univFreeze(sigTab copy),
			   minTypes   = minTypes, 
			   maxTypes   = maxTypes,
               declaredAt = location  };
       
  end; (* signatures abstype *)
    
  (* Types. *)

  (* Level at which type is generalisable. *)

  val generalisable = 9999; 
    
  (* A type is the union of these different cases. *)
  datatype types = 
    TypeVar          of typeVarForm
  | TypeConstruction of typeConstructionForm
  | FunctionType     of functionTypeForm
  | LabelledType     of labelledRecForm
  | OverloadSet		 of overloadSetForm
  | BadType
  | EmptyType

  and typeConstrs = 
     TypeConstrs of
      {
        name:       string,
        arity:      int,
        typeVars:   types list,
		updatable: (* We have a single ref here to minimise the number of refs. *)
		   {
		      equiv:      types,
			  constrs:    values list, (* List of value constructors. *)
              equal:      bool
		   }  possRef,
        identifier: typeId,
		letDepth:	int, (* Needed to check for local datatypes. *)
        declaredAt: location (* Location of declaration *)
      }

  (* Access to a value, structure or functor. *)
  and valAccess =
  	Global   of codetree
  | Local    of { addr: int ref, level: int ref }
  | Selected of { addr: int,     base:  structVals }
  | Formal   of int
  | Overloaded of typeDependent (* Values only. *)

  (* Structures. *)
  and structVals = 
    NoStruct
  | Struct of
    {
      name:   string,
      signat: signatures,
      access: valAccess,
      declaredAt: location
    }

  (* Values. *)
  (* The overloaded functions divide up into basically two groups: Those =, 
     <>, print and makestring  which are infinitely overloaded and those 
     *, + etc  which are overloaded on a limited range of types. *)  
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
        declaredAt: location (* Location of declaration *) }

  (* Classes of values. *)
  and valueClass =
  	  SimpleValue
	| Exception
	| Constructor of { nullary: bool }
  
  withtype typeConstructionForm = 
      {
        name:  string,
        value: typeConstrs possRef,
        args:  types list,
        location: location
      }
      
  and typeVarForm = 
      {
         value:    types ref,
         encoding: Word.word
      }
 
  (* A function type takes two types, the argument and the result. *)
  and functionTypeForm = 
    { 
      arg: types,
      result: types
    }
      
  (* A fixed labelled record. *)
  and labelledRecForm = 
    { 
      recList: { name: string, typeof: types } list,
      frozen: bool,
	  genericInstance: (*typeVarForm*) { value: types ref, encoding: Word.word } list
    }
	
  (* A set of type contructors.  This is used only during the
     compilation process and represents the set of possible types
	 which may occur. It functions in much the same way as a type
	 variable.  Because we only allow overloading on monomorphic
	 type constructors such as "int" and "word" we can restrict the
	 set to containing only type constructors rather than general types.
	 This overload set was added for ML 97 because ML 97, unlike ML 90,
	 defaults overloaded operators and constants if unification does
	 not result in a single type being found.  
	 The overload set is used in a similar way to a flexible record
	 and will always be pointed at by a type variable so that the
	 set can be replaced by a single type construction if the unification
	 reduces to a single type. *)
  and overloadSetForm =
  	{
		typeset: typeConstrs list
	}
    
  (* Destructors, constructors and predicates for types *)
  val emptyType            = EmptyType;
  val badType              = BadType;

  fun isEmpty             EmptyType           = true | isEmpty            _ = false;
  fun isBad               BadType             = true | isBad              _ = false;
  
  fun makeValueConstr (name, typeOf, nullary, access, location) : values =
  	Value
    { 
      name    = name,
      typeOf  = typeOf,
	  access  = access,
      class   = Constructor { nullary = nullary },
      declaredAt = location
    };

  
  (* A type variable is implemented as a true variable i.e. it can
     be assigned a particular type when it is unified. Initially it is
     set to EmptyType which represents an unset type variable.
     When it is unified with a type it is set to point to the type it
     has been unified with.  Type variables associated with variables
     have level set to the nesting level, others have level set to
     "generalisable". If two type variables are united their levels are 
     set to the lower of the two. If level is not "generalisable" the type
     variable is not generalisable. This is needed to deal with cases like
       fn a => let val x = a in x end      and
       fn a => let val x = hd a in x end
     The level is set to "generalisable" at the end of the block with that
     level. Actually ``level'' is not actually changed - instead the type
     variable is assigned to a new variable with the correct level, since
     only the last variable in a sequence is looked at.
     ``equality'' is true if this is an equality variable e.g. ''a.
     ``nonunifiable'' is true for type variables introduced explicitly
     or type variables in signatures. Such type variables can have their
     level changed but cannot be unified with other types, with other
     nonunifiable type variables or with equality variables (unless it
     is already an equality variable). 
     ``weak'' is true if this is an imperative type variable e.g. '_a *)

  fun sameTv (a : typeVarForm, b : typeVarForm) : bool = 
    #value a = #value b; (* If the same ref it must be the same *)
        
  (* To save space "equality", "nonunifiable" and "weak"
     are encoded together with the level.
  *)

    local
        open Word
        infix 8 >> <<
        infix 7 andb
        infix 6 orb
    in
        fun makeTv (t : types, lev, equality, nonunifiable, weak) : typeVarForm =
            { value    = ref t, (* REF HOTSPOT - 400 *)
              encoding = (fromInt lev << 0w3)
                           orb (if equality     then 0w4 else 0w0)
                           orb (if nonunifiable then 0w2 else 0w0)
                           orb (if weak         then 0w1 else 0w0) };
        
        fun tvSetValue ({ value, ...} : typeVarForm, t : types) = value := t;
        fun tvValue ({value = ref v, ...} : typeVarForm) : types = v;
        fun tvLevel ({encoding, ...} : typeVarForm) : int  = Word.toInt(encoding >> 0w3);
        fun tvEquality ({encoding, ...} : typeVarForm)     = encoding andb 0w4 <> 0w0;
        fun tvNonUnifiable ({encoding, ...} : typeVarForm) = encoding andb 0w2 <> 0w0;
        fun tvWeak ({encoding, ...} : typeVarForm)         = encoding andb 0w1 <> 0w0;
    end;

  (* Type constructors are identifiers which take zero or more types and yield a
     type as result. Their main property is that two type constructors can be 
     unified iff they are the same constructor. We use a "struct" so that we can 
     test for equivalence, and so we have a "nil" value for Unset type identifiers.
     In this case we will have a list of constructors for the type. Another use for
     constructors is for aliasing types. In this case "typeVars" points to a list 
     of type variables which are used in the "equivalent" type. ``equality'' is a 
     flag indicating if the values can be tested for equality. *)
      
    fun tcName       (TypeConstrs {name,...} : typeConstrs)       = name;
    fun tcArity      (TypeConstrs {arity,...} : typeConstrs)      = arity;
    fun tcTypeVars   (TypeConstrs {typeVars,...} : typeConstrs)   = typeVars;
    fun tcIdentifier (TypeConstrs {identifier,...} : typeConstrs) = identifier;
    fun tcDeclaredAt (TypeConstrs {declaredAt, ...}) = declaredAt

    local
        fun getUpdatable (TypeConstrs {updatable,...} : typeConstrs) = pling updatable;
    in
        val tcEquivalent = #equiv o getUpdatable
        val tcConstructors  = #constrs o getUpdatable
        val tcEquality = #equal o getUpdatable;
        
        fun tcSetEquivalent (TypeConstrs {updatable, name, ...}, t) =
		    let
		        val v = pling updatable
			in
			    updatePR(updatable, { equiv = t, constrs = #constrs v, equal = #equal v})
                    handle Fail _ => raise Fail ("SetEquivalent:" ^ name)
			end
		
        fun tcSetConstructors (TypeConstrs {updatable, name,...}, constrs) =
		    let
		        val v = pling updatable
			in
			    updatePR(updatable, { equiv = #equiv v, constrs = constrs, equal = #equal v})
                    handle Fail _ => raise Fail ("SetConstructors:" ^ name)
			end
			
        fun tcSetEquality (TypeConstrs {updatable, name,...}, eq) =
		    let
		        val v = pling updatable
			in
			    updatePR(updatable, { equiv = #equiv v, constrs = #constrs v, equal = eq})
                    handle Fail _ => raise Fail ("SetEquality:" ^ name)
			end
	end
	
  fun tcLetDepth	(TypeConstrs {letDepth,...} : typeConstrs) = letDepth;
        
  fun makeTypeConstrs (name, typeVars, equivalent, uid, equ, depth, location) =
    TypeConstrs
      {
		name       = name,
		arity      = length typeVars,
		typeVars   = typeVars,
		updatable  =
		    VariableRef (ref (* REF HOTSPOT - 690 refs here. *)
		         {
            		equiv   = equivalent,
            		constrs = [],
            		equal   = equ
				  }),
		identifier = uid,
		letDepth   = depth,
        declaredAt = location
      };
	  
  fun makeFrozenTypeConstrs (name, typeVars, equivalent, uid, equ, depth, location) =
    TypeConstrs
      {
		name       = name,
		arity      = length typeVars,
		typeVars   = typeVars,
		updatable  =
		    FrozenRef
		         {
            		equiv   = equivalent,
            		constrs = [],
            		equal   = equ
				  },
		identifier = uid,
		letDepth   = depth,
        declaredAt = location
      };

  val inBasis =
    { file = "Standard Basis", startLine = 0, startPosition = 0, endLine = 0, endPosition = 0}

  fun baseType name eq =
    makeFrozenTypeConstrs (name, [], EmptyType, makeFreeId (), eq, 0, inBasis);

  val boolType   = makeTypeConstrs ("bool", [], EmptyType, makeFreeId (), true, 0, inBasis);
  val intType    = baseType "int"    true;
  val charType   = baseType "char"   true;
  val stringType = baseType "string" true;
  val wordType   = baseType "word"   true;
  val realType   = baseType "real"   false; (* Not an eqtype in ML97. *)
  val exnType    = baseType "exn"    false;
  (* The unit type is equivalent to the empty record. *)
  val unitType   =
      makeFrozenTypeConstrs ("unit", [],
	       LabelledType {recList = [], frozen = true, genericInstance = []},
		   makeFreeId (), true, 0, inBasis);
   
  val listType =
     makeTypeConstrs 
       ("list",
        [TypeVar (makeTv (EmptyType, generalisable, false, false, false))],
        EmptyType,
        makeFreeId (),
        true, 0, inBasis);
            
  val refType =
    makeTypeConstrs 
      ("ref",
       [TypeVar (makeTv (EmptyType, generalisable, false, false, false))],
       EmptyType,
       makeFreeId (),
       true, 0, inBasis);
         
  val undefType = 
    makeFrozenTypeConstrs ("undefined", [], EmptyType, unsetId, false, 0, inBasis);

  (* Infix status. *) 
 
  datatype fixStatus = 
    Infix of int
  | InfixR of int
  | Nonfix;
  
      
    fun isGlobal   (Global   _) = true | isGlobal   _ = false;
    fun isLocal    (Local    _) = true | isLocal    _ = false;
    fun isSelected (Selected _) = true | isSelected _ = false;
    fun isFormal   (Formal   _) = true | isFormal   _ = false;
    
    fun vaGlobal   (Global   x) = x | vaGlobal   _ = raise Match;
    fun vaLocal    (Local    x) = x | vaLocal    _ = raise Match;
    fun vaSelected (Selected x) = x | vaSelected _ = raise Match;
    fun vaFormal   (Formal   x) = x | vaFormal   _ = raise Match;
    
    val makeGlobal = Global;
    val makeFormal = Formal;
  
    fun makeLocal () = Local { addr = ref 0, level = ref 0 };
       
    fun makeSelected (addr, base) =
      Selected { addr = addr, base = base };

    fun makeStruct (name, signat, access, location) = 
      Struct { name = name, signat = signat, access = access, declaredAt = location };
    
    val undefinedStruct = NoStruct;
    
    fun isUndefinedStruct NoStruct = true
    |   isUndefinedStruct _        = false;
    
    fun structName NoStruct            = ""
    |   structName (Struct {name,...}) = name;
    
    fun structAccess NoStruct              = raise Match
    |   structAccess (Struct {access,...}) = access;

    fun structDeclaredAt NoStruct              = raise Match
    |   structDeclaredAt (Struct {declaredAt,...}) = declaredAt;

    (* Return the signature. *)
    fun structSignat NoStruct = makeSignatures ("", inBasis) (* only if an error *)
    |   structSignat (Struct {signat,...}) = signat;
    
    (* Global structure *)
    fun makeGlobalStruct (name, signat, code, location) =
		makeStruct (name,
            makeCopy("", signat, sigMinTypes signat, sigMaxTypes signat, location), makeGlobal code, location);
 
    (* This is used for the top-level name space so must be mutable. *)
    fun makeEmptyGlobal name =
		makeStruct (name, makeSignatures ("", inBasis), makeGlobal CodeZero, inBasis);
     
    (* Local structure. *)
    fun makeLocalStruct (name, signat, location) = 
		makeStruct (name, signat, makeLocal (), location);
     
    (* Structure in a local structure or a functor argument. *)
    fun makeSelectedStruct (selected, base) = 
    (
      case structAccess selected of 
		Formal sel =>
		   makeStruct 
		     (structName selected,
		      structSignat selected,
		      makeSelected (sel, base),
              structDeclaredAt selected)
	      | _          => selected
    );
  
    fun makeFormalStruct (name, signat, addr, location) =
      makeStruct (name, signat, makeFormal addr, location);
     
  (* Values. *)
  
  fun makeGlobalV (name, typeOf, code, location) : values =
    Value{ name = name, typeOf = typeOf, access = Global code, class = SimpleValue, declaredAt = location };
  
  fun makeLocalV (name, typeOf, addr, level, location) : values =
    Value{ name = name, typeOf = typeOf, access = Local {addr = addr, level = level},
			class = SimpleValue, declaredAt = location };
  
  fun makeFormalV (name, typeOf, addr, location) : values =
    Value{ name = name, typeOf = typeOf, access = Formal addr, class = SimpleValue, declaredAt = location };

  fun makeFormalEx (name, typeOf, addr, location) : values =
    Value{ name = name, typeOf = typeOf, access = Formal addr, class = Exception, declaredAt = location };
  
  fun makeOverloaded (name, typeOf, operation) : values =
    Value{ name = name, typeOf = typeOf, access = Overloaded operation, class = SimpleValue, declaredAt = inBasis};

  val undefinedValue    = makeGlobalV("<undefined>", BadType, CodeZero, inBasis);

  fun isUndefinedValue(Value{name = "<undefined>", ...}) = true | isUndefinedValue _ = false

  fun valName (Value{name, ...}) = name
  
  fun valTypeOf (Value{typeOf, ...}) = typeOf

  fun isConstructor (Value{class=Constructor _, ...}) = true
    | isConstructor (Value{class=Exception, ...})     = true
    | isConstructor _                                  = false;

  fun isValueConstructor (Value{class=Constructor _, ...}) = true
    | isValueConstructor _                                 = false;


  (* Functor value. *)
  abstype functors =
     NoFunctor 
   | Functor of
       {
          name:   string,
          arg:    structVals,
          result: signatures,
          access: valAccess,
          declaredAt: location
       }
  with
    fun makeFunctor (name, arg, result, access, location) = 
      Functor 
	{
	  name = name,
	  arg = arg,
	  result = result,
	  access = access,
      declaredAt = location
	};
    
    val undefinedFunctor = NoFunctor;
    
    fun isUndefinedFunctor NoFunctor = true
    |   isUndefinedFunctor _         = false;
    
    fun functorName NoFunctor            = raise Match
    |   functorName (Functor {name,...}) = name;
    
    fun functorArg NoFunctor           = raise Match
    |   functorArg (Functor {arg,...}) = arg;
    
    fun functorResult NoFunctor              = raise Match
    |   functorResult (Functor {result,...}) = result;
    
    fun functorAccess NoFunctor              = raise Match
    |   functorAccess (Functor {access,...}) = access;
    
    fun functorDeclaredAt NoFunctor                  = raise Match
    |   functorDeclaredAt (Functor {declaredAt,...}) = declaredAt;
  end; (* functors abstype *)
  
  val valueVar:      values      tag = tag();
  val typeConstrVar: typeConstrs tag = tag();
  val fixVar:        fixStatus   tag = tag();
  val structVar:     structVals  tag = tag();
  val signatureVar:  signatures  tag = tag();
  val functorVar:    functors    tag = tag();

  fun makeLook (t:'a tag) table =
    fn n => univLookup (table, t, n);
  
  fun makeEnter (t:'a tag) table =
    fn (n, v) => univEnter (table, t, n, v);

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

  (* This creates functions for entering and looking up names. *)
  fun makeEnv s =
  let
    val tab = sigTab s;
  in
    Env { lookupVal    = makeLook  valueVar      tab,
          lookupType   = makeLook  typeConstrVar tab,
          lookupFix    = makeLook  fixVar        tab,
          lookupStruct = makeLook  structVar     tab,
          lookupSig    = makeLook  signatureVar  tab,
          lookupFunct  = makeLook  functorVar    tab,
          enterVal     = makeEnter valueVar      tab,
          enterType    = makeEnter typeConstrVar tab,
          enterFix     = makeEnter fixVar        tab,
          enterStruct  = makeEnter structVar     tab,
          enterSig     = makeEnter signatureVar  tab,
          enterFunct   = makeEnter functorVar    tab
        }
  end; 
end (* STRUCTVALS *);
