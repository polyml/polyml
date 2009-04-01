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
(*                  UNIVERSALTABLE                                           *)
(*****************************************************************************)
structure UNIVERSALTABLE :
sig
  type 'a tag = 'a Universal.tag;
  type univTable;
  
  val makeUnivTable: unit -> univTable;
  val univEnter:     univTable * 'a tag * string * 'a -> unit;
  val univLookup:    univTable * 'a tag * string -> 'a option;
  
  (* Freeze a mutable table so it is immutable. *)
  val univFreeze:       univTable -> univTable
end;

) :> STRUCTVALSIG where type codetree = CODETREE.codetree and type univTable = UNIVERSALTABLE.univTable 
=  

(*****************************************************************************)
(*                  STRUCTVALS functor body                                  *)
(*****************************************************************************)
struct
    open CODETREE;
  
    open Misc;
    open Universal;
    open UNIVERSALTABLE;
  
    (* Location for declarations. *)
    type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }

    (* The idea of this is reduce the number of mutable objects. *)
    datatype 'a possRef = FrozenRef of 'a | VariableRef of 'a ref
    fun pling(FrozenRef x) = x | pling(VariableRef(ref x)) = x
    fun updatePR(VariableRef r, x) = r := x | updatePR(FrozenRef _, _) = raise Fail "Assignment to frozen ref"
   
  (* typeIds are used to distinguish between concrete types.  Two
     types that share will have the same identifier.  If the identifiers are
     different they are different types.
     There are four classes of type identifier. Flexible types occur in signatures
     and can be assigned to other variables or constants by sharing constraints
     or by signature matching. Free constants arise from datatypes, abstypes or
     opaque signature macthing. Bound constants occur in the arguments to
     functors or types constructed inside a functor (and therefore generated
     when the functor is applied).  Type functions arise from type bindings
     (type abbreviations) or from "where type" definitions in signatures.
     The type identifier also contains the equality attribute.
     In ML97 only types have these identifiers.  In ML90 these were also
     needed for structures. *)

    datatype typeId =
        Unset           (* A flexible type id that hasn't yet been set. *)
    |   UnsetEquality
    |   Free            of uniqueId * bool
    |   Bound           of int * bool possRef
    |   Flexible        of typeId ref
    |   TypeFunction    of types list * types

    (* A type is the union of these different cases. *)
    and types = 
        TypeVar          of typeVarForm
    |   TypeConstruction of typeConstructionForm
    |   FunctionType     of functionTypeForm
    |   LabelledType     of labelledRecForm
    |   OverloadSet		 of overloadSetForm
    |   BadType
    |   EmptyType

    and typeConstrs = 
        TypeConstrs of
        {
            name:       string,
            arity:      int,
            typeVars:   types list,
		    constrs:    values list possRef, (* List of value constructors. *)
            identifier: typeId,
		    letDepth:	int, (* Needed to check for local datatypes. *)
            locations:  locationProp list (* Location of declaration *)
        }

    (* Access to a value, structure or functor. *)
    and valAccess =
  	    Global   of codetree
    |   Local    of { addr: int ref, level: int ref }
    |   Selected of { addr: int,     base:  structVals }
    |   Formal   of int
    |   Overloaded of typeDependent (* Values only. *)

    (* Structures. *)
    and structVals = 
        NoStruct
    |   Struct of
        {
            name:   string,
            signat: signatures,
            access: valAccess,
            locations: locationProp list
        }

    (* Signatures *)
    and signatures =
        Signatures of
        { 
            name:       string,
            tab:        univTable,
            minTypes:   int,
            maxTypes:   int,
            declaredAt: location
        }

    (* Values. *)
    (* The overloaded functions divide up into basically two groups: Those =, 
       <>, print and makestring  which are infinitely overloaded and those 
       *, + etc  which are overloaded on a limited range of types. *)  
    and typeDependent =
        Print
    |   PrintSpace
    |   MakeString
    |   MakeStringSpace
    |   InstallPP
    |   AddPretty
    |   Equal
    |   NotEqual
    |   AddOverload
    |   TypeDep

    and values =
  	    Value of
        {
		    name: string,
		    typeOf: types,
		    access: valAccess,
		    class: valueClass,
            locations: locationProp list (* Location of declaration *)
        }

    (* Classes of values. *)
    and valueClass =
  	    SimpleValue
	|   Exception
	|   Constructor of { nullary: bool }

    and locationProp =
        DeclaredAt of location
    |   OpenedAt of location
    |   StructureAt of location
  
    withtype uniqueId = bool ref
        (* We use a ref here both because we can then set equality if we
           need but also because it allows us to create a unique Id. *)
    and typeConstructionForm = 
    {
        name:  string,
        value: typeConstrs possRef,
        args:  types list,
        locations: locationProp list
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

    (* Identifiers *)
    (* Variable stamps: can be set to other stamps whether variable or free. *)
    val unsetId = Unset;
    val unsetEq = UnsetEquality

    (* REF HOTSPOT - 400 *)
    fun makeFreeId eq = Free (ref eq, false);
    (* At the moment the only reason for distinguishing makeFreeId and makeFreeIdEqUpdate
       is that it allows us to check that we're actually permitting update when needed. *)
    fun makeFreeIdEqUpdate eq = Free(ref eq, true);

    fun makeBoundId (n, eq) = Bound(n, FrozenRef eq);

    (* Within the body of a functor we make bound stamps but may need to
       set the equality attribute. *)
    fun makeBoundIdWithEqUpdate (n, eq) = Bound(n, VariableRef(ref eq))

    (* REF HOTSPOT - 260 *)
    fun makeVariableId true  = Flexible (ref UnsetEquality)
    |   makeVariableId false = Flexible (ref Unset);
    
    fun makeTypeFunction(args, result) = TypeFunction(args, result)

    (* If it is a constant or an unset variable return it,
       otherwise return the value the variable is bound to. *)
    fun realId(id as Flexible (ref Unset)) = id
    |   realId(id as Flexible (ref UnsetEquality)) = id
    |   realId(Flexible (ref v)) = realId v
    |   realId id = id
		    
    (* Unset variable. *)
    fun isUnsetId Unset = true | isUnsetId UnsetEquality = true | isUnsetId _ = false;

    fun isVariableId x = case (realId x) of Flexible _ => true | _ => false;
    fun isFreeId     x = case (realId x) of Free     _ => true | _ => false;
    fun isBoundId    x = case (realId x) of Bound    _ => true | _ => false;
    fun isTypeFunction x = case (realId x) of TypeFunction _ => true | _ => false;

    (* Find the number - assuming it is bound. *)
    fun offsetId x = 
        case (realId x) of 
	        Bound (i, _) => i
        |   _       => raise InternalError "offsetId: not a Bound";

    (* Are two type constructors the same? *)
    fun sameTypeId (cons1, cons2) =
        case (realId cons1, realId cons2) of
            (Flexible a, Flexible b)    => a = b
        |   (Free a,     Free b)        => a = b
        |   (Bound a,    Bound b)       => a = b
        |   _                           => false (* Includes type functions. *)
  
    (* Link two flexible type identifiers together as a result of a sharing
       constraint.  If either is an equality type the result is an equality type.
       If the two types are already linked it does nothing and doesn't report an error.  *)
    fun linkFlexibleTypeIds (x, y) =
    let
	    val x1 = realId x;
	    val y1 = realId y;
    in
        case (x1, y1) of
            (Flexible (p as ref Unset), Flexible(ref UnsetEquality)) => p := y1
        |   (Flexible (ref UnsetEquality), Flexible (p as ref Unset)) => p := x1
        |   (Flexible x2, Flexible y2) => if x2 = y2 then () else x2 := y1 (* Either *)
        |   _ => raise InternalError "linkTypeIds: non-variable ids"
    end

    (* Set the typeId to be a type function.  This can only happen as a result of
       a "where type" constraint. *)
    fun setAsTypeFunction(typeId, args, result) =
        case realId typeId of
            Flexible p => p := TypeFunction(args, result)
        |   _ => raise InternalError "setAsTypeFunction: setting non-flexible type"

    (* Set a flexible type id to be a new bound type id.  This is part of the
       last phase of constructing a signature so that one bound is used
       for each distinct flexible type id. *)
    fun makeTypeIdBound(t, id) =
        case realId t of
            Flexible p => p := id
        |   _ => raise InternalError "makeTypeIdBound: Not originally flexible"

    fun isEquality Unset = false
    |   isEquality UnsetEquality = true
    |   isEquality (Free(ref eq, _)) = eq
    |   isEquality (Bound(_, eq)) = pling eq
    |   isEquality (Flexible(ref v)) = isEquality v
    |   isEquality (TypeFunction _) = false (* Requires analysis of the type function. *)

    (* Set the equality property.  This may be either for flexible stamps as the
       result of sharing with an eqtype or, for either flexible or free datatypes,
       if the datatype admits equality because all its constructors do.*)
    fun setEquality(Flexible(r as ref Unset)) = r := UnsetEquality
    |   setEquality(Flexible(ref UnsetEquality)) = ()
    |   setEquality(Flexible(r as ref t)) = setEquality t
    |   setEquality(Free(id, true)) = id := true (* For datatypes *)
    |   setEquality(Bound(_, VariableRef id)) = id := true (* For datatypes *)
    |   setEquality _ = raise InternalError "setEquality: can't set equality attribute"

    (* The only circumstances in which we remove the equality attribute is for
       abstypes.  Equality may be permitted in the "with .. end" section but
       not outside. *)
    fun removeAbstypeEquality(Free(id, true)) = id := false
    |   removeAbstypeEquality _ =
            raise InternalError "removeAbsTypeEquality: not Free"

    (* Signatures: Used for both signatures of local structures and for global structures 
       (name spaces). Strictly signatures do not contain fix-status functors
       or signatures but as we use these structures for top-level name-spaces
       we have to have tables for these. *)
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
    
  (* Types. *)

  (* Level at which type is generalisable. *)

  val generalisable = 9999; 
    
    
  (* Destructors, constructors and predicates for types *)
  val emptyType            = EmptyType;
  val badType              = BadType;

  fun isEmpty             EmptyType           = true | isEmpty            _ = false;
  fun isBad               BadType             = true | isBad              _ = false;
  
  fun makeValueConstr (name, typeOf, nullary, access, locations) : values =
  	Value
    { 
      name    = name,
      typeOf  = typeOf,
	  access  = access,
      class   = Constructor { nullary = nullary },
      locations = locations
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
    fun tcIdentifier (TypeConstrs {identifier,...} : typeConstrs) = realId identifier;
    fun tcLocations  (TypeConstrs {locations, ...}) = locations

    fun tcConstructors (TypeConstrs {constrs,...} : typeConstrs) = pling constrs;

    (* Is this a type function?  N.B. It is possible, though unlikely, that it
       is a datatype as well i.e. has value constructors. *)
    fun tcIsAbbreviation (TypeConstrs {identifier,...}) = isTypeFunction identifier

    (* Equality and "equivalence" are now properties of the type id.  Retain these functions for the moment. *)
    (* This definition of tcEquivalent is a hack but it should simplify conversion. *)
    fun tcEquivalent tycons =
        case realId (tcIdentifier tycons) of
            TypeFunction(_, result) => result
        |   _ => EmptyType

    val tcEquality = isEquality o tcIdentifier;
    fun tcSetEquality(tc, true) = setEquality(tcIdentifier tc)
    |   tcSetEquality(tc, false) = removeAbstypeEquality(tcIdentifier tc);

    fun tcSetEquivalent(tycons, args, result) =
        setAsTypeFunction(tcIdentifier tycons, args, result)
	
    fun tcSetConstructors (TypeConstrs {constrs, name,...}, newConstrs) =
	    updatePR(constrs, newConstrs) handle Fail _ => raise Fail ("SetConstructors:" ^ name)
	
    fun tcLetDepth	(TypeConstrs {letDepth,...} : typeConstrs) = letDepth;

    (* Construct a datatype.  The value constructors can be added on later. *)
    fun makeDatatypeConstr (name, typeVars, uid, depth, locations) =
        TypeConstrs
        {
    		name       = name,
    		arity      = length typeVars,
    		typeVars   = typeVars,
            constrs    = VariableRef (ref []), (* REF HOTSPOT - 690 refs here. *)
    		identifier = uid,
    		letDepth   = depth,
            locations = locations
        };

    (* Construct a type abbreviation. *)
    fun makeTypeAbbreviation(name, typeVars, typeResult, locations) =
        TypeConstrs
        {
    		name       = name,
    		arity      = length typeVars,
    		typeVars   = typeVars,
            constrs    = FrozenRef [],
    		identifier = TypeFunction(typeVars, typeResult),
    		letDepth   = 0,
            locations = locations
        };

    (* Construct a type constructor that cannot have value constructors added.  This is
       primarily used in Initialise for base types such as "int". *)
    fun makeFrozenTypeConstrs (name, typeVars, uid, depth, locations) =
        TypeConstrs
        {
		    name       = name,
		    arity      = length typeVars,
		    typeVars   = typeVars,
		    constrs    = FrozenRef [],
		    identifier = uid,
		    letDepth   = depth,
            locations = locations
        };

    val inBasis =
        { file = "Standard Basis", startLine = 0, startPosition = 0, endLine = 0, endPosition = 0}

    fun baseType name eq =
        makeFrozenTypeConstrs (name, [], makeFreeId eq, 0, [DeclaredAt inBasis]);

    val boolType   = makeDatatypeConstr ("bool", [], makeFreeId true, 0, [DeclaredAt inBasis]);
    val intType    = baseType "int"    true;
    val charType   = baseType "char"   true;
    val stringType = baseType "string" true;
    val wordType   = baseType "word"   true;
    val realType   = baseType "real"   false; (* Not an eqtype in ML97. *)
    val exnType    = baseType "exn"    false;
    (* The unit type is equivalent to the empty record. *)
    val unitType   =
        makeTypeAbbreviation ("unit", [],
	       LabelledType {recList = [], frozen = true, genericInstance = []},
		   [DeclaredAt inBasis]);
   
    val listType =
        makeDatatypeConstr 
            ("list", [TypeVar (makeTv (EmptyType, generalisable, false, false, false))],
            makeFreeId true, 0, [DeclaredAt inBasis]);
            
    val refType =
        makeDatatypeConstr 
            ("ref",
            [TypeVar (makeTv (EmptyType, generalisable, false, false, false))],
            makeFreeId true, 0, [DeclaredAt inBasis]);
         
    val undefType = 
        makeFrozenTypeConstrs ("undefined", [], unsetId, 0, [DeclaredAt inBasis]);

    (* Infix status. *) 
 
    datatype fixStatus = 
        Infix of int
    |   InfixR of int
    |   Nonfix;
  
      
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

    fun makeStruct (name, signat, access, locations) = 
      Struct { name = name, signat = signat, access = access, locations = locations };
    
    val undefinedStruct = NoStruct;
    
    fun isUndefinedStruct NoStruct = true
    |   isUndefinedStruct _        = false;
    
    fun structName NoStruct            = ""
    |   structName (Struct {name,...}) = name;
    
    fun structAccess NoStruct              = raise Match
    |   structAccess (Struct {access,...}) = access;

    fun structLocations NoStruct              = raise Match
    |   structLocations (Struct {locations,...}) = locations;

    (* Return the signature. *)
    fun structSignat NoStruct = makeSignatures ("", inBasis) (* only if an error *)
    |   structSignat (Struct {signat,...}) = signat;
    
    (* Global structure *)
    fun makeGlobalStruct (name, signat, code, location) =
		makeStruct (name,
            makeCopy("", signat, sigMinTypes signat, sigMaxTypes signat, location), makeGlobal code,
            [DeclaredAt location]);
 
    (* This is used for the top-level name space so must be mutable. *)
    fun makeEmptyGlobal name =
		makeStruct (name, makeSignatures ("", inBasis), makeGlobal CodeZero, [DeclaredAt inBasis]);
     
    (* Local structure. *)
    fun makeLocalStruct (name, signat, location) = 
		makeStruct (name, signat, makeLocal (), location);
     
    (* Structure in a local structure or a functor argument. *)
    fun makeSelectedStruct (selected, base, openLocs) = 
    (
      case structAccess selected of 
		Formal sel =>
		   makeStruct(structName selected, structSignat selected, makeSelected (sel, base),
                      openLocs @ structLocations selected)
      | Global code => (* Need to add the locations. *)
		   makeStruct(structName selected, structSignat selected, Global code,
                      openLocs @ structLocations selected)
	  | _          => selected
    );
  
    fun makeFormalStruct (name, signat, addr, location) =
      makeStruct (name, signat, makeFormal addr, location);
     
  (* Values. *)
  
  fun makeGlobalV (name, typeOf, code, locations) : values =
    Value{ name = name, typeOf = typeOf, access = Global code, class = SimpleValue, locations = locations };
  
  fun makeLocalV (name, typeOf, addr, level, locations) : values =
    Value{ name = name, typeOf = typeOf, access = Local {addr = addr, level = level},
			class = SimpleValue, locations = locations };
  
  fun makeFormalV (name, typeOf, addr, locations) : values =
    Value{ name = name, typeOf = typeOf, access = Formal addr, class = SimpleValue, locations = locations };

  fun makeFormalEx (name, typeOf, addr, locations) : values =
    Value{ name = name, typeOf = typeOf, access = Formal addr, class = Exception, locations = locations };
  
  fun makeOverloaded (name, typeOf, operation) : values =
    Value{ name = name, typeOf = typeOf, access = Overloaded operation, class = SimpleValue,
           locations = [DeclaredAt inBasis]};

  val undefinedValue    = makeGlobalV("<undefined>", BadType, CodeZero, [DeclaredAt inBasis]);

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
