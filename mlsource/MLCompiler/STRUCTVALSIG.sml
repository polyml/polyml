(*
	Copyright (c) 2000
		Cambridge University Technical Services Limited
        
    Modified David C. J. Matthews 2009.

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

(******************************************************************************)
(*                  STRUCTVALS export signature                              *)
(*****************************************************************************)

signature STRUCTVALSIG =
sig
    type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }
    val inBasis: location

    (* Structures *)
    type signatures;
    type codetree;

    (* Types *)
  
    datatype 'a possRef = FrozenRef of 'a | VariableRef of 'a ref
    val pling: 'a possRef -> 'a
    val updatePR: 'a possRef * 'a -> unit
  
    (* Standard type constructors. *)
  
    type typeVarForm;
    type typeConstrs;
    type uniqueId

    datatype typeId =
        Unset           of { isEqtype: bool, isDatatype: bool }
    |   Free            of { access: valAccess ref, uid: uniqueId, allowUpdate: bool }
    |   Bound           of { access: valAccess, offset: int, eqType: bool possRef }
    |   Flexible        of typeId ref
    |   TypeFunction    of typeVarForm list * types

        (* A type is the union of these different cases. *)
    and types = 
        TypeVar of typeVarForm
    
    |   TypeConstruction of (* typeConstructionForm *)
        {
            name:  string,
            value: typeConstrs possRef,
            args:  types list,
            locations: locationProp list
        }

    |   FunctionType of (* functionTypeForm *)
        { 
            arg:    types,
            result: types
        }
  
    |   LabelledType  of (* labelledRecForm *)
        { 
            recList: { name: string, typeof: types } list,
            frozen: bool,
	        genericInstance: typeVarForm list
        }

    |   OverloadSet	  of (* overloadSetForm *)
  	    {
		    typeset: typeConstrs list
	    }

    |   BadType
  
    |   EmptyType

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

    (* Values. *)
    and typeDependent =
        Print
    |   GetPretty
    |   MakeString
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
            locations: locationProp list
        }

    (* Classes of values. *)
    and valueClass =
  	    SimpleValue
	|   Exception
	|   Constructor of { nullary: bool, ofConstrs: int }

    (* Location properties.  A value may have some or all of these. *)
    and locationProp =
        DeclaredAt of location
    |   OpenedAt of location
    |   StructureAt of location

    (* type identifiers. *)
    val isFreeId:     typeId -> bool;
    val isBoundId:    typeId -> bool;
    val isVariableId: typeId -> bool;
    val isTypeFunction: typeId -> bool;
    val isEquality:   typeId -> bool;
    val offsetId:     typeId -> int;
    val idAccess:     typeId -> valAccess;
    val sameTypeId:   typeId * typeId -> bool;
    val linkFlexibleTypeIds: typeId * typeId -> unit;
    val makeTypeIdBound: typeId * typeId -> unit;
    val setEquality:  typeId -> unit
    val setTypeAccess:typeId * valAccess -> unit
    val removeAbstypeEquality:  typeId -> unit

    val makeFreeId:     valAccess * bool -> typeId;
    val makeFreeIdEqUpdate:     valAccess * bool -> typeId;
    val makeVariableId: bool -> typeId;
    val makeBoundId:    valAccess * int*bool -> typeId;
    val makeBoundIdWithEqUpdate: valAccess * int*bool -> typeId;
    
    (* Types *)
    val badType:   types;
    val emptyType: types;

    val isBad:     types -> bool;
    val isEmpty:   types -> bool;

    val tcName:            typeConstrs -> string;
    val tcArity:           typeConstrs -> int;
    val tcTypeVars:        typeConstrs -> typeVarForm list;
    val tcEquivalent:      typeConstrs -> types;
    val tcSetEquivalent:   typeConstrs * typeVarForm list * types -> unit;
    val tcConstructors:    typeConstrs -> values list;
    val tcSetConstructors: typeConstrs * values list -> unit;
    val tcEquality:        typeConstrs -> bool;
    val tcSetEquality:     typeConstrs * bool -> unit;
    val tcIdentifier:      typeConstrs -> typeId;
    val tcLetDepth:        typeConstrs -> int;
    val tcLocations:       typeConstrs -> locationProp list
    val tcIsAbbreviation:  typeConstrs -> bool

    val makeDatatypeConstr:
        string * typeVarForm list * typeId * int * locationProp list -> typeConstrs;
    val makeFrozenTypeConstrs:
        string * typeVarForm list * typeId * int * locationProp list -> typeConstrs;
    val makeTypeAbbreviation:
        string * typeVarForm list * types * locationProp list -> typeConstrs;

    val tvLevel:        typeVarForm -> int;
    val tvEquality:     typeVarForm -> bool;
    val tvNonUnifiable: typeVarForm -> bool;
    val tvValue:        typeVarForm -> types;
    val tvSetValue:     typeVarForm * types -> unit;

    val sameTv: typeVarForm * typeVarForm -> bool;

    val makeTv: types * int * bool * bool * bool -> typeVarForm;

    val generalisable: int;

    val boolType:   typeConstrs;
    val intType:    typeConstrs;
    val charType:   typeConstrs; (* added 22/8/96 SPF *)
    val stringType: typeConstrs;
    val wordType:	typeConstrs;
    val realType:   typeConstrs;
    val refType:    typeConstrs;
    val arrayType:  typeConstrs;
    val array2Type: typeConstrs;
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
    val structLocations:   structVals -> locationProp list;

    val makeEmptyGlobal:   string -> structVals;
    val makeGlobalStruct:  string * signatures * codetree * location -> structVals;
    val makeLocalStruct:   string * signatures * locationProp list -> structVals;
    val makeFormalStruct:  string * signatures * int * locationProp list -> structVals;

    val makeSelectedStruct: structVals * structVals * locationProp list -> structVals;

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
      val sigTypeIdMap:  signatures -> (int -> typeId);

      val makeSignatureTable: unit -> univTable;
      val makeSignature: string * univTable * int * int * location * (int -> typeId) -> signatures;

      (* Values. *)
      val valName: values -> string
      val valTypeOf: values -> types
      val undefinedValue: values;
      val isUndefinedValue: values -> bool;
      val isConstructor: values -> bool;
      val isValueConstructor: values -> bool
  
      val makeGlobalV: string * types * codetree * locationProp list -> values;
      val makeLocalV: string * types * int ref * int ref * locationProp list -> values;
      val makeFormalV: string * types * int * locationProp list -> values;  
      val makeFormalEx: string * types * int * locationProp list -> values;  
      val makeOverloaded: string * types * typeDependent -> values;
      val makeValueConstr: string * types * bool * int * valAccess * locationProp list -> values;
  
    (* Infix status *)

    datatype fixStatus = 
        Infix of int
    |   InfixR of int
    |   Nonfix;

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

    val makeEnv: univTable -> env;

    val valueVar:      values      Universal.tag;
    val typeConstrVar: typeConstrs Universal.tag;
    val fixVar:        fixStatus   Universal.tag;
    val structVar:     structVals  Universal.tag;
    val signatureVar:  signatures  Universal.tag;
    val functorVar:    functors    Universal.tag;

    (* Temporary addition *)
    val defaultEqAndPrintCode: unit -> valAccess

    (* Types that can be shared. *)
    structure Sharing:
    sig
        type codetree   = codetree
        and  signatures = signatures
        and  types      = types
        and  values     = values
        and  typeId     = typeId
        and  structVals = structVals
        and  valAccess  = valAccess
        and  typeConstrs= typeConstrs
        and  env        = env
        and  univTable  = univTable
        and  fixStatus  = fixStatus
        and  functors   = functors
        and  locationProp = locationProp
        and  typeVarForm = typeVarForm
    end
end;

