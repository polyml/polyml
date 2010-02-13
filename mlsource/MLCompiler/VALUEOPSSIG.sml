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

(*****************************************************************************)
(*                  VALUEOPS exports signature                               *)
(*****************************************************************************)
signature VALUEOPSSIG =
sig
    type lexan
    type codetree
    type types
    type values
    type structVals
    type functors
    type valAccess
    type typeConstrs
    type typeConstrSet
    type signatures
    type fixStatus
    type univTable
    type pretty
    type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }
    type locationProp
    type typeId
    type typeVarForm
    type typeVarMap

    (* Construction functions. *)
    val mkGvar:        string * types * codetree * locationProp list -> values
    val mkValVar:      string * types * locationProp list -> values
    val mkPattVar:     string * types * locationProp list -> values
    val mkSelectedVar: values * structVals * locationProp list -> values
    val mkGconstr:     string * types * codetree * bool * int * locationProp list -> values
    val mkGex:         string * types * codetree * locationProp list -> values
    val mkEx:          string * types * locationProp list -> values
  
    val mkSelectedType: typeConstrSet * string * structVals option * locationProp list -> typeConstrSet

    type nameSpace =
    { 
        lookupVal:    string -> values option,
        lookupType:   string -> typeConstrSet option,
        lookupFix:    string -> fixStatus option,
        lookupStruct: string -> structVals option,
        lookupSig:    string -> signatures option,
        lookupFunct:  string -> functors option,

        enterVal:     string * values      -> unit,
        enterType:    string * typeConstrSet -> unit,
        enterFix:     string * fixStatus   -> unit,
        enterStruct:  string * structVals  -> unit,
        enterSig:     string * signatures  -> unit,
        enterFunct:   string * functors    -> unit,

        allVal:       unit -> (string*values) list,
        allType:      unit -> (string*typeConstrSet) list,
        allFix:       unit -> (string*fixStatus) list,
        allStruct:    unit -> (string*structVals) list,
        allSig:       unit -> (string*signatures) list,
        allFunct:     unit -> (string*functors) list
    };

    type printTypeEnv =
        { lookupType: string -> (typeConstrSet * (int->typeId) option) option,
          lookupStruct: string -> (structVals * (int->typeId) option) option}

    (* Print values. *)
    val displayFixStatus:  fixStatus -> pretty
    val displaySignatures: signatures * int * printTypeEnv -> pretty
    val displayStructures: structVals * int * printTypeEnv -> pretty
    val displayFunctors:   functors   * int * printTypeEnv -> pretty
    val displayValues: values * int * printTypeEnv -> pretty
    val printValues: values * int * printTypeEnv -> pretty

    val nullEnvironment : nameSpace

    val codeStruct:     structVals * int -> codetree
    val codeAccess:     valAccess  * int -> codetree
    val codeVal:
        values * int * typeVarMap * {value: types, equality: bool, printity: bool} list * lexan * location -> codetree
    val codeExFunction: values * int * typeVarMap * types list * lexan * location -> codetree
    val applyFunction:
        values * codetree * int * typeVarMap * {value: types, equality: bool, printity: bool} list *
            lexan * location -> codetree
    val getOverloadInstance: string * types * bool -> codetree*string
    val isTheSameException: values * values -> bool
    val makeGuard:      values * types list * codetree * int * typeVarMap -> codetree 
    val makeInverse:    values * types list * codetree * int * typeVarMap -> codetree
                    
    val lookupStructure:  string * {lookupStruct: string -> structVals option} * 
                            string * (string -> unit) -> structVals
                                           
    val lookupStructureAsSignature:
        (string -> structVals option) *  string * (string -> unit) -> structVals
                                           
    val lookupValue:   string * {lookupVal: string -> values option, lookupStruct: string -> structVals option} * 
                          string * (string -> unit) -> values
                                
    val lookupTyp:   {lookupType: string -> typeConstrSet option,
                      lookupStruct: string -> structVals option} * 
                        string * (string -> unit) -> typeConstrSet

    val codeLocation: location -> codetree

    val getPolymorphism: values * types * typeVarMap -> {value: types, equality: bool, printity: bool} list

    (* Types that can be shared. *)
    structure Sharing:
    sig
        type lexan          = lexan
        type codetree       = codetree
        type types          = types
        type values         = values
        type structVals     = structVals
        type functors       = functors
        type valAccess      = valAccess
        type typeConstrs    = typeConstrs
        type typeConstrSet  = typeConstrSet
        type signatures     = signatures
        type fixStatus      = fixStatus
        type univTable      = univTable
        type pretty         = pretty
        type locationProp   = locationProp
        type typeId         = typeId
        type typeVarForm    = typeVarForm
        type typeVarMap     = typeVarMap
    end
end;
