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
    type locationProp
    type typeId
    type typeVarForm

    val overloadType:      values * bool -> types

    val chooseConstrRepr : (string*types) list -> codetree list

    (* Construction functions. *)
    val mkGvar:        string * types * codetree * locationProp list -> values
    val mkVar:         string * types * locationProp list -> values
    val mkSelectedVar: values * structVals * locationProp list -> values
    val mkGconstr:     string * types * codetree * bool * int * locationProp list -> values
    val mkGex:         string * types * codetree * locationProp list -> values
    val mkEx:          string * types * locationProp list -> values
  
    val mkSelectedType: typeConstrs * string * structVals option * locationProp list -> typeConstrs

    (* Standard values *)
    val listType: typeConstrs
    val nilConstructor:  values;
    val consConstructor: values;
    val optionType: typeConstrs
    val buildBasisDatatype:
        string * typeVarForm list * bool * (typeConstrs -> values list) -> typeConstrs

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

    val nullEnvironment : nameSpace

    val codeStruct:     structVals * int -> codetree
    val codeAccess:     valAccess  * int -> codetree
    val mkExIden:       types * int -> codetree
    val codeVal:        values * int * types * lexan * location -> codetree
    val codeExFunction: values * int * types * lexan * location -> codetree
    val applyFunction:  values * codetree * int * types * lexan * location -> codetree
    val getOverloadInstance: string * types * bool -> codetree*string
    val isTheSameException: values * values -> bool
    val raiseBind:      codetree
    val raiseMatch:     codetree
    val makeGuard:      values * codetree * int -> codetree 
    val makeInverse:    values * codetree * int -> codetree
                    
    val lookupStructure:  string * {lookupStruct: string -> structVals option} * 
                            string * (string -> unit) -> structVals
                                           
    val lookupStructureAsSignature:
        (string -> structVals option) *  string * (string -> unit) -> structVals
                                           
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

    val idFromStructure: structVals -> typeId -> typeId option

    val codeLocation: location -> codetree

    (* Types that can be shared. *)
    structure Sharing:
    sig
        type machineWord    = machineWord
        type lexan          = lexan
        type codetree       = codetree
        type types          = types
        type values         = values
        type structVals     = structVals
        type functors       = functors
        type valAccess      = valAccess
        type typeConstrs    = typeConstrs
        type signatures     = signatures
        type fixStatus      = fixStatus
        type univTable      = univTable
        type pretty         = pretty
        type locationProp   = locationProp
        type typeId         = typeId
        type typeVarForm    = typeVarForm
    end
end;
