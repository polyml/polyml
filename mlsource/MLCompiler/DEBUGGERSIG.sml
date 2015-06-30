(*
    Title:      Source level debugger for Poly/ML
    Author:     David Matthews
    Copyright  (c)   David Matthews 2000, 2009, 2014-15

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

signature DEBUGGERSIG =
sig
    type types
    type values
    type machineWord
    type fixStatus
    type structVals
    type typeConstrSet
    type signatures
    type functors
    type locationProp
    type typeId
    type level
    type lexan
    type codeBinding
    type codetree
    type typeVarMap
    type breakPoint
    type environEntry
 
    type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }

    val envTypeId: typeId -> environEntry

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
      }

    (* Functions to make debug entries for various values, types etc. *)
    type debuggerStatus
    val initialDebuggerStatus: debuggerStatus

    val makeValDebugEntries:
        values list * debuggerStatus * level * lexan * (int -> int) * typeVarMap -> codeBinding list * debuggerStatus
    val makeTypeConstrDebugEntries:  typeConstrSet list * debuggerStatus * level * lexan * (int -> int) -> codeBinding list * debuggerStatus
    val makeStructDebugEntries: structVals list * debuggerStatus * level * lexan * (int->int) ->
        codeBinding list * debuggerStatus
    val makeTypeIdDebugEntries: typeId list * debuggerStatus * level * lexan * (int->int) -> codeBinding list * debuggerStatus
    
    val updateDebugLocation: debuggerStatus * location * lexan -> codeBinding list * debuggerStatus

    (* Create a local break point and check the global and local break points. *)
    val breakPointCode: breakPoint option ref * location * level * lexan * (int->int) -> codeBinding list
    (* A function to set the break point.  Included in the exported tree. *)
    val setBreakPoint: breakPoint -> (location -> unit) option -> unit

    (* Add debugging calls on entry and exit to a function. *)
    val wrapFunctionInDebug:
        codetree * string * types * location * debuggerStatus * level * lexan * (int -> int) -> codetree
    (* Create a debug entry for the start of the function. *)
    val debugFunctionEntryCode:
        string * codetree * types * location * debuggerStatus * level * lexan * (int -> int) -> codeBinding list * debuggerStatus

    (* Exported functions that appear in PolyML.DebuggerInterface. *)
    type debugState (* The run-time state. *)
    val debugNameSpace: debugState -> nameSpace
    val debugFunction: debugState -> string option
    val debugFunctionArg: debugState -> values option
    val debugFunctionResult: debugState -> values option
    val debugLocation: debugState -> location

    structure Sharing:
    sig
        type types          = types
        type values         = values
        type machineWord    = machineWord
        type fixStatus      = fixStatus
        type structVals     = structVals
        type typeConstrSet  = typeConstrSet
        type signatures     = signatures
        type functors       = functors
        type locationProp   = locationProp
        type environEntry   = environEntry
        type typeId         = typeId
        type level          = level
        type lexan          = lexan
        type codeBinding    = codeBinding
        type codetree       = codetree
        type typeVarMap     = typeVarMap
        type breakPoint     = breakPoint
        type debuggerStatus = debuggerStatus
    end
end;
