(*
    Title:      Source level debugger for Poly/ML
    Author:     David Matthews
    Copyright  (c)   David Matthews 2000, 2009

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

signature DEBUGGERSIG =
sig
    type types
	type values
	type machineWord
    type fixStatus
    type structVals
    type typeConstrs
    type signatures
    type functors
    type locationProp
    type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }

	datatype environEntry =
		EnvValue of string * types * locationProp list
	|	EnvException of string * types * locationProp list
	|	EnvVConstr of string * types * bool * int * locationProp list
	|	EnvStaticLevel

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

    (* The debugger function supplied to the compiler. *)
    type debugger = int * values * int * string * string * nameSpace -> unit
    val nullDebug: debugger

    val debuggerFunTag : debugger Universal.tag
    
    datatype debugReason =
        DebugEnter of machineWord * types
    |   DebugLeave of machineWord * types
    |   DebugException of exn
    |   DebugStep

    (* Functions inserted into the compiled code. *)
	val debugFunction:
		debugger * debugReason * string * location -> environEntry list -> machineWord list -> unit

    structure Sharing:
    sig
        type types          = types
    	type values         = values
    	type machineWord    = machineWord
        type fixStatus      = fixStatus
        type structVals     = structVals
        type typeConstrs    = typeConstrs
        type signatures     = signatures
        type functors       = functors
        type locationProp   = locationProp
        type environEntry   = environEntry
    end
end;
