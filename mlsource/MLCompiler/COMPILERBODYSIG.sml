(*
    Copyright David C. J. Matthews 2015

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
signature COMPILERBODYSIG =
 
sig
    type values;
    type typeConstrSet;
    type fixStatus;
    type structVals;
    type signatures;
    type functors;
    type exportTree

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

    type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }

    (* The completed compiler.
       Returns the parse tree and, if successful, a function that executes the
       compiled code.  *)
    val compiler :
        nameSpace * (unit->char option) * Universal.universal list ->
        exportTree option * (unit ->
           { fixes: (string * fixStatus) list, values: (string * values) list,
             structures: (string * structVals) list, signatures: (string * signatures) list,
             functors: (string * functors) list, types: (string * typeConstrSet) list }) option

    structure Sharing:
    sig
        type values = values
        and  typeConstrSet = typeConstrSet
        and  fixStatus = fixStatus
        and  structVals = structVals
        and  signatures = signatures
        and  functors = functors
        and  exportTree = exportTree
    end

end;
