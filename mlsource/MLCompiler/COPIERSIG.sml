(*
    Copyright (c) 2009 David C.J. Matthews

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

signature COPIERSIG =
sig
    type signatures
    type typeConstrSet
    type structVals
    type values
    type typeId
    type types
    type univTable

    type tsvEnv = { enterType: string * typeConstrSet -> unit,
                  enterStruct: string * structVals  -> unit,
                  enterVal   : string * values      -> unit };
    val openSignature: signatures * tsvEnv * string -> unit
    val fullCopyDatatype: typeConstrSet * (int -> typeId) * string -> typeConstrSet
    val replaceMap: signatures * (int -> typeId) * int * typeId list * (int -> typeId) -> signatures

    val getNextRuntimeOffset : signatures -> int

    structure Sharing:
    sig
        type signatures     = signatures
        type typeConstrSet  = typeConstrSet
        type structVals     = structVals
        type values         = values
        type typeId         = typeId
        type types          = types
        type univTable      = univTable
    end
end;
