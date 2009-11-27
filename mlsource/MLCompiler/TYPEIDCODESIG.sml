(*
    Copyright (c) 2009 David C. J. Matthews

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

signature TYPEIDCODESIG =
sig
    type typeId
    type codetree
    type types
    type typeConstrs
    type typeVarForm
    type typeVarMap

    val codeId: typeId * int -> codetree
    val codeGenerativeId: typeId * bool * int -> codetree
    
    val createDatatypeFunctions: typeConstrs list * bool list * (unit->int) * int -> codetree list
    
    val codeForUniqueId: unit->codetree

    (* Generate a function of the form t*int->pretty for values of type t. *)
    val printerForType: types * int * typeVarMap -> codetree
    (* Generate a function of the form (t,t) -> bool. *)
    val equalityForType: types * int * typeVarMap -> codetree

    val applyToInstance: (types list * typeVarForm list) * int * typeVarMap * (int -> codetree) -> codetree

    val extendTypeVarMap:
        (typeVarForm * (int -> codetree)) list * typeVarMap -> typeVarMap

    (* Default map. *)
    val defaultTypeVarMap: typeVarMap
    
    val mapTypeVars: typeVarMap -> typeVarForm -> types option
    
    val defaultTypeCode: codetree

    structure Sharing:
    sig
        type typeId     = typeId
        type codetree   = codetree
        type types      = types
        type typeConstrs= typeConstrs
        type typeVarForm=typeVarForm
        type typeVarMap = typeVarMap
    end
end;
