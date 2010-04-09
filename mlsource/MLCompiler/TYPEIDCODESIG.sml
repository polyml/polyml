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
    type typeConstrSet
    type typeVarForm
    type typeVarMap

    val codeId: typeId * int -> codetree
    val codeGenerativeId: typeId * bool * (int->int) * int -> codetree
    
    val createDatatypeFunctions:
         {typeConstr: typeConstrSet, eqStatus: bool, boxedCode: codetree, sizeCode: codetree } list *
            (int->int) * int * typeVarMap -> codetree list
    
    val codeForUniqueId: unit->codetree

    (* Generate a function of the form t*int->pretty for values of type t. *)
    val printerForType: types * int * typeVarMap -> codetree
    (* Generate a function of the form (t,t) -> bool. *)
    val equalityForType: types * int * typeVarMap -> codetree

    val applyToInstance:
        { value: types, equality: bool, printity: bool} list * int * typeVarMap * (int -> codetree) -> codetree
    
    structure TypeVarMap:
    sig
        (* Cache of type values and map of type variables. *)
        type typeVarMap = typeVarMap
        val defaultTypeVarMap: (int->int) * int -> typeVarMap (* The end of the chain. *)
        (* Add a set of type variables to the map. *)
        val extendTypeVarMap: (typeVarForm * (int->codetree)) list * (int->int) * int * typeVarMap -> typeVarMap
        (* Look up a type variable and return the type it's mapped to. *)
        val mapTypeVars: typeVarMap -> typeVarForm -> types option
        (* Mark in the cache chain that some type constructors are new. *)
        val markTypeConstructors: typeConstrs list * (int->int) * int * typeVarMap -> typeVarMap
        (* Get the set of cached type values that have been created after this entry. *)
        val getCachedTypeValues: typeVarMap -> codetree list
    end

    val defaultTypeCode: codetree
    
    val justForEqualityTypes: bool

    structure Sharing:
    sig
        type typeId     = typeId
        type codetree   = codetree
        type types      = types
        type typeConstrs= typeConstrs
        type typeConstrSet=typeConstrSet
        type typeVarForm=typeVarForm
        type typeVarMap = typeVarMap
    end
end;
