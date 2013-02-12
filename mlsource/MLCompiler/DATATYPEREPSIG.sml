(*
    Copyright (c) 2009 David C. J. Matthews 2009.

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

signature DATATYPEREPSIG =
sig
    type codetree
    type types
    type values
    type typeConstrSet
    type typeId
    type typeVarForm
    type typeVarMap
    type level

    val chooseConstrRepr :
        (string*types) list * types list -> { constrs: codetree list, boxed: codetree, size: codetree }

    type representations
    val RefForm:   representations;
    val EnumForm:  { tag: word, maxTag: word } -> representations;

    val createNullaryConstructor: representations * types list * string -> codetree
    val createUnaryConstructor: representations * types list * string -> codetree

    (* Standard values *)
    val listConstr: typeConstrSet
    val nilConstructor:  values;
    val consConstructor: values;
    val noneConstructor: values
    val someConstructor: values
    val optionConstr: typeConstrSet

    val locationConstr: typeConstrSet
    and contextConstr: typeConstrSet
    and prettyConstr: typeConstrSet

    val mkExIden:       types * level * typeVarMap -> codetree

    (* Types that can be shared. *)
    structure Sharing:
    sig
        type codetree       = codetree
        type types          = types
        type values         = values
        type typeConstrSet  = typeConstrSet
        type typeId         = typeId
        type typeVarForm    = typeVarForm
        type typeVarMap     = typeVarMap
        type level          = level
    end
end;
