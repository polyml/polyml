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
    type typeConstrs
    type typeId
    type typeVarForm
    type typeVarMap

    val chooseConstrRepr : (string*types) list * types list -> codetree list

    type representations
    val RefForm:   representations;
    val BoxedForm: representations;
    val EnumForm:  int -> representations;

    val createNullaryConstructor: representations * types list * string -> codetree
    val createUnaryConstructor: representations * types list * string -> codetree

    (* Standard values *)
    val listType: typeConstrs
    val nilConstructor:  values;
    val consConstructor: values;
    val noneConstructor: values
    val someConstructor: values
    val optionType: typeConstrs
    val buildBasisDatatype:
        string * string * typeVarForm list * bool * (typeConstrs -> values list) -> typeConstrs

    val locationConstr: typeConstrs
    and contextConstr: typeConstrs
    and prettyConstr: typeConstrs

    val mkExIden:       types * int * typeVarMap -> codetree

    (* Types that can be shared. *)
    structure Sharing:
    sig
        type codetree       = codetree
        type types          = types
        type values         = values
        type typeConstrs    = typeConstrs
        type typeId         = typeId
        type typeVarForm    = typeVarForm
        type typeVarMap     = typeVarMap
    end
end;
