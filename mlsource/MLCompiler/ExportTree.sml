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

functor ExportTree(
structure STRUCTVALS :
sig
    type types
    type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }

    datatype locationProp =
        DeclaredAt of location
    |   OpenedAt of location
    |   StructureAt of location
end;

structure PRETTY: PRETTYSIG

): EXPORTTREESIG =
struct
    open PRETTY STRUCTVALS

(*
    datatype ptProperties =
        PTprint of int -> pretty (* Print the tree *)
    |   PTtype of types (* Type of an expression *)
    |   PTdeclaredAt of location (* Declaration location for id. *)
    |   PTopenedAt of location (* When an identifier comes from an "open" the location of the open. *)
    |   PTstructureAt of location (* When an identifier comes from open S or S.a the declaration of S. *)
    |   PTreferences of bool * location list (* The references to the ID.  The first is true if this is exported. *)
    |   PTparent of unit -> exportTree
    |   PTpreviousSibling of unit -> exportTree
    |   PTnextSibling of unit -> exportTree
    |   PTfirstChild of unit -> exportTree
    withtype exportTree = location * ptProperties list *)
    local
        open Address
        fun cast p = toAddress(toMachineWord p)
    in
        type ptProperties = address
        type exportTree = location * ptProperties list

        fun PTdeclaredAt(loc: location): ptProperties = cast(0w0, loc)
        and PTfirstChild(entry: unit -> exportTree): ptProperties = cast(0w1, entry)
        and PTnextSibling(entry: unit -> exportTree): ptProperties = cast(0w2, entry)
        and PTopenedAt(loc: location): ptProperties = cast(0w3, loc)
        and PTparent(entry: unit -> exportTree): ptProperties = cast(0w4, entry)
        and PTpreviousSibling(entry: unit -> exportTree): ptProperties = cast(0w5, entry)
        and PTprint(pr: int -> pretty): ptProperties = cast(0w6, pr)
        and PTreferences(exp: bool, locs: location list): ptProperties = cast(0w7, exp, locs)
        and PTstructureAt(loc: location): ptProperties = cast(0w8, loc)
        and PTtype(typ: types): ptProperties = cast(0w9, typ)
    end

    (* This representation is exported so we have to use a *)

    type navigation =
        {parent: (unit -> exportTree) option,
         next: (unit -> exportTree) option,
         previous: (unit -> exportTree) option}

    (* Navigate within a list *)
    fun exportList _ [] = []
    |   exportList(exp, parent) sl =
    let
        fun getEntry(this as (s :: sl), getPrevious) () =
            exp(
                {
                    parent = parent,
                    previous = getPrevious,
                    (* If we have a successor then that is the entry and
                       its predecessor returns here. *)
                    next =
                        case sl of
                            [] => NONE
                        |   t  => SOME(getEntry(t, SOME(getEntry(this, getPrevious))))
                },
                s
                )
        |   getEntry _ () = raise Empty
    in
        [PTfirstChild (getEntry(sl, NONE))]
    end

    fun exportNavigationProps{parent, previous, next} =
    let
        fun mapProps(_, NONE) = [] | mapProps(f, SOME v) = [f v]
    in
        (* Common properties for navigation and printing. *)
        mapProps(PTparent, parent) @
        mapProps(PTpreviousSibling, previous) @
        mapProps(PTnextSibling, next)
    end

    fun getStringAsTree (navigation, s: string, location: location, otherProps) =
         (location, otherProps @ exportNavigationProps navigation @ [PTprint(fn _ => PrettyString s)])

    (* Tag used to indicate the root tree node in the compiler arguments. *)
    val rootTreeTag: navigation Universal.tag = Universal.tag()

    (* Map value locations into properties. *)
    fun mapLocationProps locs =
    let
        fun prop (DeclaredAt loc) = PTdeclaredAt loc
        |   prop (OpenedAt loc) = PTopenedAt loc
        |   prop (StructureAt loc) = PTstructureAt loc
    in
        List.map prop locs
    end

    (* Types that can be shared. *)
    structure Sharing =
    struct
        type types          = types
        and  locationProp   = locationProp
        and  pretty         = pretty
        and  ptProperties   = ptProperties
    end

end;
