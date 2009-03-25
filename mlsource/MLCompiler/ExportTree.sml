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
end;

structure PRETTY: PRETTYSIG

): EXPORTTREESIG =
struct
    open PRETTY
    type types = STRUCTVALS.types
    type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }

    datatype ptProperties =
        PTprint of int -> pretty (* Print the tree *)
    |   PTtype of types (* Type of an expression *)
    |   PTdeclaredAt of location (* Declaration location for id. *)
    |   PTparent of unit -> exportTree
    |   PTpreviousSibling of unit -> exportTree
    |   PTnextSibling of unit -> exportTree
    |   PTfirstChild of unit -> exportTree
    withtype exportTree = location * ptProperties list

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

    val rootTreeTag: (unit -> exportTree) option Universal.tag = Universal.tag()

end;
