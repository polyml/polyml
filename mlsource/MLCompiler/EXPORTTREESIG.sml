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

signature EXPORTTREESIG =
sig
    type pretty
    type types
    type location =
        { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }
    type locationProp

    (* Export tree. *)
    datatype ptProperties =
        PTprint of int -> pretty (* Print the tree *)
    |   PTtype of types (* Type of an expression *)
    |   PTdeclaredAt of location (* Declaration location for id. *)
    |   PTopenedAt of location (* When an identifier comes from an "open" the location of the open. *)
    |   PTstructureAt of location (* When an identifier comes from open S or S.a the declaration of S. *)
    |   PTparent of unit -> exportTree
    |   PTpreviousSibling of unit -> exportTree
    |   PTnextSibling of unit -> exportTree
    |   PTfirstChild of unit -> exportTree
    withtype exportTree = location * ptProperties list
    
    type navigation =
        {parent: (unit -> exportTree) option,
         next: (unit -> exportTree) option,
         previous: (unit -> exportTree) option}
    
    val exportList :
        (navigation * 'a -> exportTree)
            * (unit -> exportTree) option -> 'a list -> ptProperties list
            
    val exportNavigationProps: navigation -> ptProperties list

    val getStringAsTree: navigation * string * location * ptProperties list -> exportTree
    
    val rootTreeTag: (unit -> exportTree) option Universal.tag
    
    val mapLocationProps: locationProp list -> ptProperties list
end;
