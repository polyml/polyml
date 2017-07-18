(*
    Copyright (c) 2017 David C.J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

(* Set of integers implemented as an ordered list.  This is used
   for active register sets. *)
structure IntSet:> INTSETSIG =
struct
    datatype intSet = IntSet of int list

    val emptySet = IntSet []
    
    fun setToList(IntSet s) = s
    
    local
        fun addItem(i, []) = [i]
        |   addItem(i, hd::tl) =
                if i = hd then hd :: tl
                else if i < hd then i :: hd :: tl
                else hd :: addItem(i, tl)
    in
        (* Add an item to the list.  It seems to be better to add in order rather than reverse order. *)
        fun addToList(items, IntSet toSet) = IntSet(List.foldl(fn (d, l) => addItem(d, l)) toSet items)
        
        fun listToSet items = addToList(items, IntSet [])
    end
    
    local
        fun removeItem(_, []) = []
        |   removeItem(item, hd :: tl) = if item = hd then tl else hd :: removeItem(item, tl)
    in
        fun removeFromSet(item, IntSet fromSet) = IntSet(removeItem(item, fromSet))
    end
    
    local
        (* If the lists are already sorted we can merge them.
           This is an allocation hot-spot.  Avoid recreating the list if possible. *)
        fun mergeLists(listA as a::tlA, listB as b::tlB) =
            if a = b
            then
            let
                val (tail, tailEq) = mergeLists(tlA, tlB)
            in
                if PolyML.pointerEq(tlA, tail)
                then (listA, tailEq)
                else if PolyML.pointerEq(tlB, tail)
                then (listB, tailEq)
                else (a :: tail, false)
            end
            else if a < b
            then
            let
                val (tail, tailEq) = mergeLists(tlA, listB)
            in
                if PolyML.pointerEq(tail, tlA) orelse tailEq
                then (listA, false)
                else (a :: tail, false)
            end
            else
            let
                val (tail, tailEq) = mergeLists(listA, tlB)
            in
                if PolyML.pointerEq(tail, tlB) orelse tailEq
                then (listB, false)
                else (b :: tail, false)
            end
        |   mergeLists([], []) = ([], true)
        |   mergeLists([], b) = (b, false)
        |   mergeLists(a, []) = (a, false)
        
    in
        fun union(IntSet setA, IntSet setB) =
        let
            val (result, _) = mergeLists(setA, setB)
        in
            IntSet result
        end
    end
    
    fun cardinality(IntSet l) = List.length l
    
    fun filterSet f (IntSet l) = IntSet(List.filter f l)
    
    fun member(i, IntSet l) = List.exists(fn n => n=i) l
end;
