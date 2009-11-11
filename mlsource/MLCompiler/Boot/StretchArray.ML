(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

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

(*
    Title:      StretchArray.ML
    Author:     Simon Finn, Abstract Hardware Ltd.
*)

(* Version of array which expands as new elements are assigned.
   Used for tables which have no obvious upper limit.
   
   This is used in CODETREE and the resulting tables are captured in
   the environments of the resulting values.  Previously this resulted in
   a lot of mutable data being kept around and in particularly ending up in
   exported module.  The garbage collector has to scan mutables on every
   collection so mutable data can have a disproportionate effect on
   performance. We now freeze the tables once they have been created.
   This costs extra time during compilation but that should be more than made
   up for by faster garbage collection. *)
structure StretchArray :
sig
  type 'a stretchArray;
  
  exception Subscript;
  exception Size;
  
  val stretchArray : int * 'a -> 'a stretchArray;
  val update : 'a stretchArray * int * 'a -> unit;
  val length : 'a stretchArray -> int;
  val sub    : 'a stretchArray * int -> 'a;
  val freeze : 'a stretchArray -> unit
  
  val vector: 'a stretchArray -> 'a vector
end =

struct
    val expansionFactor = 3; (* Factor by which to increase size. *)

    exception Subscript = (*Array.*)Subscript and Size = (*Array.*)Size;

    (* use Array to hold the contents - less efficient than making
       this a primitive, but does it matter? *)
     
    datatype 'a vecOrArray = 
        AnArray of 'a array
      | AVector of 'a vector

    type 'a stretchArray =
    {
      initialVal : 'a,
      contents   : 'a vecOrArray ref
    }
     
    fun stretchArray (originalSize: int, initialVal: 'a) :'a stretchArray =
    {
      initialVal = initialVal,
      contents   = ref (AnArray(Array.array(originalSize, initialVal)))
    };

    fun length { contents = ref (AnArray a), ...} = Array.length a
      | length  { contents = ref (AVector v), ...} = Vector.length v
 
    (* Returns the value if there is one, otherwise returns the default. *)
    infix 9 sub;
    
    fun ({contents = ref (AnArray a), initialVal, ...} :'a stretchArray) sub index : 'a =
        if index < Array.length a
        then Array.sub (a, index) (* may raise Subscript *)
        else initialVal
     |  ({contents = ref (AVector v), initialVal, ...} :'a stretchArray) sub index : 'a =
        if index < Vector.length v
        then Vector.sub (v, index) (* may raise Subscript *)
        else initialVal
  
    (* Sets the appropriate entry. *)
    fun update ({contents = ref (AVector _), ...}, _, _) =
            raise Fail "Attempt to update a locked stretchArray"
            
    |   update ({contents = contents as ref (AnArray a), initialVal, ... }: 'a stretchArray, index: int, value: 'a) : unit =
            if index < Array.length a
            then Array.update (a, index, value) (* May raise Subscript if the index is -ve *)
            else
            let
               (* The new vector must be big enough to hold the new item
                  and expanded by the expansion factor. *)
               val oldSize = Array.length a;
               val newSize = Int.max (oldSize * expansionFactor, index + 1);
               val newVec = Array.array(newSize, initialVal);
            in
               Array.copy{src=a, dst=newVec, di=0};
               contents := AnArray newVec;
               Array.update (newVec, index, value)
            end

    (* Create an immutable vector from the current contents of the array. *)
    fun vector ({contents = ref (AnArray a), ...}: 'a stretchArray): 'a vector =
        Array.vector a
    |   vector {contents = ref (AVector v), ...} = v

    (* Turn an array into a vector and turn the ref into an immutable. *)
    fun freeze {contents = ref (AVector _), ...} = ()
     |  freeze {contents = contents as ref (AnArray a), ...} =
        (
        contents := AVector(Array.vector a);
        Address.lock(Address.toAddress contents)
        )

end;

