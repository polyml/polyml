(*
    Title:      Standard Basis Library: Vector and Array slice functor
    Copyright   David C.J. Matthews 2005

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

(* The mono vector slice and mono array slice operations can all be combined
   into this functor fairly easily.  Where appropriate functions can be
   redefined in the calling structure. *)

functor VectorSliceOperations(
    type vector
    type elem
    val vecLength: vector -> word
    val unsafeVecSub: vector * word -> elem
    val unsafeVecUpdate: vector * word * elem -> unit (* Array only *)
):
    sig
    (*type slice*)
    datatype slice = Slice of { vector: vector,  start: word, length: word };
    
    val length : slice -> int
    val sub : (slice * int) -> elem
    val full: vector -> slice
    val slice: vector * int * int option -> slice
    val subslice: slice * int * int option -> slice
    val base: slice -> vector * int * int
    val isEmpty: slice -> bool
    val getItem: slice -> (elem * slice) option
    
    val appi : ((int * elem) -> unit) -> slice -> unit
    val app : (elem -> unit) -> slice -> unit
    val foldli : ((int * elem * 'a) -> 'a) -> 'a -> slice -> 'a
    val foldri : ((int * elem * 'a) -> 'a) -> 'a -> slice -> 'a
    val foldl : ((elem * 'a) -> 'a) -> 'a -> slice -> 'a
    val foldr : ((elem * 'a) -> 'a) -> 'a -> slice -> 'a
    val findi: (int * elem -> bool) -> slice -> (int * elem) option
    val find: (elem -> bool) -> slice -> elem option
    val exists: (elem -> bool) -> slice -> bool
    val all: (elem -> bool) -> slice -> bool
    val collate: (elem * elem -> order) -> slice * slice -> order

    (* These functions modify the vector or array. They aren't used for vectors. *)
    val update: slice * int * elem -> unit
    val modifyi : (int * elem -> elem) -> slice -> unit
    val modify : (elem -> elem) -> slice -> unit
    end =
struct
        val wordAsInt: word -> int = RunCall.unsafeCast

        type elem = elem
        type vector = vector
        datatype slice = Slice of { vector: vector,  start: word, length: word };

        fun length(Slice{length, ...}) = wordAsInt length

        fun op sub (Slice{vector, start, length}, i: int) =
        let
            (* Check that the value is non-negative and short and cast it to word. *)
            val iW = LibrarySupport.unsignedShortOrRaiseSubscript i
        in
            if iW >= length then raise General.Subscript
            else unsafeVecSub(vector, iW+start)
        end

        (* update obviously doesn't apply to vector slices which are immutable. This function
           is filtered out by the caller's signature. *)
        fun update(Slice{vector, start, length}, i: int, x: elem) =
        let
            (* Check that the value is non-negative and short and cast it to word. *)
            val iW = LibrarySupport.unsignedShortOrRaiseSubscript i
        in
            if iW >= length then raise General.Subscript
            else unsafeVecUpdate(vector, iW+start, x)
        end

        (* Slice from the whole vector. *)
        fun full v = Slice{vector=v, start=0w0, length=vecLength v}

        (* Create a slice from a vector. *)
        fun slice(vec: vector, i: int, NONE) =
            let
                val iW = LibrarySupport.unsignedShortOrRaiseSubscript i
                val len = vecLength vec
            in
                if iW <= len
                then Slice{vector=vec, start=iW, length=len-iW} (* Length is rest of vector. *)
                else raise General.Subscript
            end
         |  slice(vec: vector, i: int, SOME l) =
            let
                val len = vecLength vec
                val iW = LibrarySupport.unsignedShortOrRaiseSubscript i
                val lW = LibrarySupport.unsignedShortOrRaiseSubscript l
            in
                if iW+lW <= len
                then Slice{vector=vec, start=iW, length=lW} (* Length is as given. *)
                else raise General.Subscript
            end

        (* Slice from existing slice *)
        fun subslice(Slice{vector, start, length}, i: int, NONE) =
            let
                val iW = LibrarySupport.unsignedShortOrRaiseSubscript i
            in
                if iW <= length
                then Slice{vector=vector, start=iW+start, length=length-iW} (* Length is rest of array. *)
                else raise General.Subscript
            end
    
         |  subslice(Slice{vector, start, length}, i: int, SOME l) =
            let
                val iW = LibrarySupport.unsignedShortOrRaiseSubscript i
                val lW = LibrarySupport.unsignedShortOrRaiseSubscript l
            in
                if iW+lW <= length
                then Slice{vector=vector, start=iW+start, length=lW} (* Length is as given. *)
                else raise General.Subscript
            end

        fun base(Slice{vector, start, length}) = (vector, wordAsInt start, wordAsInt length)
        
        fun isEmpty(Slice{length, ...}) = length = 0w0
    
        (* Return the first item of the slice and the rest of the slice. *)
        fun getItem(Slice{length=0w0, ...}) = NONE
         |  getItem(Slice{vector, start, length}) =
                SOME(unsafeVecSub(vector, start), Slice{vector=vector, start=start+0w1, length=length-0w1})

        (* Standard vector operations. *)
        structure VectorOps =
            VectorOperations(
                struct
                    type vector = slice and elem = elem
                    fun length(Slice{length, ...}) = length
                    fun unsafeSub (Slice{vector, start, ...}, i) = unsafeVecSub(vector, start + i)
                    fun unsafeSet (Slice{vector, start, ...}, i, x) = unsafeVecUpdate(vector, start + i, x)
                end);
    
        open VectorOps;
end;
