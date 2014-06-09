(*
    Title:      Standard Basis Library: Array Structure
    Author:     David Matthews
    Copyright   David Matthews 1999, 2005

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

(* G&R 2004 status: updated.  Added ArraySlice and ARRAY_SLICE. *)

signature ARRAY =
  sig
    eqtype 'a array
    type 'a vector

    val maxLen : int
    val array : (int * 'a) -> 'a array
    val fromList : 'a list -> 'a array
    val vector: 'a array -> 'a vector
    val tabulate : (int * (int -> 'a)) -> 'a array
    val length : 'a array -> int
    val sub : ('a array * int) -> 'a
    val update : ('a array * int * 'a) -> unit
    val copy : {src : 'a array, dst : 'a array, di : int} -> unit
    val copyVec : {src : 'a vector, dst : 'a array, di : int} -> unit

    val appi : ((int * 'a) -> unit) -> 'a array -> unit
    val app : ('a -> unit) -> 'a array -> unit

    val foldli : ((int * 'a * 'b) -> 'b) -> 'b -> 'a array -> 'b
    val foldri : ((int * 'a * 'b) -> 'b) -> 'b -> 'a array -> 'b
    val foldl : (('a * 'b) -> 'b) -> 'b -> 'a array -> 'b
    val foldr : (('a * 'b) -> 'b) -> 'b -> 'a array -> 'b

    val modifyi : ((int * 'a) -> 'a) -> 'a array -> unit
    val modify : ('a -> 'a) -> 'a array -> unit

    val findi: (int * 'a -> bool) -> 'a array -> (int * 'a) option
    val find: ('a -> bool) -> 'a array -> 'a option
    val exists: ('a -> bool) -> 'a array -> bool
    val all: ('a -> bool) -> 'a array -> bool
    val collate: ('a * 'a -> order) -> 'a array * 'a array -> order

  end;

local
    (* This was previously implemented as simply an n-word block of mutable
       store, length being obtained from the length field.  There was a
       complication in that equality for arrays is defined as pointer equality
       even for zero-sized arrays but the run-time system doesn't allow zero-sized
       objects.  To get round that we used a one-word object with the "negative"
       bit set in the flags byte.  This meant that the length function was quite
       complicated which is significant because we need to compute the length
       to do bounds checking on every "sub" or "update".  This method is the
       most efficient in storage.  The current version uses the first word
       to hold the length. *)
    open RuntimeCalls
    type 'a array = 'a array (* Predeclared in the basis with special equality props. *)

    val System_alloc: int*word*word->word  = RunCall.run_call3 POLY_SYS_alloc_store;
    val System_loadw: word*int->word = RunCall.run_call2 POLY_SYS_load_word;
    val System_setw: word * int * word -> unit   = RunCall.run_call3 POLY_SYS_assign_word;
    val System_lock: word -> unit   = RunCall.run_call1 POLY_SYS_lockseg;
    val System_zero: word   = RunCall.run_call1 POLY_SYS_io_operation POLY_SYS_nullvector; (* A zero word. *)
    val System_move_words:
        word*int*word*int*int->unit = RunCall.run_call5 POLY_SYS_move_words
    val System_move_words_overlap:
        word*int*word*int*int->unit = RunCall.run_call5 POLY_SYS_move_words_overlap

    (* Unsafe subscript and update functions used internally for cases
       where we've already checked the range. 
       N.B.  THESE ADD THE ONE WHICH IS NECESSARY TO SKIP THE LENGTH WORD *)
    fun unsafeSub(v: 'a array, i: int): 'a =
        RunCall.unsafeCast(System_loadw (RunCall.unsafeCast v, i+1))

    and unsafeUpdate(v: 'a array, i: int, new: 'a): unit =
        System_setw (RunCall.unsafeCast v, i+1, RunCall.unsafeCast new);

    val intAsWord: int -> word = RunCall.unsafeCast
    and wordAsInt: word -> int = RunCall.unsafeCast

    (* "vector" creates a vector from an array so the representation of a
       zero-length object is different.  Locking the resultant object turns
       into an immutable object and changes the equality function from pointer
       equality to value equality. *)
    fun makeVector(v: 'a array, start, length): 'a vector =
        if length = 0 then RunCall.unsafeCast System_zero (* Special case for zero *)
        else (* The size must have already been checked. *)
        let
            (* Make a vector initialised to zero. *)
            val new_vec = System_alloc(length, 0wx40, 0w0)
        in
            System_move_words(RunCall.unsafeCast v, start+1, new_vec, 0, length);
            System_lock new_vec;
            RunCall.unsafeCast new_vec
        end
in
structure Array: ARRAY =
struct
    type 'a array = 'a array
    type 'a vector = 'a Vector.vector
    
    (* The maximum size of an array is one less than the maximum allocation
       size to allow for the length word. *)
    val maxLen = RunCall.unsafeCast(LibrarySupport.maxAllocation - 0w1)
    
    (* Internal function: Construct an array initialised to zero. That's probably
       more efficient than the alternative of setting every word to the length. *)
    fun alloc len =
        let
            val () = if len >= maxLen then raise General.Size else ()
            val vec = System_alloc(len+1, 0wx40, 0w0)
        in
            System_setw(vec, 0, RunCall.unsafeCast len);
            RunCall.unsafeCast vec
        end
     
    fun array(len, a) =
        let
            val () = if len < 0 orelse len >= maxLen then raise General.Size else ()
            val vec = System_alloc(len+1, 0wx40, RunCall.unsafeCast a)
        in
            System_setw(vec, 0, RunCall.unsafeCast len);
            RunCall.unsafeCast vec
        end

    val listLength = length; (* Pick this up from the prelude. *)
    fun length (vec: 'a array): int = RunCall.unsafeCast(System_loadw(RunCall.unsafeCast vec, 0))
    
    fun op sub (vec: 'a array as v, i: int): 'a =
        if i < 0 orelse i >= length vec then raise General.Subscript
        else RunCall.unsafeCast(System_loadw (RunCall.unsafeCast v, i+1))
 
    fun update (vec: 'a array as v, i: int, new: 'a) : unit =
        if i < 0 orelse i >= length vec
        then raise General.Subscript
        else System_setw (RunCall.unsafeCast v, i+1, RunCall.unsafeCast new);

    (* Create an array from a list. *)
    fun fromList (l : 'a list) : 'a array =
        let
        val length = listLength l;
            
        (* Make a array initialised to zero. *)
        val vec = alloc length
        
        (* Copy the list elements into the array. *)
        fun init (v, i, a :: l) =
             (
             unsafeUpdate(v, i, a);
             init(v, i + 1, l)
             )
        |  init (_, _, []) = ();
        
    in
        init(vec, 0, l);
        vec
    end
        
    fun tabulate (length: int , f : int->'a): 'a array =
    let
        val vec =
            if length < 0 then raise General.Size
            else alloc length;
        (* Initialise it to the function values. *)
        fun init i = 
            if length <= i then ()
            else (unsafeUpdate(vec, i, f i); init(i+1))
    in
        init 0;
        vec
    end
    
    (* "vector" creates a vector from an array so the representation of a
       zero-length object is different.  Locking the resultant object turns
       into an immutable object and changes the equality function from pointer
       equality to value equality. *)
    fun vector (vec: 'a array): 'a vector = makeVector(vec, 0, length vec)
    
    (* Copy one array into another.  It's possible for the arrays
       to be the same but in that case di would have to be zero otherwise the length
       check would fail. *)
    fun copy {src: 'a array as s, dst: 'a array as d, di: int} =
        let
            val len = length src
        in
            if di < 0 orelse di+len > length dst
            then raise General.Subscript
            else System_move_words(RunCall.unsafeCast s, 1, RunCall.unsafeCast d, di+1, len)
        end

    (* Copy a vector into an array. *)
    fun copyVec {src: 'a vector, dst: 'a array as d, di: int} =
        let
            val len = Vector.length src
        in
            if di < 0 orelse di+len > length dst
            then raise General.Subscript
            else System_move_words(RunCall.unsafeCast src, 0, RunCall.unsafeCast d, di+1, len)
        end
        

    (* Create the other functions. *)
    structure VectorOps =
        PolyVectorOperations(
            struct
                type 'a vector = 'a array
                local val l = length in fun length(v: 'a array):word = intAsWord(l v) end
                local val u = unsafeSub in fun unsafeSub (v: 'a array, i: word) = u(v, wordAsInt i) end
                fun unsafeSet(v, i: word, e: 'a) = unsafeUpdate(v, wordAsInt i, e)
            end);

    open VectorOps;

    local
        (* Install the pretty printer for arrays *)
        (* We may have to do this outside the structure if we
           have opaque signature matching. *)
        fun pretty(depth: int)
                  (printElem: 'a * int -> PolyML.pretty)
                  (x: 'a array) =
            let
                open PolyML
                val last = length x - 1
                fun put_elem (index, w, (l, d)) =
                    if d = 0 then ([PrettyString "...]"], d+1)
                    else if d < 0 then ([], d+1)
                    else
                    (
                    printElem(w, d-1) ::
                        (if index <> last then PrettyString "," :: PrettyBreak(1, 0) :: l else l),
                    d+1
                    )
            in
                PrettyBlock(3, false, [],
                    PrettyString "fromList[" ::
                    (if depth <= 0 then [PrettyString "...]"]
                     else #1 (foldri put_elem ([PrettyString "]"], depth-last) x) )
               )
            end
    in
        val () = PolyML.addPrettyPrinter pretty
    end

end (* Array *)

structure ArraySlice =
struct
    
    datatype 'a slice = Slice of { array: 'a Array.array,  start: int, length: int };
    
    fun length(Slice{length, ...}) = length
    
    fun op sub (Slice{array, start, length}, i: int): 'a =
        if i < 0 orelse i >= length then raise General.Subscript
        else unsafeSub(array, i+start)

    fun update (Slice{array, start, length}, i: int, new: 'a) : unit =
        if i < 0 orelse i >= length
        then raise General.Subscript
        else unsafeUpdate (array, i+start, new);

    (* Create a slice, checking the sizes so that the resulting slice is always valid. *)
    fun slice(vec: 'a array, i: int, NONE) =
    let
        val len = Array.length vec
    in
        if i >= 0 andalso i <= len
        then Slice{array=vec, start=i, length=len-i} (* Length is rest of array. *)
        else raise General.Subscript
    end
     |  slice(vec: 'a array, i: int, SOME l) =
    let
        val len = Array.length vec
    in
        if i >= 0 andalso l >= 0 andalso i+l <= len
        then Slice{array=vec, start=i, length=l} (* Length is as given. *)
        else raise General.Subscript
    end

    (* Slice from the whole array. *)
    fun full a = Slice{array=a, start=0, length=Array.length a}

    (* Slice from existing slice *)
    fun subslice(Slice{array, start, length}, i: int, NONE) =
        if i >= 0 andalso i <= length
        then Slice{array=array, start=i+start, length=length-i} (* Length is rest of array. *)
        else raise General.Subscript

     |  subslice(Slice{array, start, length}, i: int, SOME l) =
        if i >= 0 andalso l >= 0 andalso i+l <= length
        then Slice{array=array, start=i+start, length=l} (* Length is as given. *)
        else raise General.Subscript

    fun base(Slice{array, start, length}) = (array, start, length)

    fun vector (Slice{array, start, length}): 'a vector = makeVector(array, start, length)

    
    (* Copy one array into another.  It's possible for the arrays
       to be the same and for the source and destinations to overlap so we
       have to take care of that. *)
    fun copy {src = Slice{array=s, start=srcStart, length=srcLen}, dst, di: int} =
        if di < 0 orelse di+srcLen > Array.length dst
        then raise General.Subscript
        else System_move_words_overlap(RunCall.unsafeCast s, srcStart+1, RunCall.unsafeCast dst, di+1, srcLen)

    (* Copy a vector into an array. *)
    fun copyVec {src: 'a VectorSlice.slice, dst: 'a array as d, di: int} =
        let
            val (v, i, len) = VectorSlice.base src
        in
            if di < 0 orelse di+len > Array.length dst
            then raise General.Subscript
            else System_move_words(RunCall.unsafeCast v, i, RunCall.unsafeCast d, di+1, len)
        end

    fun isEmpty(Slice{length, ...}) = length = 0

    (* Return the first item of the slice and the rest of the slice. *)
    fun getItem(Slice{length=0, ...}) = NONE
     |  getItem(Slice{array, start, length}) =
            SOME(unsafeSub(array, start), Slice{array=array, start=start+1, length=length-1})

    (* Create the other functions. *)
    structure VectorOps =
        PolyVectorOperations(
            struct
                type 'a vector = 'a slice
                fun length(Slice{length, ...}) = intAsWord length
                local 
                    val u = unsafeSub
                in
                    fun unsafeSub (Slice{array, start, ...}, i: word) = u(array, wordAsInt i + start)
                end
                fun unsafeSet(Slice{array, start, ...}, i: word, e: 'a) = unsafeUpdate(array, wordAsInt i + start, e)
            end);

    open VectorOps;

end (* ArraySlice *)

end; (* Local in end *)

(* The ARRAY_SLICE signature refers to the Array structure so has to be defined afterwards. *)
signature ARRAY_SLICE =
  sig
    type 'a slice
    val length : 'a slice -> int
    val sub : 'a slice * int -> 'a
    val update : 'a slice * int * 'a -> unit
    val full: 'a Array.array -> 'a slice
    val slice: 'a Array.array * int * int option -> 'a slice
    val subslice: 'a slice * int * int option -> 'a slice
    val base: 'a slice -> 'a Array.array * int * int
    val vector: 'a slice -> 'a Vector.vector
    val copy : {src : 'a slice, dst : 'a Array.array, di : int} -> unit
    val copyVec : {src : 'a VectorSlice.slice, dst : 'a Array.array, di : int} -> unit
    val isEmpty: 'a slice -> bool
    val getItem: 'a slice -> ('a * 'a slice) option

    val appi : (int * 'a -> unit) -> 'a slice -> unit
    val app : ('a -> unit) -> 'a slice -> unit

    val modifyi : (int * 'a -> 'a) -> 'a slice -> unit
    val modify : ('a -> 'a) -> 'a slice -> unit

    val foldli : ((int * 'a * 'b) -> 'b) -> 'b -> 'a slice -> 'b
    val foldri : ((int * 'a * 'b) -> 'b) -> 'b -> 'a slice -> 'b
    val foldl : (('a * 'b) -> 'b) -> 'b -> 'a slice -> 'b
    val foldr : (('a * 'b) -> 'b) -> 'b -> 'a slice -> 'b
    
    val findi: (int * 'a -> bool) -> 'a slice -> (int * 'a) option
    val find: ('a -> bool) -> 'a slice -> 'a option
    
    val exists: ('a -> bool) -> 'a slice -> bool
    val all:  ('a -> bool) -> 'a slice -> bool
    val collate: ('a * 'a -> order) -> 'a slice * 'a slice -> order


  end;
  
structure ArraySlice :> ARRAY_SLICE = ArraySlice;

local
    open ArraySlice

    (* Install the pretty printer for array slices *)
    (* We may have to do this outside the structure if we
       have opaque signature matching. *)
    fun pretty(depth: int)
              (printElem: 'a * int -> PolyML.pretty)
              (x: 'a slice) =
        let
            open PolyML
            val last = length x - 1
            fun put_elem (index, w, (l, d)) =
                if d = 0 then ([PrettyString "...]"], d+1)
                else if d < 0 then ([], d+1)
                else
                (
                printElem(w, d-1) ::
                    (if index <> last then PrettyString "," :: PrettyBreak(1, 0) :: l else l),
                d+1
                )
        in
            PrettyBlock(3, false, [],
                PrettyString "fromList[" ::
                (if depth <= 0 then [PrettyString "...]"]
                 else #1 (foldri put_elem ([PrettyString "]"], depth-last) x) )
           )
        end
in
    val _ = PolyML.addPrettyPrinter pretty
end
