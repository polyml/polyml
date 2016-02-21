(*
    Title:      Rebuild the basis library with the default int as IntInf.int
    Copyright   David C.J. Matthews 2016

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

RunCall.setDefaultIntTypeArbitrary true; (* Set the default for overloadings. *)

type int = IntInf.int;

(* Integer *)
(* Define this first.  It's explicitly referenced in the INTEGER signature. *)
structure Int = struct type int = IntInf.int end;

use "basis/INTEGER";

signature INT_INF =
sig
    include INTEGER
    val divMod : int * int -> int * int
    val quotRem : int * int -> int * int
    val pow : int * Int.int -> int
    val log2 : int -> Int.int
    val orb : int * int -> int
    val xorb : int * int -> int
    val andb : int * int -> int
    val notb : int -> int
    val << : int * Word.word -> int
    val ~>> : int * Word.word -> int
end;

structure IntInf: INT_INF =
struct
    open IntInf
    val toInt = toLarge and fromInt = fromLarge
    val precision = Option.map FixedInt.toLarge precision
    val sign = FixedInt.toLarge o sign
    val log2 = FixedInt.toLarge o log2
    val pow = fn (i, j) => pow(i, FixedInt.fromLarge j)
end;

structure LargeInt: INTEGER = IntInf;

structure Int: INTEGER = LargeInt;

structure FixedInt: INTEGER =
struct
    open FixedInt
    val toInt = toLarge and fromInt = fromLarge
    val precision = Option.map toLarge precision
    val sign = FixedInt.toLarge o sign
end;
val () =
    case FixedInt.precision of SOME 31 => use "basis/Int31.sml" | SOME 63 => use "basis/Int63.sml" | _ => ();

(* List *)
use "basis/ListSignature";

structure List: LIST =
struct
    open List
    val take = fn (l, i) => take(l, FixedInt.fromInt i)
    val tabulate = fn (n, f) => tabulate(FixedInt.fromInt n, fn q => f(FixedInt.toInt q))
    val length = fn l => FixedInt.toInt(length l)
    val nth = fn (l, i) => nth(l, FixedInt.fromInt i)
    val drop = fn (l, i) => drop(l, FixedInt.fromInt i)
end;

val length : 'a list -> int = List.length;

(* Char and String *)
use "basis/StringSignatures";

structure Char: CHAR =
struct
    open Char
    val maxOrd = FixedInt.toInt maxOrd
    val chr = chr o FixedInt.fromInt
    val ord = FixedInt.toInt o ord
end;

structure String: STRING =
struct
    open String
    val maxSize = FixedInt.toInt maxSize
    val size = FixedInt.toInt o size
    val sub = fn (s, i) => sub(s, FixedInt.fromInt i)
    val substring = fn (s, i, j) => substring(s, FixedInt.fromInt i, FixedInt.fromInt j)
    val extract = fn(s, i, j) => extract(s, FixedInt.fromInt i, Option.map FixedInt.fromInt j)
end;

structure Substring : SUBSTRING =
struct
    open Substring
    val base = fn s => let val (a, i, j) = base s in (a, FixedInt.toInt i, FixedInt.toInt j) end
    val size = FixedInt.toInt o size
    val sub = fn (s, i) => sub(s, FixedInt.fromInt i)
    val substring = fn (s, i, j) => substring(s, FixedInt.fromInt i, FixedInt.fromInt j)
    val extract = fn(s, i, j) => extract(s, FixedInt.fromInt i, Option.map FixedInt.fromInt j)
    val splitAt = fn (s, i) => splitAt(s, FixedInt.fromInt i)
    val slice = fn (s, i, j) => slice(s, FixedInt.fromInt i, Option.map FixedInt.fromInt j)
    val trimr = fn i => trimr(FixedInt.fromInt i)
    and triml = fn i => triml(FixedInt.fromInt i)
end;

val ord : char -> int = Char.ord 
val chr : int -> char = Char.chr 
val substring : string * int * int -> string = String.substring;
val size: string -> int = String.size;

(* Arrays and Vectors. *)

use "basis/MONO_VECTOR";

functor MapMonoVector(
    structure Old:
    sig
        type  vector
        type  elem
        val maxLen : FixedInt.int
        val fromList : elem list -> vector
        val tabulate : (FixedInt.int * (FixedInt.int -> elem)) -> vector
        val length : vector -> FixedInt.int
        val sub : (vector * FixedInt.int) -> elem
        val update: vector * FixedInt.int * elem -> vector
        val concat : vector list -> vector
        val mapi : ((FixedInt.int * elem) -> elem) -> vector -> vector
        val map : (elem -> elem) -> vector -> vector
        val appi : ((FixedInt.int * elem) -> unit) -> vector -> unit
        val app : (elem -> unit) -> vector -> unit
        val foldli : ((FixedInt.int * elem * 'a) -> 'a) -> 'a -> vector -> 'a
        val foldri : ((FixedInt.int * elem * 'a) -> 'a) -> 'a -> vector -> 'a
        val foldl : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a
        val foldr : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a
        val findi: (FixedInt.int * elem -> bool) -> vector -> (FixedInt.int * elem) option
        val find: (elem -> bool) -> vector -> elem option
        val exists: (elem -> bool) -> vector -> bool
        val all: (elem -> bool) -> vector -> bool
        val collate: (elem * elem -> order) -> vector * vector -> order
    end
): MONO_VECTOR =
struct
    open Old
    val maxLen = FixedInt.toLarge maxLen
    val sub = fn (v, i) => sub(v, FixedInt.fromLarge i)
    val length = FixedInt.toLarge o length
    val update = fn (v, i, e) => update(v, FixedInt.fromLarge i, e)
    val foldri = fn f => foldri (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val foldli = fn f => foldli (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val tabulate = fn (n, f) => tabulate(FixedInt.fromInt n, fn q => f(FixedInt.toInt q))
    val appi = fn f => appi(fn (i, e) => f(FixedInt.toLarge i, e))
    val mapi = fn f => mapi(fn (i, e) => f(FixedInt.toLarge i, e))
    val findi = fn f => fn v => 
        Option.map (fn(i, e) => (FixedInt.toLarge i, e)) (findi(fn (i, e) => f(FixedInt.toLarge i, e)) v)
end;

structure CharVector: MONO_VECTOR = MapMonoVector(structure Old = CharVector);
structure BoolVector: MONO_VECTOR = MapMonoVector(structure Old = BoolVector);
structure RealVector: MONO_VECTOR = MapMonoVector(structure Old = RealVector);
structure Word8Vector: MONO_VECTOR = MapMonoVector(structure Old = Word8Vector);


use "basis/VectorSignature.sml";

structure Vector: VECTOR =
struct
    open Vector
    val maxLen = FixedInt.toLarge maxLen
    val sub = fn (v, i) => sub(v, FixedInt.fromLarge i)
    val length = fn l => FixedInt.toLarge(length l)
    val update = fn (v, i, e) => update(v, FixedInt.fromLarge i, e)
    val foldri = fn f => foldri (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val foldli = fn f => foldli (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val tabulate = fn (n, f) => tabulate(FixedInt.fromInt n, fn q => f(FixedInt.toInt q))
    val appi = fn f => appi(fn (i, e) => f(FixedInt.toLarge i, e))
    val mapi = fn f => mapi(fn (i, e) => f(FixedInt.toLarge i, e))
    val findi = fn f => fn v => 
        Option.map (fn(i, e) => (FixedInt.toLarge i, e)) (findi(fn (i, e) => f(FixedInt.toLarge i, e)) v)
end;

use "basis/MONO_ARRAY";

functor MapMonoArray(
    structure Old:
    sig
        eqtype array
        type elem
        type vector
        val maxLen : FixedInt.int
        val array : (FixedInt.int * elem) -> array
        val fromList : elem list -> array
        val vector: array -> vector
        val tabulate : (FixedInt.int * (FixedInt.int -> elem)) -> array
        val length : array -> FixedInt.int
        val sub : (array * FixedInt.int) -> elem
        val update : (array * FixedInt.int * elem) -> unit
        val copy : {src : array, dst : array, di : FixedInt.int} -> unit
        val copyVec : {src : vector, dst : array, di : FixedInt.int} -> unit
        val appi : ((FixedInt.int * elem) -> unit) -> array -> unit
        val app : (elem -> unit) -> array -> unit
        val foldli : ((FixedInt.int * elem * 'b) -> 'b) -> 'b -> array -> 'b
        val foldri : ((FixedInt.int * elem * 'b) -> 'b) -> 'b -> array -> 'b
        val foldl : ((elem * 'b) -> 'b) -> 'b -> array -> 'b
        val foldr : ((elem * 'b) -> 'b) -> 'b -> array -> 'b
        val modifyi : ((FixedInt.int * elem) -> elem) -> array -> unit
        val modify : (elem -> elem) -> array -> unit
        val findi: (FixedInt.int * elem -> bool) -> array -> (FixedInt.int * elem) option
        val find: (elem -> bool) -> array -> elem option
        val exists: (elem -> bool) -> array -> bool
        val all: (elem -> bool) -> array -> bool
        val collate: (elem * elem -> order) -> array * array -> order
    end
) : MONO_ARRAY =
struct
    open Old
    val maxLen = FixedInt.toLarge maxLen
    val array = fn (i, a) => array(FixedInt.fromLarge i, a)
    val sub = fn (v, i) => sub(v, FixedInt.fromLarge i)
    val length = fn l => FixedInt.toLarge(length l)
    val update = fn (v, i, e) => update(v, FixedInt.fromLarge i, e)
    val copy = fn {di, dst, src} => copy {di=FixedInt.fromLarge di, dst=dst, src=src}
    val copyVec = fn {di, dst, src} => copyVec {di=FixedInt.fromLarge di, dst=dst, src=src}
    val foldri = fn f => foldri (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val foldli = fn f => foldli (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val tabulate = fn (n, f) => tabulate(FixedInt.fromInt n, fn q => f(FixedInt.toInt q))
    val appi = fn f => appi(fn (i, e) => f(FixedInt.toLarge i, e))
    val findi = fn f => fn v => 
        Option.map (fn(i, e) => (FixedInt.toLarge i, e)) (findi(fn (i, e) => f(FixedInt.toLarge i, e)) v)
    val modifyi = fn f => modifyi(fn (i, e) => f(FixedInt.toLarge i, e))
end;

structure CharArray: MONO_ARRAY = MapMonoArray(structure Old = CharArray);
structure BoolArray: MONO_ARRAY = MapMonoArray(structure Old = BoolArray);
structure RealArray: MONO_ARRAY = MapMonoArray(structure Old = RealArray);
structure Word8Array: MONO_ARRAY = MapMonoArray(structure Old = Word8Array);

use "basis/ArraySignature.sml";

structure Array: ARRAY =
struct
    open Array
    val maxLen = FixedInt.toLarge maxLen
    val array = fn (i, a) => array(FixedInt.fromLarge i, a)
    val sub = fn (v, i) => sub(v, FixedInt.fromLarge i)
    val length = fn l => FixedInt.toLarge(length l)
    val update = fn (v, i, e) => update(v, FixedInt.fromLarge i, e)
    val copy = fn {di, dst, src} => copy {di=FixedInt.fromLarge di, dst=dst, src=src}
    val copyVec = fn {di, dst, src} => copyVec {di=FixedInt.fromLarge di, dst=dst, src=src}
    val foldri = fn f => foldri (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val foldli = fn f => foldli (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val tabulate = fn (n, f) => tabulate(FixedInt.fromInt n, fn q => f(FixedInt.toInt q))
    val appi = fn f => appi(fn (i, e) => f(FixedInt.toLarge i, e))
    val findi = fn f => fn v => 
        Option.map (fn(i, e) => (FixedInt.toLarge i, e)) (findi(fn (i, e) => f(FixedInt.toLarge i, e)) v)
    val modifyi = fn f => modifyi(fn (i, e) => f(FixedInt.toLarge i, e))
end;

use "basis/MONO_VECTOR_SLICE";

functor MapMonoVectorSlice(
    structure Old:
    sig
        type vector
        type elem
        type slice
    
        val length : slice -> FixedInt.int
        val sub : (slice * FixedInt.int) -> elem
        val full: vector -> slice
        val slice: vector * FixedInt.int * FixedInt.int option -> slice
        val subslice: slice * FixedInt.int * FixedInt.int option -> slice
        val base: slice -> vector * FixedInt.int * FixedInt.int
        val vector: slice -> vector
        val concat: slice list -> vector
        val isEmpty: slice -> bool
        val getItem: slice -> (elem * slice) option
        val appi : ((FixedInt.int * elem) -> unit) -> slice -> unit
        val app : (elem -> unit) -> slice -> unit
        val mapi : ((FixedInt.int * elem) -> elem) -> slice -> vector
        val map : (elem -> elem) -> slice -> vector
        val foldli : ((FixedInt.int * elem * 'a) -> 'a) -> 'a -> slice -> 'a
        val foldri : ((FixedInt.int * elem * 'a) -> 'a) -> 'a -> slice -> 'a
        val foldl : ((elem * 'a) -> 'a) -> 'a -> slice -> 'a
        val foldr : ((elem * 'a) -> 'a) -> 'a -> slice -> 'a
        val findi: (FixedInt.int * elem -> bool) -> slice -> (FixedInt.int * elem) option
        val find: (elem -> bool) -> slice -> elem option
        val exists: (elem -> bool) -> slice -> bool
        val all: (elem -> bool) -> slice -> bool
        val collate: (elem * elem -> order) -> slice * slice -> order
    end
): MONO_VECTOR_SLICE =
struct
    open Old
    val sub = fn (v, i) => sub(v, FixedInt.fromLarge i)
    val length = FixedInt.toLarge o length
    val base = fn s => let val (a, i, j) = base s in (a, FixedInt.toInt i, FixedInt.toInt j) end
    val slice = fn (v, i, l) => slice(v, FixedInt.fromLarge i, Option.map FixedInt.fromInt l)
    val subslice = fn (v, i, l) => subslice(v, FixedInt.fromLarge i, Option.map FixedInt.fromInt l)
    val foldri = fn f => foldri (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val foldli = fn f => foldli (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val appi = fn f => appi(fn (i, e) => f(FixedInt.toLarge i, e))
    val mapi = fn f => mapi(fn (i, e) => f(FixedInt.toLarge i, e))
    val findi = fn f => fn v => 
        Option.map (fn(i, e) => (FixedInt.toLarge i, e)) (findi(fn (i, e) => f(FixedInt.toLarge i, e)) v)
end;

structure CharVectorSlice: MONO_VECTOR_SLICE = MapMonoVectorSlice(structure Old = CharVectorSlice);
structure RealVectorSlice: MONO_VECTOR_SLICE = MapMonoVectorSlice(structure Old = RealVectorSlice);
structure Word8VectorSlice: MONO_VECTOR_SLICE = MapMonoVectorSlice(structure Old = Word8VectorSlice);

use "basis/VectorSliceSignature.sml";

structure VectorSlice: VECTOR_SLICE =
struct
    open VectorSlice
    val sub = fn (v, i) => sub(v, FixedInt.fromLarge i)
    val length = fn l => FixedInt.toLarge(length l)
    val base = fn s => let val (a, i, j) = base s in (a, FixedInt.toInt i, FixedInt.toInt j) end
    val slice = fn (v, i, l) => slice(v, FixedInt.fromLarge i, Option.map FixedInt.fromInt l)
    val subslice = fn (v, i, l) => subslice(v, FixedInt.fromLarge i, Option.map FixedInt.fromInt l)
    val foldri = fn f => foldri (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val foldli = fn f => foldli (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val appi = fn f => appi(fn (i, e) => f(FixedInt.toLarge i, e))
    val mapi = fn f => mapi(fn (i, e) => f(FixedInt.toLarge i, e))
    val findi = fn f => fn v => 
        Option.map (fn(i, e) => (FixedInt.toLarge i, e)) (findi(fn (i, e) => f(FixedInt.toLarge i, e)) v)
end;


use "basis/MONO_ARRAY_SLICE";

functor MapMonoArraySlice(
    structure Old:
    sig
        type elem
        type array
        type slice
        type vector
        type vector_slice
    
        val length : slice -> FixedInt.int
        val sub : (slice * FixedInt.int) -> elem
        val update: slice * FixedInt.int * elem -> unit
        val full: array -> slice
        val slice: array * FixedInt.int * FixedInt.int option -> slice
        val subslice: slice * FixedInt.int * FixedInt.int option -> slice
        val base: slice -> array * FixedInt.int * FixedInt.int
        val vector: slice -> vector
        val copy : {src : slice, dst : array, di : FixedInt.int} -> unit
        val copyVec : {src : vector_slice, dst : array, di : FixedInt.int} -> unit
        val isEmpty: slice -> bool
        val getItem: slice -> (elem * slice) option
        val appi : ((FixedInt.int * elem) -> unit) -> slice -> unit
        val app : (elem -> unit) -> slice -> unit
        val modifyi : (FixedInt.int * elem -> elem) -> slice -> unit
        val modify : (elem -> elem) -> slice -> unit
        val foldli : ((FixedInt.int * elem * 'a) -> 'a) -> 'a -> slice -> 'a
        val foldri : ((FixedInt.int * elem * 'a) -> 'a) -> 'a -> slice -> 'a
        val foldl : ((elem * 'a) -> 'a) -> 'a -> slice -> 'a
        val foldr : ((elem * 'a) -> 'a) -> 'a -> slice -> 'a
        val findi: (FixedInt.int * elem -> bool) -> slice -> (FixedInt.int * elem) option
        val find: (elem -> bool) -> slice -> elem option
        val exists: (elem -> bool) -> slice -> bool
        val all: (elem -> bool) -> slice -> bool
        val collate: (elem * elem -> order) -> slice * slice -> order
    end
): MONO_ARRAY_SLICE =
struct
    open Old
    val sub = fn (v, i) => sub(v, FixedInt.fromLarge i)
    val length = FixedInt.toLarge o length
    val update = fn (v, i, e) => update(v, FixedInt.fromLarge i, e)
    val copy = fn {di, dst, src} => copy {di=FixedInt.fromLarge di, dst=dst, src=src}
    val copyVec = fn {di, dst, src} => copyVec {di=FixedInt.fromLarge di, dst=dst, src=src}
    val base = fn s => let val (a, i, j) = base s in (a, FixedInt.toInt i, FixedInt.toInt j) end
    val slice = fn (v, i, l) => slice(v, FixedInt.fromLarge i, Option.map FixedInt.fromInt l)
    val subslice = fn (v, i, l) => subslice(v, FixedInt.fromLarge i, Option.map FixedInt.fromInt l)
    val foldri = fn f => foldri (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val foldli = fn f => foldli (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val appi = fn f => appi(fn (i, e) => f(FixedInt.toLarge i, e))
    val findi = fn f => fn v => 
        Option.map (fn(i, e) => (FixedInt.toLarge i, e)) (findi(fn (i, e) => f(FixedInt.toLarge i, e)) v)
    val modifyi = fn f => modifyi(fn (i, e) => f(FixedInt.toLarge i, e))
end;

structure CharArraySlice: MONO_ARRAY_SLICE = MapMonoArraySlice(structure Old = CharArraySlice);
structure RealArraySlice: MONO_ARRAY_SLICE = MapMonoArraySlice(structure Old = RealArraySlice);
structure Word8ArraySlice: MONO_ARRAY_SLICE = MapMonoArraySlice(structure Old = Word8ArraySlice);

use "basis/ArraySliceSignature.sml"; (* Depends on VectorSlice. *)

structure ArraySlice: ARRAY_SLICE =
struct
    open ArraySlice
    val sub = fn (v, i) => sub(v, FixedInt.fromLarge i)
    val length = fn l => FixedInt.toLarge(length l)
    val update = fn (v, i, e) => update(v, FixedInt.fromLarge i, e)
    val copy = fn {di, dst, src} => copy {di=FixedInt.fromLarge di, dst=dst, src=src}
    val copyVec = fn {di, dst, src} => copyVec {di=FixedInt.fromLarge di, dst=dst, src=src}
    val base = fn s => let val (a, i, j) = base s in (a, FixedInt.toInt i, FixedInt.toInt j) end
    val slice = fn (v, i, l) => slice(v, FixedInt.fromLarge i, Option.map FixedInt.fromInt l)
    val subslice = fn (v, i, l) => subslice(v, FixedInt.fromLarge i, Option.map FixedInt.fromInt l)
    val foldri = fn f => foldri (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val foldli = fn f => foldli (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val appi = fn f => appi(fn (i, e) => f(FixedInt.toLarge i, e))
    val findi = fn f => fn v => 
        Option.map (fn(i, e) => (FixedInt.toLarge i, e)) (findi(fn (i, e) => f(FixedInt.toLarge i, e)) v)
    val modifyi = fn f => modifyi(fn (i, e) => f(FixedInt.toLarge i, e))
end;

(* Rebuild IntVector etc.  They are defined in terms of polymorphic vector, array etc
   and also we want the "Int" to be arbitrary precision. *)
use "basis/IntArray";

use "basis/Text"; (* Rebuild the Text structure that includes Char, String etc. *)

(* Array2 *)
use "basis/Array2Signature.sml";

structure Array2: ARRAY2 =
struct
    open Array2
    type 'a region =
    {
        base : 'a array,
        row : int,
        col : int,
        nrows : int option,
        ncols : int option
    }
    val array = fn(i, j, a) => array(FixedInt.fromLarge i, FixedInt.fromLarge j, a)
    val sub = fn (a, i, j) => sub(a, FixedInt.fromLarge i, FixedInt.fromLarge j)
    val update = fn (a, i, j, v) => update(a, FixedInt.fromLarge i, FixedInt.fromLarge j, v)
    val tabulate =
        fn t =>
            fn (i, j, f) =>
                tabulate t (FixedInt.fromLarge i, FixedInt.fromLarge j, fn (q, r) => f(FixedInt.toLarge q, FixedInt.toLarge r) )
    val row = fn (a, i) => row(a, FixedInt.fromLarge i)
    and column = fn (a, i) => row(a, FixedInt.fromLarge i)
    val dimensions = fn a => let val (x, y) = dimensions a in (FixedInt.toLarge x, FixedInt.toLarge y) end
    val nCols = fn l => FixedInt.toLarge(nCols l)
    val nRows = fn l => FixedInt.toLarge(nRows l)
    
    local
        fun mapRegion{base, row, col, nrows, ncols} =
            {base = base, row = FixedInt.fromLarge row, col = FixedInt.fromLarge col,
             nrows = Option.map FixedInt.fromLarge nrows,
             ncols = Option.map FixedInt.fromLarge ncols }

        fun modifyi' t f r =
            modifyi t
                (fn (i, j, v) => f(FixedInt.toLarge i, FixedInt.toLarge j, v))
                (mapRegion r)
        and foldi' t f b r =
            foldi t
                (fn (i, j, a, b) => f(FixedInt.toLarge i, FixedInt.toLarge j, a, b))
                b (mapRegion r)
        and appi' t f r =
            appi t
                (fn (i, j, v) => f(FixedInt.toLarge i, FixedInt.toLarge j, v))
                (mapRegion r)
        and copy' {src, dst, dst_row, dst_col} =
            copy {src = mapRegion src, dst = dst, dst_row = FixedInt.fromLarge dst_row, dst_col = FixedInt.fromLarge dst_col }
    in
        val modifyi = modifyi'
        and foldi = foldi'
        and appi = appi'
        and copy = copy'
    end
end;

(* Monomorphic two dimensional arrays.  They are defined in terms of Array2 and also
   for IntArray2 we want the base type to be arbitrary precision. *)
use "basis/IntArray2.sml";

(* Word, LargeWord etc. *)
use "basis/WordSignature.sml"; (* This depends on Word, Int, etc but the dependencies on Word don't affect this. *)

structure Word: WORD =
struct
    open Word
    val wordSize = FixedInt.toLarge wordSize
    val fromInt = fromLargeInt
    and toInt = toLargeInt
    and toIntX = toLargeIntX
end;

structure LargeWord: WORD =
struct
    open LargeWord
    val wordSize = FixedInt.toLarge wordSize
    val fromInt = fromLargeInt
    and toInt = toLargeInt
    and toIntX = toLargeIntX
end;

structure SysWord: WORD =
struct
    open SysWord
    val wordSize = FixedInt.toLarge wordSize
    val fromInt = fromLargeInt
    and toInt = toLargeInt
    and toIntX = toLargeIntX
end;


structure Word32: WORD =
struct
    open Word32
    val wordSize = FixedInt.toLarge wordSize
    val fromInt = fromLargeInt
    and toInt = toLargeInt
    and toIntX = toLargeIntX
end;

structure Word8: WORD =
struct
    open Word8
    val wordSize = FixedInt.toLarge wordSize
    val fromInt = fromLargeInt
    and toInt = toLargeInt
    and toIntX = toLargeIntX
end;

(* TODO: Word64  This requires a conditional call to "use". *)


(* Real *)
use "basis/IEEE_REAL.sml";
structure IEEEReal: IEEE_REAL =
struct
    open IEEEReal
    type decimal_approx =
        { class : float_class, sign : bool, digits : int list, exp : int }
    
    local
        fun toNewDA {class, sign, digits, exp } : decimal_approx =
            {class=class, sign=sign, digits = map FixedInt.toLarge digits, exp = FixedInt.toLarge exp }
        and fromNewDA ({class, sign, digits, exp } : decimal_approx) =
            {class=class, sign=sign, digits = map FixedInt.fromLarge digits, exp = FixedInt.fromLarge exp }
    in
        val toString = toString o fromNewDA 
        val scan = fn getc => fn src => Option.map(fn (v, c) => (toNewDA v, c)) (scan getc src)
        and fromString = (Option.map toNewDA) o fromString
    end
end;

(* There's a complication.  We need access to both the old and new versions of
   the StringCvt.realfmt datatype. *)
local
    structure OldStringCvt = StringCvt
in
    structure StringCvt: STRING_CVT =
    struct
        open StringCvt

        datatype realfmt
          = SCI of int option
          | FIX of int option
          | GEN of int option
          | EXACT

        val padRight = fn c => fn i => padRight c (FixedInt.fromInt i)
        and padLeft  = fn c => fn i => padLeft c (FixedInt.fromInt i)
    end;

    structure Real =
    struct
        open Real
        val radix = FixedInt.toLarge radix
        val precision = FixedInt.toLarge precision
        val sign = FixedInt.toLarge o sign
        val toManExp = fn r => let val {man, exp} = toManExp r in {man=man, exp= FixedInt.toLarge exp} end
        and fromManExp = fn {man, exp} => fromManExp{man=man, exp=FixedInt.fromLarge exp }
        val toInt = toLargeInt
        and fromInt = fromLargeInt

        val floor = toLargeInt IEEEReal.TO_NEGINF
        and ceil = toLargeInt IEEEReal.TO_POSINF
        and trunc = toLargeInt IEEEReal.TO_ZERO
        and round = toLargeInt IEEEReal.TO_NEAREST

        val toDecimal =
            fn r =>
                let
                    val {class, sign, digits, exp } = toDecimal r
                in
                    {class=class, sign=sign, digits = map FixedInt.toLarge digits, exp = FixedInt.toLarge exp }
                end
    
        val fromDecimal =
            fn {class, sign, digits, exp } =>
                fromDecimal {class=class, sign=sign, digits = map FixedInt.fromLarge digits, exp = FixedInt.fromLarge exp }

        local
            fun rfmt (StringCvt.SCI(SOME s)) r = fmt (OldStringCvt.SCI(SOME(FixedInt.fromLarge s))) r
            |   rfmt (StringCvt.SCI NONE) r = fmt (OldStringCvt.SCI NONE) r
            |   rfmt (StringCvt.FIX(SOME s)) r = fmt (OldStringCvt.FIX(SOME(FixedInt.fromLarge s))) r
            |   rfmt (StringCvt.FIX NONE) r = fmt (OldStringCvt.FIX NONE) r
            |   rfmt (StringCvt.GEN(SOME s)) r = fmt (OldStringCvt.GEN(SOME(FixedInt.fromLarge s))) r
            |   rfmt (StringCvt.GEN NONE) r = fmt (OldStringCvt.GEN NONE) r
            |   rfmt StringCvt.EXACT r = fmt OldStringCvt.EXACT r
        in
            val fmt = rfmt
        end
    end
end;

use "basis/RealSignature.sml"; (* This uses IEEEReal and the new StringCvt and decimal_approx *)
structure Real: REAL = Real;
structure LargeReal = Real;

val real : int -> real = Real.fromInt 
val trunc : real -> int = Real.trunc 
val floor : real -> int = Real.floor 
val ceil : real -> int = Real.ceil 
val round : real -> int =Real.round;

(* Date . *)
use "basis/DateSignature";
structure Date: DATE =
struct
    open Date
    val date =
        fn { year, month, day, hour, minute, second, offset } =>
            date {year=FixedInt.fromLarge year, month=month, day=FixedInt.fromLarge day,
                  hour=FixedInt.fromLarge hour, minute=FixedInt.fromLarge minute,
                  second=FixedInt.fromLarge second, offset=offset}
    val year = FixedInt.toLarge o year
    and day  = FixedInt.toLarge o day
    and hour = FixedInt.toLarge o hour
    and minute = FixedInt.toLarge o minute
    and second = FixedInt.toLarge o second
    and yearDay = FixedInt.toLarge o yearDay
end;

(* Thread ? *)

(* IO *)
(* This is much simpler if we ignore PrimIO. *)

use "basis/STREAM_IO.sml";
use "basis/IMPERATIVE_IO.sml";

signature TEXT_STREAM_IO =
sig
    include STREAM_IO
    where type vector = CharVector.vector
    where type elem = Char.char

    val inputLine : instream -> (string * instream) option
    val outputSubstr : outstream * Substring.substring -> unit
end;

signature TEXT_IO = sig
    (* include IMPERATIVE_IO *)
    structure StreamIO : TEXT_STREAM_IO
        where type reader = TextPrimIO.reader
        where type writer = TextPrimIO.writer
        where type pos = TextPrimIO.pos

    type vector = StreamIO.vector
    type elem = StreamIO.elem

    type instream
    type outstream

    val input : instream -> vector
    val input1 : instream -> elem option
    val inputN : instream * int -> vector
    val inputAll : instream -> vector
    val canInput : instream * int -> int option
    val lookahead : instream -> elem option
    val closeIn : instream -> unit
    val endOfStream : instream -> bool
    val output : outstream * vector -> unit
    val output1 : outstream * elem -> unit
    val flushOut : outstream -> unit
    val closeOut : outstream -> unit
    val mkInstream : StreamIO.instream -> instream
    val getInstream : instream -> StreamIO.instream
    val setInstream : instream * StreamIO.instream -> unit
    val mkOutstream : StreamIO.outstream -> outstream
    val getOutstream : outstream -> StreamIO.outstream
    val setOutstream : outstream * StreamIO.outstream -> unit
    val getPosOut : outstream -> StreamIO.out_pos
    val setPosOut : outstream * StreamIO.out_pos -> unit
    (* End of include IMPERATIVE_IO *)

    val inputLine : instream -> string option
    val outputSubstr : outstream * Substring.substring -> unit
    val openIn  : string -> instream
    val openOut : string -> outstream
    val openAppend : string -> outstream
    val openString : string -> instream

    val stdIn  : instream
    val stdOut : outstream
    val stdErr : outstream

    val print : string -> unit
    val scanStream : ((Char.char, StreamIO.instream) StringCvt.reader
                      -> ('a, StreamIO.instream) StringCvt.reader)
                      -> instream -> 'a option
end;

structure TextIO: TEXT_IO =
struct
    open TextIO
    val canInput =
        fn (s, n) => Option.map FixedInt.toLarge (canInput(s, FixedInt.fromLarge n))
    val inputN =
        fn (s, n) => inputN(s, FixedInt.fromLarge n)
    structure StreamIO =
    struct
        open StreamIO
        val canInput =
            fn (s, n) => Option.map FixedInt.toLarge (canInput(s, FixedInt.fromLarge n))
        val inputN =
            fn (s, n) => inputN(s, FixedInt.fromLarge n)
    end
end;

signature BIN_IO =
sig
    include IMPERATIVE_IO
       where type StreamIO.vector = Word8Vector.vector
       where type StreamIO.elem = Word8.word
       where type StreamIO.reader = BinPrimIO.reader
       where type StreamIO.writer = BinPrimIO.writer
       where type StreamIO.pos = BinPrimIO.pos

    val openIn  : string -> instream
    val openOut : string -> outstream
    val openAppend : string -> outstream
end;

structure BinIO: BIN_IO =
struct
    open BinIO
    val canInput =
        fn (s, n) => Option.map FixedInt.toLarge (canInput(s, FixedInt.fromLarge n))
    val inputN =
        fn (s, n) => inputN(s, FixedInt.fromLarge n)
    structure StreamIO =
    struct
        open StreamIO
        val canInput =
            fn (s, n) => Option.map FixedInt.toLarge (canInput(s, FixedInt.fromLarge n))
        val inputN =
            fn (s, n) => inputN(s, FixedInt.fromLarge n)
    end
end;


(* Socket *)

(* RunCall/RuntimeCalls - including this allows the compiler to build. *)
structure RunCall =
struct
    open RunCall
    val run_call0 = fn n => run_call0 (FixedInt.fromLarge n)
    and run_call1 = fn n => run_call1 (FixedInt.fromLarge n)
    and run_call2 = fn n => run_call2 (FixedInt.fromLarge n)
    and run_call3 = fn n => run_call3 (FixedInt.fromLarge n)
    and run_call4 = fn n => run_call4 (FixedInt.fromLarge n)
    and run_call5 = fn n => run_call5 (FixedInt.fromLarge n)
    and run_call2C2 = fn n => run_call2C2 (FixedInt.fromLarge n)
end;

use "basis/RuntimeCalls";
