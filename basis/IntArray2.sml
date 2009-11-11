(*
    Title:      Standard Basis Library: Mono_Array2 signature and structures.
    Author:     David Matthews
    Copyright   David Matthews 2000

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

(* G&R 2004 status: no change. *)

signature MONO_ARRAY2 =
sig
    eqtype array
    type elem
    type vector
    type region = {
                 base : array,
                 row : int,
                 col : int,
                 nrows : int option,
                 ncols : int option
               }
    datatype traversal = datatype Array2.traversal
    val array : int * int * elem -> array
    val fromList : elem list list -> array
    val tabulate : traversal -> int * int * (int * int -> elem) -> array
    val sub : array * int * int -> elem
    val update : array * int * int * elem -> unit
    val dimensions : array -> int * int
    val nCols      : array -> int
    val nRows      : array -> int
    val row: array * int -> vector
    val column: array * int -> vector
    val copy: {
                src : region,
                dst : array,
                dst_row : int,
                dst_col : int
              } -> unit
    val appi: Array2.traversal -> (int * int * elem -> unit) -> region -> unit
    val app: Array2.traversal -> (elem -> unit) -> array -> unit
    val modifyi: Array2.traversal -> (int * int * elem -> elem) -> region -> unit
    val modify: Array2.traversal -> (elem -> elem) -> array -> unit
    val foldi: Array2.traversal -> (int * int * elem * 'b -> 'b) -> 'b -> region -> 'b
    val fold: Array2.traversal -> (elem * 'b -> 'b) -> 'b -> array -> 'b
end;

(* All these are simply defined in terms of Array2, at least for the
   moment. *)
structure Word8Array2 : MONO_ARRAY2
    where type vector = Word8Vector.vector
    where type elem = Word8.word =
struct
    open Array2
    type elem = Word8.word
    type vector = Word8Vector.vector
    type elem = Word8.word
    type array = elem array
    type region = elem region
    (* Have to redefine row and column in terms of Word8Vector. *)
    fun row(a, i) =
        Word8Vector.tabulate(nCols a, fn j => sub(a, i, j))
    and column(a, j) =
        Word8Vector.tabulate(nRows a, fn i => sub(a, i, j))
end;

structure CharArray2 : MONO_ARRAY2
    where type vector = CharVector.vector
    where type elem = char =
struct
    open Array2
    type elem = char
    type vector = CharVector.vector
    type array = elem array
    type region = elem region
    fun row(a, i) =
        CharVector.tabulate(nCols a, fn j => sub(a, i, j))
    and column(a, j) =
        CharVector.tabulate(nRows a, fn i => sub(a, i, j))
end;

structure BoolArray2 : MONO_ARRAY2
    where type vector = BoolVector.vector
    where type elem = bool =
struct
    open Array2
    type elem = bool
    type vector = BoolVector.vector
    type array = elem array
    type region = elem region
    fun row(a, i) =
        BoolVector.tabulate(nCols a, fn j => sub(a, i, j))
    and column(a, j) =
        BoolVector.tabulate(nRows a, fn i => sub(a, i, j))
end;

structure IntArray2 : MONO_ARRAY2
    where type vector = IntVector.vector
    where type elem = int =
struct
    open Array2
    type elem = int
    type vector = IntVector.vector
    type array = elem array
    type region = elem region
    fun row(a, i) =
        IntVector.tabulate(nCols a, fn j => sub(a, i, j))
    and column(a, j) =
        IntVector.tabulate(nRows a, fn i => sub(a, i, j))
end;

structure RealArray2 : MONO_ARRAY2
    where type vector = RealVector.vector
    where type elem = real =
struct
    open Array2
    type elem = real
    type vector = RealVector.vector
    type array = elem array
    type region = elem region
    fun row(a, i) =
        RealVector.tabulate(nCols a, fn j => sub(a, i, j))
    and column(a, j) =
        RealVector.tabulate(nRows a, fn i => sub(a, i, j))
end;
