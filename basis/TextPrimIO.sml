(*
    Title:      Standard Basis Library: Primitive Text IO
    Copyright   David C.J. Matthews 2000

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


(* QUESTION: What is the relationship between TextPrimIO and TextIO?
   Is TextIO.StreamIO.reader supposed to be the same as TextPrimIO.reader
   and similarly for writer? Assume yes.  Yes, in G&R these are explicit*)
structure TextPrimIO :>
sig
    include PRIM_IO
    where type vector = CharVector.vector
    where type elem = Char.char
    where type array = CharArray.array
    where type pos = Position.int
    where type vector_slice = CharVectorSlice.slice
    where type array_slice = CharArraySlice.slice
end =
    PrimIO (
        structure Array = CharArray
        structure Vector = CharVector
        structure ArraySlice = CharArraySlice
        structure VectorSlice = CharVectorSlice
        val someElem : Array.elem = #" " (* Initialise to spaces. *)
        (* BinPrimIO.pos is defined to be Position.int.  TextPrimIO.pos
           may be abstract.  It's very convenient to be able to build
           additional readers and that requires pos to be non-abstract. *)
        type pos = Position.int
        val compare = Position.compare
    );
