(*
    Title:      Standard Basis Library: Primitive Binary IO
    Copyright   David C.J. Matthews 2000, 2005

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

(* G&R 2004 status: Minor changes. I've included sharing constraints on the
   slice types which aren't in the book. *)

structure BinPrimIO :> PRIM_IO
    where type vector = Word8Vector.vector
    where type elem = Word8.word
    where type array = Word8Array.array
    where type pos = Position.int
    where type vector_slice = Word8VectorSlice.slice
    where type array_slice = Word8ArraySlice.slice
 =
    PrimIO (
        structure Array : MONO_ARRAY = Word8Array
        structure Vector : MONO_VECTOR = Word8Vector
        structure VectorSlice = Word8VectorSlice
        structure ArraySlice = Word8ArraySlice
        val someElem : Vector.elem = 0wx20 (* Initialise to spaces. *)
        (* BinPrimIO.pos is defined to be Position.int *)
        type pos = Position.int
        val compare = Position.compare
    );

