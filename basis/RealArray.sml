(*
    Title:      Standard Basis Library: RealArray and RealVector Structures
    Author:     David Matthews
    Copyright   David Matthews 2000, 2005

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

(* G&R 2004 status: done.  Added sliced versions. *)

(* TODO: This stores the vectors as vectors of pointers to cells
   containing the real values.  It would be more efficient to store
   numbers directly in a byte-vector.  We could use the move_bytes
   function to load and store reals although it would probably be
   better to define RTS functions to load and store reals. *)
structure RealVector: MONO_VECTOR =
struct
    open Vector
    type vector = real vector
    and  elem = real
end;

structure RealArray: MONO_ARRAY =
struct
    open Array
    type vector = RealVector.vector
    and  array = real array
    and  elem = real
end;

structure RealVectorSlice: MONO_VECTOR_SLICE =
struct
    open VectorSlice
    type slice = real slice
    and  elem = real
    and  vector = RealVector.vector
end;

structure RealArraySlice: MONO_ARRAY_SLICE =
struct
    open ArraySlice
    type slice = real slice
    and  vector = RealVector.vector
    and  array = RealArray.array
    and  elem = real
    and  vector_slice = RealVectorSlice.slice
end;
