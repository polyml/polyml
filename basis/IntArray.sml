(*
    Title:      Standard Basis Library: IntArray and IntVector Structures
    Author:     David Matthews
    Copyright   David Matthews 1999-2005

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

(* G&R 2004 status: no change. Added slice versions. *)
structure IntVector: MONO_VECTOR =
struct
    open Vector
    type vector = int vector
    and  elem = int
end;

structure IntArray: MONO_ARRAY =
struct
    open Array
    type vector = int vector
    and  array = int array
    and  elem = int
end;

structure IntVectorSlice: MONO_VECTOR_SLICE =
struct
    open VectorSlice
    type slice = int slice
    and vector = IntVector.vector
    and elem = int
end;

structure IntArraySlice: MONO_ARRAY_SLICE =
struct
    open ArraySlice
    type slice = int slice
    and vector = IntVector.vector
    and array = IntArray.array
    and elem = int
    and vector_slice = IntVectorSlice.slice
end;
