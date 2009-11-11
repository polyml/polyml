(*
    Title:      Standard Basis Library: Text Signature and Structure
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

(* G&R 2004 status: updated to include CharVectorSlice and CharArraySlice. *)

signature TEXT =
sig
     structure Char : CHAR
     structure String : STRING
     structure Substring : SUBSTRING
     structure CharVector : MONO_VECTOR
     structure CharArray : MONO_ARRAY
     structure CharVectorSlice: MONO_VECTOR_SLICE
     structure CharArraySlice : MONO_ARRAY_SLICE
     sharing type Char.char = String.char = Substring.char
       = CharVector.elem = CharArray.elem = CharVectorSlice.elem = CharArraySlice.elem
     sharing type Char.string = String.string = Substring.string
       = CharVector.vector = CharArray.vector = CharVectorSlice.vector = CharArraySlice.vector
     sharing type CharArray.array = CharArraySlice.array
     sharing type CharVectorSlice.slice = CharArraySlice.vector_slice
end;

structure Text : TEXT =
struct
     structure Char = Char
     structure String = String
     structure Substring = Substring
     structure CharVector = CharVector
     structure CharArray = CharArray
     structure CharVectorSlice = CharVectorSlice
     structure CharArraySlice = CharArraySlice
end;
