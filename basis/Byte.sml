(*
    Title:      Standard Basis Library: Byte Structure and signature
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
    under the terms of that licence.
*)

(* G&R 2004 status: Complete. *)

signature BYTE =
sig
    val byteToChar : Word8.word -> char
    val charToByte : char -> Word8.word
    val bytesToString : Word8Vector.vector -> string
    val stringToBytes : string -> Word8Vector.vector
    val unpackStringVec : Word8VectorSlice.slice -> string
    val unpackString : Word8ArraySlice.slice -> string
    val packString : Word8Array.array * int * Substring.substring -> unit
end;

structure Byte: BYTE =
    struct
        (* Chars and Word8.word values are both tagged integers in the
           range 0..255. *)
        fun byteToChar (w: Word8.word): char = RunCall.unsafeCast w
        fun charToByte (c: char) : Word8.word = RunCall.unsafeCast c

        (* Conversion between Word8Vector.vector and string is just a cast. *)
        fun bytesToString (LibrarySupport.Word8Array.Vector v) : string = v
        fun stringToBytes (s: string) : Word8Vector.vector = LibrarySupport.Word8Array.Vector s

        fun unpackStringVec slice : string = bytesToString(Word8VectorSlice.vector slice)
        fun unpackString slice : string = bytesToString(Word8ArraySlice.vector slice)

        fun packString(array: Word8Array.array, i, s: substring) =
        let
            val (str, offset, size) = Substring.base s
            val slice = Word8VectorSlice.slice(stringToBytes str, offset, SOME size)
        in
            Word8ArraySlice.copyVec{src=slice, dst=array, di=i}
        end
    end;

