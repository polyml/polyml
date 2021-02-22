(*
    Title:      Standard Basis Library: Pack Real32 structures for 64-bit platforms
    Author:     David Matthews
    Copyright   David Matthews 2021

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    Licence version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public Licence for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

(* On 64-bit platforms Real32.real values are held as tagged values
   with the floating point value in the high-order 32-bits. *)

local
    val r32AsWord: Real32.real -> word = RunCall.unsafeCast
    and wordAsR32: word -> Real32.real = RunCall.unsafeCast
    
    infix << >>
    infix orb
    val op orb = Word.orb
    and op << = Word.<<
    and op >> = Word.>>

    val wordToW8 = Word8.fromLarge o Word.toLarge
    and w8ToWord = Word.fromLarge o Word8.toLarge
    
    (* Word values are already shifted by one bit so the
       actual shifts are 31, 39, 47 and 55 bits. *)
    val shift0 = Word.fromInt Word.wordSize - 0w32
    and shift1 = Word.fromInt Word.wordSize - 0w24
    and shift2 = Word.fromInt Word.wordSize - 0w16
    and shift3 = Word.fromInt Word.wordSize - 0w08
in

    structure PackReal32Little: PACK_REAL =
    struct
        type real = Real32.real
        val bytesPerElem = 4
        val isBigEndian = false
        
        fun toBytes r =
        let
            val w: word = r32AsWord r
        in
            Word8Vector.fromList[wordToW8(w >> shift0), wordToW8(w >> shift1),
                    wordToW8(w >> shift2), wordToW8(w >> shift3)]
        end

        fun subVec(v, i) =
            wordAsR32((w8ToWord(Word8Vector.sub(v, i)) << shift0) orb
                (w8ToWord(Word8Vector.sub(v, i+1)) << shift1) orb
                (w8ToWord(Word8Vector.sub(v, i+2)) << shift2) orb
                (w8ToWord(Word8Vector.sub(v, i+3)) << shift3))

        fun fromBytes v = subVec(v, 0)

        fun subArr(v, i) =
            wordAsR32((w8ToWord(Word8Array.sub(v, i)) << shift0) orb
                (w8ToWord(Word8Array.sub(v, i+1)) << shift1) orb
                (w8ToWord(Word8Array.sub(v, i+2)) << shift2) orb
                (w8ToWord(Word8Array.sub(v, i+3)) << shift3))

        fun update(v, i, r) =
        let
            val w: word = r32AsWord r
            open Word8Array
        in
            Word8Array.update(v, i, wordToW8(w >> shift0));
            Word8Array.update(v, i+1, wordToW8(w >> shift1));
            Word8Array.update(v, i+2, wordToW8(w >> shift2));
            Word8Array.update(v, i+3, wordToW8(w >> shift3))
        end
    end

    structure PackReal32Big: PACK_REAL =
    struct
        type real = Real32.real
        val bytesPerElem = 4
        val isBigEndian = false
        
        fun toBytes r =
        let
            val w: word = r32AsWord r
        in
            Word8Vector.fromList[wordToW8(w >> shift3), wordToW8(w >> shift2),
                wordToW8(w >> shift1), wordToW8(w >> shift0)]
        end

        fun subVec(v, i) =
            wordAsR32((w8ToWord(Word8Vector.sub(v, i)) << shift3) orb
                (w8ToWord(Word8Vector.sub(v, i+1)) << shift2) orb
                (w8ToWord(Word8Vector.sub(v, i+2)) << shift1) orb
                (w8ToWord(Word8Vector.sub(v, i+3)) << shift0))

        fun fromBytes v = subVec(v, 0)

        fun subArr(v, i) =
            wordAsR32((w8ToWord(Word8Array.sub(v, i)) << shift3) orb
                (w8ToWord(Word8Array.sub(v, i+1)) << shift2) orb
                (w8ToWord(Word8Array.sub(v, i+2)) << shift1) orb
                (w8ToWord(Word8Array.sub(v, i+3)) << shift0))

        fun update(v, i, r) =
        let
            val w: word = r32AsWord r
            open Word8Array
        in
            Word8Array.update(v, i, wordToW8(w >> shift3));
            Word8Array.update(v, i+1, wordToW8(w >> shift2));
            Word8Array.update(v, i+2, wordToW8(w >> shift1));
            Word8Array.update(v, i+3, wordToW8(w >> shift0))
        end
    end

end;
