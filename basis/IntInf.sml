(*
    Title:      Standard Basis Library: IntInf structure and signature.
    Copyright   David Matthews 2000, 2016-17

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

structure IntInf : INT_INF =
struct
    type int = LargeInt.int
    
    val quotRem = LibrarySupport.quotRem

    fun divMod (x, y) =
    let
        val (q, r) = quotRem(x, y)
    in
        (* If the remainder is zero or the same sign as the
           divisor then the result is the same as quotRem.
           Otherwise round down the quotient and round up the remainder. *)
        if r = 0 orelse (r < 0) = (y < 0)
        then (q, r)
        else (q-1, r+y)
    end

    (* Return the position of the highest bit set in the value. *)
    local
        val isShort: int -> bool = RunCall.isShort
        fun loadByte(l: LargeInt.int, i: Int.int):word = RunCall.loadByteFromImmutable(l, Word.fromInt i)
        val segLength: LargeInt.int -> Int.int = Word.toInt o RunCall.memoryCellLength

        (* Compute log2 for a short value.  The top bit of i will always be
           zero since we've checked that it's positive so it will always
           terminate. *)
        fun log2Word(i: word, j: word, n: Int.int) =
            if Word.>(j, i) then n-1
            else log2Word(i, Word.<<(j, 0w1), n+1)

        (* The value is represented as little-endian byte segment.
           High-order bytes may be zero so we work back until we
           find a non-zero byte and then find the bit-position
           within it. *)
        fun log2Long(i, byte) =
        let
            val b = loadByte(i, byte)
        in
            if b = 0w0 then log2Long(i, byte-1)
            else log2Word(b, 0w2, 1) + byte*8
        end
            
    in
        fun log2 (i: int) : Int.int =
            if i <= 0 then raise Domain
            else if isShort i
            then log2Word(Word.fromLargeInt i, 0w2, 1)
            else (* i is actually a pointer to a byte segment. *)
            let
                val bytes = segLength i * Word.toInt RunCall.bytesPerWord
            in
               log2Long(i, bytes-1)
            end
    end

    (* These are implemented in the RTS. *)
    val orb  : int * int -> int = RunCall.rtsCallFull2 "PolyOrArbitrary"
    and xorb : int * int -> int = RunCall.rtsCallFull2 "PolyXorArbitrary"
    and andb : int * int -> int = RunCall.rtsCallFull2 "PolyAndArbitrary"

    (* notb is defined as ~ (i+1) and there doesn't seem to be much advantage
       in implementing it any other way. *)
    fun notb i = ~(i + 1)
    
    local
        fun power(acc: LargeInt.int, _, 0w0) = acc
        |   power(acc, n, i) =
                power(
                    if Word.andb(i, 0w1) = 0w1
                    then acc * n
                    else acc,
                    n * n, Word.>>(i, 0w1)
                    )
    in
        fun pow(i: LargeInt.int, j: Int.int) =
            if j < 0
            then(* Various exceptional cases. *)
            (
                if i = 0 then raise Div
                else if i = 1 then 1
                else if i = ~1
                then if Int.rem(j, 2) = 0 then (*even*) 1 else (*odd*) ~1
                else 0
            )
            else if LibrarySupport.isShortInt j
            then power(1, i, Word.fromInt j)
            else
            (* Long: This is possible only if int is arbitrary precision.  If the
               value to be multiplied is anything other than 0 or 1
               we'll exceed the maximum size of a cell. *)
                if i = 0 then 0
            else if i = 1 then 1
            else raise Size
    end

    (* These could be implemented in the RTS although I doubt if it's
       really worth it. *)
    local
        val maxShift = Word.fromInt Word.wordSize
        val fullShift = pow(2, Word.wordSize)
    in
        fun << (i: int, j: Word.word) =
           if j < maxShift
           then i * Word.toLargeInt(Word.<<(0w1, j))
           else <<(i * fullShift, j-maxShift)
    end
    
    fun ~>> (i: int, j: Word.word) = LargeInt.div(i, pow(2, Word.toInt j))

    open LargeInt (* Inherit everything from LargeInt.  Do this last because it overrides the overloaded functions. *)
end;
