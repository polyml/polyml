(* Tests for shifts, in particular to check that shifts greater than
   the number of bits in a word give the correct result. *)

functor checkWord(structure W: WORD) =
struct
    fun verifyRange test f t =
        if f > t then ()
        else if test f then verifyRange test (f+0w1) t
        else raise Fail "verifyRange"

    val zero = W.fromInt 0
    val one = W.fromInt 1

    local
        (* Left shifts of 1 up to the wordSize-1 followed by the same
           right shift give the original value. *)
        fun testShift1 i =
           W.>>(W.<<(one, i), i) = one
    in
        val () = verifyRange testShift1 0w1 (Word.fromInt(W.wordSize-1))
    end

    local
        (* But larger shifts set the result to zero *)
        fun testShift2 i =
            W.<<(one, i) = zero andalso W.>>(W.<<(one, Word.fromInt(W.wordSize-1)), i) = zero
    in
        val () = verifyRange testShift2 (Word.fromInt(W.wordSize)) (Word.fromInt(W.wordSize*4))
    end

    local
        (* Arithmetic left shifts of 1 up to the wordSize-2 followed by the same
           right shift give the original value. *)
        fun testShift3 i =
           W.~>>(W.<<(one, i), i) = one
    in
        val () = verifyRange testShift3 0w1 (Word.fromInt(W.wordSize-2))
    end

    local
        (* But a shift of wordSize-1 puts the value in the sign bit *)
        fun testShift4 i =
           W.~>>(W.<<(one, i), i) = W.~ one
    in
        val () = verifyRange testShift4 (Word.fromInt(W.wordSize-1)) (Word.fromInt(W.wordSize-1))
    end


    local
        (* But larger shifts set the result to zero *)
        fun testShift5 i =
            W.~>>(one, i) = zero andalso 
            W.~>>(W.<<(one, Word.fromInt(W.wordSize-1)), i) = W.~ one
    in
        val () = verifyRange testShift5 (Word.fromInt(W.wordSize)) (Word.fromInt(W.wordSize*4))
    end


    
end;

structure S1 = checkWord(structure W = Word);
structure S2 = checkWord(structure W = LargeWord);
structure S3 = checkWord(structure W = Word8);
structure S4 = checkWord(structure W = Word32);

