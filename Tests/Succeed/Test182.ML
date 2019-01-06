(* There was an issue in 32-in-64 where loading the Word8 value using a 32-bit value
   was then cached as though the whole 64-bits had been loaded. *)

if LargeWord.wordSize < 64 then raise NotApplicable else ();

fun largeWordToBytes(_, 0) = []
    |   largeWordToBytes(ival: LargeWord.word, n) =
            Word8.fromLargeWord ival :: largeWordToBytes(LargeWord.>>(ival, 0w8), n-1);

if largeWordToBytes(0wxffffffffffffffff, 8) = [0wxFF, 0wxFF, 0wxFF, 0wxFF, 0wxFF, 0wxFF, 0wxFF, 0wxFF]
then ()
else raise Fail "wrong";

