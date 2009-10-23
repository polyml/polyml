(* Check some word operations.  These probably won't be checked
   elsewhere because we're converting from arbitrary precision. *)
fun verify true = ()
|   verify false = raise Fail "wrong";

(* These assume 31 bit arithmetic.  TODO: Add tests for 63-bits. *)
verify(Word.wordSize <> 31 orelse Word.fromInt 10000000000 = 0wx540BE400);
verify(Word.wordSize <> 31 orelse Word.fromInt ~10000000000 = 0wx2BF41C00);
verify(Word.fromInt 1 = 0w1);
verify(Word.wordSize <> 31 orelse Word.fromInt ~1 = 0wx7FFFFFFF);
