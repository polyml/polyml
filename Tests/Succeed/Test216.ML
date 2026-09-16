(* Test for previously incorrect bounds checks. *)


(PackRealLittle.subVec (Word8Vector.fromList [0w1], 0); raise Fail "No exception") handle Subscript => ();
(PackRealLittle.subArr(Word8Array.fromList [0w1], 0); raise Fail "No exception") handle Subscript => ();


val largeIndex = Word.toInt(Word.<<(0w1, Word.fromInt(Word.wordSize)-0w2));
val c = PackRealLittle.toBytes 1.234;

(PackRealLittle.subVec (c, largeIndex); raise Fail "No exception") handle Subscript => ();
(PackRealLittle.subArr (Word8Array.fromList [0w1], largeIndex); raise Fail "No exception") handle Subscript => ();

