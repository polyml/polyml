(* Tests for Word8Array and Word8ArraySlice. *)
fun check f = if f() then () else raise Fail "Wrong";

val a = Word8Array.tabulate (10, fn i => Word8.fromInt i);
check(fn () => Word8Array.vector a = Word8Vector.fromList(List.tabulate(10, Word8.fromInt)));
check(fn () => Word8Array.findi (fn (_, 0w3) => true | _ => false) a = SOME(3, 0w3));
check(fn () => not(isSome(Word8Array.findi (fn (_, 0w11) => true | _ => false) a)));
check(fn () => Word8Array.find (fn 0w3 => true | _ => false) a = SOME 0w3);
check(fn () => not(isSome(Word8Array.find (fn 0w11 => true | _ => false) a)));
check(fn () => Word8Array.exists (fn 0w3 => true | _ => false) a);
check(fn () => not(Word8Array.exists (fn 0w11 => true | _ => false) a));
check(fn () => not(Word8Array.all (fn 0w3 => true | _ => false) a));
check(fn () => Word8Array.all (fn x => x < 0w10) a);
check(fn () => not(Word8Array.all (fn x => x < 0w9) a));
check(fn () => Word8Array.collate Word8.compare (a, a) = EQUAL);
val b = Word8Array.array(10, 0w0);
check(fn () => Word8Array.vector b = Word8Vector.fromList(List.tabulate(10, fn _ => 0w0)));
Word8Array.copy{src = a, dst=b, di=0};
check(fn () => Word8Array.vector b = Word8Vector.fromList(List.tabulate(10, Word8.fromInt)));
check(fn () => (Word8Array.copy{src = a, dst=b, di=1}; false) handle Subscript => true);
check(fn () => Word8Array.collate Word8.compare (a, b) = EQUAL);
Word8Array.update(a, 9, 0w10);
check(fn () => Word8Array.collate Word8.compare (a, b) = GREATER);
Word8Array.update(b, 2, 0w11);
check(fn () => Word8Array.collate Word8.compare (a, b) = LESS);
check(fn () => Word8Array.collate Word8.compare (Word8Array.array(0, 0w0), Word8Array.array(0, 0w0)) = EQUAL);
check(fn () => Word8Array.collate Word8.compare (Word8Array.array(0, 0w0), a) = LESS);
check(fn () => Word8Array.collate Word8.compare (a, Word8Array.array(0, 0w0)) = GREATER);
check(fn () => Word8Array.collate Word8.compare (Word8Array.array(1, 0w0), a) = LESS);
check(fn () => Word8Array.collate Word8.compare (Word8Array.array(1, 0w1), a) = GREATER);

val sa = Word8ArraySlice.slice(a, 2, NONE);
val sb = Word8ArraySlice.slice(a, 6, SOME 3);
check(fn () => Word8ArraySlice.vector sa = Word8Vector.fromList [0w2, 0w3, 0w4, 0w5, 0w6, 0w7, 0w8, 0w10]);
check(fn () => Word8ArraySlice.vector sb = Word8Vector.fromList[0w6, 0w7, 0w8]);
check(fn () => (Word8ArraySlice.slice(a, 6, SOME 5); false) handle Subscript => true);

check(fn () => Word8ArraySlice.findi(fn (_, 0w3) => true | _ => false) sa = SOME(1, 0w3));
check(fn () => not(isSome(Word8ArraySlice.findi(fn (_, 0w3) => true | _ => false) sb)));
check(fn () => not(Word8ArraySlice.all (fn x => x < 0w9) sa));
check(fn () => Word8ArraySlice.all (fn x => x < 0w9) sb);
Word8ArraySlice.copy{src=sb, dst=a, di=1};
check(fn () => Word8Array.vector a = Word8Vector.fromList [0w0, 0w6, 0w7, 0w8, 0w4, 0w5, 0w6, 0w7, 0w8, 0w10]);
(* Check for overlapping areas. *)
Word8ArraySlice.copy{src=sb, dst=a, di=7};
check(fn () => Word8Array.vector a = Word8Vector.fromList [0w0, 0w6, 0w7, 0w8, 0w4, 0w5, 0w6, 0w6, 0w7, 0w8]);
check(fn () => Word8ArraySlice.vector sb = Word8Vector.fromList[0w6, 0w6, 0w7]);
val sc = Word8ArraySlice.slice(a, 4, SOME 3);
check(fn () => Word8ArraySlice.vector sc = Word8Vector.fromList[0w4, 0w5, 0w6]);
Word8ArraySlice.copy{src=sc, dst=a, di=3};
check(fn () => Word8Array.vector a = Word8Vector.fromList [0w0, 0w6, 0w7, 0w4, 0w5, 0w6, 0w6, 0w6, 0w7, 0w8]);
