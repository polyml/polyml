(* Tests for functions added to the Array and ArraySlice structures. *)
fun check f = if f() then () else raise Fail "Wrong";

val a = Array.tabulate (10, fn i => i);
check(fn () => Array.vector a = Vector.fromList(List.tabulate(10, fn i => i)));
check(fn () => Array.findi (fn (_, 3) => true | _ => false) a = SOME(3, 3));
check(fn () => not(isSome(Array.findi (fn (_, 11) => true | _ => false) a)));
check(fn () => Array.find (fn 3 => true | _ => false) a = SOME 3);
check(fn () => not(isSome(Array.find (fn 11 => true | _ => false) a)));
check(fn () => Array.exists (fn 3 => true | _ => false) a);
check(fn () => not(Array.exists (fn 11 => true | _ => false) a));
check(fn () => not(Array.all (fn 3 => true | _ => false) a));
check(fn () => Array.all (fn x => x < 10) a);
check(fn () => not(Array.all (fn x => x < 9) a));
check(fn () => Array.collate Int.compare (a, a) = EQUAL);
val b = Array.array(10, 0);
check(fn () => Array.vector b = Vector.fromList(List.tabulate(10, fn _ => 0)));
Array.copy{src = a, dst=b, di=0};
check(fn () => Array.vector b = Vector.fromList(List.tabulate(10, fn i => i)));
check(fn () => (Array.copy{src = a, dst=b, di=1}; false) handle Subscript => true);
check(fn () => Array.collate Int.compare (a, b) = EQUAL);
Array.update(a, 9, 10);
check(fn () => Array.collate Int.compare (a, b) = GREATER);
Array.update(b, 2, 11);
check(fn () => Array.collate Int.compare (a, b) = LESS);
check(fn () => Array.collate Int.compare (Array.array(0, 0), Array.array(0, 0)) = EQUAL);
check(fn () => Array.collate Int.compare (Array.array(0, 0), a) = LESS);
check(fn () => Array.collate Int.compare (a, Array.array(0, 0)) = GREATER);
check(fn () => Array.collate Int.compare (Array.array(1, 0), a) = LESS);
check(fn () => Array.collate Int.compare (Array.array(1, 1), a) = GREATER);

val sa = ArraySlice.slice(a, 2, NONE);
val sb = ArraySlice.slice(a, 6, SOME 3);
check(fn () => ArraySlice.vector sa = Vector.fromList [2, 3, 4, 5, 6, 7, 8, 10]);
check(fn () => ArraySlice.vector sb = Vector.fromList[6, 7, 8]);
check(fn () => (ArraySlice.slice(a, 6, SOME 5); false) handle Subscript => true);

check(fn () => ArraySlice.findi(fn (_, 3) => true | _ => false) sa = SOME(1, 3));
check(fn () => not(isSome(ArraySlice.findi(fn (_, 3) => true | _ => false) sb)));
check(fn () => not(ArraySlice.all (fn x => x < 9) sa));
check(fn () => ArraySlice.all (fn x => x < 9) sb);
ArraySlice.copy{src=sb, dst=a, di=1};
check(fn () => Array.vector a = Vector.fromList [0, 6, 7, 8, 4, 5, 6, 7, 8, 10]);
(* Check for overlapping areas. *)
ArraySlice.copy{src=sb, dst=a, di=7};
check(fn () => Array.vector a = Vector.fromList [0, 6, 7, 8, 4, 5, 6, 6, 7, 8]);
check(fn () => ArraySlice.vector sb = Vector.fromList[6, 6, 7]);
val sc = ArraySlice.slice(a, 4, SOME 3);
check(fn () => ArraySlice.vector sc = Vector.fromList[4, 5, 6]);
ArraySlice.copy{src=sc, dst=a, di=3};
check(fn () => Array.vector a = Vector.fromList [0, 6, 7, 4, 5, 6, 6, 6, 7, 8]);
