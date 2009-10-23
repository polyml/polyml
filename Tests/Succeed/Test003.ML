(* A few random tests for BoolVector and BoolArray. *)
fun verify true = ()
|   verify false = raise Fail "wrong";

val a = BoolVector.tabulate(20, fn i => i mod 2 = 1);

(* While working on converting these tests I actually discovered a new bug. *)
verify(a = BoolVector.fromList[false, true, false, true, false, true, false, true, false, true,
            false, true, false, true, false, true, false, true, false, true]);

val b = BoolArray.tabulate(20, fn i => i mod 2 = 1);
verify(BoolArray.vector b = BoolArray.vector(BoolArray.fromList[false, true, false, true, false, true, false, true, false, true,
            false, true, false, true, false, true, false, true, false, true]));


val b = BoolArray.tabulate(20, fn i => i mod 4 = 1);
verify(BoolArray.vector b = BoolArray.vector (BoolArray.fromList[false, true, false, false, false, true, false, false,
        false, true, false, false, false, true, false, false, false, true, false, false]));

verify(not(BoolVector.sub(a, 0)));
verify(BoolVector.sub(a, 9));

verify((BoolVector.sub(a, 20); false) handle Subscript => true);
verify((BoolVector.sub(a, ~1); false) handle Subscript => true);
verify(BoolVector.map (fn i => not i) a =
     BoolVector.fromList[true, false, true, false, true, false, true, false, true, false,
            true, false, true, false, true, false, true, false, true, false]);
verify(BoolVector.foldl (fn(a, b) => a orelse b) false a);
verify(BoolVector.foldr (fn(a, b) => a orelse b) false a);
val d = BoolVector.fromList [true, true, false, true, true];
val c = BoolVector.foldl (fn(a, b) => a :: b) [] a;
verify(BoolArray.vector(BoolArray.fromList c) =
    BoolVector.fromList[true, false, true, false, true, false, true, false, true, false,
         true, false, true, false, true, false, true, false, true, false]);
verify(BoolVector.concat[d, d] =
    BoolVector.fromList[true, true, false, true, true, true, true, false, true, true]);


