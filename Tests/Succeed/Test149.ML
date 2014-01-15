(* Some tests for the HashArray structure. *)

open HashArray;
val a = hash 10: int hash;
update(a, "a", 1);
update(a, "b", 2);
update(a, "k", 3); (* Happens to hash to the same as "a" *)
update(a, "l", 4);

val sum = fold (fn (_, a, b) => a+b) 0;

fun verify true = () | verify false = raise Fail "failed";

verify(sub(a, "a") = SOME 1);
verify(sub(a, "b") = SOME 2);
verify(sub(a, "c") = NONE);
verify(sub(a, "k") = SOME 3);
verify(sub(a, "l") = SOME 4);
verify(sum a = 10);

delete(a, "a");
verify(sub(a, "a") = NONE);
verify(sub(a, "k") = SOME 3);

verify(sum a = 9);

