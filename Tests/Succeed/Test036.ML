(* Reported by Phil Clayton. *)
fun verify true = ()
|   verify false = raise Fail "wrong";

val test = IntInf.pow;
verify(test (~1, ~1000000) = 1);
verify(test (~1, ~8) = 1);
verify(test (~1, ~6) = 1);
verify(test (~1, ~4) = 1);
verify(test (~1, ~2) = 1);
verify(test (~1, ~1) = ~1);
