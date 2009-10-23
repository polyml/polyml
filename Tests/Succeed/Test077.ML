(* Code-generator bug.  Found by Rob Arthan 28/11/01. *)
fun f _ = let val (a, b) = raise Fail "bad" in a end;
