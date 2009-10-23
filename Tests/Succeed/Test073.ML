(* Side-effects should still be performed even if the result is known. *)
val r = ref false;
fun g b = r := b;
fun f s = (g s; true);
fun p s = not (f s);
val _ = p true;

if ! r then () else raise Fail "Wrong";
