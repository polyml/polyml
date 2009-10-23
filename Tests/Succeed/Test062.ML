infix ++ +++;

fun a ++ b = a ^ b;
fun a +++ b = b ^ a;

val x = "a" ++ "b" +++ "c";

(* This next has changed in ML97. In ML90 two different operators
   always associated to the left. *)
infixr ++ +++;

fun a ++ b = a ^ b;
fun a +++ b = b ^ a;

val y = "a" ++ "b" +++ "c";

(* In ML90 x and y are the same. *)
if x = y then raise Size else ();
