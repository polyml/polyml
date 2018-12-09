(* A few of these are specially handled in the code-generator. *)

fun check true = () | check false = raise Fail "Incorrect";

PolyML.Compiler.maxInlineSize := 1;
fun f x = if x then 1 else 0;
check(f true = 1);
check(f false = 0);

fun f x = if x then 2 else 0;
check(f true = 2);
check(f false = 0);

fun f x = if x then 3 else 0;
check(f true = 3);
check(f false = 0);

fun f x = if x then 4 else 0;
check(f true = 4);
check(f false = 0);

fun f x = if x then 5 else 0;
check(f true = 5);
check(f false = 0);

fun f x = if x then 6 else 0;
check(f true = 6);
check(f false = 0);

fun f x = if x then 7 else 0;
check(f true = 7);
check(f false = 0);

fun f x = if x then 8 else 0;
check(f true = 8);
check(f false = 0);
