
(* These raised Size exceptions *)
fun test1 (r:real) =   Real.toLargeInt IEEEReal.TO_NEAREST r;
fun test2 (r:real) =   Real.toLargeInt IEEEReal.TO_ZERO r;
test1 2.23E34;
test1 2.23E35;
test2 2.23E34;
test2 2.23E35;
