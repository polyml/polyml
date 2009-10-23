(* Test for mismatched infixed operators. 1 of 2. *)
infix << ;
infixr >> ;
fun a << b = a ^ b and a >> b = a ^ b;
(* These are illegal in ML97 but not ML90. *)
"a" << "b" >> "c";
