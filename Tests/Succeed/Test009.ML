(* Bug in combination of exception pattern matching with
   val bindings. *)
exception E of string list;

fun f x =
let
   val E [y] = x
in
   y
end;

fun g x =
let
   val y = ref x
in
   f(!y)
end;

if g(E["OK"]) = "OK" then () else raise Fail "Wrong";

