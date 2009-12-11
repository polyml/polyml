(* Problem handling polymorphism. *)
fun q () =
let
    fun f (x, y) = (PolyML.print(x ()); PolyML.print(y ()); ())
    and g x = f (x, fn () => raise Fail "bad")
in
    f (fn () => true, fn () => false);
    f (fn () => 1, fn () => 1)
end;
q();

fun q () =
let
    fun f (x, y) = (PolyML.print(x ()); PolyML.print(y ()); ())
    and g x = f (x, fn () => raise Fail "bad")
in
    f (fn () => true, fn () => false);
    f (fn () => 1, fn () => 1);
    g (fn _ => 4)
end;

(q(); raise Fail "wrong") handle Fail "bad" => () | _ => raise Fail "wrong";
