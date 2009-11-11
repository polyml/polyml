
(* This should fail. *)
let
    val f = #a
    val g = #b

    val x = if true then f {a=1, b=2, c=3} else f {a=3,b=3, d= 4}
in
    1
end;
