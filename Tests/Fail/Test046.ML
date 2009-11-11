(* Should fail - requires equality type. *)
let
    fun f (x as {a, ...}, y as {b, ...}) = x = y
in
    f ({a=1, b=2, c = fn x => x}, {a=1, b=2, c = fn x => x})
end;
