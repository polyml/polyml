(* This should fail. *)
let
    val g = #b
    fun f (x as {a, ...}) = if x = x then g x else g x
    val _ = f{a=1, b=0.0, c = true}
in
    ()
end;
