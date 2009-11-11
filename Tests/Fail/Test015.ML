(* This fails (correctly) because f must be applied to an equality type. *)
local
    val f = valOf o #foo
    fun g x = (x=x; f x);
    val _ = f{foo=SOME 1, bar=2.0}
in
    val g = g
end;
