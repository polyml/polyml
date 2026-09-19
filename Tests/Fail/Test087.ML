(* And another occurs check failure. #312 *)
fun f t=f(t::nil,#3 t::nil);
