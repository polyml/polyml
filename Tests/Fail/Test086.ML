(* Another occurs check failure. #307 *)
fun g {base = {...}} = ()
fun f (reg as {base, ...}) = (f base; g reg);
