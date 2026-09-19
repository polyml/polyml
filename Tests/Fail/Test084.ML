(* Missing occurs check when linking flexible records. Issue #310. *)

fun f x y = (#1 x y, #1 y, [x, y]);
