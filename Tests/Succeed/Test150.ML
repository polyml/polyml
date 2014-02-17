(* Bug in code generator reported by Lars Noschinski *)

fun f xs = (xs : ''a) val _ = (fn x => f x);
