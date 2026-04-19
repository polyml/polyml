(* Top level overloadings that default to int must be frozen. *)
fun f x = x*x;
(* This can segfault if applied to a real. *)
fun g x = f x + 1.0;
