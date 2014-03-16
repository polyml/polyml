(* Bug in arbitrary precision emulation reported by Anthony Fox. *)

fun f x = IntInf.<< (1, Word.fromInt x) 
val a = f 64 
val b = f 32;
if a - b = (f 64 - f 32) then () else raise Fail "failed";

