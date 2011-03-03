(* This deadlocked with pthreads but not on Windows. *)

val w = let val x = SOME (ref 3) in Weak.weak x end;
PolyML.fullGC ();

case !w of NONE => () | SOME _ => raise Fail "Not removed";
