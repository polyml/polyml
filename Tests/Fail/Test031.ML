(* These examples are invalid. *)

val f = (fn x => x) (fn x => x);
structure A = struct end
val y = f 7
;
