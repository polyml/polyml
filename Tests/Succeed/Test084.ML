(* Examples from the Revised Definition p90
   These examples are valid in ML97 but not in ML90. *)
val f = (fn x => x) (fn x => x)
structure A = struct end
val y = f 7
;

structure A: sig val f: int->int end =
    struct
        val f = (fn x => x) (fn x => x)
    end
;
