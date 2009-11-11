(* These examples are invalid. *)
structure B: sig val f: 'a->'a end =
    struct
        val f = (fn x => x) (fn x => x)
    end
;
