(* Tests for exporting local datatypes. All these examples are invalid in ML97. *)

fn x => let datatype t = C
        val _ = if true then x else C
        in 5
        end;
