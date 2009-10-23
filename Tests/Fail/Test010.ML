(* Tests for exporting local datatypes. All these examples are invalid in ML97. *)

val rec f = fn x => let local datatype t = C in val z = f C end in 3 end;
