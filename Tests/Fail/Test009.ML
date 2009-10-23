(* Tests for exporting local datatypes. All these examples are invalid in ML97. *)

val rec f = fn x => let datatype t = C in f C end;
