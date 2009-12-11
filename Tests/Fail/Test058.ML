(* Structure tests. *)

structure S: sig exception x of int end = struct exception x end;
