(* Structure tests. *)

structure S: sig datatype t = A | B of int end =
    struct datatype t = A | B of bool end;
