(* Structure tests. *)
structure S: sig datatype t = A | B of int type int end =
    struct type int = bool datatype t = A | B of int end;
