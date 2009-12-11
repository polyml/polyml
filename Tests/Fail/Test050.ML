(* Structure tests. *)
structure S: sig datatype t = A | B | C end =
    struct datatype t = A | B end;

