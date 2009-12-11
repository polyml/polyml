(* Structure tests. *)

structure S: sig datatype 'a opt = NONE | SOME of 'a end where type 'a opt = 'a option =
    struct datatype 'a opt = NONE | SOME of 'a end;
