(* The syntax does not allow "op" before a constructor in
   a specification. *)
infix &;
signature S =
sig
   datatype t = op &
end;
