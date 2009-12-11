(* This gives a fault when we try to set the constructors. *)
structure S: sig datatype t = X end = struct type t = int end;
