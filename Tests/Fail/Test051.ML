(* Structure/Signature match test. *)
structure S: sig datatype t = X end = struct type t = int*int end;
