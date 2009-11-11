
(* This should definitely be illegal. *)
signature S =
sig
    eqtype t
end where type t = int->int;
