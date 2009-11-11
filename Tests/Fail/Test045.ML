
(* This fails in ML97. *)
structure A = struct end;
signature SIG =
sig
    structure B : sig end
    sharing A = B
end;

