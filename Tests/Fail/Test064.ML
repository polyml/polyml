(* Check for escaping datatypes.  Case 1. *)
fun f x =
let
    datatype s = S
in
    S
end;
