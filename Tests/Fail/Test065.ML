(* Check for escaping datatypes.  Case 2. *)
fun f x =
let
    datatype s = S
in
    if x = S then 1 else 2
end;
