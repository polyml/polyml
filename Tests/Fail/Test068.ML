(* Check for escaping datatypes.  Case 5. *)
fun f () =
let
    val a = ref nil
    datatype s = S
in
    a := [S]
end;
