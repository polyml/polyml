(* Check for escaping datatypes.  Case 4. *)
fun a x = x
and b () = let datatype s = S in a S; 0 end;
