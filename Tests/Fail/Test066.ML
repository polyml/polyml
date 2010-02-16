(* Check for escaping datatypes.  Case 3. *)
val rec a = fn x => x
and b = fn () => let datatype s = S in a S; 0 end;
