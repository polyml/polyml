(* This is valid, but previously failed. *)
fun f (i, d, q, r) =  if r then i+d=q else i+d=q;
if f(1, 2, 3, true) then "OK" else raise Fail "WRONG";
