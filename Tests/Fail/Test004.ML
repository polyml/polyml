(* This should fail because it is an expansive polymorphic expression. *)
val x = let val id: 'a->'a = fn z => z in id id end;
(* It's now allowed but sets it to a unique monotype.  This causes it to fail. *)
x 2;
