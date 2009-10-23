(* This is ok. *)
fun x () = let val id: 'a->'a = fn z => z in id id end;
(* This is ok. *)
fun x () = let val id: 'a->'a = fn z => z in id end: 'a->'a;
