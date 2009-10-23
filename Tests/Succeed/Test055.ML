(* This should fail because it is illegal to redeclare 'a when it is
   bound explicitly or implicitly in an outer scope. *)
(* That was a misunderstanding.  This is actually legal. *)
fun x () = let val 'a id: 'a->'a = fn z => z in id end: 'a->'a;
