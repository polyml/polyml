(* This should fail because 'a becomes scoped at the fun. *)
fun x () = (let val id: 'a->'a = fn z => z in id id end; fn z=>z:'a);
