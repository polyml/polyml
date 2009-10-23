(* An example from the Definition.  This is now legal. *)
datatype t = f; val rec f = fn x => x;
