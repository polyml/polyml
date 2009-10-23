(* This failed an assertion on the PPC architecture. *)
fun f x = Word.andb(x, 0wx108000);
RunCall.run_call0 RuntimeCalls.POLY_SYS_full_gc ();
