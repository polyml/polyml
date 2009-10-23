(* This caused a Subscript error in the back end of the compiler
   at one stage. *)
PolyML.Compiler.maxInlineSize := 1;
fun f y =
   let fun a x = b (x+y) and b x = a x in a y end handle exn => raise exn;
