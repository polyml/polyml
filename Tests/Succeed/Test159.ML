(* This caused an InternalError exception. *)


PolyML.Compiler.maxInlineSize := 1;
fun f (x: real, y: real) = (y, x) handle Bind => (y+1.0, x);
