(* Now that argument functions may have multiple arguments rather than 
   a single argument that may be a tuple it is necessary to load the
   function before overwriting it in a tail-recursive call. *)

PolyML.Compiler.maxInlineSize := 0;

fun g (a, b, c, d, e, f) =
    f (a, e, d, b, c, a);

g (1, 1, 1, 1, 1, fn _ => ());
