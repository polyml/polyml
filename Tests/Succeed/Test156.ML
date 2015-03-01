(* Functions with free variables were turned into stack closures but these
   could not be called tail-recursively.
   This code sets the maximum stack to detect the problem early on but
   with large enough examples this can exhaust the memory. *)

PolyML.Compiler.maxInlineSize := 1;

fun f x =
let
   fun g y = if y = 0 then 0 else f (x-1) 
in
   g x
end;

Thread.Thread.setAttributes[Thread.Thread.MaximumMLStack (SOME 3000)];

f 10000000 handle exn => (Thread.Thread.setAttributes[Thread.Thread.MaximumMLStack NONE]; raise exn);
Thread.Thread.setAttributes[Thread.Thread.MaximumMLStack NONE];
