(* Bug to do with raising an exception inside a new thread.
   The actual problem was that the same location on the stack
   was used for the normal return address and the exception
   case. *)

PolyML.Compiler.maxInlineSize := 1;

fun wrap _ _ _ _ _ ()  = raise Match;

Thread.Thread.fork (wrap 1 2 3 4 5, []);
