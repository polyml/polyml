(* Code-generator bug.  We have to set the maximum inline function
   size to something small to prevent "return" from being expanded
   in-line. *)

PolyML.Compiler.maxInlineSize := 2;

fun f g x =
let
    val old = !x
    fun return y = (x := old; y)
in
    return (g x handle exn => (return (); raise exn))
end;
