(* Bug with determining when to detuple the result. *)

PolyML.Compiler.maxInlineSize := 0;

let
    fun f x = ! (ref (fn y => (y, x)))
    val r = #2 (f 1 2)
in
    r
end;
