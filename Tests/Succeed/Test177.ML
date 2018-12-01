(* This was reported as an error. *)

PolyML.Compiler.narrowOverloadFlexRecord := true;

fun f a =
let
    val {x, ...} = g a
    val p = g a
in
    0
end
and g x = {x=x, y=x};
