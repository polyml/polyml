(* Record punning can be enabled manually. *)

PolyML.Compiler.recordPunning := true;

let
    val x = 3
    val z = 7
in
    {w = 2, x, y = 5, z}
end;
