(* Record update requires a base of record type. *)

PolyML.Compiler.languageExtensions := true;

let
    val r : int = 10
in
    {r with a = 11}
end;
