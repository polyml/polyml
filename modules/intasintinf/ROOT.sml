
(* If we already have int as arbitrary precision we don't need this.  This also
   deals with the problem of building with 5.6 where FixedInt is missing. *)
case Int.precision of
    NONE =>
       let
           fun printError() = print "Module not required\n"
       in
           (* Create an empty module and exit. *)
           PolyML.SaveState.saveModule("intasintinf", {functors=[], sigs=[], structs=[], onStartup=SOME printError });
           OS.Process.exit OS.Process.success
       end
|   SOME _ => ();

val dirName =
    case PolyML.getUseFileName() of
        NONE => "."
    |   SOME s => OS.Path.dir s;

use (OS.Path.joinDirFile{dir=dirName, file="BuildIntInf.sml"});

structure TestMod =
struct
    fun f () = print "Hello World\n";
end;

PolyML.SaveState.saveModule("intasintinf", {functors=[], sigs=[], structs=["TestMod"], onStartup=NONE });

