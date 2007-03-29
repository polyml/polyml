PolyML.addOverload PolyML.convString PolyML.convStringName;
PolyML.addOverload PolyML.convInt "convInt";
PolyML.addOverload PolyML.convWord "convWord";

PolyML.Inner.print_depth 1;
PolyML.Inner.use "mlsource/prelude/RunCall";
PolyML.Inner.use "mlsource/prelude/RuntimeCalls";
PolyML.Inner.use "mlsource/prelude/prelude";
PolyML.Inner.use "mlsource/prelude/prelude2";

PolyML.use "mlsource/prelude/Signal";
PolyML.use "mlsource/prelude/Universal";

(* Build the main basis library. *)
PolyML.use "basis/build";
(* Build the Process structure for backwards compatibility. *)
PolyML.use "mlsource/prelude/processes";

PolyML.use "mlsource/prelude/Address";
PolyML.make "mlsource/extra/CInterface";
PolyML.use "mlsource/extra/CInterface/clean";

(* XWindows/Motif *)
let
   val xcall: int*int->int*int =
   	RunCall.run_call1 RuntimeCalls.POLY_SYS_XWindows;
   (* See if the RTS supports the X GetTimeOfDay call. *)
   val isX = (xcall(30, 0); true) handle _ => false
in
   if isX
   then
   	(
   	(* We have to compile this in ML90 mode because it assumes
   	   that the xcall function can be polymorphic. *)
   	PolyML.Compiler.ml90 := true;
   	PolyML.make "mlsource/extra/XWindows"
   		handle exn => (PolyML.Compiler.ml90 := false; raise exn);
   	PolyML.Compiler.ml90 := false;
   	PolyML.make "mlsource/extra/Motif"
   	)
   else ()
end;

PolyML.print_depth 100;

(* Set the inline level to 40 which seems optimal. *)
PolyML.Compiler.maxInlineSize := 40;

(* Do this last.  There's a problem that replacing the standard input
   loses any buffering in the previous input which includes any commands
   after the one that does the replacing. *)
let
    val args = CommandLine.arguments();
	(* If we have -o filename use that as the output name.
	   N.B.  polyImport takes the first argument that is not recognised as
	   an RTS argument and treats that as the file name so any -o must occur
	   AFTER the import file. *)
	fun getFile [] = "polyexport" (* Default file name *)
	  | getFile ("-o" :: outFile :: _) = outFile
	  | getFile (_::tl) = getFile tl
	val fileName = getFile args
in
	PolyML.use "mlsource/prelude/ReplaceStdio";
	PolyML.shareCommonData PolyML.rootFunction;
	PolyML.export(fileName, PolyML.rootFunction)
end;

