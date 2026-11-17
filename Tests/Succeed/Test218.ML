(* With more GC threads than the GC task queue had entries (100) the mark
   phase could find the queue full and abort with an assertion in ForkNew.
   #327.  Run a full GC with 256 GC threads in a child process. *)
case #lookupStruct (PolyML.globalNameSpace) "Unix" of
    SOME _ => ()
|   NONE => raise NotApplicable;

val poly = CommandLine.name();
if OS.FileSys.access(poly, [OS.FileSys.A_EXEC]) then () else raise NotApplicable;

val proc: (TextIO.instream, TextIO.outstream) Unix.proc =
    Unix.execute(poly, ["-q", "--gcthreads=256"]);
val toChild = Unix.textOutstreamOf proc;
TextIO.output(toChild,
    "datatype t = L | N of t * t;\n\
    \fun mk 0 = L | mk n = N (mk (n-1), mk (n-1));\n\
    \val x = mk 10;\n\
    \PolyML.fullGC ();\n\
    \print \"done\\n\";\n");
TextIO.closeOut toChild;
val fromChild = Unix.textInstreamOf proc;
val output = TextIO.inputAll fromChild;
TextIO.closeIn fromChild;
if OS.Process.isSuccess(Unix.reap proc) andalso String.isSuffix "done\n" output
then () else raise Fail "Full GC with 256 GC threads failed";
