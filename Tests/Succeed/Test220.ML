(* OS.Process.exit kills the other ML threads.  A thread blocked in
   Posix.Process.waitpid must exit cleanly rather than return a bad result
   to ML and crash the process.  This runs in a child poly because the
   test calls OS.Process.exit. *)
case #lookupStruct (PolyML.globalNameSpace) "Unix" of
    SOME _ => ()
|   NONE => raise NotApplicable;

if OS.FileSys.access("/bin/cat", [OS.FileSys.A_EXEC]) then () else raise NotApplicable;

fun exitsCleanly (script: string): bool =
let
    val proc: (TextIO.instream, TextIO.outstream) Unix.proc =
        Unix.execute(CommandLine.name(), [])
    val toChild = Unix.textOutstreamOf proc
    val () = TextIO.output(toChild, script)
    val () = TextIO.closeOut toChild
    val fromChild = Unix.textInstreamOf proc
    val _ = TextIO.inputAll fromChild
    val () = TextIO.closeIn fromChild
in
    OS.Process.isSuccess(Unix.reap proc)
end;

val exitWhileWaiting =
    (* The cat child runs until this poly exits and closes its stdin,
       so the waitpid thread stays blocked. *)
    "val cat: (TextIO.instream, TextIO.outstream) Unix.proc = Unix.execute(\"/bin/cat\", []);\n\
    \val _ = Thread.Thread.fork(fn () => ignore(Posix.Process.waitpid(Posix.Process.W_ANY_CHILD, [])), []);\n\
    \val () = OS.Process.sleep(Time.fromMilliseconds 200);\n\
    \val () = OS.Process.exit OS.Process.success;\n";

if exitsCleanly exitWhileWaiting then () else raise Fail "exit while waiting";
