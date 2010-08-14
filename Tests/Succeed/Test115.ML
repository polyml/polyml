(* Uncaught exceptions in a thread.   Caused a segfault in SVN 1189 on X86. *)
Thread.Thread.fork(fn () => raise Match, []);
