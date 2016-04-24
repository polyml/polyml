(* Testing Posix is a problem because the Posix structure is not available in Windows. *)
(* This caused an assertion failure when the save vector overflowed. *)
Posix.Process.sleep (Time.fromMilliseconds 15000);
rubbish; (* Generate a "failure" i.e. the correct result *)

