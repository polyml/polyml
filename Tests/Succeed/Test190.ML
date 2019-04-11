(* After a Posix fork there is only one thread in the child.
   There could be a GC before any exec so this is a check that it works. *)
case #lookupStruct (PolyML.globalNameSpace) "Posix" of
    SOME _ => ()
|   NONE => raise NotApplicable;

let
    open Posix.Process
in
case fork() of
    NONE =>
        (
            PolyML.fullGC();
            exit 0w0
        )
    |   SOME pid => waitpid(W_CHILD pid, [])
end;

