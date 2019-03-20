(* This caused an assertion failure when the save vector overflowed. *)
case #lookupStruct (PolyML.globalNameSpace) "Posix" of
    SOME _ => ()
|   NONE => raise NotApplicable;

Posix.Process.sleep (Time.fromMilliseconds 15000);
