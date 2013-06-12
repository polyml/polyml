(* This should raise the Size exception. *)
(TextIO.canInput(TextIO.stdIn, ~1); raise Fail "No exception")
    handle Size => ();
