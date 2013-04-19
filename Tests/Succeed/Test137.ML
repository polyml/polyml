(* Verify that the correct exception is raised for negative values. *)
(TextIO.canInput(TextIO.stdIn, ~1); raise Fail "wrong") handle Size => ();
(TextIO.inputN(TextIO.stdIn, ~1); raise Fail "wrong") handle Size => ();
