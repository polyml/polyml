(* Bug fix for byte vector equality. *)

if BoolVector.tabulate(16, fn n => n < 8) =
   BoolVector.tabulate(16, fn n => n < 10)
then raise Fail "Wrong"
else ();
