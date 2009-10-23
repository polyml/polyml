(* Check for special cases of single character strings. *)
if StringCvt.padLeft #"O" 2 "K" = "OK" then () else raise Fail "wrong";
if StringCvt.padRight #"K" 2 "O" = "OK" then () else raise Fail "wrong";
