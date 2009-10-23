(* Conversion of negative hex numbers had an error. *)
if ~0x4 = ~ 0x4
then ()
else raise Fail "FAIL";
