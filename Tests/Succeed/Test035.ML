(* Bug in sparc assembly code. *)
if IntInf.andb(1234567890123456789, 1) = 1
then ()
else raise Fail "FAIL";

if IntInf.xorb(1234567890123456789, 1) = 1234567890123456788
then ()
else raise Fail "FAIL";
