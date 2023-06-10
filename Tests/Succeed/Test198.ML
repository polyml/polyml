(* Checks for Real.scan, Real.fromString, IEEEReal.scan and IEEEReal.fromString. *)
fun check true = () | check false = raise Fail "incorrect";

check(not(Real.isFinite(valOf(Real.fromString "infinity"))));
check(not(Real32.isFinite(valOf(Real32.fromString "infinity"))));

check(Real.isNan(valOf(Real.fromString "nan")));
check(Real32.isNan(valOf(Real32.fromString "nan")));
