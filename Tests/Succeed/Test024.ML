(* For some reason the tests for the sign for ZERO values in Real.fromDecimal was reversed. *)
if Real.toString(valOf (Real.fromDecimal (valOf (IEEEReal.fromString "0.0")))) = "0.0"
then () else raise Fail "WRONG!!\n";
if Real.toString(valOf (Real.fromDecimal (valOf (IEEEReal.fromString "~0.0")))) = "~0.0"
then () else raise Fail "WRONG!!\n";

