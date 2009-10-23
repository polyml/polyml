(* Tests for bugs in real conversion.  Also tests that Real.~ flips the sign bit.  It's not
   clear from the Basis Library book whether this is correct for zero but for consistency
   we assume it.
   Reported by Matthew Fluet fluet AT tti-c AT org *)
fun verify true = ()
|   verify false = raise Fail "wrong";

verify(Real.round(valOf(Real.fromString "1234567890.12345678901234")) = 1234567890);
verify(Real.round(valOf(Real.fromString "1234567890.123456789012345")) = 1234567890);
verify(Real.round(valOf(Real.fromString "1234567890.1234567890123456")) = 1234567890);
verify(Real.round(valOf(Real.fromString "1234567890.1234567890123456789")) = 1234567890);

verify(not(Real.signBit 0.0));
verify(Real.signBit (Real.~ 0.0));
verify(Real.signBit (Real.~ 1.0));

verify(Real.signBit (valOf (Real.fromString "~0.0")));

