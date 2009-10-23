(* Check the real number comparison operations.
   All operations return false on Nans except for != *)

fun verify s = if s then () else raise Fail "FAIL!!";
val Nan = 0.0/0.0;

verify(Real.==(1.0, 1.0));
verify(not (Real.==(1.0, 1.1)));
verify(not (Real.==(1.1, 1.0)));
verify(not (Real.==(Nan, Nan)));

verify(not (Real.!=(1.0, 1.0)));
verify(Real.!=(1.0, 1.1));
verify(Real.!=(1.1, 1.0));
verify(Real.!=(Nan, Nan));

verify(1.0 < 1.1);
verify(not (1.1 < 1.0));
verify(not (1.0 < Nan));
verify(not (Nan < 1.0));
verify(not (1.0 < 1.0));

verify(not (1.0 > 1.1));
verify(1.1 > 1.0);
verify(not (1.0 > Nan));
verify(not (Nan > 1.0));
verify(not (1.0 > 1.0));

verify(1.0 <= 1.1);
verify(not (1.1 <= 1.0));
verify(not (1.0 <= Nan));
verify(not (Nan <= 1.0));
verify(1.0 <= 1.0);

verify(not (1.0 >= 1.1));
verify(1.1 >= 1.0);
verify(not (1.0 >= Nan));
verify(not (Nan >= 1.0));
verify(1.0 >= 1.0);

