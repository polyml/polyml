(* Real number bugs reported by Phil Clayton together with some additional tests. *)
fun check x = x orelse raise Fail "Test failed";

let
    val a = Real.nextAfter(1.0, 2.0)
in
    check(a > 1.0 andalso a < 2.0)
    (* This was actually a code-generator bug. *)
end;


(* The old version of the basis library documentation said that this should
   print the digits.  That was removed before the book was published but not
   fixed in the source. *)
check(IEEEReal.toString {class = IEEEReal.NAN, sign = false, digits = [1,2,3], exp = 0} = "nan");

val pNan = valOf(Real.fromDecimal { class = IEEEReal.NAN, sign = false, digits = [], exp = 0})
and mNan = valOf(Real.fromDecimal { class = IEEEReal.NAN, sign = true, digits = [], exp = 0});

check(not(Real.signBit pNan));
check(Real.signBit mNan);

check(not(#sign(Real.toDecimal pNan)));
check(#sign(Real.toDecimal mNan));

check(not(Real.signBit(abs(Real.posInf * 0.0))));
check(not(Real.signBit(abs(~ (Real.posInf * 0.0)))));
check(not(Real.signBit(Real.abs(Real.posInf * 0.0))));
check(not(Real.signBit(Real.abs(~ (Real.posInf * 0.0)))));

check(Real.fmt (StringCvt.SCI NONE) mNan = "nan");
check(Real.fmt (StringCvt.FIX NONE) mNan = "nan");
check(Real.fmt (StringCvt.GEN NONE) mNan = "nan");
check(Real.fmt StringCvt.EXACT mNan = "nan");

check(not (Real.isNan 0.0));
check(not (Real.isNan ~1.0));
check(not (Real.isNan Real.posInf));
check(not (Real.isNan Real.negInf));

check (Real.isNan pNan);
check (Real.isNan mNan);

check(Real.isFinite 0.0);
check(not (Real.isFinite pNan));
check(not (Real.isFinite mNan));
check(not (Real.isFinite Real.posInf));
check(not (Real.isFinite Real.negInf));

(* The exceptions with invalid specifications should be raised when
   Real.fmt is applied to the spec. *)
check((Real.fmt(StringCvt.SCI(SOME ~1)); false) handle Size => true);
check((Real.fmt(StringCvt.FIX(SOME ~1)); false) handle Size => true);
check((Real.fmt(StringCvt.GEN(SOME ~1)); false) handle Size => true);
