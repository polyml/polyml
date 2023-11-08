(* Checks for Real.scan, Real.fromString, IEEEReal.scan and IEEEReal.fromString. *)
fun verify true = () | verify false = raise Fail "incorrect";


fun ieeeVerify(SOME(iee, subs), ieeMatch, str) =
    if iee = ieeMatch andalso Substring.string subs = str
    then ()
    else raise Fail "wrong"
|   ieeeVerify _ = raise Fail "wrong";

ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  1.23X"),
    {exp = 1, sign = false, class = IEEEReal.NORMAL, digits = [1, 2, 3]}, "X");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  1.23E3X"),
    {exp = 4, sign = false, class = IEEEReal.NORMAL, digits = [1, 2, 3]}, "X");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  1.23E 3X"),
    {exp = 1, sign = false, class = IEEEReal.NORMAL, digits = [1, 2, 3]}, "E 3X");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  ~1.23E 3X"),
    {exp = 1, sign = true, class = IEEEReal.NORMAL, digits = [1, 2, 3]}, "E 3X");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  .23E 3X"),
    {exp = 0, sign = false, class = IEEEReal.NORMAL, digits = [2, 3]}, "E 3X"); (* Valid, but E 3X isn't part of the number. *)
verify(not(isSome(IEEEReal.scan Substring.getc (Substring.full "  . 23E 3X")))); (* Not valid. *)
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  000.000X"),
    {exp = 0, sign = false, class = IEEEReal.ZERO, digits = []}, "X");
verify(not(isSome(IEEEReal.scan Substring.getc (Substring.full "  E2 3X")))); (* Not valid. *)
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  0X"),
    {exp = 0, sign = false, class = IEEEReal.ZERO, digits = []}, "X");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  1X"),
    {exp = 1, sign = false, class = IEEEReal.NORMAL, digits = [1]}, "X");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  12X"),
    {exp = 2, sign = false, class = IEEEReal.NORMAL, digits = [1, 2]}, "X");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  0.00X"),
    {exp = 0, sign = false, class = IEEEReal.ZERO, digits = []}, "X");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  10.X"),
    {exp = 2, sign = false, class = IEEEReal.NORMAL, digits = [1]}, ".X"); (* Valid. The decimal point is not part of the number. *)



verify(not(Real.isFinite(valOf(Real.fromString "infinity"))));
verify(not(Real32.isFinite(valOf(Real32.fromString "infinity"))));

verify(Real.isNan(valOf(Real.fromString "nan")));
verify(Real32.isNan(valOf(Real32.fromString "nan")));
