(* Checks for Real.scan, Real.fromString, IEEEReal.scan and IEEEReal.fromString. *)
(* There are a few more tests for overflow in the exponent in Test201.  These don't apply
   if int is IntInf.int. *)
fun verify true = () | verify false = raise Fail "incorrect";


fun ieeeVerify(SOME(iee, subs), ieeMatch, str) =
    if iee = ieeMatch andalso Substring.string subs = str
    then ()
    else raise Fail "wrong"
|   ieeeVerify _ = raise Fail "wrong";

ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  15E~1000"),
    {exp = ~998, sign = false, class = IEEEReal.NORMAL, digits = [1, 5]}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  1.23E~1"),
    {exp = 0, sign = false, class = IEEEReal.NORMAL, digits = [1, 2, 3]}, "");
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
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  10"),
    {exp = 2, sign = false, class = IEEEReal.NORMAL, digits = [1]}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  10.X"),
    {exp = 2, sign = false, class = IEEEReal.NORMAL, digits = [1]}, ".X"); (* Valid. The decimal point is not part of the number. *)
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full " ~0E4611686018427387905"), 
   {exp = 0, sign=true, class = IEEEReal.ZERO, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full " 0E4611686018427387905"), 
    {exp = 0, sign=false, class = IEEEReal.ZERO, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full " 0.01E4611686018427387904"), 
    {exp = 4611686018427387903, sign=false, class = IEEEReal.NORMAL, digits = [1]}, "");  (* mustn't be rounded to infinity or overflow, the representation of the exponent fits in an Int *)
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full " 0.0000000000001E4611686018427387915"), 
    {exp = 4611686018427387903, sign=false, class = IEEEReal.NORMAL, digits = [1]}, ""); (* mustn't be rounded to infinity or overflow, the twelve zeros substract 11 from the stated exponent *)
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full " 1E~4611686018427387905"), 
    {exp = ~4611686018427387904, sign=false, class = IEEEReal.NORMAL, digits = [1]}, "");  (* mustn't be rounded to zero or overflow, the representation of the exponent fits in an Int *)
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full " 1234567890E~4611686018427387914"), 
    {exp = ~4611686018427387904, sign=false, class = IEEEReal.NORMAL, digits = [1,2,3,4,5,6,7,8,9]}, "");  (* mustn't be rounded to zero or overflow, the representation of the exponent fits in an Int *)
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "infinity"),
    {exp = 0, sign = false, class = IEEEReal.INF, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "+infinity"),
    {exp = 0, sign = false, class = IEEEReal.INF, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "-infinity"),
    {exp = 0, sign = true, class = IEEEReal.INF, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "~infinity"),
    {exp = 0, sign = true, class = IEEEReal.INF, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "inf"),
    {exp = 0, sign = false, class = IEEEReal.INF, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "+inf"),
    {exp = 0, sign = false, class = IEEEReal.INF, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "-inf"),
    {exp = 0, sign = true, class = IEEEReal.INF, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "~inf"),
    {exp = 0, sign = true, class = IEEEReal.INF, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "nan"),
    {exp = 0, sign = false, class = IEEEReal.NAN, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "+nan"),
    {exp = 0, sign = false, class = IEEEReal.NAN, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "-nan"),
    {exp = 0, sign = true, class = IEEEReal.NAN, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "~nan"),
    {exp = 0, sign = true, class = IEEEReal.NAN, digits = []}, "");


verify(not(Real.isFinite(valOf(Real.fromString "infinity"))));
verify(not(Real32.isFinite(valOf(Real32.fromString "infinity"))));

verify(Real.isNan(valOf(Real.fromString "nan")));
verify(Real32.isNan(valOf(Real32.fromString "nan")));
