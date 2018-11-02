(* Tests on Real32.  Currently just rounding from Real. *)

fun check true = () | check false = raise Fail "incorrect";

val p32 = Real32.Math.pi;
val m32 = ~ p32;

val p = Real32.toLarge Real32.Math.pi;
val m = Real32.toLarge m32;

(* Construct values that are slightly more or less than the original. *)
val pp = Real.nextAfter(p, Real.posInf);
check(pp > p);
val pm = Real.nextAfter(p, Real.negInf);
check(pm < p);
val mp = Real.nextAfter(m, Real.posInf);
check(mp > m);
val mm = Real.nextAfter(m, Real.negInf);
check(mm < m);

infix 4 ==;
val op == = Real32.==;

(* Check the rounding of these values. *)
(* To nearest.  These all remove the small difference. *)
check(Real32.fromLarge IEEEReal.TO_NEAREST pp == p32);
check(Real32.fromLarge IEEEReal.TO_NEAREST pm == p32);
check(Real32.fromLarge IEEEReal.TO_NEAREST mp == m32);
check(Real32.fromLarge IEEEReal.TO_NEAREST mm == m32);

(* To zero.  If it's slightly nearer zero it will go to
   the next value. *)
check(Real32.fromLarge IEEEReal.TO_ZERO pp == p32);
check(Real32.fromLarge IEEEReal.TO_ZERO pm < p32);
check(Real32.fromLarge IEEEReal.TO_ZERO mp > m32);
check(Real32.fromLarge IEEEReal.TO_ZERO mm == m32);

(* To positive infinity.  If it's slightly above it will
   go to the next value. *)
check(Real32.fromLarge IEEEReal.TO_POSINF pp > p32);
check(Real32.fromLarge IEEEReal.TO_POSINF pm == p32);
check(Real32.fromLarge IEEEReal.TO_POSINF mp > m32);
check(Real32.fromLarge IEEEReal.TO_POSINF mm == m32);

(* To negative infinity.  If it's slightly below it will
   go to the next value. *)
check(Real32.fromLarge IEEEReal.TO_NEGINF pp == p32);
check(Real32.fromLarge IEEEReal.TO_NEGINF pm < p32);
check(Real32.fromLarge IEEEReal.TO_NEGINF mp == m32);
check(Real32.fromLarge IEEEReal.TO_NEGINF mm < m32);
