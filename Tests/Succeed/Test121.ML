(* The rounding settings were not being carried over correctly between the RTS
   and compiled code. *)
fun check x = if x then () else raise Fail "Wrong";
open IEEEReal;
setRoundingMode(TO_POSINF);
check(getRoundingMode() = TO_POSINF);
val pos = 1.0/3.0;
check(pos * 3.0 > 1.0);
val neg = ~1.0/3.0;
check(neg * 3.0 > ~1.0);

setRoundingMode(TO_NEGINF);
check(getRoundingMode() = TO_NEGINF);
val pos = 1.0/3.0;
check(pos * 3.0 < 1.0);
val neg = ~1.0/3.0;
check(neg * 3.0 < ~1.0);

setRoundingMode(TO_ZERO);
check(getRoundingMode() = TO_ZERO);
val pos = 1.0/3.0;
check(pos * 3.0 < 1.0);
val neg = ~1.0/3.0;
check(neg * 3.0 > ~1.0);
check(Real.==(~neg, pos));

setRoundingMode(TO_NEAREST);
check(getRoundingMode() = TO_NEAREST);
val pos = 1.0/3.0;
val neg = ~1.0/3.0;
check(Real.==(~neg, pos));
