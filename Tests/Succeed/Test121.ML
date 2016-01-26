(* The rounding settings were not being carried over correctly between the RTS
   and compiled code. *)
fun check x = if x then () else raise Fail "Wrong";
open IEEEReal;

(* setRoundingMode may raise an exception if it isn't implemented or
   only permits certain rounding modes. *)
let
    val () = setRoundingMode(TO_POSINF);
    val () = check(getRoundingMode() = TO_POSINF);
    val pos = 1.0/3.0;
    val () = check(pos * 3.0 > 1.0);
    val neg = ~1.0/3.0;
    val () = check(neg * 3.0 > ~1.0)
in
    ()
end handle _ => ();

let
    val () = setRoundingMode(TO_NEGINF);
    val () = check(getRoundingMode() = TO_NEGINF);
    val pos = 1.0/3.0;
    val () = check(pos * 3.0 < 1.0);
    val neg = ~1.0/3.0;
    val () = check(neg * 3.0 < ~1.0);
in
    ()
end handle _ => ();

let
    val () = setRoundingMode(TO_ZERO);
    val () = check(getRoundingMode() = TO_ZERO);
    val pos = 1.0/3.0;
    val () = check(pos * 3.0 < 1.0);
    val neg = ~1.0/3.0;
    val () = check(neg * 3.0 > ~1.0);
    val () = check(Real.==(~neg, pos));
in
    ()
end handle _ => ();

let
    val () = setRoundingMode(TO_NEAREST);
    val () = check(getRoundingMode() = TO_NEAREST);
    val pos = 1.0/3.0;
    val neg = ~1.0/3.0;
    val () = check(Real.==(~neg, pos));
in
    ()
end handle _ => ();
