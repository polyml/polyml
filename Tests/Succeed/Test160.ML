(* Tests for overflow in div and mod. *)
fun dv (i, j) = LargeInt.toInt(LargeInt.div(LargeInt.fromInt i, LargeInt.fromInt j));
fun md (i, j) = LargeInt.toInt(LargeInt.mod(LargeInt.fromInt i, LargeInt.fromInt j));

fun check(i, j) = if i = j then () else raise Fail "wrong";

case Int.maxInt of SOME m => check(m div 2, dv(m, 2)) | NONE => ();

case Int.maxInt of SOME m => check(m div ~2, dv(m, ~2)) | NONE => (); (* Should not overflow. *)

case Int.minInt of SOME m => check(m mod ~1, 0) | NONE => (); (* Should not overflow. *)

(* Dividing the minimum int by ~1 should raise overflow. *)
case Int.minInt of SOME m => ((dv(m, ~1); raise Fail "wrong") handle Overflow => ());

(* Random number generator. *)
local
    fun rmod x y = x - y * Real.realFloor (x / y);
    val a = 16807.0;
    val m = 2147483647.0;
    val random_seed = ref 1.0;
in

    fun random () =
    let
        val r = rmod (a * ! random_seed) m
    in
        random_seed := r;
        Real.toInt IEEEReal.TO_NEGINF r
    end
end;

fun doFor f 0 = () | doFor f n = (f(); doFor f (n-1));

fun doit () =
let
    val a = random() and b = random()
    fun checkDiv(i, j) =
        if i div j = dv(i, j)
        then ()
        else raise Fail (concat["Error - ", Int.toString i, " div ", Int.toString j])
    and checkMod(i, j) =
        if i mod j = md(i, j)
        then ()
        else raise Fail (concat["Error - ", Int.toString i, " mod ", Int.toString j])
in
    checkDiv(a, b); checkMod(a, b);
    checkDiv(~a, b); checkMod(~a, b);
    checkDiv(a, ~b); checkMod(a, ~b);
    checkDiv(~a, ~b); checkMod(~a, ~b)
end;

doFor doit 100;
