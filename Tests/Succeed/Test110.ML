(* Check integer conversion. *)

fun verifyBase radix n =
    valOf(StringCvt.scanString (Int.scan radix)(Int.fmt radix n)) = n

fun verify n =
    if verifyBase StringCvt.DEC n andalso verifyBase StringCvt.HEX n andalso
       verifyBase StringCvt.OCT n andalso verifyBase StringCvt.BIN n
    then () else raise Fail "failed";

(* Random number generator.  From Isabelle. *)

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
        Real.floor r
    end
end;

fun doFor f 0 = () | doFor f n = (f(); doFor f (n-1));

verify 0;
verify 1;
verify ~1;
verify 100000000000000000000000;
verify 100000000000000000000001;
verify 9051234567;

doFor(fn () => (verify(random()); verify(~(random())))) 100;
