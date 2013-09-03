(* Some bugs in Word32 on 64-bit and a bug in arithmetic right shift
   with a constant shift value. *)

fun check a b = if a = b then () else raise Fail "Bad";


check(Word32.toIntX 0wx0) 0;
check(Word32.toIntX 0wx1) 1;
check(Word32.toIntX 0wx7FFFFFFF) 2147483647;
check(Word32.toIntX 0wx80000000) ~2147483648;
check(Word32.toIntX 0wx80000001) ~2147483647;
check(Word32.toIntX 0wxFFFFFFFE) ~2;
check(Word32.toIntX 0wxFFFFFFFF) ~1;

check(Word32.toLargeX 0wx0) 0wx0;
check(Word32.toLargeX 0wx1) 0wx1;
check(Word32.toLargeX 0wx7FFFFFFF) 0wx7FFFFFFF;


check (let val r = ref 0w4 in Word.~>>(!r, 0w1) end) 0w2;

(* Checks that fromLargeInt returns the same result as fromInt *)

check(Word32.fromLargeInt ~1) 0wxFFFFFFFF;
check(Word32.fromLargeInt (IntInf.pow (2, 32))) 0wx0;
