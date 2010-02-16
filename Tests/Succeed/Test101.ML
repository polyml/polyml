(* Tests for various cases of arbitrary-precision arithmetic that require emulation.
   The tests depend on assumptions about how the compiler will generate code
   and may need changing in the future. *)
fun check x = x orelse raise Fail "Test failed";

(* This is used to avoid constant folding in the compiler. *)
fun I x = let val x = ref x in ! x end;
(* The maximum short precision number. *)
val maxShort = Word.toInt(Word.<<(0w1, Word.fromInt (Word.wordSize-1))-0w1);
val minShort = ~maxShort-1;
val aLong = maxShort+1; (* A long precision number. *)
check(PolyML.objSize maxShort = 0 andalso PolyML.objSize minShort = 0 andalso PolyML.objSize aLong <> 0);

fun f x = I x > 0;
check(f maxShort);
check(f aLong);

fun f x = I x < aLong;
check(f 0);
check(not(f aLong));

fun f x = I x <= x;
check(f aLong);

fun f x y = let val (_, a) = I (0, x) in a < y end;

check(f 0 1);
check(f 0 aLong);


fun f x = I x + 1;
check(f maxShort = maxShort+1); (* Overflow check *)
check(f aLong = aLong+1);

fun f x = I x + aLong;
check(f maxShort = maxShort+aLong);
check(f aLong = aLong+aLong);

fun f x = I x + x;
check(f maxShort = maxShort*2);
check(f aLong = aLong*2);

fun f x y = let val (_, a) = I (0, x) in a + y end;

check(f maxShort 1 = maxShort+1);
check(f aLong 1 = aLong+1);


fun f x = I x + 1;
check(f maxShort = maxShort+1); (* Overflow check *)
check(f aLong = aLong+1);

fun f x = I x + aLong;
check(f maxShort = maxShort+aLong);
check(f aLong = aLong+aLong);

fun f x = I x + x;
check(f maxShort = maxShort*2);
check(f aLong = aLong*2);

fun f x y = let val (_, a) = I (0, x) in a + y end;

check(f maxShort 1 = maxShort+1);
check(f aLong 1 = aLong+1);

fun f x = I x - 1;
check(f minShort = minShort-1); (* Overflow check *)
check(f aLong = aLong-1);

fun f x = I x - aLong;
check(f minShort = minShort-aLong);
check(f 0 = ~aLong);

fun f x y = I x - y;
check(f minShort 1 = minShort-1);
check(f 0 aLong = ~aLong);

fun f x y = let val (_, a) = I (0, y) in I x - a end;

check(f minShort 1 = minShort-1);
check(f aLong 1 = aLong-1);

fun f x = Real.fromInt(I x);
check(Real.round(f 3) = 3);
check(Real.round(f aLong) = aLong);

fun f x = I x * 2;
check(f aLong = aLong+aLong);
check(f minShort = minShort+minShort);
check(f maxShort = maxShort+maxShort);
