(* Cut-down example from Isabelle code-generator.  During testing this showed
   up a bug in the X86 code-generator. *)

structure Generated_Code = struct


datatype num = One | Bit0 of num | Bit1 of num;

datatype int = Zero_int | Pos of num | Neg of num;

type 'a one = {one : 'a};

type 'a times = {times : 'a -> 'a -> 'a};

fun nat _ = raise Fail "bad"

fun uminus_int (Neg m) = Pos m
  | uminus_int (Pos m) = Neg m
  | uminus_int Zero_int = Zero_int;
 
fun plus_num _ = raise Fail "bad"

fun sub _ _ = raise Fail "bad"
and plus_inta (Neg m) (Neg n) = Neg (plus_num m n)
  | plus_inta (Neg m) (Pos n) = sub n m
  | plus_inta (Pos m) (Neg n) = sub m n
  | plus_inta (Pos m) (Pos n) = Pos (plus_num m n)
  | plus_inta Zero_int l = l
  | plus_inta k Zero_int = k;

fun minus_int _ _ = raise Fail "bad";


type 'a power = {one_power : 'a one, times_power : 'a times};
  
fun times_inta _ = raise Fail "bad"

val times_int = {times = times_inta} : int times;

val one_inta : int = Pos One;

val one_int = {one = one_inta} : int one;

val power_int = {one_power = one_int, times_power = times_int} : int power;


fun less_num (Bit1 m) (Bit0 n) = less_num m n
  | less_num (Bit1 m) (Bit1 n) = less_num m n
  | less_num (Bit0 m) (Bit1 n) = less_eq_num m n
  | less_num (Bit0 m) (Bit0 n) = less_num m n
  | less_num One (Bit1 n) = true
  | less_num One (Bit0 n) = true
  | less_num m One = false
and less_eq_num (Bit1 m) (Bit0 n) = less_num m n
  | less_eq_num (Bit1 m) (Bit1 n) = less_eq_num m n
  | less_eq_num (Bit0 m) (Bit1 n) = less_eq_num m n
  | less_eq_num (Bit0 m) (Bit0 n) = less_eq_num m n
  | less_eq_num (Bit1 m) One = false
  | less_eq_num (Bit0 m) One = false
  | less_eq_num One n = true;


type 'a ord = {less_eq : 'a -> 'a -> bool, less : 'a -> 'a -> bool};
val less_eq = #less_eq : 'a ord -> 'a -> 'a -> bool;

fun less_int (Neg k) (Neg l) = less_num l k
  | less_int (Neg k) (Pos l) = true
  | less_int (Neg k) Zero_int = true
  | less_int (Pos k) (Neg l) = false
  | less_int (Pos k) (Pos l) = less_num k l
  | less_int (Pos k) Zero_int = false
  | less_int Zero_int (Neg l) = false
  | less_int Zero_int (Pos l) = true
  | less_int Zero_int Zero_int = false;

fun less_eq_int (Neg k) (Neg l) = less_eq_num l k
  | less_eq_int (Neg k) (Pos l) = true
  | less_eq_int (Neg k) Zero_int = true
  | less_eq_int (Pos k) (Neg l) = false
  | less_eq_int (Pos k) (Pos l) = less_eq_num k l
  | less_eq_int (Pos k) Zero_int = false
  | less_eq_int Zero_int (Neg l) = false
  | less_eq_int Zero_int (Pos l) = true
  | less_eq_int Zero_int Zero_int = true;

datatype float = Float of int * int;

fun min A_ a b = (if less_eq A_ a b then a else b);

fun power s = raise Fail "bad"

fun is_float_nonneg _ = raise Fail "ab"

fun uminus_float (Float (m1, e1)) = Float (uminus_int m1, e1);

fun plus_float (Float (m1, e1)) (Float (m2, e2)) =
  (if less_eq_int e1 e2
    then Float (plus_inta m1
                  (times_inta m2
                    (power power_int (Pos (Bit0 One)) (nat (minus_int e2 e1)))),
                 e1)
    else Float (plus_inta m2
                  (times_inta m1
                    (power power_int (Pos (Bit0 One)) (nat (minus_int e1 e2)))),
                 e2));

fun minus_float f g = plus_float f (uminus_float g);

fun less_eq_float a b = is_float_nonneg (minus_float b a);

fun is_float_pos (Float (m, e)) = less_int Zero_int m;

fun less_float a b = is_float_pos (minus_float b a);

val ord_float = {less_eq = less_eq_float, less = less_float} : float ord;

fun inf_float a b = min ord_float a b;

end;

