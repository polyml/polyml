type indexname = string * int;
(* Cut down version of a test that Makarius had already cut down.
   This produced a segfault. *)
(* The problem turned out to be that there was a function that returned a pair
   of tuples and only the first field of one of the tuples was used in the
   non-recursive case but whole of the tuple was used in the recursive case. *)


type class = string;
type sort  = class list;
type arity = string * sort list * sort;
datatype typ = Type  of string * typ list
             | TFree of string * sort
             | TVar  of indexname * sort;

datatype term =
    Const of string * typ
  | Free  of string * typ
  | Var   of indexname * typ
  | Bound of int
  | Abs   of string*typ*term
  | op $  of term*term;

structure Generated_Code : sig
  val value :
    'a -> 'b -> bool -> 'b -> (bool * term list) option
end = struct

datatype num = One | Bit0 of num | Bit1 of num;

datatype inta = Zero_int | Pos of num | Neg of num;

datatype buggy_type = Buggy_Type of inta * inta;

val one_int : inta = Pos One;

val one_buggy_typea : buggy_type = Buggy_Type (one_int, Zero_int);

type 'a one = {one : 'a};
val one = #one : 'a one -> 'a;

val one_buggy_type = {one = one_buggy_typea} : buggy_type one;

fun plus_num _ _ = raise Match;

fun times_num (Bit1 m) (Bit1 n) =
  Bit1 (plus_num (plus_num m n) (Bit0 (times_num m n)))
  | times_num (Bit1 m) (Bit0 n) = Bit0 (times_num (Bit1 m) n)
  | times_num (Bit0 m) (Bit1 n) = Bit0 (times_num m (Bit1 n))
  | times_num (Bit0 m) (Bit0 n) = Bit0 (Bit0 (times_num m n))
  | times_num One n = n
  | times_num m One = m;

fun times_int (Neg m) (Neg n) = Pos (times_num m n)
  | times_int (Neg m) (Pos n) = Neg (times_num m n)
  | times_int (Pos m) (Neg n) = Neg (times_num m n)
  | times_int (Pos m) (Pos n) = Pos (times_num m n)
  | times_int Zero_int l = Zero_int
  | times_int k Zero_int = Zero_int;

fun s (Buggy_Type (x1, x2)) = x2;

fun f (Buggy_Type (x1, x2)) = x1;

fun times_buggy_typea x y = Buggy_Type (s y, times_int (s x) (f y));

type 'a times = {times : 'a -> 'a -> 'a};
val times = #times : 'a times -> 'a -> 'a -> 'a;

type 'a power = {one_power : 'a one, times_power : 'a times};
val one_power = #one_power : 'a power -> 'a one;
val times_power = #times_power : 'a power -> 'a times;

val times_buggy_type = {times = times_buggy_typea} : buggy_type times;

val power_buggy_type =
  {one_power = one_buggy_type, times_power = times_buggy_type} :
  buggy_type power;

datatype nat = Zero_nat | Suc of nat;

fun term_of_num _ = raise Match
fun term_of_int _ = raise Match
fun term_of_buggy_type _ = raise Match

fun equal_num (Bit0 x2) (Bit1 x3) = false
  | equal_num (Bit1 x3) (Bit0 x2) = false
  | equal_num One (Bit1 x3) = false
  | equal_num (Bit1 x3) One = false
  | equal_num One (Bit0 x2) = false
  | equal_num (Bit0 x2) One = false
  | equal_num (Bit1 x3) (Bit1 y3) = equal_num x3 y3
  | equal_num (Bit0 x2) (Bit0 y2) = equal_num x2 y2
  | equal_num One One = true;

fun equal_int (Neg k) (Neg l) = equal_num k l
  | equal_int (Neg k) (Pos l) = false
  | equal_int (Neg k) Zero_int = false
  | equal_int (Pos k) (Neg l) = false
  | equal_int (Pos k) (Pos l) = equal_num k l
  | equal_int (Pos k) Zero_int = false
  | equal_int Zero_int (Neg l) = false
  | equal_int Zero_int (Pos l) = false
  | equal_int Zero_int Zero_int = true;

fun equal_buggy_type (Buggy_Type (x1, x2)) (Buggy_Type (y1, y2)) =
  equal_int x1 y1 andalso equal_int x2 y2;

fun equal_natural _ _ = true

fun power A_ a Zero_nat = one (one_power A_)
  | power A_ a (Suc n) = times (times_power A_) a (power A_ a n);

fun plus_nat (Suc m) n = plus_nat m (Suc n)
  | plus_nat Zero_nat n = n;

val one_nat : nat = Suc Zero_nat;

fun nat_of_num (Bit1 n) = let
                            val m = nat_of_num n;
                          in
                            Suc (plus_nat m m)
                          end
  | nat_of_num (Bit0 n) = let
                            val m = nat_of_num n;
                          in
                            plus_nat m m
                          end
  | nat_of_num One = one_nat;

fun value dummy =
  (fn uu =>
    (if equal_natural uu 1
      then (fn genuine_only => fn _ =>
             ((if equal_buggy_type
                    (power power_buggy_type (Buggy_Type (Zero_int, one_int))
                      (nat_of_num (Bit0 One)))
                    (Buggy_Type (one_int, Zero_int))
                then NONE
                else SOME (true,
                            [(term_of_buggy_type
                                (Buggy_Type
                                  (one_int,
                                    Zero_int)) handle Match => (Const
                         ("Quickcheck_Exhaustive.unknown", Type
                     ("Scratch.buggy_type", [])))),
                              (term_of_buggy_type
                                 (power power_buggy_type
                                   (Buggy_Type (Zero_int, one_int))
                                   (nat_of_num
                                     (Bit0 One))) handle Match => (Const
                            ("Quickcheck_Exhaustive.unknown", Type
                        ("Scratch.buggy_type", []))))])) handle Match => (if genuine_only
                                   then NONE
                                   else SOME
  (false,
    [(term_of_buggy_type
        (Buggy_Type
          (one_int,
            Zero_int)) handle Match => (Const
 ("Quickcheck_Exhaustive.unknown", Type ("Scratch.buggy_type", [])))),
      (term_of_buggy_type
         (power power_buggy_type (Buggy_Type (Zero_int, one_int))
           (nat_of_num
             (Bit0 One))) handle Match => (Const
    ("Quickcheck_Exhaustive.unknown", Type
("Scratch.buggy_type", []))))]))))
      else (fn _ => fn _ => NONE)));

end;

Generated_Code.value () 1 true 1;
