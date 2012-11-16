(* Bug test for pushing a value that won't fit in a 32-bit literal. *)

(* Original test from Rene Neumann *)

datatype bexp = Tt | Ff | V of IntInf.int | Not of bexp | And of bexp * bexp |
  Or of bexp * bexp;

fun bexp_rec f1 f2 f3 f4 f5 f6 Tt = f1
  | bexp_rec f1 f2 f3 f4 f5 f6 Ff = f2
  | bexp_rec f1 f2 f3 f4 f5 f6 (V nat) = f3 nat
  | bexp_rec f1 f2 f3 f4 f5 f6 (Not bexp) =
    f4 bexp (bexp_rec f1 f2 f3 f4 f5 f6 bexp)
  | bexp_rec f1 f2 f3 f4 f5 f6 (And (bexp1, bexp2)) =
    f5 bexp1 bexp2 (bexp_rec f1 f2 f3 f4 f5 f6 bexp1)
      (bexp_rec f1 f2 f3 f4 f5 f6 bexp2)
  | bexp_rec f1 f2 f3 f4 f5 f6 (Or (bexp1, bexp2)) =
    f6 bexp1 bexp2 (bexp_rec f1 f2 f3 f4 f5 f6 bexp1)
      (bexp_rec f1 f2 f3 f4 f5 f6 bexp2);

fun hashcode_bexp x =
  bexp_rec (2037113697 : IntInf.int) (1965105786 : IntInf.int)
    (fn x_0 =>
      IntInf.+ (IntInf.* ((1562730118 : IntInf.int), x_0),
        (1569390485 : IntInf.int)))
    (fn _ => fn res_0 =>
      IntInf.+ (IntInf.* ((1384320508 : IntInf.int), res_0),
        (1390980875 : IntInf.int)))
    (fn _ => fn _ => fn res_0 => fn res_1 =>
      IntInf.+ (IntInf.* ((238441399 : IntInf.int), res_0),
        IntInf.+ (IntInf.* ((231781032 : IntInf.int), res_1),
          (245101766 : IntInf.int))))
    (fn _ => fn _ => fn res_0 => fn res_1 =>
      IntInf.+ (IntInf.* ((2025552342 : IntInf.int), res_0),
        IntInf.+ (IntInf.* ((2018891975 : IntInf.int), res_1),
          (2032212709 : IntInf.int))))
    x;

(* Alternative test. *)

PolyML.Compiler.maxInlineSize := 1;

fun f (a,b,c,d,e,f) = f;

if f(0,0,0,0,0,2037113697) = 2037113697
then ()
else raise Fail "Compilation error";
