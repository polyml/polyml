(* Test for equality of local withtype. *)
fun f x =
let
    datatype t = A | B of s
    withtype s = t
    
    fun f (x: s, y) = x = y
in
    0
end;

(* Test of equality involving withtype.  This failed to match because
   the datatypes in the structure were not detected as equality types. *)
signature TEST_1 =
sig
    datatype a = A1 of b
    and b = B1 of a list * b list
end;

structure Test1 : TEST_1 =
struct
    datatype a = A1 of c
    and b = B1 of a list * c list
    withtype c = b
end;

(* Type functions are equivalent to their definitions so this is legal. *)
functor F(): sig eqtype t1 and t2 end =
struct
    datatype t1 = A1 of (real, real, int) t3
    and t2 = B1 of t1 list * (real, real, int) t3 list
    withtype ('a, 'b, 'c) t3 = 'a ref * 'c * t2 (* 'b is not used. *)
    
    fun f (x: t1, y) = x = y
end;
