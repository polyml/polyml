(* Test of code-generation of equality across signature matching. *)

structure S:> sig eqtype 'a t val mkT: 'a * int -> 'a t end =
struct
    datatype 'a X = X of 'a
    type 'a t = 'a X * int X
    fun mkT(x, n) = (X x, X n)
end;

if S.mkT(3, 4) = S.mkT(3, 4) then () else raise Fail "wrong";
