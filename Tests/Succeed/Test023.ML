(*
To check that we've handled equality correctly on datatypes whose representations
aren't known when we compile the functor.
*)
functor F(S: sig eqtype t datatype s = X | Y of t end) =
struct
    fun eq(x: S.s, y) = if x = y then () else raise Fail "FAIL"
end;

datatype s = X | Y of int;
structure R = F(struct datatype s = datatype s type t = int end);
R.eq(X, X);
R.eq(Y 9, Y 9);

datatype s = X | Y of int * int;
structure R = F(struct datatype s = datatype s type t = int * int end);
R.eq(X, X);
R.eq(Y(4,5), Y(4, 5));
