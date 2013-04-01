(* Check the pathological cases of exception aliasing. *)

fun check true = ()
|   check false = raise Fail "Wrong";


exception R of int and Q of int;

functor F (exception A of int) = struct fun f (R 1) = 1 | f (A 2) = 2 | f (R 2) = 3 | f (A 1) = 4 | f _ = 5 end;

structure A = F(exception A = R);
check(A.f(R 2) = 2);
check(A.f(R 1) = 1);

structure B = F(exception A = Q);
check(B.f(R 2) = 3);
check(B.f(R 1) = 1);



