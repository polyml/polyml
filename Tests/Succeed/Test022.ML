(* Reported by Makarius: the second case raised the Bind exception.
   I've added some more tests that also gave the wrong result because
   of the same bug. *)

datatype ('a, 'b) pair = Pair of 'a * 'b;

Pair (1, 1) = Pair (1, 1);
Pair (1, Pair (1, 1)) = Pair (1, Pair (1, 1));


val x = ref 1;
Pair (1, x) = Pair (1, x);
Pair (x, (1,1)) = Pair (x, (1,1));
Pair ((1,1), x) = Pair ((1,1), x);

datatype ('a, 'b, 'c) Triple = X of 'c | Y of 'b | Z of 'a;
(X 10000000000: (int ref, int ref, int) Triple) = X 10000000000;
(X 10000000000: (int, int ref, int) Triple) = X 10000000000;
(X 10000000000: (int ref, int, int) Triple) = X 10000000000;
