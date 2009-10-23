(* This segfaulted on x86-64 with 5.3rc1 *)
datatype ('a, 'b, 'c, 'd, 'e, 'f, 'g) t = X of 'a | Y of 'b;
(X 1: (int, int, int, int, int, int, int) t) = X 1;
