(* Previously the lexical analyser assumed that a number followed by E or e
   was always a real number and if there was no valid number after it this
   was an error. *)

val e = ("a", "b") ;   (* Vesa Karvonen's original example. *)
#1e ;

(* Other test cases. *)
val e = 3;
fun f a b c = c(a+b);
f 1e~;

val example = 1 and Example = 2;
f 1example~;
f 1Example~;

val e = 1.1;
fun f (a: real) b c = c(a+b);
f 1.2e~;
