(* Bug in optimiser transformation.  A function argument that returns a tuple
   can be transformed to take a container but only if it is not curried. *)

(* Cut down example from Isabelle that caused an internal error exception. *)

fun one _ [] = raise Fail "bad"
  | one pred (x :: xs) =
      if pred x then (x, xs) else raise Fail "bad";

fun foo (scan, f) xs = let val (x, y) = scan xs in (f x, y) end;
  
fun bar (scan1, scan2) xs =
  let
    val (x, ys) = scan1 xs;
    val (y, zs) = scan2 x ys;
  in ((x, y), zs) end;

fun bub (scan1, scan2) = foo(bar(scan1, (fn _ => scan2)), op ^);

val qqq: string list -> string * int = bub(one (fn _ => raise Match), (foo((fn _ => raise Match), String.concat)));

(* Further example - This caused a segfault. *)

PolyML.Compiler.maxInlineSize := 1;
fun f g = let val (x,y) = g 1 2 in x+y end;

fun r (x, y, z) = fn _ => (x, y+z);

val h: int-> int*int = r (4,5,6);

f (fn _ => h);
