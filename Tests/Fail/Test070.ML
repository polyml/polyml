(* Bug with polymorphic variables.  This is arguably a loop-hole in the Definition
   of Standard ML.  This has been a long-standing bug but only showed up when
   it caused run-time errors in 5.4 SVN. *)

structure S =
struct datatype ref = datatype ref end;

fun f () =
let
    val a = S.ref []
    val _ = a := [1]
in
    !a
end;

f(): (int->int) list;



