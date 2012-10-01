(* Fix to bug in code-generation for structure matching.  If the only
   difference between the original and final structure was a "coercion"
   from a constructor to a value the code to do this was lost. *)

structure S1 = struct datatype s = A of int end;
structure S2 : sig type s val A: int -> s end = S1;
S2.A 1;


signature S = sig val e: exn end;
structure s : S = struct exception e end;
s.e;
