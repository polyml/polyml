(* A little bit of Isabelle that provoked a bug. The internal expansions of
   prod_ord were being treated as recursive. *)

fun prod_ord a_ord b_ord ((x, y), (x', y')) =
  (case a_ord (x, x') of EQUAL => b_ord (y, y') | ord => ord);

val int_ord = Int.compare;

fun hd_ord f_ord (f, g) =
  prod_ord (prod_ord int_ord (prod_ord int_ord int_ord)) int_ord (f, g);

(*
 (* It should be equivalent to this but only if the expansions of the
    arguments are done first. *)
fun prod_ord8(a_ord, b_ord, x, y, x', y') =
   (case a_ord (x, x') of EQUAL => b_ord (y, y') | ord => ord);


fun hd_ord5(f_ord, f, f', g, g') =
    prod_ord8(fn ((x, x'), (y, y')) =>
            prod_ord8(int_ord, fn ((p, p'), (q, q')) =>
                  prod_ord8(int_ord, int_ord, p, q, p', q'),
                  x, y, x', y'),
            int_ord, f, g, f', g');
*)
