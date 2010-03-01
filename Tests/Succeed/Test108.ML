(* Code generator bug. *)

fun prod_ord a_ord b_ord ((x, y), (x', y')) =
  (case a_ord (x, x') of EQUAL => b_ord (y, y') | ord => ord);

fun fast_term_ord (_: int*int) = EQUAL;

val term2_ord = prod_ord fast_term_ord fast_term_ord;

val ord = prod_ord fast_term_ord term2_ord;
