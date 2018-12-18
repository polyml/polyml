(* This piece of Isabelle code caused an exception in the code-generator. *)


datatype task = Task of {name: string, id: int, pri: int};
  

val keyord = Int.compare;

fun rev_order LESS = GREATER
  | rev_order EQUAL = EQUAL
  | rev_order GREATER = LESS;

fun prod_ord a_ord b_ord ((x, y), (x', y')) =
  (case a_ord (x, x') of EQUAL => b_ord (y, y') | ord => ord);
  

val int_ord = Int.compare;


fun task_ord (Task {id = id1, pri = pri1, ...}, Task {id = id2, pri = pri2, ...}) =
  prod_ord (rev_order o int_ord) int_ord ((pri1, id1), (pri2, id2));
