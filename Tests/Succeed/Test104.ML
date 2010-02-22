(* This caused an InternalError exception in the compiler. *)

PolyML.Compiler.maxInlineSize := 80;
type typ = string;
type indexname = int;

datatype term =
    Const of string * typ |
    Abs of string * typ * term |
    $ of term * term;

infix 9 $;
exception TERM of string * term list;

fun dest_equals (Const ("==", _) $ t $ u) = (t, u)
  | dest_equals t = raise TERM ("dest_equals", [t]);
  
fun term_depth (Abs (_, _, t)) = 1 + term_depth t
  | term_depth _ = 0;

datatype thm = Thm of int *  {maxidx: int, prop: term}

fun rep_thm (Thm (_, args)) = args;

val lhs_of_thm = #1 o dest_equals o #prop o rep_thm;

fun make keyfun =
  let fun keypair x = (x, keyfun x) in map keypair end;

fun sort_lhs_depths defs =
let
    val keylist = make (term_depth o lhs_of_thm) defs
in
    map #2 keylist
end;
