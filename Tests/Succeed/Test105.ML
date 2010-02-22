(* Another Isabelle crash: This caused a segfault. *)

datatype term = Const of string * int | $ of term * term;

PolyML.Compiler.maxInlineSize := 1;
fun aeconv(t, u) = case (t, u) of (Const(s, _), Const(y, _)) => s = y | _ => false;

PolyML.Compiler.maxInlineSize := 80;

infix 9 $;
exception TERM of string * term list;

fun dest_equals (Const ("==", _) $ t $ u) = (t, u)
  | dest_equals t = raise TERM ("dest_equals", [t]);

datatype thm = Thm of int * {tpairs: (term * term) list, prop: term};

fun is_trivial(Thm (_, {prop, ...})) = aeconv(dest_equals prop);

fun doFilter raw_eqs = List.filter is_trivial raw_eqs;

doFilter[Thm(1, {tpairs=[], prop=Const("==", 0) $ Const("a", 0) $ Const("a", 0)})];
