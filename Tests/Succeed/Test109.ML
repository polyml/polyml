(* This produced an InternalError exception.  That check has been removed but the
   test has been included because it may be useful to revisit this. *)

type polarity = int


datatype rep =
    Atom of int * int |
    Struct of rep list |
    Vect of int * rep |
    Func of rep * rep
;
exception REP of string * rep list;
exception LIMIT of string * string;

PolyML.Compiler.maxInlineSize := 1;
fun card_of_rep (_: rep): int = (PolyML.print "card_of_rep"; raise Match);
fun is_lone_rep (_: rep): bool = (PolyML.print "is_lone_rep"; raise Match);
fun kk_case_switch _ = (PolyML.print "kk_case_switch"; raise Match);
fun all_singletons_for_rep _ = (PolyML.print "all_singletons_for_rep"; raise Match);

PolyML.Compiler.maxInlineSize := 60;

structure S
 =
struct

exception E

fun vect_from_rel_expr a b c d e = (
    PolyML.print a;
    PolyML.print b;
    PolyML.print c;
    PolyML.print d;
    PolyML.print e;
    PolyML.print a;
    PolyML.print b;
    PolyML.print c;
    raise E
);

fun lone_rep_fallback kk new_R old_R r =
  if old_R = new_R then
    r
  else
    let val card = card_of_rep old_R in
      if is_lone_rep old_R andalso is_lone_rep new_R
         andalso card = card_of_rep new_R then
        if card >= 4096 then
          raise LIMIT ("lone_rep_fallback",
                       "too high cardinality (" ^ Int.toString card ^ ")")
        else
          kk_case_switch kk old_R new_R r (all_singletons_for_rep old_R)
                         (all_singletons_for_rep new_R)
      else
        raise REP ("lone_rep_fallback", [old_R, new_R])
    end

and atom_from_rel_expr kk (x as (k, j0)) old_R r =
  case old_R of
    Func (R1, R2) =>
    let
      val dom_card = card_of_rep R1
      val R2' = case R2 of Atom _ => R2 | _ => Atom (card_of_rep R2, 0)
    in
      atom_from_rel_expr kk x (Vect (dom_card, R2'))
                         (vect_from_rel_expr kk dom_card R2' old_R r)
    end
  | _ => lone_rep_fallback kk (Atom x) old_R r


fun yy a kk x c d y =
    (if a then atom_from_rel_expr kk (c, d) else fn _ => raise Match) y
end
;
