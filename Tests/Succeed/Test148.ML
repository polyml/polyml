(* This caused an InternalError exception during the "open".  The problem was
   that the "garbage-collector" introduced in commit 1893 had a bug where it
   assumed that if it processed the "general" value for a binding the
   binding had been fully processed.  However the "general" value for
   "tuple" is reachable both through the closure of "f" and also through
   the entry in the structure itself but only the latter has the "special"
   entries that refer to the fields. *)


local
   val r : ((int -> int) * (int->int) * int ref * int ref) option ref = ref NONE
in
   fun doDo p = r := SOME p
end;


structure Test =
struct

    val tuple = (fn _ => raise Fail "bad", fn _ => raise Fail "bad", ref 0, ref 0)

    fun f _ = (doDo tuple; 0)

end;

open Test;

