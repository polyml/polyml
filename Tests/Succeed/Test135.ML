(* Bug in interaction between the debugger and pattern matching - this crashed. *)

PolyML.Compiler.debug := true;

fun processConstrs(_, 0) = ()
|   processConstrs([], _) = ()
|   processConstrs({a, b} :: next, toDo) =
    (
        processConstrs(next, toDo-1)
    );

processConstrs([{a=1, b=2}], 1);
