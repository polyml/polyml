(* Printing records as tuples. *)
if PolyML.makestring {1=true} = "{1 = true}"
then ()
else raise Fail "makestring";

if PolyML.makestring {1=true, 2=false} = "(true, false)"
then ()
else raise Fail "makestring";
