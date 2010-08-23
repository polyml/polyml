(* Bug in installing a pretty printer for datatypes with more than one type argument.
   Reported by Makarius in SVN 1193. *)

datatype ('a, 'b) foo = Foo of 'a * 'b;

PolyML.addPrettyPrinter (fn depth => fn (pretty1, pretty2) => 
fn Foo (x, y) => PolyML.PrettyBlock (2, false, [],
       [PolyML.PrettyString "FOO", PolyML.PrettyBreak (1, 0),
        PolyML.PrettyString "(",
        pretty1 (x, depth - 1),
        PolyML.PrettyString ",", PolyML.PrettyBreak (1, 0),
        pretty2 (y, depth - 1),
        PolyML.PrettyString ")"]));

if PolyML.makestring(Foo(100, "abc")) = "FOO (100, \"abc\")"
then ()
else raise Fail "pretty printing";
