(* Inlined loops with more loop variables than registers to hold all of them, at least on X86/32.
   This was originally just a coverage test but it turned out to be broken in 5.3 and earlier.
   Since this will go into an infinite loop there's an explicit check here. *)

if PolyML.Compiler.compilerVersionNumber <= 530 then raise Fail "Will loop" else ();

PolyML.Compiler.maxInlineSize := 100;
fun f x =
let
    fun f (i, j, k, l, m, n, p, q) =
    if i = 10 then q
    else f(i+1, j+1, k+1, l+1, m+1, n+1, p+1, q+1)
in
    f(x, x+1, x+2, x+3, x+4, x+5, x+6, x+7)
end;

if f 1 = 17 then () else raise Fail "failed";

