(* Bug in transformation of arguments which are curried functions.  It is not
   safe to transform "f" in the argument to "bar".  Although it is curried
   the application to the first argument "()" is not immediately followed
   by the application to the second. *)

local
    val r = ref 0
in
    (* Foo should be called exactly once *)
    fun foo () = (r:= !r+1; fn i => i)

    fun checkOnce () = if !r = 1 then () else raise Fail "bad"
end;

fun bar f = let val r = f() in (r 1; r 2; List.map r [1, 2, 3]) end;

bar foo;

checkOnce();

exception A and B and C;
fun rA () = raise A and rB () = raise B;
fun h (f, g) = let val a = f() in g(); a () end;
h(rA, rB) handle A => ();
