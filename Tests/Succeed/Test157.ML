(* Something that looks like it should be an integer or word constant
   but isn't should be lexically analysed as an integer constant
   followed by an identifier.  This is similar to the problem
   with real constants identified in Test026. *)

fun test 0 _ = () | test _ _ = raise Fail "wrong";

val () = let val xz = 1 in test 0xz end;
val () = let val wxz = 1 in test 0wxz end;
val () = let val xw9 = 1 in test 0xw9 end;
val () = let val x = 1 in test 0x; () end;

(* Another one identified. *)
val () = let val w1 = 1 in test ~0w1 end;
val () = let val xw1 = 1 in test ~0xw1 end;
