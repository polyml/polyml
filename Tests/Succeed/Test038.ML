(* Check that successive right assocative operators work correctly. *)

(* Include the default precedences just so we're clear. *)
infix  7  * / div mod
infix  6  + - ^
infixr 6  --
infixr 5  :: @ ++;
val op ++ = op :: ;
val op -- = op -;

(* This failed in 5.2 and earlier but is valid. *)
val q = 1 :: 1 + 2 ++ 3*4 :: nil;

(* This failed in SVN prior to SVN 639. *)
val q = 1 :: 1 + 2 :: 3*4 :: nil;
