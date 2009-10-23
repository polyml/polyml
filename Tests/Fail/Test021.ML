(* Check that successive right assocative operators work correctly. *)

(* Include the default precedences just so we're clear. *)
infix  7  * / div mod
infix  6  + - ^
infixr 6  --
infixr 5  :: @ ++;
val op ++ = op :: ;
val op -- = op -;

(* This is wrong but was not faulted in 5.2 and earlier.
   Must not mix left- and right-associative operators of the same precedence. *)
val q = 1 -- 2 * 3 + 4;
