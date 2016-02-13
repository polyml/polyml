(* Bug in Word.word functions with top bit set. *)
let
   val i = ~ 0w4;
   val j = Word.toLargeInt i
in
if Word.toLargeInt(i mod 0w272) = j mod 272
then () else raise Fail "WRONG"
end;
