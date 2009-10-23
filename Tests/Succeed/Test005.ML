(* Bug in Word.word functions with top bit set. *)
let
   val i = ~ 0w4;
   val j = Word.toInt i
in
if Word.toInt(i mod 0w272) = j mod 272
then () else raise Fail "WRONG"
end;
