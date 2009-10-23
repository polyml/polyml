(* The following is wrong.  In Poly/ML the overloading
   becomes Word8.word and so the conversion fails. *)
let fun f x = x+0w1000 in f 0w1: Word8.word; "WRONG" end;
