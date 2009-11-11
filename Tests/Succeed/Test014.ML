(* Bug in handling of relational operators in LargeWord. *)

if LargeWord.< (0w1, 0w2) then () else raise Fail "WRONG";
if LargeWord.< (0w1, 0wx80000002) then () else raise Fail "WRONG";

if LargeWord.> (0w1, 0w2) then raise Fail "WRONG" else ();
if LargeWord.> (0w1, 0wx80000002) then raise Fail "WRONG" else ();

if LargeWord.<= (0w1, 0w2) then () else raise Fail "WRONG";
if LargeWord.<= (0w1, 0wx80000002) then () else raise Fail "WRONG";

if LargeWord.>= (0w1, 0w2) then raise Fail "WRONG" else ();
if LargeWord.>= (0w1, 0wx80000002) then raise Fail "WRONG" else ();

case LargeWord.compare(0w1, 0wx80000000) of
    LESS => ()
|   _ => raise Fail "WRONG";
