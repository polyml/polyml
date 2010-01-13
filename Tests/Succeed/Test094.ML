(* The code-generator rewrites if..then into indexed cases.
   This is a check that it correctly ignores duplicates. *)

fun f i =
    if i = 0 then 0
    else if i = 1 then 1
    else if i = 2 then 2
    else if i = 3 then 3
    else if i = 4 then 4
    else if i = 5 then 5
    else if i = 6 then 6
    else if i = 7 then 7
    else if i = 8 then 8
    else if i = 0 then raise Fail "bad"
    else if i = 1 then raise Fail "bad"
    else if i = 2 then raise Fail "bad"
    else if i = 3 then raise Fail "bad"
    else if i = 4 then raise Fail "bad"
    else if i = 5 then raise Fail "bad"
    else if i = 6 then raise Fail "bad"
    else 7;

fun test n = if n = 8 then () else (f n; test(n+1));

test 0;
