(* Code-generation bug. *)
let
fun f x =
    if x = 1 then "a" else if x = 2 then "b" else if x = 3 then "c"
    (* Note duplication of the next case. *)
    else if x = 6 then "d" else if x = 6 then "f"
    else (* Missing case here *) if x = 8 then "h"
    else if x = 9 then "i" else if x = 10 then "j"
    else if x = 11 then "k" else "l";
in
if f 8 = "h" andalso f 6 = "d" andalso f 7 = "l"
then "OK"
else raise Fail "FAIL"
end;
