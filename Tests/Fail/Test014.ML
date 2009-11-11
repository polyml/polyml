(* Should fail - Checks that we get the updated generic information. *)
let
    fun g (x as {b,...}) = {c=b, d=b}
    fun i (x as {a,b,...}) = g (g x)
in
    1
end;
