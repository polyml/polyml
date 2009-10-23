(* This should fail but is not correctly checked in 5.2. *)

let
    datatype t = A | B
    type s = t * int
    val q: s = (A, 1)
in
    q
end;
