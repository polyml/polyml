(* Various tests. *)
signature S =
sig
    type s
    type t = s*s
    sharing type s = t
end;
