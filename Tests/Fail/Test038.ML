(* This is illegal. *)
signature S =
sig
    type t
    type u
    type s = int -> int
    sharing type t = u = s
end;
