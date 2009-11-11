(* This is rejected even though the second "where type" doesn't
   change anything. *)
(* In 5.3 the message isn't very helpful. *)
signature S =
sig
    type t
    type u
    sharing type t = u
end where type u = int where type t = int;
