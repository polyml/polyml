(* Some parsing problems.  All of these were rejected by 4.1.1. *)

(* This is valid but caused problems.  I think it's a relic of SML90. *)
type t = int
signature S = sig end;

(* This is valid.  Empty specifications are legal. *)
signature S = sig ; ; ; end;

(* The syntax here is odd.  It requires the word "type" to be repeated.
   That's to distinguish between
   signature T = sig ... end where type s = int and type t = bool
   and
   signature T = sig ... end where type s = int and S = S'
   In the latter case we're defining a new signature. *)
signature T = sig type s type t end where type s = int and type t = bool
and U = S;
