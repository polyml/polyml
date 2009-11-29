(* Test to construct polytype from type abbreviation by opaque matching. *)

structure S:> sig eqtype 'a t val mkT: 'a -> 'a t end =
struct type 'a t = 'a fun mkT x = x end;

PolyML.makestring(S.mkT 1); (* Check the pretty-printer. *)

if S.mkT 1 = S.mkT 1 then () else raise Fail "error";

if S.mkT 1 = S.mkT 2 then raise Fail "error" else ();

(* And from datatype.  *)
structure S:> sig eqtype 'a t val mkT: 'a -> 'a t end =
struct datatype 'a t = X of 'a val mkT = X end;

PolyML.makestring(S.mkT 1);

if S.mkT 1 = S.mkT 1 then () else raise Fail "error";

if S.mkT 1 = S.mkT 2 then raise Fail "error" else ();

