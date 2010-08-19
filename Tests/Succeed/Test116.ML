(* This produced an InternalError exception in SVN 1192. *)

structure Q = struct type t = int fun s _ = 0 end;

(* There are three properties of this example which are needed to show the bug.
   There must be no result signature on the functor.
   There must be no semicolon between the functor declaration and the
   structure.
   There must be a result signature on the structure declaration. *)

functor F(S: sig type t val s: string -> t end)
= struct type t = S.t val s = S.s end

(* N.B.  No semicolon here. *)

structure T: sig type t val s: string -> t end = F(Q)
;

(* Additional check that F can be applied to more than one type. *)
structure R = struct type t = string fun s _ = "hello" end;

structure A: sig type t=string val s: string -> t end = F(R);
