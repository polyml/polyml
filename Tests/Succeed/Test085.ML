(* Minimal version of example program that produced Subscript exception in SVN 913. *)

structure A =
struct
  structure B =
  struct
        datatype t = X
  end
end (* No semicolon here. *)

open A; (* Semicolon here. *)

B.X; (* Raised Subscript exception. *)

PolyML.prettyRepresentation (B.X, 2); (* Raised InternalError. *)
