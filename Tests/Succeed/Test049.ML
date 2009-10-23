(* This broke in 5.3 RC1 *)

signature A = sig datatype t = A | B end;
structure A: A = struct datatype t = A | B end;
structure B = struct open A datatype s = datatype t end;

(* This wasn't a problem, but include it anyway. *)
structure B = struct open A datatype s = datatype A.t end;
