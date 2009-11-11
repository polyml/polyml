(* Example of datatype replication from the Definition of Standard ML. *)
datatype t0 = C;
structure A: sig type t val C: t end =
struct
    datatype t = datatype t0
end;
