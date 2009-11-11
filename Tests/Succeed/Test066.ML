(* Example of datatype replication from the Definition of Standard ML. *)
datatype t = C | D;
signature SIG =
sig
    datatype t = datatype t
    val f : t -> t
end;
