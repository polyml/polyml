(* Datatype replication should export the constructors from an abstype. *)

abstype t = X with datatype p = datatype t end;
val q = X;
