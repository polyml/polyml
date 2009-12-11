
datatype s = X;

structure S: sig datatype t = X end where type t = s = struct datatype t = Y end;
