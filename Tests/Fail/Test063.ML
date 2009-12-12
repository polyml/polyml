(* This was incorrectly accepted in 5.3 *)
structure S: sig datatype a = A | B | D end = struct datatype a = A | B | C | D end;
