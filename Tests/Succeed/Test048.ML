(* The Definition does not actually put any restriction on what
   can follow a datatype replication so the following would
   appear to be legal. *)

structure S2: sig datatype t = datatype char end
 = struct datatype t = datatype char end;

type 'a s = 'a * 'a;
datatype t = datatype s;
