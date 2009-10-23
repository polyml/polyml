(* This is wrong but is allowed in 5.2 *)

signature S = sig datatype x = A | B end;

structure S: S =
struct
    structure T = struct val A = 1 end
    datatype x = A | B
    open T
end;
