(* Exporting datatype constructors from structures. *)
structure T1 =
struct
    datatype f = A | B
end;
open T1;

structure T2: sig datatype f = A | B end =
struct
    datatype f = A | B
end;
open T2;

(* Explicit signatures cannot rebind constructors but that doesn't
   apply to the result if there's no signature. *)
structure T3 =
struct
    datatype f = A of bool | B
    datatype g = A of int
end;
open T3;
A 1;
datatype t = datatype f;
A true;

functor F1() =
struct
    datatype f = A of bool | B
    datatype g = A of int
end;
structure T4 = F1();
open T4;
A 1;
datatype t = datatype f;
A true;
