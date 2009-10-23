(* Test for printing error messages with type abbreviations. *)

signature A =
sig
   type t = int
   type s
   type u = s * int
   val f : u -> t
   val x : u
end;

signature S =
sig
    structure A: A
    val f : A.u -> int
end;


signature T =
sig
    structure A: A
    val x: A.u
end;

functor F (structure S: S and T: T) =
struct
open S
val q = f T.x
end;

functor F (structure S: S and T: T) =
struct
open T
val q = S.f x
end;
