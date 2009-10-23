(* Equality for datatypes in signatures is more complicated if type abbreviations
   or sharing constraints have set types to equality types. *)
functor F(S: sig type t = int * int datatype s = X of t end) =
struct fun f(x, y: S.s) = x = y end;


signature Q = sig type a eqtype b end;

functor F(S: sig structure R : Q sharing type R.a = R.b datatype s = X of R.a end) =
struct fun f(x, y: S.s) = x = y end;

functor F(S: sig eqtype r structure R : Q where type a = r datatype s = X of R.a end) =
struct fun f(x, y: S.s) = x = y end;
