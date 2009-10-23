(* A type constraint was missing in the REAL signature. *)


functor F (R: REAL) =
struct
  fun f(x: R.real) = R.+(x, R.Math.tan x)
end;
