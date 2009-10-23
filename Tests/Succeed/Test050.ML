(* This produced a Subscript error in the printing code. *)


functor F(S:sig type T end) =
struct type T = S.T end;

structure S = struct
datatype a = B
datatype t = A

structure R = F(type T = (int * t) list);
type T = R.T
end;
