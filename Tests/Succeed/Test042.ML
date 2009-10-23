functor BasicNameSetFun(N : sig type name end) =
struct
    structure N = N
    datatype T = Empty
end;

datatype name = A (* N.B. No semicolon.  *) 
structure R = struct type name = name list end
structure Names = BasicNameSetFun(R);

