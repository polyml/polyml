(* This caused a Subscript error when printing. *)

functor Longid (S: sig end):
sig
      datatype node = T of int
      type obj
      val r: obj
   end =
struct
datatype node = T of int
type obj = node list
val r = nil
end
;

functor AstAtoms () = 
struct
datatype T = A
datatype S = B


structure Longcon = Longid ()

end
;
