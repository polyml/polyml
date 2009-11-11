(* This failed with the latest changes.  We need "open" to process the
   constructor list of a datatype. *)
functor F(S: sig type s datatype t = A | B of s end) =
    struct
    open S
    datatype p = datatype t
    end;
structure S = F(type s = int*int datatype t = A | B of int*int);
S.A;
