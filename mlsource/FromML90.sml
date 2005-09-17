(* Extra declarations needed to bootstrap the old compiler. *)
(* These are declared in General in the standard basis library which
   is opened at the top level. *)
exception Subscript = Array.Subscript
exception Size = Array.Size;
exception Overflow = RunCall.Inner.Overflow;
PolyML.use "mlsource/prelude/RuntimeCalls";
structure Vector =
   struct
   open Vector
   val fromList = vector
   end;
