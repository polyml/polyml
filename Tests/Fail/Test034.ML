(* Frank Pfenning's example. This is almost certainly wrong but depends a bit
   on what structure sharing implies for the types. *)
  signature A =
   sig
     datatype t = C | D
     type s = t * t
   end;

   signature B =
   sig
     structure S : A
   end;

   functor F (structure S' : A
          structure T1 : B
        sharing T1.S = S'
          structure T2 : B
        sharing T1.S = S') =
   struct
     type r = int
   end;
