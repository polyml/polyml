(* Bug in SVN 630. *)
(* The problem was that the bound variable count was being reset so that
   tt was getting the same bound variable as S.t. *)

functor U (S: sig type t val y: t end) =
   struct
      structure T: sig val x: S.t type tt end =
         struct
              val x = S.y
              type tt = int
         end
   end;
