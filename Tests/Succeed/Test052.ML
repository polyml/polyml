(* This produced a signature match error in the structure declaration even
   though it had already matched in the functor result. *)


signature LONGID = sig type node type t val x: t end; 

functor Longid (): LONGID =
struct
    datatype node = T 

    type t = node list
    val x = nil
end;

signature AST_ATOMS =
   sig
      structure Longcon: LONGID

      structure Longvid:
         sig
            type t
            val longc: Longcon.t
         end
end
;

functor AstAtoms (): AST_ATOMS = 
struct

structure Longcon = Longid ()


structure Longvid =
   struct
      structure L = Longid ()

      open L
      val longc = Longcon.x

    end
end;


structure AstCore = AstAtoms () : AST_ATOMS;



