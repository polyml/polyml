(* The identity type constructor complicates the matching since it is the
   only case where an explicit type variable can match a type construction. *)

functor F (RNGY : sig type t end) :
   sig
   type 'a tt
   (*type 'a c*)
   val ir : ('a -> 'a tt) -> ('a->'a) -> 'a -> 'a tt

   structure RNGX : sig type t end
   end
 = struct
 
   structure RNGX = struct type t = RNGY.t end

   type 'a c = 'a
   type 'a tt = RNGY.t -> 'a c

   fun ir bir b2a xy nr = b2a( (bir xy) nr)
 
end;

type 'a Id = 'a;
fun f (x: 'a Id -> 'a) = ();

val g = f : ('a Id -> 'a) -> unit;
val h : ('a Id -> 'a) -> unit = f;
