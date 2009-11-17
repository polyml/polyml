(* Examples of type definitions in which unwrapping the type
   construction gets back to a type variable.  This used to work
   for a single level but has now been fixed for arbitrary levels. *)

type 'a t = 'a;
let val f : 'a -> 'a t t t t = fn x => x in f 1 end;

type ('a,'b) x = 'a;
let val f : 'a -> (('a, bool) x, int) x = fn x => x in f 1 end;

(* Discovered another example. *)
let val f : 'a -> (('a, bool) x, int) x = fn x => x fun g () = if true then f 1 else f 2 in g end;
