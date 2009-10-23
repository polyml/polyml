(* After I removed the restriction on free type
   variables in type constructors this compiled.
   Now fixed. *)
fun 'a f x = let type t = 'a list in [x]:t end;
f 3: bool list;
