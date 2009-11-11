(* It seems that ML97 allows free type variables in datatypes, unlike
   ML90. *)
(* Actually that appears to have been an error, so this is now in the
   Invalid directory. *)
fun f x =
    let datatype t = A of 'a val b = A x val A c = b in c end;
