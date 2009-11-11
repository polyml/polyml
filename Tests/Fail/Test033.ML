(* It seems that ML97 allows free type variables in datatypes, unlike
   ML90.  Actually that was an error so this is the same as p014.
   This fails because t is not an equality type. *)
fun f x =
    let datatype t = A of 'a val b = A x in if b = b then x else x end;
