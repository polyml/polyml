(* Bug detected in Poly/ML 26/10/99. *)
(* This function permits equality on functions due to a failure to
   propagate the equality attribute on flexible records. *)

let
fun eqforfun (x: int->int, y: int->int) =
    case ({a=1,b=x}, {a=1,b=y}) of
        (xx as {a=1,...}, yy as {a=1, ...}) => xx = yy
      | _ => false
in
eqforfun(fn x=>x, fn x=>x)
end;
