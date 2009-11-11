(* This fails in ML97 although it would work in ML90 because
   at the point where t is declared it is not an equality type. *)
functor F(structure S:
    sig
        type s
        datatype t = C of s
        val x: s
        eqtype u
        sharing type s = u
    end) =
struct
    val q = S.C S.x
    val r = q = q
end;

