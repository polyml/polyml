(* This was forbidden in ML90 but is legal in ML97. *)

functor F(
    S:  sig
        eqtype s
        datatype t = C of int->int
        sharing type s = t
        end
    )
=
    struct
    end;
