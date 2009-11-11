(* This is valid in ML90 which maximises equality after applying
   any sharing constraints but should fail in ML97. *)

functor F(
    S:  sig
        type s
        datatype t = C of s
        eqtype u
        sharing type s = u
        val x : s
        end
    )
=
    struct
    fun f y = S.C S.x = y
    end;
