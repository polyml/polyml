(* The following is legal in ML97 but not in ML90. *)
functor F(
    structure S:
    sig
        eqtype s
        datatype t = C of int -> int
        sharing type s = t
    end) =
struct
    val x:S.s = S.C(fn i => i+1)
    val y = x = x
end;

(* But of course this must fail. *)
structure Z =
  F(structure S = struct datatype t = C of int -> int type s = t end);
