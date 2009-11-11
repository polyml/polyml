(* Various tests.  Some of these are included elsewhere in this directory. *)
signature S =
sig
    type s
    type t
    sharing type s = t
end;

signature S =
sig
    type s
    datatype t = C
    sharing type s = t
end;

signature S =
sig
    structure T:
    sig
        datatype t = X of int
    end
    datatype s = Y of bool;
    sharing type s = T.t
end;

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
(*structure Z =
  F(structure S = struct datatype t = C of int -> int type s = t end);*)

(* The following is legal because sharing between an eqtype and
   a type makes the type into an eqatype. *)
functor F(structure S:
    sig
        eqtype s
        type t
        val x: t
        sharing type s = t
    end) =
struct
    val q = S.x = S.x
end;


(* This was previously rejected but is actually legal. *)
signature S =
sig
    datatype t = C
end where type t = bool;

(* I don't know whether this is legal or not. *)
signature S =
sig
    eqtype t
end where type t = int;

(* This is legal. *)
signature S =
sig
    type t
    type u
    sharing type t = u
end where type u = int;


(* This is legal.  A and C share so their types share. *)
functor F(
    structure A: sig type t val x: t end
    structure B: sig end
    structure C: sig type t end
    sharing A = B = C
) =
struct
    val q = A.x : C.t
end;
