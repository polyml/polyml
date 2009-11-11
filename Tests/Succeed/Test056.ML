(* Checks on free type variables arising from expansive expressions. *)
functor F() =
struct
    fun f i = i
    fun z x = (f f) x
end;

structure Q = F();
Q.z 1;

(* This is acceptable because the signature coerces the
   type to a monotype. *)
structure S:
sig
    val z: int -> int
end =
struct
    fun f i = i
    val z = f f
end;
