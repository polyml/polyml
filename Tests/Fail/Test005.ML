(* Checks on free type variables arising from expansive expressions. *)
functor F() =
struct
    fun f i = i
    val z = f f
end;

structure S = F();
S.z 1;
