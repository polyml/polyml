(* This fails because z is polymorphic. *)
structure S:
sig
    val z: 'a -> 'a
end =
struct
    fun f i = i
    val z = f f
end;
