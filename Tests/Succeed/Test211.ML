(* Flexible record frozen by a signature. *)

structure Foo: sig type t val pred : t -> bool end =
struct
    type t = string * int
    
    fun f _ = true
    val pred = f o #1
end;
