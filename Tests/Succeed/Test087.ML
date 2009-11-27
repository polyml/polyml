(* Tests for handling of complex polymorphic declarations. *)

let
    fun f x = x
    val c = (f, f)
    val (a, b) = c
in
    a(b 1)
end; 

let
    val c = (nil, nil)
    
    fun f(a::_, b::_) = a+b | f _ = 0
in
    f c
end;


let
    val a = { x=NONE, y=NONE }
    fun f {x=SOME (x: int), y=SOME(y: int) } = x + y | f _ = 0
in
    f a
end;

let
    val SOME f = SOME (fn x => x)
in
    f (f 1)
end;
