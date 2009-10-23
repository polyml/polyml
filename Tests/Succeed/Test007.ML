(* Check that mutable values are not "captured" at compile-time. *)
let
    val x = ref 0
    fun f () = !x
    val a = f()
in
    x := 1;
    if a = 0 andalso f() = 1 andalso !x = 1
    then ()
    else raise Fail "WRONG"
end;
