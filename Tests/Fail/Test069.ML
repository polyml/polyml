(* Check for escaping datatypes.  Another case. *)

functor F() = struct datatype s = S end

local
    val a = ref nil
    structure R = F()
    open R
in
    val () = a := [S]
end;
