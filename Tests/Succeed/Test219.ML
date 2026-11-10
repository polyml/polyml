(* INetSock.fromAddr read the port through a pointer into the socket address
   after allocating, so a GC could move the address first. *)

local
    val a = valOf (NetHostDB.fromString "10.1.2.3")
    fun loop 0 = ()
    |   loop k =
        let
            val p = k mod 65536
            val (a', p') = INetSock.fromAddr (INetSock.toAddr (a, p))
        in
            if p = p' andalso NetHostDB.toString a' = "10.1.2.3" then loop (k - 1) else raise Fail "wrong address"
        end
in
    val () = loop 1000000
end;
