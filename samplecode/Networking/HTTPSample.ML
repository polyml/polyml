(* Example code to read from the Poly/ML website. *)

let
val s : Socket.active INetSock.stream_sock = INetSock.TCP.socket();
val SOME poly = NetHostDB.getByName "www.polyml.org"
val addr = INetSock.toAddr(NetHostDB.addr poly, 80)
(* There seems to be a bug in Solaris 8 which means that select indicates that there is
   data to read even when there isn't. *)
fun readit () =
   case Socket.select{rds = [Socket.sockDesc s], wrs=[], exs=[], timeOut=SOME(Time.fromSeconds 10)} of
       { rds = [], ...} => ()
   |  _ => let val text = Socket.recvVec(s, 1)
           in if Word8Vector.length text = 0 then ()
                else ( print(Byte.bytesToString text); readit() )
           end
in
Socket.connect (s, addr);
Socket.sendVec(s, Word8VectorSlice.full(Byte.stringToBytes "GET / HTTP/1.1\r\nHost: www.polyml.org\r\n\r\n"));

readit();

Socket.close s
end;
