(* IPv6 test - Test078 converted to IPv6 *)
val x = INet6Sock.TCP.socket(): Socket.passive INet6Sock.stream_sock
and y = INet6Sock.TCP.socket(): Socket.active INet6Sock.stream_sock;
val SOME me = Net6HostDB.getByName "localhost";
val localhost = Net6HostDB.addr me;
Socket.bind(x, INet6Sock.toAddr(localhost, 0));
Socket.listen(x, 5);
let
val (_, port) = INet6Sock.fromAddr(Socket.Ctl.getSockName x)
in
Socket.connect(y, INet6Sock.toAddr(localhost, port))
end;
val (cnct, _) = Socket.accept x;

(* Send the data. Use a separate thread so there's no possibility of blocking. *)
let
    fun sendData() =
        (Socket.sendVec(y, Word8VectorSlice.full(Byte.stringToBytes "A test")); ())
in
    Thread.Thread.fork(sendData, [])
end;

if Byte.bytesToString(Socket.recvVec(cnct, 6)) <> "A test"
then raise Fail "failed"
else ();

Socket.close cnct;
Socket.close x;
Socket.close y;

