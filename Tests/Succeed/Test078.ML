(* Test simple TCP connection.  There was a problem with "connect" on Windows. *)
val x = INetSock.TCP.socket(): Socket.passive INetSock.stream_sock
and y = INetSock.TCP.socket(): Socket.active INetSock.stream_sock;
val SOME me = NetHostDB.getByName "localhost";
val localhost = NetHostDB.addr me;
Socket.bind(x, INetSock.any 0);
Socket.listen(x, 5);
let
val (_, port) = INetSock.fromAddr(Socket.Ctl.getSockName x)
in
Socket.connect(y, INetSock.toAddr(localhost, port))
end;
Socket.accept x;
