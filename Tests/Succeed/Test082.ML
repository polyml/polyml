(* Variant of p060.  This tests the error when the connection is refused. *)
val x = INetSock.TCP.socket(): Socket.passive INetSock.stream_sock
and y = INetSock.TCP.socket(): Socket.active INetSock.stream_sock;
val SOME me = NetHostDB.getByName "localhost";
val localhost = NetHostDB.addr me;
Socket.bind(x, INetSock.any 0);
(* Socket.listen(x, 5); *)
(
let
val (_, port) = INetSock.fromAddr(Socket.Ctl.getSockName x)
in
Socket.connect(y, INetSock.toAddr(localhost, port));

(* Shouldn't get here. *)
raise Fail "wrong"

end
) handle OS.SysErr(_, SOME e) => if e = valOf(OS.syserror "ECONNREFUSED") then () else raise Fail "bad error";

(* We could also get ECONNABORTED here perhaps. *)
