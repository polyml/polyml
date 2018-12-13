(* Test select call.  Modified from Test083 on poll. *)
fun verify true = ()
|   verify false = raise Fail "wrong";

val x = INetSock.TCP.socket(): Socket.passive INetSock.stream_sock
and y = INetSock.TCP.socket(): Socket.active INetSock.stream_sock;
val SOME me = NetHostDB.getByName "localhost";
val localhost = NetHostDB.addr me;
Socket.bind(x, INetSock.toAddr(localhost, 0));
Socket.listen(x, 5);
Socket.acceptNB x;

val xDesc = Socket.sockDesc x and yDesc = Socket.sockDesc y;

val {rds, wrs, exs} = Socket.select{ rds=[xDesc, yDesc], wrs=[xDesc, yDesc], exs=[xDesc, yDesc], timeout=SOME(Time.fromSeconds 1)};

verify(null rds andalso null wrs andalso null exs);

let
    val (_, port) = INetSock.fromAddr(Socket.Ctl.getSockName x)
in
    Socket.connectNB(y, INetSock.toAddr(localhost, port))
end;

val {rds, wrs, exs} = Socket.select{ rds=[xDesc, yDesc], wrs=[xDesc, yDesc], exs=[xDesc, yDesc], timeout=SOME(Time.fromSeconds 1)};

verify(Socket.sameDesc(hd rds, xDesc));
verify(Socket.sameDesc(hd wrs, yDesc));

val SOME(sock1, _) = Socket.acceptNB x;
val NONE = Socket.acceptNB x;
Socket.close sock1;
Socket.close x;
Socket.close y;
