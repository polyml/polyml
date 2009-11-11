(* Test of polling and non-blocking sockets. *)
(* Updated to the new non-blocking calls. *)
fun verify true = ()
|   verify false = raise Fail "wrong";

val x = INetSock.TCP.socket(): Socket.passive INetSock.stream_sock
and y = INetSock.TCP.socket(): Socket.active INetSock.stream_sock;
val SOME me = NetHostDB.getByName "localhost";
val localhost = NetHostDB.addr me;
Socket.bind(x, INetSock.any 0);
Socket.listen(x, 5);
Socket.acceptNB x;
val xPoll = valOf(OS.IO.pollDesc(Socket.ioDesc x)) and yPoll = valOf(OS.IO.pollDesc(Socket.ioDesc y));
fun printPoll(p, l) =
let
    open OS.IO
in
    (if isIn p then ["In"] else []) @
    (if isOut p then ["Out"] else []) @
    (if isPri p then ["Pri"] else []) @ l
end;

verify(null(List.foldl printPoll [] (OS.IO.poll([OS.IO.pollIn xPoll, OS.IO.pollOut xPoll], SOME(Time.fromSeconds 1)))));
let
    val p1 = List.foldl printPoll [] (OS.IO.poll([OS.IO.pollIn yPoll, OS.IO.pollOut yPoll], SOME(Time.fromSeconds 1)))
in
    (* This gives [] in Windows but ["Out"] on Linux*)
    verify(p1 = [] orelse p1 = ["Out"])
end;

let
val (_, port) = INetSock.fromAddr(Socket.Ctl.getSockName x)
in
Socket.connectNB(y, INetSock.toAddr(localhost, port))
end;
verify(List.foldl printPoll [] (OS.IO.poll([OS.IO.pollIn xPoll, OS.IO.pollOut xPoll], SOME(Time.fromSeconds 1))) =
    ["In"]);
verify(List.foldl printPoll [] (OS.IO.poll([OS.IO.pollIn yPoll, OS.IO.pollOut yPoll], SOME(Time.fromSeconds 1))) =
    ["Out"]);
val SOME(sock1, _) = Socket.acceptNB x;
val NONE = Socket.acceptNB x;
Socket.close sock1;
Socket.close x;
Socket.close y;
