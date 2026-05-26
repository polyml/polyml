(* Unix socket test - Test078 converted to Unix socket *)
case #lookupStruct (PolyML.globalNameSpace) "UnixSock" of
    SOME _ => ()
|   NONE => raise NotApplicable;

val x = UnixSock.Strm.socket(): Socket.passive UnixSock.stream_sock
and y = UnixSock.Strm.socket(): Socket.active UnixSock.stream_sock;

val name = OS.FileSys.tmpName();
OS.FileSys.remove name handle OS.SysErr _ => ();

Socket.bind(x, UnixSock.toAddr name);
Socket.listen(x, 5);

(* Send the data. Use a separate thread so there's no possibility of blocking.
   The call to "connect" is also in here because otherwise there is a hang in Cygwin. *)
let
    fun sendData() =
        (Socket.connect(y, UnixSock.toAddr name); Socket.sendVec(y, Word8VectorSlice.full(Byte.stringToBytes "A test")); ())
in
    Thread.Thread.fork(sendData, [])
end;

val (cnct, _) = Socket.accept x;


if Byte.bytesToString(Socket.recvVec(cnct, 6)) <> "A test"
then raise Fail "failed"
else ();

Socket.close cnct;
Socket.close x;
Socket.close y;
OS.FileSys.remove name handle OS.SysErr _ => ();
