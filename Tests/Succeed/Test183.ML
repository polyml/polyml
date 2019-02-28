(* Checks on the interaction between closing a stream and the underlying reader/writer. *)

val s1 = INetSock.TCP.socket();
fun closeIt () = Socket.close s1;

val primread = BinPrimIO.RD {
    avail = fn _ => NONE,
    block = SOME(fn _ => ()),
    canInput = SOME(fn _ => true),
    chunkSize = 10,
    close = closeIt,
    endPos = NONE,
    getPos = NONE,
    ioDesc = NONE,
    name = "instream",
    readArr = SOME (fn _ => 0),
    readArrNB = NONE,
    readVec = SOME(fn _ => Word8Vector.fromList []),
    readVecNB = NONE,
    setPos = NONE,
    verifyPos = NONE
};

val reader = BinIO.StreamIO.mkInstream(primread, Word8Vector.fromList []);

(* Multiple closes should work *)
BinIO.StreamIO.closeIn reader;
BinIO.StreamIO.closeIn reader;

(closeIt(); raise Fail "Should be closed") handle OS.SysErr _ => ();


(* If close fails it should raise Io. *)
val primread = BinPrimIO.RD {
    avail = fn _ => NONE,
    block = SOME(fn _ => ()),
    canInput = SOME(fn _ => true),
    chunkSize = 10,
    close = fn () => raise Fail "close failed",
    endPos = NONE,
    getPos = NONE,
    ioDesc = NONE,
    name = "instream",
    readArr = SOME (fn _ => 0),
    readArrNB = NONE,
    readVec = SOME(fn _ => Word8Vector.fromList []),
    readVecNB = NONE,
    setPos = NONE,
    verifyPos = NONE
};

val reader = BinIO.StreamIO.mkInstream(primread, Word8Vector.fromList []);
BinIO.StreamIO.closeIn reader handle IO.Io _ => ();


val s2 = INetSock.TCP.socket();
fun closeIt () = Socket.close s2;

val primwrite = BinPrimIO.WR {
    block = SOME(fn _ => ()),
    canOutput = SOME(fn _ => true),
    chunkSize = 10,
    close = closeIt,
    endPos = NONE,
    getPos = NONE,
    ioDesc = NONE,
    name = "outstream",
    setPos = NONE,
    verifyPos = NONE,
    writeArr = SOME (fn _ => 0),
    writeArrNB = NONE,
    writeVec = SOME (fn _ => 0),
    writeVecNB = NONE
};

val writer = BinIO.StreamIO.mkOutstream(primwrite, IO.NO_BUF);

BinIO.StreamIO.closeOut writer;
BinIO.StreamIO.closeOut writer;

(closeIt(); raise Fail "Should be closed") handle OS.SysErr _ => ();

