(* Test for writing, appending and reading. Appending was broken in Windows. *)

val tempFile = OS.FileSys.tmpName();
val strm = TextIO.openOut tempFile;
TextIO.output(strm, "First Line\n");
TextIO.output(strm, "Second Line\n");
TextIO.closeOut strm;
val strm = TextIO.openAppend tempFile;
TextIO.output(strm, "Third Line\n");
TextIO.closeOut strm;
val strm = TextIO.openIn tempFile;
val readInput = TextIO.inputAll strm;
TextIO.closeIn strm;
OS.FileSys.remove tempFile;
if readInput = "First Line\nSecond Line\nThird Line\n"
then ()
else raise Fail "Incorrect reading or writing";
