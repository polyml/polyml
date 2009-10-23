(* Check that line buffering and no buffering work correctly. *)

val temp = OS.FileSys.tmpName();
val (writer, _) = TextIO.StreamIO.getWriter(TextIO.getOutstream(TextIO.openOut temp));
val sLine = TextIO.StreamIO.mkOutstream(writer, IO.LINE_BUF);
val sNone = TextIO.StreamIO.mkOutstream(writer, IO.NO_BUF);
(TextIO.StreamIO.output(sLine, "First line \n line ");
 TextIO.StreamIO.output(sNone, "Second"));
TextIO.StreamIO.flushOut sLine;

let open TextPrimIO val WR { close, ... } = writer in close () end;
val sTxt = TextIO.openIn temp;
val s = TextIO.inputAll sTxt;
TextIO.closeIn sTxt;
if s = "First line \nSecond line " then () else raise Fail "failed";
OS.FileSys.remove temp;
