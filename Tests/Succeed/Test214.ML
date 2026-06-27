(* Test for scanning input.  Currently just used for Word.scan but could be used more widely. *)

(* Version of scanString that returns the value and the rest of the input string. *)
fun scanString cvt s =
    let
    val len = String.size s
    fun rdr i =
        if i = len then NONE
        else SOME(String.sub(s, i), i+1)
    in
    case cvt rdr 0 of
        NONE => NONE
    |   SOME(res, tl) => SOME (res, String.substring(s, tl, len-tl))
    end;

fun verifyScan scan input result = if scan input = result then () else raise Fail "wrong";

verifyScan (scanString (Word.scan StringCvt.HEX)) "123G" (SOME(0wx123, "G"));
verifyScan (scanString (Word.scan StringCvt.HEX)) "0wx123G" (SOME(0wx123, "G"));
verifyScan (scanString (Word.scan StringCvt.HEX)) "0wX123G" (SOME(0wx123, "G"));
verifyScan (scanString (Word.scan StringCvt.HEX)) "0x123G" (SOME(0wx123, "G"));
verifyScan (scanString (Word.scan StringCvt.HEX)) "0X123G" (SOME(0wx123, "G"));

verifyScan (scanString (Word.scan StringCvt.HEX)) "0w123G" (SOME(0w0, "w123G"));


verifyScan (scanString (Word.scan StringCvt.DEC)) "123AG" (SOME(0w123, "AG"));
verifyScan (scanString (Word.scan StringCvt.DEC)) "0w123AG" (SOME(0w123, "AG"));
verifyScan (scanString (Word.scan StringCvt.DEC)) "AG" NONE;
verifyScan (scanString (Word.scan StringCvt.DEC)) "0wx123G" (SOME(0w0, "wx123G"));


verifyScan (scanString (Word.scan StringCvt.OCT)) "123AG" (SOME(0wx53, "AG"));
verifyScan (scanString (Word.scan StringCvt.OCT)) "0w123AG" (SOME(0wx53, "AG"));
verifyScan (scanString (Word.scan StringCvt.OCT)) "1238AG" (SOME(0wx53, "8AG"));
verifyScan (scanString (Word.scan StringCvt.OCT)) "8AG" NONE;

