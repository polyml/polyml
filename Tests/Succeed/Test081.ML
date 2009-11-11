(* Reading very long string.  This didn't fail but just took a long time since the
   process was quadratic in the length of the string rather than linear. *)
(* This code writes a very long string to a temporary file and then compiles it. *)
let
    val fname = OS.FileSys.tmpName();
    val f = TextIO.openOut fname;
    val _ = TextIO.output(f, "val _ = \"\\\n");
    fun outlines n =
        if n >= 1000 then ()
        else
          (TextIO.output(f, "\\A long line-A long line-A long line-A long line-A long line-A long line-A long line-\\n\\");
           outlines(n+1));
in
    outlines 0;
    TextIO.output(f, "\\\";");
    TextIO.closeOut f;
    use fname;
    OS.FileSys.remove fname
end;
