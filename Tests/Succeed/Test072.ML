(* Newly created files should not have execute permission. 
   Note: this only applies to Unix.
   The definition of OS.FileSys.access says that it should accept
   but ignore A_EXEC if it doesn't make sense.
*)
if RunCall.run_call2 RuntimeCalls.POLY_SYS_os_specific (0, 0) <> 0
then () (* Windows *)
else
let
val fileName = OS.FileSys.tmpName();
open TextIO OS.FileSys
val f = openOut fileName
in
 output(f, "hello\n");
 closeOut f;
let
   val isExecute = access(fileName, [A_EXEC])
   in
   remove fileName;
   if isExecute then raise Fail "FAIL" else ()
   end
end;
