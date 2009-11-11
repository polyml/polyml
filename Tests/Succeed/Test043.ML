(* Bugs fixed in OS.Path.joinDirFile. *)
fun checkPathFns s =
let
    val result = OS.Path.joinDirFile(OS.Path.splitDirFile s)
in
    (* On Windows / is accepted as a path separator but replaced by \\. *)
    if result = s orelse result = String.map(fn #"/" => #"\\" | c => c) s then () else raise Fail "FAIL"
end;

checkPathFns "single";
checkPathFns "a/b";
checkPathFns "a\\b";
checkPathFns "\\ab";
checkPathFns "C:\\ab";
checkPathFns "/ab";
