(* OS.Path.joinDirFile should raise InvalidArc if the file name does not correspond to an arc *)
(OS.Path.joinDirFile {dir="abc/def", file="ghi/jkl"}; raise Fail "incorrect") handle OS.Path.InvalidArc => ();
