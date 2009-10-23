(* Bug in OS.Path.makeCanonical reported by Scott dot Owens at cl dot cam dot ac dot uk *)
if OS.Path.mkCanonical "/a/b/../../c" = OS.Path.mkCanonical "/c"
then () else raise Fail "wrong";
