(* Bugs in handling of single-character strings. *)
fun verify true = ()
|   verify false = raise Fail "error";


verify(String.isPrefix "a" "a");
verify(String.isPrefix "a" "abcd");
verify(String.isSuffix "a" "a");
verify(not(String.isSuffix "a" "abcd"));
verify(String.isSubstring "a" "a");
verify(String.isSubstring "a" "abcd");
