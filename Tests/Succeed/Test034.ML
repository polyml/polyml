(* Tests for Substring.isPrefix, Substring.isSuffix with single character strings.
   Reported by Richard Harrison rdharrison2 at hotmail dot co dot uk *)
fun verify true = ()
|   verify false = raise Fail "wrong";

verify(Substring.isPrefix "a" (Substring.full "abc"));
verify(Substring.isPrefix "a" (Substring.full "a"));
verify(not(Substring.isPrefix "a" (Substring.full "bc")));
verify(not(Substring.isPrefix "a" (Substring.full "b")));

verify(not(Substring.isSuffix "a" (Substring.full "abc")));
verify(Substring.isSuffix "a" (Substring.full "a"));
verify(Substring.isSuffix "a" (Substring.full "bca"));
verify(not(Substring.isSuffix "a" (Substring.full "b")));

verify(Substring.isSubstring "b" (Substring.full "abc"));
verify(Substring.isSubstring "a" (Substring.full "a"));
verify(not(Substring.isSubstring "a" (Substring.full "bc")));
verify(not(Substring.isSubstring "a" (Substring.full "b")));
verify(Substring.isSubstring "" (Substring.full "a"));
