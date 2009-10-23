(* I've redone some of the string comparison routines so these are
   a few checks especially for the special cases of single-character strings. *)
fun check f = if f() then () else raise Fail "Test failed!!\n";

check (fn () => not ("abc" > "abc"));
check (fn () => "abd" > "abc");
check (fn () => "abc" > "");
check (fn () => "abcd" > "abc");
check (fn () => "a" > "");
check (fn () => "b" > "a");
check (fn () => not("a" > "a"));
check (fn () => not("a" > "b"));
check (fn () => not ("" > "a"));
check (fn () => "abc" > "a");
check (fn () => not ("a" > "abc"));

check (fn () => not ("abc" < "abc"));
check (fn () => not ("abd" < "abc"));
check (fn () => not ("abc" < ""));
check (fn () => not ("abcd" < "abc"));
check (fn () => not ("a" < ""));
check (fn () => not ("b" < "a"));
check (fn () => not("a" < "a"));
check (fn () => "a" < "b");
check (fn () => "" < "a");
check (fn () => not ("abc" < "a"));
check (fn () => "a" < "abc");

check (fn () => "abc" >= "abc");
check (fn () => "abd" >= "abc");
check (fn () => "abc" >= "");
check (fn () => "abcd" >= "abc");
check (fn () => "a" >= "");
check (fn () => "b" >= "a");
check (fn () => "a" >= "a");
check (fn () => not("a" >= "b"));
check (fn () => not ("" >= "a"));
check (fn () => "abc" >= "a");
check (fn () => not ("a" >= "abc"));

check (fn () => "abc" <= "abc");
check (fn () => not ("abd" <= "abc"));
check (fn () => not ("abc" <= ""));
check (fn () => not ("abcd" <= "abc"));
check (fn () => not ("a" <= ""));
check (fn () => not ("b" <= "a"));
check (fn () => "a" <= "a");
check (fn () => "a" <= "b");
check (fn () => "" <= "a");
check (fn () => not ("abc" <= "a"));
check (fn () => "a" <= "abc");

check (fn () => String.compare("abc", "abc") = EQUAL);
check (fn () => String.compare("abd", "abc") = GREATER);
check (fn () => String.compare("abc", "") = GREATER);
check (fn () => String.compare("abcd", "abc") = GREATER);
check (fn () => String.compare("a", "") = GREATER);
check (fn () => String.compare("b", "a") = GREATER);
check (fn () => String.compare("a", "a") = EQUAL);
check (fn () => String.compare("a", "b") = LESS);
check (fn () => String.compare("", "a") = LESS);
check (fn () => String.compare("abc", "a") = GREATER);
check (fn () => String.compare("a", "abc") = LESS);
