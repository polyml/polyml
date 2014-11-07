(* Bug in X86 code-generation.  Because the tag is subtracted AFTER the operation
   an operation that actually overflows may not be correctly detected. *)



fun t x = x + x * 1073741824;

(* Use ! and ref here to prevent the compiler interpreting it. *)
if ! (ref t) ~1 > 0 then raise Fail "wrong" else ();

fun s x = x + x * 4611686018427387904;

if ! (ref s) ~1 > 0 then raise Fail "wrong" else ();


