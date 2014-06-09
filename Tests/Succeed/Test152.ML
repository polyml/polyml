(* Creating a negative sized array should raise Size. *)

(let
   val v = Array.array(~12, 4)
in
    raise Fail "failed"
end) handle Size => ();

(let
   val v = Array.array(Array.maxLen+1, 4)
in
    raise Fail "failed"
end) handle Size => ();

(let
   val v = Array.tabulate(~12, fn i => i)
in
    raise Fail "failed"
end) handle Size => ();

(let
   val v = Array.tabulate(Array.maxLen+1, fn i => i)
in
    raise Fail "failed"
end) handle Size => ();

