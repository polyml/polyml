(* This failed because the "level" was not being correctly calculated
   when using an overloaded function as a value rather than calling it. *)
local
    datatype a = A | B
    fun s (l:(a*a) list) = List.map (op =) l
in
    val x = s [(A, A), (B,B)]
end;
if x = [true, true] then () else raise Fail "wrong";
