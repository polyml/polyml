(* Polymorphic declaration.  Ensure that Bind is raised. *)

(let
    val SOME (f: 'a -> 'a) = NONE
in
    f (f 1);
    raise Fail "No exception"
end) handle Bind => ()
;
