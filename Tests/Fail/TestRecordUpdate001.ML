(* Record update must be enabled manually. *)

let
    val r = {a = 10, b = [20, 21, 22], c = "30"}
    val r' = {r with a = 11}
in
    #a r' = 11 andalso #b r' = #b r andalso #c r' = #c r
end;
