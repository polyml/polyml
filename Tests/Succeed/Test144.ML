(* Check for bug with missing BeginLoop *)

let
    fun f n m = (n, m) handle Fail _ => f n m
    
    fun g x = #1 (f x x)

in
     g 3
end;
