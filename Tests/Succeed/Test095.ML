(* This caused a problem at one stage.  When constructing the optimised code
   for pattern matching we have to merge multiple occurrences of the same special
   constant and that requires the equality function to be evaluated at compile-time.
   This was picking up the wrong equality function. *)
structure S =
struct
    type addrs = word
    fun f (n: word, ic: addrs list) =
        case (n + hd ic) of
            0w1 => 1
        |   _ => 2
end;
