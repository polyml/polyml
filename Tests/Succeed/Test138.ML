(* Bug related to detecting usage patterns for recursion. *)


fun foldr f n [] = n
|   foldr f n (hd :: tl) = f (hd, foldr f n tl);


fun g z =
let
    val (x, _) = foldr (fn (h, t) => (ref(#2 t); t)) (0, 0) z
in
    x
end;

g[1];
