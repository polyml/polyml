(* Tests for List.partition. *)

fun check name b = if b then () else raise Fail ("List.partition: " ^ name);

fun even x = x mod 2 = 0;

(* Correctness. *)
check "empty" (List.partition even ([]: int list) = ([], []));
check "singleton true" (List.partition even [2] = ([2], []));
check "singleton false" (List.partition even [1] = ([], [1]));
check "all true" (List.partition even [2, 4, 6] = ([2, 4, 6], []));
check "all false" (List.partition even [1, 3, 5] = ([], [1, 3, 5]));
check "alternating"
    (List.partition even [1, 2, 3, 4, 5, 6] = ([2, 4, 6], [1, 3, 5]));
check "order and duplicates are preserved"
    (List.partition (fn x => x < 3) [5, 1, 3, 1, 4, 2, 5] = ([1, 1, 2], [5, 3, 4, 5]));

(* The function is applied exactly once to each element, from left to right. *)
let
    val seen = ref []
    val _ = List.partition (fn x => (seen := x :: !seen; even x)) [1, 2, 3, 4, 5]
in
    check "evaluation order" (List.rev (!seen) = [1, 2, 3, 4, 5])
end;

(* An exception raised by the function is propagated. *)
check "exception"
    ((List.partition (fn x => x < 3 orelse raise Subscript) [1, 2, 3, 4]; false)
        handle Subscript => true);

(* A suffix of the argument whose elements all go to the same side is shared
   with the argument rather than copied. *)
let
    val xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    val suffix = List.drop (xs, 6)
    val () = check "drop shares" (PolyML.pointerEq (List.drop (xs, 6), suffix))

    val (left, right) = List.partition (fn _ => true) xs
    val () = check "share all true" (PolyML.pointerEq (left, xs) andalso null right)

    val (left, right) = List.partition (fn _ => false) xs
    val () = check "share all false" (PolyML.pointerEq (right, xs) andalso null left)

    val (left, right) = List.partition (fn x => x >= 6) xs
    val () = check "share true suffix"
        (PolyML.pointerEq (left, suffix) andalso right = [0, 1, 2, 3, 4, 5])

    val (left, right) = List.partition (fn x => x < 6) xs
    val () = check "share false suffix"
        (PolyML.pointerEq (right, suffix) andalso left = [0, 1, 2, 3, 4, 5])

    (* The shared suffix is preceded by copied elements. *)
    val (left, right) = List.partition (fn x => x < 2 orelse x >= 6) xs
    val () = check "share true suffix after prefix"
        (left = [0, 1, 6, 7, 8, 9] andalso right = [2, 3, 4, 5] andalso
         PolyML.pointerEq (List.drop (left, 2), suffix))

    val (left, right) = List.partition (fn x => x >= 2 andalso x < 6) xs
    val () = check "share false suffix after prefix"
        (left = [2, 3, 4, 5] andalso right = [0, 1, 6, 7, 8, 9] andalso
         PolyML.pointerEq (List.drop (right, 2), suffix))
in
    ()
end;
