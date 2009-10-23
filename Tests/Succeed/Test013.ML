(* Bug found in the basis library. *)
CharVector.foldri(fn (_, a, b) => a :: b) [] "abc";
CharVector.foldri(fn (i, a, b) => (i, a) :: b) [] "abc";

val yy = IntVector.fromList [1, 2, 3];
IntVector.foldri(fn (i, a, b) => (i, a) :: b) [] yy;
