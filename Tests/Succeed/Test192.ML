(* Overflow at compile time in unreachable code. *)

Array.update (Array.fromList [], ~1, 0) handle Subscript => ();
let val array = Array2.array (1, 1, 0) in Array2.sub (array, ~1, 0) handle Subscript => 0 end;
fun f r = Vector.sub(r, ~1);
