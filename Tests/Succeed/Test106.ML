(* This produced an internal error exception because the equality attribute
   had been removed in the third pass. *)
abstype t = T of int
with
  fun eq a (b:t) = a = b
end;
