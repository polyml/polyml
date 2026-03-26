(* This is an error but wasn't being detected. *)
val a = ref NONE
val b: 'a option ref = a;
