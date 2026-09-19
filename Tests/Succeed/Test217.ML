(* This caused a blow-up in the match compiler.  #307 *)

fun f g ({...}: {}) = 0;
