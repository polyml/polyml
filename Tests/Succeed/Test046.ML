(* This caused a Bind exception.  *)

signature HOLPP =
sig
type ppstream
datatype frag = QUOTE of string

val lineWidth      : int -> int
val pp_to_string   : int -> (int -> 'a -> unit) -> 'a -> string
end;


signature Portable =
sig
  include HOLPP
end;

structure Portable :> Portable =
struct
type ppstream = int
datatype frag = QUOTE of string
fun lineWidth _ = raise Fail "bad"
fun pp_to_string _ = raise Fail "bad"

fun pp_to_string linewidth ppfn ob = ""

end (* Portable *)
;
datatype frag = datatype Portable.frag;
val q2 = QUOTE "" : Portable.frag;
