(* This was not detected as an error in 5.9.2 and earlier but there is actually a free type variable. *)

datatype 'a t1 = T1
type 'a t2 = int
type 'a zipper = 'a t1 * 'a t2
fun make_make () ()  : 'a zipper = (T1, 0)
val make: unit -> 'a zipper = make_make ()
;
