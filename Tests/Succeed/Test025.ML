(* Check that locks work correctly. *)

open Thread.Thread Thread.Mutex

val a = ref 0 and b = ref 0 and c = ref 0

val mut = mutex()

fun f s n () =
let
   val () = lock mut
   val aa = (a := !a+1; !a)
   and bb = (b := !b+1; !b)
   and cc = (c := !c+1; !c)
   val () = unlock mut
in
   if aa <> bb orelse aa <> cc orelse bb <> cc
   then print(concat[s, "-> a = ", Int.toString aa, " b  = ",
                     Int.toString bb, " c  = ", Int.toString cc, "\n"])
   else if aa < 10000
   then f s (n+1) ()
   else ()
end;

fork(f "A" 0, [InterruptState InterruptAsynch, EnableBroadcastInterrupt true]);
fork(f "B" 0, [InterruptState InterruptAsynch, EnableBroadcastInterrupt true]);
fork(f "C" 0, [InterruptState InterruptAsynch, EnableBroadcastInterrupt true]);
f "D" 0 ();
