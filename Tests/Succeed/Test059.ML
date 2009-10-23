(* This caused an exception in the code generator. *)
local
   val x = ref 0
in
   fun f i = (if not false then x:= i else(); "hello")
end;

val x = ref 0;

fun f i = (
if (if not false then (x:= i; !x = i) else true) then 1 else 0; "hello");

fun f i = (
if not false andalso (x:= i; !x = i) then 1 else 0; "hello");

