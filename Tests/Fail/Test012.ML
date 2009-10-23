(* This example uses opaque matching to create types which are
   distinct from the base type (int) and from each application. *)
(* This particular example also caused a problem with the code-generator
   which had to be fixed. *)
structure A = struct type t = int val x = 1 fun succ y = y + 1 end
functor f() :> sig type t val x : t val succ: t-> t end = A
structure B = f() and C = f();

(* This should fail. *)
val it = B.succ C.x;
