(* This will fail in ML97 although it was valid in ML90.
   Sharing is not transitive. *)
functor F(
    structure A: sig type t val x: t end
    structure B: sig end
    structure C: sig type t end
    sharing A = B sharing B = C
) =
struct
    val q = A.x : C.t
end;
