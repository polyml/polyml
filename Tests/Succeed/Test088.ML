(* Some more cases of polymorphism conversion. *)

(* This changes the polymorphism at some level. *)
structure S: sig type 'a t val x: 'a t val f : 'a t -> int val g: int -> 'a t end =
struct
    type 'a t = int
    val x: 'a t = 1
    fun f (n: int): 'a t = n (* int -> 'a t*)
    fun g (x :'a t): int = x (* 'a t -> int *)
end;

if S.x = 1 then () else raise Fail "Error";

if S.f S.x = 1 then () else raise Fail "Error";


(* Slightly different version that completely hides the relationship with int. *)
structure S:> sig type 'a t val x: 'a t val f : 'a t -> int val g: int -> 'a t end =
struct
    type 'a t = int
    val x: 'a t = 1
    fun f (n: int): 'a t = n (* int -> 'a t*)
    fun g (x :'a t): int = x (* 'a t -> int *)
end;

val x = S.x;
if S.f x = 1 then () else raise Fail "Error";

if S.f S.x = 1 then () else raise Fail "Error";


(* The signature adds polymorphism to a monomorphic function. *)
structure Dispatch:
sig
    type 'a union
    val Char   : word   -> 'a union;
end  =
struct
   type 'a union = word
         
   fun Char   (x:word)   : 'a union = x;

end;
