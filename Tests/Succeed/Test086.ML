(* Tests on coercions that apply between structures and signatures. *)

functor F():
    sig
        val a: exn and b: string->exn
        val checkA: (unit -> unit) -> unit
        val checkB: (unit -> unit) -> string
        type t
        val c: t and d: int->t
        val checkC: t -> unit and checkD: t -> unit
    end
=
    struct
        exception a and b of string
        datatype t = c | d of int
        
        fun checkA f = f() handle a => ()
        fun checkB f = (f(); "bad") handle b s => s
        fun checkC c = () | checkC _ = raise Fail "bad"
        fun checkD (d 1) = () | checkD _  = raise Fail "bad"
        
    end;

structure S = F();

S.checkA(fn () => raise S.a);
if S.checkB(fn () => raise S.b "test") = "test" then () else raise Fail "failed";
S.checkC S.c;
S.checkD (S.d 1);
