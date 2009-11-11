(* include must copy values, exceptions and, with the latest changes, value constructors. *)
signature S1 =
    sig
    val x : int
    type t
    datatype s = A | B of t | C of int
    val y : int
    end;

signature S2 =
    sig
    val a: int
    include S1
    val b : int
    end;

functor F(Q: S2) =
struct
    fun f (Q.A) = Q.x
      | f (Q.B _) = Q.b
      | f (Q.C i) = i
    datatype s = datatype Q.s
end;

structure P = F(struct val x = 1; type t = bool datatype s = A | B of t | C of int;
                    val y = 3; val a = 4 val b = 5 end);
if P.f(P.A) = 1
then ()
else raise Fail "WRONG";

signature S2 =
    sig
    val a: int
    include
        sig
        val x : int
        type t
        datatype s = A | B of t | C of int
        val y : int
        end
    val b : int
    end;
    
functor F(Q: S2) =
struct
    fun f (Q.A) = Q.x
      | f (Q.B _) = Q.b
      | f (Q.C i) = i
    datatype s = datatype Q.s
end;

structure P = F(struct val x = 1; type t = bool datatype s = A | B of t | C of int;
                    val y = 3; val a = 4 val b = 5 end);
if P.f(P.A) = 1
then ()
else raise Fail "WRONG";
