(* Datatypes with a single constructor are normally represented as the value,
   i.e. the contructor is an identity function.  We optimise the case of
   constructors of tuples so that B values are triples containing the
   tag, a and b.  We MUSTN'T do that for ref. *)
datatype 'a t = T of 'a

type q = {a: string, b: string};

datatype s = A | B of q t | C of int;

let
    val x = B(T{a="O", b="K"})
    val B(T{a,b}) = x
in
    a^b
end;

datatype s = A | B of q ref | C of int;

let
    val x = B(ref{a="WRO", b="NG"})
    val B r = x
    val _ = r := {a="O", b="K"}
    val B(ref{a,b}) = x
in
    if a^b = "OK" then () else raise Fail "Wrong"
end;

