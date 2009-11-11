(* Simon Finn pointed out that there might be a bug in this area and there was.
   We can't use the optimised representation for top-level datatypes because
   they may get incorporated into structures.  This didn't apply in ML90 because
   there wasn't any way to get a top-level datatype into a structure. *)

datatype t = X | Y of int * int;

functor F(S: sig type s datatype t = X | Y of s val def: s end) =
struct
open S
fun f (Y d) = d
|   f X = def
end;

structure S = F(struct type s = int * int datatype t = datatype t val def = (0,0) end);

if S.f(Y(1,1)) = (1,1)
then ()
else raise Fail "FAIL";

local
    datatype t = X | Y of int * int;
in
    structure S1 = F(struct type s = int * int datatype t = datatype t val def = (0,0) end);
    val isOk = 
        if S1.f(Y(1,1)) = (1,1) then "OK" else "FAIL";
end;

structure Q =
let
    datatype t = X | Y of int * int;
    structure S1 = F(struct type s = int * int datatype t = datatype t val def = (0,0) end);
in
    struct
        val isOk = S1.f(Y(1,1)) = (1,1);
    end
end;
if Q.isOk then () else raise Fail "wrong";

structure Q =
struct
    structure T =
    let
        datatype t = X | Y of int * int;
        structure S1 = F(struct type s = int * int datatype t = datatype t val def = (0,0) end);
    in
        struct
            val isOk = S1.f(Y(1,1)) = (1,1)
        end
    end
end;
if Q.T.isOk then () else raise Fail "wrong";
