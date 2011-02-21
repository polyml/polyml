(* This produced a "jump too large" exception on X86. *)
(* The problem occurred because extending a pending branch requires five
   bytes for the 32-bit jump instruction and if the previous pending
   branches were less than five bytes apart extending the shortest branch
   might push the other branches outside the 8-bit range. *)

datatype t =
    A1 | B1 | C1 | D1 | E1 | F1 | G1 | H1 | I1 | J1 | K1 | L1 | M1 |
    A2 | B2 | C2 | D2 | E2 | F2 | G2 | H2 | I2 | J2 | K2 | L2 | M2 |
    A3 | B3 | C3 | D3;

fun f a b c =
let
    val x =
        case a of
            A1 => c
        |   B1 => b
        |   C1 => c
        |   D1 => b
        |   E1 => c
        |   F1 => b
        |   G1 => c
        |   H1 => b
        |   I1 => c
        |   J1 => b
        |   K1 => c
        |   L1 => b
        |   M1 => c
        |   A2 => c
        |   B2 => b
        |   C2 => c
        |   D2 => b
        |   E2 => c
        |   F2 => b
        |   G2 => c
        |   H2 => b
        |   I2 => (print "hello"; print "there"; c)
        |   J2 => b
        |   K2 => c
        |   L2 => b
        |   M2 => c
        |   A3 => c
        |   B3 => b
        |   C3 => c
        |   _ => c
in
    x+c+b
end;
