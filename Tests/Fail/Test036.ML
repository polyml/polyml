
signature S =
sig
    type s = int
    datatype t = C
    sharing type s = t
end;
