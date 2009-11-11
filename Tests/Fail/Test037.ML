
signature S =
sig
    type u
    type s = u * u
    datatype t = C;
    sharing type s = t
end;
