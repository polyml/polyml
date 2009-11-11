
signature S =
sig
    type t
    type u
    structure S:
    sig
        type s
    end where type s = t->t
    sharing type S.s = u
end;
