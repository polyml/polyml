signature A = sig type t end;

signature S = sig
type a type b type c
include A where type t = int * c
sharing type a = b
end;

structure S: S =
struct
type a = int type b = int type c = int
type t = int * int
end;
