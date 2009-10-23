(* Tests for well-formedness i.e. that a type with value constructors does not share with
   a type function. *)

signature S =
sig
datatype t = A
end where type t = int * int;

signature S =
sig
datatype t = A
type s
sharing type s = t
end where type s = int * int;


signature S =
sig
datatype t = A
type s = int * int
sharing type s = t
end;

signature S = sig datatype t = A end;
signature R = sig include S end where type t = int * int;

signature R = sig structure Q: S end where type Q.t = int * int;

signature S =
sig type t = int->int end where type t = int * int;

signature S =
sig eqtype t end where type t = int->int;
