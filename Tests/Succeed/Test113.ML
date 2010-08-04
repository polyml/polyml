(* Bug in reported in SVN 1182 by Phil Clayton *)

datatype ''a t = A of ''a | B of int;
B 1;

(* Further check for nullary constructors. *)
datatype ''a s = C of ''a | D;
D;
