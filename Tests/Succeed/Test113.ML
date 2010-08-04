(* Bug in reported in SVN 1182 by Phil Clayton *)

datatype ''a t = A of ''a | B of int;
B 1;
