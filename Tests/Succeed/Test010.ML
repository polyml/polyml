(* The code to prevent infinite loops when printing circular structures
   accidentally caught this case of a unary tuple. *)
datatype t = X of {a: int} | Y;
X{a=99};
