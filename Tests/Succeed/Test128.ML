(* If type abbreviations contain type variables that are not present
   on the right-hand side we have to reduce the abbreviation before
   unification and applying the value restriction.
   The first part of the example was provided by Phil Clayton.  *)

signature A =
  sig
    type ('a, 'b) t
    val mkT  : int -> ('a, int) t
  end

structure A :> A =
  struct
    type ('a, 'b) t = 'b
    fun mkT n = n
  end


signature B =
  sig
    type 'a t
    val x : ('a t, int) A.t
  end

structure B :> B =
  struct
    type 'a t = unit
    val x = A.mkT 2
  end;


type 'a t = int;
val x: 'a t = 1 +1;
