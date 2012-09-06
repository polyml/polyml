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

(* A further example from Phil. *)
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
    type 'a t1 = unit
    type 'a t = 'a t1 * 'a t1   (* two levels of phantom types *)
    val x = A.mkT 2
  end;
  
(* A further check.  This has worked correctly for a long time because of the
   way equality is handled.  Include it here just in case it gets broken by
   a change in the future. *)

fun f (x: real t) y = x = y;

