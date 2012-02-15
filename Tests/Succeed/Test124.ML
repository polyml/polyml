(* This builds a large list but with the tail in the
   first word of the pair.  That causes a segfault in older
   versions of the garbage collector because it overflows
   the stack during the mark phase. *)

datatype 'a liszt = Nill | Conns of 'a liszt * 'a;

fun mk 0 = Nill | mk n = Conns(mk (n-1), (n,n));

val a = mk 100000;
PolyML.fullGC();
