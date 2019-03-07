(* Allocating a large array whose size was one less than the size of a block crashed
   the minor GC in 32-in-64. *)
val t = Array.array(0x47fff, 0);
