(* Word.div and Word.mod on X86 left the contents of the dividend register untagged
   resulting in a crash if a GC happened before the register was reloaded. *)

PolyML.Compiler.maxInlineSize := 1;
fun f (a: Word.word) b = b mod a;

(f 0w1 0w6; PolyML.fullGC());
