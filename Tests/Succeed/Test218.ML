(* The Arm64 callback entry boxed the argument address and then allocated the box
   for the result address without keeping the first box live.  A GC on that
   second allocation left the argument box dangling. *)

(* qsort is not exported by the executable on Windows. *)
case #lookupStruct (PolyML.globalNameSpace) "Windows" of
    SOME _ => raise NotApplicable
|   NONE => ();

local
    open Foreign
    val cmp = buildCallback2 ((cPointer, cPointer), cInt)
        (fn (a, b) => Word32.toInt (Memory.get32 (a, 0w0)) - Word32.toInt (Memory.get32 (b, 0w0)))
    val qsort: Memory.voidStar * int * int * (Memory.voidStar * Memory.voidStar -> int) closure -> unit =
        buildCall4 (getSymbol (loadExecutable ()) "qsort", (cPointer, cUlong, cUlong, cFunction), cVoid)
    val n = 2000
    val buf = Memory.malloc (Word.fromInt (4 * n))
    fun fill (i, seed) =
        if i = n then ()
        else (Memory.set32 (buf, Word.fromInt i, Word32.fromInt ((i * 7919 + seed) mod 100003)); fill (i + 1, seed))
    fun sorted i =
        i = n - 1 orelse
            (Memory.get32 (buf, Word.fromInt i) <= Memory.get32 (buf, Word.fromInt (i + 1)) andalso sorted (i + 1))
    fun loop 0 = ()
    |   loop k = (fill (0, k); qsort (buf, n, 4, cmp); if sorted 0 then loop (k - 1) else raise Fail "unsorted")
in
    val () = loop 400
    val () = Memory.free buf
end;
