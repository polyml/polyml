(* Check exact zero allocation for buffered BinIO and TextIO output1/output
   and an allocation-free ThreadLib.protect callback over repeated calls.
   Prebuild inputs and create a fresh stream for each run outside profiling.
   Keep output buffered so construction and writer costs cannot hide protection
   callback closures or saved-state allocations. *)
local
    val writeCount = 1000
    val writeSize = 8
    (* Leave one spare byte so even the final write does not fill and flush the buffer. *)
    val bufferSize = writeCount * writeSize + 1

    fun makeWriter constructor name vectorLength arrayLength =
        constructor {
            name = name, chunkSize = bufferSize,
            writeVec = SOME vectorLength, writeArr = SOME arrayLength,
            writeVecNB = NONE, writeArrNB = NONE,
            block = SOME (fn () => ()), canOutput = SOME (fn () => true),
            getPos = NONE, setPos = NONE, endPos = NONE, verifyPos = NONE,
            close = fn () => (), ioDesc = NONE}

    fun makeBinStream mode () =
        let
            val writer = makeWriter BinPrimIO.WR "bin-allocation-test"
                Word8VectorSlice.length Word8ArraySlice.length
        in
            BinIO.mkOutstream (BinIO.StreamIO.mkOutstream (writer, mode))
        end

    fun makeTextStream mode () =
        let
            val writer = makeWriter TextPrimIO.WR "text-allocation-test"
                CharVectorSlice.length CharArraySlice.length
        in
            TextIO.mkOutstream (TextIO.StreamIO.mkOutstream (writer, mode))
        end

    fun binOutput1 stream 0 : unit = ()
      | binOutput1 stream n =
            (BinIO.output1 (stream, Word8.fromInt (n mod 256));
             binOutput1 stream (n - 1))

    val binOutput : BinIO.outstream -> int -> unit =
        let
            val vectors = Vector.tabulate (256, fn i =>
                Word8Vector.tabulate (writeSize, fn j => Word8.fromInt (i + j)))
            fun output stream 0 = ()
              | output stream n =
                    (BinIO.output (stream, Vector.sub (vectors, n mod 256));
                     output stream (n - 1))
        in
            output
        end

    fun textOutput1 stream 0 : unit = ()
      | textOutput1 stream n =
            (TextIO.output1 (stream, String.sub ("0123456789abcdef", n mod 16));
             textOutput1 stream (n - 1))

    val textOutput : TextIO.outstream -> int -> unit =
        let
            val strings = Vector.tabulate (256, fn i =>
                CharVector.tabulate (writeSize, fn j =>
                    String.sub ("0123456789abcdef", (i + j) mod 16)))
            fun output stream 0 = ()
              | output stream n =
                    (TextIO.output (stream, Vector.sub (strings, n mod 256));
                     output stream (n - 1))
        in
            output
        end

    val protected : int -> unit =
        let
            val mutex = Thread.Mutex.mutex ()
            (* The identity callback allocates nothing, so this loop measures protection overhead. *)
            fun protect 0 = ()
              | protect n = (ignore (ThreadLib.protect mutex (fn x => x) n); protect (n - 1))
        in
            protect
        end

    fun checkAllocation label operation n =
        let
            val allocated = ref 0
            val () = PolyML.fullGC ()
            val () = PolyML.Profiling.profileStream (fn entries =>
                allocated := List.foldl (fn ((words, _), sum) => words + sum) 0 entries)
                PolyML.Profiling.ProfileAllocations operation n
        in
            if !allocated = 0 then ()
            else raise Fail (label ^ ": allocated " ^ Int.toString (!allocated) ^ " words")
        end

    fun checkStream makeStream closeStream n label operation =
        let
            (* Start with a fresh buffer large enough for all writes without flushing. *)
            val stream = makeStream ()
            val () = checkAllocation label (operation stream) n
                handle exn => (closeStream stream; PolyML.Exception.reraise exn)
        in
            closeStream stream
        end

    fun modeName IO.BLOCK_BUF = "BLOCK_BUF"
      | modeName IO.LINE_BUF = "LINE_BUF"
      | modeName IO.NO_BUF = "NO_BUF"

    fun checkBin mode : unit =
        let val check = checkStream (makeBinStream mode) BinIO.closeOut writeCount in
            check ("BinIO.output1/" ^ modeName mode) binOutput1;
            check ("BinIO.output/" ^ modeName mode) binOutput
        end

    fun checkText mode : unit =
        let val check = checkStream (makeTextStream mode) TextIO.closeOut writeCount in
            check ("TextIO.output1/" ^ modeName mode) textOutput1;
            check ("TextIO.output/" ^ modeName mode) textOutput
        end
in
    (* Protecting an allocation-free callback adds no allocation overhead. *)
    val () = checkAllocation "ThreadLib.protect" protected writeCount

    (* Binary output1 and output allocate nothing in either buffering mode. *)
    val () = List.app checkBin [IO.BLOCK_BUF, IO.LINE_BUF]

    (* Text output1 and output allocate nothing in either buffering mode. *)
    val () = List.app checkText [IO.BLOCK_BUF, IO.LINE_BUF]
end;
