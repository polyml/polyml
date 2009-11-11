(*
    Title:      Standard Basis Library: PrimIO functor
    Copyright   David C.J. Matthews 2000, 2005

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

(* G&R 2004 status: Done *)

functor PrimIO (
    structure Vector : MONO_VECTOR
    structure VectorSlice: MONO_VECTOR_SLICE
    structure Array : MONO_ARRAY
    structure ArraySlice : MONO_ARRAY_SLICE
    sharing type Array.elem = Vector.elem = VectorSlice.elem = ArraySlice.elem
    sharing type Array.vector = Vector.vector = VectorSlice.vector = ArraySlice.vector
    sharing type VectorSlice.slice = ArraySlice.vector_slice
    sharing type Array.array = ArraySlice.array
    val someElem : Array.elem
    eqtype pos (* Note: this was shown as just a type in the functor arg. *)
    val compare : pos * pos -> order
    ): PRIM_IO =
struct
    type array = Array.array 
    and  vector = Vector.vector
    and  elem = Array.elem
    and  vector_slice = VectorSlice.slice
    and  array_slice = ArraySlice.slice

    type pos = pos

    val compare = compare

    datatype reader
    = RD of {
        name : string,
        chunkSize : int,
        readVec : (int -> vector) Option.option, (* Use Option once to ensure dependency. *)
        readArr : (array_slice -> int) Option.option,
        readVecNB : (int -> vector Option.option) Option.option,
        readArrNB : (array_slice -> int Option.option) Option.option,
        block : (unit -> unit) option,
        canInput : (unit -> bool) option,
        avail : unit -> int option,
        getPos : (unit -> pos) option,
        setPos : (pos -> unit) option,
        endPos : (unit -> pos) option,
        verifyPos : (unit -> pos) option,
        close : unit -> unit,
        ioDesc : OS.IO.iodesc option
    }

    datatype writer = WR of {
        name : string,
        chunkSize : int,
        writeVec : (vector_slice -> int) Option.option,
        writeArr : (array_slice -> int) Option.option,
        writeVecNB : (vector_slice -> int Option.option) Option.option,
        writeArrNB : (array_slice -> int Option.option) Option.option,
        block : (unit -> unit) option,
        canOutput : (unit -> bool) option,
        getPos : (unit -> pos) option,
        setPos : (pos -> unit) option,
        endPos : (unit -> pos) option,
        verifyPos : (unit -> pos) option,
        close : unit -> unit,
        ioDesc : OS.IO.iodesc option
    }

    (* Define readVec, readArr, readVecNB, readArrNB if they are not
       provided using the functions which are. *)
    fun augmentReader ( 
        RD {
            name : string,
            chunkSize : int,
            readVec : (int -> vector) Option.option,
            readArr : (array_slice -> int) Option.option,
            readVecNB : (int -> vector Option.option) Option.option,
            readArrNB : (array_slice -> int Option.option) Option.option,
            block : (unit -> unit) option,
            canInput : (unit -> bool) option,
            avail : unit -> int option,
            getPos : (unit -> pos) option,
            setPos : (pos -> unit) option,
            endPos : (unit -> pos) option,
            verifyPos : (unit -> pos) option,
            close : unit -> unit,
            ioDesc : OS.IO.iodesc option
    }) : reader =
    let
        (* First try defining readVec in terms of readArr. *)
        val readVec' =
            case (readVec, readArr) of
                (s as SOME _, _) => s (* If readVec exists use it. *)
            |   (NONE, SOME ra) =>
                (* if readVec doesn't exists but readArr does *)
                    SOME(
                    fn (l: int) =>
                        let
                            (* Create an array initialised to zeros.
                               We have to be careful here.  Suppose
                               the caller was using "readVec maxInt" to mean
                               "give me the rest of the file" knowing that it
                               was only small. To avoid problems we
                               only read the smaller of the size requested or
                               the block size. *)
                            val v = Array.array(Int.min(l, chunkSize), someElem)
                            (* Read as much as we can. *)
                            val n = ra(ArraySlice.full v)
                        in
                            (* Return the section read. *)
                            ArraySlice.vector(ArraySlice.slice(v, 0, SOME n))
                        end
                    )
            |   (NONE, NONE) => NONE

        (* And vice-versa *)
        val readArr' =
            case (readArr, readVec) of
                (s as SOME _, _) => s(* If readArr exists use it. *)
            |   (NONE, SOME rv) =>
                (* if readArr doesn't exists but readVec does *)
                    SOME(
                    fn slice =>
                        let
                            val (buff, i, len) = ArraySlice.base slice
                            (* Read a vector and try and put that into
                               the array. *)
                            val r = rv len
                        in
                            (* Copy the vector into the array. *)
                            Array.copyVec{src=r, dst = buff, di=i};
                            (* Return the number of characters read. *)
                            Vector.length r
                        end
                    )
            |   (NONE, NONE) => NONE

        (* We now have the blocking versions of readVec and readArr if either
           of them existed. Now defining the non-blocking versions using these
           blocking versions if we have to. *)

        val readVecNB' =
            case (readVecNB, readArrNB) of
                (s as SOME _, _) => s (* If readVecNB exists use it. *)
            |   (NONE, SOME ra) => 
                (* If readVecNB does not exist but readArrNB does*)
                    SOME(
                    fn (l: int) =>
                        let
                            val v = Array.array(Int.min(l, chunkSize), someElem)
                        in
                            case ra(ArraySlice.full v) of
                                NONE => NONE
                            |   SOME n => (* Return the section read. *)
                                    SOME (ArraySlice.vector(ArraySlice.slice(v, 0, SOME n)))
                        end
                    )
            |   (NONE, NONE) => 
                (* Try using the blocking readVec' with canInput.
                   We use the readVec' we defined above so that this will
                   also try using readArr. *)
                case (canInput, readVec') of
                    (SOME canIn, SOME rv) =>
                        SOME(
                        fn (l: int) =>
                            if canIn()
                            then SOME(rv l)
                            else NONE
                        )
                |   _ => NONE (* Can't do it. *)

        val readArrNB' =
            case (readArrNB, readVecNB) of
                (s as SOME _, _) => s(* If readArrNB exists use it. *)
            |   (NONE, SOME rv) =>
                (* if readArrNB doesn't exists but readVecNB does *)
                    SOME(
                    fn slice =>
                        let
                            val (buff, i, len) = ArraySlice.base slice
                        in
                            (* Try reading a vector of this size. *)
                            case rv len of
                                NONE => NONE
                            |   SOME r =>
                                (
                                (* Copy the vector into the array. *)
                                Array.copyVec{src=r, dst = buff, di=i};
                                (* Return the number of characters read. *)
                                SOME(Vector.length r)
                            )
                        end
                    )
            |   (NONE, NONE) =>
                (* Try using the blocking readArr' with canInput.
                   We use the readArr' we defined above so that this will
                   also try using readVec. *)
                case (canInput, readArr') of
                    (SOME canIn, SOME ra) =>
                        SOME(
                        fn slice =>
                            if canIn()
                            then SOME(ra slice)
                            else NONE
                        )
                |   _ => NONE (* Can't do it. *)

        (* Finally define the blocking functions in terms of the non-blocking
           if we have to. *)
        val readVec'' =
            case readVec' of
                (* If readVec' exists use it. i.e. if readVec or readArr were
                   defined. *)
                (s as SOME _) => s
            |   NONE =>
                (* No blocking version exists - try using block and the
                   synthesised non-blocking version. *)
                case (block, readVecNB') of
                    (SOME blk, SOME rv) =>
                        SOME(
                        fn (l: int) =>
                            (
                            blk();
                            case rv l of
                                NONE => (* Should not occur. *)
                                    raise IO.Io{
                                        name=name,
                                        function="readVec",
                                        cause = IO.BlockingNotSupported }
                            |   SOME v => v
                            )
                        )
                |   _ => NONE (* Can't do it. *)

        val readArr'' =
            case readArr' of
                (* If readArr' exists use it. *)
                (s as SOME _) => s
            |   NONE =>
                (* Try using block and the synthesised readArrNB'. *)
                case (block, readArrNB') of
                    (SOME blk, SOME ra) =>
                        SOME(
                            fn slice =>
                                (
                                blk();
                                case ra slice of
                                    NONE => 
                                        raise IO.Io{
                                            name=name,
                                            function="readArr",
                                            cause = IO.BlockingNotSupported }
                                |   SOME l => l
                                )
                        )
                    |   _ => NONE (* Can't do it. *)
    in
        RD {
            name = name,
            chunkSize = chunkSize,
            readVec = readVec'',
            readArr = readArr'',
            readVecNB = readVecNB',
            readArrNB = readArrNB',
            block = block,
            canInput = canInput,
            avail = avail,
            getPos = getPos,
            setPos = setPos,
            endPos = endPos,
            verifyPos = verifyPos,
            close = close,
            ioDesc = ioDesc
        }
    end

    fun augmentWriter (WR {
        name : string,
        chunkSize : int,
        writeVec : (vector_slice -> int) Option.option,
        writeArr : (array_slice -> int) Option.option,
        writeVecNB : (vector_slice -> int Option.option) Option.option,
        writeArrNB : (array_slice -> int Option.option) Option.option,
        block : (unit -> unit) option,
        canOutput : (unit -> bool) option,
        getPos : (unit -> pos) option,
        setPos : (pos -> unit) option,
        endPos : (unit -> pos) option,
        verifyPos : (unit -> pos) option,
        close : unit -> unit,
        ioDesc : OS.IO.iodesc option
    }) : writer =
    let
        (* First try defining writeVec in terms of writeArr. *)
        val writeVec' =
            case (writeVec, writeArr) of
                (s as SOME _, _) => s (* If writeVec exists use it. *)
            |   (NONE, SOME ra) =>
                (* if writeVec doesn't exists but writeArr does *)
                    SOME(
                    fn slice =>
                        let
                            (* Create an array to hold this slice. *)
                            val a = Array.array(VectorSlice.length slice, someElem)
                        in
                            (* Copy in the vector. *)
                            ArraySlice.copyVec{src=slice, dst=a, di=0};
                            (* write as much as we can. *)
                            ra(ArraySlice.full a)
                        end
                    )
            |   (NONE, NONE) => NONE

        (* And vice-versa *)
        val writeArr' =
            case (writeArr, writeVec) of
                (s as SOME _, _) => s (* If writeArr exists use it. *)
            |   (NONE, SOME wv) =>
                (* if writeArr doesn't exists but writeVec does *)
                    SOME(
                    fn slice =>
                        let
                            (* Construct a vector from this slice. *)
                            val v = ArraySlice.vector slice;
                        in
                            (* Try writing this vector. *)
                            wv(VectorSlice.full v)
                        end
                    )
            |   (NONE, NONE) => NONE

        (* We now have the blocking versions of writeVec and writeArr if either
           of them existed. Now defining the non-blocking versions using these
           blocking versions if we have to. *)

        val writeVecNB' =
            case (writeVecNB, writeArrNB) of
                (s as SOME _, _) => s (* If writeVecNB exists use it. *)
            |   (NONE, SOME wa) => 
                (* If writeVecNB does not exist but writeArrNB does*)
                    SOME(
                    fn slice =>
                        let
                            val len = VectorSlice.length slice
                            (* Create an array to hold this slice. *)
                            val a = Array.array(len, someElem)
                        in
                            (* Copy in the vector. *)
                            ArraySlice.copyVec{src=slice, dst=a, di=0};
                            (* Try writing it and see what happened. *)
                            wa(ArraySlice.full a)
                        end
                    )
            |   (NONE, NONE) => 
                (* Try using the blocking writeVec' with canOutput.
                   We use the writeVec' we defined above so that this will
                   also try using writeArr. *)
                case (canOutput, writeVec') of
                    (SOME canOut, SOME wv) =>
                        SOME(
                        fn slice =>
                            if canOut() then SOME(wv slice) else NONE
                        )
                |   _ => NONE (* Can't do it. *)

        val writeArrNB' =
            case (writeArrNB, writeVecNB) of
                (s as SOME _, _) => s (* If writeArrNB exists use it. *)
            |   (NONE, SOME wv) =>
                (* if writeArrNB doesn't exists but writeVecNB does *)
                    SOME(
                    fn slice =>
                        let
                            (* Construct a vector from this slice. *)
                            val v = ArraySlice.vector slice;
                        in
                            (* Try writing the vector. *)
                            wv(VectorSlice.full v)
                        end
                    )
            |   (NONE, NONE) =>
                (* Try using the blocking writeArr' with canOutput.
                   We use the writeArr' we defined above so that this will
                   also try using writeVec. *)
                case (canOutput, writeArr') of
                    (SOME canOut, SOME wa) =>
                        SOME(
                        fn slice =>
                            if canOut() then SOME(wa slice) else NONE
                        )
                |   _ => NONE (* Can't do it. *)

        (* Finally define the blocking functions in terms of the non-blocking
           if we have to. *)
        val writeVec'' =
            case writeVec' of
                (* If writeVec' exists use it. i.e. if writeVec or writeArr were
                   defined. *)
                (s as SOME _) => s
            |   NONE =>
                (* No blocking version exists - try using block and the
                   synthesised non-blocking version. *)
                case (block, writeVecNB') of
                    (SOME blk, SOME wv) =>
                        SOME(
                        fn slice =>
                            (
                                blk();
                                case wv slice of
                                    NONE => (* Should not occur. *)
                                        raise IO.Io{
                                            name=name,
                                            function="writeVec",
                                            cause = IO.BlockingNotSupported }
                                |   SOME l => l
                            )
                        )
                |   _ => NONE (* Can't do it. *)

        val writeArr'' =
            case writeArr' of
                (* If writeArr' exists use it. *)
                (s as SOME _) => s
            |   NONE =>
                (* Try using block and the synthesised writeArrNB'. *)
                case (block, writeArrNB') of
                    (SOME blk, SOME wa) =>
                        SOME(
                            fn slice =>
                            (
                                blk();
                                case wa slice of
                                    NONE => 
                                        raise IO.Io{
                                            name=name,
                                            function="writeArr",
                                            cause = IO.BlockingNotSupported }
                                |   SOME l => l
                            )
                        )
                    |   _ => NONE (* Can't do it. *)
    in
        WR {
        name = name,
        chunkSize = chunkSize,
        writeVec = writeVec'',
        writeArr = writeArr'',
        writeVecNB = writeVecNB',
        writeArrNB = writeArrNB',
        block = block,
        canOutput = canOutput,
        getPos = getPos,
        setPos = setPos,
        endPos = endPos,
        verifyPos = verifyPos,
        close = close,
        ioDesc = ioDesc
        }
    end
    
    (* Null reader - always returns end-of-file except when closed when it raises IO.ClosedStream. *)
    fun nullRd () =
    let
        val isOpen = ref true
    in
        RD {
        name = "nullRd",
        chunkSize = 1,
        readVec = SOME(fn n => if n < 0 then raise Size else if !isOpen then Vector.fromList [] else raise IO.ClosedStream),
        readArr = SOME(fn _ => if !isOpen then 0 else raise IO.ClosedStream),
        readVecNB = SOME(fn n => if n < 0 then raise Size else if !isOpen then SOME(Vector.fromList []) else raise IO.ClosedStream),
        readArrNB = SOME(fn _ => if !isOpen then SOME 0 else raise IO.ClosedStream),
        block = SOME(fn () => if !isOpen then () else raise IO.ClosedStream),
        canInput = SOME(fn () => if !isOpen then true else raise IO.ClosedStream),
        avail = fn () => if !isOpen then NONE else raise IO.ClosedStream,
        getPos = NONE,
        setPos = NONE,
        endPos = NONE,
        verifyPos = NONE,
        close = fn () => isOpen := false,
        ioDesc = NONE
        }
    end

    (* Null writer - always swallows input except when closed when it raises IO.ClosedStream. *)
    fun nullWr () =
    let
        val isOpen = ref true
    in
        WR {
        name = "nullWr",
        chunkSize = 1,
        writeVec = SOME(fn slice => if !isOpen then VectorSlice.length slice else raise IO.ClosedStream),
        writeArr = SOME(fn slice => if !isOpen then ArraySlice.length slice else raise IO.ClosedStream),
        writeVecNB = SOME(fn slice => if !isOpen then SOME(VectorSlice.length slice) else raise IO.ClosedStream),
        writeArrNB = SOME(fn slice => if !isOpen then SOME(ArraySlice.length slice) else raise IO.ClosedStream),
        block = SOME(fn () => if !isOpen then () else raise IO.ClosedStream),
        canOutput = SOME(fn () => if !isOpen then true else raise IO.ClosedStream),
        getPos = NONE,
        setPos = NONE,
        endPos = NONE,
        verifyPos = NONE,
        close = fn () => isOpen := false,
        ioDesc = NONE
        }
    end
    
    fun openVector v =
    let
        val isOpen = ref true
        val len = Vector.length v
        val p = ref 0 (* Pointer to current element. *)

        (* Return a slice of the vector from the current position for either the rest of
           the vector or the size requested if that's smaller. *)
        fun getSlice n =
            if not (! isOpen) then raise IO.ClosedStream
            else
            let
                val toRead = Int.min(n, len - !p) (* Return the smaller of the size requested or the size left. *)
                val resSlice = VectorSlice.slice(v, !p, SOME toRead)
            in
                p := !p + toRead;
                resSlice
            end

        (* Return a slice of the vector. *)
        fun readVec n = if n < 0 then raise Size else VectorSlice.vector(getSlice n)

        (* Copy a portion of the vector into the array.  We can probably copy directly
           from the original vector whereas if we synthesised this function it would
           require an extra copy. *)
        fun readArr slice = 
            let
                val (base, di, len) = ArraySlice.base slice
                val resSlice = getSlice len
            in
                ArraySlice.copyVec{src=resSlice, dst=base, di=di};
                VectorSlice.length resSlice
            end
    in
        RD {
        name = "openVector",
        chunkSize = 1, (* Or the size of the vector? *)
        readVec = SOME readVec,
        readArr = SOME readArr,
        readVecNB = NONE,
        readArrNB = NONE,
        block = SOME(fn () => if !isOpen then () else raise IO.ClosedStream),
        canInput = SOME(fn () => if !isOpen then true else raise IO.ClosedStream),
        (* avail returns the number of bytes and since we don't know the size of "elem" we return NONE here *)
        avail = fn () => if !isOpen then NONE else raise IO.ClosedStream,
        getPos = NONE,
        setPos = NONE,
        endPos = NONE,
        verifyPos = NONE,
        close = fn () => isOpen := false,
        ioDesc = NONE
        }
    end

end;
