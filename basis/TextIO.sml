(*
    Title:      Standard Basis Library: Text IO
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

(* G&R 2004 status: updated.  It's possible that there are subtleties that haven't been addressed. *)

signature TEXT_STREAM_IO =
sig
    include STREAM_IO
    where type vector = CharVector.vector
    where type elem = Char.char

    val inputLine : instream -> (string * instream) option
    val outputSubstr : outstream * Substring.substring -> unit
end;

signature TEXT_IO = sig
    (* include IMPERATIVE_IO *)
    structure StreamIO : TEXT_STREAM_IO
        where type reader = TextPrimIO.reader
        where type writer = TextPrimIO.writer
        where type pos = TextPrimIO.pos

    type vector = StreamIO.vector
    type elem = StreamIO.elem

    type instream
    type outstream

    val input : instream -> vector
    val input1 : instream -> elem option
    val inputN : instream * int -> vector
    val inputAll : instream -> vector
    val canInput : instream * int -> int option
    val lookahead : instream -> elem option
    val closeIn : instream -> unit
    val endOfStream : instream -> bool
    val output : outstream * vector -> unit
    val output1 : outstream * elem -> unit
    val flushOut : outstream -> unit
    val closeOut : outstream -> unit
    val mkInstream : StreamIO.instream -> instream
    val getInstream : instream -> StreamIO.instream
    val setInstream : instream * StreamIO.instream -> unit
    val mkOutstream : StreamIO.outstream -> outstream
    val getOutstream : outstream -> StreamIO.outstream
    val setOutstream : outstream * StreamIO.outstream -> unit
    val getPosOut : outstream -> StreamIO.out_pos
    val setPosOut : outstream * StreamIO.out_pos -> unit
    (* End of include IMPERATIVE_IO *)

    val inputLine : instream -> string option
    val outputSubstr : outstream * Substring.substring -> unit
    val openIn  : string -> instream
    val openOut : string -> outstream
    val openAppend : string -> outstream
    val openString : string -> instream

    val stdIn  : instream
    val stdOut : outstream
    val stdErr : outstream

    val print : string -> unit
    val scanStream : ((Char.char, StreamIO.instream) StringCvt.reader
                      -> ('a, StreamIO.instream) StringCvt.reader)
                      -> instream -> 'a option
end;

structure TextIO :> TEXT_IO = struct
    open IO

    type vector = String.string
    and  elem = Char.char

    exception Interrupt = RunCall.Interrupt

    (* Called after any exception in the lower level reader or
       writer to map any exception other than Io into Io. *)
    fun mapToIo (io as Io _, _, _) = io
      | mapToIo (Interrupt, _, _) = Interrupt
      | mapToIo (nonIo, name, caller) =
            Io { name = name, function = caller, cause = nonIo }

    (* Functional IO Layer. *)

    structure TextStreamIO =
    struct
        structure BasicTextStreamIO = BasicStreamIO(
                structure PrimIO = TextPrimIO
                structure Vector = CharVector
                structure Array = CharArray
                structure VectorSlice = CharVectorSlice
                structure ArraySlice = CharArraySlice
                val someElem : PrimIO.elem = #" "
            );
        
        open BasicTextStreamIO

        (* Input a line.  Adds a newline if the file ends without one. *)
        fun inputLine f =
        let
            (* Read a sequence of blocks until we get a newline or EOF. *)
            fun inputBlocks read f =
            let
                (* Read the next block and see how big it is. *)
                val (blk, f') = input f
                val length = String.size blk

                (* See if it contains a newline and if so where. *)
                fun newlinePos i =
                    if i = length then length+1
                    else if String.sub(blk, i) = #"\n"
                    then i+1 (* Return characters including newline. *)
                    else newlinePos (i+1)
                val nlPos = newlinePos 0
            in
                if length = 0 (* EOF *)
                then (
                    (* If we have not read anything at all we return NONE
                       otherwise return what we had with a newline added. *)
                    case read of
                        [] => NONE
                    |   _ => SOME(String.concat(List.rev("\n"::read)), f)
                    )
                else if nlPos > length
                then inputBlocks (blk::read) f' (* No newline - get another block.. *)
                else (* The string we read included a newline. *)
                    let
                        (* Reread all up to and including the newline
                           and return the stream which gives us the rest. *)
                        val (b, f') = inputN(f, nlPos)
                    in
                        SOME(String.concat(List.rev(b::read)), f')
                    end
            end
        in
            (* If we are at end-of-stream we return NONE.  Since this is a functional stream
               that means we will always return NONE for a given f (i.e. there's no 
               temporary end-of-stream to be cleared). *)
            inputBlocks [] f
        end
        
        (* StreamIO treats line buffering on output as block buffering
           since it has no concept of a line separator. *)
        fun output(f, v) =
            case getBufferMode f of
                LINE_BUF =>
                let
                    val vecLen = CharVector.length v
                    (* Find the last newline character in the string. *)
                    fun lastNewline 0 = 0
                    |   lastNewline i =
                            if CharVector.sub(v, i-1) = #"\n" then i
                            else lastNewline(i-1)
                    val newLinePos = lastNewline vecLen
                in
                    if newLinePos = 0
                    then (* No newlines in it. *)
                        BasicTextStreamIO.output(f, v)
                    else (* There's at least one newline. *)
                        (
                        outputVec(f, CharVectorSlice.slice(v, 0, SOME newLinePos));
                        flushOut f;
                        outputVec(f, CharVectorSlice.slice(v, newLinePos, NONE))
                        )
                end

            |   _ => BasicTextStreamIO.output(f, v) (* Not line buffering. *)

        (* This could be defined in terms of output but the underlying
           output1 function is likely to be more efficient. *)
        fun output1(f, c) =
            (
            BasicTextStreamIO.output1(f, c);
            if c = #"\n" andalso getBufferMode f = LINE_BUF
            then flushOut f else ()
            )
    end (* TextStreamIO. *)


    (* The imperative IO streams *)
    structure ImpIO = ImperativeIO(
        structure StreamIO = TextStreamIO
        structure Vector = CharVector
        structure Array = CharArray)

    (* open ImpIO *)

    (* Replace the StreamIO from ImpIO by our version. *)
    structure StreamIO =
    struct
        open TextStreamIO

        (* TODO: This is unnecessary.  CharVectorSlice.slice and Substring.substring are the same type. *)
        fun outputSubstr(f, s) =
        let
            val (v, i, l) = Substring.base s
        in
            outputVec(f, CharVectorSlice.slice(v, i, SOME l))
        end
    end;

    (* The simple, and original implementation was in terms of the
       ImperativeIO structure.  The big disadvantage of it is that
       in the common case when we simply open a stream on a file
       and read it entirely through the TextIO interface we have
       a lot of overhead.  I've changed it to use the underlying
       layers if required but otherwise to use the file descriptor
       directly.  This isn't such a problem with output so we use
       the lower layers directly.
       
       Stream IO has now been reimplemented to be much more
       efficient.  It seems there is still some speed advantage
       in using the low-level directly but there's much less
       difference than there used to be. *)

    (* The type of a stream without the layers in between. If we extract
       the lower level this gets replaced. *)
    type textInstream = {
        descr: OS.IO.iodesc,
        buffer: CharArray.array,
        bufp: int ref,
        buflimit: int ref,
        (* buflimit: size of useful data in the buffer.  Some values of
           this are special.
           If this is ~1 it means that the buffer does not contain valid
           data but we have not detected an end-of-file or if we have we
           have passed this back to the caller.
           If this is 0 it means that the last read returned zero (EOF)
           AND we have not yet returned this to the caller.  This happens
           if we're reading a large amount of data and we stop because we
           reach EOF.  We return as much as we can this time and the NEXT
           read returns (and clears) EOF. *)
        name: string
    }

    datatype baseInstream =
        Underlying of ImpIO.instream
      | Direct of textInstream
      
    open Thread.Thread
    open Thread.Mutex

    datatype instream = InStream of baseInstream ref * mutex

    type outstream = ImpIO.outstream
    val output = ImpIO.output
    val output1 = ImpIO.output1
    val flushOut = ImpIO.flushOut
    val closeOut = ImpIO.closeOut
    val mkOutstream = ImpIO.mkOutstream
    val getOutstream = ImpIO.getOutstream
    val setOutstream = ImpIO.setOutstream
    val getPosOut = ImpIO.getPosOut
    val setPosOut = ImpIO.setPosOut

    open RuntimeCalls LibrarySupport.CharArray
    type fileDescr = OS.IO.iodesc;
    type address = LibrarySupport.address
    (* We have to declare doIo separately depending on the
       types of the arguments. It's possible to get round this
       but that would result in an extra call to run_call3 for
       each io call. *)
    local
        val doIo: int*int*string -> fileDescr
             = RunCall.run_call3 POLY_SYS_io_dispatch
    in
        val stdInDesc: fileDescr = doIo(0, 0, "")

        fun sys_open_in_text name = doIo(3, 0, name)
        and sys_open_out_text name = doIo(5, 0, name)
        and sys_open_append_text name = doIo(13, 0, name)
    end

    local
        val doIo = RunCall.run_call3 POLY_SYS_io_dispatch
    in
        fun sys_get_buffsize (strm: fileDescr): int = doIo(15, strm, 0)
        and sys_can_input(strm: fileDescr): int = doIo(16, strm, 0)
        and sys_avail(strm: fileDescr): int = doIo(17, strm, 0)
    end

    local
        val doIo = RunCall.run_call3 POLY_SYS_io_dispatch
    in
        fun sys_close (strm: fileDescr): unit = doIo(7, strm, 0)
    end

    local
        val doIo = RunCall.run_call3 POLY_SYS_io_dispatch
    in
        fun sys_read_text (strm: fileDescr, vil: address*word*word): int =
            doIo(8, strm, vil)
    end

    local
        val doIo = RunCall.run_call3 POLY_SYS_io_dispatch
    in
        fun sys_read_string (strm: fileDescr, len: int): string =
            doIo(10, strm, len)
    end

    (* Create the primitive IO functions and add the higher layers. *)
    fun wrapInFileDescr(n, name, buffContents) =
    let
        val textPrimRd =
            LibraryIOSupport.wrapInFileDescr{fd=n,
                name=name, initBlkMode=true}
        val streamIo = StreamIO.mkInstream(textPrimRd, buffContents)
    in
        streamIo
    end

    fun wrapOutFileDescr(n, name, buffering, isAppend) =
    let
        val buffSize = sys_get_buffsize n
        val textPrimWr =
            LibraryIOSupport.wrapOutFileDescr{fd=n,
                name=name, appendMode=isAppend, initBlkMode=true, chunkSize=buffSize}
        (* Construct a stream. *)
        val streamIo = StreamIO.mkOutstream(textPrimWr, buffering)
    in
        mkOutstream streamIo
    end

    (* Open a file for output. *)
    fun openOut s =
    let
        val f = 
            sys_open_out_text s
                handle exn => raise mapToIo(exn, s, "TextIO.openOut")
        (* Look at the stream to see what kind of buffering to use. *)
        val k = OS.IO.kind f        
    in
        wrapOutFileDescr (f, s,
            if k = OS.IO.Kind.tty then IO.LINE_BUF else IO.BLOCK_BUF,
            false (* Not append *))
    end

    fun openAppend s =
    let
        val f = 
            sys_open_append_text s
                handle exn => raise mapToIo(exn, s, "TextIO.openAppend")
        val k = OS.IO.kind f        
    in
        wrapOutFileDescr (f, s,
            if k = OS.IO.Kind.tty then IO.LINE_BUF else IO.BLOCK_BUF,
            true (* setPos will not work. *))
    end

    (* Open a file for input.  We start off using the Direct interface. *)
    fun openIn s =
    let
        val f = 
            sys_open_in_text s
                handle exn => raise mapToIo(exn, s, "TextIO.openIn")
        val buffsize = sys_get_buffsize f
    in
        InStream(
            ref (Direct{descr=f, name=s, buffer=CharArray.array(buffsize, #" "),
                        bufp=ref 0, buflimit=ref ~1}),
            mutex())
    end

    (* Get the entries for standard input, standard output and standard error. *)
    val stdIn =
        let
            val buffsize = sys_get_buffsize stdInDesc
        in
            InStream(
                ref (Direct{descr=stdInDesc, name="stdIn",
                        buffer=CharArray.array(buffsize, #" "), bufp=ref 0,
                        buflimit=ref ~1}),
                mutex())
        end

    (* This is a bit of a mess.  When we load a saved state the references associated with stdIn
       will be overwritten.  That could actually happen with any input file but stdIn is the only
       one that definitely is "persistent".  We need to save the contents of the buffer across the
       load and update the buffer with the saved contents.  *)
    local
        fun onLoad doLoad =
            case stdIn of
                InStream(ref(Direct{buffer, bufp as ref savedBufp, buflimit as ref savedBufLimit, ...}), _) =>
                    let
                        (* Have to extract the contents and save it in a local variable. *)
                        val savedBuff =
                            if savedBufLimit < 0
                            then ""
                            else CharArraySlice.vector(
                                CharArraySlice.slice(buffer, savedBufp, SOME(savedBufLimit - savedBufp)));
                    in
                        doLoad();
                        CharArray.copyVec { src=savedBuff, dst=buffer, di=savedBufp };
                        bufp := savedBufp;
                        buflimit := savedBufLimit
                    end

            |   InStream(ir as ref(Underlying impStream), _) =>
                    let
                        open ImpIO
                        open StreamIO
                        val s = ImpIO.getInstream impStream
                        val (r, v) = getReader s
                    in
                        (* Because we may have this function installed more than once
                           and because getReader truncates the stream so that a second
                           call to getReader raises an exception we have to set
                           the stream back before as well as after the load. *)
                        ir := Underlying(ImpIO.mkInstream(mkInstream(r,v)));
                        doLoad();
                        ir := Underlying(ImpIO.mkInstream(mkInstream(r,v)))
                    end

        (* On startup truncate the buffer in case there was some pending input when
           we exported.  Also install the onLoad function. *)
        fun onStartUp () =
        (
           case stdIn of
               InStream(ref(Direct{bufp, buflimit, ...}),_) => (bufp := 0; buflimit := ~1)
           |  _ => ();
           PolyML.onLoad onLoad
        )
    in
        (* Set up an onEntry handler so that this is always installed. *)
        val () = PolyML.onEntry onStartUp;
        (* Install it now. *)
        val () = PolyML.onLoad onLoad
    end;

    (* We may want to consider unbuffered output or even linking stdOut with stdIn
       so that any unbuffered
       output is flushed before reading from stdIn. That's the way that stdio
       works to ensure that prompts are written out. *)
    (* PROBLEM: The following declaration is evaluated when this structure is
       created, not at the start of the session.  The buffering will be set
       permanently to the buffering in effect at that point.
       Two solutions are possible.  One is to define special versions of the
       "write" functions to examine the stream whenever they are called and
       decide whether to change the buffering.  Another is simply to set it
       to unbuffered.  That can be changed, though, which may not be
       satisfactory. *)
    (* I've changed this from NO_BUF to LINE_BUF which should improve
       the performance.  An alternative might be to set up an "OnEntry"
       call which would examine the stream and decide what kind of
       buffering to use.  DCJM 1/9/00. *)
    val stdOut =
    let
        val f = RunCall.run_call1 POLY_SYS_io_operation POLY_SYS_stdout
    in
        wrapOutFileDescr (f, "stdOut", IO.LINE_BUF
            (* if System_is_term f then IO.LINE_BUF else IO.BLOCK_BUF *),
            false)
    end

    val stdErr =
    let
        val f = RunCall.run_call1 POLY_SYS_io_operation POLY_SYS_stderr
    in
        wrapOutFileDescr (f, "stdErr",
            IO.NO_BUF (* Defined to be unbuffered. *),
            false)
    end

    (* Lock the mutex during any lookup or entry. *)
    fun protect f (InStream(r, m)) = LibraryIOSupport.protect m f r

    (* Read something into the buffer. *)
    fun fillBuffer({buffer=Array(length, addr), bufp, buflimit, descr, name, ...}: textInstream) : unit =
        (
        bufp := 0;
        buflimit := ~1; (* Set these first in case of an exception. *)
        (* Read the text and set the buffer limit.  If the result was
           zero we've reached end-of-stream. *)
        buflimit := sys_read_text(descr, (addr, 0w0, length))
            handle exn => raise mapToIo(exn, name, "TextIO.fillBuffer")
        )

    (* If we make a text stream from the lower level we always wrap it
       up.  It might be possible to get the underlying file descriptor. *)
    fun mkInstream (s : StreamIO.instream) : instream =
        InStream(ref(Underlying(ImpIO.mkInstream s)), mutex())

    local
        fun getInstream'(ref(Underlying strm)) = ImpIO.getInstream strm
        |   getInstream'(instr as ref(Direct{descr, buffer, bufp, buflimit, name})) =
            let
                (* We have to wrap the stream at this point and pass it the
                   remains of the buffer. *)
                val unprocessed =
                    if !buflimit < 0
                    then ""
                    else CharArraySlice.vector(CharArraySlice.slice(buffer, !bufp, SOME(!buflimit - !bufp)));
                val strm = wrapInFileDescr(descr, name, unprocessed)
            in
                instr := Underlying(ImpIO.mkInstream strm);
                strm
            end
    in
        val getInstream = protect getInstream'
    end

    local
        fun setInstream' s (ref(Underlying strm)) = ImpIO.setInstream(strm, s)
        |   setInstream' s (instr as ref(Direct _)) =
                (* Should we close the existing stream or just discard it?
                   We can't have previously called getInstream because that
                   would have turned the "Direct" into an "Underlying" so
                   there can't be any other reference to this stream.
                   Leave it for the moment. *)
                instr := Underlying(ImpIO.mkInstream s)
    in
        fun setInstream(r, s) = protect (setInstream' s) r
    end
    

    local
        (* Read the next natural unit of the stream. *)
        fun input'(ref(Underlying strm)) = ImpIO.input strm
        |   input'(ref(Direct(strm as {buffer, bufp, buflimit, ...}))) =
            if !buflimit = 0
            then (* Last read returned end-of-file.  Clear the EOF state once
                    we return this empty string. *)
               (buflimit := ~1; "")
            else 
                (
                (* If we have exhausted the buffer or never read before we
                   have to try reading now. *)
                if !bufp >= !buflimit
                then fillBuffer strm else ();
                if !buflimit = 0 then
                    (* Now reached eof. Since we're returning an empty string we
                       need to set buflimit to ~1 to indicate that we should try
                       reading again. *)
                    (buflimit := ~1; "")
                else
                let
                    (* Return the rest of the buffer. *)
                    val resString =
                        CharArraySlice.vector(CharArraySlice.slice(buffer, !bufp, SOME(!buflimit - !bufp)));
                in
                    bufp := !buflimit;
                    resString
                end
                )
    in
        val input = protect input'
    end

    local
        fun input1'(ref(Underlying strm)) = ImpIO.input1 strm
        |   input1'(ref(Direct(strm as {buffer, bufp, buflimit, ...}))) =
            if !buflimit = 0
            then (* Last read returned end-of-file.  Clear the EOF state once
                    we return NONE. *)
                (buflimit := ~1; NONE)
            else
                (
                (* If we have exhausted the buffer or never read before we
                   have to try reading now. *)
                if !bufp >= !buflimit
                then fillBuffer strm else ();
                if !buflimit = 0
                then (* We must only return a single end-of-file for this read.
                        We set the limit to ~1 so that we will read again. *)
                    (buflimit := ~1; NONE)
                else
                let
                    val resCh = CharArray.sub(buffer, !bufp)
                in
                    bufp := !bufp + 1;
                    SOME resCh
                end
                )
    in
        val input1 = protect input1'
    end

    local
        fun inputN' n (ref(Underlying strm)) = ImpIO.inputN(strm, n)
        |   inputN' n (ref(Direct(strm as {buffer, bufp, buflimit, ...}))) =
            if n < 0 orelse n > CharVector.maxLen
            then raise Size
            else if n = 0
            then "" (* Return the empty string without blocking *)
            else if !buflimit = 0
            then (* Last read returned end-of-file.  Clear the EOF state once
                    we return this empty string. *)
                (buflimit := ~1; "")
            else 
            let
                fun readN toRead =
                    if !bufp + toRead <= !buflimit
                    then (* Can satisfy the request from the buffer. *)
                    let
                        val resString =
                            CharArraySlice.vector(CharArraySlice.slice(buffer, !bufp, SOME toRead));
                    in
                        bufp := !bufp + toRead;
                        [resString]
                    end
                    else
                    let
                        val available =
                            if !buflimit < 0 then 0 else !buflimit - !bufp
                        val resString =
                            CharArraySlice.vector(CharArraySlice.slice(buffer, !bufp, SOME available))
                    in
                        fillBuffer strm;
                        if !buflimit = 0
                        then (* Reached eof - return what we have. *)
                            [resString]
                        else resString :: readN (toRead - available)
                    end
                 val result = concat(readN n)
            in
                (* If we reached EOF without reading anything we clear the EOF
                   indicator.  Otherwise we leave it.  That way we always return
                   a single null string for each eof. *)
                if n <> 0 andalso size result = 0
                then buflimit := ~1
                else ();
                result
            end
    in
        fun inputN(r, n) = protect (inputN' n) r
    end


    local
        fun inputAll'(ref(Underlying strm)) = ImpIO.inputAll strm
        |   inputAll'(ref(Direct({buffer, bufp, buflimit, descr, name, ...}))) =
            if !buflimit = 0
            (* Last read returned an empty buffer.  Clear the EOF state once
               we return this empty string. *)
            then (buflimit := ~1; "")
            else
            let
                val soFar =
                    if !buflimit < 0
                    then ""
                    else CharArraySlice.vector(CharArraySlice.slice(buffer, !bufp, SOME(!buflimit - !bufp)));
    
                (* Find out how much we have available and try reading
                   a vector of that size.  It may get less than the whole
                   file so we have to keep trying. *)
                fun readAll() =
                let
                    (* The call to sys_avail can raise an exception if the file is a
                       special device e.g. in the /proc filing system on Linux. *)
                    val charsAvailable = sys_avail descr handle OS.SysErr _ => 0
                    (* If it's less than the blocksize get a block.  This way we
                       always get a reasonable amount if "avail" is giving us a
                       small amount. *)
                    val toRead = Int.max(charsAvailable, CharArray.length buffer)
                    val readRest =
                        sys_read_string(descr, toRead)
                            handle exn => raise mapToIo(exn, name, "TextIO.inputAll")
                in
                    if readRest = ""
                    then [""] (* Reached eof. *)
                    else readRest :: readAll()
                end
                (* Put it all together. *)
                val result = concat(soFar :: readAll())
            in
                bufp := 0; (* The buffer is now empty. *)
                (* If we are returning a null string then we clear the eof
                   indicator. *)
                if size result = 0
                then buflimit := ~1
                else buflimit := 0; (* We're at eof. *)
                result
            end
    in
        val inputAll = protect inputAll'
    end

    local
    fun canInput' n (ref(Underlying strm)) = ImpIO.canInput(strm, n)
    |   canInput' n (ref(Direct{bufp, buflimit, descr, name, buffer, ...})) =
        if n < 0 orelse n > CharVector.maxLen
        then raise Size
        else
        let
            val available =
                if !buflimit < 0 then 0 else !buflimit - !bufp
        in
            if available >= n then SOME n (* Sufficient in the buffer. *)
            else if !buflimit = 0 then SOME 0 (* At EOF. *)
            else (* Try reading ahead. *)
                (
                (* Copy the unused data so it is at the start of the buffer. *)
                if !bufp = 0 orelse !buflimit < 0 then () (* Nothing in the buffer. *)
                else if !bufp = !buflimit (* Nothing left in the buffer. *)
                then buflimit := ~1
                else
                    (
                    CharArraySlice.copy{src = CharArraySlice.slice(buffer, !bufp, SOME(!buflimit - !bufp)),
                                    dst = buffer, di = 0};
                    buflimit := !buflimit - !bufp
                    );
                bufp := 0;
                (* Try reading ahead into the rest of the buffer. *)
                if (sys_can_input descr > 0)
                        handle exn => raise mapToIo(exn, name, "TextIO.canInput")
                then (* We can read ahead without blocking.
                        How should we implement this?  We are supposed to
                        try reading ahead to see whether we actually have
                        n bytes available.  What if n-available > length buffer?
                        The definition says that this should use readVecNB to
                        try to read the rest.  There's no guarantee that this
                        will return more than the blocksize anyway. *)
                let
                    val Array(length, addr) = buffer
                    val inBuffer = if !buflimit < 0 then 0 else !buflimit;
                    val inBuffW = Word.fromInt inBuffer
                    val haveRead =
                        sys_read_text(descr, (addr, inBuffW, length-inBuffW))
                            handle exn => raise mapToIo(exn, name, "TextIO.canInput")
                in
                    buflimit := inBuffer + haveRead;
                    SOME(Int.min(n, !buflimit))
                end
                else if available = 0
                then NONE (* Nothing in the buffer and can't read ahead. *)
                else SOME available (* Just what's in the buffer. *)
                )
        end
    in
        fun canInput(r, n) = protect (canInput' n) r
    end

    local
        fun closeIn'(ref(Underlying strm)) = ImpIO.closeIn strm
        |   closeIn'(ref(Direct{descr, ...})) =
            (
                (* Do we need to do something to get the right effect with
                   getInstream? *)
                sys_close(descr)
            )
    in
        val closeIn = protect closeIn'
    end

    local
        fun endOfStream'(ref(Underlying strm)) = ImpIO.endOfStream strm
        |   endOfStream'(ref(Direct(strm as {buflimit, bufp, ...}))) =
                (
                (* If we have never read before or we have exhausted the
                   input we have to read now. *)
                if !bufp >= !buflimit andalso !buflimit <> 0
                then fillBuffer strm else ();
                (* At eof if the buffer is now empty. *)
                !buflimit = 0
                )
    in
        val endOfStream = protect endOfStream'
    end

    local
    fun inputLine' (ref(Underlying strm)) =
        let
            val f = ImpIO.getInstream strm
        in
            case StreamIO.inputLine f of
                NONE =>
                    let
                        (* It's not clear what should happen here.  Assume that this clears any
                           temporary EOF. *)
                        val (_, f') = StreamIO.input f
                    in
                        ImpIO.setInstream(strm, f');
                        NONE
                    end
            |   SOME (s, f') => ( ImpIO.setInstream(strm, f'); SOME s )
        end
    |  inputLine' (ref(Direct(strm as {buflimit, buffer, bufp, ...}))) =
        if !buflimit = 0 then (buflimit := ~1; NONE) (* Already at EOF. *)
        else
        let
            fun newlinePos i =
                if i = !buflimit then !buflimit+1
                else if CharArray.sub(buffer, i) = #"\n"
                then i+1 (* Return characters including newline. *)
                else newlinePos (i+1)
            fun readToNL haveRead =
                if ! buflimit = 0
                then (* At EOF.  The definition says that we should add
                        a newline if the file doesn't end with one and
                        only return NONE if we were already at EOF. *)
                    if haveRead then ["\n"] else [""]
                else
                let
                    val nlPos = newlinePos (!bufp)
                in
                    if nlPos <= !buflimit
                    then (* Newline in the buffer - extract up to it.*)
                    let
                        val resString =
                            CharArraySlice.vector(CharArraySlice.slice(buffer, !bufp, SOME(nlPos - !bufp)))
                    in
                        bufp := nlPos;
                        [resString]
                    end
                    else (* No newline in the buffer. *)
                    let
                        val resString =
                            CharArraySlice.vector(CharArraySlice.slice(buffer, !bufp, SOME(!buflimit - !bufp)))
                    in
                        fillBuffer strm;
                        resString :: readToNL true
                    end
                end
            val _ = if !bufp >= !buflimit then fillBuffer strm else ();
            val result = concat(readToNL false)
        in
            if size result = 0 (* I think the effect of this is that we only clear a temporary EOF after we
                                  have returned NONE and not if we have returned a string with a newline appended. *)
            then ( buflimit := ~1; NONE )
            else SOME result
        end
    in
        val inputLine = protect inputLine'
    end

    local
        fun lookahead' (ref(Underlying strm)) = ImpIO.lookahead strm
        |   lookahead' (ref(Direct(strm as {buflimit, buffer, bufp, ...}))) =
            (
            if !bufp >= ! buflimit andalso !buflimit <> 0
            then fillBuffer strm else ();
            if !buflimit = 0 then NONE (* EOF *)
            else SOME(CharArray.sub(buffer, !bufp))
            )
    in
        val lookahead = protect lookahead'
    end


    fun outputSubstr(f, s) = StreamIO.outputSubstr(getOutstream f, s)

    fun print s = (output(stdOut, s); flushOut stdOut)

    (* Open a string as an input stream. It would be possible to define this using
       the string as the argument to mkInstream and a null reader. This way gives
       more flexibility since it allows for random access to the string. *)
    fun openString (s: string) : instream =
    let
        val stringLength = String.size s
        val posN = ref 0

        (* We can read from the string until it is exhausted. *)
        fun readVec (len: int): vector =
        let
            val l = Int.min(len, stringLength - !posN)
            val v = String.substring(s, !posN, l)
        in
            posN := !posN + l;
            v
        end

        (* Closing it simply exhausts the input. *)
        fun close () : unit = (posN := stringLength)
        and avail () : int option = SOME(stringLength - ! posN)
        and readVecNB l = SOME(readVec l)
        and block () = ()
        and canInput () = true
        and getPos () = !posN
        and setPos n = posN := n
        and endPos () = stringLength

        val textPrimRd =
            TextPrimIO.RD {
                name = "StringPrimIO",
                chunkSize = stringLength, (* Most efficient to read the whole string. *)
                readVec = SOME readVec,
                readArr = NONE, (* Can be synthesised. *)
                readVecNB = SOME readVecNB,
                readArrNB = NONE, (* Can be synthesised. *)
                block = SOME block,
                canInput = SOME canInput,
                avail = avail,
                getPos = SOME getPos,
                setPos = SOME setPos,
                endPos = SOME endPos,
                verifyPos = SOME getPos,
                close = close,
                ioDesc = NONE
            }
        val streamIo = StreamIO.mkInstream(textPrimRd, "")
    in
        InStream(ref(Underlying(ImpIO.mkInstream streamIo)), mutex())
    end

    fun scanStream scanFn strm =
    let
        val f = getInstream strm
    in
        case (scanFn StreamIO.input1 f) of
            NONE => NONE
        |   SOME(v, f') =>
            (
                setInstream(strm, f');
                SOME v
            )
                   
    end

    local
        open PolyML
        fun prettyIn _     _ (InStream(ref(Direct{ name, ... }), _)) =
                PolyML.PrettyString(String.concat["Instream-\"", String.toString name, "\""])
        |   prettyIn depth _ (InStream(ref(Underlying s), _)) =
                PolyML.prettyRepresentation(s, depth)
    in
        val () = addPrettyPrinter prettyIn
    end
end;

(* Available unqualified at top-level. *)
val print = TextIO.print;
