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
    end (* StreamIO. *)


    (* The imperative IO streams *)
    structure ImpIO = BasicImperativeIO(
        structure StreamIO = TextStreamIO
        structure Vector = CharVector
        structure Array = CharArray)

    open ImpIO
    (* Now define StreamIO as our extended StreamIO *)

    (* Replace the StreamIO from ImpIO by our version. *)
    structure StreamIO =
    struct
        open TextStreamIO
        
        val outputSubstr = outputVec
    end

    open Thread.Thread
    open Thread.Mutex
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
    end

    (* Create the primitive IO functions and add the higher layers. *)
    fun wrapInFileDescr(n, name) =
    let
        val textPrimRd =
            LibraryIOSupport.wrapInFileDescr{fd=n,
                name=name, initBlkMode=true}
        val streamIo = StreamIO.mkInstream(textPrimRd, "")
    in
        ImpIO.mkInstream streamIo
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

    (* Open a file for input. *)
    fun openIn s =
    let
        val f = 
            sys_open_in_text s
                handle exn => raise mapToIo(exn, s, "TextIO.openIn")
    in
        wrapInFileDescr(f, s)
    end

    (* Get the entries for standard input, standard output and standard error. *)
    val stdIn = wrapInFileDescr(stdInDesc, "stdIn")

    local
        (* On startup reset stdIn to the original stream.  Among other things this clears
           any data that may have been in the buffer when we exported. *)
        fun onStartUp () =
        let
            val textPrimRd =
                LibraryIOSupport.wrapInFileDescr{fd=stdInDesc,
                    name="stdIn", initBlkMode=true}
            val streamIo = StreamIO.mkInstream(textPrimRd, "")
        in
            ImpIO.setInstream(stdIn, streamIo)
        end
    in
        (* Set up an onEntry handler so that this is always installed. *)
        val () = PolyML.onEntry onStartUp
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

    local
        (* This requires access to the underlying representation in order to be
           able to lock the stream while reading the line.  This ensures that
           if multiple threads are reading lines from a stream each thread
           will get a complete line. *)
        fun inputLine' fStream =
        let
            val f = ! fStream
        in
            case StreamIO.inputLine f of
                NONE =>
                    let
                        (* It's not clear what should happen here.  Assume that this clears any
                           temporary EOF. *)
                        val (_, f') = StreamIO.input f
                    in
                        fStream := f';
                        NONE
                    end
            |   SOME (s, f') => ( fStream := f'; SOME s )
        end
    in
        fun inputLine s = ImpIO.protect s inputLine'
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
        ImpIO.mkInstream streamIo
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
end;

(* Available unqualified at top-level. *)
val print = TextIO.print;
