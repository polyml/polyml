(*
    Title:      Standard Basis Library: Binary IO
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

(* G&R 2004 status: in progress. *)


signature BIN_IO =
sig
    include IMPERATIVE_IO
       where type StreamIO.vector = Word8Vector.vector
       where type StreamIO.elem = Word8.word
       where type StreamIO.reader = BinPrimIO.reader
       where type StreamIO.writer = BinPrimIO.writer
       where type StreamIO.pos = BinPrimIO.pos

    val openIn  : string -> instream
    val openOut : string -> outstream
    val openAppend : string -> outstream
end;


structure BinIO: BIN_IO =
struct
    open IO
    
    structure StreamIO =
    struct
        structure SIO = BasicStreamIO(
            structure PrimIO = BinPrimIO
            structure Vector = Word8Vector
            structure Array = Word8Array
            structure VectorSlice = Word8VectorSlice
            structure ArraySlice = Word8ArraySlice
            val someElem : PrimIO.elem = 0wx20
        );
        open SIO
        
        (* StreamIO treats line buffering on output as block buffering
           since it has no concept of a line separator. It's not clear whether
           line buffering makes sense for binary IO either but at least there
           is a value (0w12) which we can use. *)
        fun output(f, v) =
            case getBufferMode f of
                LINE_BUF =>
                let
                    val vecLen = Word8Vector.length v
                    (* Find the last newline character in the string. *)
                    fun lastNewline 0 = 0
                    |   lastNewline i =
                            if Word8Vector.sub(v, i-1) = 0w12 then i
                            else lastNewline(i-1)
                    val newLinePos = lastNewline vecLen
                in
                    if newLinePos = 0
                    then (* No newlines in it. *)
                        SIO.output(f, v)
                    else (* There's at least one newline. *)
                        (
                        SIO.outputVec(f, Word8VectorSlice.slice(v, 0, SOME(newLinePos-1)));
                        flushOut f;
                        SIO.outputVec(f, Word8VectorSlice.slice(v, newLinePos, NONE))
                        )
                end

            |   _ => SIO.output(f, v) (* Not line buffering. *)

        (* This could be defined in terms of output but the underlying
           output1 function is likely to be more efficient. *)
        fun output1(f, c) =
            (
            SIO.output1(f, c);
            if c = 0w12 andalso getBufferMode f = LINE_BUF
            then flushOut f else ()
            )
                
    end;
    structure ImpIO = ImperativeIO(
        structure StreamIO = StreamIO
        structure Vector = Word8Vector
        structure Array = Word8Array)
    open ImpIO

    open RuntimeCalls;

    exception Interrupt = RunCall.Interrupt

    (* Called after any exception in the lower level reader or
       writer to map any exception other than Io into Io. *)
    fun mapToIo (io as Io _, _, _) = io
      | mapToIo (Interrupt, _, _) = Interrupt
      | mapToIo (nonIo, name, caller) =
            Io { name = name, function = caller, cause = nonIo }

    type fileDescr = OS.IO.iodesc (* Actually abstract.  This isn't
                                     the file descriptor itself, rather
                                     a pointer into the io table. *)

    local
        local
            val doIo: int*int*string -> fileDescr
                 = RunCall.run_call3 POLY_SYS_io_dispatch
        in
            fun sys_open_in_bin name = doIo(4, 0, name)
            and sys_open_out_bin name = doIo(6, 0, name)
            and sys_open_append_bin name = doIo(14, 0, name)
        end

        local
            val doIo = RunCall.run_call3 POLY_SYS_io_dispatch
        in
            fun sys_get_buffsize (strm: fileDescr): int = doIo(15, strm, 0)
        end

        fun wrapInFileDescr(n, name) =
        let
            val binPrimRd =
                LibraryIOSupport.wrapBinInFileDescr {fd=n, name=name, initBlkMode=true}

            val streamIo =
                StreamIO.mkInstream(binPrimRd, Word8Vector.fromList [])
        in
            mkInstream streamIo
        end

        fun wrapOutFileDescr(n, name, buffering, isAppend) =
        let
            val buffSize = sys_get_buffsize n
            val binPrimWr =
                LibraryIOSupport.wrapBinOutFileDescr{fd=n,
                    name=name, appendMode=isAppend, chunkSize=buffSize, initBlkMode=true}
            (* Construct a stream. *)
            val streamIo = StreamIO.mkOutstream(binPrimWr, buffering)
        in
            mkOutstream streamIo
        end
    in
        (* Open a file for input. *)
        fun openIn s =
            wrapInFileDescr(
                sys_open_in_bin s
                    handle exn => raise mapToIo(exn, s, "BinIO.openIn"),
                s)

        (* Open a file for output. *)
        fun openOut s =
        let
            val f = 
                sys_open_out_bin s
                    handle exn => raise mapToIo(exn, s, "BinIO.openOut")
            (* Look at the stream to see what kind of buffering to use. *)
            val k = OS.IO.kind f        
        in
            wrapOutFileDescr (f, s,
                if k = OS.IO.Kind.tty orelse k = OS.IO.Kind.pipe orelse k = OS.IO.Kind.device
                then IO.LINE_BUF else IO.BLOCK_BUF,
                false (* Not append *))
        end

        fun openAppend s =
        let
            val f = 
                sys_open_append_bin s
                    handle exn => raise mapToIo(exn, s, "BinIO.openAppend")
            val k = OS.IO.kind f        
        in
            wrapOutFileDescr (f, s,
                if k = OS.IO.Kind.tty orelse k = OS.IO.Kind.pipe orelse k = OS.IO.Kind.device
                then IO.LINE_BUF else IO.BLOCK_BUF,
                true (* setPos will not work. *))
        end
    end
end;
