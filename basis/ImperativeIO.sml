(*
    Title:      Standard Basis Library: ImperativeIO functor
    Copyright   David C.J. Matthews 2000

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

(* G&R 2004 status: Done.  Added where type constraints to result signature. *)
functor ImperativeIO (
    structure StreamIO : STREAM_IO
    structure Vector : MONO_VECTOR
    structure Array : MONO_ARRAY
    sharing type StreamIO.elem = Vector.elem = Array.elem
    sharing type StreamIO.vector = Vector.vector = Array.vector
) : IMPERATIVE_IO
        where type StreamIO.elem = StreamIO.elem
        where type StreamIO.vector = StreamIO.vector
        where type StreamIO.instream = StreamIO.instream
        where type StreamIO.outstream = StreamIO.outstream
        where type StreamIO.out_pos = StreamIO.out_pos
        where type StreamIO.reader = StreamIO.reader
        where type StreamIO.writer = StreamIO.writer
        where type StreamIO.pos = StreamIO.pos
        =
struct

    structure StreamIO = StreamIO
    type vector = Vector.vector
    and  elem = StreamIO.elem

    datatype instream = InStream of {
        (* An imperative input stream is a reference to a lazy functional stream. *)
        fStream: StreamIO.instream ref,
        lock: Thread.Mutex.mutex
        }
    and outstream = OutStream of {
        (* An imperative output stream is a reference to the underlying stream.
           Unlike instream the underlying stream is also imperative but we need
           a reference here to allow us to redirect. *)
        fStream: StreamIO.outstream ref
        }
    (* We don't need a mutex for outstream assuming := and ! are atomic
       i.e. '!' returns either the previous value or the current one and
       not some intermediate value. *)

    fun mkInstream (s : StreamIO.instream) : instream =
        InStream{fStream = ref s, lock = Thread.Mutex.mutex()}
        
    fun protect (InStream{fStream, lock}) f =
        LibraryIOSupport.protect lock f fStream

    (* Get and set the underlying stream.  We have to interlock
       setInstream at least. *)
    fun getInstream s = protect s (fn fStream => !fStream) 
    and setInstream(InStream{fStream, lock}, s) =
        LibraryIOSupport.protect lock (fn fStream => fStream := s) fStream

    (* These are just wrappers for the underlying functional calls. *)
    fun input s = protect s 
           (fn fStream =>
            let
                val (v, f') = StreamIO.input(!fStream)
            in
                fStream := f';
                v
            end)

    (* We don't use StreamIO.input1 here because that never advances over
       a temporary EOF. *)
    fun input1 s = protect s
           (fn fStream =>
            let
                val (s, f') = StreamIO.inputN(!fStream, 1)
            in
                fStream := f';
                if Vector.length s = 0 then NONE else SOME(Vector.sub(s, 0))
            end)

    fun inputN(InStream{fStream, lock}, n) =
        LibraryIOSupport.protect lock
           (fn fStream =>
            let
                val (v, f') = StreamIO.inputN(!fStream, n)
            in
                fStream := f';
                v
            end) fStream

    fun inputAll s = protect s
           (fn fStream =>
            let
                val (v, f') = StreamIO.inputAll(!fStream)
            in
                fStream := f';
                v
            end)

    (* These next functions only query the stream and don't affect the
       fStream ref so don't really need interlocking.  If two threads
       call these functions simultaneously the result is non-deterministic
       anyway. *)
    fun canInput(InStream{fStream, lock}, n) =
        LibraryIOSupport.protect lock
           (fn fStream => StreamIO.canInput(! fStream, n)) fStream

    and closeIn s = protect s (fn fStream => StreamIO.closeIn(! fStream))
    and endOfStream s = protect s (fn fStream => StreamIO.endOfStream(! fStream))

    fun lookahead s = protect s
        (fn fStream =>
            case StreamIO.input1 (! fStream) of
                NONE => NONE
            |   SOME(s, _) => SOME s
        )

    (* These are simply wrappers. *)

    fun mkOutstream (s : StreamIO.outstream) : outstream =
        OutStream{fStream = ref s}

    fun getOutstream(OutStream{fStream = ref s}) = s
    and setOutstream(OutStream{fStream}, s) = fStream := s

    fun output(OutStream{fStream=ref f, ...}, v) = StreamIO.output(f, v)
    and output1(OutStream{fStream=ref f, ...}, c) = StreamIO.output1(f, c)
    and flushOut(OutStream{fStream=ref f, ...}) = StreamIO.flushOut f
    and closeOut(OutStream{fStream=ref f, ...}) = StreamIO.closeOut f
    and getPosOut(OutStream{fStream=ref f, ...}) = StreamIO.getPosOut f

    fun setPosOut(OutStream{fStream, ...}, p) = fStream := StreamIO.setPosOut p
    
    (* Add pretty printers to hide the internals.  These just use the implementation streams. *)
    local
        open PolyML
        fun prettyIn depth _ (InStream{ fStream = ref s, ...}) =
            PolyML.prettyRepresentation(s, depth)
        fun prettyOut depth _ (OutStream { fStream = ref s, ...}) =
            PolyML.prettyRepresentation(s, depth)
    in
        val () = addPrettyPrinter prettyIn
        val () = addPrettyPrinter prettyOut
    end
end;
