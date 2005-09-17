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
		fStream: StreamIO.instream ref
		}
	and outstream = OutStream of {
		(* An imperative output stream is a reference to the underlying stream.
		   Unlike instream the underlying stream is also imperative but we need
		   a reference here to allow us to redirect. *)
		fStream: StreamIO.outstream ref
		}

	fun mkInstream (s : StreamIO.instream) : instream =
		InStream{fStream = ref s}

	fun getInstream(InStream{fStream = ref s}) = s

	fun setInstream(InStream{fStream}, s) = fStream := s

	fun mkOutstream (s : StreamIO.outstream) : outstream =
		OutStream{fStream = ref s}

	fun getOutstream(OutStream{fStream = ref s}) = s

	fun setOutstream(OutStream{fStream}, s) = fStream := s

	(* These are just wrappers for the underlying functional calls. *)
	fun input(InStream{fStream, ...}) =
	let
		val (v, f') = StreamIO.input(!fStream)
	in
		fStream := f';
		v
	end

	(* QUESTION: StreamIO.input1 cannot advance over a temporary
	   end-of-file because it does not return a new stream if it
	   returns NONE.  Should input1 here do the same?  Assume not
	   which is why we use StreamIO.inputN here.  For TextIO this
	   isn't too expensive because single character strings are
	   implemented as the character itself. *)
	fun input1(InStream{fStream, ...}) =
	let
		val (s, f') = StreamIO.inputN(!fStream, 1)
	in
		fStream := f';
		if Vector.length s = 0 then NONE else SOME(Vector.sub(s, 0))
	end

	fun inputN(InStream{fStream, ...}, n) =
	let
		val (v, f') = StreamIO.inputN(!fStream, n)
	in
		fStream := f';
		v
	end

	fun inputAll(InStream{fStream, ...}) =
	let
		val (v, f') = StreamIO.inputAll(!fStream)
	in
		fStream := f';
		v
	end

	fun canInput(InStream{fStream=ref f, ...}, n) = StreamIO.canInput(f, n)
	and closeIn(InStream{fStream=ref f, ...}) = StreamIO.closeIn f
	and endOfStream(InStream{fStream=ref f, ...}) = StreamIO.endOfStream f

	(* Lookahead looks at the next character in the stream without
	   removing it.  It can be implemented in terms of any of the
	   input/inputN/input1 functions.  The choice of function, though,
	   has an effect on the subsequent behaviour of the stream since
	   a call to "input" will reproduce the previous result. e.g. if
	   we used StreamIO.input1 here then lookahead f; input f will
	   return a single character.  We use "input" here so that a subsequent
	   call to "input" will return the same result. *)
	fun lookahead(InStream{fStream=ref f, ...}) =
	let
		val (s, _) = StreamIO.input f
	in
		if Vector.length s = 0 then NONE else SOME(Vector.sub(s, 0))
	end

	(* These are simply wrappers. *)
	fun output(OutStream{fStream=ref f, ...}, v) = StreamIO.output(f, v)
	and output1(OutStream{fStream=ref f, ...}, c) = StreamIO.output1(f, c)
	and flushOut(OutStream{fStream=ref f, ...}) = StreamIO.flushOut f
	and closeOut(OutStream{fStream=ref f, ...}) = StreamIO.closeOut f
	and getPosOut(OutStream{fStream=ref f, ...}) = StreamIO.getPosOut f

	fun setPosOut(OutStream{fStream, ...}, p) = fStream := StreamIO.setPosOut p
end;
