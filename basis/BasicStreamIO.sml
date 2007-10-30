(*
    Title:      Standard Basis Library: StreamIO functor
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

functor BasicStreamIO(
	structure PrimIO : PRIM_IO
    structure Vector : MONO_VECTOR
    structure Array : MONO_ARRAY
	structure VectorSlice: MONO_VECTOR_SLICE
	structure ArraySlice: MONO_ARRAY_SLICE
    sharing type PrimIO.elem = Vector.elem = Array.elem = VectorSlice.elem = ArraySlice.elem
    sharing type PrimIO.vector = Vector.vector = Array.vector = VectorSlice.vector = ArraySlice.vector
    sharing type PrimIO.array = Array.array = ArraySlice.array
	sharing type PrimIO.vector_slice = VectorSlice.slice = ArraySlice.vector_slice
	sharing type PrimIO.array_slice = ArraySlice.slice
    val someElem : PrimIO.elem
	):
	sig
	include STREAM_IO
	(* Note: This is non-standard but enables us to define
	   the derived BinIO and TextIO structures more efficiently. *)
	val outputVec: outstream * PrimIO.vector_slice -> unit
	end =
struct
	open IO
	type vector = Vector.vector
	type elem = PrimIO.elem
	datatype reader = datatype PrimIO.reader
	datatype writer = datatype PrimIO.writer
	type array = Array.array
	type pos = PrimIO.pos

    local
		structure Interrupt =
			RunCall.Run_exception0( val ex_iden  = RuntimeCalls.EXC_interrupt )
	in
		exception Interrupt = Interrupt.ex
	end

	(* Called after any exception in the lower level reader or
	   writer to map any exception other than Io into Io. *)
	fun mapToIo (io as Io _, _, _) = io
	  | mapToIo (Interrupt, _, _) = Interrupt
	  | mapToIo (nonIo, name, caller) =
	  		Io { name = name, function = caller, cause = nonIo }

	val emptyVec = Vector.fromList [] (* Represents end-of-stream. *)

	datatype instream =
		InStream of { state: streamState ref,
					  buffer: buffering,
					  rdr: reader,
					  locker: Thread.Mutex.mutex }

	and streamState =
		Truncated (* The stream has been closed or truncated. *)
	|	HaveRead of
			(* This input together with the rest.  Profiling suggests that
			   we can generate a lot of garbage in inputN if we actually
			   generate real substrings, so we keep the start and length
			   information here to allow for sharing of Vectors. *)
			{vec: vector, start: int, length: int, rest: streamState ref }
	|	ToRead (* We have not yet closed or truncated the stream. *)

	and buffering =
		Unbuffered
	|	Buffered of {
			buf: array, (* The buffer itself. *)
			bufp: int ref, (* Position of next character to extract. *)
			buflimit: int ref (* Count of characters in the buffer *)
			}

	and outstream =
		OutStream of {
			wrtr: writer,
			buffType : IO.buffer_mode ref,
			buf: array,
			bufp: int ref,
			isTerm: bool ref,
			locker: Thread.Mutex.mutex
			}

	datatype out_pos = OutPos of outstream * pos

	(* Slice functions. *)
	fun arrayExtract(a: Array.array, s: int, l: int): Array.vector =
		ArraySlice.vector(ArraySlice.slice(a, s, SOME l))
	and vectorExtract(v: Vector.vector, s: int, l: int): Vector.vector =
		VectorSlice.vector(VectorSlice.slice(v, s, SOME l))
	and arrayCopyVec{src: Vector.vector, si: int, len: int, dst: Array.array, di: int} =
		ArraySlice.copyVec{src=VectorSlice.slice(src, si, SOME len), dst=dst, di=di};

	(* Lock the mutex during any lookup or entry. This code should be in a library. *)
  	fun protect m f a =
	let
	    open Thread.Thread Thread.Mutex
	    (* Set this to handle interrupts synchronously except if we are blocking
		   them.  We don't want to get an asynchronous interrupt while we are
		   actually locking or unlocking the mutex but if we have to block to do
		   IO then we should allow an interrupt at that point. *)
	    val oldAttrs = getAttributes()
		val () =
		   case List.find (fn InterruptState _ => true | _ => false) oldAttrs of
		       SOME(InterruptState InterruptDefer) => ()
			 | _ => setAttributes[InterruptState InterruptSynch];
		val () = setAttributes[InterruptState InterruptSynch]
  		val () = lock m
		val result = f a
			handle exn => (unlock m; setAttributes oldAttrs; raise exn)
	in
		unlock m;
		setAttributes oldAttrs;
		result
	end

    fun protectIn f (ins as InStream{locker, ...}) = protect locker f ins
	
	fun protectOut f (outs as OutStream{locker, ...}) = protect locker f outs

	(* Extend a stream by adding a node to it showing that we
	   have read a vector. *)
	fun extendStream(InStream { state, buffer, rdr, locker }, v: vector, start, length) =
	let
		val newState = ref ToRead
		val newStream = 
			InStream{state=newState, buffer=buffer, rdr=rdr, locker=locker}
	in
		state := HaveRead{vec=v, start=start, length=length, rest=newState};
		newStream
	end

	(* Create a new stream from the vector and the augmented reader.
	   Perhaps we should remember the original reader as well?? *)
	fun mkInstream (r as RD {chunkSize, ...}, v: vector): instream =
	let
		val buffering =
			if chunkSize = 1
			then Unbuffered
			else Buffered { buf=Array.array(chunkSize, someElem),
				   			bufp=ref 0, buflimit=ref 0}
		val veclen = Vector.length v
	in
		(* QUESTION: What exactly is the status of this vector?  It is
		   supposed to provide the initial contents of the "buffer" but
		   how does the caller know what is a reasonable buffer size?
		   I am treating a non-zero vector as the initial input in the
		   stream . *)
		InStream{state = 
					if veclen = 0
					then ref ToRead
					else ref(HaveRead{vec=v, rest=ref ToRead, start=0, length=veclen}),
				 rdr = PrimIO.augmentReader r,
				 buffer = buffering,
				 locker = Thread.Mutex.mutex() }
	end

    local
    	(* QUESTION: What is the result of applying getReader to a stream
    	   which has already been partly read?  Assume that it returns
    	   the reader and the unconsumed part of all the related streams.
    	   i.e. val (v, f') = input f; getReader f = getReader f' is true.
    	   From reading some of the other notes that looks to be the case.
    	   They call getReader on the original stream and that terminates
    	   all the "derived" streams. *)
    	fun getReader' (InStream {state,
    							 rdr=rdr as RD{name, ...},
    							 buffer, ...}) =
    	let
    		fun truncateStream(ref Truncated) =
    				raise Io { name = name, 
    						   function = "getReader",
    						   cause = ClosedStream }
    		|	truncateStream(ref (HaveRead{rest, ...})) =
    				truncateStream rest
    		|	truncateStream(r as ref ToRead) =
    			let
    				(* Get the rest of the buffer contents. *)
    				val bufferContents =
    					case buffer of
    						Unbuffered => emptyVec
    					|	Buffered{buf, bufp=ref bufp, buflimit=ref bufl} =>
    						   arrayExtract(buf, bufp, bufl-bufp)
    			in
    				r := Truncated;
    				(rdr, bufferContents)
    			end
    	in
    		truncateStream state
    	end
    
    	(* The behaviour of closeIn is more closely defined.  *)
    	fun closeIn' (InStream {state, rdr=rdr as RD{close, ...}, buffer, ...}) =
    	let
    		(* Truncate the stream and close the underlying reader. *)
    		fun truncateStream(ref Truncated) = ()
    		|	truncateStream(ref (HaveRead{rest, ...})) =
    				truncateStream rest
    		|	truncateStream(r as ref ToRead) =
    				r := Truncated
    	in
    		truncateStream state;
    		close()
    	end
    
    	(* Get the file position of the next input. *)
    	(* QUESTION: I am assuming that it returns the position of the underlying
    	   reader and not the position at which a previously read character was
    	   read. *)
    	fun filePosIn' (InStream {state = ref s, rdr = rdr as RD{name, ...}, ...}) =
    	let
    		fun checkTruncation Truncated =
    				raise Io { name = name,
    						   function = "filePosIn",
    						   cause = ClosedStream }
    		|   checkTruncation (HaveRead{rest=ref s, ...}) =
    				checkTruncation s
    		|	checkTruncation ToRead = () (* Ok. *)
    	in
    		checkTruncation s;
    		case rdr of
    			RD{getPos=SOME get, setPos=SOME _, ...} =>
    				(get() handle exn => raise mapToIo(exn, name, "filePosIn"))
    		|	_ => (* We don't have both getPos and setPos. *)
    				raise Io { name = name,
    						   function = "filePosIn",
    						   cause = RandomAccessNotSupported }
    	end
	in
		val filePosIn = protectIn filePosIn'
		val getReader = protectIn getReader'
		val closeIn = protectIn closeIn'
	end
	
	local
     
    	(* Internal function. Repeatedly call readVec until either we
    	   have enough or we reach the end-of-stream. This is required
    	   because readVec blocks only once for each read. *)
    	fun readVecForced (rdr as RD{readVec=SOME rv, name, ...}, request, caller) =
    	let
    		fun doRead needed =
    		let
    			val v = rv needed handle exn => raise mapToIo(exn, name, caller)
    			val len = Vector.length v
    		in
    			if len = 0
    			then (* Reached eof *) []
    			else if len >= needed
    			then [v] (* We've got enough. *)
    			else v :: doRead (needed-len)
    		end
    	in
    		doRead request
    	end
    	| readVecForced(RD{name, ...}, request, caller) =
    		raise Io { name = name, function = caller, cause = BlockingNotSupported }
    
    	(* Similar function using an array to fill the buffer. Ensures
    	   that there is at least "request" bytes in the buffer or else
    	   we are at eof. Assumes that needed <= length buf. *)
    	fun fillBuffer (rdr as RD{readArr=SOME ra, name, ...}, buf: array, needed, caller) =
    	let
    		fun doRead bp =
    		let
    			(* Attempt to fill the buffer completely. *)
    			val haveRead = ra (ArraySlice.slice(buf, bp, NONE))
    			 	handle exn => raise mapToIo(exn, name, caller)
    		in
    			(* If we have reached EOF or if we now have enough to satisfy the
    			   request we can return. *)
    			if haveRead = 0 orelse haveRead+bp >= needed
    			then (* Return as much as we've got. *) haveRead+bp
    			else doRead (haveRead+bp)
    		end
    	in
    		doRead 0
    	end
    	| fillBuffer(RD{name, ...}, _, _, caller) =
    		raise Io { name = name, function = caller, cause = BlockingNotSupported }
    
    	(* Try to read a vector in non-blocking mode.  We assume that if we ask
    	   for a vector of a given size and that much data is available then
    	   readVecNB from the reader will gives us it. *)
    	fun readVecNB (rdr as RD{readVecNB=SOME rv, name, ...}, request, caller) =
    		(
    			rv request handle exn => raise mapToIo(exn, name, caller)
    		)
    	| readVecNB(RD{name, ...}, request, caller) =
    		raise Io { name = name, function = "inputN",
    				   cause = NonblockingNotSupported }
    
    	(* Call readArrNB to fill the buffer if possible. *)
    	fun fillBufferNB (rdr as RD{readArrNB=SOME ra, name, ...},
    					  buf: array, needed, caller) =
    		(
    		ra(ArraySlice.full buf)
    			handle exn => raise mapToIo(exn, name, caller)
    		)
    	| fillBufferNB(RD{name, ...}, _, _, caller) =
    		raise Io { name = name, function = "canInput",
    				   cause = NonblockingNotSupported }
    
    	(* Internal function. Read from the stream in either in blocking
    	   or non-blocking mode a certain number of characters.
    	   Returns the vector, the new stream and a flag indicating whether
    	   we reached the end-of-file.*)
    	fun readFromReader(f as InStream {rdr, buffer, ...}, isBlocking, request, caller) =
    		let
    			(* If we call readVecNB and get back SOME 0 we are supposed
    			   to treat this as EOF.  The result we return from
    			   canInput may be SOME k with (k > 0) if we still have
    			   something in the buffer.  Nevertheless we MUST record
    			   in the stream the EOF as well as the k-vector. *)
    			val (resVec: vector, isEOF) =
    			case buffer of
    				Unbuffered =>
    				if isBlocking
    				then
    				let
    					val v = Vector.concat(readVecForced(rdr, request, caller))
    				in
    					(v, Vector.length v <> request)
    				end
    				else
    					(
    					case readVecNB(rdr, request, caller) of
    						NONE => (emptyVec, false)
    					|	SOME v => (* If size is zero we are at eof. *)
    							(v, Vector.length v = 0)
    					)
    			|	Buffered{buf, bufp, buflimit} =>
    				if !buflimit - !bufp >= request
    				then (* We have enough in the buffer to satisfy the request. *)
    				let (* Extract it and return it *)
    					val v = arrayExtract(buf, !bufp, request)
    				in
    					bufp := !bufp + request;
    					(v, false)
    				end
    
    				else (* Not enough in the buffer, or it's empty. *)
    					if request-(!buflimit - !bufp) <= Array.length buf
    				then (* The amount we are short is "small". *)
    				let
    					(* Save the old buffer contents so we can overwrite it. *)
    					val part = !buflimit - !bufp
    					val partInput = arrayExtract(buf, !bufp, part)
    					val request' = request-part
    					(* Reset the buffer pointer before calling fillBuffer.
    					   If we get an exception the buffer will be in an
    					   undefined state. *)
    					val unused = bufp := !buflimit
    					val fillAttempt =
    						if isBlocking
    						then SOME(fillBuffer(rdr, buf, request', caller))
    						else fillBufferNB(rdr, buf, request', caller)
    				in
    					case fillAttempt of
    						(* If we can't read without blocking the result
    						   we return depends on whether we had something
    						   in the buffer before.  If we did then we
    						   return that otherwise we can't return anything. *)
    						NONE => (partInput, false)
    					|	SOME 0 => (* End of file. *) (partInput, true)
    					|	SOME buflim =>
    						let
    							(* Extract as much as we can. *)
    							val toExtract = Int.min(request', buflim)
    							val v = arrayExtract(buf, 0, toExtract)
    						in
    							buflimit := buflim;
    							bufp := toExtract;
    							(Vector.concat[partInput, v],
    								(* In blocking mode, we have reached EOF if we
    								   have only partly filled the buffer. *)
    								isBlocking andalso buflim < request')
    						end
    				end
    
    				else (* Request for a "large" vector.  This is probably worth
    						dealing with separately rather than as repeated
    						reads via the buffer. *)
    				let
    					val part = !buflimit - !bufp
    					val partInput = arrayExtract(buf, !bufp, part)
    					val request' = request-part
    				in
    					bufp := !buflimit;
    					if isBlocking
    					then
    					let
    						val readRest = readVecForced(rdr, request', caller)
    						val v = Vector.concat (partInput :: readRest)
    					in
    						(v, Vector.length v < request)
    					end
    					else (* Non blocking. *)
    					case readVecNB(rdr, request', caller) of
    						NONE => (partInput, false)
    					|	SOME v =>
    							if Vector.length v = 0
    							then (partInput, true) (* End of file. *)
    							else (Vector.concat [partInput, v], false)
    				end; (* val (resVec, isEOF) *)
    
    			(* Find out the length we managed to read.  N.B. a
    			   value of zero here means that we could not read
    			   any characters without blocking.  The isEOF flag
    			   says whether we encountered the end-of-file. *)
    			val returnLen = Vector.length resVec
    			(* Put the vector into the stream as though we'd
    			   read it, but only if we actually found something. *)
    			val newStream =
    				if returnLen > 0
    				then extendStream(f, resVec, 0, Vector.length resVec)
    				else f
    			(* If we have reached the end of file we have
    			   to insert an empty vector into the stream
    			   as well as the vector we've read. *)
    			val newStream' =
    				if isEOF
    				then extendStream(newStream, emptyVec, 0, 0)
    				else newStream
    		in
    			(resVec, newStream', isEOF)
    		end (* readFromReader *)
    
    
    	(* Try to read something from the stream.  Note: it is possible
    	   to use "input" even if readVec is not provided.  If only readVecNB
    	   is provided we can still use input to read from the stream provided
    	   canInput is called first to "pre-load" the stream. *)
    	fun input' (f as InStream {state = ref s,
    						 	  rdr as RD{name, ...}, buffer, locker, ...}) =
    		case s of
    			Truncated => (* Return empty vector. *) (emptyVec, f)
    		|	HaveRead{vec, start, length, rest} =>(* Return what we read before. *)
    				(vectorExtract(vec, start, length),
    				 InStream{state=rest,rdr=rdr, buffer=buffer, locker = locker}) 
    		|	ToRead =>
    			case buffer of
    				Unbuffered =>
    				(
    					case rdr of
    						RD{readVec=SOME rv, ...} =>
    						let
    							(* Should we try reading more than a single
    							   character here? *)
    							val v = rv 1
    								handle exn => raise mapToIo(exn, name, "input")
    						in
    							(v, extendStream(f, v, 0, Vector.length v))
    						end
    					|	_ => (* readVec is not provided. *)
    						raise Io { name = name,
    								   function = "input",
    								   cause = BlockingNotSupported }
    				)
    			|	Buffered { buf, bufp, buflimit=ref buflim} =>
    				(
    					if !bufp < buflim (* Something in the buffer? *)
    					then
    					let (* Return the rest of the buffer. *)
    						val bp = !bufp
    						val v = arrayExtract(buf, bp, buflim - bp)
    					in
    						(* Advance the buffer pointer before linking
    						   on the new *)
    						bufp := buflim;
    						(v, extendStream(f, v, 0, Vector.length v))
    					end
    					else (* Buffer is empty - read a chunk and return it. *)
    					case rdr of
    						RD {readVec=SOME rv, chunkSize, ...} =>
    							 (* Read a vector from the stream, bypassing
    							    the buffer since we are going to return
    								everything. *)
    						let
    							val v = rv chunkSize
    								handle exn => raise mapToIo(exn, name, "input")
    						in
    							(v, extendStream(f, v, 0, Vector.length v))
    						end
    					|	_ => (* We don't have readArr (or anything that augmentReader
    								could use to make it). *)
    							raise Io { name = name,
    									   function = "input",
    									   cause = BlockingNotSupported }
    				)
    
    
    	(* Internal function.  Returns a list of vectors whose total length does
    	   not exceed the requested length. *)
    	fun inputNList request (f as InStream {state = ref s, rdr, buffer, locker}) =
    		case s of
    			Truncated => (* Return empty list. *) ([], f)
    		|	HaveRead{vec, start, length, rest} =>
    				let
    					val newStream =
    					    InStream{state=rest, rdr=rdr, buffer=buffer, locker=locker}
    				in
    					(* If the length was 0 the new stream we return
    					   is the original "next stream". *)
    					if length = 0 then ([], newStream) (* This was an end-of-file. *)
    
    					else if length = request
    					 (* The vector is exactly the right size. *)
    					then ([vectorExtract(vec, start, length)], newStream)
    
    					else if length > request
    					then (* We can satisfy the request from this but we need
    							to split the stream to ensure that the next read
    							from the stream we create will return the residue.
    							In this case only we don't record in the stream that
    							we have returned this vector. *)
    					let
    					    (* We need to construct a new stream to return the rest. *)
    						val result = vectorExtract(vec, start, request)
    						val r = HaveRead{vec=vec, start=start+request,
    										 length=length-request, rest=rest}
    					in
    						([result], InStream{state=ref r, rdr=rdr, buffer=buffer, locker=locker})
    					end
    
    					else (* We need to swallow this vector and continue. *)
    					let
    						(* See if we get the extra we need. *)
    						val nextStream = 
    							InStream {state=rest, rdr=rdr, buffer=buffer, locker=locker}
    						val (rest, f') = inputNList (request-length) nextStream
    					in
    						(vectorExtract(vec, start, length) :: rest, f')
    					end
    				end
    
    		|	ToRead =>
    			let
    				val (vec, stream', _) =
    					readFromReader(f, true (* Blocking *), request, "inputN")
    			in
    				([vec], stream')
    			end
    
     
    	(* Read everything up to the end-of-file. *)
    	fun inputAll' (f as InStream {rdr = RD{avail, chunkSize, ...}, ...}) =
    	let
    		(* If we can determine the amount left to read we try reading
    		   that much, plus a little.  Otherwise we just read a block. *)
    		fun readAll f b =
    		let
    			val toRead =
    				case avail() of
    					SOME b => b+1
    				|	NONE => chunkSize
    			val (vlist, f') = inputNList toRead f
				val vec = Vector.concat vlist
    		in
    			if Vector.length(#1 (input' f')) = 0 (* At end of stream. *)
    			then (Vector.concat(List.rev(vec::b)), f') (* Then we're finished. *)
    			else readAll f' (vec::b)
    		end
    	in
    		readAll f []
    	end
    
    
    	(* Can we input up to n characters without blocking?  Returns the
    	   number of characters that can be input.
    	   Note: As in several of the other cases it is not well-defined, especially
    	   when applied to a "committed" stream.  It is also unclear whether
    	   canInput itself actually commits a stream if applied to an uncommitted
    	   stream.  I have assumed that it does.  Hence if applied to a committed
    	   stream it returns SOME(min(s, n)) where s is the size of the vector that was
    	   originally returned and will, by the semi-deterministic rule, be returned
    	   on a subsequent call.  Applied to an uncommitted stream, if it returns
    	   SOME k, rather than NONE, then a subsequent call to input will return
    	   a vector of size k.
    	   *)
    	fun canInput' request (f as InStream {state = r as ref s, ...}) =
    		if request < 0 then raise Size (* Validate arguments. *)
    		else case s of
    			Truncated => NONE (* Not if it's been truncated. *)
    		|	HaveRead{length, ...} =>
    				(* Return the smaller of the size of the vector or
    				   the requested size. *)
    				SOME (Int.min(length, request))
    		|	ToRead =>
    				let
    					val (vec, _, isEOF) =
    						readFromReader(f, false (* non-blocking *), request, "canInput")
    					val returnLen = Vector.length vec
    				in
    					if returnLen = 0 andalso not isEOF then NONE else SOME returnLen
    				end

    in
    
		val input = protectIn input'

	    val inputAll = protectIn inputAll'

		fun canInput(f, n) = protectIn (canInput' n) f
		
    	fun inputN (f, request) =
    		if request < 0 orelse request > Vector.maxLen
    		then raise General.Size
    		else
    		let
    			val (vlist, f') = protectIn (inputNList request) f
    		in
    			(Vector.concat vlist, f')
    		end
	end
   
	(* Note a crucial difference between inputN and input1.  Because input1
	   does not return a stream if it detects EOF it cannot advance beyond
	   a temporary EOF in a stream. *)
	fun input1 f =
	let
		val (s, f') = inputN (f, 1)
	in
		if Vector.length s = 0
		then NONE
		else SOME(Vector.sub(s, 0), f')
	end

	(* Look for end-of-stream. Could be defined more directly
	   but it probably isn't worth it. *)
	fun endOfStream f =
	let
		val (v, _) = input f
	in
		Vector.length v = 0
	end


	(* OUTPUT *)
	(* In order to be able to flush and close the streams when we exit
	   we need to keep a list of the output streams.  There is also a
	   case for keeping a list of input streams in order to flush
	   any buffered data retained by "commit".  I'm ignoring that for
	   the moment.
	   One unfortunate side-effect of this is that the RTS can't
	   garbage-collect output streams since there will always be
	   a reference to a stream until it is explicitly closed. *)
	val ostreamLock = Thread.Mutex.mutex()
	val outputStreamList: outstream list ref = ref nil;
	
	fun mkOutstream'(wrtr as WR{chunkSize, ...}, buffMode) =
	let
	    open Thread.Mutex
		val strm =
			OutStream{wrtr=PrimIO.augmentWriter wrtr,
					  buffType=ref buffMode,
					  buf=Array.array(chunkSize, someElem),
					  isTerm=ref false,
					  bufp=ref 0,
					  locker=Thread.Mutex.mutex()}
	in
		(* Add it to the list. *)
		outputStreamList := strm :: ! outputStreamList;
		strm
	end
	
	val mkOutstream = protect ostreamLock mkOutstream'

	fun getBufferMode(OutStream{buffType=ref b, ...}) = b

    local
    	(* Flush anything from the buffer. *)
    	fun flushOut'(OutStream{buf, bufp=bufp as ref endBuf,
    						   wrtr=wrtr as WR{name, ...}, ...}) =
    		if endBuf = 0 then () (* Nothing buffered *)
    		else case wrtr of
    			WR{writeArr=SOME wa, ...} =>
    			let
    				fun flushBuff n =
    				let
    					val written =
    						wa(ArraySlice.slice(buf, n, SOME(endBuf-n)))
    						handle exn => raise mapToIo(exn, name, "flushOut")
    				in
    					if written+n = endBuf then ()
    					else flushBuff(written+n)
    				end
    			in
    				(* Set the buffer to empty BEFORE writing anything.  If
    				   we get an asynchronous interrupt (ctrl-C) we want to
    				   lose data in preference to duplicating it. *)
    				bufp := 0;
    				flushBuff 0
    			end
    		|	_ =>
    			raise Io { name = name, function = "flushOut",
    					   cause = BlockingNotSupported }

    	(* Terminate a stream either because it has been closed or
    	   because we have extracted the underlying writer. *)
    	fun terminateStream'(OutStream{isTerm=ref true, ...}) = () (* Nothing to do. *)
    	  | terminateStream'(f as OutStream{isTerm, ...}) =
    		let
    			(* outstream is not an equality type but we can get the
    			   desired effect by comparing the isTerm references for
    			   equality (N.B. NOT their contents). *)
    			fun removeThis(OutStream{isTerm=isTerm', ...}) =
    				isTerm' <> isTerm
        	    open Thread.Mutex
    		in
    			isTerm := true;
    		    lock ostreamLock;
    			outputStreamList := List.filter removeThis (!outputStreamList);
        		unlock ostreamLock;
    			flushOut' f
    		end;
	  
    	(* Close the stream.  It is safe to repeat this and we may need to close
    	   the writer even if the stream is terminated. *)
    	fun closeOut'(f as OutStream{wrtr=WR{close, ...}, ...}) =
    		(
    		terminateStream' f;
    		close() (* Close the underlying writer. *)
    		)

    	(* Flush the stream, terminate it and return the underlying writer. *)
    	fun getWriter'(OutStream{wrtr=WR{name, ...}, isTerm=ref true, ...}) =
    		(* Already terminated. *)
    			raise Io { name = name, function = "getWriter",
    					   cause = ClosedStream }
    	 |  getWriter'(f as OutStream{buffType, buf, bufp, wrtr, isTerm, ...}) =
    		(
    		   terminateStream' f;
    		   (wrtr, !buffType)
    		)

    	(* Set the buffer mode, possibly flushing the buffer as it does. *)
    	fun setBufferMode' newBuff (f as OutStream{buffType, buf, bufp, wrtr, ...}) =
    	(* Question: What if the stream is terminated? *)
    		(
    		if newBuff = NO_BUF andalso !bufp <> 0
    		then (* Flush pending output. *)
    			(* TODO: If we are switching from block to line buffering
    			   should we flush any completed lines? *)
    			flushOut' f
    		else ();
    		buffType := newBuff
    		)

    	(* Internal function: Write a vector directly to the writer. It only
    	   returns when the vector has been completely written. *)
    	fun writeVec(OutStream{wrtr=WR{writeVec=SOME wv, name, ...}, ...}, v, i, len) =
    	let
    		fun forceOut p =
    		let
    			val written = wv(VectorSlice.slice(v, p+i, SOME(len-p)))
    				handle exn => raise mapToIo(exn, name, "output")
    		in
    			if written+p = len then ()
    			else forceOut(written+p)
    		end
    	in
    		forceOut 0
    	end
    	|	writeVec(OutStream{wrtr=WR{name, ...}, ...}, _, _, _) =
    			raise Io { name = name, function = "output",
    					   cause = BlockingNotSupported }
    
    	(* Internal function. Write a vector to the stream using the start and
    	   length provided. *)
    	fun outputVector _ (OutStream{isTerm=ref true, wrtr=WR{name, ...}, ...}) =
    		raise Io { name = name, function = "output", cause = ClosedStream }
    	|   outputVector (v, start, vecLen) (f as OutStream{buffType, buf, bufp, wrtr, ...})  =
    	let
    		val buffLen = Array.length buf
    
    		fun addVecToBuff () =
    			if vecLen < buffLen - !bufp
    			then (* Room in the buffer. *)
    				(
    				arrayCopyVec{src=v, si=start, len=vecLen, dst=buf, di= !bufp};
    				bufp := !bufp + vecLen
    				)
    			else
    			let
    				val buffSpace = buffLen - !bufp
    			in
    				(* Copy as much of the vector as will fit *)
    				arrayCopyVec{src=v, si=start, len=buffSpace, dst=buf, di= !bufp};
    				bufp := !bufp+buffSpace;
    				(* TODO: Flushing the buffer ensures that all the
    				   buffer contents have been written.  We don't
    				   actually need that, what we need is for enough
    				   to have been written that we have space in the
    				   buffer for the rest of the vector. *)
    				flushOut' f; (* Write it out. *)
    				(* Copy the rest of the vector. *)
    				arrayCopyVec{src=v, si=start+buffSpace, len=vecLen-buffSpace, dst=buf, di=0};
    				bufp := vecLen-buffSpace
    			end (* addVecToBuff *)
    	in
    		if vecLen > buffLen
    		then (* If the vector is too large to put in the buffer we're
    				going to have to write something out.  To reduce copying
    				we simply flush the buffer and write the vector directly. *)
    			(flushOut' f; writeVec(f, v, start, vecLen))
    		else (* Try copying to the buffer. *)
    			if !buffType = IO.NO_BUF
    			then (* Write it directly *) writeVec(f, v, start, vecLen)
    			else (* Block or line buffering - add it to the buffer.
    					We can't actually do line buffering at this level
    					since it doesn't make sense when we don't know
    					what constitutes a line separator. *)
    				addVecToBuff()
    	end (* outputVec *)
    
    	(* This could be defined in terms of outputVector but this is
    	   likely to be much more efficient if we are buffering. *)
    	fun output1' _ (OutStream{isTerm=ref true, wrtr=WR{name, ...}, ...}) =
    		raise Io { name = name, function = "output1", cause = ClosedStream }
    	 |  output1' c (f as OutStream{buffType, buf, bufp, ...}) =
    		if !buffType = IO.NO_BUF
    		then writeVec(f, Vector.fromList[c], 0, 1)
    		else (* Line or block buffering. *)
    		(
    			Array.update(buf, !bufp, c);
    			bufp := !bufp + 1;
    			if !bufp = Array.length buf then flushOut' f else ()
    		)

    	fun getPosOut'(f as OutStream{wrtr=WR{name, getPos=SOME getPos, ...}, ...}) =
    		(
        		flushOut' f;
        		OutPos(f, getPos()) handle exn => raise mapToIo(exn, name, "getPosOut")
    		)
    			
    	|   getPosOut'(OutStream{wrtr=WR{name, ...}, ...}) =
    			raise Io { name = name, function = "getPosOut",
    					   cause = RandomAccessNotSupported }
    
    	fun setPosOut' p (f as OutStream{wrtr=WR{name, setPos=SOME setPos, ...}, ...}) =
			(
    			flushOut' f;
    			setPos p;
    			f
			)
    	|   setPosOut' p (OutStream{wrtr=WR{name, ...}, ...}) =
    			raise Io { name = name, function = "setPosOut",
    					   cause = RandomAccessNotSupported }
	in
	    fun output1(f, c) = protectOut (output1' c) f
    	fun output(f, v) = protectOut (outputVector(v, 0, Vector.length v)) f
      	val flushOut = protectOut flushOut'
    	val terminateStream = protectOut terminateStream'
    	val closeOut = protectOut closeOut'
	    val getWriter = protectOut getWriter'
     	fun setBufferMode(f, n) = protectOut (setBufferMode' n) f
  
    	(* Exported function to output part of a vector.  Non-standard. *)
    	fun outputVec(f, slice) =
    		let
    			val (v, i, len) = VectorSlice.base slice
    		in
    			protectOut (outputVector(v, i, len)) f
    		end

		val getPosOut = protectOut getPosOut'

		fun setPosOut(OutPos(f, p)) = protectOut (setPosOut' p) f
	end


	fun filePosOut(OutPos(_, p)) = p

	(* We need to set up a function to flush the streams when we
	   exit.  This has to be set up for every session so we set up
	   an entry function, which is persistent, to do it. *)
	local
		fun closeAll () =
		(* Close all the streams.  closeOut removes the streams
		   from the list so we should end up with outputStreamList
		   being nil. *)
			List.foldl (fn (s, ()) => closeOut s handle _ => ()) ()
				(! outputStreamList)
		(* In addition, discard any unwritten data in open streams.
		   If we have called PolyML.export with unwritten data that will still be
		   there whenever the exported function is run so we need to discard it. 
		   This issue really applies only to stdOut since stdErr is normally
		   unbuffered and other streams will generate an exception if we try to
		   write. *)
		fun discardAll () =
		    List.app (fn(OutStream{bufp, ...}) => bufp := 0) (! outputStreamList)
		(* When we load a saved state global variables are overwritten.  We need
		   to preserve the outputStreamList across the call.  We also flush the
		   buffers before the call and discard any output that had been buffered
		   in the saved state.
		   This is a bit of a mess and probably needs to be changed. *)
		fun doOnLoad doLoad =
		let
		    val savedList = ! outputStreamList
		in
			List.app flushOut savedList;
			doLoad();
			outputStreamList := savedList;
			discardAll()
		end
		fun doOnEntry () = (discardAll(); PolyML.onLoad doOnLoad; OS.Process.atExit closeAll)
	in
		val it = PolyML.onEntry doOnEntry;
		val it = doOnEntry() (* Set it up for this session as well. *)
	end
end;

(* Define the StreamIO functor in terms of BasicStreamIO to filter
   out outputVec. *)
(* This is non-standard.  According to G&R 2004 StreamIO does not take the slice structures as args. *)
functor StreamIO(
	structure PrimIO : PRIM_IO
    structure Vector : MONO_VECTOR
    structure Array : MONO_ARRAY
	structure VectorSlice: MONO_VECTOR_SLICE
	structure ArraySlice: MONO_ARRAY_SLICE
    sharing type PrimIO.elem = Vector.elem = Array.elem = VectorSlice.elem = ArraySlice.elem
    sharing type PrimIO.vector = Vector.vector = Array.vector = VectorSlice.vector = ArraySlice.vector
    sharing type PrimIO.array = Array.array = ArraySlice.array
	sharing type PrimIO.vector_slice = VectorSlice.slice = ArraySlice.vector_slice
	sharing type PrimIO.array_slice = ArraySlice.slice
    val someElem : PrimIO.elem
	): STREAM_IO =
struct
	structure StreamIO =
		BasicStreamIO(structure PrimIO = PrimIO
					  and Vector = Vector
					  and Array = Array
					  and VectorSlice = VectorSlice
					  and ArraySlice = ArraySlice
					  val someElem = someElem)
	open StreamIO
end;
