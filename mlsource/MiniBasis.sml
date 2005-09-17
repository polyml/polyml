(*
	Copyright (c) 2000
		Cambridge University Technical Services Limited
		and David C. J. Matthews

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

(*
The compiler now makes use of the Standard Basis Library but the
library can only be compiled in ML 97.  This file contains a highly
cut-down version of the basis library which will compile in ML 90
and allow the compiler to be bootstrapped.
*)
datatype 'a option = NONE | SOME of 'a

local
    structure Fail =
      RunCall.Run_exception1
        (
          type ex_type = string;
          val ex_iden  = RuntimeCalls.EXC_Fail
        )
in
    exception Fail = Fail.ex
end;

exception Subscript = Array.Subscript
exception Size = Array.Size

structure Vector =
struct
	open Vector
	val fromList = vector
end;

structure Array =
struct
	open Array

	local
		(* This relies on update and sub to do the bounds checking which
		   isn't strictly correct. *)
		fun docopy(src, si, dst, di, len) =
			if len = 0 then ()
			else
			(
			update(dst, di, sub(src, si));
			docopy(src, si+1, dst, di+1, len-1)
			)
	in
		fun copy{src: 'a array, si: int, len=SOME l, dst: 'a array, di: int} =
			docopy(src, si, dst, di, l)
		| copy{src, si, len=NONE, dst, di} =
			docopy(src, si, dst, di, length src - si)
	end
end;

structure SML90 =
struct
	exception Interrupt = Interrupt
	val std_out = std_out
	and output = output
	and chr = chr
	and ord = ord
end;

structure IO =
struct
exception Io = Io (* Actually not the same parameters. *)
end

structure TextIO =
struct
	type instream = BasicIO.instream and outstream = BasicIO.outstream
	val openAppend: string -> outstream = ExtendedIO.open_append
	val openIn: string -> instream = BasicIO.open_in
	val openOut: string -> outstream = BasicIO.open_out
	val closeIn: instream -> unit = BasicIO.close_in
	val closeOut: outstream -> unit = BasicIO.close_out
	val flushOut: outstream -> unit = ExtendedIO.flush_out
	val inputLine = ExtendedIO.input_line
	fun canInput(strm, n) =
		if ExtendedIO.can_input(strm, n) then SOME n else NONE
	val output = BasicIO.output
	val stdIn = std_in and stdOut = std_out
	val endOfStream = end_of_stream
	fun lookahead strm =
	let
		val s = BasicIO.lookahead strm
	in
		if s = "" then NONE else SOME s
	end

	fun inputN(strm, n) =
	let
		val s = BasicIO.input(strm, n)
		val len = size s
	in
		if len = 0 then s
		else if len < n then s ^ inputN(strm, n-len)
		else s
	end

	fun input strm = BasicIO.input(strm, 100 (* Arbitrary number. *))

	fun print s = (output(stdOut, s); flushOut stdOut)
		
end

structure StringCvt =
struct
    datatype radix = BIN | OCT | DEC | HEX
end;


structure Int =
struct
	val (* op quot: int * int -> int = RunCall.run_call2 POLY_SYS_adiv
	and *) op rem:  int * int -> int =
				RunCall.run_call2 RuntimeCalls.POLY_SYS_amod

	local
	    val ord0 = ord "0";
	  
	    fun soi acc 0 = acc
	    |   soi acc n = soi (chr (n mod 10 + ord0) ^ acc) (n div 10);
	in
	    fun toString 0 = "0"
	    |   toString n = if n < 0 then "~" ^ soi "" (~n) else soi "" n
	end

	local
	    val ord0 = ord "0";
	    val ord9 = ord "9";
	
	    fun ioc c =
	    let
	      val ordc = ord c
	    in
	      if ordc < ord0 orelse ordc > ord9
	      then NONE
	      else SOME(ordc - ord0)
	    end;
  
	    fun convert []     s n = if s then SOME(~n) else SOME n
	    |   convert (H::T) s n =
				case ioc H of
					NONE => NONE
				|	SOME i => convert T s (n * 10 + i);

	in
		(* This is a cut-down version which works in the cases we need. *)
		fun fromString s =
	    (
 	     case explode s of
			[]     =>   NONE
	      | ["~"]  =>   NONE
	      | "~"::t =>   convert t true 0
	      | t      =>   convert t false 0
	    )

	end

	fun fmt StringCvt.HEX n = 
	  let
	    fun hex n =
	    let
	      val d = n mod 16;
	      val r = n div 16;
	      val c = chr (if d < 10 then d + ord "0" else d - 10 + ord "a");
	    in
	      if r = 0 then c else hex r ^ c
	    end;
	  in
	    if n < 0 then "~0x" ^ hex (~n) else "0x" ^ hex n
	  end
	 | fmt StringCvt.DEC n = toString n
	 | fmt _ _ = raise Fail "Not implemented"

	fun max(a: int, b: int) = if a > b then a else b
	and min(a: int, b: int) = if a < b then a else b
end;


structure String =
struct
	(* N.B. The types of sub and implode are not the same
	   as those in basis library version of String. *)
	local
		val string_sub = RunCall.run_call2 RuntimeCalls.POLY_SYS_string_sub
	in
		val sub : string*int->string = fn (s, i) => string_sub(s, i+1)
	end

	fun str c = c;

	local
	    val ord0 = ord "0";

	    fun soi acc 0 = acc
	    |   soi acc n = soi (chr (n mod 10 + ord0) ^ acc) (n div 10);

	    fun escape (c : string) : string =
		      case c of
			"\"" => "\\\""
		      | "\\" => "\\\\"
		      | "\n" => "\\n"
		      | "\t" => "\\t"
		      | _    =>
		      let
			val i : int = ord c
	      in
			if i < 32
			  then "\\^" ^ chr (i + 64)
			else if i >= 127
			  then "\\" ^ soi "" i (* always 3 digits since 127 <= i < 256 *)
			else c
	      end
	in
	    fun toString (s: string) : string =
	      implode (map escape (explode s))
	end


	val concat: string list -> string = implode
	(* At this stage we define it to take a string list. *)
	val implode: string list -> string = implode

	fun substring(s: string, i: int, n: int): string =
	    RunCall.run_call3 RuntimeCalls.POLY_SYS_substring (s,i+1,n) ;
end;

structure Char =
struct
	val ord = ord
	val chr = chr

	fun isUpper (ch: string) =
	let
		val ch' = ord ch
	in
		ch' >= ord "A" andalso ch' <= ord "Z"
	end

	fun isLower (ch: string) =
	let
		val ch' = ord ch
	in
		ch' >= ord "a" andalso ch' <= ord "z"
	end

	local
	
	    structure Conversion =
	      RunCall.Run_exception1
	        (
	          type ex_type = string;
	          val ex_iden  = RuntimeCalls.EXC_conversion
	        );
	
	    exception Conversion = Conversion.ex;
	
		fun numToString i =
			if i < 10 then chr(ord("0") + i)
			else numToString(i div 10) ^ chr(ord("0") + i)
	
		fun isWhiteSpace (ch:int) =
		    case ch of
		      32   => true (* space *)
		    | 10   => true (* newline *)
		    | 9    => true (* tab *)
		    | 12   => true (* formfeed *)
		    | _    => false;

		fun processEscape getc src =
			case getc src of
				NONE => raise Conversion "Unexpected end of string"
			|	SOME (ch, next) =>
		 		if ord ch < 32 orelse ord ch >= 127
				then raise Conversion
							("Invalid character (code " ^ numToString(ord ch) ^ ") found")
		 		else if ch = chr 92 (* Backslash *)
				then
					case getc next of
						NONE => raise Conversion "Invalid escape sequence"
					  | SOME (ch', next') =>
					  		if ch' = "a" then (chr 7, next')
							else if ch' = "b" then (chr 8, next')
							else if ch' = "t" then (chr 9, next')
							else if ch' = "n" then (chr 10, next')
							else if ch' = "v" then (chr 11, next')
							else if ch' = "f" then (chr 12, next')
							else if ch' = "r" then (chr 13, next')
							else if ch' = chr 34 (* double quote *)
							then (chr 34, next')
							else if ch' = chr 92 (* backslash *)
							then (chr 92, next')
							else if ch' = "^" (* Escape character *)
							then
								(
								case getc next' of
									NONE => raise Conversion "Invalid escape sequence"
								  | SOME(ch'', next'') =>
								  	if ord ch'' < 64 orelse ord ch'' > 95
									then raise Conversion "Invalid escape sequence"
									else (chr(ord ch'' - 64), next'')
								)
							else if ch' = "0" orelse ch' = "1" orelse ch' = "2" (* *)
							then
								case getc next' of
									NONE => raise Conversion "Invalid escape sequence"
								|	SOME(ch2, next1) =>
									case getc next1 of
										NONE => raise Conversion "Invalid escape sequence"
									|	SOME(ch3, next'')
										=> if ord ch2 < ord "0" orelse ord ch2 > ord "9"
										   orelse ord ch3 < ord "0" orelse ord ch3 > ord "9"
										   then raise Conversion "Invalid escape sequence"
										   else
										   	  let
											  	val code = (ord ch' - ord "0") * 100 +
														   (ord ch2 - ord "0") * 10 +
														   (ord ch3 - ord "0")
											  in
											  	if code > 255
												then raise Conversion "Invalid escape sequence"
												else (chr code, next'')
											  end
							(*
							else if ch' = "u" (* Hexadecimal escape. *)
							then .... :: processEscape next'
							*)
							else if isWhiteSpace(ord ch') (* Skip white space *) 
							then
								let
								fun skipSpace n =
									case getc n of 
										NONE =>
											raise Conversion "Invalid escape sequence"
									 |  SOME(ch, next) =>
										 	if isWhiteSpace (ord ch)
											then skipSpace next
											else (ch, next)
								val (ch'', next'') = skipSpace next'
								in
									if ch'' <> chr 92
									then raise Conversion "Invalid escape sequence"
									else processEscape getc next''
								end
							else raise Conversion "Invalid escape sequence"
				else (ch, next)
	
	in
		fun scan getc src =
			(
			SOME(processEscape getc src)
			) handle _ => NONE
	end

	fun toString ch = String.toString(String.str ch)
end;

val concat = implode;

structure Time =
struct
	type time = int
	exception Time
	val zeroTime: time = 0
	fun now () : time = 
		RunCall.run_call2 RuntimeCalls.POLY_SYS_timing_dispatch (1, 0)
			handle _ => raise Time
	fun fmt (n: int) (t: time) =
		Int.toString (n div 10) ^ "." ^ Int.toString(n mod 10);
end

structure OS =
struct
    type  syserror = int
    exception SysErr of (string * syserror option)

	structure Process =
	struct
		type status = int
		val success = 0
		fun exit (n: int) : unit =
			RunCall.run_call1 RuntimeCalls.POLY_SYS_exit n;

		fun system (command: string) : int =
		let
 	    	val (inStream,outStream) = ExtendedIO.execute command;
			fun printString s = output(std_out, s)
		in
			close_out outStream;
	     
			(* Read until end-of-file, which should synchronise with the
	           command. *)
			while not (end_of_stream inStream) do
				printString (input (inStream,1));
	       
			close_in inStream;
			0
	 	end
	end

	structure FileSys =
	struct
		local
			val doIo: int*unit*string -> bool
				 = RunCall.run_call3 RuntimeCalls.POLY_SYS_io_dispatch
		in
			fun isDir s = doIo(57, (), s)
		end
		local
			val doIo: int*unit*string -> Time.time
				 = RunCall.run_call3 RuntimeCalls.POLY_SYS_io_dispatch
		in
			(* Get file modification time. *)
			fun modTime s = doIo(61, (), s)
		end
	end
end;

structure Date =
struct
	type date = int
	fun fromTimeLocal t = t

	fun toString t =
	let
		val stringOfTime: int -> string =
			RunCall.run_call1 RuntimeCalls.POLY_SYS_repr_time;
		val st = stringOfTime t
		val len = size st
	in
		(* Remove the trailing newline. *)
		String.substring(st, 0, len-1)
	end
end

structure Timer =
struct
	type cpu_timer = int*int
	local
		val doCall: int*unit -> Time.time
			 = RunCall.run_call2 RuntimeCalls.POLY_SYS_timing_dispatch
		fun getUserTime() = doCall(7, ())
		and getSysTime() = doCall(8, ())
	in
	fun startCPUTimer () = (getUserTime(), getSysTime())
	val checkCPUTimer : cpu_timer -> {usr : Time.time, sys : Time.time} =
		fn (u, s) => { sys=getSysTime()-s, usr=getSysTime()-u }
	end
end

structure List =
struct
	exception Empty
	fun hd (a::_) = a | hd _ = raise Empty
	and tl (_::a) = a | tl _ = raise Empty

	fun null [] = true | null (_::_) = false

	fun nth([], _) = raise Subscript
	 |  nth(a::_, 0) = a
	 |  nth(_::l, i) = nth(l, i-1)

	fun exists f [] = false
	  | exists f (a::b) = if f a then true else exists f b

	fun length l =
	let
	(* Tail-recursive function. *)
	fun len [] i = i
	 |  len (_::l) i = len l (i+1)
	in
	len l 0
	end

	fun foldl f b [] = b
	  | foldl f b (x::y) = foldl f (f(x, b)) y

	fun app f [] = ()
	 |  app f (h::t) = (f h; app f t)
end;

structure ListPair =
struct
	fun unzip ((a, b) :: l) =
		let
		val (x, y) = unzip l
		in
		(a :: x, b :: y)
		end
	 | unzip [] = ([], [])
end;

exception Empty = List.Empty
val hd = List.hd
val tl = List.tl
val null = List.null
val length = List.length
exception Overflow = Sum;
(*
structure General =
struct
	exception Fail of string
end;
*)