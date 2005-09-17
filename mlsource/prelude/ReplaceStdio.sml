(*
	Copyright (c) 2000
		Cambridge University Technical Services Limited

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

(* Install the new TextIO.stdIn and TextIO.stdOut.  *)

(* printString - output a string. *)
let
	fun outputString s =
		TextIO.output(TextIO.stdOut, s)
			(* If we get an exception while writing to stdOut we've got
			   a big problem and can't continue.  It could happen if
			   we have closed stdOut.  Try reporting the error through
			   stdErr and exit. *)
			handle SML90.Interrupt => raise SML90.Interrupt
			 |     exn =>
				(
					(
					TextIO.output(TextIO.stdErr,
						concat["Exception ", exnName exn,
							   " raised while writing to stdOut.\n"]);
					TextIO.flushOut TextIO.stdErr (* probably unnecessary. *)
					) handle _ => ();
				(* Get out without trying to do anything else.
				   In particular we don't want to commit the database. *)
				OS.Process.terminate OS.Process.failure
				)
in
	RunCall.printString := outputString
end;

(* inputChar - return the next character from the stream. *)
let
	(* Flush the output stream if we are going to block. *)
	fun getInput () =
		case (
				TextIO.canInput(TextIO.stdIn, 1)
				handle
					SML90.Interrupt => raise SML90.Interrupt
				| _ => NONE
			) of
			SOME _ => TextIO.input1 TextIO.stdIn
		|	NONE => (TextIO.flushOut(TextIO.stdOut); TextIO.input1 TextIO.stdIn)
in
	RunCall.inputChar := getInput
end;

(* nextIsNewline - true if the next character is a newline. *)
let
	fun checkNewline () =
  	case TextIO.lookahead TextIO.stdIn of
		NONE => false
	|   SOME c => c = #"\n"
in
	RunCall.nextIsNewline := checkNewline
end;

(* flushInput - flush any pending input if there has been an error. *)
let
	fun flushInput () =
  	  case TextIO.canInput(TextIO.stdIn, 1) of
	  	  SOME 1 => (TextIO.inputN(TextIO.stdIn, 1); flushInput())
	  |   _ => ()  (* No input waiting or we're at EOF. *)
in
  	RunCall.flushInput := flushInput
end;
