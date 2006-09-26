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


(*
    Title: 	Control debugging output.
    Author: 	Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)


structure Debug =

(*****************************************************************************)
(*                  Body of DEBUG                                            *)
(*****************************************************************************)
struct
  val bindName = ref "ml_bind";
  val archSuffix = "." ^ String.map Char.toLower (PolyML.architecture())
  (* The architecture-specific suffixes take precedence. *)
  val suffixes = ref [archSuffix, "",archSuffix^".ML", ".ML", archSuffix^".sml", ".sml"];
  val useRCS   = ref true;

  val prompt1 = ref "> ";
  val prompt2 = ref "# ";

 (* A new debug switch for the ML system.
  The other parts are the same as for Poly. *)
 
  (* local "union" datatype value constructors are represented as
     tagged tuples rather than as tagged boxes providing they have no
     more than !maxPacking fields. *)
  val maxPacking          = ref 4;
  val inlineFunctors      = ref true;
  val parsetree           = ref false;
  val codetree            = ref false;
  val pstackTrace         = ref false;
  val assemblyCode        = ref false;
  val codetreeAfterOpt    = ref false; (* If true then print the optimised code. *)
  val alwaysUseClosure    = ref false; (* If true do not use static-link calling. *)
  val maxInlineSize       = ref 40;
  (* Switches to control printing. *)
  val printInAlphabeticalOrder = ref true; (* If false, print in declaration order. *)
  val printTypesWithStructureName = ref true; (* If false, only print the last component of the name. *)

  val timing        = ref false;
  val profiling     = ref 0;
  val errorDepth    = ref 6;
  val printDepth    = ref 999;
  val lineLength    = ref 77;
  val traceCompiler = ref false;
  val ml90			= ref false;
  val debug			= ref false;

  (* The only reason for these functions is to allow us to modify
     the compiler to use the newly created stdIn and stdOut when
	 we have compiled the basis library in the newly compiled compiler.
	 That's important because it's possible to redirect stdIn and stdOut.
	 N.B.  Once the prelude and basis library have been compiled these
	 functions are replaced by equivalent functions in prelude/ReplaceStdio. *)
  val printString = ref (fn s => TextIO.output(TextIO.stdOut, s))
  val inputChar = ref(fn () => TextIO.input1 TextIO.stdIn)
  local
  	  fun checkNewline () =
	  	case TextIO.lookahead TextIO.stdIn of
		    SOME #"\n" => true (* "\n" or #"\n" *)
		|	_          => false
  in
	  val nextIsNewline = ref checkNewline
  end
  local
  	  fun flushInput () =
	  	  case TextIO.canInput(TextIO.stdIn, 1) of
		  	  SOME 1 => (TextIO.inputN(TextIO.stdIn, 1); flushInput())
		  |   _ => (* No input waiting or we're at EOF. *) ()
  in
      val flushInput = ref flushInput
  end
end;
