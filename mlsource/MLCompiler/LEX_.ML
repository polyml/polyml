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
    Title:      Lexical Analyser.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)


functor LEX_ (
(*****************************************************************************)
(*                 PRETTYPRINTER                                             *)
(*****************************************************************************)
structure PRETTYPRINTER:
sig
  type prettyPrinter 
  
  val ppAddString  : prettyPrinter -> string -> unit
  val ppBeginBlock : prettyPrinter -> int * bool -> unit
  val ppEndBlock   : prettyPrinter -> unit -> unit
  val ppBreak      : prettyPrinter -> int * int -> unit
  val ppLineBreak  : prettyPrinter -> unit -> unit
  
  val prettyPrint : int * (string -> unit) -> prettyPrinter; 
end;

(*****************************************************************************)
(*                  MISC exports signature                                   *)
(*****************************************************************************)
structure MISC :
sig
  exception InternalError of string; (* compiler error *)
end

(*****************************************************************************)
(*                  SYMBOLS                                                  *)
(*****************************************************************************)
structure SYMBOLS :
sig
  type sys;
      
  val abortParse:   sys;
  val othersy:      sys;
  val typeIdent:    sys;
  val ident:        sys;
  val integerConst: sys;
  val stringConst:  sys;
  val semicolon:    sys;
  val comma:        sys;
  val leftParen:    sys;
  val rightParen:   sys;
  val leftBrack:    sys;
  val rightBrack:   sys;
  val leftCurly:    sys;
  val rightCurly:   sys;
  val threeDots:    sys;
  val underline:    sys;
  val realConst:    sys;
  val wordConst:    sys;
  val charConst:    sys;
      
  val lookup: string -> sys;
      
  val eq : sys * sys -> bool;
  val neq : sys * sys -> bool;
end

) :


(*****************************************************************************)
(*                  LEX export signature                                     *)
(*****************************************************************************)
sig
  type lexan;
  type sys;
  type prettyPrinter;
     
  val insymbol: lexan -> unit;
  val lexPrint : lexan -> string -> unit;
     
  (* insymbol sets sy and id which are exported as "read-only" *)
     
  val sy:     lexan -> sys;
  val id:     lexan -> string;
  val lineno: lexan -> int;
  val filename: lexan -> string;
  val pushBackSymbol: lexan * sys -> unit;
     
  val initial: (unit -> char) * string * (unit -> int) * (string -> unit) -> lexan;

  (* Error handling *)
     
  val errorProc:      lexan * int * (prettyPrinter -> unit) -> unit;
  val errorMessage:   lexan * int * string -> unit;
  val warningProc:    lexan * int * (prettyPrinter -> unit) -> unit;
  val warningMessage: lexan * int * string -> unit;
     
  val errorOccurred: lexan -> bool;
  val resetLexan:    lexan -> unit;
  val flushLexan:    lexan -> unit;

  val nullLex: lexan; (* Used when no errors are expected - streams raise exceptions. *)
end (* LEX export signature *) =

(*****************************************************************************)
(*                  LEX functor body                                         *)
(*****************************************************************************)
struct

  open MISC;
  open PRETTYPRINTER;
  open SYMBOLS;              infix 8 eq neq;

  
  type lexan = 
    {
      stream:   unit -> char,
      filename: string,
      lineno:   unit -> int,
      ch:       char ref,
      sy:       sys ref,
      id:       string ref,
      print:    string -> unit,
      errors:   bool ref,
	  pushedSym: sys ref
    };

  val lexPrint : lexan -> string -> unit = #print;

  val eofChar         = Char.chr 4; (* ctrl/D *)

  val isNumeric = Char.isDigit
  and isAlphabetic = Char.isAlpha
  and isWhiteSpace = Char.isSpace
  and isHexadecimal  = Char.isHexDigit

  (* For our purposes we include quote and underscore. *)
  fun isAlphaNumeric c = Char.isAlphaNum c orelse c = #"'" orelse c = #"_"

  val isOperator = Char.contains ":=<>+*!^/|&%~-?`@\\$#";

 (* The initial state looks like we've just processed a complete ML declaration *)
  fun initial (stream, filename, lineno, print) : lexan =
    {
      stream   = stream,
      filename = filename,
      lineno   = lineno,
      ch       = ref #" ",   (* " " - we've just "clobbered the ";" *)
      sy       = ref semicolon,  (* ";"  *)
      id       = ref "",
      print    = print,
      errors   = ref false,
	  pushedSym = ref othersy
    };

   val nullLex = 
     initial
       (
         fn () => eofChar,
         "",
         fn () => 0, 
         fn _ => raise InternalError "no stream"
       );

   (* Error messages *)

   fun errorOccurred ({errors, ...}:  lexan) = ! errors;

   (* Reset lexer following bad parse *)
   fun resetLexan ({errors, ...} : lexan) = (errors := false);

   (* Flush lexer state following user interrupt *)
   fun flushLexan ({errors, ch, sy, ...} : lexan) = 
     (errors := false;
      ch := #" ";
      sy := semicolon);

   fun lineno ({lineno,...}:lexan) = lineno();
   val filename: lexan -> string = #filename

   fun ewProc ({filename,print,errors,...} : lexan) hardError line eproc =
   let
     val pprint    = prettyPrint(77, print);
     val fname     = filename;
     val errorWarn = if hardError then "Error:" else "Warning:";
	 val message =
	 	concat
			(errorWarn ::
			 (if fname = "" then []
			  else " in '" :: fname :: "'" ::
			  	(if line = 0 then [] else [", line ", Int.toString line, "."])
			 ))
   in
     (* If this is a hard error we have to set the flag
        to prevent further passes. *)
     if hardError then errors := true else ();
     (* Print out the message *)
     ppBeginBlock pprint (0, false);
     ppAddString pprint message;
     ppBreak pprint (1, 0);
     eproc pprint;
     ppLineBreak pprint (); (* Blank line between entries. *)
     ppEndBlock pprint ()
   end;
   
   (* General purpose error messages typically including
      pretty-printed parse tree. *)
   fun errorProc (state, lineno, eproc) =
       ewProc state true (*hard*) lineno eproc;

   (* Simple strings. *)
   fun errorMessage (state, lineno, str) =
      ewProc state true (*hard*) lineno (fn pp => ppAddString pp str);

   (* Warnings are non-fatal errors. i.e. errors is not set. *)
   fun warningProc (state, lineno, eproc) =
       ewProc state false (*soft*) lineno eproc;

   fun warningMessage (state, lineno, str) =
      ewProc state false (*soft*) lineno (fn pp => ppAddString pp str);


   (* Insymbol - main lexical analyser. *)

    exception EndOfLine;
    
    fun insymbol (state as {sy,ch,id,stream,pushedSym,...}:lexan) =
	if ! pushedSym neq othersy then pushedSym := othersy
	(* pushedSym is a hack to handle the difficulty of parsing
	   val ('a, 'b) f = ... compared with val (a, b) = ... and the
	   similar fun declarations. 
	   It's also used to handle where type t = int and type ... compared
	   with  where type t = int and S = sig ...*)
	else
    let
      
      (* lexical analyser. sets "sy" to the last symbol read *)
    
      fun nextCh () = (ch := stream ());
      
      (* minimise the number of expensive (RTS call) assignments *)
	  (* Not so relevant now that assignment is inline. DCJM 23/2/01. *)
      fun nextNonWhiteSpaceCh () : char =
      let
        val c : char = stream ();
      in
        if isWhiteSpace c
        then nextNonWhiteSpaceCh ()
        else c
      end;

      fun skipWhiteSpace () : char =
      let
        val c1 : char = !ch;
      in
        if isWhiteSpace c1
        then let
          val c2 : char = nextNonWhiteSpaceCh ();
          val U : unit = ch := c2;
        in
          c2
        end
        else c1
      end;
      
     (* If a character has been read which is its own terminator (e.g.";")
      then don't read the next character, just clobber the current one. (The
      only place this matters is when the user types x;y; at the terminal
      when the compiler is called first to process the x; and then it is
      called again (with reinitialisation) to process the y;.
      Replacing it with a space means that the next character will be read
      from the input stream since leading spaces are skipped.
      Many symbols (e.g. identifiers) are not self-terminating so the
      terminating character is remembered in ch. *)
      fun chRead() = ch := #" "  (* " " *);

   (* leave string construction until we have all the characters.  Since
      Single character strings are the same as single characters it doesn't
	  cost anything to apply "str". *)
   fun readChars (isOk: char -> bool) (s: string) : string = 
   let
     fun loop (): string list =
     let
       val nextChar  = stream ();
     in
       if isOk nextChar
       then str nextChar :: loop ()
       else (ch := nextChar; []) (* save unused character in ch *)
     end;
     
     val next = !ch; (* look at first unused character *)
   in
     if isOk next
     then concat (s :: str next :: loop ())
     else s
   end;

      fun parseNumber () =
      (* Read in a number. *)
         (
            sy := integerConst;
            
            (* Copy digits into the buffer. *)
            id := readChars isNumeric "";
            
            (* May be the end of an integer, part of a real number,
			   or w for word or x for hex. *)
			if !ch = #"w" andalso !id = "0"
			then (* word constant. *)
			(
				sy := wordConst;
                nextCh();
				if !ch = #"x"
				then
				(
					nextCh();
					if isHexadecimal (!ch)
					then id := readChars isHexadecimal "0wx"
					else
	                  errorMessage (state, lineno state,
	                    "malformed word constant: " ^ !id ^ str(!ch))
				)
                else if isNumeric (!ch)
                then id := readChars isNumeric "0w"
				else
                  errorMessage (state, lineno state,
                    "malformed word constant: " ^ !id ^ str(!ch))
			)
			else if !ch = #"x" andalso !id = "0"
			then (* Hexadecimal integer constant. *)
			(
				nextCh();
				if isHexadecimal (!ch)
				then id := readChars isHexadecimal "0x"
				else
                  errorMessage (state, lineno state,
                    "malformed integer constant: " ^ !id ^ str(!ch))
			)
            else if !ch = #"." orelse
					!ch = #"E" orelse !ch = #"e" (* "e" is allowed in ML97 *)
            then (* real constant. *)
			(
            	sy := realConst;
	  			if !ch = #"."
				then
	            (
	               (* Add the "." to the string. *)
	               id := !id ^ ".";
	               nextCh();
	               (* Must be followed by at least one digit. *)
	               if not (isNumeric (!ch))
	               then
	                  errorMessage (state, lineno state,
	                    "malformed real number: " ^ !id ^ str(!ch))
	               else id := readChars isNumeric (!id)
	            )
	            else ();

	            if !ch = #"E" orelse !ch = #"e"
	            then
	            (
	               (* Add the"E" to the buffer. *)
	               id := !id ^ "E";
	               nextCh();
	               
	               (* May be followed by a ~ *)
	               if !ch = #"~"
	               then (id := !id ^ "~"; nextCh())
	               else ();
	               
	               (* Must be followed by a number. *)
	               if not (isNumeric (!ch))
	               then errorMessage (state, lineno state,
	                 "malformed real number: " ^ !id ^ str(!ch))
	               else ();
	               
	               id := readChars isNumeric (!id)
	            )
	            else ()
			)
			else ()
         );

	  fun parseString () =
	  let
	  	 (* The original version of this simply concatenated the characters onto "id".
		    For very long strings that's expensive since each concatenation copies the
			existing string, resulting in quadratic performance.  This version creates a
			list and then implodes it.  DCJM 24/5/02. *)
	  	 fun getString (soFar: char list) =
		 (
			case !ch of
				#"\"" (* double-quote. *) => (* Finished - return result. *) (chRead(); soFar)

			|	#"\n" => (nextCh(); raise EndOfLine)

			|	#"\\" => (* Escape *)
					let
						val _ = nextCh(); (* Skip the escape char. *)
						val next = !ch;   (* Look at the next char. *)
						val _ = nextCh();
					in
			            (* Remove \f...\ sequences but otherwise leave the string
						   as it is.  Escape sequences are processed in the conversion
						   function.  In particular we can only decide whether \uxxxx
						   is valid when we know whether we are converting to Ascii or
						   Unicode. *)
		            if isWhiteSpace next
		            then
						(
						if skipWhiteSpace () = #"\\" then ()
						else
							(
						    errorMessage (state, lineno state,
						       "unexpected character " ^
						       String.toString (str (!ch)) ^" in \\ ... \\");
							while !ch <> #"\\"  andalso !ch <> #"\"" andalso !ch <> eofChar
							do nextCh()
							);
						nextCh();
						getString soFar
						)
					else getString (next :: #"\\" :: soFar)
				  end

			|	ch => (* Anything else *)
					(
					 nextCh();
					 if ch = eofChar then raise EndOfLine
					 else if Char.isPrint ch (* Ok if it's printable. *)
					 then getString (ch :: soFar)
					 else (* Report unprintable characters. *)
					 	(
						errorMessage (state, lineno state,
							"unprintable character " ^ Char.toString ch ^ " found in string");
						getString soFar
						)
					)
		 )

	  in
	  	nextCh(); (* Skip the opening quote. *)
	  	id := String.implode(List.rev(getString []))
		  handle EndOfLine => 
            errorMessage (state, lineno state,
                          "no matching quote found on this line")

	  end


     (* parseComment deals with nested comments.
        Returns with !ch containing the first character AFTER the comment. *)
     fun parseComment () =
     let
       (* skipComment is called after we've already seen the "(" and "*",
          and returns the first chararacter AFTER the comment. *)
       fun skipComment () : char =
       let
         val startLine : int = lineno state;
         
         (* Returns the first chararacter AFTER the comment *)
		 fun skipCommentBody (firstCh : char) : char =
		   if firstCh = eofChar
		   then 
		   	  (
		       errorMessage (state, lineno state,
				  "end of file found in comment (starts at line " ^
				  Int.toString startLine ^ ")");
			   firstCh
			  )
		   else case (firstCh, stream ()) of
				(#"*", #")") => stream () (* End of comment - return next ch. *)
			|	(#"(", #"*") => skipCommentBody (skipComment ()) (* Nested comment. *)
			|	(_, nextCh) => skipCommentBody nextCh
       in
         skipCommentBody (stream ()) (* Skip the initial "*" *)
       end; (* skipComment *)

     in 
       ch := skipComment ()
     end;
       
     (* Sets "id" and "sy" if an identifier is read.
	Looks up a word to see if it is reserved.   *)
     fun parseIdent charsetTest first (* any characters read so far *) =
     let
       val idVal = readChars charsetTest first;
     in      
	(* Qualified names may involve fields of different lexical form
	   e.g. A.B.+ *)
       if !ch = #"." (* May be qualified *)
       then let
			 val U : unit = nextCh();
			 val c = !ch;
	     	in
			 if isAlphabetic c
			   then parseIdent isAlphaNumeric (idVal ^ ".")
				 
			 else if isOperator c
			   then parseIdent isOperator (idVal ^ ".")
				 
			 else errorMessage (state, lineno state,
				 "invalid identifer - "^ idVal ^ "." ^ str c)
	       end
       else 
	 (
	   id := idVal;
	   sy := (if 0 < size idVal andalso String.str(String.sub(idVal, 0)) = "'"
		  then typeIdent
		  else lookup idVal)
	 )
      end; (* parseIdent *)
      
     fun parseToken () =
       (
	   case skipWhiteSpace () (* remove leading spaces *) of
		  #"~" => (* Either an operator or part of a number. *)
             (
               nextCh();(* get next character *)
               if isNumeric (!ch)
               then
               (
                 (* Read the number and sets sy to integerConst. *)
                 parseNumber();
                 
                 (* Prepend the "~" to the num *)
                 id := "~" ^ !id 
               )
               else
                 (* Part of an operator. *) 
                 parseIdent isOperator "~"
             )

		| #"#" =>(* Either an operator, which include a field selection or
					a character constant.
					N.B. It is not absolutely clear whether any separator
					is allowed between # and the following string constant.
					Assume that it isn't for the moment. *)
			  (
			    nextCh();(* get next character *)
				if !ch = #"\""
				then (parseString (); sy := charConst)
                else
                 (* Part of an operator. *) 
                 parseIdent isOperator "#"
			  )
        
		| #"\"" (* double quote. *) => (parseString (); sy := stringConst)
            
		| #";" => (sy := semicolon; chRead())
            
		| #"," => (sy := comma; chRead())
            
		| #"(" =>
              (
                nextCh();
                if !ch <> #"*" then sy := leftParen else parseComment()
              )
              
		| #")" => (sy := rightParen; chRead())
            
		| #"[" => (sy := leftBrack; chRead())
            
		| #"]" => (sy := rightBrack; chRead())
            
		| #"_" => (sy := underline; chRead())
            
		| #"{" => (sy := leftCurly; chRead())
            
		| #"}" => (sy := rightCurly; chRead())

		| #"." => (* "..." *)
              (
                nextCh();
                if !ch <> #"."
                then errorMessage (state, lineno state,
                        "unknown symbol ." ^ str(!ch))
                else
                (
                  nextCh();
                  if !ch <> #"." 
                  then errorMessage (state, lineno state,
                         "unknown symbol .." ^ str(!ch))
                  else (sy := threeDots; chRead())
                )
              )
              
         | firstCh =>
		 	(* These can't be so easily incorporated into a "case". *)
		 	if firstCh = eofChar
            then sy := abortParse
          
            else if isNumeric firstCh
            then parseNumber()

            else if isAlphabetic firstCh orelse firstCh = #"'"
             then parseIdent isAlphaNumeric ""
          
            else if isOperator firstCh
            (* excludes ~ which has already been done *)
            then parseIdent isOperator ""
            
            else let (* illegal character *)
	            val printableFirstCh = Char.toString firstCh
            in
	            (* Report the character. *)
	            errorMessage (state, lineno state,
	                 "unknown character \"" ^ printableFirstCh ^ "\"");
	            chRead()
            end;
		(* Read another token if this wasn't recognised. *)
        if (!sy eq othersy) then parseToken() else ()
        ); (* parseToken *)

    in
      if (! sy) eq abortParse (* already end-of-file? *)
      then
      (
         errorMessage (state, lineno state, "unexpected end of file encountered");
         raise InternalError "end of file"
      )
      else ();
      
      sy := othersy; (* default - anything unrecognisable *)
      
      parseToken ()
    end; (* insymbol *)

   fun pushBackSymbol ({pushedSym,...}:lexan, sym) =
	if !pushedSym neq othersy then raise InternalError "Attempt to push two parentheses"
	else pushedSym := sym

   (* exported version of sy and id. *)
   
   fun sy ({sy=ref sy, pushedSym = ref pushed, ...}:lexan) =
		if pushed neq othersy then pushed else sy;

   fun id ({id=ref id,...}:lexan) = id;

end (* LEX functor body *);
