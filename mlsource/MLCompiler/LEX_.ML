(*
    Original Poly version:
    Title:      Lexical Analyser.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985

    ML translation and other changes:
    Copyright (c) 2000
        Cambridge University Technical Services Limited
        
    Further development:
    Copyright (c) 2000-7 David C.J. Matthews

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
      pushedSym: sys ref,
      extraChars: char list ref
    };
    
  (* The lexical analyser reads characters from the stream and updates the
     references in the lexan structure.  That's not perhaps very ML-like
     but the lexical analyser can be a hot-spot in the compiler unless it's
     made as fast as possible. *)

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
      pushedSym = ref othersy,
      extraChars = ref []
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
   fun flushLexan ({errors, ch, sy, pushedSym, extraChars, ...} : lexan) = 
     (errors := false;
      ch := #" ";
      sy := semicolon;
      pushedSym := othersy;
      extraChars := []);

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


    exception EndOfLine;
    
    (* "ch" contains the next character in the stream.  extraChars is a hack that is
       needed to deal with a number that looks like it might be a real number
       but actually isn't. *)
    fun nextCh({ch, stream, extraChars = ref [], ...}) = ch := stream()
     |  nextCh({ch, extraChars = extra as ref(c::l), ...}) = (extra := l; ch := c)
 
    and skipWhiteSpace (state as {ch = ref c, ...}:lexan) : char =
      if isWhiteSpace c
      then (nextCh state; skipWhiteSpace state)
      else c

    (* If a character has been read which is its own terminator (e.g.";")
      then don't read the next character, just clobber the current one. (The
      only place this matters is when the user types x;y; at the terminal
      when the compiler is called first to process the x; and then it is
      called again (with reinitialisation) to process the y;.
      Replacing it with a space means that the next character will be read
      from the input stream since leading spaces are skipped.
      Many symbols (e.g. identifiers) are not self-terminating so the
      terminating character is remembered in ch. *)
    and chRead({ch, ...}:lexan) = ch := #" "  (* " " *);
   
    (* Leave string construction until we have all the characters.  Since
       Single character strings are the same as single characters it doesn't
       cost anything to apply "str" but it allows us to conatenate with any
       prefix string in one go. *)
    fun readChars (state as { stream, ch, ... }) (isOk: char -> bool) (s: string) : string = 
    let
        fun loop (): string list =
        let
            val theChar  = ! ch;
        in
            if isOk theChar
            then (nextCh state; str theChar :: loop ())
            else []
        end;
    in
        concat (s :: loop ())
    end;

    (* Read in a number. *)
    fun parseNumber (state as { sy, id, ch, extraChars, ... }) =
     (
        sy := integerConst;
        
        (* Copy digits into the buffer. *)
        id := readChars state isNumeric "";
        
        (* May be the end of an integer, part of a real number,
           or w for word or x for hex. *)
        if !ch = #"w" andalso !id = "0"
        then (* word constant. *)
        (
            sy := wordConst;
            nextCh state;
            if !ch = #"x"
            then
            (
                nextCh state;
                if isHexadecimal (!ch)
                then id := readChars state isHexadecimal "0wx"
                else
                  errorMessage (state, lineno state,
                    "malformed word constant: " ^ !id ^ str(!ch))
            )
            else if isNumeric (!ch)
            then id := readChars state isNumeric "0w"
            else
              errorMessage (state, lineno state,
                "malformed word constant: " ^ !id ^ str(!ch))
        )
        else if !ch = #"x" andalso !id = "0"
        then (* Hexadecimal integer constant. *)
        (
            nextCh state;
            if isHexadecimal (!ch)
            then id := readChars state isHexadecimal "0x"
            else
              errorMessage (state, lineno state,
                "malformed integer constant: " ^ !id ^ str(!ch))
        )
        else if !ch = #"." orelse
                !ch = #"E" orelse !ch = #"e" (* "e" is allowed in ML97 *)
        then (* possible real constant. *)
        (
            if !ch = #"."
            then
            (
               sy := realConst;
               (* Add the "." to the string. *)
               id := !id ^ ".";
               nextCh state;
               (* Must be followed by at least one digit. *)
               if not (isNumeric (!ch))
               then
                  errorMessage (state, lineno state,
                    "malformed real number: " ^ !id ^ str(!ch))
               else id := readChars state isNumeric (!id)
            )
            else ();

            (* There's a nasty here.  We may actually have 1e~; which should
               (probably) be treated as 1 e ~ ; That means that if after we've
               read the e and possible ~ we find that the next character is not
               a digit we return the number read so far and leave the e, ~
               and whatever character we found to be read next time. *)
            if !ch = #"E" orelse !ch = #"e"
            then
            let
                val eChar = !ch
            in
                nextCh state;
               
                (* May be followed by a ~ *)
                (* If it's followed by a digit we have an exponent otherwise
                  we have a number followed by a identifier.  In that case
                  we have to leave the identifier until the next time we're called. *)
                if !ch = #"~"
                then
                (
                    nextCh state;
                    if isNumeric(!ch)
                    then (sy := realConst; id := readChars state isNumeric (!id ^ "E~"))
                    else (extraChars := [#"~", !ch]; ch := eChar)
                )
                else
                (
                    if isNumeric(!ch)
                    then (sy := realConst; id := readChars state isNumeric (!id ^ "E"))
                    else (extraChars := [!ch]; ch := eChar)
                )
            end
            else ()
        )
        else ()
     );

    fun parseString (state as { ch, id, ... }) =
    let
         (* The original version of this simply concatenated the characters onto "id".
            For very long strings that's expensive since each concatenation copies the
            existing string, resulting in quadratic performance.  This version creates a
            list and then implodes it.  DCJM 24/5/02. *)
        fun getString (soFar: char list) =
         (
            case !ch of
                #"\"" (* double-quote. *) => (* Finished - return result. *) (chRead state; soFar)
    
            |   #"\n" => (nextCh state; raise EndOfLine)
    
            |   #"\\" => (* Escape *)
                    let
                        val _ = nextCh state; (* Skip the escape char. *)
                        val next = !ch;   (* Look at the next char. *)
                        val _ = nextCh state;
                    in
                        (* Remove \f...\ sequences but otherwise leave the string
                           as it is.  Escape sequences are processed in the conversion
                           function.  In particular we can only decide whether \uxxxx
                           is valid when we know whether we are converting to Ascii or
                           Unicode. *)
                    if isWhiteSpace next
                    then
                        (
                        if skipWhiteSpace state = #"\\" then ()
                        else
                            (
                            errorMessage (state, lineno state,
                               "unexpected character " ^
                               String.toString (str (!ch)) ^" in \\ ... \\");
                            while !ch <> #"\\"  andalso !ch <> #"\"" andalso !ch <> eofChar
                            do nextCh state
                            );
                        nextCh state;
                        getString soFar
                        )
            else if next = #"^" (* \^c escape sequence for Control+c *)
            then    let
                    val next2 = !ch;
                    val _ = nextCh state;
                in  getString (next2 :: #"^" :: #"\\" :: soFar)
                end
            else getString (next :: #"\\" :: soFar)
                  end
    
            |   ch => (* Anything else *)
                    (
                     nextCh state;
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
        nextCh state; (* Skip the opening quote. *)

        id := String.implode(List.rev(getString []))
            handle EndOfLine => 
                errorMessage (state, lineno state,
                      "no matching quote found on this line")

    end (* parseString *)


    (* parseComment deals with nested comments.
       Returns with !ch containing the first character AFTER the comment. *)
    fun parseComment (state as { stream, ch, ... }) =
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
            |   (#"(", #"*") => skipCommentBody (skipComment ()) (* Nested comment. *)
            |   (_, nextCh) => skipCommentBody nextCh
       in
         skipCommentBody (stream ()) (* Skip the initial "*" *)
       end; (* skipComment *)

    in 
        ch := skipComment ()
    end (* parseComment *);


    (* Sets "id" and "sy" if an identifier is read.
        Looks up a word to see if it is reserved.   *)
    fun parseIdent (state as { ch, id, sy, ... }) charsetTest first (* any characters read so far *) =
    let
        val idVal = readChars state charsetTest first;
    in      
    (* Qualified names may involve fields of different lexical form
       e.g. A.B.+ *)
        if !ch = #"." (* May be qualified *)
        then
        let
            val () = nextCh state;
            val c = !ch;
        in
             if isAlphabetic c
               then parseIdent state isAlphaNumeric (idVal ^ ".")
                 
             else if isOperator c
               then parseIdent state isOperator (idVal ^ ".")
                 
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


    (* Main lexical analyser loop. *)
    fun parseToken (state as { ch, id, sy, ... }) =
       (
       case skipWhiteSpace state (* remove leading spaces *) of
          #"~" => (* Either an operator or part of a number. *)
             (
               nextCh state;(* get next character *)
               if isNumeric (!ch)
               then
               (
                 (* Read the number and sets sy to integerConst. *)
                 parseNumber state;
                 
                 (* Prepend the "~" to the num *)
                 id := "~" ^ !id 
               )
               else
                 (* Part of an operator. *) 
                 parseIdent state isOperator "~"
             )

        | #"#" =>(* Either an operator, which include a field selection or
                    a character constant.
                    N.B. It is not absolutely clear whether any separator
                    is allowed between # and the following string constant.
                    Assume that it isn't for the moment. *)
              (
                nextCh state;(* get next character *)
                if !ch = #"\""
                then (parseString state; sy := charConst)
                else
                 (* Part of an operator. *) 
                 parseIdent state isOperator "#"
              )
        
        | #"\"" (* double quote. *) => (parseString state; sy := stringConst)
            
        | #";" => (sy := semicolon; chRead state)
            
        | #"," => (sy := comma; chRead state)
            
        | #"(" =>
              (
                nextCh state;
                if !ch <> #"*" then sy := leftParen else parseComment state
              )
              
        | #")" => (sy := rightParen; chRead state)
            
        | #"[" => (sy := leftBrack; chRead state)
            
        | #"]" => (sy := rightBrack; chRead state)
            
        | #"_" => (sy := underline; chRead state)
            
        | #"{" => (sy := leftCurly; chRead state)
            
        | #"}" => (sy := rightCurly; chRead state)

        | #"." => (* "..." *)
              (
                nextCh state;
                if !ch <> #"."
                then errorMessage (state, lineno state,
                        "unknown symbol ." ^ str(!ch))
                else
                (
                  nextCh state;
                  if !ch <> #"." 
                  then errorMessage (state, lineno state,
                         "unknown symbol .." ^ str(!ch))
                  else (sy := threeDots; chRead state)
                )
              )
              
         | firstCh =>
            (* These can't be so easily incorporated into a "case". *)
            if firstCh = eofChar
            then sy := abortParse
          
            else if isNumeric firstCh
            then parseNumber state

            else if isAlphabetic firstCh orelse firstCh = #"'"
            then parseIdent state isAlphaNumeric ""
          
            else if isOperator firstCh
            (* excludes ~ which has already been done *)
            then parseIdent state isOperator ""
            
            else let (* illegal character *)
                val printableFirstCh = Char.toString firstCh
            in
                (* Report the character. *)
                errorMessage (state, lineno state,
                     "unknown character \"" ^ printableFirstCh ^ "\"");
                chRead state
            end;
        (* Read another token if this wasn't recognised. *)
        if (!sy eq othersy) then parseToken state else ()
        ); (* parseToken *)

    (* Insymbol - exported interface to lexical analyser. *)
    fun insymbol (state as {sy,pushedSym,...}:lexan) =
    if ! pushedSym neq othersy then pushedSym := othersy
    (* pushedSym is a hack to handle the difficulty of parsing
       val ('a, 'b) f = ... compared with val (a, b) = ... and the
       similar fun declarations. 
       It's also used to handle where type t = int and type ... compared
       with  where type t = int and S = sig ...*)
    else
    (
        if (! sy) eq abortParse (* already end-of-file? *)
        then
        (
             errorMessage (state, lineno state, "unexpected end of file encountered");
             raise InternalError "end of file"
        )
        else ();
      
        sy := othersy; (* default - anything unrecognisable *)
      
        parseToken state
    ); (* insymbol *)

    fun pushBackSymbol ({pushedSym,...}:lexan, sym) =
        if !pushedSym neq othersy then raise InternalError "Attempt to push two parentheses"
        else pushedSym := sym

   (* exported version of sy and id. *)
   
   fun sy ({sy=ref sy, pushedSym = ref pushed, ...}:lexan) =
        if pushed neq othersy then pushed else sy;

   fun id ({id=ref id,...}:lexan) = id;

end (* LEX functor body *);
