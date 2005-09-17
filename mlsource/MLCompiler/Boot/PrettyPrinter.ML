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
    Title:      PrettyPrinter.ML - extracted from Bootstrap File.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1986
*)



(* Based on the paper by D.C Oppen in ACM ToPLAS Vol. 2 No. 4 Oct 1980 *)

(* N.B. There is a known bug in Oppen's algorithm. Although he claims that
   it works in constant space, this isn't true - for highly nested block
   structures the scan stack (or is it the token queue?) can overflow.
   This doesn't appear to happen for the ML pretty-printer, but it did
   occur for one version of ICL's ProofPower pretty-printer which also
   uses this interface. One day we'll have to fix the algorithm using
   stretch arrays (but it's too tricky to do it now).       SPF 16/8/94 *) 

(* HACKS: refers to Misc.InternalError *)

structure PrettyPrinter :

(*****************************************************************************)
(*                  PrettyPrinter output signature                           *)
(*****************************************************************************)
sig
  type prettyPrinter 
  
  val ppAddString  : prettyPrinter -> string -> unit
  val ppBeginBlock : prettyPrinter -> int * bool -> unit
  val ppEndBlock   : prettyPrinter -> unit -> unit
  val ppBreak      : prettyPrinter -> int * int -> unit
  val ppEliding    : prettyPrinter -> unit -> bool
  val ppEndStream  : prettyPrinter -> unit -> unit
  val ppLineBreak  : prettyPrinter -> unit -> unit
  
  val prettyPrint : int * (string -> unit) -> prettyPrinter; 
  val uglyPrint   : (string -> unit) -> prettyPrinter; 
end =

(*****************************************************************************)
(*                  PrettyPrinter structure body                             *)
(*****************************************************************************)
struct
  exception InternalError = Misc.InternalError;

  type prettyPrinter =
    { 
      addString:  string -> unit,       (* Put in a string *)
      beginBlock: int * bool -> unit,   (* Begin a block. (indent for block,
                                           true if lines must be
                                           consistently broken). *)
      endBlock:   unit -> unit,         (* End of a block. *)
      break:      int * int -> unit,    (* A break - (number of spaces,
                                                     offset for overflow) *)
      eliding:    unit -> bool,         (* True if this block is not being
                                           displayed. *)
      endStream:  unit -> unit,         (* End of stream. *)
      lineBreak:  unit -> unit          (* Force a break. *)
    } 

  val ppAddString  : prettyPrinter -> string -> unit      = #addString;
  val ppBeginBlock : prettyPrinter -> int * bool -> unit  = #beginBlock;
  val ppEndBlock   : prettyPrinter -> unit -> unit        = #endBlock;
  val ppBreak      : prettyPrinter -> int * int -> unit   = #break;
  val ppEliding    : prettyPrinter -> unit -> bool        = #eliding;
  val ppEndStream  : prettyPrinter -> unit -> unit        = #endStream;
  val ppLineBreak  : prettyPrinter -> unit -> unit        = #lineBreak;
  
  fun uglyPrint (stream : string -> unit) : prettyPrinter =
  let
    fun break (n, x) = if n <= 0 then () else (stream " "; break (n - 1, x));
  in  
    { 
      addString  = stream,
      beginBlock = fn (i : int, b : bool) => (),
      endBlock   = fn () => (),
      break      = break,
      eliding    = fn () => true,
      endStream  = fn () => (),
      lineBreak  = fn () => ()
    }
  end;

  fun  prettyPrint (lineWidth : int, stream : string -> unit) : prettyPrinter =
  let
    val space = ref lineWidth;  (* Space left on line. *)
  
(*****************************************************************************)
(*                  Tokens                                                   *)
(*****************************************************************************)  
    (* These tokens represent the different kinds of objects
       present in the stream. *)
  
    datatype token =
      String of string
    | Break  of {blanks: int, offset: int}
    | Begin  of {offset: int, consistent: bool}
    | End
    ;
  
(*****************************************************************************)
(*                  Print Stack                                              *)
(*****************************************************************************) 
  local (* only used in printOut *)

    (* An entry on the stack is either "fits" if it fits on the line, or
	consistent or inconsistent in which case a new line will be necessary
	and the value is the indentation. *)
    datatype printStackEntry =
       Fits
     | Consistent of int
     | Inconsistent of int;
    
    val printStack : printStackEntry list ref = ref [];
  
    fun pushPrintStack (x : printStackEntry) : unit =
      printStack := x :: !printStack;
      
    fun popPrintStack () : unit =
      case !printStack of
	[]      => raise InternalError "popPrintStack: print stack empty"
      | (x::xs) => printStack := xs
      ;
      
    val indentation = ref 0; (* Current indentation *)
  in  
     (* Entries are made on the print stack for each "begin" and
       removed at the corresponding "end". The top item is inspected
       on each "break" to decide whether to put in a new line. *)
    fun printOut (Break {blanks, offset}, breakSize :int) : unit =
     (* size of the blank and the next string/block *)
     
   (* Printing out a blank may either involve just printing the required
      number of spaces or it may involve breaking the line. We first look
      at the top of the stack which shows the requirements for this block.
      If the whole block fits then there is no problem, this blank and its
      following string/block will definitely fit. If the block does not
      fit we break the line if it is a consistently breaking block, or if
      the blank and its following string/block will not fit. *)

      let
        (* Indent by a given number of characters. *)
        fun indent n =
          if n <= 0 then () else (stream " "; indent (n - 1));
          
        (* All this block fits or this line fits. *)
        fun useOldLine () =
          (
           (* Just print out the required number of spaces *)
           space := !space - blanks;
           indent blanks
          )

        (* Have to put in a new line. *)
        fun useNewLine n =
          (
           (* Add any indentation for the break, and make this the
              current indentation. *)
            indentation := offset + n;
            space := lineWidth - !indentation;
            stream "\n"; (* new line *)
            indent (!indentation)
          )
      in
        case !printStack of
          [] => (* no previous block - this case added 7/10/94 SPF *)
            useNewLine 0 (* use new line SPF 12/5/95 *)
        
        | (Fits :: _) =>
            useOldLine ()
         
        | (Consistent n :: _)=>
            useNewLine n
        
        | (Inconsistent n :: _) =>
           if breakSize <= !space
           then useOldLine ()
           else useNewLine n
      end (* Break clause of printOut *)

    | printOut (Begin {offset, consistent}, breakSize :int) : unit =
  
      (* Push an entry onto the stack. *)
      (* If the whole block fits then use a "fits" entry otherwise
         either a "consistent" or "inconsistent" with the identation. *)
      let
        val stackEntry =
          if breakSize <= !space then Fits
          else
            (
              (* Add this offset to the current indentation *)
              indentation := !indentation + offset;
              
              (* Set indentation of block to this *)
              if consistent
              then Consistent (!indentation)
              else Inconsistent (!indentation)
            );
      in
        pushPrintStack stackEntry
      end (* Begin clause of printOut *)
  
    | printOut (String s, breakSize :int) : unit =
       (
        if breakSize > !space then space := 0 else space := !space - breakSize;
        stream s
       )
  
    | printOut (End, breakSize :int) : unit =
       (
        popPrintStack () (* remove the "begin" *)
       )
  
  end; (* scope of printStack *)
   
(*****************************************************************************)
(*                  Token queue                                              *)
(*****************************************************************************)
  
    (* Tokens are held on a token queue which remembers the token and the
      computed sizes. They are put on the queue by "enQueue" and removed by
      "deQueue". The sizes of begin-end blocks and of blanks are initially
      set to a negative number to indicate "unknown" and are then filled in
      later by setTokenSize. In the case of "begins" and "blanks" the
      number used is -rightTotal so that adding in rightTotal later will
      give the number of characters in the block or immediately after the
      blank. Objects whose sizes are unknown have their index pushed onto
      the scanStack so that they can be fixed up later. As soon as the
      first (leftmost) token has a positive size it, and any following
      tokens with positive sizes can be printed. *)
  
    local
      val vecSize = 3 * lineWidth;
      fun inc n = (n + 1) mod vecSize;
      fun dec n = (n - 1) mod vecSize;
  
      val left  = ref 0;
      val right = ref 0;
    in
      (* visible refs (yeuch) *)
      val rightTotal = ref 0;
      val leftTotal  = ref 0;

      (* queue contains the tokens and their sizes. *)
      val queue = Array.array (vecSize, (End,0));


      fun queueRight () : int = 
        !right;

      fun queueEmpty () : bool = 
        !right = !left;

      (* Add to the queue. *)
      fun enQueue (t: token, s : int, len: int) : unit= 
        (
          rightTotal := !rightTotal + len;
          
          right := inc (!right);
          
          if queueEmpty ()
          then raise InternalError "token queue full"
          else Array.update (queue, !right, (t,s))
        );
  
     (* Remove from the queue. *)
     fun deQueue () =
       (* Print objects from the token queue until we either exhaust it
          or we find something whose length we don't know. *)
       let
         val nextLeft = inc (!left);
         val (token, size) = Array.sub (queue, nextLeft);
       in
          if size >= 0 andalso not (queueEmpty ())
          then 
           (
            left := nextLeft;
             
            printOut (token, size);
             
            case token of
              Break {blanks, ...} =>
                leftTotal := !leftTotal + blanks
            | String _ =>
                leftTotal := !leftTotal + size
            | _ => ()
            ;
             
            deQueue()
          )
         else ()
       end  (* deQueue*);
  
      (* Reset the queue. *)
      fun clearQueue ()=  
        (             
          left       := 0;
          right      := 0;
          leftTotal  := 0;
          rightTotal := 0
        );
  
    end (* token queue functions *);
  

(*****************************************************************************)
(*                  Scan stack                                               *)
(*****************************************************************************)
  
    (* The scan stack contains pointers into the token queue. It behaves like
      a stack except that entries may be removed from the bottom as well. *)
  
    (* Indices of tokens whose sizes are unknown are pushed onto this stack
      and removed, by setTokenSize, when the size becomes known. If the
      line becomes too full tokens may be forcibly removed from the bottom by
      setting their sizes to infinity (addstring). *)
  
    local
      val vecSize = 3 * lineWidth;
      fun inc n = (n + 1) mod vecSize;
      fun dec n = (n - 1) mod vecSize;

      val vec     = Array.array (vecSize, 0);
      val top     = ref 0;
      val bottom  = ref 0;
    in  
       (* Usual stack functions. *)
       fun scanEmpty () : bool =
         !top = !bottom;
  
       fun scanTop () : int =
         if scanEmpty () then
         raise InternalError "PrettyPrinter.scanTop: stack empty"
         else Array.sub (vec, !top);
  
       fun scanPush (x : int) : unit =
         (
           top := inc (!top);
           if scanEmpty ()
           then raise InternalError "PrettyPrinter.scanPush: stack full"
           else Array.update (vec, !top, x)
         );
  
       fun scanPop () : int =
         if scanEmpty ()
         then raise InternalError "scanPop: stack empty"
         else let
           val result = Array.sub (vec, !top);
         in
           top := dec (!top);
           result
         end;
  
       fun scanPopBottom () : int =  (* Remove from the bottom *)
         if scanEmpty ()
         then raise InternalError "PrettyPrinter.scanPopBottom: stack empty"
         else
           (
             bottom := inc (!bottom);
             Array.sub (vec, !bottom)
           );
      end (* scan stack functions *);
  

(*****************************************************************************)
(*                  setTokenSize                                             *)
(*****************************************************************************)
  
       fun setTokenSize () =
       (* Sets the size of the last object or block on the stack. *)
        let
          fun adjustSize (index:int) =
          let
            val (token,size) = Array.sub (queue, index)
            val newSize = size + !rightTotal
          in
            Array.update (queue, index, (token,newSize))
          end;
 
          fun isBegin (Begin _) = true
            | isBegin _  = false
    
          fun topNotBegin () : bool =
            (
              if scanEmpty () (* scan stack *)
              then false
              else not (isBegin (#1 (Array.sub (queue, scanTop ()))))
            );
        in
          if topNotBegin ()
          then let
            val index = scanPop ()
          in
            case Array.sub (queue, index) of
              (* If it was an "end" then set the sizes of everything
                 back to the corresponding "begin". *)
              (End,size) =>
               (
                Array.update(queue, index, (End,1)); (* set size of "end" *)
            
                while topNotBegin ()     (* set sizes until the "begin" *)
                  do setTokenSize ();
                
                (* should now be at a "begin" *)
                if not (scanEmpty ()) (* set its size *)
                then adjustSize (scanPop ())
                else ();
                   
                setTokenSize () (* Process any preceeding blank. *)
              )
                
           | _ =>
             (* blank (strings aren't put on the stack). *)
             adjustSize index
          end
          else ()
       end;
  
  

(*****************************************************************************)
(*                  The interface procedures                                 *)
(*****************************************************************************)
  
    fun addString (st: string) : unit =
    let 
      val strLength = size st;
    in
      if scanEmpty ()
      then printOut (String st, strLength)
      else
        (
          (* Put the string on the queue. *)
          enQueue (String st, strLength, strLength);
          
          (* If there is no longer enough space on the
             line force out some tokens. *)
          while (
             not (queueEmpty ()) andalso
             !rightTotal - !leftTotal > !space andalso
             not (scanEmpty())
            )
          do
            let
              val index = scanPopBottom ();
              val (token,_) = Array.sub (queue,index);
            in
              Array.update (queue, index, (token, 999 (*infinity*)));
              deQueue()
            end
         )
    end (* addstring *);
  
  
    fun break (blanks : int, offset: int) : unit =
      (
        if scanEmpty()
        then clearQueue()
        else ();
          
        (* set the size of any previous block or break. *)
        setTokenSize(); 
         
        enQueue (Break {blanks = blanks, offset = offset},
                  ~ (!rightTotal), blanks);
         
        scanPush (queueRight ())
      );
  
  
    fun lineBreak () : unit =
      break (lineWidth + 1, 0);
  
    fun endStream () : unit =
      ( 
        lineBreak ();
        
        setTokenSize ();
          
        deQueue ()
      );
  
    val depth = ref 0; (* count of begins - ends *)

    fun beginBlock (offset : int, consistent : bool) : unit =
      (
        depth := !depth + 1;
         
        if scanEmpty ()
        then clearQueue ()
        else ();
          
        enQueue (Begin {offset = offset, consistent = consistent},
                  ~ (!rightTotal), 0);
         
        scanPush (queueRight ())
      );
  
    fun endBlock () : unit =
      (
        if scanEmpty ()
        then printOut (End, 0)
        else
          (
            enQueue (End, ~1, 0);
            scanPush (queueRight ())
          );
            
        depth := !depth - 1;
          
        (* Force out anything remaining *)
        if !depth = 0         
        then endStream ()
        else ()
      );

     fun eliding () : bool =
       false (* Never eliding *)
  
   in (* body of prettyPrint *)
     { 
        addString  = addString,
        beginBlock = beginBlock,
        endBlock   = endBlock,
        break      = break,
        eliding    = eliding,
        endStream  = endStream,
        lineBreak  = lineBreak
     } 
  end; (* prettyPrint *)
end (* PrettyPrinter *);


