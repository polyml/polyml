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
    Title:      Lexical symbols for ML compiler.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

structure Symbols :
sig
  type sys;
  
  val abortParse:   sys;
  val ident:        sys;
  val abstypeSy:    sys;
  val andSy:        sys; 
  val andalsoSy:    sys; 
  val asSy:         sys; 
  val caseSy:       sys;
  val datatypeSy:   sys; 
  val doSy:         sys; 
  val elseSy:       sys; 
  val endSy:        sys; 
  val exceptionSy:  sys; 
  val fnSy:         sys; 
  val funSy:        sys;
  val handleSy:     sys; 
  val ifSy:         sys; 
  val inSy:         sys; 
  val infixSy:      sys; 
  val infixrSy:     sys; 
  val letSy:        sys; 
  val localSy:      sys;
  val nonfixSy:     sys; 
  val ofSy:         sys; 
  val opSy:         sys; 
  val openSy:       sys; 
  val orelseSy:     sys; 
  val raiseSy:      sys; 
  val recSy:        sys; 
  val thenSy:       sys;
  val typeSy:       sys; 
  val valSy:        sys; 
  val withSy:       sys; 
  val whileSy:      sys; 
  val leftParen:    sys; 
  val rightParen:   sys; 
  val leftBrack:    sys;
  val rightBrack:   sys; 
  val comma:        sys; 
  val colon:        sys; 
  val semicolon:    sys; 
  val thickArrow:   sys; 
  val verticalBar:  sys;
  val equalsSign:   sys; 
  val underline:    sys; 
  val typeIdent:    sys; 
  val stringConst:  sys;
  val integerConst: sys; 
  val asterisk:     sys; 
  val arrow:        sys; 
  val realConst:    sys; 
  val leftCurly:    sys;
  val rightCurly:   sys; 
  val dot:          sys; 
  val threeDots:    sys;
  val colonGt:      sys;
  val hashSign:     sys; 
  val structureSy:  sys; 
  val signatureSy:  sys;
  val structSy:     sys; 
  val sigSy:        sys; 
  val sharingSy:    sys; 
  val functorSy:    sys; 
  val withtypeSy:   sys; 
  val eqtypeSy:     sys;
  val includeSy:    sys;
  val whereSy:      sys;
  val wordConst:    sys; 
  val charConst:    sys; 
  val othersy:      sys;
  val maxsym:       sys;
  
  val repr: sys -> string;

  val lookup: string -> sys;
  
  val eq:   sys * sys -> bool;
  val neq:  sys * sys -> bool;
  val down: sys -> int;

end =

struct
  (* The symbol declarations for the compiler *)
  abstype sys = sys of int
  with
    val abortParse     = sys 0;
    val ident          = sys 1;
   (* Reserved words *)
    val abstypeSy      = sys 2;
    val andSy          = sys 3;
    val andalsoSy      = sys 4;
    val asSy           = sys 5;
    val caseSy         = sys 6;
    val datatypeSy     = sys 7;
    val doSy           = sys 8;
    val elseSy         = sys 9;
    val endSy          = sys 10;
    val exceptionSy    = sys 11;
    val fnSy           = sys 12;
    val funSy          = sys 13;
    val handleSy       = sys 14;
    val ifSy           = sys 15;
    val inSy           = sys 16;
    val infixSy        = sys 17;
    val infixrSy       = sys 18;
    val letSy          = sys 19;
    val localSy        = sys 20;
    val nonfixSy       = sys 21;
    val ofSy           = sys 22;
    val opSy           = sys 23;
    val openSy         = sys 24;
    val orelseSy       = sys 25;
    val raiseSy        = sys 26;
    val recSy          = sys 27;
    val thenSy         = sys 28;
    val typeSy         = sys 29;
    val valSy          = sys 30;
    val withSy         = sys 31;
    val whileSy        = sys 32;
    val structureSy    = sys 33;
    val signatureSy    = sys 34;
    val structSy       = sys 35;
    val sigSy          = sys 36;
    val sharingSy      = sys 37;
    val functorSy      = sys 38;
    val withtypeSy     = sys 39;
    val eqtypeSy       = sys 40;
    val includeSy      = sys 41;
    val whereSy        = sys 42;
    (* Reserved operators *)
    val hashSign       = sys 43;
    val verticalBar    = sys 44;
    val equalsSign     = sys 45;
    val thickArrow     = sys 46;
    val colon          = sys 47;
    val asterisk       = sys 48;
    val arrow          = sys 49;
    val colonGt        = sys 50;
    (* Special symbols *)
    val underline      = sys 51;
    val leftParen      = sys 52;
    val rightParen     = sys 53;
    val leftBrack      = sys 54;
    val rightBrack     = sys 55;
    val comma          = sys 56;
    val semicolon      = sys 57;
    val leftCurly      = sys 58;
    val rightCurly     = sys 59;
    val dot            = sys 60;
    val threeDots      = sys 61;
    (* Others *)
    val typeIdent      = sys 62;
    val stringConst    = sys 63;
    val integerConst   = sys 64;
    val realConst      = sys 65;
    val wordConst      = sys 66;
    val charConst      = sys 67;
    val othersy        = sys 68;
    val maxsym         = othersy;

    fun down (sys n) = n;
    fun succ (sys n) = sys (n+1);
    
    infix 7 eq neq leq;
    
    fun a eq  b = (down a =  down b);
    fun a leq b = (down a <= down b);
    fun a neq b = (down a <> down b);
    
    open Vector; infix 8 sub;
    
    val resw = fromList ["\^G",
                       "<identifier>",
                       "abstype",
                       "and",
                       "andalso",
                       "as",
                       "case",
                       "datatype",
                       "do",
                       "else",
                       "end",
                       "exception",
                       "fn",
                       "fun",
                       "handle",
                       "if",
                       "in",
                       "infix",
                       "infixr",
                       "let",
                       "local",
                       "nonfix",
                       "of",
                       "op",
                       "open",
                       "orelse",
                       "raise",
                       "rec",
                       "then",
                       "type",
                       "val",
                       "with",
                       "while",
                       "structure",
                       "signature",
                       "struct",
                       "sig",
                       "sharing",
                       "functor",
                       "withtype",
                       "eqtype",
                       "include",
                       "where",
                       "#",
                       "|",
                       "=",
                       "=>",
                       ":",
                       "*",
                       "->",
                       ":>",
                       "_",
                       "(",
                       ")",
                       "[",
                       "]",
                       ",",
                       ";",
                       "{",
                       "}",
                       ".",
                       "...",
                       "<type identifier>",
                       "<string constant>",
                       "<integer constant>",
                       "<real constant>",
                       "<word constant>",
                       "<char constant>",
                       "unknown symbol"];

    fun repr sym = resw sub (down sym);

    local
      open Boot.HashTable;
      open Boot.Misc;

      (* Make table sparse to minimise collisions. SPF 11/5/95 *)
      val table: sys hash = hashMake (4 * down maxsym);
      
      (* Enter each reserved word in the hash table *)
      val () = 
        forsucc succ (op leq) abstypeSy colonGt
                      (fn sym => hashSet (table, repr sym, sym));
    in
      (* Returns the symbol if it is in the table else ident *)
      fun lookup str = getOpt(hashSub (table, str), ident);
    end;

  end (* with *);

end (* Symbols *);
