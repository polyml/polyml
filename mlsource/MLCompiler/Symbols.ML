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

structure Symbols :>
sig
  eqtype sys;
  
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

end =

struct
    type sys = int
  (* The symbol declarations for the compiler *)

    val abortParse     = 0;
    val ident          = 1;
   (* Reserved words *)
    val abstypeSy      = 2;
    val andSy          = 3;
    val andalsoSy      = 4;
    val asSy           = 5;
    val caseSy         = 6;
    val datatypeSy     = 7;
    val doSy           = 8;
    val elseSy         = 9;
    val endSy          = 10;
    val exceptionSy    = 11;
    val fnSy           = 12;
    val funSy          = 13;
    val handleSy       = 14;
    val ifSy           = 15;
    val inSy           = 16;
    val infixSy        = 17;
    val infixrSy       = 18;
    val letSy          = 19;
    val localSy        = 20;
    val nonfixSy       = 21;
    val ofSy           = 22;
    val opSy           = 23;
    val openSy         = 24;
    val orelseSy       = 25;
    val raiseSy        = 26;
    val recSy          = 27;
    val thenSy         = 28;
    val typeSy         = 29;
    val valSy          = 30;
    val withSy         = 31;
    val whileSy        = 32;
    val structureSy    = 33;
    val signatureSy    = 34;
    val structSy       = 35;
    val sigSy          = 36;
    val sharingSy      = 37;
    val functorSy      = 38;
    val withtypeSy     = 39;
    val eqtypeSy       = 40;
    val includeSy      = 41;
    val whereSy        = 42;
    (* Reserved operators *)
    val hashSign       = 43;
    val verticalBar    = 44;
    val equalsSign     = 45;
    val thickArrow     = 46;
    val colon          = 47;
    val asterisk       = 48;
    val arrow          = 49;
    val colonGt        = 50;
    (* Special symbols *)
    val underline      = 51;
    val leftParen      = 52;
    val rightParen     = 53;
    val leftBrack      = 54;
    val rightBrack     = 55;
    val comma          = 56;
    val semicolon      = 57;
    val leftCurly      = 58;
    val rightCurly     = 59;
    val dot            = 60;
    val threeDots      = 61;
    (* Others *)
    val typeIdent      = 62;
    val stringConst    = 63;
    val integerConst   = 64;
    val realConst      = 65;
    val wordConst      = 66;
    val charConst      = 67;
    val othersy        = 68;
    val maxsym         = othersy;
    
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

    fun repr sym = resw sub sym

    local
        open Boot.HashTable;
        open Boot.Misc;

        (* Make table sparse to minimise collisions. SPF 11/5/95 *)
        val table: int hash = hashMake (4 * maxsym);

        fun enter n =
            if n > colonGt then ()
            else (hashSet (table, repr n, n); enter (n+1))

        (* Enter each reserved word in the hash table *)
        val () = enter abstypeSy
    in
        (* Returns the symbol if it is in the table else ident *)
        fun lookup str = getOpt(hashSub (table, str), ident);
    end;

end (* Symbols *);
