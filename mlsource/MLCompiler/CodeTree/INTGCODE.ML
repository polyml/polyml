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
    Title:      Generate interpretable code for Poly system from the code tree.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

(* This code-generator is primarily intended as a porting aid from one
   machine-code to another. It is modelled on the VAX code-generator and has
   a similar structure. Note: It does not use the use-count information on
   declarations. *)
functor INTGCODE (

(*****************************************************************************)
(*                  CODECONS                                                 *)
(*****************************************************************************)
structure CODECONS :
sig
  type word;
  type address;
  type code;
  type opcode;
  eqtype addrs; (*hacky! *)
  type labels;
  
  val noJump: labels;
  
  val jumpFalse  : opcode;
  val jump       : opcode;
  val setHandler : opcode;
  val delHandler : opcode;
  
  val addrPlus  : addrs * int -> addrs;
  val addrMinus : addrs * addrs -> int;
  
  val codeCreate: bool * string -> code;  (* makes the initial segment. *)
      
  (* ic - Address for the next instruction in the segment. *)
  val ic: code -> addrs;
      
  (* putBytes : puts "length" bytes of "val" into locations "addr", "addr"+1 *)
  val putBytes : int * int * addrs * code -> unit;    
  
  (* GEN- routines all put a value at the instruction counter and add
     an appropriate amount to it. *)

   (* genWord - Puts 2 bytes. *)
   val genWord : int * code -> unit;
      
   (* gen... - put instructions and their operands. *)
   val genCallClosure : code -> unit;
   val genRaiseEx     : code -> unit;
   val genLock        : code -> unit;
   val genLdexc       : code -> unit;
   val genPushHandler : code -> unit;
      
   val genReturn      : int * code -> unit;
   val genGetStore    : int * code -> unit;
   val genLocal       : int * code -> unit;
   val genIndirect    : int * code -> unit;
   val genMoveToVec   : int * code -> unit;
   val genSetStackVal : int * code -> unit;
   val genCase        : int * code -> unit;
   val genTuple       : int * code -> unit;
   
   val genTailCall    : int * int * code -> unit;
   val genNonLocal    : int * int * int * code -> unit;

   (* genEnter instructions are only needed when machine-code routines
      can call interpreted routines or vice-versa. The enterInt instruction
      causes the interpreter to be entered and the argument indicates the
      reason. *)
      
   val genEnterIntCatch : code -> unit;
   val genEnterIntProc  : code * int -> unit;
   val genEnterIntCall  : code * int -> unit;
      
   (* pushConst - Generates code to push a constant. *)
   val pushConst        : word * code -> unit;

   (* genCallSl - Generate callSl instructions which refer to either
       constants or are forward references to procedures which have not yet
       been compiled. *)
   val genCallSl  : int * int * code * code -> unit;
       
   (* genRecRef - Recursive reference to a procedure. *)
   val genRecRef: code * code -> unit

   (* Create a container on the stack *)
   val genContainer : int * code -> unit;

   (* Copy a tuple into a container. *)
   val genSetContainer : int * code -> unit;
 
   (* Create a tuple from a container. *)
   val genTupleFromContainer : int * code -> unit;
     
   (* copyCode - Finish up after compiling a procedure. *)
   val copyCode : code -> address;
   
   (* getBytes - gets "length" bytes from locations "addr", "addr"+1...
      Returns a negative number if the first bit was set. *)
   val getBytes: int * addrs * code -> int;

   (* putBranchInstruction puts in an instruction which involves
      a forward reference. *)
   val putBranchInstruction: opcode * code -> labels;
   
   (* Instruction to delete a handler and skip round it. *)
   val fixup: labels * code -> unit; (* Fix up a forward reference. *)
   
   val linkLabels: labels * labels * code -> labels; (* Link label lists. *)
   val jumpback: addrs * code -> unit; (* Backwards jump. *)
   val resetStack: int * bool * code -> unit; (* Set a pending reset *)
   val alignOffWord: code * int -> unit; (* Add a pad byte if the value would
                                            be word-aligned. *)
end (* CODECONS *);

(*****************************************************************************)
(*                  DEBUGSWITCH                                              *)
(*****************************************************************************)
structure DEBUGSWITCH :
sig
  val maxInlineSize: int ref
end (* DEBUGSWITCH *);

(*****************************************************************************)
(*                  PRETTYPRINTER                                            *)
(*****************************************************************************)
structure PRETTYPRINTER :
sig
  type prettyPrinter 
  
  val ppAddString  : prettyPrinter -> string -> unit
  val ppBeginBlock : prettyPrinter -> int * bool -> unit
  val ppEndBlock   : prettyPrinter -> unit -> unit
  val ppBreak      : prettyPrinter -> int * int -> unit
end;

(*****************************************************************************)
(*                  MISC                                                     *)
(*****************************************************************************)
structure MISC :
sig
  exception InternalError of string;
end;

(*****************************************************************************)
(*                  RUNCALL                                                  *)
(*****************************************************************************)
(* This should be integrated with ADDRESS to form a low-level interface *)
structure RUNCALL :
sig
  val run_call1 : int -> 'a -> 'b
end;

(* DCJM 26/9/00.  Previously Address was a global but we aren't allowed
   to have sharing constraints with globals in ML97.  We could use a
   "where type" constraint but then we couldn't bootstrap from ML90. *)
(*****************************************************************************)
(*                  ADDRESS                                                  *)
(*****************************************************************************)
structure ADDRESS :
sig
  type word;  (* NB *not* an eqtype *)
  type short;
  type address;
  
  val wordEq:  'a * 'a -> bool;
  val isShort: 'a -> bool;
  
  val unsafeCast : 'a -> 'b;

  val toWord:   'a  -> word;
  val toInt:    short -> int;
  val toShort:  'a -> short;
  val toAddress: word -> address;

  val loadByte:  (address * short) -> short;
  val loadWord:  address * short -> word
  val flags:     address -> short;
  val length:    address -> short;
  
  val F_words:   short;
  val F_bytes :  short;
  val F_mutable: short;
  
  val Or:        short * short -> short
  val And:       short * short -> short; 

  val alloc:     short * short * word -> address
  
  val isCode :   address -> bool

  val call: address * word -> word
end


(*****************************************************************************)
(*                  GCODE sharing constraints                                *)
(*****************************************************************************)
sharing type
  ADDRESS.word
= CODECONS.word

sharing type
  ADDRESS.address
= CODECONS.address

) :  

(*****************************************************************************)
(*                  GCODE export signature                                   *)
(*****************************************************************************)
sig
  type word;     (* added 13/7/94 SPF *)

  type prettyPrinter
  
  datatype inlineStatus =
    NonInline
  | MaybeInline
  | SmallFunction
  | OnlyInline
  ;

  datatype 'a codetree =
    MatchFail    (* Pattern-match failure *)
    
  | AltMatch of  (* Pattern-match alternative choices *)
      'a codetree * 'a codetree
  
  | Declar of  (* Make a local declaration or push an argument *)
      {
        value:      'a codetree,
        addr:       int,
        references: int
       }

   | Newenv of  (* Start a block *)
       'a codetree list
       
   | Constnt of (* Load a constant *)
       word
        
   | Extract of (* Get a local variable, an argument or a closure value *)
       {
         addr : int, 
         level: int, 
         fpRel: bool,
		 lastRef: bool
        }
     
   | Indirect of 
      { (* Load a value from a heap record *)
       base:   'a codetree,
       offset: int
      }
     
   | Eval of (* Evaluate a function with an argument list. *)
      {
        function:  'a codetree,
        argList:   'a codetree list,
        earlyEval: bool
       }
       
   | Lambda of (* Lambda expressions. *)
     { 
       body          : 'a codetree,
       isInline      : inlineStatus,
       name          : string,
       closure       : 'a codetree list,
       numArgs  	 : int,
       level         : int,
       closureRefs   : int,
       makeClosure   : bool
      } 

   | MutualDecs of (* Set of mutually recursive declarations. *)
       'a codetree list
       
   | Cond of (* If-statement *)
       'a codetree * 'a codetree * 'a codetree
   
   | Case of (* Case expressions *)
      {
       cases   : ('a codetree * int list) list,
       test    : 'a codetree,
       default : 'a codetree,
       min     : int,
       max     : int
      }
   
   | BeginLoop of (* Start of tail-recursive inline function. *)
       'a codetree * 'a codetree list

   | Loop of (* Jump back to start of tail-recursive function. *)
       'a codetree list
      
   | Raise of (* Raise an exception *)
       'a codetree
       
   | Ldexc (* Load the exception (used at the start of a handler) *)
   
   | Handle of (* Exception handler *)
     { (* Exception handler. *)
       exp      : 'a codetree,
       taglist  : 'a codetree list,
       handler  : 'a codetree
      }

   | Recconstr of (* Records (tuples) *)
       'a codetree list

   | Container of int (* Create a container for a tuple on the stack. *)

   | SetContainer of (* Copy a tuple to a container. *)
   	  {
       container: 'a codetree,
	   tuple:     'a codetree,
	   size:      int
	  }

   | TupleFromContainer of (* Make a tuple from the contents of a container. *)
	   'a codetree * int

   | Global of 'a (* Global value *)
   | CodeNil

  (* Parameterised because GCODE doesn't know about optVals *)
  val pretty  : 'a codetree * ('a * prettyPrinter -> unit) * prettyPrinter -> unit
  val isSmall : 'a codetree * ('a * ('a codetree -> int) -> int) -> bool

  val gencode: 'a codetree -> unit -> word;
  val structureEq : word * word -> bool;
end =

let

(*****************************************************************************)
(*                  ARRAY                                                    *)
(*****************************************************************************)
structure ARRAY :
sig
  type 'a array;
  
  val array : int * '_a -> '_a array;
  val update : 'a array * int * 'a -> unit;
  val sub    : 'a array * int -> 'a;
end = Array;

(*****************************************************************************)
(*                  STRETCHARRAY                                             *)
(*****************************************************************************)
structure STRETCHARRAY :
sig
  type 'a stretchArray;
  
  val stretchArray : int * '_a -> '_a stretchArray;
  val update : '_a stretchArray * int * '_a -> unit;
  val sub    : 'a stretchArray * int -> 'a;
end = StretchArray;


in

(*****************************************************************************)
(*                  GCODE functor body                                       *)
(*****************************************************************************)
struct

  open CODECONS;
  open ADDRESS;
  open DEBUGSWITCH;
  open PRETTYPRINTER;
  open MISC;

  datatype inlineStatus =
    NonInline
  | MaybeInline
  | SmallFunction
  | OnlyInline
  ;
  
  datatype 'a codetree =
    MatchFail    (* Pattern-match failure *)
    
  | AltMatch of  (* Pattern-match alternative choices *)
      'a codetree * 'a codetree
  
  | Declar of  (* Make a local declaration or push an argument *)
      {
        value:      'a codetree,
        addr:       int,
        references: int
       }

   | Newenv of  (* Start a block *)
       'a codetree list
       
   | Constnt of (* Load a constant *)
       word
        
   | Extract of (* Get a local variable, an argument or a closure value *)
       {
         addr : int, 
         level: int, 
         fpRel: bool,
		 lastRef: bool
        }
     
   | Indirect of 
      { (* Load a value from a heap record *)
       base:   'a codetree,
       offset: int
      }
     
   | Eval of (* Evaluate a function with an argument list. *)
      {
        function:  'a codetree,
        argList:   'a codetree list,
        earlyEval: bool
       }
       
   | Lambda of (* Lambda expressions. *)
     { 
       body          : 'a codetree,
       isInline      : inlineStatus,
       name          : string,
       closure       : 'a codetree list,
       numArgs  	 : int,
       level         : int,
       closureRefs   : int,
       makeClosure   : bool
      } 

   | MutualDecs of (* Set of mutually recursive declarations. *)
       'a codetree list
       
   | Cond of (* If-statement *)
       'a codetree * 'a codetree * 'a codetree

   | Case of (* Case expressions *)
      {
       cases   : ('a codetree * int list) list,
       test    : 'a codetree,
       default : 'a codetree,
       min     : int,
       max     : int
      }

   | BeginLoop of (* Start of tail-recursive inline function. *)
       'a codetree * 'a codetree list

   | Loop of (* Jump back to start of tail-recursive function. *)
       'a codetree list
      
   | Raise of (* Raise an exception *)
       'a codetree
       
   | Ldexc (* Load the exception (used at the start of a handler) *)
   
   | Handle of (* Exception handler *)
     { (* Exception handler. *)
       exp      : 'a codetree,
       taglist  : 'a codetree list,
       handler  : 'a codetree
      }

   | Recconstr of (* Records (tuples) *)
       'a codetree list

   | Container of int (* Create a container for a tuple on the stack. *)

   | SetContainer of (* Copy a tuple to a container. *)
   	  {
       container: 'a codetree,
	   tuple:     'a codetree,
	   size:      int
	  }

   | TupleFromContainer of (* Make a tuple from the contents of a container. *)
	   'a codetree * int

   | Global of 'a (* Global value *)
   | CodeNil
   
(*****************************************************************************)
(*                         codetree functions                                *)
(*****************************************************************************)

  type 'a declarForm = 
     { (* Declare a value or push an argument. *)
       value:      'a codetree,
       addr:       int,
       references: int
      }

  and loadForm = 
     { (* Load a value. *)
       addr : int, 
       level: int, 
       fpRel: bool,
	   lastRef: bool
      }

   and 'a diadic = 
     'a codetree * 'a codetree

   and 'a triadic = 
     'a codetree * 'a codetree * 'a codetree

   and 'a lambdaForm =
     { (* Lambda expressions. *)
       body          : 'a codetree,
       isInline      : inlineStatus,
       name          : string,
       closure       : 'a codetree list,
       numArgs  	 : int,
       level         : int,
       closureRefs   : int,
       makeClosure   : bool
      };

  

(*****************************************************************************)
(*                         diadic functions                                  *)
(*****************************************************************************)
  val diaFirst:  'a diadic -> 'a codetree = #1;
  val diaSecond: 'a diadic -> 'a codetree = #2;

(*****************************************************************************)
(*                         triadic functions                                 *)
(*****************************************************************************)
  val triFirst:  'a triadic -> 'a codetree = #1;
  val triSecond: 'a triadic -> 'a codetree = #2;
  val triThird:  'a triadic -> 'a codetree = #3;

  (* stringify functions *)
  fun stringOfBool true  = "true"
    | stringOfBool false = "false";
    
  fun stringOfWord w =
    if isShort w
    then Int.toString (toInt (toShort w))
    else "<long>";
  
  fun pretty (pt : 'a codetree,
              prettyOptVal : ('a * prettyPrinter) -> unit,
              pprint: prettyPrinter) : unit =
  let
    fun pList ([]: 'b list) (sep: string) (disp: 'b->unit) = ()
      | pList [h]    sep disp = disp h
      | pList (h::t) sep disp =
        ( ppBeginBlock pprint (0, false);
          disp h;
          ppBreak pprint (0, 0);
          ppAddString pprint sep;
          ppEndBlock pprint ();
          ppBreak pprint (1, 0);
          pList t sep disp
        );

    fun printList start lst sep =
    ( ppBeginBlock pprint (1, true);
      ppAddString pprint (start ^ "(");
      pList lst sep (fn x => (pretty (x, prettyOptVal, pprint)));
      ppBreak pprint (0, 0);
      ppAddString pprint (")");
      ppEndBlock pprint ()
    );
      
    fun printMonad name pt =
    ( ppBeginBlock pprint (1, true);
      ppAddString pprint (name^"(");
      pretty (pt, prettyOptVal, pprint);
      ppBreak pprint (0, 0);
      ppAddString pprint (")");
      ppEndBlock pprint ()
    );
      
    fun printDiad name diad =
    ( ppBeginBlock pprint (1, true);
      ppAddString pprint (name^"(");
      pretty (diaFirst diad, prettyOptVal, pprint);
      ppAddString pprint ", ";
      ppBreak pprint (0, 0);
      pretty (diaSecond diad, prettyOptVal, pprint);
      ppBreak pprint (0, 0);
      ppAddString pprint (")");
      ppEndBlock pprint ()
    );

    fun printTriad name triad =
    ( ppBeginBlock pprint (1, true);
      ppAddString pprint (name^"(");
      pretty(triFirst triad, prettyOptVal, pprint);
      ppAddString pprint ", ";
      ppBreak pprint (0, 0);
      pretty(triSecond triad, prettyOptVal, pprint);
      ppAddString pprint ", ";
      ppBreak pprint (0, 0);
      pretty (triThird triad, prettyOptVal, pprint);
      ppBreak pprint (0, 0);
      ppAddString pprint (")");
      ppEndBlock pprint ()
    );

  in
    case pt of
      CodeNil =>
        ppAddString pprint "NIL"
        
    | MatchFail =>   
        ppAddString pprint "MATCHFAIL"

    | AltMatch pair =>   
        printDiad "ALTMATCH" pair

    | Eval {function, argList, earlyEval} =>
    (
      ppBeginBlock pprint (3, false);
      pretty (function, prettyOptVal, pprint);
      ppBreak pprint (0, 0);
      if earlyEval
      then
      ( ppAddString pprint "{early}";
        ppBreak pprint (0, 0)
      ) 
      else ();
      printList "$" argList ",";
      ppEndBlock pprint ()
    )

    | Declar {value, addr, references} =>
    (
      ppBeginBlock pprint (1, false);
      ppAddString pprint (concat
          ["DECL #",
            Int.toString addr, 
           "{",
            Int.toString references,
             " uses} ="]);
      ppBreak pprint (1, 0);
      pretty (value, prettyOptVal, pprint);
      ppEndBlock pprint ()
    )
      
    | Extract {addr, level, fpRel, lastRef} =>
    let
	  val last = if lastRef then ", last" else "";
      val str : string =
        if not fpRel
          then concat ["CLOS(", Int.toString level, ",", Int.toString addr, last, ")"]
        else if addr < 0
          then concat ["PARAM(", Int.toString level, ",", Int.toString (~ addr), last, ")"]
          else concat ["LOCAL(", Int.toString level, ",", Int.toString addr, last, ")"]
    in
      ppAddString pprint str
    end
     
    | Indirect {base, offset} =>
    let
      val str = "INDIRECT(" ^ Int.toString offset ^ ", ";
    in
      ppAddString pprint str;
      pretty (base, prettyOptVal, pprint);
      ppAddString pprint ")"
    end

    | Lambda {body, isInline, name, closure, numArgs, level, closureRefs, makeClosure} =>
    let
      val inl = 
        case isInline of
          NonInline   => ""
        | MaybeInline => "INLINE"
		| SmallFunction => "SMALL"
        | OnlyInline  => "ONLYINLINE"
    in
      ppBeginBlock pprint (1, true);
      ppAddString pprint ("LAMBDA"^inl^"(");
      ppBreak pprint (1, 0);
      ppAddString pprint name;
      ppBreak pprint (1, 0);
      ppAddString pprint ( "CL="  ^ stringOfBool makeClosure);
      ppAddString pprint (" CR="  ^ Int.toString closureRefs);
      ppAddString pprint (" LEV=" ^ Int.toString level);
      ppAddString pprint (" ARGS=" ^ Int.toString numArgs);
      printList " CLOS=" closure ",";
      ppBreak pprint (1, 0);
      pretty (body, prettyOptVal, pprint);
      ppAddString pprint "){LAMBDA}";
      ppEndBlock pprint ()
    end
    
    | Constnt w =>
        ppAddString pprint ("LIT" ^ stringOfWord w)
      
    | Cond triple =>
        printTriad "IF" triple
      
    | Newenv ptl =>
        printList "BLOCK" ptl ";"

	| BeginLoop(loopExp, args) =>
	    (
	      ppBeginBlock pprint (3, false);
	      printList "BEGINLOOP" args ",";
	      ppBreak pprint (0, 0);
      	  ppAddString pprint "(";
	      ppBreak pprint (0, 0);
	      pretty (loopExp, prettyOptVal, pprint);
	      ppBreak pprint (0, 0);
      	  ppAddString pprint ")";
	      ppEndBlock pprint ()
	    )

    | Loop ptl =>
        printList "LOOP" ptl ","
      
    | Raise c =>
       printMonad "RAISE" c
      
    | Handle {exp, taglist, handler} =>
    (
      ppBeginBlock pprint (3, false);
      ppAddString pprint "HANDLE(";
      pretty (exp, prettyOptVal, pprint);
      ppAddString pprint "WITH";
      ppBreak pprint (1, 0);
      pretty (handler, prettyOptVal, pprint);
      ppAddString pprint ")";
      ppEndBlock pprint ()
    )
     
    | Ldexc =>
       ppAddString pprint "LDEXC"
      
    | Case {cases, test, default, min, max} =>
    (
      ppBeginBlock pprint (1, true);
      ppAddString pprint
       (concat 
         ["CASE ",
           Int.toString min,
           "-",
           Int.toString max,
           "("
          ]);
      pretty (test, prettyOptVal, pprint);
      ppAddString pprint ")";
      ppBreak pprint (1, 0);
      ppAddString pprint "(";
      ppBreak pprint (1, 0);
      pList cases ","
            (fn (exp : 'a codetree, labels : int list) =>
               (
                ppBeginBlock pprint (1, true);
                List.app (fn l =>
                         (
                          ppAddString pprint (Int.toString l ^ ":");
                          ppBreak pprint (1, 0)
                         )
                       ) labels;
                 pretty (exp, prettyOptVal, pprint);
                 ppEndBlock pprint ()
                )
              );
	  case default of
	  	CodeNil => ()
	   | _ =>
	      ( ppBreak pprint (1, 0);
	        ppBeginBlock pprint (1, false);
	        ppAddString pprint "ELSE:";
	        ppBreak pprint (1, 0);
	        pretty (default, prettyOptVal, pprint);
	        ppEndBlock pprint ()
	      );
      ppBreak pprint (1, 0);
      ppAddString pprint (") {"^"CASE"^"}");
      ppEndBlock pprint ()
    )
    
    | MutualDecs ptl =>
        printList "MUTUAL" ptl " AND "
      
    | Recconstr ptl =>
        printList "RECCONSTR" ptl ","

    | Container size =>
      ppAddString pprint ("CONTAINER " ^ Int.toString size)
    
    | SetContainer{container, tuple, size} =>
	    (
	      ppBeginBlock pprint (3, false);
	      ppAddString pprint ("SETCONTAINER(" ^ Int.toString size ^ ", ");
	      pretty (container, prettyOptVal, pprint);
	      ppBreak pprint (0, 0);
      	  ppAddString pprint ",";
	      ppBreak pprint (1, 0);
	      pretty (tuple, prettyOptVal, pprint);
	      ppBreak pprint (0, 0);
      	  ppAddString pprint ")";
	      ppEndBlock pprint ()
	    )

    | TupleFromContainer (container, size) =>
	  (
	      ppBeginBlock pprint (3, false);
	      ppAddString pprint ("TUPLECONTAINER(" ^ Int.toString size ^ ", ");
	      ppBreak pprint (0, 0);
	      pretty (container, prettyOptVal, pprint);
	      ppBreak pprint (0, 0);
	      ppAddString pprint ")";
	      ppEndBlock pprint ()
	  )
      
    | Global glob =>
    (
      ppBeginBlock pprint (1, true);
      ppAddString pprint "GLOBAL ";
      prettyOptVal (glob, pprint);
      ppAddString pprint " (*GLOBAL*)";
      ppEndBlock pprint ()
    )
    
    (* That list should be exhaustive! *)
  end (* pretty *);

  fun isSmall (pt, sizeOptVal) = 
  let
    fun sizeList [] = 0
      | sizeList (c::cs) = size c + sizeList cs
      
    and sizeCaseList []           = 0
      | sizeCaseList ((c,il)::cs) = size c + List.length il + sizeCaseList cs
      
    (* some very rough size estimates *)
    and size pt =
      case pt of
	CodeNil                         => 0
      | MatchFail                       => 1
      | AltMatch (m1, m2)               => size m1 + size m2 + 1
      | Declar {value, ...}             => size value
      | Newenv cl                       => sizeList cl
      | Constnt w                       => if isShort w then 0 else 1
      | Extract _                       => 1  (* optimistic *)
      | Indirect {base,...}             => size base + 1
      | Lambda {body,numArgs,...}		=> size body + numArgs
      | Eval {function,argList,...}     => size function + sizeList argList + 2
      | MutualDecs _                    => !maxInlineSize (* no inlining *)
      | Cond (i,t,e)                    => size i + size t + size e + 2
      | BeginLoop (b, args)             => size b + sizeList args
      | Loop args                       => sizeList args + 1
      | Raise c                         => size c + 1
      | Ldexc                           => 1
      | Handle {exp,taglist,handler}    => size exp + size handler + sizeList taglist + List.length taglist
      | Recconstr cl                    => sizeList cl + 2 (* optimistic *)
	  | Container size                  => 1 (* optimistic *)
	  | SetContainer{container, tuple = Recconstr cl, ...} =>
	  									(* We can optimise this. *) sizeList cl + size container
	  | SetContainer{container, tuple, size=len} => size container + size tuple + len
	  | TupleFromContainer(container, len) => len + size container + 2 (* As with Recconstr *)
      | Global glob                     => sizeOptVal (glob, size)
      | Case {test,default,cases,...}   =>
	  size test + size default + sizeCaseList cases
      ;
   in
   	 (* We previously treated functions which only contained Lambdas as always small.
	    I've now taken that out because it caused the code to blow up if the Lambda
		turned out to be a large function (e.g. fun f x = fn y => ...BIGCODE...).
		There may be a case for lifting the inner lambda out much as we already do
		with fun declarations but this needs to be done in the front end.  DCJM 14/3/02.  *)
     size pt < !maxInlineSize
   end;

  open RuntimeCalls; (* for POLY_SYS numbers *)
  
  val F_mutable_words = Or (F_mutable, F_words);
  
  val objLength = ADDRESS.length;

  (* gets a value from the run-time system; 
    usually this is a closure, but sometimes it's an int.  *)
  val ioOp : int -> word = RUNCALL.run_call1 POLY_SYS_io_operation;

  (* minor HACKS *)
  fun forLoop f i n = if i > n then () else (f i; forLoop f (i + 1) n);
  fun apply f [] = () | apply f (h::t) = (f h; apply f t);
  
  val short0 : short = toShort 0;
  val short1 : short = toShort 1;
  val short2 : short = toShort 2;
      
  val word0 = toWord 0;
  val word1 = toWord 1;
  
  val DummyValue : word = word0; (* used as result of "raise e" etc. *)
  val UnitValue : word = word0; (* unit *)
  val False : word = word0;     (* false *)
  val True  : word = word1;     (* true *)
  val Zero  : word = word0;     (* 0 *)
  
  val constntTrue  = Constnt True;
  val constntFalse = Constnt False;

(* copied from CTREE.ML for efficiency (local calls can be inlined) *)  

  type 'a evalForm = 
     { (* Evaluate a function with an argument list. *)
       function:  'a codetree,
       argList:   'a codetree list,
       earlyEval: bool
     }

  type 'a caseForm =
     { (* Case expressions *)
       cases   : ('a codetree * int list) list,
       test    : 'a codetree,
       default : 'a codetree,
       min     : int,
       max     : int
      }
   
  
(*****************************************************************************)
(*                         code for structureEq                              *)
(*****************************************************************************)
  local
    infix 6 And;
    infix 7 Or;
  
   (* Compare two values for equality of their structures. Values with
       the mutable bit set are assumed to be references and are only equal
       if they are the same address. We assume that this will not be applied
       to code segments or stacks. The address of this function is put
       in the code to do the equality testing. *)
  in
    fun structureEq (a:word, b:word) =
     if wordEq (a, b) then true
     else 
       (* If either is a short, then they cannot be equal unless a = b. 
           Other values are addresses so we have to use structural
           equality. *)
           
       if isShort a then false else
       if isShort b then false (* nil is a short now SPF 14/7/94 *)
       else let
         (* we promise to be very careful! *)
         val toAddress : 'a -> address = unsafeCast;
         val toShort   : 'a -> short   = unsafeCast;
       
         (* Both addresses *)
         val aa  : address = toAddress a;
         val bb  : address = toAddress b;
         val alw : short   = objLength aa;
         val blw : short   = objLength bb;
       in
         if not (wordEq (alw, blw))
           then (* Must be same size and type. *) false
           
         else let
           val orFlags : short = flags aa Or flags bb
         in
           if not (wordEq (orFlags And F_mutable, short0))
              (* In ML mutable objects are equal only if addresses are.
                 This avoids the need to check for circularity. *)
             then false
                     
           else if not (wordEq (orFlags And F_bytes, short0))
             then let
               (* Byte vector. Each value is a byte and treated as equal
                  only if each byte is equal. Can also arise when the garbage
                  collector turns vectors of small integers into byte vectors
                  to save repeated scans. *)
               (* Would it be more efficient to compare a word at a time?
                  No, because byte-segments contain bit-patterns that 
                   aren't proper "words"  SPF 14/7/94 *)
               fun compBytes 0 = 
                     wordEq (loadByte (aa, short0), loadByte (bb, short0))
                 | compBytes i =
                     wordEq (loadByte (aa, toShort i), loadByte (bb, toShort i))
                         andalso compBytes (i - 1);
             in
               compBytes (toInt alw * 4 - 1)
             end
             
           else if wordEq (alw, short1)
             then structureEq (loadWord (aa, short0), loadWord (bb, short0))
  
           else if wordEq (alw, short2)
           then let
             (* Most objects are list cells or tagged values. *)
             (* We want to avoid recursing if we can easily see that the
                objects differ. Short integers differing mean that the
                objects do differ, otherwise if they are different pointers
                we have to follow them. *)
             val a0 : word = loadWord (aa, short0);
             val a1 : word = loadWord (aa, short1);
             val b0 : word = loadWord (bb, short0);
             val b1 : word = loadWord (bb, short1);
           in
             if isShort a0 
               then wordEq (a0, b0) andalso structureEq (a1, b1)
             
             else if isShort b0
               then false
  
             else if isShort a1
               then wordEq (a1, b1) andalso structureEq (a0, b0)
               
             else if isShort b1
               then false
               
             else
               structureEq (a0, b0) andalso structureEq (a1, b1)
           end
  
           else let
             (* Larger objects. *)
             (* Compare the words of the vector.  Work from the end back - this
                is more efficient for tagged values. *)
             fun compWords 0 =
                 (* Tail recurse on last entry. *)
                 structureEq (loadWord (aa, short0), loadWord (bb, short0))
                 
               | compWords i =
                   structureEq (loadWord (aa, toShort i), loadWord (bb, toShort i))
                   andalso compWords (i - 1);
           in
             compWords (toInt alw - 1)
           end
         end
       end (* structureEq *);
  end; (* local *)


(*************************** end of code for structureEq *********************)  
  (* Where the result, if any, should go *)
  datatype whereto =
    NoResult     (* discard result *)
  | ToStack     (* Need a result but it can stay on the pseudo-stack *);
  
  (* Are we at the end of the procedure. *)
  datatype tail =
    EndOfProc
  | NotEnd;

  datatype slValue =
    Address    of int  * int (* Address of an entry on the stack. *)
  | StaticLink of code * int (* A static linked procedure. *)
  | Recursive  of code       (* A recursive reference to a closure. *)

  (* Code generate a procedure or global declaration *)
  fun codegen
       (pt : 'a codetree,
        cvec : code,
        loadStaticLink : int * int -> slValue,
        numOfArgs: int) : address =
  let
    fun matchFailed _ = raise InternalError "codegen: unhandled pattern-match failure"

    val initTrans = 5; (* Initial size of tables. *)
    val initStack = 10;
    
    datatype decEntry =
      StackAddr of int
    | ProcConst of code
    | Empty;
    
    val decVec : decEntry STRETCHARRAY.stretchArray =
      STRETCHARRAY.stretchArray (initTrans, Empty);
    
    (* Count of number of items on the stack. *)
    val realstackptr = ref 1; (* The static-link/closure ptr is already there *)
 
    (* Exited - set to true if we have jumped out. *)
    val exited = ref false;
 
    (* Push a value onto the stack. For the moment this merely involves
       adding corresponding values to the real and pseudo-stacks. *)
    fun incsp () : unit = realstackptr := !realstackptr + 1;
 
    (* An entry has been removed from the stack. *)
    fun decsp ()  : unit= realstackptr := !realstackptr - 1;
 
    (* Pushes a local or non-local stack value. *)
    fun pushStackValue (addr : int, level : int) : unit =
    let
      val U : unit =
        if level > 0
        then (* Non-local *)
          genNonLocal(!realstackptr, level, addr, cvec)
        else (* Locals and references to the closure. *)
           genLocal(!realstackptr + addr, cvec);
    in
      incsp ()
    end;


    (* Loads a local, argument or closure value; translating local
       stack addresses to real stack offsets. *)
   fun locaddr (ext : loadForm) : unit =
   let
     val locn = #addr ext;
   in
     if #fpRel ext
     then 
       if locn < 0 (* Args. *)
       then pushStackValue (~locn, 0)
       else (* positive address - on the stack. *)
	 case STRETCHARRAY.sub (decVec, locn) of
	   StackAddr n => pushStackValue (~ n, 0)
 
	 | _ => (* Should be on the stack, not a procedure. *)
	   raise InternalError "locaddr: bad stack address"
         
     else (* closure-pointer relative *)
	case loadStaticLink (locn, 1) of
	  Recursive code =>
	  let
	    val U : unit = genRecRef (code, cvec)
	  in
	    incsp ()
	  end
	  
       | Address (addr, level) =>
	   pushStackValue (addr, level)
       
       | StaticLink (code, level) =>
	   raise InternalError "locaddr: illegal use of static-link function"
   end; (* locaddr *)

   (* generates code from the tree *)
   fun gencde (pt : 'a codetree, whereto : whereto, tailKind : tail, matchFailFn : unit -> unit, loopAddr) : unit =
   let
     (* Save the stack pointer value here. We may want to reset the stack. *)
     val oldsp = !realstackptr;

     val U : unit =
       case pt of
         MatchFail =>
           (* Leave stack adjustments until later *)
	   matchFailFn ()

       | AltMatch (exp1, exp2) => (* A bit like Cond *)
	 let
	   val failLabs = ref ([] : labels list);
	   fun newMatchFailFn () = 
	   let
	     (* Cut back the stack and branch *)
	     val adjustment = !realstackptr - oldsp;
	     
	     val U : unit =
               if adjustment < 0
                 then raise InternalError ("gencde (AltMatch): bad adjustment " ^ Int.toString adjustment)
               else if !exited orelse adjustment = 0
                 then ()
               else
                 resetStack (adjustment, false, cvec);
                 
             val U : unit = realstackptr := oldsp;

	     val thisFailure : labels = putBranchInstruction (jump, cvec);
	     val U : unit = exited := true
	   in
             failLabs := thisFailure :: !failLabs
	   end;
	   
	   val U : unit = 
	      gencde (exp1, whereto, tailKind, newMatchFailFn, loopAddr);
	     
	   (* Get rid of the result from the stack.
	      If there is a result then exp2 will push it. *)
	   val U : unit = case whereto of ToStack => decsp () | NoResult => ();
	   val exp1Exited : bool = !exited;

	   (* If exp1 succeeded, we skip exp2 *)
	   val suceedLab : labels = 
	     if exp1Exited then noJump else putBranchInstruction (jump, cvec);
	     
	   val U : unit =
	     if !realstackptr = oldsp then ()
	     else raise InternalError "gencde: bad stack value"
	   
	   (* If exp1 failed, we come here (with NO result). *)
	   val U : unit = exited := false; (* Don't try to be too clever *)
	   val U : unit = apply (fn (lab : labels) => fixup (lab, cvec)) (!failLabs);
	   
	   (* Compile exp2 using the OLD matchFailFn *)
	   val U : unit = gencde (exp2, whereto, tailKind, matchFailFn, loopAddr);

	   (* If exp1 succeeded, we merge back in here. *)
	   val U : unit = fixup (suceedLab, cvec);
	 in
	   exited := (!exited andalso exp1Exited)
	 end
            
       | Eval evl =>
           genEval (evl, tailKind, matchFailFn)
         
       (* Declarations are not necessarily nested in Newenv *)
       | Declar {value, addr, ...} =>
	   (* Put the entry for this declaration in the table. *)
	   (case value of
	      Lambda (lam as {makeClosure = false, name, ...}) => 
	      let
		val newCode : code = codeCreate (true, name);
		val U : unit =
		  STRETCHARRAY.update (decVec, addr, ProcConst newCode);
	      in
		genSlProc (lam, newCode)
	      end
	      
	    | _ => (* Other declaration - to the stack. *)
	      let
		val U : unit = gencde (value, ToStack, NotEnd, matchFailFn, loopAddr);
	      in
		STRETCHARRAY.update (decVec, addr, StackAddr(!realstackptr))
	      end
	   )

        (* MutualDecs should nested in Newenv??? *)
      | MutualDecs _ =>
            raise InternalError "gencde: MutualDecs only allowed inside Newenv"

       | Extract ext =>
	   	   (* This may just be being used to discard a value which isn't
		      used on this branch. *)
	   	   if whereto = NoResult then ()
           else locaddr ext
       
       | Indirect {base, offset} =>
         let
           val U : unit = gencde (base, ToStack, NotEnd, matchFailFn, loopAddr);
         in
           genIndirect (offset, cvec)
         end
       
       | Lambda lam =>
           genProc (lam, false, fn () => (), matchFailFn)
           
       | Constnt w =>
         let
           val U : unit = pushConst (w, cvec);
         in
           incsp ()
         end
  
       | Cond (testPart, thenPart, elsePart) =>
           genCond (testPart, thenPart, elsePart, whereto, tailKind, matchFailFn, loopAddr)
  
       | Newenv vl =>
         let
           (* We can't just call "gencde" on all the elements of the list,
              because declarations must persist as long as they're in scope.
              We treat Declar and MutualDecs specially here, but just naively
              call gencde for expressions.
              SPF 7/1/97
           *)
         
            (* Processes a list of entries. *)
            fun codeList [] = ()
              | codeList (valu :: valus) = 
            let
              val U : unit =
                case valu of
                  MutualDecs dl => genMutualDecs (dl, matchFailFn)
                | Declar _ => gencde (valu, ToStack, NotEnd, matchFailFn, loopAddr)
                | _ =>
                (* If this is the last one it is the result of the block
                   so it is generated to the stack if the block needs a
                   result. *)
                   case valus of
                     [] => gencde (valu, whereto, tailKind, matchFailFn, loopAddr)
                   | _  => gencde (valu, NoResult, NotEnd, matchFailFn, loopAddr)
            in (* process the tail *)
              codeList valus
            end
          in
            codeList vl
          end
		  
		 | BeginLoop (body, args) =>
		    (* Execute the body which will contain at least one Loop instruction.
		  	   There will also be path(s) which don't contain Loops and these
			   will drop through. *)
		 	let
		 		(* Evaluate each of the arguments, pushing the result onto the stack. *)
				fun genLoopArg (Declar {addr, value, ...}) =
					(
					 gencde (value, ToStack, NotEnd, matchFailFn, loopAddr);
					 STRETCHARRAY.update (decVec, addr, StackAddr (!realstackptr));
					 !realstackptr (* Return the posn on the stack. *)
					)
				|	genLoopArg _ = raise InternalError "genLoopArg: not a declaration"
				val argIndexList = map genLoopArg args;

				val startSp = ! realstackptr; (* Remember the current top of stack. *)
				val startLoop : addrs = ic cvec; (* Start of loop *)
			in
				(* Process the body, passing the jump-back address down for the Loop instruction(s). *)
				gencde (body, whereto, tailKind, matchFailFn, SOME(startLoop, startSp, argIndexList))
				(* Leave the arguments on the stack.  They can be cleared later if needed. *)
			end

		 | Loop argList => (* Jump back to the enclosing BeginLoop. *)
		 	let
				val (startLoop, startSp, argIndexList) =
					case loopAddr of
						SOME l => l
					|	NONE => raise InternalError "No BeginLoop for Loop instr"
				(* Evaluate the arguments.  First push them to the stack because evaluating
				   an argument may depend on the current value of others.  Only when we've
				   evaluated all of them can we overwrite the original argument positions. *)
				fun loadArgs ([], []) = !realstackptr - startSp (* The offset of all the args. *)
				  | loadArgs (arg:: argList, argOffset :: argIndexList) =
				  	let
						(* Evaluate all the arguments. *)
						val U: unit = gencde (arg, ToStack, NotEnd, matchFailFn, NONE);
						val argOffset = loadArgs(argList, argIndexList);
					in
						genSetStackVal(argOffset, cvec); (* Copy the arg over. *)
						decsp(); (* The argument has now been popped. *)
						argOffset
					end
				  | loadArgs _ = raise InternalError "loadArgs: Mismatched arguments";

				val U: int = loadArgs(argList, argIndexList)
			in
				if !realstackptr <> startSp
				then resetStack (!realstackptr - startSp, false, cvec) (* Remove any local variables. *)
				else ();
		    
				(* Jump back to the start of the loop. *)
				jumpback (startLoop, cvec)
			end
  
       | Raise exp =>
         let
           val U : unit = gencde (exp, ToStack, NotEnd, matchFailFn, loopAddr);
           val U : unit = genRaiseEx cvec;
         in
           exited := true
         end
  
       | Handle {exp, taglist, handler} =>
         let
           type handler = labels;
           
           (* Save old handler *)
           val U : unit = genPushHandler cvec;
           val U : unit = incsp ();
  
	   fun genTag (tag : 'a codetree) : handler =
	   let
	     (* Push address of new handler. *)
	     val handlerLab : labels = putBranchInstruction (setHandler, cvec);
             val U : unit = incsp ();
 
	     (* Push the exception to be caught. *)
	     val U : unit = gencde (tag, ToStack, NotEnd, matchFailFn, loopAddr)
	   in
	     handlerLab
	   end;

           (* Generate the list of tags and handler addreses. We reverse
              the taglist so the tags that are first in the list are
              put on the stack last, so they are checked first.
              We reverse the result so that they get fixed up in
              stack order. (I don't think this is important, but
              I'm not sure.) Did you get all that? SPF 26/11/96
           *)
	   val handlerList : handler list = rev (map genTag (rev taglist));

	  (* Code generate the body; "NotEnd" because we have to come back
	     to remove the handler; "ToStack" because delHandler needs
	     a result to carry down. *)
	  val U : unit = gencde (exp, ToStack, NotEnd, matchFailFn, loopAddr);
	  
          (* Now get out of the handler and restore the old one. *)
          val skipHandler : labels = putBranchInstruction (delHandler, cvec);
          
          (* Now process the handler itself. First we have to reset the stack.
             Note that we have to use "ToStack" again to be consistent with
             the stack-handling in the body-part. If we actually wanted "NoResult",
             the stack adjustment code at the end of gencde will take care
             of this. This means that I don't want to do any clever "end-of-function"
             optimisation either. SPF 6/1/97

	     The exception handler must be aligned to an odd word boundary
	     so that the garbage collector does not get confused when it
	     finds the address on the stack. Values on full-word
	     boundaries always point to the start of objects whereas
	     values off full-word boundaries (either return addresses or
	     catch-phrases) point into code-segments.
          *)
          val U : unit = realstackptr := oldsp;
          val U : unit = exited := false;
	  val U : unit = alignOffWord (cvec, 0);
          val U : unit = apply (fn handlerLab => fixup (handlerLab, cvec)) handlerList;
	  (* If we were executing machine code we must re-enter the interpreter. *)
          val U : unit = genEnterIntCatch cvec;
          val U : unit = gencde (handler, ToStack, NotEnd, matchFailFn, loopAddr);
          
          (* Finally fix-up the jump around the handler *)
          val U : unit = fixup (skipHandler, cvec);
         in
           exited := false
         end
  
       | Ldexc =>
         let
           (* Get the name of the exception. *)
           val U : unit = genLdexc cvec
         in
           incsp ()
         end
  
       | Case (cas as {cases, test, default, min, max}) =>
         let
           val numberOfCases = List.length cases;
         in
           if 3 * numberOfCases < max - min + 5
           then genSparseCase(cas, whereto, tailKind, matchFailFn, loopAddr)
           else genDenseCase (cas, whereto, tailKind, matchFailFn, loopAddr)
         end
  
       | Recconstr recList =>
         let
           (* Move the fields into the vector. *)
           fun loadItems [] = ()
             | loadItems (v :: vs) =
             let
               val U : unit = gencde (v, ToStack, NotEnd, matchFailFn, loopAddr);
             in
               loadItems vs
             end;
             
           val size : int = List.length recList;
  
           val U : unit = loadItems recList;
           val U : unit = genTuple (size, cvec);
         in
           realstackptr := !realstackptr - (size - 1)
         end

			(* Containers are supposed to be implemented on the stack for efficiency.
			   Unfortunately they cause problems in this code because we assume
			   that if we generate a value to the stack it only occupies a single
			   word.  Rather than change this code and risk breaking something I'm
			   simply generating them as mutable tuples on the heap. *)
	    | Container size =>
			(* Reserve a number of words on the stack for use as a tuple on the
			   stack.  The result is the address of this space. *)
			(
			genContainer(size, cvec); (* Push the address of this container. *)
			incsp() (* Pushes a single word. *)
			)

		| SetContainer{container, tuple, size} =>
			(* Copy the contents of a tuple into a container.  If the tuple is a
			   Recconstr instruction we can avoid generating the tuple and then
			   unpacking it and simply copy the fields that make up the tuple
			   directly into the container. *)
			(
				(* Load the address of the container. *)
				gencde (container, ToStack, NotEnd, matchFailFn, loopAddr);
			
				case tuple of
					Recconstr cl =>
						(* Simply set the container from the values. *)
					let
						fun setValue(v, offset) =
						(
							gencde (v, ToStack, NotEnd, matchFailFn, loopAddr);
							(* Move the entry into the container. This instruction
							   pops the value to be moved but not the destination. *)
							genMoveToVec(offset, cvec);
							decsp();
							offset + 1
						)
					in
						List.foldl setValue 0 cl;
						() (* The container address is still on the stack. *)
					end

				|	_ =>
					( (* General case. *)
						gencde (tuple, ToStack, NotEnd, matchFailFn, loopAddr);
						genSetContainer(size, cvec);
						decsp(); decsp()
					)

			)

		| TupleFromContainer(container, size) =>
			(* Create a tuple from the contents of a container. *)
			(
			    (* TODO: This returns a MUTABLE record which is different from the
			       other code-generators.  That could cause problems if we expect
			       an immutable object. *)
			    gencde(container, ToStack, NotEnd, matchFailFn, loopAddr)
			)
   
        | CodeNil => 
            raise InternalError "gencde: can't code-generate CodeNil value"
  
        | Global _ =>
            raise InternalError "gencde: can't code-generate Global value";

    in (* body of gencde *) 

      (* This ensures that there is precisely one item on the stack if
         whereto = ToStack and no items if whereto = NoResult. 
         There are two points to note carefully here:
           (1) Negative stack adjustments are legal if we have exited.
               This is because matchFailFn can cut the stack back too
               far for its immediately enclosing expression. This is
               harmless because the code actually exits that expression.
           (2) A stack adjustment of ~1 is legal if we're generating
               a declaration in "ToStack" mode, because not all declarations
               actually generate the dummy value that we expect. This
               used to be handled in resetStack itself, but it's more
               transparent to do it here. (In addition, there was a bug in
               resetStack - it accumulated the stack resets, but didn't
               correctly accumulate these "~1" dummy value pushes.)
               It's all much better now.
               SPF 9/1/97
     *)
      case whereto of
        ToStack =>
        let
          val newsp = oldsp + 1;
          val adjustment = !realstackptr - newsp;

          val U : unit =
            if !exited orelse adjustment = 0
              then ()
            else if adjustment < ~1
              then raise InternalError ("gencde: bad adjustment " ^ Int.toString adjustment)
            (* Hack for declarations that should push values, but don't *)
            else if adjustment = ~1
              then pushConst (DummyValue, cvec)
            else
              resetStack (adjustment, true, cvec);
        in
          realstackptr := newsp
        end
          
      | NoResult =>
        let
          val adjustment = !realstackptr - oldsp;

          val U : unit =
            if !exited orelse adjustment = 0
              then ()
            else if adjustment < 0
              then raise InternalError ("gencde: bad adjustment " ^ Int.toString adjustment)
            else
              resetStack (adjustment, false, cvec);
        in
          realstackptr := oldsp
        end
    end (* gencde *)

   (* doNext is only used for mutually recursive procedures where a
     procedure may not be able to fill in its closure if it does not have
     all the remaining declarations. *)
   and genProc (lam : 'a lambdaForm,
               mutualDecs: bool,
               doNext: unit -> unit,
               matchFailFn : unit -> unit) : unit =
      if #makeClosure lam = false (* static link form *)
      then raise InternalError "static link not allowed here"

      else if (case #closure lam of [] => true | _ => false)
      then let
        val newCode : code = codeCreate(false, #name lam);
        
        (* The only global references are recursive ones (?) *)
        fun loadRecLink (addr : int, level : int) : slValue =
          Recursive newCode;
        
        (* Code-gen procedure. No non-local references. *)
         val res : address =
           codegen (#body lam, newCode, loadRecLink, #numArgs lam);

        val U : unit = pushConst(toWord res, cvec);
        val U : unit = incsp();
      in
        if mutualDecs then doNext () else ()
      end

      else let (* Full closure required. *)
	(* If there is a closure we must fetch all non-local references
	   from it. *)

	(* Loads the value from the closure. *)
	fun loadSl (addr : int, level : int) : slValue =
	  if addr = 0
	  then (* recursive - return address of closure. *)
	    Address (~1, level - 1)
	  else (* Return an entry in the closure. *)
	    Address (addr, level);

        val newCode : code = codeCreate (true, #name lam);

        (* Code-gen procedure. *)
        val res : address =
          codegen (#body lam, newCode, loadSl, #numArgs lam);

        val sizeOfClosure = List.length (#closure lam) + 1;
      in
         if mutualDecs
         then let (* Have to make the closure now and fill it in later. *)
           val U : unit = genGetStore (sizeOfClosure, cvec);
           val U : unit = incsp ();
           
           (* Put code address into closure *)
           val U : unit = pushConst(toWord res, cvec); 
           val U : unit = genMoveToVec(0, cvec);
           
           val entryAddr : int = !realstackptr;

           val U : unit = doNext (); (* Any mutually recursive procedures. *)

           (* Push the address of the vector - If we have processed other
              closures the vector will no longer be on the top of the stack. *)
           val U : unit = pushStackValue (~ entryAddr, 0);

           (* Load items for the closure. *)
           fun loadItems ([], _) = ()
             | loadItems (v :: vs, addr : int) =
             let
               (* Generate an item and move it into the vector *)
               val U : unit = gencde (v, ToStack, NotEnd, matchFailed, NONE);
               val U : unit = genMoveToVec(addr, cvec);
               val U : unit = decsp ();
             in
               loadItems (vs, addr + 1)
             end;
             
           val U : unit = loadItems (#closure lam, 1);
           val U : unit = genLock cvec; (* Lock it. *)
           
           (* Remove the extra reference. *)
           val U : unit = resetStack (1, false, cvec);
         in
           realstackptr := !realstackptr - 1
         end
         
         else let
           (* Put it on the stack. *)
           val U : unit = pushConst (toWord res, cvec);
           val U : unit = incsp ();
           
           val U : unit =
             apply (fn (pt: 'a codetree) => gencde (pt, ToStack, NotEnd, matchFailFn, NONE)) (#closure lam);
                
           val U : unit = genTuple (sizeOfClosure, cvec);
         in
           realstackptr := !realstackptr - (sizeOfClosure - 1)
         end
      end

   (* Generate a procedure to be called with a static link. *)
   and genSlProc (lam: 'a lambdaForm, newCode: code) : unit =
   let
     (* If a procedure can be called by static link references then
        non-locals can be loaded by following the static chain. The offset
        is the entry in the (pseudo-)closure as with a procedure that
        requires a closure, but these can be translated into real stack
        offsets. *)
     val closureList = #closure lam;

     (* Finds the nth. item in the closure and returns the entry *)
     fun findClosure ((Extract ext) :: _, 1) : loadForm = ext
       | findClosure (_ :: t, n) = findClosure (t, n - 1)
       | findClosure (_, _) = raise InternalError "findClosure: bad Closure";

     (* Get static link level and offset. *)
     fun loadSl (prevloc : int, level: int) : slValue =
       if prevloc = 0 (* recursive call *)
       then StaticLink (newCode, level)
       else let
	 val closureEntry = findClosure (closureList, prevloc);
	 val locn = #addr closureEntry; (* Get new address *)
       in
	 if #fpRel closureEntry
	 then (* This level *)
	    if locn < 0 (* Address  *)
	    then Address (~ locn, level)
	    else (* Local - is it a procedure? *)
	      case STRETCHARRAY.sub (decVec, locn) of
		StackAddr n => Address (~ n, level)
	      | ProcConst c => StaticLink (c, level)
	      | Empty       => raise InternalError "loadSl: missing decVec entry"
	  else (* Try the next level *) 
	    loadStaticLink (locn, level + 1)
       end; (* loadSl *)
       
     (* Now code-generate the procedure. We can throw away the result because
        it will be assigned into the value we have just returned. *)
     val U : address =
       codegen (#body lam, newCode, loadSl, #numArgs lam)
   in
     ()
   end (* genSlProc*)

   and genCond (first: 'a codetree,
                second: 'a codetree,
                third: 'a codetree,
                whereto: whereto,
                tailKind: tail,
                matchFailFn : unit -> unit,
				loopAddr) : unit =
   let
     val U : unit = gencde (first, ToStack, NotEnd, matchFailFn, loopAddr);
     val toElse : labels = putBranchInstruction(jumpFalse, cvec);
     val U : unit = decsp();
   in
     case third of 
       CodeNil => (* No else-part *)
       let
        (* If there is no else-part then the then-part must return a value
           of type void for it to type-check. To save having to pop this
           value to get the stack level right we generate it with ``noresult''
           and the stack resetting mechanism will ensure that a result is
           pushed if it is needed. *)
         val U : unit = gencde (second, NoResult, tailKind, matchFailFn, loopAddr);
         val U : unit = fixup (toElse, cvec) (* Skipped the then-part. *)
       in
         exited := false (* If the test failed we won't have exited. *)
       end
       
     | _ =>
       let
	 val U : unit = gencde (second, whereto, tailKind, matchFailFn, loopAddr);
	 (* Get rid of the result from the stack. If there is a result then the
	    ``else-part'' will push it. *)
	 val U : unit = case whereto of ToStack => decsp () | NoResult => ();
	 
	 val thenExited : bool = !exited;
 
	 val toExit : labels =
	   if thenExited then noJump
	   else putBranchInstruction (jump, cvec);
    
	 (* start of "else part" *)
	 val U : unit = fixup (toElse, cvec);
	 val U : unit = exited := false;
	 val U : unit = gencde (third, whereto, tailKind, matchFailFn, loopAddr);
 
	 val elseExited : bool= !exited;
	 
	 val U : unit = fixup (toExit, cvec);
       in
	 exited := (thenExited andalso elseExited) (* Only exited if both sides did. *)
       end
   end (* genCond *)

   and genEval (eval : 'a evalForm, tailKind : tail, matchFailFn : unit -> unit) : unit =
   let
     val argList : 'a codetree list = #argList eval;
     val argsToPass : int = List.length argList;

     (* Load arguments *)
     fun loadArgs [] = ()
       | loadArgs (v :: vs) =
       let (* Push each expression onto the stack. *)
         val U : unit = gencde(v, ToStack, NotEnd, matchFailFn, NONE);
       in
         loadArgs vs
       end;

     (* Called after the args and the closure to call have been pushed
        onto the stack. *)
     fun callClosure () : unit =
       case tailKind of
         NotEnd => (* Normal call. *)
           genCallClosure cvec
         
       | EndOfProc => (* Tail recursive call. *)
         let
           (* Get the return address onto the top of the stack. *)
           val U : unit = pushStackValue (0, 0);
           
           (* Slide the return address, closure and args over the
              old closure, return address and args, and reset the
              stack. Then jump to the closure. *)
           val U : unit =
             genTailCall(argsToPass + 2, !realstackptr - 1 + (numOfArgs - argsToPass), cvec);
            (* It's "-1" not "-2", because we didn't bump the realstackptr
               when we pushed the return address. SPF 3/1/97 *)
         in
           exited := true
         end;
         
      val U : unit =
	case #function eval of
	   (* The procedure is being loaded from the stack or closure so it
	      may be a static-link procedure. *)
	  Extract ext =>
	  let
	    (* Since the procedure is on the stack there can be no side-effects
	       in loading it. Can therefore load the arguments now. *)
	    val U : unit = loadArgs argList;
  
	    val staticLinkValue =
	      if #fpRel ext
	      then let (* Local *)
	        val addr : int = #addr ext;
	      in
		if addr < 0 (* Address *)
		then Address (~ addr, 0)
		else (* Local - is it a procedure? *)
		  case STRETCHARRAY.sub (decVec, addr) of
		    StackAddr n => Address (~ n, 0)
		  | ProcConst c => StaticLink (c, 0)
		  | Empty       => raise InternalError "staticLinkValue: missing decVec entry"
	      end
	      else (* Non-local or recursive. *) 
		loadStaticLink (#addr ext, 1);
	  in
	    case staticLinkValue of
	      Address (addr, level) =>
	      let
	        val U : unit = pushStackValue (addr, level);
	      in
	        callClosure ()
	      end
	      
	      (* recursive reference to a procedure - not static link. *)
	    | Recursive code =>
	      let
	        val U : unit = genRecRef (code, cvec);
	        val U : unit = incsp();
	      in
	        callClosure ()
	      end
	      
	     (* Static link *)
	    | StaticLink (code, level) =>
	      let
	        val U : unit = genCallSl(!realstackptr, level, code, cvec);
	      in
	        incsp ()
	      end
	  end (* Extract *)
	
	| _ => (* The procedure is not being found by simply loading a value
		  from the stack or the closure. *)
	  let
	    (* Must load the closure and call. *)
	    (* Have to guarantee that the expression to return the procedure
	      is evaluated before the arguments. *)
    
	    (* Returns true if evaluating it later is safe. *)
	    fun safeToLeave (node: 'a codetree) : bool =
	      case node of
		Constnt _ => true
	      | Lambda _  => true
	      | Indirect {base, ...} => 
		(* Safe only if the expression (always a type) being indirected
		   is. This is put in because calling procedures in a type is a
		   common occurence and should be made reasonably efficient. *)
		   safeToLeave base
	      | _        => false
    
	    val U : unit =
	      if (case argList of [] => true | _ => safeToLeave (#function eval))
	      then let
		(* Can load the args first. *)
		val U : unit = loadArgs argList;
	      in 
		gencde (#function eval, ToStack, NotEnd, matchFailFn, NONE)
	      end
	      
	      else let
		(* The expression for the procedure is too complicated to
		   risk leaving. It might have a side-effect and we must
		   ensure that any side-effects it has are done before the
		   arguments are loaded. *)
		val U : unit = gencde(#function eval, ToStack, NotEnd, matchFailFn, NONE);
		val U : unit = loadArgs(argList);
		(* Load the procedure again. *)
		val U : unit = genLocal(argsToPass, cvec);
	      in
		incsp ()
	      end
	 in
	   (* Call the procedure. *)
	   callClosure ()
	 end; (* Not Extract *)

       (* Make sure we interpret when we return from the call *)
       val U : unit = genEnterIntCall (cvec, argsToPass);

     in (* body of genEval *)
       realstackptr := !realstackptr - argsToPass (* Args popped by caller. *)
     end

   (* This is used for cases which are not sparse i.e. the values occupy
      a narrow enough range to make indexing efficient. *)
   and genDenseCase 
        (pt: 'a caseForm,
         whereto: whereto,
         tailKind: tail,
         matchFailFn : unit -> unit,
		 loopAddr) : unit =
   let
     (* If there is no default case the results are all
        void for the type-checking to work so we don't
        generate them. *)
     val whereto = case #default pt of CodeNil => NoResult | _ => whereto;
     
     val U : unit = gencde (#test pt, ToStack, NotEnd, matchFailFn, loopAddr);

     (* The exit jumps are chained together. *)
     val lastEndJump : labels ref = ref noJump;
     val limit : int = #max pt - #min pt;

     val U : unit =
       if #min pt = 0 then ()
       else let (* Subtract lower limit. *)
         val U : unit = pushConst(toWord (#min pt), cvec);
         val U : unit = pushConst(ioOp POLY_SYS_aminus, cvec);
         val U : unit = genCallClosure cvec;
       in
         genEnterIntCall (cvec, 2) (* added SPF 28/6/95 *)
       end;

     val U : unit = genCase (limit, cvec);
     val U : unit = decsp ();
 
     (* Addresses are relative to the first entry in the vector. *)
     val startVec : addrs = ic cvec;
     
     (* Set each address in the vector to point to the default - they
         will be overwritten by the actual address later. *)
     val defaultAddr : int = (limit + 1) * 2;
     
     val U : unit =
       forLoop (fn (_ : int) => genWord (defaultAddr, cvec)) 0 limit;

     (* The default case, if any, follows the case statement. *)
     val U : unit =
       case #default pt of
         CodeNil => ()
       | c       => gencde (c, whereto, tailKind, matchFailFn, loopAddr);

     val U : unit = exited := false;

     (* Now generate the code for each of the cases. *)
     fun genEachCase ([] : ('a codetree * int list) list) : unit = ()
       | genEachCase ((body : 'a codetree, matches : int list) :: otherCases) : unit =
       let
	 (* First exit from the previous case or the default if
	    this is the first. *)
	 val U : unit = 
	   lastEndJump :=
	     linkLabels
	       (!lastEndJump,
		if !exited then noJump else putBranchInstruction(jump, cvec),
		cvec);
		
	 (* Remove the result - the last case will leave it. *)
	 val U : unit = case whereto of ToStack => decsp () | NoResult => ();
	 
	 (* Now put the address of this code into the table if
	    an entry has not already been set. If it has the new
	    entry should be ignored. It is NOT an error (just
	    redundant). *)
	 fun genEachMatch (i: int) =
	 let
	   val entryAddr : addrs = addrPlus(startVec, (i - #min pt) * 2);
	 in
	   if getBytes(2, entryAddr, cvec) = defaultAddr
	   then putBytes(addrMinus(ic cvec, startVec), 2, entryAddr, cvec)
	   else ()
	 end
	    
	 val U : unit = apply genEachMatch matches;

	 (* Generate code for this case *)
	 val U : unit = exited := false;
	 val U : unit = gencde (body, whereto, tailKind, matchFailFn, loopAddr);
       in
	 genEachCase otherCases
       end; (* genEachCase *)
         
     val U : unit = genEachCase (#cases pt);
     
     (* Finally go down the list of exit labels pointing them to here. *)
     val U : unit = fixup (!lastEndJump, cvec);
   in
     exited := false
   end (* genDenseCase *)

   (* Generate a sparse case. *)
   and genSparseCase
        (pt : 'a caseForm,
         whereto : whereto,
         tailKind : tail,
         matchFailFn : unit -> unit,
		 loopAddr) : unit =
   let
     (* Have already dealt with cases where there is only one case. *)
     (* If there is no default case the results are all
        void for the type-checking to work so we don't
        generate them. *)
     val whereto = case #default pt of CodeNil => NoResult | _ => whereto;

      (* The exit jumps are chained together. *)
      val lastEndJump = ref noJump;

      fun caseCode ([], othersExited : bool) : bool = othersExited
        | caseCode ((body : 'a codetree, matches : int list) :: otherCases, othersExited : bool) : bool =
      let
        fun putInCases ([]: int list) : labels = 
              raise InternalError "putInCases: no labels"
          | putInCases (c :: cs) : labels =
        let
          val lastOne : bool = case cs of [] => true | _ => false;
          
          (* Is this really safe? What about multiple side-effects? SPF *)
          val U : unit = gencde (#test pt, ToStack, NotEnd, matchFailFn, loopAddr);
            
          (* Push the value to be compared. *)
          val U : unit = pushConst(toWord c, cvec);
            
          (* Compare them. If this is the last one compare for equality and
             so skip to the next case if it is not equal, if there are more
             compare for inequality and skip the other tests if it matches. *)
          val U : unit =
            pushConst (ioOp (if lastOne then POLY_SYS_int_eq else POLY_SYS_int_neq), cvec);
               
          val U : unit = genCallClosure cvec;
          val U : unit = genEnterIntCall(cvec, 2); (* added SPF 28/6/95 *)
          
          val lab : labels = putBranchInstruction (jumpFalse, cvec);
          val U : unit     = decsp (); (* Remove result of test. *)
        in (* body of putInCases *)
	  if lastOne
	  then lab (* last one - skip if value does not match. *)
	  else let
	    (* More than one. If this one matches skip the other tests. *)
	    (* Drop through to other tests if it does not match. *)
	    val rLab : labels = putInCases cs;
	    val U : unit = fixup (lab, cvec);
	  in
	    rLab
	  end
        end; (* putInCases *)
            
        val lab : labels = putInCases matches;
        
        (* Now the expression. *)
        val U : unit = gencde(body, whereto, tailKind, matchFailFn, loopAddr);
        val thisHasExited : bool = !exited;
        
        (* Remove the result - the default case will leave it. *)
        val U : unit = case whereto of ToStack => decsp () | NoResult => ();
        
        val U : unit = 
          lastEndJump :=
             linkLabels
               (!lastEndJump,
                if thisHasExited then noJump else putBranchInstruction(jump, cvec),
                cvec);
                
        (* Now the next case. *)
        val U : unit = fixup (lab, cvec);
        val U : unit = exited := false;
      in
        caseCode (otherCases, othersExited andalso thisHasExited)
      end; (* caseCode *)

      (* First the cases. *)
      val casesExited : bool = caseCode (#cases pt, true);
      
      val U : unit = exited := false;
      
      val U : unit =
        case #default pt of
          CodeNil => ()
        | c => (* put in the default *)
	    let
	      val U : unit = gencde(#default pt, whereto, tailKind, matchFailFn, loopAddr);
	    in
	      exited := (!exited andalso casesExited)
	    end;
    in
      (* Finally go down the list of exit labels pointing them to here. *)
      fixup (!lastEndJump, cvec)
    end

   (* Mutually recursive declarations. May be either procedures, constants
      or reccons (from type constructors). Recurse down the list pushing the
      addresses of the closure vectors, then unwind the recursion and fill them in. *)
   and genMutualDecs ([], matchFailFn) : unit = ()
     | genMutualDecs (Declar decl :: otherDecs, matchFailFn) : unit =
     (
       case #value decl of
         Lambda lam =>
	   if not (#makeClosure lam)
	   then let (* Static link. *)
	     (* Create a code-segment and put it in the table in case of
		 mutually recursive references. *)
	     val newCode : code = codeCreate(true, #name lam);
	     val U : unit = STRETCHARRAY.update (decVec, #addr decl, ProcConst newCode);
	     (* Deal with any other possible references. *)
	     val U : unit = genMutualDecs (otherDecs, matchFailFn);
	   in
	     (* Can now process this procedure since we have made an entry
		in the table for everything it could refer to. *)
	      genSlProc (lam, newCode)
	   end
	   
	   else let (* Closure. *)
	     fun doRest () : unit  = 
	     let
	       val U : unit = STRETCHARRAY.update (decVec, #addr decl, StackAddr (! realstackptr));
	     in
	       (* Now time to do the other closures. *)
	       genMutualDecs (otherDecs, matchFailFn)
	     end
	   in
	     genProc (lam, true, doRest, matchFailFn)
	   end
	 
       | dec => (* constants or reccons.*)
	 let
	   val U : unit = gencde (dec, ToStack, NotEnd, matchFailFn, NONE);
	   val U : unit = STRETCHARRAY.update (decVec, #addr decl, StackAddr (!realstackptr));
	 in
	   genMutualDecs (otherDecs, matchFailFn)
	 end
     )
     | genMutualDecs _ : unit =
        raise InternalError "genMutualDecs: mutual declaration list contains non-declaration";
     
    (* Closure procedures must start with ``enterIntProc''. This is actually a
       break-point instruction in the machine-code instruction set to make sure
       that the code is interpreted. It is a no-op if we are already
       interpreting. *)
    val U : unit = genEnterIntProc (cvec, numOfArgs); (* SPF 23/6/95 *)

   (* Generate the procedure. *)
   (* Assume we always want a result. There is otherwise a problem if the
      called routine returns a result of type void (i.e. no result) but the
      caller wants a result (e.g. the identity function). *)
    val U : unit = gencde (pt, ToStack, EndOfProc, matchFailed, NONE);

    val U : unit = if !exited then () else genReturn (numOfArgs, cvec);
  in (* body of codegen *)
   (* Having code-generated the body of the procedure, it is copied
      into a new data segment. *)
   copyCode cvec
 end (* codegen *);


  fun gencode (pt: 'a codetree) : unit -> word =
    case pt of
      Lambda lam => (* We are compiling a procedure. *)
      let
        (* It is not essential to treat this specially, but it saves generating
           a piece of code whose only function is to return the address of the
           procedure. *)
        (* make the code buffer for the new procedure. *)
        val newCode : code = codeCreate (false, #name lam);

        (* The only global references are recursive ones (?) *)
        fun loadRecLink (level : int, addr : int) : slValue =
          Recursive newCode;

        (* This procedure must have no non-local references. *)
        val closureAddr : address =
          codegen (#body lam, newCode, loadRecLink, #numArgs lam);
            
        val res : word = toWord closureAddr;
      in
        (* Result is a procedure which returns the address of the procedure. *)
        fn () => res
      end
      
   | _ =>
     let (* Compile a top-level expression. *)
       val newCode : code = codeCreate(false, "<top level>");

        (* There ane *no*  global references at all *)
        fun loadRecLink (level : int, addr : int) : slValue =
          raise InternalError "top level reached";

       val closureAddr : address =
	 codegen (pt, newCode, loadRecLink, 0)
     in
       (* Result is a procedure to execute the code. *)
       fn () => call (closureAddr, toWord ())
     end; (* gencode *)
end (* GCODE functor body *)

end; (* structure-level let *)
