(*
	Copyright (c) 2000
		Cambridge University Technical Services Limited

    Modified David C. J. Matthews 2008

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

(* Basic code-tree data structure.  This was previously partly in GCODE.ML
   and partly in CODETREE.ML. *)

structure BaseCodeTree: BaseCodeTreeSig =
struct
    open Address

    datatype inlineStatus =
        NonInline
    |   MaybeInline
    |   SmallFunction
    |   OnlyInline;
    
    datatype codetree =
        MatchFail    (* Pattern-match failure *)
    
    |   AltMatch of codetree * codetree(* Pattern-match alternative choices *)

    |   Declar of declarForm (* Make a local declaration or push an argument *)
     
    |   Newenv of codetree list (* Start a block *)

    |   Constnt of machineWord (* Load a constant *)

    |   Extract of loadForm (* Get a local variable, an argument or a closure value *)
    
    |   Indirect of 
    { (* Load a value from a heap record *)
        base:   codetree,
        offset: int
    }
    
    | Eval of (* Evaluate a function with an argument list. *)
    {
        function:  codetree,
        argList:   codetree list,
        earlyEval: bool
    }
    
    | Lambda of lambdaForm (* Lambda expressions. *)
    
    | MutualDecs of codetree list (* Set of mutually recursive declarations. *)

    | Cond of codetree * codetree * codetree (* If-statement *)

    | Case of (* Case expressions *)
    {
        cases   : (codetree * int list) list,
        test    : codetree,
        default : codetree,
        min     : int,
        max     : int
    }
    
    | BeginLoop of codetree * codetree list(* Start of tail-recursive inline function. *)

    | Loop of codetree list (* Jump back to start of tail-recursive function. *)
    
    
    | Raise of codetree (* Raise an exception *)

    | Ldexc (* Load the exception (used at the start of a handler) *)
    
    | Handle of (* Exception handler *)
    { (* Exception handler. *)
        exp      : codetree,
        taglist  : codetree list,
        handler  : codetree
    }
    
    | Recconstr of codetree list (* Records (tuples) *)

    | Container of int (* Create a container for a tuple on the stack. *)
    
    | SetContainer of (* Copy a tuple to a container. *)
    {
        container: codetree,
        tuple:     codetree,
        size:      int
    }
    
    | TupleFromContainer of codetree * int (* Make a tuple from the contents of a container. *)

    | Global of optVal (* Global value *)

    | CodeNil
    
    and optVal = (* Global values - Also used in the optimiser. *)
        JustTheVal of codetree
    
    |   ValWithDecs of {general : codetree, decs : codetree list}
    
    |   OptVal of
    {
        (* Expression to load this value - always a constant in global values. *)
        general : codetree,
        (* If it is not CodeNil it is the code which generated the general
           value - either an inline procedure, a type constructor or a tuple. *)
        special : codetree,
        (* Environment for the special value. *)
        environ : loadForm * int * int -> optVal,
        (* Declarations to precede the value - Always nil for global values. *)
        decs : codetree list,
        (* A reference which is used to detect recursive inline expansions. *)
        recCall: bool ref
    }
    
    withtype loadForm = 
    { (* Load a value. *)
        addr : int, 
        level: int, 
        fpRel: bool,
        lastRef: bool
    }
    
    and declarForm = 
    { (* Declare a value or push an argument. *)
        value:      codetree,
        addr:       int,
        references: int
    }
    
    and diadic = codetree * codetree
    
    and triadic =  codetree * codetree * codetree
    
    and lambdaForm =
    { (* Lambda expressions. *)
        body          : codetree,
        isInline      : inlineStatus,
        name          : string,
        closure       : codetree list,
        numArgs  	 : int,
        level         : int,
        closureRefs   : int,
        makeClosure   : bool
    };

    open PrettyPrinter

    val ioOp : int -> machineWord = RunCall.run_call1 RuntimeCalls.POLY_SYS_io_operation;

    fun stringOfWord w =
    if isShort w
    then Word.toString (toShort w)
    else if isIoAddress(toAddress w)
	then (* RTS call - print the number. *)
		let
			fun matchIo n =
				if n = 256 then raise Misc.InternalError "Unknown RTS entry"
				else if wordEq (w, ioOp n)
				then " RTS" ^ Int.toString n
				else matchIo (n+1)
		in
			matchIo 0
		end
	else if isWords(toAddress w) andalso Word.toInt(Address.length(toAddress w)) >= 1
	then (* If it's the closure of a function try to print that. *)
		let
			val firstWord = loadWord(toAddress w, toShort 0)
			val doCall: int*machineWord -> string
				= RunCall.run_call2 RuntimeCalls.POLY_SYS_process_env
		in
			if not (isShort firstWord) andalso isCode(toAddress firstWord)
			then " (" ^ doCall(105, firstWord) ^ ")" (* Get the function name. *)
			else "<long>"
		end
	else "<long>";
  
    fun pretty (pt : codetree, pprint: prettyPrinter) : unit =
    let
        fun pList ([]: 'b list) (sep: string) (disp: 'b->unit) = ()
        | pList [h]    sep disp = disp h
        | pList (h::t) sep disp =
        (
            ppBeginBlock pprint (0, false);
            disp h;
            ppBreak pprint (0, 0);
            ppAddString pprint sep;
            ppEndBlock pprint ();
            ppBreak pprint (1, 0);
            pList t sep disp
        );
        
        fun printList start lst sep =
        (
            ppBeginBlock pprint (1, true);
            ppAddString pprint (start ^ "(");
            pList lst sep (fn x => (pretty (x,  pprint)));
            ppBreak pprint (0, 0);
            ppAddString pprint (")");
            ppEndBlock pprint ()
        );
        
        fun printMonad name pt =
        (
            ppBeginBlock pprint (1, true);
            ppAddString pprint (name^"(");
            pretty (pt,  pprint);
            ppBreak pprint (0, 0);
            ppAddString pprint (")");
            ppEndBlock pprint ()
        );
    
        fun printDiad name (f,s) =
        (
            ppBeginBlock pprint (1, true);
            ppAddString pprint (name^"(");
            pretty (f,  pprint);
            ppAddString pprint ", ";
            ppBreak pprint (0, 0);
            pretty (s, pprint);
            ppBreak pprint (0, 0);
            ppAddString pprint (")");
            ppEndBlock pprint ()
        );
        
        fun printTriad name (f,s,t) =
        (
            ppBeginBlock pprint (1, true);
            ppAddString pprint (name^"(");
            pretty(f, pprint);
            ppAddString pprint ", ";
            ppBreak pprint (0, 0);
            pretty(s, pprint);
            ppAddString pprint ", ";
            ppBreak pprint (0, 0);
            pretty (t, pprint);
            ppBreak pprint (0, 0);
            ppAddString pprint (")");
            ppEndBlock pprint ()
        );
        
    in
        case pt of
            CodeNil => ppAddString pprint "NIL"
        
        | MatchFail => ppAddString pprint "MATCHFAIL"
        
        | AltMatch pair => printDiad "ALTMATCH" pair
        
        | Eval {function, argList, earlyEval} =>
        (
            ppBeginBlock pprint (3, false);
            pretty (function, pprint);
            ppBreak pprint (0, 0);
            if earlyEval
            then
            (
                ppAddString pprint "{early}";
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
            pretty (value, pprint);
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
            pretty (base, pprint);
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
            ppAddString pprint ( "CL="  ^ Bool.toString makeClosure);
            ppAddString pprint (" CR="  ^ Int.toString closureRefs);
            ppAddString pprint (" LEV=" ^ Int.toString level);
            ppAddString pprint (" ARGS=" ^ Int.toString numArgs);
            printList " CLOS=" closure ",";
            ppBreak pprint (1, 0);
            pretty (body, pprint);
            ppAddString pprint "){LAMBDA}";
            ppEndBlock pprint ()
        end
        
        | Constnt w => ppAddString pprint ("LIT" ^ stringOfWord w)
        
        | Cond triple => printTriad "IF" triple
        
        | Newenv ptl => printList "BLOCK" ptl ";"
        
        | BeginLoop(loopExp, args) =>
        (
            ppBeginBlock pprint (3, false);
            printList "BEGINLOOP" args ",";
            ppBreak pprint (0, 0);
            ppAddString pprint "(";
            ppBreak pprint (0, 0);
            pretty (loopExp, pprint);
            ppBreak pprint (0, 0);
            ppAddString pprint ")";
            ppEndBlock pprint ()
        )
        
        | Loop ptl => printList "LOOP" ptl ","
        
        | Raise c => printMonad "RAISE" c
        
        | Handle {exp, taglist, handler} =>
        (
            ppBeginBlock pprint (3, false);
            ppAddString pprint "HANDLE(";
            pretty (exp, pprint);
            ppAddString pprint "WITH";
            ppBreak pprint (1, 0);
            pretty (handler, pprint);
            ppAddString pprint ")";
            ppEndBlock pprint ()
        )
        
        | Ldexc => ppAddString pprint "LDEXC"
        
        | Case {cases, test, default, min, max} =>
        (
            ppBeginBlock pprint (1, true);
            ppAddString pprint
                (concat ["CASE ", Int.toString min, "-", Int.toString max, "(" ]);
            pretty (test, pprint);
            ppAddString pprint ")";
            ppBreak pprint (1, 0);
            ppAddString pprint "(";
            ppBreak pprint (1, 0);
            pList cases ","
                (fn (exp : codetree, labels : int list) =>
                    (
                    ppBeginBlock pprint (1, true);
                    List.app (fn l =>
                     (
                      ppAddString pprint (Int.toString l ^ ":");
                      ppBreak pprint (1, 0)
                     )
                    ) labels;
                    pretty (exp, pprint);
                    ppEndBlock pprint ()
                    )
                );
            case default of
                CodeNil => ()
            |   _ =>
            (
                ppBreak pprint (1, 0);
                ppBeginBlock pprint (1, false);
                ppAddString pprint "ELSE:";
                ppBreak pprint (1, 0);
                pretty (default, pprint);
                ppEndBlock pprint ()
            );
            ppBreak pprint (1, 0);
            ppAddString pprint (") {"^"CASE"^"}");
            ppEndBlock pprint ()
        )
        
        | MutualDecs ptl => printList "MUTUAL" ptl " AND "
        
        | Recconstr ptl => printList "RECCONSTR" ptl ","
        
        | Container size => ppAddString pprint ("CONTAINER " ^ Int.toString size)
        
        | SetContainer{container, tuple, size} =>
        (
            ppBeginBlock pprint (3, false);
            ppAddString pprint ("SETCONTAINER(" ^ Int.toString size ^ ", ");
            pretty (container, pprint);
            ppBreak pprint (0, 0);
            ppAddString pprint ",";
            ppBreak pprint (1, 0);
            pretty (tuple,  pprint);
            ppBreak pprint (0, 0);
            ppAddString pprint ")";
            ppEndBlock pprint ()
        )
        
        | TupleFromContainer (container, size) =>
        (
            ppBeginBlock pprint (3, false);
            ppAddString pprint ("TUPLECONTAINER(" ^ Int.toString size ^ ", ");
            ppBreak pprint (0, 0);
            pretty (container,  pprint);
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
    end (* pretty *)
    
    and prettyOptVal (ov : optVal, pprint : prettyPrinter) =
    (
        ppAddString pprint "(";
        pretty (optGeneral ov, pprint);
        ppAddString pprint ", ";
        ppBreak pprint (1, 0);
        pretty (optSpecial ov, pprint);
        ppBreak pprint (1, 0);
        ppAddString pprint ")"
    )
  
    and optGeneral (OptVal {general,...})       = general 
      | optGeneral (ValWithDecs {general, ...}) = general
      | optGeneral (JustTheVal ct)              = ct
      
    and optSpecial (OptVal {special,...}) = special
      | optSpecial _                      = CodeNil
    
    (* Test whether a piece of code is "small".  This is used to decide whether a function
       should be made inline.  *)
    fun isSmall (pt, maxSize) = 
    let
        fun sizeList [] = 0
        |   sizeList (c::cs) = size c + sizeList cs
        
        and sizeCaseList []           = 0
        |   sizeCaseList ((c,il)::cs) = size c + List.length il + sizeCaseList cs

        and sizeOptVal (OptVal {general,...})       = size general 
        |   sizeOptVal (ValWithDecs {general, ...}) = size general
        |   sizeOptVal (JustTheVal ct)              = size ct
        
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
            | Lambda {body,numArgs,...}		  => size body + numArgs
            | Eval {function,argList,...}     => size function + sizeList argList + 2
            | MutualDecs decs                 => sizeList decs (*!maxInlineSize*)
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
            | Global glob                     => sizeOptVal glob
            | Case {test,default,cases,...}   =>
                size test + size default + sizeCaseList cases
            ;
        in
        (* We previously treated functions which only contained Lambdas as always small.
           I've now taken that out because it caused the code to blow up if the Lambda
           turned out to be a large function (e.g. fun f x = fn y => ...BIGCODE...).
           There may be a case for lifting the inner lambda out much as we already do
           with fun declarations but this needs to be done in the front end.  DCJM 14/3/02.  *)
        size pt < maxSize
    end;


end;
