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

    open Pretty

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
  
    fun pretty (pt : codetree) : pretty =
    let
        fun pList ([]: 'b list) (sep: string) (disp: 'b->pretty) = []
        | pList [h]    sep disp = [disp h]
        | pList (h::t) sep disp =
            PrettyBlock (0, false, [],
                [
                    disp h,
                    PrettyBreak (0, 0),
                    PrettyString sep
                ]
            ) ::
            PrettyBreak (1, 0) ::
            pList t sep disp
        
        fun printList start lst sep : pretty =
            PrettyBlock (1, true, [],
                PrettyString (start ^ "(") ::
                pList lst sep (fn x => (pretty x)) @
                [ PrettyBreak (0, 0), PrettyString (")") ]
            )
        
        fun printMonad name pt =
            PrettyBlock (1, true, [],
                [
                    PrettyString (name^"("),
                    pretty pt,
                    PrettyBreak (0, 0),
                    PrettyString (")")
                ]
            )
    
        fun printDiad name (f,s) =
            PrettyBlock (1, true, [],
                [
                    PrettyString (name^"("),
                    pretty f,
                    PrettyString ", ",
                    PrettyBreak (0, 0),
                    pretty s,
                    PrettyBreak (0, 0),
                    PrettyString (")")
                ]
            )
        
        fun printTriad name (f,s,t) =
            PrettyBlock (1, true, [],
                [
                    PrettyString (name^"("),
                    pretty f,
                    PrettyString ", ",
                    PrettyBreak (0, 0),
                    pretty s,
                    PrettyString ", ",
                    PrettyBreak (0, 0),
                    pretty t,
                    PrettyBreak (0, 0),
                    PrettyString (")")
                ]
            )
        
    in
        case pt of
            CodeNil => PrettyString "NIL"
        
        | MatchFail => PrettyString "MATCHFAIL"
        
        | AltMatch pair => printDiad "ALTMATCH" pair
        
        | Eval {function, argList, earlyEval} =>
            PrettyBlock (3, false, [],
                pretty function ::
                PrettyBreak (0, 0) ::
                (
                    if earlyEval
                    then [ PrettyString "{early}", PrettyBreak (0, 0) ]
                    else []
                ) @
                [ printList "$" argList "," ]
            )
        
        | Declar {value, addr, references} =>
            PrettyBlock (1, false, [],
                [
                    PrettyString (concat
                        ["DECL #", Int.toString addr, "{", Int.toString references, " uses} ="]),
                    PrettyBreak (1, 0),
                    pretty value
                ]
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
                PrettyString str
            end
        
        | Indirect {base, offset} =>
            let
                val str = "INDIRECT(" ^ Int.toString offset ^ ", ";
            in
                PrettyBlock(0, false, [],
                    [ PrettyString str, pretty base, PrettyString ")" ]
                )
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
                PrettyBlock (1, true, [],
                    [
                        PrettyString ("LAMBDA"^inl^"("),
                        PrettyBreak (1, 0),
                        PrettyString name,
                        PrettyBreak (1, 0),
                        PrettyString ( "CL="  ^ Bool.toString makeClosure),
                        PrettyString (" CR="  ^ Int.toString closureRefs),
                        PrettyString (" LEV=" ^ Int.toString level),
                        PrettyString (" ARGS=" ^ Int.toString numArgs),
                        printList " CLOS=" closure ",",
                        PrettyBreak (1, 0),
                        pretty body,
                        PrettyString "){LAMBDA}"
                    ]
                )
            end
        
        | Constnt w => PrettyString ("LIT" ^ stringOfWord w)
        
        | Cond triple => printTriad "IF" triple
        
        | Newenv ptl => printList "BLOCK" ptl ";"
        
        | BeginLoop(loopExp, args) =>
            PrettyBlock (3, false, [],
                [
                    printList "BEGINLOOP" args ",",
                    PrettyBreak (0, 0),
                    PrettyString "(",
                    PrettyBreak (0, 0),
                    pretty loopExp,
                    PrettyBreak (0, 0),
                    PrettyString ")"
                ]
            )
        
        | Loop ptl => printList "LOOP" ptl ","
        
        | Raise c => printMonad "RAISE" c
        
        | Handle {exp, taglist, handler} =>
            PrettyBlock (3, false, [],
                [
                    PrettyString "HANDLE(",
                    pretty exp,
                    PrettyString "WITH",
                    PrettyBreak (1, 0),
                    pretty handler,
                    PrettyString ")"
                ]
            )
        
        | Ldexc => PrettyString "LDEXC"
        
        | Case {cases, test, default, min, max} =>
            PrettyBlock (1, true, [],
                PrettyString
                    (concat ["CASE ", Int.toString min, "-", Int.toString max, "(" ]) ::
                pretty test ::
                PrettyBreak (1, 0) ::
                PrettyString "(" ::
                PrettyBreak (1, 0) ::
                pList cases ","
                    (fn (exp : codetree, labels : int list) =>
                        PrettyBlock (1, true, [],
                            List.foldr (
                                fn (l, t) =>
                                    PrettyString (Int.toString l ^ ":") ::
                                    PrettyBreak (1, 0) :: t
                                ) [pretty exp] labels
                            )
                    ) @
                (
                    case default of
                        CodeNil => []
                    |   _ =>
                        [
                            PrettyBreak (1, 0),
                            PrettyBlock (1, false, [],
                                [
                                    PrettyString "ELSE:",
                                    PrettyBreak (1, 0),
                                    pretty default
                                ]
                            )
                        ]
                ) @
                [ PrettyBreak (1, 0), PrettyString (") {"^"CASE"^"}") ]
            )
        
        | MutualDecs ptl =>
            printList "MUTUAL" ptl " AND "
        
        | Recconstr ptl =>
            printList "RECCONSTR" ptl ","
        
        | Container size => PrettyString ("CONTAINER " ^ Int.toString size)
        
        | SetContainer{container, tuple, size} =>
            PrettyBlock (3, false, [],
                [
                    PrettyString ("SETCONTAINER(" ^ Int.toString size ^ ", "),
                    pretty container,
                    PrettyBreak (0, 0),
                    PrettyString ",",
                    PrettyBreak (1, 0),
                    pretty tuple,
                    PrettyBreak (0, 0),
                    PrettyString ")"
                ]
            )
        
        | TupleFromContainer (container, size) =>
            PrettyBlock (3, false, [],
                [
                    PrettyString ("TUPLECONTAINER(" ^ Int.toString size ^ ", "),
                    PrettyBreak (0, 0),
                    pretty container,
                    PrettyBreak (0, 0),
                    PrettyString ")"
                ]
            )
        
        | Global ov =>
            PrettyBlock (1, true, [],
                [
                    PrettyString "GLOBAL (",
                    pretty (optGeneral ov),
                    PrettyString ", ",
                    PrettyBreak (1, 0),
                    pretty (optSpecial ov),
                    PrettyBreak (1, 0),
                    PrettyString ") (*GLOBAL*)"
                ]
            )
        
        (* That list should be exhaustive! *)
    end (* pretty *)
   
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
