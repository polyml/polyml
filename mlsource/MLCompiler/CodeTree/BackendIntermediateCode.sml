(*
    Copyright (c) 2012 David C.J. Matthews

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

(* Intermediate code tree for the back end of the compiler. *)

structure BackendIntermediateCode: BackendIntermediateCodeSig =
struct
    open Address

    datatype argumentType =
        GeneralType
    |   FloatingPtType
    
    datatype backendIC =
        BICMatchFail    (* Pattern-match failure *)
    
    |   BICAltMatch of backendIC * backendIC(* Pattern-match alternative choices *)

    |   BICNewenv of bicCodeBinding list * backendIC (* Set of bindings with an expression. *)

    |   BICConstnt of machineWord (* Load a constant *)

    |   BICExtract of bicLoadForm * bool (* Get a local variable, an argument or a closure value *)

    |   BICField of {base: backendIC, offset: int }
         (* Load a field from a tuple or record *)
    
    |   BICEval of (* Evaluate a function with an argument list. *)
        {
            function:  backendIC,
            argList:   (backendIC * argumentType) list,
            resultType: argumentType
        }
    
    |   BICLambda of bicLambdaForm (* Lambda expressions. *)

    |   BICCond of backendIC * backendIC * backendIC (* If-then-else expression *)

    |   BICCase of (* Case expressions *)
        {
            cases   : (backendIC * word) list,
            test    : backendIC,
            caseType: caseType,
            default : backendIC
        }
    
    |   BICBeginLoop of (* Start of tail-recursive inline function. *)
        { loop: backendIC, arguments: (bicSimpleBinding * argumentType) list }

    |   BICLoop of (backendIC * argumentType) list (* Jump back to start of tail-recursive function. *)

    |   BICKillItems of
            (* Kill entries.  Used to mark a branch where a binding is no longer required.
               "killSet" is always an Extract with lastRef=true so the type should
               be loadForm list rather than backendIC list. *)
            { expression: backendIC, killSet: backendIC list, killBefore: bool }

    |   BICRaise of backendIC (* Raise an exception *)

    |   BICLdexc (* Load the exception (used at the start of a handler) *)

    |   BICHandle of (* Exception handler. *) { exp: backendIC, handler: backendIC }

    |   BICTuple of backendIC list (* Tuple *)

    |   BICContainer of int (* Create a container for a tuple on the stack. *)

    |   BICSetContainer of (* Copy a tuple to a container. *)
        {
            container: backendIC,
            tuple:     backendIC,
            size:      int
        }

    |   BICTupleFromContainer of backendIC * int (* Make a tuple from the contents of a container. *)

    |   BICTagTest of { test: backendIC, tag: word, maxTag: word }

    |   BICIndirectVariable of { base: backendIC, offset: backendIC }
        (* Similar to Indirect except the offset is a variable. *)

    |   BICTupleVariable of bicVarTuple list * backendIC (* total length *)
        (* Construct a tuple using one or more multi-word items. *)

    and bicCodeBinding =
        BICDeclar  of bicSimpleBinding (* Make a local declaration or push an argument *)
    |   BICRecDecs of { addr: int, references: int, lambda: bicLambdaForm } list (* Set of mutually recursive declarations. *)
    |   BICNullBinding of backendIC (* Just evaluate the expression and discard the result. *)

    and caseType =
        CaseInt
    |   CaseWord
    |   CaseTag of word

    and bicVarTuple =
        BICVarTupleSingle of { source: backendIC, destOffset: backendIC }
    |   BICVarTupleMultiple of
            { base: backendIC, length: backendIC, destOffset: backendIC, sourceOffset: backendIC }

    and bicLoadForm =
        BICLoadStack of int | BICLoadClosure of int

    withtype bicSimpleBinding = 
    { (* Declare a value or push an argument. *)
        value:      backendIC,
        addr:       int,
        references: int
    }

    and bicLambdaForm =
    { (* Lambda expressions. *)
        body          : backendIC,
        name          : string,
        closure       : backendIC list,
        argTypes      : argumentType list,
        resultType    : argumentType,
        level         : int,
        closureRefs   : int,
        localCount    : int,
        makeClosure   : bool,
        argLifetimes  : int list
    }

    open Pretty

    fun pList ([]: 'b list, _: string, _: 'b->pretty) = []
    |   pList ([h],    _, disp) = [disp h]
    |   pList (h::t, sep, disp) =
        PrettyBlock (0, false, [],
            [
                disp h,
                PrettyBreak (0, 0),
                PrettyString sep
            ]
        ) ::
        PrettyBreak (1, 0) ::
        pList (t, sep, disp)

    fun pretty (pt : backendIC) : pretty =
    let
        
        fun printList(start, lst, sep) : pretty =
            PrettyBlock (1, true, [],
                PrettyString (start ^ "(") ::
                pList(lst, sep, pretty) @
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

        fun prettyArgType GeneralType = PrettyString "G"
        |   prettyArgType FloatingPtType = PrettyString "F"
        
        fun prettyArg (c, t) =
                PrettyBlock(1, false, [], [pretty c, PrettyBreak (1, 0), prettyArgType t])

        fun prettyArgs(start, lst, sep) : pretty =
            PrettyBlock (1, true, [],
                PrettyString (start ^ "(") ::
                pList(lst, sep, prettyArg) @
                [ PrettyBreak (0, 0), PrettyString (")") ]
            )

    in
        case pt of
            BICMatchFail => PrettyString "MATCHFAIL"
        
        |   BICAltMatch pair => printDiad "ALTMATCH" pair

        |   BICEval {function, argList, resultType} =>
            let
                val prettyArgs =
                    PrettyBlock (1, true, [],
                        PrettyString ("$(") ::
                        pList(argList, ",", prettyArg) @
                        [ PrettyBreak (0, 0), PrettyString (")") ]
                    )
            in
                PrettyBlock (3, false, [],
                    [ pretty function, PrettyBreak(1, 0), prettyArgType resultType, PrettyBreak(1, 0), prettyArgs ]
                )
            end

        |   BICKillItems {expression, killSet, killBefore} =>
            PrettyBlock(1, false, [],
                [
                    PrettyString(if killBefore then "KILLBEFORE(" else "KILLAFTER("),
                    PrettyBreak(1, 0),
                    pretty expression,
                    PrettyBreak(1, 0),
                    printList (" KILL=", killSet, ","),
                    PrettyString ")"
                ]
            )
         
        |   BICExtract (BICLoadStack addr, lastRef) =>
            let
                val last = if lastRef then ", last" else "";
                val str : string =
                    if addr < 0
                    then concat ["PARAM(", Int.toString (~ addr), last, ")"]
                    else concat ["LOCAL(", Int.toString addr, last, ")"]
            in
                PrettyString str
            end

       |   BICExtract (BICLoadClosure addr, lastRef) =>
            let
                val last = if lastRef then ", last" else "";
                val str : string =
                    concat ["CLOS(", Int.toString addr, last, ")"]
            in
                PrettyString str
            end

        |   BICField {base, offset} =>
            let
                val str = "INDIRECT(" ^ Int.toString offset ^ ", ";
            in
                PrettyBlock(0, false, [],
                    [ PrettyString str, pretty base, PrettyString ")" ]
                )
            end
        
        |   BICLambda {body, name, closure, argTypes, level, closureRefs,
                  makeClosure, resultType, localCount, argLifetimes} =>
            let
                fun prettyArgTypes [] = []
                |   prettyArgTypes [last] = [prettyArgType last]
                |   prettyArgTypes (hd::tl) = prettyArgType hd :: PrettyBreak(1, 0) :: prettyArgTypes tl
                fun prettyArgLife [] = []
                |   prettyArgLife [last] = [PrettyString(Int.toString last)]
                |   prettyArgLife (hd::tl) = PrettyString(Int.toString hd) :: PrettyBreak(1, 0) :: prettyArgLife tl
            in
                PrettyBlock (1, true, [],
                    [
                        PrettyString ("LAMBDA("),
                        PrettyBreak (1, 0),
                        PrettyString name,
                        PrettyBreak (1, 0),
                        PrettyString ( "CL="  ^ Bool.toString makeClosure),
                        PrettyString (" CR="  ^ Int.toString closureRefs),
                        PrettyString (" LEV=" ^ Int.toString level),
                        PrettyString (" LOCALS=" ^ Int.toString localCount),
                        PrettyBreak(1, 0),
                        PrettyBlock (1, false, [], PrettyString "ARGS=" :: prettyArgTypes argTypes),
                        PrettyBreak(1, 0),
                        PrettyBlock (1, false, [], PrettyString "ARGLIVES=" :: prettyArgLife argLifetimes),
                        PrettyBreak(1, 0),
                        PrettyBlock (1, false, [], [PrettyString "RES=", prettyArgType resultType]),
                        printList (" CLOS=", (*map BICExtract *)closure, ","),
                        PrettyBreak (1, 0),
                        pretty body,
                        PrettyString "){LAMBDA}"
                    ]
                )
            end
        
        |   BICConstnt w => PrettyString (stringOfWord w)
        
        |   BICCond triple => printTriad "IF" triple
        
        |   BICNewenv(decs, final) =>
            PrettyBlock (1, true, [],
                PrettyString ("BLOCK" ^ "(") ::
                pList(decs, ";", prettyBinding) @
                [ PrettyBreak (1, 0), pretty final, PrettyBreak (0, 0), PrettyString (")") ]
            )

        |   BICBeginLoop{loop=loopExp, arguments=args } =>
            let
                fun prettyArg (c, t) =
                    PrettyBlock(1, false, [],
                        [prettySimpleBinding c, PrettyBreak (1, 0), prettyArgType t])
            in
                PrettyBlock (3, false, [],
                    [
                        PrettyBlock (1, true, [],
                            PrettyString ("BEGINLOOP(") ::
                            pList(args, ",", prettyArg) @
                            [ PrettyBreak (0, 0), PrettyString (")") ]
                        ),
                        PrettyBreak (0, 0),
                        PrettyString "(",
                        PrettyBreak (0, 0),
                        pretty loopExp,
                        PrettyBreak (0, 0),
                        PrettyString ")"
                    ]
                )
            end
        
        |   BICLoop ptl => prettyArgs("LOOP", ptl, ",")
        
        |   BICRaise c => printMonad "RAISE" c
        
        |   BICHandle {exp, handler, ...} =>
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
        
        |   BICLdexc => PrettyString "LDEXC"
        
        |   BICCase {cases, test, default, caseType} =>
            PrettyBlock (1, true, [],
                PrettyString
                    (concat ["CASE ",
                        case caseType of CaseInt => "INT" | CaseWord => "WORD" | CaseTag n => "TAG " ^ Word.toString n,
                        " (" ]) ::
                pretty test ::
                PrettyBreak (1, 0) ::
                PrettyString "(" ::
                PrettyBreak (1, 0) ::
                pList(cases, ",",
                    fn (exp, label : word) =>
                        PrettyBlock (1, true, [],
                            [
                                PrettyString (Word.toString label ^ ":"),
                                PrettyBreak (1, 0),
                                pretty exp
                            ])
                    ) @
                [
                    PrettyBreak (1, 0),
                    PrettyBlock (1, false, [],
                        [
                            PrettyString "ELSE:",
                            PrettyBreak (1, 0),
                            pretty default
                        ]
                    ),
                    PrettyBreak (1, 0), 
                    PrettyString (") {"^"CASE"^"}")
                ]
            )
         
        |   BICTuple ptl => printList("RECCONSTR", ptl, ",")
        
        |   BICContainer size => PrettyString ("CONTAINER " ^ Int.toString size)
        
        |   BICSetContainer{container, tuple, size} =>
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
        
        |   BICTupleFromContainer (container, size) =>
            PrettyBlock (3, false, [],
                [
                    PrettyString ("TUPLECONTAINER(" ^ Int.toString size ^ ","),
                    PrettyBreak (0, 0),
                    pretty container,
                    PrettyBreak (0, 0),
                    PrettyString ")"
                ]
            )

        |   BICTagTest { test, tag, maxTag } =>
            PrettyBlock (3, false, [],
                [
                    PrettyString (concat["TAGTEST(", Word.toString tag, ", ", Word.toString maxTag, ","]),
                    PrettyBreak (1, 0),
                    pretty test,
                    PrettyBreak (0, 0),
                    PrettyString ")"
                ]
            )

        |   BICIndirectVariable { base, offset } =>
            PrettyBlock (3, false, [],
                [
                    PrettyString("IndirectVariable ("),
                    PrettyBreak (1, 0),
                    pretty base,
                    PrettyBreak (0, 0),
                    pretty offset,
                    PrettyBreak (0, 0),
                    PrettyString ")"
                ]
            )

        |   BICTupleVariable(vars, length) =>
            let
                fun printTup(BICVarTupleSingle{source, destOffset}) =
                    PrettyBlock(3, false, [],
                    [
                        PrettyString "Single (",
                        pretty source,
                        PrettyBreak (0, 0),
                        pretty destOffset,
                        PrettyBreak (0, 0),
                        PrettyString ")"
                    ]
                    )
                |   printTup(BICVarTupleMultiple{base, length, destOffset, sourceOffset}) = 
                    PrettyBlock(3, false, [],
                    [
                        PrettyString "Multiple (",
                        pretty base,
                        PrettyBreak (0, 0),
                        pretty length,
                        PrettyBreak (0, 0),
                        pretty sourceOffset,
                        PrettyBreak (0, 0),
                        pretty destOffset,
                        PrettyBreak (0, 0),
                        PrettyString ")"
                    ]
                    )
            in
                PrettyBlock (3, false, [],
                [
                    PrettyString "TupleVariable (",
                    PrettyBreak (1, 0),
                    pretty length,
                    PrettyBreak (0, 0)
                ] @ pList(vars, ",", printTup) @
                [
                    PrettyBreak (0, 0),
                    PrettyString ")"
                ]
            )
            end
            

        (* That list should be exhaustive! *)
    end (* pretty *)

    and prettyBinding(BICDeclar dec) = prettySimpleBinding dec
       
    |   prettyBinding(BICRecDecs ptl) =
        let
            fun prettyRDec {lambda, addr, references} =
            PrettyBlock (1, false, [],
                [
                    PrettyString (concat
                        ["DECL #", Int.toString addr, "{", Int.toString references, " uses} ="]),
                    PrettyBreak (1, 0),
                    pretty(BICLambda lambda)
                ]
            )
        in
            PrettyBlock (1, true, [],
                PrettyString ("MUTUAL" ^ "(") ::
                pList(ptl, " AND ", prettyRDec) @
                [ PrettyBreak (0, 0), PrettyString (")") ]
            )
        end
    |   prettyBinding(BICNullBinding c) = pretty c

    and prettySimpleBinding{value, addr, references} =
        PrettyBlock (1, false, [],
            [
                PrettyString (concat
                    ["DECL #", Int.toString addr, "{", Int.toString references, " uses} ="]),
                PrettyBreak (1, 0),
                pretty value
            ]
        )

    structure Sharing =
    struct
        type backendIC = backendIC
        and  caseType = caseType
        and  pretty = pretty
        and  argumentType = argumentType
        and  bicVarTuple = bicVarTuple
        and  bicCodeBinding = bicCodeBinding
        and  bicSimpleBinding = bicSimpleBinding
    end

end;
