(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Modified David C. J. Matthews 2008-2010

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
    |   OnlyInline

    datatype argumentType =
        GeneralType
    |   FloatingPtType
    
    datatype codetree =
        MatchFail    (* Pattern-match failure *)
    
    |   AltMatch of codetree * codetree(* Pattern-match alternative choices *)

    |   Declar of declarForm (* Make a local declaration or push an argument *)
     
    |   Newenv of codetree list (* Start a block *)

    |   Constnt of machineWord (* Load a constant *)

    |   Extract of loadForm (* Get a local variable, an argument or a closure value *)
    
    |   Indirect of {base: codetree, offset: int }
         (* Load a value from a heap record *)
    
    |   Eval of (* Evaluate a function with an argument list. *)
        {
            function:  codetree,
            argList:   (codetree * argumentType) list,
            resultType: argumentType,
            earlyEval: bool
        }
    
    |   Lambda of lambdaForm (* Lambda expressions. *)
    
    |   MutualDecs of codetree list (* Set of mutually recursive declarations. *)

    |   Cond of codetree * codetree * codetree (* If-statement *)

    |   Case of (* Case expressions *)
        {
            cases   : (codetree * word) list,
            test    : codetree,
            caseType: caseType,
            default : codetree
        }
    
    |   BeginLoop of (* Start of tail-recursive inline function. *)
        { loop: codetree, arguments: (codetree * argumentType) list }

    |   Loop of (codetree * argumentType) list (* Jump back to start of tail-recursive function. *)

    |   KillItems of { expression: codetree, killSet: codetree list, killBefore: bool }

    |   Raise of codetree (* Raise an exception *)

    |   Ldexc (* Load the exception (used at the start of a handler) *)

    |   Handle of (* Exception handler. *) { exp: codetree, handler: codetree }

    |   Recconstr of codetree list (* Records (tuples) *)

    |   Container of int (* Create a container for a tuple on the stack. *)

    |   SetContainer of (* Copy a tuple to a container. *)
        {
            container: codetree,
            tuple:     codetree,
            size:      int
        }

    |   TupleFromContainer of codetree * int (* Make a tuple from the contents of a container. *)

    |   TagTest of { test: codetree, tag: word, maxTag: word }

    |   IndirectVariable of { base: codetree, offset: codetree }
        (* Similar to Indirect except the offset is a variable. *)

    |   TupleVariable of varTuple list * codetree (* total length *)
        (* Construct a tuple using one or more multi-word items. *)

    |   Global of optVal (* Global value *)

    |   CodeNil

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

    and caseType =
        CaseInt
    |   CaseWord
    |   CaseTag of word

    and varTuple =
        VarTupleSingle of { source: codetree, destOffset: codetree }
    |   VarTupleMultiple of
            { base: codetree, length: codetree, destOffset: codetree, sourceOffset: codetree }

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
        argTypes      : argumentType list,
        resultType    : argumentType,
        level         : int,
        closureRefs   : int,
        localCount    : int,
        makeClosure   : bool,
        argLifetimes  : int list
    }

    open Pretty

    val ioOp : int -> machineWord = RunCall.run_call1 RuntimeCalls.POLY_SYS_io_operation;

    local
        open RuntimeCalls
        val rtsTable =
            [
            (POLY_SYS_exit,"POLY_SYS_exit"),
            (POLY_SYS_chdir,"POLY_SYS_chdir"),
            (POLY_SYS_alloc_store,"POLY_SYS_alloc_store"),
            (POLY_SYS_raisex,"POLY_SYS_raisex"),
            (POLY_SYS_get_length,"POLY_SYS_get_length"),
            (POLY_SYS_get_flags,"POLY_SYS_get_flags"),
            (POLY_SYS_str_compare,"POLY_SYS_str_compare"),
            (POLY_SYS_teststrgtr,"POLY_SYS_teststrgtr"),
            (POLY_SYS_teststrlss,"POLY_SYS_teststrlss"),
            (POLY_SYS_teststrgeq,"POLY_SYS_teststrgeq"),
            (POLY_SYS_teststrleq,"POLY_SYS_teststrleq"),
            (POLY_SYS_exception_trace,"POLY_SYS_exception_trace"),
            (POLY_SYS_give_ex_trace,"POLY_SYS_give_ex_trace"),
            (POLY_SYS_lockseg,"POLY_SYS_lockseg"),
            (POLY_SYS_emptystring,"POLY_SYS_emptystring"),
            (POLY_SYS_nullvector,"POLY_SYS_nullvector"),
            (POLY_SYS_network,"POLY_SYS_network"),
            (POLY_SYS_os_specific,"POLY_SYS_os_specific"),
            (POLY_SYS_io_dispatch,"POLY_SYS_io_dispatch"),
            (POLY_SYS_signal_handler,"POLY_SYS_signal_handler"),
            (POLY_SYS_atomic_incr,"POLY_SYS_atomic_incr"),
            (POLY_SYS_atomic_decr,"POLY_SYS_atomic_decr"),
            (POLY_SYS_thread_self,"POLY_SYS_thread_self"),
            (POLY_SYS_thread_dispatch,"POLY_SYS_thread_dispatch"),
            (POLY_SYS_kill_self,"POLY_SYS_kill_self"),
            (POLY_SYS_profiler,"POLY_SYS_profiler"),
            (POLY_SYS_full_gc,"POLY_SYS_full_gc"),
            (POLY_SYS_stack_trace,"POLY_SYS_stack_trace"),
            (POLY_SYS_timing_dispatch,"POLY_SYS_timing_dispatch"),
            (POLY_SYS_objsize,"POLY_SYS_objsize"),
            (POLY_SYS_showsize,"POLY_SYS_showsize"),
            (POLY_SYS_is_short,"POLY_SYS_is_short"),
            (POLY_SYS_aplus,"POLY_SYS_aplus"),
            (POLY_SYS_aminus,"POLY_SYS_aminus"),
            (POLY_SYS_amul,"POLY_SYS_amul"),
            (POLY_SYS_adiv,"POLY_SYS_adiv"),
            (POLY_SYS_amod,"POLY_SYS_amod"),
            (POLY_SYS_aneg,"POLY_SYS_aneg"),
            (POLY_SYS_xora,"POLY_SYS_xora"),
            (POLY_SYS_equala,"POLY_SYS_equala"),
            (POLY_SYS_ora,"POLY_SYS_ora"),
            (POLY_SYS_anda,"POLY_SYS_anda"),
            (POLY_SYS_Real_str,"POLY_SYS_Real_str"),
            (POLY_SYS_Real_geq,"POLY_SYS_Real_geq"),
            (POLY_SYS_Real_leq,"POLY_SYS_Real_leq"),
            (POLY_SYS_Real_gtr,"POLY_SYS_Real_gtr"),
            (POLY_SYS_Real_lss,"POLY_SYS_Real_lss"),
            (POLY_SYS_Real_eq,"POLY_SYS_Real_eq"),
            (POLY_SYS_Real_neq,"POLY_SYS_Real_neq"),
            (POLY_SYS_Real_Dispatch,"POLY_SYS_Real_Dispatch"),
            (POLY_SYS_Add_real,"POLY_SYS_Add_real"),
            (POLY_SYS_Sub_real,"POLY_SYS_Sub_real"),
            (POLY_SYS_Mul_real,"POLY_SYS_Mul_real"),
            (POLY_SYS_Div_real,"POLY_SYS_Div_real"),
            (POLY_SYS_Neg_real,"POLY_SYS_Neg_real"),
            (POLY_SYS_Repr_real,"POLY_SYS_Repr_real"),
            (POLY_SYS_conv_real,"POLY_SYS_conv_real"),
            (POLY_SYS_real_to_int,"POLY_SYS_real_to_int"),
            (POLY_SYS_int_to_real,"POLY_SYS_int_to_real"),
            (POLY_SYS_sqrt_real,"POLY_SYS_sqrt_real"),
            (POLY_SYS_sin_real,"POLY_SYS_sin_real"),
            (POLY_SYS_cos_real,"POLY_SYS_cos_real"),
            (POLY_SYS_arctan_real,"POLY_SYS_arctan_real"),
            (POLY_SYS_exp_real,"POLY_SYS_exp_real"),
            (POLY_SYS_ln_real,"POLY_SYS_ln_real"),
            (POLY_SYS_stdin,"POLY_SYS_stdin"),
            (POLY_SYS_stdout,"POLY_SYS_stdout"),
            (POLY_SYS_process_env,"POLY_SYS_process_env"),
            (POLY_SYS_set_string_length,"POLY_SYS_set_string_length"),
            (POLY_SYS_get_first_long_word,"POLY_SYS_get_first_long_word"),
            (POLY_SYS_poly_specific,"POLY_SYS_poly_specific"),
            (POLY_SYS_bytevec_eq, "POLY_SYS_bytevec_eq"),
            (POLY_SYS_io_operation,"POLY_SYS_io_operation"),
            (POLY_SYS_set_code_constant,"POLY_SYS_set_code_constant"),
            (POLY_SYS_move_words,"POLY_SYS_move_words"),
            (POLY_SYS_shift_right_arith_word,"POLY_SYS_shift_right_arith_word"),
            (POLY_SYS_move_bytes,"POLY_SYS_move_bytes"),
            (POLY_SYS_code_flags,"POLY_SYS_code_flags"),
            (POLY_SYS_shrink_stack,"POLY_SYS_shrink_stack"),
            (POLY_SYS_stderr,"POLY_SYS_stderr"),
            (POLY_SYS_callcode_tupled,"POLY_SYS_callcode_tupled"),
            (POLY_SYS_foreign_dispatch,"POLY_SYS_foreign_dispatch"),
            (POLY_SYS_XWindows,"POLY_SYS_XWindows"),
            (POLY_SYS_is_big_endian,"POLY_SYS_is_big_endian"),
            (POLY_SYS_bytes_per_word,"POLY_SYS_bytes_per_word"),
            (POLY_SYS_offset_address,"POLY_SYS_offset_address"),
            (POLY_SYS_shift_right_word,"POLY_SYS_shift_right_word"),
            (POLY_SYS_word_neq,"POLY_SYS_word_neq"),
            (POLY_SYS_not_bool,"POLY_SYS_not_bool"),
            (POLY_SYS_string_length,"POLY_SYS_string_length"),
            (POLY_SYS_int_geq,"POLY_SYS_int_geq"),
            (POLY_SYS_int_leq,"POLY_SYS_int_leq"),
            (POLY_SYS_int_gtr,"POLY_SYS_int_gtr"),
            (POLY_SYS_int_lss,"POLY_SYS_int_lss"),
            (POLY_SYS_mul_word,"POLY_SYS_mul_word"),
            (POLY_SYS_plus_word,"POLY_SYS_plus_word"),
            (POLY_SYS_minus_word,"POLY_SYS_minus_word"),
            (POLY_SYS_div_word,"POLY_SYS_div_word"),
            (POLY_SYS_or_word,"POLY_SYS_or_word"),
            (POLY_SYS_and_word,"POLY_SYS_and_word"),
            (POLY_SYS_xor_word,"POLY_SYS_xor_word"),
            (POLY_SYS_shift_left_word,"POLY_SYS_shift_left_word"),
            (POLY_SYS_mod_word,"POLY_SYS_mod_word"),
            (POLY_SYS_word_geq,"POLY_SYS_word_geq"),
            (POLY_SYS_word_leq,"POLY_SYS_word_leq"),
            (POLY_SYS_word_gtr,"POLY_SYS_word_gtr"),
            (POLY_SYS_word_lss,"POLY_SYS_word_lss"),
            (POLY_SYS_word_eq,"POLY_SYS_word_eq"),
            (POLY_SYS_load_byte,"POLY_SYS_load_byte"),
            (POLY_SYS_load_word,"POLY_SYS_load_word"),
            (POLY_SYS_assign_byte,"POLY_SYS_assign_byte"),
            (POLY_SYS_assign_word,"POLY_SYS_assign_word")
            ]
    in
        val rtsNames =
            Vector.tabulate(256,
                fn n => case List.find(fn (rtsNo, _) => rtsNo=n) rtsTable of
                    SOME(_, name) => name | _ => " RTS" ^ Int.toString n)
    end

    fun stringOfWord w =
    if isShort w
    then "LIT" ^ Word.toString (toShort w)
    else if isIoAddress(toAddress w)
    then (* RTS call - print the number. *)
        let
            fun matchIo n =
                if n = 256 then raise Misc.InternalError "Unknown RTS entry"
                else if wordEq (w, ioOp n)
                then Vector.sub(rtsNames, n)
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
            then "FUN \"" ^ doCall(105, firstWord) ^ "\"" (* Get the function name. *)
            else "LIT <long>"
        end
    else "LIT <long>"
  
    fun pretty (pt : codetree) : pretty =
    let
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
        
        fun printList(start, lst, sep) : pretty =
            PrettyBlock (1, true, [],
                PrettyString (start ^ "(") ::
                pList(lst, sep, fn x => (pretty x)) @
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
            CodeNil => PrettyString "NIL"
        
        | MatchFail => PrettyString "MATCHFAIL"
        
        | AltMatch pair => printDiad "ALTMATCH" pair

        | Eval {function, argList, earlyEval, resultType} =>
            let
                val prettyArgs =
                    PrettyBlock (1, true, [],
                        PrettyString ("$(") ::
                        pList(argList, ",", prettyArg) @
                        [ PrettyBreak (0, 0), PrettyString (")") ]
                    )
            in
                PrettyBlock (3, false, [],
                    pretty function ::
                    PrettyBreak (0, 0) ::
                    (
                        if earlyEval
                        then [ PrettyString "{early}", PrettyBreak (0, 0) ]
                        else []
                    ) @
                    [ PrettyBreak(1, 0), prettyArgType resultType, PrettyBreak(1, 0), prettyArgs ]
                )
            end

        | KillItems {expression, killSet, killBefore} =>
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
        
        | Lambda {body, isInline, name, closure, argTypes, level, closureRefs,
                  makeClosure, resultType, localCount, argLifetimes} =>
            let
                val inl = 
                    case isInline of
                      NonInline   => ""
                    | MaybeInline => "INLINE"
                    | SmallFunction => "SMALL"
                    | OnlyInline  => "ONLYINLINE"
                fun prettyArgTypes [] = []
                |   prettyArgTypes [last] = [prettyArgType last]
                |   prettyArgTypes (hd::tl) = prettyArgType hd :: PrettyBreak(1, 0) :: prettyArgTypes tl
                fun prettyArgLife [] = []
                |   prettyArgLife [last] = [PrettyString(Int.toString last)]
                |   prettyArgLife (hd::tl) = PrettyString(Int.toString hd) :: PrettyBreak(1, 0) :: prettyArgLife tl
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
                        PrettyString (" LOCALS=" ^ Int.toString localCount),
                        PrettyBreak(1, 0),
                        PrettyBlock (1, false, [], PrettyString "ARGS=" :: prettyArgTypes argTypes),
                        PrettyBreak(1, 0),
                        PrettyBlock (1, false, [], PrettyString "ARGLIVES=" :: prettyArgLife argLifetimes),
                        PrettyBreak(1, 0),
                        PrettyBlock (1, false, [], [PrettyString "RES=", prettyArgType resultType]),
                        printList (" CLOS=", closure, ","),
                        PrettyBreak (1, 0),
                        pretty body,
                        PrettyString "){LAMBDA}"
                    ]
                )
            end
        
        | Constnt w => PrettyString (stringOfWord w)
        
        | Cond triple => printTriad "IF" triple
        
        | Newenv ptl => printList ("BLOCK", ptl, ";")
        
        | BeginLoop{loop=loopExp, arguments=args } =>
            PrettyBlock (3, false, [],
                [
                    prettyArgs("BEGINLOOP", args, ","),
                    PrettyBreak (0, 0),
                    PrettyString "(",
                    PrettyBreak (0, 0),
                    pretty loopExp,
                    PrettyBreak (0, 0),
                    PrettyString ")"
                ]
            )
        
        | Loop ptl => prettyArgs("LOOP", ptl, ",")
        
        | Raise c => printMonad "RAISE" c
        
        | Handle {exp, handler, ...} =>
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
        
        | Case {cases, test, default, caseType} =>
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
                    fn (exp : codetree, label : word) =>
                        PrettyBlock (1, true, [],
                            [
                                PrettyString (Word.toString label ^ ":"),
                                PrettyBreak (1, 0),
                                pretty exp
                            ])
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
        
        | MutualDecs ptl => printList("MUTUAL", ptl, " AND ")
        
        | Recconstr ptl => printList("RECCONSTR", ptl, ",")
        
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
                    PrettyString ("TUPLECONTAINER(" ^ Int.toString size ^ ","),
                    PrettyBreak (0, 0),
                    pretty container,
                    PrettyBreak (0, 0),
                    PrettyString ")"
                ]
            )

        |   TagTest { test, tag, maxTag } =>
            PrettyBlock (3, false, [],
                [
                    PrettyString (concat["TAGTEST(", Word.toString tag, ", ", Word.toString maxTag, ","]),
                    PrettyBreak (1, 0),
                    pretty test,
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

        | IndirectVariable { base, offset } =>
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

        |   TupleVariable(vars, length) =>
            let
                fun printTup(VarTupleSingle{source, destOffset}) =
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
                |   printTup(VarTupleMultiple{base, length, destOffset, sourceOffset}) = 
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
   
    and optGeneral (OptVal {general,...})       = general 
      | optGeneral (ValWithDecs {general, ...}) = general
      | optGeneral (JustTheVal ct)              = ct
      
    and optSpecial (OptVal {special,...}) = special
      | optSpecial _                      = CodeNil


    (* Map a function over the code-tree creating a new code tree from the results. *)
    (* Not currently used so it's commented out. *)
(*    fun mapCodeTreeNode _CodeNil = CodeNil
    |   mapCodeTreeNode _ MatchFail = MatchFail
    |   mapCodeTreeNode f (AltMatch(m1, m2)) = AltMatch(f m1, f m2)
    |   mapCodeTreeNode f (Declar {value, addr, references}) =
            Declar{value=f value, addr=addr, references=references}
    |   mapCodeTreeNode f (Newenv cl) = Newenv(map f cl)
    |   mapCodeTreeNode _ (c as Constnt _) = c
    |   mapCodeTreeNode _ (c as Extract _) = c
    |   mapCodeTreeNode f (Indirect{base, offset}) = Indirect{base=f base, offset=offset}
    |   mapCodeTreeNode f (Eval{function, argList, resultType, earlyEval}) =
            Eval{function=f function, argList=map(fn(c, t) => (f c, t)) argList,
                 resultType=resultType, earlyEval=earlyEval}
    |   mapCodeTreeNode f
            (Lambda{ body, isInline, name, closure, argTypes, resultType, level, closureRefs, makeClosure}) =
            Lambda{ body=f body, isInline=isInline, name=name, closure=closure, argTypes=argTypes,
                    resultType=resultType, level=level, closureRefs=closureRefs, makeClosure=makeClosure}
    |   mapCodeTreeNode f (MutualDecs decs) = MutualDecs(map f decs)
    |   mapCodeTreeNode f (Cond(c, t, e)) = Cond(f c, f t, f e)
    |   mapCodeTreeNode f (Case{cases, test, caseType, default}) =
            Case{cases = map (fn (c, w) => (f c, w)) cases, test=f test, caseType=caseType, default=f default}
    |   mapCodeTreeNode f (BeginLoop(c, l)) = BeginLoop(f c, map(fn(c, t) => (f c, t)) l)
    |   mapCodeTreeNode f (Loop l) = Loop(map(fn(c, t) => (f c, t)) l)
    |   mapCodeTreeNode f (Raise c) = Raise(f c)
    |   mapCodeTreeNode _ Ldexc = Ldexc
    |   mapCodeTreeNode f (Handle{exp, taglist, handler}) = Handle{exp=f exp, taglist = map f taglist, handler=f handler}
    |   mapCodeTreeNode f (Recconstr l) = Recconstr(map f l)
    |   mapCodeTreeNode _ (c as Container _) = c
    |   mapCodeTreeNode f (SetContainer{container, tuple, size}) =
            SetContainer{container=f container, tuple=f tuple, size=size}
    |   mapCodeTreeNode f (TupleFromContainer(c, s)) = TupleFromContainer(f c, s)
    |   mapCodeTreeNode f (TagTest{test, tag, maxTag}) = TagTest{test=f test, tag=tag, maxTag=maxTag}
    |   mapCodeTreeNode _ (Global _) = raise Misc.InternalError "mapCodeTreeNode: Global"*)

 
    (* Return the "size" of a piece of code. *)
    fun codeSize (pt, includeSubfunctions) = 
    let
        fun sizeList l = List.foldl (fn (p, s) => size p + s) 0 l

        and sizeCaseList []           = 0
        |   sizeCaseList ((c,_)::cs) = size c + 1 + sizeCaseList cs

        and sizeOptVal (OptVal {general,...})       = size general 
        |   sizeOptVal (ValWithDecs {general, ...}) = size general
        |   sizeOptVal (JustTheVal ct)              = size ct

        (* some very rough size estimates *)
        and size pt =
            case pt of
                CodeNil                         => 0
            |   MatchFail                       => 1
            |   AltMatch (m1, m2)               => size m1 + size m2 + 1
            |   Declar {value, ...}             => size value
            |   Newenv cl                       => sizeList cl
            |   Constnt w                       => if isShort w then 0 else 1
(*            |   Extract {level=0, fpRel=true, ...} => 0 (* Probably in a register*)*)
            |   Extract _                       => 1
            |   Indirect {base,...}             => size base + 1
            |   Lambda {body, argTypes, ...}    => if includeSubfunctions then size body + List.length argTypes else 0
(*            |   Eval {function=Constnt w ,argList,...}     =>
                    (* If this is an RTS call it's probably really an instruction that
                       the code-generator will inline and if it isn't we're not going
                       to greatly wrong.  *)
                    if isIoAddress(toAddress w) then 1 + sizeList(List.map #1 argList)
                    else sizeList(List.map #1 argList) + 2*)
            |   Eval {function, argList,...}     => size function + sizeList(List.map #1 argList) + 2
            |   KillItems{expression, ...}     => size expression
            |   MutualDecs decs                 => sizeList decs (*!maxInlineSize*)
            |   Cond (i,t,e)                    => size i + size t + size e + 2
            |   BeginLoop {loop, arguments, ...}=> size loop + sizeList(List.map #1 arguments)
            |   Loop args                       => sizeList(List.map #1 args) + 1
            |   Raise c                         => size c + 1
            |   Ldexc                           => 1
            |   Handle {exp, handler}           => size exp + size handler
            |   Recconstr cl                    => sizeList cl + 2 (* optimistic *)
            |   Container _                     => 1 (* optimistic *)
            |   SetContainer{container, tuple = Recconstr cl, ...} =>
                            (* We can optimise this. *) sizeList cl + size container
            |   SetContainer{container, tuple, size=len} => size container + size tuple + len
            |   TupleFromContainer(container, len) => len + size container + 2 (* As with Recconstr *)
            |   Global glob                     => sizeOptVal glob
            |   TagTest { test, ... }           => 1 + size test
            |   Case {test,default,cases,...}   =>
                    size test + size default + sizeCaseList cases
            |   IndirectVariable{base, offset, ...} => size base + size offset + 1
            |   TupleVariable(vars, _)=>
                let
                    fun sizeTuple(VarTupleSingle{source, destOffset, ...}) =
                            size source + size destOffset
                    |   sizeTuple(VarTupleMultiple{base, length, destOffset, sourceOffset, ...}) =
                            size base + size length + size destOffset + size sourceOffset
                in
                    List.foldl (fn (p, s) => sizeTuple p + s) 0 vars
                end
(*        and size pt =
            case pt of
                CodeNil                         => 0
            |   MatchFail                       => 0
            |   AltMatch (m1, m2)               => size m1 + size m2
            |   Declar {value, ...}             => size value
            |   Newenv cl                       => sizeList cl
            |   Constnt _                       => 0
            |   Extract _                       => 0
            |   Indirect {base,...}             => size base
            |   Lambda {body, ...}              => 10 + size body
            |   Eval {function=Constnt _,argList, ...}     => sizeList(List.map #1 argList) + 1
            |   Eval {function, argList,...}    => size function + sizeList(List.map #1 argList) + 10
            |   KillItems{expression, ...}      => size expression
            |   MutualDecs decs                 => sizeList decs
            |   Cond (i,t,e)                    => size i + size t + size e 
            |   BeginLoop {loop, arguments, ...}=> size loop + sizeList(List.map #1 arguments)
            |   Loop args                       => sizeList(List.map #1 args)
            |   Raise c                         => size c
            |   Ldexc                           => 0
            |   Handle {exp,taglist,handler}    => size exp + size handler + sizeList taglist + List.length taglist
            |   Recconstr cl                    => sizeList cl
            |   Container _                     => 0
            |   SetContainer{container, tuple, ...} => size container + size tuple
            |   TupleFromContainer(container, _) => size container
            |   Global glob                     => sizeOptVal glob
            |   TagTest { test, ... }           => size test
            |   Case {test,default,cases,...}   =>
                    size test + size default + sizeCaseList cases*)
    in
        size pt
    end

    (* Tests if the function is non-tail recursive.  Since this is only used as an indication as
       to whether to inline the function it doesn't matter if it's not precise. *)
(*    fun isRecursive(pt, baseLevel) =
    let
        fun checkList l = List.foldl (fn (p, s) => s orelse check (p, false)) false l

        and check(pt, tail) =
            case pt of
                CodeNil                         => false
            |   MatchFail                       => false
            |   AltMatch (m1, m2)               => check(m1, tail) orelse check (m2, tail)
            |   Declar {value, ...}             => check(value, false)
            |   Newenv cl                       =>
                let
                    fun checkList([], t) = t
                    |   checkList([last], t) = t orelse check(last, tail)
                    |   checkList(hd::tl, t) = t orelse check(hd, false) orelse checkList(tl, false)
                in
                    checkList(cl, false)
                end
            |   Constnt _                       => false
            |   Extract _                       => false
            |   Indirect {base,...}             => check (base, false)
            |   Lambda {body, ...}              => check (body, false)
            |   Eval {function=Extract{level, addr, ...}, argList, ...} =>
                       if level > 0 orelse level = 0 andalso addr=0 andalso not tail
                       then true else checkList(List.map #1 argList)
            |   Eval {function, argList,...}     => check(function, false) orelse checkList(List.map #1 argList)
            |   MutualDecs decs                 => checkList decs
            |   Cond (i,t,e)                    => check (i, false) orelse check (t, tail) orelse check (e, tail)
            |   BeginLoop {loop, arguments, ...}=> check (loop, false) orelse checkList(List.map #1 arguments)
            |   Loop args                       => checkList(List.map #1 args)
            |   Raise c                         => check (c, false)
            |   Ldexc                           => false
            |   Handle {exp,taglist,handler}    => check (exp, tail) orelse check (handler, tail) orelse checkList taglist
            |   Recconstr cl                    => checkList cl
            |   Container _                     => false
            |   SetContainer{container, tuple = Recconstr cl, ...} =>
                            (* We can optimise this. *) checkList cl orelse check (container, false)
            |   SetContainer{container, tuple, ...} => check (container, false) orelse check (tuple, false)
            |   TupleFromContainer(container, _) => check (container, false)
            |   Global _                         => false
            |   TagTest { test, ... }           => check (test, false)
            |   Case {test,default,cases,...}   =>
                let
                    fun sizeCaseList []           = false
                    |   sizeCaseList ((c,_)::cs) = check(c, tail) orelse sizeCaseList cs
                in
                    check (test, false) orelse check (default, tail) orelse sizeCaseList cases
                end
    in
        check(pt, true)
    end*)

    structure Sharing =
    struct
        type codetree = codetree
        and  optVal = optVal
        and  caseType = caseType
        and  pretty = pretty
    end

end;
