(* This caused a problem in the Sparc version because the stack frame
   grew to more than 4096 bytes.  Thanks to Rob Arthan for reporting it. *)
type STATE = int;
type STATE_STACK = STATE list;
datatype ('tok, 'lc, 'pp) INPUT_STACK_ITEM = Token of 'tok * 'lc | Parsed of 'pp;
type('tok, 'lc, 'pp) INPUT_STACK = ('tok, 'lc, 'pp) INPUT_STACK_ITEM list;
datatype ACTION = Shift of STATE
            | Reduce of ((string * int) * int)
            | Dynamic of STATE * ((string * int) * int)
            | Accept
            | Error;
(*
type ('lc)ACTION_TABLE =  ('lc * ACTION) list PPArray.array;
type GOTO_TABLE = (string * STATE) list PPArray.array;
type ('tok, 'lc, 'pp)REDUCTION_TABLE =
    (('tok, 'lc, 'pp)INPUT_STACK -> 'pp) list E_DICT;
datatype RESOLUTION = DoShift
            | DoReduce
            | DoError;
type ('tok, 'lc, 'pp)RESOLVER =
    ('tok * 'lc) * ('tok, 'lc, 'pp)INPUT_STACK * ((string * int) * int) -> RESOLUTION;
type ('tok, 'lc)CLASSIFIER = 'tok -> 'lc;
type ('tok, 'lc, 'pp, 'st)ERROR_ROUTINE =
    'tok * ('tok, 'lc, 'pp)INPUT_STACK * STATE_STACK * 'st -> ('tok*'st);
type ('tok, 'st)READER = 'st -> ('tok * 'st);
exception SYNTAX_ERROR;
exception PARSER_ERROR of string;
local
    fun get_action (actions : (''lc)ACTION_TABLE) (s:STATE) (lc:''lc) : ACTION = (
    (let    open PPArray
    in  lassoc3 (sub(actions, s)) lc
    end handle Fail _ => Error)
    handle PPArray.Subscript => raise PARSER_ERROR "invalid state"
    );
in  fun lookup_action
        (resolver : ('tok, ''lc, 'pp)RESOLVER)
        (actions : (''lc)ACTION_TABLE)
        (tok : 'tok)
        (stk : ('tok, ''lc, 'pp)INPUT_STACK)
        (s : STATE)
        (lc : ''lc) : ACTION = (
        case get_action actions s lc of
            Dynamic (s', altsymn) => (
                case resolver((tok, lc), stk, altsymn) of
                    DoShift => Shift s'
                |   DoReduce => Reduce altsymn
                |   DoError => Error
            ) | easy => easy
    );
end;
fun lookup_goto (gotos : GOTO_TABLE) (s:STATE) (nt : string) : STATE = (
    (let    open PPArray
    in  lassoc3 (sub(gotos, s)) nt
    end handle Fail _ => raise PARSER_ERROR "invalid non-terminal")
    handle PPArray.Subscript => raise PARSER_ERROR "invalid state"

);
fun lookup_reducer (reducers : ('tok, ''lc, 'pp)REDUCTION_TABLE)
        (nt:string) (alt : int) : ('tok, ''lc, 'pp)INPUT_STACK -> 'pp = (
    case e_lookup nt reducers of
        Value tab => nth alt tab
    |   Nil  => raise PARSER_ERROR "invalid non-terminal/alternative"
);
fun slrp'parse
    (s0 : STATE)
    (actions : (''lc)ACTION_TABLE)
    (gotos : GOTO_TABLE)
    (reducers : ('tok, ''lc, 'pp)REDUCTION_TABLE)
    (resolver : ('tok, ''lc, 'pp) RESOLVER)
    (classify : ('tok, ''lc)CLASSIFIER)
    (error : ('tok, ''lc, 'pp, 'st)ERROR_ROUTINE)
    (reader : ('tok, 'st) READER) : 'st -> 'pp = (
    let open PPArray;
        val init_ss = [s0];
        val init_is = [];
        val fetch_action = lookup_action resolver actions;
        val fetch_reducer = lookup_reducer reducers;
        val fetch_goto = lookup_goto gotos;
        fun go (tok, st) = (fn (ss, is) =>
            let val lc = classify tok;
                val s = hd ss;
            in  case fetch_action tok is s lc of 
                    Shift s' => go (reader st)((s' :: ss), Token(tok, lc) :: is)
                |   Reduce((nt, alt), r) => (
                        let val reducer = fetch_reducer nt alt;
                            val A = reducer is;
                            val ss' = ss from r;
                            val is' = is from r;
                            val s' = fetch_goto (hd ss') nt;
                        in go (tok, st) (s' :: ss', Parsed A :: is')
                        end
                ) | Accept => ((ss, is)
                ) | Error =>(
                    go (error (tok, is, ss, st)) (ss, is)
                ) | Dynamic _ => (
                    raise PARSER_ERROR "conflict resolution error"
                )
            end
        );
    in
        (fn st => (
            case go (reader st) (init_ss, init_is)of
                (_, [Parsed res]) => res
            |   _ => raise PARSER_ERROR "stacks corrupt at end of parse"))
    end
);
fun format_stack (do_tok : 'tok -> string)(stk : ('tok, 'lc, 'pp)INPUT_STACK) : string = (
    format_list (fn Token (tk, _) => do_tok tk | _ => "...") (rev stk) " "
);
fun default_error
    (do_tok : 'tok -> string)
    (tok: 'tok, stk : ('tok, 'lc, 'pp)INPUT_STACK, _ : STATE_STACK, st : 'st) : ('tok*'st) = (
    raw_diag_line("*** ERROR Syntax error ***");
    if any stk (fn Token _ => true | _ => false)
    then raw_diag_line(do_tok tok ^ " not expected after: " ^ format_stack do_tok stk)
    else raw_diag_line(do_tok tok ^ " not expected here");
    raise SYNTAX_ERROR; (tok, st)
);
val (default_resolver : ('tok, 'lc, 'pp)RESOLVER) = (fn _ =>
    raise PARSER_ERROR "shift/reduce conflict"
);
fun simple_resolver (prec : (('tok * 'lc) * ('tok * 'lc)) -> RESOLUTION)
        : ('tok, 'lc, 'pp)RESOLVER = (fn (toklc, stk, ignored) =>
    case stk of
        Token toklc' :: _ => prec (toklc', toklc)
    |   _ :: more => simple_resolver prec (toklc, more, ignored)
    |   [] => DoError
);
end; (* of structure SlrpDriver *)
open SlrpDriver;
*)
datatype CN_LEX_CLASS
            = LCAbs | LCAmpersand | LCAnd
            | LCAnnotation | LCAnnotationLabel | LCArbitraryAda
            | LCArray | LCAt | LCAuxiliary 
            | LCBar |LCBBody | LCBecomes | LCBegin | LCBy
            | LCCase| LCCharacterLiteral
            | LCColon | LCComma | LCComment
            | LCCompLabel | LCConstant
            | LCCon | LCCrd
            | LCDecLabel  | LCDeclare| LCDelta | LCDigits | LCDiv
            | LCDotDot | LCDot
            | LCElse | LCElsIf | LCEnd | LCEos | LCEquals | LCExit
            | LCFatDot | LCFor | LCFunction
            | LCGoesTo | LCGreaterEquals | LCGreaterThan
            | LCGrkGamma | LCGrkDelta | LCGrkXi
            | LCIdentifier | LCIf | LCImplement | LCImplementedBy
            | LCIn | LCIs
            | LCKSlot
            | LCLBrace | LCLChevron | LCLessEquals
            | LCLessGreat | LCLessThan
            | LCLimited | LCLoop | LCLSqBrack | LCLStrachey
            | LCMinus | LCMod
            | LCNotEquals | LCNot | LCNull | LCNumericLiteral
            | LCOf | LCOrd | LCOr | LCOthers | LCOut
            | LCPackage | LCPlus | LCPPartLabel | LCPrime
            | LCPrimedIdentifier | LCPragma | LCPrivate
            | LCProcedure
            | LCRBrace | LCRChevron | LCRecord
            | LCReferences | LCRefinedBy | LCRem
            | LCRenames | LCReplacedBy | LCReturn
            | LCReverse | LCRRange | LCRSqBrack | LCRStrachey
            | LCSemi | LCSeparate | LCSpecLabel | LCStarStar
            | LCStar | LCStmtLabel | LCStringLiteral
            | LCSubType | LCThen | LCTill | LCType
            | LCUse| LCUsing | LCVPartLabel
            | LCWhen | LCWhile | LCWith
            | LCXor
            | LCZ;
local
val slrp'a711=[[(LCFor,Shift 596),(LCPackage,Shift 533),(LCPragma,Shift 481),
(LCFunction,Shift 565),(LCUse,Shift 509),(LCType,Shift 551),(LCUsing,Shift 619),
(LCProcedure,Shift 530),(LCLChevron,Shift 462),
(LCEos,Reduce(("declarative_part",0),0)),
(LCBegin,Reduce(("declarative_part",0),0)),
(LCEnd,Reduce(("declarative_part",0),0)),(LCSubType,Shift 44),
(LCIdentifier,Shift 443),(LCAnnotation,Shift 22)]];
val slrp'a710=[[(LCFor,Shift 596),(LCPackage,Shift 533),(LCSeparate,Shift 402),
(LCPragma,Shift 481),(LCFunction,Shift 565),(LCUse,Shift 509),(LCType,Shift 551)
,(LCProcedure,Shift 530),(LCLChevron,Shift 462),
(LCEos,Reduce(("declarative_part",0),0)),
(LCBegin,Reduce(("declarative_part",0),0)),
(LCEnd,Reduce(("declarative_part",0),0)),(LCSubType,Shift 44),
(LCIdentifier,Shift 443),(LCAnnotation,Shift 22)]]@slrp'a711;
val slrp'a709=[[(LCFor,Shift 596),(LCPackage,Shift 533),(LCSeparate,Shift 400),
(LCPragma,Shift 481),(LCFunction,Shift 565),(LCUse,Shift 509),(LCType,Shift 551)
,(LCProcedure,Shift 530),(LCLChevron,Shift 462),
(LCEos,Reduce(("declarative_part",0),0)),
(LCBegin,Reduce(("declarative_part",0),0)),
(LCEnd,Reduce(("declarative_part",0),0)),(LCSubType,Shift 44),
(LCIdentifier,Shift 443),(LCAnnotation,Shift 22)]]@slrp'a710;
val slrp'a708=[[(LCFor,Shift 596),(LCPackage,Shift 533),(LCPragma,Shift 481),
(LCFunction,Shift 565),(LCUse,Shift 509),(LCType,Shift 551),
(LCProcedure,Shift 530),(LCLChevron,Shift 462),
(LCEos,Reduce(("declarative_part",0),0)),
(LCBegin,Reduce(("declarative_part",0),0)),
(LCEnd,Reduce(("declarative_part",0),0)),(LCSubType,Shift 44),
(LCIdentifier,Shift 443),(LCAnnotation,Shift 22)]]@slrp'a709;
val slrp'a707=[[(LCFor,Shift 596),(LCPackage,Shift 533),(LCPragma,Shift 481),
(LCFunction,Shift 565),(LCUse,Shift 509),(LCType,Shift 551),
(LCProcedure,Shift 530),(LCLChevron,Shift 462),
(LCEos,Reduce(("declarative_part",0),0)),
(LCBegin,Reduce(("declarative_part",0),0)),
(LCEnd,Reduce(("declarative_part",0),0)),(LCSubType,Shift 44),
(LCIdentifier,Shift 443),(LCAnnotation,Shift 22)]]@slrp'a708;
val slrp'a706=[[(LCFor,Shift 596),(LCPackage,Shift 533),(LCPragma,Shift 481),
(LCFunction,Shift 565),(LCUse,Shift 509),(LCType,Shift 551),
(LCProcedure,Shift 530),(LCLChevron,Shift 462),
(LCEos,Reduce(("declarative_part",0),0)),
(LCBegin,Reduce(("declarative_part",0),0)),
(LCEnd,Reduce(("declarative_part",0),0)),(LCSubType,Shift 44),
(LCIdentifier,Shift 443),(LCAnnotation,Shift 22)]]@slrp'a707;
val slrp'a705=[[(LCFor,Shift 596),(LCPackage,Shift 533),(LCPragma,Shift 481),
(LCFunction,Shift 565),(LCUse,Shift 509),(LCType,Shift 551),
(LCProcedure,Shift 530),(LCLChevron,Shift 462),
(LCEos,Reduce(("declarative_part",0),0)),
(LCBegin,Reduce(("declarative_part",0),0)),
(LCEnd,Reduce(("declarative_part",0),0)),(LCSubType,Shift 44),
(LCIdentifier,Shift 443),(LCAnnotation,Shift 22)]]@slrp'a706;
val slrp'a704=[[(LCFor,Shift 596),(LCPackage,Shift 533),(LCPragma,Shift 481),
(LCFunction,Shift 565),(LCUse,Shift 509),(LCType,Shift 551),
(LCProcedure,Shift 530),(LCLChevron,Shift 462),
(LCEos,Reduce(("declarative_part",0),0)),
(LCBegin,Reduce(("declarative_part",0),0)),
(LCEnd,Reduce(("declarative_part",0),0)),(LCSubType,Shift 44),
(LCIdentifier,Shift 443),(LCAnnotation,Shift 22)]]@slrp'a705;
val slrp'a703=[[(LCFor,Shift 596),(LCPackage,Shift 533),(LCPragma,Shift 481),
(LCFunction,Shift 565),(LCUse,Shift 509),(LCType,Shift 551),
(LCProcedure,Shift 530),(LCLChevron,Shift 462),
(LCEos,Reduce(("declarative_part",0),0)),
(LCBegin,Reduce(("declarative_part",0),0)),
(LCEnd,Reduce(("declarative_part",0),0)),(LCSubType,Shift 44),
(LCIdentifier,Shift 443),(LCAnnotation,Shift 22)]]@slrp'a704;
val slrp'a702=[[(LCFor,Shift 596),(LCPragma,Shift 481),(LCFunction,Shift 565),
(LCPackage,Shift 358),(LCUse,Shift 509),(LCType,Shift 551),
(LCAuxiliary,Shift 313),(LCEos,Reduce(("visible_part",0),0)),
(LCEnd,Reduce(("visible_part",0),0)),(LCPrivate,Reduce(("visible_part",0),0)),
(LCProcedure,Shift 530),(LCLChevron,Shift 462),(LCSubType,Shift 44),
(LCIdentifier,Shift 443),(LCAnnotation,Shift 22)]]@slrp'a703;
val slrp'a701=[[(LCFor,Shift 596),(LCPragma,Shift 481),(LCFunction,Shift 565),
(LCPackage,Shift 358),(LCUse,Shift 509),(LCType,Shift 551),
(LCAuxiliary,Shift 313),(LCEos,Reduce(("visible_part",0),0)),
(LCEnd,Reduce(("visible_part",0),0)),(LCPrivate,Reduce(("visible_part",0),0)),
(LCProcedure,Shift 530),(LCLChevron,Shift 462),(LCSubType,Shift 44),
(LCIdentifier,Shift 443),(LCAnnotation,Shift 22)]]@slrp'a702;
val slrp'a700=[[(LCFor,Shift 596),(LCPragma,Shift 481),(LCFunction,Shift 565),
(LCPackage,Shift 358),(LCUse,Shift 509),(LCType,Shift 551),
(LCAuxiliary,Shift 313),(LCEos,Reduce(("visible_part",0),0)),
(LCEnd,Reduce(("visible_part",0),0)),(LCPrivate,Reduce(("visible_part",0),0)),
(LCProcedure,Shift 530),(LCLChevron,Shift 462),(LCSubType,Shift 44),
(LCIdentifier,Shift 443),(LCAnnotation,Shift 22)]]@slrp'a701;
val slrp'a699=[[(LCFor,Shift 596),(LCPragma,Shift 481),(LCFunction,Shift 565),
(LCPackage,Shift 358),(LCUse,Shift 509),(LCType,Shift 551),
(LCEos,Reduce(("private_part",0),1)),(LCEnd,Reduce(("private_part",0),1)),
(LCProcedure,Shift 530),(LCLChevron,Shift 462),(LCSubType,Shift 44),
(LCIdentifier,Shift 443),(LCAnnotation,Shift 22)]]@slrp'a700;
val slrp'a698=[[(LCFor,Shift 596),(LCPragma,Shift 481),(LCFunction,Shift 565),
(LCPackage,Shift 358),(LCUse,Shift 509),(LCType,Shift 551),
(LCProcedure,Shift 530),(LCLChevron,Shift 462),(LCSubType,Shift 44),
(LCIdentifier,Shift 443),(LCAnnotation,Shift 22)]]@slrp'a699;
val slrp'a697=[[(LCFor,Shift 596),(LCPragma,Shift 481),(LCFunction,Shift 565),
(LCPackage,Shift 358),(LCUse,Shift 509),(LCType,Shift 551),
(LCProcedure,Shift 530),(LCLChevron,Shift 462),(LCSubType,Shift 44),
(LCIdentifier,Shift 443),(LCAnnotation,Shift 22)]]@slrp'a698;
val slrp'a696=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOthers,Shift 112),(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a697;
val slrp'a695=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOthers,Shift 112),(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a696;
val slrp'a694=[[(LCPragma,Shift 481),(LCReturn,Shift 655),(LCExit,Shift 527),
(LCIdentifier,Shift 447),(LCFor,Reduce(("name_colon_opt",0),0)),
(LCWhile,Reduce(("name_colon_opt",0),0)),(LCTill,Reduce(("name_colon_opt",0),0))
,(LCLoop,Reduce(("name_colon_opt",0),0)),
(LCDeclare,Reduce(("name_colon_opt",0),0)),
(LCBegin,Reduce(("name_colon_opt",0),0)),(LCCase,Shift 646),(LCIf,Shift 651),
(LCNull,Shift 197),(LCLChevron,Shift 462),(LCGrkGamma,Shift 181),
(LCGrkDelta,Shift 524),(LCAnnotation,Shift 168),(LCCon,Shift 459),
(LCCharacterLiteral,Shift 105)]]@slrp'a695;
val slrp'a693=[[(LCPragma,Shift 481),(LCReturn,Shift 655),(LCExit,Shift 527),
(LCIdentifier,Shift 447),(LCFor,Reduce(("name_colon_opt",0),0)),
(LCWhile,Reduce(("name_colon_opt",0),0)),(LCTill,Reduce(("name_colon_opt",0),0))
,(LCLoop,Reduce(("name_colon_opt",0),0)),
(LCDeclare,Reduce(("name_colon_opt",0),0)),
(LCBegin,Reduce(("name_colon_opt",0),0)),(LCCase,Shift 646),(LCIf,Shift 651),
(LCNull,Shift 197),(LCLChevron,Shift 462),(LCGrkGamma,Shift 181),
(LCGrkDelta,Shift 524),(LCAnnotation,Shift 168),(LCCon,Shift 459),
(LCCharacterLiteral,Shift 105)]]@slrp'a694;
val slrp'a692=[[(LCPragma,Shift 481),(LCReturn,Shift 655),(LCExit,Shift 527),
(LCIdentifier,Shift 447),(LCFor,Reduce(("name_colon_opt",0),0)),
(LCWhile,Reduce(("name_colon_opt",0),0)),(LCTill,Reduce(("name_colon_opt",0),0))
,(LCLoop,Reduce(("name_colon_opt",0),0)),
(LCDeclare,Reduce(("name_colon_opt",0),0)),
(LCBegin,Reduce(("name_colon_opt",0),0)),(LCCase,Shift 646),(LCIf,Shift 651),
(LCNull,Shift 197),(LCLChevron,Shift 462),(LCGrkGamma,Shift 181),
(LCGrkDelta,Shift 524),(LCAnnotation,Shift 168),(LCCon,Shift 459),
(LCCharacterLiteral,Shift 105)]]@slrp'a693;
val slrp'a691=[[(LCPragma,Shift 481),(LCReturn,Shift 655),(LCExit,Shift 527),
(LCIdentifier,Shift 447),(LCFor,Reduce(("name_colon_opt",0),0)),
(LCWhile,Reduce(("name_colon_opt",0),0)),(LCTill,Reduce(("name_colon_opt",0),0))
,(LCLoop,Reduce(("name_colon_opt",0),0)),
(LCDeclare,Reduce(("name_colon_opt",0),0)),
(LCBegin,Reduce(("name_colon_opt",0),0)),(LCCase,Shift 646),(LCIf,Shift 651),
(LCNull,Shift 197),(LCLChevron,Shift 462),(LCGrkGamma,Shift 181),
(LCGrkDelta,Shift 524),(LCAnnotation,Shift 168),(LCCon,Shift 459),
(LCCharacterLiteral,Shift 105)]]@slrp'a692;
val slrp'a690=[[(LCPragma,Shift 481),(LCReturn,Shift 655),(LCExit,Shift 527),
(LCIdentifier,Shift 447),(LCFor,Reduce(("name_colon_opt",0),0)),
(LCWhile,Reduce(("name_colon_opt",0),0)),(LCTill,Reduce(("name_colon_opt",0),0))
,(LCLoop,Reduce(("name_colon_opt",0),0)),
(LCDeclare,Reduce(("name_colon_opt",0),0)),
(LCBegin,Reduce(("name_colon_opt",0),0)),(LCCase,Shift 646),(LCIf,Shift 651),
(LCNull,Shift 197),(LCLChevron,Shift 462),(LCGrkGamma,Shift 181),
(LCGrkDelta,Shift 524),(LCAnnotation,Shift 168),(LCCon,Shift 459),
(LCCharacterLiteral,Shift 105)]]@slrp'a691;
val slrp'a689=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOthers,Shift 112),(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a690;
val slrp'a688=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOthers,Shift 112),(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a689;
val slrp'a687=[[(LCPragma,Shift 481),(LCReturn,Shift 655),(LCExit,Shift 527),
(LCIdentifier,Shift 447),(LCFor,Reduce(("name_colon_opt",0),0)),
(LCWhile,Reduce(("name_colon_opt",0),0)),(LCTill,Reduce(("name_colon_opt",0),0))
,(LCLoop,Reduce(("name_colon_opt",0),0)),
(LCDeclare,Reduce(("name_colon_opt",0),0)),
(LCBegin,Reduce(("name_colon_opt",0),0)),(LCCase,Shift 646),(LCIf,Shift 651),
(LCNull,Shift 197),(LCLChevron,Shift 462),(LCGrkGamma,Shift 181),
(LCGrkDelta,Shift 524),(LCAnnotation,Shift 168),(LCCon,Shift 459),
(LCCharacterLiteral,Shift 105)]]@slrp'a688;
val slrp'a686=[[(LCPragma,Shift 481),(LCReturn,Shift 655),(LCExit,Shift 527),
(LCIdentifier,Shift 447),(LCFor,Reduce(("name_colon_opt",0),0)),
(LCWhile,Reduce(("name_colon_opt",0),0)),(LCTill,Reduce(("name_colon_opt",0),0))
,(LCLoop,Reduce(("name_colon_opt",0),0)),
(LCDeclare,Reduce(("name_colon_opt",0),0)),
(LCBegin,Reduce(("name_colon_opt",0),0)),(LCCase,Shift 646),(LCIf,Shift 651),
(LCNull,Shift 197),(LCLChevron,Shift 462),(LCGrkGamma,Shift 181),
(LCGrkDelta,Shift 524),(LCAnnotation,Shift 168),(LCCon,Shift 459),
(LCCharacterLiteral,Shift 105)]]@slrp'a687;
val slrp'a685=[[(LCPragma,Shift 481),(LCReturn,Shift 655),(LCExit,Shift 527),
(LCIdentifier,Shift 447),(LCFor,Reduce(("name_colon_opt",0),0)),
(LCWhile,Reduce(("name_colon_opt",0),0)),(LCTill,Reduce(("name_colon_opt",0),0))
,(LCLoop,Reduce(("name_colon_opt",0),0)),
(LCDeclare,Reduce(("name_colon_opt",0),0)),
(LCBegin,Reduce(("name_colon_opt",0),0)),(LCCase,Shift 646),(LCIf,Shift 651),
(LCNull,Shift 197),(LCLChevron,Shift 462),(LCGrkGamma,Shift 181),
(LCGrkDelta,Shift 524),(LCAnnotation,Shift 168),(LCCon,Shift 459),
(LCEos,Reduce(("sequence_of_statements",0),1)),
(LCElsIf,Reduce(("sequence_of_statements",0),1)),
(LCElse,Reduce(("sequence_of_statements",0),1)),
(LCEnd,Reduce(("sequence_of_statements",0),1)),
(LCWhen,Reduce(("sequence_of_statements",0),1)),(LCCharacterLiteral,Shift 105)]]@slrp'a686;
val slrp'a684=[[(LCPragma,Shift 481),(LCReturn,Shift 655),(LCExit,Shift 527),
(LCIdentifier,Shift 447),(LCFor,Reduce(("name_colon_opt",0),0)),
(LCWhile,Reduce(("name_colon_opt",0),0)),(LCTill,Reduce(("name_colon_opt",0),0))
,(LCLoop,Reduce(("name_colon_opt",0),0)),
(LCDeclare,Reduce(("name_colon_opt",0),0)),
(LCBegin,Reduce(("name_colon_opt",0),0)),(LCCase,Shift 646),(LCIf,Shift 651),
(LCNull,Shift 197),(LCLChevron,Shift 462),(LCGrkGamma,Shift 181),
(LCGrkDelta,Shift 524),(LCAnnotation,Shift 168),(LCCon,Shift 459),
(LCCharacterLiteral,Shift 105)]]@slrp'a685;
val slrp'a683=[[(LCPragma,Shift 481),(LCReturn,Shift 655),(LCExit,Shift 527),
(LCIdentifier,Shift 447),(LCFor,Reduce(("name_colon_opt",0),0)),
(LCWhile,Reduce(("name_colon_opt",0),0)),(LCTill,Reduce(("name_colon_opt",0),0))
,(LCLoop,Reduce(("name_colon_opt",0),0)),
(LCDeclare,Reduce(("name_colon_opt",0),0)),
(LCBegin,Reduce(("name_colon_opt",0),0)),(LCCase,Shift 646),(LCIf,Shift 651),
(LCNull,Shift 197),(LCLChevron,Shift 462),(LCGrkGamma,Shift 181),
(LCGrkDelta,Shift 524),(LCAnnotation,Shift 168),(LCCon,Shift 459),
(LCCharacterLiteral,Shift 105)]]@slrp'a684;
val slrp'a682=[[(LCPragma,Shift 481),(LCReturn,Shift 655),(LCExit,Shift 527),
(LCIdentifier,Shift 447),(LCFor,Reduce(("name_colon_opt",0),0)),
(LCWhile,Reduce(("name_colon_opt",0),0)),(LCTill,Reduce(("name_colon_opt",0),0))
,(LCLoop,Reduce(("name_colon_opt",0),0)),
(LCDeclare,Reduce(("name_colon_opt",0),0)),
(LCBegin,Reduce(("name_colon_opt",0),0)),(LCCase,Shift 646),(LCIf,Shift 651),
(LCNull,Shift 197),(LCLChevron,Shift 462),(LCGrkGamma,Shift 181),
(LCGrkDelta,Shift 524),(LCAnnotation,Shift 168),(LCCon,Shift 459),
(LCCharacterLiteral,Shift 105)]]@slrp'a683;
val slrp'a681=[[(LCPragma,Shift 481),(LCReturn,Shift 655),(LCExit,Shift 527),
(LCIdentifier,Shift 447),(LCFor,Reduce(("name_colon_opt",0),0)),
(LCWhile,Reduce(("name_colon_opt",0),0)),(LCTill,Reduce(("name_colon_opt",0),0))
,(LCLoop,Reduce(("name_colon_opt",0),0)),
(LCDeclare,Reduce(("name_colon_opt",0),0)),
(LCBegin,Reduce(("name_colon_opt",0),0)),(LCCase,Shift 646),(LCIf,Shift 651),
(LCNull,Shift 197),(LCLChevron,Shift 462),(LCGrkGamma,Shift 181),
(LCGrkDelta,Shift 524),(LCAnnotation,Shift 168),(LCCon,Shift 459),
(LCCharacterLiteral,Shift 105)]]@slrp'a682;
val slrp'a680=[[(LCPragma,Shift 481),(LCReturn,Shift 655),(LCExit,Shift 527),
(LCIdentifier,Shift 447),(LCFor,Reduce(("name_colon_opt",0),0)),
(LCWhile,Reduce(("name_colon_opt",0),0)),(LCTill,Reduce(("name_colon_opt",0),0))
,(LCLoop,Reduce(("name_colon_opt",0),0)),
(LCDeclare,Reduce(("name_colon_opt",0),0)),
(LCBegin,Reduce(("name_colon_opt",0),0)),(LCCase,Shift 646),(LCIf,Shift 651),
(LCNull,Shift 197),(LCLChevron,Shift 462),(LCGrkGamma,Shift 181),
(LCGrkDelta,Shift 524),(LCAnnotation,Shift 168),(LCCon,Shift 459),
(LCCharacterLiteral,Shift 105)]]@slrp'a681;
val slrp'a679=[[(LCPragma,Shift 481),(LCReturn,Shift 655),(LCExit,Shift 527),
(LCIdentifier,Shift 447),(LCFor,Reduce(("name_colon_opt",0),0)),
(LCWhile,Reduce(("name_colon_opt",0),0)),(LCTill,Reduce(("name_colon_opt",0),0))
,(LCLoop,Reduce(("name_colon_opt",0),0)),
(LCDeclare,Reduce(("name_colon_opt",0),0)),
(LCBegin,Reduce(("name_colon_opt",0),0)),(LCCase,Shift 646),(LCIf,Shift 651),
(LCNull,Shift 197),(LCLChevron,Shift 462),(LCGrkGamma,Shift 181),
(LCGrkDelta,Shift 524),(LCAnnotation,Shift 168),(LCCon,Shift 459),
(LCCharacterLiteral,Shift 105)]]@slrp'a680;
val slrp'a678=[[(LCPragma,Shift 481),(LCReturn,Shift 655),(LCExit,Shift 527),
(LCIdentifier,Shift 447),(LCFor,Reduce(("name_colon_opt",0),0)),
(LCWhile,Reduce(("name_colon_opt",0),0)),(LCTill,Reduce(("name_colon_opt",0),0))
,(LCLoop,Reduce(("name_colon_opt",0),0)),
(LCDeclare,Reduce(("name_colon_opt",0),0)),
(LCBegin,Reduce(("name_colon_opt",0),0)),(LCCase,Shift 646),(LCIf,Shift 651),
(LCNull,Shift 197),(LCLChevron,Shift 462),(LCGrkGamma,Shift 181),
(LCGrkDelta,Shift 524),(LCAnnotation,Shift 168),(LCCon,Shift 459),
(LCCharacterLiteral,Shift 105)]]@slrp'a679;
val slrp'a677=[[(LCPragma,Shift 481),(LCReturn,Shift 655),(LCExit,Shift 527),
(LCIdentifier,Shift 447),(LCFor,Reduce(("name_colon_opt",0),0)),
(LCWhile,Reduce(("name_colon_opt",0),0)),(LCTill,Reduce(("name_colon_opt",0),0))
,(LCLoop,Reduce(("name_colon_opt",0),0)),
(LCDeclare,Reduce(("name_colon_opt",0),0)),
(LCBegin,Reduce(("name_colon_opt",0),0)),(LCCase,Shift 646),(LCIf,Shift 651),
(LCNull,Shift 197),(LCLChevron,Shift 462),(LCGrkGamma,Shift 181),
(LCGrkDelta,Shift 524),(LCAnnotation,Shift 168),(LCCon,Shift 459),
(LCCharacterLiteral,Shift 105)]]@slrp'a678;
val slrp'a676=[[(LCPragma,Shift 481),(LCReturn,Shift 655),(LCExit,Shift 527),
(LCIdentifier,Shift 447),(LCFor,Reduce(("name_colon_opt",0),0)),
(LCWhile,Reduce(("name_colon_opt",0),0)),(LCTill,Reduce(("name_colon_opt",0),0))
,(LCLoop,Reduce(("name_colon_opt",0),0)),
(LCDeclare,Reduce(("name_colon_opt",0),0)),
(LCBegin,Reduce(("name_colon_opt",0),0)),(LCCase,Shift 646),(LCIf,Shift 651),
(LCNull,Shift 197),(LCLChevron,Shift 462),(LCGrkGamma,Shift 181),
(LCGrkDelta,Shift 524),(LCAnnotation,Shift 168),(LCCon,Shift 459),
(LCCharacterLiteral,Shift 105)]]@slrp'a677;
val slrp'a675=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOthers,Shift 112),(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a676;
val slrp'a674=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a675;
val slrp'a673=[[(LCConstant,Shift 664),(LCLStrachey,Shift 236),
(LCMinus,Shift 155),(LCPlus,Shift 154),(LCStringLiteral,Shift 141),
(LCCharacterLiteral,Shift 450),(LCNumericLiteral,Shift 140),(LCNot,Shift 610),
(LCAbs,Shift 609),(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a674;
val slrp'a672=[[(LCLStrachey,Shift 236),(LCOthers,Shift 221),(LCMinus,Shift 155)
,(LCPlus,Shift 154),(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a673;
val slrp'a671=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOthers,Shift 112),(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a672;
val slrp'a670=[[(LCConstant,Shift 664),(LCLStrachey,Shift 236),
(LCMinus,Shift 155),(LCPlus,Shift 154),(LCStringLiteral,Shift 141),
(LCCharacterLiteral,Shift 450),(LCNumericLiteral,Shift 140),(LCNot,Shift 610),
(LCAbs,Shift 609),(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a671;
val slrp'a669=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a670;
val slrp'a668=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a669;
val slrp'a667=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a668;
val slrp'a666=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a667;
val slrp'a665=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96),(LCBecomes,Shift 642)]]@slrp'a666;
val slrp'a664=[[(LCLStrachey,Shift 236),(LCReverse,Shift 661),
(LCMinus,Shift 155),(LCPlus,Shift 154),(LCStringLiteral,Shift 141),
(LCCharacterLiteral,Shift 450),(LCNumericLiteral,Shift 140),(LCNot,Shift 610),
(LCAbs,Shift 609),(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a665;
val slrp'a663=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a664;
val slrp'a662=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a663;
val slrp'a661=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a662;
val slrp'a660=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a661;
val slrp'a659=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a660;
val slrp'a658=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a659;
val slrp'a657=[[(LCReferences,Shift 490),(LCWith,Shift 489),(LCPragma,Shift 481)
,(LCAnnotation,Shift 563),(LCPackage,Reduce(("context_clause",0),0)),
(LCProcedure,Reduce(("context_clause",0),0)),
(LCFunction,Reduce(("context_clause",0),0)),
(LCSeparate,Reduce(("context_clause",0),0)),(LCUse,Shift 509),
(LCLChevron,Shift 462),(LCAnnotationLabel,Shift 440),(LCSpecLabel,Shift 485),
(LCStmtLabel,Shift 439),(LCDecLabel,Shift 438),(LCVPartLabel,Shift 437),
(LCPPartLabel,Shift 436),(LCCompLabel,Shift 435),(LCArbitraryAda,Shift 13),
(LCImplementedBy,Shift 693),(LCRefinedBy,Shift 691)]]@slrp'a658;
val slrp'a656=[[(LCSemi,Shift 249),(LCLStrachey,Shift 236),(LCMinus,Shift 155),
(LCPlus,Shift 154),(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a657;
val slrp'a655=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a656;
val slrp'a654=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a655;
val slrp'a653=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a654;
val slrp'a652=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a653;
val slrp'a651=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a652;
val slrp'a650=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOthers,Shift 115),(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a651;
val slrp'a649=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a650;
val slrp'a648=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a649;
val slrp'a647=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a648;
val slrp'a646=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a647;
val slrp'a645=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a646;
val slrp'a644=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a645;
val slrp'a643=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a644;
val slrp'a642=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a643;
val slrp'a641=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCElse,Shift 637),(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a642;
val slrp'a640=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCThen,Shift 635),(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a641;
val slrp'a639=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a640;
val slrp'a638=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a639;
val slrp'a637=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a638;
val slrp'a636=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a637;
val slrp'a635=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a636;
val slrp'a634=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96),(LCLessGreat,Shift 72)]]@slrp'a635;
val slrp'a633=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a634;
val slrp'a632=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a633;
val slrp'a631=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a632;
val slrp'a630=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a631;
val slrp'a629=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a630;
val slrp'a628=[[(LCPragma,Shift 481),(LCType,Shift 551),
(LCImplement,Reduce(("basic_dec_list",0),1)),(LCSubType,Shift 44),
(LCIdentifier,Shift 443),(LCAnnotation,Shift 22)]]@slrp'a629;
val slrp'a627=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a628;
val slrp'a626=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a627;
val slrp'a625=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a626;
val slrp'a624=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a625;
val slrp'a623=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a624;
val slrp'a622=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a623;
val slrp'a621=[[(LCLStrachey,Shift 236),(LCMinus,Shift 155),(LCPlus,Shift 154),
(LCStringLiteral,Shift 141),(LCCharacterLiteral,Shift 450),
(LCNumericLiteral,Shift 140),(LCNot,Shift 610),(LCAbs,Shift 609),
(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a622;
val slrp'a620=[[(LCPragma,Shift 481),(LCType,Shift 551),(LCSubType,Shift 44),
(LCIdentifier,Shift 443),(LCAnnotation,Shift 22)]]@slrp'a621;
val slrp'a619=[[(LCSeparate,Shift 484),(LCPackage,Shift 505),
(LCFunction,Shift 532),(LCProcedure,Shift 530)]]@slrp'a620;
val slrp'a618=[[(LCLStrachey,Shift 236),(LCStringLiteral,Shift 141),
(LCCharacterLiteral,Shift 450),(LCNumericLiteral,Shift 140),(LCNot,Shift 610),
(LCAbs,Shift 609),(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a619;
val slrp'a617=[[(LCLStrachey,Shift 236),(LCStringLiteral,Shift 141),
(LCCharacterLiteral,Shift 450),(LCNumericLiteral,Shift 140),(LCNot,Shift 610),
(LCAbs,Shift 609),(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a618;
val slrp'a616=[[(LCReferences,Shift 490),(LCWith,Shift 489),(LCPragma,Shift 481)
,(LCAnnotation,Shift 563),(LCPackage,Reduce(("context_clause",0),0)),
(LCProcedure,Reduce(("context_clause",0),0)),
(LCFunction,Reduce(("context_clause",0),0)),
(LCSeparate,Reduce(("context_clause",0),0)),(LCEos,Reduce(("compilation",1),1)),
(LCUse,Shift 509),(LCLChevron,Shift 462)]]@slrp'a617;
val slrp'a615=[[(LCLimited,Shift 347),(LCPrivate,Shift 342),(LCRecord,Shift 550)
,(LCArray,Shift 495),(LCDelta,Shift 621),(LCDigits,Shift 620),(LCOrd,Shift 487),
(LCRRange,Shift 628)]]@slrp'a616;
val slrp'a614=[[(LCLimited,Shift 344),(LCPrivate,Shift 340),(LCRecord,Shift 550)
,(LCArray,Shift 495),(LCDelta,Shift 621),(LCDigits,Shift 620),(LCOrd,Shift 487),
(LCRRange,Shift 628)]]@slrp'a615;
val slrp'a613=[[(LCLStrachey,Shift 236),(LCStringLiteral,Shift 141),
(LCCharacterLiteral,Shift 450),(LCNumericLiteral,Shift 140),(LCNot,Shift 610),
(LCAbs,Shift 609),(LCOrd,Shift 694),(LCIdentifier,Shift 96)]]@slrp'a614;
val slrp'a612=[[(LCReferences,Shift 490),(LCWith,Shift 489),(LCPragma,Shift 481)
,(LCAnnotation,Shift 563),(LCPackage,Reduce(("context_clause",0),0)),
(LCProcedure,Reduce(("context_clause",0),0)),
(LCFunction,Reduce(("context_clause",0),0)),
(LCSeparate,Reduce(("context_clause",0),0)),(LCUse,Shift 509),
(LCLChevron,Shift 462)]]@slrp'a613;
val slrp'a611=[[(LCLStrachey,Shift 236),(LCStringLiteral,Shift 141),
(LCCharacterLiteral,Shift 450),(LCNumericLiteral,Shift 140),(LCOrd,Shift 694),
(LCIdentifier,Shift 96)]]@slrp'a612;
val slrp'a610=[[(LCLStrachey,Shift 236),(LCStringLiteral,Shift 141),
(LCCharacterLiteral,Shift 450),(LCNumericLiteral,Shift 140),(LCOrd,Shift 694),
(LCIdentifier,Shift 96)]]@slrp'a611;
val slrp'a609=[[(LCLStrachey,Shift 236),(LCStringLiteral,Shift 141),
(LCCharacterLiteral,Shift 450),(LCNumericLiteral,Shift 140),(LCOrd,Shift 694),
(LCIdentifier,Shift 96)]]@slrp'a610;
val slrp'a608=[[(LCReferences,Shift 490),(LCWith,Shift 489),(LCUse,Shift 509),
(LCAnnotation,Shift 559),(LCEos,Reduce(("annotation_list",0),0)),
(LCSemi,Reduce(("annotation_list",0),0)),(LCIs,Reduce(("annotation_list",0),0)),
(LCRenames,Reduce(("annotation_list",0),0)),
(LCPackage,Reduce(("annotation_list",0),0)),
(LCProcedure,Reduce(("annotation_list",0),0)),
(LCFunction,Reduce(("annotation_list",0),0)),
(LCSeparate,Reduce(("annotation_list",0),0)),(LCLChevron,Shift 462)]]@slrp'a609;
val slrp'a607=[[(LCReferences,Shift 490),(LCWith,Shift 489),(LCUse,Shift 509),
(LCAnnotation,Shift 559),(LCEos,Reduce(("annotation_list",0),0)),
(LCSemi,Reduce(("annotation_list",0),0)),(LCIs,Reduce(("annotation_list",0),0)),
(LCRenames,Reduce(("annotation_list",0),0)),
(LCPackage,Reduce(("annotation_list",0),0)),
(LCProcedure,Reduce(("annotation_list",0),0)),
(LCFunction,Reduce(("annotation_list",0),0)),
(LCSeparate,Reduce(("annotation_list",0),0)),(LCLChevron,Shift 462)]]@slrp'a608;
val slrp'a606=[[(LCReferences,Shift 490),(LCWith,Shift 489),(LCUse,Shift 509),
(LCAnnotation,Shift 559),(LCEos,Reduce(("annotation_list",0),0)),
(LCSemi,Reduce(("annotation_list",0),0)),(LCIs,Reduce(("annotation_list",0),0)),
(LCRenames,Reduce(("annotation_list",0),0)),
(LCPackage,Reduce(("annotation_list",0),0)),
(LCProcedure,Reduce(("annotation_list",0),0)),
(LCFunction,Reduce(("annotation_list",0),0)),
(LCSeparate,Reduce(("annotation_list",0),0)),(LCLChevron,Shift 462)]]@slrp'a607;
val slrp'a605=[[(LCReferences,Shift 490),(LCWith,Shift 489),(LCUse,Shift 509),
(LCAnnotation,Shift 559),(LCEos,Reduce(("annotation_list",0),0)),
(LCSemi,Reduce(("annotation_list",0),0)),(LCIs,Reduce(("annotation_list",0),0)),
(LCRenames,Reduce(("annotation_list",0),0)),
(LCPackage,Reduce(("annotation_list",0),0)),
(LCProcedure,Reduce(("annotation_list",0),0)),
(LCFunction,Reduce(("annotation_list",0),0)),
(LCSeparate,Reduce(("annotation_list",0),0)),(LCLChevron,Shift 462)]]@slrp'a606;
val slrp'a604=[[(LCXor,Shift 638),(LCOr,Shift 640),(LCAnd,Shift 639),
(LCSemi,Reduce(("expression",0),1)),(LCRRange,Reduce(("expression",0),1)),
(LCDigits,Reduce(("expression",0),1)),(LCDelta,Reduce(("expression",0),1)),
(LCBecomes,Reduce(("expression",0),1)),(LCComma,Reduce(("expression",0),1)),
(LCCrd,Reduce(("expression",0),1)),(LCThen,Reduce(("expression",0),1)),
(LCIs,Reduce(("expression",0),1)),(LCGoesTo,Reduce(("expression",0),1)),
(LCBar,Reduce(("expression",0),1)),(LCTill,Reduce(("expression",0),1)),
(LCLoop,Reduce(("expression",0),1))]]@slrp'a605;
val slrp'a603=[[(LCPackage,Shift 321),(LCFunction,Shift 532),
(LCProcedure,Shift 530)]]@slrp'a604;
val slrp'a602=[[(LCGoesTo,Reduce(("discrete_range",1),1)),
(LCBar,Reduce(("discrete_range",1),1)),(LCTill,Reduce(("discrete_range",1),1)),
(LCLoop,Reduce(("discrete_range",1),1)),(LCDelta,Shift 621),(LCDigits,Shift 620)
,(LCRRange,Shift 628),(LCBecomes,Reduce(("subtype_indication",1),1)),
(LCSemi,Reduce(("subtype_indication",1),1)),
(LCCrd,Reduce(("subtype_indication",1),1)),
(LCComma,Reduce(("subtype_indication",1),1))]]@slrp'a603;
val slrp'a601=[[(LCOrd,Shift 568),(LCGrkDelta,Shift 590),
(LCSemi,Reduce(("procedure_specification_1",0),2)),
(LCAnnotation,Reduce(("procedure_specification_1",0),2)),
(LCLChevron,Reduce(("procedure_specification_1",0),2)),
(LCIs,Reduce(("procedure_specification_1",0),2)),
(LCRenames,Reduce(("procedure_specification_1",0),2)),(LCGrkGamma,Shift 181)]]@slrp'a602;
val slrp'a600=[[(LCEnd,Reduce(("component_pack",0),1)),
(LCCharacterLiteral,Shift 105),(LCIdentifier,Shift 96)]]@slrp'a601;
val slrp'a599=[[(LCRRange,Shift 633),(LCDelta,Shift 621),(LCDigits,Shift 620),
(LCBecomes,Reduce(("subtype_indication",1),1)),
(LCSemi,Reduce(("subtype_indication",1),1)),
(LCCrd,Reduce(("subtype_indication",1),1)),
(LCComma,Reduce(("subtype_indication",1),1))]]@slrp'a600;
val slrp'a598=[[(LCCharacterLiteral,Shift 105),(LCIdentifier,Shift 96)]]@slrp'a599;
val slrp'a597=[[(LCIdentifier,Shift 512),(LCCharacterLiteral,Shift 105)]]@slrp'a598;
val slrp'a596=[[(LCStringLiteral,Shift 279),(LCCharacterLiteral,Shift 105),
(LCIdentifier,Shift 96)]]@slrp'a597;
val slrp'a595=[[(LCDelta,Shift 621),(LCDigits,Shift 620),(LCRRange,Shift 628),
(LCBecomes,Reduce(("subtype_indication",1),1)),
(LCSemi,Reduce(("subtype_indication",1),1)),
(LCCrd,Reduce(("subtype_indication",1),1)),
(LCComma,Reduce(("subtype_indication",1),1))]]@slrp'a596;
val slrp'a594=[[(LCRem,Shift 159),(LCMod,Shift 158),(LCDiv,Shift 157),
(LCStar,Shift 156),(LCAmpersand,Shift 153),(LCMinus,Shift 152),
(LCPlus,Shift 151),(LCBecomes,Reduce(("binoptail",0),0)),
(LCSemi,Reduce(("binoptail",0),0)),(LCRRange,Reduce(("binoptail",0),0)),
(LCDigits,Reduce(("binoptail",0),0)),(LCDelta,Reduce(("binoptail",0),0)),
(LCComma,Reduce(("binoptail",0),0)),(LCCrd,Reduce(("binoptail",0),0)),
(LCAnd,Reduce(("binoptail",0),0)),(LCOr,Reduce(("binoptail",0),0)),
(LCXor,Reduce(("binoptail",0),0)),(LCGreaterEquals,Reduce(("binoptail",0),0)),
(LCGreaterThan,Reduce(("binoptail",0),0)),
(LCLessEquals,Reduce(("binoptail",0),0)),(LCLessThan,Reduce(("binoptail",0),0)),
(LCNotEquals,Reduce(("binoptail",0),0)),(LCEquals,Reduce(("binoptail",0),0)),
(LCIn,Reduce(("binoptail",0),0)),(LCNot,Reduce(("binoptail",0),0)),
(LCDotDot,Reduce(("binoptail",0),0)),(LCThen,Reduce(("binoptail",0),0)),
(LCIs,Reduce(("binoptail",0),0)),(LCGoesTo,Reduce(("binoptail",0),0)),
(LCBar,Reduce(("binoptail",0),0)),(LCTill,Reduce(("binoptail",0),0)),
(LCLoop,Reduce(("binoptail",0),0))]]@slrp'a595;
val slrp'a593=[[(LCRem,Shift 159),(LCMod,Shift 158),(LCDiv,Shift 157),
(LCStar,Shift 156),(LCAmpersand,Shift 153),(LCMinus,Shift 152),
(LCPlus,Shift 151),(LCBecomes,Reduce(("binoptail",0),0)),
(LCSemi,Reduce(("binoptail",0),0)),(LCRRange,Reduce(("binoptail",0),0)),
(LCDigits,Reduce(("binoptail",0),0)),(LCDelta,Reduce(("binoptail",0),0)),
(LCComma,Reduce(("binoptail",0),0)),(LCCrd,Reduce(("binoptail",0),0)),
(LCAnd,Reduce(("binoptail",0),0)),(LCOr,Reduce(("binoptail",0),0)),
(LCXor,Reduce(("binoptail",0),0)),(LCGreaterEquals,Reduce(("binoptail",0),0)),
(LCGreaterThan,Reduce(("binoptail",0),0)),
(LCLessEquals,Reduce(("binoptail",0),0)),(LCLessThan,Reduce(("binoptail",0),0)),
(LCNotEquals,Reduce(("binoptail",0),0)),(LCEquals,Reduce(("binoptail",0),0)),
(LCIn,Reduce(("binoptail",0),0)),(LCNot,Reduce(("binoptail",0),0)),
(LCDotDot,Reduce(("binoptail",0),0)),(LCThen,Reduce(("binoptail",0),0)),
(LCIs,Reduce(("binoptail",0),0)),(LCGoesTo,Reduce(("binoptail",0),0)),
(LCBar,Reduce(("binoptail",0),0)),(LCTill,Reduce(("binoptail",0),0)),
(LCLoop,Reduce(("binoptail",0),0))]]@slrp'a594;
val slrp'a592=[[(LCRem,Shift 159),(LCMod,Shift 158),(LCDiv,Shift 157),
(LCStar,Shift 156),(LCAmpersand,Shift 153),(LCMinus,Shift 152),
(LCPlus,Shift 151),(LCBecomes,Reduce(("binoptail",0),0)),
(LCSemi,Reduce(("binoptail",0),0)),(LCRRange,Reduce(("binoptail",0),0)),
(LCDigits,Reduce(("binoptail",0),0)),(LCDelta,Reduce(("binoptail",0),0)),
(LCComma,Reduce(("binoptail",0),0)),(LCCrd,Reduce(("binoptail",0),0)),
(LCAnd,Reduce(("binoptail",0),0)),(LCOr,Reduce(("binoptail",0),0)),
(LCXor,Reduce(("binoptail",0),0)),(LCGreaterEquals,Reduce(("binoptail",0),0)),
(LCGreaterThan,Reduce(("binoptail",0),0)),
(LCLessEquals,Reduce(("binoptail",0),0)),(LCLessThan,Reduce(("binoptail",0),0)),
(LCNotEquals,Reduce(("binoptail",0),0)),(LCEquals,Reduce(("binoptail",0),0)),
(LCIn,Reduce(("binoptail",0),0)),(LCNot,Reduce(("binoptail",0),0)),
(LCDotDot,Reduce(("binoptail",0),0)),(LCThen,Reduce(("binoptail",0),0)),
(LCIs,Reduce(("binoptail",0),0)),(LCGoesTo,Reduce(("binoptail",0),0)),
(LCBar,Reduce(("binoptail",0),0)),(LCTill,Reduce(("binoptail",0),0)),
(LCLoop,Reduce(("binoptail",0),0))]]@slrp'a593;
val slrp'a591=[[(LCGrkXi,Shift 531),(LCZ,Shift 573),(LCLBrace,Shift 178),
(LCLSqBrack,Shift 175)]]@slrp'a592;
val slrp'a590=[[(LCGrkDelta,Shift 590),
(LCSemi,Reduce(("procedure_specification_1",2),3)),
(LCAnnotation,Reduce(("procedure_specification_1",2),3)),
(LCLChevron,Reduce(("procedure_specification_1",2),3)),
(LCIs,Reduce(("procedure_specification_1",2),3)),
(LCRenames,Reduce(("procedure_specification_1",2),3)),(LCGrkGamma,Shift 181)]]@slrp'a591;
val slrp'a589=[[(LCGreaterEquals,Shift 150),(LCGreaterThan,Shift 149),
(LCLessEquals,Shift 148),(LCLessThan,Shift 147),(LCNotEquals,Shift 146),
(LCEquals,Shift 145),(LCNot,Shift 129),(LCIn,Shift 630),
(LCSemi,Reduce(("relation",0),1)),(LCRRange,Reduce(("relation",0),1)),
(LCDigits,Reduce(("relation",0),1)),(LCDelta,Reduce(("relation",0),1)),
(LCBecomes,Reduce(("relation",0),1)),(LCComma,Reduce(("relation",0),1)),
(LCCrd,Reduce(("relation",0),1)),(LCAnd,Reduce(("relation",0),1)),
(LCOr,Reduce(("relation",0),1)),(LCXor,Reduce(("relation",0),1)),
(LCThen,Reduce(("relation",0),1)),(LCIs,Reduce(("relation",0),1)),
(LCGoesTo,Reduce(("relation",0),1)),(LCBar,Reduce(("relation",0),1)),
(LCTill,Reduce(("relation",0),1)),(LCLoop,Reduce(("relation",0),1))]]@slrp'a590;
val slrp'a588=[[(LCOrd,Shift 695),
(LCSemi,Reduce(("function_specification_1",2),5)),
(LCAnnotation,Reduce(("function_specification_1",2),5)),
(LCLChevron,Reduce(("function_specification_1",2),5)),
(LCIs,Reduce(("function_specification_1",2),5)),
(LCRenames,Reduce(("function_specification_1",2),5)),(LCGrkXi,Shift 531),
(LCPrimedIdentifier,Shift 104),(LCDot,Shift 449)]]@slrp'a589;
val slrp'a587=[[(LCOrd,Shift 695),
(LCSemi,Reduce(("function_specification_1",0),4)),
(LCAnnotation,Reduce(("function_specification_1",0),4)),
(LCLChevron,Reduce(("function_specification_1",0),4)),
(LCIs,Reduce(("function_specification_1",0),4)),
(LCRenames,Reduce(("function_specification_1",0),4)),(LCGrkXi,Shift 531),
(LCPrimedIdentifier,Shift 104),(LCDot,Shift 449)]]@slrp'a588;
val slrp'a586=[[(LCCharacterLiteral,Shift 105),(LCIdentifier,Shift 96)]]@slrp'a587;
val slrp'a585=[[(LCCharacterLiteral,Shift 105),(LCIdentifier,Shift 96)]]@slrp'a586;
val slrp'a584=[[(LCCharacterLiteral,Shift 105),(LCIdentifier,Shift 96)]]@slrp'a585;
val slrp'a583=[[(LCCharacterLiteral,Shift 105),(LCIdentifier,Shift 96)]]@slrp'a584;
val slrp'a582=[[(LCNumericLiteral,Shift 185),(LCCharacterLiteral,Shift 105),
(LCIdentifier,Shift 96)]]@slrp'a583;
val slrp'a581=[[(LCCharacterLiteral,Shift 105),(LCIdentifier,Shift 96)]]@slrp'a582;
val slrp'a580=[[(LCCharacterLiteral,Shift 105),(LCIdentifier,Shift 96)]]@slrp'a581;
val slrp'a579=[[(LCCharacterLiteral,Shift 105),(LCIdentifier,Shift 96)]]@slrp'a580;
val slrp'a578=[[(LCCharacterLiteral,Shift 105),(LCIdentifier,Shift 96)]]@slrp'a579;
val slrp'a577=[[(LCAnnotation,Shift 559),(LCEos,Reduce(("annotation_list",0),0))
,(LCSemi,Reduce(("annotation_list",0),0)),(LCIs,Reduce(("annotation_list",0),0))
,(LCRenames,Reduce(("annotation_list",0),0)),
(LCPackage,Reduce(("annotation_list",0),0)),
(LCProcedure,Reduce(("annotation_list",0),0)),
(LCFunction,Reduce(("annotation_list",0),0)),
(LCSeparate,Reduce(("annotation_list",0),0)),(LCLChevron,Shift 462)]]@slrp'a578;
val slrp'a576=[[(LCBegin,Shift 682),(LCDeclare,Shift 704),(LCFor,Shift 504),
(LCWhile,Shift 653),(LCTill,Reduce(("iteration_scheme_opt",0),0)),
(LCLoop,Reduce(("iteration_scheme_opt",0),0))]]@slrp'a577;
val slrp'a575=[[(LCAnnotation,Shift 559),(LCEos,Reduce(("annotation_list",0),0))
,(LCSemi,Reduce(("annotation_list",0),0)),(LCIs,Reduce(("annotation_list",0),0))
,(LCRenames,Reduce(("annotation_list",0),0)),
(LCPackage,Reduce(("annotation_list",0),0)),
(LCProcedure,Reduce(("annotation_list",0),0)),
(LCFunction,Reduce(("annotation_list",0),0)),
(LCSeparate,Reduce(("annotation_list",0),0)),(LCLChevron,Shift 462)]]@slrp'a576;
val slrp'a574=[[(LCGrkXi,Shift 531),(LCLBrace,Shift 172),(LCLSqBrack,Shift 169)]]@slrp'a575;
val slrp'a573=[[(LCGrkGamma,Shift 181),(LCGrkDelta,Shift 524)]]@slrp'a574;
val slrp'a572=[[(LCRenames,Shift 577),(LCOrd,Shift 695),(LCPrime,Shift 451),
(LCBecomes,Reduce(("primary",3),1)),(LCSemi,Reduce(("primary",3),1)),
(LCRRange,Reduce(("primary",3),1)),(LCDigits,Reduce(("primary",3),1)),
(LCDelta,Reduce(("primary",3),1)),(LCComma,Reduce(("primary",3),1)),
(LCCrd,Reduce(("primary",3),1)),(LCAnd,Reduce(("primary",3),1)),
(LCOr,Reduce(("primary",3),1)),(LCXor,Reduce(("primary",3),1)),
(LCGreaterEquals,Reduce(("primary",3),1)),
(LCGreaterThan,Reduce(("primary",3),1)),(LCLessEquals,Reduce(("primary",3),1)),
(LCLessThan,Reduce(("primary",3),1)),(LCNotEquals,Reduce(("primary",3),1)),
(LCEquals,Reduce(("primary",3),1)),(LCIn,Reduce(("primary",3),1)),
(LCNot,Reduce(("primary",3),1)),(LCDotDot,Reduce(("primary",3),1)),
(LCAmpersand,Reduce(("primary",3),1)),(LCMinus,Reduce(("primary",3),1)),
(LCPlus,Reduce(("primary",3),1)),(LCRem,Reduce(("primary",3),1)),
(LCMod,Reduce(("primary",3),1)),(LCDiv,Reduce(("primary",3),1)),
(LCStar,Reduce(("primary",3),1)),(LCStarStar,Reduce(("primary",3),1)),
(LCThen,Reduce(("primary",3),1)),(LCIs,Reduce(("primary",3),1)),
(LCGoesTo,Reduce(("primary",3),1)),(LCBar,Reduce(("primary",3),1)),
(LCTill,Reduce(("primary",3),1)),(LCLoop,Reduce(("primary",3),1)),
(LCPrimedIdentifier,Shift 104),(LCDot,Shift 449)]]@slrp'a573;
val slrp'a571=[[(LCIs,Shift 613),(LCOrd,Shift 568)]]@slrp'a572;
val slrp'a570=[[(LCIdentifier,Shift 443)]]@slrp'a571;
val slrp'a569=[[(LCIdentifier,Shift 443)]]@slrp'a570;
val slrp'a568=[[(LCEnd,Reduce(("component_list",0),1)),(LCIdentifier,Shift 443)]]@slrp'a569;
val slrp'a567=[[(LCIn,Shift 476),(LCOut,Shift 284),
(LCIdentifier,Reduce(("mode",0),0)),(LCCharacterLiteral,Reduce(("mode",0),0))]]@slrp'a568;
val slrp'a566=[[(LCStringLiteral,Shift 279),(LCIdentifier,Shift 543)]]@slrp'a567;
val slrp'a565=[[(LCAnnotation,Shift 559),(LCEos,Reduce(("annotation_list",0),0))
,(LCSemi,Reduce(("annotation_list",0),0)),(LCIs,Reduce(("annotation_list",0),0))
,(LCRenames,Reduce(("annotation_list",0),0)),
(LCPackage,Reduce(("annotation_list",0),0)),
(LCProcedure,Reduce(("annotation_list",0),0)),
(LCFunction,Reduce(("annotation_list",0),0)),
(LCSeparate,Reduce(("annotation_list",0),0)),(LCLChevron,Shift 462)]]@slrp'a566;
val slrp'a564=[[(LCAnnotation,Shift 559),(LCEos,Reduce(("annotation_list",0),0))
,(LCSemi,Reduce(("annotation_list",0),0)),(LCIs,Reduce(("annotation_list",0),0))
,(LCRenames,Reduce(("annotation_list",0),0)),
(LCPackage,Reduce(("annotation_list",0),0)),
(LCProcedure,Reduce(("annotation_list",0),0)),
(LCFunction,Reduce(("annotation_list",0),0)),
(LCSeparate,Reduce(("annotation_list",0),0)),(LCLChevron,Shift 462)]]@slrp'a565;
val slrp'a563=[[(LCAnnotation,Shift 559),(LCEos,Reduce(("annotation_list",0),0))
,(LCSemi,Reduce(("annotation_list",0),0)),(LCIs,Reduce(("annotation_list",0),0))
,(LCRenames,Reduce(("annotation_list",0),0)),
(LCPackage,Reduce(("annotation_list",0),0)),
(LCProcedure,Reduce(("annotation_list",0),0)),
(LCFunction,Reduce(("annotation_list",0),0)),
(LCSeparate,Reduce(("annotation_list",0),0)),(LCLChevron,Shift 462)]]@slrp'a564;
val slrp'a562=[[(LCAnnotation,Shift 559),(LCEos,Reduce(("annotation_list",0),0))
,(LCSemi,Reduce(("annotation_list",0),0)),(LCIs,Reduce(("annotation_list",0),0))
,(LCRenames,Reduce(("annotation_list",0),0)),
(LCPackage,Reduce(("annotation_list",0),0)),
(LCProcedure,Reduce(("annotation_list",0),0)),
(LCFunction,Reduce(("annotation_list",0),0)),
(LCSeparate,Reduce(("annotation_list",0),0)),(LCLChevron,Shift 462)]]@slrp'a563;
val slrp'a561=[[(LCAnnotation,Shift 559),(LCEos,Reduce(("annotation_list",0),0))
,(LCSemi,Reduce(("annotation_list",0),0)),(LCIs,Reduce(("annotation_list",0),0))
,(LCRenames,Reduce(("annotation_list",0),0)),
(LCPackage,Reduce(("annotation_list",0),0)),
(LCProcedure,Reduce(("annotation_list",0),0)),
(LCFunction,Reduce(("annotation_list",0),0)),
(LCSeparate,Reduce(("annotation_list",0),0)),(LCLChevron,Shift 462)]]@slrp'a562;
val slrp'a560=[[(LCAnnotation,Shift 559),(LCEos,Reduce(("annotation_list",0),0))
,(LCSemi,Reduce(("annotation_list",0),0)),(LCIs,Reduce(("annotation_list",0),0))
,(LCRenames,Reduce(("annotation_list",0),0)),
(LCPackage,Reduce(("annotation_list",0),0)),
(LCProcedure,Reduce(("annotation_list",0),0)),
(LCFunction,Reduce(("annotation_list",0),0)),
(LCSeparate,Reduce(("annotation_list",0),0)),(LCLChevron,Shift 462)]]@slrp'a561;
val slrp'a559=[[(LCAnnotation,Shift 559),(LCEos,Reduce(("annotation_list",0),0))
,(LCSemi,Reduce(("annotation_list",0),0)),(LCIs,Reduce(("annotation_list",0),0))
,(LCRenames,Reduce(("annotation_list",0),0)),
(LCPackage,Reduce(("annotation_list",0),0)),
(LCProcedure,Reduce(("annotation_list",0),0)),
(LCFunction,Reduce(("annotation_list",0),0)),
(LCSeparate,Reduce(("annotation_list",0),0)),(LCLChevron,Shift 462)]]@slrp'a560;
val slrp'a558=[[(LCAnnotation,Shift 559),(LCEos,Reduce(("annotation_list",0),0))
,(LCSemi,Reduce(("annotation_list",0),0)),(LCIs,Reduce(("annotation_list",0),0))
,(LCRenames,Reduce(("annotation_list",0),0)),
(LCPackage,Reduce(("annotation_list",0),0)),
(LCProcedure,Reduce(("annotation_list",0),0)),
(LCFunction,Reduce(("annotation_list",0),0)),
(LCSeparate,Reduce(("annotation_list",0),0)),(LCLChevron,Shift 462)]]@slrp'a559;
val slrp'a557=[[(LCCrd,Shift 602),(LCOrd,Shift 695),
(LCPrimedIdentifier,Shift 104),(LCDot,Shift 449)]]@slrp'a558;
val slrp'a556=[[(LCDot,Shift 515),(LCSemi,Shift 364),(LCOrd,Shift 695),
(LCPrimedIdentifier,Shift 104)]]@slrp'a557;
val slrp'a555=[[(LCOrd,Shift 695),(LCBecomes,Shift 647),
(LCCrd,Reduce(("parameter_specification",0),4)),
(LCSemi,Reduce(("parameter_specification",0),4)),(LCPrimedIdentifier,Shift 104),
(LCDot,Shift 449)]]@slrp'a556;
val slrp'a554=[[(LCOrd,Shift 695),(LCSemi,Shift 295),(LCBecomes,Shift 645),
(LCPrimedIdentifier,Shift 104),(LCDot,Shift 449)]]@slrp'a555;
val slrp'a553=[[(LCOrd,Shift 695),(LCPrime,Shift 451),
(LCBecomes,Reduce(("primary",3),1)),(LCSemi,Reduce(("primary",3),1)),
(LCRRange,Reduce(("primary",3),1)),(LCDigits,Reduce(("primary",3),1)),
(LCDelta,Reduce(("primary",3),1)),(LCComma,Reduce(("primary",3),1)),
(LCCrd,Reduce(("primary",3),1)),(LCAnd,Reduce(("primary",3),1)),
(LCOr,Reduce(("primary",3),1)),(LCXor,Reduce(("primary",3),1)),
(LCGreaterEquals,Reduce(("primary",3),1)),
(LCGreaterThan,Reduce(("primary",3),1)),(LCLessEquals,Reduce(("primary",3),1)),
(LCLessThan,Reduce(("primary",3),1)),(LCNotEquals,Reduce(("primary",3),1)),
(LCEquals,Reduce(("primary",3),1)),(LCIn,Reduce(("primary",3),1)),
(LCNot,Reduce(("primary",3),1)),(LCDotDot,Reduce(("primary",3),1)),
(LCAmpersand,Reduce(("primary",3),1)),(LCMinus,Reduce(("primary",3),1)),
(LCPlus,Reduce(("primary",3),1)),(LCRem,Reduce(("primary",3),1)),
(LCMod,Reduce(("primary",3),1)),(LCDiv,Reduce(("primary",3),1)),
(LCStar,Reduce(("primary",3),1)),(LCStarStar,Reduce(("primary",3),1)),
(LCThen,Reduce(("primary",3),1)),(LCIs,Reduce(("primary",3),1)),
(LCGoesTo,Reduce(("primary",3),1)),(LCBar,Reduce(("primary",3),1)),
(LCTill,Reduce(("primary",3),1)),(LCLoop,Reduce(("primary",3),1)),
(LCPrimedIdentifier,Shift 104),(LCDot,Shift 449)]]@slrp'a554;
val slrp'a552=[[(LCIdentifier,Shift 570)]]@slrp'a553;
val slrp'a551=[[(LCIdentifier,Shift 443)]]@slrp'a552;
val slrp'a550=[[(LCColon,Shift 672)]]@slrp'a551;
val slrp'a549=[[(LCAnnotation,Shift 559),(LCEos,Reduce(("annotation_list",0),0))
,(LCSemi,Reduce(("annotation_list",0),0)),(LCIs,Reduce(("annotation_list",0),0))
,(LCRenames,Reduce(("annotation_list",0),0)),
(LCPackage,Reduce(("annotation_list",0),0)),
(LCProcedure,Reduce(("annotation_list",0),0)),
(LCFunction,Reduce(("annotation_list",0),0)),
(LCSeparate,Reduce(("annotation_list",0),0)),(LCLChevron,Shift 462)]]@slrp'a550;
val slrp'a548=[[(LCPragma,Shift 481),(LCPackage,Reduce(("pragma_list",0),0)),
(LCProcedure,Reduce(("pragma_list",0),0)),
(LCFunction,Reduce(("pragma_list",0),0)),
(LCSeparate,Reduce(("pragma_list",0),0)),
(LCReferences,Reduce(("pragma_list",0),0)),
(LCAnnotation,Reduce(("pragma_list",0),0)),
(LCLChevron,Reduce(("pragma_list",0),0)),(LCWith,Reduce(("pragma_list",0),0)),
(LCUse,Reduce(("pragma_list",0),0))]]@slrp'a549;
val slrp'a547=[[(LCPragma,Shift 481),(LCPackage,Reduce(("pragma_list",0),0)),
(LCProcedure,Reduce(("pragma_list",0),0)),
(LCFunction,Reduce(("pragma_list",0),0)),
(LCSeparate,Reduce(("pragma_list",0),0)),
(LCReferences,Reduce(("pragma_list",0),0)),
(LCAnnotation,Reduce(("pragma_list",0),0)),
(LCLChevron,Reduce(("pragma_list",0),0)),(LCWith,Reduce(("pragma_list",0),0)),
(LCUse,Reduce(("pragma_list",0),0))]]@slrp'a548;
val slrp'a546=[[(LCPragma,Shift 481),(LCPackage,Reduce(("pragma_list",0),0)),
(LCProcedure,Reduce(("pragma_list",0),0)),
(LCFunction,Reduce(("pragma_list",0),0)),
(LCSeparate,Reduce(("pragma_list",0),0)),
(LCReferences,Reduce(("pragma_list",0),0)),
(LCAnnotation,Reduce(("pragma_list",0),0)),
(LCLChevron,Reduce(("pragma_list",0),0)),(LCWith,Reduce(("pragma_list",0),0)),
(LCUse,Reduce(("pragma_list",0),0))]]@slrp'a547;
val slrp'a545=[[(LCUsing,Shift 619),(LCLChevron,Reduce(("using_list",0),1)),
(LCFunction,Reduce(("using_list",0),1)),(LCPackage,Reduce(("using_list",0),1)),
(LCIdentifier,Reduce(("using_list",0),1)),
(LCAnnotation,Reduce(("using_list",0),1)),(LCSubType,Reduce(("using_list",0),1))
,(LCPragma,Reduce(("using_list",0),1)),(LCProcedure,Reduce(("using_list",0),1)),
(LCType,Reduce(("using_list",0),1)),(LCFor,Reduce(("using_list",0),1)),
(LCUse,Reduce(("using_list",0),1)),(LCBegin,Reduce(("using_list",0),1))]]@slrp'a546;
val slrp'a544=[[(LCOrd,Shift 568),(LCReturn,Shift 582)]]@slrp'a545;
val slrp'a543=[[(LCFor,Shift 504),(LCWhile,Shift 653),
(LCTill,Reduce(("iteration_scheme_opt",0),0)),
(LCLoop,Reduce(("iteration_scheme_opt",0),0))]]@slrp'a544;
val slrp'a542=[[(LCWhen,Shift 671),(LCEnd,Reduce(("others_part",0),0))]]@slrp'a543;
val slrp'a541=[[(LCAt,Shift 625),(LCOrd,Shift 695),
(LCPrimedIdentifier,Shift 104),(LCDot,Shift 449)]]@slrp'a542;
val slrp'a540=[[(LCDot,Shift 514),(LCOrd,Shift 695),
(LCPrimedIdentifier,Shift 104)]]@slrp'a541;
val slrp'a539=[[(LCRenames,Shift 580),(LCOrd,Shift 695),
(LCPrimedIdentifier,Shift 104),(LCDot,Shift 449)]]@slrp'a540;
val slrp'a538=[[(LCSemi,Shift 360),(LCOrd,Shift 695),
(LCPrimedIdentifier,Shift 104),(LCDot,Shift 449)]]@slrp'a539;
val slrp'a537=[[(LCSemi,Shift 357),(LCOrd,Shift 695),
(LCPrimedIdentifier,Shift 104),(LCDot,Shift 449)]]@slrp'a538;
val slrp'a536=[[(LCOrd,Shift 695),(LCCrd,Shift 187),
(LCPrimedIdentifier,Shift 104),(LCDot,Shift 449)]]@slrp'a537;
val slrp'a535=[[(LCColon,Shift 669)]]@slrp'a536;
val slrp'a534=[[(LCBBody,Shift 404),(LCIdentifier,Shift 576)]]@slrp'a535;
val slrp'a533=[[(LCIdentifier,Shift 543)]]@slrp'a534;
val slrp'a532=[[(LCLBrace,Shift 271),(LCLSqBrack,Shift 268),(LCZ,Shift 471)]]@slrp'a533;
val slrp'a531=[[(LCIdentifier,Shift 600)]]@slrp'a532;
val slrp'a530=[[(LCSemi,Shift 410),(LCIs,Shift 709),
(LCRenames,Reduce(("subprogram_specification",1),1))]]@slrp'a531;
val slrp'a529=[[(LCSemi,Shift 409),(LCIs,Shift 708),
(LCRenames,Reduce(("subprogram_specification",0),1))]]@slrp'a530;
val slrp'a528=[[(LCIdentifier,Shift 229),(LCSemi,Reduce(("name_opt",0),0)),
(LCWhen,Reduce(("name_opt",0),0))]]@slrp'a529;
val slrp'a527=[[(LCWhen,Shift 667)]]@slrp'a528;
val slrp'a526=[[(LCEos,Reduce(("k_slot",0),3)),
(LCLChevron,Reduce(("k_slot",0),3)),(LCFunction,Reduce(("k_slot",0),3)),
(LCPackage,Reduce(("k_slot",0),3)),(LCIdentifier,Reduce(("k_slot",0),3)),
(LCAnnotation,Reduce(("k_slot",0),3)),(LCSubType,Reduce(("k_slot",0),3)),
(LCPragma,Reduce(("k_slot",0),3)),(LCProcedure,Reduce(("k_slot",0),3)),
(LCType,Reduce(("k_slot",0),3)),(LCFor,Reduce(("k_slot",0),3)),
(LCUse,Reduce(("k_slot",0),3)),(LCCon,Reduce(("k_slot",0),3)),
(LCGrkGamma,Reduce(("k_slot",0),3)),(LCGrkDelta,Reduce(("k_slot",0),3)),
(LCNull,Reduce(("k_slot",0),3)),(LCExit,Reduce(("k_slot",0),3)),
(LCReturn,Reduce(("k_slot",0),3)),(LCIf,Reduce(("k_slot",0),3)),
(LCCase,Reduce(("k_slot",0),3)),(LCCharacterLiteral,Reduce(("k_slot",0),3)),
(LCTill,Reduce(("k_slot",0),3)),(LCLoop,Reduce(("k_slot",0),3)),
(LCWhile,Reduce(("k_slot",0),3)),(LCSemi,Reduce(("k_slot",0),3)),
(LCEnd,Reduce(("k_slot",0),3)),(LCPrivate,Reduce(("k_slot",0),3)),
(LCAuxiliary,Reduce(("k_slot",0),3)),(LCBegin,Reduce(("k_slot",0),3)),
(LCElsIf,Reduce(("k_slot",0),3)),(LCElse,Reduce(("k_slot",0),3)),
(LCWhen,Reduce(("k_slot",0),3)),(LCIs,Reduce(("k_slot",0),3)),
(LCRenames,Reduce(("k_slot",0),3)),(LCSeparate,Reduce(("k_slot",0),3)),
(LCOrd,Shift 581)]]@slrp'a527;
val slrp'a525=[[(LCLBrace,Shift 178),(LCLSqBrack,Shift 175),(LCZ,Shift 461)]]@slrp'a526;
val slrp'a524=[[(LCOrd,Shift 581),(LCEos,Reduce(("statement",3),4)),
(LCAnnotation,Reduce(("statement",3),4)),(LCCon,Reduce(("statement",3),4)),
(LCLChevron,Reduce(("statement",3),4)),(LCGrkGamma,Reduce(("statement",3),4)),
(LCGrkDelta,Reduce(("statement",3),4)),(LCPragma,Reduce(("statement",3),4)),
(LCNull,Reduce(("statement",3),4)),(LCExit,Reduce(("statement",3),4)),
(LCReturn,Reduce(("statement",3),4)),(LCIf,Reduce(("statement",3),4)),
(LCCase,Reduce(("statement",3),4)),(LCIdentifier,Reduce(("statement",3),4)),
(LCCharacterLiteral,Reduce(("statement",3),4)),
(LCTill,Reduce(("statement",3),4)),(LCLoop,Reduce(("statement",3),4)),
(LCFor,Reduce(("statement",3),4)),(LCWhile,Reduce(("statement",3),4)),
(LCElsIf,Reduce(("statement",3),4)),(LCElse,Reduce(("statement",3),4)),
(LCEnd,Reduce(("statement",3),4)),(LCWhen,Reduce(("statement",3),4))]]@slrp'a525;
val slrp'a523=[[(LCOrd,Shift 581),(LCEos,Reduce(("statement",1),1)),
(LCAnnotation,Reduce(("statement",1),1)),(LCCon,Reduce(("statement",1),1)),
(LCLChevron,Reduce(("statement",1),1)),(LCGrkGamma,Reduce(("statement",1),1)),
(LCGrkDelta,Reduce(("statement",1),1)),(LCPragma,Reduce(("statement",1),1)),
(LCNull,Reduce(("statement",1),1)),(LCExit,Reduce(("statement",1),1)),
(LCReturn,Reduce(("statement",1),1)),(LCIf,Reduce(("statement",1),1)),
(LCCase,Reduce(("statement",1),1)),(LCIdentifier,Reduce(("statement",1),1)),
(LCCharacterLiteral,Reduce(("statement",1),1)),
(LCTill,Reduce(("statement",1),1)),(LCLoop,Reduce(("statement",1),1)),
(LCFor,Reduce(("statement",1),1)),(LCWhile,Reduce(("statement",1),1)),
(LCElsIf,Reduce(("statement",1),1)),(LCElse,Reduce(("statement",1),1)),
(LCEnd,Reduce(("statement",1),1)),(LCWhen,Reduce(("statement",1),1))]]@slrp'a524;
val slrp'a522=[[(LCXor,Shift 638),(LCSemi,Reduce(("xor_relation",0),2)),
(LCRRange,Reduce(("xor_relation",0),2)),(LCDigits,Reduce(("xor_relation",0),2)),
(LCDelta,Reduce(("xor_relation",0),2)),(LCBecomes,Reduce(("xor_relation",0),2)),
(LCComma,Reduce(("xor_relation",0),2)),(LCCrd,Reduce(("xor_relation",0),2)),
(LCThen,Reduce(("xor_relation",0),2)),(LCIs,Reduce(("xor_relation",0),2)),
(LCGoesTo,Reduce(("xor_relation",0),2)),(LCBar,Reduce(("xor_relation",0),2)),
(LCTill,Reduce(("xor_relation",0),2)),(LCLoop,Reduce(("xor_relation",0),2))]]@slrp'a523;
val slrp'a521=[[(LCOr,Shift 456),(LCSemi,Reduce(("or_else_relation",0),3)),
(LCRRange,Reduce(("or_else_relation",0),3)),
(LCDigits,Reduce(("or_else_relation",0),3)),
(LCDelta,Reduce(("or_else_relation",0),3)),
(LCBecomes,Reduce(("or_else_relation",0),3)),
(LCComma,Reduce(("or_else_relation",0),3)),
(LCCrd,Reduce(("or_else_relation",0),3)),
(LCThen,Reduce(("or_else_relation",0),3)),
(LCIs,Reduce(("or_else_relation",0),3)),
(LCGoesTo,Reduce(("or_else_relation",0),3)),
(LCBar,Reduce(("or_else_relation",0),3)),
(LCTill,Reduce(("or_else_relation",0),3)),
(LCLoop,Reduce(("or_else_relation",0),3))]]@slrp'a522;
val slrp'a520=[[(LCOr,Shift 636),(LCSemi,Reduce(("or_relation",0),2)),
(LCRRange,Reduce(("or_relation",0),2)),(LCDigits,Reduce(("or_relation",0),2)),
(LCDelta,Reduce(("or_relation",0),2)),(LCBecomes,Reduce(("or_relation",0),2)),
(LCComma,Reduce(("or_relation",0),2)),(LCCrd,Reduce(("or_relation",0),2)),
(LCThen,Reduce(("or_relation",0),2)),(LCIs,Reduce(("or_relation",0),2)),
(LCGoesTo,Reduce(("or_relation",0),2)),(LCBar,Reduce(("or_relation",0),2)),
(LCTill,Reduce(("or_relation",0),2)),(LCLoop,Reduce(("or_relation",0),2))]]@slrp'a521;
val slrp'a519=[[(LCAnd,Shift 455),(LCSemi,Reduce(("and_then_relation",0),3)),
(LCRRange,Reduce(("and_then_relation",0),3)),
(LCDigits,Reduce(("and_then_relation",0),3)),
(LCDelta,Reduce(("and_then_relation",0),3)),
(LCBecomes,Reduce(("and_then_relation",0),3)),
(LCComma,Reduce(("and_then_relation",0),3)),
(LCCrd,Reduce(("and_then_relation",0),3)),
(LCThen,Reduce(("and_then_relation",0),3)),
(LCIs,Reduce(("and_then_relation",0),3)),
(LCGoesTo,Reduce(("and_then_relation",0),3)),
(LCBar,Reduce(("and_then_relation",0),3)),
(LCTill,Reduce(("and_then_relation",0),3)),
(LCLoop,Reduce(("and_then_relation",0),3))]]@slrp'a520;
val slrp'a518=[[(LCAnd,Shift 634),(LCSemi,Reduce(("and_relation",0),2)),
(LCRRange,Reduce(("and_relation",0),2)),(LCDigits,Reduce(("and_relation",0),2)),
(LCDelta,Reduce(("and_relation",0),2)),(LCBecomes,Reduce(("and_relation",0),2)),
(LCComma,Reduce(("and_relation",0),2)),(LCCrd,Reduce(("and_relation",0),2)),
(LCThen,Reduce(("and_relation",0),2)),(LCIs,Reduce(("and_relation",0),2)),
(LCGoesTo,Reduce(("and_relation",0),2)),(LCBar,Reduce(("and_relation",0),2)),
(LCTill,Reduce(("and_relation",0),2)),(LCLoop,Reduce(("and_relation",0),2))]]@slrp'a519;
val slrp'a517=[[(LCAt,Shift 626),(LCRecord,Shift 508),(LCOrd,Shift 694)]]@slrp'a518;
val slrp'a516=[[(LCStringLiteral,Shift 279),(LCIdentifier,Shift 102)]]@slrp'a517;
val slrp'a515=[[(LCStringLiteral,Shift 279),(LCIdentifier,Shift 102)]]@slrp'a516;
val slrp'a514=[[(LCOrd,Shift 695),(LCPrimedIdentifier,Shift 104),
(LCDot,Shift 449)]]@slrp'a515;
val slrp'a513=[[(LCUse,Shift 516),(LCOrd,Reduce(("name",0),1)),
(LCDot,Reduce(("name",0),1)),(LCPrimedIdentifier,Reduce(("name",0),1)),
(LCBecomes,Reduce(("name",0),1)),(LCSemi,Reduce(("name",0),1)),
(LCRRange,Reduce(("name",0),1)),(LCDigits,Reduce(("name",0),1)),
(LCDelta,Reduce(("name",0),1)),(LCComma,Reduce(("name",0),1)),
(LCCrd,Reduce(("name",0),1)),(LCAnd,Reduce(("name",0),1)),
(LCOr,Reduce(("name",0),1)),(LCXor,Reduce(("name",0),1)),
(LCGreaterEquals,Reduce(("name",0),1)),(LCGreaterThan,Reduce(("name",0),1)),
(LCLessEquals,Reduce(("name",0),1)),(LCLessThan,Reduce(("name",0),1)),
(LCNotEquals,Reduce(("name",0),1)),(LCEquals,Reduce(("name",0),1)),
(LCIn,Reduce(("name",0),1)),(LCNot,Reduce(("name",0),1)),
(LCDotDot,Reduce(("name",0),1)),(LCAmpersand,Reduce(("name",0),1)),
(LCMinus,Reduce(("name",0),1)),(LCPlus,Reduce(("name",0),1)),
(LCRem,Reduce(("name",0),1)),(LCMod,Reduce(("name",0),1)),
(LCDiv,Reduce(("name",0),1)),(LCStar,Reduce(("name",0),1)),
(LCStarStar,Reduce(("name",0),1)),(LCPrime,Reduce(("name",0),1)),
(LCGrkXi,Reduce(("name",0),1)),(LCRenames,Reduce(("name",0),1)),
(LCAt,Reduce(("name",0),1)),(LCThen,Reduce(("name",0),1)),
(LCIs,Reduce(("name",0),1)),(LCGoesTo,Reduce(("name",0),1)),
(LCBar,Reduce(("name",0),1)),(LCTill,Reduce(("name",0),1)),
(LCLoop,Reduce(("name",0),1)),(LCAnnotation,Reduce(("name",0),1)),
(LCLChevron,Reduce(("name",0),1))]]@slrp'a514;
val slrp'a512=[[(LCCrd,Reduce(("argument_association",1),1)),
(LCComma,Reduce(("argument_association",1),1)),
(LCGoesTo,Reduce(("discrete_range",1),1)),(LCBar,Reduce(("discrete_range",1),1))
,(LCTill,Reduce(("discrete_range",1),1)),(LCLoop,Reduce(("discrete_range",1),1))
,(LCRRange,Shift 628)]]@slrp'a513;
val slrp'a511=[[(LCComma,Reduce(("expression_list",0),1)),
(LCCrd,Reduce(("expression_list",0),1)),
(LCGoesTo,Reduce(("discrete_range",1),1)),(LCBar,Reduce(("discrete_range",1),1))
,(LCTill,Reduce(("discrete_range",1),1)),(LCLoop,Reduce(("discrete_range",1),1))
,(LCRRange,Shift 628)]]@slrp'a512;
val slrp'a510=[[(LCType,Shift 488),(LCIdentifier,Shift 443)]]@slrp'a511;
val slrp'a509=[[(LCAt,Shift 426),(LCIdentifier,Reduce(("alignment_opt",0),0)),
(LCCharacterLiteral,Reduce(("alignment_opt",0),0))]]@slrp'a510;
val slrp'a508=[[(LCRenames,Shift 595)]]@slrp'a509;
val slrp'a507=[[(LCRenames,Shift 578),(LCIs,Shift 701)]]@slrp'a508;
val slrp'a506=[[(LCBBody,Shift 322),(LCIdentifier,Shift 574)]]@slrp'a507;
val slrp'a505=[[(LCIdentifier,Shift 466)]]@slrp'a506;
val slrp'a504=[[(LCIdentifier,Shift 229),(LCSemi,Reduce(("name_opt",0),0)),
(LCWhen,Reduce(("name_opt",0),0))]]@slrp'a505;
val slrp'a503=[[(LCIdentifier,Shift 229),(LCSemi,Reduce(("name_opt",0),0)),
(LCWhen,Reduce(("name_opt",0),0))]]@slrp'a504;
val slrp'a502=[[(LCIdentifier,Shift 229),(LCSemi,Reduce(("name_opt",0),0)),
(LCWhen,Reduce(("name_opt",0),0))]]@slrp'a503;
val slrp'a501=[[(LCTill,Shift 467),(LCLoop,Reduce(("till_opt",0),0))]]@slrp'a502;
val slrp'a500=[[(LCElsIf,Shift 652),(LCElse,Reduce(("elsif_part",0),0)),
(LCEnd,Reduce(("elsif_part",0),0))]]@slrp'a501;
val slrp'a499=[[(LCElse,Shift 677),(LCEnd,Reduce(("else_part",0),0))]]@slrp'a500;
val slrp'a498=[[(LCElsIf,Shift 652),(LCElse,Reduce(("elsif_part",0),0)),
(LCEnd,Reduce(("elsif_part",0),0))]]@slrp'a499;
val slrp'a497=[[(LCComma,Shift 649),
(LCCrd,Reduce(("positional_association",0),1))]]@slrp'a498;
val slrp'a496=[[(LCOrd,Shift 673)]]@slrp'a497;
val slrp'a495=[[(LCGoesTo,Reduce(("discrete_range",1),1)),
(LCBar,Reduce(("discrete_range",1),1)),(LCTill,Reduce(("discrete_range",1),1)),
(LCLoop,Reduce(("discrete_range",1),1)),(LCRRange,Shift 628)]]@slrp'a496;
val slrp'a494=[[(LCBecomes,Reduce(("fixed_point_constraint",0),1)),
(LCSemi,Reduce(("fixed_point_constraint",0),1)),
(LCCrd,Reduce(("fixed_point_constraint",0),1)),
(LCComma,Reduce(("fixed_point_constraint",0),1)),(LCRRange,Shift 628)]]@slrp'a495;
val slrp'a493=[[(LCBecomes,Reduce(("floating_point_constraint",0),1)),
(LCSemi,Reduce(("floating_point_constraint",0),1)),
(LCCrd,Reduce(("floating_point_constraint",0),1)),
(LCComma,Reduce(("floating_point_constraint",0),1)),(LCRRange,Shift 628)]]@slrp'a494;
val slrp'a492=[[(LCIs,Shift 614)]]@slrp'a493;
val slrp'a491=[[(LCIdentifier,Shift 443)]]@slrp'a492;
val slrp'a490=[[(LCIdentifier,Shift 443)]]@slrp'a491;
val slrp'a489=[[(LCIdentifier,Shift 443)]]@slrp'a490;
val slrp'a488=[[(LCIdentifier,Shift 443)]]@slrp'a489;
val slrp'a487=[[(LCIdentifier,Shift 443)]]@slrp'a488;
val slrp'a486=[[(LCArbitraryAda,Reduce(("any_label",5),1)),
(LCImplementedBy,Shift 692),(LCRefinedBy,Shift 690)]]@slrp'a487;
val slrp'a485=[[(LCOrd,Shift 585)]]@slrp'a486;
val slrp'a484=[[(LCComma,Shift 688),(LCCrd,Reduce(("arg_ass_list",0),1))]]@slrp'a485;
val slrp'a483=[[(LCOrd,Shift 687),(LCSemi,Shift 389)]]@slrp'a484;
val slrp'a482=[[(LCIdentifier,Shift 482)]]@slrp'a483;
val slrp'a481=[[(LCBegin,Shift 685),
(LCEnd,Reduce(("package_implementation",0),1))]]@slrp'a482;
val slrp'a480=[[(LCPrivate,Shift 697),(LCEnd,Shift 304)]]@slrp'a481;
val slrp'a479=[[(LCIs,Shift 701)]]@slrp'a480;
val slrp'a478=[[(LCComma,Shift 666),
(LCCrd,Reduce(("positional_parameter_association",0),1))]]@slrp'a479;
val slrp'a477=[[(LCOut,Shift 285),(LCIdentifier,Reduce(("mode",1),1)),
(LCCharacterLiteral,Reduce(("mode",1),1))]]@slrp'a478;
val slrp'a476=[[(LCColon,Shift 566)]]@slrp'a477;
val slrp'a475=[[(LCSemi,Shift 569),(LCCrd,Reduce(("parlist",0),1))]]@slrp'a476;
val slrp'a474=[[(LCOrd,Shift 568)]]@slrp'a475;
val slrp'a473=[[(LCReturn,Shift 583)]]@slrp'a474;
val slrp'a472=[[(LCLBrace,Shift 265),(LCLSqBrack,Shift 262)]]@slrp'a473;
val slrp'a471=[[(LCRenames,Reduce(("subprogram_specification",1),1)),
(LCSemi,Shift 253)]]@slrp'a472;
val slrp'a470=[[(LCRenames,Reduce(("subprogram_specification",0),1)),
(LCSemi,Shift 252)]]@slrp'a471;
val slrp'a469=[[(LCWhen,Shift 654),(LCSemi,Shift 246)]]@slrp'a470;
val slrp'a468=[[(LCLStrachey,Shift 236)]]@slrp'a469;
val slrp'a467=[[(LCIn,Shift 663)]]@slrp'a468;
val slrp'a466=[[(LCLoop,Shift 680)]]@slrp'a467;
val slrp'a465=[[(LCBar,Shift 668),(LCGoesTo,Reduce(("case_choice_list",0),1))]]@slrp'a466;
val slrp'a464=[[(LCRChevron,Shift 525)]]@slrp'a465;
val slrp'a463=[[(LCKSlot,Shift 463)]]@slrp'a464;
val slrp'a462=[[(LCLBrace,Shift 172),(LCLSqBrack,Shift 169)]]@slrp'a463;
val slrp'a461=[[(LCFatDot,Shift 572)]]@slrp'a462;
val slrp'a460=[[(LCZ,Shift 460)]]@slrp'a461;
val slrp'a459=[[(LCStarStar,Shift 608),(LCBecomes,Reduce(("factor",0),1)),
(LCSemi,Reduce(("factor",0),1)),(LCRRange,Reduce(("factor",0),1)),
(LCDigits,Reduce(("factor",0),1)),(LCDelta,Reduce(("factor",0),1)),
(LCComma,Reduce(("factor",0),1)),(LCCrd,Reduce(("factor",0),1)),
(LCAnd,Reduce(("factor",0),1)),(LCOr,Reduce(("factor",0),1)),
(LCXor,Reduce(("factor",0),1)),(LCGreaterEquals,Reduce(("factor",0),1)),
(LCGreaterThan,Reduce(("factor",0),1)),(LCLessEquals,Reduce(("factor",0),1)),
(LCLessThan,Reduce(("factor",0),1)),(LCNotEquals,Reduce(("factor",0),1)),
(LCEquals,Reduce(("factor",0),1)),(LCIn,Reduce(("factor",0),1)),
(LCNot,Reduce(("factor",0),1)),(LCDotDot,Reduce(("factor",0),1)),
(LCAmpersand,Reduce(("factor",0),1)),(LCMinus,Reduce(("factor",0),1)),
(LCPlus,Reduce(("factor",0),1)),(LCRem,Reduce(("factor",0),1)),
(LCMod,Reduce(("factor",0),1)),(LCDiv,Reduce(("factor",0),1)),
(LCStar,Reduce(("factor",0),1)),(LCThen,Reduce(("factor",0),1)),
(LCIs,Reduce(("factor",0),1)),(LCGoesTo,Reduce(("factor",0),1)),
(LCBar,Reduce(("factor",0),1)),(LCTill,Reduce(("factor",0),1)),
(LCLoop,Reduce(("factor",0),1))]]@slrp'a460;
val slrp'a458=[[(LCDotDot,Shift 622),(LCBecomes,Reduce(("sr_expression",0),1)),
(LCSemi,Reduce(("sr_expression",0),1)),(LCRRange,Reduce(("sr_expression",0),1)),
(LCDigits,Reduce(("sr_expression",0),1)),(LCDelta,Reduce(("sr_expression",0),1))
,(LCComma,Reduce(("sr_expression",0),1)),(LCCrd,Reduce(("sr_expression",0),1)),
(LCAnd,Reduce(("sr_expression",0),1)),(LCOr,Reduce(("sr_expression",0),1)),
(LCXor,Reduce(("sr_expression",0),1)),
(LCGreaterEquals,Reduce(("sr_expression",0),1)),
(LCGreaterThan,Reduce(("sr_expression",0),1)),
(LCLessEquals,Reduce(("sr_expression",0),1)),
(LCLessThan,Reduce(("sr_expression",0),1)),
(LCNotEquals,Reduce(("sr_expression",0),1)),
(LCEquals,Reduce(("sr_expression",0),1)),(LCIn,Reduce(("sr_expression",0),1)),
(LCNot,Reduce(("sr_expression",0),1)),(LCThen,Reduce(("sr_expression",0),1)),
(LCIs,Reduce(("sr_expression",0),1)),(LCGoesTo,Reduce(("sr_expression",0),1)),
(LCBar,Reduce(("sr_expression",0),1)),(LCTill,Reduce(("sr_expression",0),1)),
(LCLoop,Reduce(("sr_expression",0),1))]]@slrp'a459;
val slrp'a457=[[(LCElse,Shift 637)]]@slrp'a458;
val slrp'a456=[[(LCThen,Shift 635)]]@slrp'a457;
val slrp'a455=[[(LCBar,Shift 670),
(LCGoesTo,Reduce(("aggregate_choice_list",1),1))]]@slrp'a456;
val slrp'a454=[[(LCComma,Shift 674),(LCCrd,Reduce(("named_association",0),3))]]@slrp'a455;
val slrp'a453=[[(LCGoesTo,Shift 650)]]@slrp'a454;
val slrp'a452=[[(LCOrd,Shift 694)]]@slrp'a453;
val slrp'a451=[[(LCBecomes,Reduce(("primary",1),1)),
(LCSemi,Reduce(("primary",1),1)),(LCRRange,Reduce(("primary",1),1)),
(LCDigits,Reduce(("primary",1),1)),(LCDelta,Reduce(("primary",1),1)),
(LCComma,Reduce(("primary",1),1)),(LCCrd,Reduce(("primary",1),1)),
(LCAnd,Reduce(("primary",1),1)),(LCOr,Reduce(("primary",1),1)),
(LCXor,Reduce(("primary",1),1)),(LCGreaterEquals,Reduce(("primary",1),1)),
(LCGreaterThan,Reduce(("primary",1),1)),(LCLessEquals,Reduce(("primary",1),1)),
(LCLessThan,Reduce(("primary",1),1)),(LCNotEquals,Reduce(("primary",1),1)),
(LCEquals,Reduce(("primary",1),1)),(LCIn,Reduce(("primary",1),1)),
(LCNot,Reduce(("primary",1),1)),(LCDotDot,Reduce(("primary",1),1)),
(LCAmpersand,Reduce(("primary",1),1)),(LCMinus,Reduce(("primary",1),1)),
(LCPlus,Reduce(("primary",1),1)),(LCRem,Reduce(("primary",1),1)),
(LCMod,Reduce(("primary",1),1)),(LCDiv,Reduce(("primary",1),1)),
(LCStar,Reduce(("primary",1),1)),(LCStarStar,Reduce(("primary",1),1)),
(LCThen,Reduce(("primary",1),1)),(LCIs,Reduce(("primary",1),1)),
(LCGoesTo,Reduce(("primary",1),1)),(LCBar,Reduce(("primary",1),1)),
(LCTill,Reduce(("primary",1),1)),(LCLoop,Reduce(("primary",1),1)),
(LCPrimedIdentifier,Shift 106)]]@slrp'a452;
val slrp'a450=[[(LCIdentifier,Shift 102)]]@slrp'a451;
val slrp'a449=[[(LCUse,Shift 623),(LCOrd,Reduce(("name",3),1)),
(LCDot,Reduce(("name",3),1)),(LCPrimedIdentifier,Reduce(("name",3),1)),
(LCBecomes,Reduce(("name",3),1)),(LCSemi,Reduce(("name",3),1)),
(LCRRange,Reduce(("name",3),1)),(LCDigits,Reduce(("name",3),1)),
(LCDelta,Reduce(("name",3),1)),(LCComma,Reduce(("name",3),1)),
(LCCrd,Reduce(("name",3),1)),(LCAnd,Reduce(("name",3),1)),
(LCOr,Reduce(("name",3),1)),(LCXor,Reduce(("name",3),1)),
(LCGreaterEquals,Reduce(("name",3),1)),(LCGreaterThan,Reduce(("name",3),1)),
(LCLessEquals,Reduce(("name",3),1)),(LCLessThan,Reduce(("name",3),1)),
(LCNotEquals,Reduce(("name",3),1)),(LCEquals,Reduce(("name",3),1)),
(LCIn,Reduce(("name",3),1)),(LCNot,Reduce(("name",3),1)),
(LCDotDot,Reduce(("name",3),1)),(LCAmpersand,Reduce(("name",3),1)),
(LCMinus,Reduce(("name",3),1)),(LCPlus,Reduce(("name",3),1)),
(LCRem,Reduce(("name",3),1)),(LCMod,Reduce(("name",3),1)),
(LCDiv,Reduce(("name",3),1)),(LCStar,Reduce(("name",3),1)),
(LCStarStar,Reduce(("name",3),1)),(LCPrime,Reduce(("name",3),1)),
(LCGrkXi,Reduce(("name",3),1)),(LCRenames,Reduce(("name",3),1)),
(LCAt,Reduce(("name",3),1)),(LCThen,Reduce(("name",3),1)),
(LCIs,Reduce(("name",3),1)),(LCGoesTo,Reduce(("name",3),1)),
(LCBar,Reduce(("name",3),1)),(LCTill,Reduce(("name",3),1)),
(LCLoop,Reduce(("name",3),1)),(LCAnnotation,Reduce(("name",3),1)),
(LCLChevron,Reduce(("name",3),1))]]@slrp'a450;
val slrp'a448=[[(LCColon,Shift 228),(LCOrd,Reduce(("name",0),1)),
(LCDot,Reduce(("name",0),1)),(LCPrimedIdentifier,Reduce(("name",0),1)),
(LCBecomes,Reduce(("name",0),1)),(LCSemi,Reduce(("name",0),1)),
(LCRRange,Reduce(("name",0),1)),(LCDigits,Reduce(("name",0),1)),
(LCDelta,Reduce(("name",0),1)),(LCComma,Reduce(("name",0),1)),
(LCCrd,Reduce(("name",0),1)),(LCAnd,Reduce(("name",0),1)),
(LCOr,Reduce(("name",0),1)),(LCXor,Reduce(("name",0),1)),
(LCGreaterEquals,Reduce(("name",0),1)),(LCGreaterThan,Reduce(("name",0),1)),
(LCLessEquals,Reduce(("name",0),1)),(LCLessThan,Reduce(("name",0),1)),
(LCNotEquals,Reduce(("name",0),1)),(LCEquals,Reduce(("name",0),1)),
(LCIn,Reduce(("name",0),1)),(LCNot,Reduce(("name",0),1)),
(LCDotDot,Reduce(("name",0),1)),(LCAmpersand,Reduce(("name",0),1)),
(LCMinus,Reduce(("name",0),1)),(LCPlus,Reduce(("name",0),1)),
(LCRem,Reduce(("name",0),1)),(LCMod,Reduce(("name",0),1)),
(LCDiv,Reduce(("name",0),1)),(LCStar,Reduce(("name",0),1)),
(LCStarStar,Reduce(("name",0),1)),(LCPrime,Reduce(("name",0),1)),
(LCGrkXi,Reduce(("name",0),1)),(LCRenames,Reduce(("name",0),1)),
(LCAt,Reduce(("name",0),1)),(LCThen,Reduce(("name",0),1)),
(LCIs,Reduce(("name",0),1)),(LCGoesTo,Reduce(("name",0),1)),
(LCBar,Reduce(("name",0),1)),(LCTill,Reduce(("name",0),1)),
(LCLoop,Reduce(("name",0),1)),(LCAnnotation,Reduce(("name",0),1)),
(LCLChevron,Reduce(("name",0),1))]]@slrp'a449;
val slrp'a447=[[(LCComma,Shift 665),(LCCrd,Reduce(("discrete_range_list",0),1))]]@slrp'a448;
val slrp'a446=[[(LCComma,Shift 662),
(LCCrd,Reduce(("index_subtype_definition_list",0),1))]]@slrp'a447;
val slrp'a445=[[(LCGoesTo,Reduce(("discrete_range",0),2)),
(LCBar,Reduce(("discrete_range",0),2)),(LCTill,Reduce(("discrete_range",0),2)),
(LCLoop,Reduce(("discrete_range",0),2)),(LCBecomes,Reduce(("constraint",0),1)),
(LCSemi,Reduce(("constraint",0),1)),(LCCrd,Reduce(("constraint",0),1)),
(LCComma,Reduce(("constraint",0),1))]]@slrp'a446;
val slrp'a444=[[(LCComma,Shift 486),(LCColon,Reduce(("identifier_list",0),1)),
(LCCrd,Reduce(("identifier_list",0),1)),(LCSemi,Reduce(("identifier_list",0),1))
]]@slrp'a445;
val slrp'a443=[[(LCBecomes,Shift 643),(LCSemi,Shift 29)]]@slrp'a444;
val slrp'a442=[[(LCSemi,Shift 350),(LCBecomes,Shift 641)]]@slrp'a443;
val slrp'a441=[[(LCArbitraryAda,Reduce(("any_label",6),1)),
(LCReplacedBy,Shift 548)]]@slrp'a442;
val slrp'a440=[[(LCArbitraryAda,Reduce(("any_label",4),1)),
(LCImplementedBy,Shift 689)]]@slrp'a441;
val slrp'a439=[[(LCArbitraryAda,Reduce(("any_label",3),1)),
(LCReplacedBy,Shift 702)]]@slrp'a440;
val slrp'a438=[[(LCArbitraryAda,Reduce(("any_label",2),1)),
(LCReplacedBy,Shift 699)]]@slrp'a439;
val slrp'a437=[[(LCArbitraryAda,Reduce(("any_label",1),1)),
(LCReplacedBy,Shift 696)]]@slrp'a438;
val slrp'a436=[[(LCArbitraryAda,Reduce(("any_label",0),1)),
(LCReplacedBy,Shift 611)]]@slrp'a437;
val slrp'a435=[[(LCEos,Reduce(("address_clause",0),6)),
(LCLChevron,Reduce(("address_clause",0),6)),
(LCFunction,Reduce(("address_clause",0),6)),
(LCPackage,Reduce(("address_clause",0),6)),
(LCIdentifier,Reduce(("address_clause",0),6)),
(LCAnnotation,Reduce(("address_clause",0),6)),
(LCSubType,Reduce(("address_clause",0),6)),
(LCPragma,Reduce(("address_clause",0),6)),
(LCProcedure,Reduce(("address_clause",0),6)),
(LCType,Reduce(("address_clause",0),6)),(LCFor,Reduce(("address_clause",0),6)),
(LCUse,Reduce(("address_clause",0),6)),(LCEnd,Reduce(("address_clause",0),6)),
(LCPrivate,Reduce(("address_clause",0),6)),
(LCAuxiliary,Reduce(("address_clause",0),6)),
(LCBegin,Reduce(("address_clause",0),6))]]@slrp'a436;
val slrp'a434=[[(LCSemi,Shift 434)]]@slrp'a435;
val slrp'a433=[[(LCEnd,Reduce(("component_clause",0),6)),
(LCIdentifier,Reduce(("component_clause",0),6)),
(LCCharacterLiteral,Reduce(("component_clause",0),6))]]@slrp'a434;
val slrp'a432=[[(LCSemi,Shift 432)]]@slrp'a433;
val slrp'a431=[[(LCRRange,Shift 632)]]@slrp'a432;
val slrp'a430=[[(LCEnd,Reduce(("component_pack",1),2))]]@slrp'a431;
val slrp'a429=[[(LCIdentifier,Reduce(("alignment_opt",1),4)),
(LCCharacterLiteral,Reduce(("alignment_opt",1),4))]]@slrp'a430;
val slrp'a428=[[(LCSemi,Shift 428)]]@slrp'a429;
val slrp'a427=[[(LCMod,Shift 624)]]@slrp'a428;
val slrp'a426=[[(LCEos,Reduce(("record_representation_clause",0),9)),
(LCLChevron,Reduce(("record_representation_clause",0),9)),
(LCFunction,Reduce(("record_representation_clause",0),9)),
(LCPackage,Reduce(("record_representation_clause",0),9)),
(LCIdentifier,Reduce(("record_representation_clause",0),9)),
(LCAnnotation,Reduce(("record_representation_clause",0),9)),
(LCSubType,Reduce(("record_representation_clause",0),9)),
(LCPragma,Reduce(("record_representation_clause",0),9)),
(LCProcedure,Reduce(("record_representation_clause",0),9)),
(LCType,Reduce(("record_representation_clause",0),9)),
(LCFor,Reduce(("record_representation_clause",0),9)),
(LCUse,Reduce(("record_representation_clause",0),9)),
(LCEnd,Reduce(("record_representation_clause",0),9)),
(LCPrivate,Reduce(("record_representation_clause",0),9)),
(LCAuxiliary,Reduce(("record_representation_clause",0),9)),
(LCBegin,Reduce(("record_representation_clause",0),9))]]@slrp'a427;
val slrp'a425=[[(LCSemi,Shift 425)]]@slrp'a426;
val slrp'a424=[[(LCRecord,Shift 424)]]@slrp'a425;
val slrp'a423=[[(LCEnd,Shift 423)]]@slrp'a424;
val slrp'a422=[[(LCEos,Reduce(("enumeration_representation_clause",0),5)),
(LCLChevron,Reduce(("enumeration_representation_clause",0),5)),
(LCFunction,Reduce(("enumeration_representation_clause",0),5)),
(LCPackage,Reduce(("enumeration_representation_clause",0),5)),
(LCIdentifier,Reduce(("enumeration_representation_clause",0),5)),
(LCAnnotation,Reduce(("enumeration_representation_clause",0),5)),
(LCSubType,Reduce(("enumeration_representation_clause",0),5)),
(LCPragma,Reduce(("enumeration_representation_clause",0),5)),
(LCProcedure,Reduce(("enumeration_representation_clause",0),5)),
(LCType,Reduce(("enumeration_representation_clause",0),5)),
(LCFor,Reduce(("enumeration_representation_clause",0),5)),
(LCUse,Reduce(("enumeration_representation_clause",0),5)),
(LCEnd,Reduce(("enumeration_representation_clause",0),5)),
(LCPrivate,Reduce(("enumeration_representation_clause",0),5)),
(LCAuxiliary,Reduce(("enumeration_representation_clause",0),5)),
(LCBegin,Reduce(("enumeration_representation_clause",0),5))]]@slrp'a423;
val slrp'a421=[[(LCSemi,Shift 421)]]@slrp'a422;
val slrp'a420=[[(LCEos,Reduce(("length_clause",0),5)),
(LCLChevron,Reduce(("length_clause",0),5)),
(LCFunction,Reduce(("length_clause",0),5)),
(LCPackage,Reduce(("length_clause",0),5)),
(LCIdentifier,Reduce(("length_clause",0),5)),
(LCAnnotation,Reduce(("length_clause",0),5)),
(LCSubType,Reduce(("length_clause",0),5)),
(LCPragma,Reduce(("length_clause",0),5)),
(LCProcedure,Reduce(("length_clause",0),5)),
(LCType,Reduce(("length_clause",0),5)),(LCFor,Reduce(("length_clause",0),5)),
(LCUse,Reduce(("length_clause",0),5)),(LCEnd,Reduce(("length_clause",0),5)),
(LCPrivate,Reduce(("length_clause",0),5)),
(LCAuxiliary,Reduce(("length_clause",0),5)),
(LCBegin,Reduce(("length_clause",0),5))]]@slrp'a421;
val slrp'a419=[[(LCSemi,Shift 419)]]@slrp'a420;
val slrp'a418=[[(LCEos,Reduce(("type_representation_clause",2),1)),
(LCLChevron,Reduce(("type_representation_clause",2),1)),
(LCFunction,Reduce(("type_representation_clause",2),1)),
(LCPackage,Reduce(("type_representation_clause",2),1)),
(LCIdentifier,Reduce(("type_representation_clause",2),1)),
(LCAnnotation,Reduce(("type_representation_clause",2),1)),
(LCSubType,Reduce(("type_representation_clause",2),1)),
(LCPragma,Reduce(("type_representation_clause",2),1)),
(LCProcedure,Reduce(("type_representation_clause",2),1)),
(LCType,Reduce(("type_representation_clause",2),1)),
(LCFor,Reduce(("type_representation_clause",2),1)),
(LCUse,Reduce(("type_representation_clause",2),1)),
(LCEnd,Reduce(("type_representation_clause",2),1)),
(LCPrivate,Reduce(("type_representation_clause",2),1)),
(LCAuxiliary,Reduce(("type_representation_clause",2),1)),
(LCBegin,Reduce(("type_representation_clause",2),1))]]@slrp'a419;
val slrp'a417=[[(LCEos,Reduce(("type_representation_clause",1),1)),
(LCLChevron,Reduce(("type_representation_clause",1),1)),
(LCFunction,Reduce(("type_representation_clause",1),1)),
(LCPackage,Reduce(("type_representation_clause",1),1)),
(LCIdentifier,Reduce(("type_representation_clause",1),1)),
(LCAnnotation,Reduce(("type_representation_clause",1),1)),
(LCSubType,Reduce(("type_representation_clause",1),1)),
(LCPragma,Reduce(("type_representation_clause",1),1)),
(LCProcedure,Reduce(("type_representation_clause",1),1)),
(LCType,Reduce(("type_representation_clause",1),1)),
(LCFor,Reduce(("type_representation_clause",1),1)),
(LCUse,Reduce(("type_representation_clause",1),1)),
(LCEnd,Reduce(("type_representation_clause",1),1)),
(LCPrivate,Reduce(("type_representation_clause",1),1)),
(LCAuxiliary,Reduce(("type_representation_clause",1),1)),
(LCBegin,Reduce(("type_representation_clause",1),1))]]@slrp'a418;
val slrp'a416=[[(LCEos,Reduce(("type_representation_clause",0),1)),
(LCLChevron,Reduce(("type_representation_clause",0),1)),
(LCFunction,Reduce(("type_representation_clause",0),1)),
(LCPackage,Reduce(("type_representation_clause",0),1)),
(LCIdentifier,Reduce(("type_representation_clause",0),1)),
(LCAnnotation,Reduce(("type_representation_clause",0),1)),
(LCSubType,Reduce(("type_representation_clause",0),1)),
(LCPragma,Reduce(("type_representation_clause",0),1)),
(LCProcedure,Reduce(("type_representation_clause",0),1)),
(LCType,Reduce(("type_representation_clause",0),1)),
(LCFor,Reduce(("type_representation_clause",0),1)),
(LCUse,Reduce(("type_representation_clause",0),1)),
(LCEnd,Reduce(("type_representation_clause",0),1)),
(LCPrivate,Reduce(("type_representation_clause",0),1)),
(LCAuxiliary,Reduce(("type_representation_clause",0),1)),
(LCBegin,Reduce(("type_representation_clause",0),1))]]@slrp'a417;
val slrp'a415=[[(LCEos,Reduce(("representation_clause",1),1)),
(LCLChevron,Reduce(("representation_clause",1),1)),
(LCFunction,Reduce(("representation_clause",1),1)),
(LCPackage,Reduce(("representation_clause",1),1)),
(LCIdentifier,Reduce(("representation_clause",1),1)),
(LCAnnotation,Reduce(("representation_clause",1),1)),
(LCSubType,Reduce(("representation_clause",1),1)),
(LCPragma,Reduce(("representation_clause",1),1)),
(LCProcedure,Reduce(("representation_clause",1),1)),
(LCType,Reduce(("representation_clause",1),1)),
(LCFor,Reduce(("representation_clause",1),1)),
(LCUse,Reduce(("representation_clause",1),1)),
(LCEnd,Reduce(("representation_clause",1),1)),
(LCPrivate,Reduce(("representation_clause",1),1)),
(LCAuxiliary,Reduce(("representation_clause",1),1)),
(LCBegin,Reduce(("representation_clause",1),1))]]@slrp'a416;
val slrp'a414=[[(LCEos,Reduce(("representation_clause",0),1)),
(LCLChevron,Reduce(("representation_clause",0),1)),
(LCFunction,Reduce(("representation_clause",0),1)),
(LCPackage,Reduce(("representation_clause",0),1)),
(LCIdentifier,Reduce(("representation_clause",0),1)),
(LCAnnotation,Reduce(("representation_clause",0),1)),
(LCSubType,Reduce(("representation_clause",0),1)),
(LCPragma,Reduce(("representation_clause",0),1)),
(LCProcedure,Reduce(("representation_clause",0),1)),
(LCType,Reduce(("representation_clause",0),1)),
(LCFor,Reduce(("representation_clause",0),1)),
(LCUse,Reduce(("representation_clause",0),1)),
(LCEnd,Reduce(("representation_clause",0),1)),
(LCPrivate,Reduce(("representation_clause",0),1)),
(LCAuxiliary,Reduce(("representation_clause",0),1)),
(LCBegin,Reduce(("representation_clause",0),1))]]@slrp'a415;
val slrp'a413=[[(LCEos,Reduce(("subunit",1),5)),
(LCLChevron,Reduce(("subunit",1),5)),(LCAnnotation,Reduce(("subunit",1),5)),
(LCPragma,Reduce(("subunit",1),5)),(LCReferences,Reduce(("subunit",1),5)),
(LCPackage,Reduce(("subunit",1),5)),(LCSeparate,Reduce(("subunit",1),5)),
(LCWith,Reduce(("subunit",1),5)),(LCUse,Reduce(("subunit",1),5)),
(LCProcedure,Reduce(("subunit",1),5)),(LCFunction,Reduce(("subunit",1),5))]]@slrp'a414;
val slrp'a412=[[(LCEos,Reduce(("subunit",0),5)),
(LCLChevron,Reduce(("subunit",0),5)),(LCAnnotation,Reduce(("subunit",0),5)),
(LCPragma,Reduce(("subunit",0),5)),(LCReferences,Reduce(("subunit",0),5)),
(LCPackage,Reduce(("subunit",0),5)),(LCSeparate,Reduce(("subunit",0),5)),
(LCWith,Reduce(("subunit",0),5)),(LCUse,Reduce(("subunit",0),5)),
(LCProcedure,Reduce(("subunit",0),5)),(LCFunction,Reduce(("subunit",0),5))]]@slrp'a413;
val slrp'a411=[[(LCEos,Reduce(("body_stub",4),2)),
(LCLChevron,Reduce(("body_stub",4),2)),(LCFunction,Reduce(("body_stub",4),2)),
(LCPackage,Reduce(("body_stub",4),2)),(LCIdentifier,Reduce(("body_stub",4),2)),
(LCAnnotation,Reduce(("body_stub",4),2)),(LCSubType,Reduce(("body_stub",4),2)),
(LCPragma,Reduce(("body_stub",4),2)),(LCProcedure,Reduce(("body_stub",4),2)),
(LCType,Reduce(("body_stub",4),2)),(LCFor,Reduce(("body_stub",4),2)),
(LCUse,Reduce(("body_stub",4),2)),(LCBegin,Reduce(("body_stub",4),2)),
(LCEnd,Reduce(("body_stub",4),2))]]@slrp'a412;
val slrp'a410=[[(LCEos,Reduce(("body_stub",3),2)),
(LCLChevron,Reduce(("body_stub",3),2)),(LCFunction,Reduce(("body_stub",3),2)),
(LCPackage,Reduce(("body_stub",3),2)),(LCIdentifier,Reduce(("body_stub",3),2)),
(LCAnnotation,Reduce(("body_stub",3),2)),(LCSubType,Reduce(("body_stub",3),2)),
(LCPragma,Reduce(("body_stub",3),2)),(LCProcedure,Reduce(("body_stub",3),2)),
(LCType,Reduce(("body_stub",3),2)),(LCFor,Reduce(("body_stub",3),2)),
(LCUse,Reduce(("body_stub",3),2)),(LCBegin,Reduce(("body_stub",3),2)),
(LCEnd,Reduce(("body_stub",3),2))]]@slrp'a411;
val slrp'a409=[[(LCEos,Reduce(("body_stub",2),7)),
(LCLChevron,Reduce(("body_stub",2),7)),(LCFunction,Reduce(("body_stub",2),7)),
(LCPackage,Reduce(("body_stub",2),7)),(LCIdentifier,Reduce(("body_stub",2),7)),
(LCAnnotation,Reduce(("body_stub",2),7)),(LCSubType,Reduce(("body_stub",2),7)),
(LCPragma,Reduce(("body_stub",2),7)),(LCProcedure,Reduce(("body_stub",2),7)),
(LCType,Reduce(("body_stub",2),7)),(LCFor,Reduce(("body_stub",2),7)),
(LCUse,Reduce(("body_stub",2),7)),(LCBegin,Reduce(("body_stub",2),7)),
(LCEnd,Reduce(("body_stub",2),7))]]@slrp'a410;
val slrp'a408=[[(LCSemi,Shift 408)]]@slrp'a409;
val slrp'a407=[[(LCSeparate,Shift 407)]]@slrp'a408;
val slrp'a406=[[(LCIs,Shift 406)]]@slrp'a407;
val slrp'a405=[[(LCIdentifier,Shift 564)]]@slrp'a406;
val slrp'a404=[[(LCEos,Reduce(("body_stub",1),4)),
(LCLChevron,Reduce(("body_stub",1),4)),(LCFunction,Reduce(("body_stub",1),4)),
(LCPackage,Reduce(("body_stub",1),4)),(LCIdentifier,Reduce(("body_stub",1),4)),
(LCAnnotation,Reduce(("body_stub",1),4)),(LCSubType,Reduce(("body_stub",1),4)),
(LCPragma,Reduce(("body_stub",1),4)),(LCProcedure,Reduce(("body_stub",1),4)),
(LCType,Reduce(("body_stub",1),4)),(LCFor,Reduce(("body_stub",1),4)),
(LCUse,Reduce(("body_stub",1),4)),(LCBegin,Reduce(("body_stub",1),4)),
(LCEnd,Reduce(("body_stub",1),4))]]@slrp'a405;
val slrp'a403=[[(LCSemi,Shift 403)]]@slrp'a404;
val slrp'a402=[[(LCEos,Reduce(("body_stub",0),4)),
(LCLChevron,Reduce(("body_stub",0),4)),(LCFunction,Reduce(("body_stub",0),4)),
(LCPackage,Reduce(("body_stub",0),4)),(LCIdentifier,Reduce(("body_stub",0),4)),
(LCAnnotation,Reduce(("body_stub",0),4)),(LCSubType,Reduce(("body_stub",0),4)),
(LCPragma,Reduce(("body_stub",0),4)),(LCProcedure,Reduce(("body_stub",0),4)),
(LCType,Reduce(("body_stub",0),4)),(LCFor,Reduce(("body_stub",0),4)),
(LCUse,Reduce(("body_stub",0),4)),(LCBegin,Reduce(("body_stub",0),4)),
(LCEnd,Reduce(("body_stub",0),4))]]@slrp'a403;
val slrp'a401=[[(LCSemi,Shift 401)]]@slrp'a402;
val slrp'a400=[[(LCPackage,Reduce(("references_clause",0),3)),
(LCProcedure,Reduce(("references_clause",0),3)),
(LCFunction,Reduce(("references_clause",0),3)),
(LCSeparate,Reduce(("references_clause",0),3)),
(LCReferences,Reduce(("references_clause",0),3)),
(LCAnnotation,Reduce(("references_clause",0),3)),
(LCLChevron,Reduce(("references_clause",0),3)),
(LCWith,Reduce(("references_clause",0),3)),
(LCUse,Reduce(("references_clause",0),3))]]@slrp'a401;
val slrp'a399=[[(LCSemi,Shift 399)]]@slrp'a400;
val slrp'a398=[[(LCPackage,Reduce(("with_clause",0),3)),
(LCProcedure,Reduce(("with_clause",0),3)),
(LCFunction,Reduce(("with_clause",0),3)),
(LCSeparate,Reduce(("with_clause",0),3)),(LCPragma,Reduce(("with_clause",0),3)),
(LCReferences,Reduce(("with_clause",0),3)),
(LCAnnotation,Reduce(("with_clause",0),3)),
(LCLChevron,Reduce(("with_clause",0),3)),(LCWith,Reduce(("with_clause",0),3)),
(LCUse,Reduce(("with_clause",0),3))]]@slrp'a399;
val slrp'a397=[[(LCSemi,Shift 397)]]@slrp'a398;
val slrp'a396=[[(LCCrd,Reduce(("argument_association",0),3)),
(LCComma,Reduce(("argument_association",0),3))]]@slrp'a397;
val slrp'a395=[[(LCGoesTo,Shift 648)]]@slrp'a396;
val slrp'a394=[[(LCCrd,Reduce(("arg_ass_list",1),3))]]@slrp'a395;
val slrp'a393=[[(LCEos,Reduce(("pragma",1),6)),
(LCAnnotation,Reduce(("pragma",1),6)),(LCCon,Reduce(("pragma",1),6)),
(LCLChevron,Reduce(("pragma",1),6)),(LCGrkGamma,Reduce(("pragma",1),6)),
(LCGrkDelta,Reduce(("pragma",1),6)),(LCPragma,Reduce(("pragma",1),6)),
(LCNull,Reduce(("pragma",1),6)),(LCExit,Reduce(("pragma",1),6)),
(LCReturn,Reduce(("pragma",1),6)),(LCIf,Reduce(("pragma",1),6)),
(LCCase,Reduce(("pragma",1),6)),(LCIdentifier,Reduce(("pragma",1),6)),
(LCCharacterLiteral,Reduce(("pragma",1),6)),(LCTill,Reduce(("pragma",1),6)),
(LCLoop,Reduce(("pragma",1),6)),(LCFor,Reduce(("pragma",1),6)),
(LCWhile,Reduce(("pragma",1),6)),(LCReferences,Reduce(("pragma",1),6)),
(LCPackage,Reduce(("pragma",1),6)),(LCSeparate,Reduce(("pragma",1),6)),
(LCWith,Reduce(("pragma",1),6)),(LCUse,Reduce(("pragma",1),6)),
(LCProcedure,Reduce(("pragma",1),6)),(LCFunction,Reduce(("pragma",1),6)),
(LCSubType,Reduce(("pragma",1),6)),(LCType,Reduce(("pragma",1),6)),
(LCImplement,Reduce(("pragma",1),6)),(LCElsIf,Reduce(("pragma",1),6)),
(LCElse,Reduce(("pragma",1),6)),(LCEnd,Reduce(("pragma",1),6)),
(LCWhen,Reduce(("pragma",1),6)),(LCPrivate,Reduce(("pragma",1),6)),
(LCAuxiliary,Reduce(("pragma",1),6)),(LCBegin,Reduce(("pragma",1),6))]]@slrp'a394;
val slrp'a392=[[(LCSemi,Shift 392)]]@slrp'a393;
val slrp'a391=[[(LCCrd,Shift 391)]]@slrp'a392;
val slrp'a390=[[(LCEos,Reduce(("pragma",0),3)),
(LCAnnotation,Reduce(("pragma",0),3)),(LCCon,Reduce(("pragma",0),3)),
(LCLChevron,Reduce(("pragma",0),3)),(LCGrkGamma,Reduce(("pragma",0),3)),
(LCGrkDelta,Reduce(("pragma",0),3)),(LCPragma,Reduce(("pragma",0),3)),
(LCNull,Reduce(("pragma",0),3)),(LCExit,Reduce(("pragma",0),3)),
(LCReturn,Reduce(("pragma",0),3)),(LCIf,Reduce(("pragma",0),3)),
(LCCase,Reduce(("pragma",0),3)),(LCIdentifier,Reduce(("pragma",0),3)),
(LCCharacterLiteral,Reduce(("pragma",0),3)),(LCTill,Reduce(("pragma",0),3)),
(LCLoop,Reduce(("pragma",0),3)),(LCFor,Reduce(("pragma",0),3)),
(LCWhile,Reduce(("pragma",0),3)),(LCReferences,Reduce(("pragma",0),3)),
(LCPackage,Reduce(("pragma",0),3)),(LCSeparate,Reduce(("pragma",0),3)),
(LCWith,Reduce(("pragma",0),3)),(LCUse,Reduce(("pragma",0),3)),
(LCProcedure,Reduce(("pragma",0),3)),(LCFunction,Reduce(("pragma",0),3)),
(LCSubType,Reduce(("pragma",0),3)),(LCType,Reduce(("pragma",0),3)),
(LCImplement,Reduce(("pragma",0),3)),(LCElsIf,Reduce(("pragma",0),3)),
(LCElse,Reduce(("pragma",0),3)),(LCEnd,Reduce(("pragma",0),3)),
(LCWhen,Reduce(("pragma",0),3)),(LCPrivate,Reduce(("pragma",0),3)),
(LCAuxiliary,Reduce(("pragma",0),3)),(LCBegin,Reduce(("pragma",0),3))]]@slrp'a391;
val slrp'a389=[[(LCPackage,Reduce(("pragma_list",1),2)),
(LCProcedure,Reduce(("pragma_list",1),2)),
(LCFunction,Reduce(("pragma_list",1),2)),
(LCSeparate,Reduce(("pragma_list",1),2)),
(LCReferences,Reduce(("pragma_list",1),2)),
(LCAnnotation,Reduce(("pragma_list",1),2)),
(LCLChevron,Reduce(("pragma_list",1),2)),(LCWith,Reduce(("pragma_list",1),2)),
(LCUse,Reduce(("pragma_list",1),2))]]@slrp'a390;
val slrp'a388=[[(LCPackage,Reduce(("with_or_use_clause",1),1)),
(LCProcedure,Reduce(("with_or_use_clause",1),1)),
(LCFunction,Reduce(("with_or_use_clause",1),1)),
(LCSeparate,Reduce(("with_or_use_clause",1),1)),
(LCPragma,Reduce(("with_or_use_clause",1),1)),
(LCReferences,Reduce(("with_or_use_clause",1),1)),
(LCAnnotation,Reduce(("with_or_use_clause",1),1)),
(LCLChevron,Reduce(("with_or_use_clause",1),1)),
(LCWith,Reduce(("with_or_use_clause",1),1)),
(LCUse,Reduce(("with_or_use_clause",1),1))]]@slrp'a389;
val slrp'a387=[[(LCPackage,Reduce(("with_or_use_clause",0),1)),
(LCProcedure,Reduce(("with_or_use_clause",0),1)),
(LCFunction,Reduce(("with_or_use_clause",0),1)),
(LCSeparate,Reduce(("with_or_use_clause",0),1)),
(LCPragma,Reduce(("with_or_use_clause",0),1)),
(LCReferences,Reduce(("with_or_use_clause",0),1)),
(LCAnnotation,Reduce(("with_or_use_clause",0),1)),
(LCLChevron,Reduce(("with_or_use_clause",0),1)),
(LCWith,Reduce(("with_or_use_clause",0),1)),
(LCUse,Reduce(("with_or_use_clause",0),1))]]@slrp'a388;
val slrp'a386=[[(LCPackage,Reduce(("annotated_context_clause",2),1)),
(LCProcedure,Reduce(("annotated_context_clause",2),1)),
(LCFunction,Reduce(("annotated_context_clause",2),1)),
(LCSeparate,Reduce(("annotated_context_clause",2),1))]]@slrp'a387;
val slrp'a385=[[(LCPackage,Reduce(("annotated_context_clause",1),2)),
(LCProcedure,Reduce(("annotated_context_clause",1),2)),
(LCFunction,Reduce(("annotated_context_clause",1),2)),
(LCSeparate,Reduce(("annotated_context_clause",1),2))]]@slrp'a386;
val slrp'a384=[[(LCPackage,Reduce(("annotated_context_clause",0),3)),
(LCProcedure,Reduce(("annotated_context_clause",0),3)),
(LCFunction,Reduce(("annotated_context_clause",0),3)),
(LCSeparate,Reduce(("annotated_context_clause",0),3))]]@slrp'a385;
val slrp'a383=[[(LCPackage,Reduce(("context_clause",3),2)),
(LCProcedure,Reduce(("context_clause",3),2)),
(LCFunction,Reduce(("context_clause",3),2)),
(LCSeparate,Reduce(("context_clause",3),2))]]@slrp'a384;
val slrp'a382=[[(LCPackage,Reduce(("context_clause",2),2)),
(LCProcedure,Reduce(("context_clause",2),2)),
(LCFunction,Reduce(("context_clause",2),2)),
(LCSeparate,Reduce(("context_clause",2),2))]]@slrp'a383;
val slrp'a381=[[(LCPackage,Reduce(("context_clause",1),3)),
(LCProcedure,Reduce(("context_clause",1),3)),
(LCFunction,Reduce(("context_clause",1),3)),
(LCSeparate,Reduce(("context_clause",1),3))]]@slrp'a382;
val slrp'a380=[[(LCEos,Reduce(("main_program",0),1)),
(LCLChevron,Reduce(("main_program",0),1)),
(LCAnnotation,Reduce(("main_program",0),1)),
(LCPragma,Reduce(("main_program",0),1)),
(LCReferences,Reduce(("main_program",0),1)),
(LCPackage,Reduce(("main_program",0),1)),
(LCSeparate,Reduce(("main_program",0),1)),(LCWith,Reduce(("main_program",0),1)),
(LCUse,Reduce(("main_program",0),1)),(LCProcedure,Reduce(("main_program",0),1)),
(LCFunction,Reduce(("main_program",0),1))]]@slrp'a381;
val slrp'a379=[[(LCEos,Reduce(("library_unit_body",0),1)),
(LCLChevron,Reduce(("library_unit_body",0),1)),
(LCAnnotation,Reduce(("library_unit_body",0),1)),
(LCPragma,Reduce(("library_unit_body",0),1)),
(LCReferences,Reduce(("library_unit_body",0),1)),
(LCPackage,Reduce(("library_unit_body",0),1)),
(LCSeparate,Reduce(("library_unit_body",0),1)),
(LCWith,Reduce(("library_unit_body",0),1)),
(LCUse,Reduce(("library_unit_body",0),1)),
(LCProcedure,Reduce(("library_unit_body",0),1)),
(LCFunction,Reduce(("library_unit_body",0),1))]]@slrp'a380;
val slrp'a378=[[(LCEos,Reduce(("secondary_unit",1),1)),
(LCLChevron,Reduce(("secondary_unit",1),1)),
(LCAnnotation,Reduce(("secondary_unit",1),1)),
(LCPragma,Reduce(("secondary_unit",1),1)),
(LCReferences,Reduce(("secondary_unit",1),1)),
(LCPackage,Reduce(("secondary_unit",1),1)),
(LCSeparate,Reduce(("secondary_unit",1),1)),
(LCWith,Reduce(("secondary_unit",1),1)),(LCUse,Reduce(("secondary_unit",1),1)),
(LCProcedure,Reduce(("secondary_unit",1),1)),
(LCFunction,Reduce(("secondary_unit",1),1))]]@slrp'a379;
val slrp'a377=[[(LCEos,Reduce(("secondary_unit",0),1)),
(LCLChevron,Reduce(("secondary_unit",0),1)),
(LCAnnotation,Reduce(("secondary_unit",0),1)),
(LCPragma,Reduce(("secondary_unit",0),1)),
(LCReferences,Reduce(("secondary_unit",0),1)),
(LCPackage,Reduce(("secondary_unit",0),1)),
(LCSeparate,Reduce(("secondary_unit",0),1)),
(LCWith,Reduce(("secondary_unit",0),1)),(LCUse,Reduce(("secondary_unit",0),1)),
(LCProcedure,Reduce(("secondary_unit",0),1)),
(LCFunction,Reduce(("secondary_unit",0),1))]]@slrp'a378;
val slrp'a376=[[(LCEos,Reduce(("library_unit",1),1)),
(LCLChevron,Reduce(("library_unit",1),1)),
(LCAnnotation,Reduce(("library_unit",1),1)),
(LCPragma,Reduce(("library_unit",1),1)),
(LCReferences,Reduce(("library_unit",1),1)),
(LCPackage,Reduce(("library_unit",1),1)),
(LCSeparate,Reduce(("library_unit",1),1)),(LCWith,Reduce(("library_unit",1),1)),
(LCUse,Reduce(("library_unit",1),1)),(LCProcedure,Reduce(("library_unit",1),1)),
(LCFunction,Reduce(("library_unit",1),1))]]@slrp'a377;
val slrp'a375=[[(LCEos,Reduce(("library_unit",0),1)),
(LCLChevron,Reduce(("library_unit",0),1)),
(LCAnnotation,Reduce(("library_unit",0),1)),
(LCPragma,Reduce(("library_unit",0),1)),
(LCReferences,Reduce(("library_unit",0),1)),
(LCPackage,Reduce(("library_unit",0),1)),
(LCSeparate,Reduce(("library_unit",0),1)),(LCWith,Reduce(("library_unit",0),1)),
(LCUse,Reduce(("library_unit",0),1)),(LCProcedure,Reduce(("library_unit",0),1)),
(LCFunction,Reduce(("library_unit",0),1))]]@slrp'a376;
val slrp'a374=[[(LCEos,Reduce(("compilation_unit",2),1)),
(LCLChevron,Reduce(("compilation_unit",2),1)),
(LCAnnotation,Reduce(("compilation_unit",2),1)),
(LCPragma,Reduce(("compilation_unit",2),1)),
(LCReferences,Reduce(("compilation_unit",2),1)),
(LCPackage,Reduce(("compilation_unit",2),1)),
(LCSeparate,Reduce(("compilation_unit",2),1)),
(LCWith,Reduce(("compilation_unit",2),1)),
(LCUse,Reduce(("compilation_unit",2),1)),
(LCProcedure,Reduce(("compilation_unit",2),1)),
(LCFunction,Reduce(("compilation_unit",2),1))]]@slrp'a375;
val slrp'a373=[[(LCEos,Reduce(("compilation_unit",1),2)),
(LCLChevron,Reduce(("compilation_unit",1),2)),
(LCAnnotation,Reduce(("compilation_unit",1),2)),
(LCPragma,Reduce(("compilation_unit",1),2)),
(LCReferences,Reduce(("compilation_unit",1),2)),
(LCPackage,Reduce(("compilation_unit",1),2)),
(LCSeparate,Reduce(("compilation_unit",1),2)),
(LCWith,Reduce(("compilation_unit",1),2)),
(LCUse,Reduce(("compilation_unit",1),2)),
(LCProcedure,Reduce(("compilation_unit",1),2)),
(LCFunction,Reduce(("compilation_unit",1),2))]]@slrp'a374;
val slrp'a372=[[(LCEos,Reduce(("compilation_unit",0),2)),
(LCLChevron,Reduce(("compilation_unit",0),2)),
(LCAnnotation,Reduce(("compilation_unit",0),2)),
(LCPragma,Reduce(("compilation_unit",0),2)),
(LCReferences,Reduce(("compilation_unit",0),2)),
(LCPackage,Reduce(("compilation_unit",0),2)),
(LCSeparate,Reduce(("compilation_unit",0),2)),
(LCWith,Reduce(("compilation_unit",0),2)),
(LCUse,Reduce(("compilation_unit",0),2)),
(LCProcedure,Reduce(("compilation_unit",0),2)),
(LCFunction,Reduce(("compilation_unit",0),2))]]@slrp'a373;
val slrp'a371=[[(LCEos,Reduce(("compilation",2),2))]]@slrp'a372;
val slrp'a370=[[(LCEos,Reduce(("compilation",0),1))]]@slrp'a371;
val slrp'a369=[[(LCEos,Reduce(("renaming_declaration",5),4)),
(LCLChevron,Reduce(("renaming_declaration",5),4)),
(LCFunction,Reduce(("renaming_declaration",5),4)),
(LCPackage,Reduce(("renaming_declaration",5),4)),
(LCIdentifier,Reduce(("renaming_declaration",5),4)),
(LCAnnotation,Reduce(("renaming_declaration",5),4)),
(LCSubType,Reduce(("renaming_declaration",5),4)),
(LCPragma,Reduce(("renaming_declaration",5),4)),
(LCProcedure,Reduce(("renaming_declaration",5),4)),
(LCType,Reduce(("renaming_declaration",5),4)),
(LCFor,Reduce(("renaming_declaration",5),4)),
(LCUse,Reduce(("renaming_declaration",5),4)),
(LCEnd,Reduce(("renaming_declaration",5),4)),
(LCPrivate,Reduce(("renaming_declaration",5),4)),
(LCAuxiliary,Reduce(("renaming_declaration",5),4)),
(LCBegin,Reduce(("renaming_declaration",5),4))]]@slrp'a370;
val slrp'a368=[[(LCSemi,Shift 368)]]@slrp'a369;
val slrp'a367=[[(LCEos,Reduce(("renaming_declaration",4),6)),
(LCLChevron,Reduce(("renaming_declaration",4),6)),
(LCFunction,Reduce(("renaming_declaration",4),6)),
(LCPackage,Reduce(("renaming_declaration",4),6)),
(LCIdentifier,Reduce(("renaming_declaration",4),6)),
(LCAnnotation,Reduce(("renaming_declaration",4),6)),
(LCSubType,Reduce(("renaming_declaration",4),6)),
(LCPragma,Reduce(("renaming_declaration",4),6)),
(LCProcedure,Reduce(("renaming_declaration",4),6)),
(LCType,Reduce(("renaming_declaration",4),6)),
(LCFor,Reduce(("renaming_declaration",4),6)),
(LCUse,Reduce(("renaming_declaration",4),6)),
(LCEnd,Reduce(("renaming_declaration",4),6)),
(LCPrivate,Reduce(("renaming_declaration",4),6)),
(LCAuxiliary,Reduce(("renaming_declaration",4),6)),
(LCBegin,Reduce(("renaming_declaration",4),6))]]@slrp'a368;
val slrp'a366=[[(LCSemi,Shift 366)]]@slrp'a367;
val slrp'a365=[[(LCEos,Reduce(("renaming_declaration",3),4)),
(LCLChevron,Reduce(("renaming_declaration",3),4)),
(LCFunction,Reduce(("renaming_declaration",3),4)),
(LCPackage,Reduce(("renaming_declaration",3),4)),
(LCIdentifier,Reduce(("renaming_declaration",3),4)),
(LCAnnotation,Reduce(("renaming_declaration",3),4)),
(LCSubType,Reduce(("renaming_declaration",3),4)),
(LCPragma,Reduce(("renaming_declaration",3),4)),
(LCProcedure,Reduce(("renaming_declaration",3),4)),
(LCType,Reduce(("renaming_declaration",3),4)),
(LCFor,Reduce(("renaming_declaration",3),4)),
(LCUse,Reduce(("renaming_declaration",3),4)),
(LCEnd,Reduce(("renaming_declaration",3),4)),
(LCPrivate,Reduce(("renaming_declaration",3),4)),
(LCAuxiliary,Reduce(("renaming_declaration",3),4)),
(LCBegin,Reduce(("renaming_declaration",3),4))]]@slrp'a366;
val slrp'a364=[[(LCEos,Reduce(("renaming_declaration",2),10)),
(LCLChevron,Reduce(("renaming_declaration",2),10)),
(LCFunction,Reduce(("renaming_declaration",2),10)),
(LCPackage,Reduce(("renaming_declaration",2),10)),
(LCIdentifier,Reduce(("renaming_declaration",2),10)),
(LCAnnotation,Reduce(("renaming_declaration",2),10)),
(LCSubType,Reduce(("renaming_declaration",2),10)),
(LCPragma,Reduce(("renaming_declaration",2),10)),
(LCProcedure,Reduce(("renaming_declaration",2),10)),
(LCType,Reduce(("renaming_declaration",2),10)),
(LCFor,Reduce(("renaming_declaration",2),10)),
(LCUse,Reduce(("renaming_declaration",2),10)),
(LCEnd,Reduce(("renaming_declaration",2),10)),
(LCPrivate,Reduce(("renaming_declaration",2),10)),
(LCAuxiliary,Reduce(("renaming_declaration",2),10)),
(LCBegin,Reduce(("renaming_declaration",2),10))]]@slrp'a365;
val slrp'a363=[[(LCSemi,Shift 363)]]@slrp'a364;
val slrp'a362=[[(LCReturn,Shift 579)]]@slrp'a363;
val slrp'a361=[[(LCEos,Reduce(("renaming_declaration",1),6)),
(LCLChevron,Reduce(("renaming_declaration",1),6)),
(LCFunction,Reduce(("renaming_declaration",1),6)),
(LCPackage,Reduce(("renaming_declaration",1),6)),
(LCIdentifier,Reduce(("renaming_declaration",1),6)),
(LCAnnotation,Reduce(("renaming_declaration",1),6)),
(LCSubType,Reduce(("renaming_declaration",1),6)),
(LCPragma,Reduce(("renaming_declaration",1),6)),
(LCProcedure,Reduce(("renaming_declaration",1),6)),
(LCType,Reduce(("renaming_declaration",1),6)),
(LCFor,Reduce(("renaming_declaration",1),6)),
(LCUse,Reduce(("renaming_declaration",1),6)),
(LCEnd,Reduce(("renaming_declaration",1),6)),
(LCPrivate,Reduce(("renaming_declaration",1),6)),
(LCAuxiliary,Reduce(("renaming_declaration",1),6)),
(LCBegin,Reduce(("renaming_declaration",1),6))]]@slrp'a362;
val slrp'a360=[[(LCRenames,Shift 578)]]@slrp'a361;
val slrp'a359=[[(LCIdentifier,Shift 562)]]@slrp'a360;
val slrp'a358=[[(LCEos,Reduce(("renaming_declaration",0),6)),
(LCLChevron,Reduce(("renaming_declaration",0),6)),
(LCFunction,Reduce(("renaming_declaration",0),6)),
(LCPackage,Reduce(("renaming_declaration",0),6)),
(LCIdentifier,Reduce(("renaming_declaration",0),6)),
(LCAnnotation,Reduce(("renaming_declaration",0),6)),
(LCSubType,Reduce(("renaming_declaration",0),6)),
(LCPragma,Reduce(("renaming_declaration",0),6)),
(LCProcedure,Reduce(("renaming_declaration",0),6)),
(LCType,Reduce(("renaming_declaration",0),6)),
(LCFor,Reduce(("renaming_declaration",0),6)),
(LCUse,Reduce(("renaming_declaration",0),6)),
(LCEnd,Reduce(("renaming_declaration",0),6)),
(LCPrivate,Reduce(("renaming_declaration",0),6)),
(LCAuxiliary,Reduce(("renaming_declaration",0),6)),
(LCBegin,Reduce(("renaming_declaration",0),6))]]@slrp'a359;
val slrp'a357=[[(LCEos,Reduce(("use_type_clause",0),4)),
(LCLChevron,Reduce(("use_type_clause",0),4)),
(LCFunction,Reduce(("use_type_clause",0),4)),
(LCPackage,Reduce(("use_type_clause",0),4)),
(LCIdentifier,Reduce(("use_type_clause",0),4)),
(LCAnnotation,Reduce(("use_type_clause",0),4)),
(LCSubType,Reduce(("use_type_clause",0),4)),
(LCPragma,Reduce(("use_type_clause",0),4)),
(LCProcedure,Reduce(("use_type_clause",0),4)),
(LCType,Reduce(("use_type_clause",0),4)),(LCFor,Reduce(("use_type_clause",0),4))
,(LCUse,Reduce(("use_type_clause",0),4)),
(LCSeparate,Reduce(("use_type_clause",0),4)),
(LCReferences,Reduce(("use_type_clause",0),4)),
(LCWith,Reduce(("use_type_clause",0),4)),(LCEnd,Reduce(("use_type_clause",0),4))
,(LCPrivate,Reduce(("use_type_clause",0),4)),
(LCAuxiliary,Reduce(("use_type_clause",0),4)),
(LCBegin,Reduce(("use_type_clause",0),4))]]@slrp'a358;
val slrp'a356=[[(LCSemi,Shift 356)]]@slrp'a357;
val slrp'a355=[[(LCEos,Reduce(("use_package_clause",0),3)),
(LCLChevron,Reduce(("use_package_clause",0),3)),
(LCFunction,Reduce(("use_package_clause",0),3)),
(LCPackage,Reduce(("use_package_clause",0),3)),
(LCIdentifier,Reduce(("use_package_clause",0),3)),
(LCAnnotation,Reduce(("use_package_clause",0),3)),
(LCSubType,Reduce(("use_package_clause",0),3)),
(LCPragma,Reduce(("use_package_clause",0),3)),
(LCProcedure,Reduce(("use_package_clause",0),3)),
(LCType,Reduce(("use_package_clause",0),3)),
(LCFor,Reduce(("use_package_clause",0),3)),
(LCUse,Reduce(("use_package_clause",0),3)),
(LCSeparate,Reduce(("use_package_clause",0),3)),
(LCReferences,Reduce(("use_package_clause",0),3)),
(LCWith,Reduce(("use_package_clause",0),3)),
(LCEnd,Reduce(("use_package_clause",0),3)),
(LCPrivate,Reduce(("use_package_clause",0),3)),
(LCAuxiliary,Reduce(("use_package_clause",0),3)),
(LCBegin,Reduce(("use_package_clause",0),3))]]@slrp'a356;
val slrp'a354=[[(LCSemi,Shift 354)]]@slrp'a355;
val slrp'a353=[[(LCEos,Reduce(("use_clause",1),1)),
(LCLChevron,Reduce(("use_clause",1),1)),(LCFunction,Reduce(("use_clause",1),1)),
(LCPackage,Reduce(("use_clause",1),1)),(LCIdentifier,Reduce(("use_clause",1),1))
,(LCAnnotation,Reduce(("use_clause",1),1)),
(LCSubType,Reduce(("use_clause",1),1)),(LCPragma,Reduce(("use_clause",1),1)),
(LCProcedure,Reduce(("use_clause",1),1)),(LCType,Reduce(("use_clause",1),1)),
(LCFor,Reduce(("use_clause",1),1)),(LCUse,Reduce(("use_clause",1),1)),
(LCSeparate,Reduce(("use_clause",1),1)),
(LCReferences,Reduce(("use_clause",1),1)),(LCWith,Reduce(("use_clause",1),1)),
(LCEnd,Reduce(("use_clause",1),1)),(LCPrivate,Reduce(("use_clause",1),1)),
(LCAuxiliary,Reduce(("use_clause",1),1)),(LCBegin,Reduce(("use_clause",1),1))]]@slrp'a354;
val slrp'a352=[[(LCEos,Reduce(("use_clause",0),1)),
(LCLChevron,Reduce(("use_clause",0),1)),(LCFunction,Reduce(("use_clause",0),1)),
(LCPackage,Reduce(("use_clause",0),1)),(LCIdentifier,Reduce(("use_clause",0),1))
,(LCAnnotation,Reduce(("use_clause",0),1)),
(LCSubType,Reduce(("use_clause",0),1)),(LCPragma,Reduce(("use_clause",0),1)),
(LCProcedure,Reduce(("use_clause",0),1)),(LCType,Reduce(("use_clause",0),1)),
(LCFor,Reduce(("use_clause",0),1)),(LCUse,Reduce(("use_clause",0),1)),
(LCSeparate,Reduce(("use_clause",0),1)),
(LCReferences,Reduce(("use_clause",0),1)),(LCWith,Reduce(("use_clause",0),1)),
(LCEnd,Reduce(("use_clause",0),1)),(LCPrivate,Reduce(("use_clause",0),1)),
(LCAuxiliary,Reduce(("use_clause",0),1)),(LCBegin,Reduce(("use_clause",0),1))]]@slrp'a353;
val slrp'a351=[[(LCEos,Reduce(("deferred_constant_declaration",0),5)),
(LCLChevron,Reduce(("deferred_constant_declaration",0),5)),
(LCFunction,Reduce(("deferred_constant_declaration",0),5)),
(LCPackage,Reduce(("deferred_constant_declaration",0),5)),
(LCIdentifier,Reduce(("deferred_constant_declaration",0),5)),
(LCAnnotation,Reduce(("deferred_constant_declaration",0),5)),
(LCSubType,Reduce(("deferred_constant_declaration",0),5)),
(LCPragma,Reduce(("deferred_constant_declaration",0),5)),
(LCProcedure,Reduce(("deferred_constant_declaration",0),5)),
(LCType,Reduce(("deferred_constant_declaration",0),5)),
(LCFor,Reduce(("deferred_constant_declaration",0),5)),
(LCUse,Reduce(("deferred_constant_declaration",0),5)),
(LCImplement,Reduce(("deferred_constant_declaration",0),5)),
(LCEnd,Reduce(("deferred_constant_declaration",0),5)),
(LCPrivate,Reduce(("deferred_constant_declaration",0),5)),
(LCAuxiliary,Reduce(("deferred_constant_declaration",0),5)),
(LCBegin,Reduce(("deferred_constant_declaration",0),5))]]@slrp'a352;
val slrp'a350=[[(LCEos,Reduce(("private_type_declaration",3),7)),
(LCLChevron,Reduce(("private_type_declaration",3),7)),
(LCFunction,Reduce(("private_type_declaration",3),7)),
(LCPackage,Reduce(("private_type_declaration",3),7)),
(LCIdentifier,Reduce(("private_type_declaration",3),7)),
(LCAnnotation,Reduce(("private_type_declaration",3),7)),
(LCSubType,Reduce(("private_type_declaration",3),7)),
(LCPragma,Reduce(("private_type_declaration",3),7)),
(LCProcedure,Reduce(("private_type_declaration",3),7)),
(LCType,Reduce(("private_type_declaration",3),7)),
(LCFor,Reduce(("private_type_declaration",3),7)),
(LCUse,Reduce(("private_type_declaration",3),7)),
(LCImplement,Reduce(("private_type_declaration",3),7)),
(LCEnd,Reduce(("private_type_declaration",3),7)),
(LCPrivate,Reduce(("private_type_declaration",3),7)),
(LCAuxiliary,Reduce(("private_type_declaration",3),7)),
(LCBegin,Reduce(("private_type_declaration",3),7))]]@slrp'a351;
val slrp'a349=[[(LCSemi,Shift 349)]]@slrp'a350;
val slrp'a348=[[(LCPrivate,Shift 348)]]@slrp'a349;
val slrp'a347=[[(LCEos,Reduce(("private_type_declaration",2),6)),
(LCLChevron,Reduce(("private_type_declaration",2),6)),
(LCFunction,Reduce(("private_type_declaration",2),6)),
(LCPackage,Reduce(("private_type_declaration",2),6)),
(LCIdentifier,Reduce(("private_type_declaration",2),6)),
(LCAnnotation,Reduce(("private_type_declaration",2),6)),
(LCSubType,Reduce(("private_type_declaration",2),6)),
(LCPragma,Reduce(("private_type_declaration",2),6)),
(LCProcedure,Reduce(("private_type_declaration",2),6)),
(LCType,Reduce(("private_type_declaration",2),6)),
(LCFor,Reduce(("private_type_declaration",2),6)),
(LCUse,Reduce(("private_type_declaration",2),6)),
(LCImplement,Reduce(("private_type_declaration",2),6)),
(LCEnd,Reduce(("private_type_declaration",2),6)),
(LCPrivate,Reduce(("private_type_declaration",2),6)),
(LCAuxiliary,Reduce(("private_type_declaration",2),6)),
(LCBegin,Reduce(("private_type_declaration",2),6))]]@slrp'a348;
val slrp'a346=[[(LCSemi,Shift 346)]]@slrp'a347;
val slrp'a345=[[(LCPrivate,Shift 345)]]@slrp'a346;
val slrp'a344=[[(LCEos,Reduce(("private_type_declaration",1),6)),
(LCLChevron,Reduce(("private_type_declaration",1),6)),
(LCFunction,Reduce(("private_type_declaration",1),6)),
(LCPackage,Reduce(("private_type_declaration",1),6)),
(LCIdentifier,Reduce(("private_type_declaration",1),6)),
(LCAnnotation,Reduce(("private_type_declaration",1),6)),
(LCSubType,Reduce(("private_type_declaration",1),6)),
(LCPragma,Reduce(("private_type_declaration",1),6)),
(LCProcedure,Reduce(("private_type_declaration",1),6)),
(LCType,Reduce(("private_type_declaration",1),6)),
(LCFor,Reduce(("private_type_declaration",1),6)),
(LCUse,Reduce(("private_type_declaration",1),6)),
(LCImplement,Reduce(("private_type_declaration",1),6)),
(LCEnd,Reduce(("private_type_declaration",1),6)),
(LCPrivate,Reduce(("private_type_declaration",1),6)),
(LCAuxiliary,Reduce(("private_type_declaration",1),6)),
(LCBegin,Reduce(("private_type_declaration",1),6))]]@slrp'a345;
val slrp'a343=[[(LCSemi,Shift 343)]]@slrp'a344;
val slrp'a342=[[(LCEos,Reduce(("private_type_declaration",0),5)),
(LCLChevron,Reduce(("private_type_declaration",0),5)),
(LCFunction,Reduce(("private_type_declaration",0),5)),
(LCPackage,Reduce(("private_type_declaration",0),5)),
(LCIdentifier,Reduce(("private_type_declaration",0),5)),
(LCAnnotation,Reduce(("private_type_declaration",0),5)),
(LCSubType,Reduce(("private_type_declaration",0),5)),
(LCPragma,Reduce(("private_type_declaration",0),5)),
(LCProcedure,Reduce(("private_type_declaration",0),5)),
(LCType,Reduce(("private_type_declaration",0),5)),
(LCFor,Reduce(("private_type_declaration",0),5)),
(LCUse,Reduce(("private_type_declaration",0),5)),
(LCImplement,Reduce(("private_type_declaration",0),5)),
(LCEnd,Reduce(("private_type_declaration",0),5)),
(LCPrivate,Reduce(("private_type_declaration",0),5)),
(LCAuxiliary,Reduce(("private_type_declaration",0),5)),
(LCBegin,Reduce(("private_type_declaration",0),5))]]@slrp'a343;
val slrp'a341=[[(LCSemi,Shift 341)]]@slrp'a342;
val slrp'a340=[[(LCEnd,Reduce(("package_initialization",0),1))]]@slrp'a341;
val slrp'a339=[[(LCImplement,Reduce(("basic_dec_list",1),2))]]@slrp'a340;
val slrp'a338=[[(LCLChevron,Reduce(("using_dec",0),7)),
(LCFunction,Reduce(("using_dec",0),7)),(LCPackage,Reduce(("using_dec",0),7)),
(LCIdentifier,Reduce(("using_dec",0),7)),
(LCAnnotation,Reduce(("using_dec",0),7)),(LCSubType,Reduce(("using_dec",0),7)),
(LCPragma,Reduce(("using_dec",0),7)),(LCProcedure,Reduce(("using_dec",0),7)),
(LCType,Reduce(("using_dec",0),7)),(LCFor,Reduce(("using_dec",0),7)),
(LCUse,Reduce(("using_dec",0),7)),(LCBegin,Reduce(("using_dec",0),7)),
(LCUsing,Reduce(("using_dec",0),7))]]@slrp'a339;
val slrp'a337=[[(LCSemi,Shift 337)]]@slrp'a338;
val slrp'a336=[[(LCZ,Shift 336)]]@slrp'a337;
val slrp'a335=[[(LCBy,Shift 335)]]@slrp'a336;
val slrp'a334=[[(LCZ,Shift 334)]]@slrp'a335;
val slrp'a333=[[(LCImplement,Shift 333)]]@slrp'a334;
val slrp'a332=[[(LCLChevron,Reduce(("using_list",1),2)),
(LCFunction,Reduce(("using_list",1),2)),(LCPackage,Reduce(("using_list",1),2)),
(LCIdentifier,Reduce(("using_list",1),2)),
(LCAnnotation,Reduce(("using_list",1),2)),(LCSubType,Reduce(("using_list",1),2))
,(LCPragma,Reduce(("using_list",1),2)),(LCProcedure,Reduce(("using_list",1),2)),
(LCType,Reduce(("using_list",1),2)),(LCFor,Reduce(("using_list",1),2)),
(LCUse,Reduce(("using_list",1),2)),(LCBegin,Reduce(("using_list",1),2))]]@slrp'a333;
val slrp'a331=[[(LCEnd,Reduce(("package_implementation",2),4))]]@slrp'a332;
val slrp'a330=[[(LCBegin,Shift 686)]]@slrp'a331;
val slrp'a329=[[(LCEnd,Reduce(("package_implementation",1),3))]]@slrp'a330;
val slrp'a328=[[(LCEos,Reduce(("package_body",0),9)),
(LCLChevron,Reduce(("package_body",0),9)),
(LCAnnotation,Reduce(("package_body",0),9)),
(LCPragma,Reduce(("package_body",0),9)),
(LCReferences,Reduce(("package_body",0),9)),
(LCPackage,Reduce(("package_body",0),9)),
(LCSeparate,Reduce(("package_body",0),9)),(LCWith,Reduce(("package_body",0),9)),
(LCUse,Reduce(("package_body",0),9)),(LCProcedure,Reduce(("package_body",0),9)),
(LCFunction,Reduce(("package_body",0),9))]]@slrp'a329;
val slrp'a327=[[(LCSemi,Shift 327)]]@slrp'a328;
val slrp'a326=[[(LCIdentifier,Shift 326)]]@slrp'a327;
val slrp'a325=[[(LCEnd,Shift 325)]]@slrp'a326;
val slrp'a324=[[(LCIs,Shift 710)]]@slrp'a325;
val slrp'a323=[[(LCIdentifier,Shift 561)]]@slrp'a324;
val slrp'a322=[[(LCBBody,Shift 322)]]@slrp'a323;
val slrp'a321=[[(LCEos,Reduce(("pdec",2),1)),(LCEnd,Reduce(("pdec",2),1)),
(LCLChevron,Reduce(("pdec",2),1)),(LCAnnotation,Reduce(("pdec",2),1)),
(LCSubType,Reduce(("pdec",2),1)),(LCPragma,Reduce(("pdec",2),1)),
(LCFunction,Reduce(("pdec",2),1)),(LCPackage,Reduce(("pdec",2),1)),
(LCType,Reduce(("pdec",2),1)),(LCIdentifier,Reduce(("pdec",2),1)),
(LCFor,Reduce(("pdec",2),1)),(LCUse,Reduce(("pdec",2),1)),
(LCProcedure,Reduce(("pdec",2),1))]]@slrp'a322;
val slrp'a320=[[(LCEos,Reduce(("pdec",1),1)),(LCEnd,Reduce(("pdec",1),1)),
(LCLChevron,Reduce(("pdec",1),1)),(LCAnnotation,Reduce(("pdec",1),1)),
(LCSubType,Reduce(("pdec",1),1)),(LCPragma,Reduce(("pdec",1),1)),
(LCFunction,Reduce(("pdec",1),1)),(LCPackage,Reduce(("pdec",1),1)),
(LCType,Reduce(("pdec",1),1)),(LCIdentifier,Reduce(("pdec",1),1)),
(LCFor,Reduce(("pdec",1),1)),(LCUse,Reduce(("pdec",1),1)),
(LCProcedure,Reduce(("pdec",1),1))]]@slrp'a321;
val slrp'a319=[[(LCEos,Reduce(("pdec",0),1)),(LCEnd,Reduce(("pdec",0),1)),
(LCLChevron,Reduce(("pdec",0),1)),(LCAnnotation,Reduce(("pdec",0),1)),
(LCSubType,Reduce(("pdec",0),1)),(LCPragma,Reduce(("pdec",0),1)),
(LCFunction,Reduce(("pdec",0),1)),(LCPackage,Reduce(("pdec",0),1)),
(LCType,Reduce(("pdec",0),1)),(LCIdentifier,Reduce(("pdec",0),1)),
(LCFor,Reduce(("pdec",0),1)),(LCUse,Reduce(("pdec",0),1)),
(LCProcedure,Reduce(("pdec",0),1))]]@slrp'a320;
val slrp'a318=[[(LCEos,Reduce(("private_part",1),2)),
(LCEnd,Reduce(("private_part",1),2))]]@slrp'a319;
val slrp'a317=[[(LCEos,Reduce(("visdec",4),1)),(LCEnd,Reduce(("visdec",4),1)),
(LCPrivate,Reduce(("visdec",4),1)),(LCAuxiliary,Reduce(("visdec",4),1)),
(LCLChevron,Reduce(("visdec",4),1)),(LCAnnotation,Reduce(("visdec",4),1)),
(LCSubType,Reduce(("visdec",4),1)),(LCPragma,Reduce(("visdec",4),1)),
(LCFunction,Reduce(("visdec",4),1)),(LCPackage,Reduce(("visdec",4),1)),
(LCType,Reduce(("visdec",4),1)),(LCIdentifier,Reduce(("visdec",4),1)),
(LCFor,Reduce(("visdec",4),1)),(LCUse,Reduce(("visdec",4),1)),
(LCProcedure,Reduce(("visdec",4),1))]]@slrp'a318;
val slrp'a316=[[(LCEos,Reduce(("visdec",3),3)),(LCEnd,Reduce(("visdec",3),3)),
(LCPrivate,Reduce(("visdec",3),3)),(LCAuxiliary,Reduce(("visdec",3),3)),
(LCLChevron,Reduce(("visdec",3),3)),(LCAnnotation,Reduce(("visdec",3),3)),
(LCSubType,Reduce(("visdec",3),3)),(LCPragma,Reduce(("visdec",3),3)),
(LCFunction,Reduce(("visdec",3),3)),(LCPackage,Reduce(("visdec",3),3)),
(LCType,Reduce(("visdec",3),3)),(LCIdentifier,Reduce(("visdec",3),3)),
(LCFor,Reduce(("visdec",3),3)),(LCUse,Reduce(("visdec",3),3)),
(LCProcedure,Reduce(("visdec",3),3))]]@slrp'a317;
val slrp'a315=[[(LCSemi,Shift 315)]]@slrp'a316;
val slrp'a314=[[(LCZ,Shift 314)]]@slrp'a315;
val slrp'a313=[[(LCEos,Reduce(("visdec",2),1)),(LCEnd,Reduce(("visdec",2),1)),
(LCPrivate,Reduce(("visdec",2),1)),(LCAuxiliary,Reduce(("visdec",2),1)),
(LCLChevron,Reduce(("visdec",2),1)),(LCAnnotation,Reduce(("visdec",2),1)),
(LCSubType,Reduce(("visdec",2),1)),(LCPragma,Reduce(("visdec",2),1)),
(LCFunction,Reduce(("visdec",2),1)),(LCPackage,Reduce(("visdec",2),1)),
(LCType,Reduce(("visdec",2),1)),(LCIdentifier,Reduce(("visdec",2),1)),
(LCFor,Reduce(("visdec",2),1)),(LCUse,Reduce(("visdec",2),1)),
(LCProcedure,Reduce(("visdec",2),1))]]@slrp'a314;
val slrp'a312=[[(LCEos,Reduce(("visdec",1),1)),(LCEnd,Reduce(("visdec",1),1)),
(LCPrivate,Reduce(("visdec",1),1)),(LCAuxiliary,Reduce(("visdec",1),1)),
(LCLChevron,Reduce(("visdec",1),1)),(LCAnnotation,Reduce(("visdec",1),1)),
(LCSubType,Reduce(("visdec",1),1)),(LCPragma,Reduce(("visdec",1),1)),
(LCFunction,Reduce(("visdec",1),1)),(LCPackage,Reduce(("visdec",1),1)),
(LCType,Reduce(("visdec",1),1)),(LCIdentifier,Reduce(("visdec",1),1)),
(LCFor,Reduce(("visdec",1),1)),(LCUse,Reduce(("visdec",1),1)),
(LCProcedure,Reduce(("visdec",1),1))]]@slrp'a313;
val slrp'a311=[[(LCEos,Reduce(("visdec",0),1)),(LCEnd,Reduce(("visdec",0),1)),
(LCPrivate,Reduce(("visdec",0),1)),(LCAuxiliary,Reduce(("visdec",0),1)),
(LCLChevron,Reduce(("visdec",0),1)),(LCAnnotation,Reduce(("visdec",0),1)),
(LCSubType,Reduce(("visdec",0),1)),(LCPragma,Reduce(("visdec",0),1)),
(LCFunction,Reduce(("visdec",0),1)),(LCPackage,Reduce(("visdec",0),1)),
(LCType,Reduce(("visdec",0),1)),(LCIdentifier,Reduce(("visdec",0),1)),
(LCFor,Reduce(("visdec",0),1)),(LCUse,Reduce(("visdec",0),1)),
(LCProcedure,Reduce(("visdec",0),1))]]@slrp'a312;
val slrp'a310=[[(LCEos,Reduce(("visible_part",1),2)),
(LCEnd,Reduce(("visible_part",1),2)),(LCPrivate,Reduce(("visible_part",1),2))]]@slrp'a311;
val slrp'a309=[[(LCSemi,Reduce(("package_specification",1),9))]]@slrp'a310;
val slrp'a308=[[(LCIdentifier,Shift 308)]]@slrp'a309;
val slrp'a307=[[(LCEnd,Shift 307)]]@slrp'a308;
val slrp'a306=[[(LCSemi,Reduce(("package_specification",0),7))]]@slrp'a307;
val slrp'a305=[[(LCIdentifier,Shift 305)]]@slrp'a306;
val slrp'a304=[[(LCEos,Reduce(("package_declaration",0),2)),
(LCLChevron,Reduce(("package_declaration",0),2)),
(LCFunction,Reduce(("package_declaration",0),2)),
(LCPackage,Reduce(("package_declaration",0),2)),
(LCIdentifier,Reduce(("package_declaration",0),2)),
(LCAnnotation,Reduce(("package_declaration",0),2)),
(LCSubType,Reduce(("package_declaration",0),2)),
(LCPragma,Reduce(("package_declaration",0),2)),
(LCProcedure,Reduce(("package_declaration",0),2)),
(LCType,Reduce(("package_declaration",0),2)),
(LCFor,Reduce(("package_declaration",0),2)),
(LCUse,Reduce(("package_declaration",0),2)),
(LCReferences,Reduce(("package_declaration",0),2)),
(LCSeparate,Reduce(("package_declaration",0),2)),
(LCWith,Reduce(("package_declaration",0),2)),
(LCBegin,Reduce(("package_declaration",0),2)),
(LCEnd,Reduce(("package_declaration",0),2))]]@slrp'a305;
val slrp'a303=[[(LCSemi,Shift 303)]]@slrp'a304;
val slrp'a302=[[(LCCrd,Reduce(("actual_parameter",0),1)),
(LCComma,Reduce(("actual_parameter",0),1))]]@slrp'a303;
val slrp'a301=[[(LCCrd,Reduce(("positional_parameter_association",1),3))]]@slrp'a302;
val slrp'a300=[[(LCCrd,Reduce(("parameter_association",1),1))]]@slrp'a301;
val slrp'a299=[[(LCCrd,Reduce(("parameter_association",0),1))]]@slrp'a300;
val slrp'a298=[[(LCOrd,Reduce(("actual_parameter_part",0),3)),
(LCDot,Reduce(("actual_parameter_part",0),3)),
(LCPrimedIdentifier,Reduce(("actual_parameter_part",0),3)),
(LCBecomes,Reduce(("actual_parameter_part",0),3)),
(LCSemi,Reduce(("actual_parameter_part",0),3)),
(LCRRange,Reduce(("actual_parameter_part",0),3)),
(LCDigits,Reduce(("actual_parameter_part",0),3)),
(LCDelta,Reduce(("actual_parameter_part",0),3)),
(LCComma,Reduce(("actual_parameter_part",0),3)),
(LCCrd,Reduce(("actual_parameter_part",0),3)),
(LCAnd,Reduce(("actual_parameter_part",0),3)),
(LCOr,Reduce(("actual_parameter_part",0),3)),
(LCXor,Reduce(("actual_parameter_part",0),3)),
(LCGreaterEquals,Reduce(("actual_parameter_part",0),3)),
(LCGreaterThan,Reduce(("actual_parameter_part",0),3)),
(LCLessEquals,Reduce(("actual_parameter_part",0),3)),
(LCLessThan,Reduce(("actual_parameter_part",0),3)),
(LCNotEquals,Reduce(("actual_parameter_part",0),3)),
(LCEquals,Reduce(("actual_parameter_part",0),3)),
(LCIn,Reduce(("actual_parameter_part",0),3)),
(LCNot,Reduce(("actual_parameter_part",0),3)),
(LCDotDot,Reduce(("actual_parameter_part",0),3)),
(LCAmpersand,Reduce(("actual_parameter_part",0),3)),
(LCMinus,Reduce(("actual_parameter_part",0),3)),
(LCPlus,Reduce(("actual_parameter_part",0),3)),
(LCRem,Reduce(("actual_parameter_part",0),3)),
(LCMod,Reduce(("actual_parameter_part",0),3)),
(LCDiv,Reduce(("actual_parameter_part",0),3)),
(LCStar,Reduce(("actual_parameter_part",0),3)),
(LCStarStar,Reduce(("actual_parameter_part",0),3)),
(LCPrime,Reduce(("actual_parameter_part",0),3)),
(LCGrkXi,Reduce(("actual_parameter_part",0),3)),
(LCRenames,Reduce(("actual_parameter_part",0),3)),
(LCAt,Reduce(("actual_parameter_part",0),3)),
(LCThen,Reduce(("actual_parameter_part",0),3)),
(LCIs,Reduce(("actual_parameter_part",0),3)),
(LCGoesTo,Reduce(("actual_parameter_part",0),3)),
(LCBar,Reduce(("actual_parameter_part",0),3)),
(LCTill,Reduce(("actual_parameter_part",0),3)),
(LCLoop,Reduce(("actual_parameter_part",0),3)),
(LCAnnotation,Reduce(("actual_parameter_part",0),3)),
(LCLChevron,Reduce(("actual_parameter_part",0),3))]]@slrp'a299;
val slrp'a297=[[(LCCrd,Shift 297)]]@slrp'a298;
val slrp'a296=[[(LCEos,Reduce(("procedure_call_statement",0),2)),
(LCAnnotation,Reduce(("procedure_call_statement",0),2)),
(LCCon,Reduce(("procedure_call_statement",0),2)),
(LCLChevron,Reduce(("procedure_call_statement",0),2)),
(LCGrkGamma,Reduce(("procedure_call_statement",0),2)),
(LCGrkDelta,Reduce(("procedure_call_statement",0),2)),
(LCPragma,Reduce(("procedure_call_statement",0),2)),
(LCNull,Reduce(("procedure_call_statement",0),2)),
(LCExit,Reduce(("procedure_call_statement",0),2)),
(LCReturn,Reduce(("procedure_call_statement",0),2)),
(LCIf,Reduce(("procedure_call_statement",0),2)),
(LCCase,Reduce(("procedure_call_statement",0),2)),
(LCIdentifier,Reduce(("procedure_call_statement",0),2)),
(LCCharacterLiteral,Reduce(("procedure_call_statement",0),2)),
(LCTill,Reduce(("procedure_call_statement",0),2)),
(LCLoop,Reduce(("procedure_call_statement",0),2)),
(LCFor,Reduce(("procedure_call_statement",0),2)),
(LCWhile,Reduce(("procedure_call_statement",0),2)),
(LCElsIf,Reduce(("procedure_call_statement",0),2)),
(LCElse,Reduce(("procedure_call_statement",0),2)),
(LCEnd,Reduce(("procedure_call_statement",0),2)),
(LCWhen,Reduce(("procedure_call_statement",0),2))]]@slrp'a297;
val slrp'a295=[[(LCEos,Reduce(("subprogram_implementation",0),6)),
(LCLChevron,Reduce(("subprogram_implementation",0),6)),
(LCFunction,Reduce(("subprogram_implementation",0),6)),
(LCPackage,Reduce(("subprogram_implementation",0),6)),
(LCIdentifier,Reduce(("subprogram_implementation",0),6)),
(LCAnnotation,Reduce(("subprogram_implementation",0),6)),
(LCSubType,Reduce(("subprogram_implementation",0),6)),
(LCPragma,Reduce(("subprogram_implementation",0),6)),
(LCProcedure,Reduce(("subprogram_implementation",0),6)),
(LCType,Reduce(("subprogram_implementation",0),6)),
(LCFor,Reduce(("subprogram_implementation",0),6)),
(LCUse,Reduce(("subprogram_implementation",0),6)),
(LCReferences,Reduce(("subprogram_implementation",0),6)),
(LCSeparate,Reduce(("subprogram_implementation",0),6)),
(LCWith,Reduce(("subprogram_implementation",0),6)),
(LCBegin,Reduce(("subprogram_implementation",0),6)),
(LCEnd,Reduce(("subprogram_implementation",0),6))]]@slrp'a296;
val slrp'a294=[[(LCSemi,Shift 294)]]@slrp'a295;
val slrp'a293=[[(LCIdentifier,Shift 293)]]@slrp'a294;
val slrp'a292=[[(LCEnd,Shift 292)]]@slrp'a293;
val slrp'a291=[[(LCBegin,Shift 683)]]@slrp'a292;
val slrp'a290=[[(LCEos,Reduce(("subprogram_body",1),3)),
(LCLChevron,Reduce(("subprogram_body",1),3)),
(LCFunction,Reduce(("subprogram_body",1),3)),
(LCPackage,Reduce(("subprogram_body",1),3)),
(LCIdentifier,Reduce(("subprogram_body",1),3)),
(LCAnnotation,Reduce(("subprogram_body",1),3)),
(LCSubType,Reduce(("subprogram_body",1),3)),
(LCPragma,Reduce(("subprogram_body",1),3)),
(LCProcedure,Reduce(("subprogram_body",1),3)),
(LCType,Reduce(("subprogram_body",1),3)),(LCFor,Reduce(("subprogram_body",1),3))
,(LCUse,Reduce(("subprogram_body",1),3)),
(LCReferences,Reduce(("subprogram_body",1),3)),
(LCSeparate,Reduce(("subprogram_body",1),3)),
(LCWith,Reduce(("subprogram_body",1),3)),
(LCBegin,Reduce(("subprogram_body",1),3)),
(LCEnd,Reduce(("subprogram_body",1),3))]]@slrp'a291;
val slrp'a289=[[(LCIs,Shift 707)]]@slrp'a290;
val slrp'a288=[[(LCEos,Reduce(("subprogram_body",0),3)),
(LCLChevron,Reduce(("subprogram_body",0),3)),
(LCFunction,Reduce(("subprogram_body",0),3)),
(LCPackage,Reduce(("subprogram_body",0),3)),
(LCIdentifier,Reduce(("subprogram_body",0),3)),
(LCAnnotation,Reduce(("subprogram_body",0),3)),
(LCSubType,Reduce(("subprogram_body",0),3)),
(LCPragma,Reduce(("subprogram_body",0),3)),
(LCProcedure,Reduce(("subprogram_body",0),3)),
(LCType,Reduce(("subprogram_body",0),3)),(LCFor,Reduce(("subprogram_body",0),3))
,(LCUse,Reduce(("subprogram_body",0),3)),
(LCReferences,Reduce(("subprogram_body",0),3)),
(LCSeparate,Reduce(("subprogram_body",0),3)),
(LCWith,Reduce(("subprogram_body",0),3)),
(LCBegin,Reduce(("subprogram_body",0),3)),
(LCEnd,Reduce(("subprogram_body",0),3))]]@slrp'a289;
val slrp'a287=[[(LCIs,Shift 706)]]@slrp'a288;
val slrp'a286=[[(LCIdentifier,Reduce(("mode",3),2)),
(LCCharacterLiteral,Reduce(("mode",3),2))]]@slrp'a287;
val slrp'a285=[[(LCIdentifier,Reduce(("mode",2),1)),
(LCCharacterLiteral,Reduce(("mode",2),1))]]@slrp'a286;
val slrp'a284=[[(LCCrd,Reduce(("parameter_specification",1),6)),
(LCSemi,Reduce(("parameter_specification",1),6))]]@slrp'a285;
val slrp'a283=[[(LCCrd,Reduce(("parlist",1),3))]]@slrp'a284;
val slrp'a282=[[(LCIs,Reduce(("formal_part",0),3)),
(LCGrkDelta,Reduce(("formal_part",0),3)),
(LCGrkGamma,Reduce(("formal_part",0),3)),(LCReturn,Reduce(("formal_part",0),3)),
(LCSemi,Reduce(("formal_part",0),3)),(LCAnnotation,Reduce(("formal_part",0),3)),
(LCLChevron,Reduce(("formal_part",0),3)),(LCRenames,Reduce(("formal_part",0),3))
]]@slrp'a283;
val slrp'a281=[[(LCCrd,Shift 281)]]@slrp'a282;
val slrp'a280=[[(LCOrd,Reduce(("operator_symbol",0),1)),
(LCSemi,Reduce(("operator_symbol",0),1))]]@slrp'a281;
val slrp'a279=[[(LCEos,Reduce(("annotation_list",2),2)),
(LCSemi,Reduce(("annotation_list",2),2)),(LCIs,Reduce(("annotation_list",2),2)),
(LCRenames,Reduce(("annotation_list",2),2)),
(LCPackage,Reduce(("annotation_list",2),2)),
(LCProcedure,Reduce(("annotation_list",2),2)),
(LCFunction,Reduce(("annotation_list",2),2)),
(LCSeparate,Reduce(("annotation_list",2),2))]]@slrp'a280;
val slrp'a278=[[(LCEos,Reduce(("annotation_list",1),2)),
(LCSemi,Reduce(("annotation_list",1),2)),(LCIs,Reduce(("annotation_list",1),2)),
(LCRenames,Reduce(("annotation_list",1),2)),
(LCPackage,Reduce(("annotation_list",1),2)),
(LCProcedure,Reduce(("annotation_list",1),2)),
(LCFunction,Reduce(("annotation_list",1),2)),
(LCSeparate,Reduce(("annotation_list",1),2))]]@slrp'a279;
val slrp'a277=[[(LCSemi,Reduce(("function_specification",0),2)),
(LCIs,Reduce(("function_specification",0),2)),
(LCRenames,Reduce(("function_specification",0),2))]]@slrp'a278;
val slrp'a276=[[(LCSemi,Reduce(("function_specification_1",3),6)),
(LCAnnotation,Reduce(("function_specification_1",3),6)),
(LCLChevron,Reduce(("function_specification_1",3),6)),
(LCIs,Reduce(("function_specification_1",3),6)),
(LCRenames,Reduce(("function_specification_1",3),6))]]@slrp'a277;
val slrp'a275=[[(LCSemi,Reduce(("function_specification_1",1),5)),
(LCAnnotation,Reduce(("function_specification_1",1),5)),
(LCLChevron,Reduce(("function_specification_1",1),5)),
(LCIs,Reduce(("function_specification_1",1),5)),
(LCRenames,Reduce(("function_specification_1",1),5))]]@slrp'a276;
val slrp'a274=[[(LCSemi,Reduce(("function_specification_statement",3),4)),
(LCAnnotation,Reduce(("function_specification_statement",3),4)),
(LCLChevron,Reduce(("function_specification_statement",3),4)),
(LCIs,Reduce(("function_specification_statement",3),4)),
(LCRenames,Reduce(("function_specification_statement",3),4))]]@slrp'a275;
val slrp'a273=[[(LCRBrace,Shift 273)]]@slrp'a274;
val slrp'a272=[[(LCZ,Shift 272)]]@slrp'a273;
val slrp'a271=[[(LCSemi,Reduce(("function_specification_statement",2),4)),
(LCAnnotation,Reduce(("function_specification_statement",2),4)),
(LCLChevron,Reduce(("function_specification_statement",2),4)),
(LCIs,Reduce(("function_specification_statement",2),4)),
(LCRenames,Reduce(("function_specification_statement",2),4))]]@slrp'a272;
val slrp'a270=[[(LCRSqBrack,Shift 270)]]@slrp'a271;
val slrp'a269=[[(LCZ,Shift 269)]]@slrp'a270;
val slrp'a268=[[(LCSemi,Reduce(("function_specification_statement",1),5)),
(LCAnnotation,Reduce(("function_specification_statement",1),5)),
(LCLChevron,Reduce(("function_specification_statement",1),5)),
(LCIs,Reduce(("function_specification_statement",1),5)),
(LCRenames,Reduce(("function_specification_statement",1),5))]]@slrp'a269;
val slrp'a267=[[(LCRBrace,Shift 267)]]@slrp'a268;
val slrp'a266=[[(LCZ,Shift 266)]]@slrp'a267;
val slrp'a265=[[(LCSemi,Reduce(("function_specification_statement",0),5)),
(LCAnnotation,Reduce(("function_specification_statement",0),5)),
(LCLChevron,Reduce(("function_specification_statement",0),5)),
(LCIs,Reduce(("function_specification_statement",0),5)),
(LCRenames,Reduce(("function_specification_statement",0),5))]]@slrp'a266;
val slrp'a264=[[(LCRSqBrack,Shift 264)]]@slrp'a265;
val slrp'a263=[[(LCZ,Shift 263)]]@slrp'a264;
val slrp'a262=[[(LCSemi,Reduce(("procedure_specification_statement",2),1)),
(LCAnnotation,Reduce(("procedure_specification_statement",2),1)),
(LCLChevron,Reduce(("procedure_specification_statement",2),1)),
(LCIs,Reduce(("procedure_specification_statement",2),1)),
(LCRenames,Reduce(("procedure_specification_statement",2),1))]]@slrp'a263;
val slrp'a261=[[(LCSemi,Reduce(("procedure_specification_statement",1),2)),
(LCAnnotation,Reduce(("procedure_specification_statement",1),2)),
(LCLChevron,Reduce(("procedure_specification_statement",1),2)),
(LCIs,Reduce(("procedure_specification_statement",1),2)),
(LCRenames,Reduce(("procedure_specification_statement",1),2))]]@slrp'a262;
val slrp'a260=[[(LCSemi,Reduce(("procedure_specification_statement",0),3)),
(LCAnnotation,Reduce(("procedure_specification_statement",0),3)),
(LCLChevron,Reduce(("procedure_specification_statement",0),3)),
(LCIs,Reduce(("procedure_specification_statement",0),3)),
(LCRenames,Reduce(("procedure_specification_statement",0),3))]]@slrp'a261;
val slrp'a259=[[(LCSemi,Reduce(("procedure_specification",0),2)),
(LCIs,Reduce(("procedure_specification",0),2)),
(LCRenames,Reduce(("procedure_specification",0),2))]]@slrp'a260;
val slrp'a258=[[(LCSemi,Reduce(("procedure_specification_1",3),4)),
(LCAnnotation,Reduce(("procedure_specification_1",3),4)),
(LCLChevron,Reduce(("procedure_specification_1",3),4)),
(LCIs,Reduce(("procedure_specification_1",3),4)),
(LCRenames,Reduce(("procedure_specification_1",3),4))]]@slrp'a259;
val slrp'a257=[[(LCSemi,Reduce(("procedure_specification_1",1),3)),
(LCAnnotation,Reduce(("procedure_specification_1",1),3)),
(LCLChevron,Reduce(("procedure_specification_1",1),3)),
(LCIs,Reduce(("procedure_specification_1",1),3)),
(LCRenames,Reduce(("procedure_specification_1",1),3))]]@slrp'a258;
val slrp'a256=[[(LCRenames,Reduce(("subprogram_specification",1),1))]]@slrp'a257;
val slrp'a255=[[(LCRenames,Reduce(("subprogram_specification",0),1))]]@slrp'a256;
val slrp'a254=[[(LCEos,Reduce(("subprogram_declaration",1),2)),
(LCEnd,Reduce(("subprogram_declaration",1),2)),
(LCPrivate,Reduce(("subprogram_declaration",1),2)),
(LCAuxiliary,Reduce(("subprogram_declaration",1),2)),
(LCLChevron,Reduce(("subprogram_declaration",1),2)),
(LCAnnotation,Reduce(("subprogram_declaration",1),2)),
(LCSubType,Reduce(("subprogram_declaration",1),2)),
(LCPragma,Reduce(("subprogram_declaration",1),2)),
(LCFunction,Reduce(("subprogram_declaration",1),2)),
(LCPackage,Reduce(("subprogram_declaration",1),2)),
(LCType,Reduce(("subprogram_declaration",1),2)),
(LCIdentifier,Reduce(("subprogram_declaration",1),2)),
(LCFor,Reduce(("subprogram_declaration",1),2)),
(LCUse,Reduce(("subprogram_declaration",1),2)),
(LCProcedure,Reduce(("subprogram_declaration",1),2))]]@slrp'a255;
val slrp'a253=[[(LCEos,Reduce(("subprogram_declaration",0),2)),
(LCEnd,Reduce(("subprogram_declaration",0),2)),
(LCPrivate,Reduce(("subprogram_declaration",0),2)),
(LCAuxiliary,Reduce(("subprogram_declaration",0),2)),
(LCLChevron,Reduce(("subprogram_declaration",0),2)),
(LCAnnotation,Reduce(("subprogram_declaration",0),2)),
(LCSubType,Reduce(("subprogram_declaration",0),2)),
(LCPragma,Reduce(("subprogram_declaration",0),2)),
(LCFunction,Reduce(("subprogram_declaration",0),2)),
(LCPackage,Reduce(("subprogram_declaration",0),2)),
(LCType,Reduce(("subprogram_declaration",0),2)),
(LCIdentifier,Reduce(("subprogram_declaration",0),2)),
(LCFor,Reduce(("subprogram_declaration",0),2)),
(LCUse,Reduce(("subprogram_declaration",0),2)),
(LCProcedure,Reduce(("subprogram_declaration",0),2))]]@slrp'a254;
val slrp'a252=[[(LCEos,Reduce(("return_statement",1),3)),
(LCAnnotation,Reduce(("return_statement",1),3)),
(LCCon,Reduce(("return_statement",1),3)),
(LCLChevron,Reduce(("return_statement",1),3)),
(LCGrkGamma,Reduce(("return_statement",1),3)),
(LCGrkDelta,Reduce(("return_statement",1),3)),
(LCPragma,Reduce(("return_statement",1),3)),
(LCNull,Reduce(("return_statement",1),3)),
(LCExit,Reduce(("return_statement",1),3)),
(LCReturn,Reduce(("return_statement",1),3)),
(LCIf,Reduce(("return_statement",1),3)),
(LCCase,Reduce(("return_statement",1),3)),
(LCIdentifier,Reduce(("return_statement",1),3)),
(LCCharacterLiteral,Reduce(("return_statement",1),3)),
(LCTill,Reduce(("return_statement",1),3)),
(LCLoop,Reduce(("return_statement",1),3)),
(LCFor,Reduce(("return_statement",1),3)),
(LCWhile,Reduce(("return_statement",1),3)),
(LCElsIf,Reduce(("return_statement",1),3)),
(LCElse,Reduce(("return_statement",1),3)),
(LCEnd,Reduce(("return_statement",1),3)),
(LCWhen,Reduce(("return_statement",1),3))]]@slrp'a253;
val slrp'a251=[[(LCSemi,Shift 251)]]@slrp'a252;
val slrp'a250=[[(LCEos,Reduce(("return_statement",0),2)),
(LCAnnotation,Reduce(("return_statement",0),2)),
(LCCon,Reduce(("return_statement",0),2)),
(LCLChevron,Reduce(("return_statement",0),2)),
(LCGrkGamma,Reduce(("return_statement",0),2)),
(LCGrkDelta,Reduce(("return_statement",0),2)),
(LCPragma,Reduce(("return_statement",0),2)),
(LCNull,Reduce(("return_statement",0),2)),
(LCExit,Reduce(("return_statement",0),2)),
(LCReturn,Reduce(("return_statement",0),2)),
(LCIf,Reduce(("return_statement",0),2)),
(LCCase,Reduce(("return_statement",0),2)),
(LCIdentifier,Reduce(("return_statement",0),2)),
(LCCharacterLiteral,Reduce(("return_statement",0),2)),
(LCTill,Reduce(("return_statement",0),2)),
(LCLoop,Reduce(("return_statement",0),2)),
(LCFor,Reduce(("return_statement",0),2)),
(LCWhile,Reduce(("return_statement",0),2)),
(LCElsIf,Reduce(("return_statement",0),2)),
(LCElse,Reduce(("return_statement",0),2)),
(LCEnd,Reduce(("return_statement",0),2)),
(LCWhen,Reduce(("return_statement",0),2))]]@slrp'a251;
val slrp'a249=[[(LCEos,Reduce(("exit_statement",1),5)),
(LCAnnotation,Reduce(("exit_statement",1),5)),
(LCCon,Reduce(("exit_statement",1),5)),
(LCLChevron,Reduce(("exit_statement",1),5)),
(LCGrkGamma,Reduce(("exit_statement",1),5)),
(LCGrkDelta,Reduce(("exit_statement",1),5)),
(LCPragma,Reduce(("exit_statement",1),5)),
(LCNull,Reduce(("exit_statement",1),5)),(LCExit,Reduce(("exit_statement",1),5)),
(LCReturn,Reduce(("exit_statement",1),5)),(LCIf,Reduce(("exit_statement",1),5)),
(LCCase,Reduce(("exit_statement",1),5)),
(LCIdentifier,Reduce(("exit_statement",1),5)),
(LCCharacterLiteral,Reduce(("exit_statement",1),5)),
(LCTill,Reduce(("exit_statement",1),5)),(LCLoop,Reduce(("exit_statement",1),5)),
(LCFor,Reduce(("exit_statement",1),5)),(LCWhile,Reduce(("exit_statement",1),5)),
(LCElsIf,Reduce(("exit_statement",1),5)),(LCElse,Reduce(("exit_statement",1),5))
,(LCEnd,Reduce(("exit_statement",1),5)),(LCWhen,Reduce(("exit_statement",1),5))]]@slrp'a250;
val slrp'a248=[[(LCSemi,Shift 248)]]@slrp'a249;
val slrp'a247=[[(LCEos,Reduce(("exit_statement",0),3)),
(LCAnnotation,Reduce(("exit_statement",0),3)),
(LCCon,Reduce(("exit_statement",0),3)),
(LCLChevron,Reduce(("exit_statement",0),3)),
(LCGrkGamma,Reduce(("exit_statement",0),3)),
(LCGrkDelta,Reduce(("exit_statement",0),3)),
(LCPragma,Reduce(("exit_statement",0),3)),
(LCNull,Reduce(("exit_statement",0),3)),(LCExit,Reduce(("exit_statement",0),3)),
(LCReturn,Reduce(("exit_statement",0),3)),(LCIf,Reduce(("exit_statement",0),3)),
(LCCase,Reduce(("exit_statement",0),3)),
(LCIdentifier,Reduce(("exit_statement",0),3)),
(LCCharacterLiteral,Reduce(("exit_statement",0),3)),
(LCTill,Reduce(("exit_statement",0),3)),(LCLoop,Reduce(("exit_statement",0),3)),
(LCFor,Reduce(("exit_statement",0),3)),(LCWhile,Reduce(("exit_statement",0),3)),
(LCElsIf,Reduce(("exit_statement",0),3)),(LCElse,Reduce(("exit_statement",0),3))
,(LCEnd,Reduce(("exit_statement",0),3)),(LCWhen,Reduce(("exit_statement",0),3))]]@slrp'a248;
val slrp'a246=[[(LCEos,Reduce(("block_statement",1),6))]]@slrp'a247;
val slrp'a245=[[(LCSemi,Shift 245)]]@slrp'a246;
val slrp'a244=[[(LCEnd,Shift 503)]]@slrp'a245;
val slrp'a243=[[(LCEos,Reduce(("block_statement",0),8))]]@slrp'a244;
val slrp'a242=[[(LCSemi,Shift 242)]]@slrp'a243;
val slrp'a241=[[(LCEnd,Shift 502)]]@slrp'a242;
val slrp'a240=[[(LCBegin,Shift 681)]]@slrp'a241;
val slrp'a239=[[(LCBecomes,Reduce(("auxiliary_expression",0),3)),
(LCSemi,Reduce(("auxiliary_expression",0),3)),
(LCRRange,Reduce(("auxiliary_expression",0),3)),
(LCDigits,Reduce(("auxiliary_expression",0),3)),
(LCDelta,Reduce(("auxiliary_expression",0),3)),
(LCComma,Reduce(("auxiliary_expression",0),3)),
(LCCrd,Reduce(("auxiliary_expression",0),3)),
(LCAnd,Reduce(("auxiliary_expression",0),3)),
(LCOr,Reduce(("auxiliary_expression",0),3)),
(LCXor,Reduce(("auxiliary_expression",0),3)),
(LCGreaterEquals,Reduce(("auxiliary_expression",0),3)),
(LCGreaterThan,Reduce(("auxiliary_expression",0),3)),
(LCLessEquals,Reduce(("auxiliary_expression",0),3)),
(LCLessThan,Reduce(("auxiliary_expression",0),3)),
(LCNotEquals,Reduce(("auxiliary_expression",0),3)),
(LCEquals,Reduce(("auxiliary_expression",0),3)),
(LCIn,Reduce(("auxiliary_expression",0),3)),
(LCNot,Reduce(("auxiliary_expression",0),3)),
(LCDotDot,Reduce(("auxiliary_expression",0),3)),
(LCAmpersand,Reduce(("auxiliary_expression",0),3)),
(LCMinus,Reduce(("auxiliary_expression",0),3)),
(LCPlus,Reduce(("auxiliary_expression",0),3)),
(LCRem,Reduce(("auxiliary_expression",0),3)),
(LCMod,Reduce(("auxiliary_expression",0),3)),
(LCDiv,Reduce(("auxiliary_expression",0),3)),
(LCStar,Reduce(("auxiliary_expression",0),3)),
(LCStarStar,Reduce(("auxiliary_expression",0),3)),
(LCLoop,Reduce(("auxiliary_expression",0),3)),
(LCThen,Reduce(("auxiliary_expression",0),3)),
(LCIs,Reduce(("auxiliary_expression",0),3)),
(LCGoesTo,Reduce(("auxiliary_expression",0),3)),
(LCBar,Reduce(("auxiliary_expression",0),3)),
(LCTill,Reduce(("auxiliary_expression",0),3))]]@slrp'a240;
val slrp'a238=[[(LCRStrachey,Shift 238)]]@slrp'a239;
val slrp'a237=[[(LCZ,Shift 237)]]@slrp'a238;
val slrp'a236=[[(LCLoop,Reduce(("till_opt",1),2))]]@slrp'a237;
val slrp'a235=[[(LCTill,Reduce(("loop_parameter_specification",1),4)),
(LCLoop,Reduce(("loop_parameter_specification",1),4))]]@slrp'a236;
val slrp'a234=[[(LCTill,Reduce(("loop_parameter_specification",0),3)),
(LCLoop,Reduce(("loop_parameter_specification",0),3))]]@slrp'a235;
val slrp'a233=[[(LCTill,Reduce(("iteration_scheme",1),2)),
(LCLoop,Reduce(("iteration_scheme",1),2))]]@slrp'a234;
val slrp'a232=[[(LCTill,Reduce(("iteration_scheme",0),2)),
(LCLoop,Reduce(("iteration_scheme",0),2))]]@slrp'a233;
val slrp'a231=[[(LCTill,Reduce(("iteration_scheme_opt",1),1)),
(LCLoop,Reduce(("iteration_scheme_opt",1),1))]]@slrp'a232;
val slrp'a230=[[(LCSemi,Reduce(("name_opt",1),1)),
(LCWhen,Reduce(("name_opt",1),1))]]@slrp'a231;
val slrp'a229=[[(LCFor,Reduce(("name_colon_opt",1),2)),
(LCWhile,Reduce(("name_colon_opt",1),2)),(LCTill,Reduce(("name_colon_opt",1),2))
,(LCLoop,Reduce(("name_colon_opt",1),2)),
(LCDeclare,Reduce(("name_colon_opt",1),2)),
(LCBegin,Reduce(("name_colon_opt",1),2))]]@slrp'a230;
val slrp'a228=[[(LCIdentifier,Reduce(("loop_body",0),4)),
(LCSemi,Reduce(("loop_body",0),4))]]@slrp'a229;
val slrp'a227=[[(LCLoop,Shift 227)]]@slrp'a228;
val slrp'a226=[[(LCEnd,Shift 226)]]@slrp'a227;
val slrp'a225=[[(LCEos,Reduce(("loop_statement",0),6)),
(LCAnnotation,Reduce(("loop_statement",0),6)),
(LCCon,Reduce(("loop_statement",0),6)),
(LCLChevron,Reduce(("loop_statement",0),6)),
(LCGrkGamma,Reduce(("loop_statement",0),6)),
(LCGrkDelta,Reduce(("loop_statement",0),6)),
(LCPragma,Reduce(("loop_statement",0),6)),
(LCNull,Reduce(("loop_statement",0),6)),(LCExit,Reduce(("loop_statement",0),6)),
(LCReturn,Reduce(("loop_statement",0),6)),(LCIf,Reduce(("loop_statement",0),6)),
(LCCase,Reduce(("loop_statement",0),6)),
(LCIdentifier,Reduce(("loop_statement",0),6)),
(LCCharacterLiteral,Reduce(("loop_statement",0),6)),
(LCTill,Reduce(("loop_statement",0),6)),(LCLoop,Reduce(("loop_statement",0),6)),
(LCFor,Reduce(("loop_statement",0),6)),(LCWhile,Reduce(("loop_statement",0),6)),
(LCElsIf,Reduce(("loop_statement",0),6)),(LCElse,Reduce(("loop_statement",0),6))
,(LCEnd,Reduce(("loop_statement",0),6)),(LCWhen,Reduce(("loop_statement",0),6))]]@slrp'a226;
val slrp'a224=[[(LCSemi,Shift 224)]]@slrp'a225;
val slrp'a223=[[(LCEnd,Reduce(("others_part",1),4))]]@slrp'a224;
val slrp'a222=[[(LCGoesTo,Shift 679)]]@slrp'a223;
val slrp'a221=[[(LCGoesTo,Reduce(("case_choice",0),1)),
(LCBar,Reduce(("case_choice",0),1))]]@slrp'a222;
val slrp'a220=[[(LCGoesTo,Reduce(("case_choice_list",1),3))]]@slrp'a221;
val slrp'a219=[[(LCWhen,Reduce(("case_statement_alternative",0),4)),
(LCEnd,Reduce(("case_statement_alternative",0),4))]]@slrp'a220;
val slrp'a218=[[(LCGoesTo,Shift 678)]]@slrp'a219;
val slrp'a217=[[(LCWhen,Reduce(("case_statement_alternative_list",1),2)),
(LCEnd,Reduce(("case_statement_alternative_list",1),2))]]@slrp'a218;
val slrp'a216=[[(LCWhen,Reduce(("case_statement_alternative_list",0),1)),
(LCEnd,Reduce(("case_statement_alternative_list",0),1))]]@slrp'a217;
val slrp'a215=[[(LCEos,Reduce(("case_statement",0),8)),
(LCAnnotation,Reduce(("case_statement",0),8)),
(LCCon,Reduce(("case_statement",0),8)),
(LCLChevron,Reduce(("case_statement",0),8)),
(LCGrkGamma,Reduce(("case_statement",0),8)),
(LCGrkDelta,Reduce(("case_statement",0),8)),
(LCPragma,Reduce(("case_statement",0),8)),
(LCNull,Reduce(("case_statement",0),8)),(LCExit,Reduce(("case_statement",0),8)),
(LCReturn,Reduce(("case_statement",0),8)),(LCIf,Reduce(("case_statement",0),8)),
(LCCase,Reduce(("case_statement",0),8)),
(LCIdentifier,Reduce(("case_statement",0),8)),
(LCCharacterLiteral,Reduce(("case_statement",0),8)),
(LCTill,Reduce(("case_statement",0),8)),(LCLoop,Reduce(("case_statement",0),8)),
(LCFor,Reduce(("case_statement",0),8)),(LCWhile,Reduce(("case_statement",0),8)),
(LCElsIf,Reduce(("case_statement",0),8)),(LCElse,Reduce(("case_statement",0),8))
,(LCEnd,Reduce(("case_statement",0),8)),(LCWhen,Reduce(("case_statement",0),8))]]@slrp'a216;
val slrp'a214=[[(LCSemi,Shift 214)]]@slrp'a215;
val slrp'a213=[[(LCCase,Shift 213)]]@slrp'a214;
val slrp'a212=[[(LCEnd,Shift 212)]]@slrp'a213;
val slrp'a211=[[(LCIs,Shift 526)]]@slrp'a212;
val slrp'a210=[[(LCThen,Reduce(("condition",0),1)),
(LCTill,Reduce(("condition",0),1)),(LCLoop,Reduce(("condition",0),1)),
(LCSemi,Reduce(("condition",0),1))]]@slrp'a211;
val slrp'a209=[[(LCEnd,Reduce(("else_part",1),2))]]@slrp'a210;
val slrp'a208=[[(LCElse,Reduce(("elsif_part",1),5)),
(LCEnd,Reduce(("elsif_part",1),5))]]@slrp'a209;
val slrp'a207=[[(LCThen,Shift 676)]]@slrp'a208;
val slrp'a206=[[(LCEos,Reduce(("if_statement",0),9)),
(LCAnnotation,Reduce(("if_statement",0),9)),(LCCon,Reduce(("if_statement",0),9))
,(LCLChevron,Reduce(("if_statement",0),9)),
(LCGrkGamma,Reduce(("if_statement",0),9)),
(LCGrkDelta,Reduce(("if_statement",0),9)),
(LCPragma,Reduce(("if_statement",0),9)),(LCNull,Reduce(("if_statement",0),9)),
(LCExit,Reduce(("if_statement",0),9)),(LCReturn,Reduce(("if_statement",0),9)),
(LCIf,Reduce(("if_statement",0),9)),(LCCase,Reduce(("if_statement",0),9)),
(LCIdentifier,Reduce(("if_statement",0),9)),
(LCCharacterLiteral,Reduce(("if_statement",0),9)),
(LCTill,Reduce(("if_statement",0),9)),(LCLoop,Reduce(("if_statement",0),9)),
(LCFor,Reduce(("if_statement",0),9)),(LCWhile,Reduce(("if_statement",0),9)),
(LCElsIf,Reduce(("if_statement",0),9)),(LCElse,Reduce(("if_statement",0),9)),
(LCEnd,Reduce(("if_statement",0),9)),(LCWhen,Reduce(("if_statement",0),9))]]@slrp'a207;
val slrp'a205=[[(LCSemi,Shift 205)]]@slrp'a206;
val slrp'a204=[[(LCIf,Shift 204)]]@slrp'a205;
val slrp'a203=[[(LCEnd,Shift 203)]]@slrp'a204;
val slrp'a202=[[(LCThen,Shift 675)]]@slrp'a203;
val slrp'a201=[[(LCEos,Reduce(("assignment_statement",0),4)),
(LCAnnotation,Reduce(("assignment_statement",0),4)),
(LCCon,Reduce(("assignment_statement",0),4)),
(LCLChevron,Reduce(("assignment_statement",0),4)),
(LCGrkGamma,Reduce(("assignment_statement",0),4)),
(LCGrkDelta,Reduce(("assignment_statement",0),4)),
(LCPragma,Reduce(("assignment_statement",0),4)),
(LCNull,Reduce(("assignment_statement",0),4)),
(LCExit,Reduce(("assignment_statement",0),4)),
(LCReturn,Reduce(("assignment_statement",0),4)),
(LCIf,Reduce(("assignment_statement",0),4)),
(LCCase,Reduce(("assignment_statement",0),4)),
(LCIdentifier,Reduce(("assignment_statement",0),4)),
(LCCharacterLiteral,Reduce(("assignment_statement",0),4)),
(LCTill,Reduce(("assignment_statement",0),4)),
(LCLoop,Reduce(("assignment_statement",0),4)),
(LCFor,Reduce(("assignment_statement",0),4)),
(LCWhile,Reduce(("assignment_statement",0),4)),
(LCElsIf,Reduce(("assignment_statement",0),4)),
(LCElse,Reduce(("assignment_statement",0),4)),
(LCEnd,Reduce(("assignment_statement",0),4)),
(LCWhen,Reduce(("assignment_statement",0),4))]]@slrp'a202;
val slrp'a200=[[(LCSemi,Shift 200)]]@slrp'a201;
val slrp'a199=[[(LCEos,Reduce(("null_statement",0),2)),
(LCAnnotation,Reduce(("null_statement",0),2)),
(LCCon,Reduce(("null_statement",0),2)),
(LCLChevron,Reduce(("null_statement",0),2)),
(LCGrkGamma,Reduce(("null_statement",0),2)),
(LCGrkDelta,Reduce(("null_statement",0),2)),
(LCPragma,Reduce(("null_statement",0),2)),
(LCNull,Reduce(("null_statement",0),2)),(LCExit,Reduce(("null_statement",0),2)),
(LCReturn,Reduce(("null_statement",0),2)),(LCIf,Reduce(("null_statement",0),2)),
(LCCase,Reduce(("null_statement",0),2)),
(LCIdentifier,Reduce(("null_statement",0),2)),
(LCCharacterLiteral,Reduce(("null_statement",0),2)),
(LCTill,Reduce(("null_statement",0),2)),(LCLoop,Reduce(("null_statement",0),2)),
(LCFor,Reduce(("null_statement",0),2)),(LCWhile,Reduce(("null_statement",0),2)),
(LCElsIf,Reduce(("null_statement",0),2)),(LCElse,Reduce(("null_statement",0),2))
,(LCEnd,Reduce(("null_statement",0),2)),(LCWhen,Reduce(("null_statement",0),2))]]@slrp'a200;
val slrp'a198=[[(LCSemi,Shift 198)]]@slrp'a199;
val slrp'a197=[[(LCEos,Reduce(("compound_statement",2),1)),
(LCAnnotation,Reduce(("compound_statement",2),1)),
(LCCon,Reduce(("compound_statement",2),1)),
(LCLChevron,Reduce(("compound_statement",2),1)),
(LCGrkGamma,Reduce(("compound_statement",2),1)),
(LCGrkDelta,Reduce(("compound_statement",2),1)),
(LCPragma,Reduce(("compound_statement",2),1)),
(LCNull,Reduce(("compound_statement",2),1)),
(LCExit,Reduce(("compound_statement",2),1)),
(LCReturn,Reduce(("compound_statement",2),1)),
(LCIf,Reduce(("compound_statement",2),1)),
(LCCase,Reduce(("compound_statement",2),1)),
(LCIdentifier,Reduce(("compound_statement",2),1)),
(LCCharacterLiteral,Reduce(("compound_statement",2),1)),
(LCTill,Reduce(("compound_statement",2),1)),
(LCLoop,Reduce(("compound_statement",2),1)),
(LCFor,Reduce(("compound_statement",2),1)),
(LCWhile,Reduce(("compound_statement",2),1)),
(LCElsIf,Reduce(("compound_statement",2),1)),
(LCElse,Reduce(("compound_statement",2),1)),
(LCEnd,Reduce(("compound_statement",2),1)),
(LCWhen,Reduce(("compound_statement",2),1))]]@slrp'a198;
val slrp'a196=[[(LCEos,Reduce(("compound_statement",1),1)),
(LCAnnotation,Reduce(("compound_statement",1),1)),
(LCCon,Reduce(("compound_statement",1),1)),
(LCLChevron,Reduce(("compound_statement",1),1)),
(LCGrkGamma,Reduce(("compound_statement",1),1)),
(LCGrkDelta,Reduce(("compound_statement",1),1)),
(LCPragma,Reduce(("compound_statement",1),1)),
(LCNull,Reduce(("compound_statement",1),1)),
(LCExit,Reduce(("compound_statement",1),1)),
(LCReturn,Reduce(("compound_statement",1),1)),
(LCIf,Reduce(("compound_statement",1),1)),
(LCCase,Reduce(("compound_statement",1),1)),
(LCIdentifier,Reduce(("compound_statement",1),1)),
(LCCharacterLiteral,Reduce(("compound_statement",1),1)),
(LCTill,Reduce(("compound_statement",1),1)),
(LCLoop,Reduce(("compound_statement",1),1)),
(LCFor,Reduce(("compound_statement",1),1)),
(LCWhile,Reduce(("compound_statement",1),1)),
(LCElsIf,Reduce(("compound_statement",1),1)),
(LCElse,Reduce(("compound_statement",1),1)),
(LCEnd,Reduce(("compound_statement",1),1)),
(LCWhen,Reduce(("compound_statement",1),1))]]@slrp'a197;
val slrp'a195=[[(LCEos,Reduce(("compound_statement",0),1)),
(LCAnnotation,Reduce(("compound_statement",0),1)),
(LCCon,Reduce(("compound_statement",0),1)),
(LCLChevron,Reduce(("compound_statement",0),1)),
(LCGrkGamma,Reduce(("compound_statement",0),1)),
(LCGrkDelta,Reduce(("compound_statement",0),1)),
(LCPragma,Reduce(("compound_statement",0),1)),
(LCNull,Reduce(("compound_statement",0),1)),
(LCExit,Reduce(("compound_statement",0),1)),
(LCReturn,Reduce(("compound_statement",0),1)),
(LCIf,Reduce(("compound_statement",0),1)),
(LCCase,Reduce(("compound_statement",0),1)),
(LCIdentifier,Reduce(("compound_statement",0),1)),
(LCCharacterLiteral,Reduce(("compound_statement",0),1)),
(LCTill,Reduce(("compound_statement",0),1)),
(LCLoop,Reduce(("compound_statement",0),1)),
(LCFor,Reduce(("compound_statement",0),1)),
(LCWhile,Reduce(("compound_statement",0),1)),
(LCElsIf,Reduce(("compound_statement",0),1)),
(LCElse,Reduce(("compound_statement",0),1)),
(LCEnd,Reduce(("compound_statement",0),1)),
(LCWhen,Reduce(("compound_statement",0),1))]]@slrp'a196;
val slrp'a194=[[(LCEos,Reduce(("simple_statement",4),1)),
(LCAnnotation,Reduce(("simple_statement",4),1)),
(LCCon,Reduce(("simple_statement",4),1)),
(LCLChevron,Reduce(("simple_statement",4),1)),
(LCGrkGamma,Reduce(("simple_statement",4),1)),
(LCGrkDelta,Reduce(("simple_statement",4),1)),
(LCPragma,Reduce(("simple_statement",4),1)),
(LCNull,Reduce(("simple_statement",4),1)),
(LCExit,Reduce(("simple_statement",4),1)),
(LCReturn,Reduce(("simple_statement",4),1)),
(LCIf,Reduce(("simple_statement",4),1)),
(LCCase,Reduce(("simple_statement",4),1)),
(LCIdentifier,Reduce(("simple_statement",4),1)),
(LCCharacterLiteral,Reduce(("simple_statement",4),1)),
(LCTill,Reduce(("simple_statement",4),1)),
(LCLoop,Reduce(("simple_statement",4),1)),
(LCFor,Reduce(("simple_statement",4),1)),
(LCWhile,Reduce(("simple_statement",4),1)),
(LCElsIf,Reduce(("simple_statement",4),1)),
(LCElse,Reduce(("simple_statement",4),1)),
(LCEnd,Reduce(("simple_statement",4),1)),
(LCWhen,Reduce(("simple_statement",4),1))]]@slrp'a195;
val slrp'a193=[[(LCEos,Reduce(("simple_statement",3),1)),
(LCAnnotation,Reduce(("simple_statement",3),1)),
(LCCon,Reduce(("simple_statement",3),1)),
(LCLChevron,Reduce(("simple_statement",3),1)),
(LCGrkGamma,Reduce(("simple_statement",3),1)),
(LCGrkDelta,Reduce(("simple_statement",3),1)),
(LCPragma,Reduce(("simple_statement",3),1)),
(LCNull,Reduce(("simple_statement",3),1)),
(LCExit,Reduce(("simple_statement",3),1)),
(LCReturn,Reduce(("simple_statement",3),1)),
(LCIf,Reduce(("simple_statement",3),1)),
(LCCase,Reduce(("simple_statement",3),1)),
(LCIdentifier,Reduce(("simple_statement",3),1)),
(LCCharacterLiteral,Reduce(("simple_statement",3),1)),
(LCTill,Reduce(("simple_statement",3),1)),
(LCLoop,Reduce(("simple_statement",3),1)),
(LCFor,Reduce(("simple_statement",3),1)),
(LCWhile,Reduce(("simple_statement",3),1)),
(LCElsIf,Reduce(("simple_statement",3),1)),
(LCElse,Reduce(("simple_statement",3),1)),
(LCEnd,Reduce(("simple_statement",3),1)),
(LCWhen,Reduce(("simple_statement",3),1))]]@slrp'a194;
val slrp'a192=[[(LCEos,Reduce(("simple_statement",2),1)),
(LCAnnotation,Reduce(("simple_statement",2),1)),
(LCCon,Reduce(("simple_statement",2),1)),
(LCLChevron,Reduce(("simple_statement",2),1)),
(LCGrkGamma,Reduce(("simple_statement",2),1)),
(LCGrkDelta,Reduce(("simple_statement",2),1)),
(LCPragma,Reduce(("simple_statement",2),1)),
(LCNull,Reduce(("simple_statement",2),1)),
(LCExit,Reduce(("simple_statement",2),1)),
(LCReturn,Reduce(("simple_statement",2),1)),
(LCIf,Reduce(("simple_statement",2),1)),
(LCCase,Reduce(("simple_statement",2),1)),
(LCIdentifier,Reduce(("simple_statement",2),1)),
(LCCharacterLiteral,Reduce(("simple_statement",2),1)),
(LCTill,Reduce(("simple_statement",2),1)),
(LCLoop,Reduce(("simple_statement",2),1)),
(LCFor,Reduce(("simple_statement",2),1)),
(LCWhile,Reduce(("simple_statement",2),1)),
(LCElsIf,Reduce(("simple_statement",2),1)),
(LCElse,Reduce(("simple_statement",2),1)),
(LCEnd,Reduce(("simple_statement",2),1)),
(LCWhen,Reduce(("simple_statement",2),1))]]@slrp'a193;
val slrp'a191=[[(LCEos,Reduce(("simple_statement",1),1)),
(LCAnnotation,Reduce(("simple_statement",1),1)),
(LCCon,Reduce(("simple_statement",1),1)),
(LCLChevron,Reduce(("simple_statement",1),1)),
(LCGrkGamma,Reduce(("simple_statement",1),1)),
(LCGrkDelta,Reduce(("simple_statement",1),1)),
(LCPragma,Reduce(("simple_statement",1),1)),
(LCNull,Reduce(("simple_statement",1),1)),
(LCExit,Reduce(("simple_statement",1),1)),
(LCReturn,Reduce(("simple_statement",1),1)),
(LCIf,Reduce(("simple_statement",1),1)),
(LCCase,Reduce(("simple_statement",1),1)),
(LCIdentifier,Reduce(("simple_statement",1),1)),
(LCCharacterLiteral,Reduce(("simple_statement",1),1)),
(LCTill,Reduce(("simple_statement",1),1)),
(LCLoop,Reduce(("simple_statement",1),1)),
(LCFor,Reduce(("simple_statement",1),1)),
(LCWhile,Reduce(("simple_statement",1),1)),
(LCElsIf,Reduce(("simple_statement",1),1)),
(LCElse,Reduce(("simple_statement",1),1)),
(LCEnd,Reduce(("simple_statement",1),1)),
(LCWhen,Reduce(("simple_statement",1),1))]]@slrp'a192;
val slrp'a190=[[(LCEos,Reduce(("simple_statement",0),1)),
(LCAnnotation,Reduce(("simple_statement",0),1)),
(LCCon,Reduce(("simple_statement",0),1)),
(LCLChevron,Reduce(("simple_statement",0),1)),
(LCGrkGamma,Reduce(("simple_statement",0),1)),
(LCGrkDelta,Reduce(("simple_statement",0),1)),
(LCPragma,Reduce(("simple_statement",0),1)),
(LCNull,Reduce(("simple_statement",0),1)),
(LCExit,Reduce(("simple_statement",0),1)),
(LCReturn,Reduce(("simple_statement",0),1)),
(LCIf,Reduce(("simple_statement",0),1)),
(LCCase,Reduce(("simple_statement",0),1)),
(LCIdentifier,Reduce(("simple_statement",0),1)),
(LCCharacterLiteral,Reduce(("simple_statement",0),1)),
(LCTill,Reduce(("simple_statement",0),1)),
(LCLoop,Reduce(("simple_statement",0),1)),
(LCFor,Reduce(("simple_statement",0),1)),
(LCWhile,Reduce(("simple_statement",0),1)),
(LCElsIf,Reduce(("simple_statement",0),1)),
(LCElse,Reduce(("simple_statement",0),1)),
(LCEnd,Reduce(("simple_statement",0),1)),
(LCWhen,Reduce(("simple_statement",0),1))]]@slrp'a191;
val slrp'a189=[[(LCEos,Reduce(("k_slot",1),4)),
(LCLChevron,Reduce(("k_slot",1),4)),(LCFunction,Reduce(("k_slot",1),4)),
(LCPackage,Reduce(("k_slot",1),4)),(LCIdentifier,Reduce(("k_slot",1),4)),
(LCAnnotation,Reduce(("k_slot",1),4)),(LCSubType,Reduce(("k_slot",1),4)),
(LCPragma,Reduce(("k_slot",1),4)),(LCProcedure,Reduce(("k_slot",1),4)),
(LCType,Reduce(("k_slot",1),4)),(LCFor,Reduce(("k_slot",1),4)),
(LCUse,Reduce(("k_slot",1),4)),(LCCon,Reduce(("k_slot",1),4)),
(LCGrkGamma,Reduce(("k_slot",1),4)),(LCGrkDelta,Reduce(("k_slot",1),4)),
(LCNull,Reduce(("k_slot",1),4)),(LCExit,Reduce(("k_slot",1),4)),
(LCReturn,Reduce(("k_slot",1),4)),(LCIf,Reduce(("k_slot",1),4)),
(LCCase,Reduce(("k_slot",1),4)),(LCCharacterLiteral,Reduce(("k_slot",1),4)),
(LCTill,Reduce(("k_slot",1),4)),(LCLoop,Reduce(("k_slot",1),4)),
(LCWhile,Reduce(("k_slot",1),4)),(LCSemi,Reduce(("k_slot",1),4)),
(LCEnd,Reduce(("k_slot",1),4)),(LCPrivate,Reduce(("k_slot",1),4)),
(LCAuxiliary,Reduce(("k_slot",1),4)),(LCBegin,Reduce(("k_slot",1),4)),
(LCElsIf,Reduce(("k_slot",1),4)),(LCElse,Reduce(("k_slot",1),4)),
(LCWhen,Reduce(("k_slot",1),4)),(LCIs,Reduce(("k_slot",1),4)),
(LCRenames,Reduce(("k_slot",1),4)),(LCSeparate,Reduce(("k_slot",1),4))]]@slrp'a190;
val slrp'a188=[[(LCEos,Reduce(("label",1),3)),
(LCAnnotation,Reduce(("label",1),3)),(LCCon,Reduce(("label",1),3)),
(LCLChevron,Reduce(("label",1),3)),(LCGrkGamma,Reduce(("label",1),3)),
(LCGrkDelta,Reduce(("label",1),3)),(LCPragma,Reduce(("label",1),3)),
(LCNull,Reduce(("label",1),3)),(LCExit,Reduce(("label",1),3)),
(LCReturn,Reduce(("label",1),3)),(LCIf,Reduce(("label",1),3)),
(LCCase,Reduce(("label",1),3)),(LCIdentifier,Reduce(("label",1),3)),
(LCCharacterLiteral,Reduce(("label",1),3)),(LCTill,Reduce(("label",1),3)),
(LCLoop,Reduce(("label",1),3)),(LCFor,Reduce(("label",1),3)),
(LCWhile,Reduce(("label",1),3)),(LCFunction,Reduce(("label",1),3)),
(LCPackage,Reduce(("label",1),3)),(LCSubType,Reduce(("label",1),3)),
(LCProcedure,Reduce(("label",1),3)),(LCType,Reduce(("label",1),3)),
(LCUse,Reduce(("label",1),3)),(LCElsIf,Reduce(("label",1),3)),
(LCElse,Reduce(("label",1),3)),(LCEnd,Reduce(("label",1),3)),
(LCWhen,Reduce(("label",1),3)),(LCSemi,Reduce(("label",1),3)),
(LCPrivate,Reduce(("label",1),3)),(LCAuxiliary,Reduce(("label",1),3)),
(LCBegin,Reduce(("label",1),3)),(LCIs,Reduce(("label",1),3)),
(LCRenames,Reduce(("label",1),3)),(LCSeparate,Reduce(("label",1),3))]]@slrp'a189;
val slrp'a187=[[(LCEos,Reduce(("label",0),3)),
(LCAnnotation,Reduce(("label",0),3)),(LCCon,Reduce(("label",0),3)),
(LCLChevron,Reduce(("label",0),3)),(LCGrkGamma,Reduce(("label",0),3)),
(LCGrkDelta,Reduce(("label",0),3)),(LCPragma,Reduce(("label",0),3)),
(LCNull,Reduce(("label",0),3)),(LCExit,Reduce(("label",0),3)),
(LCReturn,Reduce(("label",0),3)),(LCIf,Reduce(("label",0),3)),
(LCCase,Reduce(("label",0),3)),(LCIdentifier,Reduce(("label",0),3)),
(LCCharacterLiteral,Reduce(("label",0),3)),(LCTill,Reduce(("label",0),3)),
(LCLoop,Reduce(("label",0),3)),(LCFor,Reduce(("label",0),3)),
(LCWhile,Reduce(("label",0),3)),(LCFunction,Reduce(("label",0),3)),
(LCPackage,Reduce(("label",0),3)),(LCSubType,Reduce(("label",0),3)),
(LCProcedure,Reduce(("label",0),3)),(LCType,Reduce(("label",0),3)),
(LCUse,Reduce(("label",0),3)),(LCElsIf,Reduce(("label",0),3)),
(LCElse,Reduce(("label",0),3)),(LCEnd,Reduce(("label",0),3)),
(LCWhen,Reduce(("label",0),3)),(LCSemi,Reduce(("label",0),3)),
(LCPrivate,Reduce(("label",0),3)),(LCAuxiliary,Reduce(("label",0),3)),
(LCBegin,Reduce(("label",0),3)),(LCIs,Reduce(("label",0),3)),
(LCRenames,Reduce(("label",0),3)),(LCSeparate,Reduce(("label",0),3))]]@slrp'a188;
val slrp'a186=[[(LCCrd,Shift 186)]]@slrp'a187;
val slrp'a185=[[(LCEos,Reduce(("specification_statement",4),4)),
(LCAnnotation,Reduce(("specification_statement",4),4)),
(LCCon,Reduce(("specification_statement",4),4)),
(LCLChevron,Reduce(("specification_statement",4),4)),
(LCGrkGamma,Reduce(("specification_statement",4),4)),
(LCGrkDelta,Reduce(("specification_statement",4),4)),
(LCPragma,Reduce(("specification_statement",4),4)),
(LCNull,Reduce(("specification_statement",4),4)),
(LCExit,Reduce(("specification_statement",4),4)),
(LCReturn,Reduce(("specification_statement",4),4)),
(LCIf,Reduce(("specification_statement",4),4)),
(LCCase,Reduce(("specification_statement",4),4)),
(LCIdentifier,Reduce(("specification_statement",4),4)),
(LCCharacterLiteral,Reduce(("specification_statement",4),4)),
(LCTill,Reduce(("specification_statement",4),4)),
(LCLoop,Reduce(("specification_statement",4),4)),
(LCFor,Reduce(("specification_statement",4),4)),
(LCWhile,Reduce(("specification_statement",4),4)),
(LCOrd,Reduce(("specification_statement",4),4)),
(LCElsIf,Reduce(("specification_statement",4),4)),
(LCElse,Reduce(("specification_statement",4),4)),
(LCEnd,Reduce(("specification_statement",4),4)),
(LCWhen,Reduce(("specification_statement",4),4)),
(LCSemi,Reduce(("specification_statement",4),4)),
(LCIs,Reduce(("specification_statement",4),4)),
(LCRenames,Reduce(("specification_statement",4),4))]]@slrp'a186;
val slrp'a184=[[(LCRBrace,Shift 184)]]@slrp'a185;
val slrp'a183=[[(LCZ,Shift 183)]]@slrp'a184;
val slrp'a182=[[(LCLBrace,Shift 182)]]@slrp'a183;
val slrp'a181=[[(LCEos,Reduce(("specification_statement",3),4)),
(LCAnnotation,Reduce(("specification_statement",3),4)),
(LCCon,Reduce(("specification_statement",3),4)),
(LCLChevron,Reduce(("specification_statement",3),4)),
(LCGrkGamma,Reduce(("specification_statement",3),4)),
(LCGrkDelta,Reduce(("specification_statement",3),4)),
(LCPragma,Reduce(("specification_statement",3),4)),
(LCNull,Reduce(("specification_statement",3),4)),
(LCExit,Reduce(("specification_statement",3),4)),
(LCReturn,Reduce(("specification_statement",3),4)),
(LCIf,Reduce(("specification_statement",3),4)),
(LCCase,Reduce(("specification_statement",3),4)),
(LCIdentifier,Reduce(("specification_statement",3),4)),
(LCCharacterLiteral,Reduce(("specification_statement",3),4)),
(LCTill,Reduce(("specification_statement",3),4)),
(LCLoop,Reduce(("specification_statement",3),4)),
(LCFor,Reduce(("specification_statement",3),4)),
(LCWhile,Reduce(("specification_statement",3),4)),
(LCOrd,Reduce(("specification_statement",3),4)),
(LCElsIf,Reduce(("specification_statement",3),4)),
(LCElse,Reduce(("specification_statement",3),4)),
(LCEnd,Reduce(("specification_statement",3),4)),
(LCWhen,Reduce(("specification_statement",3),4)),
(LCSemi,Reduce(("specification_statement",3),4)),
(LCIs,Reduce(("specification_statement",3),4)),
(LCRenames,Reduce(("specification_statement",3),4))]]@slrp'a182;
val slrp'a180=[[(LCRBrace,Shift 180)]]@slrp'a181;
val slrp'a179=[[(LCZ,Shift 179)]]@slrp'a180;
val slrp'a178=[[(LCEos,Reduce(("specification_statement",2),4)),
(LCAnnotation,Reduce(("specification_statement",2),4)),
(LCCon,Reduce(("specification_statement",2),4)),
(LCLChevron,Reduce(("specification_statement",2),4)),
(LCGrkGamma,Reduce(("specification_statement",2),4)),
(LCGrkDelta,Reduce(("specification_statement",2),4)),
(LCPragma,Reduce(("specification_statement",2),4)),
(LCNull,Reduce(("specification_statement",2),4)),
(LCExit,Reduce(("specification_statement",2),4)),
(LCReturn,Reduce(("specification_statement",2),4)),
(LCIf,Reduce(("specification_statement",2),4)),
(LCCase,Reduce(("specification_statement",2),4)),
(LCIdentifier,Reduce(("specification_statement",2),4)),
(LCCharacterLiteral,Reduce(("specification_statement",2),4)),
(LCTill,Reduce(("specification_statement",2),4)),
(LCLoop,Reduce(("specification_statement",2),4)),
(LCFor,Reduce(("specification_statement",2),4)),
(LCWhile,Reduce(("specification_statement",2),4)),
(LCOrd,Reduce(("specification_statement",2),4)),
(LCElsIf,Reduce(("specification_statement",2),4)),
(LCElse,Reduce(("specification_statement",2),4)),
(LCEnd,Reduce(("specification_statement",2),4)),
(LCWhen,Reduce(("specification_statement",2),4)),
(LCSemi,Reduce(("specification_statement",2),4)),
(LCIs,Reduce(("specification_statement",2),4)),
(LCRenames,Reduce(("specification_statement",2),4))]]@slrp'a179;
val slrp'a177=[[(LCRSqBrack,Shift 177)]]@slrp'a178;
val slrp'a176=[[(LCZ,Shift 176)]]@slrp'a177;
val slrp'a175=[[(LCEos,Reduce(("specification_statement",1),5)),
(LCAnnotation,Reduce(("specification_statement",1),5)),
(LCCon,Reduce(("specification_statement",1),5)),
(LCLChevron,Reduce(("specification_statement",1),5)),
(LCGrkGamma,Reduce(("specification_statement",1),5)),
(LCGrkDelta,Reduce(("specification_statement",1),5)),
(LCPragma,Reduce(("specification_statement",1),5)),
(LCNull,Reduce(("specification_statement",1),5)),
(LCExit,Reduce(("specification_statement",1),5)),
(LCReturn,Reduce(("specification_statement",1),5)),
(LCIf,Reduce(("specification_statement",1),5)),
(LCCase,Reduce(("specification_statement",1),5)),
(LCIdentifier,Reduce(("specification_statement",1),5)),
(LCCharacterLiteral,Reduce(("specification_statement",1),5)),
(LCTill,Reduce(("specification_statement",1),5)),
(LCLoop,Reduce(("specification_statement",1),5)),
(LCFor,Reduce(("specification_statement",1),5)),
(LCWhile,Reduce(("specification_statement",1),5)),
(LCOrd,Reduce(("specification_statement",1),5)),
(LCElsIf,Reduce(("specification_statement",1),5)),
(LCElse,Reduce(("specification_statement",1),5)),
(LCEnd,Reduce(("specification_statement",1),5)),
(LCWhen,Reduce(("specification_statement",1),5)),
(LCSemi,Reduce(("specification_statement",1),5)),
(LCIs,Reduce(("specification_statement",1),5)),
(LCRenames,Reduce(("specification_statement",1),5))]]@slrp'a176;
val slrp'a174=[[(LCRBrace,Shift 174)]]@slrp'a175;
val slrp'a173=[[(LCZ,Shift 173)]]@slrp'a174;
val slrp'a172=[[(LCEos,Reduce(("specification_statement",0),5)),
(LCAnnotation,Reduce(("specification_statement",0),5)),
(LCCon,Reduce(("specification_statement",0),5)),
(LCLChevron,Reduce(("specification_statement",0),5)),
(LCGrkGamma,Reduce(("specification_statement",0),5)),
(LCGrkDelta,Reduce(("specification_statement",0),5)),
(LCPragma,Reduce(("specification_statement",0),5)),
(LCNull,Reduce(("specification_statement",0),5)),
(LCExit,Reduce(("specification_statement",0),5)),
(LCReturn,Reduce(("specification_statement",0),5)),
(LCIf,Reduce(("specification_statement",0),5)),
(LCCase,Reduce(("specification_statement",0),5)),
(LCIdentifier,Reduce(("specification_statement",0),5)),
(LCCharacterLiteral,Reduce(("specification_statement",0),5)),
(LCTill,Reduce(("specification_statement",0),5)),
(LCLoop,Reduce(("specification_statement",0),5)),
(LCFor,Reduce(("specification_statement",0),5)),
(LCWhile,Reduce(("specification_statement",0),5)),
(LCOrd,Reduce(("specification_statement",0),5)),
(LCElsIf,Reduce(("specification_statement",0),5)),
(LCElse,Reduce(("specification_statement",0),5)),
(LCEnd,Reduce(("specification_statement",0),5)),
(LCWhen,Reduce(("specification_statement",0),5)),
(LCSemi,Reduce(("specification_statement",0),5)),
(LCIs,Reduce(("specification_statement",0),5)),
(LCRenames,Reduce(("specification_statement",0),5))]]@slrp'a173;
val slrp'a171=[[(LCRSqBrack,Shift 171)]]@slrp'a172;
val slrp'a170=[[(LCZ,Shift 170)]]@slrp'a171;
val slrp'a169=[[(LCEos,Reduce(("statement",8),1)),
(LCAnnotation,Reduce(("statement",8),1)),(LCCon,Reduce(("statement",8),1)),
(LCLChevron,Reduce(("statement",8),1)),(LCGrkGamma,Reduce(("statement",8),1)),
(LCGrkDelta,Reduce(("statement",8),1)),(LCPragma,Reduce(("statement",8),1)),
(LCNull,Reduce(("statement",8),1)),(LCExit,Reduce(("statement",8),1)),
(LCReturn,Reduce(("statement",8),1)),(LCIf,Reduce(("statement",8),1)),
(LCCase,Reduce(("statement",8),1)),(LCIdentifier,Reduce(("statement",8),1)),
(LCCharacterLiteral,Reduce(("statement",8),1)),
(LCTill,Reduce(("statement",8),1)),(LCLoop,Reduce(("statement",8),1)),
(LCFor,Reduce(("statement",8),1)),(LCWhile,Reduce(("statement",8),1)),
(LCElsIf,Reduce(("statement",8),1)),(LCElse,Reduce(("statement",8),1)),
(LCEnd,Reduce(("statement",8),1)),(LCWhen,Reduce(("statement",8),1))]]@slrp'a170;
val slrp'a168=[[(LCEos,Reduce(("statement",7),1)),
(LCAnnotation,Reduce(("statement",7),1)),(LCCon,Reduce(("statement",7),1)),
(LCLChevron,Reduce(("statement",7),1)),(LCGrkGamma,Reduce(("statement",7),1)),
(LCGrkDelta,Reduce(("statement",7),1)),(LCPragma,Reduce(("statement",7),1)),
(LCNull,Reduce(("statement",7),1)),(LCExit,Reduce(("statement",7),1)),
(LCReturn,Reduce(("statement",7),1)),(LCIf,Reduce(("statement",7),1)),
(LCCase,Reduce(("statement",7),1)),(LCIdentifier,Reduce(("statement",7),1)),
(LCCharacterLiteral,Reduce(("statement",7),1)),
(LCTill,Reduce(("statement",7),1)),(LCLoop,Reduce(("statement",7),1)),
(LCFor,Reduce(("statement",7),1)),(LCWhile,Reduce(("statement",7),1)),
(LCElsIf,Reduce(("statement",7),1)),(LCElse,Reduce(("statement",7),1)),
(LCEnd,Reduce(("statement",7),1)),(LCWhen,Reduce(("statement",7),1))]]@slrp'a169;
val slrp'a167=[[(LCEos,Reduce(("statement",6),1)),
(LCAnnotation,Reduce(("statement",6),1)),(LCCon,Reduce(("statement",6),1)),
(LCLChevron,Reduce(("statement",6),1)),(LCGrkGamma,Reduce(("statement",6),1)),
(LCGrkDelta,Reduce(("statement",6),1)),(LCPragma,Reduce(("statement",6),1)),
(LCNull,Reduce(("statement",6),1)),(LCExit,Reduce(("statement",6),1)),
(LCReturn,Reduce(("statement",6),1)),(LCIf,Reduce(("statement",6),1)),
(LCCase,Reduce(("statement",6),1)),(LCIdentifier,Reduce(("statement",6),1)),
(LCCharacterLiteral,Reduce(("statement",6),1)),
(LCTill,Reduce(("statement",6),1)),(LCLoop,Reduce(("statement",6),1)),
(LCFor,Reduce(("statement",6),1)),(LCWhile,Reduce(("statement",6),1)),
(LCElsIf,Reduce(("statement",6),1)),(LCElse,Reduce(("statement",6),1)),
(LCEnd,Reduce(("statement",6),1)),(LCWhen,Reduce(("statement",6),1))]]@slrp'a168;
val slrp'a166=[[(LCEos,Reduce(("statement",5),1)),
(LCAnnotation,Reduce(("statement",5),1)),(LCCon,Reduce(("statement",5),1)),
(LCLChevron,Reduce(("statement",5),1)),(LCGrkGamma,Reduce(("statement",5),1)),
(LCGrkDelta,Reduce(("statement",5),1)),(LCPragma,Reduce(("statement",5),1)),
(LCNull,Reduce(("statement",5),1)),(LCExit,Reduce(("statement",5),1)),
(LCReturn,Reduce(("statement",5),1)),(LCIf,Reduce(("statement",5),1)),
(LCCase,Reduce(("statement",5),1)),(LCIdentifier,Reduce(("statement",5),1)),
(LCCharacterLiteral,Reduce(("statement",5),1)),
(LCTill,Reduce(("statement",5),1)),(LCLoop,Reduce(("statement",5),1)),
(LCFor,Reduce(("statement",5),1)),(LCWhile,Reduce(("statement",5),1)),
(LCElsIf,Reduce(("statement",5),1)),(LCElse,Reduce(("statement",5),1)),
(LCEnd,Reduce(("statement",5),1)),(LCWhen,Reduce(("statement",5),1))]]@slrp'a167;
val slrp'a165=[[(LCEos,Reduce(("statement",4),5)),
(LCAnnotation,Reduce(("statement",4),5)),(LCCon,Reduce(("statement",4),5)),
(LCLChevron,Reduce(("statement",4),5)),(LCGrkGamma,Reduce(("statement",4),5)),
(LCGrkDelta,Reduce(("statement",4),5)),(LCPragma,Reduce(("statement",4),5)),
(LCNull,Reduce(("statement",4),5)),(LCExit,Reduce(("statement",4),5)),
(LCReturn,Reduce(("statement",4),5)),(LCIf,Reduce(("statement",4),5)),
(LCCase,Reduce(("statement",4),5)),(LCIdentifier,Reduce(("statement",4),5)),
(LCCharacterLiteral,Reduce(("statement",4),5)),
(LCTill,Reduce(("statement",4),5)),(LCLoop,Reduce(("statement",4),5)),
(LCFor,Reduce(("statement",4),5)),(LCWhile,Reduce(("statement",4),5)),
(LCElsIf,Reduce(("statement",4),5)),(LCElse,Reduce(("statement",4),5)),
(LCEnd,Reduce(("statement",4),5)),(LCWhen,Reduce(("statement",4),5))]]@slrp'a166;
val slrp'a164=[[(LCEos,Reduce(("statement",2),2)),
(LCAnnotation,Reduce(("statement",2),2)),(LCCon,Reduce(("statement",2),2)),
(LCLChevron,Reduce(("statement",2),2)),(LCGrkGamma,Reduce(("statement",2),2)),
(LCGrkDelta,Reduce(("statement",2),2)),(LCPragma,Reduce(("statement",2),2)),
(LCNull,Reduce(("statement",2),2)),(LCExit,Reduce(("statement",2),2)),
(LCReturn,Reduce(("statement",2),2)),(LCIf,Reduce(("statement",2),2)),
(LCCase,Reduce(("statement",2),2)),(LCIdentifier,Reduce(("statement",2),2)),
(LCCharacterLiteral,Reduce(("statement",2),2)),
(LCTill,Reduce(("statement",2),2)),(LCLoop,Reduce(("statement",2),2)),
(LCFor,Reduce(("statement",2),2)),(LCWhile,Reduce(("statement",2),2)),
(LCElsIf,Reduce(("statement",2),2)),(LCElse,Reduce(("statement",2),2)),
(LCEnd,Reduce(("statement",2),2)),(LCWhen,Reduce(("statement",2),2))]]@slrp'a165;
val slrp'a163=[[(LCEos,Reduce(("statement",0),1)),
(LCAnnotation,Reduce(("statement",0),1)),(LCCon,Reduce(("statement",0),1)),
(LCLChevron,Reduce(("statement",0),1)),(LCGrkGamma,Reduce(("statement",0),1)),
(LCGrkDelta,Reduce(("statement",0),1)),(LCPragma,Reduce(("statement",0),1)),
(LCNull,Reduce(("statement",0),1)),(LCExit,Reduce(("statement",0),1)),
(LCReturn,Reduce(("statement",0),1)),(LCIf,Reduce(("statement",0),1)),
(LCCase,Reduce(("statement",0),1)),(LCIdentifier,Reduce(("statement",0),1)),
(LCCharacterLiteral,Reduce(("statement",0),1)),
(LCTill,Reduce(("statement",0),1)),(LCLoop,Reduce(("statement",0),1)),
(LCFor,Reduce(("statement",0),1)),(LCWhile,Reduce(("statement",0),1)),
(LCElsIf,Reduce(("statement",0),1)),(LCElse,Reduce(("statement",0),1)),
(LCEnd,Reduce(("statement",0),1)),(LCWhen,Reduce(("statement",0),1))]]@slrp'a164;
val slrp'a162=[[(LCEos,Reduce(("sequence_of_statements",1),2)),
(LCElsIf,Reduce(("sequence_of_statements",1),2)),
(LCElse,Reduce(("sequence_of_statements",1),2)),
(LCEnd,Reduce(("sequence_of_statements",1),2)),
(LCWhen,Reduce(("sequence_of_statements",1),2))]]@slrp'a163;
val slrp'a161=[[(LCBecomes,Reduce(("qualified_expression",0),3)),
(LCSemi,Reduce(("qualified_expression",0),3)),
(LCRRange,Reduce(("qualified_expression",0),3)),
(LCDigits,Reduce(("qualified_expression",0),3)),
(LCDelta,Reduce(("qualified_expression",0),3)),
(LCComma,Reduce(("qualified_expression",0),3)),
(LCCrd,Reduce(("qualified_expression",0),3)),
(LCAnd,Reduce(("qualified_expression",0),3)),
(LCOr,Reduce(("qualified_expression",0),3)),
(LCXor,Reduce(("qualified_expression",0),3)),
(LCGreaterEquals,Reduce(("qualified_expression",0),3)),
(LCGreaterThan,Reduce(("qualified_expression",0),3)),
(LCLessEquals,Reduce(("qualified_expression",0),3)),
(LCLessThan,Reduce(("qualified_expression",0),3)),
(LCNotEquals,Reduce(("qualified_expression",0),3)),
(LCEquals,Reduce(("qualified_expression",0),3)),
(LCIn,Reduce(("qualified_expression",0),3)),
(LCNot,Reduce(("qualified_expression",0),3)),
(LCDotDot,Reduce(("qualified_expression",0),3)),
(LCAmpersand,Reduce(("qualified_expression",0),3)),
(LCMinus,Reduce(("qualified_expression",0),3)),
(LCPlus,Reduce(("qualified_expression",0),3)),
(LCRem,Reduce(("qualified_expression",0),3)),
(LCMod,Reduce(("qualified_expression",0),3)),
(LCDiv,Reduce(("qualified_expression",0),3)),
(LCStar,Reduce(("qualified_expression",0),3)),
(LCStarStar,Reduce(("qualified_expression",0),3)),
(LCThen,Reduce(("qualified_expression",0),3)),
(LCIs,Reduce(("qualified_expression",0),3)),
(LCGoesTo,Reduce(("qualified_expression",0),3)),
(LCBar,Reduce(("qualified_expression",0),3)),
(LCTill,Reduce(("qualified_expression",0),3)),
(LCLoop,Reduce(("qualified_expression",0),3))]]@slrp'a162;
val slrp'a160=[[(LCNot,Reduce(("multiplying_operator",3),1)),
(LCAbs,Reduce(("multiplying_operator",3),1)),
(LCStringLiteral,Reduce(("multiplying_operator",3),1)),
(LCCharacterLiteral,Reduce(("multiplying_operator",3),1)),
(LCNumericLiteral,Reduce(("multiplying_operator",3),1)),
(LCIdentifier,Reduce(("multiplying_operator",3),1)),
(LCOrd,Reduce(("multiplying_operator",3),1)),
(LCLStrachey,Reduce(("multiplying_operator",3),1))]]@slrp'a161;
val slrp'a159=[[(LCNot,Reduce(("multiplying_operator",2),1)),
(LCAbs,Reduce(("multiplying_operator",2),1)),
(LCStringLiteral,Reduce(("multiplying_operator",2),1)),
(LCCharacterLiteral,Reduce(("multiplying_operator",2),1)),
(LCNumericLiteral,Reduce(("multiplying_operator",2),1)),
(LCIdentifier,Reduce(("multiplying_operator",2),1)),
(LCOrd,Reduce(("multiplying_operator",2),1)),
(LCLStrachey,Reduce(("multiplying_operator",2),1))]]@slrp'a160;
val slrp'a158=[[(LCNot,Reduce(("multiplying_operator",1),1)),
(LCAbs,Reduce(("multiplying_operator",1),1)),
(LCStringLiteral,Reduce(("multiplying_operator",1),1)),
(LCCharacterLiteral,Reduce(("multiplying_operator",1),1)),
(LCNumericLiteral,Reduce(("multiplying_operator",1),1)),
(LCIdentifier,Reduce(("multiplying_operator",1),1)),
(LCOrd,Reduce(("multiplying_operator",1),1)),
(LCLStrachey,Reduce(("multiplying_operator",1),1))]]@slrp'a159;
val slrp'a157=[[(LCNot,Reduce(("multiplying_operator",0),1)),
(LCAbs,Reduce(("multiplying_operator",0),1)),
(LCStringLiteral,Reduce(("multiplying_operator",0),1)),
(LCCharacterLiteral,Reduce(("multiplying_operator",0),1)),
(LCNumericLiteral,Reduce(("multiplying_operator",0),1)),
(LCIdentifier,Reduce(("multiplying_operator",0),1)),
(LCOrd,Reduce(("multiplying_operator",0),1)),
(LCLStrachey,Reduce(("multiplying_operator",0),1))]]@slrp'a158;
val slrp'a156=[[(LCNot,Reduce(("unary_adding_operator",1),1)),
(LCAbs,Reduce(("unary_adding_operator",1),1)),
(LCStringLiteral,Reduce(("unary_adding_operator",1),1)),
(LCCharacterLiteral,Reduce(("unary_adding_operator",1),1)),
(LCNumericLiteral,Reduce(("unary_adding_operator",1),1)),
(LCIdentifier,Reduce(("unary_adding_operator",1),1)),
(LCOrd,Reduce(("unary_adding_operator",1),1)),
(LCLStrachey,Reduce(("unary_adding_operator",1),1))]]@slrp'a157;
val slrp'a155=[[(LCNot,Reduce(("unary_adding_operator",0),1)),
(LCAbs,Reduce(("unary_adding_operator",0),1)),
(LCStringLiteral,Reduce(("unary_adding_operator",0),1)),
(LCCharacterLiteral,Reduce(("unary_adding_operator",0),1)),
(LCNumericLiteral,Reduce(("unary_adding_operator",0),1)),
(LCIdentifier,Reduce(("unary_adding_operator",0),1)),
(LCOrd,Reduce(("unary_adding_operator",0),1)),
(LCLStrachey,Reduce(("unary_adding_operator",0),1))]]@slrp'a156;
val slrp'a154=[[(LCNot,Reduce(("binary_adding_operator",2),1)),
(LCAbs,Reduce(("binary_adding_operator",2),1)),
(LCStringLiteral,Reduce(("binary_adding_operator",2),1)),
(LCCharacterLiteral,Reduce(("binary_adding_operator",2),1)),
(LCNumericLiteral,Reduce(("binary_adding_operator",2),1)),
(LCIdentifier,Reduce(("binary_adding_operator",2),1)),
(LCOrd,Reduce(("binary_adding_operator",2),1)),
(LCLStrachey,Reduce(("binary_adding_operator",2),1))]]@slrp'a155;
val slrp'a153=[[(LCNot,Reduce(("binary_adding_operator",1),1)),
(LCAbs,Reduce(("binary_adding_operator",1),1)),
(LCStringLiteral,Reduce(("binary_adding_operator",1),1)),
(LCCharacterLiteral,Reduce(("binary_adding_operator",1),1)),
(LCNumericLiteral,Reduce(("binary_adding_operator",1),1)),
(LCIdentifier,Reduce(("binary_adding_operator",1),1)),
(LCOrd,Reduce(("binary_adding_operator",1),1)),
(LCLStrachey,Reduce(("binary_adding_operator",1),1))]]@slrp'a154;
val slrp'a152=[[(LCNot,Reduce(("binary_adding_operator",0),1)),
(LCAbs,Reduce(("binary_adding_operator",0),1)),
(LCStringLiteral,Reduce(("binary_adding_operator",0),1)),
(LCCharacterLiteral,Reduce(("binary_adding_operator",0),1)),
(LCNumericLiteral,Reduce(("binary_adding_operator",0),1)),
(LCIdentifier,Reduce(("binary_adding_operator",0),1)),
(LCOrd,Reduce(("binary_adding_operator",0),1)),
(LCLStrachey,Reduce(("binary_adding_operator",0),1))]]@slrp'a153;
val slrp'a151=[[(LCMinus,Reduce(("relational_operator",5),1)),
(LCPlus,Reduce(("relational_operator",5),1)),
(LCNot,Reduce(("relational_operator",5),1)),
(LCAbs,Reduce(("relational_operator",5),1)),
(LCStringLiteral,Reduce(("relational_operator",5),1)),
(LCCharacterLiteral,Reduce(("relational_operator",5),1)),
(LCNumericLiteral,Reduce(("relational_operator",5),1)),
(LCIdentifier,Reduce(("relational_operator",5),1)),
(LCOrd,Reduce(("relational_operator",5),1)),
(LCLStrachey,Reduce(("relational_operator",5),1))]]@slrp'a152;
val slrp'a150=[[(LCMinus,Reduce(("relational_operator",4),1)),
(LCPlus,Reduce(("relational_operator",4),1)),
(LCNot,Reduce(("relational_operator",4),1)),
(LCAbs,Reduce(("relational_operator",4),1)),
(LCStringLiteral,Reduce(("relational_operator",4),1)),
(LCCharacterLiteral,Reduce(("relational_operator",4),1)),
(LCNumericLiteral,Reduce(("relational_operator",4),1)),
(LCIdentifier,Reduce(("relational_operator",4),1)),
(LCOrd,Reduce(("relational_operator",4),1)),
(LCLStrachey,Reduce(("relational_operator",4),1))]]@slrp'a151;
val slrp'a149=[[(LCMinus,Reduce(("relational_operator",3),1)),
(LCPlus,Reduce(("relational_operator",3),1)),
(LCNot,Reduce(("relational_operator",3),1)),
(LCAbs,Reduce(("relational_operator",3),1)),
(LCStringLiteral,Reduce(("relational_operator",3),1)),
(LCCharacterLiteral,Reduce(("relational_operator",3),1)),
(LCNumericLiteral,Reduce(("relational_operator",3),1)),
(LCIdentifier,Reduce(("relational_operator",3),1)),
(LCOrd,Reduce(("relational_operator",3),1)),
(LCLStrachey,Reduce(("relational_operator",3),1))]]@slrp'a150;
val slrp'a148=[[(LCMinus,Reduce(("relational_operator",2),1)),
(LCPlus,Reduce(("relational_operator",2),1)),
(LCNot,Reduce(("relational_operator",2),1)),
(LCAbs,Reduce(("relational_operator",2),1)),
(LCStringLiteral,Reduce(("relational_operator",2),1)),
(LCCharacterLiteral,Reduce(("relational_operator",2),1)),
(LCNumericLiteral,Reduce(("relational_operator",2),1)),
(LCIdentifier,Reduce(("relational_operator",2),1)),
(LCOrd,Reduce(("relational_operator",2),1)),
(LCLStrachey,Reduce(("relational_operator",2),1))]]@slrp'a149;
val slrp'a147=[[(LCMinus,Reduce(("relational_operator",1),1)),
(LCPlus,Reduce(("relational_operator",1),1)),
(LCNot,Reduce(("relational_operator",1),1)),
(LCAbs,Reduce(("relational_operator",1),1)),
(LCStringLiteral,Reduce(("relational_operator",1),1)),
(LCCharacterLiteral,Reduce(("relational_operator",1),1)),
(LCNumericLiteral,Reduce(("relational_operator",1),1)),
(LCIdentifier,Reduce(("relational_operator",1),1)),
(LCOrd,Reduce(("relational_operator",1),1)),
(LCLStrachey,Reduce(("relational_operator",1),1))]]@slrp'a148;
val slrp'a146=[[(LCMinus,Reduce(("relational_operator",0),1)),
(LCPlus,Reduce(("relational_operator",0),1)),
(LCNot,Reduce(("relational_operator",0),1)),
(LCAbs,Reduce(("relational_operator",0),1)),
(LCStringLiteral,Reduce(("relational_operator",0),1)),
(LCCharacterLiteral,Reduce(("relational_operator",0),1)),
(LCNumericLiteral,Reduce(("relational_operator",0),1)),
(LCIdentifier,Reduce(("relational_operator",0),1)),
(LCOrd,Reduce(("relational_operator",0),1)),
(LCLStrachey,Reduce(("relational_operator",0),1))]]@slrp'a147;
val slrp'a145=[[(LCBecomes,Reduce(("primary",6),1)),
(LCSemi,Reduce(("primary",6),1)),(LCRRange,Reduce(("primary",6),1)),
(LCDigits,Reduce(("primary",6),1)),(LCDelta,Reduce(("primary",6),1)),
(LCComma,Reduce(("primary",6),1)),(LCCrd,Reduce(("primary",6),1)),
(LCAnd,Reduce(("primary",6),1)),(LCOr,Reduce(("primary",6),1)),
(LCXor,Reduce(("primary",6),1)),(LCGreaterEquals,Reduce(("primary",6),1)),
(LCGreaterThan,Reduce(("primary",6),1)),(LCLessEquals,Reduce(("primary",6),1)),
(LCLessThan,Reduce(("primary",6),1)),(LCNotEquals,Reduce(("primary",6),1)),
(LCEquals,Reduce(("primary",6),1)),(LCIn,Reduce(("primary",6),1)),
(LCNot,Reduce(("primary",6),1)),(LCDotDot,Reduce(("primary",6),1)),
(LCAmpersand,Reduce(("primary",6),1)),(LCMinus,Reduce(("primary",6),1)),
(LCPlus,Reduce(("primary",6),1)),(LCRem,Reduce(("primary",6),1)),
(LCMod,Reduce(("primary",6),1)),(LCDiv,Reduce(("primary",6),1)),
(LCStar,Reduce(("primary",6),1)),(LCStarStar,Reduce(("primary",6),1)),
(LCThen,Reduce(("primary",6),1)),(LCIs,Reduce(("primary",6),1)),
(LCGoesTo,Reduce(("primary",6),1)),(LCBar,Reduce(("primary",6),1)),
(LCTill,Reduce(("primary",6),1)),(LCLoop,Reduce(("primary",6),1))]]@slrp'a146;
val slrp'a144=[[(LCBecomes,Reduce(("primary",5),1)),
(LCSemi,Reduce(("primary",5),1)),(LCRRange,Reduce(("primary",5),1)),
(LCDigits,Reduce(("primary",5),1)),(LCDelta,Reduce(("primary",5),1)),
(LCComma,Reduce(("primary",5),1)),(LCCrd,Reduce(("primary",5),1)),
(LCAnd,Reduce(("primary",5),1)),(LCOr,Reduce(("primary",5),1)),
(LCXor,Reduce(("primary",5),1)),(LCGreaterEquals,Reduce(("primary",5),1)),
(LCGreaterThan,Reduce(("primary",5),1)),(LCLessEquals,Reduce(("primary",5),1)),
(LCLessThan,Reduce(("primary",5),1)),(LCNotEquals,Reduce(("primary",5),1)),
(LCEquals,Reduce(("primary",5),1)),(LCIn,Reduce(("primary",5),1)),
(LCNot,Reduce(("primary",5),1)),(LCDotDot,Reduce(("primary",5),1)),
(LCAmpersand,Reduce(("primary",5),1)),(LCMinus,Reduce(("primary",5),1)),
(LCPlus,Reduce(("primary",5),1)),(LCRem,Reduce(("primary",5),1)),
(LCMod,Reduce(("primary",5),1)),(LCDiv,Reduce(("primary",5),1)),
(LCStar,Reduce(("primary",5),1)),(LCStarStar,Reduce(("primary",5),1)),
(LCThen,Reduce(("primary",5),1)),(LCIs,Reduce(("primary",5),1)),
(LCGoesTo,Reduce(("primary",5),1)),(LCBar,Reduce(("primary",5),1)),
(LCTill,Reduce(("primary",5),1)),(LCLoop,Reduce(("primary",5),1))]]@slrp'a145;
val slrp'a143=[[(LCBecomes,Reduce(("primary",4),1)),
(LCSemi,Reduce(("primary",4),1)),(LCRRange,Reduce(("primary",4),1)),
(LCDigits,Reduce(("primary",4),1)),(LCDelta,Reduce(("primary",4),1)),
(LCComma,Reduce(("primary",4),1)),(LCCrd,Reduce(("primary",4),1)),
(LCAnd,Reduce(("primary",4),1)),(LCOr,Reduce(("primary",4),1)),
(LCXor,Reduce(("primary",4),1)),(LCGreaterEquals,Reduce(("primary",4),1)),
(LCGreaterThan,Reduce(("primary",4),1)),(LCLessEquals,Reduce(("primary",4),1)),
(LCLessThan,Reduce(("primary",4),1)),(LCNotEquals,Reduce(("primary",4),1)),
(LCEquals,Reduce(("primary",4),1)),(LCIn,Reduce(("primary",4),1)),
(LCNot,Reduce(("primary",4),1)),(LCDotDot,Reduce(("primary",4),1)),
(LCAmpersand,Reduce(("primary",4),1)),(LCMinus,Reduce(("primary",4),1)),
(LCPlus,Reduce(("primary",4),1)),(LCRem,Reduce(("primary",4),1)),
(LCMod,Reduce(("primary",4),1)),(LCDiv,Reduce(("primary",4),1)),
(LCStar,Reduce(("primary",4),1)),(LCStarStar,Reduce(("primary",4),1)),
(LCThen,Reduce(("primary",4),1)),(LCIs,Reduce(("primary",4),1)),
(LCGoesTo,Reduce(("primary",4),1)),(LCBar,Reduce(("primary",4),1)),
(LCTill,Reduce(("primary",4),1)),(LCLoop,Reduce(("primary",4),1))]]@slrp'a144;
val slrp'a142=[[(LCBecomes,Reduce(("primary",2),1)),
(LCSemi,Reduce(("primary",2),1)),(LCRRange,Reduce(("primary",2),1)),
(LCDigits,Reduce(("primary",2),1)),(LCDelta,Reduce(("primary",2),1)),
(LCComma,Reduce(("primary",2),1)),(LCCrd,Reduce(("primary",2),1)),
(LCAnd,Reduce(("primary",2),1)),(LCOr,Reduce(("primary",2),1)),
(LCXor,Reduce(("primary",2),1)),(LCGreaterEquals,Reduce(("primary",2),1)),
(LCGreaterThan,Reduce(("primary",2),1)),(LCLessEquals,Reduce(("primary",2),1)),
(LCLessThan,Reduce(("primary",2),1)),(LCNotEquals,Reduce(("primary",2),1)),
(LCEquals,Reduce(("primary",2),1)),(LCIn,Reduce(("primary",2),1)),
(LCNot,Reduce(("primary",2),1)),(LCDotDot,Reduce(("primary",2),1)),
(LCAmpersand,Reduce(("primary",2),1)),(LCMinus,Reduce(("primary",2),1)),
(LCPlus,Reduce(("primary",2),1)),(LCRem,Reduce(("primary",2),1)),
(LCMod,Reduce(("primary",2),1)),(LCDiv,Reduce(("primary",2),1)),
(LCStar,Reduce(("primary",2),1)),(LCStarStar,Reduce(("primary",2),1)),
(LCThen,Reduce(("primary",2),1)),(LCIs,Reduce(("primary",2),1)),
(LCGoesTo,Reduce(("primary",2),1)),(LCBar,Reduce(("primary",2),1)),
(LCTill,Reduce(("primary",2),1)),(LCLoop,Reduce(("primary",2),1))]]@slrp'a143;
val slrp'a141=[[(LCBecomes,Reduce(("primary",0),1)),
(LCSemi,Reduce(("primary",0),1)),(LCRRange,Reduce(("primary",0),1)),
(LCDigits,Reduce(("primary",0),1)),(LCDelta,Reduce(("primary",0),1)),
(LCComma,Reduce(("primary",0),1)),(LCCrd,Reduce(("primary",0),1)),
(LCAnd,Reduce(("primary",0),1)),(LCOr,Reduce(("primary",0),1)),
(LCXor,Reduce(("primary",0),1)),(LCGreaterEquals,Reduce(("primary",0),1)),
(LCGreaterThan,Reduce(("primary",0),1)),(LCLessEquals,Reduce(("primary",0),1)),
(LCLessThan,Reduce(("primary",0),1)),(LCNotEquals,Reduce(("primary",0),1)),
(LCEquals,Reduce(("primary",0),1)),(LCIn,Reduce(("primary",0),1)),
(LCNot,Reduce(("primary",0),1)),(LCDotDot,Reduce(("primary",0),1)),
(LCAmpersand,Reduce(("primary",0),1)),(LCMinus,Reduce(("primary",0),1)),
(LCPlus,Reduce(("primary",0),1)),(LCRem,Reduce(("primary",0),1)),
(LCMod,Reduce(("primary",0),1)),(LCDiv,Reduce(("primary",0),1)),
(LCStar,Reduce(("primary",0),1)),(LCStarStar,Reduce(("primary",0),1)),
(LCThen,Reduce(("primary",0),1)),(LCIs,Reduce(("primary",0),1)),
(LCGoesTo,Reduce(("primary",0),1)),(LCBar,Reduce(("primary",0),1)),
(LCTill,Reduce(("primary",0),1)),(LCLoop,Reduce(("primary",0),1))]]@slrp'a142;
val slrp'a140=[[(LCBecomes,Reduce(("factor",3),2)),
(LCSemi,Reduce(("factor",3),2)),(LCRRange,Reduce(("factor",3),2)),
(LCDigits,Reduce(("factor",3),2)),(LCDelta,Reduce(("factor",3),2)),
(LCComma,Reduce(("factor",3),2)),(LCCrd,Reduce(("factor",3),2)),
(LCAnd,Reduce(("factor",3),2)),(LCOr,Reduce(("factor",3),2)),
(LCXor,Reduce(("factor",3),2)),(LCGreaterEquals,Reduce(("factor",3),2)),
(LCGreaterThan,Reduce(("factor",3),2)),(LCLessEquals,Reduce(("factor",3),2)),
(LCLessThan,Reduce(("factor",3),2)),(LCNotEquals,Reduce(("factor",3),2)),
(LCEquals,Reduce(("factor",3),2)),(LCIn,Reduce(("factor",3),2)),
(LCNot,Reduce(("factor",3),2)),(LCDotDot,Reduce(("factor",3),2)),
(LCAmpersand,Reduce(("factor",3),2)),(LCMinus,Reduce(("factor",3),2)),
(LCPlus,Reduce(("factor",3),2)),(LCRem,Reduce(("factor",3),2)),
(LCMod,Reduce(("factor",3),2)),(LCDiv,Reduce(("factor",3),2)),
(LCStar,Reduce(("factor",3),2)),(LCThen,Reduce(("factor",3),2)),
(LCIs,Reduce(("factor",3),2)),(LCGoesTo,Reduce(("factor",3),2)),
(LCBar,Reduce(("factor",3),2)),(LCTill,Reduce(("factor",3),2)),
(LCLoop,Reduce(("factor",3),2))]]@slrp'a141;
val slrp'a139=[[(LCBecomes,Reduce(("factor",2),2)),
(LCSemi,Reduce(("factor",2),2)),(LCRRange,Reduce(("factor",2),2)),
(LCDigits,Reduce(("factor",2),2)),(LCDelta,Reduce(("factor",2),2)),
(LCComma,Reduce(("factor",2),2)),(LCCrd,Reduce(("factor",2),2)),
(LCAnd,Reduce(("factor",2),2)),(LCOr,Reduce(("factor",2),2)),
(LCXor,Reduce(("factor",2),2)),(LCGreaterEquals,Reduce(("factor",2),2)),
(LCGreaterThan,Reduce(("factor",2),2)),(LCLessEquals,Reduce(("factor",2),2)),
(LCLessThan,Reduce(("factor",2),2)),(LCNotEquals,Reduce(("factor",2),2)),
(LCEquals,Reduce(("factor",2),2)),(LCIn,Reduce(("factor",2),2)),
(LCNot,Reduce(("factor",2),2)),(LCDotDot,Reduce(("factor",2),2)),
(LCAmpersand,Reduce(("factor",2),2)),(LCMinus,Reduce(("factor",2),2)),
(LCPlus,Reduce(("factor",2),2)),(LCRem,Reduce(("factor",2),2)),
(LCMod,Reduce(("factor",2),2)),(LCDiv,Reduce(("factor",2),2)),
(LCStar,Reduce(("factor",2),2)),(LCThen,Reduce(("factor",2),2)),
(LCIs,Reduce(("factor",2),2)),(LCGoesTo,Reduce(("factor",2),2)),
(LCBar,Reduce(("factor",2),2)),(LCTill,Reduce(("factor",2),2)),
(LCLoop,Reduce(("factor",2),2))]]@slrp'a140;
val slrp'a138=[[(LCBecomes,Reduce(("factor",1),3)),
(LCSemi,Reduce(("factor",1),3)),(LCRRange,Reduce(("factor",1),3)),
(LCDigits,Reduce(("factor",1),3)),(LCDelta,Reduce(("factor",1),3)),
(LCComma,Reduce(("factor",1),3)),(LCCrd,Reduce(("factor",1),3)),
(LCAnd,Reduce(("factor",1),3)),(LCOr,Reduce(("factor",1),3)),
(LCXor,Reduce(("factor",1),3)),(LCGreaterEquals,Reduce(("factor",1),3)),
(LCGreaterThan,Reduce(("factor",1),3)),(LCLessEquals,Reduce(("factor",1),3)),
(LCLessThan,Reduce(("factor",1),3)),(LCNotEquals,Reduce(("factor",1),3)),
(LCEquals,Reduce(("factor",1),3)),(LCIn,Reduce(("factor",1),3)),
(LCNot,Reduce(("factor",1),3)),(LCDotDot,Reduce(("factor",1),3)),
(LCAmpersand,Reduce(("factor",1),3)),(LCMinus,Reduce(("factor",1),3)),
(LCPlus,Reduce(("factor",1),3)),(LCRem,Reduce(("factor",1),3)),
(LCMod,Reduce(("factor",1),3)),(LCDiv,Reduce(("factor",1),3)),
(LCStar,Reduce(("factor",1),3)),(LCThen,Reduce(("factor",1),3)),
(LCIs,Reduce(("factor",1),3)),(LCGoesTo,Reduce(("factor",1),3)),
(LCBar,Reduce(("factor",1),3)),(LCTill,Reduce(("factor",1),3)),
(LCLoop,Reduce(("factor",1),3))]]@slrp'a139;
val slrp'a137=[[(LCBecomes,Reduce(("term",1),3)),(LCSemi,Reduce(("term",1),3)),
(LCRRange,Reduce(("term",1),3)),(LCDigits,Reduce(("term",1),3)),
(LCDelta,Reduce(("term",1),3)),(LCComma,Reduce(("term",1),3)),
(LCCrd,Reduce(("term",1),3)),(LCAnd,Reduce(("term",1),3)),
(LCOr,Reduce(("term",1),3)),(LCXor,Reduce(("term",1),3)),
(LCGreaterEquals,Reduce(("term",1),3)),(LCGreaterThan,Reduce(("term",1),3)),
(LCLessEquals,Reduce(("term",1),3)),(LCLessThan,Reduce(("term",1),3)),
(LCNotEquals,Reduce(("term",1),3)),(LCEquals,Reduce(("term",1),3)),
(LCIn,Reduce(("term",1),3)),(LCNot,Reduce(("term",1),3)),
(LCDotDot,Reduce(("term",1),3)),(LCAmpersand,Reduce(("term",1),3)),
(LCMinus,Reduce(("term",1),3)),(LCPlus,Reduce(("term",1),3)),
(LCRem,Reduce(("term",1),3)),(LCMod,Reduce(("term",1),3)),
(LCDiv,Reduce(("term",1),3)),(LCStar,Reduce(("term",1),3)),
(LCThen,Reduce(("term",1),3)),(LCIs,Reduce(("term",1),3)),
(LCGoesTo,Reduce(("term",1),3)),(LCBar,Reduce(("term",1),3)),
(LCTill,Reduce(("term",1),3)),(LCLoop,Reduce(("term",1),3))]]@slrp'a138;
val slrp'a136=[[(LCBecomes,Reduce(("term",0),1)),(LCSemi,Reduce(("term",0),1)),
(LCRRange,Reduce(("term",0),1)),(LCDigits,Reduce(("term",0),1)),
(LCDelta,Reduce(("term",0),1)),(LCComma,Reduce(("term",0),1)),
(LCCrd,Reduce(("term",0),1)),(LCAnd,Reduce(("term",0),1)),
(LCOr,Reduce(("term",0),1)),(LCXor,Reduce(("term",0),1)),
(LCGreaterEquals,Reduce(("term",0),1)),(LCGreaterThan,Reduce(("term",0),1)),
(LCLessEquals,Reduce(("term",0),1)),(LCLessThan,Reduce(("term",0),1)),
(LCNotEquals,Reduce(("term",0),1)),(LCEquals,Reduce(("term",0),1)),
(LCIn,Reduce(("term",0),1)),(LCNot,Reduce(("term",0),1)),
(LCDotDot,Reduce(("term",0),1)),(LCAmpersand,Reduce(("term",0),1)),
(LCMinus,Reduce(("term",0),1)),(LCPlus,Reduce(("term",0),1)),
(LCRem,Reduce(("term",0),1)),(LCMod,Reduce(("term",0),1)),
(LCDiv,Reduce(("term",0),1)),(LCStar,Reduce(("term",0),1)),
(LCThen,Reduce(("term",0),1)),(LCIs,Reduce(("term",0),1)),
(LCGoesTo,Reduce(("term",0),1)),(LCBar,Reduce(("term",0),1)),
(LCTill,Reduce(("term",0),1)),(LCLoop,Reduce(("term",0),1))]]@slrp'a137;
val slrp'a135=[[(LCBecomes,Reduce(("binoptail",1),3)),
(LCSemi,Reduce(("binoptail",1),3)),(LCRRange,Reduce(("binoptail",1),3)),
(LCDigits,Reduce(("binoptail",1),3)),(LCDelta,Reduce(("binoptail",1),3)),
(LCComma,Reduce(("binoptail",1),3)),(LCCrd,Reduce(("binoptail",1),3)),
(LCAnd,Reduce(("binoptail",1),3)),(LCOr,Reduce(("binoptail",1),3)),
(LCXor,Reduce(("binoptail",1),3)),(LCGreaterEquals,Reduce(("binoptail",1),3)),
(LCGreaterThan,Reduce(("binoptail",1),3)),
(LCLessEquals,Reduce(("binoptail",1),3)),(LCLessThan,Reduce(("binoptail",1),3)),
(LCNotEquals,Reduce(("binoptail",1),3)),(LCEquals,Reduce(("binoptail",1),3)),
(LCIn,Reduce(("binoptail",1),3)),(LCNot,Reduce(("binoptail",1),3)),
(LCDotDot,Reduce(("binoptail",1),3)),(LCThen,Reduce(("binoptail",1),3)),
(LCIs,Reduce(("binoptail",1),3)),(LCGoesTo,Reduce(("binoptail",1),3)),
(LCBar,Reduce(("binoptail",1),3)),(LCTill,Reduce(("binoptail",1),3)),
(LCLoop,Reduce(("binoptail",1),3))]]@slrp'a136;
val slrp'a134=[[(LCBecomes,Reduce(("simple_expression",1),3)),
(LCSemi,Reduce(("simple_expression",1),3)),
(LCRRange,Reduce(("simple_expression",1),3)),
(LCDigits,Reduce(("simple_expression",1),3)),
(LCDelta,Reduce(("simple_expression",1),3)),
(LCComma,Reduce(("simple_expression",1),3)),
(LCCrd,Reduce(("simple_expression",1),3)),
(LCAnd,Reduce(("simple_expression",1),3)),
(LCOr,Reduce(("simple_expression",1),3)),
(LCXor,Reduce(("simple_expression",1),3)),
(LCGreaterEquals,Reduce(("simple_expression",1),3)),
(LCGreaterThan,Reduce(("simple_expression",1),3)),
(LCLessEquals,Reduce(("simple_expression",1),3)),
(LCLessThan,Reduce(("simple_expression",1),3)),
(LCNotEquals,Reduce(("simple_expression",1),3)),
(LCEquals,Reduce(("simple_expression",1),3)),
(LCIn,Reduce(("simple_expression",1),3)),
(LCNot,Reduce(("simple_expression",1),3)),
(LCDotDot,Reduce(("simple_expression",1),3)),
(LCThen,Reduce(("simple_expression",1),3)),
(LCIs,Reduce(("simple_expression",1),3)),
(LCGoesTo,Reduce(("simple_expression",1),3)),
(LCBar,Reduce(("simple_expression",1),3)),
(LCTill,Reduce(("simple_expression",1),3)),
(LCLoop,Reduce(("simple_expression",1),3))]]@slrp'a135;
val slrp'a133=[[(LCBecomes,Reduce(("simple_expression",0),2)),
(LCSemi,Reduce(("simple_expression",0),2)),
(LCRRange,Reduce(("simple_expression",0),2)),
(LCDigits,Reduce(("simple_expression",0),2)),
(LCDelta,Reduce(("simple_expression",0),2)),
(LCComma,Reduce(("simple_expression",0),2)),
(LCCrd,Reduce(("simple_expression",0),2)),
(LCAnd,Reduce(("simple_expression",0),2)),
(LCOr,Reduce(("simple_expression",0),2)),
(LCXor,Reduce(("simple_expression",0),2)),
(LCGreaterEquals,Reduce(("simple_expression",0),2)),
(LCGreaterThan,Reduce(("simple_expression",0),2)),
(LCLessEquals,Reduce(("simple_expression",0),2)),
(LCLessThan,Reduce(("simple_expression",0),2)),
(LCNotEquals,Reduce(("simple_expression",0),2)),
(LCEquals,Reduce(("simple_expression",0),2)),
(LCIn,Reduce(("simple_expression",0),2)),
(LCNot,Reduce(("simple_expression",0),2)),
(LCDotDot,Reduce(("simple_expression",0),2)),
(LCThen,Reduce(("simple_expression",0),2)),
(LCIs,Reduce(("simple_expression",0),2)),
(LCGoesTo,Reduce(("simple_expression",0),2)),
(LCBar,Reduce(("simple_expression",0),2)),
(LCTill,Reduce(("simple_expression",0),2)),
(LCLoop,Reduce(("simple_expression",0),2))]]@slrp'a134;
val slrp'a132=[[(LCBecomes,Reduce(("sr_expression",1),3)),
(LCSemi,Reduce(("sr_expression",1),3)),(LCRRange,Reduce(("sr_expression",1),3)),
(LCDigits,Reduce(("sr_expression",1),3)),(LCDelta,Reduce(("sr_expression",1),3))
,(LCComma,Reduce(("sr_expression",1),3)),(LCCrd,Reduce(("sr_expression",1),3)),
(LCAnd,Reduce(("sr_expression",1),3)),(LCOr,Reduce(("sr_expression",1),3)),
(LCXor,Reduce(("sr_expression",1),3)),
(LCGreaterEquals,Reduce(("sr_expression",1),3)),
(LCGreaterThan,Reduce(("sr_expression",1),3)),
(LCLessEquals,Reduce(("sr_expression",1),3)),
(LCLessThan,Reduce(("sr_expression",1),3)),
(LCNotEquals,Reduce(("sr_expression",1),3)),
(LCEquals,Reduce(("sr_expression",1),3)),(LCIn,Reduce(("sr_expression",1),3)),
(LCNot,Reduce(("sr_expression",1),3)),(LCThen,Reduce(("sr_expression",1),3)),
(LCIs,Reduce(("sr_expression",1),3)),(LCGoesTo,Reduce(("sr_expression",1),3)),
(LCBar,Reduce(("sr_expression",1),3)),(LCTill,Reduce(("sr_expression",1),3)),
(LCLoop,Reduce(("sr_expression",1),3))]]@slrp'a133;
val slrp'a131=[[(LCSemi,Reduce(("relation",3),4)),
(LCRRange,Reduce(("relation",3),4)),(LCDigits,Reduce(("relation",3),4)),
(LCDelta,Reduce(("relation",3),4)),(LCBecomes,Reduce(("relation",3),4)),
(LCComma,Reduce(("relation",3),4)),(LCCrd,Reduce(("relation",3),4)),
(LCAnd,Reduce(("relation",3),4)),(LCOr,Reduce(("relation",3),4)),
(LCXor,Reduce(("relation",3),4)),(LCThen,Reduce(("relation",3),4)),
(LCIs,Reduce(("relation",3),4)),(LCGoesTo,Reduce(("relation",3),4)),
(LCBar,Reduce(("relation",3),4)),(LCTill,Reduce(("relation",3),4)),
(LCLoop,Reduce(("relation",3),4))]]@slrp'a132;
val slrp'a130=[[(LCIn,Shift 631)]]@slrp'a131;
val slrp'a129=[[(LCSemi,Reduce(("relation",2),3)),
(LCRRange,Reduce(("relation",2),3)),(LCDigits,Reduce(("relation",2),3)),
(LCDelta,Reduce(("relation",2),3)),(LCBecomes,Reduce(("relation",2),3)),
(LCComma,Reduce(("relation",2),3)),(LCCrd,Reduce(("relation",2),3)),
(LCAnd,Reduce(("relation",2),3)),(LCOr,Reduce(("relation",2),3)),
(LCXor,Reduce(("relation",2),3)),(LCThen,Reduce(("relation",2),3)),
(LCIs,Reduce(("relation",2),3)),(LCGoesTo,Reduce(("relation",2),3)),
(LCBar,Reduce(("relation",2),3)),(LCTill,Reduce(("relation",2),3)),
(LCLoop,Reduce(("relation",2),3))]]@slrp'a130;
val slrp'a128=[[(LCSemi,Reduce(("relation",1),3)),
(LCRRange,Reduce(("relation",1),3)),(LCDigits,Reduce(("relation",1),3)),
(LCDelta,Reduce(("relation",1),3)),(LCBecomes,Reduce(("relation",1),3)),
(LCComma,Reduce(("relation",1),3)),(LCCrd,Reduce(("relation",1),3)),
(LCAnd,Reduce(("relation",1),3)),(LCOr,Reduce(("relation",1),3)),
(LCXor,Reduce(("relation",1),3)),(LCThen,Reduce(("relation",1),3)),
(LCIs,Reduce(("relation",1),3)),(LCGoesTo,Reduce(("relation",1),3)),
(LCBar,Reduce(("relation",1),3)),(LCTill,Reduce(("relation",1),3)),
(LCLoop,Reduce(("relation",1),3))]]@slrp'a129;
val slrp'a127=[[(LCSemi,Reduce(("xor_relation",1),3)),
(LCRRange,Reduce(("xor_relation",1),3)),(LCDigits,Reduce(("xor_relation",1),3)),
(LCDelta,Reduce(("xor_relation",1),3)),(LCBecomes,Reduce(("xor_relation",1),3)),
(LCComma,Reduce(("xor_relation",1),3)),(LCCrd,Reduce(("xor_relation",1),3)),
(LCThen,Reduce(("xor_relation",1),3)),(LCIs,Reduce(("xor_relation",1),3)),
(LCGoesTo,Reduce(("xor_relation",1),3)),(LCBar,Reduce(("xor_relation",1),3)),
(LCTill,Reduce(("xor_relation",1),3)),(LCLoop,Reduce(("xor_relation",1),3))]]@slrp'a128;
val slrp'a126=[[(LCSemi,Reduce(("or_else_relation",1),4)),
(LCRRange,Reduce(("or_else_relation",1),4)),
(LCDigits,Reduce(("or_else_relation",1),4)),
(LCDelta,Reduce(("or_else_relation",1),4)),
(LCBecomes,Reduce(("or_else_relation",1),4)),
(LCComma,Reduce(("or_else_relation",1),4)),
(LCCrd,Reduce(("or_else_relation",1),4)),
(LCThen,Reduce(("or_else_relation",1),4)),
(LCIs,Reduce(("or_else_relation",1),4)),
(LCGoesTo,Reduce(("or_else_relation",1),4)),
(LCBar,Reduce(("or_else_relation",1),4)),
(LCTill,Reduce(("or_else_relation",1),4)),
(LCLoop,Reduce(("or_else_relation",1),4))]]@slrp'a127;
val slrp'a125=[[(LCSemi,Reduce(("or_relation",1),3)),
(LCRRange,Reduce(("or_relation",1),3)),(LCDigits,Reduce(("or_relation",1),3)),
(LCDelta,Reduce(("or_relation",1),3)),(LCBecomes,Reduce(("or_relation",1),3)),
(LCComma,Reduce(("or_relation",1),3)),(LCCrd,Reduce(("or_relation",1),3)),
(LCThen,Reduce(("or_relation",1),3)),(LCIs,Reduce(("or_relation",1),3)),
(LCGoesTo,Reduce(("or_relation",1),3)),(LCBar,Reduce(("or_relation",1),3)),
(LCTill,Reduce(("or_relation",1),3)),(LCLoop,Reduce(("or_relation",1),3))]]@slrp'a126;
val slrp'a124=[[(LCSemi,Reduce(("and_then_relation",1),4)),
(LCRRange,Reduce(("and_then_relation",1),4)),
(LCDigits,Reduce(("and_then_relation",1),4)),
(LCDelta,Reduce(("and_then_relation",1),4)),
(LCBecomes,Reduce(("and_then_relation",1),4)),
(LCComma,Reduce(("and_then_relation",1),4)),
(LCCrd,Reduce(("and_then_relation",1),4)),
(LCThen,Reduce(("and_then_relation",1),4)),
(LCIs,Reduce(("and_then_relation",1),4)),
(LCGoesTo,Reduce(("and_then_relation",1),4)),
(LCBar,Reduce(("and_then_relation",1),4)),
(LCTill,Reduce(("and_then_relation",1),4)),
(LCLoop,Reduce(("and_then_relation",1),4))]]@slrp'a125;
val slrp'a123=[[(LCSemi,Reduce(("and_relation",1),3)),
(LCRRange,Reduce(("and_relation",1),3)),(LCDigits,Reduce(("and_relation",1),3)),
(LCDelta,Reduce(("and_relation",1),3)),(LCBecomes,Reduce(("and_relation",1),3)),
(LCComma,Reduce(("and_relation",1),3)),(LCCrd,Reduce(("and_relation",1),3)),
(LCThen,Reduce(("and_relation",1),3)),(LCIs,Reduce(("and_relation",1),3)),
(LCGoesTo,Reduce(("and_relation",1),3)),(LCBar,Reduce(("and_relation",1),3)),
(LCTill,Reduce(("and_relation",1),3)),(LCLoop,Reduce(("and_relation",1),3))]]@slrp'a124;
val slrp'a122=[[(LCSemi,Reduce(("expression",5),2)),
(LCRRange,Reduce(("expression",5),2)),(LCDigits,Reduce(("expression",5),2)),
(LCDelta,Reduce(("expression",5),2)),(LCBecomes,Reduce(("expression",5),2)),
(LCComma,Reduce(("expression",5),2)),(LCCrd,Reduce(("expression",5),2)),
(LCThen,Reduce(("expression",5),2)),(LCIs,Reduce(("expression",5),2)),
(LCGoesTo,Reduce(("expression",5),2)),(LCBar,Reduce(("expression",5),2)),
(LCTill,Reduce(("expression",5),2)),(LCLoop,Reduce(("expression",5),2))]]@slrp'a123;
val slrp'a121=[[(LCSemi,Reduce(("expression",4),2)),
(LCRRange,Reduce(("expression",4),2)),(LCDigits,Reduce(("expression",4),2)),
(LCDelta,Reduce(("expression",4),2)),(LCBecomes,Reduce(("expression",4),2)),
(LCComma,Reduce(("expression",4),2)),(LCCrd,Reduce(("expression",4),2)),
(LCThen,Reduce(("expression",4),2)),(LCIs,Reduce(("expression",4),2)),
(LCGoesTo,Reduce(("expression",4),2)),(LCBar,Reduce(("expression",4),2)),
(LCTill,Reduce(("expression",4),2)),(LCLoop,Reduce(("expression",4),2))]]@slrp'a122;
val slrp'a120=[[(LCSemi,Reduce(("expression",3),2)),
(LCRRange,Reduce(("expression",3),2)),(LCDigits,Reduce(("expression",3),2)),
(LCDelta,Reduce(("expression",3),2)),(LCBecomes,Reduce(("expression",3),2)),
(LCComma,Reduce(("expression",3),2)),(LCCrd,Reduce(("expression",3),2)),
(LCThen,Reduce(("expression",3),2)),(LCIs,Reduce(("expression",3),2)),
(LCGoesTo,Reduce(("expression",3),2)),(LCBar,Reduce(("expression",3),2)),
(LCTill,Reduce(("expression",3),2)),(LCLoop,Reduce(("expression",3),2))]]@slrp'a121;
val slrp'a119=[[(LCSemi,Reduce(("expression",2),2)),
(LCRRange,Reduce(("expression",2),2)),(LCDigits,Reduce(("expression",2),2)),
(LCDelta,Reduce(("expression",2),2)),(LCBecomes,Reduce(("expression",2),2)),
(LCComma,Reduce(("expression",2),2)),(LCCrd,Reduce(("expression",2),2)),
(LCThen,Reduce(("expression",2),2)),(LCIs,Reduce(("expression",2),2)),
(LCGoesTo,Reduce(("expression",2),2)),(LCBar,Reduce(("expression",2),2)),
(LCTill,Reduce(("expression",2),2)),(LCLoop,Reduce(("expression",2),2))]]@slrp'a120;
val slrp'a118=[[(LCSemi,Reduce(("expression",1),2)),
(LCRRange,Reduce(("expression",1),2)),(LCDigits,Reduce(("expression",1),2)),
(LCDelta,Reduce(("expression",1),2)),(LCBecomes,Reduce(("expression",1),2)),
(LCComma,Reduce(("expression",1),2)),(LCCrd,Reduce(("expression",1),2)),
(LCThen,Reduce(("expression",1),2)),(LCIs,Reduce(("expression",1),2)),
(LCGoesTo,Reduce(("expression",1),2)),(LCBar,Reduce(("expression",1),2)),
(LCTill,Reduce(("expression",1),2)),(LCLoop,Reduce(("expression",1),2))]]@slrp'a119;
val slrp'a117=[[(LCCrd,Reduce(("positional_association",1),5))]]@slrp'a118;
val slrp'a116=[[(LCGoesTo,Shift 644)]]@slrp'a117;
val slrp'a115=[[(LCGoesTo,Reduce(("aggregate_choice",0),1)),
(LCBar,Reduce(("aggregate_choice",0),1))]]@slrp'a116;
val slrp'a114=[[(LCGoesTo,Reduce(("aggregate_choice_list",2),3))]]@slrp'a115;
val slrp'a113=[[(LCGoesTo,Reduce(("aggregate_choice_list",0),1))]]@slrp'a114;
val slrp'a112=[[(LCCrd,Reduce(("named_association",1),5))]]@slrp'a113;
val slrp'a111=[[(LCCrd,Reduce(("component_association",1),1))]]@slrp'a112;
val slrp'a110=[[(LCCrd,Reduce(("component_association",0),1))]]@slrp'a111;
val slrp'a109=[[(LCBecomes,Reduce(("aggregate",0),3)),
(LCSemi,Reduce(("aggregate",0),3)),(LCRRange,Reduce(("aggregate",0),3)),
(LCDigits,Reduce(("aggregate",0),3)),(LCDelta,Reduce(("aggregate",0),3)),
(LCComma,Reduce(("aggregate",0),3)),(LCCrd,Reduce(("aggregate",0),3)),
(LCAnd,Reduce(("aggregate",0),3)),(LCOr,Reduce(("aggregate",0),3)),
(LCXor,Reduce(("aggregate",0),3)),(LCGreaterEquals,Reduce(("aggregate",0),3)),
(LCGreaterThan,Reduce(("aggregate",0),3)),
(LCLessEquals,Reduce(("aggregate",0),3)),(LCLessThan,Reduce(("aggregate",0),3)),
(LCNotEquals,Reduce(("aggregate",0),3)),(LCEquals,Reduce(("aggregate",0),3)),
(LCIn,Reduce(("aggregate",0),3)),(LCNot,Reduce(("aggregate",0),3)),
(LCDotDot,Reduce(("aggregate",0),3)),(LCAmpersand,Reduce(("aggregate",0),3)),
(LCMinus,Reduce(("aggregate",0),3)),(LCPlus,Reduce(("aggregate",0),3)),
(LCRem,Reduce(("aggregate",0),3)),(LCMod,Reduce(("aggregate",0),3)),
(LCDiv,Reduce(("aggregate",0),3)),(LCStar,Reduce(("aggregate",0),3)),
(LCStarStar,Reduce(("aggregate",0),3)),(LCThen,Reduce(("aggregate",0),3)),
(LCIs,Reduce(("aggregate",0),3)),(LCGoesTo,Reduce(("aggregate",0),3)),
(LCBar,Reduce(("aggregate",0),3)),(LCTill,Reduce(("aggregate",0),3)),
(LCLoop,Reduce(("aggregate",0),3))]]@slrp'a110;
val slrp'a108=[[(LCCrd,Shift 108)]]@slrp'a109;
val slrp'a107=[[(LCUse,Reduce(("attribute",1),2)),
(LCOrd,Reduce(("attribute",1),2)),(LCDot,Reduce(("attribute",1),2)),
(LCPrimedIdentifier,Reduce(("attribute",1),2)),
(LCBecomes,Reduce(("attribute",1),2)),(LCSemi,Reduce(("attribute",1),2)),
(LCRRange,Reduce(("attribute",1),2)),(LCDigits,Reduce(("attribute",1),2)),
(LCDelta,Reduce(("attribute",1),2)),(LCComma,Reduce(("attribute",1),2)),
(LCCrd,Reduce(("attribute",1),2)),(LCAnd,Reduce(("attribute",1),2)),
(LCOr,Reduce(("attribute",1),2)),(LCXor,Reduce(("attribute",1),2)),
(LCGreaterEquals,Reduce(("attribute",1),2)),
(LCGreaterThan,Reduce(("attribute",1),2)),
(LCLessEquals,Reduce(("attribute",1),2)),(LCLessThan,Reduce(("attribute",1),2)),
(LCNotEquals,Reduce(("attribute",1),2)),(LCEquals,Reduce(("attribute",1),2)),
(LCIn,Reduce(("attribute",1),2)),(LCNot,Reduce(("attribute",1),2)),
(LCDotDot,Reduce(("attribute",1),2)),(LCAmpersand,Reduce(("attribute",1),2)),
(LCMinus,Reduce(("attribute",1),2)),(LCPlus,Reduce(("attribute",1),2)),
(LCRem,Reduce(("attribute",1),2)),(LCMod,Reduce(("attribute",1),2)),
(LCDiv,Reduce(("attribute",1),2)),(LCStar,Reduce(("attribute",1),2)),
(LCStarStar,Reduce(("attribute",1),2)),(LCPrime,Reduce(("attribute",1),2)),
(LCGrkXi,Reduce(("attribute",1),2)),(LCRenames,Reduce(("attribute",1),2)),
(LCAt,Reduce(("attribute",1),2)),(LCThen,Reduce(("attribute",1),2)),
(LCIs,Reduce(("attribute",1),2)),(LCGoesTo,Reduce(("attribute",1),2)),
(LCBar,Reduce(("attribute",1),2)),(LCTill,Reduce(("attribute",1),2)),
(LCLoop,Reduce(("attribute",1),2)),(LCAnnotation,Reduce(("attribute",1),2)),
(LCLChevron,Reduce(("attribute",1),2))]]@slrp'a108;
val slrp'a106=[[(LCPrimedIdentifier,Shift 106)]]@slrp'a107;
val slrp'a105=[[(LCUse,Reduce(("attribute",0),2)),
(LCOrd,Reduce(("attribute",0),2)),(LCDot,Reduce(("attribute",0),2)),
(LCPrimedIdentifier,Reduce(("attribute",0),2)),
(LCBecomes,Reduce(("attribute",0),2)),(LCSemi,Reduce(("attribute",0),2)),
(LCRRange,Reduce(("attribute",0),2)),(LCDigits,Reduce(("attribute",0),2)),
(LCDelta,Reduce(("attribute",0),2)),(LCComma,Reduce(("attribute",0),2)),
(LCCrd,Reduce(("attribute",0),2)),(LCAnd,Reduce(("attribute",0),2)),
(LCOr,Reduce(("attribute",0),2)),(LCXor,Reduce(("attribute",0),2)),
(LCGreaterEquals,Reduce(("attribute",0),2)),
(LCGreaterThan,Reduce(("attribute",0),2)),
(LCLessEquals,Reduce(("attribute",0),2)),(LCLessThan,Reduce(("attribute",0),2)),
(LCNotEquals,Reduce(("attribute",0),2)),(LCEquals,Reduce(("attribute",0),2)),
(LCIn,Reduce(("attribute",0),2)),(LCNot,Reduce(("attribute",0),2)),
(LCDotDot,Reduce(("attribute",0),2)),(LCAmpersand,Reduce(("attribute",0),2)),
(LCMinus,Reduce(("attribute",0),2)),(LCPlus,Reduce(("attribute",0),2)),
(LCRem,Reduce(("attribute",0),2)),(LCMod,Reduce(("attribute",0),2)),
(LCDiv,Reduce(("attribute",0),2)),(LCStar,Reduce(("attribute",0),2)),
(LCStarStar,Reduce(("attribute",0),2)),(LCPrime,Reduce(("attribute",0),2)),
(LCGrkXi,Reduce(("attribute",0),2)),(LCRenames,Reduce(("attribute",0),2)),
(LCAt,Reduce(("attribute",0),2)),(LCThen,Reduce(("attribute",0),2)),
(LCIs,Reduce(("attribute",0),2)),(LCGoesTo,Reduce(("attribute",0),2)),
(LCBar,Reduce(("attribute",0),2)),(LCTill,Reduce(("attribute",0),2)),
(LCLoop,Reduce(("attribute",0),2)),(LCAnnotation,Reduce(("attribute",0),2)),
(LCLChevron,Reduce(("attribute",0),2))]]@slrp'a106;
val slrp'a104=[[(LCComma,Reduce(("expression_list",1),3)),
(LCCrd,Reduce(("expression_list",1),3))]]@slrp'a105;
val slrp'a103=[[(LCOrd,Reduce(("selector",0),1)),
(LCDot,Reduce(("selector",0),1)),(LCPrimedIdentifier,Reduce(("selector",0),1)),
(LCBecomes,Reduce(("selector",0),1)),(LCSemi,Reduce(("selector",0),1)),
(LCRRange,Reduce(("selector",0),1)),(LCDigits,Reduce(("selector",0),1)),
(LCDelta,Reduce(("selector",0),1)),(LCComma,Reduce(("selector",0),1)),
(LCCrd,Reduce(("selector",0),1)),(LCAnd,Reduce(("selector",0),1)),
(LCOr,Reduce(("selector",0),1)),(LCXor,Reduce(("selector",0),1)),
(LCGreaterEquals,Reduce(("selector",0),1)),
(LCGreaterThan,Reduce(("selector",0),1)),(LCLessEquals,Reduce(("selector",0),1))
,(LCLessThan,Reduce(("selector",0),1)),(LCNotEquals,Reduce(("selector",0),1)),
(LCEquals,Reduce(("selector",0),1)),(LCIn,Reduce(("selector",0),1)),
(LCNot,Reduce(("selector",0),1)),(LCDotDot,Reduce(("selector",0),1)),
(LCAmpersand,Reduce(("selector",0),1)),(LCMinus,Reduce(("selector",0),1)),
(LCPlus,Reduce(("selector",0),1)),(LCRem,Reduce(("selector",0),1)),
(LCMod,Reduce(("selector",0),1)),(LCDiv,Reduce(("selector",0),1)),
(LCStar,Reduce(("selector",0),1)),(LCStarStar,Reduce(("selector",0),1)),
(LCPrime,Reduce(("selector",0),1)),(LCGrkXi,Reduce(("selector",0),1)),
(LCRenames,Reduce(("selector",0),1)),(LCAt,Reduce(("selector",0),1)),
(LCThen,Reduce(("selector",0),1)),(LCIs,Reduce(("selector",0),1)),
(LCGoesTo,Reduce(("selector",0),1)),(LCBar,Reduce(("selector",0),1)),
(LCTill,Reduce(("selector",0),1)),(LCLoop,Reduce(("selector",0),1)),
(LCAnnotation,Reduce(("selector",0),1)),(LCLChevron,Reduce(("selector",0),1))]]@slrp'a104;
val slrp'a102=[[(LCOrd,Reduce(("selected_component",0),3)),
(LCDot,Reduce(("selected_component",0),3)),
(LCPrimedIdentifier,Reduce(("selected_component",0),3)),
(LCBecomes,Reduce(("selected_component",0),3)),
(LCSemi,Reduce(("selected_component",0),3)),
(LCRRange,Reduce(("selected_component",0),3)),
(LCDigits,Reduce(("selected_component",0),3)),
(LCDelta,Reduce(("selected_component",0),3)),
(LCComma,Reduce(("selected_component",0),3)),
(LCCrd,Reduce(("selected_component",0),3)),
(LCAnd,Reduce(("selected_component",0),3)),
(LCOr,Reduce(("selected_component",0),3)),
(LCXor,Reduce(("selected_component",0),3)),
(LCGreaterEquals,Reduce(("selected_component",0),3)),
(LCGreaterThan,Reduce(("selected_component",0),3)),
(LCLessEquals,Reduce(("selected_component",0),3)),
(LCLessThan,Reduce(("selected_component",0),3)),
(LCNotEquals,Reduce(("selected_component",0),3)),
(LCEquals,Reduce(("selected_component",0),3)),
(LCIn,Reduce(("selected_component",0),3)),
(LCNot,Reduce(("selected_component",0),3)),
(LCDotDot,Reduce(("selected_component",0),3)),
(LCAmpersand,Reduce(("selected_component",0),3)),
(LCMinus,Reduce(("selected_component",0),3)),
(LCPlus,Reduce(("selected_component",0),3)),
(LCRem,Reduce(("selected_component",0),3)),
(LCMod,Reduce(("selected_component",0),3)),
(LCDiv,Reduce(("selected_component",0),3)),
(LCStar,Reduce(("selected_component",0),3)),
(LCStarStar,Reduce(("selected_component",0),3)),
(LCPrime,Reduce(("selected_component",0),3)),
(LCGrkXi,Reduce(("selected_component",0),3)),
(LCRenames,Reduce(("selected_component",0),3)),
(LCAt,Reduce(("selected_component",0),3)),
(LCThen,Reduce(("selected_component",0),3)),
(LCIs,Reduce(("selected_component",0),3)),
(LCGoesTo,Reduce(("selected_component",0),3)),
(LCBar,Reduce(("selected_component",0),3)),
(LCTill,Reduce(("selected_component",0),3)),
(LCLoop,Reduce(("selected_component",0),3)),
(LCAnnotation,Reduce(("selected_component",0),3)),
(LCLChevron,Reduce(("selected_component",0),3))]]@slrp'a103;
val slrp'a101=[[(LCOrd,Reduce(("indexed_component",0),2)),
(LCDot,Reduce(("indexed_component",0),2)),
(LCPrimedIdentifier,Reduce(("indexed_component",0),2)),
(LCBecomes,Reduce(("indexed_component",0),2)),
(LCSemi,Reduce(("indexed_component",0),2)),
(LCRRange,Reduce(("indexed_component",0),2)),
(LCDigits,Reduce(("indexed_component",0),2)),
(LCDelta,Reduce(("indexed_component",0),2)),
(LCComma,Reduce(("indexed_component",0),2)),
(LCCrd,Reduce(("indexed_component",0),2)),
(LCAnd,Reduce(("indexed_component",0),2)),
(LCOr,Reduce(("indexed_component",0),2)),
(LCXor,Reduce(("indexed_component",0),2)),
(LCGreaterEquals,Reduce(("indexed_component",0),2)),
(LCGreaterThan,Reduce(("indexed_component",0),2)),
(LCLessEquals,Reduce(("indexed_component",0),2)),
(LCLessThan,Reduce(("indexed_component",0),2)),
(LCNotEquals,Reduce(("indexed_component",0),2)),
(LCEquals,Reduce(("indexed_component",0),2)),
(LCIn,Reduce(("indexed_component",0),2)),
(LCNot,Reduce(("indexed_component",0),2)),
(LCDotDot,Reduce(("indexed_component",0),2)),
(LCAmpersand,Reduce(("indexed_component",0),2)),
(LCMinus,Reduce(("indexed_component",0),2)),
(LCPlus,Reduce(("indexed_component",0),2)),
(LCRem,Reduce(("indexed_component",0),2)),
(LCMod,Reduce(("indexed_component",0),2)),
(LCDiv,Reduce(("indexed_component",0),2)),
(LCStar,Reduce(("indexed_component",0),2)),
(LCStarStar,Reduce(("indexed_component",0),2)),
(LCPrime,Reduce(("indexed_component",0),2)),
(LCGrkXi,Reduce(("indexed_component",0),2)),
(LCRenames,Reduce(("indexed_component",0),2)),
(LCAt,Reduce(("indexed_component",0),2)),
(LCThen,Reduce(("indexed_component",0),2)),
(LCIs,Reduce(("indexed_component",0),2)),
(LCGoesTo,Reduce(("indexed_component",0),2)),
(LCBar,Reduce(("indexed_component",0),2)),
(LCTill,Reduce(("indexed_component",0),2)),
(LCLoop,Reduce(("indexed_component",0),2)),
(LCAnnotation,Reduce(("indexed_component",0),2)),
(LCLChevron,Reduce(("indexed_component",0),2))]]@slrp'a102;
val slrp'a100=[[(LCOrd,Reduce(("name",3),1)),(LCDot,Reduce(("name",3),1)),
(LCPrimedIdentifier,Reduce(("name",3),1)),(LCBecomes,Reduce(("name",3),1)),
(LCSemi,Reduce(("name",3),1)),(LCRRange,Reduce(("name",3),1)),
(LCDigits,Reduce(("name",3),1)),(LCDelta,Reduce(("name",3),1)),
(LCComma,Reduce(("name",3),1)),(LCCrd,Reduce(("name",3),1)),
(LCAnd,Reduce(("name",3),1)),(LCOr,Reduce(("name",3),1)),
(LCXor,Reduce(("name",3),1)),(LCGreaterEquals,Reduce(("name",3),1)),
(LCGreaterThan,Reduce(("name",3),1)),(LCLessEquals,Reduce(("name",3),1)),
(LCLessThan,Reduce(("name",3),1)),(LCNotEquals,Reduce(("name",3),1)),
(LCEquals,Reduce(("name",3),1)),(LCIn,Reduce(("name",3),1)),
(LCNot,Reduce(("name",3),1)),(LCDotDot,Reduce(("name",3),1)),
(LCAmpersand,Reduce(("name",3),1)),(LCMinus,Reduce(("name",3),1)),
(LCPlus,Reduce(("name",3),1)),(LCRem,Reduce(("name",3),1)),
(LCMod,Reduce(("name",3),1)),(LCDiv,Reduce(("name",3),1)),
(LCStar,Reduce(("name",3),1)),(LCStarStar,Reduce(("name",3),1)),
(LCPrime,Reduce(("name",3),1)),(LCGrkXi,Reduce(("name",3),1)),
(LCRenames,Reduce(("name",3),1)),(LCAt,Reduce(("name",3),1)),
(LCThen,Reduce(("name",3),1)),(LCIs,Reduce(("name",3),1)),
(LCGoesTo,Reduce(("name",3),1)),(LCBar,Reduce(("name",3),1)),
(LCTill,Reduce(("name",3),1)),(LCLoop,Reduce(("name",3),1)),
(LCAnnotation,Reduce(("name",3),1)),(LCLChevron,Reduce(("name",3),1))]]@slrp'a101;
val slrp'a99=[[(LCOrd,Reduce(("name",2),1)),(LCDot,Reduce(("name",2),1)),
(LCPrimedIdentifier,Reduce(("name",2),1)),(LCBecomes,Reduce(("name",2),1)),
(LCSemi,Reduce(("name",2),1)),(LCRRange,Reduce(("name",2),1)),
(LCDigits,Reduce(("name",2),1)),(LCDelta,Reduce(("name",2),1)),
(LCComma,Reduce(("name",2),1)),(LCCrd,Reduce(("name",2),1)),
(LCAnd,Reduce(("name",2),1)),(LCOr,Reduce(("name",2),1)),
(LCXor,Reduce(("name",2),1)),(LCGreaterEquals,Reduce(("name",2),1)),
(LCGreaterThan,Reduce(("name",2),1)),(LCLessEquals,Reduce(("name",2),1)),
(LCLessThan,Reduce(("name",2),1)),(LCNotEquals,Reduce(("name",2),1)),
(LCEquals,Reduce(("name",2),1)),(LCIn,Reduce(("name",2),1)),
(LCNot,Reduce(("name",2),1)),(LCDotDot,Reduce(("name",2),1)),
(LCAmpersand,Reduce(("name",2),1)),(LCMinus,Reduce(("name",2),1)),
(LCPlus,Reduce(("name",2),1)),(LCRem,Reduce(("name",2),1)),
(LCMod,Reduce(("name",2),1)),(LCDiv,Reduce(("name",2),1)),
(LCStar,Reduce(("name",2),1)),(LCStarStar,Reduce(("name",2),1)),
(LCPrime,Reduce(("name",2),1)),(LCGrkXi,Reduce(("name",2),1)),
(LCRenames,Reduce(("name",2),1)),(LCAt,Reduce(("name",2),1)),
(LCThen,Reduce(("name",2),1)),(LCIs,Reduce(("name",2),1)),
(LCGoesTo,Reduce(("name",2),1)),(LCBar,Reduce(("name",2),1)),
(LCTill,Reduce(("name",2),1)),(LCLoop,Reduce(("name",2),1)),
(LCAnnotation,Reduce(("name",2),1)),(LCLChevron,Reduce(("name",2),1))]]@slrp'a100;
val slrp'a98=[[(LCOrd,Reduce(("name",1),1)),(LCDot,Reduce(("name",1),1)),
(LCPrimedIdentifier,Reduce(("name",1),1)),(LCBecomes,Reduce(("name",1),1)),
(LCSemi,Reduce(("name",1),1)),(LCRRange,Reduce(("name",1),1)),
(LCDigits,Reduce(("name",1),1)),(LCDelta,Reduce(("name",1),1)),
(LCComma,Reduce(("name",1),1)),(LCCrd,Reduce(("name",1),1)),
(LCAnd,Reduce(("name",1),1)),(LCOr,Reduce(("name",1),1)),
(LCXor,Reduce(("name",1),1)),(LCGreaterEquals,Reduce(("name",1),1)),
(LCGreaterThan,Reduce(("name",1),1)),(LCLessEquals,Reduce(("name",1),1)),
(LCLessThan,Reduce(("name",1),1)),(LCNotEquals,Reduce(("name",1),1)),
(LCEquals,Reduce(("name",1),1)),(LCIn,Reduce(("name",1),1)),
(LCNot,Reduce(("name",1),1)),(LCDotDot,Reduce(("name",1),1)),
(LCAmpersand,Reduce(("name",1),1)),(LCMinus,Reduce(("name",1),1)),
(LCPlus,Reduce(("name",1),1)),(LCRem,Reduce(("name",1),1)),
(LCMod,Reduce(("name",1),1)),(LCDiv,Reduce(("name",1),1)),
(LCStar,Reduce(("name",1),1)),(LCStarStar,Reduce(("name",1),1)),
(LCPrime,Reduce(("name",1),1)),(LCGrkXi,Reduce(("name",1),1)),
(LCRenames,Reduce(("name",1),1)),(LCAt,Reduce(("name",1),1)),
(LCThen,Reduce(("name",1),1)),(LCIs,Reduce(("name",1),1)),
(LCGoesTo,Reduce(("name",1),1)),(LCBar,Reduce(("name",1),1)),
(LCTill,Reduce(("name",1),1)),(LCLoop,Reduce(("name",1),1)),
(LCAnnotation,Reduce(("name",1),1)),(LCLChevron,Reduce(("name",1),1))]]@slrp'a99;
val slrp'a97=[[(LCOrd,Reduce(("name",0),1)),(LCDot,Reduce(("name",0),1)),
(LCPrimedIdentifier,Reduce(("name",0),1)),(LCBecomes,Reduce(("name",0),1)),
(LCSemi,Reduce(("name",0),1)),(LCRRange,Reduce(("name",0),1)),
(LCDigits,Reduce(("name",0),1)),(LCDelta,Reduce(("name",0),1)),
(LCComma,Reduce(("name",0),1)),(LCCrd,Reduce(("name",0),1)),
(LCAnd,Reduce(("name",0),1)),(LCOr,Reduce(("name",0),1)),
(LCXor,Reduce(("name",0),1)),(LCGreaterEquals,Reduce(("name",0),1)),
(LCGreaterThan,Reduce(("name",0),1)),(LCLessEquals,Reduce(("name",0),1)),
(LCLessThan,Reduce(("name",0),1)),(LCNotEquals,Reduce(("name",0),1)),
(LCEquals,Reduce(("name",0),1)),(LCIn,Reduce(("name",0),1)),
(LCNot,Reduce(("name",0),1)),(LCDotDot,Reduce(("name",0),1)),
(LCAmpersand,Reduce(("name",0),1)),(LCMinus,Reduce(("name",0),1)),
(LCPlus,Reduce(("name",0),1)),(LCRem,Reduce(("name",0),1)),
(LCMod,Reduce(("name",0),1)),(LCDiv,Reduce(("name",0),1)),
(LCStar,Reduce(("name",0),1)),(LCStarStar,Reduce(("name",0),1)),
(LCPrime,Reduce(("name",0),1)),(LCGrkXi,Reduce(("name",0),1)),
(LCRenames,Reduce(("name",0),1)),(LCAt,Reduce(("name",0),1)),
(LCThen,Reduce(("name",0),1)),(LCIs,Reduce(("name",0),1)),
(LCGoesTo,Reduce(("name",0),1)),(LCBar,Reduce(("name",0),1)),
(LCTill,Reduce(("name",0),1)),(LCLoop,Reduce(("name",0),1)),
(LCAnnotation,Reduce(("name",0),1)),(LCLChevron,Reduce(("name",0),1))]]@slrp'a98;
val slrp'a96=[[(LCEos,Reduce(("proper_body",0),1)),
(LCLChevron,Reduce(("proper_body",0),1)),
(LCFunction,Reduce(("proper_body",0),1)),(LCPackage,Reduce(("proper_body",0),1))
,(LCIdentifier,Reduce(("proper_body",0),1)),
(LCAnnotation,Reduce(("proper_body",0),1)),
(LCSubType,Reduce(("proper_body",0),1)),(LCPragma,Reduce(("proper_body",0),1)),
(LCProcedure,Reduce(("proper_body",0),1)),(LCType,Reduce(("proper_body",0),1)),
(LCFor,Reduce(("proper_body",0),1)),(LCUse,Reduce(("proper_body",0),1)),
(LCReferences,Reduce(("proper_body",0),1)),
(LCSeparate,Reduce(("proper_body",0),1)),(LCWith,Reduce(("proper_body",0),1)),
(LCBegin,Reduce(("proper_body",0),1)),(LCEnd,Reduce(("proper_body",0),1))]]@slrp'a97;
val slrp'a95=[[(LCEos,Reduce(("body",1),1)),(LCLChevron,Reduce(("body",1),1)),
(LCFunction,Reduce(("body",1),1)),(LCPackage,Reduce(("body",1),1)),
(LCIdentifier,Reduce(("body",1),1)),(LCAnnotation,Reduce(("body",1),1)),
(LCSubType,Reduce(("body",1),1)),(LCPragma,Reduce(("body",1),1)),
(LCProcedure,Reduce(("body",1),1)),(LCType,Reduce(("body",1),1)),
(LCFor,Reduce(("body",1),1)),(LCUse,Reduce(("body",1),1)),
(LCBegin,Reduce(("body",1),1)),(LCEnd,Reduce(("body",1),1))]]@slrp'a96;
val slrp'a94=[[(LCEos,Reduce(("body",0),1)),(LCLChevron,Reduce(("body",0),1)),
(LCFunction,Reduce(("body",0),1)),(LCPackage,Reduce(("body",0),1)),
(LCIdentifier,Reduce(("body",0),1)),(LCAnnotation,Reduce(("body",0),1)),
(LCSubType,Reduce(("body",0),1)),(LCPragma,Reduce(("body",0),1)),
(LCProcedure,Reduce(("body",0),1)),(LCType,Reduce(("body",0),1)),
(LCFor,Reduce(("body",0),1)),(LCUse,Reduce(("body",0),1)),
(LCBegin,Reduce(("body",0),1)),(LCEnd,Reduce(("body",0),1))]]@slrp'a95;
val slrp'a93=[[(LCEos,Reduce(("basic_declarative_item",2),1)),
(LCLChevron,Reduce(("basic_declarative_item",2),1)),
(LCFunction,Reduce(("basic_declarative_item",2),1)),
(LCPackage,Reduce(("basic_declarative_item",2),1)),
(LCIdentifier,Reduce(("basic_declarative_item",2),1)),
(LCAnnotation,Reduce(("basic_declarative_item",2),1)),
(LCSubType,Reduce(("basic_declarative_item",2),1)),
(LCPragma,Reduce(("basic_declarative_item",2),1)),
(LCProcedure,Reduce(("basic_declarative_item",2),1)),
(LCType,Reduce(("basic_declarative_item",2),1)),
(LCFor,Reduce(("basic_declarative_item",2),1)),
(LCUse,Reduce(("basic_declarative_item",2),1)),
(LCEnd,Reduce(("basic_declarative_item",2),1)),
(LCPrivate,Reduce(("basic_declarative_item",2),1)),
(LCAuxiliary,Reduce(("basic_declarative_item",2),1)),
(LCBegin,Reduce(("basic_declarative_item",2),1))]]@slrp'a94;
val slrp'a92=[[(LCEos,Reduce(("basic_declarative_item",1),1)),
(LCLChevron,Reduce(("basic_declarative_item",1),1)),
(LCFunction,Reduce(("basic_declarative_item",1),1)),
(LCPackage,Reduce(("basic_declarative_item",1),1)),
(LCIdentifier,Reduce(("basic_declarative_item",1),1)),
(LCAnnotation,Reduce(("basic_declarative_item",1),1)),
(LCSubType,Reduce(("basic_declarative_item",1),1)),
(LCPragma,Reduce(("basic_declarative_item",1),1)),
(LCProcedure,Reduce(("basic_declarative_item",1),1)),
(LCType,Reduce(("basic_declarative_item",1),1)),
(LCFor,Reduce(("basic_declarative_item",1),1)),
(LCUse,Reduce(("basic_declarative_item",1),1)),
(LCEnd,Reduce(("basic_declarative_item",1),1)),
(LCPrivate,Reduce(("basic_declarative_item",1),1)),
(LCAuxiliary,Reduce(("basic_declarative_item",1),1)),
(LCBegin,Reduce(("basic_declarative_item",1),1))]]@slrp'a93;
val slrp'a91=[[(LCEos,Reduce(("basic_declarative_item",0),1)),
(LCLChevron,Reduce(("basic_declarative_item",0),1)),
(LCFunction,Reduce(("basic_declarative_item",0),1)),
(LCPackage,Reduce(("basic_declarative_item",0),1)),
(LCIdentifier,Reduce(("basic_declarative_item",0),1)),
(LCAnnotation,Reduce(("basic_declarative_item",0),1)),
(LCSubType,Reduce(("basic_declarative_item",0),1)),
(LCPragma,Reduce(("basic_declarative_item",0),1)),
(LCProcedure,Reduce(("basic_declarative_item",0),1)),
(LCType,Reduce(("basic_declarative_item",0),1)),
(LCFor,Reduce(("basic_declarative_item",0),1)),
(LCUse,Reduce(("basic_declarative_item",0),1)),
(LCEnd,Reduce(("basic_declarative_item",0),1)),
(LCPrivate,Reduce(("basic_declarative_item",0),1)),
(LCAuxiliary,Reduce(("basic_declarative_item",0),1)),
(LCBegin,Reduce(("basic_declarative_item",0),1))]]@slrp'a92;
val slrp'a90=[[(LCEos,Reduce(("dec",4),1)),(LCLChevron,Reduce(("dec",4),1)),
(LCFunction,Reduce(("dec",4),1)),(LCPackage,Reduce(("dec",4),1)),
(LCIdentifier,Reduce(("dec",4),1)),(LCAnnotation,Reduce(("dec",4),1)),
(LCSubType,Reduce(("dec",4),1)),(LCPragma,Reduce(("dec",4),1)),
(LCProcedure,Reduce(("dec",4),1)),(LCType,Reduce(("dec",4),1)),
(LCFor,Reduce(("dec",4),1)),(LCUse,Reduce(("dec",4),1)),
(LCBegin,Reduce(("dec",4),1)),(LCEnd,Reduce(("dec",4),1))]]@slrp'a91;
val slrp'a89=[[(LCEos,Reduce(("dec",3),1)),(LCLChevron,Reduce(("dec",3),1)),
(LCFunction,Reduce(("dec",3),1)),(LCPackage,Reduce(("dec",3),1)),
(LCIdentifier,Reduce(("dec",3),1)),(LCAnnotation,Reduce(("dec",3),1)),
(LCSubType,Reduce(("dec",3),1)),(LCPragma,Reduce(("dec",3),1)),
(LCProcedure,Reduce(("dec",3),1)),(LCType,Reduce(("dec",3),1)),
(LCFor,Reduce(("dec",3),1)),(LCUse,Reduce(("dec",3),1)),
(LCBegin,Reduce(("dec",3),1)),(LCEnd,Reduce(("dec",3),1))]]@slrp'a90;
val slrp'a88=[[(LCEos,Reduce(("dec",2),1)),(LCLChevron,Reduce(("dec",2),1)),
(LCFunction,Reduce(("dec",2),1)),(LCPackage,Reduce(("dec",2),1)),
(LCIdentifier,Reduce(("dec",2),1)),(LCAnnotation,Reduce(("dec",2),1)),
(LCSubType,Reduce(("dec",2),1)),(LCPragma,Reduce(("dec",2),1)),
(LCProcedure,Reduce(("dec",2),1)),(LCType,Reduce(("dec",2),1)),
(LCFor,Reduce(("dec",2),1)),(LCUse,Reduce(("dec",2),1)),
(LCBegin,Reduce(("dec",2),1)),(LCEnd,Reduce(("dec",2),1))]]@slrp'a89;
val slrp'a87=[[(LCEos,Reduce(("dec",1),1)),(LCLChevron,Reduce(("dec",1),1)),
(LCFunction,Reduce(("dec",1),1)),(LCPackage,Reduce(("dec",1),1)),
(LCIdentifier,Reduce(("dec",1),1)),(LCAnnotation,Reduce(("dec",1),1)),
(LCSubType,Reduce(("dec",1),1)),(LCPragma,Reduce(("dec",1),1)),
(LCProcedure,Reduce(("dec",1),1)),(LCType,Reduce(("dec",1),1)),
(LCFor,Reduce(("dec",1),1)),(LCUse,Reduce(("dec",1),1)),
(LCBegin,Reduce(("dec",1),1)),(LCEnd,Reduce(("dec",1),1))]]@slrp'a88;
val slrp'a86=[[(LCEos,Reduce(("dec",0),1)),(LCLChevron,Reduce(("dec",0),1)),
(LCFunction,Reduce(("dec",0),1)),(LCPackage,Reduce(("dec",0),1)),
(LCIdentifier,Reduce(("dec",0),1)),(LCAnnotation,Reduce(("dec",0),1)),
(LCSubType,Reduce(("dec",0),1)),(LCPragma,Reduce(("dec",0),1)),
(LCProcedure,Reduce(("dec",0),1)),(LCType,Reduce(("dec",0),1)),
(LCFor,Reduce(("dec",0),1)),(LCUse,Reduce(("dec",0),1)),
(LCBegin,Reduce(("dec",0),1)),(LCEnd,Reduce(("dec",0),1))]]@slrp'a87;
val slrp'a85=[[(LCEos,Reduce(("declarative_part",1),2)),
(LCBegin,Reduce(("declarative_part",1),2)),
(LCEnd,Reduce(("declarative_part",1),2))]]@slrp'a86;
val slrp'a84=[[(LCEnd,Reduce(("component_declaration",0),4)),
(LCIdentifier,Reduce(("component_declaration",0),4))]]@slrp'a85;
val slrp'a83=[[(LCSemi,Shift 83)]]@slrp'a84;
val slrp'a82=[[(LCColon,Shift 660)]]@slrp'a83;
val slrp'a81=[[(LCEnd,Reduce(("component_list",1),2))]]@slrp'a82;
val slrp'a80=[[(LCSemi,Reduce(("record_type_definition",0),4))]]@slrp'a81;
val slrp'a79=[[(LCRecord,Shift 79)]]@slrp'a80;
val slrp'a78=[[(LCEnd,Shift 78)]]@slrp'a79;
val slrp'a77=[[(LCGoesTo,Reduce(("discrete_range",0),2)),
(LCBar,Reduce(("discrete_range",0),2)),(LCTill,Reduce(("discrete_range",0),2)),
(LCLoop,Reduce(("discrete_range",0),2))]]@slrp'a78;
val slrp'a76=[[(LCCrd,Reduce(("discrete_range_list",1),3))]]@slrp'a77;
val slrp'a75=[[(LCOf,Reduce(("index_constraint",0),3))]]@slrp'a76;
val slrp'a74=[[(LCCrd,Shift 74)]]@slrp'a75;
val slrp'a73=[[(LCCrd,Reduce(("index_subtype_definition",0),3)),
(LCComma,Reduce(("index_subtype_definition",0),3))]]@slrp'a74;
val slrp'a72=[[(LCLessGreat,Shift 72)]]@slrp'a73;
val slrp'a71=[[(LCRRange,Shift 71)]]@slrp'a72;
val slrp'a70=[[(LCCrd,Reduce(("index_subtype_definition_list",1),3))]]@slrp'a71;
val slrp'a69=[[(LCSemi,Reduce(("constrained_array_definition",0),4))]]@slrp'a70;
val slrp'a68=[[(LCOf,Shift 659)]]@slrp'a69;
val slrp'a67=[[(LCSemi,Reduce(("unconstrained_array_definition",0),6))]]@slrp'a68;
val slrp'a66=[[(LCOf,Shift 658)]]@slrp'a67;
val slrp'a65=[[(LCCrd,Shift 65)]]@slrp'a66;
val slrp'a64=[[(LCSemi,Reduce(("array_type_definition",1),1))]]@slrp'a65;
val slrp'a63=[[(LCSemi,Reduce(("array_type_definition",0),1))]]@slrp'a64;
val slrp'a62=[[(LCBecomes,Reduce(("fixed_accuracy_definition",0),2)),
(LCSemi,Reduce(("fixed_accuracy_definition",0),2)),
(LCRRange,Reduce(("fixed_accuracy_definition",0),2)),
(LCCrd,Reduce(("fixed_accuracy_definition",0),2)),
(LCComma,Reduce(("fixed_accuracy_definition",0),2))]]@slrp'a63;
val slrp'a61=[[(LCBecomes,Reduce(("fixed_point_constraint",1),2)),
(LCSemi,Reduce(("fixed_point_constraint",1),2)),
(LCCrd,Reduce(("fixed_point_constraint",1),2)),
(LCComma,Reduce(("fixed_point_constraint",1),2))]]@slrp'a62;
val slrp'a60=[[(LCBecomes,Reduce(("floating_accuracy_definition",0),2)),
(LCSemi,Reduce(("floating_accuracy_definition",0),2)),
(LCRRange,Reduce(("floating_accuracy_definition",0),2)),
(LCCrd,Reduce(("floating_accuracy_definition",0),2)),
(LCComma,Reduce(("floating_accuracy_definition",0),2))]]@slrp'a61;
val slrp'a59=[[(LCBecomes,Reduce(("floating_point_constraint",1),2)),
(LCSemi,Reduce(("floating_point_constraint",1),2)),
(LCCrd,Reduce(("floating_point_constraint",1),2)),
(LCComma,Reduce(("floating_point_constraint",1),2))]]@slrp'a60;
val slrp'a58=[[(LCSemi,Reduce(("real_type_definition",1),1))]]@slrp'a59;
val slrp'a57=[[(LCSemi,Reduce(("real_type_definition",0),1))]]@slrp'a58;
val slrp'a56=[[(LCSemi,Reduce(("integer_type_definition",0),1))]]@slrp'a57;
val slrp'a55=[[(LCSemi,Reduce(("enumeration_type_definition",0),3))]]@slrp'a56;
val slrp'a54=[[(LCCrd,Shift 54)]]@slrp'a55;
val slrp'a53=[[(LCBecomes,Reduce(("range_constraint",0),2)),
(LCSemi,Reduce(("range_constraint",0),2)),
(LCCrd,Reduce(("range_constraint",0),2)),
(LCComma,Reduce(("range_constraint",0),2)),
(LCGoesTo,Reduce(("range_constraint",0),2)),
(LCBar,Reduce(("range_constraint",0),2)),
(LCTill,Reduce(("range_constraint",0),2)),
(LCLoop,Reduce(("range_constraint",0),2))]]@slrp'a54;
val slrp'a52=[[(LCBecomes,Reduce(("constraint",2),1)),
(LCSemi,Reduce(("constraint",2),1)),(LCCrd,Reduce(("constraint",2),1)),
(LCComma,Reduce(("constraint",2),1))]]@slrp'a53;
val slrp'a51=[[(LCBecomes,Reduce(("constraint",1),1)),
(LCSemi,Reduce(("constraint",1),1)),(LCCrd,Reduce(("constraint",1),1)),
(LCComma,Reduce(("constraint",1),1))]]@slrp'a52;
val slrp'a50=[[(LCBecomes,Reduce(("constraint",0),1)),
(LCSemi,Reduce(("constraint",0),1)),(LCCrd,Reduce(("constraint",0),1)),
(LCComma,Reduce(("constraint",0),1))]]@slrp'a51;
val slrp'a49=[[(LCBecomes,Reduce(("subtype_indication",0),2)),
(LCSemi,Reduce(("subtype_indication",0),2)),
(LCCrd,Reduce(("subtype_indication",0),2)),
(LCComma,Reduce(("subtype_indication",0),2))]]@slrp'a50;
val slrp'a48=[[(LCEos,Reduce(("subtype_declaration",0),5)),
(LCLChevron,Reduce(("subtype_declaration",0),5)),
(LCFunction,Reduce(("subtype_declaration",0),5)),
(LCPackage,Reduce(("subtype_declaration",0),5)),
(LCIdentifier,Reduce(("subtype_declaration",0),5)),
(LCAnnotation,Reduce(("subtype_declaration",0),5)),
(LCSubType,Reduce(("subtype_declaration",0),5)),
(LCPragma,Reduce(("subtype_declaration",0),5)),
(LCProcedure,Reduce(("subtype_declaration",0),5)),
(LCType,Reduce(("subtype_declaration",0),5)),
(LCFor,Reduce(("subtype_declaration",0),5)),
(LCUse,Reduce(("subtype_declaration",0),5)),
(LCImplement,Reduce(("subtype_declaration",0),5)),
(LCEnd,Reduce(("subtype_declaration",0),5)),
(LCPrivate,Reduce(("subtype_declaration",0),5)),
(LCAuxiliary,Reduce(("subtype_declaration",0),5)),
(LCBegin,Reduce(("subtype_declaration",0),5))]]@slrp'a49;
val slrp'a47=[[(LCSemi,Shift 47)]]@slrp'a48;
val slrp'a46=[[(LCIs,Shift 657)]]@slrp'a47;
val slrp'a45=[[(LCIdentifier,Shift 45)]]@slrp'a46;
val slrp'a44=[[(LCSemi,Reduce(("type_definition",4),1))]]@slrp'a45;
val slrp'a43=[[(LCSemi,Reduce(("type_definition",3),1))]]@slrp'a44;
val slrp'a42=[[(LCSemi,Reduce(("type_definition",2),1))]]@slrp'a43;
val slrp'a41=[[(LCSemi,Reduce(("type_definition",1),1))]]@slrp'a42;
val slrp'a40=[[(LCSemi,Reduce(("type_definition",0),1))]]@slrp'a41;
val slrp'a39=[[(LCEos,Reduce(("full_type_declaration",1),6)),
(LCLChevron,Reduce(("full_type_declaration",1),6)),
(LCFunction,Reduce(("full_type_declaration",1),6)),
(LCPackage,Reduce(("full_type_declaration",1),6)),
(LCIdentifier,Reduce(("full_type_declaration",1),6)),
(LCAnnotation,Reduce(("full_type_declaration",1),6)),
(LCSubType,Reduce(("full_type_declaration",1),6)),
(LCPragma,Reduce(("full_type_declaration",1),6)),
(LCProcedure,Reduce(("full_type_declaration",1),6)),
(LCType,Reduce(("full_type_declaration",1),6)),
(LCFor,Reduce(("full_type_declaration",1),6)),
(LCUse,Reduce(("full_type_declaration",1),6)),
(LCImplement,Reduce(("full_type_declaration",1),6)),
(LCEnd,Reduce(("full_type_declaration",1),6)),
(LCPrivate,Reduce(("full_type_declaration",1),6)),
(LCAuxiliary,Reduce(("full_type_declaration",1),6)),
(LCBegin,Reduce(("full_type_declaration",1),6))]]@slrp'a40;
val slrp'a38=[[(LCSemi,Shift 38)]]@slrp'a39;
val slrp'a37=[[(LCEos,Reduce(("full_type_declaration",0),5)),
(LCLChevron,Reduce(("full_type_declaration",0),5)),
(LCFunction,Reduce(("full_type_declaration",0),5)),
(LCPackage,Reduce(("full_type_declaration",0),5)),
(LCIdentifier,Reduce(("full_type_declaration",0),5)),
(LCAnnotation,Reduce(("full_type_declaration",0),5)),
(LCSubType,Reduce(("full_type_declaration",0),5)),
(LCPragma,Reduce(("full_type_declaration",0),5)),
(LCProcedure,Reduce(("full_type_declaration",0),5)),
(LCType,Reduce(("full_type_declaration",0),5)),
(LCFor,Reduce(("full_type_declaration",0),5)),
(LCUse,Reduce(("full_type_declaration",0),5)),
(LCImplement,Reduce(("full_type_declaration",0),5)),
(LCEnd,Reduce(("full_type_declaration",0),5)),
(LCPrivate,Reduce(("full_type_declaration",0),5)),
(LCAuxiliary,Reduce(("full_type_declaration",0),5)),
(LCBegin,Reduce(("full_type_declaration",0),5))]]@slrp'a38;
val slrp'a36=[[(LCSemi,Shift 36)]]@slrp'a37;
val slrp'a35=[[(LCEos,Reduce(("type_declaration",1),1)),
(LCLChevron,Reduce(("type_declaration",1),1)),
(LCFunction,Reduce(("type_declaration",1),1)),
(LCPackage,Reduce(("type_declaration",1),1)),
(LCIdentifier,Reduce(("type_declaration",1),1)),
(LCAnnotation,Reduce(("type_declaration",1),1)),
(LCSubType,Reduce(("type_declaration",1),1)),
(LCPragma,Reduce(("type_declaration",1),1)),
(LCProcedure,Reduce(("type_declaration",1),1)),
(LCType,Reduce(("type_declaration",1),1)),
(LCFor,Reduce(("type_declaration",1),1)),
(LCUse,Reduce(("type_declaration",1),1)),
(LCImplement,Reduce(("type_declaration",1),1)),
(LCEnd,Reduce(("type_declaration",1),1)),
(LCPrivate,Reduce(("type_declaration",1),1)),
(LCAuxiliary,Reduce(("type_declaration",1),1)),
(LCBegin,Reduce(("type_declaration",1),1))]]@slrp'a36;
val slrp'a34=[[(LCEos,Reduce(("type_declaration",0),1)),
(LCLChevron,Reduce(("type_declaration",0),1)),
(LCFunction,Reduce(("type_declaration",0),1)),
(LCPackage,Reduce(("type_declaration",0),1)),
(LCIdentifier,Reduce(("type_declaration",0),1)),
(LCAnnotation,Reduce(("type_declaration",0),1)),
(LCSubType,Reduce(("type_declaration",0),1)),
(LCPragma,Reduce(("type_declaration",0),1)),
(LCProcedure,Reduce(("type_declaration",0),1)),
(LCType,Reduce(("type_declaration",0),1)),
(LCFor,Reduce(("type_declaration",0),1)),
(LCUse,Reduce(("type_declaration",0),1)),
(LCImplement,Reduce(("type_declaration",0),1)),
(LCEnd,Reduce(("type_declaration",0),1)),
(LCPrivate,Reduce(("type_declaration",0),1)),
(LCAuxiliary,Reduce(("type_declaration",0),1)),
(LCBegin,Reduce(("type_declaration",0),1))]]@slrp'a35;
val slrp'a33=[[(LCColon,Reduce(("identifier_list",1),3)),
(LCCrd,Reduce(("identifier_list",1),3)),(LCSemi,Reduce(("identifier_list",1),3))
]]@slrp'a34;
val slrp'a32=[[(LCEos,Reduce(("variable_declaration",1),6)),
(LCLChevron,Reduce(("variable_declaration",1),6)),
(LCFunction,Reduce(("variable_declaration",1),6)),
(LCPackage,Reduce(("variable_declaration",1),6)),
(LCIdentifier,Reduce(("variable_declaration",1),6)),
(LCAnnotation,Reduce(("variable_declaration",1),6)),
(LCSubType,Reduce(("variable_declaration",1),6)),
(LCPragma,Reduce(("variable_declaration",1),6)),
(LCProcedure,Reduce(("variable_declaration",1),6)),
(LCType,Reduce(("variable_declaration",1),6)),
(LCFor,Reduce(("variable_declaration",1),6)),
(LCUse,Reduce(("variable_declaration",1),6)),
(LCImplement,Reduce(("variable_declaration",1),6)),
(LCEnd,Reduce(("variable_declaration",1),6)),
(LCPrivate,Reduce(("variable_declaration",1),6)),
(LCAuxiliary,Reduce(("variable_declaration",1),6)),
(LCBegin,Reduce(("variable_declaration",1),6))]]@slrp'a33;
val slrp'a31=[[(LCSemi,Shift 31)]]@slrp'a32;
val slrp'a30=[[(LCEos,Reduce(("variable_declaration",0),4)),
(LCLChevron,Reduce(("variable_declaration",0),4)),
(LCFunction,Reduce(("variable_declaration",0),4)),
(LCPackage,Reduce(("variable_declaration",0),4)),
(LCIdentifier,Reduce(("variable_declaration",0),4)),
(LCAnnotation,Reduce(("variable_declaration",0),4)),
(LCSubType,Reduce(("variable_declaration",0),4)),
(LCPragma,Reduce(("variable_declaration",0),4)),
(LCProcedure,Reduce(("variable_declaration",0),4)),
(LCType,Reduce(("variable_declaration",0),4)),
(LCFor,Reduce(("variable_declaration",0),4)),
(LCUse,Reduce(("variable_declaration",0),4)),
(LCImplement,Reduce(("variable_declaration",0),4)),
(LCEnd,Reduce(("variable_declaration",0),4)),
(LCPrivate,Reduce(("variable_declaration",0),4)),
(LCAuxiliary,Reduce(("variable_declaration",0),4)),
(LCBegin,Reduce(("variable_declaration",0),4))]]@slrp'a31;
val slrp'a29=[[(LCEos,Reduce(("constant_declaration",1),6)),
(LCLChevron,Reduce(("constant_declaration",1),6)),
(LCFunction,Reduce(("constant_declaration",1),6)),
(LCPackage,Reduce(("constant_declaration",1),6)),
(LCIdentifier,Reduce(("constant_declaration",1),6)),
(LCAnnotation,Reduce(("constant_declaration",1),6)),
(LCSubType,Reduce(("constant_declaration",1),6)),
(LCPragma,Reduce(("constant_declaration",1),6)),
(LCProcedure,Reduce(("constant_declaration",1),6)),
(LCType,Reduce(("constant_declaration",1),6)),
(LCFor,Reduce(("constant_declaration",1),6)),
(LCUse,Reduce(("constant_declaration",1),6)),
(LCImplement,Reduce(("constant_declaration",1),6)),
(LCEnd,Reduce(("constant_declaration",1),6)),
(LCPrivate,Reduce(("constant_declaration",1),6)),
(LCAuxiliary,Reduce(("constant_declaration",1),6)),
(LCBegin,Reduce(("constant_declaration",1),6))]]@slrp'a30;
val slrp'a28=[[(LCSemi,Shift 28)]]@slrp'a29;
val slrp'a27=[[(LCEos,Reduce(("constant_declaration",0),7)),
(LCLChevron,Reduce(("constant_declaration",0),7)),
(LCFunction,Reduce(("constant_declaration",0),7)),
(LCPackage,Reduce(("constant_declaration",0),7)),
(LCIdentifier,Reduce(("constant_declaration",0),7)),
(LCAnnotation,Reduce(("constant_declaration",0),7)),
(LCSubType,Reduce(("constant_declaration",0),7)),
(LCPragma,Reduce(("constant_declaration",0),7)),
(LCProcedure,Reduce(("constant_declaration",0),7)),
(LCType,Reduce(("constant_declaration",0),7)),
(LCFor,Reduce(("constant_declaration",0),7)),
(LCUse,Reduce(("constant_declaration",0),7)),
(LCImplement,Reduce(("constant_declaration",0),7)),
(LCEnd,Reduce(("constant_declaration",0),7)),
(LCPrivate,Reduce(("constant_declaration",0),7)),
(LCAuxiliary,Reduce(("constant_declaration",0),7)),
(LCBegin,Reduce(("constant_declaration",0),7))]]@slrp'a28;
val slrp'a26=[[(LCSemi,Shift 26)]]@slrp'a27;
val slrp'a25=[[(LCEos,Reduce(("object_declaration",1),1)),
(LCLChevron,Reduce(("object_declaration",1),1)),
(LCFunction,Reduce(("object_declaration",1),1)),
(LCPackage,Reduce(("object_declaration",1),1)),
(LCIdentifier,Reduce(("object_declaration",1),1)),
(LCAnnotation,Reduce(("object_declaration",1),1)),
(LCSubType,Reduce(("object_declaration",1),1)),
(LCPragma,Reduce(("object_declaration",1),1)),
(LCProcedure,Reduce(("object_declaration",1),1)),
(LCType,Reduce(("object_declaration",1),1)),
(LCFor,Reduce(("object_declaration",1),1)),
(LCUse,Reduce(("object_declaration",1),1)),
(LCImplement,Reduce(("object_declaration",1),1)),
(LCEnd,Reduce(("object_declaration",1),1)),
(LCPrivate,Reduce(("object_declaration",1),1)),
(LCAuxiliary,Reduce(("object_declaration",1),1)),
(LCBegin,Reduce(("object_declaration",1),1))]]@slrp'a26;
val slrp'a24=[[(LCEos,Reduce(("object_declaration",0),1)),
(LCLChevron,Reduce(("object_declaration",0),1)),
(LCFunction,Reduce(("object_declaration",0),1)),
(LCPackage,Reduce(("object_declaration",0),1)),
(LCIdentifier,Reduce(("object_declaration",0),1)),
(LCAnnotation,Reduce(("object_declaration",0),1)),
(LCSubType,Reduce(("object_declaration",0),1)),
(LCPragma,Reduce(("object_declaration",0),1)),
(LCProcedure,Reduce(("object_declaration",0),1)),
(LCType,Reduce(("object_declaration",0),1)),
(LCFor,Reduce(("object_declaration",0),1)),
(LCUse,Reduce(("object_declaration",0),1)),
(LCImplement,Reduce(("object_declaration",0),1)),
(LCEnd,Reduce(("object_declaration",0),1)),
(LCPrivate,Reduce(("object_declaration",0),1)),
(LCAuxiliary,Reduce(("object_declaration",0),1)),
(LCBegin,Reduce(("object_declaration",0),1))]]@slrp'a25;
val slrp'a23=[[(LCEos,Reduce(("basic_declaration",5),1)),
(LCLChevron,Reduce(("basic_declaration",5),1)),
(LCFunction,Reduce(("basic_declaration",5),1)),
(LCPackage,Reduce(("basic_declaration",5),1)),
(LCIdentifier,Reduce(("basic_declaration",5),1)),
(LCAnnotation,Reduce(("basic_declaration",5),1)),
(LCSubType,Reduce(("basic_declaration",5),1)),
(LCPragma,Reduce(("basic_declaration",5),1)),
(LCProcedure,Reduce(("basic_declaration",5),1)),
(LCType,Reduce(("basic_declaration",5),1)),
(LCFor,Reduce(("basic_declaration",5),1)),
(LCUse,Reduce(("basic_declaration",5),1)),
(LCImplement,Reduce(("basic_declaration",5),1)),
(LCEnd,Reduce(("basic_declaration",5),1)),
(LCPrivate,Reduce(("basic_declaration",5),1)),
(LCAuxiliary,Reduce(("basic_declaration",5),1)),
(LCBegin,Reduce(("basic_declaration",5),1))]]@slrp'a24;
val slrp'a22=[[(LCEos,Reduce(("basic_declaration",4),1)),
(LCLChevron,Reduce(("basic_declaration",4),1)),
(LCFunction,Reduce(("basic_declaration",4),1)),
(LCPackage,Reduce(("basic_declaration",4),1)),
(LCIdentifier,Reduce(("basic_declaration",4),1)),
(LCAnnotation,Reduce(("basic_declaration",4),1)),
(LCSubType,Reduce(("basic_declaration",4),1)),
(LCPragma,Reduce(("basic_declaration",4),1)),
(LCProcedure,Reduce(("basic_declaration",4),1)),
(LCType,Reduce(("basic_declaration",4),1)),
(LCFor,Reduce(("basic_declaration",4),1)),
(LCUse,Reduce(("basic_declaration",4),1)),
(LCImplement,Reduce(("basic_declaration",4),1)),
(LCEnd,Reduce(("basic_declaration",4),1)),
(LCPrivate,Reduce(("basic_declaration",4),1)),
(LCAuxiliary,Reduce(("basic_declaration",4),1)),
(LCBegin,Reduce(("basic_declaration",4),1))]]@slrp'a23;
val slrp'a21=[[(LCEos,Reduce(("basic_declaration",3),1)),
(LCLChevron,Reduce(("basic_declaration",3),1)),
(LCFunction,Reduce(("basic_declaration",3),1)),
(LCPackage,Reduce(("basic_declaration",3),1)),
(LCIdentifier,Reduce(("basic_declaration",3),1)),
(LCAnnotation,Reduce(("basic_declaration",3),1)),
(LCSubType,Reduce(("basic_declaration",3),1)),
(LCPragma,Reduce(("basic_declaration",3),1)),
(LCProcedure,Reduce(("basic_declaration",3),1)),
(LCType,Reduce(("basic_declaration",3),1)),
(LCFor,Reduce(("basic_declaration",3),1)),
(LCUse,Reduce(("basic_declaration",3),1)),
(LCImplement,Reduce(("basic_declaration",3),1)),
(LCEnd,Reduce(("basic_declaration",3),1)),
(LCPrivate,Reduce(("basic_declaration",3),1)),
(LCAuxiliary,Reduce(("basic_declaration",3),1)),
(LCBegin,Reduce(("basic_declaration",3),1))]]@slrp'a22;
val slrp'a20=[[(LCEos,Reduce(("basic_declaration",2),1)),
(LCLChevron,Reduce(("basic_declaration",2),1)),
(LCFunction,Reduce(("basic_declaration",2),1)),
(LCPackage,Reduce(("basic_declaration",2),1)),
(LCIdentifier,Reduce(("basic_declaration",2),1)),
(LCAnnotation,Reduce(("basic_declaration",2),1)),
(LCSubType,Reduce(("basic_declaration",2),1)),
(LCPragma,Reduce(("basic_declaration",2),1)),
(LCProcedure,Reduce(("basic_declaration",2),1)),
(LCType,Reduce(("basic_declaration",2),1)),
(LCFor,Reduce(("basic_declaration",2),1)),
(LCUse,Reduce(("basic_declaration",2),1)),
(LCImplement,Reduce(("basic_declaration",2),1)),
(LCEnd,Reduce(("basic_declaration",2),1)),
(LCPrivate,Reduce(("basic_declaration",2),1)),
(LCAuxiliary,Reduce(("basic_declaration",2),1)),
(LCBegin,Reduce(("basic_declaration",2),1))]]@slrp'a21;
val slrp'a19=[[(LCEos,Reduce(("basic_declaration",1),1)),
(LCLChevron,Reduce(("basic_declaration",1),1)),
(LCFunction,Reduce(("basic_declaration",1),1)),
(LCPackage,Reduce(("basic_declaration",1),1)),
(LCIdentifier,Reduce(("basic_declaration",1),1)),
(LCAnnotation,Reduce(("basic_declaration",1),1)),
(LCSubType,Reduce(("basic_declaration",1),1)),
(LCPragma,Reduce(("basic_declaration",1),1)),
(LCProcedure,Reduce(("basic_declaration",1),1)),
(LCType,Reduce(("basic_declaration",1),1)),
(LCFor,Reduce(("basic_declaration",1),1)),
(LCUse,Reduce(("basic_declaration",1),1)),
(LCImplement,Reduce(("basic_declaration",1),1)),
(LCEnd,Reduce(("basic_declaration",1),1)),
(LCPrivate,Reduce(("basic_declaration",1),1)),
(LCAuxiliary,Reduce(("basic_declaration",1),1)),
(LCBegin,Reduce(("basic_declaration",1),1))]]@slrp'a20;
val slrp'a18=[[(LCEos,Reduce(("basic_declaration",0),1)),
(LCLChevron,Reduce(("basic_declaration",0),1)),
(LCFunction,Reduce(("basic_declaration",0),1)),
(LCPackage,Reduce(("basic_declaration",0),1)),
(LCIdentifier,Reduce(("basic_declaration",0),1)),
(LCAnnotation,Reduce(("basic_declaration",0),1)),
(LCSubType,Reduce(("basic_declaration",0),1)),
(LCPragma,Reduce(("basic_declaration",0),1)),
(LCProcedure,Reduce(("basic_declaration",0),1)),
(LCType,Reduce(("basic_declaration",0),1)),
(LCFor,Reduce(("basic_declaration",0),1)),
(LCUse,Reduce(("basic_declaration",0),1)),
(LCImplement,Reduce(("basic_declaration",0),1)),
(LCEnd,Reduce(("basic_declaration",0),1)),
(LCPrivate,Reduce(("basic_declaration",0),1)),
(LCAuxiliary,Reduce(("basic_declaration",0),1)),
(LCBegin,Reduce(("basic_declaration",0),1))]]@slrp'a19;
val slrp'a17=[[(LCEos,Reduce(("sequence_of_statements_or_block",1),1))]]@slrp'a18;
val slrp'a16=[[(LCEos,Reduce(("sequence_of_statements_or_block",0),1))]]@slrp'a17;
val slrp'a15=[[(LCEos,Reduce(("web_clause",12),3))]]@slrp'a16;
val slrp'a14=[[(LCEos,Reduce(("web_clause",11),1))]]@slrp'a15;
val slrp'a13=[[(LCEos,Reduce(("web_clause",10),2))]]@slrp'a14;
val slrp'a12=[[(LCArbitraryAda,Shift 12)]]@slrp'a13;
val slrp'a11=[[(LCEos,Reduce(("web_clause",9),2))]]@slrp'a12;
val slrp'a10=[[(LCEos,Reduce(("web_clause",8),3))]]@slrp'a11;
val slrp'a9=[[(LCEos,Reduce(("web_clause",7),2))]]@slrp'a10;
val slrp'a8=[[(LCEos,Reduce(("web_clause",6),3))]]@slrp'a9;
val slrp'a7=[[(LCEos,Reduce(("web_clause",5),3))]]@slrp'a8;
val slrp'a6=[[(LCEos,Reduce(("web_clause",4),3))]]@slrp'a7;
val slrp'a5=[[(LCEos,Reduce(("web_clause",3),3))]]@slrp'a6;
val slrp'a4=[[(LCEos,Reduce(("web_clause",2),3))]]@slrp'a5;
val slrp'a3=[[(LCEos,Reduce(("web_clause",1),3))]]@slrp'a4;
val slrp'a2=[[(LCEos,Reduce(("web_clause",0),1))]]@slrp'a3;
val slrp'a1=[[(LCEos,Accept)]]@slrp'a2;
in
val slrp'actions=Array.fromList slrp'a1;
end;
