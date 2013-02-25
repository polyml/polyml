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

    datatype argumentType = datatype BackendIntermediateCode.argumentType

    datatype inlineStatus =
        NonInline
    |   MaybeInline
    |   SmallFunction
    |   OnlyInline

    (* How variables are used.  Added and examined by the optimisation pass. *)
    datatype codeUse =
        UseGeneral (* Used in some other context. *)
    |   UseExport  (* Exported i.e. the result of a top-level binding. *)
    |   UseApply of codeUse list (* Applied as a function - the list is where the result goes *)
    |   UseField of int * codeUse list (* Selected as a field - the list is where the result goes *)
    
    datatype codetree =
        MatchFail    (* Pattern-match failure *)
    
    |   AltMatch of codetree * codetree(* Pattern-match alternative choices *)

    |   Newenv of codeBinding list * codetree (* Set of bindings with an expression. *)

    |   Constnt of machineWord (* Load a constant *)

    |   Extract of loadForm (* Get a local variable, an argument or a closure value *)
    
    |   Indirect of {base: codetree, offset: int, isVariant: bool }
         (* Load a value from a heap record *)
    
    |   Eval of (* Evaluate a function with an argument list. *)
        {
            function:  codetree,
            argList:   (codetree * argumentType) list,
            resultType: argumentType
        }
    
    |   Lambda of lambdaForm (* Lambda expressions. *)

    |   Cond of codetree * codetree * codetree (* If-statement *)
    
    |   BeginLoop of (* Start of tail-recursive inline function. *)
        { loop: codetree, arguments: (simpleBinding * argumentType) list }

    |   Loop of (codetree * argumentType) list (* Jump back to start of tail-recursive function. *)

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

        (* A constant together with the code for either an inline function or a
           tuple.  This is used for global values. *)
    |   ConstntWithInline of machineWord * envSpecial

    and codeBinding =
        Declar  of simpleBinding (* Make a local declaration or push an argument *)
    |   RecDecs of { addr: int, lambda: lambdaForm, use: codeUse list } list (* Set of mutually recursive declarations. *)
    |   NullBinding of codetree (* Just evaluate the expression and discard the result. *)

    and loadForm =
        LoadArgument of int
    |   LoadLocal of int
    |   LoadClosure of int
    |   LoadRecursive

    (* When we look up an entry in the environment we get a pair of
       a "general" value, which is either a constant or a load, and
       an optional special value, which is either a tuple or an
       inline function.  Tuple entries are functions from an integer
       offset to one of these pairs; inline function entries are a
       lambda together with a map for the free variables. *)
    and envGeneral =
        EnvGenLoad of loadForm | EnvGenConst of machineWord

    and envSpecial =
        EnvSpecNone
    |   EnvSpecTuple of int * (int -> envGeneral * envSpecial)
    |   EnvSpecInlineFunction of lambdaForm * (int -> envGeneral * envSpecial)
    
    withtype simpleBinding = 
    { (* Declare a value or push an argument. *)
        value:      codetree,
        addr:       int,
        use:        codeUse list
    }
    
    and lambdaForm =
    { (* Lambda expressions. *)
        body          : codetree,
        isInline      : inlineStatus,
        name          : string,
        closure       : loadForm list,
        argTypes      : (argumentType * codeUse list) list,
        resultType    : argumentType,
        localCount    : int
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

    fun pretty (pt : codetree) : pretty =
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
            MatchFail => PrettyString "MATCHFAIL"
        
        | AltMatch pair => printDiad "ALTMATCH" pair

        | Eval {function, argList, resultType} =>
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
         
        | Extract ext =>
            let
                val str =
                    case ext of
                        LoadArgument addr => concat ["PARAM(", Int.toString addr, ")"]
                    |   LoadLocal addr => concat ["LOCAL(", Int.toString addr, ")"]
                    |   LoadClosure addr => concat ["CLOS(", Int.toString addr, ")"]
                    |   LoadRecursive => "RECURSIVE"
            in
                PrettyString str
            end
        
        | Indirect {base, offset, isVariant} =>
            let
                val str = (if isVariant then "VARIND(" else "INDIRECT(") ^ Int.toString offset ^ ", ";
            in
                PrettyBlock(0, false, [],
                    [ PrettyString str, pretty base, PrettyString ")" ]
                )
            end
        
        | Lambda {body, isInline, name, closure, argTypes, resultType, localCount} =>
            let
                val inl = 
                    case isInline of
                      NonInline   => ""
                    | MaybeInline => "INLINE"
                    | SmallFunction => "SMALL"
                    | OnlyInline  => "ONLYINLINE"
                fun prettyArgTypes [] = []
                |   prettyArgTypes [(last, use)] = [prettyArgType last, prettyUses "" use]
                |   prettyArgTypes ((hd, use)::tl) =
                        prettyArgType hd :: prettyUses "" use :: PrettyBreak(1, 0) :: prettyArgTypes tl
            in
                PrettyBlock (1, true, [],
                    [
                        PrettyString ("LAMBDA"^inl^"("),
                        PrettyBreak (1, 0),
                        PrettyString name,
                        PrettyBreak (1, 0),
                        PrettyString (" LOCALS=" ^ Int.toString localCount),
                        PrettyBreak(1, 0),
                        PrettyBlock (1, false, [], PrettyString "ARGS=" :: prettyArgTypes argTypes),
                        PrettyBreak(1, 0),
                        PrettyBlock (1, false, [], [PrettyString "RES=", prettyArgType resultType]),
                        printList (" CLOS=", map Extract closure, ","),
                        PrettyBreak (1, 0),
                        pretty body,
                        PrettyString "){LAMBDA}"
                    ]
                )
            end
        
        | Constnt w => PrettyString (stringOfWord w)
        
        | Cond triple => printTriad "IF" triple
        
        | Newenv(decs, final) =>
            PrettyBlock (1, true, [],
                PrettyString ("BLOCK" ^ "(") ::
                pList(decs @ [NullBinding final], ";", prettyBinding)
            )

        | BeginLoop{loop=loopExp, arguments=args } =>
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

        |   ConstntWithInline(w, _) =>
            PrettyBlock (1, true, [],
                [
                    PrettyString "CONSTWITHINLINE (",
                    PrettyString (stringOfWord w),
                    PrettyString ", ",
                    PrettyBreak (1, 0),
                    (*pretty (spec),
                    PrettyBreak (1, 0),*)
                    PrettyString ") (*CONSTWITHINLINE*)"
                ]
            )
        (* That list should be exhaustive! *)
    end (* pretty *)

    and prettyBinding(Declar dec) = prettySimpleBinding dec
       
    |   prettyBinding(RecDecs ptl) =
        let
            fun prettyRDec {lambda, addr, use, ...} =
            PrettyBlock (1, false, [],
                [
                    PrettyString ("DECL #" ^ Int.toString addr),
                    PrettyBreak (1, 0),
                    prettyUses "" use,
                    PrettyBreak (1, 0),
                    pretty(Lambda lambda)
                ]
            )
        in
            PrettyBlock (1, true, [],
                PrettyString ("MUTUAL" ^ "(") ::
                pList(ptl, " AND ", prettyRDec) @
                [ PrettyBreak (0, 0), PrettyString (")") ]
            )
        end
    |   prettyBinding(NullBinding c) = pretty c

    and prettySimpleBinding{value, addr, use, ...} =
        PrettyBlock (1, false, [],
            [
                PrettyString ("DECL #" ^ Int.toString addr),
                PrettyBreak (1, 0),
                prettyUses "" use,
                PrettyBreak(1, 0),
                PrettyString "=",
                PrettyBreak (1, 0),
                pretty value
            ]
        )

    and prettyUses prefix cl =
           PrettyBlock (1, true, [],
                PrettyString (prefix ^ "[") ::
                pList(cl, ",", prettyUsage) @
                [ PrettyBreak (0, 0), PrettyString ("]") ]
            )

    and prettyUsage UseGeneral = PrettyString "UseGen"
    |   prettyUsage UseExport = PrettyString "UseExp"
    |   prettyUsage (UseApply cl) = prettyUses "UseApp" cl
    |   prettyUsage (UseField (n, cl)) = prettyUses ("UseField"^ Int.toString n) cl

    (* Mapping function to enable parts of the tree to be replaced. *)
    fun mapCodetree f code =
    let
        (* We use these functions to allow all nodes to be processed even if
           they are not full codetree nodes. *)
        fun deExtract(Extract l) = l | deExtract _ = raise Misc.InternalError "deExtract"
        fun deLambda (Lambda l) = l | deLambda _ = raise Misc.InternalError "deLambda"

        fun mapt MatchFail = MatchFail
        |   mapt (AltMatch(m1, m2)) = AltMatch(mapCodetree f m1, mapCodetree f m2)
        |   mapt (Newenv(decs, exp)) =
            let
                fun mapbinding(Declar{value, addr, use}) = Declar{value=mapCodetree f value, addr=addr, use=use}
                |   mapbinding(RecDecs l) =
                        RecDecs(map(fn {addr, lambda, use} =>
                            {addr=addr, use = use, lambda = deLambda(mapCodetree f (Lambda lambda))}) l)
                |   mapbinding(NullBinding exp) = NullBinding(mapCodetree f exp)
            in
                Newenv(map mapbinding decs, mapCodetree f exp)
            end
        |   mapt (c as Constnt _) = c
        |   mapt (e as Extract _) = e
        |   mapt (Indirect { base, offset, isVariant }) =
                Indirect{ base = mapCodetree f base, offset = offset, isVariant = isVariant }
        |   mapt (Eval { function, argList, resultType }) =
                Eval {
                    function = mapCodetree f function, 
                    argList = map (fn(c, a) => (mapCodetree f c, a)) argList,
                    resultType = resultType
                }
        |   mapt (Lambda { body, isInline, name, closure, argTypes, resultType, localCount }) =
                Lambda {
                    body = mapCodetree f body, isInline = isInline, name = name,
                    closure = map (deExtract o (mapCodetree f) o Extract) closure,
                    argTypes = argTypes, resultType = resultType, localCount = localCount
                }
        |   mapt (Cond(i, t, e)) = Cond(mapCodetree f i, mapCodetree f t, mapCodetree f e)
        |   mapt (BeginLoop{loop, arguments}) =
                BeginLoop {
                    loop = mapCodetree f loop,
                    arguments = map(fn({value, addr, use}, t) => ({value=mapCodetree f value, addr=addr, use=use}, t)) arguments
                
                }
        |   mapt (Loop l) = Loop (map(fn(c, t) => (mapCodetree f c, t)) l)
        |   mapt (Raise r) = Raise(mapCodetree f r)
        |   mapt Ldexc = Ldexc
        |   mapt (Handle{exp, handler}) = Handle{exp=mapCodetree f exp, handler=mapCodetree f handler }
        |   mapt (Recconstr l) = Recconstr(map (mapCodetree f) l)
        |   mapt (c as Container _) = c
        |   mapt (SetContainer{container, tuple, size}) =
                SetContainer{
                    container = mapCodetree f container, tuple = mapCodetree f tuple, size = size }
        |   mapt (TupleFromContainer(c, s)) = TupleFromContainer(mapCodetree f c, s)
        |   mapt (TagTest{test, tag, maxTag}) = TagTest{test = mapCodetree f test, tag = tag, maxTag = maxTag }
        |   mapt (c as ConstntWithInline _) = c
    in
        (* Apply f to node.  If it returns SOME c use that otherwise
           traverse the tree. *)
        case f code of
            SOME c => c
        |   NONE => mapt code
    end

    structure Sharing =
    struct
        type codetree = codetree
        and  pretty = pretty
        and  inlineStatus = inlineStatus
        and  argumentType = argumentType
        and  codeBinding = codeBinding
        and  simpleBinding = simpleBinding
        and  loadForm = loadForm
        and  envGeneral = envGeneral
        and  envSpecial = envSpecial
        and  codeUse = codeUse
    end

end;
