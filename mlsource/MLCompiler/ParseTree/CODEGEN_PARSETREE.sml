(*
    Copyright (c) 2013, 2014 David C.J. Matthews

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
    Derived from the original parse-tree

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further development:
    Copyright (c) 2000-13 David C.J. Matthews

    Title:      Parse Tree Structure and Operations.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985

*)

functor CODEGEN_PARSETREE (
    structure BASEPARSETREE : BaseParseTreeSig
    structure PRINTTREE: PrintParsetreeSig
    structure EXPORTTREE: ExportParsetreeSig
    structure MATCHCOMPILER: MatchCompilerSig
    structure LEX : LEXSIG
    structure CODETREE : CODETREESIG
    structure DEBUGGER : DEBUGGERSIG
    structure TYPETREE : TYPETREESIG
    structure TYPEIDCODE: TYPEIDCODESIG
    structure STRUCTVALS : STRUCTVALSIG
    structure VALUEOPS : VALUEOPSSIG
    structure DATATYPEREP: DATATYPEREPSIG

    structure DEBUG :
    sig
        val debugTag: bool Universal.tag
        val errorDepthTag : int Universal.tag
        val fileNameTag: string Universal.tag
        val reportUnreferencedIdsTag: bool Universal.tag
        val reportExhaustiveHandlersTag: bool Universal.tag
        val getParameter :
               'a Universal.tag -> Universal.universal list -> 'a 
    end

    structure MISC :
    sig
        (* These are handled in the compiler *)
        exception Conversion of string     (* string to int conversion failure *)
  
        (* This isn't handled at all (except generically) *)
        exception InternalError of string (* compiler error *)
    end

    structure ADDRESS : AddressSig

    sharing BASEPARSETREE.Sharing
    =       PRINTTREE.Sharing
    =       EXPORTTREE.Sharing
    =       MATCHCOMPILER.Sharing
    =       LEX.Sharing
    =       CODETREE.Sharing
    =       DEBUGGER.Sharing
    =       TYPETREE.Sharing
    =       TYPEIDCODE.Sharing
    =       STRUCTVALS.Sharing
    =       VALUEOPS.Sharing
    =       DATATYPEREP.Sharing
    =       ADDRESS
): CodegenParsetreeSig =
struct
    open BASEPARSETREE
    open PRINTTREE
    open EXPORTTREE
    open MATCHCOMPILER
    open CODETREE
    open TYPEIDCODE
    open LEX
    open TYPETREE
    open DEBUG
    open STRUCTVALS
    open VALUEOPS
    open MISC
    open DATATYPEREP
    open TypeVarMap
    open DEBUGGER

    open RuntimeCalls; (* for POLY_SYS numbers *)

    datatype environEntry = datatype DEBUGGER.environEntry

    (* To simplify passing the context it is wrapped up in this type. *)
    type cgContext =
        {
            decName: string, debugEnv: environEntry list * (level->codetree), mkAddr: int->int,
            level: level, typeVarMap: typeVarMap, lex: lexan, lastDebugLine: int ref,
            isOuterLevel: bool (* Used only to decide if we need to report non-exhaustive matches. *)
        }

    fun repDecName decName ({debugEnv, mkAddr, level, typeVarMap, lex, lastDebugLine, isOuterLevel, ...}: cgContext) =
        { debugEnv=debugEnv, mkAddr=mkAddr, level=level, typeVarMap=typeVarMap,
          decName=decName, lex=lex, lastDebugLine=lastDebugLine, isOuterLevel = isOuterLevel}: cgContext
    and repDebugEnv debugEnv ({decName, mkAddr, level, typeVarMap, lex, lastDebugLine, isOuterLevel, ...}: cgContext) =
        { debugEnv=debugEnv, mkAddr=mkAddr, level=level, typeVarMap=typeVarMap,
          decName=decName, lex=lex, lastDebugLine=lastDebugLine, isOuterLevel = isOuterLevel}: cgContext
    and repTypeVarMap typeVarMap ({decName, debugEnv, mkAddr, level, lex, lastDebugLine, isOuterLevel, ...}: cgContext) =
        { debugEnv=debugEnv, mkAddr=mkAddr, level=level, typeVarMap=typeVarMap,
          decName=decName, lex=lex, lastDebugLine=lastDebugLine, isOuterLevel = isOuterLevel}: cgContext
    (* Create a new level.  Sets isOuterLevel to false. *)
    and repNewLevel(decName, mkAddr, level) ({debugEnv, lex, lastDebugLine, typeVarMap, ...}: cgContext) =
        { debugEnv=debugEnv, mkAddr=mkAddr, level=level, typeVarMap=typeVarMap,
          decName=decName, lex=lex, lastDebugLine=lastDebugLine, isOuterLevel = false}: cgContext

    (* Try this pipeline function *)
    infix |>
    fun a |> f = f a

    val singleArg = mkLoadArgument 0

    (* Make a tuple out of a set of arguments or return the single
       argument if there is just one. *)
    fun mkArgTuple(from, nTuple) =
        if nTuple = 1 (* "tuple" is a singleton *)
        then mkLoadArgument from
        else if nTuple <= 0 then raise InternalError "mkArgTuple"
        else mkTuple(List.tabulate(nTuple, fn n => mkLoadArgument(n+from)))

    (* Load args by selecting from a tuple. *)
    fun loadArgsFromTuple([t], arg) = [(arg, t)](* "tuple" is a singleton *)
    |   loadArgsFromTuple(types, arg) =
            ListPair.zip(List.tabulate(List.length types, fn num => mkInd (num, arg)), types)

    (* Return the argument/result type which is currently just floating point or everything else. *)
    fun getCodeArgType t = if isFloatingPt t then FloatingPtType else GeneralType

    (* tupleWidth returns the width of a tuple or record or 1 if it
       isn't one.  It is used to detect both argument tuples and results.
       When used for arguments the idea is that frequently a tuple is
       used as a way of passing multiple arguments and these can be
       passed on the stack.  When used for results the idea is to
       create the result tuple  on the stack and avoid garbage collector
       and allocator time.  If we could tell that the caller was simply going
       to explode it we would gain but if the caller needed a
       tuple on the heap we wouldn't.  We wouldn't actually lose
       if we were going to create a tuple and return it but we
       would lose if we exploded a tuple here and then created
       a new one in the caller.
       This version of the code assumes that if we create a tuple
       on one branch we're going to create one on others which may
       not be correct. *)
    (* This now returns the argument type for each entry so returns a list rather
       than a number. *)
    fun tupleWidth(TupleTree{expType=ref expType, ...}) = recordFieldMap getCodeArgType expType

    |  tupleWidth(Labelled{expType=ref expType, ...}) =
       if recordNotFrozen expType (* An error, but reported elsewhere. *)
       then [GeneralType] (* Safe enough *)
       else recordFieldMap getCodeArgType expType

    |  tupleWidth(Cond{thenpt, elsept, ...}) =
        (
            case tupleWidth thenpt of
                [_] => tupleWidth elsept
            |   w => w
        )

    |  tupleWidth(Constraint{value, ...}) = tupleWidth value

    |  tupleWidth(HandleTree{exp, ...}) =
          (* Look only at the expression and ignore
           the handlers on the, possibly erroneous,
           assumption that they won't normally be
           executed. *)
          tupleWidth exp

    |  tupleWidth(Localdec{body=[], ...}) = raise InternalError "tupleWidth: empty localdec"

    |  tupleWidth(Localdec{body, ...}) =
          (* We are only interested in the last expression. *)
          tupleWidth(List.last body)

    |  tupleWidth(Case{match, ...}) =
        let
            fun getWidth(MatchTree{exp, ...}) = tupleWidth exp
        in
            List.foldl(fn(v, [_]) => getWidth v | (_, s) => s)
                      [GeneralType] match
        end

    |  tupleWidth(Parenthesised(p, _)) = tupleWidth p

    |  tupleWidth(ExpSeq(p, _)) = tupleWidth(List.last p)

    |  tupleWidth(Ident{ expType=ref expType, ...}) = [getCodeArgType expType]

    |  tupleWidth(Literal{ expType=ref expType, ...}) = [getCodeArgType expType]

    |  tupleWidth(Applic{ expType=ref expType, ...}) = [getCodeArgType expType]

    |  tupleWidth _ = [GeneralType]

    (* Start of the code-generator itself. *)
    type debugenv = environEntry list * (level->codetree)
  
    (* Report unreferenced identifiers. *)

    fun reportUnreferencedValue lex
            (Value{name, references=SOME{exportedRef=ref false, localRef=ref nil, ...}, locations, ...}) =
        let
            fun getDeclLoc (DeclaredAt loc :: _) = loc
            |   getDeclLoc (_ :: locs) = getDeclLoc locs
            |   getDeclLoc [] = nullLocation (* Shouldn't happen. *)
        in
            warningMessage(lex, getDeclLoc locations,
                "Value identifier ("^name^") has not been referenced.")
        end
    |   reportUnreferencedValue _ _ = ()

    (* Process a list of possibly mutually recursive functions and identify those that
       are really referenced. *)
    fun reportUnreferencedValues(valList, lex) =
    let
        fun checkRefs valList =
        let
            fun unReferenced(Value{references=SOME{exportedRef=ref false, localRef=ref nil, ...}, ...}) = true
            |   unReferenced _ = false
            val (unrefed, refed) = List.partition unReferenced valList
            fun update(Value{references=SOME{localRef, recursiveRef, ...}, ...}, changed) =
                let
                    (* If it is referred to by a referenced function it is referenced. *)
                    fun inReferenced(_, refName) = List.exists (fn Value{name, ...} => name=refName) refed
                    val (present, absent) = List.partition inReferenced (!recursiveRef)
                in
                    if null present
                    then changed
                    else
                    (
                        localRef := List.map #1 present @ ! localRef;
                        recursiveRef := absent;
                        true
                    )
                end
            |   update(_, changed) = changed
        in
            (* Repeat until there's no change. *)
            if List.foldl update false unrefed then checkRefs unrefed else ()
        end
    in
        checkRefs valList;
        List.app (reportUnreferencedValue lex) valList
    end


    (* Debugging control and debug function. *)
    fun debuggerFun lex =
        case List.find (Universal.tagIs DEBUGGER.debuggerFunTag) (LEX.debugParams lex) of
            SOME f => Universal.tagProject DEBUGGER.debuggerFunTag f
        |   NONE => DEBUGGER.nullDebug

    (* Add a call to the debugger. *)
    fun addDebugCall (location, {decName, debugEnv=(ctEnv, rtEnv), level, lastDebugLine, lex, ...}) : codetree =
        let
            open DEBUGGER
            val currLine = #startLine location
            val debugger =
                debugFunction(debuggerFun lex, DebugStep, decName, location) ctEnv
        in
            lastDebugLine := currLine;
            mkEval(mkConst(ADDRESS.toMachineWord debugger), [rtEnv level])
        end

    (* Add a debug call if line has changed.  This is used between
       declarations and expression sequences to avoid more than one
       call on a line.  If there is no line number, e.g. input typed
       at the top level, always put this in. *)
    fun changeLine (loc, context as { lex, lastDebugLine, ...}) =
        if not (getParameter debugTag (debugParams lex)) orelse
           (#startLine loc = !lastDebugLine andalso #startLine loc <> 0)
        then []
        else [mkNullDec(addDebugCall(loc, context))]

    (* TODO: This could probably use makeValDebugEntries *)
    fun createDebugEntry (v: values, loadVal, {mkAddr, level, debugEnv=(ctEnv, rtEnv: level -> codetree), lex, ...}: cgContext) =
        if not (getParameter debugTag (debugParams lex))
        then { dec = [], rtEnv = rtEnv, ctEnv = ctEnv }
        else let
                val newEnv =
                (* Create a new entry in the environment. *)
                      mkTuple [ loadVal (* Value. *), rtEnv level ]
                val { dec, load } = multipleUses (newEnv, fn () => mkAddr 1, level)
                val ctEntry =
                    case v of
                        Value{class=Exception, name, typeOf, locations, ...} =>
                            EnvException(name, typeOf, locations)
                    |   Value{class=Constructor{nullary, ofConstrs, ...}, name, typeOf, locations, ...} =>
                            EnvVConstr(name, typeOf, nullary, ofConstrs, locations)
                    |   Value{name, typeOf, locations, ...} =>
                            EnvValue(name, typeOf, locations)
            in
                { dec = dec, rtEnv = load, ctEnv = ctEntry :: ctEnv}
            end

    (* Start a new static level.  This is currently used only to
       distinguish function arguments from the surrounding static
       environment. *)
    fun newDebugLevel (ctEnv, rtEnv) = (EnvStaticLevel :: ctEnv, rtEnv)

    fun makeDebugEntries (vars: values list, {debugEnv, level, typeVarMap, lex, mkAddr, ...}: cgContext) =
        DEBUGGER.makeValDebugEntries(vars, debugEnv, level, lex, mkAddr, typeVarMap)

    (* In order to build a call stack in the debugger we need to know about
       function entry and exit.  It would be simpler to wrap the whole function
       in a debug function (i.e. loop the call through the debugger) but that
       would prevent us from using certain call optimisations. *)
    fun wrapFunctionInDebug(body: codetree, name: string, argCode: codetree,
                            argType: types, restype: types, location,
                            {debugEnv=(ctEnv, rtEnv), mkAddr, level, lex, ...}) =
        if not (getParameter debugTag (debugParams lex)) then body (* Return it unchanged. *)
        else
        let
            open DEBUGGER
            (* At the moment we can't deal with function arguments. *)
            fun enterFunction (rtEnv, args) =
                debugFunction(debuggerFun lex, DebugEnter(args, argType), name, location) ctEnv rtEnv
            and leaveFunction (rtEnv, result) =
                (debugFunction(debuggerFun lex, DebugLeave(result, restype), name, location) ctEnv rtEnv; result)
            and exceptionFunction (rtEnv, exn) =
                (debugFunction(debuggerFun lex, DebugException exn, name, location) ctEnv rtEnv; raise exn)

            val entryCode = ADDRESS.toMachineWord enterFunction
            and exitCode = ADDRESS.toMachineWord leaveFunction
            and exceptionCode = ADDRESS.toMachineWord exceptionFunction
            val ldexAddr = mkAddr 1
        in
            mkEnv(
                (* Call the enter code. *)
                [mkNullDec (mkEval(mkConst entryCode, [mkTuple[rtEnv level, argCode]]))],
                (* Call the exit code with the function result. The
                   function is wrapped in a handler that catches all
                   exceptions and calls the exception code. *)
                mkEval(mkConst exitCode,
                    [mkTuple[rtEnv level,
                        mkHandle (body,
                        mkEnv
                            (
                                (* Must save the exception packet first. *)
                                [mkDec(ldexAddr, Ldexc)],
                                mkEval(mkConst exceptionCode,
                                    [mkTuple[rtEnv level, mkLoadLocal ldexAddr]]))
                            )
                    ]])
            )
        end

           (* Find all the variables declared by each pattern. *)
      fun findVars vars varl =
        case vars of
          Ident {value, ...} =>
          let
            val ident = ! value;
          in
            if isConstructor ident
            then varl (* Ignore constructors *)
            else ident :: varl
          end
      
        | TupleTree{fields, ...} =>
            List.foldl (fn (v1, v2) => findVars v1 v2) varl fields
      
        | Labelled {recList, ...} =>
            List.foldl (fn ({valOrPat, ...}, v) => findVars valOrPat v) varl recList
      
         (* Application of a constructor: only the argument
            can contain vars. *)
        | Applic {arg, ...} =>
            findVars arg varl
      
        | List{elements, ...} =>
            List.foldl (fn (v1, v2) => findVars v1 v2) varl elements
      
        | Constraint {value, ...} =>
            findVars value varl
      
        | Layered {var, pattern, ...} =>
             (* There may be a constraint on the variable
                so it is easiest to recurse. *)
            findVars pattern (findVars var varl)

        | Parenthesised(p, _) =>
            findVars p varl
       
        | _ =>
            varl (* constants and error cases. *);

      val findAllVars =
          map (fn (MatchTree{vars, ...}) => findVars vars []);

      (* Make an argument list from the variables bound in the pattern. *)
      fun makeArglist []        _ = []
        | makeArglist (Value{access=Local{addr=ref lvAddr, ...}, ...} :: vs) argno =
            mkLoadLocal lvAddr :: makeArglist vs (argno - 1) 
        | makeArglist _ _ = raise InternalError "makeArgList"

    (* If we are only passing equality types filter out the others. *)
    val filterTypeVars = List.filter (fn tv => not justForEqualityTypes orelse tvEquality tv)


    fun codeMatch(near : parsetree, alt : matchtree list, arg : codetree, cgExpression,
                  isHandlerMatch, matchContext as { level, mkAddr, lex, ...}): codetree =
    let
        val noOfPats  = length alt
        val allVars   = findAllVars alt
        (* Check for unreferenced variables. *)
        val () =
            if getParameter reportUnreferencedIdsTag (debugParams lex)
            then List.app (fn l => List.app (reportUnreferencedValue lex) l) allVars
            else ()

        val lineNo =
            case alt of
                MatchTree {location, ... } :: _ => location
            | _ => raise Match

        (* Save the argument in a variable. *)
        val decCode   = multipleUses (arg, fn () => mkAddr 1, level);

        (* Generate code to load it. *)
        val loadExpCode = #load decCode level;

        (* Generate a range of addresses for any functions that have to
           be generated for the expressions. *)  
        val baseAddr  = mkAddr noOfPats

        (* We want to avoid the code blowing up if we have a large expression which occurs
           multiple times in the resulting code. 
           e.g. case x of [1,2,3,4] => exp1 | _ => exp2
           Here exp2 will be called at several points in the code.  Most patterns occur
           only once, sometimes a few more times.  The first three times the pattern
           occurs the code is inserted directly.  Further cases are dealt with as
           function calls.  *)
        val insertDirectCount = 3 (* First three cases are inserted directly. *)

        (* Make an array to count the number of references to a pattern.
            This is used to decide whether to use a function for certain
            expressions or to make it inline. *)
        val uses = IntArray.array (noOfPats, 0);

        (* Called when a selection has been made to code-generate the expression. *)
        fun codePatternExpression(pattChosenIndex, context) =
        let
            (* Increment the count for this pattern. *)
            val useCount = IntArray.sub(uses, pattChosenIndex) + 1
            val () = IntArray.update (uses, pattChosenIndex, useCount)
        in
            if useCount <= insertDirectCount
            then (* Use the expression directly *)
            let
                val MatchTree {exp, ... } = List.nth(alt, pattChosenIndex)            
            in
                cgExpression (exp, context)
            end
            else
            let (* Put in a call to the expression as a function. *)
                val thisVars    = List.nth(allVars, pattChosenIndex)
                val noOfArgs    = length thisVars
                val argsForCall = makeArglist thisVars noOfArgs
            in
                mkEval(mkLoadLocal (baseAddr + pattChosenIndex), argsForCall)
            end
        end

        (* Generate the code and also check for redundancy
           and exhaustiveness. *)
        val (matchCode, exhaustive) =
            codeMatchPatterns(alt, loadExpCode, isHandlerMatch, lineNo, codePatternExpression, matchContext)

        (* Report inexhaustiveness if necessary.  TODO: It would be nice to have
           some example of a pattern that isn't matched for. *)
        (* If this is a handler we may have set the option to report exhaustiveness.
           This helps in tracking down handlers that don't treat Interrupt specially. *)
        val () = 
            if exhaustive
            then if isHandlerMatch andalso getParameter reportExhaustiveHandlersTag (debugParams lex)
            then errorNear (lex, false, near, lineNo, "Handler catches all exceptions.")
            else ()
            else if isHandlerMatch
            then ()
            else errorNear (lex, false, near, lineNo, "Matches are not exhaustive.")
        (* Report redundant patterns. *)
        local
            fun reportRedundant(patNo, 0) =
                let
                    val MatchTree {location, ... } = List.nth(alt, patNo)
                in
                    errorNear (lex, false, near, location,
                                "Pattern " ^ Int.toString (patNo+1) ^ " is redundant.")
                end
            |   reportRedundant _ = ()
        in
            val () = IntArray.appi reportRedundant uses
        end

      (* Generate variable-bindings (declarations) for each of the
          expressions as functions. *)
        fun cgExps([],  _,    _, _, _, _, _, _, _) = []
        |   cgExps (MatchTree {exp, ...} ::al, vl::vll,
                base, patNo, uses, cgExpression, lex, near,
                cgContext as { decName, debugEnv, level, ...}) =
            if IntArray.sub(uses, patNo - 1) <= insertDirectCount
            then cgExps(al, vll, base, patNo + 1, uses, cgExpression, lex, near, cgContext)
            else
            let
                val noOfArgs = length vl
                val patNoIndex = patNo - 1

                val functionLevel = newLevel level (* For the function. *)
                local
                    val addresses = ref 1
                in
                    fun fnMkAddrs n = ! addresses before (addresses := !addresses + n)
                end
    
                val fnContext = cgContext |> repNewLevel(decName, fnMkAddrs, functionLevel)

                (* Set the addresses to be suitable for arguments.  At the
                   same time create a debugging environment if required. *)
                fun setAddr (v as Value{access=Local{addr=lvAddr, level=lvLevel}, ...},
                            (localAddr, oldDec, oldEnv)) =
                    let
                        val load = mkLoad (localAddr, functionLevel, functionLevel)
                        val {dec=nextDec, ctEnv, rtEnv} =
                            createDebugEntry(v, load, fnContext |> repDebugEnv oldEnv)
                    in
                        lvAddr  := localAddr;
                        lvLevel := functionLevel;
                        (localAddr + 1, oldDec @ nextDec, (ctEnv, rtEnv))
                    end
                |   setAddr _ = raise InternalError "setAddr"

                (* Bind a local variable to the argument so we can set the variable address as
                   a local address. *)
                val firstLocal = fnMkAddrs 0
                val localDecs =
                    List.tabulate(noOfArgs, fn n => mkDec(fnMkAddrs 1, mkLoadArgument n)) (* One address for each argument. *)

                val (_, envDec, newEnv) = List.foldl setAddr (firstLocal, [], debugEnv) vl

                val functionBody =
                    mkEnv(localDecs @ envDec, cgExpression (exp, fnContext |> repDebugEnv newEnv))
            in
                mkDec(base + patNoIndex,
                    mkProc (functionBody, noOfArgs, decName ^ "/" ^ Int.toString patNo, getClosure functionLevel, fnMkAddrs 0)) ::
                    cgExps(al, vll, base, patNo + 1, uses, cgExpression, lex, near, cgContext)
            end

        |   cgExps _ = raise InternalError "cgExps"
                (* Now generate the expressions as functions, inline
           if only used once. *)
        val expressionFuns =
            cgExps(alt, allVars, baseAddr, 1, uses, cgExpression, lex, near, matchContext)
    in
        (* Return the code in a block. *)
        mkEnv (#dec decCode @ expressionFuns, matchCode)
    end (* codeMatch *)

    (* Makes a block from a series of alternatives in a match.
       Used only for functions. *)
    fun codeAlt(near: parsetree, alt : matchtree list, arg : codetree, context as {lex, ...}) =
    let
        (* Insert a call to the debugger in each arm of the match after
           the variables have been bound but before the body. *)
        fun cgExp (c: parsetree, context) =
        let
            val code = codegen(c, context)
        in
            if not (getParameter debugTag (debugParams lex))
            then code
            else mkEnv([mkNullDec(addDebugCall(getLocation c, context))], code)
        end
    in
        codeMatch (near, alt, arg, cgExp, false, context)
    end

      (* Code-generates a piece of tree. *)
    and codegen (c: parsetree, context as { level, decName, typeVarMap, mkAddr, lex, ...}) : codetree =
    let
    in
        case c of
            Ident {value = ref (v as Value{class, ...}), expType=ref expType, location, ...} =>
            let
                (* The instance type is not necessarily the same as the type
                   of the value of the identifier. e.g. in the expression
                   1 :: nil, "::" has an instance type of
                   int * list int -> list int but the type of "::" is
                   'a * 'a list -> 'a list. *)
                val polyVars = getPolymorphism (v, expType, typeVarMap)
                (*val () =
                    print(concat[name, " has ", Int.toString(List.length(#1 polyVars)), " ",
                            Int.toString(List.length(#2 polyVars)), "\n"]);*)
            in
                case class of
                    Exception => codeExFunction (v, level, typeVarMap, [], lex, location)
                |   Constructor _ =>
                    let
                        (* When using the constructor as a value we just want
                           then second word.  Must pass [] as the polyVars otherwise
                           this will be applied BEFORE extracting the construction
                           function not afterwards. *)
                        fun getConstr level =
                            ValueConstructor.extractInjection(codeVal (v, level, typeVarMap, [], lex, location))
                    in
                        applyToInstance(if justForEqualityTypes then [] else polyVars, level, typeVarMap, getConstr)
                    end

                |   _ => codeVal (v, level, typeVarMap, polyVars, lex, location)
            end
  
        |   Literal{converter, literal, expType=ref expType, location} =>
            (
                case getLiteralValue(converter, literal, expType, fn s => errorNear(lex, true, c, location, s)) of
                    SOME w => mkConst w
                  | NONE => CodeZero
            )

        | Applic {f, arg, location, ...} =>
            let
                (* The overloaded functions of more than one argument are
                   applied to their arguments rather than to a tuple. *)
                (* The only other optimisation we make is to remove applications
                   of constructors such as ``::'' which are no-ops. *)
                val argument : codetree = codegen (arg, context)
            in
                (* Some functions are special e.g. overloaded and type-specific functions.
                   These need to picked out and processed by applyFunction. *)
                case f of
                    Ident {value = ref function, expType=ref expType, ...} =>
                    let
                        val polyVars = getPolymorphism (function, expType, typeVarMap)
                        (*val () =
                            print(concat[name, " has ", Int.toString(List.length(#1 polyVars)), " ",
                                    Int.toString(List.length(#2 polyVars)), "\n"]);*)
                    in
                        applyFunction (function, argument, level, typeVarMap, polyVars, lex, location)
                    end
                |   _ =>
                    mkEval (codegen (f, context), [argument])
            end

        | Cond {test, thenpt, elsept, ...} =>
            mkIf (codegen (test, context), codegen (thenpt, context), codegen (elsept, context))

        | TupleTree{fields=[(*pt*)_], ...} =>
            (* There was previously a special case to optimise unary tuples but I can't
               understand how they can occur.  Check this and remove the special case
               if it really doesn't. *)
            raise InternalError "codegen: Unary tuple" (*codegen (pt, context)*)

        | TupleTree{fields, ...} => (* Construct a vector of objects. *)
            mkTuple(map (fn x => codegen (x, context)) fields)

        | Labelled {recList = [{valOrPat, ...}], ...} =>
            codegen (valOrPat, context) (* optimise unary records *)

        | Labelled {recList, expType=ref expType, ...} =>
            let
                (* We must evaluate the expressions in the order they are
                   written. This is not necessarily the order they appear
                   in the record. *)
                val recordSize = length recList; (* The size of the record. *)
        
                (* First declare the values as local variables. *)
                (* We work down the list evaluating the expressions and putting
                   the results away in temporaries. When we reach the end we
                   construct the tuple by asking for each entry in turn. *) 
                fun declist [] look = ([], mkTuple (List.tabulate (recordSize, look)))
          
                |   declist ({name, valOrPat, ...} :: t) look =
                    let
                        val thisDec = 
                            multipleUses (codegen (valOrPat, context), fn () => mkAddr 1, level);
            
                        val myPosition = entryNumber (name, expType);
            
                        fun lookFn i =
                            if i = myPosition then #load thisDec (level) else look i
                        val (otherDecs, tuple) = declist t lookFn
                    in
                        (#dec thisDec @ otherDecs, tuple)
                    end
            in
                (* Create the record and package it up as a block. *)
                mkEnv (declist recList (fn _ => raise InternalError "missing in record"))  : codetree
            end

        | Selector {name, labType, location, typeof, ...} =>
            let
                (* Check that the type is frozen. *)
                val () =
                    if recordNotFrozen labType
                    then errorNear (lex, true, c, location, "Can't find a fixed record type.")
                    else ();

                val selectorBody : codetree =
                    if recordWidth labType = 1
                    then singleArg (* optimise unary tuples - no indirection! *)
                    else
                    let
                        val offset : int = entryNumber (name, labType);
                    in
                        mkInd (offset, singleArg)
                    end
            in    (* Make an inline function. *)
                case filterTypeVars (getPolyTypeVars(typeof, mapTypeVars typeVarMap)) of
                    [] => mkInlproc (selectorBody, 1, decName ^ "#" ^ name, [], 0)
                |   polyVars => (* This may be polymorphic. *)
                        mkInlproc(
                            mkInlproc (selectorBody, 1, decName ^ "#" ^ name, [], 0),
                            List.length polyVars, decName ^ "#" ^ name ^ "(P)", [], 0)
            end

        | Unit _ => (* Use zero.  It is possible to have () = (). *)
            CodeZero : codetree

        |   List{elements, expType = ref listType, location, ...} =>
            let (* Construct a list.  We need to apply the constructors appropriate to the type. *)
                val baseType =
                    case listType of
                        TypeConstruction{args=[baseType], ...} => baseType
                    |   _ => raise InternalError "List: bad element type"
                val consType = mkFunctionType(mkProductType[baseType, listType], listType)
                fun consList [] =
                    let (* "nil" *)
                        val polyVars = getPolymorphism (nilConstructor, listType, typeVarMap)
                        fun getConstr level =
                            ValueConstructor.extractInjection(
                                codeVal (nilConstructor, level, typeVarMap, [], lex, location))
                    in
                        applyToInstance(polyVars, level, typeVarMap, getConstr)
                    end
                |   consList (h::t) =
                    let (* :: *)
                        val H = codegen (h, context) and T = consList t
                        val polyVars = getPolymorphism (consConstructor, consType, typeVarMap)
                    in
                        applyFunction (consConstructor, mkTuple [H,T], level, typeVarMap, polyVars, lex, location)
                    end
            in
                consList elements : codetree
            end

        | Constraint {value, ...} => codegen (value, context) (* code gen. the value *)

        | Fn { location, expType=ref expType, ... } =>
            (* Function *)
            codeProc(c, location, filterTypeVars(getPolyTypeVars(expType, mapTypeVars typeVarMap)), context)

        |   Localdec {decs, body, ...} =>
            (* Local expressions only. Local declarations will be handled
                by codeSequence.*)
            let
                (* This is the continuation called when the declarations have been
                   processed.  We need to ensure that if there are local datatypes
                   we make new entries in the type value cache after them. *)
                (* TODO: This is a bit of a mess.  We want to return the result of the
                   last expression as an expression rather than a codeBinding. *)
                fun processBody (previousDecs: codeBinding list, newContext as {debugEnv, ...}) =
                let
                    fun codeList [] = []
                     |  codeList (x::tl) =
                         (* Generate any line change code first, then this entry, then the rest. *)
                        let
                            val lineChange = changeLine(getLocation x, newContext)
                            val code = mkNullDec(codegen (x, newContext))
                        in
                            lineChange @ code :: codeList tl
                        end
                    val exps = codeList body;
                in
                    (previousDecs @ exps, debugEnv)
                end

                val (decs, _) = codeSequence (decs, [], context, processBody)
            in
                decSequenceWithFinalExp decs
            end

        | ExpSeq(ptl, _) =>
          (* Sequence of expressions. Discard results of all except the
             last. It isn't clear whether this will work properly since
             the code-generator does not expect expressions to return
             results unless they are wanted. It may be necessary to turn
             all except the last into declarations. *)
            let
                fun codeList [] = raise InternalError "ExpSeq: empty sequence"
                |   codeList [last] =
                        (changeLine(getLocation last, context), codegen (last, context))
                 |  codeList (x::tl) =
                    let
                        val lineChange = changeLine(getLocation x, context)
                        val code = mkNullDec(codegen (x, context))
                        val (otherDecs, expCode) = codeList tl
                    in
                        (lineChange @ (code :: otherDecs), expCode)
                    end
            in
                mkEnv (codeList ptl)
            end

        |   Raise (pt, location) =>
            let
                val {dec, load} = 
                    multipleUses (codegen (pt, context), fn () => mkAddr 1, level)
                val load = load level
                (* Copy the identifier, name and argument from the packet and add this location. *)
                val excPacket =
                    mkEnv(dec,
                        mkTuple[mkInd(0, load), mkInd(1, load), mkInd(2, load), codeLocation location])
            in
                if getParameter debugTag (debugParams lex)
                then
                let
                    (* If we are compiling with debugging on we call the debugger before raising the
                       exception in case we are tracing this exception. *)
                    open DEBUGGER
                    val {debugEnv=(ctEnv, rtEnv), ...} = context
                    fun exceptionFunction (rtEnv, exn) =
                        (debugFunction(debuggerFun lex, DebugException exn, decName ^ "-raise", location) ctEnv rtEnv; raise exn)
                    val exceptionCode = ADDRESS.toMachineWord exceptionFunction
                in
                    mkEval(mkConst exceptionCode, [mkTuple[rtEnv level, excPacket]])
                end
                else mkRaise excPacket
            end

        | HandleTree {exp, hrules, ...} =>
          (* Execute an expression in the scope of a handler *)
          let
            val handleExp = codegen (exp, context)          
            val handlerCode = codeMatch (c, hrules, Ldexc, codegen, true, context)
          in
            mkHandle (handleExp, handlerCode)
          end

        | While {test, body, ...} => mkWhile (codegen (test, context), codegen (body, context)) : codetree

        | Case {test, match, ...} =>
          (* The matches are made into a series of tests and
             applied to the test expression. *)
          let
            val testCode = codegen (test, context)
          in
            codeMatch (c, match, testCode, codegen, false, context)
          end

        | Andalso {first, second, ...} =>
          (* Equivalent to  if first then second else false *)
            mkCand (codegen (first,  context), codegen (second, context)) : codetree

        | Orelse {first, second, ...} =>
          (* Equivalent to  if first then true else second *)
            mkCor (codegen (first,  context), codegen (second, context))

        | Parenthesised(p, _) => codegen (p, context)

        | _ => (* empty and any others *)
           CodeZero : codetree

      end (* codegen *)

    (* Generate a function either as a free standing lambda expression or as
       a val rec declaration. *)
    and codeProc(c, location, polyVars,
                    cpContext as
                        {mkAddr=originalmkAddr, level=originalLevel, decName, debugEnv, ...}) =
    let
        fun getFnBody (Constraint {value, ...}) = getFnBody value
        |   getFnBody (Fn{matches, ...})  = matches
        |   getFnBody (Parenthesised(p, _)) = getFnBody p
        |   getFnBody _ = raise InternalError "getFnBody: not a constrained fn-expression";

        val f        = getFnBody c;
        (* This function comprises a new declaration level *)
        val nLevel =
            if null polyVars then originalLevel else newLevel originalLevel

        local
            val addresses = ref 1
        in
            fun fnMkAddr n = (! addresses) before (addresses := ! addresses + n)
        end

        val (firstPat, resType, argType) = 
            case f of 
                MatchTree {vars, resType = ref rtype, argType = ref atype, ...} :: _  => (vars, rtype, atype)
            |   _ => raise InternalError "codeProc: body of fn is not a clause list";

        val tupleSize = List.length(tupleWidth firstPat)
    in
        if tupleSize <> 1 andalso null polyVars
        then
        let
            (* If the first pattern is a tuple we make a tuple from the
               arguments and pass that in. Could possibly treat labelled 
               records in the same way but we have the problem of
               finding the size of the record.
               Currently, we don't apply this optimisation if the function is
               polymorphic. *)
            val newDecName : string = decName ^ "(" ^ Int.toString tupleSize ^ ")";
            val newDebugEnv = newDebugLevel debugEnv

            val fnLevel  = newLevel nLevel
            val argumentCode = mkArgTuple(0, tupleSize)
            val newContext =
                cpContext |> repDebugEnv newDebugEnv |> repNewLevel(newDecName, fnMkAddr, fnLevel)
            val mainProc =
                mkProc
                    (wrapFunctionInDebug
                        (codeAlt (c, f, argumentCode, newContext),
                        newDecName, argumentCode, argType, resType, location, newContext), 
                    tupleSize, newDecName, getClosure fnLevel, fnMkAddr 0)
    
            (* Now make a block containing the procedure which expects
               multiple arguments and an inline procedure which expects
               a single tuple argument and calls the main procedure after
               taking the tuple apart. *)
            val thisDec = multipleUses (mainProc, fn () => originalmkAddr 1, originalLevel);

            val resProc =  (* Result procedure. *)
                let
                    val nLevel = newLevel originalLevel
                in
                    mkInlproc 
                        (mkEval(#load thisDec nLevel,
                            List.map #1 (loadArgsFromTuple(List.tabulate(tupleSize, fn _ => GeneralType), singleArg))),
                        1, decName ^ "(1)", getClosure nLevel, 0)
                end
        in
            mkEnv(#dec thisDec, resProc)
        end
    
        else
        let (* No tuple or polymorphic. *)
            val newDecName : string  = decName ^ "(1)";
            val newDebug = newDebugLevel debugEnv
            val fnLevel  = newLevel nLevel
            val newContext =
                cpContext |> repDebugEnv newDebug |> repNewLevel(newDecName, fnMkAddr, fnLevel)

            val alt  = codeAlt (c, f, mkLoadArgument 0, newContext);
            (* If we're debugging add the debug info before resetting the level. *)
            val wrapped =
                wrapFunctionInDebug(alt, newDecName, mkLoadArgument 0, argType, resType,
                    location, newContext)
            val pr = mkProc (wrapped, 1, newDecName, getClosure fnLevel,  fnMkAddr 0)
        in
            if null polyVars then pr
            else mkProc(pr, List.length polyVars, newDecName^"(P)", getClosure nLevel, 0)
        end
    end (* codeProc *)


    (* Code-generates a sequence of declarations. *)
    and codeSequence ([]: parsetree list, leading, codeSeqContext, processBody): codeBinding list * debugenv =
            processBody(leading, codeSeqContext) (* Do the continuation. *)

    |   codeSequence ((firstEntry as FunDeclaration {dec, ...}) :: pTail, leading, codeSeqContext, processBody) =
        let
            val (firstDec, firstEnv) = codeFunBindings(dec, firstEntry, codeSeqContext)
        in
            codeSequence (pTail, leading @ firstDec, codeSeqContext |> repDebugEnv firstEnv, processBody)
        end

    |   codeSequence ((firstEntry as ValDeclaration {dec, location, ...}) :: pTail, leading, codeSeqContext as {lex, ...}, processBody) =
        let
            (* Check the types for escaped datatypes. *)
            local
                fun checkVars(ValBind{variables=ref vars, line, ...}) =
                    List.app(fn var => checkForEscapingDatatypes(valTypeOf var,
                        fn message => errorNear (lex, true, firstEntry, line, message))) vars
            in
                val () = List.app checkVars dec
            end
            (* Split the bindings into recursive and non-recursive.  These have to
               be processed differently. *)
            val (recBindings, nonrecBindings) =
                List.partition(fn ValBind{isRecursive, ...} => isRecursive) dec

            val nonRecCode = codeNonRecValBindings(nonrecBindings, firstEntry, codeSeqContext)
            val recCode =
                case recBindings of
                    [] => []
                |   _ => #1 (codeRecValBindings(recBindings, firstEntry, codeSeqContext))
            (* Construct the debugging environment by loading all variables. *)
            val vars = List.foldl(fn (ValBind{variables=ref v, ...}, vars) => v @ vars) [] dec
            val (decEnv, env) = makeDebugEntries (vars, codeSeqContext)
        in
            codeSequence (pTail, leading @ changeLine(location, codeSeqContext) @ nonRecCode @ recCode @ decEnv,
                    codeSeqContext |> repDebugEnv env, processBody)
        end

    |   codeSequence (Localdec {decs, body, varsInBody=ref vars, ...} :: pTail, leading, codeSeqContext, processBody) =
        let (* Local declarations only *)
            (* The debug environment needs to reflect the local...in...end structure but if
               there are local datatypes we need to process all subsequent declarations in the
               scope of the "stopper" we've put onto the typeVarMap. *)
            fun processTail(previous, newContext) =
            let
                (* The debug env for the tail is the original environment together with the
                   variables in the body, excluding variables in the local...in part. *)
                val (decEnv, resEnv) = makeDebugEntries (vars, codeSeqContext) (* Original context. *)
            in
                codeSequence (pTail, previous @ decEnv, newContext |> repDebugEnv resEnv, processBody)
            end
        in
            (* Process the declarations then the tail. *)
            codeSequence (decs @ body, leading, codeSeqContext, processTail)
        end

    |   codeSequence (ExDeclaration(tlist, _) :: pTail, leading,
                      codeSeqContext as {mkAddr, level, typeVarMap, lex, ...}, processBody) =
        let
            fun codeEx (ExBind{value=ref exval, previous, ... }) =
            let
                val ex     = exval;
                (* This exception is treated in the same way as a local
                  variable except that the value it contains is created
                  by generating a word on the heap. The address of this word
                  constitutes a unique identifier. Non-generative exception
                  bindings i.e. exception ex=ex'  merely copy the word from
                  the previous exception. *)
                val (lvAddr, lvLevel, exType) =
                    case ex of
                        Value{access=Local{addr, level}, typeOf, ...} => (addr, level, typeOf)
                    |   _ => raise InternalError "lvAddr"
            in
                lvAddr  := mkAddr 1;
                lvLevel := level;
   
                mkDec 
                 (! lvAddr,
                  case previous of
                      EmptyTree => 
                        (* Generate a new exception. This is a single
                           mutable word which acts as a token. It is a
                           mutable to ensure that there is precisely one
                           copy of it. It contains a function to print values
                           of the type so when we raise the exception we can print
                           the exception packet without knowing the type. *)
                        mkExIden (exType, level, typeVarMap)
                  | Ident{value=ref prevVal, location, ...} =>
                          (* Copy the previous value. N.B. We want the exception
                           identifier here so we can't call codegen. *)
                        codeVal (prevVal, level, typeVarMap, [], lex, location)
                  | _ => raise InternalError "codeEx"
                 )
            end  (* codeEx *);

            val exdecs = map codeEx tlist

            fun getValue(ExBind{value=ref exval, ...}) = exval
            val (debugDecs, newDebugEnv) = makeDebugEntries(map getValue tlist, codeSeqContext)
 
        in
            codeSequence (pTail, leading @ exdecs @ debugDecs, codeSeqContext |> repDebugEnv newDebugEnv, processBody)
        end (* ExDeclaration *)

    |   codeSequence (
            AbsDatatypeDeclaration {typelist, declist, equalityStatus = ref absEq, isAbsType, withtypes, ...} :: pTail,
            leading, codeSeqContext as {mkAddr, level, typeVarMap, debugEnv, lex, ...}, processBody) =
        let (* Code-generate the eq and print functions for the abstype first
               then the declarations, which may use these. *)
            (* The debugging environment for the declarations should include
               the constructors but the result shouldn't.  For the moment
               ignore the constructors. *)
            val typeCons = List.map(fn (DatatypeBind {tcon = ref tc, ...}) => tc) typelist
            val eqStatus = if isAbsType then absEq else List.map (tcEquality o tsConstr) typeCons

            local
                fun getConstrCode(DatatypeBind {tcon = ref (tc as TypeConstrSet(_, constrs)), typeVars, ...}, eqStatus) =
                let
                    (* Get the argument types or EmptyType if this is nullary. *)
                    fun getConstrType(Value{typeOf=FunctionType{arg, ...}, name, ...}) = (name, arg)
                    |   getConstrType(Value{name, ...}) = (name, EmptyType)
                    val constrTypesAndNames = List.map getConstrType constrs
                    val {constrs, boxed, size} = chooseConstrRepr(constrTypesAndNames, List.map TypeVar typeVars)
                in
                    ({typeConstr=tc, eqStatus=eqStatus, boxedCode=boxed, sizeCode=size}, constrs)
                end
            in
                val constrAndBoxSizeCode = ListPair.mapEq getConstrCode (typelist, eqStatus)
                val (tcEqBoxSize, constrsCode) = ListPair.unzip constrAndBoxSizeCode
            end

            local
                fun decConstrs(DatatypeBind {tcon = ref (TypeConstrSet(_, constrs)), ...}, reprs, (decs, debugEnv)) =
                let
                    (* Declare the constructors as local variables. *)
                    fun decCons(Value{access=Local{addr, level=lev}, ...}, repr) =
                        let
                            val newAddr = mkAddr 1
                        in
                            addr := newAddr;
                            lev := level;
                            mkDec(newAddr, repr)
                        end
                    |   decCons _ = raise InternalError "decCons: Not local"
                    val constrDecs = ListPair.map decCons (constrs, reprs)
                    val (newDecs, newDebug) =
                        makeDebugEntries(constrs, codeSeqContext |> repDebugEnv debugEnv)
                in
                    (constrDecs @ decs @ newDecs, newDebug)
                end
            in
                val (valConstrDecs: codeBinding list, constrDebugenv: debugenv) =
                    ListPair.foldl decConstrs ([], debugEnv) (typelist, constrsCode)
            end

            val typeFunctions =
                createDatatypeFunctions(tcEqBoxSize, mkAddr, level, typeVarMap)
            local
                (* Create debug entries for the type constructors and the new type ids. *)
                val (dataTypeDebugDecs, dataTypeDebugEnv) =
                    makeTypeConstrDebugEntries(typeCons, constrDebugenv, level, lex, mkAddr)
                val withTypeTypes = List.map(fn (TypeBind {tcon = ref tc, ...}) => tc) withtypes
                val (withTypeDebugDecs, withTypeDebugEnv) =
                    makeTypeConstrDebugEntries(withTypeTypes, dataTypeDebugEnv, level, lex, mkAddr)
            in
                val typeDebugDecs = dataTypeDebugDecs @ withTypeDebugDecs
                val typeDebugEnv = withTypeDebugEnv
            end

            (* Mark these in the type value cache.  If they are used in subsequent polymorphic IDs
               we must create them after this. *)
            val newTypeVarMap =
                markTypeConstructors(List.map tsConstr typeCons, mkAddr, level, typeVarMap)

            (* Process the with..end part. We have to restore the equality attribute for abstypes
               here in case getPolymorphism requires it. *)
            val () =
                if isAbsType
                then ListPair.appEq(fn(TypeConstrSet(tc, _), eqt) => tcSetEquality (tc, eqt)) (typeCons, absEq)
                else ()
            val (localDecs, newDebug) =
                codeSequence (declist, [],
                              codeSeqContext |> repDebugEnv typeDebugEnv |> repTypeVarMap newTypeVarMap,
                              fn (code, {debugEnv, ...}) => (code, debugEnv))
            val () =
                if isAbsType
                then List.app(fn TypeConstrSet(tc, _) => tcSetEquality (tc, false)) typeCons else ()

            (* Then the subsequent declarations. *)
            val (tailDecs, finalEnv) =
                codeSequence (pTail, [], codeSeqContext |> repDebugEnv newDebug |> repTypeVarMap newTypeVarMap, processBody)
        in
            (* The code consists of previous declarations, the value constructors, the type IDs,
               debug declarations for the types and value constructors, any type values created for
               subsequent polymorphic calls, declarations in with...end and finally code after
               this declaration within the same "let..in..end" block. *)
            (leading @ valConstrDecs @ typeFunctions @ typeDebugDecs @
              getCachedTypeValues newTypeVarMap @ localDecs @ tailDecs, finalEnv)
        end

    |   codeSequence (OpenDec {variables=ref vars, structures = ref structs, typeconstrs = ref types, ...} :: pTail,
                      leading, codeSeqContext as { level, lex, mkAddr, ...}, processBody) =
        let
                (* All we need to do here is make debugging entries. *)
            val (firstDec, firstEnv) = makeDebugEntries(vars, codeSeqContext)
            val (secondDec, secondEnv) = makeTypeConstrDebugEntries(types, firstEnv, level, lex, mkAddr)
            val (thirdDec, thirdEnv) = makeStructDebugEntries(structs, secondEnv, level, lex, mkAddr)
        in
            codeSequence (pTail, leading @ firstDec @ secondDec @ thirdDec, codeSeqContext |> repDebugEnv thirdEnv, processBody)
        end

    |   codeSequence (TypeDeclaration (typebinds, _) :: pTail, leading,
                      codeSeqContext as { debugEnv, level, lex, mkAddr, ...}, processBody) =
        let
            (* Just create debug entries for the type constructors. *)
            val typeCons = List.map(fn (TypeBind {tcon = ref tc, ...}) => tc) typebinds
            val (typeDebugDecs, typeDebugEnv) =
                makeTypeConstrDebugEntries(typeCons, debugEnv, level, lex, mkAddr)
        in
            codeSequence (pTail, leading @ typeDebugDecs, codeSeqContext |> repDebugEnv typeDebugEnv, processBody)
        end

    |   codeSequence (_ :: pTail, leading, (* Directive *) codeSeqContext, processBody) =
            codeSequence (pTail, leading, codeSeqContext, processBody)

    (* Code generate a set of fun bindings.  This is used for other function creation as
       well since it handles the most general case. *)
    and codeFunBindings(tlist: fvalbind list, near,
                        context as {decName, mkAddr, level, typeVarMap, debugEnv, lex, ...}) =
        let
            (* Check the types for escaped datatypes. *)
            local
                fun checkVars(FValBind{functVar=ref var, location, ...}) =
                    checkForEscapingDatatypes(valTypeOf var,
                        fn message => errorNear (lex, true, near, location, message))
            in
                val () = List.app checkVars tlist
            end
            (* Each function may result in either one or two functions
               actually being generated. If a function is not curried
               it will generate a single function of one argument, but
               if it is curried (e.g. fun f a b = ...) it will
               generate two mutually recursive functions. A function
               fun f a b = X will be translated into
               val rec f' = fn(a,b) => X and f = fn a => b => f'(a,b)
               with the second function (f) being inline. This allows
               the optimiser to replace references to f with all its
               arguments by f' which avoids building unneccessary
               closures. *)

            fun setValueAddress(
                  FValBind{functVar = ref(Value{access=Local{addr, level}, ...}), ...}, ad, lev) =
                    (addr := ad; level := lev)
            |   setValueAddress _ = raise InternalError "setValueAddress"

            (* Create a list of addresses for the functions.  This is the address
               used for the most general case.  Also set the variable addresses.
               These may be changed for polymorphic functions but will eventually
               be reset. *)

            val addressList = List.map (fn _ => mkAddr 2 (* We need two addresses. *)) tlist
            val () = ListPair.appEq (fn (t, a) => setValueAddress(t, a, level)) (tlist, addressList)

            (* Get the polymorphic variables for each function. *)
            local
                fun getPoly(FValBind{functVar = ref (Value{typeOf, ...}), ...}) =
                    filterTypeVars(getPolyTypeVars(typeOf, mapTypeVars typeVarMap))
            in
                val polyVarList = List.map getPoly tlist
            end

            (* Now we can process the function bindings. *)
            fun loadFunDecs ((fb as FValBind{numOfPatts = ref numOfPats, functVar = ref(Value{name, ...}),
                              clauses, argType = ref aType, resultType = ref resType, location, ...})::otherDecs,
                             polyVars :: otherPolyVars,
                             addr :: otherAddresses) =
                let
                    (* Make up the function, and if there are several mutually
                       recursive functions, put it in the vector. *)
                    val procName  = decName ^ name;
                    val nPolyVars = List.length polyVars
                    (*val _ =
                        print(concat[name, " is ", Int.toString nPolyVars, "-ary\n"])*)
                    (* Check that all the type-vars are in the list. *)
                    (*local
                        fun checkVars tv =
                            case List.find(fn t => sameTv(t, tv)) fdTypeVars of
                                SOME _ => ()
                            |   NONE => raise InternalError "Type var not found"
                    in
                        val _ = List.app checkVars polyVars
                    end*)

                    (* Produce a list of the size of any tuples or labelled records
                       in the first clause. Tuples in the first clause are passed as
                       separate arguments. We could look at the other clauses and only
                       pass them as separate arguments if each clause contains a tuple.

                       We can treat labelled records exactly like tuples here - we only
                       need to worry about the mapping from labels to tuple offsets
                       when we create the record (getting the order of evaluation right)
                       and in the pattern-matching code (extracting the right fields).
                       We don't have to worry about that here, because all we're doing
                       is untupling and retupling, taking care always to put the values
                       back at exactly the same offset we got them from. *)
                    val tupleSeq : argumentType list list =
                        case clauses of
                            (FValClause{dec= { args, ...}, ...} :: _) => List.map tupleWidth args
                        |   _ => raise InternalError "badly formed parse tree";

                    local
                        fun getResultTuple(FValClause{exp, ...}) = tupleWidth exp

                        val resultTuples =
                            List.foldl(fn(t, [_]) => getResultTuple t  | (_, s) => s) [GeneralType] clauses

                        (* If we're debugging we want the result of the function so we don't do this optimisation. *)
                        (* Now disable this optimisation completely.  The lower-level optimiser does this. *)
                        val resultTuple =
                            if (getParameter debugTag (debugParams lex)) then [GeneralType]
                            else if List.length resultTuples > 1 then [GeneralType] else resultTuples
                    in
                        val resTupleLength = List.length resultTuple
                        (*val _ = resTupleLength = 1 orelse raise InternalError "resTupleLength <> 1"*)
                        (* If there's a single argument return the type of that otherwise if we're tupling the
                           result is general. *)
                        val (resultType, extraArg) = case resultTuple of [one] => (one, 0) | _ => (GeneralType, 1)
                    end

                    (* Count the total number of arguments needed. *)
                    val totalArgs = List.foldl (op +) (extraArg+nPolyVars) (List.map List.length tupleSeq)

                    (* The old test was "totalArgs = 1", but that's not really
                       right, because we could have one genuine arg plus a
                       lot of "()" patterns. We now use the normal inlining
                       mechanism to optimise this (unusual) case too. *)
                    val noInlineFunction =
                        numOfPats = 1 andalso totalArgs = 1 andalso tupleSeq = [[GeneralType]] andalso resultType = GeneralType

                    (* Turn the list of clauses into a match. *)
                    fun clauseToTree(FValClause {dec={ args, ...}, exp, line, ...}) =
                        MatchTree
                        {
                            vars =
                                if numOfPats = 1 then hd args
                                else TupleTree{fields=args, location=line, expType=ref EmptyType},
                            exp  = exp,
                            location = line,
                            argType = ref badType,
                            resType = ref badType
                        }
                    val matches = map clauseToTree clauses

                    (* We arrange for the inner function to be called with
                    the curried arguments in reverse order, but the tupled
                    arguments in the normal order. For example, the
                    ML declaration:

                     fun g a b c              = ... gives the order <c,b,a>
                     fun g (a, b, c)          = ... gives the order <a,b,c>
                     fun g (a, b) c (d, e, f) = ... gives the order <d,e,f,c,a,b>

                   We want reverse the order of curried arguments to produce
                   better code. (The last curried argument often gets put
                   into the first argument register by the normal calling
                   mechanism, so we try to ensure that it stays there.)
                   We don't reverse the order of tupled arguments because
                   I'm still a bit confused about when a tuple is an
                   argument tuple (reversed?) and when it isn't (not reversed).

                   Just to add to this, if the function is polymorphic we
                   have to add the polymorphic arguments on at the end.

                 *)
                    local
                        (* Create the argument type list.  I'm sure this can be combined with the
                           next version of makeArgs but it's all too complicated. *)
                        fun makeArgs(parms, []) =
                            let
                                val polyParms = List.tabulate(nPolyVars, fn _ => GeneralType)
                                val resTupleSize = resTupleLength
                            in
                                if resTupleSize = 1
                                then parms @ polyParms
                                else parms @ polyParms @ [GeneralType]
                            end
                        |    makeArgs(parms, t::ts) = makeArgs (t @ parms, ts)
                    in
                        val argTypes = makeArgs ([], tupleSeq)
                    end

                    local
                        (* This function comprises a new declaration level *)
                        val nArgTypes = List.length argTypes
                        val fnLevel = newLevel level

                        val argList : codetree =
                            if numOfPats = 1
                            then mkArgTuple(nArgTypes-totalArgs, totalArgs-extraArg-nPolyVars)
                            else
                            let
                                fun makeArgs([],  _) = []
                                |   makeArgs(h::t, n) = mkArgTuple(nArgTypes-n-List.length h, List.length h) :: makeArgs(t, n + List.length h)
                            in
                                mkTuple (makeArgs(tupleSeq, extraArg+nPolyVars))
                            end

                        local
                            val addresses = ref 1
                        in
                            fun fnMkAddr n = (! addresses) before (addresses := ! addresses + n)
                        end

                        val innerProcName : string = 
                            concat ([procName,  "(" , Int.toString totalArgs, ")"]);
    
                        local
                            (* The poly args come after any result tuple. *)
                            val tupleOffset = if resTupleLength = 1 then 0 else 1
                            val argAddrs =
                                List.tabulate(nPolyVars, fn n => fn l => mkLoadParam(n+nArgTypes-nPolyVars-tupleOffset, l, fnLevel))
                            val mainTypeVars = ListPair.zipEq(polyVars, argAddrs)
                            (* Also need to add any variables used by other polymorphic
                               functions but not in the existing list.  This is only for very unusual cases. *)
                            fun addExtras (fPolyVars, pVarList) =
                            let
                                fun checkPolymorphism(fpVar, pVars) =
                                    if isSome(List.find (fn(t, _) => sameTv(t, fpVar)) mainTypeVars)
                                       orelse isSome(List.find (fn (t, _) => sameTv(t, fpVar)) pVars)
                                    then pVars else (fpVar, fn _ => defaultTypeCode) :: pVars
                            in
                                List.foldl checkPolymorphism pVarList fPolyVars
                            end
                            val extraEntries = List.foldl addExtras [] polyVarList
                        in
                            val typevarArgMap = mainTypeVars @ extraEntries
                            val newTypeVarMap =
                                extendTypeVarMap(typevarArgMap, fnMkAddr, fnLevel, typeVarMap)
                        end
                    
                        val fnContext =
                            context |> repDebugEnv(newDebugLevel debugEnv) |>
                               repNewLevel(innerProcName, fnMkAddr, fnLevel) |> repTypeVarMap newTypeVarMap

                        (* If we have (mutually) recursive references to polymorphic functions
                           we need to create local versions applied to the polymorphic variables.
                           We only need to consider functions that use the polymorphic variables
                           for this function.  If another function uses different variables it
                           can't be called from this one.  If it had been called from this any
                           type variables would have been fixed as monotypes or the type variables
                           of this function.
                           Except this is wrong in one case.  If one of the recursive calls involves
                           an exception (e.g. f (fn _ => raise Fail "") (or perhaps some other case
                           involving "don't care" polymorphic variables) it is possible to call a
                           function with more polymorphism. *)
                        local
                            fun createApplications(fVal::fVals, addr::addrList, [] :: polyVarList, otherDecs) =
                                (
                                    (* Monomorphic functions. *)
                                    setValueAddress(fVal, addr, level);
                                    createApplications(fVals, addrList, polyVarList, otherDecs)
                                )

                            |   createApplications(
                                    fVal::fVals, addr::addrList, fPolyVars ::polyVarList, otherDecs) =
                                let
                                    fun createMatches fpVar =
                                        case List.find (fn(t, _) => sameTv(t, fpVar)) typevarArgMap of
                                            SOME (_, codeFn) => codeFn fnLevel
                                        |   NONE => raise InternalError "createMatches: Missing type var"
                                    val polyArgs = List.map createMatches fPolyVars
                                    val newAddr = fnMkAddr 1
                                    val polyFn = mkLoad(addr, fnLevel, level)
                                        (* Set the address to this so if we use this function we pick
                                           up this declaration. *)
                                    val () = setValueAddress(fVal, newAddr, fnLevel);
                                    val newDecs = mkDec(newAddr, mkEval(polyFn, polyArgs)) :: otherDecs
                                in
                                    createApplications(fVals, addrList, polyVarList, newDecs)
                                end

                            |   createApplications(_, _, _, decs) = decs
                        in 
                            val appDecs =
                                if noInlineFunction then [] (* This may be directly recursive. *)
                                else createApplications (tlist, addressList, polyVarList, [])
                        end

                        val codeMatches = codeAlt (near, matches, argList, fnContext);

                        (* If the result is a tuple we try to avoid creating it by adding
                           an extra argument to the inline function and setting this to
                           the result. *)
                        val bodyCode =
                        if resTupleLength = 1
                        then codeMatches
                        else
                            (* The function sets the extra argument to the result
                               of the body of the function.  We use the last
                               argument for the container so that
                               other arguments will be passed in registers in
                               preference.  Since the container is used for the
                               result this argument is more likely to have to be
                               pushed onto the stack within the function than an
                               argument which may have its last use early on. *)
                            mkSetContainer(mkLoadParam(nArgTypes-1, fnLevel, fnLevel), codeMatches, resTupleLength)

                        (* If we're debugging add the debug info before resetting the level. *)
                        val wrapped =
                            wrapFunctionInDebug(bodyCode, procName, argList,
                                             aType, resType, location, fnContext)
                        val () =
                            if List.length argTypes = totalArgs then () else raise InternalError "Argument length problem"
                    in
                        val innerFun =
                            mkFunction{
                                body=mkEnv(getCachedTypeValues newTypeVarMap @ appDecs, wrapped),
                                argTypes=argTypes, resultType=resultType, name=innerProcName,
                                closure=getClosure fnLevel, numLocals=fnMkAddr 0}
                    end;
  
                    (* We now have a function which can be applied to the
                       arguments once we have them. If the function is curried 
                       we must make a set of nested inline procedures which
                       will take one of the parameters at a time. If all the
                       parameters are provided at once they will be
                       optimised away. *)
                   
                    val polyLevel =
                        if null polyVars then level else newLevel level

                    (* Make into curried functions *)
                    fun makeFuns(innerLevel, _, mkParms, []) =
                        let
                            (* Load a reference to the inner function. *)
                            val loadInnerFun = mkLoad (addr + 1, innerLevel, level)
                            val polyParms =
                                List.tabulate(nPolyVars, fn n => (mkLoadParam(n, innerLevel, polyLevel), GeneralType))
                            val resTupleSize = resTupleLength
                            val parms = mkParms innerLevel
                        in
                            (* Got to the bottom. - put in a call to the procedure. *)
                            if resTupleSize = 1
                            then (mkCall (loadInnerFun, parms @ polyParms, resultType), 0)
                            else (* Create a container for the result, side-effect
                                    it in the function, then create a tuple from it.
                                    Most of the time this will be optimised away. *)
                            let
                                val containerAddr = 0 (* In a new space *)
                                val loadContainer = mkLoadLocal containerAddr
                            in
                                (mkEnv(
                                    [mkContainer(containerAddr, resTupleSize,
                                       mkCall(loadInnerFun, parms @ polyParms @ [(loadContainer, GeneralType)], GeneralType))],
                                    mkTupleFromContainer(containerAddr, resTupleSize)),
                                 containerAddr+1 (* One local *))
                            end
                        end
                |    makeFuns(innerLevel, decName, mkParms, t::ts) =
                        let (* Make a function. *)
                            val nLevel = newLevel innerLevel
                            val newDecName : string = decName ^ "(1)"
                            (* Arguments from this tuple precede older arguments,
                               but order of arguments within the tuple is preserved. *) 
                            fun nextParms l = loadArgsFromTuple(t, mkLoadParam (0, l, nLevel)) @ mkParms l
                            val (body, lCount) = makeFuns (nLevel, newDecName, nextParms, ts)
                        in
                            (mkInlproc (body, 1, newDecName, getClosure nLevel, lCount), 0)
                        end (* end makeFuns *);

                    (* Reset the address of the variable. *)
                    val () = setValueAddress(fb, addr, level)
               in
                    if noInlineFunction
                    then (addr, innerFun) :: loadFunDecs(otherDecs, otherPolyVars, otherAddresses)
                    else
                    let
                        val (baseFun, _) = makeFuns (polyLevel, procName, fn _ => [], tupleSeq)
                        val polyFun =
                            if null polyVars then baseFun
                            else mkInlproc(baseFun, List.length polyVars, procName ^ "(P)", getClosure polyLevel, 0)
                    in
                        (* Return the `inner' procedure and the inline
                          functions as a mutually recursive pair. Try putting
                          the inner function first to see if the optimiser
                          does better this way. *)
                        (addr + 1, innerFun) :: (addr, polyFun) ::
                            loadFunDecs(otherDecs, otherPolyVars, otherAddresses)
                    end
               end (* loadFunDecs *)
            |   loadFunDecs _ = []

            val loaded = loadFunDecs(tlist, polyVarList, addressList)

            (* Set the final addresses in case they have changed.  N.B.  Do this before
               loading any debug references. *)
            val () = ListPair.appEq (fn (t, a) => setValueAddress(t, a, level)) (tlist, addressList)

            (* Construct the debugging environment by loading all variables.
               This won't be available recursively in the
               functions but it will be in the rest of the scope. *)
            val vars = map (fn(FValBind{functVar, ...}) => !functVar) tlist
            val (decEnv, newDebugEnv) = makeDebugEntries(vars, context)
            (* Check whether any of the functions were unreferenced. *)
            val _ =
                if getParameter reportUnreferencedIdsTag (debugParams lex)
                then reportUnreferencedValues(vars, lex)
                else ()

        in
            (* Put the declarations into a package of mutual decs. *)
            (mkMutualDecs loaded :: decEnv, newDebugEnv)
        end (* codeFunBindings *)

    (* Recursive val declarations.  Turn them into fun-bindings.  This avoids duplicating a lot
       of code and codeFunBindings does a lot of optimisation. *)
    and codeRecValBindings(valDecs, near, context) =
        let
            (* Turn this into a fun binding. *)
            fun valBindToFvalBind(ValBind{ exp, line, variables=ref vars, ...}, fVals) =
            let
                fun getMatches (Fn { matches: matchtree list, ... })  = matches
                |   getMatches (Constraint {value, ...}) = getMatches value
                |   getMatches (Parenthesised(p, _)) = getMatches p
                |   getMatches _       = raise InternalError "getMatches"

                fun matchTreeToClause(MatchTree{vars, exp, location, ...}) =
                let
                    val dec =
                        { ident = { name="", expType=ref EmptyType, location=location},
                            isInfix = false, args=[vars], constraint=NONE}
                in
                    FValClause{dec = dec, exp=exp, line=location }
                end
                
                val clauses = List.map matchTreeToClause (getMatches exp)
 
                fun mkFValBind(var as Value{typeOf, ...}) =
                let
                    val argType = mkTypeVar(generalisable, false, false, false)
                    and resultType = mkTypeVar(generalisable, false, false, false)
                    val () =
                        if isSome(unifyTypes(typeOf, mkFunctionType(argType, resultType)))
                        then raise InternalError "mkFValBind"
                        else ()
                in
                    FValBind { clauses=clauses, numOfPatts=ref 1, functVar=ref var,
                               argType=ref argType, resultType=ref resultType, location=line }
                end
            in
                fVals @ List.map mkFValBind vars
            end

            val converted = List.foldl valBindToFvalBind [] valDecs
        in
            codeFunBindings(converted, near, context)
        end (* codeRecValBindings *)

    (* Non-recursive val bindings. *)
    and codeNonRecValBindings(valBindings, near, originalContext: cgContext as { decName, typeVarMap, lex, isOuterLevel, ...}) =
        let
            (* Non-recursive val bindings. *)
            fun codeBinding (ValBind{dec=vbDec, exp=vbExp, line, variables=ref vars, ...}, otherDecs) =
            let (* A binding. *)
                (* Get a name for any functions. This is used for profiling and exception trace. *)
                val fName =
                    case vars of [] => "_" | _ => String.concatWith "|" (List.map valName vars)

                (* Does this contain polymorphism? *)
                val polyVarsForVals =
                    List.map(fn Value{typeOf, ...} =>
                                filterTypeVars (getPolyTypeVars(typeOf, mapTypeVars typeVarMap))) vars
                val polyVars = List.foldl(op @) [] polyVarsForVals
                val nPolyVars = List.length polyVars
                
                (* In almost all cases polymorphic declarations are of the form
                   val a = b   or  val a = fn ...  .  They can, though, arise in
                   pathological cases with arbitrary patterns and complex expressions.
                   If any of the variables are polymorphic the expression must have been
                   non-expansive.  That means that we can safely evaluate it repeatedly.
                   There's one exception: it may raise Bind. (e.g. val SOME x = NONE).
                   For that reason we make sure it is evaluated at least once.
                   We build the code as a function and then apply it one or more times.
                   This is really to deal with pathological cases and pretty well all
                   of this will be optimised away. *)
                val localContext as {level, mkAddr, typeVarMap, ...} =
                    if nPolyVars = 0
                    then originalContext
                    else
                    let
                        val addresses = ref 1
                        fun fnMkAddr n = (! addresses) before (addresses := ! addresses + n)
                        val fnLevel = newLevel (#level originalContext)
                        val argAddrs = List.tabulate(nPolyVars, fn n => fn l => mkLoadParam(n, l, fnLevel))
                        val argMap = ListPair.zipEq(polyVars, argAddrs)
                        val newTypeVarMap =
                            extendTypeVarMap(argMap, fnMkAddr, fnLevel, #typeVarMap originalContext)
                    in
                        originalContext |> repNewLevel(decName, fnMkAddr, fnLevel) |> repTypeVarMap newTypeVarMap
                    end

                val exp = codegen (vbExp, localContext |> repDecName (decName ^ fName ^ "-"))
                (* Save the argument in a variable. *)
                val decCode = multipleUses (exp, fn () => mkAddr 1, level)

                (* Generate the code and also check for redundancy and exhaustiveness. *)
                val (bindCode, exhaustive) =
                    codeBindingPattern(vbDec, #load decCode level, line, localContext)

                (* Report inexhaustiveness if necessary. *)
                val () =
                    if not exhaustive andalso not isOuterLevel
                    then errorNear (lex, false, near, line, "Pattern is not exhaustive.")
                    else ()

                (* Check for unreferenced variables. *)
                val () =
                    if getParameter reportUnreferencedIdsTag (debugParams lex)
                    then List.app (reportUnreferencedValue lex) (findVars vbDec [])
                    else ()
                
                val resultCode =
                    if nPolyVars = 0 then #dec decCode @ bindCode
                    else
                    let
                        fun loadVal(Value{access=Local{addr=ref add, ...}, ...}) = mkLoadLocal add
                        |   loadVal _ = raise InternalError "loadVal"

                        val outerAddrs = #mkAddr originalContext
                        and outerLevel = #level originalContext

                        (* Construct a function that, when applied, returns all the variables. *)
                        val fnAddr = outerAddrs 1
                        val resFunction =
                            mkDec(fnAddr,
                                mkInlproc(
                                    mkEnv(getCachedTypeValues typeVarMap @ #dec decCode
                                          @ bindCode, mkTuple(List.map loadVal vars)),
                                    nPolyVars, "(P)", getClosure level, mkAddr 0))

                        (* Apply the general function to the set of type variables using either the
                           actual type variables if they are in this particular variable or defaults
                           if they're not. *)
                        fun application(pVars, level) =
                        let
                            val nPVars = List.length pVars
                            val varNos = ListPair.zipEq(pVars, List.tabulate(nPVars, fn x=>x))
                            fun getArg argV =
                                case List.find (fn (v, _) => sameTv(v, argV)) varNos of
                                    SOME (_, n) => mkLoadParam(n, level, level)
                                |   NONE => defaultTypeCode
                        in
                            mkEval(mkLoad(fnAddr, level, outerLevel), List.map getArg polyVars)
                        end

                        (* For each variable construct either a new function if it is polymorphic
                           or a simple value if it is not (e.g. val (a, b) = (fn x=>x, 1)).
                           Set the local addresses at the same time. *)
                        fun loadFunctions(var::vars, polyV::polyVs, n) =
                            let
                                val vAddr = outerAddrs 1
                                val () =
                                    case var of
                                        Value{access=Local{addr, level}, ...} =>
                                            (addr := vAddr; level := outerLevel)
                                    |   _ => raise InternalError "loadFunctions"
                            in
                                mkDec(vAddr,
                                    case polyV of
                                        [] => (* monomorphic *) mkInd(n, application([], outerLevel))
                                    |   _ => (* polymorphic *)
                                        let
                                            val nPolyVars = List.length polyV
                                            val nLevel = newLevel outerLevel
                                        in
                                            mkInlproc(
                                                mkInd(n, application(polyV, nLevel)),
                                                nPolyVars, "(P)", getClosure nLevel, 0)
                                        end
                                ) :: loadFunctions(vars, polyVs, n+1)
                            end
                        |   loadFunctions _ = []

                        val loadCode = loadFunctions(vars, polyVarsForVals, 0)
                    in
                        (* Return the declaration of the function, a dummy application that will
                           force any pattern checking and raise a Match if necessary and the
                           declarations of the variables. *)
                        resFunction :: mkNullDec(application([], outerLevel)) :: loadCode
                    end
            in
                otherDecs @ resultCode
            end
        in
            List.foldl codeBinding [] valBindings
        end (* codeNonRecValBindings *)

    (* Code generates the parse tree. *)
    fun gencode
            (pt : parsetree, lex: lexan, debugEnv: debugenv, outerLevel, 
             mkOuterAddresses, outerTypeVarMap, structName: string, continuation) : codeBinding list * debugenv =
        codeSequence ([pt], [],
            {decName=structName, mkAddr=mkOuterAddresses, level=outerLevel, typeVarMap=outerTypeVarMap,
             debugEnv=debugEnv, lex=lex, lastDebugLine=ref 0, isOuterLevel = true},
             fn (code: codeBinding list, {debugEnv, typeVarMap, ...}) => continuation(code, debugEnv, typeVarMap))

    (* Types that can be shared. *)
    structure Sharing =
    struct
        type parsetree = parsetree
        and  lexan = lexan
        and  codetree = codetree
        and  environEntry = environEntry
        and  level = level
        and  typeVarMap = typeVarMap
        and  codeBinding = codeBinding
    end

end;

