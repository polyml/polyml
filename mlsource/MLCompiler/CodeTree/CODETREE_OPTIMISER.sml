(*
    Copyright (c) 2012,13 David C.J. Matthews

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

functor CODETREE_OPTIMISER(
    structure BASECODETREE: BaseCodeTreeSig

    structure CODETREE_FUNCTIONS: CodetreeFunctionsSig

    structure REMOVE_REDUNDANT:
    sig
        type codetree
        type loadForm
        val cleanProc : (codetree * (loadForm * int * int -> codetree) * int * bool array) -> codetree
        structure Sharing: sig type codetree = codetree and loadForm = loadForm end
    end

    structure DEBUG :
    sig
        val codetreeTag:            bool Universal.tag (* If true then print the original code. *)
        val maxInlineSizeTag:       int  Universal.tag
        val getParameter : 'a Universal.tag -> Universal.universal list -> 'a
    end

    structure PRETTY : PRETTYSIG

    structure BACKEND:
    sig
        type codetree
        type machineWord = Address.machineWord
        val codeGenerate: codetree * int * Universal.universal list -> unit -> machineWord
        structure Sharing : sig type codetree = codetree end
    end

    sharing 
        BASECODETREE.Sharing 
    =   CODETREE_FUNCTIONS.Sharing
    =   REMOVE_REDUNDANT.Sharing
    =   PRETTY.Sharing
    =   BACKEND.Sharing

) :
    sig
        type codetree and envType and codeBinding
        val codetreeOptimiser: codetree  * Universal.universal list ->
            { numLocals: int, general: codetree, bindings: codeBinding list, special: (codetree * envType) option }
        structure Sharing: sig type codetree = codetree and envType = envType and codeBinding = codeBinding end
    end
=
struct
    open BASECODETREE
    open Address
    open CODETREE_FUNCTIONS
    open StretchArray
    
    infix 9 sub
    
    exception InternalError = Misc.InternalError

    val isConstnt    = fn (Constnt _)    => true | _ => false

    val word0 = toMachineWord 0;
    val word1 = toMachineWord 1;
  
    val False = word0;   
    val True  = word1;   

    fun mkGenLoad  (i1, i2, bl) =
        if i2 = 0 andalso i1 >= 0 andalso bl
        then Extract(LoadLocal i1)
        else if i2 < 0 then raise InternalError "mkGenLoad: level is negative"
        else Extract(LoadLegacy{addr  = i1, level = i2, fpRel = bl})

  (* Processing each expression results in a "optVal" value. This contains a 
     "general" value which can be used anywhere and a "special" value which
     provides optimisations of inline procedures and tuples. "environ" is a
     procedure for mapping addresses in "special" if it is used and "decs" is 
     any declarations needed by either "general" or "special". The reason for
     returning both "general"  and "special" is so that we only create a
     tuple or a procedure once. In the case of a tuple "special" contains
     code to generate the tuple from its elements and is provided so that
     operations which select from the tuple can be optimised into loading
     the element. "General" will contain code to generate the tuple, or in
     the case of a declaration of a tuple, will contain a "load" instruction 
     to get the value.
  *)
    val errorEnv = EnvType (fn _ => raise InternalError "error env")
 
    (* Wrap up the general and any decs. *)
    fun getGeneral (gen, [], _) = gen
    |   getGeneral (gen, decs, _) = mkEnv(decs, gen)


(************************************************************************)

  val initTrans = 10; (* Initial size of arrays. *)

(*****************************************************************************)
(*                  changeLevel                                              *)
(*****************************************************************************)
  (* Change the level of an entry if necessary. This *)
  (* happens if we have a function inside an inline function. *)
    fun changeLevel entry 0 = entry (* No change needed*)
    |   changeLevel (entry: codetree * (codetree * envType) option) correction =
        let
            fun changeL(ext as Extract(LoadLegacy{addr, level, fpRel, ...}), nesting) =
                if level < 0 then raise InternalError "level is negative"
                else if level < nesting
                    (* Local variables don't need to be changed. *)
                then ext (* It's local to the function(s) we're processing. *)
                else mkGenLoad (addr, level + correction, fpRel)

            |   changeL(ext as Extract(LoadLocal addr), nesting) =
                if nesting > 0
                then ext
                else mkGenLoad (addr, correction, true)

            |   changeL(Extract _, _) = raise InternalError "changeL: TODO"

            |   changeL(Lambda{body, isInline, name, closure, argTypes, resultType,
                             level, localCount, ...}, nesting) =
                (* We have to process the free variables in the function.  Since we
                   don't currently keep the closure we have to process the whole
                   body.  *)
                Lambda{body = changeL(body, nesting+1), isInline = isInline,
                       name = name, closure = closure, argTypes = argTypes, resultType=resultType,
                       level = level + correction, localCount=localCount}

            |   changeL(Eval{function, argList, resultType}, nesting) =
                Eval{function = changeL(function, nesting),
                     argList = map (fn (a, t) => (changeL(a, nesting), t)) argList,
                     resultType=resultType}

            |   changeL(Indirect{ base, offset }, nesting) =
                Indirect{base = changeL(base, nesting), offset = offset }

            |   changeL(Newenv(decs,exp), nesting) =
                let
                    fun changeLDec(Declar{value, addr, ...}, nesting) = mkDec(addr, changeL(value, nesting))
                    |   changeLDec(NullBinding exp, nesting) = NullBinding(changeL(exp, nesting))
                    |   changeLDec _ = raise InternalError "changeLDec: Unknown code"
                in
                    Newenv(map(fn d => changeLDec(d, nesting)) decs, changeL(exp, nesting))
                end

            |   changeL(c as Container _, _) = c

            |   changeL(TupleFromContainer(container, size), nesting) =
                    TupleFromContainer(changeL(container, nesting), size)

            |   changeL(_, _) =
                (* The code we produce in these inline functions is very limited. *)
                   raise InternalError "changeL: Unknown code"

            val (gen, spec) = entry
        in
            case gen of
                gen as Extract _ => (changeL(gen, 0), spec)

            |   Constnt _ => entry

            |   gen as Lambda _ =>
                    (* optimiseLambda returns the Lambda as the general result for
                       functions that were explicitly inlined by the front-end. *)
                     (changeL(gen, 0), spec)
            
            | _ => raise InternalError "changeLevel: Unknown entry"
        end

    (* Code-generate a function or set of mutually recursive functions that contain no free variables
       and run the code to return the address.  This allows us to further fold the address as
       a constant if, for example, it is used in a tuple. *)
    local
        exception Interrupt = Thread.Thread.Interrupt
    in
        fun codeGenerateToConstant debugSwitches (pt, localCount) =
        let
            val () =
                if DEBUG.getParameter DEBUG.codetreeTag debugSwitches
                then PRETTY.getCompilerOutput debugSwitches (BASECODETREE.pretty pt) else ()

            val code = BACKEND.codeGenerate(pt, localCount, debugSwitches)
        in
            Constnt (code()) (* Evaluate it and convert any exceptions into Raise instrs. *)
                handle Interrupt => raise Interrupt (* Must not handle this *)
                | exn => Raise (Constnt(toMachineWord exn))
        end
    end

    (* Call and RTS function to fold constants.  The function must be safe to evaluate "early". *)
    fun callRTSFunction(rtsCall: machineWord, argList) =
    let
        exception Interrupt = Thread.Thread.Interrupt
        val _ = (isIoAddress(toAddress rtsCall) andalso earlyRtsCall rtsCall)
                    orelse raise InternalError "not early rts"

        (* Turn the arguments into a vector.  *)
        val argVector =
            case makeConstVal(Recconstr argList) of
                Constnt w => w
            |   _ => raise InternalError "makeConstVal: Not constant"

        (* Call the function.  If it raises an exception (e.g. divide
           by zero) generate code to raise the exception at run-time.
           We don't do that for Interrupt which we assume only arises
           by user interaction and not as a result of executing the
           code so we reraise that exception immediately. *)
    in
        Constnt (call(toAddress rtsCall, argVector))
            handle exn as Interrupt => raise exn (* Must not handle this *)
            | exn => Raise (Constnt(toMachineWord exn))
    end

    (* Optimiser context.  The optimiser may add or remove bindings so the "name" space
       is different for codetree elements before and after processing.  The "Old" environment
       is the space for addresses before processing and the "New" environment the space
       for addresses after processing.  Normally items are looked up in the "Old" environment
       but when we create a "special" entry the context (environ) for it may be the Old or
       New lookup function depending on whether we use the unprocessed or processed
       codetree as the "special" entry. *)
    type nameEnv = 
    { 
        (* Look-up.  This takes a "load" entry as used in Extract, the nesting depth of
           the point where the result value will be used and the "level" count in
           the loadForm. *)
        lookup: envType,
        enter: int * (codetree * (codetree * envType) option) -> unit
    }
    
    fun errorEnter(_: int * (codetree * (codetree * envType) option)): unit  = raise InternalError "errorEnter"

    type optContext =
    {
         oldEnv: nameEnv,
         newEnv: nameEnv,
         nestingOfThisFunction : int,
         spval : int ref,
         recursiveExpansions:
            ((codetree * argumentType) list * bool * int -> (codetree * argumentType) list) option,
         loopFilter: (codetree * argumentType) list -> (codetree * argumentType) list,
         debugArgs: Universal.universal list
    }

    fun loopFilterError _ = raise InternalError "Loop instruction without BeginLoop"

    (* Prepares a binding for entry into a look-up table.  Returns the entry
       to put into the table together with any bindings that must be made.
       If the general part of the optVal is a constant we can just put the
       constant in the table. If it is a load (Extract) it is just renaming
       an existing entry so we can return it.  Otherwise we have to make
       a new binding and return a load (Extract) entry for it. *)
    fun makeNewDecl((gen, decs, spec), { spval, ...}: optContext) =
        case gen of
            Constnt _ => ((gen, spec), decs)
                (* No need to create a binding for a constant. *)

        |   Extract (LoadLegacy{ level = 0, ...}) => ((gen, spec), decs)
                (* Declaration is simply giving a new name to a local
                   - can ignore this declaration. *) 
        |   Extract (LoadLocal _) => ((gen, spec), decs)
        |   Extract (LoadArgument _) => ((gen, spec), decs)

        |   gen =>
            let (* Create a binding for this value. *)
                val decSpval = ! spval before (spval := !spval + 1)
            
                (* The table entry is similar to the result of the expression except
                    that the declarations are taken off and put into the containing
                    block, and the general value is put into a local variable and
                    replaced by an instruction to load from there. If the special
                    is a non-inline function it is removed. *)
            in
                ((mkLoad (decSpval, 0), spec), decs @ [mkDec(decSpval, gen)])
            end

     (* The main optimisation routine. *)
    (* Returns only the general value from an expression. *)
    and generalOptimise(pt, tailCall, context) = getGeneral(optimise(pt, tailCall, context))

    and general context pt = generalOptimise(pt, false, context)

    and optimise (pt as MatchFail, _, _) = (pt, [], NONE)

     |  optimise (AltMatch(a, b), _, context) = (AltMatch(general context a, general context b), [], NONE)
       
     |  optimise (Eval {function, argList, resultType}, tailCall,
                    context: optContext as {nestingOfThisFunction, recursiveExpansions, ...}) =
        let
            (* Function call - This may involve inlining the function. *)

            (* Get the function to be called and see if it is inline or
               a lambda expression. *)
            val funct as (genFunct, decsFunct, specFunct) = optimise(function, false, context)

            (* There are essentially two cases to consider - the function
               may be "inline" in which case it must be expanded as a block,
               or it must be called. *)
        in
            case (function, specFunct, genFunct) of
                (Extract(LoadLegacy{addr=0, fpRel = false, level = 0, ...}), _, _) =>
                     (* We're already expanding this function - don't recursively expand
                        it.  Either loop or generate a function call. *)
                let
                    val copiedArgs = map (fn (arg, argType) => (general context arg, argType)) argList
                    val evCopiedCode = 
                        case recursiveExpansions of
                            (* This is a recursive call to a function we're expanding.
                               Is it tail recursive?  We may have several levels of
                               expansion. *)
                            SOME filterArgs =>
                                if tailCall
                                then Loop (filterArgs(copiedArgs, true, nestingOfThisFunction))
                                else Eval {function = genFunct,
                                        (* N.B. We still need to use "filterArgs" even if this is a call. *)
                                        argList = filterArgs(copiedArgs, false, nestingOfThisFunction),
                                        resultType=resultType}
                            (* Not a recursive expansion. *)
                        |   NONE => Eval {function = genFunct, argList = copiedArgs, resultType=resultType}
                in
                    (evCopiedCode, decsFunct, NONE)
                end

            |   (Extract(LoadLegacy{addr=0, fpRel = false, ...}), _, _) =>
                    (* Recursive call to a function at an outer level.  Cannot loop but
                       we mustn't expand it inline. *)
                let
                    val copiedArgs = map (fn (arg, argType) => (general context arg, argType)) argList
                    val evCopiedCode =
                        Eval {function = genFunct, argList = copiedArgs, resultType=resultType}
                in
                    (evCopiedCode, decsFunct, NONE)
                end

            |   (Extract(LoadArgument _), _, _) => raise InternalError "Eval: TODO"
            |   (Extract(LoadClosure _), _, _) => raise InternalError "Eval: TODO"
            |   (Extract LoadRecursive, _, _) => raise InternalError "Eval: TODO"

            |   (_, SOME(Lambda (lambda as { isInline = MaybeInline, ...}), env), _) =>
                    inlineInlineOnlyFunction(funct, lambda, env, argList, tailCall, context)

            |   (_, SOME(Lambda (lambda as { isInline = OnlyInline, ...}), env), _) =>
                    inlineInlineOnlyFunction(funct, lambda, env, argList, tailCall, context)

            |   (_, SOME(Lambda lambda, env), _) =>
                    inlineSmallFunction(funct, function, lambda, env, argList, resultType, context)

            | (_, _, gen as Constnt w) => (* Not inlinable - constant function. *)
                let
                    val copiedArgs = map (fn (arg, argType) => (general context arg, argType)) argList
                    (* If the function is an RTS call that is safe to evaluate immediately and all the
                       arguments are constants evaluate it now. *)
                    val evCopiedCode = 
                        if isIoAddress(toAddress w) andalso earlyRtsCall w andalso List.all (isConstnt o #1) copiedArgs
                        then callRTSFunction(w, List.map #1 copiedArgs)
                        else Eval {function = gen, argList = copiedArgs, resultType=resultType}
                in
                    (evCopiedCode, decsFunct, NONE)
                end

            | (_, _, gen) => (* Anything else. *)
                let
                    val copiedArgs = map (fn (arg, argType) => (general context arg, argType)) argList
                    val evCopiedCode = 
                        Eval {function = gen, argList = copiedArgs, resultType=resultType}
                in
                    (evCopiedCode, decsFunct, NONE)
                end

        end (* Eval { } *)
        
     |  optimise (Extract ext, _, {oldEnv = {lookup=EnvType lookupOldAddr, ...}, nestingOfThisFunction, ...}: optContext) =
        let
            val level = case ext of LoadLegacy {level, ...} => level | _ => 0
            val (gen, spec) = lookupOldAddr (ext, nestingOfThisFunction, level)
        in
            (gen, [], spec)
        end

     |  optimise (pt as Constnt _, _, _) = (pt, [], NONE) (* Return the original constant. *)
     
     |  optimise(Lambda lambda, _, context) =
        let
            val (general, special) = optimiseLambda(lambda, context)
        in
            (general, [], special)
        end
           
     |  optimise (BeginLoop{loop=body, arguments=args, ...}, tailCall,
              context as { newEnv, oldEnv as {enter = enterOldDec, ...}, nestingOfThisFunction, spval,
                            recursiveExpansions, debugArgs, ...}) =
        let
             (* We could try extracting redundant loop variables but for
                the time being we just see whether we actually need a loop
                or not.  This is needed if we have already constructed a loop
                from a recursive inline function and then expand it in another
                function.  If some of the loop variables are now constants we may
                optimise away the loop altogether. e.g. equality for lists where
                we actually have   if x = nil then ... *)
             val loops = ref false
             fun filterArgs a = (loops := true; a)

             (* First process as though it was not a BeginLoop but just a
                set of declarations followed by an expression. *)
             val _ =
                optimise
                  (mkEnv(List.map (Declar o #1) args, body), true,
                  {newEnv = newEnv, oldEnv = oldEnv,
                   nestingOfThisFunction=nestingOfThisFunction,
                   spval=spval,
                   recursiveExpansions=recursiveExpansions,
                   loopFilter=filterArgs,
                   debugArgs=debugArgs})
        in
            if not (! loops)
            then (* The Loop instructions have been optimised away.  Since there's
                    no BeginLoop we can reprocess it with the surrounding
                    tail recursion. *)
                optimise(mkEnv(List.map (Declar o #1) args, body), tailCall, context)
            else (* It loops - have to reprocess. *)
            let
                (* The arguments to the functions are Declar entries but they
                   must not be optimised. *)
                fun declArg({addr, value, ...}, typ) =
                    let
                        val optVal = optimise(value, false, context)
                        val decSpval = ! spval
                        val _ = spval := decSpval + 1
                        val optV = mkLoad (decSpval, 0)
                    in
                        enterOldDec(addr, (optV, NONE));
                        ({addr = decSpval, value = getGeneral optVal}, typ)
                    end
                 val declArgs = map declArg args
                 val beginBody =
                    optimise(body, true,
                      {newEnv = newEnv, oldEnv = oldEnv,
                       nestingOfThisFunction=nestingOfThisFunction, spval=spval,
                       recursiveExpansions=recursiveExpansions,
                       loopFilter=filterArgs,
                       debugArgs=debugArgs})
            in
                (BeginLoop {loop=getGeneral beginBody, arguments=declArgs}, [], NONE)
            end
        end

     |  optimise (Loop args, _, context as { loopFilter, ... }) =
            (* The Loop instruction should be at the tail of the
               corresponding BeginLoop. *)
            let
                fun gen(c, t) = (general context c, t)
            in
                (Loop (loopFilter(map gen args)), [], NONE)
            end
          
     |  optimise (Raise x, _, context) = (Raise (general context x), [], NONE)
         
     |  optimise (Cond(condTest, condThen, condElse), tailCall, context as { spval, nestingOfThisFunction, ...}) =
        let
            val insFirst = general context condTest
        in
            (* If the condition is a constant we need only
               return the appropriate arm. *)
            case insFirst of
                Constnt testResult =>
                    if wordEq (testResult, False) (* false - return else-part *)
                    then (* if false then x else y == y *)
                        optimise(condElse, tailCall, context)
                    (* if true then x else y == x *)
                    else optimise(condThen, tailCall, context)  (* return then-part *)
            
            |   _ => (* Condition is not a constant. *)
                let
                    (* Perhaps the "if" is really a simpler expression?
                       Unfortunately, we don't know whether we're returning
                       a boolean result here so we can't optimise to
                       andalso/orelse but we can at least look for the
                       case where both results are constants. *)
                    val insSecond as (thenGen, thenDecs, _) = optimise(condThen, tailCall, context)
                    val insThird as (elseGen, elseDecs, _)  = optimise(condElse, tailCall, context)

                    (* If we have tuples on both arms we can probably combine them.  If we have
                       a container on a branch we will have declared it at some point so we
                       have to remove that declaration and "lift" it outside the "if-". *)
                    fun combineTuples(containerAddr, thenAddr, elseAddr, thenRec, elseRec, size) =
                    let
                        fun replaceContainerDec([], _) =
                            raise InternalError "replaceContainerDec"
                         |  replaceContainerDec((hd as Declar{addr, ...})::tl, ad)=
                                if addr = ad
                                then
                                    (* Found the declaration. If we are using this
                                       container address we remove this declaration
                                       because we are using the container created outside
                                       the if-.  If we have containers on both branches
                                       and we are using a different container on this branch
                                       we replace the creation of the continer here with a
                                       reference to the outer container. *)
                                    if addr = containerAddr
                                    then tl
                                    else mkDec(addr, mkLoad(containerAddr, 0)) :: tl
                                else hd :: replaceContainerDec(tl, ad) 
                        | replaceContainerDec(hd :: tl, ad) =
                                hd :: replaceContainerDec(tl, ad)

                        fun createBranch(recEntries, decEntries, cAddr) =
                            case cAddr of
                                SOME ad => (* We have a container on that branch ... *)
                                    (* TODO: We add a CodeZero here to balance things up but
                                       really we have a NullBinding(SetContainer ...) at the
                                       end of the list which we could extract. *)
                                    mkEnv(replaceContainerDec(decEntries, ad), CodeZero)
                            |   NONE => 
                                    mkEnv(decEntries,
                                        mkSetContainer(
                                            mkLoad(containerAddr, 0), Recconstr recEntries,
                                            size))

                        val thenPart = createBranch(thenRec, thenDecs, thenAddr)
                        and elsePart = createBranch(elseRec, elseDecs, elseAddr)
                        (* The result is a block which declares the container, side-effects it
                           in the "if" and makes a tuple from the result.  If we're lucky
                           the resulting tuple will be optimised away. *)
                        (* This code is the same as that used to optimise TupleFromContainer
                           and is designed to allow us to optimise away the tuple creation
                           if we use the individual fields. *)
                        val baseAddr = !spval before spval := !spval + size
                        val specialDecs =
                            List.tabulate(size,
                                fn n => mkDec(n+baseAddr, mkInd(n, mkLoad(containerAddr, 0))))
                        val specialEntries = List.tabulate(size, fn n => mkLoad(n+baseAddr, 0))
                        fun env (l:loadForm, depth, _) =
                            changeLevel (Extract l, NONE) (depth - nestingOfThisFunction)
                        val recAddr = !spval before spval := !spval + 1
                    in
                        (   mkLoad(recAddr, 0),
                            mkDec(containerAddr, Container size) ::
                                  NullBinding(mkIf(insFirst, thenPart, elsePart)) ::
                                    (specialDecs @ [mkDec(recAddr, TupleFromContainer(mkLoad(containerAddr, 0), size))]),
                            SOME(Recconstr specialEntries, EnvType env)
                         )
                    end (* combineTuples *)
                in
                    (* Optimise various cases depending on what the then- and else-parts return. *)
                    case (thenGen, thenDecs, elseGen, elseDecs) of
                        (second as Constnt c2, [], third as Constnt c3, []) =>
                            (* The results of the then- and else-parts are just constants. *)
                            (* if x then y else y == (x; y) *)
                        if wordEq (c2, c3)
                        then if sideEffectFree insFirst
                        then insSecond
                        else (* Must put insFirst in decs, so it gets executed *)
                            (second, [NullBinding insFirst], NONE)
                  
                        (* if x then true else false == x *)
                        else if wordEq (c2, True) andalso wordEq (c3, False)
                        then (insFirst, [], NONE)
              
                        (* if x then false else y == not x *)
                        else if wordEq (c2, False) andalso wordEq (c3, True)
                        then (mkNot insFirst, [], NONE)
              
                        else (* can't optimise *) (mkIf (insFirst, second, third), [], NONE)

                    |   (Recconstr thenRec, _, Recconstr elseRec, _) =>
                        (* Both tuples - are they the same size?  They may not be if they
                           are actually datatypes. *)
                        if List.length thenRec = List.length elseRec
                        then (* We can transform this into an operation which creates space
                                on the stack, side-effects it and then picks up the result
                                from it. *)
                        let
                            val size = List.length thenRec (* = List.length elseRec *)
                            (* Create a new address for the container. *)
                            val containerAddr = let val ad = !spval in spval := ad + 1; ad end
                        in
                            combineTuples(containerAddr, NONE, NONE, thenRec, elseRec, size)
                        end
                        else (* Different sizes - use default. *)
                            (mkIf (insFirst, getGeneral insSecond, getGeneral insThird), [], NONE)

                    |   (TupleFromContainer(Extract(LoadLegacy{addr=thenAddr,level=0,fpRel=true, ...}), thenSize), _,
                         TupleFromContainer(Extract(LoadLegacy{addr=elseAddr,level=0,fpRel=true, ...}), elseSize), _) =>
                        (* Have both been converted already.  If we are returning a tuple from
                           a container the container must be declared locally. *)
                        if thenSize = elseSize
                        then (* We can combine the containers.  We can't if these are actually
                                datatypes in which case they could be different sizes. *)
                        let
                            (* If we have already transformed this we will have a
                               declaration of a container somewhere in the list. *)
                            (* Use the address which has already been allocated for the else part.
                               That makes it easier for the subsequent pass to convert this into
                               a "case" if appropriate. *)
                            val containerAddr = elseAddr
                        in
                            combineTuples(containerAddr, SOME thenAddr, SOME elseAddr, [], [], thenSize)
                        end
                        else (* Different sizes - use default. *)
                            (mkIf (insFirst, getGeneral insSecond, getGeneral insThird), [], NONE)

                    |   (TupleFromContainer(Extract(LoadLegacy{addr=thenAddr,level=0,fpRel=true, ...}), thenSize), _,
                         Recconstr elseRec, _) =>
                        (* The then-part has already been converted *)
                        if thenSize = List.length elseRec
                        then combineTuples(thenAddr, SOME thenAddr, NONE, [], elseRec, thenSize)
                        else (* Different sizes - use default. *)
                            (mkIf (insFirst, getGeneral insSecond, getGeneral insThird), [], NONE)

                    |   (Recconstr thenRec, _,
                         TupleFromContainer(Extract(LoadLegacy{addr=elseAddr,level=0,fpRel=true, ...}), elseSize), _) =>
                        (* The else-part has already been converted *)
                        if elseSize = List.length thenRec
                        then
                            combineTuples(elseAddr, NONE, SOME elseAddr, thenRec, [], elseSize)
                        else (* Different sizes - use default. *)
                            (mkIf (insFirst, getGeneral insSecond, getGeneral insThird), [], NONE)

                    |   (TupleFromContainer(Extract _, _), _, _, _) => raise InternalError "combinetuples: TODO"
                    
                    |   (_, _, TupleFromContainer(Extract _, _), _) => raise InternalError "combinetuples: TODO"

                     |   _ => (* Not constants or records. *)
                            (mkIf (insFirst, getGeneral insSecond, getGeneral insThird), [], NONE)
                end
        end (* Cond ... *)
         
     |  optimise (Newenv(envDecs, envExp), tailCall,
                  context: optContext as 
                    {oldEnv = {enter = enterOldDec, lookup = EnvType lookupOldAddr },
                     newEnv = { enter =  enterNewDec, lookup = EnvType lookupNewAddr}, 
                     spval, nestingOfThisFunction, debugArgs, recursiveExpansions, loopFilter }) =
        let
            (* Recurses down the list of declarations and expressions processing
               each, and then reconstructs the list on the way back. *)
            fun copyDeclarations []  =
                (* End of the list - process the result expression. *)
                    optimise(envExp, tailCall, context)

            |   copyDeclarations (Declar{addr=caddr, value, ...} :: vs) = 
                let
                    (* Add the declaration to the table. *)
                    val (optV, dec) = makeNewDecl(optimise(value, false, context), context)
                    val () = enterOldDec(caddr, optV)                  
                    (* Deal with the rest of the block. *)
                    val (rGen, rDecs, rSpec) = copyDeclarations vs
                in
                    (rGen, dec @ rDecs, rSpec)
                end

            |   copyDeclarations (RecDecs mutualDecs :: vs) = 
                (* Mutually recursive declarations. Any of the declarations may
                   refer to any of the others. They should all be lambdas.

                   The front end generates functions with more than one argument
                   (either curried or tupled) as pairs of mutually recursive
                   functions.  The main function body takes its arguments on
                   the stack (or in registers) and the auxiliary inline function,
                   possibly nested, takes the tupled or curried arguments and
                   calls it.  If the main function is recursive it will first
                   call the inline function which is why the pair are mutually
                   recursive.
                   As far as possible we want to use the main function since that
                   uses the least memory.  Specifically, if the function recurses
                   we want the recursive call to pass all the arguments if it
                   can.  We force the inline functions to be macros while
                   processing the non-inline functions and then process the
                   inlines. DCJM 23/1/01. *)
                let
                    (* Split the inline and non-inline functions. *)
                    val (inlines, nonInlines) =
                        List.partition (
                            fn {lambda = { isInline=MaybeInline, ...}, ... } => true | _ => false) mutualDecs
                    
                    (* Go down the non-inline functions creating new addresses
                       for them and entering them in the table. *)
                    val startAddr = !spval
                    val addresses =
                        map (fn {addr, ... } =>
                            let
                                val decSpval   = !spval;
                            in
                                enterOldDec (addr, (mkLoad (decSpval, 0), NONE));
                                spval := !spval + 1;
                                decSpval
                            end)
                        nonInlines
                    val endAddr = !spval
                    (* We can now process the inline functions.  Since these
                       can't be directly recursive we don't need to do anything
                       special. *)
                    local
                        fun appLambda {lambda, addr, ... } =
                            case optimise(Lambda lambda, false, context) of
                                (gen, [], spec) => enterOldDec(addr, (gen, spec))
                            |   _ => raise InternalError "appLambda: decs not empty"
                    in
                        val () = List.app appLambda inlines
                    end

                    (* Next process the non-inlines.  We really want to be able to
                       compile the functions now if we can and get a constant for
                       the code address.  We can do that for functions which make
                       no non-local references or whose non-local references are
                       by means of constants.  For non-recursive declarations this
                       is easy since an earlier declaration cannot depend on a later
                       one but for mutually recursive declarations we don't know
                       the dependencies.
                       The simple case is where we have a function which does not
                       depend on anything and so can be code-generated in the Lambda
                       case.  Code-generating that may allow others to be code-generated.
                       Another case is where the functions depend on each other but not
                       on anything else.  We can compile them together but not
                       individually.  There are various versions of this second case.
                       The only one we consider here is if all the (non-constant)
                       functions are of that form in which case we process the
                       whole mutually-recursive declaration. *)
                    val hasNonLocalReference = ref false

                    fun checkClosure (Extract(LoadLegacy{addr, level=0, fpRel=true, ...})) =
                            if addr >= startAddr andalso addr < endAddr
                            then ()
                            else hasNonLocalReference := true
                    |   checkClosure(Extract(LoadLocal addr)) =
                            if addr >= startAddr andalso addr < endAddr
                            then ()
                            else hasNonLocalReference := true
                        
                    |   checkClosure _ = hasNonLocalReference := true

                    fun processNonInlines ({ lambda, addr = decAddr, ... }, decSpval, (decs, otherChanges)) =
                    (* Have a look at the old entry to see if it's a constant. *)
                    let
                        val (oldGeneral, oldSpecial) =
                            lookupOldAddr(LoadLocal decAddr, nestingOfThisFunction, 0)
                    in
                        case oldGeneral of
                            oldGen as Constnt _ =>
                                ({addr=decSpval, value=oldGen} :: decs, otherChanges) (* It's already a constant - don't reprocess. *)
                        |   _ =>
                            let
                                (* Now copy this entry. *)
                                local
                                    val functionNesting = nestingOfThisFunction+1
                                    (* We need to trap references to our own address and turn them into
                                       recursive references.  They could be in either the new or the old
                                       address space. *)
                                    fun localOldAddr(ext as LoadLegacy{ addr, ...}, depth, level) =
                                        if level = 0 andalso addr = decAddr
                                        then (mkGenLoad (0, depth - functionNesting, false), NONE)
                                        else lookupOldAddr(ext, depth, level)
                                    |   localOldAddr _ = raise InternalError "recursive old: TODO"

                                    fun localNewAddr(ext as LoadLegacy{ addr, ...}, depth, level) =
                                        if level = 0 andalso addr = decSpval
                                        then (mkGenLoad (0, depth - functionNesting, false), NONE)
                                        else lookupNewAddr(ext, depth, level)
                                    |   localNewAddr _ = raise InternalError "recursive new: TODO"

                                    val lambdaContext: optContext =
                                        {oldEnv = {enter = enterOldDec, lookup = EnvType localOldAddr },
                                         newEnv = {enter =  enterNewDec, lookup = EnvType localNewAddr}, 
                                         spval = spval, nestingOfThisFunction = nestingOfThisFunction,
                                         debugArgs = debugArgs, recursiveExpansions = recursiveExpansions,
                                         loopFilter = loopFilter}
                                in
                                    val (gen, spec) = optimiseLambda(lambda, lambdaContext)
                                end

                                (* The general value is either a reference to the
                                   declaration or a constant if the function has just
                                   been compiled into a code segment. *)
                                val isConstant = isConstnt gen
                                val optGen =
                                    case gen of
                                        Constnt _ => gen
                                    |   Lambda{closure, ...} => (
                                            List.app checkClosure closure;
                                            mkLoad (decSpval, 0)
                                        )
                                    |   _ => raise InternalError "processNonInlines: Not a function";
              
                                (* If this is a small function we leave the special
                                   value so it can be inserted inline.  Otherwise
                                   we clear it. *)
                                val optSpec =
                                    case spec of
                                        SOME(Lambda{ isInline=NonInline, ...}, _) => NONE
                                    |   _ => spec
                                val nowInline = isSome optSpec andalso not (isSome oldSpecial)
                                (* If this is now a constant or it is a small function when it
                                   wasn't before we need to reprocess everything
                                   which depends on it to try to get the constant inserted
                                   everywhere it can be. *)
                          in
                                enterOldDec (decAddr, (optGen, optSpec));
                                (
                                 {addr=decSpval, value=gen} :: decs,
                                 otherChanges orelse isConstant orelse nowInline
                                )
                          end
                   end

                  fun repeatProcess () =
                  let
                      val (decs, haveChanged) =
                         (* Use foldr here to keep the result in the same order
                            in case we can compile them immediately below. *)
                         ListPair.foldr processNonInlines
                            ([], false) (nonInlines, addresses);
                  in
                     if haveChanged
                     then repeatProcess ()
                     else decs
                  end

                  val decs = repeatProcess ()

                  val allAreConstants =
                    List.foldl
                        (fn({value=Constnt _, ...}, others) => others
                          | _ => false) true decs

                    fun convertDecs(decs: simpleBinding list): codeBinding list =
                    let
                        (* Separate out those entries that have been converted
                           to constants from those that are still lambdas. *)
                        fun isLambda {value=Lambda _, ...} = true
                        |   isLambda {value=Constnt _, ...} = false
                        |   isLambda _ = raise InternalError "isLambda: not a lambda or a constant"
                        val (lambdas, constants) = List.partition isLambda decs

                        fun toLambda{addr, value=Lambda lambda} =
                            {addr=addr, lambda=lambda}
                        |   toLambda _ = raise InternalError "toLambda: not a lambda"
                    in
                        map Declar constants @ (if null lambdas then nil else [RecDecs(map toLambda lambdas)])
                    end

                  (* If hasNonLocalReference is still false we can code-generate
                     the mutual declarations. *)
                  val decs =
                     if ! hasNonLocalReference orelse allAreConstants
                     then decs
                     else
                        let
                            (* Create a tuple of Extract entries to get the result. *)
                            val extracts =
                                List.map (
                                    fn ({addr, ...}) => mkLoad(addr, 0))
                                    decs
                            val code = mkEnv(convertDecs decs, mkTuple extracts)
                            (* Code generate it. *)
                            val results = codeGenerateToConstant debugArgs (code, !spval+1)

                            fun reprocessDec({addr=decAddr, ...}, decSpval, (offset, others)) =
                            let
                                val (_, oldSpec) =
                                    lookupOldAddr(LoadLocal decAddr, nestingOfThisFunction, 0)
                            in
                                let
                                    val newConstant = findEntryInBlock results offset
                                in
                                    (* Replace the entry by an entry with a constant. *)
                                    enterNewDec(decSpval, (newConstant, NONE));
                                    enterOldDec(decAddr, (newConstant, oldSpec));
                                    (offset+1, {addr=decSpval, value=newConstant} :: others)
                                end
                            end
                        
                            val (_, newDecs) = ListPair.foldl reprocessDec (0, []) (nonInlines, addresses);
                        in
                            newDecs (* We've converted them all to constants. *)
                        end

                    (* Deal with the rest of the block *)
                    val (rGen, rDecs, rSpec) = copyDeclarations vs
                in
                    (* and put these declarations onto the list. *)
                    (rGen, convertDecs decs @ rDecs, rSpec)
                end

            |   copyDeclarations (NullBinding v :: vs) =
                let (* Not a declaration - process this and the rest.*)
                    val (cGen, cDecs, _) = optimise(v, false, context)
                    val (rGen, rDecs, rSpec) = copyDeclarations vs
                in  (* This must be a statement whose
                       result is ignored. Put it into the declaration list. *)
                    (rGen, cDecs @  (NullBinding cGen :: rDecs), rSpec)
                end (* copyDeclarations *)

        in
            copyDeclarations envDecs
        end (* Newenv(... *)
          
    |   optimise (Recconstr entries, _, context as { nestingOfThisFunction, spval, ...}) =
         (* The main reason for optimising record constructions is that they
            appear as tuples in ML. We try to ensure that loads from locally
            created tuples do not involve indirecting from the tuple but can
            get the value which was put into the tuple directly. If that is
            successful we may find that the tuple is never used directly so
            the use-count mechanism will ensure it is never created. *)
        let
            (* The record construction is treated as a block of local
               declarations so that any expressions which might have side-effects
               are done exactly once. *)
            val (fieldEntries, bindings) =
                ListPair.unzip(
                    List.map(fn h => makeNewDecl(optimise(h, false, context), context)) entries)

            val generalFields = List.map #1 fieldEntries
            (* Special fields - Create a list of "load" (Extract) entries.
               The reason for this is to allow us to get the full "optVal" for the field.
               The Extract entry will be looked up in "env" to return the "optVal" *)
            val specialFields = List.tabulate(List.length entries, fn addr => mkLoad(addr+1, 0))

            fun env (LoadLegacy{addr, ...}, depth, _) =
                    changeLevel (List.nth(fieldEntries, addr-1)) (depth - nestingOfThisFunction)
            |   env (LoadLocal addr, depth, _) =
                    changeLevel (List.nth(fieldEntries, addr-1)) (depth - nestingOfThisFunction)
            |   env _ = raise InternalError "env: TODO"

            val newRec  = Recconstr generalFields
        in
            if List.all isConstnt generalFields
            then (* If all the general values are constants we can create the tuple now. *)
                (makeConstVal newRec, List.foldr(op @) [] bindings, SOME(Recconstr specialFields, EnvType env))
            else
            let
                val newAddr = !spval before spval := !spval + 1
            in
                (mkLoad(newAddr, 0), List.foldr(op @) [] bindings @ [mkDec(newAddr, newRec)], SOME(Recconstr specialFields, EnvType env))
            end
        end
          
      |  optimise (Indirect{ base, offset }, _,
                   context as 
                    {nestingOfThisFunction,  spval, recursiveExpansions, loopFilter, debugArgs, ... }) =
            let
                val (genSource, decSource, specSource) = optimise(base, false, context)
            in
                (* Try to do the selection now if possible. *)
                case (specSource, genSource) of
                  (SOME(spec as Recconstr _, recEnv), _) =>
                    let
                        (* The "special" entry we've found is a tuple.  That means that
                           we are taking a field from a tuple we made earlier and so we
                           should be able to get the original code we used when we made
                           the tuple.  That might mean the tuple is never used and
                           we can optimise away the construction of it completely. The
                           entry we get back from findEntryInBlock will either be a
                           constant or a load.  In that case we need to look it up in
                           the environment we used for the record to give us an optVal.
                           The previous code in newDecl, commented out by AHL, also put
                           indirections into the table.  To save having the various cases
                           in here we simply call "optimise" which will deal with them. *)
                        val (newGen, newDecs, newSpec) =
                         optimise(findEntryInBlock spec offset, false,
                            {
                            newEnv = { lookup = errorEnv, enter = errorEnter }, (* We must always look up old addresses. *)
                            oldEnv = { lookup = recEnv, enter = errorEnter },
                            nestingOfThisFunction= nestingOfThisFunction,
                            spval= spval,
                            recursiveExpansions= recursiveExpansions,
                            loopFilter= loopFilter,
                            debugArgs= debugArgs}: optContext)
                    in
                        (newGen, decSource @ newDecs, newSpec)
                    end

                | (_ , gen as Constnt _ ) => (* General is a constant -  Do the selection now. *)
                        (findEntryInBlock gen offset, decSource, NONE)
                           
                | (_, _) => (mkInd (offset, genSource), decSource, NONE) (* No special case possible. *)
            end
       
      |  optimise (pt as Ldexc, _, _) = (pt, [], NONE) (* just a constant so return it *)
        
      |  optimise (Handle { exp, handler }, tailCall, context) =
            (Handle {exp = general context exp, handler = generalOptimise(handler, tailCall, context)}, [], NONE)

      |  optimise (c as Container _, _, _) = (c, [], NONE)

      |  optimise (TupleFromContainer(container, size), _, context as { spval, nestingOfThisFunction, ...}) =
        let
            (* If possible we want to optimise this away in the same way as
               a tuple made with Recconstr.  We have to be careful, though,
               that we have no references to the container after we return.
               We first make declarations for all the fields and then return 
               a special entry which when we apply the "env" environment
               function to it gives us returns.  That way if we never actually
               use this tuple as a single entity it won't be created.
               If we don't actually use a field the corresponding declaration
               will be removed in cleanCode. *)
            val (genCont, decsCont, _) = optimise(container, false, context)
            (* Since "container" will always be an Extract entry we can have multiple
               references to it in the declarations.  Include an assertion to that
               effect just in case future changes make that no longer true. *)
            val _ =
                case genCont of
                   Extract _ => ()
                | _ => raise InternalError "optimise - container is not Extract"
            val baseAddr = !spval before spval := !spval + size
            val specialDecs =
                List.tabulate(size, fn n => mkDec(n+baseAddr, mkInd(n, genCont)))
            val specialEntries = List.tabulate(size, fn n => mkLoad(n+baseAddr, 0))
            fun env (l:loadForm, depth, _) =
                changeLevel (Extract l, NONE) (depth - nestingOfThisFunction)
            val recAddr = !spval before spval := !spval + 1
        in
            (mkLoad(recAddr, 0),
              decsCont @ specialDecs @
                            [mkDec(recAddr, TupleFromContainer(genCont, size))],
                SOME(Recconstr specialEntries, EnvType env))
        end

      |  optimise (SetContainer{container, tuple as Cond _ , size}, _, context) =
            (* mkSetContainer transforms this. *)
            optimise(mkSetContainer(container, tuple, size), false, context)

      |  optimise (SetContainer{container, tuple as Newenv _ , size}, _, context) =
            (* mkSetContainer transforms this. *)
            optimise(mkSetContainer(container, tuple, size), false, context)

      |  optimise (SetContainer{container, tuple, size}, _, context) =
            (* Push the set-container down the tree and then process it. If we've
               expanded an inline function we want to be able to find any
               tuple we're creating. *)
            let
                val optCont = general context container
                and optTuple = general context tuple
                val () =
                    case optCont of
                        Extract(LoadLegacy{level=0, fpRel=true, ...}) => () (* Argument *)
                    |   Extract(LoadLocal _) => () (* Local *)
                    |   _ => raise InternalError "optimise: SetContainer - not local"
                
                (* If the "tuple" is an expanded inline function it may well
                   contain an if-expression.  If both branches were tuples
                   we will have expanded it already and the result will be
                   a TupleFromContainer. *)
                fun pushSetContainer(Cond(ifpt, thenpt, elsept), decs): codetree =
                    mkEnv(List.rev decs,
                           Cond(ifpt, pushSetContainer(thenpt, []), pushSetContainer(elsept, [])))

                |   pushSetContainer(Newenv(envDecs, envExp), decs) =
                        pushSetContainer(envExp, List.rev envDecs @ decs)

                |   pushSetContainer(tuple as
                        TupleFromContainer(
                            Extract(LoadLegacy{addr=innerAddr, level=0, fpRel=true, ...}), innerSize),
                        decs) =
                    if innerSize = size
                    then
                        (
                        case optCont of
                            Extract(LoadLegacy{addr=containerAddr, level=0, fpRel=true, ...}) =>
                            let
                                (* We can remove the inner container and replace it by
                                   a reference to the outer. *)
                                fun replaceContainerDec [] =
                                    raise InternalError "replaceContainerDec"
                                  | replaceContainerDec ((hd as Declar{addr, ...}) :: tl) =
                                        if addr = innerAddr
                                        then mkDec(addr, mkLoad(containerAddr, 0)) :: tl
                                        else hd :: replaceContainerDec tl 
                                  | replaceContainerDec(hd :: tl) =
                                        hd :: replaceContainerDec tl
                                (* Sometimes the TupleFromContainer is preceded by some
                                   Declar entries.  We need to remove them so that the
                                   last entry in the list is the NullBinding that contains the
                                   code that sets the container, usually as the result of
                                   passing it as an argument to a function.  For some reason
                                   just adding a dummy CodeZero at the end results in a
                                   reference to a non-local container.  I don't understand this. *)
                                fun removeFinalDecs(Declar _ :: tl) = removeFinalDecs tl
                                |   removeFinalDecs(RecDecs _ :: tl) = removeFinalDecs tl
                                |   removeFinalDecs(NullBinding c :: tl) = (c, tl)
                                |   removeFinalDecs [] = raise InternalError "removeFinalDecs: Empty"
                                val (exp, rdecs) = removeFinalDecs(replaceContainerDec decs)
                            in
                                (* Just replace the declaration. *)
                                mkEnv(List.rev rdecs, exp)
                            end
                        |   Extract _ => raise InternalError "pushSetContainer: TODO"
                        | _ => mkEnv(List.rev decs, mkSetContainer(optCont, tuple, size))
                        )
                    else mkEnv(List.rev decs, mkSetContainer(optCont, tuple, size))

                |   pushSetContainer(tuple, decs) =
                        mkEnv(List.rev decs, mkSetContainer(optCont, tuple, size))
              
            in
                (pushSetContainer(optTuple, []), [], NONE)
            end

      |  optimise (ConstntWithInline (w, spec, env), _, _) = (Constnt w, [], SOME(spec, env))
            (* These are global values.  Just return them. *)

      |  optimise (TagTest{test, tag, maxTag}, _, context) =
            let
                val optTest = general context test
            in
                case optTest of
                    Constnt testResult =>
                        if isShort testResult andalso toShort testResult = tag
                        then (CodeTrue, [], NONE)
                        else (CodeFalse, [], NONE)
                |   _ => (TagTest{test=optTest, tag=tag, maxTag=maxTag}, [], NONE)
            end

    and optimiseLambda(original as ({isInline=OnlyInline, ...}), { oldEnv = {lookup=EnvType lookupOldAddr, ...}, ...}) =
        (* Used only for functors.  Leave unchanged. *)
            (CodeZero,
              (* Changed from using CodeNil here to CodeZero.  This avoids a problem
                 which only surfaced with the changes to ML97 and the possibility of
                 mixing functor and value declarations in the same "program" (i.e.
                 top-level declarations with a terminating semicolon.
                 OnlyInline is used for functors which can only ever be called,
                 never passed as values, so the "general" value is not really
                 required.  It can though appear in the result tuple of the "program"
                 from which the (value) results of the program are extracted.

                 Further change: This previously returned the processed body but the effect of
                 that was to prevent small functions inside the functor from becoming
                 inline functions when the functor was applied.  Instead we just return
                 the original code. *)
              SOME(Lambda original, EnvType lookupOldAddr) (* Old addresses with unprocessed body. *)
            )

     |  optimiseLambda(original as ({body=lambdaBody, isInline=lambdaInline, name=lambdaName,
                          argTypes, resultType, ...}),
                 { nestingOfThisFunction, oldEnv = {lookup=EnvType lookupOldAddr, ...}, newEnv = {lookup=EnvType lookupNewAddr, ...},
                   debugArgs, spval, ... }: optContext) =
        let
            (* The nesting of this new function is the current nesting level
             plus one. Normally this will be the same as lambda.level except
             when we have a function inside an inline function. *)
            val nesting = nestingOfThisFunction + 1;
          
            (* A new table for the new function. *)
            val oldAddrTab = stretchArray (initTrans, NONE)
            and newAddrTab = stretchArray (initTrans, NONE)

            local
                fun localOldAddr (LoadLocal addr, depth, 0) =
                    changeLevel(valOf(oldAddrTab sub addr)) (depth - nesting)

                |   localOldAddr (LoadLegacy{ addr=index, ...}, depth, 0) =
                    (* local declaration or argument. *)
                    if index > 0 
                    (* Local declaration. *)
                    then localOldAddr(LoadLocal index, depth, 0)
                     (* Argument or closure. *)
                    else (mkGenLoad (index, depth - nesting, index <> 0), NONE)

                |   localOldAddr (_, _, 0) = raise InternalError "localOldAddr: TODO"

                |   localOldAddr (ptr, depth, levels) =
                        lookupOldAddr (ptr, depth, levels - 1)

                and setTab (index, v) = update (oldAddrTab, index, SOME v)
            in
                val oldEnv = { lookup = EnvType localOldAddr, enter = setTab }
            end

            local
                (* localNewAddr is used as the environment of inline functions within
                   the function we're processing.  All the entries in this table will
                   have their "general" entries as simply Extract entries with the
                   original address.  Their "special" entries may be different. The
                   only entries in the table will be those which correspond to
                   declarations in the original code so there may be new declarations
                   which aren't in the table. *)
                fun localNewAddr (LoadLegacy{ addr=index, ...}, depth, 0) =
                    if index > 0 
                    then case newAddrTab sub index of
                        NONE => (* Return the original entry if it's not there. *)
                            (mkGenLoad (index, depth - nesting, true), NONE)
                    |   SOME v => changeLevel v (depth - nesting) 
                    else (mkGenLoad (index, depth - nesting, index <> 0), NONE)

                |   localNewAddr (ptr as LoadLegacy _, depth, levels) =
                        lookupNewAddr (ptr, depth, levels - 1)

                |   localNewAddr _ = raise InternalError "localNewAddr: TODO"

                fun setNewTab (addr, v) = update (newAddrTab, addr, SOME v)
            in
                val newEnv = {lookup=EnvType localNewAddr, enter=setNewTab}
            end

            val newAddressAllocator = ref 1      val newCode =
                optimise (lambdaBody, false,
                {
                newEnv=newEnv,
                oldEnv=oldEnv,
                nestingOfThisFunction=nesting,
                spval=newAddressAllocator,
                recursiveExpansions=NONE,
                loopFilter=loopFilterError,
                debugArgs=debugArgs})

          (* nonLocals - a list of the non-local references made by this
             function.  If this is empty the function can be code-generated
             immediately and returned as a constant.  If it is non-empty it
             is set as the closure for the function.  This is then used
             when processing mutually recursive functions to find the
             dependencies. *)
             
            val nonLocals = ref nil

            fun addNonLocal((addr, level, fpRel), depth) =
                let
                    (* The level will be correct relative to the use, which may be
                       in an inner function.  We want the level relative to the
                       scope in which this function is declared. *)
                    val correctedLevel = level - (depth - nestingOfThisFunction)

                    fun findNonLocal(addr', level', fpRel') =
                            addr = addr' andalso correctedLevel = level' andalso fpRel=fpRel'
                in
                    if List.exists findNonLocal (!nonLocals)
                    then () (* Already there. *)
                    else nonLocals := (addr, correctedLevel, fpRel) :: ! nonLocals
                end

          fun checkRecursion(ext as LoadLegacy {fpRel=oldfpRel, ...}, levels, depth) =
          (
              case #1(lookupNewAddr (ext, depth, levels)) of
                 (res as Extract(LoadLegacy {addr=0, fpRel=false, level, ...})) =>
                     (
                     (* If this is just a recursive call it doesn't count
                        as a non-local reference.  This only happens if
                        we turned a reference to a local into a recursive
                        reference (i.e. fpRel was previously true). *)
                     if levels = 0 andalso oldfpRel
                     then ()
                     else addNonLocal((0, level, false), depth);
                     res
                     )
              |  res as Extract(LoadLegacy{addr, level, fpRel}) =>
                    (
                     addNonLocal((addr, level, fpRel), depth);
                     res
                    )

              |  Extract _ => raise InternalError "checkRecursion: TODO"

              |  res => res (* We may have a constant in this table. *)
         )
         |  checkRecursion _ = raise InternalError "checkRecursion: TODO"

          val cleanedBody =
            REMOVE_REDUNDANT.cleanProc(getGeneral newCode, checkRecursion, nesting,
                      Array.array (! newAddressAllocator + 1, false))

        in
            (
            case lambdaInline of
                MaybeInline => (* Explicitly inlined functions. *)
                    (* We return the processed version of the function as
                       the general value but the unprocessed version as
                       the special value. *)
                    (
                        Lambda 
                          {
                           body          = cleanedBody,
                           isInline      = MaybeInline,
                           name          = lambdaName,
                           closure       = List.map mkGenLoad (!nonLocals), (* Only looked at in MutualDecs. *)
                           argTypes      = argTypes,
                           resultType    = resultType,
                           level         = nesting,
                           localCount    = ! newAddressAllocator + 1
                         },
                        SOME(Lambda original, EnvType lookupOldAddr)(* Old addresses with unprocessed body. *)
                    )

            |   _ => (* "Normal" function.  If the function is small we mark it as
                        inlineable.  If the body has no free variables we compile it
                        now so that we can propagate the resulting constant, otherwise
                        we return the processed body.  We return the processed body as
                        the special value so that it can be inlined.  We do this even
                        in the case where the function isn't small because it is just
                        possible we're going to apply the function immediately and in
                        that case it's worth inlining it anyway. *)
              let
                    val inlineType =
                        if lambdaInline = NonInline
                        then if (* Is it small? *) codeSize(cleanedBody, true) < 
                                DEBUG.getParameter DEBUG.maxInlineSizeTag debugArgs
                        then SmallFunction else NonInline
                        else lambdaInline

                  val copiedLambda =
                    Lambda 
                      {
                       body          = cleanedBody,
                       isInline      = inlineType,
                       name          = lambdaName,
                       closure       = List.map mkGenLoad (!nonLocals), (* Only looked at in MutualDecs. *)
                       argTypes      = argTypes,
                       resultType    = resultType,
                       level         = nesting,
                       localCount    = ! newAddressAllocator + 1
                     };
                 val general = 
                   (* If this has no free variables we can code-generate it now. *)
                   if null (!nonLocals)
                   then codeGenerateToConstant debugArgs (copiedLambda, !spval+1)
                   else copiedLambda
              in
                    (general,
                        (* If this function may be inlined include it in the special entry
                           otherwise return CodeNil here. *)
                        if inlineType = NonInline
                        then NONE
                        else
                        SOME(Lambda 
                          {
                           body          = cleanedBody,
                           isInline      = inlineType,
                           name          = lambdaName,
                           closure       = [],
                           argTypes      = argTypes,
                           resultType    = resultType,
                           level         = nesting,
                           localCount    = ! newAddressAllocator + 1
                         }, EnvType lookupNewAddr)
                    )
            end
            ) before StretchArray.freeze oldAddrTab before  StretchArray.freeze newAddrTab
        end (* optimiseLambda *)


    (* Inline a function that has been explicitly marked by the frontend.  These are
       either functors or auxiliary functions in curried or tupled fun bindings.
       They are never directly recursive but could contain "small" functions that are. *)
    and inlineInlineOnlyFunction(funct, {body=lambdaBody, name=lambdaName, ...}, EnvType functEnv, argList, tailCall,
            context as {spval, nestingOfThisFunction, recursiveExpansions, newEnv = { lookup = EnvType lookupNewAddr, ...}, debugArgs, ...}) =
        let
            (* Calling inline proc or a lambda expression which is just called.
               The function is replaced with a block containing declarations
               of the parameters.  We need a new table here because the addresses
               we use to index it are the addresses which are local to the function.
               New addresses are created in the range of the surrounding function. *)
            val localVec = stretchArray (initTrans, NONE);
            val localNewVec = stretchArray (initTrans, NONE);

            local
                val (params, bindings) =
                    ListPair.unzip(
                        List.map (fn (h, _) => makeNewDecl(optimise(h, false, context), context)) argList)

                val paramVec = Vector.fromList params
                val nArgs = List.length argList
            in
                (* The arguments are numbered ~1, ~2 etc. *)
                fun getParameter n = Vector.sub(paramVec, nArgs+n)

                (* Bindings necessary for the arguments *)
                val copiedArgs = List.foldr (op @) [] bindings
            end

            val newEnv =
            { 
                enter = fn (addr, v) => update (localNewVec, addr, SOME v),
                lookup =
                    EnvType(
                        fn (ext as LoadLegacy{ addr, ...}, depth, levels) =>
                            (* It may be local to this function or to the surrounding scope. *)
                        if levels <> 0 orelse addr <= 0
                        then lookupNewAddr(ext, depth, levels)
                        else
                        (case localNewVec sub addr of
                                SOME v => changeLevel v (depth - nestingOfThisFunction)
                            |   NONE => lookupNewAddr(ext, depth, levels)
                        )
                        | _ => raise InternalError "lookup: TODO"
                    )
            }

            local
                (* The environment for the expansion of this function
                   is the table for local declarations and the original
                   environment in which the function was declared for
                   non-locals. *)
                fun lookupDec (LoadLegacy{ addr=0, fpRel = false, ...}, _, 0) =
                    (* Recursive reference - shouldn't happen. *)
                        raise InternalError ("lookupDec: Inline function recurses " ^ lambdaName)
                |   lookupDec (LoadLegacy{ addr=index, level, ...}, depth, 0) =
                    let (* On this level. *)
                        val optVal =
                            if index = 0 then raise InternalError "It's zero"
                            else if index > 0
                            then localVec sub index (* locals *)
                            else SOME(getParameter index) (* parameters *)
                        val value =
                            case optVal of
                                SOME value => value
                            |   NONE => raise InternalError(
                                                concat["Missing value address=", Int.toString index,
                                                       " level= ", Int.toString level, " while expanding ", lambdaName])
                    in
                        changeLevel value (depth - nestingOfThisFunction)
                    end

                |   lookupDec (LoadLocal addr, depth, 0) =
                        changeLevel (valOf(localVec sub addr)) (depth - nestingOfThisFunction)

                |   lookupDec (_, _, 0) = raise InternalError "TODO"

                |   lookupDec (ptr, depth, levels) = functEnv(ptr, depth, levels - 1)
            in
                val oldEnv = 
                    { lookup = EnvType lookupDec, enter = fn (index, v) => update (localVec, index, SOME v) }
            end

            val (cGen, cDecs, cSpec) =
                optimise(lambdaBody, tailCall,
                    {newEnv = newEnv, oldEnv = oldEnv,
                     nestingOfThisFunction=nestingOfThisFunction,
                     spval=spval,
                     recursiveExpansions=recursiveExpansions,
                     loopFilter=loopFilterError,
                     debugArgs=debugArgs})
        in
            StretchArray.freeze localVec;
            StretchArray.freeze localNewVec;
            (* The result is the result of the body of the inline function.
               The declarations needed for the inline function, the
               declarations used to load the arguments and the declarations
               in the expanded function are all concatenated together. *)
            (cGen, (#2 funct @ (copiedArgs @ cDecs)), cSpec)
        end

    (* Expand inline a "small" function i.e. a normal function that appears to be small
       enough to expand inline. *)
    and inlineSmallFunction((functGen, functDecs, functSpec), original, {body=lambdaBody, name=lambdaName, argTypes, ...},
            EnvType functEnv, argList, resultType,
            context: optContext as {spval, nestingOfThisFunction, newEnv as { lookup = EnvType lookupNewAddr, ...}, debugArgs, 
            oldEnv = { lookup = EnvType lookupOldAddr, enter=enterOldAddr}, loopFilter, recursiveExpansions, ...}) =
        let
            (* Calling inline proc or a lambda expression which is just called.
               The function is replaced with a block containing declarations
               of the parameters.  We need a new table here because the addresses
               we use to index it are the addresses which are local to the function.
               New addresses are created in the range of the surrounding function. *)
            val localVec = stretchArray (initTrans, NONE)
            val localNewVec = stretchArray (initTrans, NONE)

            val nArgs = List.length argList

            local
                (* Process the arguments.  We have to make a special check here that we are not passing in the function
                   we are trying to expand.  This could result in an infinitely recursive expansion.  It is only
                   going to happen in very special circumstances such as a definition of the Y combinator.  If we
                   do encounter this function we strip the "special" entry so any application will always result
                   in a function call. *)
                val argContext =
                    case (original, {lookup=lookupOldAddr, enter=enterOldAddr}) of
                        (Extract(LoadLegacy{addr, level, fpRel=true, ...}), { lookup, enter}) =>
                            let
                                fun stripRecursiveInline(ext as LoadLegacy{ addr=eAddr, ...}, depth, levels) =
                                    let
                                        val result as (genResult, _) = lookup(ext, depth, levels)
                                    in
                                        if addr = eAddr andalso level = levels
                                        then (genResult, NONE)
                                        else result
                                    end
                                |   stripRecursiveInline(ext as LoadLocal eAddr, depth, levels) =
                                    let
                                        val result as (genResult, _) = lookup(ext, depth, levels)
                                    in
                                        if addr = eAddr andalso level = levels
                                        then (genResult, NONE)
                                        else result
                                    end
                                |   stripRecursiveInline _ = raise InternalError "TODO"
                            in
                                { oldEnv = { lookup=EnvType stripRecursiveInline, enter = enter}, newEnv = newEnv,
                                  nestingOfThisFunction = nestingOfThisFunction, spval = spval,
                                  loopFilter = loopFilter, debugArgs = debugArgs,
                                  recursiveExpansions = recursiveExpansions}
                            end
                    |   (Extract(LoadLocal addr), { lookup, enter}) =>
                            let
                                fun stripRecursiveInline(ext as LoadLegacy{ addr=eAddr, ...}, depth, levels) =
                                    let
                                        val result as (genResult, _) = lookup(ext, depth, levels)
                                    in
                                        if addr = eAddr andalso 0 = levels
                                        then (genResult, NONE)
                                        else result
                                    end
                                |   stripRecursiveInline(ext as LoadLocal eAddr, depth, levels) =
                                    let
                                        val result as (genResult, _) = lookup(ext, depth, levels)
                                    in
                                        if addr = eAddr andalso 0 = levels
                                        then (genResult, NONE)
                                        else result
                                    end
                                |   stripRecursiveInline _ = raise InternalError "TODO"
                            in
                                { oldEnv = { lookup=EnvType stripRecursiveInline, enter = enter}, newEnv = newEnv,
                                  nestingOfThisFunction = nestingOfThisFunction, spval = spval,
                                  loopFilter = loopFilter, debugArgs = debugArgs,
                                  recursiveExpansions = recursiveExpansions}
                            end
                            
                    |   (Extract _, _) => raise InternalError "argContext: TODO"
                    |   _ => context:optContext

                val (params, bindings) =
                    ListPair.unzip(
                        List.map (fn (h, _) => makeNewDecl(optimise(h, false, argContext), context)) argList)

                val paramVec = Vector.fromList params
            in
                (* The arguments are numbered ~1, ~2 etc. *)
                fun getParameter n = Vector.sub(paramVec, nArgs+n)

                (* Bindings necessary for the arguments *)
                val copiedArgs = List.foldr (op @) [] bindings
            end

            fun lookupLocalNewAddr (ext as LoadLegacy{ addr, ...}, depth, levels) =
                    (* It may be local to this function or to the surrounding scope. *)
                if levels <> 0 orelse addr (*<=*) < 0
                then lookupNewAddr(ext, depth, levels)
                else
                (case localNewVec sub addr of
                        SOME v => changeLevel v (depth - nestingOfThisFunction)
                    |   NONE => lookupNewAddr(ext, depth, levels)
                )
            |   lookupLocalNewAddr _ = raise InternalError "lookupLocalNewAddr: TODO"

            (* Now load the function body itself.  We first process it assuming
               that we won't need to treat any of the arguments specially.  If
               we find that we generate a Loop instruction somewhere we have
               to make sure that any arguments we change in the course of the
               loop are taken out.  For example:
               fun count'(n, []) = n | count' (n, _::t) = count'(n+1, t);
               fun count l = count'(0, l).
               In this case we would start by expanding count' using 0 for n
               throughout, since it's a constant.  When we find the recursive
               call in which n becomes n+1 we find we have to take n out of the
               loop and treat it as a variable.
               We don't need to do this if the argument is passed through unchanged
               e.g. fun foldl f b [] = b  | foldl f b (x::y) = foldl f (f(x, b)) y;
               where the same value for f is used everywhere and by treating it
               specially we can expand its call. 
               This two-pass (it will normally be two-pass but could be more) approach
               allows us to optimise cases where we have a recursive function which
               happens to be non-recursive with particular constant values of the
               arguments.
               e.g. if x = nil ... generates a general recursive function for
               equality on lists but because of the nil argument this optimises
               down to a simple test. *)
            (*
               I'm now extending this to the general recursive case not just tail
               recursion.  If we discover a recursive call while processing the
               function we turn this expansion into a function call and give up
               trying to inline it.  Instead we create a special-purpose function
               for this call but with only the arguments that change as a
               result of the recursive calls actually passed as arguments.  Other
               arguments can be inserted inline in function.
               e.g. fun map f [] = [] | map f (a::b) = f a :: map f b
               where when we map a function over a list we compile a specialised
               mapping function with the actual value of f inserted in it.
            *)
                val needsBeginLoop = ref false
                val needsRecursiveCall = ref false
                val argModificationVec = Array.array(nArgs, false);
                (* Create addresses for the new variables for modified arguments.
                   If newdecl created a variable we might be able to reuse that
                   but it's easier to create new ones. *)
                val argBaseAddr = ! spval; val _ = spval := argBaseAddr + nArgs

                (* filterArgs is called whenever a recursive call is made to
                   this function. *)
                fun filterArgs (argList: (codetree * argumentType) list, isTail, depth) =
                let
                    fun filterArgs' 0 [] = []
                      | filterArgs' _ [] =
                            raise InternalError "filterArgs: wrong number of args"
                      | filterArgs' n ((arg, argType) :: rest) =
                        let
                            (* Is this simply passing the original argument value?  *)
                            val original = getParameter(~n)
                            val unChanged =
                                case (arg, #1 original) of
                                    (Constnt w, Constnt w') =>
                                        (* These may well be functions so don't use
                                           structure equality. *)
                                        wordEq(w, w')
                                |   (Extract ext1, Extract ext2) =>
                                    let
                                        fun addrLevel(LoadLegacy{addr, level, fpRel, ...}, fix) =
                                                (addr, level+fix, fpRel)
                                        |   addrLevel(LoadLocal addr, fix) = (addr, fix, true)
                                        |   addrLevel _ = raise InternalError "filterArgs: TODO"
                                    in
                                        addrLevel(ext1, 0) = addrLevel(ext2, depth-nestingOfThisFunction)
                                    end
                                | _ => false

                        in
                            if unChanged
                            then ()
                            else Array.update(argModificationVec, n-1, true);
                            (* If any recursive call has changed it we need
                               to include this argument even if it didn't
                               change on this particular call. *)
                            if Array.sub(argModificationVec, n-1)
                            then (arg, argType) :: filterArgs' (n-1) rest
                            else (* Not modified *) filterArgs' (n-1) rest
                        end
                in
                    needsBeginLoop := true; (* Indicate we generated a Loop instr. *)
                    (* If this isn't tail recursion we need a full call. *)
                    if isTail then () else needsRecursiveCall := true;
                    (* If we have a recursive inline function containing a
                       local recursive inline function which calls the outer
                       function
                       (e.g. fun f a b = .... map (f a) ....) we may process
                       the body of the inner function twice, once as a lambda
                       and once when we attempt to expand it inline.  That
                       means we will process the recursive call to the outer
                       function twice.  The first call may filter out redundant
                       arguments (e.g. "a" in the above example).  *)
                    if List.length argList <> nArgs
                    then argList
                    else filterArgs' nArgs argList
                end

                (* See how many arguments changed. *)
                fun countSet() =
                    Array.foldl(fn (true, n) => n+1 | (false, n) => n) 0 argModificationVec

                fun checkForLoopInstrs lastModCount =
                (* Try inlining the function.  Called initially or while we only have
                   tail recursive calls.  Returns SOME code if this is possible or NONE
                   if we will have to construct an auxiliary function. *)
                let
                    (* There's still something strange going on where we end up with
                       a BeginLoop that doesn't contain any Loop.  See regression
                       test Succeed/Test108.ML. *)
                    val _ = needsBeginLoop := false

                    fun localOldAddr(LoadLegacy{ addr=0, fpRel = false, ...}, depth, 0) =
                         (* Recursive reference - return the copied function after removing
                            the declarations.  These will be put on in the surrounding
                            scope.  Since we're not actually inside the function (we're
                            expanding it) we can't return a recursive reference so we
                            return the reference to the function that was passed in.
                            This cannot be a recursive or tail recursive call because
                            that case is dealt with specially in the Eval case of
                            "optimise". *)
                         changeLevel (functGen, functSpec) (depth - nestingOfThisFunction)

                    |   localOldAddr (LoadLegacy{ addr=index, ...}, depth, 0) =
                        let (* On this level. *)
                            val optVal =
                                if index > 0
                                then valOf (localVec sub index)  (* locals *)
                                else (* index < 0 *) if Array.sub(argModificationVec, ~index-1)
                                then (* This argument changes - must use a variable even if
                                        the original argument was a constant. *)
                                    (mkLoad(argBaseAddr + nArgs + index, 0), NONE)
                                else getParameter index (* parameters *)
                        in
                            changeLevel optVal (depth - nestingOfThisFunction)
                        end

                    |   localOldAddr (LoadLocal index, depth, 0) =
                        let (* On this level. *)
                            val optVal = valOf (localVec sub index)  (* locals *)
                        in
                            changeLevel optVal (depth - nestingOfThisFunction)
                        end

                    |   localOldAddr (_, _, 0) = raise InternalError "localOldAddr: TODO"


                    |   localOldAddr (ptr, depth, levels) = (* On another level. *)
                            functEnv(ptr, depth, levels - 1)

                    (* All declarations should be of positive addresses. *)
                    fun setNewTabForInline (addr, v) = update (localNewVec, addr, SOME v)
                    and setTabForInline (index, v) = update (localVec, index, SOME v)

                    val copiedBody =
                         optimise(lambdaBody, true,
                          {newEnv = {lookup=EnvType lookupLocalNewAddr, enter = setNewTabForInline },
                           oldEnv = {lookup=EnvType localOldAddr, enter =setTabForInline },
                           nestingOfThisFunction=nestingOfThisFunction,
                           spval=spval,
                           recursiveExpansions=SOME(filterArgs),
                           loopFilter=loopFilterError,
                           debugArgs=debugArgs})
                  
                 in
                    if ! needsRecursiveCall
                    then (* We need a fully recursive call. *)
                        NONE (* *)
                    else if ! needsBeginLoop 
                    then (* We've found at least one recursive call which changes its
                            argument value. *)
                    let
                        val newModCount = countSet()
                    in
                        if newModCount > lastModCount
                        then checkForLoopInstrs newModCount
                        else SOME copiedBody
                    end
                    else SOME copiedBody
                end

                fun checkRecursiveCalls lastModCount =
                (* Called if a simple inline expansion won't work.  We're
                   going to have to generate this function as a function and
                   a call to that function.  However we may well gain by inserting
                   in line arguments which don't change as a result of recursion.
                   TODO: If all the arguments are modified we could just as
                   well fall back to calling the general function. *)
                let
                    val nesting = nestingOfThisFunction + 1
                    (* Find the parameter we're actually going to use. *)
                    fun getParamNo n =
                        if n = 0 then 0
                        else if Array.sub(argModificationVec, ~n -1)
                        then getParamNo (n+1) - 1
                        else getParamNo (n+1)

                    fun localOldAddr (LoadLegacy{ addr=0, fpRel = false, ...}, depth, 0) =
                         (* Recursive reference.  We're going to generate this
                            as a function so return a reference to the closure. *)
                         (mkGenLoad (0, depth - nesting, false), NONE)

                    |   localOldAddr (LoadLocal index, depth, 0) =
                            changeLevel(valOf (localVec sub index)) (depth - nesting)

                    |   localOldAddr (LoadLegacy{ addr=index, ...}, depth, 0) =
                         if index > 0  (* locals *)
                         then changeLevel(valOf (localVec sub index)) (depth - nesting)
                         else (* index < 0 - parameters *)
                            if Array.sub(argModificationVec, ~index -1)
                         then (* This argument has changed - find the corresponding
                                 actual argument. *)
                            (mkLoad(getParamNo index, depth-nesting), NONE)
                         else (* Unchanged - get the entry from the table, converting
                                 the level because it's in the surrounding scope. *)
                            changeLevel (getParameter index) (depth-nesting+1)

                    |   localOldAddr(_, _, 0) = raise InternalError "localOldAddr: TODO"

                    |   localOldAddr (ptr, depth, levels) = functEnv(ptr, depth, levels - 1)

                    val newAddrTab = stretchArray (initTrans, NONE);

                    (* localNewAddr is used as the environment of inline functions within
                       the function we're processing. *)
                    fun localNewAddr (LoadLegacy{ addr=index, ...}, depth, 0) =
                        if index >= 0 
                        then case newAddrTab sub index of
                            NONE => (* Return the original entry if it's not there. *)
                                (mkGenLoad (index, depth - nesting, true), NONE)
                        |   SOME v => changeLevel v (depth - nesting) 
                        else (mkGenLoad (index, depth - nesting, index <> 0), NONE)
                    |   localNewAddr (ptr as LoadLegacy _, depth, levels) =
                            lookupNewAddr (ptr, depth, levels - 1)
                    |   localNewAddr _ = raise InternalError "localNewAddr: TODO"

                    fun setNewTab (addr, v) = update (newAddrTab, addr, SOME v)
                    and setTab (index, v) = update (localVec, index, SOME v)

                    val newAddressAllocator = ref 1 (* Starts at one. *)

                    val copiedBody =
                         optimise(lambdaBody, false, (* Don't generate loop instructions. *)
                          {newEnv = {lookup=EnvType localNewAddr, enter=setNewTab },
                           oldEnv = {lookup=EnvType localOldAddr, enter=setTab },
                           nestingOfThisFunction=nesting,
                           spval=newAddressAllocator,
                           recursiveExpansions=SOME filterArgs,
                           loopFilter=loopFilterError,
                           debugArgs=debugArgs})

                    val newModCount = countSet()
                in
                    if newModCount > lastModCount
                    then (* We have some (more) arguments to include. *)
                        checkRecursiveCalls newModCount
                    else
                    let
                        fun filterArgTypes([], _) = []
                        |   filterArgTypes(hd:: tl, n) =
                                if Array.sub(argModificationVec, n-1)
                                then hd :: filterArgTypes(tl, n-1)
                                else filterArgTypes(tl, n-1)
                        val newArgList = filterArgTypes(argTypes, nArgs)
                    in
                        (Lambda {
                                body = getGeneral copiedBody,
                                isInline = NonInline,
                                name = lambdaName,
                                closure = [],
                                argTypes = newArgList, resultType=resultType,
                                level = nestingOfThisFunction + 1,
                                localCount = ! newAddressAllocator + 1 },
                         [], NONE)
                    end
                end

            (* If we need to make the declarations put them in at the
               beginning of the loop. *)
            fun makeDecs(0, []) = []
              | makeDecs(n, (_, typ):: rest) =
                    if not (Array.sub(argModificationVec, n-1))
                    then makeDecs (n-1, rest)
                    else
                    let
                        val argVal = #1(getParameter(~n))
                        val argDec =
                            (* Include the address at this stage even if it's a call *)
                            {value = argVal, addr=argBaseAddr+nArgs-n}
                    in
                        (argDec, typ) :: makeDecs (n-1, rest)
                    end
             |  makeDecs _ = raise InternalError "Unequal lengths"

        in
            case checkForLoopInstrs 0 of
                SOME (procBody as (pGen, pDecs, pSpec)) =>
                let
                    (* Simple inline expansion is possible. *)
                    val newDecs = makeDecs(nArgs, argList)
                    val (cGen, cDecs, cSpec) =
                        if ! needsBeginLoop
                        then (BeginLoop{loop=getGeneral procBody, arguments=newDecs}, [], NONE)
                        else if Array.exists(fn x => x) argModificationVec
                        then (* We could have reset needsBeginLoop to false and ended up not needing a loop.
                                If we have declarations that we made earlier we have to include them. *)
                            (pGen, List.map (Declar o #1) newDecs @ pDecs, pSpec)
                        else procBody
                in
                    StretchArray.freeze localVec;
                    StretchArray.freeze localNewVec;
                    (* The result is the result of the body of the inline function.
                       The declarations needed for the inline function, the
                       declarations used to load the arguments and the declarations
                       in the expanded function are all concatenated together. *)
                    (cGen, functDecs @ (copiedArgs @ cDecs), cSpec)
                end

            |   NONE =>
                let
                    (* Need to build a function. *)
                    val procBody = checkRecursiveCalls (countSet())
                    val newDecs = makeDecs(nArgs, argList)
                    (* Put the function into the declarations. *)
                    val addr = ! spval before spval := !spval+1
                in
                    (Eval {function = mkLoad(addr, 0), 
                                  argList = List.map (fn({value, ...}, t) => (value, t)) newDecs,
                                  resultType=resultType},
                     functDecs @ (copiedArgs @  [mkDec(addr, getGeneral procBody)]),
                     NONE)
                end
        end

    fun codetreeOptimiser(pt, debugSwitches) =
    let
        val localAddressAllocator = ref 1
        val oldAddrTab = stretchArray (initTrans, NONE)
        val newAddrTab = stretchArray (initTrans, NONE)
        
        local
            (* Return the entry from the "old" table with the depth adjusted if necessary.
               The entry must exist. *)
            fun lookupOldAddr (LoadLocal addr, depth, 0) =
                    changeLevel(valOf(oldAddrTab sub addr)) depth
            |   lookupOldAddr (LoadLegacy{addr, ...}, depth, 0) =
                    changeLevel(valOf(oldAddrTab sub addr)) depth
            |   lookupOldAddr _ = raise InternalError "top level reached"

            (* Make an entry in the "old" table *)
            and enterOldDec (addr, tab) = update (oldAddrTab, addr, SOME tab)
        in
            val oldEnv = { lookup = EnvType lookupOldAddr, enter = enterOldDec }
        end

        local
            (* Return the entry from the "new" table.  If the entry does not exist
               just return a "load" entry with the depth adjusted as necessary. *)
            fun lookupNewAddr (LoadLegacy{addr, ...}: loadForm, depth, 0) =
            (
                case newAddrTab sub addr of
                    NONE => (mkGenLoad (addr, depth, true), NONE)
                |   SOME v => changeLevel v depth 
                )
            |  lookupNewAddr _ = raise InternalError "outer level reached in lookupNewAddr"

            (* Make an entry in the "new" table. *)
            fun enterNewDec (addr, tab) = update (newAddrTab, addr, SOME tab)
        in
            val newEnv = { lookup = EnvType lookupNewAddr, enter = enterNewDec }
        end

        val (rGeneral, rDecs, rSpec) =
            optimise(pt, false,
                {newEnv = newEnv, oldEnv = oldEnv,
                 nestingOfThisFunction=0, spval=localAddressAllocator,
                 recursiveExpansions=NONE, loopFilter=loopFilterError, debugArgs=debugSwitches})
    in
        (* Turn the arrays into vectors. *)
        StretchArray.freeze oldAddrTab;
        StretchArray.freeze newAddrTab;
        { numLocals = ! localAddressAllocator + 1, general = rGeneral, bindings = rDecs, special = rSpec }
    end

    structure Sharing = struct type codetree = codetree and envType = envType and codeBinding = codeBinding end

end;
