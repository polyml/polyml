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
        type codeUse
        val cleanProc : (codetree * codeUse list * (int -> loadForm) * int) -> codetree
        structure Sharing: sig type codetree = codetree and loadForm = loadForm and codeUse = codeUse end
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
        type codetree and envSpecial and codeBinding
        val codetreeOptimiser: codetree  * Universal.universal list * int ->
            { numLocals: int, general: codetree, bindings: codeBinding list, special: envSpecial }
        structure Sharing: sig type codetree = codetree and envSpecial = envSpecial and codeBinding = codeBinding end
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

    val word0 = toMachineWord 0
    val word1 = toMachineWord 1
  
    val False = word0
    val True  = word1

    fun mkDec (laddr, res) = Declar{value = res, addr = laddr, use=[]}

    local
        open RuntimeCalls
        val ioOp : int -> machineWord = RunCall.run_call1 POLY_SYS_io_operation
        val rtsFunction = Constnt o ioOp

        fun mkEval (ct, clist)   =
        Eval {
            function = ct,
            argList = List.map(fn c => (c, GeneralType)) clist,
            resultType=GeneralType
        }
    in
        fun mkNot arg = mkEval (rtsFunction POLY_SYS_not_bool, [arg])
    end

    datatype inlineTest =
        TooBig
    |   NonRecursive
    |   TailRecursive of bool vector
    |   NonTailRecursive of bool vector

    fun evaluateInlining(function, numArgs, maxInlineSize) =
    let
        (* This checks for the possibility of inlining a function.  It sees if it is
           small enough according to some rough estimate of the cost and it also looks
           for recursive uses of the function.
           Typically if the function is small enough to inline there will be only
           one recursive use but we consider the possibility of more than one.  If
           the only uses are tail recursive we can replace the recursive calls by
           a Loop with a BeginLoop outside it.  If there are non-tail recursive
           calls we may be able to lift out arguments that are unchanged.  For
           example for fun map f [] = [] | map f (a::b) = f a :: map f b 
           it may be worth lifting out f and generating specific mapping
           functions for each application. *)
        val hasRecursiveCall = ref false (* Set to true if rec call *)
        val allTail = ref true (* Set to false if non recursive *)
        (* An element of this is set to false if the actual value if anything
           other than the original argument.  At the end we are then
           left with the arguments that are unchanged. *)
        val argMod = Array.array(numArgs, true)

        infix 6 --
        (* Subtract y from x but return 0 rather than a negative number. *)
        fun x -- y = if x >= y then x-y else 0

        fun checkUse _ (_, 0, _) = 0 (* The function is too big to inline. *)
        
        |   checkUse _ (MatchFail, cl, _) = cl -- 1
        |   checkUse isMain (AltMatch(a, b), cl, _) = checkUse isMain (a, checkUse isMain (b, cl -- 1, false), false)

        |   checkUse isMain (Newenv(decs, exp), cl, isTail) =
            let
                fun checkBind (Declar{value, ...}, cl) = checkUse isMain(value, cl, false)
                |   checkBind (RecDecs decs, cl) = List.foldl(fn ({lambda, ...}, n) => checkUse isMain (Lambda lambda, n, false)) cl decs
                |   checkBind (NullBinding c, cl) = checkUse isMain (c, cl, false)
            in
                checkUse isMain (exp, List.foldl checkBind cl decs, isTail)
            end

        |   checkUse _      (Constnt w, cl, _) = if isShort w then cl else cl -- 1

            (* A recursive reference in any context other than a call prevents any inlining. *)
        |   checkUse true   (Extract LoadRecursive, _, _) = 0
        |   checkUse _      (Extract _, cl, _) = cl -- 1

        |   checkUse isMain (Indirect{base, ...}, cl, _) = checkUse isMain (base, cl -- 1, false)

        |   checkUse _      (Lambda {body, argTypes, closure, ...}, cl, _) =
                (* For the moment, any recursive use in an inner function prevents inlining. *)
                if List.exists (fn LoadRecursive => true | _ => false) closure
                then 0
                else checkUse false (body, cl -- (List.length argTypes + List.length closure), false)

        |   checkUse true (Eval{function = Extract LoadRecursive, argList, ...}, cl, isTail) =
            let
                (* If the actual argument is anything but the original argument
                   then the corresponding entry in the array is set to false. *)
                fun testArg((exp, _), n) =
                (
                    if (case exp of Extract(LoadArgument a) => n = a | _ => false)
                    then ()
                    else Array.update(argMod, n, false);
                    n+1
                )
            in
                List.foldl testArg 0 argList;
                hasRecursiveCall := true;
                if isTail then () else allTail := false;
                List.foldl(fn ((e, _), n) => checkUse true (e, n, false)) (cl--3) argList
            end

        |   checkUse isMain (Eval{function, argList, ...}, cl, _) =
                checkUse isMain (function, List.foldl(fn ((e, _), n) => checkUse isMain (e, n, false)) (cl--2) argList, false)

        |   checkUse isMain (Cond(i, t, e), cl, isTail) =
                checkUse isMain (i, checkUse isMain (t, checkUse isMain (e, cl -- 2, isTail), isTail), false)
        |   checkUse isMain (BeginLoop { loop, arguments, ...}, cl, _) =
                checkUse isMain (loop, List.foldl (fn (({value, ...}, _), n) => checkUse isMain (value, n, false)) cl arguments, false)
        |   checkUse isMain (Loop args, cl, _) = List.foldl(fn ((e, _), n) => checkUse isMain (e, n, false)) cl args
        |   checkUse isMain (Raise c, cl, _) = checkUse isMain (c, cl -- 1, false)
        |   checkUse _      (Ldexc, cl, _) = cl -- 1
        |   checkUse isMain (Handle {exp, handler}, cl, isTail) =
                checkUse isMain (exp, checkUse isMain (handler, cl, isTail), false)
        |   checkUse isMain (Recconstr tuple, cl, _) = checkUseList isMain (tuple, cl)
        |   checkUse _      (Container _, cl, _) = cl -- 1

        |   checkUse isMain (SetContainer{container, tuple = Recconstr tuple, ...}, cl, _) =
                (* This can be optimised *)
                checkUse isMain (container, checkUseList isMain (tuple, cl), false)
        |   checkUse isMain (SetContainer{container, tuple, size}, cl, _) =
                checkUse isMain (container, checkUse isMain (tuple, cl -- size, false), false)

        |   checkUse isMain (TupleFromContainer(container, len), cl, _) = checkUse isMain (container, cl -- (len+2), false)
        |   checkUse isMain (ConstntWithInline(w, _), cl, _) = checkUse isMain (Constnt w, cl, false)
        |   checkUse isMain (TagTest{test, ...}, cl, _) = checkUse isMain (test, cl -- 1, false)
        
        and checkUseList isMain (elems, cl) =
            List.foldl(fn (e, n) => checkUse isMain (e, n, false)) cl elems
        
        val costLeft = checkUse true (function, maxInlineSize, true)
    in
        if costLeft = 0
        then TooBig
        else if not (! hasRecursiveCall) 
        then NonRecursive
        else if ! allTail then TailRecursive(Array.vector argMod)
        else NonTailRecursive(Array.vector argMod)
    end

    local
        (* This transforms the body of a "small" recursive function replacing any reference
           to the arguments by the appropriate entry and the recursive calls themselves
           by either a Loop or a recursive call. *)
        fun mapCodeForFunctionRewriting(code, argMap, modVec, transformCall) =
        let
            fun repEntry(Extract(LoadArgument n)) = SOME(Extract(Vector.sub(argMap, n)))
            |   repEntry(Lambda{ body, isInline, name, closure, argTypes, resultType, localCount }) =
                    (* Process the closure but leave the body untouched *)
                    SOME(
                        Lambda {
                            body = body, isInline = isInline, name = name,
                            closure = map (fn (LoadArgument n) => Vector.sub(argMap, n) | l => l) closure,
                            argTypes = argTypes, resultType = resultType, localCount = localCount
                        }
                    )
            |   repEntry(Eval { function = Extract LoadRecursive, argList, resultType }) =
                let
                    (* Filter arguments to include only those that are changed and map any values we pass.
                       They may include references to the parameters. *)
                    fun mapArg((arg, argT)::rest, n) =
                        if Vector.sub(modVec, n) then mapArg(rest, n+1)
                        else (mapCode arg, argT) :: mapArg(rest, n+1)
                    |   mapArg([], _) = []
                in
                    SOME(transformCall(mapArg(argList, 0), resultType))
                end
            |   repEntry _ = NONE
        
            and mapCode code = mapCodetree repEntry code
        in
            mapCode code
        end
    in
        (* If we have a tail recursive function we can replace the tail calls
           by a loop.  modVec indicates the arguments that have not changed. *)
        fun replaceTailRecursiveWithLoop(body, argTypes, modVec, nextAddress) =
        let
            (* We need to create local bindings for arguments that will change.
               Those that do not can be reused. *)
            local
                fun mapArgs((argT, use):: rest, n, decs, mapList) =
                    if Vector.sub(modVec, n)
                    then mapArgs (rest, n+1, decs, LoadArgument n :: mapList)
                    else
                    let
                        val na = ! nextAddress before nextAddress := !nextAddress + 1
                    in
                        mapArgs (rest, n+1, ({addr = na, value = mkLoadArgument n, use=use}, argT) :: decs, LoadLocal na :: mapList)
                    end
                |   mapArgs([], _, decs, mapList) = (List.rev decs, List.rev mapList)
                val (decs, mapList) = mapArgs(argTypes, 0, [], [])
            in
                val argMap = Vector.fromList mapList
                val loopArgs = decs
            end
        
        in
            BeginLoop { arguments = loopArgs, loop = mapCodeForFunctionRewriting(body, argMap, modVec, fn (l, _) => Loop l) }
        end

        (* If we have a small recursive function where some arguments are passed
           through unchanged we can transform it by extracting the
           stable arguments and only passing the changing arguments.  The
           advantage is that this allows the stable arguments to be inserted
           inline which is important if they are functions. The canonical
           example is List.map. *)
        fun liftRecursiveFunction(body, argTypes, modVec, closureSize, name, resultType, localCount) =
        let
            local
                fun getArgs((argType, use)::rest, nArg, clCount, argCount, stable, change, mapList) =
                    let
                        (* This is the argument from the outer function.  It is either added
                           to the closure or passed to the inner function. *)
                        val argN = LoadArgument nArg
                    in
                        if Vector.sub(modVec, nArg)
                        then getArgs(rest, nArg+1, clCount+1, argCount,
                                    argN :: stable, change, LoadClosure clCount :: mapList)
                        else getArgs(rest, nArg+1, clCount, argCount+1,
                                    stable, (Extract argN, argType, use) :: change, LoadArgument argCount :: mapList)
                    end
                |   getArgs([], _, _, _, stable, change, mapList) =
                        (List.rev stable, List.rev change, List.rev mapList)
            in
                (* The stable args go into the closure.  The changeable args are passed in. *)
                val (stableArgs, changeArgsAndTypes, mapList) =
                    getArgs(argTypes, 0, closureSize, 0, [], [], [])
                val argMap = Vector.fromList mapList
            end

            val subFunction =
                Lambda {
                    body = mapCodeForFunctionRewriting(body, argMap, modVec, 
                            fn (l, t) => Eval {
                                function = Extract LoadRecursive, argList = l, resultType = t
                            }),
                    isInline = NonInline, (* Don't inline this function. *)
                    name = name ^ "()",
                    closure = List.tabulate(closureSize, fn n => LoadClosure n) @ stableArgs,
                    argTypes = List.map (fn (_, t, u) => (t, u)) changeArgsAndTypes,
                    resultType = resultType,
                    localCount = localCount
                }
        in
            Eval {
                function = subFunction,
                argList = map (fn (c, t, _) => (c, t)) changeArgsAndTypes,
                resultType = resultType
            }
        end
    end

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
    type optVal = codetree * codeBinding list * envSpecial

    fun mkEnv([], exp) = exp
    |   mkEnv(decs, exp as Extract(LoadLocal loadAddr)) =
        (
            (* A common case is where we have a binding as the last item
               and then a load of that binding.  Reduce this so other
               optimisations are possible.
               This is still something of a special case that could/should
               be generalised. *)
            case List.last decs of
                Declar{addr=decAddr, value, ... } =>
                    if loadAddr = decAddr
                    then mkEnv(List.take(decs, List.length decs - 1), value)
                    else Newenv(decs, exp)
            |   _ => Newenv(decs, exp)
        )
    |   mkEnv(decs, exp) = Newenv(decs, exp)

    (* Wrap up the general and any decs. *)
    fun getGeneral (gen, [], _: envSpecial) = gen
    |   getGeneral (gen, decs, _) = mkEnv(decs, gen)

    fun envGeneralToCodetree(EnvGenLoad ext) = Extract ext
    |   envGeneralToCodetree(EnvGenConst w) = Constnt w


    val initTrans = 10; (* Initial size of arrays. *)

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

    (* Optimiser context. *)
    type optContext =
    {
        lookupAddr: loadForm -> envGeneral * envSpecial,
        enterAddr: int * (envGeneral * envSpecial) -> unit,
        nextAddress : int ref,
        loopFilter: (codetree * argumentType) list -> (codetree * argumentType) list,
        debugArgs: Universal.universal list
    }

    fun loopFilterError _ = raise InternalError "Loop instruction without BeginLoop"

    (* Function to build a closure.  Items are added to the closure if they are not already there. *)
    fun rebuildClosure (closureList: (loadForm * int) list ref) (ext: loadForm): loadForm =
        case (List.find (fn (l, _) => l = ext) (!closureList), ! closureList) of
            (SOME(_, n), _) => (* Already there *) LoadClosure n
        |   (NONE, []) => (* Not there - first *) (closureList := [(ext, 0)]; LoadClosure 0)
        |   (NONE, cl as (_, n) :: _) => (closureList := (ext, n+1) :: cl; LoadClosure(n+1))


    (* Prepares a binding for entry into a look-up table.  Returns the entry
       to put into the table together with any bindings that must be made.
       If the general part of the optVal is a constant we can just put the
       constant in the table. If it is a load (Extract) it is just renaming
       an existing entry so we can return it.  Otherwise we have to make
       a new binding and return a load (Extract) entry for it. *)
    fun makeNewDecl((gen, decs, spec), { nextAddress, ...}: optContext) =
        case gen of
            Constnt w => ((EnvGenConst w, spec), decs)
                (* No need to create a binding for a constant. *)

        |   Extract ext => ((EnvGenLoad ext, spec), decs)
                (* Declaration is simply giving a new name to a variable
                   - can ignore this declaration. *) 

        |   gen =>
            let (* Create a binding for this value. *)
                val decSpval = ! nextAddress before (nextAddress := !nextAddress + 1)
            
                (* The table entry is similar to the result of the expression except
                    that the declarations are taken off and put into the containing
                    block, and the general value is put into a local variable and
                    replaced by an instruction to load from there. If the special
                    is a non-inline function it is removed. *)
            in
                ((EnvGenLoad(LoadLocal decSpval), spec), decs @ [mkDec(decSpval, gen)])
            end

    (* Returns only the general value from an expression.  Used when we're
       not interested in trying to treat the result of a sub-expression specially. *)
    fun general context pt: codetree = getGeneral(optimise(pt: codetree, context))

    (* The main optimisation routine. *)
    and optimise (pt: codetree as MatchFail, _: optContext): optVal = (pt, [], EnvSpecNone)

    |   optimise (AltMatch(a, b), context) = (AltMatch(general context a, general context b), [], EnvSpecNone)
       
    |   optimise (Eval {function, argList, resultType}, context) =
        let
            (* Function call - This may involve inlining the function. *)

            (* Get the function to be called and see if it is inline or
               a lambda expression. *)
            val funct as (genFunct, decsFunct, specFunct) = optimise(function, context)

            (* There are essentially two cases to consider - the function
               may be "inline" in which case it must be expanded as a block,
               or it must be called. *)
        in
            case (function, specFunct, genFunct) of
                (Extract LoadRecursive, _, _) =>
                let
                    (* Ignore any inline part. *)
                    val copiedArgs = map (fn (arg, argType) => (general context arg, argType)) argList
                    val evCopiedCode = 
                        Eval {function = genFunct, argList = copiedArgs, resultType=resultType}
                in
                    (evCopiedCode, decsFunct, EnvSpecNone)
                end

            |   (_, EnvSpecInlineFunction(lambda, env), _) =>
                    expandInlineFunction(funct, function, lambda, env, argList, context)

            |   (_, _, gen as Constnt w) => (* Not inlinable - constant function. *)
                let
                    val copiedArgs = map (fn (arg, argType) => (general context arg, argType)) argList
                    (* If the function is an RTS call that is safe to evaluate immediately and all the
                       arguments are constants evaluate it now. *)
                    val evCopiedCode = 
                        if isIoAddress(toAddress w) andalso earlyRtsCall w andalso List.all (isConstnt o #1) copiedArgs
                        then callRTSFunction(w, List.map #1 copiedArgs)
                        else Eval {function = gen, argList = copiedArgs, resultType=resultType}
                in
                    (evCopiedCode, decsFunct, EnvSpecNone)
                end

            | (_, _, gen) => (* Anything else. *)
                let
                    val copiedArgs = map (fn (arg, argType) => (general context arg, argType)) argList
                    val evCopiedCode = 
                        Eval {function = gen, argList = copiedArgs, resultType=resultType}
                in
                    (evCopiedCode, decsFunct, EnvSpecNone)
                end

        end (* Eval { } *)
        
     |  optimise (Extract ext, {lookupAddr, ...}: optContext) =
        let
            val (gen, spec) = lookupAddr ext
        in
            (envGeneralToCodetree gen, [], spec)
        end

     |  optimise (pt as Constnt _, _) = (pt, [], EnvSpecNone) (* Return the original constant. *)
     
     |  optimise(Lambda lambda, context) =
        let
            val (general, special) = optimiseLambda(lambda, context, NONE, NONE)
        in
            (general, [], special)
        end
           
     |  optimise (BeginLoop{loop=body, arguments=args, ...},
              context as { enterAddr, lookupAddr, nextAddress, debugArgs, ...}: optContext) =
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
                    (mkEnv(List.map (Declar o #1) args, body),
                        {enterAddr = enterAddr, lookupAddr = lookupAddr, nextAddress=nextAddress,
                         loopFilter=filterArgs, debugArgs=debugArgs})
        in
            if not (! loops)
            then (* The Loop instructions have been optimised away.  Since there's
                    no BeginLoop we can reprocess it with the surrounding
                    tail recursion. *)
                optimise(mkEnv(List.map (Declar o #1) args, body), context)
            else (* It loops - have to reprocess. *)
            let
                (* The arguments to the functions are Declar entries but they
                   must not be optimised. *)
                fun declArg({addr, value, use, ...}, typ) =
                    let
                        val optVal = optimise(value, context)
                        val decSpval = ! nextAddress
                        val _ = nextAddress := decSpval + 1
                    in
                        enterAddr(addr, (EnvGenLoad(LoadLocal decSpval), EnvSpecNone));
                        ({addr = decSpval, value = getGeneral optVal, use = use }, typ)
                    end
                 val declArgs = map declArg args
                 val beginBody =
                    optimise(body,
                        {enterAddr = enterAddr, lookupAddr = lookupAddr, nextAddress=nextAddress, loopFilter=filterArgs, debugArgs=debugArgs})
            in
                (BeginLoop {loop=getGeneral beginBody, arguments=declArgs}, [], EnvSpecNone)
            end
        end

     |  optimise (Loop args, context as { loopFilter, ... }) =
            (* The Loop instruction should be at the tail of the
               corresponding BeginLoop. *)
            let
                fun gen(c, t) = (general context c, t)
            in
                (Loop (loopFilter(map gen args)), [], EnvSpecNone)
            end
          
     |  optimise (Raise x, context) = (Raise (general context x), [], EnvSpecNone)
         
     |  optimise (Cond(condTest, condThen, condElse), context as { nextAddress, ...}) =
        let
            val insFirst = general context condTest
        in
            (* If the condition is a constant we need only
               return the appropriate arm. *)
            case insFirst of
                Constnt testResult =>
                    if wordEq (testResult, False) (* false - return else-part *)
                    then (* if false then x else y == y *)
                        optimise(condElse, context)
                    (* if true then x else y == x *)
                    else optimise(condThen, context)  (* return then-part *)
            
            |   _ => (* Condition is not a constant. *)
                let
                    (* Perhaps the "if" is really a simpler expression?
                       Unfortunately, we don't know whether we're returning
                       a boolean result here so we can't optimise to
                       andalso/orelse but we can at least look for the
                       case where both results are constants. *)
                    val insSecond as (thenGen, thenDecs, _) = optimise(condThen, context)
                    val insThird as (elseGen, elseDecs, _)  = optimise(condElse, context)

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
                                    else mkDec(addr, mkLoadLocal containerAddr) :: tl
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
                                            mkLoadLocal containerAddr, Recconstr recEntries,
                                            size))

                        val thenPart = createBranch(thenRec, thenDecs, thenAddr)
                        and elsePart = createBranch(elseRec, elseDecs, elseAddr)
                        (* The result is a block which declares the container, side-effects it
                           in the "if" and makes a tuple from the result.  If we're lucky
                           the resulting tuple will be optimised away. *)
                        (* This code is the same as that used to optimise TupleFromContainer
                           and is designed to allow us to optimise away the tuple creation
                           if we use the individual fields. *)
                        val baseAddr = !nextAddress before nextAddress := !nextAddress + size
                        val specialDecs =
                            List.tabulate(size,
                                fn n => mkDec(n+baseAddr, mkInd(n, mkLoadLocal containerAddr)))
                        val recAddr = !nextAddress before nextAddress := !nextAddress + 1
                    in
                        (   mkLoadLocal recAddr,
                            mkDec(containerAddr, Container size) ::
                                  NullBinding(Cond(insFirst, thenPart, elsePart)) ::
                                    (specialDecs @ [mkDec(recAddr, TupleFromContainer(mkLoadLocal containerAddr, size))]),
                            EnvSpecTuple(size, fn i => (EnvGenLoad(LoadLocal(i+baseAddr)), EnvSpecNone))
                         )
                    end (* combineTuples *)
                in
                    (* Optimise various cases depending on what the then- and else-parts return. *)
                    (* TODO: I don't think this works because of the way tuples are made.
                       The "general" part is a LoadLocal rather than a Reccconstr. *)
                    case (thenGen, thenDecs, elseGen, elseDecs) of
                        (second as Constnt c2, [], third as Constnt c3, []) =>
                            (* The results of the then- and else-parts are just constants. *)
                            (* if x then y else y == (x; y) *)
                        if wordEq (c2, c3)
                        then if sideEffectFree insFirst
                        then insSecond
                        else (* Must put insFirst in decs, so it gets executed *)
                            (second, [NullBinding insFirst], EnvSpecNone)
                  
                        (* if x then true else false == x *)
                        else if wordEq (c2, True) andalso wordEq (c3, False)
                        then (insFirst, [], EnvSpecNone)
              
                        (* if x then false else y == not x *)
                        else if wordEq (c2, False) andalso wordEq (c3, True)
                        then (mkNot insFirst, [], EnvSpecNone)
              
                        else (* can't optimise *) (Cond (insFirst, second, third), [], EnvSpecNone)

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
                            val containerAddr = let val ad = !nextAddress in nextAddress := ad + 1; ad end
                        in
                            combineTuples(containerAddr, NONE, NONE, thenRec, elseRec, size)
                        end
                        else (* Different sizes - use default. *)
                            (Cond (insFirst, getGeneral insSecond, getGeneral insThird), [], EnvSpecNone)

                        (* TODO: Should we also consider a Extract(LoadArgument _) or is that not relevant? *)
                    |   (TupleFromContainer(Extract(LoadLocal thenAddr), thenSize), _,
                         TupleFromContainer(Extract(LoadLocal elseAddr), elseSize), _) =>
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
                            (Cond (insFirst, getGeneral insSecond, getGeneral insThird), [], EnvSpecNone)

                    |   (TupleFromContainer(Extract(LoadLocal thenAddr), thenSize), _,
                         Recconstr elseRec, _) =>
                        (* The then-part has already been converted *)
                        if thenSize = List.length elseRec
                        then combineTuples(thenAddr, SOME thenAddr, NONE, [], elseRec, thenSize)
                        else (* Different sizes - use default. *)
                            (Cond (insFirst, getGeneral insSecond, getGeneral insThird), [], EnvSpecNone)

                    |   (Recconstr thenRec, _,
                         TupleFromContainer(Extract(LoadLocal elseAddr), elseSize), _) =>
                        (* The else-part has already been converted *)
                        if elseSize = List.length thenRec
                        then
                            combineTuples(elseAddr, NONE, SOME elseAddr, thenRec, [], elseSize)
                        else (* Different sizes - use default. *)
                            (Cond (insFirst, getGeneral insSecond, getGeneral insThird), [], EnvSpecNone)

                    |   (TupleFromContainer(Extract _, _), _, _, _) => raise InternalError "combinetuples: TODO"
                    
                    |   (_, _, TupleFromContainer(Extract _, _), _) => raise InternalError "combinetuples: TODO"

                     |   _ => (* Not constants or records. *)
                            (Cond (insFirst, getGeneral insSecond, getGeneral insThird), [], EnvSpecNone)
                end
        end (* Cond ... *)
         
     |  optimise (Newenv(envDecs, envExp),
                  context: optContext as  {enterAddr, lookupAddr, nextAddress, debugArgs, ... }) =
        let
            (* Recurses down the list of declarations and expressions processing
               each, and then reconstructs the list on the way back. *)
            fun copyDeclarations []  =
                (* End of the list - process the result expression. *)
                    optimise(envExp, context)

            |   copyDeclarations (Declar{addr=caddr, value, ...} :: vs) = 
                let
                    (* Add the declaration to the table. *)
                    val (optV, dec) = makeNewDecl(optimise(value, context), context)
                    val () = enterAddr(caddr, optV)                  
                    (* Deal with the rest of the block. *)
                    val (rGen, rDecs, rSpec) = copyDeclarations vs
                in
                    (rGen, dec @ rDecs, rSpec)
                end

            |   copyDeclarations (RecDecs mutuals :: vs) = 
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
                   can. *)
                let
                    (* Reorder the function so the explicitly-inlined ones come first.
                       Their code can then be inserted into the main functions. *)
                    local
                        val (inlines, nonInlines) =
                            List.partition (
                                fn {lambda = { isInline=MaybeInline, ...}, ... } => true | _ => false) mutuals
                    in
                        val orderedDecs = inlines @ nonInlines
                    end
                    
                    (* Go down the functions creating new addresses for them and entering them in the table. *)
                    val startAddr = !nextAddress
                    val addresses =
                        map (fn {addr, ... } =>
                            let
                                val decAddr = !nextAddress before nextAddress := !nextAddress + 1;
                            in
                                enterAddr (addr, (EnvGenLoad(LoadLocal decAddr), EnvSpecNone));
                                decAddr
                            end)
                        orderedDecs
                    val endAddr = !nextAddress

                    (* We really want to be able to compile the functions now if we can
                       and get a constant for the code address.  We can do that for
                       functions that make no non-local references or whose non-local
                       references are by means of constants.  For non-recursive
                       declarations this is easy since an earlier declaration cannot
                       depend on a later one but for mutually recursive declarations we
                       don't know the dependencies.
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

                    fun processFunctions ({ lambda, addr = decAddr, ... }, decSpval, (decs, otherChanges)) =
                    (* Have a look at the old entry to see if it's a constant. *)
                    let
                        val (oldGeneral, oldSpecial) = lookupAddr(LoadLocal decAddr)
                    in
                        case oldGeneral of
                            EnvGenConst w =>
                                ({addr=decSpval, value=Constnt w, use=[]} :: decs, otherChanges) (* It's already a constant - don't reprocess. *)
                        |   _ =>
                            let
                                (* Now copy this entry. *)
                                val (gen, spec) = optimiseLambda(lambda, context, SOME decAddr, SOME decSpval)

                                (* The general value is either a reference to the
                                   declaration or a constant if the function has just
                                   been compiled into a code segment. *)
                                val isConstant = isConstnt gen

                                (* Set hasNonLocalReference if the closure contains anything except
                                   another of these mutual decs. *)
                                fun refersToAnother(LoadLocal addr) =
                                        if addr >= startAddr andalso addr < endAddr
                                        then ()
                                        else hasNonLocalReference := true
                                |   refersToAnother _ = hasNonLocalReference := true

                                val optGen =
                                (* The general value may be a constant if we've compiled it now or
                                   a lambda if we haven't. *)
                                    case gen of
                                        Constnt w => EnvGenConst w
                                    |   Lambda{closure, ...} =>
                                        (
                                            List.app refersToAnother closure;
                                            EnvGenLoad (LoadLocal decSpval)
                                        )
                                    |   _ => raise InternalError "processNonInlines: Not a function";
                                fun isSome EnvSpecNone = false | isSome _ = true
                                val nowInline = isSome spec andalso not (isSome oldSpecial)
                                (* If this is now a constant or it is a small function when it
                                   wasn't before we need to reprocess everything
                                   which depends on it to try to get the constant inserted
                                   everywhere it can be. *)
                          in
                                enterAddr (decAddr, (optGen, spec));
                                (
                                    {addr=decSpval, value=gen, use=[]} :: decs,
                                    otherChanges orelse isConstant orelse nowInline
                                )
                          end
                   end

                  fun repeatProcess () =
                  let
                      val (decs, haveChanged) =
                         (* Use foldr here to keep the result in the same order
                            in case we can compile them immediately below. *)
                         ListPair.foldr processFunctions
                            ([], false) (orderedDecs, addresses);
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

                        fun toLambda{addr, value=Lambda lambda, use} =
                            {addr=addr, lambda=lambda, use = use}
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
                                    fn ({addr, ...}) => mkLoadLocal addr)
                                    decs
                            val code = mkEnv(convertDecs decs, mkTuple extracts)
                            (* Code generate it. *)
                            val results = codeGenerateToConstant debugArgs (code, !nextAddress+1)

                            fun reprocessDec({addr=decAddr, ...}, decSpval, (offset, others)) =
                            let
                                val (_, oldSpec) = lookupAddr(LoadLocal decAddr)
                            in
                                let
                                    val newConstant = findEntryInBlock(results,  offset, false)
                                    val enterConst =
                                        case newConstant of
                                            Constnt w => EnvGenConst w
                                        |   _ => raise InternalError "reprocessDec: Not a constant"
                                in
                                    (* Replace the entry by an entry with a constant. *)
                                    enterAddr(decAddr, (enterConst, oldSpec));
                                    (offset+1, {addr=decSpval, value=newConstant, use=[]} :: others)
                                end
                            end
                        
                            val (_, newDecs) = ListPair.foldl reprocessDec (0, []) (orderedDecs, addresses);
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
                    val (cGen, cDecs, _) = optimise(v, context)
                    val (rGen, rDecs, rSpec) = copyDeclarations vs
                in  (* This must be a statement whose
                       result is ignored. Put it into the declaration list. *)
                    (rGen, cDecs @  (NullBinding cGen :: rDecs), rSpec)
                end (* copyDeclarations *)

        in
            copyDeclarations envDecs
        end (* Newenv(... *)
          
    |   optimise (Recconstr entries, context as { nextAddress, ...}) =
         (* The main reason for optimising record constructions is that they
            appear as tuples in ML. We try to ensure that loads from locally
            created tuples do not involve indirecting from the tuple but can
            get the value which was put into the tuple directly. If that is
            successful we may find that the tuple is never used directly so
            the use-count mechanism will ensure it is never created. *)
        let
            val tupleSize = List.length entries
            (* The record construction is treated as a block of local
               declarations so that any expressions which might have side-effects
               are done exactly once. *)
            val (fieldEntries, bindings) =
                ListPair.unzip(
                    List.map(fn h => makeNewDecl(optimise(h, context), context)) entries)

            val generalFields = List.map (fn (a, _) => envGeneralToCodetree a) fieldEntries

            fun env addr = List.nth(fieldEntries, addr)

            val newRec  = Recconstr generalFields
        in
            if List.all isConstnt generalFields
            then (* If all the general values are constants we can create the tuple now. *)
                (makeConstVal newRec, List.foldr(op @) [] bindings, EnvSpecTuple(tupleSize, env))
            else
            let
                val newAddr = !nextAddress before nextAddress := !nextAddress + 1
            in
                (mkLoadLocal newAddr, List.foldr(op @) [] bindings @ [mkDec(newAddr, newRec)], EnvSpecTuple(tupleSize, env))
            end
        end
          
      |  optimise (Indirect{ base, offset, isVariant }, context) =
            let
                val (genSource, decSource, specSource) = optimise(base, context)
            in
                (* Try to do the selection now if possible. *)
                case (specSource, genSource) of
                    (EnvSpecTuple(_, recEnv), _) =>
                    let
                        (* The "special" entry we've found is a tuple.  That means that
                           we are taking a field from a tuple we made earlier and so we
                           should be able to get the original code we used when we made
                           the tuple.  That might mean the tuple is never used and
                           we can optimise away the construction of it completely. *)
                        val (newGen, newSpec) = recEnv offset
                    in
                        (envGeneralToCodetree newGen, decSource, newSpec)
                    end

                | (_ , gen as Constnt _ ) => (* General is a constant -  Do the selection now. *)
                        (findEntryInBlock(gen, offset, isVariant), decSource, EnvSpecNone)
                           
                | (_, _) => (* No special case possible. *)
                    ((if isVariant then mkVarField else mkInd) (offset, genSource), decSource, EnvSpecNone)
            end
       
      |  optimise (pt as Ldexc, _) = (pt, [], EnvSpecNone) (* just a constant so return it *)
        
      |  optimise (Handle { exp, handler }, context) =
            (Handle {exp = general context exp, handler = getGeneral(optimise(handler, context))}, [], EnvSpecNone)

      |  optimise (c as Container _, _) = (c, [], EnvSpecNone)

      |  optimise (TupleFromContainer(container, size), context as { nextAddress, ...}) =
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
            val (genCont, decsCont, _) = optimise(container, context)
            (* Since "container" will always be an Extract entry we can have multiple
               references to it in the declarations.  Include an assertion to that
               effect just in case future changes make that no longer true. *)
            val _ =
                case genCont of
                   Extract _ => ()
                | _ => raise InternalError "optimise - container is not Extract"
            val baseAddr = !nextAddress before nextAddress := !nextAddress + size
            val specialDecs =
                List.tabulate(size, fn n => mkDec(n+baseAddr, mkInd(n, genCont)))
            val recAddr = !nextAddress before nextAddress := !nextAddress + 1
        in
            (
                mkLoadLocal recAddr,
                decsCont @ specialDecs @ [mkDec(recAddr, TupleFromContainer(genCont, size))],
                EnvSpecTuple(size, fn n => (EnvGenLoad(LoadLocal(n+baseAddr)), EnvSpecNone))
            )
        end

      |  optimise (SetContainer{container, tuple as Cond _ , size}, context) =
            (* mkSetContainer transforms this. *)
            optimise(mkSetContainer(container, tuple, size), context)

      |  optimise (SetContainer{container, tuple as Newenv _ , size}, context) =
            (* mkSetContainer transforms this. *)
            optimise(mkSetContainer(container, tuple, size), context)

      |  optimise (SetContainer{container, tuple, size}, context) =
            (* Push the set-container down the tree and then process it. If we've
               expanded an inline function we want to be able to find any
               tuple we're creating. *)
            let
                val optCont = general context container
                and optTuple = general context tuple
                val () =
                    case optCont of
                        Extract(LoadLocal _) => () (* Local *)
                    |   Extract(LoadArgument _) => () (* Local argument *)
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

                |   pushSetContainer(tuple as TupleFromContainer(Extract(LoadLocal innerAddr), innerSize), decs) =
                    if innerSize = size
                    then
                    let
                        (* We can remove the inner container and replace it by
                           a reference to the outer. *)
                        fun replaceContainerDec [] =
                            raise InternalError "replaceContainerDec"
                          | replaceContainerDec ((hd as Declar{addr, ...}) :: tl) =
                                if addr = innerAddr
                                then mkDec(addr, optCont) :: tl
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
                    else mkEnv(List.rev decs, mkSetContainer(optCont, tuple, size))

                |   pushSetContainer(tuple, decs) =
                        mkEnv(List.rev decs, mkSetContainer(optCont, tuple, size))
              
            in
                (pushSetContainer(optTuple, []), [], EnvSpecNone)
            end

      |  optimise (ConstntWithInline (w, spec), _) = (Constnt w, [], spec)
            (* These are global values.  Just return them. *)

      |  optimise (TagTest{test, tag, maxTag}, context) =
            let
                val optTest = general context test
            in
                case optTest of
                    Constnt testResult =>
                        if isShort testResult andalso toShort testResult = tag
                        then (CodeTrue, [], EnvSpecNone)
                        else (CodeFalse, [], EnvSpecNone)
                |   _ => (TagTest{test=optTest, tag=tag, maxTag=maxTag}, [], EnvSpecNone)
            end

    and optimiseLambda(original as ({isInline=OnlyInline, closure, ...}), { lookupAddr, ...}, _, _) =
        (* Used only for functors.  Leave unchanged. *)
            (CodeZero,
              (* Because a functor can only ever be applied we don't need a general value here.
                 We return the original body and the old environment.  Generally the environment
                 won't be needed since all free references will be to code that has already
                 been compiled and they will be constants.  The exception is if we have a
                 structure followed by a functor without an intervening semicolon. *)
              EnvSpecInlineFunction(original, fn addr => lookupAddr(List.nth(closure, addr)))
            )

     |  optimiseLambda(original as ({body=lambdaBody, isInline=lambdaInline, name=lambdaName,
                       argTypes, resultType, closure, ...}),
                       { lookupAddr, debugArgs, nextAddress, ... }: optContext,
                       myOldAddrOpt, myNewAddrOpt): codetree * envSpecial =
        let
            (* A new table for the new function. *)
            val oldAddrTab = stretchArray (initTrans, NONE)

            (* First phase - this is the closure after calling optimise.  It may contain items that
               are not actually used so it is rebuilt in cleanCode. *)
            val optClosureList = ref []

            local
                fun localOldAddr (LoadLocal addr) = valOf(oldAddrTab sub addr)
                |   localOldAddr (ext as LoadArgument _) = (EnvGenLoad ext, EnvSpecNone)
                |   localOldAddr (ext as LoadRecursive) = (EnvGenLoad ext, EnvSpecNone)
                |   localOldAddr (LoadClosure addr) =
                    let
                        val oldEntry = List.nth(closure, addr)
                        (* If the entry in the closure is our own address this is recursive. *)
                        fun isRecursive(EnvGenLoad(LoadLocal a), SOME b) = a = b
                        |   isRecursive _ = false
                    in
                        if isRecursive(EnvGenLoad oldEntry, myOldAddrOpt) then (EnvGenLoad LoadRecursive, EnvSpecNone)
                        else
                        let
                            val newEntry = lookupAddr oldEntry
                            val makeClosure = rebuildClosure optClosureList

                            fun convertResult(genEntry, specEntry) =
                                (* If after looking up the entry we get our new address it's recursive. *)
                                if isRecursive(genEntry, myNewAddrOpt)
                                then (EnvGenLoad LoadRecursive, EnvSpecNone)
                                else
                                let
                                    val newGeneral =
                                        case genEntry of
                                            EnvGenLoad ext => EnvGenLoad(makeClosure ext)
                                        |   EnvGenConst w => EnvGenConst w
                                    (* Have to modify the environment here so that if we look up free variables
                                       we add them to the closure. *)
                                    fun convertEnv env args = convertResult(env args)
                                    val newSpecial =
                                        case specEntry of
                                            EnvSpecTuple(size, env) => EnvSpecTuple(size, convertEnv env)
                                        |   EnvSpecInlineFunction(spec, env) => EnvSpecInlineFunction(spec, convertEnv env)
                                        |   EnvSpecNone => EnvSpecNone
                                in
                                    (newGeneral, newSpecial)
                                end
                        in
                            convertResult newEntry
                        end
                    end

                and setTab (index, v) = update (oldAddrTab, index, SOME v)
            in
                val newAddressAllocator = ref 0

                val newCode =
                    optimise (lambdaBody,
                    {
                        enterAddr = setTab, lookupAddr = localOldAddr,
                        nextAddress=newAddressAllocator,
                        loopFilter=loopFilterError,
                        debugArgs=debugArgs
                    })
            end

            val closureAfterOpt = List.foldl (fn ((ext, _), l) => ext :: l) [] (!optClosureList)

            (* New closure after cleaning. *)
            val cleanClosureList = ref nil
 
            fun lookupInClean closureEntry =
                rebuildClosure cleanClosureList (List.nth(closureAfterOpt, closureEntry))

            fun doclean () = REMOVE_REDUNDANT.cleanProc(getGeneral newCode, [UseGeneral], lookupInClean,
                          ! newAddressAllocator)

            val cleanedBody = doclean()
                
            val finalClosure = List.foldl (fn ((ext, _), l) => ext :: l) [] (!cleanClosureList)
 
            val (inlineType, updatedBody, localCount) =
                case lambdaInline of
                    NonInline =>
                    (
                        case evaluateInlining(cleanedBody, List.length argTypes,
                                DEBUG.getParameter DEBUG.maxInlineSizeTag debugArgs) of
                            NonRecursive  => (SmallFunction, cleanedBody, ! newAddressAllocator)
                        |   TailRecursive bv => (SmallFunction, replaceTailRecursiveWithLoop(cleanedBody, argTypes, bv, newAddressAllocator), ! newAddressAllocator)
                        |   NonTailRecursive bv =>
                                if Vector.exists (fn n => n) bv
                                then (SmallFunction, liftRecursiveFunction(cleanedBody, argTypes, bv, List.length finalClosure, lambdaName, resultType, !newAddressAllocator), 0)
                                else (NonInline, cleanedBody, ! newAddressAllocator) (* All arguments have been modified *)
                        |   TooBig => (NonInline, cleanedBody, ! newAddressAllocator)
                    )
                |   oldInline => (oldInline, cleanedBody, ! newAddressAllocator) (* e.g. MayBeInline *)

            val copiedLambda =
                {
                    body          = updatedBody,
                    isInline      = inlineType,
                    name          = lambdaName,
                    closure       = finalClosure,
                    argTypes      = argTypes,
                    resultType    = resultType,
                    localCount    = localCount
                }
         in
            (
            case inlineType of
                MaybeInline => (* Explicitly inlined functions. *)
                    (* These are usually auxiliary functions that produce the "standard"
                       version of a function and contain a call to the function with multiple
                       arguments.  The idea of these is that we want this function to be
                       expanded inline whereever it is used so that the call to the
                       "standard" (e.g. curried) function is replaced by a call to the
                       more efficient version.
                       We need to return a valid "general" version to keep the back-end
                       happy and provide some code if the standard version is ever used
                       in a context other than a call.  The "special" result we return
                       is the original code with a suitable environment.  Typically
                       we will have called optimiseLambda from the MutualDecs case and
                       we may not yet have processed the main function that this calls.
                       It is tempting to iterate over the environment at this point to
                       produce a vector of (general, special) pairs but that's not a good
                       idea because we may update the outer environment array with some
                       inlineable code for the main function.
                       We can't return the processed code because we would lose the
                       ability to pick up any inline code for the main function. Also
                       we may turn this function into a recursive function if the main
                       function is recursive. *)
                    (
                        Lambda copiedLambda,
                        EnvSpecInlineFunction(original, fn addr => lookupAddr(List.nth(closure, addr)))
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
               val general = 
                    (* If this has no free variables we can code-generate it now. *)
                    if null finalClosure
                    then codeGenerateToConstant debugArgs (Lambda copiedLambda, !nextAddress+1)
                    else Lambda copiedLambda

                val special =
                    if inlineType = NonInline
                    then EnvSpecNone
                    else EnvSpecInlineFunction(copiedLambda, fn addr => (EnvGenLoad(List.nth(finalClosure, addr)), EnvSpecNone))
            in
                (general, special)
            end
            ) before StretchArray.freeze oldAddrTab
        end (* optimiseLambda *)

    (* Expand a function inline, either one marked explicitly to be inlined or one detected as "small". *)
    and expandInlineFunction((_, functDecs, _), original, {body=lambdaBody, localCount, ...}: lambdaForm,
            functEnv: int -> envGeneral * envSpecial, argList,
            context as {nextAddress, debugArgs, lookupAddr, enterAddr, loopFilter, ...}) =
        let
            (* Calling inline proc or a lambda expression which is just called.
               The function is replaced with a block containing declarations
               of the parameters.  We need a new table here because the addresses
               we use to index it are the addresses which are local to the function.
               New addresses are created in the range of the surrounding function. *)
            val localVec = Array.array(localCount, NONE)

            local
                (* Process the arguments.  We have to make a special check here that we are not passing in the function
                   we are trying to expand.  This could result in an infinitely recursive expansion.  It is only
                   going to happen in very special circumstances such as a definition of the Y combinator.  If we
                   do encounter this function we strip the "special" entry so any application will always result
                   in a function call. *)
                val stripRecursiveInline =
                    case original of
                        Extract extOrig =>
                            (
                            fn ext =>
                                if ext = extOrig
                                then (#1(lookupAddr ext), EnvSpecNone)
                                else lookupAddr ext
                            )
                    |   _ => lookupAddr

                val argContext =
                    { lookupAddr=stripRecursiveInline, enterAddr = enterAddr,
                      nextAddress = nextAddress, loopFilter = loopFilter, debugArgs = debugArgs}

                val (params, bindings) =
                    ListPair.unzip(
                        List.map (fn (h, _) => makeNewDecl(optimise(h, argContext), context)) argList)

                val paramVec = Vector.fromList params
            in
                fun getParameter n = Vector.sub(paramVec, n)

                (* Bindings necessary for the arguments *)
                val copiedArgs = List.foldr (op @) [] bindings
            end

            local
                fun localOldAddr(LoadLocal addr) = valOf(Array.sub(localVec, addr))
                |   localOldAddr(LoadArgument addr) = getParameter addr
                |   localOldAddr(LoadClosure closureEntry) = functEnv closureEntry
                |   localOldAddr LoadRecursive = raise InternalError "localOldAddr: LoadRecursive"

                fun setTabForInline (index, v) = Array.update (localVec, index, SOME v)
            in
                val (cGen, cDecs, cSpec) =
                     optimise(lambdaBody,
                      {lookupAddr=localOldAddr, enterAddr=setTabForInline,
                       nextAddress=nextAddress, loopFilter=loopFilterError, debugArgs=debugArgs})
            end

        in
            (*StretchArray.freeze localVec;*)
            (cGen, functDecs @ (copiedArgs @ cDecs), cSpec)
        end

    fun codetreeOptimiser(pt, debugSwitches, numLocals) =
    let
        val localAddressAllocator = ref 0
        val addrTab = stretchArray (numLocals, NONE)
        
        fun lookupAddr (LoadLocal addr) = valOf(addrTab sub addr)
        |   lookupAddr _ = raise InternalError "top level reached"

        (* Make an entry in the "old" table *)
        and enterAddr (addr, tab) = update (addrTab, addr, SOME tab)

        val (rGeneral, rDecs, rSpec: envSpecial) =
            optimise(pt,
                {lookupAddr = lookupAddr, enterAddr = enterAddr,
                 nextAddress=localAddressAllocator,
                 loopFilter=loopFilterError, debugArgs=debugSwitches})
    in
        (* Turn the array into a vector. *)
        StretchArray.freeze addrTab;
        { numLocals = ! localAddressAllocator, general = rGeneral, bindings = rDecs, special = rSpec }
    end

    structure Sharing = struct type codetree = codetree and envSpecial = envSpecial and codeBinding = codeBinding end

end;
