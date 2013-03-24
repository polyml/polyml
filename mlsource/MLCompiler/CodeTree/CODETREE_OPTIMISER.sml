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

    structure SIMPLIFIER:
    sig
        type codetree and codeBinding and envSpecial

        val simplifier:
            codetree * int -> (codetree * codeBinding list * envSpecial) * int * bool
        val specialToGeneral:
            codetree * codeBinding list * envSpecial -> codetree

        structure Sharing:
        sig
            type codetree = codetree
            and codeBinding = codeBinding
            and envSpecial = envSpecial
        end
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
    =   SIMPLIFIER.Sharing
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

        (* Check for the code size and also recursive references.  N,B. We assume in hasLoop
           that tail recursion applies only with Cond, Newenv and Handler. *)
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
        |   checkUse isMain (Tuple{ fields, ...}, cl, _) = checkUseList isMain (fields, cl)
        |   checkUse _      (Container _, cl, _) = cl -- 1

        |   checkUse isMain (SetContainer{container, tuple = Tuple { fields, ...}, ...}, cl, _) =
                (* This can be optimised *)
                checkUse isMain (container, checkUseList isMain (fields, cl), false)
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

    fun optimise (context, use) (Lambda (lambda as { isInline = NonInline, ...} )) =
            SOME(Lambda(optLambda(context, use, lambda)))

    |   optimise (context, use) (Lambda (lambda as { isInline = MaybeInline, ...} )) =
            if List.exists (fn UseExport => true | _ => false) use
            then NONE
            else SOME(Lambda(optLambda(context, use, lambda)))
  
    |   optimise (context, use) (Newenv(envDecs, envExp)) =
        let
            fun mapExp mapUse = mapCodetree (optimise(context, mapUse))

            fun mapbinding(Declar{value, addr, use}) = Declar{value=mapExp use value, addr=addr, use=use}
            |   mapbinding(RecDecs l) =
                let
                    fun mapRecDec {addr, lambda, use} =
                    let
                        val opt = mapExp use (Lambda lambda)
                    in
                        case opt of
                            Lambda res => {addr=addr, use = use, lambda = res }
                        |   _ => raise InternalError "mapbinding: not lambda"
                    end
                in
                    RecDecs(map mapRecDec l)
                end
            |   mapbinding(NullBinding exp) = NullBinding(mapExp [UseGeneral] exp)
        in
            SOME(Newenv(map mapbinding envDecs, mapExp use envExp))
        end

    |   optimise _ _ = NONE

    and optLambda({ debugArgs, reprocess, ... }, use, { body, name, argTypes, resultType, closure, localCount, ...}) =
    let
(*
        val allApply = List.all(fn UseApply _ => true | _ => false)
        fun allCurried [] = false
        |   allCurried l  = List.all(fn UseApply l => allApply l | _ => false) l
        val _ = if allCurried use then print (name ^ " is curried\n") else ()
        
        val _ =
            if null argTypes
            then ()
            else List.app(fn (_, argUse) => if allCurried argUse then print("Arg for " ^ name ^ " is curried\n") else ()) argTypes
*)
        (* Allocate any new addresses after the existing ones. *)
        val addressAllocator = ref localCount
        fun makeAddr() =
            (! addressAllocator) before addressAllocator := ! addressAllocator + 1
        val optContext =
        {
            makeAddr = makeAddr,
            reprocess = reprocess,
            debugArgs = debugArgs
        }
        val optBody = mapCodetree (optimise(optContext, [UseGeneral])) body

        (* See if this should be expanded inline. *)
        val (inlineType, updatedBody, localCount) =
            case evaluateInlining(optBody, List.length argTypes,
                    DEBUG.getParameter DEBUG.maxInlineSizeTag debugArgs) of
                NonRecursive  => (SmallFunction, optBody, ! addressAllocator)
            |   TailRecursive bv =>
                    (SmallFunction,
                        replaceTailRecursiveWithLoop(optBody, argTypes, bv, addressAllocator), ! addressAllocator)
            |   NonTailRecursive bv =>
                    if Vector.exists (fn n => n) bv
                    then (SmallFunction, 
                            liftRecursiveFunction(
                                optBody, argTypes, bv, List.length closure, name, resultType, !addressAllocator), 0)
                    else (NonInline, optBody, ! addressAllocator) (* All arguments have been modified *)
            |   TooBig => (NonInline, optBody, ! addressAllocator)
        (* If this is to be inlined we may need to reprocess. *)
        val () =
            case inlineType of
                SmallFunction => reprocess := true
            |   _ => ()
    in
        {
            body = updatedBody, name = name, argTypes = argTypes, closure = closure,
            resultType = resultType, isInline = inlineType, localCount = localCount
        }
    end

    fun codetreeOptimiser(code, debugSwitches, numLocals) =
    let
        fun topLevel _ = raise InternalError "top level reached"

        fun processTree (code, nLocals, optAgain) =
        let
            (* First run the simplifier.  Among other things this does inline
               expansion and if it does any we at least need to run cleanProc
               on the code so it will have set simpAgain. *)
            val (simpCode, simpCount, simpAgain) = SIMPLIFIER.simplifier(code, nLocals)
        in
            if optAgain orelse simpAgain
            then
            let
                (* Identify usage information and remove redundant code. *)
                val preOptCode =
                    REMOVE_REDUNDANT.cleanProc(
                        SIMPLIFIER.specialToGeneral simpCode, [UseExport], topLevel, simpCount)

                val reprocess = ref false (* May be set in the optimiser *)
                (* Allocate any new addresses after the existing ones. *)
                val addressAllocator = ref simpCount
                fun makeAddr() =
                    (! addressAllocator) before addressAllocator := ! addressAllocator + 1
                val optContext =
                {
                    makeAddr = makeAddr,
                    reprocess = reprocess,
                    debugArgs = debugSwitches
                }
                (* Optimise the code, rewriting it as necessary. *)
                val optCode = mapCodetree (optimise(optContext, [UseExport])) preOptCode
            in
                (* Rerun the simplifier at least. *)
                processTree(optCode, ! addressAllocator, ! reprocess)
            end
            else (simpCode, simpCount) (* We're done *)
        end

        val (postOptCode, postOptCount) = processTree(code, numLocals, true (* Once at least *))
        val (rGeneral, rDecs, rSpec) = postOptCode
    in
        { numLocals = postOptCount, general = rGeneral, bindings = rDecs, special = rSpec }
    end

    structure Sharing = struct type codetree = codetree and envSpecial = envSpecial and codeBinding = codeBinding end

end;
