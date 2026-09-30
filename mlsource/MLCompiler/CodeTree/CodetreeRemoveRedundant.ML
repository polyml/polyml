(*
    Copyright (c) 2012,13, 17, 20-1, 2025 David C.J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

functor CodetreeRemoveRedundant(
    structure BaseCodeTree: BASECODETREE
    structure CodetreeFunctions: CODETREEFUNCTIONS

    sharing BaseCodeTree.Sharing = CodetreeFunctions.Sharing
) :
    sig
        type codetree
        type loadForm
        val cleanProc : (codetree * (int -> loadForm) * int) -> codetree
        structure Sharing: sig type codetree = codetree and loadForm = loadForm end
    end
=
struct
    open BaseCodeTree
    open CodetreeFunctions
    exception InternalError = Misc.InternalError

    type cleanContext =
    {
        locals:       codeUse list array,
        prev:         int * codeUse list -> loadForm,
        recursiveRef: codeUse list -> unit,
        checkArg:     int * codeUse list -> unit
    }

    fun cleanProc (code, prev, localCount) =
    let
        (* Filter the use information so there is only a single instance of each kind of
           usage.  The usage lists can blow up.  See #197. *)
        fun addUsesToArray useArray addr uses =
        let
            fun match(UseGeneral, UseGeneral) = true
            |   match(UseExport, UseExport) = true
            |   match(UseField(i1, l1), UseField(i2, l2)) = i1 = i2 andalso matchList (l1, l2)
            |   match _ = false
            and matchList([], []) = true
            |   matchList(h1::t1, h2::t2) = match(h1, h2) andalso matchList(t1, t2)
            |   matchList _ = false

            fun addUse newUse =
            let
                val currentUsage = Array.sub(useArray, addr)
            in
                if List.exists(fn u => match(u, newUse)) currentUsage
                then ()
                else Array.update(useArray, addr, newUse :: currentUsage)
            end
        in
            List.app addUse uses
        end

        (* This function annotates the tree with information about how variables are used.  This assists
           the optimiser to choose the best alternative for code.  It also discards bindings that
           are unused and side-effect-free.  These can arise as the result of optimiser constructing
           bindings in case they are required.  That was originally its only function; hence the name. *)
        fun cleanLambda(context: cleanContext,
                        lambda as {body, isInline, name, argTypes, resultType, localCount, closure, ...}: lambdaForm,
                        lambdaUse) =
        let
            (* Rebuild the closure with the entries actually used. *)
            val closureUse = makeClosure()

            fun lookup (closureEntry, clUse) =
                let
                    (* Find the original closure entry. *)
                    val ext = List.nth(closure, closureEntry)
                    (* Process the closure entry.  We need to do this to record the
                       usage information even if we have already seen this entry. *)
                    val copied = cleanExtract(context, ext, clUse)
                in
                    addToClosure closureUse copied
                end

            (* This array records the way the arguments are used inside the function. *)
            val argUses = Array.array (List.length argTypes, [])
            fun checkArg(addr, uses) = Array.update(argUses, addr, uses @ Array.sub(argUses, addr))

            val recursiveRefRef = ref []
            fun addRef use = recursiveRefRef := use @ !recursiveRefRef

            val resultOfApps =
                List.foldl
                    (fn (UseApply(l, _), r) => l @ r | (UseExport, r) => UseExport :: r | (_, r) => UseGeneral :: r) []

            fun filter(true, true, []) = [UseExport, UseGeneral]
            |   filter(false, true, []) = [UseGeneral]
            |   filter(true, false, []) = [UseExport]
            |   filter(false, false, []) = []
            |   filter(_, isGen, UseExport :: l) = filter(true, isGen, l)
            |   filter(isExp, _, UseGeneral :: l) = filter(isExp, true, l)
            |   filter(isExp, isGen, a (* UseApply, UseField *) :: l) = a :: filter(isExp, isGen, l)

            val bodyUse = filter(false, false, resultOfApps lambdaUse)

            val bodyContext =
                { locals = Array.array(localCount, []), prev = lookup, recursiveRef = addRef,
                  checkArg = checkArg } : cleanContext

            val bodyCode = cleanCode(bodyContext, body, bodyUse)
            val recursiveApps = !recursiveRefRef
            (* If we have called this function somewhere and used the result that gives us a hint on the
               preferred result.  If the function is recursive, though, we can't assume anything
               because the result of the recursive calls may be used in some other context.  For
               example they could be passed into an argument function which may require more fields.
               That in turn affects any functions whose results are used.  See Test138.ML.
               So, we check to see whether the result of recursive use has added anything to the
               original usage and reprocess the body if it has.

               This has been extended to a general recursive case since the original
               single level case had a bug.  See Test191.ML. *)
            val recursiveResults = resultOfApps recursiveApps

            (* CBottom is the "no/empty usage" element *)
            datatype canonical = CBottom | CExp | CGen | CApp of canonical | CFields of (int * canonical) list

            fun tocanon UseExport = CExp
            |   tocanon UseGeneral = CGen
            |   tocanon (UseApply(apps, _)) = CApp(tocanonical apps)
            |   tocanon (UseField(i, uses)) = CFields[(i, tocanonical uses)]

            and mergecanon(CBottom, y) = y
            |   mergecanon(x, CBottom) = x
            |   mergecanon(CExp, _) = CExp
            |   mergecanon(_, CExp) = CExp
            |   mergecanon(CGen, _) = CGen
            |   mergecanon(_, CGen) = CGen
            |   mergecanon(CFields a, CFields b) = CFields(mergefield(a, b))
            |   mergecanon(CApp a, CApp b) = CApp(mergecanon(a, b))
            |   mergecanon _ = CGen

            and mergefield(l1 as ((f1 as (i1, u1)) :: tl1), l2 as (f2 as (i2, u2)) ::tl2) =
                if i1 < i2 then f1 :: mergefield(tl1, l2)
                else if i1 > i2 then f2 :: mergefield(l1, tl2)
                else (i1, mergecanon(u1, u2)) :: mergefield(tl1, tl2)
            |   mergefield([], l) = l
            |   mergefield(l, []) = l

            and tocanonical [] = CBottom
            |   tocanonical (hd::tl) = List.foldl (fn (a, b) => mergecanon(tocanon a, b)) (tocanon hd) tl

        in
            if not (null recursiveResults) (* short cut *)
                andalso tocanonical bodyUse <> tocanonical(recursiveResults @ bodyUse)
            then cleanLambda(context, lambda, lambdaUse @ recursiveApps)
            else
                let
                    val newClosure = extractClosure closureUse

                    val newArgTypes = ListPair.zip(map #1 argTypes, Array.foldr (op ::) [] argUses)
                in
                    {body=bodyCode, isInline=isInline, name=name,
                       closure=newClosure, argTypes=newArgTypes, resultType=resultType,
                       localCount=localCount, recUse = recursiveApps} : lambdaForm
                end
        end

        (* Process a load from a variable.  Locals and arguments operate on the relevant array,
           closure entries involve a look-up *)
        and cleanExtract(context: cleanContext, ext as LoadLocal addr, codeUse) =
            (
                (* Check we're actually adding to the usage. *)
                null codeUse andalso raise InternalError "cleanExtract: empty usage";
                addUsesToArray (#locals context) addr codeUse;
                ext
            )

        |   cleanExtract(context, ext as LoadArgument addr, codeUse) =
            (
                #checkArg context (addr, codeUse);
                ext
            )

        |   cleanExtract(context, LoadClosure addr, codeUse) = #prev context (addr, codeUse)

        |   cleanExtract(context, LoadRecursive, codeUse) = (#recursiveRef context codeUse; LoadRecursive)

        (* Process a Newenv.  "codeUse" describes how the result value is used; an empty
           codeUse means the value is not required, in which case the final expression is
           cleaned for its side-effects only and the whole thing collapses to NONE if
           nothing survives.  For a required value (non-empty codeUse) the result is always
           SOME. *)
        and cleanNewenv(context: cleanContext, (decs, exp), codeUse) =
            let
                (* Process the declarations in reverse order.  A binding may be used in a
                   later declaration but apart from mutually-recursive functions no binding
                   can be used in an earlier one. *)
                fun processDecs [] = []

                |   processDecs(Declar{value, addr, ...} :: rest) =
                    let
                        val processedRest = processDecs rest
                    in
                        (* We can drop bindings that are unused if they have no side-effects.
                           If we retain the binding we must set at least one reference. *)
                        case Array.sub(#locals context, addr) of
                            [] => (case cleanDead(context, value) of
                                       NONE => processedRest
                                   |   SOME cleanedvalue => NullBinding cleanedvalue :: processedRest)
                        |   uses => Declar{value=cleanCode(context, value, uses), addr=addr, use=uses} :: processedRest
                    end

                |   processDecs(RecDecs decs :: rest) =
                    let
                        val processedRest = processDecs rest
                        (* We now know the entries that have actually been used
                           in the rest of the code.  We need to include those
                           declarations and any that they use.
                           The result we pass down may well exclude some or all
                           recursive uses.  We need to include UseGeneral in
                           the result for safety. *)
                        fun processMutuals([], excluded, true) =
                                (* If we have included a function in this
                                   pass we have to reprocess the list of
                                   those we excluded before. *)
                                processMutuals(excluded, [], false)
                         |  processMutuals([], _, false) =
                                (* We didn't add anything more - finish *) []
                         |  processMutuals(
                                (this as {addr, lambda, ...}) :: rest, excluded, added) =
                            (
                                case Array.sub(#locals context, addr) of
                                    [] => (* Put this on the excluded list. *)
                                        processMutuals(rest, this::excluded, added)
                                |   useSoFar =>
                                        (* Process this then the rest of the list. *)
                                        (addr, cleanLambda(context, lambda, UseGeneral :: useSoFar)) ::
                                            processMutuals(rest, excluded, true)
                            )
                        val entriesUsed = processMutuals(decs, [], false)
                        (* Get all the uses now we're finished and have identified
                           all the recursive uses. *)
                        val processedDecs =
                            map (fn(a, l) => {addr=a, lambda=l, use=Array.sub(#locals context, a)}) entriesUsed
                    in
                        if null processedDecs
                        then processedRest
                        else RecDecs processedDecs :: processedRest
                    end

                |   processDecs(NullBinding exp :: rest) =
                    let
                        val processedRest = processDecs rest
                    in
                        case cleanDead(context, exp) of
                            NONE => processedRest
                        |   SOME cleanedexp => NullBinding cleanedexp :: processedRest
                    end

                |   processDecs(Container{setter, size, addr, ...} :: rest) =
                    let
                        val processedRest = processDecs rest
                        val decUses =
                            case Array.sub(#locals context, addr) of
                                [] => if sideEffectFree setter then [] else [UseGeneral]
                            |   uses => uses
                    in
                        (* We can drop bindings that are unused if they have no side-effects.
                           If we retain the binding we must set at least one reference. *)
                        (* Currently SetContainer is treated as having a side-effect so
                           we will never discard this even if the container is unused. *)
                        if null decUses
                        then processedRest (* Skip it *)
                        else Container{setter=cleanCode(context, setter, [UseGeneral]), addr=addr, size=size, use=decUses} :: processedRest
                    end

                (* Process the expression first so any references it makes are marked,
                   then the bindings (in reverse order). *)
                val processedExp =
                    if null codeUse then cleanDead(context, exp) else SOME(cleanCode(context, exp, codeUse))
            in
                case (processDecs decs, processedExp) of
                    ([], NONE) => NONE
                |   (decs, expOpt) => SOME(mkEnv(decs, Option.getOpt(expOpt, CodeZero)))
            end

        (* Clean a list of sub-expressions whose values are not required, keeping their
           side-effects in evaluation order.  NONE if they are all side-effect-free. *)
        and cleanDeadList(context: cleanContext, codes) =
            let
                fun keep [] = []
                |   keep(c :: rest) =
                        (case cleanDead(context, c) of NONE => keep rest | SOME r => NullBinding r :: keep rest)
            in
                case keep codes of
                    [] => NONE
                |   binds => SOME(mkEnv(binds, CodeZero))
            end

        (* Process a BeginLoop.  "codeUse" is how the loop's result value is used, which is
           the usage of every exit branch (a tail-position expression that is not a Loop).
           When the result is unused (codeUse = []) those branches are cleaned dead, while
           the Loop jumps that continue iteration are kept. *)
        and cleanBeginLoop(context: cleanContext, (loop, arguments), codeUse) =
            let
                val cleanBody = cleanCode(context, loop, codeUse)
                (* Remove unused arguments.  They're unnecessary and may cause problems
                   later on. *)
                fun filterUnused [] = ([], [])
                |   filterUnused(({value, addr, ...}, t) :: args) =
                    let
                        val (used, discards) = filterUnused args
                    in
                        case Array.sub(#locals context, addr) of
                            [] => (* We only need to keep this if it might have a side-effect. *)
                                (used, NullBinding(cleanCode(context, value, [UseGeneral])) :: discards)
                        |   use => (({value=cleanCode(context, value, use), addr=addr, use=use}, t) :: used, discards)
                    end
                val (usedArgs, discards) = filterUnused arguments
            in
                if not(null discards)
                then
                let
                    fun splitArgs([], []) = ([], [])
                    |   splitArgs((arg as (argVal, _)) :: args, ({addr, ...}, _) :: arguments) =
                        let
                            val (useArgs, discards) = splitArgs(args, arguments)
                        in
                            (* We actually only need to keep this argument if it might have
                               a side-effect but keep it anyway. *)
                            case Array.sub(#locals context, addr) of
                                [] => (useArgs, NullBinding argVal :: discards)
                            |   _ => (arg :: useArgs, discards)
                        end
                    |   splitArgs _ = raise InternalError "splitArgs"

                    fun filterLoopArgs(Loop l) =
                        let
                            val (useArgs, discards) = splitArgs(l, arguments)
                        in
                            SOME(Newenv(discards, Loop useArgs))
                        end
                        (* Don't descend into functions or inner loops. *)
                    |   filterLoopArgs(instr as Lambda _) = SOME instr
                    |   filterLoopArgs(instr as BeginLoop _) = SOME instr
                    |   filterLoopArgs _ = NONE

                    val newLoop =
                        BeginLoop {loop = mapCodetree filterLoopArgs cleanBody, arguments = usedArgs}
                in
                    Newenv(discards, newLoop)
                end
                else BeginLoop {loop = cleanBody, arguments = usedArgs}
            end

        (* Clean an Eval.  Pass the usage down to the function so that if it is a
           variable it is marked as "called"; resultUse is how the call's result is
           used ([] when it is discarded). *)
        and cleanEval(context, {function, argList, resultType}, resultUse) =
            let
                val args = map (fn (c, t) => (cleanCode(context, c, [UseGeneral]), t)) argList
                val argTuples = map #1 args
            in
                Eval{function = cleanCode(context, function, [UseApply(resultUse, argTuples)]),
                     argList = args, resultType = resultType}
            end

        (* Process code whose result value is not required, retaining only what is needed
           for its side-effects.  Returns NONE if the code is side-effect-free (nothing to
           keep) or SOME of the residual code. *)
        and cleanDead(context: cleanContext, Newenv nenv) = cleanNewenv(context, nenv, [])

        |   cleanDead(context, Cond(test, thenpt, elsept)) =
                (
                    case (cleanDead(context, thenpt), cleanDead(context, elsept)) of
                        (* Both arms side-effect-free: the branch selection is unobservable
                           when the result is discarded, so only the test's effects remain. *)
                        (NONE, NONE) => cleanDead(context, test)
                    |   (t, e) =>
                            SOME(Cond(cleanCode(context, test, [UseGeneral]),
                                      Option.getOpt(t, CodeZero), Option.getOpt(e, CodeZero)))
                )

            (* Building a tuple has no observable effect, so drop it and keep only the
               fields' side-effects, in order. *)
        |   cleanDead(context, Tuple{fields, ...}) = cleanDeadList(context, fields)

        |   cleanDead(context, Indirect{base, ...}) = cleanDead(context, base) (* Indexing is side-effect-free. *)

            (* A discarded handler must be kept even when the handler is pure, because
               catching and discarding an exception suppresses it.  But if the body cannot
               raise or have an effect the handler is unreachable and the whole thing dies. *)
        |   cleanDead(context, Handle{exp, handler, exPacketAddr}) =
                (
                    case cleanDead(context, exp) of
                        NONE => NONE
                    |   SOME e =>
                            SOME(Handle{exp=e, handler=Option.getOpt(cleanDead(context, handler), CodeZero),
                                        exPacketAddr=exPacketAddr})
                )

            (* Operation nodes.  Decide whether the operator itself has an effect by calling
               sideEffectFree about a copy with constant operands (O(1), and it tracks
               codeProps).  If the operator is pure, drop it and keep the operand effects;
               otherwise keep the whole node. *)
        |   cleanDead(_, code as Nullary _) = if sideEffectFree code then NONE else SOME code

        |   cleanDead(context, code as Unary{oper, arg1}) =
                if sideEffectFree(Unary{oper=oper, arg1=CodeZero})
                then cleanDead(context, arg1)
                else SOME(cleanCode(context, code, [UseGeneral]))

        |   cleanDead(context, code as Binary{oper, arg1, arg2}) =
                if sideEffectFree(Binary{oper=oper, arg1=CodeZero, arg2=CodeZero})
                then cleanDeadList(context, [arg1, arg2])
                else SOME(cleanCode(context, code, [UseGeneral]))

            (* A tag test, a load (immutable or mutable) and an allocation all have no
               effect of their own, so keep only their operand effects. *)
        |   cleanDead(context, TagTest{test, ...}) = cleanDead(context, test)

        |   cleanDead(context, LoadOperation{address={base, index, ...}, ...}) =
                cleanDeadList(context, base :: (case index of SOME i => [i] | NONE => []))

        |   cleanDead(context, AllocateWordMemory{numWords, flags, initial}) =
                cleanDeadList(context, [numWords, flags, initial])

            (* A block move is a store (keep); byte equality/compare is applicative. *)
        |   cleanDead(context, code as BlockOperation{kind, sourceLeft, destRight, length}) =
                (
                    case kind of
                        BlockOpMove _ => SOME(cleanCode(context, code, [UseGeneral]))
                    |   _ =>
                        let
                            fun addrParts{base, index, ...} =
                                base :: (case index of SOME i => [i] | NONE => [])
                        in
                            cleanDeadList(context, addrParts sourceLeft @ addrParts destRight @ [length])
                        end
                )

            (* Arbitrary precision: the compiler treats its operands as just [arg1, arg2]
               (see evaluateInlining); shortCond and longCall are pure functions of them. *)
        |   cleanDead(context, Arbitrary{arg1, arg2, ...}) = cleanDeadList(context, [arg1, arg2])

            (* Keep the loop, but clean its body dead: every exit branch (a tail-position
               expression that is not a Loop) has its now-unused result discarded, and any
               loop argument that only fed such a branch is dropped. *)
        |   cleanDead(context, BeginLoop{loop, arguments}) = SOME(cleanBeginLoop(context, (loop, arguments), []))

            (* Keep the call (it has an effect), but tell the function its result is
               unused: mark it UseApply with an empty result usage.  A function whose
               call sites discard the result then has bodyUse = [] in cleanLambda and
               so has its body dead-cleaned. *)
        |   cleanDead(context, Eval eval) = SOME(cleanEval(context, eval, []))

        |   cleanDead(_, Extract _) = NONE
        |   cleanDead(_, Constnt _) = NONE
        |   cleanDead(_, Lambda _) = NONE

            (* Everything else — Raise, Loop, StoreOperation, SetContainer — always carries a
               side-effect, so keep it with the result discarded. *)
        |   cleanDead(context, code) = SOME(cleanCode(context, code, [UseGeneral]))

        and cleanCode (context: cleanContext, code, codeUse) =
        let
            fun doClean codeUse (Newenv nenv) = SOME(Option.getOpt(cleanNewenv(context, nenv, codeUse), CodeZero))

                (* Reference to a binding. *)
            |   doClean codeUse (Extract ext) = SOME(Extract(cleanExtract(context, ext, codeUse)))

                (* Select a field from a tuple or container.  We can't do this for selection from datatypes because
                   some fields may not be present on all paths. *)
            |   doClean codeUse (Indirect{base, offset, indKind}) =
                let
                    (* Try to pass down the use.  If the "base" is an Extract or another Indirect
                       we may be able to record this information.  If it is something else we can't. *)
                in
                    case indKind of
                         IndVariant => NONE
                    |    _ => SOME(Indirect{base=cleanCode(context, base, [UseField(offset, codeUse)]), offset=offset, indKind = indKind})
                end

            |   doClean codeUse (Tuple{ fields, isVariant = false}) =
                let
                    (* If the use of the tuple include UseGeneral or UseExport then every field is
                       required.  If, though, we have UseField we can transfer the corresponding
                       usage onto the field of the tuple. *)
                    fun fieldUse n (UseField(offset, uses), tl) =
                            if n = offset then uses @ tl else tl
                    |   fieldUse _ (use, tl) = use :: tl

                    fun fieldUses n =
                        List.foldl(fieldUse n) [] codeUse

                    fun processField([], _) = []
                    |   processField(hd::tl, n) =
                            cleanCode(context, hd, fieldUses n) :: processField(tl, n+1)
                in
                    SOME(Tuple{ fields = processField(fields, 0), isVariant = false})
                end

            |   doClean codeUse (Lambda lam) = SOME(Lambda(cleanLambda(context, lam, codeUse)))

            |   doClean codeUse (Eval eval) = SOME(cleanEval(context, eval, codeUse))

            |   doClean codeUse (Cond(i, t, e)) =
                    SOME(Cond(cleanCode(context, i, [UseGeneral]), cleanCode(context, t, codeUse), cleanCode(context, e, codeUse)))

            |   doClean codeUse (Handle{exp, handler, exPacketAddr}) =
                    SOME(Handle{exp = cleanCode(context, exp, codeUse),
                                handler = cleanCode(context, handler, codeUse),
                                exPacketAddr = exPacketAddr})

            |   doClean use (BeginLoop{loop, arguments}) = SOME(cleanBeginLoop(context, (loop, arguments), use))

            |   doClean _ _ = NONE (* Anything else *)

        in
            (* If we recognise this as a special case we use the result otherwise
               we process it as a general value using UseGeneral as the usage. *)
            if null codeUse
            then Option.getOpt(cleanDead(context, code), CodeZero)
            else
            (case doClean codeUse code of
                SOME result => result
            |   NONE => mapCodetree (doClean [UseGeneral]) code)
        end
    in
        cleanCode({ locals = Array.array(localCount, []), prev = fn (i, _) => prev i,
                    recursiveRef = fn _ => (), checkArg = fn _ => () },
                  code, [UseExport])
    end

    structure Sharing =
    struct
        type codetree = codetree
        and loadForm = loadForm
        and codeUse = codeUse
    end
end;
