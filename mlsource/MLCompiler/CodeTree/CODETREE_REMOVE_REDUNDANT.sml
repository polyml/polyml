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

functor CODETREE_REMOVE_REDUNDANT(
    structure BASECODETREE: BaseCodeTreeSig
    structure CODETREE_FUNCTIONS: CodetreeFunctionsSig

    sharing BASECODETREE.Sharing = CODETREE_FUNCTIONS.Sharing
) :
    sig
        type codetree
        type loadForm
        type codeUse
        val cleanProc : (codetree * codeUse list * (int -> loadForm) * int) -> codetree
        structure Sharing: sig type codetree = codetree and loadForm = loadForm and codeUse = codeUse end
    end
=
struct
    open BASECODETREE
    open CODETREE_FUNCTIONS
    exception InternalError = Misc.InternalError

    (* This function annotates the tree with information about how variables are used.  This assists
       the optimiser to choose the best alternative for code.  It also discards bindings that
       are unused and side-effect-free.  These can arise as the result of optimiser constructing
       bindings in case they are required.  That was originally its only function; hence the name. *)
    fun cleanProc (pt, procUses: codeUse list, prev: int * codeUse list -> loadForm, localCount, checkArg) =
    let
        val locals = Array.array(localCount, [])

        fun cleanLambda(lambda as { isInline = OnlyInline, ...}, _) = lambda
            (* Don't process functors - they will be processed when they are applied. *)

        |   cleanLambda({body, isInline, name, argTypes, resultType, localCount, closure, ...}: lambdaForm, lambdaUse) =
        let
            (* If we have called this function somewhere and used the result that gives us a hint on the
               preferred result. *)
            val bodyUse =
                List.foldl
                    (fn (UseApply l, r) => l @ r | (UseExport, r) => UseExport :: r | (_, r) => UseGeneral :: r)
                    [] lambdaUse
            (* Rebuild the closure with the entries actually used. *)
            val closureUse: {closureAddr: int, source: loadForm, result: loadForm} list ref = ref []

            fun lookup (closureEntry, clUse) =
                let
                    (* Find the original closure entry. *)
                    val ext = List.nth(closure, closureEntry)
                    (* Process the closure entry.  We need to do this to record the
                       usage information even if we have already seen this entry. *)
                    val copied = cleanExtract(ext, clUse)

                    val newClosure =
                        case List.find (fn { source, ...} => source = ext) (!closureUse) of
                            SOME{ closureAddr, ...} => closureAddr
                        |   NONE =>
                            let
                                (* Find the next address in the sequence. = List.length + 1 *)
                                val nextAddr = case !closureUse of [] => 0 | { closureAddr, ...} :: _ => closureAddr+1
                            in
                                closureUse := { closureAddr = nextAddr, source = ext, result = copied } :: ! closureUse;
                                nextAddr
                            end
                in
                    LoadClosure newClosure
                end

            (* This array records the way the arguments are used inside the function. *)
            val argUses = Array.array (List.length argTypes, [])
            fun checkArg(addr, uses) = Array.update(argUses, addr, uses @ Array.sub(argUses, addr))

            val bodyCode = cleanProc(body, bodyUse, lookup, localCount, checkArg)

            val newClosure = List.foldl (fn ({ result, ...}, tl) => result :: tl) [] (!closureUse)

            val newArgTypes = ListPair.zip(map #1 argTypes, Array.foldr (op ::) [] argUses)
        in
            {body=bodyCode, isInline=isInline, name=name,
               closure=newClosure, argTypes=newArgTypes, resultType=resultType,
               localCount=localCount} : lambdaForm
        end

        (* Process a load from a variable.  Locals and arguments operate on the relevant array,
           closure entries involve a look-up *)
        and cleanExtract(ext as LoadLocal addr, codeUse) =
            (
                (* Check we're actually adding to the usage. *)
                null codeUse andalso raise InternalError "cleanExtract: empty usage";
                Array.update(locals, addr, codeUse @ Array.sub(locals, addr));
                ext
            )

        |   cleanExtract(ext as LoadArgument addr, codeUse) =
            (
                checkArg(addr, codeUse);
                ext
            )

        |   cleanExtract(LoadClosure addr, codeUse) = prev(addr, codeUse)
        
        |   cleanExtract(LoadRecursive, _) = LoadRecursive

        and cleanCode (code, codeUse) =
        let
            fun doClean codeUse (Newenv(decs, exp)) =
                let
                    (* First process the expression so as to mark any references it makes. *)
                    val processedExp = cleanCode (exp, codeUse)
                
                    (* Process the declarations in reverse order.  A binding may be used in
                       a later declaration but apart from mutually-recursive functions no binding
                       can be used in an earlier one. *)
                    fun processDecs [] = []

                    |   processDecs(Declar{value, addr, ...} :: rest) =
                        let
                            val processedRest = processDecs rest
                            val decUses =
                                case Array.sub(locals, addr) of
                                    [] => if sideEffectFree value then [] else [UseGeneral]
                                |   uses => uses
                        in
                            (* We can drop bindings that are unused if they have no side-effects.
                               If we retain the binding we must set at least one reference. *)
                            if null decUses
                            then processedRest (* Skip it *)
                            else Declar{value=cleanCode (value, decUses), addr=addr, use=decUses} :: processedRest
                        end

                    |   processDecs(RecDecs decs :: rest) =
                        let
                            val processedRest = processDecs rest
                            (* We now know the entries that have actually been used
                               in the rest of the code.  We need to include those
                               declarations and any that they use.
                               TODO:  We process a binding as soon as we know it is
                               used in order to detect other bindings.  However there
                               may be other recursive references.  That means that the
                               "useSoFar" passed as the way the function result is used
                               may be incomplete.  *)
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
                                    case Array.sub(locals, addr) of
                                        [] => (* Put this on the excluded list. *)
                                            processMutuals(rest, this::excluded, added)
                                    |   useSoFar =>
                                            (* Process this then the rest of the list. *)
                                            (addr, cleanLambda(lambda, useSoFar)) ::
                                                processMutuals(rest, excluded, true)
                                )
                            val entriesUsed = processMutuals(decs, [], false)
                            (* Get all the uses now we're finished and have identified
                               all the recursive uses. *)
                            val processedDecs =
                                map (fn(a, l) => {addr=a, lambda=l, use=Array.sub(locals, a)}) entriesUsed
                        in
                            if null processedDecs
                            then processedRest
                            else RecDecs processedDecs :: processedRest
                        end

                    |   processDecs(NullBinding exp :: rest) =
                        let
                            val processedRest = processDecs rest
                        in
                            if sideEffectFree exp
                            then processedRest
                            else NullBinding(cleanCode(exp, [UseGeneral])) :: processedRest
                        end

                    val processedDecs = processDecs decs
                in
                    SOME(mkEnv(processedDecs, processedExp))
                end (* Newenv *)

                (* Reference to a binding. *)
            |   doClean codeUse (Extract ext) = SOME(Extract(cleanExtract(ext, codeUse)))

                (* Select a field from a tuple.  We can't do this for selection from datatypes because
                   some fields may not be present on all paths. *)
            |   doClean codeUse (Indirect{base, offset, isVariant = false}) =
                    (* Try to pass down the use.  If the "base" is an Extract or another Indirect
                       we may be able to record this information.  If it is something else we can't. *)
                    SOME(Indirect{base=cleanCode(base, [UseField(offset, codeUse)]), offset=offset, isVariant=false})

            |   doClean codeUse (Recconstr fields) =
                let
                    (* If the use of the tuple include UseGeneral or UseExport then every field is
                       required.  If, though, we have UseField we can transfer the corresponding
                       usage onto the field of the tuple. *)
                    fun fieldUse n (UseField(offset, uses), tl) =
                            if n = offset then uses @ tl else tl
                    |   fieldUse _ (use, tl) = use :: tl

                    fun fieldUses n =
                        (* For the moment, if we find that the field is not used we set the
                           usage to UseGeneral.  I'm not convinced it would be safe to
                           discard anything in the expression at this point. *)
                        case List.foldl(fieldUse n) [] codeUse of
                            [] => [UseGeneral]
                        |   other => other
                            
                    fun processField([], _) = []
                    |   processField(hd::tl, n) =
                            cleanCode(hd, fieldUses n) :: processField(tl, n+1)
                in
                    SOME(Recconstr(processField(fields, 0)))
                end

            |   doClean codeUse (Lambda lam) = SOME(Lambda(cleanLambda(lam, codeUse)))

            |   doClean codeUse (Eval{function, argList, resultType}) =
                (* As with Indirect we try to pass this information down so that if
                   the function is a variable it will be marked as "called". *)
                SOME(
                    Eval{
                        function=cleanCode(function, [UseApply codeUse]),
                        argList=map (fn (c, t) => (cleanCode(c, [UseGeneral]), t)) argList,
                        resultType = resultType
                    })

            |   doClean codeUse (Cond(i, t, e)) =
                    SOME(Cond(cleanCode(i, [UseGeneral]), cleanCode(t, codeUse), cleanCode(e, codeUse)))
        
            |   doClean _ _ = NONE (* Anything else *)
        in
            (* If we recognise this as a special case we use the result otherwise
               we process it as a general value using UseGeneral as the usage. *)
            case doClean codeUse code of
                SOME result => result
            |   NONE => mapCodetree (doClean [UseGeneral]) code
        end

    in
        cleanCode (pt, procUses)
    end (* cleanProc *)

    val cleanProc =
        fn (code, procUse, prev, localCount) =>
            cleanProc(code, procUse, fn (i, _) => prev i, localCount, fn _ => ())

    structure Sharing =
    struct
        type codetree = codetree
        and loadForm = loadForm
        and codeUse = codeUse
    end
end;
