(*
    Title:      Source level debugger for Poly/ML
    Author:     David Matthews
    Copyright  (c)   David Matthews 2000, 2014, 2015

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

functor DEBUGGER_ (

    structure STRUCTVALS : STRUCTVALSIG
    structure VALUEOPS : VALUEOPSSIG
    structure CODETREE : CODETREESIG
    structure TYPETREE: TYPETREESIG
    structure ADDRESS : AddressSig
    structure COPIER: COPIERSIG
    structure TYPEIDCODE: TYPEIDCODESIG
    structure LEX : LEXSIG

    structure DEBUG :
    sig
        val debugTag: bool Universal.tag
        val getParameter :
           'a Universal.tag -> Universal.universal list -> 'a
    end

    structure UTILITIES :
    sig
        val splitString: string -> { first:string,second:string }
    end

sharing STRUCTVALS.Sharing = VALUEOPS.Sharing = TYPETREE.Sharing = COPIER.Sharing =
        TYPEIDCODE.Sharing = CODETREE.Sharing = ADDRESS = LEX.Sharing
)
: DEBUGGERSIG
=
struct
    open STRUCTVALS VALUEOPS CODETREE COPIER TYPETREE DEBUG

    (* The static environment contains these kinds of entries. *)
    datatype environEntry =
        EnvValue of string * types * locationProp list
    |   EnvException of string * types * locationProp list
    |   EnvVConstr of string * types * bool * int * locationProp list
    |   EnvTypeid of { original: typeId, freeId: typeId }
    |   EnvStructure of string * signatures * locationProp list
    |   EnvTConstr of string * typeConstrSet
    |   EnvStartFunction of string * location * types
    |   EnvEndFunction of string * location * types

    (* When stopped at a break-point any Bound ids must be replaced by Free ids.
       We make new Free ids at this point.  *)
    fun envTypeId (id as TypeId{ description, idKind = Bound _, ...}) =
            EnvTypeid { original = id, freeId = makeFreeId(Global CodeZero, isEquality id, description) }
    |   envTypeId id = EnvTypeid { original = id, freeId = id }

    fun searchEnvs match (staticEntry :: statics, dlist as dynamicEntry :: dynamics) =
    (
        case (match (staticEntry, dynamicEntry), staticEntry) of
            (SOME result, _) => SOME result
        |   (NONE, EnvTypeid _) => searchEnvs match (statics, dynamics)
        |   (NONE, EnvVConstr _) => searchEnvs match (statics, dynamics)
        |   (NONE, EnvValue _) => searchEnvs match (statics, dynamics)
        |   (NONE, EnvException _) => searchEnvs match (statics, dynamics)
        |   (NONE, EnvStructure _) => searchEnvs match (statics, dynamics)
        |   (NONE, EnvStartFunction _) => searchEnvs match (statics, dynamics)
        |   (NONE, EnvEndFunction _) => searchEnvs match (statics, dynamics)
                (* EnvTConstr doesn't have an entry in the dynamic list *)
        |   (NONE, EnvTConstr _) => searchEnvs match (statics, dlist)
            
    )
    |   searchEnvs _ ([], []) = NONE
    
    |   searchEnvs _ _ = raise Misc.InternalError "searchEnvs: Static and dynamic lists have different lengths"

    fun searchType envs typeid =
    let
        fun match (EnvTypeid{original, freeId }, valu) =
            if sameTypeId(original, typeid)
            then 
                case freeId of
                    TypeId{description, idKind as Free _, typeFn, ...} =>
                        (* This can occur for datatypes inside functions. *)
                        SOME(TypeId { access= Global(mkConst valu), idKind=idKind, description=description, typeFn = typeFn })
                |   _ => raise Misc.InternalError "searchType: TypeFunction"
            else NONE
        |   match _ = NONE
    in
        case (searchEnvs match envs, typeid) of
            (SOME t, _) => t
        |   (NONE, typeid as TypeId{description, typeFn=(_, EmptyType), ...}) =>
                (* The type ID is missing.  Make a new temporary ID. *)
                makeFreeId(Global(TYPEIDCODE.codeForUniqueId()), isEquality typeid, description)

        |   (NONE, TypeId{description, typeFn, ...}) => makeTypeFunction(description, typeFn)
    end

    fun runTimeType debugEnviron ty =
    let
        fun copyId(TypeId{idKind=Free _, access=Global _ , ...}) = NONE (* Use original *)
        |   copyId id = SOME(searchType debugEnviron id)
    in
            copyType (ty, fn x => x,
                fn tcon => copyTypeConstr (tcon, copyId, fn x => x, fn s => s))
    end

    (* Exported functions that appear in PolyML.DebuggerInterface. *)
    type debugState = environEntry list * machineWord list

    fun debugNameSpace (debugEnviron: debugState) : nameSpace =
    let
        (* Values must be copied so that compile-time type IDs are replaced by their run-time values. *)
        fun copyTheTypeConstructor (TypeConstrSet(tcons, (*tcConstructors*) _)) =
        let
            val typeID = searchType debugEnviron (tcIdentifier tcons)
            val newTypeCons =
                makeTypeConstructor(tcName tcons, tcTypeVars tcons, typeID, tcLocations tcons)

            val newValConstrs = (*map copyAConstructor tcConstructors*) []
        in
            TypeConstrSet(newTypeCons, newValConstrs)
        end

        (* When creating a structure we have to add a type map that will look up the bound Ids. *)
        fun replaceSignature (Signatures{ name, tab, typeIdMap, firstBoundIndex, declaredAt, ... }) =
        let
            fun getFreeId n = searchType debugEnviron (makeBoundId(Global CodeZero, n, false, false, basisDescription ""))
        in
            makeSignature(name, tab, firstBoundIndex, declaredAt, composeMaps(typeIdMap, getFreeId), [])
        end

        val runTimeType = runTimeType debugEnviron

        (* Lookup and "all" functions for the environment.  We can't easily use a general
           function for the lookup because we have dynamic entries for values and structures
           but not for type constructors. *)
        fun lookupValues (EnvValue(name, ty, location) :: ntl, valu :: vl) s =
                if name = s
                then SOME(mkGvar(name, runTimeType ty, mkConst valu, location))
                else lookupValues(ntl, vl) s

        |  lookupValues (EnvException(name, ty, location) :: ntl, valu :: vl) s =
                if name = s
                then SOME(mkGex(name, runTimeType ty, mkConst valu, location))
                else lookupValues(ntl, vl) s

        |  lookupValues (EnvVConstr(name, ty, nullary, count, location) :: ntl, valu :: vl) s =
                if name = s
                then SOME(makeValueConstr(name, runTimeType ty, nullary, count, Global(mkConst valu), location))
                else lookupValues(ntl, vl) s

        |  lookupValues (EnvTConstr _ :: ntl, vl) s = lookupValues(ntl, vl) s
        
        |  lookupValues (_ :: ntl, _ :: vl) s = lookupValues(ntl, vl) s

        |  lookupValues _ _ =
             (* The name we are looking for isn't in
                the environment.
                The lists should be the same length. *)
             NONE

        fun allValues (EnvValue(name, ty, location) :: ntl, valu :: vl) =
                (name, mkGvar(name, runTimeType ty, mkConst valu, location)) :: allValues(ntl, vl)

         |  allValues (EnvException(name, ty, location) :: ntl, valu :: vl) =
                (name, mkGex(name, runTimeType ty, mkConst valu, location)) :: allValues(ntl, vl)

         |  allValues (EnvVConstr(name, ty, nullary, count, location) :: ntl, valu :: vl) =
                (name, makeValueConstr(name, runTimeType ty, nullary, count, Global(mkConst valu), location)) ::
                    allValues(ntl, vl)

         |  allValues (EnvTConstr _ :: ntl, vl) = allValues(ntl, vl)
         |  allValues (_ :: ntl, _ :: vl) = allValues(ntl, vl)
         |  allValues _ = []

        fun lookupTypes (EnvTConstr (name, tcSet) :: ntl, vl) s =
                if name = s
                then SOME (copyTheTypeConstructor tcSet)
                else lookupTypes(ntl, vl) s

        |   lookupTypes (_ :: ntl, _ :: vl) s = lookupTypes(ntl, vl) s
        |   lookupTypes _ _ = NONE

        fun allTypes (EnvTConstr(name, tcSet) :: ntl, vl) =
                (name, copyTheTypeConstructor tcSet) :: allTypes(ntl, vl)
         |  allTypes (_ :: ntl, _ :: vl) = allTypes(ntl, vl)
         |  allTypes _ = []

        fun lookupStructs (EnvStructure (name, rSig, locations) :: ntl, valu :: vl) s =
                if name = s
                then SOME(makeGlobalStruct (name, replaceSignature rSig, mkConst valu, locations))
                else lookupStructs(ntl, vl) s

        |   lookupStructs (EnvTConstr _ :: ntl, vl) s = lookupStructs(ntl, vl) s
        |   lookupStructs (_ :: ntl, _ :: vl) s = lookupStructs(ntl, vl) s
        |   lookupStructs _ _ = NONE

        fun allStructs (EnvStructure (name, rSig, locations) :: ntl, valu :: vl) =
                (name, makeGlobalStruct(name, replaceSignature rSig, mkConst valu, locations)) :: allStructs(ntl, vl)

         |  allStructs (EnvTypeid _ :: ntl, _ :: vl) = allStructs(ntl, vl)
         |  allStructs (_ :: ntl, vl) = allStructs(ntl, vl)
         |  allStructs _ = []

        (* We have a full environment here for future expansion but at
           the moment only some of the entries are used. *)
        fun noLook _ = NONE
        and noEnter _ = raise Fail "Cannot update this name space"
        and allEmpty _ = []
   in
       {
            lookupVal = lookupValues debugEnviron,
            lookupType = lookupTypes debugEnviron,
            lookupFix = noLook,
            lookupStruct = lookupStructs debugEnviron,
            lookupSig = noLook, lookupFunct = noLook, enterVal = noEnter,
            enterType = noEnter, enterFix = noEnter, enterStruct = noEnter,
            enterSig = noEnter, enterFunct = noEnter,
            allVal = fn () => allValues debugEnviron,
            allType = fn () => allTypes debugEnviron,
            allFix = allEmpty,
            allStruct = fn () => allStructs debugEnviron,
            allSig = allEmpty,
            allFunct = allEmpty }
    end

    (* debugFunction just looks at the static data. *)
    fun debugFunction (cList, _) =
    (
        case List.find(fn (EnvStartFunction _) => true | _ => false) cList of
            SOME(EnvStartFunction(s, l, _)) => SOME (s, l)
        |   _ => NONE
    )

    and debugFunctionArg d =
    let
        fun match (EnvStartFunction(_, _, ty), valu) =
            SOME(mkGvar("", runTimeType d ty, mkConst valu, []))
        |   match _ = NONE
    in
        searchEnvs match d
    end

    and debugFunctionResult d =
    let
        fun match (EnvEndFunction(_, _, ty), valu) =
            SOME(mkGvar("", runTimeType d ty, mkConst valu, []))
        |   match _ = NONE
    in
        searchEnvs match d
    end


    (* A pointer to this function is inserted in the code for each line. *)
    (* Although the nameTypeList and valueList are the same
       length we build them separately.  This allows the
       nameTypeList to be built at compile time and reduces
       the run-time costs. *)
(*    fun debugFunction (debugger, reason, functionName, location) staticEnv valueList =
    let
        (* The function name supplied is made up to be suitable for output
           when profiling.  We need to clean it up a bit for use here. The
           general form is F().S.v-f(2)g where F is a functor, S a structure,
           v a val declaration, f a curried function and g the function itself.
           If we're within a function we may also have '()' at the end.
           For the moment just strip out the argument numbers. *)
        fun checkChar (#")", (_, l)) = (true,  #")" :: l) (* Start of parens *)
          | checkChar (#"(", (_, l)) = (false, #"(" :: l) (* End of parens *)
          | checkChar (_, (true, l)) = (true, l) (* Remove the character *)
          | checkChar (c, (false, l)) = (false, c :: l)
        val (_, chars) = List.foldr checkChar (false, []) (explode functionName)
        val name1 = String.implode chars
        (* Remove final '()'.  This makes the name within the function consistent
           with the name on entry and exit to the function. *)
        fun removeSuffix s1 s2 =
            if String.isSuffix s1 s2
            then String.substring(s2, 0, String.size s2-String.size s1)
            else s2
        (* Remove the trailing '()' which appears inside the function so that
           we get the same name for the entry and exit code as we do inside
           the function.
           If this function is defined by a val declaration rather than a
           fun declaration remove the trailing '-' *)
        val processedName = removeSuffix "-" (removeSuffix "()" name1)

        val debugEnviron = (staticEnv, valueList)

        val (code, value) =
            case reason of
                DebugEnter (argValue, argType) =>
                    (debugEnterFun, mkGvar("", runTimeType debugEnviron argType, mkConst argValue, [DeclaredAt location]))
            |   DebugLeave (fnResult, resType) =>
                    (debugLeaveFun, mkGvar("", runTimeType debugEnviron resType, mkConst fnResult, [DeclaredAt location]))
            |   DebugException exn =>
                let
                    val exnVal = ADDRESS.toMachineWord exn
                    (* The exception is always a value of type exn. *)
                    val resVal = mkGvar("", TYPETREE.exnType, mkConst exnVal, [DeclaredAt location])
                in
                    (debugExceptFun, resVal)
                end
            |   DebugStep =>
                    (debugLineChange, dummyValue)
    in
        debugger(code, value, #startLine location, #file location, processedName, makeSpace debugEnviron)
    end*)

    (* Functions to make the debug entries.   These are needed both in CODEGEN_PARSETREE for
       the core language and STRUCTURES for the module language. *)
    type debugenv = environEntry list * (level->codetree)

    fun makeValDebugEntries (vars: values list, debugEnv: debugenv, level, lex, mkAddr, typeVarMap) =
    if getParameter debugTag (LEX.debugParams lex)
    then
        let
            fun loadVar (var, (decs, env)) =
                let
                    val loadVal =
                        codeVal (var, level, typeVarMap, [], lex, LEX.nullLocation)
                    (*val {dec, rtEnv, ctEnv} =
                        createValDebugEntry(var, loadVal, level, lex, mkAddr, env)*)
                    val (ctEnv, rtEnv) = env
                    val newEnv =
                    (* Create a new entry in the environment. *)
                          mkTuple [ loadVal (* Value. *), rtEnv level ]
                    val { dec, load } = multipleUses (newEnv, fn () => mkAddr 1, level)
                    val ctEntry =
                        case var of
                            Value{class=Exception, name, typeOf, locations, ...} =>
                                EnvException(name, typeOf, locations)
                        |   Value{class=Constructor{nullary, ofConstrs, ...}, name, typeOf, locations, ...} =>
                                EnvVConstr(name, typeOf, nullary, ofConstrs, locations)
                        |   Value{name, typeOf, locations, ...} =>
                                EnvValue(name, typeOf, locations)
                in
                    (decs @ dec, (ctEntry :: ctEnv, load))
                end
        in
            List.foldl loadVar ([], debugEnv) vars
        end
    else ([], debugEnv)

    fun makeTypeConstrDebugEntries(typeCons, debugEnv, level, lex, mkAddr) =
    if not (getParameter debugTag (LEX.debugParams lex))
    then ([], debugEnv)
    else
    let
        fun foldIds(tc :: tcs, (ctEnv, rtEnv)) =
            let
                val cons = tsConstr tc
                val id = tcIdentifier cons
                val {second = typeName, ...} = UTILITIES.splitString(tcName cons)
            in
                if isTypeFunction (tcIdentifier(tsConstr tc))
                then foldIds(tcs, (EnvTConstr(typeName, tc) :: ctEnv, rtEnv))
                else
                let
                    (* This code will build a cons cell containing the run-time value
                       associated with the type Id as the hd and the rest of the run-time
                       environment as the tl. *)                
                    val loadTypeId = TYPEIDCODE.codeId(id, level)
                    val newEnv = mkTuple [ loadTypeId, rtEnv level ]
                    val { dec, load } = multipleUses (newEnv, fn () => mkAddr 1, level)
                    (* Make an entry for the type constructor itself as well as the new type id.
                       The type Id is used both for the type constructor and also for any values
                       of the type. *)
                    val (decs, newEnv) =
                        foldIds(tcs, (EnvTConstr(typeName, tc) :: envTypeId id :: ctEnv, load))
                in
                    (dec @ decs, newEnv)
                end
            end
        |   foldIds([], debugEnv) = ([], debugEnv)
    in
        foldIds(typeCons, debugEnv)
    end

    fun makeStructDebugEntries (strs: structVals list, debugEnv, level, lex, mkAddr) =
    if getParameter debugTag (LEX.debugParams lex)
    then
        let
            fun loadStruct (str as Struct { name, signat, locations, ...}, (decs, (ctEnv, rtEnv))) =
                let
                    val loadStruct = codeStruct (str, level)
                    val newEnv = mkTuple [ loadStruct (* Structure. *), rtEnv level ]
                    val { dec, load } = multipleUses (newEnv, fn () => mkAddr 1, level)
                    val ctEntry = EnvStructure(name, signat, locations)
                in
                    (decs @ dec, (ctEntry :: ctEnv, load))
                end
        in
            List.foldl loadStruct ([], debugEnv) strs
        end
    else ([], debugEnv)

    (* Create debug entries for typeIDs.  The idea is that if we stop in the debugger we
       can access the type ID, particularly for printing values of the type.
       "envTypeId" creates a free id for each bound id but the print and equality
       functions are extracted when we are stopped in the debugger. *)
    fun makeTypeIdDebugEntries(typeIds, debugEnv, level, lex, mkAddr) =
    if not (getParameter debugTag (LEX.debugParams lex))
    then ([], debugEnv)
    else
    let
        fun foldIds(id :: ids, (ctEnv, rtEnv)) =
            let
                (* This code will build a cons cell containing the run-time value
                   associated with the type Id as the hd and the rest of the run-time
                   environment as the tl. *)                
                val loadTypeId =
                    case id of TypeId { access = Formal addr, ... } =>
                        (* If we are processing functor arguments we will have a Formal here. *)
                        mkInd(addr, mkLoadArgument 0)
                    |   _ => TYPEIDCODE.codeId(id, level)
                val newEnv = mkTuple [ loadTypeId, rtEnv level ]
                val { dec, load } = multipleUses (newEnv, fn () => mkAddr 1, level)
                val (decs, newEnv) =
                    foldIds(ids, (envTypeId id :: ctEnv, load))
            in
                (dec @ decs, newEnv)
            end
        |   foldIds([], debugEnv) = ([], debugEnv)
    in
        foldIds(typeIds, debugEnv)
    end

    (* Set the current state in the thread data. *)
    fun updateDebugState((ctEnv, rtEnv), level, lex, mkAddr) =
        if not (getParameter debugTag (LEX.debugParams lex)) then []
        else
        let
            open ADDRESS RuntimeCalls
            val threadIdCurrentStatic   = mkConst(toMachineWord 0w6)
            and threadIdCurrentDynamic  = mkConst(toMachineWord 0w7)
            val threadId = multipleUses(mkEval(rtsFunction POLY_SYS_thread_self, []), fn () => mkAddr 1, level)
            fun assignItem(offset, value) =
                mkNullDec(mkEval(rtsFunction POLY_SYS_assign_word, [#load threadId level, offset, value]))
        in
            #dec threadId @
                [assignItem(threadIdCurrentStatic, mkConst(toMachineWord ctEnv)),
                 assignItem(threadIdCurrentDynamic, rtEnv level)]
        end

    (* Function entry code.  This needs to precede any values in the body. *)
    fun debugFunctionEntryCode(name: string, argCode, argType, location, debugEnv as (ctEnv, rtEnv), level, lex, mkAddr) =
    if not (getParameter debugTag (LEX.debugParams lex)) then ([], debugEnv)
    else
    let
        val functionName = name (* TODO: munge this to get the root. *)
        val newEnv = mkTuple [ argCode, rtEnv level ]
        val { dec, load } = multipleUses (newEnv, fn () => mkAddr 1, level)
        val ctEntry = EnvStartFunction(functionName, location, argType)
    in
        (dec, (ctEntry :: ctEnv, load))
    end

    (* Add debugging calls on entry and exit to a function. *)
    fun wrapFunctionInDebug(body: codetree, name: string, resType: types, location,
                            entryEnv as (ctEnv, rtEnv), level, lex, mkAddr) =
        if not (getParameter debugTag (LEX.debugParams lex)) then body (* Return it unchanged. *)
        else
        let
            open ADDRESS RuntimeCalls
            val threadIdStack           = mkConst(toMachineWord 0w5)
            and threadIdCurrentStatic   = mkConst(toMachineWord 0w6)
            and threadIdCurrentDynamic  = mkConst(toMachineWord 0w7)
            and threadIdOnEntry         = mkConst(toMachineWord 0w8)
            and threadIdOnExit          = mkConst(toMachineWord 0w9)
            and threadIdOnExitExc       = mkConst(toMachineWord 0w10)
            
            val functionName = name (* TODO: munge this to get the root. *)
            
            fun addStartExitEntry((ctEnv, rtEnv), code, ty, startExit) =
            let
                val newEnv = mkTuple [ code, rtEnv level ]
                val { dec, load } = multipleUses (newEnv, fn () => mkAddr 1, level)
                val ctEntry = startExit(functionName, location, ty)
            in
                (dec, (ctEntry :: ctEnv, load))
            end

            (* All the "on" functions take this as an argument. *)
            val onArgs = [mkConst(toMachineWord(functionName, location))]

            val threadId = multipleUses(mkEval(rtsFunction POLY_SYS_thread_self, []), fn () => mkAddr 1, level)
            fun loadIdEntry offset =
                multipleUses(mkEval(rtsFunction POLY_SYS_load_word, [#load threadId level, offset]), fn () => mkAddr 1, level)
            val currStatic = loadIdEntry threadIdCurrentStatic
            and currDynamic = loadIdEntry threadIdCurrentDynamic
            and currStack = loadIdEntry threadIdStack
            
            val prefixCode = #dec threadId @ #dec currStatic @ #dec currDynamic @ #dec currStack
            
            local
                (* At the start of the function:
                   1.  Push the previous state to the stack.
                   2.  Update the state to the state on entry, including the args
                   3.  Call the global onEntry function if it's set
                   4.  Call the local onEntry function if it's set *)
                val assignStack =
                    mkEval(rtsFunction POLY_SYS_assign_word, [#load threadId level, threadIdStack,
                        mkDatatype[#load currStatic level, #load currDynamic level, #load currStack level]])
                (* TODO: Add a "function" entry with the argument info onto the debug state
                   before we make this assignment. *)
                val assignStatic =
                    mkEval(rtsFunction POLY_SYS_assign_word, [#load threadId level, threadIdCurrentStatic,
                        mkConst(toMachineWord ctEnv)])
                val assignDynamic =
                    mkEval(rtsFunction POLY_SYS_assign_word, [#load threadId level, threadIdCurrentDynamic,
                        rtEnv level])
                val onEntryFn = loadIdEntry threadIdOnEntry
                val optCallOnEntry =
                    mkIf(mkTagTest(#load onEntryFn level, 0w0, 0w0), CodeZero, mkEval(#load onEntryFn level, onArgs))
                (* TODO: optionally call the function-specific breakpoint. *)
            in
                val entryCode =
                    [mkNullDec assignStack, mkNullDec assignStatic, mkNullDec assignDynamic] @
                    #dec onEntryFn @ [mkNullDec optCallOnEntry]
            end
            
            (* Restore the state.  Used both if the function returns normally or if
               it raises an exception.  We use the old state rather than popping the stack
               because that is more reliable if we have an asynchronous exception. *)
            local
                (* Set the entry in the thread vector to an entry from the top-of-stack. *)
                fun restoreEntry(offset, value) =
                    mkNullDec(
                        mkEval(rtsFunction POLY_SYS_assign_word, [#load threadId level, offset, value]))
            in
                val restoreState =
                    [restoreEntry(threadIdCurrentStatic, #load currStatic level),
                     restoreEntry(threadIdCurrentDynamic, #load currDynamic level),
                     restoreEntry(threadIdStack, #load currStack level)]
            end

            local
                (* If an exception is raised we need to call the onExitException entry, restore the state
                   and reraise the exception. *)
                (* There are potential race conditions here if we have asynchronous exceptions. *)
                val savedExn = multipleUses(Ldexc, fn () => mkAddr 1, level)
                val onExitExcFn = loadIdEntry threadIdOnExitExc
                (* OnExitException has an extra curried argument - the exception packet. *)
                val optCallOnExitExc =
                    mkIf(mkTagTest(#load onExitExcFn level, 0w0, 0w0), CodeZero,
                        mkEval(mkEval(#load onExitExcFn level, onArgs), [#load savedExn level]))
            in
                val exceptionCase =
                    mkEnv(#dec savedExn @ #dec onExitExcFn @  [mkNullDec optCallOnExitExc]  @ restoreState,
                        mkRaise(#load savedExn level))
            end
            
            (* Code for the body and the exception. *)
            val bodyCode =
                multipleUses(mkHandle(body, exceptionCase), fn () => mkAddr 1, level)

            (* Code for normal exit. *)
            local
                val (rtEnvDec, exitEnv) = addStartExitEntry(entryEnv, #load bodyCode level, resType, EnvEndFunction)
                val setResult = updateDebugState(exitEnv, level, lex, mkAddr)

                val onExitFn = loadIdEntry threadIdOnExit
                val optCallOnExit =
                    mkIf(mkTagTest(#load onExitFn level, 0w0, 0w0), CodeZero, mkEval(#load onExitFn level, onArgs))
            in
                val exitCode = rtEnvDec @ setResult @ #dec onExitFn @ [mkNullDec optCallOnExit]
            end
        in
            mkEnv(prefixCode @ entryCode @ #dec bodyCode @ exitCode @ restoreState, #load bodyCode level)
        end

    type breakPoint = machineWord ref
 
    (* Create a local break point and check the global and local break points. *)
    fun breakPointCode(location, level, mkAddr) =
    let
        open ADDRESS RuntimeCalls
        (* Create a new breakpoint ref. *)
        val localBreakPoint = ref (toMachineWord 0w0)
        (* First check the global breakpoint. *)
        val threadIdBreakPoint = mkConst(toMachineWord 0w11)
        val threadId = mkEval(rtsFunction POLY_SYS_thread_self, [])
        val globalBpt =
            multipleUses(
                mkEval(rtsFunction POLY_SYS_load_word, [threadId, threadIdBreakPoint]), fn () => mkAddr 1, level)
        (* Then the local breakpoint. *)
        val localBpt =
            multipleUses(
                mkEval(rtsFunction POLY_SYS_load_word,
                    [mkConst(toMachineWord localBreakPoint), CodeZero]), fn () => mkAddr 1, level)
        val testCode =
            mkIf(
                mkNot(mkTagTest(#load globalBpt level, 0w0, 0w0)), 
                mkEval(#load globalBpt level, [mkConst(toMachineWord location)]),
                mkEnv(
                    #dec localBpt,
                    mkIf(
                        mkTagTest(#load localBpt level, 0w0, 0w0),
                        CodeZero,
                        mkEval(#load globalBpt level, [mkConst(toMachineWord location)])
                    )
                )
            )
    in
        (#dec globalBpt @ [mkNullDec testCode], localBreakPoint)
    end

    fun setBreakPoint bpt NONE = bpt := ADDRESS.toMachineWord 0w0
    |   setBreakPoint bpt (SOME f) = bpt := ADDRESS.toMachineWord f

    structure Sharing =
    struct
        type types          = types
        type values         = values
        type machineWord    = machineWord
        type fixStatus      = fixStatus
        type structVals     = structVals
        type typeConstrSet  = typeConstrSet
        type signatures     = signatures
        type functors       = functors
        type locationProp   = locationProp
        type environEntry   = environEntry
        type typeId         = typeId
        type level          = level
        type lexan          = lexan
        type codeBinding    = codeBinding
        type codetree       = codetree
        type typeVarMap     = typeVarMap
        type breakPoint     = breakPoint
    end
end;
